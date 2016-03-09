{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

import qualified Web.Spock.Safe                       as Spock

import           Database.Persist.Postgresql
import           Pusher.App
import           Pusher.Database

import           Data.ByteString                      (ByteString)
import           Data.ByteString.Char8                (pack)
import qualified Data.Configurator                    as Cfg
import qualified Data.Configurator.Types              as Cfg
import qualified Data.List                            as List
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Encoding                   (encodeUtf8)

import           Control.Concurrent                   (forkIO)
import           Control.Monad                        (when)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Logger

import           Network.Wai.Handler.Warp             (defaultSettings, setPort)
import           Network.Wai.Handler.WarpTLS          (runTLS, tlsSettings)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified System.Metrics                       as Ekg
import qualified System.Remote.Monitoring             as Ekg

import           Pusher.Config

main :: IO ()
main = do
  config    <- Cfg.load [ Cfg.Required "pusher.cfg" ]
  dbConf    <- toDbConfig config
  serverCfg <- toServerCfg config

  case toConnectionString <$> dbConf of
    Nothing -> putStrLn "Configuration incorrect"
    Just c  -> do
      -- start monitor if configuration present
      server <- flip (maybe (return Nothing)) (serverMonCfg serverCfg) $ \cfg -> do
        let monitorPort = serverMonPort cfg
        putStrLn ("Monitor is running on port " <> show monitorPort)
        server  <- Ekg.forkServer "localhost" monitorPort
        metrics <- defaultServerMetrics server
        return $ Just $ ServerMonitor { monitor = server
                                      , metrics = metrics }

      start serverCfg{serverMonitor = server} c

start :: ServerCfg  -- ^ app config
      -> ByteString -- ^ DB connection string
      -> IO ()
start config connStr = do
  let port = serverPort config

  runStderrLoggingT $ do
    pool <- createPostgresqlPool connStr 10
    runSqlPool (runMigration migrateAll) pool

    let spockCfg  = Spock.defaultSpockCfg Nothing (Spock.PCPool pool) ()
        app       = Spock.spock spockCfg (pusherApp config)
        startHttp = do
            putStrLn $ "Running HTTP on port " <> show port
            Spock.runSpockNoBanner port app

    -- start TLS server if config is present
    liftIO $ flip (maybe (return ())) (serverTlsCfg config) $ \cfg -> do
        let tlsConfig  = tlsSettings (serverTlsCert cfg)
                                    (serverTlsKey  cfg)
            portConfig = setPort (serverTlsPort cfg) defaultSettings
        putStrLn $ "Running HTTPS on port " <> show (serverTlsPort cfg)
        tlsApp <- Spock.spockAsApp app
        forkIO $ runTLS tlsConfig portConfig tlsApp
        return ()

    -- kick off straight HTTP
    liftIO startHttp
