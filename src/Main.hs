{-# LANGUAGE DeriveGeneric       #-}
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

import           Control.Monad                        (when)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Logger

import           Network.Wai.Handler.Warp             (defaultSettings, setPort)
import           Network.Wai.Handler.WarpTLS          (runTLS, tlsSettings)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified System.Remote.Monitoring             as Ekg

data DbConfig = DbConfig { dbUser :: ByteString
                         , dbPass :: ByteString
                         , dbHost :: ByteString
                         , dbPort :: Int
                         , dbName :: ByteString }

toDbConfig :: Cfg.Config -> IO (Maybe DbConfig)
toDbConfig config = do
  username <- Cfg.lookup config "database.username"
  password <- Cfg.lookup config "database.password"
  hostname <- Cfg.lookup config "database.host"
  port     <- Cfg.lookup config "database.port"
  name     <- Cfg.lookup config "database.name"
  return $ DbConfig <$> username <*> password <*> hostname <*> port <*> name

toConnectionString :: DbConfig -> ByteString
toConnectionString config =
  makeIt [ "host="     <> dbHost config
         , "port="     <> (pack . show $ dbPort config)
         , "dbname="   <> dbName config
         , "user="     <> dbUser config
         , "password=" <> dbPass config ]
  where makeIt = mconcat . List.intersperse " "

toPusherCfg :: Cfg.Config -> IO PusherCfg
toPusherCfg config = PusherCfg <$> Cfg.lookupDefault "Push'd!" config "push.title"
                               <*> Cfg.lookupDefault "You got a notification" config "push.message"


data ServerTlsCfg = ServerTlsCfg { serverTlsPort :: Int
                                 , serverTlsCert :: String
                                 , serverTlsKey  :: String }

toServerTlsCfg :: Cfg.Config -> IO (Maybe ServerTlsCfg)
toServerTlsCfg config = do
  port <- Cfg.lookup config "server.tls.port"
  cert <- Cfg.lookup config "server.tls.certificate"
  key  <- Cfg.lookup config "server.tls.key"
  return $ ServerTlsCfg <$> port <*> cert <*> key

data ServerMonCfg = ServerMonCfg { serverMonPort :: Int }

toServerMonCfg :: Cfg.Config -> IO (Maybe ServerMonCfg)
toServerMonCfg config = do
  port <- Cfg.lookup config "server.monitor.port"
  return $ ServerMonCfg <$> port

data ServerCfg = ServerCfg { serverPort   :: Int
                           , serverTlsCfg :: Maybe ServerTlsCfg
                           , serverMonCfg :: Maybe ServerMonCfg }

toServerCfg :: Cfg.Config -> IO ServerCfg
toServerCfg config = do
  port      <- Cfg.lookupDefault 8080 config "server.port"
  tlsConfig <- toServerTlsCfg config
  monConfig <- toServerMonCfg config
  return $ ServerCfg port tlsConfig monConfig

main :: IO ()
main = do
  config    <- Cfg.load [ Cfg.Required "pusher.cfg" ]
  dbConf    <- toDbConfig config
  pusherCfg <- toPusherCfg config
  serverCfg <- toServerCfg config

  case toConnectionString <$> dbConf of
    Nothing -> putStrLn "Configuration incorrect"
    Just c  -> do
      -- start monitor if configuration present
      flip (maybe (return ())) (serverMonCfg serverCfg) $ \cfg -> do
        let monitorPort = serverMonPort cfg
        Ekg.forkServer "localhost" monitorPort
        putStrLn ("Monitor is running on port " <> show monitorPort)

      start serverCfg pusherCfg c

start :: ServerCfg  -- ^ server config
      -> PusherCfg  -- ^ app config
      -> ByteString -- ^ DB connection string
      -> IO ()
start config pusherCfg connStr = do
  let port = serverPort config

  runStderrLoggingT $ do
    pool <- createPostgresqlPool connStr 10
    runSqlPool (runMigration migrateAll) pool

    let spockCfg  = Spock.defaultSpockCfg Nothing (Spock.PCPool pool) ()
        app       = Spock.spock spockCfg (pusherApp pusherCfg)
        startHttp = do
          putStrLn $ "Running HTTP on port " <> show port
          Spock.runSpockNoBanner port app

    -- start TLS server if config is present, otherwise just use HTTP
    liftIO $ flip (maybe startHttp) (serverTlsCfg config) $ \cfg -> do
      let tlsConfig  = tlsSettings (serverTlsCert cfg)
                                   (serverTlsKey  cfg)
          portConfig = setPort (serverTlsPort cfg) defaultSettings
      putStrLn $ "Running HTTPS on port " <> show (serverTlsPort cfg)
      tlsApp <- Spock.spockAsApp app
      runTLS tlsConfig portConfig tlsApp


