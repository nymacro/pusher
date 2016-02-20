{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

import qualified Web.Spock.Safe              as Spock

import           Database.Persist.Postgresql
import           Pusher.App
import           Pusher.Database

import           Data.ByteString             (ByteString)
import qualified Data.Configurator           as Cfg
import qualified Data.Configurator.Types     as Cfg
import qualified Data.List                   as List
import           Data.Monoid

import           Control.Monad               (when)
import           Control.Monad.Logger

import           System.Remote.Monitoring

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
         , "dbname="   <> dbName config
         , "user="     <> dbUser config
         , "password=" <> dbPass config ]
  where makeIt = mconcat . List.intersperse " "

toPusherCfg :: Cfg.Config -> IO PusherCfg
toPusherCfg config = PusherCfg <$> Cfg.lookupDefault "Push'd!" config "push.title"
                               <*> Cfg.lookupDefault "You got a notification" config "push.message"

main :: IO ()
main = do
  config      <- Cfg.load [ Cfg.Required "pusher.cfg" ]
  dbConf      <- toDbConfig config
  pusherCfg   <- toPusherCfg config
  port        <- Cfg.lookupDefault 8080 config "server.port"

  monitor     <- Cfg.lookupDefault False config "server.monitor.start"
  monitorPort <- (Cfg.lookupDefault 6969 config "server.monitor.port")

  case toConnectionString <$> dbConf of
    Nothing -> putStrLn "Configuration incorrect"
    Just c  -> do
      when monitor $ forkServer "localhost" monitorPort >> putStrLn ("Monitor is running on port " <> show monitorPort)
      start port pusherCfg c

start :: Int -> PusherCfg -> ByteString -> IO ()
start port pusherCfg connStr = do
  pool <- runStderrLoggingT $ createPostgresqlPool connStr 10
  runStderrLoggingT $ runSqlPool (runMigration migrateAll) pool

  let spockCfg = Spock.defaultSpockCfg Nothing (Spock.PCPool pool) ()
  Spock.runSpock port $ Spock.spock spockCfg (pusherApp pusherCfg)
