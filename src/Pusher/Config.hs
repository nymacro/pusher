{-# LANGUAGE OverloadedStrings #-}
module Pusher.Config ( PusherCfg(..)
                     , toPusherCfg
                     , DbConfig(..)
                     , toDbConfig
                     , toConnectionString
                     , ServerTlsCfg(..)
                     , toServerTlsCfg
                     , ServerMonCfg(..)
                     , toServerMonCfg
                     , ServerCfg(..)
                     , toServerCfg ) where

import           Data.ByteString
import qualified Data.ByteString.Char8   as C8 (pack)
import qualified Data.Configurator       as Cfg
import qualified Data.Configurator.Types as Cfg
import qualified Data.List               as List
import           Data.Monoid
import           Data.Text

data PusherCfg = PusherCfg { pushTitle    :: Text
                           , pushMessage  :: Text
                           , pushHostname :: Text }

toPusherCfg :: Cfg.Config -> IO PusherCfg
toPusherCfg config = PusherCfg <$> Cfg.lookupDefault "Push'd!" config "push.title"
                               <*> Cfg.lookupDefault "You got a notification" config "push.message"
                               <*> Cfg.lookupDefault "localhost" config "push.hostname"


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
         , "port="     <> (C8.pack . show $ dbPort config)
         , "dbname="   <> dbName config
         , "user="     <> dbUser config
         , "password=" <> dbPass config ]
  where makeIt = mconcat . List.intersperse " "

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
