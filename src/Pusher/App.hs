{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Pusher.App (pusherApp, subscribe, module Pusher.Config) where

import           Network.HTTP.Types.Status
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Spock.Safe                       (ClientPreferredFormat (..),
                                                       jsonBody, middleware,
                                                       param, preferredFormat,
                                                       setStatus, text, var,
                                                       (<//>))
import qualified Web.Spock.Safe                       as Spock
import qualified Web.Spock.Shared                     as Spock

import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base64               as B64
import qualified Data.ByteString.Base64.URL           as UB64
import qualified Data.ByteString.Char8                as Char8
import qualified Data.ByteString.Lazy                 as Lazy
import           Data.Monoid
import           Data.Text                            (Text, pack, unpack)
import           Data.Text.Encoding                   (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Encoding              as LazyText
import           Data.Time.Clock

import           Database.Persist
import           Database.Persist.Class
import           Database.Persist.Postgresql

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Network.Wreq                         as Wreq

import           GHC.Generics

import           Pusher.Config
import           Pusher.Database
import           Pusher.Scripts

-- | JSON response message for API endpoints
data ResponseMsg = ErrorMsg   { errorMessage :: Text }
                 | SuccessMsg { message :: Text }
$(deriveJSON defaultOptions ''ResponseMsg)

-- | JSON subscription message for /subscribe
data SubscribeMsg = SubscribeMsg { statusType :: Text
                                 , name       :: Text
                                 , endpoint   :: Text
                                 , key        :: Text }
                  deriving (Generic, Show, Eq)
$(deriveJSON defaultOptions ''SubscribeMsg)

-- | Send a basic push message with no message content
sendEmptyNotification :: Text -> IO (Response Lazy.ByteString)
sendEmptyNotification endpoint = do
  let opts = defaults & header "Content-Type"     .~ ["application/octet-stream"]
                      & header "TTL"              .~ ["2419200"]
                      & header "Content-Length" .~ ["0"]
  postWith opts (unpack endpoint) ("" :: ByteString)

-- TODO randomise salt
salt :: ByteString
salt = "Hello World Salt"

-- | Headers used by running app. Certain headers required to allow cross-origin JS
pusherHeaders :: Middleware
pusherHeaders = addHeaders [ ("Access-Control-Allow-Origin", "*")
                           , ("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept, Access-Control-Allow-Origin, Access-Control-Allow-Headers") ]

-- | Run Persistent under Spock
runDB :: (Spock.HasSpock m, Spock.SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDB action = Spock.runQuery $ \conn ->
  runResourceT $ runNoLoggingT $ runSqlConn action conn

subscribe :: ( Control.Monad.IO.Class.MonadIO m
            , Spock.HasSpock (Spock.ActionCtxT ctx m)
            , Spock.SpockConn (Spock.ActionCtxT ctx m) ~ SqlBackend)
            => Pusher.App.SubscribeMsg
            -> Spock.ActionCtxT ctx m b
subscribe msg = do
  time <- liftIO getCurrentTime
  let subscriber = Subscriber (name msg)
                              (endpoint msg)
                              (key msg)
                              time
  runDB $ do
    existing <- selectFirst [SubscriberName ==. name msg] []
    case existing of
      Nothing               -> insert_ subscriber
      Just (Entity subId _) -> replace subId subscriber

  success "subscribed"

unsubscribe :: ( MonadIO m
              , Spock.HasSpock (Spock.ActionCtxT ctx m)
              , Spock.SpockConn (Spock.ActionCtxT ctx m) ~ SqlBackend)
              => Pusher.App.SubscribeMsg
              -> Spock.ActionCtxT ctx m b
unsubscribe msg = do
  runDB $ deleteWhere [SubscriberName ==. name msg]
  success "unsubscribed"

doPushSimple :: ( MonadIO m
               , Spock.HasSpock (Spock.ActionCtxT ctx m)
               , Spock.SpockConn (Spock.ActionCtxT ctx m) ~ SqlBackend)
               => Text
               -> Spock.ActionCtxT ctx m b
doPushSimple sub = do
  subscriber <- runDB $ selectFirst [SubscriberName ==. sub] []
  case subscriber of
    Nothing           -> error_ "invalid sub"
    Just (Entity _ s) -> do
      time <- liftIO getCurrentTime
      if subscriberExpiry s < time
      then do
        liftIO $ forkIO $ do
          r <- sendEmptyNotification (subscriberEndpoint s)
          print r
        success "submitted push"
      else error_ "user subscription has expired"

-- simple helpers for response JSON messages
success msg = Spock.json $ SuccessMsg msg
error_  msg = Spock.json $ ErrorMsg msg

-- simple helper to return JavaScript
javaScript script = do
  Spock.setHeader "Content-Type" "text/javascript"
  Spock.lazyBytes $ LazyText.encodeUtf8 script

pusherApp :: PusherCfg -> Spock.SpockM SqlBackend (Maybe a) () ()
pusherApp config = do
  middleware logStdout
  middleware pusherHeaders
  middleware $ staticPolicy $ addBase "static"

  Spock.get "sw.js"     $ javaScript $ serviceWorker config
  Spock.get "pusher.js" $ javaScript $ pusherScript config

  Spock.post "subscribe" $ do
    msg <- jsonBody
    case msg of
      Nothing -> setStatus status400
      Just m  -> do
        case statusType m of
          "subscribe"   -> subscribe m
          "unsubscribe" -> unsubscribe m
          s             -> error_ $ "unhandled status type: " <> s

  Spock.get ("push" <//> var) $ \sub -> doPushSimple sub
