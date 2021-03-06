{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Pusher.Scripts (serviceWorker, pusherScript) where

import           Data.Aeson
import           Data.Monoid
import           Data.Text.Lazy
import           Text.Julius

import qualified Pusher.Config  as Cfg

data JsonPusherCfg = JsonPusherCfg { pushTitle    :: Value
                                   , pushMessage  :: Value
                                   , pushHostname :: Value}

toJsonPusherCfg :: Cfg.PusherCfg -> JsonPusherCfg
toJsonPusherCfg cfg = JsonPusherCfg (toJSON $ Cfg.pushTitle cfg)
                                    (toJSON $ Cfg.pushMessage cfg)
                                    (toJSON $ Cfg.pushHostname cfg)

serviceWorker :: Cfg.PusherCfg -> Text
serviceWorker cfg = let config = toJsonPusherCfg cfg
                    in renderJavascript $ $(jsFile "templates/sw.julius") (\_ _ -> "")

pusherScript :: Cfg.PusherCfg -> Text
pusherScript cfg = let config = toJsonPusherCfg cfg
                       subscribeUrl = toJSON $ "//" <> Cfg.pushHostname cfg <> ":8080/subscribe"
                   in renderJavascript $ $(jsFile "templates/pusher.julius") (\_ _ -> "")
