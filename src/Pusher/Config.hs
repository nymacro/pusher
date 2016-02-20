module Pusher.Config (PusherCfg(..)) where

import           Data.Text

data PusherCfg = PusherCfg { pushTitle   :: Text
                           , pushMessage :: Text }
