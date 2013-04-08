module Util.ChatState where

import Data.Text
import Data.Time.Calendar
import Data.Time.LocalTime
import Yesod.Json
import Data.Aeson hiding (object)
import Control.Applicative ((<$>), (<*>))
import Util.EasyDate
import Prelude (show)

data ChatItem = ChatItem { sender :: Text, chatTime :: LocalTime, chatContent :: Text } 
data Timeless = Timeless { tsender :: Text, tcontent :: Text }

instance ToJSON ChatItem where
   toJSON (ChatItem sender time content) = object
      [ "sender" .= sender
      , "time" .= (show time)
      , "content" .= content
      ]

instance FromJSON Timeless where
   parseJSON (Object o) = Timeless
      <$> o .: "sender"
      <*> o .: "content"

newtype Signature = Signature Text

instance FromJSON Signature where
   parseJSON (Object o) = Signature
      <$> o .: "signature"

instance ToJSON Signature where
   toJSON (Signature sig) = object
      [ "signature" .= sig ]
