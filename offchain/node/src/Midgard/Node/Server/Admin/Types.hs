module Midgard.Node.Server.Admin.Types (
  MessageResponse (..),
  StateQueueResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype MessageResponse = MessageResponse
  { message :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype StateQueueResponse = StateQueueResponse
  { headers :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
