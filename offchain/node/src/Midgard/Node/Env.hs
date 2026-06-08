module Midgard.Node.Env (NodeEnv (..)) where

import Data.Pool (Pool)
import Database.Persist.Postgresql (SqlBackend)
import Midgard.Node.Config (MidgardNodeConfig)

data NodeEnv = NodeEnv
  { config :: MidgardNodeConfig
  , migrationDirectory :: FilePath
  , dbPool :: Maybe (Pool SqlBackend)
  }
