module Midgard.Node.Env (NodeEnv (..)) where

import Data.Pool (Pool)
import Database.Persist.Postgresql (SqlBackend)

import Midgard.Node.Config (MidgardNodeConfig)

-- | Shared process environment for the HTTP handlers.
data NodeEnv = NodeEnv
  { config :: MidgardNodeConfig
  , dbPool :: Pool SqlBackend
  }
