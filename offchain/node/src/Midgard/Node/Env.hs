module Midgard.Node.Env (NodeEnv (..)) where

import Midgard.Node.Config (MidgardNodeConfig)

-- | Shared process environment for the HTTP handlers.
data NodeEnv = NodeEnv
  { config :: MidgardNodeConfig
  , migrationDirectory :: FilePath
  }
