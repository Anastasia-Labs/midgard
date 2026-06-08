module Midgard.Node.Env (NodeEnv (..)) where

import Midgard.Node.Config (MidgardNodeConfig)

data NodeEnv = NodeEnv
  { config :: MidgardNodeConfig
  , migrationDirectory :: FilePath
  }
