{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.StateQueueScope (
  StateQueueScope (..),
) where

import Database.Persist.TH (derivePersistField)

data StateQueueScope
  = StateQueue
  deriving stock (Eq, Read, Show)

$(derivePersistField "StateQueueScope")
