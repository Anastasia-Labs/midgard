{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.StateQueueLeaseStatus (
  StateQueueLeaseStatus (..),
) where

import Database.Persist.TH (derivePersistField)

data StateQueueLeaseStatus
  = Active
  | Released
  | Failed
  deriving stock (Eq, Read, Show)

$(derivePersistField "StateQueueLeaseStatus")
