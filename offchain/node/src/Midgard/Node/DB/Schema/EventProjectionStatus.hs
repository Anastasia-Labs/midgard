{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.EventProjectionStatus (
  EventProjectionStatus (..),
) where

import Database.Persist.TH (derivePersistField)

data EventProjectionStatus
  = Awaiting
  | Projected
  | Finalized
  deriving stock (Eq, Read, Show)

$(derivePersistField "EventProjectionStatus")
