{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.PendingBlockFinalizationStatus (
  PendingBlockFinalizationStatus (..),
) where

import Database.Persist.TH (derivePersistField)

data PendingBlockFinalizationStatus
  = PendingSubmission
  | SubmittedLocalFinalizationPending
  | SubmittedUnconfirmed
  | ObservedWaitingStability
  | Finalized
  | Abandoned
  deriving stock (Eq, Read, Show)

$(derivePersistField "PendingBlockFinalizationStatus")
