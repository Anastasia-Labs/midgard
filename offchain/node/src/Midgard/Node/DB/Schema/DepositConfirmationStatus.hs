{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.DepositConfirmationStatus (
  DepositConfirmationStatus (..),
) where

import Database.Persist.TH (derivePersistField)

data DepositConfirmationStatus
  = SubmittedConfirmationUnknown
  | Confirmed
  | ReconciledAfterTimeout
  | Ambiguous
  | RetryAllowed
  deriving stock (Eq, Read, Show)

$(derivePersistField "DepositConfirmationStatus")
