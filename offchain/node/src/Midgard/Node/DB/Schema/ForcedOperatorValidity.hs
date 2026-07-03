{-# LANGUAGE TemplateHaskell #-}

module Midgard.Node.DB.Schema.ForcedOperatorValidity (
  ForcedOperatorValidity (..),
) where

import Database.Persist.TH (derivePersistField)

data ForcedOperatorValidity
  = TxIsValid
  | NonExistentInputUtxo
  | InvalidSignature
  | FailedScript
  | FeeTooLow
  | UnbalancedTx
  deriving stock (Eq, Read, Show)

$(derivePersistField "ForcedOperatorValidity")
