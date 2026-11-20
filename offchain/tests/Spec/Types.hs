module Spec.Types (TestTxError (..)) where

import Control.Exception (Exception)

import Convex.Class (SendTxError)
import Convex.CoinSelection (BalanceTxError)

-- | Errors that may be raised during tests.
data TestTxError era
  = -- | Error raised through the return value of balanceAndSubmit.
    SubmissionError (SendTxError era)
  | -- | Error raised during balancing.
    TxBalancingError (BalanceTxError era)
  | -- | Error raised within our transaction building.
    TxBuildingError String
  deriving stock (Show)
  deriving anyclass (Exception)
