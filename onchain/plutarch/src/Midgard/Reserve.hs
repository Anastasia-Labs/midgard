{- |
Module      : Midgard.Reserve
Description : Plutarch port of @lib/midgard/reserve.ak@.
-}
module Midgard.Reserve (PSpendRedeemer (..)) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

{- | Aiken @reserve.SpendRedeemer@.

A single constructor carrying four positional hints: where the reserve input
sits, where the payout input sits, which redeemer entry is the payout's, and
which reference input is the hub oracle.
-}
data PSpendRedeemer (s :: S) = PSpend
  { preserveSpend'reserveInputIndex :: Term s (PAsData PInteger)
  , preserveSpend'payoutInputIndex :: Term s (PAsData PInteger)
  , preserveSpend'payoutSpendRedeemerIndex :: Term s (PAsData PInteger)
  , preserveSpend'hubRefInputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)
