{- |
Module      : Midgard.Payout
Description : Partial Plutarch port of @lib/midgard/payout.ak@.

Ported so far: the datum and both redeemers. The reserve validator reads the
spend redeemer to check the two agree on which inputs they are pairing, and the
withdrawal validator reads the mint redeemer when initialising an accumulator.
The payout validator itself (1,436 lines) is a separate slice.
-}
module Midgard.Payout (
  PSpendRedeemer (..),
  PMintRedeemer (..),
  PPayoutDatum (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PAddress, POutputDatum, PTokenName, PTxOutRef)
import Plutarch.Prelude

import Midgard.Common.Types (PValuePairs)

{- | Aiken @payout.SpendRedeemer@.

Constructor order fixes the on-chain tag: @AddFunds@ is 0, @ConcludeWithdrawal@
is 1. The reserve validator accepts only @AddFunds@, so a swapped order would
silently let a withdrawal drain the reserve.
-}
data PSpendRedeemer (s :: S)
  = PAddFunds
      { ppayoutAddFunds'payoutInputIndex :: Term s (PAsData PInteger)
      , ppayoutAddFunds'payoutOutputIndex :: Term s (PAsData PInteger)
      , ppayoutAddFunds'reserveInputIndex :: Term s (PAsData PInteger)
      , ppayoutAddFunds'reserveChangeOutputIndex :: Term s (PMaybeData PInteger)
      , ppayoutAddFunds'reserveSpendRedeemerIndex :: Term s (PAsData PInteger)
      , ppayoutAddFunds'payoutSpendRedeemerIndex :: Term s (PAsData PInteger)
      , ppayoutAddFunds'hubRefInputIndex :: Term s (PAsData PInteger)
      }
  | PConcludeWithdrawal
      { ppayoutConclude'payoutInputIndex :: Term s (PAsData PInteger)
      , ppayoutConclude'l1OutputIndex :: Term s (PAsData PInteger)
      , ppayoutConclude'burnRedeemerIndex :: Term s (PAsData PInteger)
      , ppayoutConclude'hubRefInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)

{- | Aiken @payout.MintRedeemer@.

Tags: @MintPayout@ 0, @BurnPayout@ 1.

@MintPayout@ is read by the withdrawal validator, which cross-checks every field
against its own redeemer — each script is relying on the other's indices, so
neither may be taken on trust.
-}
data PMintRedeemer (s :: S)
  = PMintPayout
      { pmintPayout'withdrawalUtxoOutRef :: Term s (PAsData PTxOutRef)
      , pmintPayout'withdrawalInputIndex :: Term s (PAsData PInteger)
      , pmintPayout'withdrawalSpendRedeemerIndex :: Term s (PAsData PInteger)
      , pmintPayout'hubRefInputIndex :: Term s (PAsData PInteger)
      }
  | PBurnPayout
      { pburnPayout'payoutInputIndex :: Term s (PAsData PInteger)
      , pburnPayout'payoutAssetName :: Term s (PAsData PTokenName)
      , pburnPayout'payoutSpendRedeemerIndex :: Term s (PAsData PInteger)
      , pburnPayout'hubRefInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @payout.Datum@.

What the accumulator is working towards: the L2 value owed, and where on L1 it
must eventually land.
-}
data PPayoutDatum (s :: S) = PPayoutDatum
  { ppayoutDatum'l2Value :: Term s (PAsData PValuePairs)
  , ppayoutDatum'l1Address :: Term s (PAsData PAddress)
  , ppayoutDatum'l1Datum :: Term s POutputDatum
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPayoutDatum)
