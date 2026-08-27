{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.Scheduler
Description : Partial Plutarch port of @lib/midgard/scheduler.ak@.

The scheduler names the one operator currently appointed to commit blocks, and
advances that appointment from one operator to the next. The active operators
set has to stay in step with it: an operator cannot be removed from the active
set while the scheduler still points at it.

Ported so far: the datum, the spend redeemer and its advancing approaches, and
the authenticated datum reader — everything the active set needs to check that
synchronisation. The scheduler's own validator is a separate slice.
-}
module Midgard.Scheduler (
  passetName,
  PSchedDatum (..),
  PMintRedeemer (..),
  PNeglectedUserEvent (..),
  pshiftEndTime,
  POperatorRemovalReason (..),
  PAdvancingApproach (..),
  PSpendRedeemer (..),
  pgetDatum,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PPubKeyHash,
  PTokenName (..),
  PTxInInfo,
 )
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PPosixTime)
import Midgard.Env (pshiftDuration)
import Midgard.Common.Utils (pgetAuthenticInputDatumWithNftAt)

-- | Aiken @scheduler.asset_name@.
passetName :: forall (s :: S). Term s (PAsData PTokenName)
passetName = pdata (pcon (PTokenName (pconstant "MIDGARD_SCHEDULER")))

{- | Aiken @scheduler.SchedDatum@.

Tags: @NoActiveOperators@ 0, @ActiveOperator@ 1. The empty case is a real state,
not an error — the protocol can run out of active operators, and the registered
set's activation branch exists to recover from exactly that.
-}
data PSchedDatum (s :: S)
  = PNoActiveOperators
  | PActiveOperator
      { pschedActive'operator :: Term s (PAsData PPubKeyHash)
      , pschedActive'startTime :: Term s (PAsData PPosixTime)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSchedDatum)

-- | Aiken @scheduler.OperatorRemovalReason@.
data POperatorRemovalReason (s :: S)
  = POperatorRetirement
  | POperatorSlashing
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POperatorRemovalReason)

{- | Aiken @scheduler.AdvancingApproach@.

Tags, in order: @GoToNextDueToEndOfShift@ 0, @RewindDueToEndOfShift@ 1,
@GoToNextDueToSkippedOperator@ 2, @RewindDueToSkippedOperator@ 3,
@GoToNextDueToOperatorRemoval@ 4, @RewindDueToOperatorRemoval@ 5,
@AppointFirstOperator@ 6.

Every field is typed: unlike the redeemers this package reads only in passing,
the scheduler's own validator consumes all of them, so there is nothing here to
leave as raw 'PData'.

The @Rewind@ variants exist because the active-operator set is a linked list
walked in order — when a shift ends at the last node, the next shift wraps back
to the first, and that is a structurally different transaction from stepping to
a successor. Both members of each pair otherwise mean the same thing, which is
why the reasons (@EndOfShift@, @SkippedOperator@, @OperatorRemoval@) repeat
across them.
-}
data PAdvancingApproach (s :: S)
  = PGoToNextDueToEndOfShift
      {pgoToNextEnd'newShiftsOperatorNodeRefInputIndex :: Term s (PAsData PInteger)}
  | PRewindDueToEndOfShift
      { prewindEnd'activeOperatorsRootRefInputIndex :: Term s (PAsData PInteger)
      , prewindEnd'activeOperatorsLastNodeRefInputIndex :: Term s (PAsData PInteger)
      , prewindEnd'registeredElementRefInputIndex :: Term s (PAsData PInteger)
      }
  | PGoToNextDueToSkippedOperator
      { pgoToNextSkipped'newShiftsOperatorNodeRefInputIndex :: Term s (PAsData PInteger)
      , pgoToNextSkipped'skippedOperatorNodeInputIndex :: Term s (PAsData PInteger)
      , pgoToNextSkipped'activeOperatorsSpendRedeemerIndex :: Term s (PAsData PInteger)
      , pgoToNextSkipped'stateQueueRefInputIndex :: Term s (PAsData PInteger)
      , pgoToNextSkipped'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      , pgoToNextSkipped'neglectedUserEvent :: Term s (PAsData PNeglectedUserEvent)
      }
  | PRewindDueToSkippedOperator
      { prewindSkipped'activeOperatorsRootRefInputIndex :: Term s (PAsData PInteger)
      , prewindSkipped'skippedOperatorNodeInputIndex :: Term s (PAsData PInteger)
      , prewindSkipped'activeOperatorsSpendRedeemerIndex :: Term s (PAsData PInteger)
      , prewindSkipped'stateQueueRefInputIndex :: Term s (PAsData PInteger)
      , prewindSkipped'hubOracleRefInputIndex :: Term s (PAsData PInteger)
      , prewindSkipped'mActiveOperatorsLastNodeRefInputIndex :: Term s (PMaybeData PInteger)
      , prewindSkipped'registeredElementRefInputIndex :: Term s (PAsData PInteger)
      , prewindSkipped'neglectedUserEvent :: Term s (PAsData PNeglectedUserEvent)
      }
  | PGoToNextDueToOperatorRemoval
      { pgoToNextRemoval'activeOperatorsMintRedeemerIndex :: Term s (PAsData PInteger)
      , pgoToNextRemoval'removalReason :: Term s (PAsData POperatorRemovalReason)
      }
  | PRewindDueToOperatorRemoval
      { prewindRemoval'activeOperatorsMintRedeemerIndex :: Term s (PAsData PInteger)
      , prewindRemoval'mActiveOperatorsLastNodeRefInputIndex :: Term s (PMaybeData PInteger)
      , prewindRemoval'removalReason :: Term s (PAsData POperatorRemovalReason)
      , prewindRemoval'registeredElementRefInputIndex :: Term s (PAsData PInteger)
      }
  | PAppointFirstOperator
      { pappointFirst'newShiftsOperatorNodeRefInputIndex :: Term s (PAsData PInteger)
      , pappointFirst'registeredElementRefInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAdvancingApproach)

{- | Aiken @scheduler.MintRedeemer@ — @Init@ 0, @Deinit@ 1.

The scheduler is a single UTxO rather than a list, so its policy only has to
mint the one NFT and burn it; everything else about scheduling happens on the
spend side.
-}
data PMintRedeemer (s :: S)
  = PInit
  | PDeinit
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @scheduler.NeglectedUserEvent@.

Which queued user event, if any, an operator is being accused of neglecting.
@NoNeglectedUserEvent@ is tag 0 and means the accusation rests on
'Midgard.Env.pmaxInactivityBetweenBlockCommitments' alone — the operator
committed no block at all — rather than on a specific event going uncollected.

The three event variants each carry the reference-input index at which the
neglected event can be read, so the scheduler can check the event's inclusion
time against how long the operator has been idle.
-}
data PNeglectedUserEvent (s :: S)
  = PNoNeglectedUserEvent
  | PNeglectedDeposit {pneglected'depositRefInputIndex :: Term s (PAsData PInteger)}
  | PNeglectedWithdrawal {pneglected'withdrawalRefInputIndex :: Term s (PAsData PInteger)}
  | PNeglectedTxOrder {pneglected'txOrderRefInputIndex :: Term s (PAsData PInteger)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNeglectedUserEvent)

{- | Aiken @scheduler.shift_end_time@.

@start_time + shift_duration@. A shift's end is never stored, only computed, so
there is no way for a datum to claim a shift longer than the parameter allows.
-}
pshiftEndTime :: forall (s :: S). Term s (PInteger :--> PInteger)
pshiftEndTime = phoistAcyclic $ plam $ \startTime -> startTime + pshiftDuration

{- | Aiken @scheduler.SpendRedeemer@ — a record, so @Constr 0@.

@scheduler_input_index@ is duplicated between this redeemer and the redeemers of
the scripts that read it; the reader compares the two, which is what stops a
caller pointing at a scheduler input other than the one the scheduler itself is
validating.
-}
data PSpendRedeemer (s :: S) = PSpendRedeemer
  { pschedSpend'schedulerInputIndex :: Term s (PAsData PInteger)
  , pschedSpend'schedulerOutputIndex :: Term s (PAsData PInteger)
  , pschedSpend'advancingApproach :: Term s (PAsData PAdvancingApproach)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)

{- | Aiken @scheduler.get_datum@.

Reads the scheduler's datum from a reference input authenticated by its NFT.
Aiken's @expect scheduler_datum: SchedDatum = ...@ does validate the shape here,
unlike the positional reads elsewhere; the coercion below skips that check, so a
malformed datum fails at the first field read rather than up front. Both reject.
-}
pgetDatum ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PSchedDatum
    )
pgetDatum = phoistAcyclic $
  plam $ \referenceInputs schedulerPolicyId schedulerRefInputIndex ->
    pfromData
      ( punsafeCoerce @(PAsData PSchedDatum)
          ( pgetAuthenticInputDatumWithNftAt
              # referenceInputs
              # schedulerPolicyId
              # passetName
              # schedulerRefInputIndex
          )
      )
