{- |
Module      : Midgard.UserEvents.Withdrawal
Description : Partial Plutarch port of @lib/midgard/user-events/withdrawal.ak@.

A withdrawal is an L2 request to move funds back to L1. Unlike a deposit, its
datum is the full @OptimisticDatum@: it carries a refund address and datum, so
that a withdrawal an operator judged invalid can be returned to the user rather
than stranded.

@get_datum@ is not ported — its only consumer is the scheduler.
-}
module Midgard.UserEvents.Withdrawal (
  PWithdrawalDatum (..),
  pgetDatum,
  PSpendPurpose (..),
  PSpendRedeemer (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PAddress, POutputDatum, PScriptHash)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTxInInfo)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PPosixTime)
import Midgard.Common.Utils (pgetAuthenticInputDatumWithPolicyAt)
import Midgard.LedgerState (PWithdrawalEvent, PWithdrawalValidity)
import Midgard.TransitionTrace (PRootMembershipProof)

{- | Aiken @user_events.OptimisticDatum<WithdrawalEvent>@.

Aiken parameterises @OptimisticDatum@ over the event type; this is the
withdrawal instantiation, the only one a ported validator reads.

Field order is load-bearing beyond this type: "Midgard.UserEvents" reads the
first three fields of /every/ event datum positionally when authenticating a
new event, so @event@, @inclusion_time@ and @witness@ must stay first and in
this order.
-}
data PWithdrawalDatum (s :: S) = PWithdrawalDatum
  { pwithdrawalDatum'event :: Term s (PAsData PWithdrawalEvent)
  , pwithdrawalDatum'inclusionTime :: Term s (PAsData PPosixTime)
  , pwithdrawalDatum'witness :: Term s (PAsData PScriptHash)
  , pwithdrawalDatum'refundAddress :: Term s (PAsData PAddress)
  , pwithdrawalDatum'refundDatum :: Term s POutputDatum
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWithdrawalDatum)

{- | Aiken @withdrawal.SpendPurpose@.

Tags: @InitializePayout@ 0, @Refund@ 1.

The two ways a withdrawal leaves: forward into a payout accumulator if the
operator judged it valid, or back to the user if not. @Refund@ carries the
verdict being claimed, which the settlement proof must corroborate.
-}
data PSpendPurpose (s :: S)
  = PInitializePayout
  | PRefund {prefund'validityOverride :: Term s (PAsData PWithdrawalValidity)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendPurpose)

-- | Aiken @withdrawal.SpendRedeemer@ — a record, so @Constr 0@.
data PSpendRedeemer (s :: S) = PSpendRedeemer
  { pwithdrawalSpend'inputIndex :: Term s (PAsData PInteger)
  , pwithdrawalSpend'outputIndex :: Term s (PAsData PInteger)
  , pwithdrawalSpend'hubRefInputIndex :: Term s (PAsData PInteger)
  , pwithdrawalSpend'settlementRefInputIndex :: Term s (PAsData PInteger)
  , pwithdrawalSpend'burnRedeemerIndex :: Term s (PAsData PInteger)
  , pwithdrawalSpend'payoutMintRedeemerIndex :: Term s (PAsData PInteger)
  , pwithdrawalSpend'membershipProof :: Term s (PAsData PRootMembershipProof)
  , pwithdrawalSpend'inclusionProofScriptWithdrawRedeemerIndex :: Term s (PAsData PInteger)
  , pwithdrawalSpend'purpose :: Term s (PAsData PSpendPurpose)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)

{- | Aiken @withdrawal.get_datum@.

Reads a withdrawal's datum from a reference input authenticated by its policy id.
The asset /name/ is unconstrained here, because each withdrawal carries its own
one-off event NFT; what identifies it is the policy.

Aiken's @expect withdrawal_datum: Datum = ...@ structurally validates the datum;
the coercion below does not, so a malformed datum fails at the first field read
rather than up front. Both reject.
-}
pgetDatum ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PWithdrawalDatum
    )
pgetDatum = phoistAcyclic $
  plam $ \referenceInputs withdrawalPolicyId withdrawalInputIndex ->
    pfromData
      ( punsafeCoerce @(PAsData PWithdrawalDatum)
          ( pgetAuthenticInputDatumWithPolicyAt
              # referenceInputs
              # withdrawalPolicyId
              # withdrawalInputIndex
          )
      )
