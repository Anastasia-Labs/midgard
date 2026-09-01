{- |
Module      : Midgard.UserEvents.Deposit
Description : Partial Plutarch port of @lib/midgard/user-events/deposit.ak@.

A deposit is L1 funds queued to appear on L2. The datum records where they
should land and when the event becomes includable.

@get_datum@ is not ported: its only consumer is the scheduler, which is a
separate slice.
-}
module Midgard.UserEvents.Deposit (
  PDepositDatum (..),
  pgetDatum,
  PSpendRedeemer (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PScriptHash)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTxInInfo)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PPosixTime)
import Midgard.Common.Utils (pgetAuthenticInputDatumWithPolicyAt)
import Midgard.LedgerState (PDepositEvent)
import Midgard.TransitionTrace (PRootMembershipProof)

{- | Aiken @deposit.DepositDatum@.

Field order is load-bearing beyond this type: "Midgard.UserEvents" reads the
first three fields of /every/ event datum positionally, so @event@,
@inclusion_time@ and @witness@ must stay first and in this order across all
three user events.
-}
data PDepositDatum (s :: S) = PDepositDatum
  { pdepositDatum'event :: Term s (PAsData PDepositEvent)
  , pdepositDatum'inclusionTime :: Term s (PAsData PPosixTime)
  , pdepositDatum'witness :: Term s (PAsData PScriptHash)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDepositDatum)

{- | Aiken @deposit.SpendRedeemer@ — a record, so @Constr 0@.

Six indices and a proof. The indices are caller-supplied hints that keep the
lookups constant-time; each is validated at the point it is used, so a wrong
index rejects rather than mis-resolving.
-}
data PSpendRedeemer (s :: S) = PSpendRedeemer
  { pdepositSpend'inputIndex :: Term s (PAsData PInteger)
  , pdepositSpend'outputIndex :: Term s (PAsData PInteger)
  , pdepositSpend'hubRefInputIndex :: Term s (PAsData PInteger)
  , pdepositSpend'settlementRefInputIndex :: Term s (PAsData PInteger)
  , pdepositSpend'mintRedeemerIndex :: Term s (PAsData PInteger)
  , pdepositSpend'membershipProof :: Term s (PAsData PRootMembershipProof)
  , pdepositSpend'inclusionProofScriptWithdrawRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)

{- | Aiken @deposit.get_datum@.

Reads a deposit's datum from a reference input authenticated by its policy id.
The asset /name/ is unconstrained here, because each deposit carries its own
one-off event NFT; what identifies it is the policy.

Aiken's @expect deposit_datum: Datum = ...@ structurally validates the datum;
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
        :--> PDepositDatum
    )
pgetDatum = phoistAcyclic $
  plam $ \referenceInputs depositPolicyId depositInputIndex ->
    pfromData
      ( punsafeCoerce @(PAsData PDepositDatum)
          ( pgetAuthenticInputDatumWithPolicyAt
              # referenceInputs
              # depositPolicyId
              # depositInputIndex
          )
      )
