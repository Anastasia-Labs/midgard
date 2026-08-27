{- |
Module      : Midgard.FraudProofs.ZeroInput
Description : Plutarch port of @lib/midgard/fraud-proofs/zero-input/step-0{1,2}.ak@.

The thread state and redeemer payload of the zero-input family (spec §5.1.1): a
committed transaction that spends nothing.

Step-01 has no state of its own — its @Datum@ is @StepDatum<Data>@ — so only
step-02's pair is a type.

=== Why the id travels, and not a commitment

The obvious shape for this family is to forward the disputed transaction's
spend-inputs commitment and compare it against the pinned commitment of the empty
field. That shape is wrong, and it is wrong in a way that reads as correct: §4
removed field-index domain separation, so the empty field has /one/ commitment
that is the same value in all nine slots. A hash equality would prove "some field
of this transaction is empty" where the rule needs "field 0 is".

So what travels is the transaction id, and step-02 opens field 0 through the
door and reads its authenticated item count. Positional identity lives inside the
door; this is how the family gets it.
-}
module Midgard.FraudProofs.ZeroInput (
  PStep02State (..),
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)

-- | Aiken @zero_input/step_02.State@ — the disputed transaction's verified id.
newtype PStep02State (s :: S) = PStep02State
  {pstep02State'badTxId :: Term s (PAsData PByteString)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @zero_input/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for field 0's preimage — for a
    -- genuinely empty field, the single byte @80@.
    pstep02Args'spendInputsOpening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)
