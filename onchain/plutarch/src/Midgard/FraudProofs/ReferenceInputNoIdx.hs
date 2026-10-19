{- |
Module      : Midgard.FraudProofs.ReferenceInputNoIdx
Description : Plutarch port of @lib/midgard/fraud-proofs/reference-input-no-idx/step-0{1,2,3,4}.ak@.

The thread state and redeemer payloads of the reference-input-index family (spec
§5.1.1): a committed transaction /referencing/ output @n@ of a transaction that
has fewer than @n+1@ outputs.

Structurally this is 'Midgard.FraudProofs.InputNoIdx' one field over — §2.5's
table puts spend inputs at slot 0 and reference inputs at slot 1, and the two
share a stride and an item shape. That similarity is exactly why the /index/ has
to be what separates them: §4 removed field-index domain separation, so a
field-0 preimage and a field-1 preimage over the same items commit identically.
The slot comes from what the validator passes the door, and nothing in these
types names it.

The family also inherits the shape change recorded in
'Midgard.FraudProofs.InputNoIdx': the fold arms and the self-loop state that once
streamed the collection one counted opening at a time are gone, because the door
hashes the preimage once and reads item @n@ by arithmetic.
-}
module Midgard.FraudProofs.ReferenceInputNoIdx (
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep04State (..),
  PStep04Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)

-- | Aiken @reference_input_no_idx/step_02.State@ — the disputed transaction's verified id.
newtype PStep02State (s :: S) = PStep02State
  {pstep02State'verifiedTxId :: Term s (PAsData PByteString)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @reference_input_no_idx/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for field 0's preimage.
    pstep02Args'referenceInputsOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep02Args'badReferenceInputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

{- | Aiken @reference_input_no_idx/step_03.State@ — the disputed reference input,
split.

The output index travels beside the transaction id rather than inside a decoded
input, because the two are used by different steps: step-03 matches the id
against a producing transaction it binds, and step-04 compares the index against
that transaction's authenticated output count.
-}
data PStep03State (s :: S) = PStep03State
  { pstep03State'badReferenceInputTxId :: Term s (PAsData PByteString)
  , pstep03State'badReferenceInputOutputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

-- | Aiken @reference_input_no_idx/step_04.State@.
data PStep04State (s :: S) = PStep04State
  { pstep04State'producingTxId :: Term s (PAsData PByteString)
  , pstep04State'badReferenceInputOutputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

-- | Aiken @reference_input_no_idx/step_04.Args@.
data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for the producing transaction's field-2
    -- preimage. Its authenticated item count is the output count the
    -- out-of-range verdict rests on (§5.2).
    pstep04Args'outputsOpening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)
