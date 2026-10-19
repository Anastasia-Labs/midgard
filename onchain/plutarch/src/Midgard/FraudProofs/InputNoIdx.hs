{- |
Module      : Midgard.FraudProofs.InputNoIdx
Description : Plutarch port of @lib/midgard/fraud-proofs/input-no-idx/step-0{1,2,3,4}.ak@.

The thread state and redeemer payloads of the input-index family (spec §5.1.1):
a committed transaction spending output /n/ of a transaction that has fewer than
/n+1/ outputs.

=== One state, one route

The Aiken step-02 module carries a header worth repeating, because it records
what this family used to be. It had a two-constructor state and a
four-constructor redeemer: a direct arm, a tier-2 "complete publication" arm, and
a @FoldStart@/@FoldNext@ pair that streamed the spend-input collection one
counted @bounded_collection_v1.verify_item@ opening at a time. All of that
existed for one reason — the whole collection had to be reproduced inside the
step to re-hash it, and at real cardinalities it did not fit (finding Q1X-F6,
issue #551).

Under the flat scheme the door hashes the preimage once and reads item /n/ by
arithmetic, so there is nothing left to stream. The fold arms, the self-loop
state and the family's own publication type are gone, and tier-2 carriage is the
door's @RawUtxo@ arm rather than a type this family maintains. What is left is
one state per step and one @Args@ record.
-}
module Midgard.FraudProofs.InputNoIdx (
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

-- | Aiken @input_no_idx/step_02.State@ — the disputed transaction's verified id.
newtype PStep02State (s :: S) = PStep02State
  {pstep02State'verifiedTxId :: Term s (PAsData PByteString)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @input_no_idx/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for field 0's preimage.
    pstep02Args'spendInputsOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep02Args'badInputsIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

{- | Aiken @input_no_idx/step_03.State@ — the disputed input, split.

The output index travels beside the transaction id rather than inside a decoded
input, because the two are used by different steps: step-03 matches the id
against a producing transaction it binds, and step-04 compares the index against
that transaction's authenticated output count.
-}
data PStep03State (s :: S) = PStep03State
  { pstep03State'badInputTxId :: Term s (PAsData PByteString)
  , pstep03State'badInputOutputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

-- | Aiken @input_no_idx/step_04.State@.
data PStep04State (s :: S) = PStep04State
  { pstep04State'producingTxId :: Term s (PAsData PByteString)
  , pstep04State'badInputOutputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

-- | Aiken @input_no_idx/step_04.Args@.
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
