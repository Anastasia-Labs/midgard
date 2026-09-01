{- |
Module      : Midgard.FraudProofs.MissingSignature
Description : Plutarch port of @lib/midgard/fraud-proofs/missing-signature/step-0{1,2,3,4}.ak@.

The thread state and redeemer payloads of the missing-signature family (spec
§5.1.1): a committed transaction that names a required signer and carries no
witness for it.

=== The §2.5 anchor is two values, and only step-01 can supply the second

@verified_tx_id@ authenticates fields 0–5, because §3's id preimage is the body
alone. It says /nothing/ about the witness set: the compact structure's trailing
@witness_set_hash@ sits outside the preimage, so bytes carrying the genuine body
and an invented tail re-derive to the same id.

Step-02 opens field 4 and needs only the id. Step-04 opens field 7 and needs the
hash as well — and the only step that can authenticate it is step-01, which reads
it off the compact structure the block's counted @transactions_root@ committed.
Downstream it would be a prover's assertion; there it is a reading. So it is
carried the length of the family, in every state, at the 32 bytes the retired
per-collection @addr_tx_wits_hash@ cost — covering all three witness-set fields
rather than one.

Without it the forgery is total: a door that checked only the id would accept the
/empty/ witness set against any transaction, making "the required signature is
absent" true of every transaction ever committed.

=== One id, two fields

Steps 02 and 04 each re-open a different field of the same transaction from that
one anchor — field 4 there, field 7 here — so the thread never carries a field
hash whose slot §4's plain hashing no longer records.
-}
module Midgard.FraudProofs.MissingSignature (
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep03Args (..),
  PStep04State (..),
  PStep04Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)

-- | Aiken @missing_signature/step_02.State@ — the §2.5 anchor, both halves.
data PStep02State (s :: S) = PStep02State
  { pstep02State'verifiedTxId :: Term s (PAsData PByteString)
  , pstep02State'verifiedWitnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @missing_signature/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for field 4's preimage.
    pstep02Args'requiredSignersOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep02Args'badRequiredSignerHashIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

-- | Aiken @missing_signature/step_03.State@.
data PStep03State (s :: S) = PStep03State
  { pstep03State'missingRequiredSignerHash :: Term s (PAsData PByteString)
  , pstep03State'verifiedTxId :: Term s (PAsData PByteString)
  , pstep03State'verifiedWitnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

{- | Aiken @missing_signature/step_03.Args@.

The verification key, which the thread has only the /hash/ of. Field 4 holds
28-byte hashes and field 7 holds 32-byte keys, so the two collections cannot be
compared directly; this step is the bridge, and its only guard is that the key
hashes to what step-02 read.
-}
data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'missingRequiredSignerVkey :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

-- | Aiken @missing_signature/step_04.State@.
data PStep04State (s :: S) = PStep04State
  { pstep04State'missingRequiredSignerVkey :: Term s (PAsData PByteString)
  , pstep04State'verifiedTxId :: Term s (PAsData PByteString)
  , pstep04State'verifiedWitnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

-- | Aiken @missing_signature/step_04.Args@.
data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for field 7's preimage, plus the
    -- witness set the door checks against the thread-anchored hash.
    pstep04Args'addrTxWitsOpening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)
