{- |
Module      : Midgard.FraudProofs.MissingNativeScriptTx
Description : Plutarch port of @lib/midgard/fraud-proofs/missing-native-script-tx/step-0{1..8}.ak@.

The thread state and redeemer payloads of the missing-native-script-tx family
(spec §5.1.1): a committed transaction spending a script-locked output whose
required native script it never witnessed.

Six steps, the longest chain in the machine, because the claim needs two
transactions and three of the bad one's fields:

  1. bind the bad transaction, write its §2.5 anchor;
  2. open field 0 and name the spent input;
  3. bind the /producing/ transaction — the one whose id that input names;
  4. open the producing transaction's field 2 and read the spent output's
     payment credential, which must be a script;
  5. exhibit script bytes hashing to that credential under the native
     language tag;
  6. open the bad transaction's field 6 and show those bytes are absent.

=== Why the anchor's second half rides the whole chain

@bad_tx_witness_set_hash@ is threaded through every state from step-02 to
step-06 even though only step-06 uses it. §3's transaction id preimage is the
/body/, so the compact structure's trailing @witness_set_hash@ is outside it:
bytes carrying the genuine body and an invented tail re-derive to the same
@bad_tx_id@. Step-01 is the only step that sees the structure the block's
counted @transactions_root@ committed, so step-01 is the only step that can read
that hash honestly. Were step-06 to take it from its redeemer, an empty witness
set would make "the required native script is absent" true of every transaction
ever committed.

It costs the same 32 bytes the retired @bad_tx_script_wits_hash@ cost, and
covers all three witness-set fields instead of one.

=== Two ids, from step-04 on

Step-03 swaps the /subject/ transaction: from step-04 the field openings are
against @producing_tx_id@, not @bad_tx_id@, and both must travel because
step-06 goes back to the bad one. That is why step-04's state is the widest in
the family.
-}
module Midgard.FraudProofs.MissingNativeScriptTx (
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep04State (..),
  PStep04Args (..),
  PStep05State (..),
  PStep05Args (..),
  PStep06PhaseV1 (..),
  PStep06State (..),
  PStep06Args (..),
  PStep07Args (..),
  PStep08Args (..),
  pdirectScriptWitnessLimit,
  pstagedScriptWitnessBatchLimit,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput)

{- | Aiken @missing_native_script_tx/step_02.State@ — the bad transaction's §2.5
anchor, both halves.
-}
data PStep02State (s :: S) = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @missing_native_script_tx/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'badInputIndex :: Term s (PAsData PInteger)
  , pstep02Args'spendInputsOpening :: Term s (PAsData PFieldOpeningV1)
  {- ^ The prover's chosen §8 carriage for the bad transaction's field-0
  preimage.
  -}
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

{- | Aiken @missing_native_script_tx/step_03.State@.

The named input travels whole rather than split, because step-03 needs both of
its halves: the id to match the producing transaction it binds, the output index
to hand to step-04's field-2 read.
-}
data PStep03State (s :: S) = PStep03State
  { pstep03State'inputWithMissingScript :: Term s (PAsData PMidgardTxInput)
  , pstep03State'badTxId :: Term s (PAsData PByteString)
  , pstep03State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

{- | Aiken @missing_native_script_tx/step_04.State@ — the widest state in the
family, because two transactions are in play at once.
-}
data PStep04State (s :: S) = PStep04State
  { pstep04State'producingTxId :: Term s (PAsData PByteString)
  , pstep04State'badInputOutputIndex :: Term s (PAsData PInteger)
  , pstep04State'badTxId :: Term s (PAsData PByteString)
  , pstep04State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

-- | Aiken @missing_native_script_tx/step_04.Args@.
data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputsOpening :: Term s (PAsData PFieldOpeningV1)
  {- ^ The prover's chosen §8 carriage for the producing transaction's field-2
  preimage.
  -}
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

{- | Aiken @missing_native_script_tx/step_05.State@.

The producing transaction has done its job by now — what survives it is the one
script hash it yielded.
-}
data PStep05State (s :: S) = PStep05State
  { pstep05State'expectedMissingScriptHash :: Term s (PAsData PByteString)
  , pstep05State'badTxId :: Term s (PAsData PByteString)
  , pstep05State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05State)

-- | Aiken @missing_native_script_tx/step_05.Args@.
data PStep05Args (s :: S) = PStep05Args
  { pstep05Args'inputIndex :: Term s (PAsData PInteger)
  , pstep05Args'outputIndex :: Term s (PAsData PInteger)
  , pstep05Args'missingNativeScriptBytes :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05Args)

data PStep06PhaseV1 (s :: S)
  = PReady
  | PGrammarCertification (Term s (PAsData PByteString))
  | PSemanticScan (Term s (PAsData PByteString)) (Term s (PAsData PBool))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep06PhaseV1)

-- | Aiken @missing_native_script_tx/step_06.State@.
data PStep06State (s :: S) = PStep06State
  { pstep06State'expectedMissingScriptHash :: Term s (PAsData PByteString)
  , pstep06State'badTxId :: Term s (PAsData PByteString)
  , pstep06State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  , pstep06State'phase :: Term s (PAsData PStep06PhaseV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep06State)

data PStep06Args (s :: S)
  = PDirectFinalize
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
  | PStartGrammarCertification
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep06Args)

data PStep07Args (s :: S)
  = PResumeGrammarCertification
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  | PStartSemanticScan
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep07Args)

data PStep08Args (s :: S)
  = PResumeSemanticScan
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  | PFinalizeSemanticScan
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep08Args)

pdirectScriptWitnessLimit :: forall s. Term s PInteger
pdirectScriptWitnessLimit = 64

pstagedScriptWitnessBatchLimit :: forall s. Term s PInteger
pstagedScriptWitnessBatchLimit = 32
