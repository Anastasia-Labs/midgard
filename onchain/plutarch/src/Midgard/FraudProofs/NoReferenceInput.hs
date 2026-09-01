{- |
Module      : Midgard.FraudProofs.NoReferenceInput
Description : Plutarch port of @lib/midgard/fraud-proofs/no-reference-input/step-0{1,2,3,4}.ak@.

The thread state and redeemer payloads of the non-existent-reference-input family
(spec §5.1.1): a committed transaction referencing an output that never existed.

'Midgard.FraudProofs.NoInput' one §2.5 slot over — field 1 instead of field 0 —
and the same two absences: not in the block's initial ledger, and not produced by
any transaction of the block.

=== The carriage difference is real, and it is the Aiken tree's

@no-input@'s absence steps take a
'Midgard.FraudProofs.Common.PNonMembershipCarriage', so a prover may publish the
proof beforehand as chunks (issue #545). This family's take a bare @Proof@ and a
withdrawal index instead, which means the proof must ride in the step
transaction's own redeemers. The port reproduces that difference rather than
levelling it: the two families' redeemers are wire format, and an SDK built
against a levelled one would produce bytes neither validator decodes.

The index is vestigial on both sides. Aiken's @plutarch_pexcludes_raw@ binds it
with @expect _withdraw_redeemer_index = withdraw_redeemer_index@ and then finds
the redeemer by script hash, requiring it to be unique; the port's
'Midgard.Common.Utils.pplutarchPexcludesRaw' drops the parameter for the same
reason. It stays in the redeemer type because the redeemer type is the interface.
-}
module Midgard.FraudProofs.NoReferenceInput (
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
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput)
import Midgard.MpfProof.Types (PProof)

-- | Aiken @no_reference_input/step_02.State@.
data PStep02State (s :: S) = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'blocksPrevUtxosRoot :: Term s (PAsData PByteString)
  , pstep02State'blocksTransactionsRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @no_reference_input/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , -- | The prover's chosen §8 carriage for field 1's preimage, together with
    -- the compact structures the id authenticates.
    pstep02Args'referenceInputsOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep02Args'badReferenceInputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

-- | Aiken @no_reference_input/step_03.State@.
data PStep03State (s :: S) = PStep03State
  { pstep03State'missingReferenceInput :: Term s (PAsData PMidgardTxInput)
  , pstep03State'blocksPrevUtxosRoot :: Term s (PAsData PByteString)
  , pstep03State'blocksTransactionsRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

-- | Aiken @no_reference_input/step_03.Args@.
data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'nonMembershipProofInLedger :: Term s (PAsData PProof)
  , -- | Vestigial; see the module header.
    pstep03Args'nonMembershipProofScriptRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

-- | Aiken @no_reference_input/step_04.State@.
data PStep04State (s :: S) = PStep04State
  { pstep04State'missingReferenceInputTxId :: Term s (PAsData PByteString)
  , pstep04State'blocksTransactionsRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

-- | Aiken @no_reference_input/step_04.Args@.
data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'nonMembershipProofInTxs :: Term s (PAsData PProof)
  , -- | Vestigial; see the module header.
    pstep04Args'nonMembershipProofScriptRedeemerIndex :: Term s (PAsData PInteger)
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)
