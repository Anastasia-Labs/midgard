{- |
Module      : Midgard.FraudProofs.NoReferenceInput
Description : Plutarch port of @lib/midgard/fraud-proofs/no-reference-input/step-0{1,2,3,4}.ak@.

The thread state and redeemer payloads of the non-existent-reference-input family
(spec §5.1.1): a committed transaction referencing an output that never existed.

'Midgard.FraudProofs.NoInput' one §2.5 slot over — field 1 instead of field 0 —
and the same two absences: not in the block's initial ledger, and not produced by
any transaction of the block.

Both absence steps use the shared non-membership carriage. A fitting proof may
travel in the transaction's withdrawal redeemer, while a deep proof may be
published as authenticated chunks; both routes establish the same root/key
absence predicate.
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

import Midgard.FraudProofs.Common (PNonMembershipCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput)

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
  , pstep03Args'nonMembershipInLedger :: Term s (PAsData PNonMembershipCarriage)
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
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , pstep04Args'nonMembershipInTxs :: Term s (PAsData PNonMembershipCarriage)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)
