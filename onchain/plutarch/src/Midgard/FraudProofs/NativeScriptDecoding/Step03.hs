module Midgard.FraudProofs.NativeScriptDecoding.Step03 (
  POpenSubjectArgs (..),
  PBindDescriptorArgs (..),
  PAdvanceOrCloseArgs (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.Prelude

import Midgard.BoundedItem (PChunkProofV1)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.MpfProof.Types (PProof)
import Midgard.NativeScriptScan (PNativeScriptFrameV1)

data POpenSubjectArgs s = POpenSubjectArgs
  { popenSubjectArgs'inputIndex :: Term s (PAsData PInteger)
  , popenSubjectArgs'outputIndex :: Term s (PAsData PInteger)
  , popenSubjectArgs'fieldOpening :: Term s (PAsData (PMaybeData PFieldOpeningV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POpenSubjectArgs)

data PBindDescriptorArgs s = PBindDescriptorArgs
  { pbindDescriptorArgs'inputIndex :: Term s (PAsData PInteger)
  , pbindDescriptorArgs'outputIndex :: Term s (PAsData PInteger)
  , pbindDescriptorArgs'outpointKeyCbor :: Term s (PAsData PByteString)
  , pbindDescriptorArgs'descriptorCbor :: Term s (PAsData PByteString)
  , pbindDescriptorArgs'ledgerMembershipProof :: Term s (PAsData PProof)
  , pbindDescriptorArgs'firstChunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBindDescriptorArgs)

data PAdvanceOrCloseArgs s = PAdvanceOrCloseArgs
  { padvanceArgs'inputIndex :: Term s (PAsData PInteger)
  , padvanceArgs'outputIndex :: Term s (PAsData PInteger)
  , padvanceArgs'controlCbor :: Term s (PAsData PByteString)
  , padvanceArgs'chunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
  , padvanceArgs'nextChunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
  , padvanceArgs'frames :: Term s (PAsData (PBuiltinList (PAsData PNativeScriptFrameV1)))
  , padvanceArgs'stepBudget :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAdvanceOrCloseArgs)
