module Midgard.FraudProofs.MintAuthorization (
  PStep01Args (..),
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep03Args (..),
  PStep04State (..),
  PStep04Args (..),
  PStep05State (..),
  PStep05Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.LedgerState (PHeaderV1)
import Midgard.MpfProof.Types (PProof)
import Midgard.TransitionTrace (PRootMembershipProof)

newtype PStep01Args s = PStep01Args
  {pstep01Args'carriage :: Term s (PAsData PNativeTxInclusionCarriage)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State s = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  , pstep02State'validityIntervalStart :: Term s (PAsData PInteger)
  , pstep02State'validityIntervalEnd :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args s = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'header :: Term s (PAsData PHeaderV1)
  , pstep02Args'eventToStepMembership :: Term s (PAsData PRootMembershipProof)
  , pstep02Args'transitionStepMembership :: Term s (PAsData PRootMembershipProof)
  , pstep02Args'policyIndex :: Term s (PAsData PInteger)
  , pstep02Args'direction :: Term s (PAsData PInteger)
  , pstep02Args'mintOpening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03State s = PStep03State
  { pstep03State'policyId :: Term s (PAsData PByteString)
  , pstep03State'direction :: Term s (PAsData PInteger)
  , pstep03State'badTxId :: Term s (PAsData PByteString)
  , pstep03State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  , pstep03State'validityIntervalStart :: Term s (PAsData PInteger)
  , pstep03State'validityIntervalEnd :: Term s (PAsData PInteger)
  , pstep03State'priorLedgerRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Args s
  = PWitnessAbsence
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
  | PEvaluateUnsatisfied
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PFieldOpeningV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04State s = PStep04State
  { pstep04State'policyId :: Term s (PAsData PByteString)
  , pstep04State'badTxId :: Term s (PAsData PByteString)
  , pstep04State'priorLedgerRoot :: Term s (PAsData PByteString)
  , pstep04State'refCursor :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

data PStep04Args s
  = PResolveNext
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PProof))
  | PAdvanceComplete
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PStep05State s = PStep05State
  { pstep05State'policyId :: Term s (PAsData PByteString)
  , pstep05State'direction :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05State)

data PStep05Args s = PStep05Args
  { pstep05Args'inputIndex :: Term s (PAsData PInteger)
  , pstep05Args'outputIndex :: Term s (PAsData PInteger)
  , pstep05Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05Args)
