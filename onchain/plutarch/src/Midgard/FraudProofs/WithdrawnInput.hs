module Midgard.FraudProofs.WithdrawnInput (
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep03Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput)
import Midgard.TransitionTrace (PRootMembershipProof)

data PStep02State s = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'blocksWithdrawalsRoot :: Term s (PAsData PByteString)
  , pstep02State'blocksWithdrawalCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args s = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'spendInputsOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep02Args'badInputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03State s = PStep03State
  { pstep03State'withdrawnInput :: Term s (PAsData PMidgardTxInput)
  , pstep03State'blocksWithdrawalsRoot :: Term s (PAsData PByteString)
  , pstep03State'blocksWithdrawalCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Args s = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , pstep03Args'withdrawalMembership :: Term s (PAsData PRootMembershipProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)
