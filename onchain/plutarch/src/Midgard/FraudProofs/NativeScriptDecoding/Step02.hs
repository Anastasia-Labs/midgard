module Midgard.FraudProofs.NativeScriptDecoding.Step02 (PStep02Args (..)) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.Prelude

import Midgard.LedgerState (PHeaderV1)
import Midgard.TransitionTrace (PRootMembershipProof)

data PStep02Args s = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'header :: Term s (PAsData PHeaderV1)
  , pstep02Args'eventToStepMembership :: Term s (PAsData PRootMembershipProof)
  , pstep02Args'transitionStepMembership :: Term s (PAsData PRootMembershipProof)
  , pstep02Args'forcedMembership :: Term s (PAsData (PMaybeData PRootMembershipProof))
  , pstep02Args'chosenOutpointSourceKind :: Term s (PAsData PInteger)
  , pstep02Args'chosenOutpointCursor :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)
