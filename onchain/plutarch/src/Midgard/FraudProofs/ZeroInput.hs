-- | Zero-input source carriage, verdict-subject state and decisive rule.
module Midgard.FraudProofs.ZeroInput (
  PStep01Source (..),
  PStep01Args (..),
  PStep02State (..),
  pbindState,
  pterminalContradiction,
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PEmptyInputs))
import Midgard.TransitionTrace (PRootMembershipProof)

-- | Aiken @zero_input/step_01.SourceV1@ and @Args@.
data PStep01Source (s :: S)
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PHeaderV1))
      (Term s (PAsData PRootMembershipProof))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Source)

newtype PStep01Args (s :: S) = PStep01Args (Term s (PAsData PStep01Source))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

-- | Aiken @zero_input/rule.StateV1@.
newtype PStep02State (s :: S) = PStep02State
  {pstep02State'subject :: Term s (PAsData Subject.PVerdictSubject)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

pbindState :: forall s. Term s (Subject.PVerdictSubject :--> PStep02State)
pbindState = phoistAcyclic $ plam $ \subject ->
  pif
    (Subject.psubjectIsCanonical # subject)
    ( pmatch subject $ \Subject.PVerdictSubject {Subject.psubject'direction} ->
        let state = pcon $ PStep02State $ pdata subject
         in pif
              (pfromData psubject'direction #== 1)
              (plet (Subject.pbindExactRejectionReason # subject # pcon PEmptyInputs) $ \_ -> state)
              state
    )
    perror

pterminalContradiction :: forall s. Term s (PStep02State :--> PInteger :--> PBool)
pterminalContradiction = phoistAcyclic $ plam $ \state count ->
  pif
    (count #>= 0)
    ( pmatch state $ \PStep02State {pstep02State'subject} ->
        Subject.pterminalContradiction # pfromData pstep02State'subject # (count #== 0)
    )
    perror

-- | Aiken @zero_input/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , pstep02Args'spendInputsOpening :: Term s (PAsData PFieldOpeningV1)
  {- ^ The prover's chosen §8 carriage for field 0's preimage — for a
  genuinely empty field, the single byte @80@.
  -}
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)
