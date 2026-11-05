-- | Invalid-range source carriage, authenticated subject state and decisive rule.
module Midgard.FraudProofs.InvalidRange (
  PStep01Source (..),
  PStep01Args (..),
  PStep02State (..),
  prangeExcludesSlot,
  pterminalContradiction,
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Prelude

import DesignPatterns.ValidityRangeNormalization (PNormalizedTimeRange (..))
import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof)

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

{- | Aiken @invalid_range/step_02.State@.

The block's replay slot and the transaction's normalised range. The slot is read
off the header in step-01, which is the only step that authenticated a header —
the same reason the counted commitment travels in
'Midgard.FraudProofs.WithdrawnReferenceInput'.
-}
data PStep02State (s :: S) = PStep02State
  { pstep02State'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pstep02State'blockSlot :: Term s (PAsData PInteger)
  , pstep02State'badTxNormalizedValidityRange ::
      Term s (PAsData PNormalizedTimeRange)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @invalid_range/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

-- | InvalidRange is a malformed interval, not an exclusion claim.
prangeExcludesSlot :: forall s. Term s (PNormalizedTimeRange :--> PInteger :--> PBool)
prangeExcludesSlot = phoistAcyclic $ plam $ \range slot ->
  pmatch range $ \case
    PClosedRange {pntr'lower, pntr'upper} -> pfromData pntr'lower #> slot #|| pfromData pntr'upper #< slot
    PFromNegInf {pntr'upperOnly} -> pfromData pntr'upperOnly #< slot
    PToPosInf {pntr'lowerOnly} -> pfromData pntr'lowerOnly #> slot
    PAlways -> pconstant False
    PInvalidRange -> pconstant False

pterminalContradiction :: forall s. Term s (Subject.PVerdictSubject :--> PNormalizedTimeRange :--> PInteger :--> PBool)
pterminalContradiction = phoistAcyclic $ plam $ \subject range slot ->
  pmatch subject $ \Subject.PVerdictSubject {Subject.psubject'direction} ->
    let malformed = range #== pcon PInvalidRange
        excludes = prangeExcludesSlot # range # slot
        exact reason fault = plet (Subject.pbindExactRejectionReason # subject # pcon reason) $ \_ -> fault
        fault =
          pif
            (pfromData psubject'direction #== 0)
            (malformed #|| excludes)
            ( pmatch (Subject.prejectionReasonOf # subject) $ \case
                PValidityIntervalMalformed -> exact PValidityIntervalMalformed malformed
                PValidityIntervalExcludesBlockSlot -> exact PValidityIntervalExcludesBlockSlot excludes
                _ -> perror
            )
     in Subject.pterminalContradiction # subject # fault
