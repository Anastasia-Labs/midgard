{-# LANGUAGE OverloadedStrings #-}

-- | Authenticated field-length evidence and terminal rule.
module Midgard.FraudProofs.FieldPreimageLengthMismatch (
  PStep01Args (..),
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep03Args (..),
  PLengthEvidenceV1 (..),
  pfieldPreimageLengthAtV1,
  pauthenticatedLengthEvidenceV1,
  pbindForcedLengthEvidenceV1,
  pdecisiveFaultHoldsV1,
  pterminalContradictionV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTxInInfo)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.CanonicalDecodability (PCommittedFieldClaimV1 (..))
import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (pfirstWitnessSetFieldIndex)
import Midgard.FraudProofs.NativeTx.Compact (pdecodeNativeTxFieldPreimageLengthsV1)
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact,
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.NativeTxFieldAccess (
  pauthenticatedCommittedPreimage,
  pfieldCount,
  pmaxTransactionAggregateFieldBytes,
 )
import Midgard.RejectionReason (PRejectionReasonV1 (PFieldPreimageLengthMismatch))
import Midgard.TransitionTrace (PRootMembershipProof)

data PStep01Args (s :: S)
  = PBindAccepted
      (Term s (PAsData PNativeTxInclusionCarriage))
      (Term s (PAsData PCommittedFieldClaimV1))
  | PRecordForced
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State (s :: S)
  = PBoundSource
      (Term s (PAsData Subject.PVerdictSubject))
      (Term s (PAsData PByteString))
  | PPendingForced (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args (s :: S)
  = PAuthenticateAccepted
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PCommittedFieldClaimV1))
  | PAuthenticateForced
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PHeaderV1))
      (Term s (PAsData PRootMembershipProof))
      (Term s (PAsData PCommittedFieldClaimV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03State (s :: S) = PStep03State
  { pstep03State'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pstep03State'fieldIndex :: Term s (PAsData PInteger)
  , pstep03State'declaredLength :: Term s (PAsData PInteger)
  , pstep03State'actualLength :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PLengthEvidenceV1 (s :: S) = PLengthEvidenceV1
  { plengthEvidence'fieldIndex :: Term s (PAsData PInteger)
  , plengthEvidence'declaredLength :: Term s (PAsData PInteger)
  , plengthEvidence'actualLength :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLengthEvidenceV1)

{- | The target's positional nine-field vector, including its script/address
record-order wrinkle at coordinates six and seven.
-}
pfieldPreimageLengthAtV1 :: forall s. Term s (PNativeTxFieldPreimageLengthsV1 :--> PInteger :--> PInteger)
pfieldPreimageLengthAtV1 = phoistAcyclic $ plam $ \lengths fieldIndex ->
  pmatch lengths $ \PNativeTxFieldPreimageLengthsV1{..} ->
    pif (fieldIndex #< 0) perror $
      pif (fieldIndex #== 0) plengths'spendInputs $
        pif (fieldIndex #== 1) plengths'referenceInputs $
          pif (fieldIndex #== 2) plengths'outputs $
            pif (fieldIndex #== 3) plengths'requiredObservers $
              pif (fieldIndex #== 4) plengths'requiredSigners $
                pif (fieldIndex #== 5) plengths'mint $
                  pif (fieldIndex #== 6) plengths'scriptWitnesses $
                    pif (fieldIndex #== 7) plengths'addressWitnesses $
                      pif (fieldIndex #== 8) plengths'redeemers perror

{- | Authenticate exactly one positional field preimage and derive its two
lengths. No item grammar is imposed before the mismatch decision.
-}
pauthenticatedLengthEvidenceV1 ::
  forall s.
  Term
    s
    ( PVerifiedMidgardNativeTxCompact
        :--> PByteString
        :--> PCommittedFieldClaimV1
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PLengthEvidenceV1
    )
pauthenticatedLengthEvidenceV1 = phoistAcyclic $ plam $ \verified lengthsCbor claim referenceInputs certificatePolicy -> P.do
  lengths <- plet $ pdecodeNativeTxFieldPreimageLengthsV1 # lengthsCbor
  PPair fieldIndex preimage <- pmatch $
    pmatch claim $ \case
      PBodyFieldClaim fieldIndexD carriageD ->
        pif
          (pfromData fieldIndexD #< pfirstWitnessSetFieldIndex)
          ( pcon $
              PPair
                (pfromData fieldIndexD)
                ( pauthenticatedCommittedPreimage
                    # verified
                    # punreadWitnessSet
                    # pfromData fieldIndexD
                    # pfromData carriageD
                    # referenceInputs
                    # certificatePolicy
                )
          )
          perror
      PWitnessFieldClaim fieldIndexD witnessSetD carriageD ->
        pif
          (pfromData fieldIndexD #>= pfirstWitnessSetFieldIndex)
          ( pcon $
              PPair
                (pfromData fieldIndexD)
                ( pauthenticatedCommittedPreimage
                    # verified
                    # pfromData witnessSetD
                    # pfromData fieldIndexD
                    # pfromData carriageD
                    # referenceInputs
                    # certificatePolicy
                )
          )
          perror
  actualLength <- plet $ plengthBS # preimage
  pif
    (fieldIndex #< pfieldCount #&& actualLength #<= pmaxTransactionAggregateFieldBytes)
    ( pcon $
        PLengthEvidenceV1
          (pdata fieldIndex)
          (pdata $ pfieldPreimageLengthAtV1 # lengths # fieldIndex)
          (pdata actualLength)
    )
    perror

pdecisiveFaultHoldsV1 :: forall s. Term s (PInteger :--> PInteger :--> PInteger :--> PBool)
pdecisiveFaultHoldsV1 = phoistAcyclic $ plam $ \fieldIndex declaredLength actualLength ->
  pif
    ( fieldIndex
        #>= 0
        #&& fieldIndex
        #< pfieldCount
        #&& declaredLength
        #>= 0
        #&& actualLength
        #>= 0
        #&& actualLength
        #<= pmaxTransactionAggregateFieldBytes
    )
    (pnot # (declaredLength #== actualLength))
    perror

pbindForcedLengthEvidenceV1 :: forall s. Term s (Subject.PVerdictSubject :--> PLengthEvidenceV1 :--> PLengthEvidenceV1)
pbindForcedLengthEvidenceV1 = phoistAcyclic $ plam $ \subject evidence ->
  pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
    pmatch evidence $ \PLengthEvidenceV1{plengthEvidence'fieldIndex} ->
      pif
        (pfromData psubject'direction #== 1)
        ( plet
            ( Subject.pbindExactRejectionReason
                # subject
                # pcon (PFieldPreimageLengthMismatch plengthEvidence'fieldIndex)
            )
            $ \_ -> evidence
        )
        (pif (pfromData psubject'direction #== 0) evidence perror)

pterminalContradictionV1 :: forall s. Term s (PStep03State :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PStep03State{..} ->
    Subject.pterminalContradiction
      # pfromData pstep03State'subject
      # ( pdecisiveFaultHoldsV1
            # pfromData pstep03State'fieldIndex
            # pfromData pstep03State'declaredLength
            # pfromData pstep03State'actualLength
        )

punreadWitnessSet :: forall s. Term s PNativeTxWitnessSetCompact
punreadWitnessSet =
  pcon $
    PNativeTxWitnessSetCompact
      (pdata $ pconstant "")
      (pdata $ pconstant "")
      (pdata $ pconstant "")
