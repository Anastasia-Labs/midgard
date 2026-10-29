-- | Exact spend-input signer fault rule and five-step Data ABI.
module Midgard.FraudProofs.SpendInputSignerMissing (
  PBoundSpendInputV1 (..),
  PAuthenticatedCredentialV1 (..),
  PWitnessScanV1 (..),
  PVerdictV1 (..),
  PSourceV1 (..),
  PStep01Args (..),
  PStep02Args (..),
  PStep03Args (..),
  PStep04Args (..),
  PStep05Args (..),
  pwitnessScanBatchSize,
  pbindSpendInputV1,
  pauthenticateCredentialV1,
  pscanVerdictV1,
  pdirectVerdictV1,
  pterminalV1,
  pencodeBoundV1,
  pencodeAuthenticatedV1,
  pencodeScanV1,
  pencodeVerdictV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PMembershipCarriage, PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PSpendInputSignerMissing))
import Midgard.TransitionTrace (PRootMembershipProof)

data PBoundSpendInputV1 (s :: S) = PBoundSpendInputV1
  { pboundSpendInput'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundSpendInput'inputIndex :: Term s (PAsData PInteger)
  , pboundSpendInput'priorRoot :: Term s (PAsData PByteString)
  , pboundSpendInput'witnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundSpendInputV1)

data PAuthenticatedCredentialV1 (s :: S) = PAuthenticatedCredentialV1
  { pauthenticatedCredential'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pauthenticatedCredential'transactionId :: Term s (PAsData PByteString)
  , pauthenticatedCredential'witnessSetHash :: Term s (PAsData PByteString)
  , pauthenticatedCredential'paymentCredential :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedCredentialV1)

data PWitnessScanV1 (s :: S) = PWitnessScanV1
  { pwitnessScan'authenticated :: Term s (PAsData PAuthenticatedCredentialV1)
  , pwitnessScan'checkpointHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWitnessScanV1)

data PVerdictV1 (s :: S) = PVerdictV1
  { pverdict'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pverdict'signerRequired :: Term s (PAsData PBool)
  , pverdict'signerMissing :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVerdictV1)

data PSourceV1 (s :: S)
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PHeaderV1))
      (Term s (PAsData PRootMembershipProof))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSourceV1)

data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'source :: Term s (PAsData PSourceV1)
  , pstep01Args'inputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'spendInputsOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep02Args'descriptorCbor :: Term s (PAsData PByteString)
  , pstep02Args'membership :: Term s (PAsData PMembershipCarriage)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'witnessesOpening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'witnessesOpening :: Term s (PAsData PFieldOpeningV1)
  , pstep04Args'checkpointCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PStep05Args (s :: S) = PStep05Args
  { pstep05Args'inputIndex :: Term s (PAsData PInteger)
  , pstep05Args'outputIndex :: Term s (PAsData PInteger)
  , pstep05Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05Args)

pwitnessScanBatchSize :: forall s. Term s PInteger
pwitnessScanBatchSize = 16

pbindSpendInputV1 :: forall s. Term s (Subject.PVerdictSubject :--> PInteger :--> PByteString :--> PByteString :--> PBoundSpendInputV1)
pbindSpendInputV1 = phoistAcyclic $ plam $ \subject inputIndex priorRoot witnessSetHash ->
  pif
    ( Subject.psubjectIsCanonical
        # subject
        #&& inputIndex
        #>= 0
        #&& plengthBS
        # priorRoot
        #== 32
        #&& plengthBS
        # witnessSetHash
        #== 32
    )
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        pif
          (pfromData psubject'direction #== 1)
          ( plet
              (Subject.pbindExactRejectionReason # subject # pcon (PSpendInputSignerMissing $ pdata inputIndex))
              (\_ -> pcon $ PBoundSpendInputV1 (pdata subject) (pdata inputIndex) (pdata priorRoot) (pdata witnessSetHash))
          )
          (pcon $ PBoundSpendInputV1 (pdata subject) (pdata inputIndex) (pdata priorRoot) (pdata witnessSetHash))
    )
    perror

pauthenticateCredentialV1 :: forall s. Term s (PBoundSpendInputV1 :--> PByteString :--> PAuthenticatedCredentialV1)
pauthenticateCredentialV1 = phoistAcyclic $ plam $ \bound paymentCredential ->
  pmatch bound $ \PBoundSpendInputV1{pboundSpendInput'subject, pboundSpendInput'witnessSetHash} ->
    pmatch (pfromData pboundSpendInput'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} ->
      pif
        (plengthBS # paymentCredential #== 28)
        (pcon $ PAuthenticatedCredentialV1 pboundSpendInput'subject psubject'transactionId pboundSpendInput'witnessSetHash (pdata paymentCredential))
        perror

pscanVerdictV1 :: forall s. Term s (Subject.PVerdictSubject :--> PBool :--> PVerdictV1)
pscanVerdictV1 = phoistAcyclic $ plam $ \subject signerMissing ->
  pcon $ PVerdictV1 (pdata subject) (pdata $ pconstant True) (pdata signerMissing)

pdirectVerdictV1 :: forall s. Term s (PBoundSpendInputV1 :--> PVerdictV1)
pdirectVerdictV1 = phoistAcyclic $ plam $ \bound ->
  pmatch bound $ \PBoundSpendInputV1{pboundSpendInput'subject} ->
    pmatch (pfromData pboundSpendInput'subject) $ \subject@Subject.PVerdictSubject{Subject.psubject'direction} ->
      pif
        (pfromData psubject'direction #== 1)
        (pcon $ PVerdictV1 (pdata $ pcon subject) (pdata $ pconstant False) (pdata $ pconstant False))
        perror

pterminalV1 :: forall s. Term s (PVerdictV1 :--> PBool)
pterminalV1 = phoistAcyclic $ plam $ \verdict ->
  pmatch verdict $ \PVerdictV1{..} ->
    Subject.pterminalContradiction
      # pfromData pverdict'subject
      # (pfromData pverdict'signerRequired #&& pfromData pverdict'signerMissing)

pencodeBoundV1 :: forall s. Term s (PBoundSpendInputV1 :--> PByteString)
pencodeBoundV1 = phoistAcyclic $ plam $ \value -> pserialiseData # pforgetData (pdata value)

pencodeAuthenticatedV1 :: forall s. Term s (PAuthenticatedCredentialV1 :--> PByteString)
pencodeAuthenticatedV1 = phoistAcyclic $ plam $ \value -> pserialiseData # pforgetData (pdata value)

pencodeScanV1 :: forall s. Term s (PWitnessScanV1 :--> PByteString)
pencodeScanV1 = phoistAcyclic $ plam $ \value -> pserialiseData # pforgetData (pdata value)

pencodeVerdictV1 :: forall s. Term s (PVerdictV1 :--> PByteString)
pencodeVerdictV1 = phoistAcyclic $ plam $ \value -> pserialiseData # pforgetData (pdata value)
