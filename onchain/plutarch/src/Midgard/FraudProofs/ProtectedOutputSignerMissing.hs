-- | Exact protected-output signer fault rule and five-step Data ABI.
module Midgard.FraudProofs.ProtectedOutputSignerMissing (
  PBoundOutputV1 (..),
  PProtectedCredentialV1 (..),
  PWitnessScanV1 (..),
  PVerdictV1 (..),
  PStep01Source (..),
  PStep01Args (..),
  PStep02State (..),
  PStep02Args (..),
  PStep03Args (..),
  PStep04Args (..),
  PStep05Args (..),
  poutputsFieldIndex,
  paddressWitnessesFieldIndex,
  pwitnessScanBatchSize,
  pbindOutputV1,
  pscanVerdictV1,
  pdirectVerdictV1,
  pterminalV1,
  pencodeBoundV1,
  pencodeProtectedV1,
  pencodeScanV1,
  pencodeVerdictV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PProtectedOutputSignerMissing))
import Midgard.TransitionTrace (PRootMembershipProof)

data PBoundOutputV1 (s :: S) = PBoundOutputV1
  { pboundOutput'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundOutput'outputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundOutputV1)

data PProtectedCredentialV1 (s :: S) = PProtectedCredentialV1
  { pprotected'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pprotected'transactionId :: Term s (PAsData PByteString)
  , pprotected'witnessSetHash :: Term s (PAsData PByteString)
  , pprotected'outputIndex :: Term s (PAsData PInteger)
  , pprotected'paymentCredential :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProtectedCredentialV1)

data PWitnessScanV1 (s :: S) = PWitnessScanV1
  { pwitnessScan'protected :: Term s (PAsData PProtectedCredentialV1)
  , pwitnessScan'checkpointHash :: Term s (PAsData PByteString)
  , pwitnessScan'signerPresent :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWitnessScanV1)

data PVerdictV1 (s :: S) = PVerdictV1
  { pverdict'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pverdict'signerRequired :: Term s (PAsData PBool)
  , pverdict'signerPresent :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVerdictV1)

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

data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'source :: Term s (PAsData PStep01Source)
  , pstep01Args'outputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State (s :: S) = PStep02State
  { pstep02State'bound :: Term s (PAsData PBoundOutputV1)
  , pstep02State'witnessSetHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'opening :: Term s (PAsData PFieldOpeningV1)
  , pstep02Args'coordinateOutOfRange :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'opening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'opening :: Term s (PAsData PFieldOpeningV1)
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

poutputsFieldIndex, paddressWitnessesFieldIndex, pwitnessScanBatchSize :: forall s. Term s PInteger
poutputsFieldIndex = 2
paddressWitnessesFieldIndex = 7
pwitnessScanBatchSize = 32

pbindOutputV1 :: forall s. Term s (Subject.PVerdictSubject :--> PInteger :--> PBoundOutputV1)
pbindOutputV1 = phoistAcyclic $ plam $ \subject outputIndex ->
  pif
    (Subject.psubjectIsCanonical # subject #&& outputIndex #>= 0)
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        pif
          (pfromData psubject'direction #== 1)
          ( plet
              (Subject.pbindExactRejectionReason # subject # pcon (PProtectedOutputSignerMissing $ pdata outputIndex))
              (\_ -> pcon $ PBoundOutputV1 (pdata subject) (pdata outputIndex))
          )
          (pcon $ PBoundOutputV1 (pdata subject) (pdata outputIndex))
    )
    perror

pscanVerdictV1 :: forall s. Term s (Subject.PVerdictSubject :--> PBool :--> PVerdictV1)
pscanVerdictV1 = phoistAcyclic $ plam $ \subject signerPresent ->
  pcon $ PVerdictV1 (pdata subject) (pdata $ pconstant True) (pdata signerPresent)

pdirectVerdictV1 :: forall s. Term s (PBoundOutputV1 :--> PVerdictV1)
pdirectVerdictV1 = phoistAcyclic $ plam $ \bound ->
  pmatch bound $ \PBoundOutputV1{pboundOutput'subject} ->
    pmatch (pfromData pboundOutput'subject) $ \subject@Subject.PVerdictSubject{Subject.psubject'direction} ->
      pif
        (pfromData psubject'direction #== 1)
        (pcon $ PVerdictV1 (pdata $ pcon subject) (pdata $ pconstant False) (pdata $ pconstant False))
        perror

pterminalV1 :: forall s. Term s (PVerdictV1 :--> PBool)
pterminalV1 = phoistAcyclic $ plam $ \verdict ->
  pmatch verdict $ \PVerdictV1{..} ->
    Subject.pterminalContradiction
      # pfromData pverdict'subject
      # (pfromData pverdict'signerRequired #&& pnot # pfromData pverdict'signerPresent)

pencodeBoundV1 :: forall s. Term s (PBoundOutputV1 :--> PByteString)
pencodeBoundV1 = phoistAcyclic $ plam $ \value -> pserialiseData # pforgetData (pdata value)

pencodeProtectedV1 :: forall s. Term s (PProtectedCredentialV1 :--> PByteString)
pencodeProtectedV1 = phoistAcyclic $ plam $ \value -> pserialiseData # pforgetData (pdata value)

pencodeScanV1 :: forall s. Term s (PWitnessScanV1 :--> PByteString)
pencodeScanV1 = phoistAcyclic $ plam $ \value -> pserialiseData # pforgetData (pdata value)

pencodeVerdictV1 :: forall s. Term s (PVerdictV1 :--> PByteString)
pencodeVerdictV1 = phoistAcyclic $ plam $ \value -> pserialiseData # pforgetData (pdata value)
