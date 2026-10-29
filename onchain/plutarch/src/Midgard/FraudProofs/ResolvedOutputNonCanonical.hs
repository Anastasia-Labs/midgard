-- | Exact prior-ledger output canonicity rule and five-step Data ABI.
module Midgard.FraudProofs.ResolvedOutputNonCanonical (
  PBoundInputV1 (..),
  PAuthenticatedOutRefV1 (..),
  PCanonicalVerdictV1 (..),
  PReconstructionV1 (..),
  PSourceV1 (..),
  PStep01Args (..),
  PStep02Args (..),
  PStep03Args (..),
  PActionV1 (..),
  PStep04Args (..),
  PStep05Args (..),
  pbindInputV1,
  pauthenticateOutRefV1,
  pvalidChunkV1,
  padvanceReconstructionV1,
  pfinalizeCanonicalV1,
  pterminalContradictionV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PTxOutRef (..))
import Plutarch.Prelude

import Midgard.BoundedItem (PChunkProofV1 (..), pchunkBytes, pchunkCount)
import Midgard.FraudProofs.Common (PMembershipCarriage, PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pverifyOutputChunk)
import Midgard.LedgerOutputScan (PLedgerOutputScanControlV1, pfinishV1, pstepV1, pterminalIsExactV1)
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PInputSpentOutputNonCanonical))
import Midgard.TransitionTrace (PRootMembershipProof)

data PBoundInputV1 (s :: S) = PBoundInputV1
  { pboundInput'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundInput'sourceKind :: Term s (PAsData PInteger)
  , pboundInput'inputIndex :: Term s (PAsData PInteger)
  , pboundInput'priorRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundInputV1)

data PAuthenticatedOutRefV1 (s :: S) = PAuthenticatedOutRefV1
  { pauthenticatedOutRef'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pauthenticatedOutRef'priorRoot :: Term s (PAsData PByteString)
  , pauthenticatedOutRef'outRef :: Term s (PAsData PTxOutRef)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedOutRefV1)

data PCanonicalVerdictV1 (s :: S) = PCanonicalVerdictV1
  { pcanonicalVerdict'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pcanonicalVerdict'outputIsNonCanonical :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalVerdictV1)

data PReconstructionV1 (s :: S) = PReconstructionV1
  { preconstruction'subject :: Term s (PAsData Subject.PVerdictSubject)
  , preconstruction'descriptorCbor :: Term s (PAsData PByteString)
  , preconstruction'control :: Term s (PAsData PLedgerOutputScanControlV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PReconstructionV1)

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
  , pstep01Args'sourceKind :: Term s (PAsData PInteger)
  , pstep01Args'inputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'opening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'descriptorCbor :: Term s (PAsData PByteString)
  , pstep03Args'membership :: Term s (PAsData PMembershipCarriage)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PActionV1 (s :: S)
  = PAdvance
      (Term s (PAsData PChunkProofV1))
      (Term s (PMaybeData PChunkProofV1))
  | PFinalizeCanonical
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PActionV1)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'action :: Term s (PAsData PActionV1)
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

pbindInputV1 :: forall s. Term s (Subject.PVerdictSubject :--> PInteger :--> PInteger :--> PByteString :--> PBoundInputV1)
pbindInputV1 = phoistAcyclic $ plam $ \subject sourceKind inputIndex priorRoot ->
  pif
    ( Subject.psubjectIsCanonical
        # subject
        #&& (sourceKind #== 0 #|| sourceKind #== 1)
        #&& inputIndex
        #>= 0
        #&& plengthBS
        # priorRoot
        #== 32
    )
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        pif
          (pfromData psubject'direction #== 1)
          ( plet
              ( Subject.pbindExactRejectionReason
                  # subject
                  # pcon (PInputSpentOutputNonCanonical (pdata sourceKind) (pdata inputIndex))
              )
              (\_ -> pcon $ PBoundInputV1 (pdata subject) (pdata sourceKind) (pdata inputIndex) (pdata priorRoot))
          )
          (pcon $ PBoundInputV1 (pdata subject) (pdata sourceKind) (pdata inputIndex) (pdata priorRoot))
    )
    perror

pauthenticateOutRefV1 :: forall s. Term s (PBoundInputV1 :--> PTxOutRef :--> PAuthenticatedOutRefV1)
pauthenticateOutRefV1 = phoistAcyclic $ plam $ \bound outRef ->
  pmatch bound $ \PBoundInputV1{pboundInput'subject, pboundInput'priorRoot} ->
    pmatch outRef $ \PTxOutRef{ptxOutRef'id, ptxOutRef'idx} ->
      pif
        (plengthBS # pto (pfromData ptxOutRef'id) #== 32 #&& pfromData ptxOutRef'idx #>= 0)
        (pcon $ PAuthenticatedOutRefV1 pboundInput'subject pboundInput'priorRoot (pdata outRef))
        perror

pvalidChunkV1 :: forall s. Term s (PLedgerOutputCommitmentV1 :--> PChunkProofV1 :--> PBool)
pvalidChunkV1 = pverifyOutputChunk

padvanceReconstructionV1 :: forall s. Term s (PLedgerOutputCommitmentV1 :--> PLedgerOutputScanControlV1 :--> PChunkProofV1 :--> PMaybeData PChunkProofV1 :--> PMaybe PLedgerOutputScanControlV1)
padvanceReconstructionV1 = phoistAcyclic $ plam $ \descriptor control chunkProof nextChunkProof ->
  pmatch descriptor $ \PLedgerOutputCommitmentV1{poutputCommitment'totalLength} ->
    pmatch chunkProof $ \PChunkProofV1{pchunkProof'chunkIndex, pchunkProof'chunk} ->
      pmatch control $ \scan ->
        let totalLength = pfromData poutputCommitment'totalLength
            chunkIndex = pfromData pchunkProof'chunkIndex
            expectedChunkIndex = pdiv # pfromData (Scan.pscan'cursor scan) # pchunkBytes
            hasNext = chunkIndex + 1 #< pchunkCount # totalLength
         in pif
              ( pmatch (pfinishV1 # pcon scan # totalLength) $ \case
                  PNothing -> pconstant True
                  PJust _ -> pconstant False
              )
              ( pif
                  (pvalidChunkV1 # descriptor # chunkProof #&& chunkIndex #== expectedChunkIndex)
                  ( pmatch nextChunkProof $ \case
                      PDNothing ->
                        pif
                          (pnot # hasNext)
                          (pstepV1 # pcon scan # totalLength # pfromData pchunkProof'chunk # (pfromData (Scan.pscan'cursor scan) - chunkIndex * pchunkBytes))
                          perror
                      PDJust nextProofData ->
                        pmatch (pfromData nextProofData) $ \nextProof@PChunkProofV1{pchunkProof'chunkIndex = nextIndex, pchunkProof'chunk = nextChunk} ->
                          pif
                            (hasNext #&& pfromData nextIndex #== chunkIndex + 1 #&& pvalidChunkV1 # descriptor # pcon nextProof)
                            ( pstepV1
                                # pcon scan
                                # totalLength
                                # (pfromData pchunkProof'chunk <> pfromData nextChunk)
                                # (pfromData (Scan.pscan'cursor scan) - chunkIndex * pchunkBytes)
                            )
                            perror
                  )
                  perror
              )
              perror

pfinalizeCanonicalV1 :: forall s. Term s (PLedgerOutputCommitmentV1 :--> PLedgerOutputScanControlV1 :--> PBool)
pfinalizeCanonicalV1 = phoistAcyclic $ plam $ \descriptor control ->
  pmatch descriptor $ \PLedgerOutputCommitmentV1{poutputCommitment'totalLength} ->
    pmatch (pfinishV1 # control # pfromData poutputCommitment'totalLength) $ \case
      PNothing -> pconstant False
      PJust terminal -> pterminalIsExactV1 # terminal # pfromData poutputCommitment'totalLength

pterminalContradictionV1 :: forall s. Term s (PCanonicalVerdictV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \verdict ->
  pmatch verdict $ \PCanonicalVerdictV1{..} ->
    Subject.pterminalContradiction
      # pfromData pcanonicalVerdict'subject
      # pfromData pcanonicalVerdict'outputIsNonCanonical
