module Midgard.FraudProofs.NativeScriptInvalid (
  PStep01Source (..),
  PStep01Args (..),
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep03Args (..),
  PStep04State (..),
  PStep04Args (..),
  PStep05PhaseV1 (..),
  PStep05State (..),
  PSignerQueryV1 (..),
  PStep05Args (..),
  pdirectSignerLimit,
  pdirectScriptBytesLimit,
  pstagedSignerStartBatchLimit,
  pstagedSignerResumeBatchLimit,
  pstagedSignerFinalizeBatchLimit,
  pbindScriptIndex,
  pauthenticatedSigner,
  pstagedNodeBatchLimit,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_224, pverifyEd25519Signature)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardAddressWitnessCbor, pencodeMidgardAddressWitness)
import Midgard.FraudProofs.NativeTx.Types (PMidgardAddressWitness (..))
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.NativeTxScriptPushdown (PNativeScriptFrameV1)
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof)
import Midgard.ValidationMachine (PSignerSetProofV1)
import Midgard.ValidationMerkle (PFrontierPeak)
import Midgard.Validators.FraudProofs.Step (pexpecting)

data PStep01Source s
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

newtype PStep01Args s = PStep01Args
  {pstep01Args'source :: Term s (PAsData PStep01Source)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State s = PStep02State
  { pstep02State'subject :: Term s (PAsData PVerdictSubject)
  , pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  , pstep02State'validityIntervalStart :: Term s (PAsData PInteger)
  , pstep02State'validityIntervalEnd :: Term s (PAsData PInteger)
  , pstep02State'grammarCheckpointHash :: Term s (PAsData PByteString)
  , pstep02State'grammarComplete :: Term s (PAsData PBool)
  , pstep02State'scriptCheckpointHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args s
  = PStep02Args
      { pstep02Args'inputIndex :: Term s (PAsData PInteger)
      , pstep02Args'outputIndex :: Term s (PAsData PInteger)
      , pstep02Args'scriptIndex :: Term s (PAsData PInteger)
      , pstep02Args'scriptTxWitsOpening :: Term s (PAsData PFieldOpeningV1)
      }
  | PCertifyScriptField
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  | PSelectCertifiedScript
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03State s = PStep03State
  { pstep03State'subject :: Term s (PAsData PVerdictSubject)
  , pstep03State'badTxId :: Term s (PAsData PByteString)
  , pstep03State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  , pstep03State'scriptItemHash :: Term s (PAsData PByteString)
  , pstep03State'validityIntervalStart :: Term s (PAsData PInteger)
  , pstep03State'validityIntervalEnd :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Args s
  = PDirectFinalize
      { pdirectFinalize'inputIndex :: Term s (PAsData PInteger)
      , pdirectFinalize'outputIndex :: Term s (PAsData PInteger)
      , pdirectFinalize'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
      , pdirectFinalize'scriptItemCbor :: Term s (PAsData PByteString)
      , pdirectFinalize'addressWitnessesOpening :: Term s (PAsData PFieldOpeningV1)
      }
  | PStartSignerScan
      { pstartSignerScan'inputIndex :: Term s (PAsData PInteger)
      , pstartSignerScan'outputIndex :: Term s (PAsData PInteger)
      , pstartSignerScan'scriptItemCbor :: Term s (PAsData PByteString)
      , pstartSignerScan'addressWitnessesOpening :: Term s (PAsData PFieldOpeningV1)
      , pstartSignerScan'itemBudget :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04State s = PStep04State
  { pstep04State'subject :: Term s (PAsData PVerdictSubject)
  , pstep04State'badTxId :: Term s (PAsData PByteString)
  , pstep04State'badTxWitnessSetHash :: Term s (PAsData PByteString)
  , pstep04State'scriptItemHash :: Term s (PAsData PByteString)
  , pstep04State'validityIntervalStart :: Term s (PAsData PInteger)
  , pstep04State'validityIntervalEnd :: Term s (PAsData PInteger)
  , pstep04State'signerCheckpointHash :: Term s (PAsData PByteString)
  , pstep04State'previousSignerHash :: Term s (PAsData PByteString)
  , pstep04State'signerCount :: Term s (PAsData PInteger)
  , pstep04State'signerPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

data PStep04Args s
  = PResumeSignerScan
      { presumeSignerScan'inputIndex :: Term s (PAsData PInteger)
      , presumeSignerScan'outputIndex :: Term s (PAsData PInteger)
      , presumeSignerScan'addressWitnessesOpening :: Term s (PAsData PFieldOpeningV1)
      , presumeSignerScan'checkpointBytes :: Term s (PAsData PByteString)
      , presumeSignerScan'itemBudget :: Term s (PAsData PInteger)
      }
  | PFinalizeSignerScan
      { pfinalizeSignerScan'inputIndex :: Term s (PAsData PInteger)
      , pfinalizeSignerScan'outputIndex :: Term s (PAsData PInteger)
      , pfinalizeSignerScan'addressWitnessesOpening :: Term s (PAsData PFieldOpeningV1)
      , pfinalizeSignerScan'checkpointBytes :: Term s (PAsData PByteString)
      , pfinalizeSignerScan'itemBudget :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PStep05PhaseV1 s
  = PScriptReady
  | PScriptWalk {pscriptWalk'cursorHash :: Term s (PAsData PByteString)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05PhaseV1)

data PStep05State s = PStep05State
  { pstep05State'subject :: Term s (PAsData PVerdictSubject)
  , pstep05State'badTxId :: Term s (PAsData PByteString)
  , pstep05State'scriptItemHash :: Term s (PAsData PByteString)
  , pstep05State'validityIntervalStart :: Term s (PAsData PInteger)
  , pstep05State'validityIntervalEnd :: Term s (PAsData PInteger)
  , pstep05State'signerCount :: Term s (PAsData PInteger)
  , pstep05State'signerPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pstep05State'phase :: Term s (PAsData PStep05PhaseV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05State)

data PSignerQueryV1 s = PSignerQueryV1
  { psignerQuery'signerHash :: Term s (PAsData PByteString)
  , psignerQuery'proof :: Term s (PAsData PSignerSetProofV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSignerQueryV1)

data PStep05Args s
  = PStartScriptScan
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PSignerQueryV1))))
  | PResumeScriptScan
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PNativeScriptFrameV1))))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PSignerQueryV1))))
  | PStartScriptFinalize
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PSignerQueryV1))))
  | PFinalizeScriptScan
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PNativeScriptFrameV1))))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PSignerQueryV1))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05Args)

pdirectSignerLimit, pdirectScriptBytesLimit, pstagedSignerStartBatchLimit, pstagedSignerResumeBatchLimit, pstagedSignerFinalizeBatchLimit, pstagedNodeBatchLimit :: forall s. Term s PInteger
pdirectSignerLimit = 28
pdirectScriptBytesLimit = 1024
pstagedSignerStartBatchLimit = 16
pstagedSignerResumeBatchLimit = 16
pstagedSignerFinalizeBatchLimit = 16
pstagedNodeBatchLimit = 16

pbindScriptIndex :: forall s. Term s (PVerdictSubject :--> PInteger :--> PBool)
pbindScriptIndex = phoistAcyclic $ plam $ \subject index ->
  pexpecting (Subject.psubjectIsCanonical # subject) $
    pexpecting (index #>= 0) $
      pmatch subject $ \PVerdictSubject {psubject'direction} ->
        pif
          (pfromData psubject'direction #== 1)
          ( pmatch (Subject.prejectionReasonOf # subject) $ \case
              PWitnessNativeScriptFalse committed -> index #== pfromData committed
              _ -> perror
          )
          (pconstant True)

pauthenticatedSigner :: forall s. Term s (PVerdictSubject :--> PByteString :--> PMaybe PByteString)
pauthenticatedSigner = phoistAcyclic $ plam $ \subject item -> P.do
  witness <- plet $ pdecodeMidgardAddressWitnessCbor # item
  PMidgardAddressWitness {paddressWitness'verificationKey, paddressWitness'signature} <- pmatch witness
  PVerdictSubject {psubject'direction, psubject'transactionId} <- pmatch subject
  pexpecting (pencodeMidgardAddressWitness # witness #== item) $
    pif
      ( pfromData psubject'direction
          #== 1
          #&& ( pnot
                  # ( pverifyEd25519Signature
                        # pfromData paddressWitness'verificationKey
                        # pfromData psubject'transactionId
                        # pfromData paddressWitness'signature
                    )
              )
      )
      (pcon PNothing)
      (pcon $ PJust $ pblake2b_224 # pfromData paddressWitness'verificationKey)
