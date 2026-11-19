module Midgard.FraudProofs.ExecutionNativeScriptInvalid.Types (
    PStep01Source (..),
    PStep01Args (..),
    PStep02Args (..),
    PAcceptedReconstructionInitArgs (..),
    PAcceptedSpendPrefixAction (..),
    PAcceptedMintPrefixAction (..),
    PAcceptedObserverPrefixAction (..),
    PAcceptedReceivePrefixAction (..),
    PAcceptedInlineSourceAction (..),
    PAcceptedReferenceSourceArgs (..),
    PExecutionNativeStep04State (..),
    PExecutionNativeStep03Args (..),
    PExecutionNativeStep03State (..),
    PExecutionNativeStep04Args (..),
    PExecutionNativeStep05State (..),
    PExecutionNativeStep05Args (..),
    PExecutionNativeStep06PhaseV1 (..),
    PExecutionNativeStep06State (..),
    PExecutionNativeSignerQueryV1 (..),
    PExecutionNativeStep06Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PMembershipCarriage, PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.LedgerState (PHeaderV1)
import Midgard.NativeTxScriptPushdown (PNativeScriptFrameV1)
import Midgard.TransitionTrace (PRootMembershipProof)
import Midgard.ValidationMachine (PNativeScriptsControlV1, PSignerSetProofV1)
import Midgard.ValidationMerkle (PFrontierPeak)
import Midgard.ValidationTrace (PValidationMachineStateV1, PValidationTraceProof)

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
    , pstep01Args'executionIndex :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02Args (s :: S) = PStep02Args
    { pstep02Args'inputIndex :: Term s (PAsData PInteger)
    , pstep02Args'outputIndex :: Term s (PAsData PInteger)
    , pstep02Args'traceMembership :: Term s (PAsData PRootMembershipProof)
    , pstep02Args'machineState :: Term s (PAsData PValidationMachineStateV1)
    , pstep02Args'traceProof :: Term s (PAsData PValidationTraceProof)
    , pstep02Args'control :: Term s (PAsData PNativeScriptsControlV1)
    , pstep02Args'purposeKind :: Term s (PAsData PInteger)
    , pstep02Args'purposeIndex :: Term s (PAsData PInteger)
    , pstep02Args'scriptHash :: Term s (PAsData PByteString)
    , pstep02Args'purposeSubject :: Term s (PAsData PByteString)
    , pstep02Args'purposeSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
    , pstep02Args'sourceIndex :: Term s (PAsData PInteger)
    , pstep02Args'originKind :: Term s (PAsData PInteger)
    , pstep02Args'sourceKey :: Term s (PAsData PByteString)
    , pstep02Args'languageTag :: Term s (PAsData PInteger)
    , pstep02Args'totalLength :: Term s (PAsData PInteger)
    , pstep02Args'itemCommitment :: Term s (PAsData PByteString)
    , pstep02Args'sourceSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
    , pstep02Args'redeemerLeaf :: Term s (PAsData PByteString)
    , pstep02Args'executionSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PAcceptedReconstructionInitArgs (s :: S) = PAcceptedReconstructionInitArgs
    { pacceptedInitArgs'inputIndex :: Term s (PAsData PInteger)
    , pacceptedInitArgs'outputIndex :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAcceptedReconstructionInitArgs)

data PAcceptedSpendPrefixAction (s :: S)
    = PScanSpend
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
        (Term s (PAsData PByteString))
        (Term s (PAsData PMembershipCarriage))
    | PFinishSpends
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAcceptedSpendPrefixAction)

data PAcceptedMintPrefixAction (s :: S)
    = PScanMint
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    | PFinishMint
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAcceptedMintPrefixAction)

data PAcceptedObserverPrefixAction (s :: S)
    = PScanObserver
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    | PFinishObservers
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAcceptedObserverPrefixAction)

data PAcceptedReceivePrefixAction (s :: S)
    = PScanOutput
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    | PFinishOutputPass
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAcceptedReceivePrefixAction)

data PAcceptedInlineSourceAction (s :: S)
    = PScanInline
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    | PFinishInline
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAcceptedInlineSourceAction)

data PAcceptedReferenceSourceArgs (s :: S) = PAcceptedReferenceSourceArgs
    { pacceptedReferenceArgs'inputIndex :: Term s (PAsData PInteger)
    , pacceptedReferenceArgs'outputIndex :: Term s (PAsData PInteger)
    , pacceptedReferenceArgs'referenceInputsOpening :: Term s (PAsData PFieldOpeningV1)
    , pacceptedReferenceArgs'descriptorCbor :: Term s (PAsData PByteString)
    , pacceptedReferenceArgs'membership :: Term s (PAsData PMembershipCarriage)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAcceptedReferenceSourceArgs)

data PExecutionNativeStep04State (s :: S) = PExecutionNativeStep04State
    { pexecutionStep04State'direction :: Term s (PAsData PInteger)
    , pexecutionStep04State'executionIndex :: Term s (PAsData PInteger)
    , pexecutionStep04State'sourceIndex :: Term s (PAsData PInteger)
    , pexecutionStep04State'originKind :: Term s (PAsData PInteger)
    , pexecutionStep04State'itemCommitment :: Term s (PAsData PByteString)
    , pexecutionStep04State'badTxId :: Term s (PAsData PByteString)
    , pexecutionStep04State'badTxWitnessSetHash :: Term s (PAsData PByteString)
    , pexecutionStep04State'scriptItemHash :: Term s (PAsData PByteString)
    , pexecutionStep04State'validityIntervalStart :: Term s (PAsData PInteger)
    , pexecutionStep04State'validityIntervalEnd :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionNativeStep04State)

data PExecutionNativeStep03Args (s :: S) = PExecutionNativeStep03Args
    { pexecutionStep03Args'inputIndex :: Term s (PAsData PInteger)
    , pexecutionStep03Args'outputIndex :: Term s (PAsData PInteger)
    , pexecutionStep03Args'scriptItemCbor :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionNativeStep03Args)

data PExecutionNativeStep03State (s :: S) = PExecutionNativeStep03State
    { pexecutionStep03State'direction :: Term s (PAsData PInteger)
    , pexecutionStep03State'executionIndex :: Term s (PAsData PInteger)
    , pexecutionStep03State'sourceIndex :: Term s (PAsData PInteger)
    , pexecutionStep03State'originKind :: Term s (PAsData PInteger)
    , pexecutionStep03State'itemCommitment :: Term s (PAsData PByteString)
    , pexecutionStep03State'badTxId :: Term s (PAsData PByteString)
    , pexecutionStep03State'badTxWitnessSetHash :: Term s (PAsData PByteString)
    , pexecutionStep03State'validityIntervalStart :: Term s (PAsData PInteger)
    , pexecutionStep03State'validityIntervalEnd :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionNativeStep03State)

data PExecutionNativeStep04Args (s :: S)
    = PExecutionNativeDirectFinalize
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PByteString))
        (Term s (PAsData PFieldOpeningV1))
    | PExecutionNativeStartSignerScan
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PByteString))
        (Term s (PAsData PFieldOpeningV1))
        (Term s (PAsData PInteger))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionNativeStep04Args)

data PExecutionNativeStep05State (s :: S) = PExecutionNativeStep05State
    { pexecutionStep05State'direction :: Term s (PAsData PInteger)
    , pexecutionStep05State'executionIndex :: Term s (PAsData PInteger)
    , pexecutionStep05State'sourceIndex :: Term s (PAsData PInteger)
    , pexecutionStep05State'originKind :: Term s (PAsData PInteger)
    , pexecutionStep05State'itemCommitment :: Term s (PAsData PByteString)
    , pexecutionStep05State'badTxId :: Term s (PAsData PByteString)
    , pexecutionStep05State'badTxWitnessSetHash :: Term s (PAsData PByteString)
    , pexecutionStep05State'scriptItemHash :: Term s (PAsData PByteString)
    , pexecutionStep05State'validityIntervalStart :: Term s (PAsData PInteger)
    , pexecutionStep05State'validityIntervalEnd :: Term s (PAsData PInteger)
    , pexecutionStep05State'signerCheckpointHash :: Term s (PAsData PByteString)
    , pexecutionStep05State'previousSignerHash :: Term s (PAsData PByteString)
    , pexecutionStep05State'signerCount :: Term s (PAsData PInteger)
    , pexecutionStep05State'signerPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionNativeStep05State)

data PExecutionNativeStep05Args (s :: S)
    = PExecutionNativeResumeSignerScan
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
        (Term s (PAsData PByteString))
        (Term s (PAsData PInteger))
    | PExecutionNativeFinalizeSignerScan
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
        (Term s (PAsData PByteString))
        (Term s (PAsData PInteger))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionNativeStep05Args)

data PExecutionNativeStep06PhaseV1 (s :: S)
    = PExecutionNativeScriptReady
    | PExecutionNativeScriptWalk (Term s (PAsData PByteString))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionNativeStep06PhaseV1)

data PExecutionNativeStep06State (s :: S) = PExecutionNativeStep06State
    { pexecutionStep06State'direction :: Term s (PAsData PInteger)
    , pexecutionStep06State'executionIndex :: Term s (PAsData PInteger)
    , pexecutionStep06State'sourceIndex :: Term s (PAsData PInteger)
    , pexecutionStep06State'originKind :: Term s (PAsData PInteger)
    , pexecutionStep06State'itemCommitment :: Term s (PAsData PByteString)
    , pexecutionStep06State'badTxId :: Term s (PAsData PByteString)
    , pexecutionStep06State'scriptItemHash :: Term s (PAsData PByteString)
    , pexecutionStep06State'validityIntervalStart :: Term s (PAsData PInteger)
    , pexecutionStep06State'validityIntervalEnd :: Term s (PAsData PInteger)
    , pexecutionStep06State'signerCount :: Term s (PAsData PInteger)
    , pexecutionStep06State'signerPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
    , pexecutionStep06State'phase :: Term s (PAsData PExecutionNativeStep06PhaseV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionNativeStep06State)

data PExecutionNativeSignerQueryV1 (s :: S) = PExecutionNativeSignerQueryV1
    { pexecutionSignerQuery'signerHash :: Term s (PAsData PByteString)
    , pexecutionSignerQuery'proof :: Term s (PAsData PSignerSetProofV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionNativeSignerQueryV1)

data PExecutionNativeStep06Args (s :: S)
    = PExecutionNativeStartScriptScan
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PByteString))
        (Term s (PAsData PInteger))
        (Term s (PAsData (PBuiltinList (PAsData PExecutionNativeSignerQueryV1))))
    | PExecutionNativeResumeScriptScan
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PByteString))
        (Term s (PAsData PByteString))
        (Term s (PAsData (PBuiltinList (PAsData PNativeScriptFrameV1))))
        (Term s (PAsData PInteger))
        (Term s (PAsData (PBuiltinList (PAsData PExecutionNativeSignerQueryV1))))
    | PExecutionNativeStartScriptFinalize
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PByteString))
        (Term s (PAsData PInteger))
        (Term s (PAsData (PBuiltinList (PAsData PExecutionNativeSignerQueryV1))))
    | PExecutionNativeFinalizeScriptScan
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PByteString))
        (Term s (PAsData PByteString))
        (Term s (PAsData (PBuiltinList (PAsData PNativeScriptFrameV1))))
        (Term s (PAsData PInteger))
        (Term s (PAsData (PBuiltinList (PAsData PExecutionNativeSignerQueryV1))))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionNativeStep06Args)
