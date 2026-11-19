{-# LANGUAGE OverloadedStrings #-}

-- | Direct receive-purpose/language rule (category 34) and staged Data ABI.
module Midgard.FraudProofs.ReceivePurposeLanguage (
    PStep01Source (..),
    PStep01Args (..),
    PBoundExecutionV1 (..),
    PStep02Args (..),
    PAuthenticatedReceiveLanguageV1 (..),
    PStep03Args (..),
    pbindExecutionV1,
    pauthenticateReceiveLanguageV1,
    pforbiddenReceiveLanguageHoldsV1,
    pterminalContradictionV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PEventKey (..), PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PReceivePurposePlutusV3Forbidden), prejectionCodeOf)
import Midgard.ScriptProof (pexecutionLeafHash, ppurposeLeafHash, psourceDescriptorLeafHash)
import Midgard.TransitionTrace (PRootDomain (PValidationTracesRootDomain), PRootMembershipProof (..), pverifyRootMembershipWithBytes)
import Midgard.ValidationMachine (PNativeScriptsControlV1 (..), pencodeNativeScriptsControlV1)
import Midgard.ValidationMerkle (pverifyMembership)
import Midgard.ValidationTrace (
    PValidationMachineStateV1 (..),
    PValidationPhase (PNativeScripts),
    PValidationSourceKind (PForced, PNormal),
    PValidationTraceDescriptorV1 (..),
    PValidationTraceProof (..),
    PValidationVerdict (PAccepted, PRejected),
    phashMachineState,
    phashRejectionCode,
    phashWorkWitness,
    pverifyTraceProof,
 )

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

data PBoundExecutionV1 (s :: S) = PBoundExecutionV1
    { pboundExecution'subject :: Term s (PAsData Subject.PVerdictSubject)
    , pboundExecution'validationTracesRoot :: Term s (PAsData PByteString)
    , pboundExecution'validationTraceCount :: Term s (PAsData PInteger)
    , pboundExecution'executionIndex :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PBoundExecutionV1)

data PStep02Args (s :: S) = PStep02Args
    { pstep02Args'inputIndex :: Term s (PAsData PInteger)
    , pstep02Args'outputIndex :: Term s (PAsData PInteger)
    , pstep02Args'traceMembership :: Term s (PAsData PRootMembershipProof)
    , pstep02Args'machineState :: Term s (PAsData PValidationMachineStateV1)
    , pstep02Args'traceProof :: Term s (PAsData PValidationTraceProof)
    , pstep02Args'control :: Term s (PAsData PNativeScriptsControlV1)
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

data PAuthenticatedReceiveLanguageV1 (s :: S) = PAuthenticatedReceiveLanguageV1
    { pauthenticatedReceive'bound :: Term s (PAsData PBoundExecutionV1)
    , pauthenticatedReceive'priorLedgerRoot :: Term s (PAsData PByteString)
    , pauthenticatedReceive'purposeKind :: Term s (PAsData PInteger)
    , pauthenticatedReceive'purposeIndex :: Term s (PAsData PInteger)
    , pauthenticatedReceive'sourceIndex :: Term s (PAsData PInteger)
    , pauthenticatedReceive'originKind :: Term s (PAsData PInteger)
    , pauthenticatedReceive'sourceKey :: Term s (PAsData PByteString)
    , pauthenticatedReceive'languageTag :: Term s (PAsData PInteger)
    , pauthenticatedReceive'scriptHash :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedReceiveLanguageV1)

data PStep03Args (s :: S) = PStep03Args
    { pstep03Args'inputIndex :: Term s (PAsData PInteger)
    , pstep03Args'outputIndex :: Term s (PAsData PInteger)
    , pstep03Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

pbindExecutionV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PInteger :--> PInteger :--> PBoundExecutionV1)
pbindExecutionV1 = phoistAcyclic $ plam $ \subject validationTracesRoot validationTraceCount executionIndex ->
    pif
        ( Subject.psubjectIsCanonical
            # subject
            #&& plengthBS
            # validationTracesRoot
            #== 32
            #&& validationTraceCount
            #> 0
            #&& executionIndex
            #>= 0
        )
        ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
            let bound =
                    pcon $
                        PBoundExecutionV1
                            (pdata subject)
                            (pdata validationTracesRoot)
                            (pdata validationTraceCount)
                            (pdata executionIndex)
             in pif
                    (pfromData psubject'direction #== 1)
                    ( pmatch (Subject.prejectionReasonOf # subject) $ \case
                        PReceivePurposePlutusV3Forbidden committedIndex ->
                            pif (pfromData committedIndex #== executionIndex) bound perror
                        _ -> perror
                    )
                    bound
        )
        perror

pauthenticateReceiveLanguageV1 ::
    forall s.
    Term
        s
        ( PBoundExecutionV1
            :--> PRootMembershipProof
            :--> PValidationMachineStateV1
            :--> PValidationTraceProof
            :--> PNativeScriptsControlV1
            :--> PInteger
            :--> PByteString
            :--> PByteString
            :--> PBuiltinList (PAsData PByteString)
            :--> PInteger
            :--> PInteger
            :--> PByteString
            :--> PInteger
            :--> PInteger
            :--> PByteString
            :--> PBuiltinList (PAsData PByteString)
            :--> PByteString
            :--> PBuiltinList (PAsData PByteString)
            :--> PAuthenticatedReceiveLanguageV1
        )
pauthenticateReceiveLanguageV1 = phoistAcyclic $ plam $ \bound membership machineState traceProof control purposeIndex scriptHash purposeSubject purposeSiblings sourceIndex originKind sourceKey languageTag totalLength itemCommitment sourceSiblings redeemerLeaf executionSiblings -> P.do
    PBoundExecutionV1{..} <- pmatch bound
    subject <- plet $ pfromData pboundExecution'subject
    Subject.PVerdictSubject{..} <- pmatch subject
    PRootMembershipProof{prootMembership'key, prootMembership'value} <- pmatch membership
    eventKey <- plet $ pcoerceData @PEventKey prootMembership'key
    descriptor <- plet $ pcoerceData @PValidationTraceDescriptorV1 prootMembership'value
    PValidationTraceDescriptorV1{pdescriptor'verdict, pdescriptor'rejectionCodeHash} <- pmatch descriptor
    PValidationMachineStateV1{..} <- pmatch machineState
    PValidationTraceProof{ptraceProof'stateHash} <- pmatch traceProof
    PNativeScriptsControlV1{..} <- pmatch control
    purposeLeaf <- plet $ ppurposeLeafHash # 3 # purposeIndex # scriptHash # purposeSubject
    sourceLeaf <- plet $ psourceDescriptorLeafHash # originKind # sourceKey # languageTag # scriptHash # totalLength # itemCommitment
    executionLeaf <- plet $ pexecutionLeafHash # languageTag # purposeLeaf # sourceLeaf # redeemerLeaf
    let subjectSourceKind = pfromData psubject'sourceKind
        expectedRejectionHash =
            phashRejectionCode
                # (prejectionCodeOf # pforgetData (pdata $ Subject.prejectionReasonOf # subject))
        sourceAndVerdictMatch =
            pif
                (subjectSourceKind #== 0)
                (pfromData pmachineState'sourceKind #== pcon PNormal #&& pfromData pdescriptor'verdict #== pcon PAccepted)
                ( pfromData pmachineState'sourceKind
                    #== pcon PForced
                    #&& pfromData
                        pdescriptor'verdict
                    #== pcon PRejected
                    #&& pfromData
                        pdescriptor'rejectionCodeHash
                    #== expectedRejectionHash
                )
        expected =
            pcon $
                PAuthenticatedReceiveLanguageV1
                    (pdata bound)
                    pmachineState'priorLedgerRoot
                    (pdata 3)
                    (pdata purposeIndex)
                    (pdata sourceIndex)
                    (pdata originKind)
                    (pdata sourceKey)
                    (pdata languageTag)
                    (pdata scriptHash)
    pif
        ( (languageTag #== 0 #|| languageTag #== 3 #|| languageTag #== 128)
            #&& peventKeyMatchesSubject
                eventKey
                subject
            #&& pfromData
                pmachineState'eventKeyHash
            #== (pblake2b_256 #$ pserialiseData # prootMembership'key)
            #&& pverifyRootMembershipWithBytes
                membership
                (pdata $ pcon PValidationTracesRootDomain)
                (pfromData pboundExecution'validationTracesRoot)
                (pfromData pboundExecution'validationTraceCount)
                (pserialiseData # prootMembership'key)
                (pserialiseData # prootMembership'value)
            #&& pfromData
                pmachineState'transactionId
            #== pfromData
                psubject'transactionId
            #&& sourceAndVerdictMatch
            #&& pfromData
                pmachineState'phase
            #== pcon PNativeScripts
            #&& pfromData
                pmachineState'workRoot
            #== (phashWorkWitness # pcon PNativeScripts # pfromData pmachineState'programCounter # (pencodeNativeScriptsControlV1 # control))
            #&& pfromData
                ptraceProof'stateHash
            #== (phashMachineState # machineState)
            #&& (pverifyTraceProof # descriptor # traceProof)
            #&& pfromData
                pboundExecution'executionIndex
            #< pfromData
                pnativeControl'executionCount
            #&& pfromData
                pnativeControl'executionCount
            #== pfromData
                pnativeControl'purposeCount
            #&& pverifyMembership
            # pfromData pnativeControl'purposeCount
            # pfromData pnativeControl'purposePeaks
            # pfromData pboundExecution'executionIndex
            # purposeLeaf
            # purposeSiblings
            #&& pverifyMembership
            # pfromData pnativeControl'sourceCount
            # pfromData pnativeControl'sourcePeaks
            # sourceIndex
            # sourceLeaf
            # sourceSiblings
            #&& pverifyMembership
            # pfromData pnativeControl'executionCount
            # pfromData pnativeControl'executionPeaks
            # pfromData pboundExecution'executionIndex
            # executionLeaf
            # executionSiblings
            #&& totalLength
            #> 0
            #&& plengthBS
            # scriptHash
            #== 28
            #&& plengthBS
            # itemCommitment
            #== 32
        )
        expected
        perror

pforbiddenReceiveLanguageHoldsV1 :: forall s. Term s (PInteger :--> PInteger :--> PBool)
pforbiddenReceiveLanguageHoldsV1 = phoistAcyclic $ plam $ \purposeKind languageTag ->
    purposeKind #== 3 #&& languageTag #== 3

pterminalContradictionV1 :: forall s. Term s (PAuthenticatedReceiveLanguageV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state ->
    pmatch state $ \PAuthenticatedReceiveLanguageV1{..} ->
        pmatch (pfromData pauthenticatedReceive'bound) $ \PBoundExecutionV1{pboundExecution'subject} ->
            Subject.pterminalContradiction
                # pfromData pboundExecution'subject
                # (pforbiddenReceiveLanguageHoldsV1 # pfromData pauthenticatedReceive'purposeKind # pfromData pauthenticatedReceive'languageTag)

peventKeyMatchesSubject :: forall s. Term s PEventKey -> Term s Subject.PVerdictSubject -> Term s PBool
peventKeyMatchesSubject eventKey subject =
    pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'sourceKind, Subject.psubject'transactionId, Subject.psubject'sourceKey} ->
        pmatch eventKey $ \case
            PL2TransactionEventKey txId ->
                pfromData psubject'sourceKind #== 0 #&& pfromData txId #== pfromData psubject'transactionId
            PForcedTransactionEventKey txOrderId ->
                pfromData psubject'sourceKind #== 1 #&& (pserialiseData # pforgetData txOrderId) #== pfromData psubject'sourceKey
            _ -> pconstant False

pcoerceData :: forall a s. (PIsData a) => Term s PData -> Term s a
pcoerceData = pfromData . punsafeCoerce
