{-# LANGUAGE OverloadedStrings #-}

-- | Exact execution-source script-decoding rule and five-step ABI.
module Midgard.FraudProofs.ExecutionSourceScriptDecoding (
    PBoundExecutionV1 (..),
    PAuthenticatedExecutionSourceV1 (..),
    PExecutionSourceScanStateV1 (..),
    PStep01Source (..),
    PStep01Args (..),
    PStep02Args (..),
    PStep03Args (..),
    PStep04Args (..),
    PStep05Args (..),
    presultPending,
    presultNoFault,
    presultMalformed,
    presultNodeLimit,
    presultDepthLimit,
    pbindExecutionV1,
    pauthenticateExecutionSourceV1,
    pcheckpointV1,
    pbindExactItemV1,
    pstateIsAuthenticV1,
    pauthenticatedWindowV1,
    padvancedStateV1,
    pclosedStateV1,
    pmappedRefusalClassV1,
    pterminalContradictionV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.NativeScriptDecoding.Engine qualified as Engine
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardTxInputCbor)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PEventKey (..), PHeaderV1)
import Midgard.NativeScriptScan qualified as Scan
import Midgard.RejectionReason (PRejectionReasonV1 (..), prejectionCodeOf)
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

data PBoundExecutionV1 (s :: S) = PBoundExecutionV1
    { pboundExecution'subject :: Term s (PAsData Subject.PVerdictSubject)
    , pboundExecution'validationTracesRoot :: Term s (PAsData PByteString)
    , pboundExecution'validationTraceCount :: Term s (PAsData PInteger)
    , pboundExecution'executionIndex :: Term s (PAsData PInteger)
    , pboundExecution'accusedClass :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PBoundExecutionV1)

data PAuthenticatedExecutionSourceV1 (s :: S) = PAuthenticatedExecutionSourceV1
    { pauthenticatedSource'bound :: Term s (PAsData PBoundExecutionV1)
    , pauthenticatedSource'priorLedgerRoot :: Term s (PAsData PByteString)
    , pauthenticatedSource'sourceIndex :: Term s (PAsData PInteger)
    , pauthenticatedSource'originKind :: Term s (PAsData PInteger)
    , pauthenticatedSource'sourceKey :: Term s (PAsData PByteString)
    , pauthenticatedSource'languageTag :: Term s (PAsData PInteger)
    , pauthenticatedSource'scriptHash :: Term s (PAsData PByteString)
    , pauthenticatedSource'totalLength :: Term s (PAsData PInteger)
    , pauthenticatedSource'itemCommitment :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedExecutionSourceV1)

data PExecutionSourceScanStateV1 (s :: S) = PExecutionSourceScanStateV1
    { pscanState'source :: Term s (PAsData PAuthenticatedExecutionSourceV1)
    , pscanState'controlCbor :: Term s (PAsData PByteString)
    , pscanState'nextExpectedScriptHash :: Term s (PAsData PByteString)
    , pscanState'checkpointHash :: Term s (PAsData PByteString)
    , pscanState'resultClass :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PExecutionSourceScanStateV1)

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

data PStep03Args (s :: S) = PStep03Args
    { pstep03Args'inputIndex :: Term s (PAsData PInteger)
    , pstep03Args'outputIndex :: Term s (PAsData PInteger)
    , pstep03Args'firstChunk :: Term s (PAsData Bounded.PChunkProofV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04Args (s :: S) = PStep04Args
    { pstep04Args'inputIndex :: Term s (PAsData PInteger)
    , pstep04Args'outputIndex :: Term s (PAsData PInteger)
    , pstep04Args'controlCbor :: Term s (PAsData PByteString)
    , pstep04Args'chunkProof :: Term s (PAsData (PMaybeData Bounded.PChunkProofV1))
    , pstep04Args'nextChunkProof :: Term s (PAsData (PMaybeData Bounded.PChunkProofV1))
    , pstep04Args'frames :: Term s (PAsData (PBuiltinList (PAsData Scan.PNativeScriptFrameV1)))
    , pstep04Args'stepBudget :: Term s (PAsData PInteger)
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

presultPending, presultNoFault, presultMalformed, presultNodeLimit, presultDepthLimit :: forall s. Term s PInteger
presultPending = -1
presultNoFault = -2
presultMalformed = 0
presultNodeLimit = 1
presultDepthLimit = 2

pcheckpointDomain :: forall s. Term s PByteString
pcheckpointDomain = pconstant "midgard/fraud-proofs/execution-source-script-decoding/checkpoint-v1"

pclassIsFault :: forall s. Term s (PInteger :--> PBool)
pclassIsFault = phoistAcyclic $ plam $ \value -> value #>= presultMalformed #&& value #<= presultDepthLimit

pbindExecutionV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PInteger :--> PInteger :--> PBoundExecutionV1)
pbindExecutionV1 = phoistAcyclic $ plam $ \subject root count executionIndex ->
    pif
        (Subject.psubjectIsCanonical # subject #&& plengthBS # root #== 32 #&& count #> 0 #&& executionIndex #>= 0)
        ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
            let accusedClass =
                    pif
                        (pfromData psubject'direction #== 1)
                        ( pmatch (Subject.prejectionReasonOf # subject) $ \case
                            PExecutionNativeScriptMalformed actual -> pif (pfromData actual #== executionIndex) presultMalformed perror
                            PExecutionNativeScriptNodeLimit actual -> pif (pfromData actual #== executionIndex) presultNodeLimit perror
                            PExecutionNativeScriptDepthLimit actual -> pif (pfromData actual #== executionIndex) presultDepthLimit perror
                            _ -> perror
                        )
                        presultPending
             in pcon $ PBoundExecutionV1 (pdata subject) (pdata root) (pdata count) (pdata executionIndex) (pdata accusedClass)
        )
        perror

pauthenticateExecutionSourceV1 ::
    forall s.
    Term
        s
        ( PBoundExecutionV1
            :--> PRootMembershipProof
            :--> PValidationMachineStateV1
            :--> PValidationTraceProof
            :--> PNativeScriptsControlV1
            :--> PInteger
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
            :--> PAuthenticatedExecutionSourceV1
        )
pauthenticateExecutionSourceV1 = phoistAcyclic $ plam $ \bound membership machineState traceProof control purposeKind purposeIndex scriptHash purposeSubject purposeSiblings sourceIndex originKind sourceKey languageTag totalLength itemCommitment sourceSiblings redeemerLeaf executionSiblings -> P.do
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
    purposeLeaf <- plet $ ppurposeLeafHash # purposeKind # purposeIndex # scriptHash # purposeSubject
    sourceLeaf <- plet $ psourceDescriptorLeafHash # originKind # sourceKey # languageTag # scriptHash # totalLength # itemCommitment
    executionLeaf <- plet $ pexecutionLeafHash # languageTag # purposeLeaf # sourceLeaf # redeemerLeaf
    let sourceAndVerdictMatch =
            pif
                (pfromData psubject'sourceKind #== 0)
                (pfromData pmachineState'sourceKind #== pcon PNormal #&& pfromData pdescriptor'verdict #== pcon PAccepted)
                ( pfromData pmachineState'sourceKind
                    #== pcon PForced
                    #&& pfromData pdescriptor'verdict
                    #== pcon PRejected
                    #&& pfromData pdescriptor'rejectionCodeHash
                    #== (phashRejectionCode # (prejectionCodeOf # pforgetData (pdata $ Subject.prejectionReasonOf # subject)))
                )
        expected =
            pcon $
                PAuthenticatedExecutionSourceV1
                    (pdata bound)
                    pmachineState'priorLedgerRoot
                    (pdata sourceIndex)
                    (pdata originKind)
                    (pdata sourceKey)
                    (pdata languageTag)
                    (pdata scriptHash)
                    (pdata totalLength)
                    (pdata itemCommitment)
    pif
        ( peventKeyMatchesSubject eventKey subject
            #&& pfromData pmachineState'eventKeyHash
            #== (pblake2b_256 #$ pserialiseData # prootMembership'key)
            #&& pverifyRootMembershipWithBytes membership (pdata $ pcon PValidationTracesRootDomain) (pfromData pboundExecution'validationTracesRoot) (pfromData pboundExecution'validationTraceCount) (pserialiseData # prootMembership'key) (pserialiseData # prootMembership'value)
            #&& pfromData pmachineState'transactionId
            #== pfromData psubject'transactionId
            #&& sourceAndVerdictMatch
            #&& pfromData pmachineState'phase
            #== pcon PNativeScripts
            #&& pfromData pmachineState'workRoot
            #== (phashWorkWitness # pcon PNativeScripts # pfromData pmachineState'programCounter # (pencodeNativeScriptsControlV1 # control))
            #&& pfromData ptraceProof'stateHash
            #== (phashMachineState # machineState)
            #&& (pverifyTraceProof # descriptor # traceProof)
            #&& pfromData pboundExecution'executionIndex
            #< pfromData pnativeControl'executionCount
            #&& pfromData pnativeControl'executionCount
            #== pfromData pnativeControl'purposeCount
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
            #&& languageTag
            #== 0
            #&& redeemerLeaf
            #== pconstant ""
            #&& totalLength
            #> 0
            #&& plengthBS
            # itemCommitment
            #== 32
        )
        expected
        perror

pcheckpointV1 :: forall s. Term s (PAuthenticatedExecutionSourceV1 :--> PByteString :--> PByteString :--> PByteString)
pcheckpointV1 = phoistAcyclic $ plam $ \source controlCbor nextHash -> pmatch source $ \PAuthenticatedExecutionSourceV1{..} ->
    pmatch (pfromData pauthenticatedSource'bound) $ \PBoundExecutionV1{pboundExecution'subject, pboundExecution'executionIndex} ->
        pblake2b_256
            #$ pcheckpointDomain
            <> (Subject.pencodeVerdictSubject # pfromData pboundExecution'subject)
            <> pcborInt (pfromData pboundExecution'executionIndex)
            <> pcborInt (pfromData pauthenticatedSource'sourceIndex)
            <> pcborInt (pfromData pauthenticatedSource'originKind)
            <> (pencodeDefiniteBytes # pfromData pauthenticatedSource'sourceKey)
            <> pcborInt (pfromData pauthenticatedSource'totalLength)
            <> (pencodeDefiniteBytes # pfromData pauthenticatedSource'itemCommitment)
            <> (pencodeDefiniteBytes # controlCbor)
            <> (pencodeDefiniteBytes # nextHash)

pstateWith :: forall s. Term s PAuthenticatedExecutionSourceV1 -> Term s PByteString -> Term s PByteString -> Term s PInteger -> Term s PExecutionSourceScanStateV1
pstateWith source controlCbor nextHash resultClass =
    pcon $
        PExecutionSourceScanStateV1
            (pdata source)
            (pdata controlCbor)
            (pdata nextHash)
            (pdata $ pcheckpointV1 # source # controlCbor # nextHash)
            (pdata resultClass)

pchunkMatches :: forall s. Term s PAuthenticatedExecutionSourceV1 -> Term s Bounded.PChunkProofV1 -> Term s PInteger -> Term s PBool
pchunkMatches source proof expectedIndex = pmatch source $ \PAuthenticatedExecutionSourceV1{..} ->
    pmatch proof $ \Bounded.PChunkProofV1{..} ->
        let coordinateMatches =
                pif
                    (pfromData pauthenticatedSource'originKind #== 0)
                    ( pfromData pchunkProof'fieldIndex
                        #== 6
                        #&& pfromData pchunkProof'itemIndex
                        #== pfromData pauthenticatedSource'sourceIndex
                    )
                    ( pif
                        (pfromData pauthenticatedSource'originKind #== 1)
                        ( pmatch (pdecodeMidgardTxInputCbor # pfromData pauthenticatedSource'sourceKey) $ \PMidgardTxInput{ptxInput'outputIndex} ->
                            pfromData pchunkProof'fieldIndex #== 2 #&& pfromData pchunkProof'itemIndex #== pfromData ptxInput'outputIndex
                        )
                        perror
                    )
         in pand'List
                [ coordinateMatches
                , pfromData pchunkProof'totalLength #== pfromData pauthenticatedSource'totalLength
                , pfromData pchunkProof'chunkIndex #== expectedIndex
                , Bounded.pverifyChunk # pfromData pauthenticatedSource'itemCommitment # proof
                ]

pbindExactItemV1 :: forall s. Term s (PAuthenticatedExecutionSourceV1 :--> Bounded.PChunkProofV1 :--> PByteString :--> PExecutionSourceScanStateV1)
pbindExactItemV1 = phoistAcyclic $ plam $ \source firstChunk scanHash ->
    pif
        (pchunkMatches source firstChunk 0)
        ( pmatch firstChunk $ \Bounded.PChunkProofV1{pchunkProof'chunk} ->
            pmatch source $ \PAuthenticatedExecutionSourceV1{pauthenticatedSource'totalLength} ->
                pmatch (Engine.pbindMachineV1 # pfromData pchunkProof'chunk # pfromData pauthenticatedSource'totalLength) $ \case
                    Engine.PMachineBindMalformedV1 -> pstateWith source (pconstant "") scanHash presultMalformed
                    Engine.PMachineBindNonNativeV1 _ -> pstateWith source (pconstant "") scanHash presultNoFault
                    Engine.PMachineBoundV1 control -> pstateWith source (Scan.pencodeStructureControlV1 # pfromData control) scanHash presultPending
        )
        perror

pstateIsAuthenticV1 :: forall s. Term s (PExecutionSourceScanStateV1 :--> PBool)
pstateIsAuthenticV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PExecutionSourceScanStateV1{..} ->
    pmatch (pfromData pscanState'source) $ \PAuthenticatedExecutionSourceV1{pauthenticatedSource'itemCommitment} ->
        pand'List
            [ plengthBS # pfromData pauthenticatedSource'itemCommitment #== 32
            , plengthBS # pfromData pscanState'nextExpectedScriptHash #== 28
            , pfromData pscanState'checkpointHash #== pcheckpointV1 # pfromData pscanState'source # pfromData pscanState'controlCbor # pfromData pscanState'nextExpectedScriptHash
            ]

pauthenticatedWindowV1 :: forall s. Term s (PExecutionSourceScanStateV1 :--> Scan.PNativeScriptStructureControlV1 :--> Bounded.PChunkProofV1 :--> PMaybeData Bounded.PChunkProofV1 :--> Engine.PScanWindowV1)
pauthenticatedWindowV1 = phoistAcyclic $ plam $ \state control proof nextProof ->
    pmatch state $ \PExecutionSourceScanStateV1{pscanState'source} ->
        pmatch control $ \Scan.PNativeScriptStructureControlV1{Scan.pstructure'cursor} ->
            pmatch (pfromData pscanState'source) $ \source@PAuthenticatedExecutionSourceV1{pauthenticatedSource'totalLength} ->
                let index = pdiv # pfromData pstructure'cursor # Bounded.pchunkBytes
                 in pif
                        (pchunkMatches (pcon source) proof index)
                        ( pmatch proof $ \Bounded.PChunkProofV1{pchunkProof'chunk} ->
                            pif
                                (index + 1 #< Bounded.pchunkCount # pfromData pauthenticatedSource'totalLength)
                                ( pmatch nextProof $ \case
                                    PDNothing -> perror
                                    PDJust nextData ->
                                        let next = pfromData nextData
                                         in pif
                                                (pchunkMatches (pcon source) next (index + 1))
                                                (pmatch next $ \Bounded.PChunkProofV1{pchunkProof'chunk = nextChunk} -> pcon $ Engine.PScanWindowV1 (pdata $ pfromData pchunkProof'chunk <> pfromData nextChunk) (pdata $ index * Bounded.pchunkBytes))
                                                perror
                                )
                                (pmatch nextProof $ \case PDNothing -> pcon $ Engine.PScanWindowV1 pchunkProof'chunk (pdata $ index * Bounded.pchunkBytes); PDJust _ -> perror)
                        )
                        perror

padvancedStateV1 :: forall s. Term s (PExecutionSourceScanStateV1 :--> Scan.PNativeScriptStructureControlV1 :--> PByteString :--> PExecutionSourceScanStateV1)
padvancedStateV1 = phoistAcyclic $ plam $ \state control nextHash -> pmatch state $ \PExecutionSourceScanStateV1{pscanState'source} ->
    pstateWith (pfromData pscanState'source) (Scan.pencodeStructureControlV1 # control) nextHash presultPending

pclosedStateV1 :: forall s. Term s (PExecutionSourceScanStateV1 :--> PInteger :--> PByteString :--> PExecutionSourceScanStateV1)
pclosedStateV1 = phoistAcyclic $ plam $ \state resultClass finalHash -> pmatch state $ \PExecutionSourceScanStateV1{pscanState'source, pscanState'controlCbor} ->
    pif
        (resultClass #== presultNoFault #|| pclassIsFault # resultClass)
        (pstateWith (pfromData pscanState'source) (pfromData pscanState'controlCbor) finalHash resultClass)
        perror

pmappedRefusalClassV1 :: forall s. Term s (PInteger :--> PInteger)
pmappedRefusalClassV1 = phoistAcyclic $ plam $ \scanClass ->
    pif
        (scanClass #== Engine.prefusalClassMalformed)
        presultMalformed
        (pif (scanClass #== Engine.prefusalClassNodeLimit) presultNodeLimit (pif (scanClass #== Engine.prefusalClassDepthLimit) presultDepthLimit perror))

pterminalContradictionV1 :: forall s. Term s (PExecutionSourceScanStateV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PExecutionSourceScanStateV1{pscanState'source, pscanState'resultClass} ->
    pif
        (pstateIsAuthenticV1 # state #&& pfromData pscanState'resultClass #/= presultPending)
        ( pmatch (pfromData pscanState'source) $ \PAuthenticatedExecutionSourceV1{pauthenticatedSource'bound} ->
            pmatch (pfromData pauthenticatedSource'bound) $ \PBoundExecutionV1{pboundExecution'subject, pboundExecution'accusedClass} ->
                pmatch (pfromData pboundExecution'subject) $ \subject@Subject.PVerdictSubject{Subject.psubject'direction} ->
                    pif
                        (pfromData psubject'direction #== 1)
                        (pfromData pscanState'resultClass #/= pfromData pboundExecution'accusedClass)
                        (Subject.pterminalContradiction # pcon subject # (pclassIsFault # pfromData pscanState'resultClass))
        )
        perror

peventKeyMatchesSubject :: forall s. Term s PEventKey -> Term s Subject.PVerdictSubject -> Term s PBool
peventKeyMatchesSubject eventKey subject =
    pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'sourceKind, Subject.psubject'transactionId, Subject.psubject'sourceKey} ->
        pmatch eventKey $ \case
            PL2TransactionEventKey txId -> pfromData psubject'sourceKind #== 0 #&& pfromData txId #== pfromData psubject'transactionId
            PForcedTransactionEventKey txOrderId -> pfromData psubject'sourceKind #== 1 #&& (pserialiseData # pforgetData txOrderId) #== pfromData psubject'sourceKey
            _ -> pconstant False

pcoerceData :: forall a s. (PIsData a) => Term s PData -> Term s a
pcoerceData = pfromData . punsafeCoerce
