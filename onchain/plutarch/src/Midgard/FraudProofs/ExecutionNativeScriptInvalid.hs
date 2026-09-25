{-# LANGUAGE OverloadedStrings #-}

-- | Exact execution-native-script-invalid shared rule.
module Midgard.FraudProofs.ExecutionNativeScriptInvalid (
    PBoundExecutionV1 (..),
    PAuthenticatedExecutionSourceV1 (..),
    PExecutionSourceScanStateV1 (..),
    presultPending,
    presultNoFault,
    presultMalformed,
    presultNodeLimit,
    presultDepthLimit,
    pbindExecutionV1,
    pauthenticateExecutionSourceV1,
    psourceItemCoordinate,
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
import Midgard.FraudProofs.NativeScriptDecoding.Engine qualified as Engine
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardTxInputCbor)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PEventKey (..))
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
    , pboundExecution'priorLedgerRoot :: Term s (PAsData PByteString)
    , pboundExecution'compactCbor :: Term s (PAsData PByteString)
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
    , pauthenticatedSource'compactCbor :: Term s (PAsData PByteString)
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

presultPending, presultNoFault, presultMalformed, presultNodeLimit, presultDepthLimit :: forall s. Term s PInteger
presultPending = -1
presultNoFault = -2
presultMalformed = 0
presultNodeLimit = 1
presultDepthLimit = 2

pcheckpointDomain :: forall s. Term s PByteString
pcheckpointDomain = pconstant "midgard/fraud-proofs/execution-native-script-invalid/checkpoint-v1"

pclassIsFault :: forall s. Term s (PInteger :--> PBool)
pclassIsFault = phoistAcyclic $ plam $ \value -> value #>= presultMalformed #&& value #<= presultDepthLimit

pbindExecutionV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PInteger :--> PInteger :--> PByteString :--> PByteString :--> PBoundExecutionV1)
pbindExecutionV1 = phoistAcyclic $ plam $ \subject root count executionIndex priorLedgerRoot compactCbor ->
    pif
        ( pand'List
            [ Subject.psubjectIsCanonical # subject
            , plengthBS # root #== 32
            , count #> 0
            , executionIndex #>= 0
            , plengthBS # priorLedgerRoot #== 32
            , plengthBS # compactCbor #> 0
            ]
        )
        ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
            let accusedClass =
                    pif
                        (pfromData psubject'direction #== 1)
                        ( pmatch (Subject.prejectionReasonOf # subject) $ \case
                            PExecutionNativeScriptFalse actual -> pif (pfromData actual #== executionIndex) presultPending perror
                            _ -> perror
                        )
                        presultPending
             in pcon $ PBoundExecutionV1 (pdata subject) (pdata root) (pdata count) (pdata executionIndex) (pdata accusedClass) (pdata priorLedgerRoot) (pdata compactCbor)
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
                    pnativeControl'compactCbor
    pif
        ( peventKeyMatchesSubject eventKey subject
            #&& pfromData pmachineState'eventKeyHash
            #== (pblake2b_256 #$ pserialiseData # prootMembership'key)
            #&& pverifyRootMembershipWithBytes membership (pdata $ pcon PValidationTracesRootDomain) (pfromData pboundExecution'validationTracesRoot) (pfromData pboundExecution'validationTraceCount) (pserialiseData # prootMembership'key) (pserialiseData # prootMembership'value)
            #&& pfromData pmachineState'transactionId
            #== pfromData psubject'transactionId
            #&& pfromData pmachineState'priorLedgerRoot
            #== pfromData pboundExecution'priorLedgerRoot
            #&& pfromData pnativeControl'compactCbor
            #== pfromData pboundExecution'compactCbor
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

psourceItemCoordinate :: forall s. Term s (PAuthenticatedExecutionSourceV1 :--> PBuiltinPair (PAsData PInteger) (PAsData PInteger))
psourceItemCoordinate = phoistAcyclic $ plam $ \source -> pmatch source $ \PAuthenticatedExecutionSourceV1{pauthenticatedSource'originKind, pauthenticatedSource'sourceIndex, pauthenticatedSource'sourceKey} ->
    pif
        (pfromData pauthenticatedSource'originKind #== 0)
        (ppairDataBuiltin # pdata 6 # pauthenticatedSource'sourceIndex)
        ( pif
            (pfromData pauthenticatedSource'originKind #== 1)
            ( pmatch (pdecodeMidgardTxInputCbor # pfromData pauthenticatedSource'sourceKey) $ \PMidgardTxInput{ptxInput'outputIndex} ->
                ppairDataBuiltin # pdata 2 # ptxInput'outputIndex
            )
            perror
        )

pchunkMatches :: forall s. Term s PAuthenticatedExecutionSourceV1 -> Term s Bounded.PChunkProofV1 -> Term s PInteger -> Term s PBool
pchunkMatches source proof expectedIndex = pmatch source $ \sourceFields@PAuthenticatedExecutionSourceV1{..} ->
    pmatch proof $ \Bounded.PChunkProofV1{..} ->
        let coordinate = psourceItemCoordinate # pcon sourceFields
            coordinateMatches =
                pfromData pchunkProof'fieldIndex
                    #== pfromData (pfstBuiltin # coordinate)
                    #&& pfromData pchunkProof'itemIndex
                    #== pfromData (psndBuiltin # coordinate)
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
                pmatch (Scan.pversionedScriptHeaderV1 # pfromData pchunkProof'chunk # pfromData pauthenticatedSource'totalLength) $ \case
                    PNothing -> pstateWith source (pconstant "") scanHash presultMalformed
                    PJust header -> pmatch header $ \Scan.PVersionedScriptHeaderV1{Scan.pheader'languageTag, Scan.pheader'payloadOffset, Scan.pheader'payloadLength} ->
                        pif
                            (pfromData pheader'languageTag #/= 0)
                            (pstateWith source (pconstant "") scanHash presultNoFault)
                            ( pstateWith
                                source
                                (Scan.pencodeStructureControlV1 #$ Scan.pinitialStructureControlV1 # pfromData pheader'payloadOffset # pfromData pheader'payloadLength)
                                scanHash
                                presultPending
                            )
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
