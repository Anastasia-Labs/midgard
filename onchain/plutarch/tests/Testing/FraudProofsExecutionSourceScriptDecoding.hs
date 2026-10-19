{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsExecutionSourceScriptDecoding (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.ExecutionSourceScriptDecoding
import Midgard.FraudProofs.NativeScriptDecoding.Engine qualified as Engine
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.NativeScriptScan qualified as Scan
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Midgard.ValidationMerkle (pappendLeaf)
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
    testGroup
        "Execution-source-script decoding rule"
        [ testCase "authenticates the exact execution and source frontiers" $ passertEvalNoTrace authenticSourceShape
        , testCase "binds exact reason and refuses coordinate substitution" $ pfails substitutedMalformedCoordinate
        , testCase "header and payload malformed map to the same execution class" $ passertEvalNoTrace headerAndPayloadMalformed
        , testCase "empty native payload closes malformed at bind" $ passertEvalNoTrace emptyNativePayloadClosesMalformed
        , testCase "scan resume changes checkpoint and reaches exact terminal" $ passertEvalNoTrace scanResumeReachesTerminal
        , testCase "refuses an adjacent chunk for a single chunk item" $ pfails adjacentChunkForSingleChunk
        , testCase "refuses a substituted source item chunk" $ pfails substitutedSourceChunk
        , testCase "exact node boundary advances and adjacent node refuses" $ passertEvalNoTrace exactAndAdjacentNodeBoundary
        , testCase "exact depth boundary advances and adjacent depth refuses" $ passertEvalNoTrace exactAndAdjacentDepthBoundary
        , testCase "malformed acceptance and decodable wrongful rejection close" $ passertEvalNoTrace wrongfulVerdictsClose
        , testCase "honest results do not convict" $ passertEvalNoTrace honestResultsDoNotConvict
        , testCase "frame-stage segment reads tokens through cursor window" $ passertEvalNoTrace frameStageUsesCursorWindow
        , testCase "forced node and depth accusations convict on a decodable item" $ passertEvalNoTrace forcedBoundsConvict
        , testCase "honest node and depth rejections do not convict" $ passertEvalNoTrace honestBoundsDoNotConvict
        , testCase "accepted node and depth results convict" $ passertEvalNoTrace acceptedBoundsConvict
        , testCase "binds node-limit reason to its exact execution coordinate" $ pfails substitutedNodeCoordinate
        , testCase "binds depth-limit reason to its exact execution coordinate" $ pfails substitutedDepthCoordinate
        , testCase "refuses a substituted checkpoint" $ pfails substitutedCheckpoint
        , testCase "wrong successor is bound into the checkpoint" $ pfails substitutedSuccessor
        ]

authenticSourceShape :: forall s. Term s PBool
authenticSourceShape =
    pmatch (directSource acceptedSubject signatureItem) $ \PAuthenticatedExecutionSourceV1{..} ->
        pmatch (pfromData pauthenticatedSource'bound) $ \PBoundExecutionV1{pboundExecution'executionIndex} ->
            pfromData pboundExecution'executionIndex
                #== 0
                #&& pfromData pauthenticatedSource'sourceIndex
                #== 0
                #&& pfromData pauthenticatedSource'itemCommitment
                #== (Bounded.pfromBytes # 6 # 0 # signatureItem)

substitutedMalformedCoordinate :: forall s. Term s PBool
substitutedMalformedCoordinate = accusedClass (pbindExecutionV1 # forcedMalformed 0 # h32 # 1 # 1) #== presultMalformed

headerAndPayloadMalformed :: forall s. Term s PBool
headerAndPayloadMalformed =
    resultClass (opened acceptedSubject malformedWrapper)
        #== presultMalformed
        #&& resultClass (opened acceptedSubject malformedPayload)
        #== presultPending

emptyNativePayloadClosesMalformed :: forall s. Term s PBool
emptyNativePayloadClosesMalformed =
    plet (opened acceptedSubject emptyPayload) $ \state ->
        resultClass state
            #== presultMalformed
            #&& controlCbor state
            #== pconstant ""
            #&& pterminalContradictionV1
            # (pclosedStateV1 # state # presultMalformed # finalHash)
            #&& resultClass (opened acceptedSubject plutusItem)
            #== presultNoFault
            #&& resultClass (opened acceptedSubject signatureItem)
            #== presultPending

scanResumeReachesTerminal :: forall s. Term s PBool
scanResumeReachesTerminal =
    plet (opened acceptedSubject signatureItem) $ \state ->
        plet (controlOf state) $ \control ->
            pmatch (Engine.pbudgetedScanV1 # control # exactWindow state control signatureItem # pnil # 2) $ \case
                Engine.PScanAdvancedV1 terminalData ->
                    let terminal = pfromData terminalData
                        closed = pclosedStateV1 # state # presultNoFault # finalHash
                     in Scan.pstructureTerminalIsExactV1
                            # terminal
                            #&& checkpoint closed
                            #/= checkpoint state
                            #&& pstateIsAuthenticV1
                            # closed
                _ -> pconstant False

adjacentChunkForSingleChunk :: forall s. Term s PBool
adjacentChunkForSingleChunk =
    plet (opened acceptedSubject signatureItem) $ \state ->
        pauthenticatedWindowV1
            # state
            # controlOf state
            # itemProof signatureItem
            # pcon (PDJust $ pdata $ itemProof signatureItem)
            #== pcon (Engine.PScanWindowV1 (pdata signatureItem) (pdata 0))

substitutedSourceChunk :: forall s. Term s PBool
substitutedSourceChunk =
    plet (opened acceptedSubject signatureItem) $ \state ->
        pauthenticatedWindowV1
            # state
            # controlOf state
            # itemProof malformedPayload
            # pcon PDNothing
            #== pcon (Engine.PScanWindowV1 (pdata malformedPayload) (pdata 0))

exactAndAdjacentNodeBoundary :: forall s. Term s PBool
exactAndAdjacentNodeBoundary =
    plet (structureControl signatureNode (Scan.pmaxNativeScriptNodes - 1) 0 (pconstant "")) $ \exact ->
        plet (structureControl signatureNode Scan.pmaxNativeScriptNodes 0 (pconstant "")) $ \adjacent ->
            pmatch (Engine.pbudgetedScanV1 # exact # fullWindow signatureNode # pnil # 1) $ \case
                Engine.PScanAdvancedV1 nextData ->
                    pmatch (pfromData nextData) $ \Scan.PNativeScriptStructureControlV1{Scan.pstructure'nodeCount} ->
                        pfromData pstructure'nodeCount
                            #== Scan.pmaxNativeScriptNodes
                            #&& pmatch
                                (Engine.pbudgetedScanV1 # adjacent # fullWindow signatureNode # pnil # 1)
                                (\case Engine.PScanRefusedV1 refusal -> pmappedRefusalClassV1 # pfromData refusal #== presultNodeLimit; _ -> pconstant False)
                _ -> pconstant False

exactAndAdjacentDepthBoundary :: forall s. Term s PBool
exactAndAdjacentDepthBoundary =
    plet (Scan.phashFrameV1 # rootFrame 2) $ \root ->
        plet (structureControl allOfTwoPayload 1 (Scan.pmaxNativeScriptDepth - 1) root) $ \exact ->
            plet (structureControl allOfTwoPayload 1 Scan.pmaxNativeScriptDepth root) $ \adjacent ->
                pmatch (Engine.pbudgetedScanV1 # exact # fullWindow allOfTwoPayload # pnil # 1) $ \case
                    Engine.PScanAdvancedV1 nextData ->
                        pmatch (pfromData nextData) $ \Scan.PNativeScriptStructureControlV1{Scan.pstructure'stackDepth} ->
                            pfromData pstructure'stackDepth
                                #== Scan.pmaxNativeScriptDepth
                                #&& pmatch
                                    (Engine.pbudgetedScanV1 # adjacent # fullWindow allOfTwoPayload # pnil # 1)
                                    (\case Engine.PScanRefusedV1 refusal -> pmappedRefusalClassV1 # pfromData refusal #== presultDepthLimit; _ -> pconstant False)
                    _ -> pconstant False

wrongfulVerdictsClose :: forall s. Term s PBool
wrongfulVerdictsClose =
    pterminalContradictionV1
        # (pclosedStateV1 # opened acceptedSubject malformedWrapper # presultMalformed # finalHash)
        #&& pterminalContradictionV1
        # (pclosedStateV1 # opened (forcedMalformed 0) signatureItem # presultNoFault # finalHash)

honestResultsDoNotConvict :: forall s. Term s PBool
honestResultsDoNotConvict =
    pnot
        # (pterminalContradictionV1 # (pclosedStateV1 # opened acceptedSubject plutusItem # presultNoFault # finalHash))
        #&& pnot
        # (pterminalContradictionV1 # (pclosedStateV1 # opened (forcedMalformed 0) malformedWrapper # presultMalformed # finalHash))

frameStageUsesCursorWindow :: forall s. Term s PBool
frameStageUsesCursorWindow =
    plet (opened acceptedSubject allOfTwoItem) $ \state ->
        plet (controlOf state) $ \control ->
            pmatch (Engine.pbudgetedScanV1 # control # exactWindow state control allOfTwoItem # pnil # 2) $ \case
                Engine.PScanAdvancedV1 midData ->
                    let mid = pfromData midData
                        resumed = padvancedStateV1 # state # mid # scanHash
                        frame = rootFrame 2
                        frames = pcons # pdata frame # (pcons # pdata (rootFrame 1) # pnil)
                     in pmatch (Engine.pbudgetedScanV1 # mid # exactWindow resumed mid allOfTwoItem # frames # 4) $ \case
                            Engine.PScanAdvancedV1 terminalData ->
                                pmatch (Engine.pbudgetedScanV1 # mid # pcon PNothing # frames # 4) $ \case
                                    Engine.PScanAdvancedV1 stalledData ->
                                        checkpoint resumed
                                            #/= checkpoint state
                                            #&& Scan.pstructureTerminalIsExactV1
                                            # pfromData terminalData
                                            #&& nodeCount (pfromData terminalData)
                                            #== 3
                                            #&& nodeCount (pfromData stalledData)
                                            #== 2
                                    _ -> pconstant False
                            _ -> pconstant False
                _ -> pconstant False

forcedBoundsConvict :: forall s. Term s PBool
forcedBoundsConvict =
    let closed subject result = pclosedStateV1 # opened subject signatureItem # result # finalHash
     in accusedClass (boundOf $ closed (forcedNode 0) presultNoFault)
            #== presultNodeLimit
            #&& accusedClass (boundOf $ closed (forcedDepth 0) presultNoFault)
            #== presultDepthLimit
            #&& pterminalContradictionV1
            # closed (forcedNode 0) presultNoFault
            #&& pterminalContradictionV1
            # closed (forcedDepth 0) presultNoFault

honestBoundsDoNotConvict :: forall s. Term s PBool
honestBoundsDoNotConvict =
    let closed subject result = pclosedStateV1 # opened subject allOfTwoItem # result # finalHash
     in pnot
            # (pterminalContradictionV1 # closed (forcedNode 0) presultNodeLimit)
            #&& pnot
            # (pterminalContradictionV1 # closed (forcedDepth 0) presultDepthLimit)
            #&& pterminalContradictionV1
            # closed (forcedNode 0) presultDepthLimit
            #&& pterminalContradictionV1
            # closed (forcedDepth 0) presultNodeLimit

acceptedBoundsConvict :: forall s. Term s PBool
acceptedBoundsConvict =
    let closed result = pclosedStateV1 # opened acceptedSubject allOfTwoItem # result # finalHash
     in pterminalContradictionV1
            # closed presultNodeLimit
            #&& pterminalContradictionV1
            # closed presultDepthLimit
            #&& pterminalContradictionV1
            # closed presultMalformed
            #&& pnot
            # (pterminalContradictionV1 # closed presultNoFault)
            #&& pmappedRefusalClassV1
            # Engine.prefusalClassMalformed
            #== presultMalformed
            #&& pmappedRefusalClassV1
            # Engine.prefusalClassNodeLimit
            #== presultNodeLimit
            #&& pmappedRefusalClassV1
            # Engine.prefusalClassDepthLimit
            #== presultDepthLimit

substitutedNodeCoordinate :: forall s. Term s PBool
substitutedNodeCoordinate = accusedClass (pbindExecutionV1 # forcedNode 1 # h32 # 1 # 0) #== presultNodeLimit

substitutedDepthCoordinate :: forall s. Term s PBool
substitutedDepthCoordinate = accusedClass (pbindExecutionV1 # forcedDepth 1 # h32 # 1 # 0) #== presultDepthLimit

substitutedCheckpoint :: forall s. Term s PBool
substitutedCheckpoint =
    plet (pclosedStateV1 # opened acceptedSubject malformedPayload # presultMalformed # finalHash) $ \state ->
        pmatch state $ \PExecutionSourceScanStateV1{..} ->
            pterminalContradictionV1
                # pcon
                    ( PExecutionSourceScanStateV1
                        pscanState'source
                        pscanState'controlCbor
                        pscanState'nextExpectedScriptHash
                        (pdata h32)
                        pscanState'resultClass
                    )

substitutedSuccessor :: forall s. Term s PBool
substitutedSuccessor =
    plet (opened acceptedSubject malformedPayload) $ \state ->
        plet (pclosedStateV1 # state # presultMalformed # finalHash) $ \toFinal ->
            plet (pclosedStateV1 # state # presultMalformed # scanHash) $ \toScan ->
                pif
                    (checkpoint toFinal #/= checkpoint toScan)
                    ( pmatch toFinal $ \PExecutionSourceScanStateV1{..} ->
                        pterminalContradictionV1
                            # pcon
                                ( PExecutionSourceScanStateV1
                                    pscanState'source
                                    pscanState'controlCbor
                                    (pdata scanHash)
                                    pscanState'checkpointHash
                                    pscanState'resultClass
                                )
                    )
                    perror

opened :: forall s. Term s Subject.PVerdictSubject -> Term s PByteString -> Term s PExecutionSourceScanStateV1
opened subject item = pbindExactItemV1 # directSource subject item # itemProof item # scanHash

directSource :: forall s. Term s Subject.PVerdictSubject -> Term s PByteString -> Term s PAuthenticatedExecutionSourceV1
directSource subject item =
    pcon $
        PAuthenticatedExecutionSourceV1
            (pdata $ pbindExecutionV1 # subject # h32 # 1 # 0)
            (pdata h32)
            (pdata 0)
            (pdata 0)
            (pdata $ phexByteStr "00")
            (pdata 0)
            (pdata h28)
            (pdata $ plengthBS # item)
            (pdata $ Bounded.pfromBytes # 6 # 0 # item)

itemProof :: forall s. Term s PByteString -> Term s Bounded.PChunkProofV1
itemProof item =
    plet (Bounded.phashChunk # 6 # 0 # 0 # item) $ \leaf ->
        pcon $
            Bounded.PChunkProofV1
                (pdata Bounded.pversion)
                (pdata 6)
                (pdata 0)
                (pdata $ plengthBS # item)
                (pdata 0)
                (pdata item)
                (pdata $ pappendLeaf # 0 # pnil # leaf)
                (pdata pnil)

exactWindow :: forall s. Term s PExecutionSourceScanStateV1 -> Term s Scan.PNativeScriptStructureControlV1 -> Term s PByteString -> Term s (PMaybe Engine.PScanWindowV1)
exactWindow state control item = pcon $ PJust $ pauthenticatedWindowV1 # state # control # itemProof item # pcon PDNothing

fullWindow :: forall s. Term s PByteString -> Term s (PMaybe Engine.PScanWindowV1)
fullWindow item = pcon $ PJust $ pcon $ Engine.PScanWindowV1 (pdata item) (pdata 0)

rootFrame :: forall s. Term s PInteger -> Term s Scan.PNativeScriptFrameV1
rootFrame remaining = pcon $ Scan.PNativeScriptFrameV1 (pdata $ pconstant "") (pdata Scan.pallNode) (pdata 2) (pdata remaining) (pdata 0) (pdata 0)

structureControl :: forall s. Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s Scan.PNativeScriptStructureControlV1
structureControl payload nodes depth root =
    pcon $ Scan.PNativeScriptStructureControlV1 (pdata 1) (pdata Scan.pstructureStageToken) (pdata 0) (pdata 0) (pdata $ plengthBS # payload) (pdata root) (pdata depth) (pdata nodes)

acceptedSubject :: forall s. Term s Subject.PVerdictSubject
acceptedSubject = pcon $ Subject.PVerdictSubject (pdata 1) (pdata 0) (pdata 0) (pdata txId) (pdata $ pconstant "") (pdata $ pcon PDNothing)

forcedMalformed, forcedNode, forcedDepth :: forall s. Term s PInteger -> Term s Subject.PVerdictSubject
forcedMalformed index = forcedWith $ pcon $ PExecutionNativeScriptMalformed $ pdata index
forcedNode index = forcedWith $ pcon $ PExecutionNativeScriptNodeLimit $ pdata index
forcedDepth index = forcedWith $ pcon $ PExecutionNativeScriptDepthLimit $ pdata index

forcedWith :: forall s. Term s PRejectionReasonV1 -> Term s Subject.PVerdictSubject
forcedWith reason = pcon $ Subject.PVerdictSubject (pdata 1) (pdata 1) (pdata 1) (pdata txId) (pdata $ phexByteStr "00") (pdata $ pcon $ PDJust $ pdata reason)

boundOf :: forall s. Term s PExecutionSourceScanStateV1 -> Term s PBoundExecutionV1
boundOf state = pmatch state $ \PExecutionSourceScanStateV1{pscanState'source} -> pmatch (pfromData pscanState'source) $ \PAuthenticatedExecutionSourceV1{pauthenticatedSource'bound} -> pfromData pauthenticatedSource'bound

accusedClass :: forall s. Term s PBoundExecutionV1 -> Term s PInteger
accusedClass bound = pmatch bound $ \PBoundExecutionV1{pboundExecution'accusedClass} -> pfromData pboundExecution'accusedClass

resultClass :: forall s. Term s PExecutionSourceScanStateV1 -> Term s PInteger
resultClass state = pmatch state $ \PExecutionSourceScanStateV1{pscanState'resultClass} -> pfromData pscanState'resultClass

controlCbor :: forall s. Term s PExecutionSourceScanStateV1 -> Term s PByteString
controlCbor state = pmatch state $ \PExecutionSourceScanStateV1{pscanState'controlCbor} -> pfromData pscanState'controlCbor

controlOf :: forall s. Term s PExecutionSourceScanStateV1 -> Term s Scan.PNativeScriptStructureControlV1
controlOf state = Scan.pdecodeStructureControlV1 # controlCbor state

checkpoint :: forall s. Term s PExecutionSourceScanStateV1 -> Term s PByteString
checkpoint state = pmatch state $ \PExecutionSourceScanStateV1{pscanState'checkpointHash} -> pfromData pscanState'checkpointHash

nodeCount :: forall s. Term s Scan.PNativeScriptStructureControlV1 -> Term s PInteger
nodeCount control = pmatch control $ \Scan.PNativeScriptStructureControlV1{Scan.pstructure'nodeCount} -> pfromData pstructure'nodeCount

txId, h32, h28, scanHash, finalHash :: forall s. Term s PByteString
txId = phexByteStr "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
h32 = phexByteStr "1111111111111111111111111111111111111111111111111111111111111111"
h28 = phexByteStr "22222222222222222222222222222222222222222222222222222222"
scanHash = phexByteStr "33333333333333333333333333333333333333333333333333333333"
finalHash = phexByteStr "44444444444444444444444444444444444444444444444444444444"

signatureItem, signatureNode, allOfTwoItem, allOfTwoPayload, malformedPayload, malformedWrapper, emptyPayload, plutusItem :: forall s. Term s PByteString
signatureItem = bytes $ "82005820" <> signatureNodeHex
signatureNode = bytes signatureNodeHex
allOfTwoItem = bytes $ "82005843" <> allOfTwoPayloadHex
allOfTwoPayload = bytes allOfTwoPayloadHex
malformedPayload = phexByteStr "820043820700"
malformedWrapper = phexByteStr "8201410a"
emptyPayload = phexByteStr "820040"
plutusItem = phexByteStr "82034401020304"

signatureNodeHex, allOfTwoPayloadHex :: BS.ByteString
signatureNodeHex = "8200581c" <> BS.concat (replicate 28 "99")
allOfTwoPayloadHex = "820182" <> signatureNodeHex <> signatureNodeHex

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
