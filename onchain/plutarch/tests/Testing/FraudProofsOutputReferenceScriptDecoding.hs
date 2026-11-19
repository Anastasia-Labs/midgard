{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsOutputReferenceScriptDecoding (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.NativeScriptDecoding.Engine qualified as Engine
import Midgard.FraudProofs.OutputReferenceScriptDecoding
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.NativeScriptScan qualified as Scan
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Midgard.ValidationMerkle (pappendLeaf)
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
    testGroup
        "Output-reference-script decoding rule"
        [ testCase "binds exact forced reason and coordinate" $ passertEvalNoTrace bindsExactForcedReason
        , testCase "refuses substituted output coordinate" $ pfails substitutedOutputCoordinate
        , testCase "refuses another typed reason" $ pfails anotherTypedReason
        , testCase "closes wrongful acceptance and decodable wrongful rejection" $ passertEvalNoTrace closesWrongfulVerdicts
        , testCase "honest polarities do not convict" $ passertEvalNoTrace honestPolaritiesDoNotConvict
        , testCase "checkpoint refuses successor substitution" $ passertEvalNoTrace checkpointRefusesSuccessorSubstitution
        , testCase "exact node boundary is admitted" $ passertEvalNoTrace exactNodeBoundary
        , testCase "adjacent node over bound is refused" $ passertEvalNoTrace adjacentNodeRefused
        , testCase "exact and adjacent depth boundary" $ passertEvalNoTrace exactAndAdjacentDepthBoundary
        , testCase "empty native payload closes malformed at bind" $ passertEvalNoTrace emptyNativePayloadClosesMalformed
        , testCase "signature native payload binds pending" $ passertEvalNoTrace signatureNativePayloadBindsPending
        , testCase "plutus payload closes no fault at bind" $ passertEvalNoTrace plutusPayloadClosesNoFault
        , testCase "window refuses substituted chunk coordinate" $ pfails substitutedChunkCoordinate
        , testCase "window admits the exact chunk" $ passertEvalNoTrace admitsExactChunk
        , testCase "tampered checkpoint is not authentic" $ passertEvalNoTrace tamperedCheckpointIsNotAuthentic
        ]

bindsExactForcedReason :: forall s. Term s PBool
bindsExactForcedReason = boundAccused (pbindOutputV1 # forcedSubject 2 # 2) #== presultMalformed

substitutedOutputCoordinate :: forall s. Term s PBool
substitutedOutputCoordinate = boundAccused (pbindOutputV1 # forcedSubject 2 # 1) #== presultMalformed

anotherTypedReason :: forall s. Term s PBool
anotherTypedReason =
    pif
        (boundAccused (pbindOutputV1 # forcedNodeSubject 0 # 0) #== presultMalformed)
        (pconstant True)
        perror

closesWrongfulVerdicts :: forall s. Term s PBool
closesWrongfulVerdicts =
    pterminalContradictionV1
        # closedState acceptedSubject presultPending presultMalformed
        #&& pterminalContradictionV1
        # closedState (forcedSubject 0) presultMalformed presultNoFault

honestPolaritiesDoNotConvict :: forall s. Term s PBool
honestPolaritiesDoNotConvict =
    pnot
        # (pterminalContradictionV1 # closedState acceptedSubject presultPending presultNoFault)
        #&& pnot
        # (pterminalContradictionV1 # closedState (forcedSubject 0) presultMalformed presultMalformed)

checkpointRefusesSuccessorSubstitution :: forall s. Term s PBool
checkpointRefusesSuccessorSubstitution =
    plet (closedState acceptedSubject presultPending presultMalformed) $ \state ->
        pmatch state $ \PReferenceScriptScanStateV1{..} ->
            pfromData preferenceScan'checkpointHash
                #/= pcheckpointV1
                # pfromData preferenceScan'bound
                # pfromData preferenceScan'totalLength
                # pfromData preferenceScan'itemCommitment
                # pfromData preferenceScan'controlCbor
                # scanHash

exactNodeBoundary :: forall s. Term s PBool
exactNodeBoundary =
    plet (structureControl signatureNode (Scan.pmaxNativeScriptNodes - 1) 0 (pconstant "")) $ \control ->
        pmatch (Engine.pbudgetedScanV1 # control # fullWindow signatureNode # pnil # 2) $ \case
            Engine.PScanAdvancedV1 terminalData ->
                plet (pfromData terminalData) $ \terminal ->
                    pmatch terminal $ \Scan.PNativeScriptStructureControlV1{Scan.pstructure'nodeCount} ->
                        pfromData pstructure'nodeCount #== Scan.pmaxNativeScriptNodes #&& Scan.pstructureTerminalIsExactV1 # terminal
            _ -> pconstant False

adjacentNodeRefused :: forall s. Term s PBool
adjacentNodeRefused =
    plet (structureControl signatureNode Scan.pmaxNativeScriptNodes 0 (pconstant "")) $ \control ->
        pmatch (Engine.pbudgetedScanV1 # control # fullWindow signatureNode # pnil # 1) $ \case
            Engine.PScanRefusedV1 refusal -> pmappedRefusalClassV1 # pfromData refusal #== presultNodeLimit
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
                                    ( \case
                                        Engine.PScanRefusedV1 refusal -> pmappedRefusalClassV1 # pfromData refusal #== presultDepthLimit
                                        _ -> pconstant False
                                    )
                    _ -> pconstant False

emptyNativePayloadClosesMalformed :: forall s. Term s PBool
emptyNativePayloadClosesMalformed =
    plet (scanForOutput $ canonicalOutput emptyPayloadItem) $ \scan ->
        resultClass scan
            #== presultMalformed
            #&& controlCbor scan
            #== pconstant ""
            #&& totalLength scan
            #== 3
            #&& nextHash scan
            #== scanHash

signatureNativePayloadBindsPending :: forall s. Term s PBool
signatureNativePayloadBindsPending =
    plet (scanForOutput $ canonicalOutput signatureItem) $ \scan ->
        resultClass scan
            #== presultPending
            #&& controlCbor scan
            #== (Scan.pencodeStructureControlV1 # (Scan.pinitialStructureControlV1 # 4 # 32))
            #&& totalLength scan
            #== plengthBS
            # signatureItem
            #&& itemCommitment scan
            #== (Bounded.pfromBytes # poutputsFieldIndex # 0 # signatureItem)

plutusPayloadClosesNoFault :: forall s. Term s PBool
plutusPayloadClosesNoFault =
    plet (scanForOutput $ canonicalOutput plutusItem) $ \scan ->
        resultClass scan #== presultNoFault #&& controlCbor scan #== pconstant ""

substitutedChunkCoordinate :: forall s. Term s PBool
substitutedChunkCoordinate =
    plet (scanForOutput $ canonicalOutput signatureItem) $ \scan ->
        plet (controlOf scan) $ \control ->
            pmatch (proof 0 signatureItem) $ \Bounded.PChunkProofV1{..} ->
                pauthenticatedWindowV1
                    # scan
                    # control
                    # pcon
                        ( Bounded.PChunkProofV1
                            pchunkProof'version
                            pchunkProof'fieldIndex
                            (pdata 1)
                            pchunkProof'totalLength
                            pchunkProof'chunkIndex
                            pchunkProof'chunk
                            pchunkProof'frontier
                            pchunkProof'siblings
                        )
                    # pcon PDNothing
                    #== pcon (Engine.PScanWindowV1 (pdata signatureItem) (pdata 0))

admitsExactChunk :: forall s. Term s PBool
admitsExactChunk =
    plet (scanForOutput $ canonicalOutput signatureItem) $ \scan ->
        plet (pauthenticatedWindowV1 # scan # controlOf scan # proof 0 signatureItem # pcon PDNothing) $ \window ->
            window #== pcon (Engine.PScanWindowV1 (pdata signatureItem) (pdata 0))

tamperedCheckpointIsNotAuthentic :: forall s. Term s PBool
tamperedCheckpointIsNotAuthentic =
    plet (closedState acceptedSubject presultPending presultMalformed) $ \state ->
        pmatch state $ \PReferenceScriptScanStateV1{..} ->
            pscanStateIsAuthenticV1
                # state
                #&& pnot
                # ( pscanStateIsAuthenticV1
                        # pcon
                            ( PReferenceScriptScanStateV1
                                preferenceScan'bound
                                (pdata 1)
                                preferenceScan'itemCommitment
                                preferenceScan'controlCbor
                                preferenceScan'nextExpectedScriptHash
                                preferenceScan'checkpointHash
                                preferenceScan'resultClass
                            )
                  )
                #&& pnot
                # ( pscanStateIsAuthenticV1
                        # pcon
                            ( PReferenceScriptScanStateV1
                                preferenceScan'bound
                                preferenceScan'totalLength
                                preferenceScan'itemCommitment
                                preferenceScan'controlCbor
                                preferenceScan'nextExpectedScriptHash
                                (pdata acceptedTxId)
                                preferenceScan'resultClass
                            )
                  )
                #&& pnot
                # ( pscanStateIsAuthenticV1
                        # pcon
                            ( PReferenceScriptScanStateV1
                                preferenceScan'bound
                                preferenceScan'totalLength
                                preferenceScan'itemCommitment
                                (pdata $ phexByteStr "00")
                                preferenceScan'nextExpectedScriptHash
                                preferenceScan'checkpointHash
                                preferenceScan'resultClass
                            )
                  )

scanForOutput :: forall s. Term s PByteString -> Term s PReferenceScriptScanStateV1
scanForOutput output =
    pbindReferenceScriptV1 # (scanOutputToTerminal # (pinitialOutputScanV1 # (pbindOutputV1 # acceptedSubject # 0) # output) # output # 64) # output # scanHash

scanOutputToTerminal :: forall s. Term s (POutputDescriptorStateV1 :--> PByteString :--> PInteger :--> POutputDescriptorStateV1)
scanOutputToTerminal = pfix $ \self -> plam $ \state output remaining ->
    pmatch state $ \POutputDescriptorStateV1{poutputDescriptor'outcome} ->
        pif
            (pfromData poutputDescriptor'outcome #/= poutputScanning)
            state
            (pif (remaining #> 0) (self # (padvanceOutputScanV1 # state # output) # output # (remaining - 1)) perror)

closedState :: forall s. Term s Subject.PVerdictSubject -> Term s PInteger -> Term s PInteger -> Term s PReferenceScriptScanStateV1
closedState subject accused result =
    plet (pcon $ PBoundOutputV1 (pdata subject) (pdata 0) (pdata accused)) $ \bound ->
        plet (Bounded.pfromBytes # poutputsFieldIndex # 0 # signatureItem) $ \commitment ->
            pcon $
                PReferenceScriptScanStateV1
                    (pdata bound)
                    (pdata $ plengthBS # signatureItem)
                    (pdata commitment)
                    (pdata $ pconstant "")
                    (pdata finalHash)
                    (pdata $ pcheckpointV1 # bound # (plengthBS # signatureItem) # commitment # pconstant "" # finalHash)
                    (pdata result)

acceptedSubject :: forall s. Term s Subject.PVerdictSubject
acceptedSubject =
    pcon $
        Subject.PVerdictSubject
            (pdata 1)
            (pdata 0)
            (pdata 0)
            (pdata acceptedTxId)
            (pdata $ pconstant "")
            (pdata $ pcon PDNothing)

forcedSubject :: forall s. Term s PInteger -> Term s Subject.PVerdictSubject
forcedSubject index = forcedWith $ pcon $ POutputReferenceScriptMalformed $ pdata index

forcedNodeSubject :: forall s. Term s PInteger -> Term s Subject.PVerdictSubject
forcedNodeSubject index = forcedWith $ pcon $ POutputReferenceScriptNodeLimit $ pdata index

forcedWith :: forall s. Term s PRejectionReasonV1 -> Term s Subject.PVerdictSubject
forcedWith reason =
    pcon $
        Subject.PVerdictSubject
            (pdata 1)
            (pdata 1)
            (pdata 1)
            (pdata acceptedTxId)
            (pdata $ phexByteStr "01")
            (pdata $ pcon $ PDJust $ pdata reason)

boundAccused :: forall s. Term s PBoundOutputV1 -> Term s PInteger
boundAccused bound = pmatch bound $ \PBoundOutputV1{pboundOutput'accusedClass} -> pfromData pboundOutput'accusedClass

resultClass :: forall s. Term s PReferenceScriptScanStateV1 -> Term s PInteger
resultClass state = pmatch state $ \PReferenceScriptScanStateV1{preferenceScan'resultClass} -> pfromData preferenceScan'resultClass

controlCbor :: forall s. Term s PReferenceScriptScanStateV1 -> Term s PByteString
controlCbor state = pmatch state $ \PReferenceScriptScanStateV1{preferenceScan'controlCbor} -> pfromData preferenceScan'controlCbor

totalLength :: forall s. Term s PReferenceScriptScanStateV1 -> Term s PInteger
totalLength state = pmatch state $ \PReferenceScriptScanStateV1{preferenceScan'totalLength} -> pfromData preferenceScan'totalLength

itemCommitment :: forall s. Term s PReferenceScriptScanStateV1 -> Term s PByteString
itemCommitment state = pmatch state $ \PReferenceScriptScanStateV1{preferenceScan'itemCommitment} -> pfromData preferenceScan'itemCommitment

nextHash :: forall s. Term s PReferenceScriptScanStateV1 -> Term s PByteString
nextHash state = pmatch state $ \PReferenceScriptScanStateV1{preferenceScan'nextExpectedScriptHash} -> pfromData preferenceScan'nextExpectedScriptHash

controlOf :: forall s. Term s PReferenceScriptScanStateV1 -> Term s Scan.PNativeScriptStructureControlV1
controlOf state = Scan.pdecodeStructureControlV1 # controlCbor state

proof :: forall s. Term s PInteger -> Term s PByteString -> Term s Bounded.PChunkProofV1
proof index item =
    plet (Bounded.phashChunk # poutputsFieldIndex # index # 0 # item) $ \leaf ->
        pcon $
            Bounded.PChunkProofV1
                (pdata Bounded.pversion)
                (pdata poutputsFieldIndex)
                (pdata index)
                (pdata $ plengthBS # item)
                (pdata 0)
                (pdata item)
                (pdata $ pappendLeaf # 0 # pnil # leaf)
                (pdata pnil)

fullWindow :: forall s. Term s PByteString -> Term s (PMaybe Engine.PScanWindowV1)
fullWindow item = pcon $ PJust $ pcon $ Engine.PScanWindowV1 (pdata item) (pdata 0)

rootFrame :: forall s. Term s PInteger -> Term s Scan.PNativeScriptFrameV1
rootFrame remaining = pcon $ Scan.PNativeScriptFrameV1 (pdata $ pconstant "") (pdata Scan.pallNode) (pdata 2) (pdata remaining) (pdata 0) (pdata 0)

structureControl :: forall s. Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s Scan.PNativeScriptStructureControlV1
structureControl payload nodeCount depth stackRoot =
    pcon $
        Scan.PNativeScriptStructureControlV1
            (pdata 1)
            (pdata Scan.pstructureStageToken)
            (pdata 0)
            (pdata 0)
            (pdata $ plengthBS # payload)
            (pdata stackRoot)
            (pdata depth)
            (pdata nodeCount)

canonicalOutput :: forall s. Term s PByteString -> Term s PByteString
canonicalOutput item = outputHead <> item

acceptedTxId, finalHash, scanHash, outputHead :: forall s. Term s PByteString
acceptedTxId = phexByteStr "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
scanHash = phexByteStr "11111111111111111111111111111111111111111111111111111111"
finalHash = phexByteStr "22222222222222222222222222222222222222222222222222222222"
outputHead = phexByteStr "a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a003"

signatureItem, signatureNode, allOfTwoPayload, emptyPayloadItem, plutusItem :: forall s. Term s PByteString
signatureItem = bytes $ "82005820" <> signatureNodeHex
signatureNode = bytes signatureNodeHex
allOfTwoPayload = bytes $ "820182" <> signatureNodeHex <> signatureNodeHex
emptyPayloadItem = phexByteStr "820040"
plutusItem = phexByteStr "82034401020304"

signatureNodeHex :: BS.ByteString
signatureNodeHex = "8200581c" <> BS.concat (replicate 28 "99")

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
