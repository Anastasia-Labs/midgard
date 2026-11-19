{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsWitnessScriptDecoding (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.NativeScriptDecoding.Engine qualified as Engine
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.WitnessScriptDecoding
import Midgard.NativeScriptScan qualified as Scan
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Midgard.ValidationMerkle (pappendLeaf)
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
  testGroup
    "Witness-script decoding rule"
    [ testCase "binds the exact forced reason and coordinate" $ passertEvalNoTrace bindsExactForcedReason
    , testCase "refuses a substituted forced reason coordinate" $ pfails substitutedForcedCoordinate
    , testCase "refuses another typed reason" $ pfails anotherTypedReason
    , testCase "distinguishes header payload and non-native results" $ passertEvalNoTrace distinguishesResults
    , testCase "frozen scan reaches exact terminal through authenticated chunk" $ passertEvalNoTrace reachesExactTerminal
    , testCase "frozen scan preserves native malformed class" $ passertEvalNoTrace preservesNativeMalformed
    , testCase "refuses a substituted item chunk" $ pfails substitutedItemChunk
    , testCase "checkpoint binds the next expected script" $ passertEvalNoTrace checkpointBindsSuccessor
    , testCase "closes both verdict directions with exact polarity" $ passertEvalNoTrace closesBothDirections
    , testCase "honest verdicts do not convict" $ passertEvalNoTrace honestVerdictsDoNotConvict
    , testCase "exact node boundary is admitted" $ passertEvalNoTrace exactNodeBoundary
    , testCase "adjacent node over bound is refused" $ passertEvalNoTrace adjacentNodeRefused
    , testCase "exact depth boundary is admitted" $ passertEvalNoTrace exactDepthBoundary
    , testCase "adjacent depth over bound is refused" $ passertEvalNoTrace adjacentDepthRefused
    , testCase "empty tag-0 payload is native malformed" $ passertEvalNoTrace emptyPayloadIsNativeMalformed
    , testCase "binds every accused class and contradicts only a different result" $ passertEvalNoTrace everyAccusedClass
    , testCase "refuses a negative coordinate" $ pfails negativeCoordinate
    , testCase "refuses a substituted witness-set hash width" $ pfails shortWitnessHash
    , testCase "refuses a malformed checkpoint" $ pfails malformedCheckpoint
    , testCase "refuses a substituted successor in the checkpoint" $ pfails substitutedSuccessor
    , testCase "closed non-native result is honest for an accepted block" $ passertEvalNoTrace closedNonNativeIsHonest
    ]

bindsExactForcedReason :: forall s. Term s PBool
bindsExactForcedReason = boundAccused (pbindSubjectV1 # forcedHeaderSubject # witnessHash # 0) #== presultHeaderMalformed

substitutedForcedCoordinate :: forall s. Term s PBool
substitutedForcedCoordinate = boundAccused (pbindSubjectV1 # forcedHeaderSubject # witnessHash # 1) #== presultHeaderMalformed

anotherTypedReason :: forall s. Term s PBool
anotherTypedReason =
  pif
    ( boundAccused (pbindSubjectV1 # forcedSubject (pcon $ PWitnessNativeScriptMalformed $ pdata 0) # witnessHash # 0)
        #== presultHeaderMalformed
    )
    (pconstant True)
    perror

distinguishesResults :: forall s. Term s PBool
distinguishesResults =
  plet (pbindSubjectV1 # acceptedSubject # witnessHash # 0) $ \bound ->
    plet (pauthenticateItemV1 # bound # malformedWrapperItem # scanHash) $ \header ->
      plet (pauthenticateItemV1 # bound # malformedPayloadItem # scanHash) $ \payload ->
        plet (pauthenticateItemV1 # bound # plutusItem # scanHash) $ \plutus ->
          resultClass header
            #== presultHeaderMalformed
            #&& resultClass payload
            #== presultPending
            #&& resultClass plutus
            #== presultNoFault
            #&& pstateIsAuthenticV1
            # header
            #&& pstateIsAuthenticV1
            # payload
            #&& pstateIsAuthenticV1
            # plutus

reachesExactTerminal :: forall s. Term s PBool
reachesExactTerminal =
  plet (pauthenticatedSignatureState acceptedSubject) $ \state ->
    plet (controlOf state) $ \control ->
      pmatch (Engine.pbudgetedScanV1 # control # (pcon $ PJust $ pauthenticatedWindowV1 # state # control # proof signatureItem # pcon PDNothing) # pnil # 2) $ \case
        Engine.PScanAdvancedV1 terminal -> Scan.pstructureTerminalIsExactV1 # pfromData terminal
        _ -> pconstant False

preservesNativeMalformed :: forall s. Term s PBool
preservesNativeMalformed =
  plet (pbindSubjectV1 # acceptedSubject # witnessHash # 0) $ \bound ->
    plet (pauthenticateItemV1 # bound # malformedPayloadItem # scanHash) $ \state ->
      plet (controlOf state) $ \control ->
        pmatch (Engine.pbudgetedScanV1 # control # (pcon $ PJust $ pauthenticatedWindowV1 # state # control # proof malformedPayloadItem # pcon PDNothing) # pnil # 1) $ \case
          Engine.PScanRefusedV1 refusal -> pmappedRefusalClassV1 # pfromData refusal #== presultNativeMalformed
          _ -> pconstant False

substitutedItemChunk :: forall s. Term s PBool
substitutedItemChunk =
  plet (pauthenticatedSignatureState acceptedSubject) $ \state ->
    plet (controlOf state) $ \control ->
      pauthenticatedWindowV1
        # state
        # control
        # proof malformedPayloadItem
        # pcon PDNothing
        #== pcon (Engine.PScanWindowV1 (pdata malformedPayloadItem) (pdata 0))

checkpointBindsSuccessor :: forall s. Term s PBool
checkpointBindsSuccessor =
  plet (pauthenticatedSignatureState acceptedSubject) $ \state ->
    pmatch state $ \PWitnessScriptScanStateV1{..} ->
      pstateIsAuthenticV1
        # state
        #&& pfromData pwitnessScan'checkpointHash
        #/= pcheckpointV1
        # pfromData pwitnessScan'bound
        # pfromData pwitnessScan'totalLength
        # pfromData pwitnessScan'itemCommitment
        # pfromData pwitnessScan'controlCbor
        # finalHash

closesBothDirections :: forall s. Term s PBool
closesBothDirections =
  plet (pbindSubjectV1 # acceptedSubject # witnessHash # 0) $ \acceptedBound ->
    plet (pauthenticateItemV1 # acceptedBound # malformedWrapperItem # scanHash) $ \malformed ->
      plet (pclosedStateV1 # malformed # resultClass malformed # finalHash) $ \acceptedClosed ->
        plet (pbindSubjectV1 # forcedHeaderSubject # witnessHash # 0) $ \forcedBound ->
          plet (pauthenticateItemV1 # forcedBound # signatureItem # scanHash) $ \valid ->
            pterminalContradictionV1
              # acceptedClosed
              #&& pterminalContradictionV1
              # (pclosedStateV1 # valid # presultNoFault # finalHash)

honestVerdictsDoNotConvict :: forall s. Term s PBool
honestVerdictsDoNotConvict =
  plet (pbindSubjectV1 # acceptedSubject # witnessHash # 0) $ \acceptedBound ->
    plet (pauthenticateItemV1 # acceptedBound # plutusItem # scanHash) $ \valid ->
      plet (pclosedStateV1 # valid # presultNoFault # finalHash) $ \acceptedClosed ->
        plet (pbindSubjectV1 # forcedHeaderSubject # witnessHash # 0) $ \forcedBound ->
          plet (pauthenticateItemV1 # forcedBound # malformedWrapperItem # scanHash) $ \malformed ->
            pnot
              # (pterminalContradictionV1 # acceptedClosed)
              #&& pnot
              # (pterminalContradictionV1 # (pclosedStateV1 # malformed # resultClass malformed # finalHash))

exactNodeBoundary :: forall s. Term s PBool
exactNodeBoundary =
  plet (structureControl signatureNode (Scan.pmaxNativeScriptNodes - 1) 0 (pconstant "")) $ \control ->
    pmatch (Engine.pbudgetedScanV1 # control # fullWindow signatureNode # pnil # 2) $ \case
      Engine.PScanAdvancedV1 terminalD ->
        plet (pfromData terminalD) $ \terminal ->
          pmatch terminal $ \Scan.PNativeScriptStructureControlV1{Scan.pstructure'nodeCount} ->
            pfromData pstructure'nodeCount
              #== Scan.pmaxNativeScriptNodes
              #&& Scan.pstructureTerminalIsExactV1
              # terminal
      _ -> pconstant False

adjacentNodeRefused :: forall s. Term s PBool
adjacentNodeRefused =
  plet (structureControl signatureNode Scan.pmaxNativeScriptNodes 0 (pconstant "")) $ \control ->
    pmatch (Engine.pbudgetedScanV1 # control # fullWindow signatureNode # pnil # 1) $ \case
      Engine.PScanRefusedV1 refusal -> pmappedRefusalClassV1 # pfromData refusal #== presultNodeLimit
      _ -> pconstant False

exactDepthBoundary :: forall s. Term s PBool
exactDepthBoundary =
  plet (Scan.phashFrameV1 # rootFrame 2) $ \occupiedRoot ->
    plet (structureControl allOfTwoPayload 1 (Scan.pmaxNativeScriptDepth - 1) occupiedRoot) $ \control ->
      pmatch (Engine.pbudgetedScanV1 # control # fullWindow allOfTwoPayload # pnil # 1) $ \case
        Engine.PScanAdvancedV1 nextD ->
          pmatch (pfromData nextD) $ \Scan.PNativeScriptStructureControlV1{Scan.pstructure'stackDepth} ->
            pfromData pstructure'stackDepth #== Scan.pmaxNativeScriptDepth
        _ -> pconstant False

adjacentDepthRefused :: forall s. Term s PBool
adjacentDepthRefused =
  plet (Scan.phashFrameV1 # rootFrame 2) $ \occupiedRoot ->
    plet (structureControl allOfTwoPayload 1 Scan.pmaxNativeScriptDepth occupiedRoot) $ \control ->
      pmatch (Engine.pbudgetedScanV1 # control # fullWindow allOfTwoPayload # pnil # 1) $ \case
        Engine.PScanRefusedV1 refusal -> pmappedRefusalClassV1 # pfromData refusal #== presultDepthLimit
        _ -> pconstant False

emptyPayloadIsNativeMalformed :: forall s. Term s PBool
emptyPayloadIsNativeMalformed =
  plet (pauthenticateItemV1 # (pbindSubjectV1 # acceptedSubject # witnessHash # 0) # emptyPayloadItem # scanHash) $ \accepted ->
    plet (pauthenticateItemV1 # (pbindSubjectV1 # forcedSubject (pcon $ PWitnessNativeScriptMalformed $ pdata 0) # witnessHash # 0) # emptyPayloadItem # scanHash) $ \honest ->
      plet (pauthenticateItemV1 # (pbindSubjectV1 # forcedHeaderSubject # witnessHash # 0) # emptyPayloadItem # scanHash) $ \wrongClass ->
        resultClass accepted
          #== presultNativeMalformed
          #&& controlCbor accepted
          #== pconstant ""
          #&& pstateIsAuthenticV1
          # accepted
          #&& pterminalContradictionV1
          # (pclosedStateV1 # accepted # resultClass accepted # finalHash)
          #&& pnot
          # (pterminalContradictionV1 # (pclosedStateV1 # honest # resultClass honest # finalHash))
          #&& pterminalContradictionV1
          # (pclosedStateV1 # wrongClass # resultClass wrongClass # finalHash)

everyAccusedClass :: forall s. Term s PBool
everyAccusedClass =
  classCase (pcon $ PWitnessScriptHeaderMalformed $ pdata 2) presultHeaderMalformed
    #&& classCase (pcon $ PWitnessNativeScriptMalformed $ pdata 2) presultNativeMalformed
    #&& classCase (pcon $ PWitnessNativeScriptNodeLimit $ pdata 2) presultNodeLimit
    #&& classCase (pcon $ PWitnessNativeScriptDepthLimit $ pdata 2) presultDepthLimit

negativeCoordinate :: forall s. Term s PBool
negativeCoordinate = boundAccused (pbindSubjectV1 # acceptedSubject # witnessHash # (-1)) #== presultPending

shortWitnessHash :: forall s. Term s PBool
shortWitnessHash = boundAccused (pbindSubjectV1 # acceptedSubject # (psliceBS # 1 # 31 # witnessHash) # 0) #== presultPending

malformedCheckpoint :: forall s. Term s PBool
malformedCheckpoint =
  plet (pauthenticateItemV1 # (pbindSubjectV1 # acceptedSubject # witnessHash # 0) # malformedWrapperItem # scanHash) $ \opened ->
    plet (pclosedStateV1 # opened # presultHeaderMalformed # finalHash) $ \state ->
      pmatch state $ \PWitnessScriptScanStateV1{..} ->
        pterminalContradictionV1
          # pcon
            ( PWitnessScriptScanStateV1
                pwitnessScan'bound
                pwitnessScan'totalLength
                pwitnessScan'itemCommitment
                pwitnessScan'controlCbor
                pwitnessScan'nextExpectedScriptHash
                (pdata witnessHash)
                pwitnessScan'resultClass
            )

substitutedSuccessor :: forall s. Term s PBool
substitutedSuccessor =
  plet (pauthenticatedSignatureState acceptedSubject) $ \state ->
    pmatch state $ \PWitnessScriptScanStateV1{..} ->
      pif
        ( pstateIsAuthenticV1
            # pcon
              ( PWitnessScriptScanStateV1
                  pwitnessScan'bound
                  pwitnessScan'totalLength
                  pwitnessScan'itemCommitment
                  pwitnessScan'controlCbor
                  (pdata finalHash)
                  pwitnessScan'checkpointHash
                  pwitnessScan'resultClass
              )
        )
        (pconstant True)
        perror

closedNonNativeIsHonest :: forall s. Term s PBool
closedNonNativeIsHonest =
  pnot
    # ( pterminalContradictionV1
          # ( pclosedStateV1
                # (pauthenticateItemV1 # (pbindSubjectV1 # acceptedSubject # witnessHash # 0) # plutusItem # scanHash)
                # presultNoFault
                # finalHash
            )
      )

classCase :: forall s. Term s PRejectionReasonV1 -> Term s PInteger -> Term s PBool
classCase reason accused =
  plet (pbindSubjectV1 # forcedSubject reason # witnessHash # 2) $ \bound ->
    plet (pauthenticateItemV1 # bound # signatureItem # scanHash) $ \state ->
      let other = pif (accused #== presultNodeLimit) presultDepthLimit presultNodeLimit
       in boundAccused bound
            #== accused
            #&& pterminalContradictionV1
            # (pclosedStateV1 # state # presultNoFault # finalHash)
            #&& pterminalContradictionV1
            # (pclosedStateV1 # state # other # finalHash)
            #&& pnot
            # (pterminalContradictionV1 # (pclosedStateV1 # state # accused # finalHash))

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

forcedHeaderSubject :: forall s. Term s Subject.PVerdictSubject
forcedHeaderSubject = forcedSubject $ pcon $ PWitnessScriptHeaderMalformed $ pdata 0

forcedSubject :: forall s. Term s PRejectionReasonV1 -> Term s Subject.PVerdictSubject
forcedSubject reason =
  pcon $
    Subject.PVerdictSubject
      (pdata 1)
      (pdata 1)
      (pdata 1)
      (pdata acceptedTxId)
      (pdata $ phexByteStr "01")
      (pdata $ pcon $ PDJust $ pdata reason)

pauthenticatedSignatureState :: forall s. Term s Subject.PVerdictSubject -> Term s PWitnessScriptScanStateV1
pauthenticatedSignatureState subject = pauthenticateItemV1 # (pbindSubjectV1 # subject # witnessHash # 0) # signatureItem # scanHash

boundAccused :: forall s. Term s PBoundWitnessScriptV1 -> Term s PInteger
boundAccused bound = pmatch bound $ \PBoundWitnessScriptV1{pboundWitness'accusedClass} -> pfromData pboundWitness'accusedClass

resultClass :: forall s. Term s PWitnessScriptScanStateV1 -> Term s PInteger
resultClass state = pmatch state $ \PWitnessScriptScanStateV1{pwitnessScan'resultClass} -> pfromData pwitnessScan'resultClass

controlCbor :: forall s. Term s PWitnessScriptScanStateV1 -> Term s PByteString
controlCbor state = pmatch state $ \PWitnessScriptScanStateV1{pwitnessScan'controlCbor} -> pfromData pwitnessScan'controlCbor

controlOf :: forall s. Term s PWitnessScriptScanStateV1 -> Term s Scan.PNativeScriptStructureControlV1
controlOf state = Scan.pdecodeStructureControlV1 # controlCbor state

proof :: forall s. Term s PByteString -> Term s Bounded.PChunkProofV1
proof item =
  plet (Bounded.phashChunk # pfieldIndex # 0 # 0 # item) $ \leaf ->
    pcon $
      Bounded.PChunkProofV1
        (pdata Bounded.pversion)
        (pdata pfieldIndex)
        (pdata 0)
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
structureControl bytes nodeCount depth stackRoot =
  pcon $
    Scan.PNativeScriptStructureControlV1
      (pdata 1)
      (pdata Scan.pstructureStageToken)
      (pdata 0)
      (pdata 0)
      (pdata $ plengthBS # bytes)
      (pdata stackRoot)
      (pdata depth)
      (pdata nodeCount)

acceptedTxId, witnessHash, finalHash, scanHash :: forall s. Term s PByteString
acceptedTxId = phexByteStr "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
witnessHash = phexByteStr "1111111111111111111111111111111111111111111111111111111111111111"
finalHash = phexByteStr "22222222222222222222222222222222222222222222222222222222"
scanHash = phexByteStr "33333333333333333333333333333333333333333333333333333333"

signatureItem, signatureNode, allOfTwoPayload, malformedPayloadItem, malformedWrapperItem, emptyPayloadItem, plutusItem :: forall s. Term s PByteString
signatureItem = bytes $ "82005820" <> signatureNodeHex
signatureNode = bytes signatureNodeHex
allOfTwoPayload = bytes $ "820182" <> signatureNodeHex <> signatureNodeHex
malformedPayloadItem = phexByteStr "820043820700"
malformedWrapperItem = phexByteStr "8201410a"
emptyPayloadItem = phexByteStr "820040"
plutusItem = phexByteStr "82034401020304"

signatureNodeHex :: BS.ByteString
signatureNodeHex = "8200581c" <> BS.concat (replicate 28 "99")

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
