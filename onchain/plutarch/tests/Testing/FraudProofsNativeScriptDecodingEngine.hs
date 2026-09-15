{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsNativeScriptDecodingEngine (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils ((#/=))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.BoundedItem qualified as Bounded
import Midgard.CekData (PDataSummaryV1 (..))
import Midgard.FraudProofs.NativeScriptDecoding.Engine
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pledgerOutputCommitmentVersion, poutputFieldIndex)
import Midgard.NativeScriptScan qualified as Scan
import Midgard.ValidationMerkle (pappendLeaf)
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests = testGroup "Native-script decoding engine"
  [ testCase "scans a signature script to the exact terminal" $ passertEvalNoTrace scansSignature
  , testCase "scans a container through its frames" $ passertEvalNoTrace scansContainer
  , testCase "resumes from a budget stop" $ passertEvalNoTrace resumesAfterBudget
  , testCase "refuses a malformed payload" $ passertEvalNoTrace refusesMalformed
  , testCase "classifies the node limit" $ passertEvalNoTrace classifiesNodeLimit
  , testCase "classifies the depth limit" $ passertEvalNoTrace classifiesDepthLimit
  , testCase "stops on a truncated window instead of refusing" $ passertEvalNoTrace stopsOnTruncation
  , testCase "stops without a window" $ passertEvalNoTrace stopsWithoutWindow
  , testCase "bind machine dispatches on the wrapper" $ passertEvalNoTrace bindDispatch
  , testCase "authenticates a single-chunk window" $ passertEvalNoTrace authenticatesWindow
  , testCase "rejects a phantom next chunk" $ pfails phantomNextChunk
  , testCase "thread-state encoding is flat and injective" $ passertEvalNoTrace stateEncoding
  ]

scansSignature :: forall s. Term s PBool
scansSignature = plet (boundControl signatureItem) $ \control ->
  pmatch (pbudgetedScanV1 # control # fullWindow signatureItem # pnil # 3) $ \case
    PScanAdvancedV1 terminalD -> plet (pfromData terminalD) $ \terminal -> pmatch terminal $ \t ->
      Scan.pstructureTerminalIsExactV1 # terminal
        #&& pfromData (Scan.pstructure'nodeCount t) #== 1
    _ -> pconstant False

scansContainer :: forall s. Term s PBool
scansContainer = plet (boundControl allOfTwoItem) $ \control ->
  plet (rootFrame 2) $ \firstFrame ->
  plet (rootFrame 1) $ \secondFrame ->
  pmatch (pbudgetedScanV1 # control # fullWindow allOfTwoItem
    # (pcons # pdata firstFrame #$ pcons # pdata secondFrame # pnil) # 6) $ \case
      PScanAdvancedV1 terminalD -> plet (pfromData terminalD) $ \terminal -> pmatch terminal $ \t ->
        Scan.pstructureTerminalIsExactV1 # terminal
          #&& pfromData (Scan.pstructure'nodeCount t) #== 3
      _ -> pconstant False

resumesAfterBudget :: forall s. Term s PBool
resumesAfterBudget = plet (boundControl allOfTwoItem) $ \control ->
  pmatch (pbudgetedScanV1 # control # fullWindow allOfTwoItem # pnil # 2) $ \case
    PScanAdvancedV1 midD -> plet (pfromData midD) $ \mid -> pmatch mid $ \m ->
      pfromData (Scan.pstructure'stage m) #== Scan.pstructureStageFrame
        #&& pmatch (pbudgetedScanV1 # mid # fullWindow allOfTwoItem
          # (pcons # pdata (rootFrame 2) #$ pcons # pdata (rootFrame 1) # pnil) # 10) (\case
            PScanAdvancedV1 terminalD -> Scan.pstructureTerminalIsExactV1 # pfromData terminalD
            _ -> pconstant False)
    _ -> pconstant False

refusesMalformed :: forall s. Term s PBool
refusesMalformed = plet (boundControl malformedPayloadItem) $ \control ->
  pbudgetedScanV1 # control # fullWindow malformedPayloadItem # pnil # 1
    #== pcon (PScanRefusedV1 $ pdata prefusalClassMalformed)

classifiesNodeLimit :: forall s. Term s PBool
classifiesNodeLimit = plet nodeLimitControl $ \control ->
  pbudgetedScanV1 # control # fullWindow signatureNode # pnil # 1
    #== pcon (PScanRefusedV1 $ pdata prefusalClassNodeLimit)

classifiesDepthLimit :: forall s. Term s PBool
classifiesDepthLimit = plet (rootFrame 2) $ \frame ->
  plet (Scan.phashFrameV1 # frame) $ \occupiedRoot ->
  plet (pcon $ Scan.PNativeScriptStructureControlV1
    (pdata 1) (pdata Scan.pstructureStageToken) (pdata 0) (pdata 0)
    (pdata $ plengthBS # allOfTwoPayload) (pdata occupiedRoot)
    (pdata Scan.pmaxNativeScriptDepth) (pdata 1)) $ \control ->
      pbudgetedScanV1 # control # fullWindow allOfTwoPayload # pnil # 1
        #== pcon (PScanRefusedV1 $ pdata prefusalClassDepthLimit)

stopsOnTruncation :: forall s. Term s PBool
stopsOnTruncation = plet (boundControl signatureItem) $ \control ->
  plet (pcon $ PJust $ pcon $ PScanWindowV1 (pdata $ psliceBS # 0 # 14 # signatureItem) (pdata 0)) $ \truncated ->
    pbudgetedScanV1 # control # truncated # pnil # 5
      #== pcon (PScanAdvancedV1 $ pdata control)

stopsWithoutWindow :: forall s. Term s PBool
stopsWithoutWindow = plet (boundControl signatureItem) $ \control ->
  pbudgetedScanV1 # control # pcon PNothing # pnil # 5
    #== pcon (PScanAdvancedV1 $ pdata control)

bindDispatch :: forall s. Term s PBool
bindDispatch = pmatch (pbindMachineV1 # signatureItem # (plengthBS # signatureItem)) $ \case
  PMachineBoundV1 controlD -> plet (pfromData controlD) $ \control -> pmatch control $ \c ->
    pfromData (Scan.pstructure'startOffset c) #== 4
      #&& pfromData (Scan.pstructure'cursor c) #== 4
      #&& pfromData (Scan.pstructure'endOffset c) #== plengthBS # signatureItem
      #&& pbindMachineV1 # malformedWrapperItem # (plengthBS # malformedWrapperItem) #== pcon PMachineBindMalformedV1
      #&& pbindMachineV1 # emptyPayloadItem # (plengthBS # emptyPayloadItem) #== pcon PMachineBindMalformedV1
      #&& pbindMachineV1 # plutusItem # (plengthBS # plutusItem) #== pcon (PMachineBindNonNativeV1 $ pdata 3)
  _ -> pconstant False

authenticatesWindow :: forall s. Term s PBool
authenticatesWindow = plet (Bounded.pfromBytes # poutputFieldIndex # 0 # signatureItem) $ \commitment ->
  pauthenticatedScanWindowV1 # 0 # (plengthBS # signatureItem) # commitment # 4
    # singleChunkProof # pcon PNothing
    #== pcon (PScanWindowV1 (pdata signatureItem) (pdata 0))

phantomNextChunk :: forall s. Term s PBool
phantomNextChunk = plet (Bounded.pfromBytes # poutputFieldIndex # 0 # signatureItem) $ \commitment ->
  pauthenticatedScanWindowV1 # 0 # (plengthBS # signatureItem) # commitment # 4
    # singleChunkProof # pcon (PJust singleChunkProof)
    #== pcon (PScanWindowV1 (pdata signatureItem) (pdata 0))

stateEncoding :: forall s. Term s PBool
stateEncoding = plet
  (ppreBindScanStateV1 # pdirectionWrongfulAcceptance # psourceKindNormal # h32 # pconstant ""
    # pclassPending # h32 # poutpointSourceSpend # 0) $ \pre ->
  plet (popenedSubjectScanStateV1 # pre # bytes "8258205555" # 0) $ \opened ->
  plet (pboundDescriptorScanStateV1 # opened # scanDescriptor) $ \bound ->
  plet (pencodeScanThreadStateV1 # bound) $ \boundBytes ->
  pmatch bound $ \b ->
    plet (pcon $ PScanThreadStateV1
      (pdata pdirectionWrongfulRejection) (pscanState'sourceKind b) (pscanState'verifiedTxId b)
      (pscanState'txOrderId b) (pscanState'scanReasonClass b) (pscanState'priorLedgerRoot b)
      (pscanState'outpointSourceKind b) (pscanState'outpointCursor b) (pscanState'outpointKeyHash b)
      (pscanState'referenceScriptLanguage b) (pscanState'outputIndex b) (pscanState'totalLength b)
      (pscanState'itemCommitment b) (pscanState'machineStateHash b) (pscanState'refusalClass b)) $ \opposite ->
      plengthBS # boundBytes #< 300
        #&& pencodeScanThreadStateV1 # pre #/= boundBytes
        #&& pencodeScanThreadStateV1 # opposite #/= boundBytes

boundControl :: forall s. Term s PByteString -> Term s Scan.PNativeScriptStructureControlV1
boundControl item = pmatch (pbindMachineV1 # item # (plengthBS # item)) $ \case
  PMachineBoundV1 control -> pfromData control
  _ -> perror

fullWindow :: forall s. Term s PByteString -> Term s (PMaybe PScanWindowV1)
fullWindow item = pcon $ PJust $ pcon $ PScanWindowV1 (pdata item) (pdata 0)

rootFrame :: forall s. Term s PInteger -> Term s Scan.PNativeScriptFrameV1
rootFrame remaining = pcon $ Scan.PNativeScriptFrameV1
  (pdata $ pconstant "") (pdata Scan.pallNode) (pdata 2) (pdata remaining) (pdata 0) (pdata 0)

nodeLimitControl :: forall s. Term s Scan.PNativeScriptStructureControlV1
nodeLimitControl = pcon $ Scan.PNativeScriptStructureControlV1
  (pdata 1) (pdata Scan.pstructureStageToken) (pdata 0) (pdata 0) (pdata $ plengthBS # signatureNode)
  (pdata $ pconstant "") (pdata 0) (pdata Scan.pmaxNativeScriptNodes)

singleChunkProof :: forall s. Term s Bounded.PChunkProofV1
singleChunkProof = plet (Bounded.phashChunk # poutputFieldIndex # 0 # 0 # signatureItem) $ \leaf ->
  pcon $ Bounded.PChunkProofV1
    (pdata Bounded.pversion) (pdata poutputFieldIndex) (pdata 0) (pdata $ plengthBS # signatureItem)
    (pdata 0) (pdata signatureItem) (pdata $ pappendLeaf # 0 # pnil # leaf) (pdata pnil)

scanDescriptor :: forall s. Term s PLedgerOutputCommitmentV1
scanDescriptor = pcon $ PLedgerOutputCommitmentV1
  (pdata pledgerOutputCommitmentVersion) (pdata 0) (pdata 5_000) (pdata h32)
  (pdata $ bytes "6011111111111111111111111111111111111111111111111111111111")
  (pdata 5_000_000) (pdata 0)
  (pdata $ bytes "b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd")
  (pdata 5) (pdata 0) (pdata $ bytes "99999999999999999999999999999999999999999999999999999999")
  (pdata $ plengthBS # signatureItem) (pdata $ Bounded.pfromBytes # poutputFieldIndex # 0 # signatureItem)
  (pdata $ summary 101 202) (pdata $ summary 103 204) (pdata $ summary 3 4)

summary :: forall s. Term s PInteger -> Term s PInteger -> Term s PDataSummaryV1
summary cborLength memory = pcon $ PDataSummaryV1 (pdata h32) (pdata cborLength) (pdata memory)

signatureItem, signatureNode, allOfTwoItem, allOfTwoPayload, malformedPayloadItem :: forall s. Term s PByteString
signatureItem = bytes $ "82005820" <> signatureNodeHex
signatureNode = bytes signatureNodeHex
allOfTwoItem = bytes $ "82005843" <> allOfTwoPayloadHex
allOfTwoPayload = bytes allOfTwoPayloadHex
malformedPayloadItem = bytes "820043820700"

malformedWrapperItem, emptyPayloadItem, plutusItem, h32 :: forall s. Term s PByteString
malformedWrapperItem = bytes "8201410a"
emptyPayloadItem = bytes "820040"
plutusItem = bytes "82034401020304"
h32 = bytes $ BS.concat $ replicate 32 "55"

signatureNodeHex, allOfTwoPayloadHex :: BS.ByteString
signatureNodeHex = "8200581c" <> BS.concat (replicate 28 "99")
allOfTwoPayloadHex = "820182" <> signatureNodeHex <> signatureNodeHex

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
