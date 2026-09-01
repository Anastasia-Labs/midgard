{-# LANGUAGE OverloadedStrings #-}

module Testing.CekDataTraverse (tests) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Control.Monad (replicateM, unless)
import Data.Kind (Type)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Plutarch.Core.Utils (pand'List)
import Plutarch.Evaluate (applyArguments, evalScriptHuge)
import Plutarch.Internal.Other (printScript)
import Plutarch.Internal.Term (Config (NoTracing), compile)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Plutarch.Script (Script)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.CekData qualified as Data
import Midgard.CekDataBytes qualified as Bytes
import Midgard.CekDataFrame qualified as Frame
import Midgard.CekDataInteger qualified as Integer
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.CekSourceBlob qualified as Blob
import Midgard.FraudProofs.NativeTx.Codec (psliceLen)
import Midgard.ValidationMerkle qualified as Merkle
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.CekDataTraverse"
  [ traverseV1ParityTests
  , maximumCardanoParityTests
  , c26UnaryDepthParityTests
  , breadthParityTests
  ]

--------------------------------------------------------------------------------
-- cek-data-traverse-v1.test.ak
--------------------------------------------------------------------------------

traverseV1ParityTests :: TestTree
traverseV1ParityTests =
  testGroup
    "cek-data-traverse-v1 Aiken parity"
    [ testCase
        "streams_large_constructor_before_fields_and_matches_typescript"
        streamsLargeConstructorRuntime
    , testCase
        "pops_an_authenticated_empty_child_container_into_parent"
        popsEmptyChildRuntime
    , testCase
        "maximum_delegated_source_window_is_132_bytes"
        maximumSourceWindowRuntime
    , testCase "decodes_active_typescript_nested_controls_canonically" $
        passertEvalNoTrace decodesActiveControls
    , testCase
        "malformed_counts_trailing_bytes_and_small_large_constructor_fail_closed"
        malformedInputsRuntime
    ]

--------------------------------------------------------------------------------
-- cek-data-traverse.max-cardano.test.ak
--------------------------------------------------------------------------------

maximumCardanoParityTests :: TestTree
maximumCardanoParityTests =
  testGroup
    "cek-data-traverse.max-cardano Aiken parity"
    [ testCase
        "maximum_cardano_nested_data_applies_real_head_controls"
        maximumHeadControlsRuntime
    , testCase
        "maximum_cardano_nested_data_applies_real_fold_controls"
        maximumFoldControlsRuntime
    , testCase
        "maximum_cardano_nested_data_terminal_matches_typescript"
        maximumDataTerminalRuntime
    , testCase
        "maximum_cardano_nested_redeemer_terminal_matches_typescript"
        maximumRedeemerTerminalRuntime
    ]

--------------------------------------------------------------------------------
-- fraud-proofs/c26-unary-depth-v1.test.ak
--------------------------------------------------------------------------------

c26UnaryDepthParityTests :: TestTree
c26UnaryDepthParityTests =
  testGroup
    "c26-unary-depth-v1 Aiken parity"
    [ testCase "maximum_unary_depth_is_bound_by_signed_cardano_capacity" $ do
        let acceptedDepth, acceptedDatumBytes, adjacentDepth, adjacentDatumBytes :: Integer
            signedTransactionOverheadBytes, maximumCardanoTransactionBytes :: Integer
            acceptedDepth = 4_043
            acceptedDatumBytes = 16_173
            adjacentDepth = 4_044
            adjacentDatumBytes = 16_177
            signedTransactionOverheadBytes = 211
            maximumCardanoTransactionBytes = 16_384
        acceptedDatumBytes @?= acceptedDepth * 4 + 1
        adjacentDepth @?= acceptedDepth + 1
        adjacentDatumBytes @?= acceptedDatumBytes + 4
        acceptedDatumBytes + signedTransactionOverheadBytes
          @?= maximumCardanoTransactionBytes
        assertBool
          "the adjacent unary datum exceeds signed Cardano capacity"
          (adjacentDatumBytes + signedTransactionOverheadBytes > maximumCardanoTransactionBytes)
    , testCase
        "maximum_unary_depth_terminal_agrees_with_typescript_and_rejects_mutation"
        maximumUnaryDepthTerminalRuntime
    , testCase "maximum_unary_redeemer_depth_is_bound_by_signed_cardano_capacity" $ do
        let acceptedDepth, acceptedDataBytes, adjacentDepth, adjacentDataBytes :: Integer
            signedDatumOverheadBytes, signedRedeemerOverheadBytes :: Integer
            maximumCardanoTransactionBytes :: Integer
            acceptedDepth = 3_995
            acceptedDataBytes = 15_981
            adjacentDepth = 3_996
            adjacentDataBytes = 15_985
            signedDatumOverheadBytes = 211
            signedRedeemerOverheadBytes = 400
            maximumCardanoTransactionBytes = 16_384
        acceptedDataBytes @?= acceptedDepth * 4 + 1
        adjacentDepth @?= acceptedDepth + 1
        adjacentDataBytes @?= acceptedDataBytes + 4
        assertBool
          "the accepted unary redeemer fits signed Cardano capacity"
          (acceptedDataBytes + signedRedeemerOverheadBytes <= maximumCardanoTransactionBytes)
        assertBool
          "the adjacent unary redeemer exceeds signed Cardano capacity"
          (adjacentDataBytes + signedRedeemerOverheadBytes > maximumCardanoTransactionBytes)
        assertBool
          "a spend redeemer has more envelope overhead than an output datum"
          (signedRedeemerOverheadBytes > signedDatumOverheadBytes)
        assertBool
          "the redeemer envelope admits less unary depth"
          (acceptedDepth < 4_043)
    , testCase
        "maximum_unary_redeemer_depth_terminal_agrees_with_typescript_and_rejects_mutation"
        maximumUnaryRedeemerDepthTerminalRuntime
    ]

maximumUnaryDepthTerminalRuntime :: Assertion
maximumUnaryDepthTerminalRuntime = do
  let pre = "8a010600193f2d193f2d58204b1583076e081511c0c79da0bd361e87a6da46e2bbea22cf93f153a3dbb28203d87a80d87a80d87a80d87a80"
      frameCbor = "8b000040000040010181820058200349c700d41147fa43955b7c1ee2578d2ef8f08599dd99307121859dd2ee8e860184582087f3ecadbcf7a9f6aacd8fb875358df0898dafbc3e02ac97b94590971260e71201193f29193f2d"
      post = "8a010700193f2d193f2d40d87a80d87a80d87a80d8799f835820db84befa89735cb7e184bc06890e5b922bcb7e2550caffdff82dcec934fdd723193f2d193f31ff"
      root = "db84befa89735cb7e184bc06890e5b922bcb7e2550caffdff82dcec934fdd723"
      frame = unaryTerminalFrame
        "0349c700d41147fa43955b7c1ee2578d2ef8f08599dd99307121859dd2ee8e86"
        "87f3ecadbcf7a9f6aacd8fb875358df0898dafbc3e02ac97b94590971260e712"
        16_169
        16_173
      mutated = replaceSequenceRoot
        "86f3ecadbcf7a9f6aacd8fb875358df0898dafbc3e02ac97b94590971260e712"
        frame
      action exactFrame = PD.Constr 8 [exactFrame, PD.Constr 1 []]
  assertFrameEncoding frameCbor
  assertStep pre Nothing (action frame) post
  assertTerminalSummary post root 16_173 16_177
  assertRejected pre Nothing (action mutated)

maximumUnaryRedeemerDepthTerminalRuntime :: Assertion
maximumUnaryRedeemerDepthTerminalRuntime = do
  let pre = "8a010600193e6d193e6d582023fabfae62d51cba8147f76aef6c613f4f3525e8257361873c678e77aa750929d87a80d87a80d87a80d87a80"
      frameCbor = "8b000040000040010181820058202e255919cf99b2743582ee389fb462ccb7562cf0a614647a01d9ce9fb14000bc0184582003a6eed9be7ce3bf1e01f104842a3b8fc33bfb24f5941e2fc84019a4bdce9c2001193e69193e6d"
      post = "8a010700193e6d193e6d40d87a80d87a80d87a80d8799f8358207102b7fc9525a54adf2b32de87dfc544c46f2cf275bd45bfa794dbb518f4fa1e193e6d193e71ff"
      root = "7102b7fc9525a54adf2b32de87dfc544c46f2cf275bd45bfa794dbb518f4fa1e"
      frame = unaryTerminalFrame
        "2e255919cf99b2743582ee389fb462ccb7562cf0a614647a01d9ce9fb14000bc"
        "03a6eed9be7ce3bf1e01f104842a3b8fc33bfb24f5941e2fc84019a4bdce9c20"
        15_977
        15_981
      mutated = replaceSequenceRoot
        "02a6eed9be7ce3bf1e01f104842a3b8fc33bfb24f5941e2fc84019a4bdce9c20"
        frame
      action exactFrame = PD.Constr 8 [exactFrame, PD.Constr 1 []]
  assertFrameEncoding frameCbor
  assertStep pre Nothing (action frame) post
  assertTerminalSummary post root 15_981 15_985
  assertRejected pre Nothing (action mutated)

unaryTerminalFrame :: String -> String -> Integer -> Integer -> PD.Data
unaryTerminalFrame peakHash sequenceRoot payloadCborLength memory =
  dFrame
    0
    0
    ""
    0
    0
    ""
    1
    1
    [dPeak 0 peakHash]
    1
    (dSequence sequenceRoot 1 payloadCborLength memory)

--------------------------------------------------------------------------------
-- cek-data-breadth-v1.test.ak
--------------------------------------------------------------------------------

{-# NOINLINE compiledFrameEncodingHarness #-}
compiledFrameEncodingHarness :: Script
compiledFrameEncodingHarness =
  either (error . show) id $ compile NoTracing frameEncodingHarness

frameEncodingHarness :: forall (s :: S). Term s (PData :--> PData :--> PBool)
frameEncodingHarness =
  plam $ \frameData expectedData ->
    Frame.pencodeFrameV1
      # pfromData (punsafeCoerce frameData :: Term s (PAsData Frame.PDataFrameV1))
      #== pasByteStr # expectedData

terminalSummaryHarness :: forall (s :: S). Term s (PData :--> PData :--> PData :--> PData :--> PBool)
terminalSummaryHarness =
  plam $ \postCborData expectedRootData expectedCborLengthData expectedMemoryData ->
    pmatch (Traverse.pfinalizeV1 # (Traverse.pdecodeControlV1 # (pasByteStr # postCborData))) $ \case
      PNothing -> pconstant False
      PJust summary ->
        pmatch summary $ \s ->
          pand'List
            [ pfromData (Data.psummary'root s) #== pasByteStr # expectedRootData
            , pfromData (Data.psummary'cborLength s) #== pasInt # expectedCborLengthData
            , pfromData (Data.psummary'memory s) #== pasInt # expectedMemoryData
            ]

{-# NOINLINE compiledTerminalSummaryHarness #-}
compiledTerminalSummaryHarness :: Script
compiledTerminalSummaryHarness =
  either (error . show) id $ compile NoTracing terminalSummaryHarness

assertCompiledBool :: String -> Script -> [PD.Data] -> Assertion
assertCompiledBool label script arguments =
  case evalScriptHuge (applyArguments script arguments) of
    (Left err, _, traces) -> assertFailure (label <> " failed: " <> show err <> " " <> show traces)
    (Right result, _, _) -> assertEqual label (printScript compiledTrue) (printScript result)

assertFrameEncoding :: String -> Assertion
assertFrameEncoding frameCbor =
  assertCompiledBool
    "frame encoding"
    compiledFrameEncodingHarness
    [decodeFrameData frameCbor, PD.B (decodeHex frameCbor)]

assertTerminalSummary :: String -> String -> Integer -> Integer -> Assertion
assertTerminalSummary postCbor root cborLength memory =
  assertCompiledBool
    "terminal summary"
    compiledTerminalSummaryHarness
    [PD.B (decodeHex postCbor), PD.B (decodeHex root), PD.I cborLength, PD.I memory]

assertBreadthListFrontier ::
  String -> String -> Integer -> [PD.Data] -> String -> Assertion
assertBreadthListFrontier preCbor frameCbor childIndex siblings postCbor = do
  let frame = decodeFrameData frameCbor
      action index exactSiblings =
        PD.Constr 6 [frame, PD.I index, unitSummary, PD.List exactSiblings]
  assertFrameEncoding frameCbor
  assertStep preCbor Nothing (action childIndex siblings) postCbor
  assertRejected preCbor Nothing (action (childIndex - 1) siblings)
  assertRejected preCbor Nothing (action childIndex (replaceFirstSibling listMutation siblings))

assertBreadthMapFrontier ::
  String -> String -> Integer -> PD.Data -> PD.Data -> [PD.Data] -> [PD.Data] ->
  String -> String -> String -> Assertion
assertBreadthMapFrontier preCbor frameCbor pairIndex key value keySiblings valueSiblings
  mutatedKeyFirst mutatedValueFirst postCbor = do
    let frame = decodeFrameData frameCbor
        action index exactKeySiblings exactValueSiblings =
          PD.Constr
            7
            [ frame
            , PD.I index
            , key
            , value
            , PD.List exactKeySiblings
            , PD.List exactValueSiblings
            ]
    assertFrameEncoding frameCbor
    assertStep preCbor Nothing (action pairIndex keySiblings valueSiblings) postCbor
    assertRejected preCbor Nothing (action (pairIndex - 1) keySiblings valueSiblings)
    assertRejected preCbor Nothing $
      action pairIndex (replaceFirstSibling mutatedKeyFirst keySiblings) valueSiblings
    assertRejected preCbor Nothing $
      action pairIndex keySiblings (replaceFirstSibling mutatedValueFirst valueSiblings)

assertBreadthTerminal ::
  String -> String -> String -> String -> Integer -> Integer -> String -> Assertion
assertBreadthTerminal preCbor frameCbor postCbor expectedRoot expectedCborLength expectedMemory
  mutatedSequenceRoot = do
    let frame = decodeFrameData frameCbor
        finalize exactFrame = PD.Constr 8 [exactFrame, PD.Constr 1 []]
    assertFrameEncoding frameCbor
    assertStep preCbor Nothing (finalize frame) postCbor
    assertTerminalSummary postCbor expectedRoot expectedCborLength expectedMemory
    assertRejected preCbor Nothing $
      finalize (replaceSequenceRoot mutatedSequenceRoot frame)

unitSummary :: PD.Data
unitSummary =
  dSummary "0c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da2" 1 5

listMutation :: String
listMutation = "aa2a972c81b27a008e51e7bfc21fa624480c25a10ef4cf8b2fc4126bede787a0"

replaceFirstSibling :: String -> [PD.Data] -> [PD.Data]
replaceFirstSibling replacement (_ : rest) = PD.B (decodeHex replacement) : rest
replaceFirstSibling _ [] = error "breadth vector has no first sibling"

replaceSequenceRoot :: String -> PD.Data -> PD.Data
replaceSequenceRoot root (PD.Constr 0 [kind, constructor, constructorRoot, constructorLength,
  constructorMemory, tailRoot, expected, childCount, peaks, cursor,
  PD.Constr 0 [_oldRoot, sequenceLength, payloadCborLength, memory]]) =
    PD.Constr
      0
      [ kind
      , constructor
      , constructorRoot
      , constructorLength
      , constructorMemory
      , tailRoot
      , expected
      , childCount
      , peaks
      , cursor
      , PD.Constr 0 [PD.B (decodeHex root), sequenceLength, payloadCborLength, memory]
      ]
replaceSequenceRoot _ _ = error "invalid breadth frame fixture"

decodeFrameData :: String -> PD.Data
decodeFrameData encoded =
  case CBOR.deserialiseFromBytes decodeFrame (LBS.fromStrict $ decodeHex encoded) of
    Left err -> error (show err)
    Right (leftover, frame)
      | LBS.null leftover -> frame
      | otherwise -> error "trailing bytes in breadth frame fixture"

decodeFrame :: CBOR.Decoder s PD.Data
decodeFrame = do
  expectListLength "frame" 11
  kind <- PD.I <$> CBOR.decodeInteger
  constructor <- PD.I <$> CBOR.decodeInteger
  constructorRoot <- PD.B <$> CBOR.decodeBytes
  constructorLength <- PD.I <$> CBOR.decodeInteger
  constructorMemory <- PD.I <$> CBOR.decodeInteger
  tailRoot <- PD.B <$> CBOR.decodeBytes
  expected <- PD.I <$> CBOR.decodeInteger
  childCount <- PD.I <$> CBOR.decodeInteger
  peakCount <- CBOR.decodeListLen
  peaks <- PD.List <$> replicateM peakCount decodePeak
  cursor <- PD.I <$> CBOR.decodeInteger
  sequenceSummary <- decodeSequenceSummary
  pure $
    PD.Constr
      0
      [ kind
      , constructor
      , constructorRoot
      , constructorLength
      , constructorMemory
      , tailRoot
      , expected
      , childCount
      , peaks
      , cursor
      , sequenceSummary
      ]

decodePeak :: CBOR.Decoder s PD.Data
decodePeak = do
  expectListLength "peak" 2
  height <- PD.I <$> CBOR.decodeInteger
  hash <- PD.B <$> CBOR.decodeBytes
  pure $ PD.Constr 0 [height, hash]

decodeSequenceSummary :: CBOR.Decoder s PD.Data
decodeSequenceSummary = do
  expectListLength "sequence summary" 4
  root <- PD.B <$> CBOR.decodeBytes
  sequenceLength <- PD.I <$> CBOR.decodeInteger
  payloadCborLength <- PD.I <$> CBOR.decodeInteger
  memory <- PD.I <$> CBOR.decodeInteger
  pure $ PD.Constr 0 [root, sequenceLength, payloadCborLength, memory]

expectListLength :: String -> Int -> CBOR.Decoder s ()
expectListLength label expected = do
  actual <- CBOR.decodeListLen
  unless (actual == expected) $
    fail (label <> " has " <> show actual <> " fields; expected " <> show expected)

breadthParityTests :: TestTree
breadthParityTests =
  testGroup
    "cek-data-breadth-v1 Aiken parity"
    [
    testCase "datum_constructor_frontier_agrees" $
        assertBreadthListFrontier "8a01061831193f2d193f2d5820b4e51a813e0db08144e69ffc377ab17348ee60c68e4a10204db19d19ac4c164fd87a80d87a80d87a80d87a80" "8b010058203ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf020640193f26193f268982015820efe8f3010418beaf5549119f466aed141636fcf69a3bd24a7256a2d9b73591268202582071bcb569dc0f82be3304bbf21d970e6dfa45ba658d55a3fd26d8c6128604f9078205582014aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb82085820c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc118209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429191f2684582068bdcf182521f7e6dbfd0522ac74ac3f3f1cae35e2e44ffbb03b87b35354b46c191f26191f26199bbe" 8191
          broadListSiblings "8a01061831193f2d193f2d58207427b8b1fb4b4bb54bcfc605f0460e416caa00ee13314c878b7370b747e95c01d87a80d87a80d87a80d87a80"
    , testCase "datum_constructor_terminal_agrees" $
        assertBreadthTerminal "8a01061831193f2d193f2d582026d0f372cf8e7fc0eea66dcc131c5f6e288560e31a048dc87968548a00ab7970d87a80d87a80d87a80d87a80" "8b010058203ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf020640193f26193f268982015820efe8f3010418beaf5549119f466aed141636fcf69a3bd24a7256a2d9b73591268202582071bcb569dc0f82be3304bbf21d970e6dfa45ba658d55a3fd26d8c6128604f9078205582014aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb82085820c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc118209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429193f26845820e32b0616afebaedfeac9da563c2513bc53853b891d194fe57e2392b1a0e70a24193f26193f261a00013bbe" "8a01071831193f2d193f2d40d87a80d87a80d87a80d8799f8358207b303304d0263d23d7b6a26c2596af652b0d0909c126cb8a7da4559bc8eb6d4a193f2d1a00013bc2ff"
          "7b303304d0263d23d7b6a26c2596af652b0d0909c126cb8a7da4559bc8eb6d4a" 16173 80834 "e22b0616afebaedfeac9da563c2513bc53853b891d194fe57e2392b1a0e70a24"
    , testCase "datum_list_frontier_agrees" $
        assertBreadthListFrontier "8a01061831193f2d193f2d582081e863eecbabad08d04e6e1c653b05dfa61ea10fb4f3ba1738271ed95f998db5d87a80d87a80d87a80d87a80" "8b020040000040193f2b193f2b8a820058206ba167794ebd11ca5626c5adcbeb856de4458e5b91f6a9a3390ccb4294ee863c82015820d494f923c728c631f44776b8286db5cd8a1bf5748c40f7cb7a8059aaa2babe8d82035820d1adbec1a123cde6a98e75420abb4ab7a0e3d3f2f5aa43aab2eb977c0ea2c33b8205582014aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb82085820c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc118209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429191f2b8458205e52f38a5649f614cb32ceb5bbc7dce1a3c8dd2e8d9b3bab1dad0f1503cf18b8191f2b191f2b199bd7" 8191
          broadListSiblings "8a01061831193f2d193f2d5820d6a49cd6c3355bd06dfb354023be99abec63fb0f2ebeb19db47679ba92f0939ad87a80d87a80d87a80d87a80"
    , testCase "datum_list_terminal_agrees" $
        assertBreadthTerminal "8a01061831193f2d193f2d5820af079bcf11e4599a3004f73daba99841c1b61c54dd59b6d30e0d614ae075f8ffd87a80d87a80d87a80d87a80" "8b020040000040193f2b193f2b8a820058206ba167794ebd11ca5626c5adcbeb856de4458e5b91f6a9a3390ccb4294ee863c82015820d494f923c728c631f44776b8286db5cd8a1bf5748c40f7cb7a8059aaa2babe8d82035820d1adbec1a123cde6a98e75420abb4ab7a0e3d3f2f5aa43aab2eb977c0ea2c33b8205582014aa69ffbc423ad4227000bc3452e2919f0199f091854f1c490353e9bef82bdb82085820c9893e419e867e3849b9f9b8dc8c269131753a91213ca75ac7e3347c89badc118209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429193f2b84582063ddf7a530c9d366defd7da8dfec2c9058188289265fd7529b018f60b8ff809a193f2b193f2b1a00013bd7" "8a01071831193f2d193f2d40d87a80d87a80d87a80d8799f835820af1bd84873581d7cd6042fd677fc42508c943d337970e6bc7ef1f16e8d3f1652193f2d1a00013bdbff"
          "af1bd84873581d7cd6042fd677fc42508c943d337970e6bc7ef1f16e8d3f1652" 16173 80859 "62ddf7a530c9d366defd7da8dfec2c9058188289265fd7529b018f60b8ff809a"
    , testCase "datum_map_frontier_agrees" $
        assertBreadthMapFrontier "8a01061831193f2b193f2b5820e2b34612aada46585b509cdfbdde9662639edee29b7cb649d8ca33f64e7137ccd87a80d87a80d87a80d87a80" "8b03004000004019202019202082820558202c04aeaacfa69cd0155a4de323c3c002871b2ae45004526459611075a5223a20820d5820364791a54122388cbfc3d2923152cc12ffed5a40c08657fedf959ba66dcfe79b10845820440882f7141af971b11729a306e3bb7f86c3e073d2847cafccb584749440670410184018b0" 4095
          (dSummary "bc49f33ecd4551db7a0ca279af9ee46f50789d7ac4c7f9971914c048fa1e5c16" 3 6) (unitSummary)
          [PD.B (decodeHex "fbf47176a866dbbe9f0abaed5089e8f98310b409ba158e5886c2062f5db8032f"), PD.B (decodeHex "a02aa4ae212513cad8b63e01fb30e5488f9c492a3d4a811e3f9162d1f0c1a616"), PD.B (decodeHex "ef6b44bc646b33107c750304b749342525346ffddd8dce5ce1cdc1e77cc8c1e7"), PD.B (decodeHex "700a93e0057e26a4ea4dae231e16fdf824d07acee08a979d4759d9418acfceb9"), PD.B (decodeHex "58f86bac0e70c28d08cbbed4cf5496b064ffdbad4dc0cfab13a4fd65748611e5"), PD.B (decodeHex "ecbb22a3a1d75307d2fd9d90775b8b23f8db09d9b1f47a5e82f618ac51c5a9b3"), PD.B (decodeHex "70e8a7e998658c9f78eb59347c292f370048d5091be592fe49b1a309bc8d1a58"), PD.B (decodeHex "157d35913868ba65a89c345f4608f576eed2983d1dd54666f8ebb1336582ac3a"), PD.B (decodeHex "d724fb6dec21668ad636d3086ca320dcc0fb28a430fbbd56793b2f1722dae181"), PD.B (decodeHex "1c6e60ec8cda6d56d25e31593fe27c4c095c9165bbae228bddecc58e48fd1d6b"), PD.B (decodeHex "01df2a83bfa691263b55224415c38bcb6fb9d6b1072856791f94be1f3cff44ce"), PD.B (decodeHex "31e6deebc71c02b30ee9eb84049891a53f1aaee7aa180d094a841c45dc279e0d"), PD.B (decodeHex "68e5ef0e59874f52b2d773ae6768549dfd50e97e5d83aedf6feda87d40134314")] [PD.B (decodeHex "cbe38915df4c2a785be65ecce73240d5eec035faeb54c1c21190c5adf4fd203c"), PD.B (decodeHex "a02aa4ae212513cad8b63e01fb30e5488f9c492a3d4a811e3f9162d1f0c1a616"), PD.B (decodeHex "ef6b44bc646b33107c750304b749342525346ffddd8dce5ce1cdc1e77cc8c1e7"), PD.B (decodeHex "700a93e0057e26a4ea4dae231e16fdf824d07acee08a979d4759d9418acfceb9"), PD.B (decodeHex "58f86bac0e70c28d08cbbed4cf5496b064ffdbad4dc0cfab13a4fd65748611e5"), PD.B (decodeHex "ecbb22a3a1d75307d2fd9d90775b8b23f8db09d9b1f47a5e82f618ac51c5a9b3"), PD.B (decodeHex "70e8a7e998658c9f78eb59347c292f370048d5091be592fe49b1a309bc8d1a58"), PD.B (decodeHex "157d35913868ba65a89c345f4608f576eed2983d1dd54666f8ebb1336582ac3a"), PD.B (decodeHex "d724fb6dec21668ad636d3086ca320dcc0fb28a430fbbd56793b2f1722dae181"), PD.B (decodeHex "1c6e60ec8cda6d56d25e31593fe27c4c095c9165bbae228bddecc58e48fd1d6b"), PD.B (decodeHex "01df2a83bfa691263b55224415c38bcb6fb9d6b1072856791f94be1f3cff44ce"), PD.B (decodeHex "31e6deebc71c02b30ee9eb84049891a53f1aaee7aa180d094a841c45dc279e0d"), PD.B (decodeHex "68e5ef0e59874f52b2d773ae6768549dfd50e97e5d83aedf6feda87d40134314")]
          "faf47176a866dbbe9f0abaed5089e8f98310b409ba158e5886c2062f5db8032f" "cae38915df4c2a785be65ecce73240d5eec035faeb54c1c21190c5adf4fd203c" "8a01061831193f2b193f2b5820cc9b98c851406eee37c0d72c6547320a7c629efac14eeea20cf22ea7f42ae90fd87a80d87a80d87a80d87a80"
    , testCase "datum_map_terminal_agrees" $
        assertBreadthTerminal "8a01061831193f2b193f2b5820501699a6d4f2648cec8cb75f23edaba0009816fdaeba9fb575561fdd121bed75d87a80d87a80d87a80d87a80" "8b03004000004019202019202082820558202c04aeaacfa69cd0155a4de323c3c002871b2ae45004526459611075a5223a20820d5820364791a54122388cbfc3d2923152cc12ffed5a40c08657fedf959ba66dcfe79b1910108458205609775593de4a6045c86efde1bb729c30b40b56835fba9e23b2aab8188d764c191010193f2819b030" "8a01071831193f2b193f2b40d87a80d87a80d87a80d8799f83582027bf1066c2477eaae456420999f6b6f83139dbce2502cea5424a0924db643581193f2b19b034ff"
          "27bf1066c2477eaae456420999f6b6f83139dbce2502cea5424a0924db643581" 16171 45108 "5709775593de4a6045c86efde1bb729c30b40b56835fba9e23b2aab8188d764c"
    , testCase "redeemer_constructor_frontier_agrees" $
        assertBreadthListFrontier "8a010606193e70193e70582075b4bbdd15c02fe5e282d357c2748627bf9a9c65728c9355609fa8d0a461b674d87a80d87a80d87a80d87a80" "8b010058203ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf020640193e69193e698982005820a87a488e10297529b04c01a69c4634c480a41d277730fbc6824c3bf91ff16ebb82035820f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da82055820d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa82065820bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da38209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429191e69845820db769f6bbcb7b2cf462ce920a89553ecc3b0ab0264c5bf421abaf02c8039428b191e69191e6919980d" 8191
          broadListSiblings "8a010606193e70193e70582087f4606562f56ac4eba1adff36e216bbf013a02d9c39e3ecb45fe4dcbb625ba2d87a80d87a80d87a80d87a80"
    , testCase "redeemer_constructor_terminal_agrees" $
        assertBreadthTerminal "8a010606193e70193e70582029201c895676ea4211866a5ab8183f6bc1fa37ce08e4a950b70d7bb095d8c9c2d87a80d87a80d87a80d87a80" "8b010058203ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf020640193e69193e698982005820a87a488e10297529b04c01a69c4634c480a41d277730fbc6824c3bf91ff16ebb82035820f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da82055820d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa82065820bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da38209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429193e6984582006de6a3a6db23a497f9a88a768d15d6f7ff63ec40cee256326ce3a4e57f7fb46193e69193e691a0001380d" "8a010706193e70193e7040d87a80d87a80d87a80d8799f83582068202260a6d772b441b247400f69fa7799355da692120ac670cb7547b01bb745193e701a00013811ff"
          "68202260a6d772b441b247400f69fa7799355da692120ac670cb7547b01bb745" 15984 79889 "07de6a3a6db23a497f9a88a768d15d6f7ff63ec40cee256326ce3a4e57f7fb46"
    , testCase "redeemer_list_frontier_agrees" $
        assertBreadthListFrontier "8a010606193e70193e7058200ed22209f9048bfb21061585a17b038e1a02dac8247d79f9c3981383a664ef39d87a80d87a80d87a80d87a80" "8b020040000040193e6e193e6e8a82015820e3b249eefffab2fa881b2cec5ab9934a3fecb09dfd9717a97349e76fe7a002a6820258206c990dab77d0f16c66eee567a54bbbc774ff06e7fc647010bcd2e0f366d046f382035820f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da82055820d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa82065820bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da38209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429191e6e845820a105d7dbe5bc6de2fd32a3415fa52b56c67703652b147f722d4218f6132f6ce4191e6e191e6e199826" 8191
          broadListSiblings "8a010606193e70193e7058207efd673d3c799af342565968e2a6686452d94ab3ce9efdd369f0fe18bef3aef3d87a80d87a80d87a80d87a80"
    , testCase "redeemer_list_terminal_agrees" $
        assertBreadthTerminal "8a010606193e70193e705820463b8f5c39a691bdecc4e9bd7d02b21c97c599b5d789621460a5aa611e0da11ad87a80d87a80d87a80d87a80" "8b020040000040193e6e193e6e8a82015820e3b249eefffab2fa881b2cec5ab9934a3fecb09dfd9717a97349e76fe7a002a6820258206c990dab77d0f16c66eee567a54bbbc774ff06e7fc647010bcd2e0f366d046f382035820f730c0466e2b97777f9e6dc5c8f355a6136604e6aaf824711e456ad7161fa5da82055820d99513b5091d84d62a5157c78e01fd80cd4d2981efbbaf508a74397e6f04a7fa82065820bebd3cdb3dfca360952b5f78b82cdc420ade16d39d3e4626343ec11d81377da38209582084bc5c3cf81c51a605b962b2719b7a951a1a821a2d12ca796465ede3df0085c2820a5820d179289bb351f3dfc94a3d9d5434f1c24823316ece8bfc1c9260f61f8588e91d820b58200bbc97211b3c41c845928b52a556e6e345f823741237e0dc1166c44c41461124820c58206963b04ac925d53a29762f201e0a1068d43087da2f492230db13c49c008ed95f820d5820936b2b9b1b34e09da8073e3a3d699dab965a7d18e3dc2613213e6074b10e6429193e6e845820c98415365fccc4c818ac3e255707782ef7d850c5fa3f84f1b5d3ad007691d8b8193e6e193e6e1a00013826" "8a010706193e70193e7040d87a80d87a80d87a80d8799f835820e267f1612bd596a10ed967889fe81efb005fdf09c6651720f0e87715774d35d5193e701a0001382aff"
          "e267f1612bd596a10ed967889fe81efb005fdf09c6651720f0e87715774d35d5" 15984 79914 "c88415365fccc4c818ac3e255707782ef7d850c5fa3f84f1b5d3ad007691d8b8"
    , testCase "redeemer_map_frontier_agrees" $
        assertBreadthMapFrontier "8a010606193e6f193e6f58205441034e8f9e79524aa4961daa3564b41e27fe9d4e08aa3f1eff491cde3fe1efd87a80d87a80d87a80d87a80" "8b030040000040191fc2191fc28882015820ef30dea6d2c17a299ef4549031e2516ea9e5a5b792a0d84cbf25d15b72a5f0bd8206582070e8a7e998658c9f78eb59347c292f370048d5091be592fe49b1a309bc8d1a5882075820157d35913868ba65a89c345f4608f576eed2983d1dd54666f8ebb1336582ac3a82085820d724fb6dec21668ad636d3086ca320dcc0fb28a430fbbd56793b2f1722dae181820958201c6e60ec8cda6d56d25e31593fe27c4c095c9165bbae228bddecc58e48fd1d6b820a582001df2a83bfa691263b55224415c38bcb6fb9d6b1072856791f94be1f3cff44ce820b582031e6deebc71c02b30ee9eb84049891a53f1aaee7aa180d094a841c45dc279e0d820c582068e5ef0e59874f52b2d773ae6768549dfd50e97e5d83aedf6feda87d401343141907e18458202d3237b8bd77ed407f3048c41d73e35d8861e3ac1c3311f4b33c574c22f69e591907e1191f841956ab" 2047
          (dSummary "4f0c41d3a758682c3f345581483b1f05f8dc1cb4b7a75b7c7379dc98e1833d8a" 3 6) (unitSummary)
          [PD.B (decodeHex "7baae1975ecb5118c570c8dca87afd59c7b504ff383f9315669f212557c976e1"), PD.B (decodeHex "15f439dde5c4396dde95691bf3e4e1806dfc2c48ae5ef373fb39701279b873c1"), PD.B (decodeHex "9e10aa07c1263733115d5fe0fb8a7ac2e9ad6eb13c2f1b047906c7fbaae93c7f"), PD.B (decodeHex "23c71c630aca0f74959719e6ac08c5bc8f1dce9f7d85517089a1aadf8772e2f3"), PD.B (decodeHex "d57b256ebdc7916c9a661f6641f49d55756962ab05fe2d777db16174e7370334"), PD.B (decodeHex "cb50da584379e82471566e77ad56ea60a65acd218e6e65c0770ee0fb571e5f6c"), PD.B (decodeHex "a935a034cdd1f046a665efc1dbe926d1a6337b0c29038fc7f608b3e05b500283"), PD.B (decodeHex "a39fd0f277153d1a4d279fc8b13c15e4fe872077d0c348d1fd9f420dfd9a3606"), PD.B (decodeHex "76df3f162ec7bf8c10fbc88fa71dfd42759cdc7846f09e3ea2b74558afc9f7df"), PD.B (decodeHex "3aa0206c9a518a16b5ac79597cc0245f241b44d8602cd0d2805ec23cd7f23db4"), PD.B (decodeHex "83e00ebb50f88562c1ce30345925d3c3f25aaaed82623b722c02d895cac884ab"), PD.B (decodeHex "f58c85354a6e958f0f5f0cb36d9111d58f29b0aac8eb48eb36460b9abc9c93a3")] [PD.B (decodeHex "66b81fbe02ed0e9419f22881a3d07fd1bf07d9ea5c4c826079dd29ed6743cdf6"), PD.B (decodeHex "15f439dde5c4396dde95691bf3e4e1806dfc2c48ae5ef373fb39701279b873c1"), PD.B (decodeHex "9e10aa07c1263733115d5fe0fb8a7ac2e9ad6eb13c2f1b047906c7fbaae93c7f"), PD.B (decodeHex "23c71c630aca0f74959719e6ac08c5bc8f1dce9f7d85517089a1aadf8772e2f3"), PD.B (decodeHex "d57b256ebdc7916c9a661f6641f49d55756962ab05fe2d777db16174e7370334"), PD.B (decodeHex "cb50da584379e82471566e77ad56ea60a65acd218e6e65c0770ee0fb571e5f6c"), PD.B (decodeHex "a935a034cdd1f046a665efc1dbe926d1a6337b0c29038fc7f608b3e05b500283"), PD.B (decodeHex "a39fd0f277153d1a4d279fc8b13c15e4fe872077d0c348d1fd9f420dfd9a3606"), PD.B (decodeHex "76df3f162ec7bf8c10fbc88fa71dfd42759cdc7846f09e3ea2b74558afc9f7df"), PD.B (decodeHex "3aa0206c9a518a16b5ac79597cc0245f241b44d8602cd0d2805ec23cd7f23db4"), PD.B (decodeHex "83e00ebb50f88562c1ce30345925d3c3f25aaaed82623b722c02d895cac884ab"), PD.B (decodeHex "f58c85354a6e958f0f5f0cb36d9111d58f29b0aac8eb48eb36460b9abc9c93a3")]
          "7aaae1975ecb5118c570c8dca87afd59c7b504ff383f9315669f212557c976e1" "67b81fbe02ed0e9419f22881a3d07fd1bf07d9ea5c4c826079dd29ed6743cdf6" "8a010606193e6f193e6f58207e8007304fea4b6142e88a3ade6f06ebf4c5e6fc8f683897e0dd1d02319fa106d87a80d87a80d87a80d87a80"
    , testCase "redeemer_map_terminal_agrees" $
        assertBreadthTerminal "8a010606193e6f193e6f582082b86d7d8db1d1517ef3ab50d1e5592af116a98566c8ec31df0c1840f0d9d2ecd87a80d87a80d87a80d87a80" "8b030040000040191fc2191fc28882015820ef30dea6d2c17a299ef4549031e2516ea9e5a5b792a0d84cbf25d15b72a5f0bd8206582070e8a7e998658c9f78eb59347c292f370048d5091be592fe49b1a309bc8d1a5882075820157d35913868ba65a89c345f4608f576eed2983d1dd54666f8ebb1336582ac3a82085820d724fb6dec21668ad636d3086ca320dcc0fb28a430fbbd56793b2f1722dae181820958201c6e60ec8cda6d56d25e31593fe27c4c095c9165bbae228bddecc58e48fd1d6b820a582001df2a83bfa691263b55224415c38bcb6fb9d6b1072856791f94be1f3cff44ce820b582031e6deebc71c02b30ee9eb84049891a53f1aaee7aa180d094a841c45dc279e0d820c582068e5ef0e59874f52b2d773ae6768549dfd50e97e5d83aedf6feda87d40134314190fe1845820acf786ffedfff0505125acc7c2e8e3ca534993740205a60ec4a1771e4915ae18190fe1193e6c19ae2b" "8a010706193e6f193e6f40d87a80d87a80d87a80d8799f835820875b2886163d7c6869f44c94fcde51131baa99e550d7331cf25b517670f961a8193e6f19ae2fff"
          "875b2886163d7c6869f44c94fcde51131baa99e550d7331cf25b517670f961a8" 15983 44591 "adf786ffedfff0505125acc7c2e8e3ca534993740205a60ec4a1771e4915ae18"

    ]

broadListSiblings :: [PD.Data]
broadListSiblings =
  [PD.B (decodeHex "ab2a972c81b27a008e51e7bfc21fa624480c25a10ef4cf8b2fc4126bede787a0"), PD.B (decodeHex "d335dd87d228b2aa7b1151e9ab91c5c0699453dc85d749f99c30bf2c136c8113"), PD.B (decodeHex "df44478b5bde4ecf70e2a07efe7b6bd3e1f61fb2b1ef83e3b259ddf1e2adea2d"), PD.B (decodeHex "87f8eb1b3ae3706b7ac4ae9a3645aacc8ba9f8420b6525a4d7ca80e06ecf1cfc"), PD.B (decodeHex "5eba7fe157aea07d5009512556d5efc706a3bdb80c2078d84198d98fb5d5b6fc"), PD.B (decodeHex "01495dc87dadb536912d40c6d64d714335aa2aac0efae101bfc550bde531bb7b"), PD.B (decodeHex "728b21775172eed0ad58d1cdc095ca8cf2833dfeca327ce050cb940bc48f2ecb"), PD.B (decodeHex "188ec8994e9bf0e212b106d8c82d0660ea3e35bd3b21f2e07ca49e1c6e97bd36"), PD.B (decodeHex "103ee780ab37963f93630784f513a345a679d0dbea43345ad232255490a869ed"), PD.B (decodeHex "e9c15cef679bfd1f8fb5efbd4735a3db216ecb2718b768a7adf22c79ab6f7274"), PD.B (decodeHex "7a6a5674af79f93e6709a502a46b6ae3010cb1a59afaa957491389b9052ca2c0"), PD.B (decodeHex "4706813a35b188f4ddbf2ba007a43310918357ceb859220a510a48d6a228f254"), PD.B (decodeHex "f819c60cad2e369ea86b906a87c2af5585714c072d0b67e2be7d143492d8fe25")]


decodeHex :: String -> BS.ByteString
decodeHex value = either error id (Base16.decode (BS8.pack value))

stepHarness :: forall (s :: S). Term s (PData :--> PData :--> PData :--> PData :--> PBool)
stepHarness = plam $ \preData sourceData actionData postData ->
  plet (Traverse.pdecodeControlV1 # (pasByteStr # preData)) $ \control ->
  plet
    ( pmatch (pasConstr # sourceData) $ \(PBuiltinPair tag fields) ->
        pif (tag #== 0 #&& plength # fields #== 1)
          (pcon (PJust (pasByteStr # (pelemAt # 0 # fields))))
          (pif (tag #== 1 #&& pnull # fields) (pcon PNothing) perror)
    )
    $ \source ->
      plet (pfromData (punsafeCoerce actionData) :: Term s Traverse.PDataTraverseActionV1) $ \action ->
        pmatch (pasConstr # postData) $ \(PBuiltinPair postTag postFields) ->
          pmatch (Traverse.pstepV1 # control # source # action) $ \case
            PNothing -> postTag #== 1 #&& pnull # postFields
            PJust next ->
              postTag #== 0
                #&& plength # postFields #== 1
                #&& Traverse.pencodeControlV1 # next #== pasByteStr # (pelemAt # 0 # postFields)

{-# NOINLINE compiledStepHarness #-}
compiledStepHarness :: Script
compiledStepHarness = either (error . show) id (compile NoTracing stepHarness)

{-# NOINLINE compiledTrue #-}
compiledTrue :: Script
compiledTrue = either (error . show) id (compile NoTracing (pconstant @PBool True))

assertStep :: String -> Maybe String -> PD.Data -> String -> Assertion
assertStep pre source action post =
  let sourceData = maybe (PD.Constr 1 []) (PD.Constr 0 . pure . PD.B . decodeHex) source
      expected = PD.Constr 0 [PD.B (decodeHex post)]
      script = applyArguments compiledStepHarness [PD.B (decodeHex pre), sourceData, action, expected]
   in case evalScriptHuge script of
        (Left err, _, traces) -> assertFailure ("step failed: " <> show err <> " " <> show traces)
        (Right result, _, _) -> assertEqual "step result" (printScript compiledTrue) (printScript result)

assertRejected :: String -> Maybe String -> PD.Data -> Assertion
assertRejected pre source action =
  let sourceData = maybe (PD.Constr 1 []) (PD.Constr 0 . pure . PD.B . decodeHex) source
      script = applyArguments compiledStepHarness [PD.B (decodeHex pre), sourceData, action, PD.Constr 1 []]
   in case evalScriptHuge script of
        (Left err, _, traces) -> assertFailure ("rejection check failed: " <> show err <> " " <> show traces)
        (Right result, _, _) -> assertEqual "rejection result" (printScript compiledTrue) (printScript result)

streamsLargeConstructorRuntime :: Assertion
streamsLargeConstructorRuntime = do
  assertStep
    "8a010011110040d87a80d87a80d87a80d87a80"
    (Just "d86682c249010000000000000000")
    (PD.Constr 4 [PD.I 11, PD.I 1])
    "8a010311110340d8799f01ffd8799f860100140b00d87a80ffd87a80d87a80"
  passertEvalNoTrace terminalGolden

terminalGolden :: forall (s :: S). Term s PBool
terminalGolden =
  plet (Traverse.pdecodeControlV1 # phexByteStr "8a010711111140d87a80d87a80d87a80d8799f835820844cdd8ac8dc97d87e4ed149da121054504365b523034a804a12c014d55c2c441109ff") $ \terminal ->
  plet (pexpectJust (Traverse.pfinalizeV1 # terminal)) $ \summary -> pmatch summary $ \s ->
    pand'List
      [ pfromData (Data.psummary'root s) #== phexByteStr "844cdd8ac8dc97d87e4ed149da121054504365b523034a804a12c014d55c2c44"
      , pfromData (Data.psummary'cborLength s) #== 17
      , pfromData (Data.psummary'memory s) #== 9
      , Traverse.phashControlV1 # terminal #== phexByteStr "173ab9eb57665546414d5c286c55bc1fcd939a1784a7b800863e9970b82f6c16"
      ]

dSequence :: String -> Integer -> Integer -> Integer -> PD.Data
dSequence root len payload memory =
  PD.Constr 0 [PD.B (decodeHex root), PD.I len, PD.I payload, PD.I memory]

dSummary :: String -> Integer -> Integer -> PD.Data
dSummary root cborLength memory =
  PD.Constr 0 [PD.B (decodeHex root), PD.I cborLength, PD.I memory]

dPeak :: Integer -> String -> PD.Data
dPeak height hash = PD.Constr 0 [PD.I height, PD.B (decodeHex hash)]

dFrame ::
  Integer -> Integer -> String -> Integer -> Integer -> String -> Integer -> Integer ->
  [PD.Data] -> Integer -> PD.Data -> PD.Data
dFrame kind constructor constructorRoot constructorLength constructorMemory tailRoot expected childCount peaks cursor sequence =
  PD.Constr 0
    [ PD.I kind, PD.I constructor, PD.B (decodeHex constructorRoot), PD.I constructorLength
    , PD.I constructorMemory, PD.B (decodeHex tailRoot), PD.I expected, PD.I childCount
    , PD.List peaks, PD.I cursor, sequence
    ]

popsEmptyChildRuntime :: Assertion
popsEmptyChildRuntime = do
  let emptyRoot = "8c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56"
      emptySequence = dSequence emptyRoot 0 0 0
      parent = dFrame 0 0 "" 0 0 "" 1 0 [] 0 emptySequence
      child = dFrame 2 0 "" 0 0 "9aec9c7a1962916bc4312d41e99b81538e7865adeacddcbfe734d6118198a48f" 0 0 [] 0 emptySequence
      childSummary = dSummary "40789c0e0bcfa83d22dbad06c2fd2bcbcabef4fad15df776b3f25250f0511998" 1 4
      peak = dPeak 0 "2914a6f88a99d13919d2c93eb1a487fee662e18e51b614501dc120abcb3c4bd2"
      fullParent = dFrame 0 0 "" 0 0 "" 1 1 [peak] 0 emptySequence
      foldedParent = dFrame 0 0 "" 0 0 "" 1 1 [peak] 1
        (dSequence "c5489fe62bb466b588bf98f189f23b55d2119ce13630900c8175e198cacb6d95" 1 1 4)
  assertStep
    "8a010011050040d87a80d87a80d87a80d87a80" (Just "d8799f80ff")
    (PD.Constr 2 [PD.I 1])
    "8a010011050358209aec9c7a1962916bc4312d41e99b81538e7865adeacddcbfe734d6118198a48fd87a80d87a80d87a80d87a80"
  assertStep
    "8a010011050358209aec9c7a1962916bc4312d41e99b81538e7865adeacddcbfe734d6118198a48fd87a80d87a80d87a80d87a80" (Just "80ff")
    (PD.Constr 2 [PD.I 0])
    "8a010611050458201b58ce4338ad2cba8c83fc072342759755ae780beec25cf84de2abd668c0b4e9d87a80d87a80d87a80d87a80"
  assertStep
    "8a010611050458201b58ce4338ad2cba8c83fc072342759755ae780beec25cf84de2abd668c0b4e9d87a80d87a80d87a80d87a80" Nothing
    (PD.Constr 8 [child, PD.Constr 0 [parent]])
    "8a01051105045820f17f3d227003e2afcb3e5f693c326fc2f2166baefa26ede5b8c3d4f2a363107ad87a80d87a80d87a80d87a80"
  assertStep
    "8a01051105045820f17f3d227003e2afcb3e5f693c326fc2f2166baefa26ede5b8c3d4f2a363107ad87a80d87a80d87a80d87a80" (Just "ff")
    (PD.Constr 0 [])
    "8a01061105055820f17f3d227003e2afcb3e5f693c326fc2f2166baefa26ede5b8c3d4f2a363107ad87a80d87a80d87a80d87a80"
  assertStep
    "8a01061105055820f17f3d227003e2afcb3e5f693c326fc2f2166baefa26ede5b8c3d4f2a363107ad87a80d87a80d87a80d87a80" Nothing
    (PD.Constr 6 [fullParent, PD.I 0, childSummary, PD.List []])
    "8a01061105055820ef7e84aab7fc209f721124a80836f1352ec9b055f24566fdd9d72c8fbd95aba9d87a80d87a80d87a80d87a80"
  assertStep
    "8a01061105055820ef7e84aab7fc209f721124a80836f1352ec9b055f24566fdd9d72c8fbd95aba9d87a80d87a80d87a80d87a80" Nothing
    (PD.Constr 8 [foldedParent, PD.Constr 1 []])
    "8a010711050540d87a80d87a80d87a80d8799f83582078079981f2bef09c49b6981de04db9f1ca94dc0de168cdc61e2681d501607f960508ff"

maximumHeadControlsRuntime :: Assertion
maximumHeadControlsRuntime = do
  let largePre = "8a01001831193f2b0040d87a80d87a80d87a80d87a80"
      largeSource = "d8668218809fa1d879809f9f9f9f"
      sequencePre = "8a01001831193f2b07582030d75ccee289617aaa8612e21cc72076bcebd188667fb119d9224f76f05a2d01d87a80d87a80d87a80d87a80"
      sequenceSource = "d879809f9f9f9f9f9f9f9f9f9f9f"
      mapPre = "8a01001831193f2b06582001afca7b219108b07ea760418c63d6be0179f69c368e796a8cd0ba46f5e342fcd87a80d87a80d87a80d87a80"
      mapSource = "a1d879809f9f9f9f9f9f9f9f9f9f"
      scalarPre = "8a01001831193f2b165820bf2bd3eb8fc61e7769e9f918c8cdcc9668c96c8be328284060e76931c62005b8d87a80d87a80d87a80d87a80"
      scalarSource = "410100ff9f009f0000ffffff9f9f"
  assertStep largePre (Just largeSource) (PD.Constr 4 [PD.I 2, PD.I 1])
    "8a01031831193f2b0340d8799f01ffd8799f86010018340200d87a80ffd87a80d87a80"
  assertStep sequencePre (Just sequenceSource) (PD.Constr 2 [PD.I 0])
    "8a01061831193f2b0a5820df8903414ebd963ae3a2ed2ca1ec2f15b56e1017fe0701620626657b7fa92a12d87a80d87a80d87a80d87a80"
  assertStep mapPre (Just mapSource) (PD.Constr 3 [])
    "8a01001831193f2b07582030d75ccee289617aaa8612e21cc72076bcebd188667fb119d9224f76f05a2d01d87a80d87a80d87a80d87a80"
  assertStep scalarPre (Just scalarSource) (PD.Constr 1 [PD.I 2])
    "8a01021831193f2b165820bf2bd3eb8fc61e7769e9f918c8cdcc9668c96c8be328284060e76931c62005b8d87a80d87a80d8799f86010018470200d87a80ffd87a80"
  assertRejected largePre (Just "d8658218809fa1d879809f9f9f9f") (PD.Constr 4 [PD.I 2, PD.I 1])
  assertRejected sequencePre (Just sequenceSource) (PD.Constr 2 [PD.I 1])
  assertRejected mapPre (Just "9fd879809f9f9f9f9f9f9f9f9f9f") (PD.Constr 3 [])
  assertRejected scalarPre (Just scalarSource) (PD.Constr 1 [PD.I 0])

maximumFoldControlsRuntime :: Assertion
maximumFoldControlsRuntime = do
  let emptyListSequence = dSequence "8c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56" 0 0 0
      emptyMapSequence = dSequence "bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2" 0 0 0
      listFrame = dFrame 2 0 "" 0 0 "7f014d1984be15f9587409aa022afa8f6c4e1488924df30c012383e3bbf28e2c" 2 2
        [dPeak 1 "1bc19900bb2341f556a93b216efdc1e85b076c82d3ef67f35f42367b491cc22d"] 0 emptyListSequence
      mapFrame = dFrame 3 0 "" 0 0 "01afca7b219108b07ea760418c63d6be0179f69c368e796a8cd0ba46f5e342fc" 2 2
        [dPeak 1 "1f137899e881e56f1776f5788ffd5cdb32aac1b7541970499657c0f354e942bc"] 0 emptyMapSequence
      emptyKeyFrame = dFrame 0 0 "" 0 0 "30d75ccee289617aaa8612e21cc72076bcebd188667fb119d9224f76f05a2d01" 0 0 [] 0 emptyListSequence
      emptyMapParent = dFrame 3 0 "" 0 0 "01afca7b219108b07ea760418c63d6be0179f69c368e796a8cd0ba46f5e342fc" 2 0 [] 0 emptyMapSequence
      listChild = dSummary "0c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da2" 1 5
      key = dSummary "284d708d538f4a2427de3995ea06dbb2ffadff18ebb763b56498bb66882e17f9" 3 4
      value = dSummary "a5423cf93544d255ed2af3806b522663bb0b3a807128a2304524935b7703e347" 16160 48479
      keySiblings = PD.List [PD.B (decodeHex "fc2a353cbdde9156f280ae88c1eadee3d3b42a22d0396ebfa4662dc860f076c9")]
      valueSiblings = PD.List [PD.B (decodeHex "731f16f5e9418e13a0d46f037c791150d0285d1f8d2c96cf39847d0a8c56030d")]
      listPre = "8a01061831193f2b181a58207af9fbf871cd41cd5754b69c195d71a3d671739de3f37e177e7313de792b51dcd87a80d87a80d87a80d87a80"
      mapPre = "8a01061831193f2b193f2a58204af2404008f74a4ee3eb9123aa0bed99e829e1bf31c6ad5c5ca5efceb0292d85d87a80d87a80d87a80d87a80"
  assertStep listPre Nothing
    (PD.Constr 6 [listFrame, PD.I 1, listChild, PD.List [PD.B (decodeHex "48e5cbf5c18eade239c5fdea7e7c105612da100e19047628fc75ca7175197fca")]])
    "8a01061831193f2b181a58205ff9de6f99abc0e417a8e81850431e4fecf6fe4b0a902e85b8ec2373939440d3d87a80d87a80d87a80d87a80"
  assertStep mapPre Nothing (PD.Constr 7 [mapFrame, PD.I 0, key, value, keySiblings, valueSiblings])
    "8a01061831193f2b193f2a582062601ffbae712e9b1f490fc9c1b52b6e7f46560afa3c873539a2a54d45afb98fd87a80d87a80d87a80d87a80"
  assertStep
    "8a01061831193f2b0a5820df8903414ebd963ae3a2ed2ca1ec2f15b56e1017fe0701620626657b7fa92a12d87a80d87a80d87a80d87a80"
    Nothing (PD.Constr 8 [emptyKeyFrame, PD.Constr 0 [emptyMapParent]])
    "8a01001831193f2b0a58202ce6e5a25de1f43178e311d53992e750c87a95234d34fddc2f0f6d4477f533f4d87a80d87a80d87a80d87a80"
  assertRejected listPre Nothing
    (PD.Constr 6 [listFrame, PD.I 1, listChild, PD.List [PD.B (decodeHex "49e5cbf5c18eade239c5fdea7e7c105612da100e19047628fc75ca7175197fca")]])
  assertRejected mapPre Nothing (PD.Constr 7 [mapFrame, PD.I 1, key, value, keySiblings, valueSiblings])

dTerminalFrame :: String -> String -> Integer -> Integer -> PD.Data
dTerminalFrame peakHash sequenceRoot payload memory =
  dFrame 1 0 "3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf" 2 6 "" 1 1
    [dPeak 0 peakHash] 1 (dSequence sequenceRoot 1 payload memory)

maximumDataTerminalRuntime :: Assertion
maximumDataTerminalRuntime = do
  let pre = "8a01061831193f2b193f2b582064c916e4a790b0d133bb36d04d9e9ecca7dc2f3c679690d2d005f84b311e9d94d87a80d87a80d87a80d87a80"
      frame = dTerminalFrame
        "9f62b20d5db17ead31389c3864fb7ea3a2f68726b21ef48452c670dab47a8a66"
        "f64559d8fa739e5dec6e218602ff2ebd0d24b477421f38b1872a2274454f84c7" 16164 48487
  assertStep pre Nothing (PD.Constr 8 [frame, PD.Constr 1 []])
    "8a01071831193f2b193f2b40d87a80d87a80d87a80d8799f83582077156535ea7ff621233f808b4995b94294f504a0dd78455593440e3d03ad2b6f193f2b19bd6bff"
  assertRejected pre Nothing (PD.Constr 8
    [dTerminalFrame "9f62b20d5db17ead31389c3864fb7ea3a2f68726b21ef48452c670dab47a8a66"
      "f74559d8fa739e5dec6e218602ff2ebd0d24b477421f38b1872a2274454f84c7" 16164 48487, PD.Constr 1 []])

maximumRedeemerTerminalRuntime :: Assertion
maximumRedeemerTerminalRuntime = do
  let pre = "8a010600193e6e193e6e582008f6a2dc24df8fbc23b2d4255dda3ca30a2fd28eb361e9ab31bf01732c764eead87a80d87a80d87a80d87a80"
      frame = dTerminalFrame
        "20de66bc0f1322c9c61884ce582d6698c9075e35c183a4264c6c7c27fbf1401b"
        "0f7bb776751d400f727bf81b02cd7ed66457e144209eb5f9f90e2c6500fe1496" 15975 47920
  assertStep pre Nothing (PD.Constr 8 [frame, PD.Constr 1 []])
    "8a010700193e6e193e6e40d87a80d87a80d87a80d8799f83582026ef420c9e803ba9d74f048b521bff6c99e6a6b4d8aefd077c300a8e31a4dc20193e6e19bb34ff"
  assertRejected pre Nothing (PD.Constr 8
    [dTerminalFrame "20de66bc0f1322c9c61884ce582d6698c9075e35c183a4264c6c7c27fbf1401b"
      "1f7bb776751d400f727bf81b02cd7ed66457e144209eb5f9f90e2c6500fe1496" 15975 47920, PD.Constr 1 []])

maximumSourceWindowRuntime :: Assertion
maximumSourceWindowRuntime = do
  let initial = "8a01001118860040d87a80d87a80d87a80d87a80"
      opened = "8a01021118860040d87a80d87a80d8799f86010011188600d87a80ffd87a80"
      parsed = "8a01021118860040d87a80d87a80d8799f8601011118861880d8799f8601000018808401000080d8799f890100001897584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffd87a80"
  assertStep initial (Just "5f58406a6a6a6a6a6a6a6a6a6a6a") (PD.Constr 1 [PD.I 134]) opened
  assertStep opened (Just "5f58") (PD.Constr 0 []) parsed
  passertEvalNoTrace $
    pmatch (pexpectJust (Traverse.pnextSourceSpanV1 # (Traverse.pdecodeControlV1 # pconstant (decodeHex parsed)))) $
      \(Blob.PCekSourceBlobSpanV1 _ len) -> pfromData len #== 109 #&& pfromData len #<= Traverse.pmaxSourceSpan

malformedInputsRuntime :: Assertion
malformedInputsRuntime = do
  assertStep
    "8a010011030040d87a80d87a80d87a80d87a80" (Just "9f01ff") (PD.Constr 2 [PD.I 2])
    "8a0100110301582070f3276a7b9d93840009e06bb630bc34579c668e29a348c92867135475d68b53d87a80d87a80d87a80d87a80"
  assertStep
    "8a0100110301582070f3276a7b9d93840009e06bb630bc34579c668e29a348c92867135475d68b53d87a80d87a80d87a80d87a80"
    (Just "01ff") (PD.Constr 1 [PD.I 1])
    "8a0101110301582070f3276a7b9d93840009e06bb630bc34579c668e29a348c92867135475d68b53d87a80d8799f860100120100d87a80ffd87a80d87a80"
  assertStep
    "8a010011060040d87a80d87a80d87a80d87a80" (Just "d86682187f80") (PD.Constr 4 [PD.I 2, PD.I 0])
    "8a010311060340d8799f00ffd8799f860100140200d87a80ffd87a80d87a80"
  assertRejected
    "8a010311060340d8799f00ffd8799f860100140200d87a80ffd87a80d87a80" (Just "187f") (PD.Constr 0 [])

pexpectJust :: forall (s :: S) (a :: S -> Type). Term s (PMaybe a) -> Term s a
pexpectJust value = pmatch value $ \case PNothing -> perror; PJust exact -> exact

pnoneData :: forall (s :: S) (a :: S -> Type). Term s (PMaybeData a)
pnoneData = pcon PDNothing

psomeData :: forall (s :: S) (a :: S -> Type). PIsData a => Term s a -> Term s (PMaybeData a)
psomeData value = pcon (PDJust (pdata value))

pactionNone :: forall (s :: S). Term s Traverse.PDataTraverseActionV1
pactionNone = pcon Traverse.PNoAction

psourceFor :: forall (s :: S). Term s Traverse.PDataTraverseControlV1 -> Term s PByteString -> Term s PInteger -> Term s (PMaybe PByteString)
psourceFor control source sourceStart = pmatch (Traverse.pnextSourceSpanV1 # control) $ \case
  PNothing -> pcon PNothing
  PJust span -> pmatch span $ \(Blob.PCekSourceBlobSpanV1 start len) ->
    pcon (PJust (psliceLen # source # (pfromData start - sourceStart) # pfromData len))

pfinishLargeConstructor :: forall (s :: S). Term s (Traverse.PDataTraverseControlV1 :--> PByteString :--> PInteger :--> Traverse.PDataTraverseControlV1)
pfinishLargeConstructor = phoistAcyclic $ pfix $ \self -> plam $ \control source sourceStart ->
  pif (pstage control #== Traverse.pstageLargeFields) control $
    self # (pexpectJust (Traverse.pstepV1 # control # psourceFor control source sourceStart # pactionNone)) # source # sourceStart

pfinishScalar :: forall (s :: S). Term s (Traverse.PDataTraverseControlV1 :--> PByteString :--> PInteger :--> Traverse.PDataTraverseControlV1)
pfinishScalar = phoistAcyclic $ pfix $ \self -> plam $ \control source sourceStart ->
  plet
    (pmatch control $ \c ->
      pif (pfromData (Traverse.ptraverse'stage c) #== Traverse.pstageInteger)
        (pmatch (pfromData (Traverse.ptraverse'integer c)) $ \case
          PDNothing -> perror
          PDJust integer -> pmatch (pfromData integer) $ \i ->
            pfromData (Integer.pint'stage i) #== Integer.pstageTerminal)
        (pmatch (pfromData (Traverse.ptraverse'bytes c)) $ \case
          PDNothing -> perror
          PDJust bytes -> pmatch (pfromData bytes) $ \b ->
            pfromData (Bytes.pbytes'stage b) #== Bytes.pstageTerminal))
    $ \terminal -> pif terminal control $
      self # (pexpectJust (Traverse.pstepV1 # control # psourceFor control source sourceStart # pactionNone)) # source # sourceStart

pstage :: forall (s :: S). Term s Traverse.PDataTraverseControlV1 -> Term s PInteger
pstage control = pmatch control $ \c -> pfromData (Traverse.ptraverse'stage c)

streamsLargeConstructor :: forall (s :: S). Term s PBool
streamsLargeConstructor =
  plet (phexByteStr "d86682c2490100000000000000009f01ff") $ \source ->
  plet (Traverse.pinitialControlV1 # 17 # 17) $ \initial ->
  plet (pexpectJust (Traverse.pstepV1 # initial # psourceFor initial source 17 #
      pcon (Traverse.PHeadLargeConstructor (pdata 11) (pdata 1)))) $ \opened ->
  plet (pfinishLargeConstructor # opened # source # 17) $ \largeFields ->
  pmatch largeFields $ \lf -> pmatch (pfromData (Traverse.ptraverse'integer lf)) $ \case
    PDNothing -> perror
    PDJust integerData -> plet (pfromData integerData) $ \integer -> pmatch integer $ \i ->
      pmatch (pfromData (Integer.pint'blob i)) $ \case
        PDNothing -> perror
        PDJust blobData ->
          plet (pexpectJust (Blob.pfinalizeV1 # pfromData blobData)) $ \constructorRoot ->
          plet (Frame.pinitialLargeConstrFrameV1 # constructorRoot # pfromData (Integer.pint'sourceLength i) #
                pfromData (Integer.pint'memory i) # pconstant "" # 1) $ \frame ->
          plet (pexpectJust (Traverse.pstepV1 # largeFields # psourceFor largeFields source 17 # pactionNone)) $ \fieldsOpened ->
          plet (pexpectJust (Traverse.pstepV1 # fieldsOpened # psourceFor fieldsOpened source 17 #
                pcon (Traverse.PHeadScalar (pdata 1)))) $ \fieldOpened ->
          plet (pfinishScalar # fieldOpened # source # 17) $ \fieldTerminal ->
          pmatch fieldTerminal $ \ft -> pmatch (pfromData (Traverse.ptraverse'integer ft)) $ \case
            PDNothing -> perror
            PDJust fieldInteger ->
              plet (pexpectJust (Integer.pfinalizeV1 # pfromData fieldInteger)) $ \fieldSummary ->
              plet (pexpectJust (Traverse.pstepV1 # fieldTerminal # pcon PNothing #
                    pcon (Traverse.PAttachScalar (pdata (psomeData frame))))) $ \attached ->
              plet (pexpectJust (Frame.pappendChildV1 # frame # fieldSummary)) $ \fullFrame ->
              plet (pexpectJust (Traverse.pstepV1 # attached # psourceFor attached source 17 # pactionNone)) $ \closed ->
              plet (pexpectJust (Traverse.pstepV1 # closed # pcon PNothing #
                    pcon (Traverse.PFoldList (pdata fullFrame) (pdata 0) (pdata fieldSummary) (pdata (pcon PNil))))) $ \folded ->
              plet (pexpectJust (Frame.pfoldListChildV1 # fullFrame # 0 # fieldSummary # pcon PNil)) $ \foldedFrame ->
              plet (pexpectJust (Traverse.pstepV1 # folded # pcon PNothing #
                    pcon (Traverse.PFinalizeFrame (pdata foldedFrame) (pdata pnoneData)))) $ \terminal ->
              plet (pexpectJust (Traverse.pfinalizeV1 # terminal)) $ \summary -> pmatch summary $ \s ->
                pand'List
                  [ pfromData (Data.psummary'root s) #== phexByteStr "844cdd8ac8dc97d87e4ed149da121054504365b523034a804a12c014d55c2c44"
                  , pfromData (Data.psummary'cborLength s) #== 17
                  , pfromData (Data.psummary'memory s) #== 9
                  , Traverse.pencodeControlV1 # terminal #== phexByteStr "8a010711111140d87a80d87a80d87a80d8799f835820844cdd8ac8dc97d87e4ed149da121054504365b523034a804a12c014d55c2c441109ff"
                  , Traverse.phashControlV1 # terminal #== phexByteStr "173ab9eb57665546414d5c286c55bc1fcd939a1784a7b800863e9970b82f6c16"
                  ]

popsEmptyChild :: forall (s :: S). Term s PBool
popsEmptyChild =
  plet (phexByteStr "d8799f80ff") $ \source ->
  plet (Traverse.pinitialControlV1 # 17 # 5) $ \initial ->
  plet (Frame.pinitialSmallConstrFrameV1 # 0 # pconstant "" # 1) $ \parent ->
  plet (pexpectJust (Traverse.pstepV1 # initial # psourceFor initial source 17 #
        pcon (Traverse.PHeadSequence (pdata 1)))) $ \parentOpened ->
  plet (Frame.pinitialListFrameV1 # (Frame.phashFrameV1 # parent) # 0) $ \child ->
  plet (pexpectJust (Traverse.pstepV1 # parentOpened # psourceFor parentOpened source 17 #
        pcon (Traverse.PHeadSequence (pdata 0)))) $ \childOpened ->
  plet (pexpectJust (Frame.pfinalizedSummaryV1 # child)) $ \childSummary ->
  plet (pexpectJust (Traverse.pstepV1 # childOpened # pcon PNothing #
        pcon (Traverse.PFinalizeFrame (pdata child) (pdata (psomeData parent))))) $ \attached ->
  plet (pexpectJust (Frame.pappendChildV1 # parent # childSummary)) $ \fullParent ->
  plet (pexpectJust (Traverse.pstepV1 # attached # psourceFor attached source 17 # pactionNone)) $ \closed ->
  plet (pexpectJust (Traverse.pstepV1 # closed # pcon PNothing #
        pcon (Traverse.PFoldList (pdata fullParent) (pdata 0) (pdata childSummary) (pdata (pcon PNil))))) $ \folded ->
  plet (pexpectJust (Frame.pfoldListChildV1 # fullParent # 0 # childSummary # pcon PNil)) $ \foldedParent ->
    pstage (pexpectJust (Traverse.pstepV1 # folded # pcon PNothing #
      pcon (Traverse.PFinalizeFrame (pdata foldedParent) (pdata pnoneData)))) #== Traverse.pstageTerminal

maximumSourceWindow :: forall (s :: S). Term s PBool
maximumSourceWindow =
  plet (phexByteStr ("5f5840" <> concat (replicate 64 "6a") <> "5840" <> concat (replicate 64 "6a") <> "ff")) $ \source ->
  plet (Traverse.pinitialControlV1 # 17 # 134) $ \initial ->
  plet (pexpectJust (Traverse.pstepV1 # initial # psourceFor initial source 17 # pcon (Traverse.PHeadScalar (pdata 134)))) $ \opened ->
  plet (pexpectJust (Traverse.pstepV1 # opened # psourceFor opened source 17 # pactionNone)) $ \parsed ->
  pmatch (pexpectJust (Traverse.pnextSourceSpanV1 # parsed)) $ \(Blob.PCekSourceBlobSpanV1 _ len) ->
    pfromData len #== 109 #&& pfromData len #<= Traverse.pmaxSourceSpan

decodesActiveControls :: forall (s :: S). Term s PBool
decodesActiveControls =
  plet (phexByteStr "8a0101110b0040d87a80d8799f860100110b00d87a80ffd87a80d87a80") $ \integerCbor ->
  plet (phexByteStr "8a01021118460040d87a80d87a80d8799f86010011184600d87a80ffd87a80") $ \bytesCbor ->
  plet (Traverse.pdecodeControlV1 # integerCbor) $ \integer ->
  plet (Traverse.pdecodeControlV1 # bytesCbor) $ \bytes ->
    pstage integer #== Traverse.pstageInteger
      #&& Traverse.pencodeControlV1 # integer #== integerCbor
      #&& pstage bytes #== Traverse.pstageBytes
      #&& Traverse.pencodeControlV1 # bytes #== bytesCbor

malformedInputs :: forall (s :: S). Term s PBool
malformedInputs =
  plet (phexByteStr "9f01ff") $ \source ->
  plet (Traverse.pinitialControlV1 # 17 # 3) $ \initial ->
  plet (pexpectJust (Traverse.pstepV1 # initial # psourceFor initial source 17 # pcon (Traverse.PHeadSequence (pdata 2)))) $ \opened ->
  plet (pexpectJust (Traverse.pstepV1 # opened # psourceFor opened source 17 # pcon (Traverse.PHeadScalar (pdata 1)))) $ \childOpened ->
  plet (pfinishScalar # childOpened # source # 17) $ \childTerminal ->
  plet (Frame.pinitialListFrameV1 # pconstant "" # 2) $ \parent ->
  pmatch childTerminal $ \ct -> pmatch (pfromData (Traverse.ptraverse'integer ct)) $ \case
    PDNothing -> perror
    PDJust childInteger -> plet (pexpectJust (Integer.pfinalizeV1 # pfromData childInteger)) $ \childSummary ->
      plet (pexpectJust (Traverse.pstepV1 # childTerminal # pcon PNothing #
            pcon (Traverse.PAttachScalar (pdata (psomeData parent))))) $ \oneAttached ->
      plet (Traverse.pinitialControlV1 # 17 # 6) $ \smallLarge ->
      plet (phexByteStr "d86682187f80") $ \smallSource ->
      plet (pexpectJust (Traverse.pstepV1 # smallLarge # psourceFor smallLarge smallSource 17 #
            pcon (Traverse.PHeadLargeConstructor (pdata 2) (pdata 0)))) $ \smallOpened ->
        Traverse.pstepV1 # oneAttached # pcon (PJust (phexByteStr "ff")) # pcon (Traverse.PHeadScalar (pdata 1)) #== pcon PNothing
          #&& Traverse.pstepV1 # smallOpened # pcon (PJust (phexByteStr "187f")) # pactionNone #== pcon PNothing
          #&& pmatch childSummary (\s -> pfromData (Data.psummary'cborLength s) #== 1)

papplied ::
  forall (s :: S).
  Term s PByteString -> Term s (PMaybe PByteString) -> Term s Traverse.PDataTraverseActionV1 -> Term s PByteString -> Term s PBool
papplied preCbor source action postCbor =
  Traverse.pencodeControlV1 #
    pexpectJust (Traverse.pstepV1 # (Traverse.pdecodeControlV1 # preCbor) # source # action)
    #== postCbor

maximumHeadControls :: forall (s :: S). Term s PBool
maximumHeadControls =
  plet (phexByteStr "8a01001831193f2b0040d87a80d87a80d87a80d87a80") $ \largePre ->
  plet (phexByteStr "d8668218809fa1d879809f9f9f9f") $ \largeSource ->
  plet (phexByteStr "8a01031831193f2b0340d8799f01ffd8799f86010018340200d87a80ffd87a80d87a80") $ \largePost ->
  plet (phexByteStr "8a01001831193f2b07582030d75ccee289617aaa8612e21cc72076bcebd188667fb119d9224f76f05a2d01d87a80d87a80d87a80d87a80") $ \sequencePre ->
  plet (phexByteStr "d879809f9f9f9f9f9f9f9f9f9f9f") $ \sequenceSource ->
  plet (phexByteStr "8a01061831193f2b0a5820df8903414ebd963ae3a2ed2ca1ec2f15b56e1017fe0701620626657b7fa92a12d87a80d87a80d87a80d87a80") $ \sequencePost ->
  plet (phexByteStr "8a01001831193f2b06582001afca7b219108b07ea760418c63d6be0179f69c368e796a8cd0ba46f5e342fcd87a80d87a80d87a80d87a80") $ \mapPre ->
  plet (phexByteStr "a1d879809f9f9f9f9f9f9f9f9f9f") $ \mapSource ->
  plet (phexByteStr "8a01001831193f2b07582030d75ccee289617aaa8612e21cc72076bcebd188667fb119d9224f76f05a2d01d87a80d87a80d87a80d87a80") $ \mapPost ->
  plet (phexByteStr "8a01001831193f2b165820bf2bd3eb8fc61e7769e9f918c8cdcc9668c96c8be328284060e76931c62005b8d87a80d87a80d87a80d87a80") $ \scalarPre ->
  plet (phexByteStr "410100ff9f009f0000ffffff9f9f") $ \scalarSource ->
  plet (phexByteStr "8a01021831193f2b165820bf2bd3eb8fc61e7769e9f918c8cdcc9668c96c8be328284060e76931c62005b8d87a80d87a80d8799f86010018470200d87a80ffd87a80") $ \scalarPost ->
    pand'List
      [ papplied largePre (pcon (PJust largeSource)) (pcon (Traverse.PHeadLargeConstructor (pdata 2) (pdata 1))) largePost
      , papplied sequencePre (pcon (PJust sequenceSource)) (pcon (Traverse.PHeadSequence (pdata 0))) sequencePost
      , papplied mapPre (pcon (PJust mapSource)) (pcon Traverse.PHeadMap) mapPost
      , papplied scalarPre (pcon (PJust scalarSource)) (pcon (Traverse.PHeadScalar (pdata 2))) scalarPost
      , Traverse.pstepV1 # (Traverse.pdecodeControlV1 # largePre) # pcon (PJust (phexByteStr "d8658218809fa1d879809f9f9f9f")) #
          pcon (Traverse.PHeadLargeConstructor (pdata 2) (pdata 1)) #== pcon PNothing
      , Traverse.pstepV1 # (Traverse.pdecodeControlV1 # sequencePre) # pcon (PJust sequenceSource) #
          pcon (Traverse.PHeadSequence (pdata 1)) #== pcon PNothing
      , Traverse.pstepV1 # (Traverse.pdecodeControlV1 # mapPre) # pcon (PJust (phexByteStr "9fd879809f9f9f9f9f9f9f9f9f9f")) #
          pcon Traverse.PHeadMap #== pcon PNothing
      , Traverse.pstepV1 # (Traverse.pdecodeControlV1 # scalarPre) # pcon (PJust scalarSource) #
          pcon (Traverse.PHeadScalar (pdata 0)) #== pcon PNothing
      ]

psummary :: forall (s :: S). Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s Data.PDataSummaryV1
psummary root cborLength memory = pcon Data.PDataSummaryV1
  { Data.psummary'root = pdata root
  , Data.psummary'cborLength = pdata cborLength
  , Data.psummary'memory = pdata memory
  }

psequence :: forall (s :: S). Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s Data.PDataSequenceSummaryV1
psequence root len payload memory = pcon Data.PDataSequenceSummaryV1
  { Data.pseq'root = pdata root
  , Data.pseq'length = pdata len
  , Data.pseq'payloadCborLength = pdata payload
  , Data.pseq'memory = pdata memory
  }

ponePeak :: forall (s :: S). Term s PInteger -> Term s PByteString -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
ponePeak height hash = pcons # pdata (pcon (Merkle.PFrontierPeak (pdata height) (pdata hash))) # pcon PNil

pframe ::
  forall (s :: S).
  Term s PInteger -> Term s PByteString -> Term s PByteString -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) -> Term s PInteger -> Term s Data.PDataSequenceSummaryV1 -> Term s Frame.PDataFrameV1
pframe kind constructorRoot tailRoot expected childCount peaks cursor sequence = pcon Frame.PDataFrameV1
  { Frame.pframe'kind = pdata kind
  , Frame.pframe'constructor = pdata 0
  , Frame.pframe'constructorCborRoot = pdata constructorRoot
  , Frame.pframe'constructorCborLength = pdata (pif (kind #== Frame.pconstrLargeFrame) 2 0)
  , Frame.pframe'constructorMemory = pdata (pif (kind #== Frame.pconstrLargeFrame) 6 0)
  , Frame.pframe'tail = pdata tailRoot
  , Frame.pframe'expectedChildren = pdata expected
  , Frame.pframe'childCount = pdata childCount
  , Frame.pframe'childPeaks = pdata peaks
  , Frame.pframe'foldCursor = pdata cursor
  , Frame.pframe'sequence = pdata sequence
  }

pmaximumFirstListFoldFrame :: forall (s :: S). Term s Frame.PDataFrameV1
pmaximumFirstListFoldFrame = pframe Frame.plistFrame (pconstant "")
  (phexByteStr "7f014d1984be15f9587409aa022afa8f6c4e1488924df30c012383e3bbf28e2c") 2 2
  (ponePeak 1 (phexByteStr "1bc19900bb2341f556a93b216efdc1e85b076c82d3ef67f35f42367b491cc22d")) 0
  (psequence (phexByteStr "8c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56") 0 0 0)

pmaximumMapFoldFrame :: forall (s :: S). Term s Frame.PDataFrameV1
pmaximumMapFoldFrame = pframe Frame.pmapFrame (pconstant "")
  (phexByteStr "01afca7b219108b07ea760418c63d6be0179f69c368e796a8cd0ba46f5e342fc") 2 2
  (ponePeak 1 (phexByteStr "1f137899e881e56f1776f5788ffd5cdb32aac1b7541970499657c0f354e942bc")) 0
  (psequence (phexByteStr "bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2") 0 0 0)

pmaximumEmptyKeyFrame :: forall (s :: S). Term s Frame.PDataFrameV1
pmaximumEmptyKeyFrame = pframe Frame.pconstrSmallFrame (pconstant "")
  (phexByteStr "30d75ccee289617aaa8612e21cc72076bcebd188667fb119d9224f76f05a2d01") 0 0 (pcon PNil) 0
  (psequence (phexByteStr "8c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56") 0 0 0)

pmaximumEmptyMapParent :: forall (s :: S). Term s Frame.PDataFrameV1
pmaximumEmptyMapParent = pframe Frame.pmapFrame (pconstant "")
  (phexByteStr "01afca7b219108b07ea760418c63d6be0179f69c368e796a8cd0ba46f5e342fc") 2 0 (pcon PNil) 0
  (psequence (phexByteStr "bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2") 0 0 0)

maximumFoldControls :: forall (s :: S). Term s PBool
maximumFoldControls =
  plet (phexByteStr "8a01061831193f2b181a58207af9fbf871cd41cd5754b69c195d71a3d671739de3f37e177e7313de792b51dcd87a80d87a80d87a80d87a80") $ \listPre ->
  plet (phexByteStr "8a01061831193f2b181a58205ff9de6f99abc0e417a8e81850431e4fecf6fe4b0a902e85b8ec2373939440d3d87a80d87a80d87a80d87a80") $ \listPost ->
  plet (psummary (phexByteStr "0c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da2") 1 5) $ \listChild ->
  plet (pcons # pdata (phexByteStr "48e5cbf5c18eade239c5fdea7e7c105612da100e19047628fc75ca7175197fca") # pcon PNil) $ \listSiblings ->
  plet (phexByteStr "8a01061831193f2b193f2a58204af2404008f74a4ee3eb9123aa0bed99e829e1bf31c6ad5c5ca5efceb0292d85d87a80d87a80d87a80d87a80") $ \mapPre ->
  plet (phexByteStr "8a01061831193f2b193f2a582062601ffbae712e9b1f490fc9c1b52b6e7f46560afa3c873539a2a54d45afb98fd87a80d87a80d87a80d87a80") $ \mapPost ->
  plet (psummary (phexByteStr "284d708d538f4a2427de3995ea06dbb2ffadff18ebb763b56498bb66882e17f9") 3 4) $ \key ->
  plet (psummary (phexByteStr "a5423cf93544d255ed2af3806b522663bb0b3a807128a2304524935b7703e347") 16160 48479) $ \value ->
  plet (pcons # pdata (phexByteStr "fc2a353cbdde9156f280ae88c1eadee3d3b42a22d0396ebfa4662dc860f076c9") # pcon PNil) $ \keySiblings ->
  plet (pcons # pdata (phexByteStr "731f16f5e9418e13a0d46f037c791150d0285d1f8d2c96cf39847d0a8c56030d") # pcon PNil) $ \valueSiblings ->
  plet (phexByteStr "8a01061831193f2b0a5820df8903414ebd963ae3a2ed2ca1ec2f15b56e1017fe0701620626657b7fa92a12d87a80d87a80d87a80d87a80") $ \finalizePre ->
  plet (phexByteStr "8a01001831193f2b0a58202ce6e5a25de1f43178e311d53992e750c87a95234d34fddc2f0f6d4477f533f4d87a80d87a80d87a80d87a80") $ \finalizePost ->
    pand'List
      [ papplied listPre (pcon PNothing)
          (pcon (Traverse.PFoldList (pdata pmaximumFirstListFoldFrame) (pdata 1) (pdata listChild) (pdata listSiblings))) listPost
      , papplied mapPre (pcon PNothing)
          (pcon (Traverse.PFoldMap (pdata pmaximumMapFoldFrame) (pdata 0) (pdata key) (pdata value) (pdata keySiblings) (pdata valueSiblings))) mapPost
      , papplied finalizePre (pcon PNothing)
          (pcon (Traverse.PFinalizeFrame (pdata pmaximumEmptyKeyFrame) (pdata (psomeData pmaximumEmptyMapParent)))) finalizePost
      , Traverse.pstepV1 # (Traverse.pdecodeControlV1 # listPre) # pcon PNothing #
          pcon (Traverse.PFoldList (pdata pmaximumFirstListFoldFrame) (pdata 1) (pdata listChild)
            (pdata (pcons # pdata (phexByteStr "49e5cbf5c18eade239c5fdea7e7c105612da100e19047628fc75ca7175197fca") # pcon PNil))) #== pcon PNothing
      , Traverse.pstepV1 # (Traverse.pdecodeControlV1 # mapPre) # pcon PNothing #
          pcon (Traverse.PFoldMap (pdata pmaximumMapFoldFrame) (pdata 1) (pdata key) (pdata value)
            (pdata keySiblings) (pdata valueSiblings)) #== pcon PNothing
      ]

pterminalFrame :: forall (s :: S). Term s PByteString -> Term s PByteString -> Term s Frame.PDataFrameV1
pterminalFrame peakHash sequenceRoot = pframe Frame.pconstrLargeFrame
  (phexByteStr "3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf") (pconstant "") 1 1
  (ponePeak 0 peakHash) 1 (psequence sequenceRoot 1 16164 48487)

pterminalCase ::
  forall (s :: S).
  Term s PByteString -> Term s Frame.PDataFrameV1 -> Term s PByteString -> Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PBool
pterminalCase preCbor frame postCbor expectedRoot expectedLength expectedMemory =
  plet (Traverse.pdecodeControlV1 # preCbor) $ \pre ->
  plet (pexpectJust (Traverse.pstepV1 # pre # pcon PNothing #
        pcon (Traverse.PFinalizeFrame (pdata frame) (pdata pnoneData)))) $ \post ->
  plet (pexpectJust (Traverse.pfinalizeV1 # post)) $ \summary -> pmatch summary $ \s ->
    pand'List
      [ pstage pre #== Traverse.pstageFold
      , Traverse.pencodeControlV1 # post #== postCbor
      , pfromData (Data.psummary'root s) #== expectedRoot
      , pfromData (Data.psummary'cborLength s) #== expectedLength
      , pfromData (Data.psummary'memory s) #== expectedMemory
      ]

maximumDataTerminal :: forall (s :: S). Term s PBool
maximumDataTerminal = pterminalCase
  (phexByteStr "8a01061831193f2b193f2b582064c916e4a790b0d133bb36d04d9e9ecca7dc2f3c679690d2d005f84b311e9d94d87a80d87a80d87a80d87a80")
  (pterminalFrame
    (phexByteStr "9f62b20d5db17ead31389c3864fb7ea3a2f68726b21ef48452c670dab47a8a66")
    (phexByteStr "f64559d8fa739e5dec6e218602ff2ebd0d24b477421f38b1872a2274454f84c7"))
  (phexByteStr "8a01071831193f2b193f2b40d87a80d87a80d87a80d8799f83582077156535ea7ff621233f808b4995b94294f504a0dd78455593440e3d03ad2b6f193f2b19bd6bff")
  (phexByteStr "77156535ea7ff621233f808b4995b94294f504a0dd78455593440e3d03ad2b6f") 16171 48491

maximumRedeemerTerminal :: forall (s :: S). Term s PBool
maximumRedeemerTerminal =
  plet (pframe Frame.pconstrLargeFrame
    (phexByteStr "3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf") (pconstant "") 1 1
    (ponePeak 0 (phexByteStr "20de66bc0f1322c9c61884ce582d6698c9075e35c183a4264c6c7c27fbf1401b")) 1
    (psequence (phexByteStr "0f7bb776751d400f727bf81b02cd7ed66457e144209eb5f9f90e2c6500fe1496") 1 15975 47920)) $ \frame ->
  pterminalCase
    (phexByteStr "8a010600193e6e193e6e582008f6a2dc24df8fbc23b2d4255dda3ca30a2fd28eb361e9ab31bf01732c764eead87a80d87a80d87a80d87a80")
    frame
    (phexByteStr "8a010700193e6e193e6e40d87a80d87a80d87a80d8799f83582026ef420c9e803ba9d74f048b521bff6c99e6a6b4d8aefd077c300a8e31a4dc20193e6e19bb34ff")
    (phexByteStr "26ef420c9e803ba9d74f048b521bff6c99e6a6b4d8aefd077c300a8e31a4dc20") 15982 47924
