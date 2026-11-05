{- | Exact second-wave scan cardinalities and descriptor byte-size ladder.
Inputs are built on the host: reported costs meter the Plutarch predicate,
not Aiken fixture construction. A passing behavior test can exceed the basis.
-}
module Testing.ExecutionFrontiers (tests, measure) where

import Aiken.Cbor (pdeserialise)
import Codec.Serialise (serialise)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.LedgerOutputDescriptor (pledgerValueV1)
import Midgard.NativeScript (pinspectNativeScriptV1)
import Midgard.NativeScriptScanOneShot (ppayloadStructureIsCanonicalV1)
import Plutarch.Evaluate (applyArguments, evalScriptHuge)
import Plutarch.Internal.Term (Config (NoTracing), InternalConfig (..), compileWithInternalConfig)
import Plutarch.Prelude
import Plutarch.Script (Script)
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit
import Testing.FraudProofsFixture (midgardOutputCbor)

measure :: String -> Bool -> Script -> [PD.Data] -> Assertion
measure name accepts script arguments = do
  let (result, budget, traces) = evalScriptHuge $ applyArguments script arguments
  putStrLn $ "EXEC_UNITS " <> name <> " " <> show budget
  case result of
    Right _ -> assertBool "Expected rejection" accepts
    Left err -> assertBool (show err <> " " <> show traces) (not accepts)

compiled :: (forall s. Term s a) -> Script
compiled term = either (error . Text.unpack) id $ compileWithInternalConfig (InternalConfig False False) NoTracing term

scan, inspect, decodeCbor, descriptor :: Script
scan = compiled $ plam $ \raw -> pif (ppayloadStructureIsCanonicalV1 # (pasByteStr # raw)) (pcon PUnit) perror
inspect = compiled $ plam $ \raw -> pif (present $ pinspectNativeScriptV1 # (pasByteStr # raw) # 0 # 0 # pnil) (pcon PUnit) perror
decodeCbor = compiled $ plam $ \raw -> pif (present $ pdeserialise # (pasByteStr # raw)) (pcon PUnit) perror
descriptor = compiled $ plam $ \raw -> pif (present $ pledgerValueV1 # 0 # (pasByteStr # raw)) (pcon PUnit) perror

present :: Term s (PMaybe a) -> Term s PBool
present value = pmatch value $ \case
  PNothing -> pconstant False
  PJust _ -> pconstant True

tests :: TestTree
tests =
  testGroup "Second-wave execution frontiers" $
    [ testCase "reachable maximum payload shapes and node counts" $ do
        BS.length (deep 5446) @?= 16341
        BS.length (wide 5445) @?= 16340
        assertBool "A further wide node exceeds the payload cap" $ BS.length (wide 5446) > 16341
        BS.length (wide 64) @?= 196
        BS.length (deep 64) @?= 195
    , row "worst_case_wide_predicate" True scan (wide 5445)
    , row "worst_case_wide_predicate_refuses_a_malformed_payload" False scan "\x82\x07\x00"
    , row "inspect_refuses_malformed_native_tag" False inspect "\x82\x07\x00"
    , row "cbor_accepts_non_native_tag" True decodeCbor "\x82\x07\x00"
    , row "cbor_refuses_truncated_array" False decodeCbor "\x82"
    , row "derivation_refuses_truncated_output_bytes" False descriptor (BS.take 574 $ output 512)
    , row "derivation_accepts_an_empty_inline_datum" True descriptor (output 0)
    , row "derivation_accepts_a_one_byte_inline_datum" True descriptor (output 1)
    ]
      <> [ row ("curve_" <> shape <> "_" <> show n) True scan (build n)
         | (shape, build) <- [("wide", wide), ("deep", deep)]
         , n <- [64, 256, 1024, 4096]
         ]
      <> [ row name True script bytes
         | (name, script, bytes) <-
             [ ("cmp_inspect_wide_00064", inspect, wide 64)
             , ("cmp_inspect_wide_01024", inspect, wide 1024)
             , ("cmp_inspect_deep_01024", inspect, deep 1024)
             , ("cmp_cbor_wide_00064", decodeCbor, wide 64)
             , ("cmp_cbor_wide_01024", decodeCbor, wide 1024)
             ]
         ]
      <> [ testCase ("descriptor_" <> show size <> "_bytes_derives") $ do
             let bytes = output datumBytes
             BS.length bytes @?= size
             measure ("descriptor_" <> show size) True descriptor [PD.B bytes]
         | (datumBytes, size) <- [(512, 575), (768, 839), (1024, 1103), (1280, 1367), (1408, 1499), (1536, 1631), (1664, 1763), (1792, 1895), (2048, 2159), (2304, 2423), (2560, 2687), (2624, 2753), (2688, 2819), (2944, 3083)]
         ]

row :: String -> Bool -> Script -> BS.ByteString -> TestTree
row name accepts script bytes = testCase name $ measure name accepts script [PD.B bytes]

wide :: Int -> BS.ByteString
wide n = "\x82\x01" <> arrayHeader n <> BS.concat (replicate n "\x82\x04\x00")

deep :: Int -> BS.ByteString
deep containers = BS.concat (replicate containers "\x82\x01\x81") <> "\x82\x04\x00"

arrayHeader :: Int -> BS.ByteString
arrayHeader n
  | n < 24 = BS.singleton (fromIntegral $ 0x80 + n)
  | n < 256 = BS.pack [0x98, fromIntegral n]
  | otherwise = BS.pack [0x99, fromIntegral $ n `div` 256, fromIntegral n]

output :: Int -> BS.ByteString
output n = BS.cons 0xa3 (BS.tail required) <> "\x02" <> definiteBytes datum
  where
    required = midgardOutputCbor (BS.cons 0x60 $ BS.replicate 28 0xaa) 1_000_000 Nothing
    datum = LBS.toStrict $ serialise $ PD.B $ BS.replicate n 0xab
    definiteBytes bytes = header (BS.length bytes) <> bytes
    header size
      | size < 24 = BS.singleton (fromIntegral $ 0x40 + size)
      | size < 256 = BS.pack [0x58, fromIntegral size]
      | otherwise = BS.pack [0x59, fromIntegral $ size `div` 256, fromIntegral size]
