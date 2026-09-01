{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxFieldAccess
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/native-tx-field-access-v1.ak@ — the field-access
              door and its accessors.

The §5.1 envelope half of that module is exercised by "Testing.NativeTxPreimages";
what is tested here is everything the door added: the stride table, the §8.6
certificate asset name, the three carriage tiers, and the slice-only accessors.

Four properties are what the tests are organised around, because they are what
the door exists to own.

__Positional identity.__ §4 removed field-index domain separation, so a field-0
preimage and a field-1 preimage with the same items hash /identically/. The
index is the only thing that tells a reference-input opening from a spend-input
one. Three tests state that directly: one preimage opens at both indices when
the body commits it in both slots, and — with the stride, the structure and the
bytes all held fixed — the very same opening is accepted at the slot the body
commits and refused at its sibling.

__Abort, never clamp.__ Two clamped out-of-range reads are byte-equal, which is
how a clamping read fabricates equality evidence out of a valid block. Every
out-of-range case here asserts an abort.

__Lazy chunk verify.__ Under tier 3 a chunk is hashed the first time a read
reaches it, and a chunk nobody reads is never hashed. The test for this corrupts
the /second/ chunk's digest and then shows that reading an item inside the first
chunk still succeeds while reading one inside the second aborts. A port that
verified eagerly would fail the first of those, and a port that never verified
would fail the second.

__One grammar, one verdict.__ The item head decoder admits exactly the minimal
width, and a non-canonical wrapper is refused at the read even when the
preimage's hash and its arithmetic count check both pass.

The reference encoder below is written from the format — §5.1's envelope, §5.3's
strides, §8.4's split rule — rather than from the port, so a change on either
side fails a test instead of two copies agreeing.
-}
module Testing.NativeTxFieldAccess (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
  Datum (..),
  OutputDatum (..),
  ScriptHash (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PTxInInfo)
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.NativeTxFieldAccess (
  PFieldCarriageV1 (..),
  PFieldViewV1 (..),
  paddressWitnessItemBytes,
  paddressWitnessStride,
  pchunkBytesK,
  pdecodeFieldArrayHeader,
  pemptyFieldCommitment,
  pencodeFieldArrayHeader,
  pencodeFieldPreimage,
  pexpectedChunkCount,
  pfieldCommitment,
  pfieldCommitmentFromItems,
  pfieldHeaderLen,
  pfieldItemAt,
  pfieldItemCount,
  pfieldItemExtent,
  pfieldItemHeaderAt,
  pfieldPreimageCertificateAssetName,
  pfieldReadRange,
  pfieldStride,
  pfieldTotalLength,
  pfieldViewStride,
  pfixedItemWrapperBytes,
  phash28ItemBytes,
  phash28Stride,
  pmaxFieldItemCount,
  pmaxSpendInputsPreimageBytes,
  pmaxTier1RedeemerPreimageBytes,
  pmaxTier3ChunkCount,
  pmaxTransactionAggregateFieldBytes,
  pmaximumCardanoSpendRedeemerCount,
  pauthenticatedFieldView,
  pspendInputItemBytes,
  pspendInputStride,
  pwalkDerivedStride,
 )
import Testing.Eval (passertEval, pfails)
import Testing.FraudProofsFixture (cborInt)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Native Tx Field Access Tests"
    [ fieldAccessGoldenTests
    , testGroup "constants" constantTests
    , testGroup "field_stride" strideTests
    , testGroup "field_preimage_certificate_asset_name" assetNameTests
    , testGroup "authenticated_field_view / positional identity" positionalTests
    , testGroup "authenticated_field_view / all nine nonempty fields" doorServesTests
    , testGroup "authenticated_field_view / empty fields" emptyFieldTests
    , testGroup "watcher recomputation" watcherTests
    , testGroup "authenticated_field_view / tier 1" tier1Tests
    , testGroup "authenticated_field_view / tier 2" tier2Tests
    , testGroup "authenticated_field_view / tier 3" tier3Tests
    , testGroup "accessors" accessorTests
    , testGroup "item head grammar" headerTests
    ]

--------------------------------------------------------------------------------
-- native-tx-field-access-v1-golden.test.ak
--------------------------------------------------------------------------------

fieldAccessGoldenTests :: TestTree
fieldAccessGoldenTests =
  testGroup
    "native-tx-field-access-v1-golden Aiken parity"
    [ testCase "golden_carriage_constants_match_typescript" $
        passertEval $
          pand'List
            [ pmaxFieldItemCount #== 65_535
            , pmaxTransactionAggregateFieldBytes #== 32_768
            , pmaxSpendInputsPreimageBytes #== 32_768
            , pmaximumCardanoSpendRedeemerCount #== 296
            , pchunkBytesK #== 15_900
            , pmaxTier1RedeemerPreimageBytes #== 14_336
            , pmaxTier3ChunkCount #== 3
            , pspendInputItemBytes #== 38
            , phash28ItemBytes #== 28
            , paddressWitnessItemBytes #== 101
            ]
    , testCase "golden_empty_field_commitment_matches_typescript" $
        passertEval $
          pand'List
            [ pencodeFieldPreimage # pnil #== pconstant "\x80"
            , pfieldCommitment # pconstant "\x80" #== pconstant goldenEmptyCommitment
            , pfieldCommitmentFromItems # pnil #== pconstant goldenEmptyCommitment
            , pemptyFieldCommitment #== pconstant goldenEmptyCommitment
            ]
    , testCase "golden_field_strides_match_typescript" $
        passertEval $
          pand'List
            [ pfieldStride # pconstant index #== pconstant stride
            | (index, stride) <- zip [0 .. 8] [40, 40, 0, 30, 30, 0, 0, 103, 0]
            ]
    , testCase "golden_field_array_headers_match_typescript" $
        passertEval $
          pand'List
            [ pencodeFieldArrayHeader # pconstant count #== pconstant bytes
            | (count, bytes) <- goldenArrayHeaders
            ]
    , testCase "golden_field_array_header_decode_matches_typescript" $
        passertEval $
          pand'List
            [ pairIs (pdecodeFieldArrayHeader # pconstant bytes) width count
            | (count, bytes) <- goldenArrayHeaders
            , let width = fromIntegral (BS.length bytes)
            ]
    , testCase "golden_item_wrappers_match_typescript" $
        passertEval $
          pand'List
            [ pencodeDefiniteBytes # pconstant item #== pconstant (header <> item)
            | (item, header) <- goldenWrapperVectors
            ]
    , testCase "golden_expected_chunk_counts_match_typescript" $
        passertEval $
          pand'List
            [ pexpectedChunkCount # 15_900 #== 1
            , pexpectedChunkCount # 15_901 #== 2
            , pexpectedChunkCount # 31_800 #== 2
            , pexpectedChunkCount # 31_801 #== 3
            , pexpectedChunkCount # 32_768 #== 3
            ]
    , testCase "golden_tier3_chunk_digests_match_typescript" $
        passertEval $
          pand'List
            [ plengthBS # pconstant goldenTier3Preimage #== 16_417
            , plengthBS # pconstant (head goldenTier3Chunks) #== 15_900
            , plengthBS # pconstant (goldenTier3Chunks !! 1) #== 517
            , pblake2b_256 # pconstant (head goldenTier3Chunks)
                #== pconstant (head goldenTier3ChunkDigests)
            , pblake2b_256 # pconstant (goldenTier3Chunks !! 1)
                #== pconstant (goldenTier3ChunkDigests !! 1)
            ]
    , testCase "golden_certificate_asset_names_match_typescript" $
        passertEval $
          pand'List
            [ pfieldPreimageCertificateAssetName # pconstant goldenCertificateTxId # pconstant index
                #== pconstant assetName
            | (index, assetName) <- zip [0 .. 8] goldenCertificateAssetNames
            ]
    , testCase "golden_tier3_certificate_shape_matches_typescript" $
        passertEval $
          pand'List
            [ pexpectedChunkCount # 16_417 #== 2
            , pconstant @PInteger (fromIntegral (length goldenTier3ChunkDigests)) #== 2
            , pfieldPreimageCertificateAssetName # pconstant goldenCertificateTxId # 0
                #== pconstant (head goldenCertificateAssetNames)
            ]
    , goldenFieldCase "golden_empty_envelope_matches_typescript" goldenEmptyEnvelope
    , goldenFieldCase "golden_single_item_payload_0_matches_typescript" goldenPayload0
    , goldenFieldCase "golden_single_item_payload_1_matches_typescript" goldenPayload1
    , goldenFieldCase "golden_single_item_payload_23_matches_typescript" goldenPayload23
    , goldenFieldCase "golden_single_item_payload_24_matches_typescript" goldenPayload24
    , goldenFieldCase "golden_single_item_payload_255_matches_typescript" goldenPayload255
    , goldenFieldCase "golden_single_item_payload_256_matches_typescript" goldenPayload256
    , goldenFieldCase "golden_variable_width_walk_matches_typescript" goldenVariableWidth
    , goldenFieldCase "golden_spend_inputs_stride_40_matches_typescript" goldenSpendInputs
    , goldenFieldCase "golden_hash28_stride_30_matches_typescript" goldenHash28
    , goldenFieldCase "golden_address_witness_stride_103_matches_typescript" goldenAddressWitness
    , goldenFieldCase "golden_hash28_two_byte_header_matches_typescript" goldenHash28Wide
    ]

pairIs :: forall s. Term s (PPair PInteger PInteger) -> Integer -> Integer -> Term s PBool
pairIs pair expectedFirst expectedSecond =
  pmatch pair $ \(PPair first second) ->
    first #== pconstant expectedFirst #&& second #== pconstant expectedSecond

data GoldenField = GoldenField
  { goldenItems :: [BS.ByteString]
  , goldenPreimage :: BS.ByteString
  , goldenCommitment :: BS.ByteString
  , goldenHeaderWidth :: Integer
  , goldenStride :: Integer
  , goldenExtents :: [(Integer, Integer)]
  }

goldenFieldCase :: String -> GoldenField -> TestTree
goldenFieldCase name fixture = testCase name $ passertEval (goldenFieldMatches fixture)

goldenFieldMatches :: forall s. GoldenField -> Term s PBool
goldenFieldMatches fixture =
  plet
    ( pcon $
        PWholeView
          (pconstant (goldenPreimage fixture))
          (pconstant (fromIntegral (length (goldenItems fixture))))
          (pconstant (goldenStride fixture))
    )
    $ \view ->
      pand'List $
        [ pencodeFieldPreimage # pbyteStringList (goldenItems fixture)
            #== pconstant (goldenPreimage fixture)
        , pfieldCommitment # pconstant (goldenPreimage fixture)
            #== pconstant (goldenCommitment fixture)
        , pfieldCommitmentFromItems # pbyteStringList (goldenItems fixture)
            #== pconstant (goldenCommitment fixture)
        , pairIs
            (pdecodeFieldArrayHeader # pconstant (goldenPreimage fixture))
            (goldenHeaderWidth fixture)
            (fromIntegral (length (goldenItems fixture)))
        ]
          <> [ pairIs (pfieldItemExtent # view # pconstant index) offset len
             | (index, (offset, len)) <- zip [0 ..] (goldenExtents fixture)
             ]
          <> [ pfieldItemAt # view # pconstant index #== pconstant item
             | (index, item) <- zip [0 ..] (goldenItems fixture)
             ]

pbyteStringList :: forall s. [BS.ByteString] -> Term s (PBuiltinList PByteString)
pbyteStringList = foldr (\value rest -> pcons # pconstant value # rest) pnil

goldenHex :: String -> BS.ByteString
goldenHex = Base16.decodeLenient . BS8.pack

goldenEmptyCommitment, goldenCertificateTxId :: BS.ByteString
goldenEmptyCommitment = goldenHex "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"
goldenCertificateTxId = goldenHex "1920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2"

goldenArrayHeaders :: [(Integer, BS.ByteString)]
goldenArrayHeaders =
  [ (0, goldenHex "80")
  , (1, goldenHex "81")
  , (23, goldenHex "97")
  , (24, goldenHex "9818")
  , (255, goldenHex "98ff")
  , (256, goldenHex "990100")
  , (65_535, goldenHex "99ffff")
  ]

goldenFiller0, goldenFiller1, goldenFiller23, goldenFiller24, goldenFiller255, goldenFiller256 :: BS.ByteString
goldenFiller0 = ""
goldenFiller1 = goldenHex "22"
goldenFiller23 = goldenHex "ccd3dae1e8eff6fd040b121920272e353c434a51585f66"
goldenFiller24 = goldenHex "ebf2f900070e151c232a31383f464d545b626970777e858c"
goldenFiller255 = goldenHex "e4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6"
goldenFiller256 = goldenHex "030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc"

goldenWrapperVectors :: [(BS.ByteString, BS.ByteString)]
goldenWrapperVectors =
  [ (goldenFiller0, goldenHex "40")
  , (goldenFiller1, goldenHex "41")
  , (goldenFiller23, goldenHex "57")
  , (goldenFiller24, goldenHex "5818")
  , (goldenFiller255, goldenHex "58ff")
  , (goldenFiller256, goldenHex "590100")
  ]

goldenTier3Block, goldenTier3Preimage :: BS.ByteString
goldenTier3Block = goldenHex "565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f"
goldenTier3Preimage = BS.take 16_417 (BS.concat (replicate 65 goldenTier3Block))

goldenTier3Chunks :: [BS.ByteString]
goldenTier3Chunks = [BS.take 15_900 goldenTier3Preimage, BS.drop 15_900 goldenTier3Preimage]

goldenTier3ChunkDigests :: [BS.ByteString]
goldenTier3ChunkDigests =
  map
    goldenHex
    [ "eabb6cf3843972474294c28713974f244be7335cd05559417f1f5bd061a72e74"
    , "c7e754df8126357f25157e78b1b1612e400921e11ab80d82c7a55f025e0fbfed"
    ]

goldenCertificateAssetNames :: [BS.ByteString]
goldenCertificateAssetNames = map goldenHex
  [ "7df7bbbe2ab0d3756058cf13f94dc275c78dfbd057eb33687a7eb64206e7f170"
  , "3ff0bc572fec1f90b658fdab128993e808c96a42265f2a7b86da5eac63bd9abb"
  , "3a71c99091cec28b2cad280faa69795803cf130389440088958d634139d9d3ea"
  , "73be379009d9471e07389ecf7bf5404604e8ba58f976a06f17e923d259b9c823"
  , "48103981c2ff91dd4167c8cd21d7b64bd67c38573104a1dbc72e3472962c5510"
  , "e80980b7a92b8bf56f467fed395348354f50982695dc48393236ddfe961a7b53"
  , "d147537e4f5c52949346ef1de32ff8458f1f9c243d3484ea140cb42361afb451"
  , "e4a72ac556e7c0b406a28b899d801d47bdfa4c5507afe561cf9753dced5f1c5e"
  , "fad6ed39a10e33fa1b9fbcab5da89cf7ddf4b5dcf188d3f2d7768126c5a0d798"
  ]

goldenEmptyEnvelope, goldenPayload0, goldenPayload1, goldenPayload23, goldenPayload24, goldenPayload255, goldenPayload256 :: GoldenField
goldenEmptyEnvelope = GoldenField [] (goldenHex "80") goldenEmptyCommitment 1 0 []
goldenPayload0 = goldenPayload goldenFiller0 "8140" "3935ed59a9a9735c3f35edc39c823278aff9d7ae54f16df5ca66d2f925d4acc0" 1 2
goldenPayload1 = goldenPayload goldenFiller1 "814122" "85eb8924d13a6d1488f260ea7a39ea08c55ccb5c57e37a946133f0f772989649" 1 2
goldenPayload23 = goldenPayload goldenFiller23 "8157ccd3dae1e8eff6fd040b121920272e353c434a51585f66" "015ad516310c79401086886c46f3ea9c67cc5003ec4d3e007b672119a26fc9c3" 1 2
goldenPayload24 = goldenPayload goldenFiller24 "815818ebf2f900070e151c232a31383f464d545b626970777e858c" "4d1d83fb6aa49ea06f2519ec990c4e993209123d0928a3f5620881a37c0bf67d" 1 3
goldenPayload255 = goldenPayload goldenFiller255 "8158ffe4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6" "25d188cdd96fd55483bef6295e9a15e2984c9def865b45596ffd5ed11cf6947f" 1 3
goldenPayload256 = goldenPayload goldenFiller256 "81590100030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc" "262527d7f203b93069010893aaf27d42e5d3d15c1a7efa1da4a0a36edd09aa85" 1 4

goldenPayload :: BS.ByteString -> String -> String -> Integer -> Integer -> GoldenField
goldenPayload item preimage commitment headerWidth payloadOffset =
  GoldenField [item] (goldenHex preimage) (goldenHex commitment) headerWidth 0 [(payloadOffset, fromIntegral (BS.length item))]

goldenVariableWidth, goldenSpendInputs, goldenHash28, goldenAddressWitness, goldenHash28Wide :: GoldenField
goldenVariableWidth =
  goldenField
    [ "22"
    , "41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2"
    , "60676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d"
    ]
    "834122581841484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe259012c60676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d"
    "8ea899780b67a87cef3d3041e2566844faf0fcf31ee334d807d9a74b08c50e19"
    1
    0
    [(2, 1), (5, 24), (32, 300)]
goldenSpendInputs =
  goldenField
    [ "825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb190000"
    , "82582041484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a190017"
    , "82582060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323919ffff"
    ]
    "835826825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb190000582682582041484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a190017582682582060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323919ffff"
    "3250a9797dd14257ad1e2d16b637cdab6b4cc50a644f1ac686db54a670901c7b"
    1
    40
    [(3, 38), (43, 38), (83, 38)]
goldenHash28 =
  goldenField
    [ "7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c"
    , "9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b"
    ]
    "82581c7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c581c9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b"
    "7e2ebe7895dd901407cfd4766e1d9318f2e07a564cdd7dcce9caf809e549e835"
    1
    30
    [(3, 28), (33, 28)]
goldenAddressWitness =
  goldenField
    [ "825820bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f965840dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e95"
    , "825820fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd458401a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3"
    ]
    "825865825820bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f965840dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e955865825820fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd458401a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3"
    "1e5f9aec31f249953f6b9a1103eb53c16721d292923da454a3e0d044bc37c59a"
    1
    103
    [(3, 101), (106, 101)]
goldenHash28Wide =
  goldenField
    [ "1a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7"
    , "3940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6"
    , "585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e15"
    , "777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d34"
    , "969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c53"
    , "b5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b72"
    , "d4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91"
    , "f3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0"
    , "121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cf"
    , "31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7ee"
    , "50575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d"
    , "6f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c"
    , "8e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b"
    , "adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a"
    , "ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b8289"
    , "ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8"
    , "0a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7"
    , "2930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6"
    , "484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe05"
    , "676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d24"
    , "868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c43"
    , "a5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b62"
    , "c4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81"
    , "e3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0"
    ]
    "9818581c1a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7581c3940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6581c585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e15581c777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d34581c969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c53581cb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b72581cd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91581cf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0581c121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cf581c31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7ee581c50575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d581c6f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c581c8e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b581cadb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a581cccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b8289581cebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8581c0a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7581c2930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6581c484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe05581c676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d24581c868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c43581ca5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b62581cc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81581ce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0"
    "1d6ce21ef9b069fe085433f972a0e8e41c245e77c3e962541cac91a9347f76c9"
    2
    30
    [ (4, 28)
    , (34, 28)
    , (64, 28)
    , (94, 28)
    , (124, 28)
    , (154, 28)
    , (184, 28)
    , (214, 28)
    , (244, 28)
    , (274, 28)
    , (304, 28)
    , (334, 28)
    , (364, 28)
    , (394, 28)
    , (424, 28)
    , (454, 28)
    , (484, 28)
    , (514, 28)
    , (544, 28)
    , (574, 28)
    , (604, 28)
    , (634, 28)
    , (664, 28)
    , (694, 28)
    ]

goldenField :: [String] -> String -> String -> Integer -> Integer -> [(Integer, Integer)] -> GoldenField
goldenField items preimage commitment headerWidth stride extents =
  GoldenField
    (map goldenHex items)
    (goldenHex preimage)
    (goldenHex commitment)
    headerWidth
    stride
    extents

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

constantTests :: [TestTree]
constantTests =
  [ -- §5.3: every fixed-width item carries a two-byte wrapper, so the stride is
    -- the item width plus two. Stated as an identity rather than as three
    -- separate literals, because that is the relation the accessors rely on.
    testCase "each fixed stride is its item width plus the two-byte wrapper" $
      passertEval $
        pand'List
          [ pspendInputStride #== pspendInputItemBytes + pfixedItemWrapperBytes
          , phash28Stride #== phash28ItemBytes + pfixedItemWrapperBytes
          , paddressWitnessStride #== paddressWitnessItemBytes + pfixedItemWrapperBytes
          ]
  , testCase "the aggregate field bound is three chunks' worth" $
      passertEval $
        (pexpectedChunkCount # pmaxTransactionAggregateFieldBytes) #== pmaxTier3ChunkCount
  , -- §8.4's split rule at its boundaries. The K+1 case is what makes the tier
    -- ladder a partition: one byte over and the field needs two chunks.
    testCase "expected_chunk_count is ceil(total / K)" $
      passertEval $
        pand'List
          [ (pexpectedChunkCount # 1) #== 1
          , (pexpectedChunkCount # pchunkBytesK) #== 1
          , (pexpectedChunkCount # (pchunkBytesK + 1)) #== 2
          , (pexpectedChunkCount # (pchunkBytesK * 2)) #== 2
          , (pexpectedChunkCount # (pchunkBytesK * 2 + 1)) #== 3
          ]
  , testCase "expected_chunk_count aborts on an empty field" $
      pfails $ pexpectedChunkCount # 0
  , -- The E1 divergence, pinned so that re-cutting K is a deliberate edit and
    -- not a silent one. See the module header of the port.
    testCase "chunk_bytes_k is still the pre-erratum 15900" $
      passertEval $ pchunkBytesK #== 15900
  ]

--------------------------------------------------------------------------------
-- The stride table
--------------------------------------------------------------------------------

strideTests :: [TestTree]
strideTests =
  [ testCase "§5.3's stride table, field by field" $
      passertEval $
        pand'List
          [ (pfieldStride # pconstant (fromIntegral i)) #== pconstant (fromIntegral expected)
          | (i, expected) <- zip [0 :: Int ..] referenceStrides
          ]
  , -- The sentinel is what an accessor branches on to choose walking over
    -- arithmetic, so it has to be exactly zero rather than merely falsy.
    testCase "the variable-width fields answer the walk sentinel" $
      passertEval $
        pand'List
          [ (pfieldStride # pconstant (fromIntegral i)) #== pwalkDerivedStride
          | i <- [2, 5, 6, 8 :: Int]
          ]
  , testCase "aborts on a field index of nine" $ pfails $ pfieldStride # 9
  , testCase "aborts on a negative field index" $ pfails $ pfieldStride # (-1)
  ]

-- | §5.3's table, written out from the spec rather than read off the port.
referenceStrides :: [Integer]
referenceStrides = [40, 40, 0, 30, 30, 0, 0, 103, 0]

--------------------------------------------------------------------------------
-- The §8.6 certificate asset name
--------------------------------------------------------------------------------

assetNameTests :: [TestTree]
assetNameTests =
  [ testCase "is blake2b_256 of the field index byte followed by the tx id" $
      passertEval $
        pand'List
          [ (pfieldPreimageCertificateAssetName # pconstant txId # pconstant (fromIntegral i))
            #== pconstant (referenceAssetName txId (fromIntegral i))
          | i <- [0 .. 8 :: Int]
          ]
  , -- The single-byte prefix is domain separation: two fields of one
    -- transaction must not share a certificate token.
    testCase "differs between two field indices of the same transaction" $
      passertEval $
        pnot
          #$ (pfieldPreimageCertificateAssetName # pconstant txId # 0)
          #== (pfieldPreimageCertificateAssetName # pconstant txId # 1)
  , -- Both bounds are what make the 33-byte preimage unambiguous, so both are
    -- enforced rather than assumed of the caller.
    testCase "aborts on a transaction id that is not 32 bytes" $
      pfails $
        pfieldPreimageCertificateAssetName # pconstant (BS.replicate 31 0x01) # 0
  , testCase "aborts on a field index of nine" $
      pfails $ pfieldPreimageCertificateAssetName # pconstant txId # 9
  , testCase "aborts on a negative field index" $
      pfails $ pfieldPreimageCertificateAssetName # pconstant txId # (-1)
  ]

-- | @blake2b_256(field_index_byte ‖ tx_id)@, recomputed from §8.6's erratum.
referenceAssetName :: BS.ByteString -> Integer -> BS.ByteString
referenceAssetName tid index = blake2b256 (BS.cons (fromIntegral index) tid)

--------------------------------------------------------------------------------
-- Positional identity
--------------------------------------------------------------------------------

{- | §4 removed field-index domain separation from the commitment, and these two
tests are what that costs and what still holds. The same preimage opens at both
spend-input and reference-input indices when the body commits it in both slots —
there is nothing in the hash to stop it — and opens at neither when the body
commits something else there.
-}
positionalTests :: [TestTree]
positionalTests =
  [ testCase "one preimage opens at two indices the body commits it in" $
      passertEval $
        pand'List
          [ (pfieldItemCount # openInline bothSlots 0 spendInputPreimage) #== 3
          , (pfieldItemCount # openInline bothSlots 1 spendInputPreimage) #== 3
          ]
  , -- Same preimage, same stride, same structural checks: the only thing that
    -- differs between this and the accepting case above is which slot the body
    -- commits it in.
    testCase "accepts at the slot the body commits it in" $
      passertEval $
        (pfieldItemCount # openInline onlySpendSlot 0 spendInputPreimage) #== 3
  , testCase "aborts at the sibling index, whose slot commits another field" $
      pfails $ pfieldItemCount # openInline onlySpendSlot 1 spendInputPreimage
  , testCase "aborts on a field index of nine" $
      pfails $ pfieldItemCount # openInline defaultBody 9 hash28Preimage
  , -- The two hash slots differ here, so this is the check that the door read
    -- slot 3 and not slot 4.
    testCase "reads the required-signers slot at index 4, not the observers' one" $
      passertEval $
        (pfieldItemCount # openInline defaultBody 4 signersPreimage) #== 2
  , testCase "aborts when the observers' preimage is offered at the signers' index" $
      pfails $ pfieldItemCount # openInline defaultBody 4 hash28Preimage
  ]

--------------------------------------------------------------------------------
-- One door, all nine nonempty fields
--------------------------------------------------------------------------------

doorServesTests :: [TestTree]
doorServesTests =
  [ doorServesCase "door_serves_field_0_spend_inputs" 0
  , doorServesCase "door_serves_field_1_reference_inputs" 1
  , doorServesCase "door_serves_field_2_outputs" 2
  , doorServesCase "door_serves_field_3_required_observers" 3
  , doorServesCase "door_serves_field_4_required_signers" 4
  , doorServesCase "door_serves_field_5_mint" 5
  , doorServesCase "door_serves_field_6_script_witnesses" 6
  , doorServesCase "door_serves_field_7_address_witnesses" 7
  , doorServesCase "door_serves_field_8_redeemer_witnesses" 8
  ]

doorServesCase :: String -> Integer -> TestTree
doorServesCase name fieldIndex = testCase name $ passertEval $
  plet (openExactInlineField fieldIndex preimage) $ \view ->
    pand'List
      [ pencodeFieldPreimage # pbyteStringList items #== pconstant preimage
      , (pfieldItemCount # view) #== 3
      , (pfieldItemAt # view # 0) #== pconstant (items !! 0)
      , (pfieldItemAt # view # 1) #== pconstant (items !! 1)
      , (pfieldItemAt # view # 2) #== pconstant (items !! 2)
      , pconstant @PInteger (fromIntegral (length items)) #== 3
      ]
  where
    items = aikenFieldItems (fromIntegral fieldIndex)
    preimage = fieldPreimage items

--------------------------------------------------------------------------------
-- Empty fields
--------------------------------------------------------------------------------

emptyFieldTests :: [TestTree]
emptyFieldTests =
  [ testCase "empty_field_opens_as_the_single_byte_form" $
      passertEval $
        pand'List $
          [pencodeFieldPreimage # pnil #== pconstant "\x80"]
            <> [ (pfieldItemCount # openExactEmptyField fieldIndex) #== 0
               | fieldIndex <- [0 .. 8]
               ]
  ]

openExactEmptyField :: forall s. Integer -> Term s PFieldViewV1
openExactEmptyField fieldIndex = openExactInlineField fieldIndex "\x80"

openExactInlineField :: forall s. Integer -> BS.ByteString -> Term s PFieldViewV1
openExactInlineField fieldIndex preimage =
  pauthenticatedFieldView
    # exactSourceVerifiedT fieldIndex preimage
    # witnessSetT (exactSourceWitnessSet fieldIndex preimage)
    # pconstant fieldIndex
    # pcon (PInline (pdata (pconstant preimage)))
    # inputsT []
    # pdata (pconstant aikenCertificatePolicy)

exactSourceVerifiedT :: forall s. Integer -> BS.ByteString -> Term s PVerifiedMidgardNativeTxCompact
exactSourceVerifiedT fieldIndex preimage =
  pcon $
    PVerifiedMidgardNativeTxCompact
      { pverified'txId = pconstant aikenSampleTxId
      , pverified'version = 1
      , pverified'txCompact =
          pcon $
            PNativeTxCompact
              { pcompact'body = exactSourceBodyT fieldIndex preimage
              , pcompact'witnessSetHash = pconstant (wsHashOf (exactSourceWitnessSet fieldIndex preimage))
              , pcompact'validityCode = 0
              }
      }

exactSourceBodyT :: forall s. Integer -> BS.ByteString -> Term s PNativeTxBodyCompact
exactSourceBodyT fieldIndex preimage =
  pcon $
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash = pconstant (exactSourceSlot fieldIndex 0 preimage)
      , pbodyCompact'referenceInputsHash = pconstant (exactSourceSlot fieldIndex 1 preimage)
      , pbodyCompact'outputsHash = pconstant (exactSourceSlot fieldIndex 2 preimage)
      , pbodyCompact'fee = 0
      , pbodyCompact'validityIntervalStart = -1
      , pbodyCompact'validityIntervalEnd = -1
      , pbodyCompact'requiredObserversHash = pconstant (exactSourceSlot fieldIndex 3 preimage)
      , pbodyCompact'requiredSignersHash = pconstant (exactSourceSlot fieldIndex 4 preimage)
      , pbodyCompact'mintHash = pconstant (exactSourceSlot fieldIndex 5 preimage)
      , pbodyCompact'scriptIntegrityHash = pconstant zeroHash32
      , pbodyCompact'auxiliaryDataHash = pconstant zeroHash32
      , pbodyCompact'networkId = 255
      }

exactSourceWitnessSet :: Integer -> BS.ByteString -> Ws
exactSourceWitnessSet fieldIndex preimage =
  Ws
    { wAddr = exactSourceSlot fieldIndex 7 preimage
    , wScript = exactSourceSlot fieldIndex 6 preimage
    , wRedeemer = exactSourceSlot fieldIndex 8 preimage
    }

exactSourceSlot :: Integer -> Integer -> BS.ByteString -> BS.ByteString
exactSourceSlot fieldIndex slotIndex preimage
  | fieldIndex == slotIndex = blake2b256 preimage
  | otherwise = zeroHash32

--------------------------------------------------------------------------------
-- Watcher recomputation
--------------------------------------------------------------------------------

watcherTests :: [TestTree]
watcherTests =
  [ testCase "watcher_recomputes_every_commitment_from_bytes_alone" $
      passertEval $
        pand'List [watcherRecomputes fieldIndex | fieldIndex <- [0 .. 8]]
  ]

watcherRecomputes :: forall s. Integer -> Term s PBool
watcherRecomputes fieldIndex =
  (pblake2b_256 # pconstant preimage)
    #== exactSourceExpectedCommitment fieldIndex preimage
  where
    preimage = fieldPreimage (aikenFieldItems (fromIntegral fieldIndex))

exactSourceExpectedCommitment :: forall s. Integer -> BS.ByteString -> Term s PByteString
exactSourceExpectedCommitment fieldIndex preimage = case fieldIndex of
  0 -> pmatch body $ \PNativeTxBodyCompact {pbodyCompact'spendInputsHash} -> pbodyCompact'spendInputsHash
  1 -> pmatch body $ \PNativeTxBodyCompact {pbodyCompact'referenceInputsHash} -> pbodyCompact'referenceInputsHash
  2 -> pmatch body $ \PNativeTxBodyCompact {pbodyCompact'outputsHash} -> pbodyCompact'outputsHash
  3 -> pmatch body $ \PNativeTxBodyCompact {pbodyCompact'requiredObserversHash} -> pbodyCompact'requiredObserversHash
  4 -> pmatch body $ \PNativeTxBodyCompact {pbodyCompact'requiredSignersHash} -> pbodyCompact'requiredSignersHash
  5 -> pmatch body $ \PNativeTxBodyCompact {pbodyCompact'mintHash} -> pbodyCompact'mintHash
  6 -> pmatch witnessSet $ \PNativeTxWitnessSetCompact {pwitnessSetCompact'scriptTxWitsHash} ->
    pfromData pwitnessSetCompact'scriptTxWitsHash
  7 -> pmatch witnessSet $ \PNativeTxWitnessSetCompact {pwitnessSetCompact'addrTxWitsHash} ->
    pfromData pwitnessSetCompact'addrTxWitsHash
  8 -> pmatch witnessSet $ \PNativeTxWitnessSetCompact {pwitnessSetCompact'redeemerTxWitsHash} ->
    pfromData pwitnessSetCompact'redeemerTxWitsHash
  _ -> perror
  where
    body =
      pmatch (exactSourceVerifiedT fieldIndex preimage) $
        \PVerifiedMidgardNativeTxCompact {pverified'txCompact} ->
          pmatch pverified'txCompact $ \PNativeTxCompact {pcompact'body} -> pcompact'body
    witnessSet = witnessSetT (exactSourceWitnessSet fieldIndex preimage)

--------------------------------------------------------------------------------
-- Tier 1 — inline carriage
--------------------------------------------------------------------------------

tier1Tests :: [TestTree]
tier1Tests =
  [ testCase "accepts a preimage the body commits, and counts its items" $
      passertEval $
        pand'List
          [ (pfieldItemCount # openInline defaultBody 3 hash28Preimage) #== 3
          , (pfieldViewStride # openInline defaultBody 3 hash28Preimage) #== 30
          , (pfieldTotalLength # openInline defaultBody 3 hash28Preimage)
              #== pconstant (fromIntegral (BS.length hash28Preimage))
          ]
  , testCase "aborts when the preimage hashes to something else" $
      pfails $ pfieldItemCount # openInline defaultBody 3 (hash28PreimageOf [0x71, 0x72, 0x73])
  , -- §7.4: the declared count and the byte length must agree by arithmetic for
    -- a fixed-stride field. A header claiming one item fewer leaves the last
    -- item's bytes unaccounted for.
    testCase "aborts when a fixed-stride header undercounts its items" $
      pfails $ pfieldItemCount # openInlineCommitting 3 undercountedPreimage
  , testCase "aborts when a fixed-stride header overcounts its items" $
      pfails $ pfieldItemCount # openInlineCommitting 3 overcountedPreimage
  , -- §5.1 fail-closed for the variable-width fields: the walk must land
    -- exactly on the end.
    testCase "accepts a well-formed variable-width preimage" $
      passertEval $
        pand'List
          [ (pfieldItemCount # openInlineCommitting 2 variablePreimage) #== 3
          , (pfieldViewStride # openInlineCommitting 2 variablePreimage) #== 0
          ]
  , testCase "aborts when a variable-width preimage has trailing bytes" $
      pfails $ pfieldItemCount # openInlineCommitting 2 (variablePreimage <> "\x00")
  , testCase "aborts when a variable-width header undercounts its items" $
      pfails $ pfieldItemCount # openInlineCommitting 2 variableUndercounted
  , testCase "aborts when the preimage exceeds the aggregate field bound" $
      pfails $ pfieldItemCount # openInlineCommitting 3 oversizePreimage
  , -- §2.5's positional table: fields 6–8 are only readable once the supplied
    -- witness set re-derives to the compact structure's `witness_set_hash`.
    testCase "accepts a witness-set field when the witness set re-derives" $
      passertEval $
        (pfieldItemCount # openWitness defaultWitnessSet 7 addressWitnessPreimage) #== 1
  , testCase "aborts when the supplied witness set is not the committed one" $
      pfails $ pfieldItemCount # openWitness otherWitnessSet 7 addressWitnessPreimage
  , -- The `or {}` in Aiken short-circuits, so a body field never consults the
    -- witness set at all. A garbage witness set has to be harmless at field 3
    -- and fatal at field 7, and this pair is what says so.
    testCase "a body field ignores the witness set entirely" $
      passertEval $
        (pfieldItemCount # openWitness otherWitnessSet 3 hash28Preimage) #== 3
  ]

--------------------------------------------------------------------------------
-- Tier 2 — raw UTxO carriage
--------------------------------------------------------------------------------

tier2Tests :: [TestTree]
tier2Tests =
  [ testCase "reads the preimage out of the named reference input" $
      passertEval $
        (pfieldItemCount # openRawUtxo 3 [bytesRefIn hash28Preimage] 0) #== 3
  , testCase "reads the input at the index it is given, not the first one" $
      passertEval $
        (pfieldItemCount # openRawUtxo 3 [bytesRefIn "junk", bytesRefIn hash28Preimage] 1)
          #== 3
  , testCase "aborts when the named input carries other bytes" $
      pfails $ pfieldItemCount # openRawUtxo 3 [bytesRefIn hash28Preimage, bytesRefIn "junk"] 1
  , testCase "aborts on a negative reference-input index" $
      pfails $ pfieldItemCount # openRawUtxo 3 [bytesRefIn hash28Preimage] (-1)
  , testCase "aborts when the index is past the reference inputs" $
      pfails $ pfieldItemCount # openRawUtxo 3 [bytesRefIn hash28Preimage] 1
  , -- §8.5 says raw carriage is a nothing-but-bytes *inline* datum; a datum
    -- hash carries no bytes to read.
    testCase "aborts when the named input has no inline datum" $
      pfails $ pfieldItemCount # openRawUtxo 3 [noDatumRefIn] 0
  , testCase "aborts when the inline datum is not a byte string" $
      pfails $ pfieldItemCount # openRawUtxo 3 [dataRefIn (PD.I 1)] 0
  ]

--------------------------------------------------------------------------------
-- Tier 3 — certified chunk carriage
--------------------------------------------------------------------------------

tier3Tests :: [TestTree]
tier3Tests =
  [ testCase "accepts a certified two-chunk field and counts it arithmetically" $
      passertEval $
        pand'List
          [ (pfieldItemCount # openCertified defaultCert) #== pconstant bigItemCount
          , (pfieldTotalLength # openCertified defaultCert)
              #== pconstant (fromIntegral (BS.length bigPreimage))
          ]
  , testCase "reads an item that lies wholly inside the first chunk" $
      passertEval $
        (pfieldItemAt # openCertified defaultCert # 0) #== pconstant (bigItem 0)
  , testCase "reads an item that lies wholly inside the last chunk" $
      passertEval $
        (pfieldItemAt # openCertified defaultCert # pconstant (bigItemCount - 1))
          #== pconstant (bigItem (fromIntegral bigItemCount - 1))
  , -- §8.8 straddle awareness: this item's 28 payload bytes cross the chunk
    -- boundary, so the read has to stitch two chunks and verify both.
    testCase "reads an item that straddles the chunk boundary" $
      passertEval $
        (pfieldItemAt # openCertified defaultCert # pconstant straddlingIndex)
          #== pconstant (bigItem (fromIntegral straddlingIndex))
  , -- Lazy verify, stated as the two halves it actually is. Chunk 1's digest is
    -- wrong in both cases; only the read that reaches chunk 1 notices.
    testCase "a wrong digest on an untouched chunk does not stop a read of another" $
      passertEval $
        (pfieldItemAt # openCertified defaultCert {cDigests = corruptedDigests} # 0)
          #== pconstant (bigItem 0)
  , testCase "a wrong digest is fatal the moment a read reaches that chunk" $
      pfails $
        pfieldItemAt
          # openCertified defaultCert {cDigests = corruptedDigests}
          # pconstant (bigItemCount - 1)
  , -- §8.4's lower bound is what makes the tier ladder a partition. Without it
    -- a one-chunk "certificate" would re-carry a preimage that tiers 1–2
    -- structurally validate, skipping every check they run.
    testCase "aborts when the certified length is within the tier-2 bound" $
      pfails $ pfieldItemCount # openCertified smallCert
  , testCase "aborts when the certified length exceeds the aggregate bound" $
      pfails $ pfieldItemCount # openCertified defaultCert {cTotalLength = Just 32769}
  , -- §8.6: the token names (tx_id, field_index), and the datum must agree with
    -- the transaction the door was handed rather than with itself.
    testCase "aborts when the certificate names another transaction" $
      pfails $ pfieldItemCount # openCertified defaultCert {cDatumTxId = Just otherTxId}
  , testCase "aborts when the certificate names another field" $
      pfails $ pfieldItemCount # openCertified defaultCert {cDatumFieldIndex = Just 4}
  , testCase "aborts when the certificate input holds no certificate token" $
      pfails $ pfieldItemCount # openCertified defaultCert {cTokenPolicy = Just otherPolicy}
  , testCase "aborts when the certificate token names another field" $
      pfails $ pfieldItemCount # openCertified defaultCert {cTokenFieldIndex = Just 4}
  , testCase "aborts when the certificate input has no inline datum" $
      pfails $ pfieldItemCount # openCertified defaultCert {cInlineDatum = False}
  , testCase "aborts on a negative certificate reference-input index" $
      pfails $ pfieldItemCount # openCertified defaultCert {cCertIndex = -1}
  , testCase "aborts when the digest vector is not the expected chunk count" $
      pfails $ pfieldItemCount # openCertified defaultCert {cDigests = take 1 defaultDigests}
  , testCase "aborts when the chunk indices are not the expected chunk count" $
      pfails $ pfieldItemCount # openCertified defaultCert {cChunkIndices = Just [1]}
  , -- §8.4's split is deterministic: every chunk but the last is exactly K.
    testCase "aborts when the chunks are not cut at the deterministic boundary" $
      pfails $ pfieldItemCount # openCertified defaultCert {cChunks = Just misCutChunks}
  , -- A variable-width field's count under tier 3 exists only in the §5.1
    -- header and nothing affordable authenticates it, so the door declines to
    -- answer rather than hand back a number nobody checked. Reads still work.
    testCase "refuses to count a variable-width field under tier 3" $
      pfails $ pfieldItemCount # openCertified bigVariableCert
  , testCase "still serves reads of a variable-width field under tier 3" $
      passertEval $
        (pfieldItemAt # openCertified bigVariableCert # 0)
          #== pconstant (bigVariableItem 0)
  , testCase "whole_and_chunked_views_agree_on_the_same_bytes" $
      passertEval wholeAndChunkedViewsAgree
  ]

wholeAndChunkedViewsAgree :: forall s. Term s PBool
wholeAndChunkedViewsAgree =
  plet (openInlineCommitting 3 agreementPreimage) $ \whole ->
    plet (openCertified agreementCert) $ \chunked ->
      (pfieldItemAt # chunked # 529) #== (pfieldItemAt # whole # 529)
        #&& (pfieldReadRange # chunked # 15_898 # 6)
          #== (pfieldReadRange # whole # 15_898 # 6)

--------------------------------------------------------------------------------
-- Accessors
--------------------------------------------------------------------------------

accessorTests :: [TestTree]
accessorTests =
  [ testCase "field_header_len is the §5.1 header's own width" $
      passertEval $
        pand'List
          [ (pfieldHeaderLen # openInlineCommitting 3 (hash28PreimageOf (replicate 3 0x41))) #== 1
          , (pfieldHeaderLen # openInlineCommitting 3 (hash28PreimageOf (replicate 30 0x41))) #== 2
          , (pfieldHeaderLen # openCertified defaultCert) #== 3
          ]
  , testCase "field_item_at returns each item of a fixed-stride field" $
      passertEval $
        pand'List
          [ (pfieldItemAt # openInline defaultBody 3 hash28Preimage # pconstant (fromIntegral i))
            #== pconstant (BS.replicate 28 (fromIntegral (0x41 + i)))
          | i <- [0 .. 2 :: Int]
          ]
  , testCase "field_item_at returns each item of a variable-width field" $
      passertEval $
        pand'List
          [ (pfieldItemAt # openInlineCommitting 2 variablePreimage # pconstant (fromIntegral i))
            #== pconstant (variableItems !! i)
          | i <- [0 .. 2 :: Int]
          ]
  , testCase "field_item_extent gives the payload's offset and length" $
      passertEval $
        pmatch (pfieldItemExtent # openInline defaultBody 3 hash28Preimage # 1) $
          \(PPair offset len) ->
            pand'List [offset #== (1 + 30 + 2), len #== 28]
  , -- §7.3 abort-never-clamp, on both ends.
    testCase "field_item_at aborts past the last item" $
      pfails $ pfieldItemAt # openInline defaultBody 3 hash28Preimage # 3
  , testCase "field_item_at aborts on a negative index" $
      pfails $ pfieldItemAt # openInline defaultBody 3 hash28Preimage # (-1)
  , testCase "field_read_range aborts on a read that runs past the preimage" $
      pfails $ pfieldReadRange # openInline defaultBody 3 hash28Preimage # 80 # 20
  , testCase "field_read_range aborts on a negative offset" $
      pfails $ pfieldReadRange # openInline defaultBody 3 hash28Preimage # (-1) # 1
  , testCase "field_read_range returns the empty string for a zero-length read" $
      passertEval $
        (pfieldReadRange # openInline defaultBody 3 hash28Preimage # 5 # 0) #== pconstant ""
  , testCase "rewrapped_preimages_still_pass_the_construction_check" $
      passertEval $
        ( plengthBS # pconstant rewrappedConstructionPreimage
            #== (plengthBS #$ pencodeFieldPreimage # pbyteStringList rewrappedConstructionItems)
        )
          #&& ( (pfieldItemCount # openInlineCommitting 3 rewrappedConstructionPreimage)
                  #== 3
              )
  , -- §6.1: the fixed-stride arithmetic says *where* the item begins; only the
    -- wrapper says that it is spelled the one admissible way. Both preimages
    -- below pass the hash check and the §7.4 count check, and differ only in a
    -- wrapper byte.
    testCase "field_item_at refuses a non-canonically wrapped fixed-stride item" $
      pfails $ pfieldItemAt # openInlineCommitting 3 wrapperCorruptedPreimage # 1
  , testCase "field_item_at still serves the untouched items of that preimage" $
      passertEval $
        (pfieldItemAt # openInlineCommitting 3 wrapperCorruptedPreimage # 0)
          #== pconstant (BS.replicate 28 0x41)
  ]

--------------------------------------------------------------------------------
-- The §5.1 item head grammar
--------------------------------------------------------------------------------

{- | The item head decoder is the tree's only reader of a §5.1 item head, and
its acceptance set is narrower than CBOR's: minimal width, capped at
@59 LLLL@. Each case below is driven through a whole-preimage view whose bytes
are the head under test.
-}
headerTests :: [TestTree]
headerTests =
  [ testCase "decodes the packed head" $
      passertEval $ pheadIs (BS.pack [0x40 + 5]) 5
  , testCase "decodes the one-byte head" $
      passertEval $ pheadIs (BS.pack [0x58, 24]) 24
  , testCase "decodes the two-byte head" $
      passertEval $ pheadIs (BS.pack [0x59, 0x01, 0x00]) 256
  , -- Minimality, in both directions, and each fixture carries a payload of the
    -- length it declares — so the head is well-formed CBOR and is refused for
    -- its width alone.
    testCase "refuses a one-byte head spelling a packable length" $
      pfails $ pheadIs (BS.pack [0x58, 23]) 23
  , testCase "refuses a two-byte head spelling a one-byte length" $
      pfails $ pheadIs (BS.pack [0x59, 0x00, 0xff]) 255
  , -- The grammar stops two forms short of CBOR's: `5a` and `5b` are both
    -- well-formed CBOR and both reject here.
    testCase "refuses the four-byte CBOR head" $
      pfails $ pheadIs (BS.pack [0x5a, 0, 0, 1, 0]) 256
  , testCase "refuses a head that is not a byte string at all" $
      pfails $ pheadIs (BS.pack [0x81]) 1
  ]

{- | Drives 'pfieldItemHeaderAt' over a one-item field 2 preimage whose item
head is @headBytes@ and whose payload is @payloadLength@ bytes.

Field 2 is variable-width, so no §7.4 arithmetic applies — but the §5.1 walk
does, and it steps through the very decoder under test. A head the grammar
refuses therefore aborts at view construction rather than at the read, which is
the same verdict by the same code and is what "one grammar, one verdict" means.
-}
pheadIs :: forall s. BS.ByteString -> Integer -> Term s PBool
pheadIs headBytes payloadLength =
  pmatch (pfieldItemHeaderAt # view # 1) $ \(PPair offset len) ->
    pand'List
      [ offset #== pconstant (1 + fromIntegral (BS.length headBytes))
      , len #== pconstant payloadLength
      ]
  where
    view =
      openInlineCommitting
        2
        ( "\x81"
            <> headBytes
            <> BS.replicate (fromIntegral payloadLength) 0x77
        )

--------------------------------------------------------------------------------
-- Driving the door
--------------------------------------------------------------------------------

-- | Tier 1, with the body's hash slots supplied by the caller.
openInline :: forall s. Body -> Integer -> BS.ByteString -> Term s PFieldViewV1
openInline body fieldIndex preimage =
  door body defaultWitnessSet fieldIndex (inlineCarriage preimage) []

{- | Tier 1 over a body that commits this preimage at this index — the shape
almost every structural test wants, where the hash check is satisfied by
construction so that the /next/ check is the one under test.
-}
openInlineCommitting :: forall s. Integer -> BS.ByteString -> Term s PFieldViewV1
openInlineCommitting fieldIndex preimage =
  openInline (bodyCommitting fieldIndex preimage) fieldIndex preimage

-- | Tier 1 at a witness-set index, with the witness set supplied by the caller.
openWitness :: forall s. Ws -> Integer -> BS.ByteString -> Term s PFieldViewV1
openWitness ws fieldIndex preimage =
  door
    (bodyCommitting fieldIndex preimage)
    ws
    fieldIndex
    (inlineCarriage preimage)
    []

-- | Tier 2.
openRawUtxo :: forall s. Integer -> [TxInInfo] -> Integer -> Term s PFieldViewV1
openRawUtxo fieldIndex referenceInputs index =
  door
    (bodyCommitting fieldIndex hash28Preimage)
    defaultWitnessSet
    fieldIndex
    (pcon (PRawUtxo (pdata (pconstant index))))
    referenceInputs

inlineCarriage :: forall s. BS.ByteString -> Term s PFieldCarriageV1
inlineCarriage preimage = pcon (PInline (pdata (pconstant preimage)))

{- | The door itself. The witness-set commitment in the compact structure is
always 'defaultWitnessSet''s, so a caller passing 'otherWitnessSet' is
substituting one the transaction never committed.
-}
door ::
  forall s.
  Body ->
  Ws ->
  Integer ->
  Term s PFieldCarriageV1 ->
  [TxInInfo] ->
  Term s PFieldViewV1
door body ws fieldIndex carriage referenceInputs =
  pauthenticatedFieldView
    # verifiedT body
    # witnessSetT ws
    # pconstant fieldIndex
    # carriage
    # inputsT referenceInputs
    # pdata (pconstant certificatePolicy)

verifiedT :: forall s. Body -> Term s PVerifiedMidgardNativeTxCompact
verifiedT body =
  pcon $
    PVerifiedMidgardNativeTxCompact
      { pverified'txId = pconstant txId
      , pverified'version = 1
      , pverified'txCompact =
          pcon $
            PNativeTxCompact
              { pcompact'body = bodyT body
              , pcompact'witnessSetHash = pconstant (wsHashOf defaultWitnessSet)
              , pcompact'validityCode = 3
              }
      }

bodyT :: forall s. Body -> Term s PNativeTxBodyCompact
bodyT body =
  pcon $
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash = pconstant (bSpendInputs body)
      , pbodyCompact'referenceInputsHash = pconstant (bReferenceInputs body)
      , pbodyCompact'outputsHash = pconstant (bOutputs body)
      , pbodyCompact'fee = 1_000_000
      , pbodyCompact'validityIntervalStart = 0
      , pbodyCompact'validityIntervalEnd = 1
      , pbodyCompact'requiredObserversHash = pconstant (bObservers body)
      , pbodyCompact'requiredSignersHash = pconstant (bSigners body)
      , pbodyCompact'mintHash = pconstant (bMint body)
      , pbodyCompact'scriptIntegrityHash = pconstant (hash32 0x07)
      , pbodyCompact'auxiliaryDataHash = pconstant (hash32 0x08)
      , pbodyCompact'networkId = 1
      }

witnessSetT :: forall s. Ws -> Term s PNativeTxWitnessSetCompact
witnessSetT ws =
  pcon $
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash = pdata (pconstant (wAddr ws))
      , pwitnessSetCompact'scriptTxWitsHash = pdata (pconstant (wScript ws))
      , pwitnessSetCompact'redeemerTxWitsHash = pdata (pconstant (wRedeemer ws))
      }

inputsT :: forall s. [TxInInfo] -> Term s (PBuiltinList (PAsData PTxInInfo))
inputsT = pconstant

--------------------------------------------------------------------------------
-- The body's six hash slots
--------------------------------------------------------------------------------

-- | The body's six field commitments, as a record so a test can move one.
data Body = Body
  { bSpendInputs :: BS.ByteString
  , bReferenceInputs :: BS.ByteString
  , bOutputs :: BS.ByteString
  , bObservers :: BS.ByteString
  , bSigners :: BS.ByteString
  , bMint :: BS.ByteString
  }

{- | Six distinct commitments, so that a transposed slot shows up as a hash
mismatch rather than as an accidental match. Slot 3 commits 'hash28Preimage'
and slot 4 commits 'signersPreimage'.
-}
defaultBody :: Body
defaultBody =
  Body
    { bSpendInputs = hash32 0x01
    , bReferenceInputs = hash32 0x02
    , bOutputs = hash32 0x03
    , bObservers = blake2b256 hash28Preimage
    , bSigners = blake2b256 signersPreimage
    , bMint = hash32 0x06
    }

{- | Slots 0 and 1 both commit 'spendInputPreimage' — which §4 permits, since
the commitment carries no field index and the two fields share a stride.
-}
bothSlots :: Body
bothSlots =
  defaultBody
    { bSpendInputs = blake2b256 spendInputPreimage
    , bReferenceInputs = blake2b256 spendInputPreimage
    }

-- | Only slot 0 commits it, so slot 1 is the positional refusal.
onlySpendSlot :: Body
onlySpendSlot = defaultBody {bSpendInputs = blake2b256 spendInputPreimage}

-- | 'defaultBody' with slot @fieldIndex@ set to this preimage's commitment.
bodyCommitting :: Integer -> BS.ByteString -> Body
bodyCommitting fieldIndex preimage = case fieldIndex of
  0 -> defaultBody {bSpendInputs = commitment}
  1 -> defaultBody {bReferenceInputs = commitment}
  2 -> defaultBody {bOutputs = commitment}
  3 -> defaultBody {bObservers = commitment}
  4 -> defaultBody {bSigners = commitment}
  5 -> defaultBody {bMint = commitment}
  _ -> defaultBody
  where
    commitment = blake2b256 preimage

--------------------------------------------------------------------------------
-- The witness set
--------------------------------------------------------------------------------

data Ws = Ws
  { wAddr :: BS.ByteString
  , wScript :: BS.ByteString
  , wRedeemer :: BS.ByteString
  }

{- | The committed witness set. Slot 7 — address witnesses — commits
'addressWitnessPreimage'.
-}
defaultWitnessSet :: Ws
defaultWitnessSet =
  Ws
    { wAddr = blake2b256 addressWitnessPreimage
    , wScript = hash32 0x12
    , wRedeemer = hash32 0x13
    }

{- | A witness set that also commits 'addressWitnessPreimage' at slot 7 but
re-derives to a different @witness_set_hash@. That is the point: the preimage
would pass its own hash check, and the door still refuses because the witness
set it came from is not the one the compact structure committed.
-}
otherWitnessSet :: Ws
otherWitnessSet = defaultWitnessSet {wScript = hash32 0x22}

-- | @encode_native_tx_witness_set_compact@, written out from §2.5.
encodeWitnessSet :: Ws -> BS.ByteString
encodeWitnessSet w =
  BS.concat ["\x83", defBytes32 (wAddr w), defBytes32 (wScript w), defBytes32 (wRedeemer w)]

wsHashOf :: Ws -> BS.ByteString
wsHashOf = blake2b256 . encodeWitnessSet

--------------------------------------------------------------------------------
-- Tier 3 fixtures
--------------------------------------------------------------------------------

-- | The knobs the tier-3 negative cases turn; 'defaultCert' verifies.
data Cert = Cert
  { cFieldIndex :: Integer
  , cPreimage :: BS.ByteString
  , cTotalLength :: Maybe Integer
  , cDigests :: [BS.ByteString]
  , cChunks :: Maybe [BS.ByteString]
  , cChunkIndices :: Maybe [Integer]
  , cCertIndex :: Integer
  , cDatumTxId :: Maybe BS.ByteString
  , cDatumFieldIndex :: Maybe Integer
  , cTokenPolicy :: Maybe CurrencySymbol
  , cTokenFieldIndex :: Maybe Integer
  , cInlineDatum :: Bool
  }

certFor :: Integer -> BS.ByteString -> Cert
certFor fieldIndex preimage =
  Cert
    { cFieldIndex = fieldIndex
    , cPreimage = preimage
    , cTotalLength = Nothing
    , cDigests = map blake2b256 (chunksOf preimage)
    , cChunks = Nothing
    , cChunkIndices = Nothing
    , cCertIndex = 0
    , cDatumTxId = Nothing
    , cDatumFieldIndex = Nothing
    , cTokenPolicy = Nothing
    , cTokenFieldIndex = Nothing
    , cInlineDatum = True
    }

-- | A fixed-stride field (3) over two chunks.
defaultCert :: Cert
defaultCert = certFor 3 bigPreimage

defaultDigests :: [BS.ByteString]
defaultDigests = cDigests defaultCert

-- | Chunk 1's digest is wrong; chunk 0's is right.
corruptedDigests :: [BS.ByteString]
corruptedDigests = case defaultDigests of
  [first, _] -> [first, BS.replicate 32 0xff]
  _ -> error "corruptedDigests: fixture is two chunks"

-- | Within the tier-2 bound, so tier 3 must refuse it outright.
smallCert :: Cert
smallCert = certFor 3 hash28Preimage

-- | A variable-width field (2) over two chunks.
bigVariableCert :: Cert
bigVariableCert = certFor 2 bigVariablePreimage

-- | The deterministic split with its last boundary moved one byte early.
misCutChunks :: [BS.ByteString]
misCutChunks =
  [ BS.take (fromIntegral pchunkBytesKRef - 1) bigPreimage
  , BS.drop (fromIntegral pchunkBytesKRef - 1) bigPreimage
  ]

openCertified :: forall s. Cert -> Term s PFieldViewV1
openCertified c =
  door
    (bodyCommitting (cFieldIndex c) (cPreimage c))
    defaultWitnessSet
    (cFieldIndex c)
    ( pcon
        ( PCertified
            { pcertified'certRefInputIndex = pdata (pconstant (cCertIndex c))
            , pcertified'chunkRefInputIndices = pdata (pconstant chunkIndices)
            }
        )
    )
    referenceInputs
  where
    chunks = maybe (chunksOf (cPreimage c)) id (cChunks c)
    chunkIndices =
      maybe [1 .. fromIntegral (length chunks)] id (cChunkIndices c)
    referenceInputs = certRefIn c : map bytesRefIn chunks

certRefIn :: Cert -> TxInInfo
certRefIn c =
  TxInInfo
    (outRefN 0)
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin certificateScript)))
        ( adaValue 2_000_000
            <> singleton
              (maybe certificatePolicy id (cTokenPolicy c))
              ( TokenName
                  ( toBuiltin
                      ( referenceAssetName
                          txId
                          (maybe (cFieldIndex c) id (cTokenFieldIndex c))
                      )
                  )
              )
              1
        )
        ( if cInlineDatum c
            then OutputDatum (Datum (dataToBuiltinData datum))
            else NoOutputDatum
        )
        Nothing
    )
  where
    datum =
      PD.Constr
        0
        [ PD.B (BS.replicate 28 0x31)
        , PD.B (maybe txId id (cDatumTxId c))
        , PD.I (maybe (cFieldIndex c) id (cDatumFieldIndex c))
        , PD.I
            ( maybe (fromIntegral (BS.length (cPreimage c))) id (cTotalLength c)
            )
        , PD.List (map PD.B (cDigests c))
        ]

--------------------------------------------------------------------------------
-- Reference preimages
--------------------------------------------------------------------------------

{- | §5.1: a definite array header followed by one definite byte string per
item. Written out here rather than taken from the port.
-}
fieldPreimage :: [BS.ByteString] -> BS.ByteString
fieldPreimage items = arrayHeader (length items) <> BS.concat (map wrapItem items)

arrayHeader :: Int -> BS.ByteString
arrayHeader n
  | n <= 23 = BS.pack [fromIntegral (0x80 + n)]
  | n <= 255 = BS.pack [0x98, fromIntegral n]
  | n <= 65535 = BS.pack [0x99, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]
  | otherwise = error "arrayHeader: out of fixture range"

wrapItem :: BS.ByteString -> BS.ByteString
wrapItem bytes
  | n <= 23 = BS.cons (fromIntegral (0x40 + n)) bytes
  | n <= 255 = BS.pack [0x58, fromIntegral n] <> bytes
  | n <= 65535 = BS.pack [0x59, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)] <> bytes
  | otherwise = error "wrapItem: out of fixture range"
  where
    n = BS.length bytes

{- | §5.3 fields 0 and 1: @82 ‖ 58 20 tx_id ‖ 19 index_be16@, 38 bytes an item,
stride 40. The fixed three-byte output index is what makes the width constant.
-}
spendInputItem :: Int -> BS.ByteString
spendInputItem i =
  BS.concat
    [ "\x82"
    , defBytes32 (BS.replicate 32 (fromIntegral (0x81 + i)))
    , "\x19"
    , BS.pack [0, fromIntegral i]
    ]

spendInputPreimage :: BS.ByteString
spendInputPreimage = fieldPreimage [spendInputItem i | i <- [0 .. 2]]

-- | Three 28-byte items — §5.3's field 3 and 4 shape, stride 30.
hash28Preimage :: BS.ByteString
hash28Preimage = hash28PreimageOf [0x41, 0x42, 0x43]

hash28PreimageOf :: [Int] -> BS.ByteString
hash28PreimageOf fills = fieldPreimage [BS.replicate 28 (fromIntegral f) | f <- fills]

-- | Two items, so a preimage offered at slot 4 is distinguishable by its count.
signersPreimage :: BS.ByteString
signersPreimage = hash28PreimageOf [0x51, 0x52]

-- | §5.3 field 7: @82 ‖ 58 20 vkey ‖ 58 40 signature@, 101 bytes, stride 103.
addressWitnessPreimage :: BS.ByteString
addressWitnessPreimage =
  fieldPreimage
    [ BS.concat
        [ "\x82"
        , defBytes32 (BS.replicate 32 0x61)
        , "\x58\x40" <> BS.replicate 64 0x62
        ]
    ]

-- | A variable-width field: three items of three different widths.
variableItems :: [BS.ByteString]
variableItems = [BS.replicate 4 0x71, BS.replicate 30 0x72, BS.replicate 300 0x73]

variablePreimage :: BS.ByteString
variablePreimage = fieldPreimage variableItems

-- | The same items under a header claiming one fewer, so the walk stops short.
variableUndercounted :: BS.ByteString
variableUndercounted = arrayHeader 2 <> BS.concat (map wrapItem variableItems)

-- | Three items under a header claiming two, and two under a header claiming three.
undercountedPreimage, overcountedPreimage :: BS.ByteString
undercountedPreimage =
  arrayHeader 2 <> BS.concat [wrapItem (BS.replicate 28 f) | f <- [0x41, 0x42, 0x43]]
overcountedPreimage =
  arrayHeader 3 <> BS.concat [wrapItem (BS.replicate 28 f) | f <- [0x41, 0x42]]

{- | Item 1's wrapper tag moved from @58@ to @59@. The preimage's length and its
declared count are untouched, so §7.4's arithmetic still passes and only the
per-read wrapper check catches it.
-}
wrapperCorruptedPreimage :: BS.ByteString
wrapperCorruptedPreimage =
  BS.concat
    [ BS.take wrapperOffset hash28Preimage
    , BS.singleton 0x59
    , BS.drop (wrapperOffset + 1) hash28Preimage
    ]
  where
    wrapperOffset = 1 + 30

-- | One byte over the aggregate field bound, as a fixed-stride field.
oversizePreimage :: BS.ByteString
oversizePreimage = hash28PreimageOf (replicate 1093 0x41)

--------------------------------------------------------------------------------
-- The big (tier-3) preimages
--------------------------------------------------------------------------------

-- | 600 items of 28 bytes: @3 + 30·600 = 18003@ bytes, so two chunks.
bigItemCount :: Integer
bigItemCount = 600

bigItem :: Int -> BS.ByteString
bigItem i = BS.replicate 28 (fromIntegral (i `mod` 251))

bigPreimage :: BS.ByteString
bigPreimage = fieldPreimage [bigItem i | i <- [0 .. fromIntegral bigItemCount - 1]]

-- | The exact Aiken 600-item straddle fixture used to compare view variants.
agreementPreimage :: BS.ByteString
agreementPreimage =
  arrayHeader 600 <> BS.concat (replicate 600 (wrapItem agreementItem))

agreementItem :: BS.ByteString
agreementItem = aikenH28 5

agreementCert :: Cert
agreementCert = certFor 3 agreementPreimage

rewrappedConstructionItems :: [BS.ByteString]
rewrappedConstructionItems = map aikenH28 [1, 2, 3]

rewrappedConstructionPreimage :: BS.ByteString
rewrappedConstructionPreimage = case rewrappedConstructionItems of
  first : rest -> "\x83\x00\x00" <> first <> BS.concat (map wrapItem rest)
  [] -> error "rewrappedConstructionPreimage: fixture is empty"

aikenFieldItems :: Int -> [BS.ByteString]
aikenFieldItems fieldIndex = [aikenFieldItem fieldIndex seed | seed <- [1, 2, 3]]

aikenFieldItem :: Int -> Int -> BS.ByteString
aikenFieldItem fieldIndex seed
  | fieldIndex == 0 || fieldIndex == 1 =
      "\x82"
        <> defBytes32 (aikenH32 seed)
        <> "\x19\x00"
        <> BS.singleton (fromIntegral seed)
  | fieldIndex == 2 =
      "\xa2\x00"
        <> wrapItem ("\x60" <> aikenH28 seed)
        <> "\x01\x82"
        <> cborInt (fromIntegral (1_000_000 * seed + 7))
        <> "\xa0"
  | fieldIndex == 3 || fieldIndex == 4 = aikenH28 seed
  | fieldIndex == 5 =
      "\x82"
        <> wrapItem (aikenH28 seed)
        <> "\xa1"
        <> wrapItem (aikenH28 (seed + 1))
        <> cborInt (fromIntegral (seed + 1))
  | fieldIndex == 6 =
      "\x82\x03"
        <> wrapItem (BS.take (20 + seed * 9) (aikenH64 seed))
  | fieldIndex == 7 =
      "\x82"
        <> defBytes32 (aikenH32 seed)
        <> "\x58\x40"
        <> aikenH64 seed
  | fieldIndex == 8 =
      "\x84\x00"
        <> cborInt (fromIntegral seed)
        <> wrapItem (BS.take (1 + seed * 5) (aikenH32 seed))
        <> "\x82"
        <> cborInt (fromIntegral (1_000 + seed))
        <> cborInt (fromIntegral (2_000 + seed))
  | otherwise = error "aikenFieldItem: field index outside 0..8"

aikenH32 :: Int -> BS.ByteString
aikenH32 seed =
  blake2b256 (BS.pack [0, 0, 0, fromIntegral seed])

aikenH28 :: Int -> BS.ByteString
aikenH28 = BS.take 28 . aikenH32

aikenH64 :: Int -> BS.ByteString
aikenH64 seed = aikenH32 seed <> aikenH32 (seed + 100)

{- | The item whose 28 payload bytes cross the chunk boundary. Item @i@'s payload
begins at @3 + 30·i + 2@, so the straddler is the one with
@offset < K < offset + 28@.
-}
straddlingIndex :: Integer
straddlingIndex =
  head
    [ fromIntegral i
    | i <- [0 .. fromIntegral bigItemCount - 1 :: Int]
    , let offset = 3 + 30 * i + 2
    , offset < fromIntegral pchunkBytesKRef
    , offset + 28 > fromIntegral pchunkBytesKRef
    ]

-- | A variable-width field over two chunks: 500 items alternating 40 and 50 bytes.
bigVariableItem :: Int -> BS.ByteString
bigVariableItem i =
  BS.replicate (if even i then 40 else 50) (fromIntegral (i `mod` 251))

bigVariablePreimage :: BS.ByteString
bigVariablePreimage = fieldPreimage [bigVariableItem i | i <- [0 .. 499]]

{- | §8.4's split: chunk @j@ is bytes @[j·K, (j+1)·K)@ with a ragged last chunk.
Recomputed here from the rule.
-}
chunksOf :: BS.ByteString -> [BS.ByteString]
chunksOf bytes
  | BS.null bytes = []
  | BS.length bytes <= k = [bytes]
  | otherwise = BS.take k bytes : chunksOf (BS.drop k bytes)
  where
    k = fromIntegral pchunkBytesKRef

-- | @env@-free copy of K, so the fixtures do not read the constant under test.
pchunkBytesKRef :: Integer
pchunkBytesKRef = 15900

--------------------------------------------------------------------------------
-- Reference CBOR and hashing
--------------------------------------------------------------------------------

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

defBytes32 :: BS.ByteString -> BS.ByteString
defBytes32 h = "\x58\x20" <> h

hash32 :: Int -> BS.ByteString
hash32 n = blake2b256 (BS.pack [fromIntegral n])

--------------------------------------------------------------------------------
-- Identities
--------------------------------------------------------------------------------

txId, otherTxId, aikenSampleTxId, zeroHash32 :: BS.ByteString
txId = BS.replicate 32 0x0a
otherTxId = BS.replicate 32 0x0b
aikenSampleTxId = BS.replicate 32 0x11
zeroHash32 = BS.replicate 32 0

certificatePolicy, otherPolicy, aikenCertificatePolicy :: CurrencySymbol
certificatePolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x91))
otherPolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x92))
aikenCertificatePolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x22))

certificateScript, carriageScript :: BS.ByteString
certificateScript = BS.replicate 28 0x93
carriageScript = BS.replicate 28 0x94

adaValue :: Integer -> Value
adaValue = singleton (CurrencySymbol "") (TokenName "")

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId (toBuiltin (BS.replicate 32 0x01)))

-- | §8.5 raw carriage: a nothing-but-bytes inline datum.
bytesRefIn :: BS.ByteString -> TxInInfo
bytesRefIn bytes = dataRefIn (PD.B bytes)

dataRefIn :: PD.Data -> TxInInfo
dataRefIn d =
  TxInInfo
    (outRefN 1)
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin carriageScript)))
        (adaValue 2_000_000)
        (OutputDatum (Datum (dataToBuiltinData d)))
        Nothing
    )

noDatumRefIn :: TxInInfo
noDatumRefIn =
  TxInInfo
    (outRefN 1)
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin carriageScript)))
        (adaValue 2_000_000)
        NoOutputDatum
        Nothing
    )
