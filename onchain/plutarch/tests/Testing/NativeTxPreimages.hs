{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxPreimages
Description : Behavioural tests for the Plutarch ports of
              @lib/midgard/fraud-proofs/native-tx/preimages.ak@ and the §5.1
              envelope in @lib/midgard/native-tx-field-access-v1.ak@.

The nine field preimages, and the header grammar they all share.

Three things these tests exist to pin.

**§5.1 is narrower than CBOR.** Minimal width only, capped at the `99 NNNN`
form. The four-byte `9a` head is well-formed CBOR and must reject here, and a
count of 24 spelt in the packed range or a count of 200 spelt in `99` must reject
too. Every non-minimal spelling gets its own case, because a lenient header
reader gives one field several preimages and therefore several commitments.

**The empty-field commitment is a pinned literal.** All nine empty fields share
it, since §4 hashes the bytes with no field index, so it is a constant rather
than something to recompute. It is checked here against the producer, exactly as
the Aiken test file checks it, so the pin cannot drift.

**Mint never re-encodes.** Every other field is verified by decode-then-re-encode,
so a lenient decoder shows up in the byte comparison. Field 5 is walked in place
instead, to keep a large mint out of `Data`, which makes the canonical rules the
*only* thing keeping one mint from having two encodings. Each rule — ascending
keys, no duplicates, minimal widths, non-empty policy, non-zero quantity — is
tested on the encoder and on the decoder separately, and
`verify_canonical_mint_preimage_cbor` is tested a third time, since it walks the
bytes on its own path.
-}
module Testing.NativeTxPreimages (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import PlutusTx.Builtins qualified as Builtins
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Prelude

import PlutusLedgerApi.V3 (Data (..))

import Midgard.FraudProofs.NativeTx.Compact (pencodeNativeTxFieldPreimageLengthsV1)
import Midgard.FraudProofs.NativeTx.Components (
  pencodeFixedOutputIndex,
  pencodeMidgardAddressWitness,
  pencodeMidgardRedeemerWitness,
  pencodeMidgardTxInput,
  pencodeMidgardVersionedScript,
 )
import Midgard.FraudProofs.NativeTx.Preimages (
  pcanonicalBytesKeyPrecedes,
  pdecodeMidgardTxAddressWitnessesPreimageCbor,
  pdecodeMidgardTxByteListPreimageCbor,
  pdecodeMidgardTxHash28ListPreimageCbor,
  pdecodeMidgardTxInputsPreimageCbor,
  pdecodeMidgardTxMintPreimageCbor,
  pdecodeMidgardTxOutputsPreimageCbor,
  pdecodeMidgardTxRedeemerWitnessesPreimageCbor,
  pdecodeMidgardTxScriptWitnessesPreimageCbor,
  pencodeAddressWitnessPreimage,
  pencodeHash28ListPreimage,
  pencodeInputPreimage,
  pencodeMintPolicyItem,
  pencodeMintPreimage,
  pmintPolicyItems,
  pencodeOutputPreimage,
  pencodeRedeemerWitnessPreimage,
  pencodeScriptWitnessPreimage,
  pverifyCanonicalMintPreimageCbor,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardAddressWitness (..),
  PMidgardCredential (..),
  PMidgardExecutionUnits (..),
  PMidgardRedeemerPurpose (..),
  PMidgardRedeemerWitness (..),
  PMidgardScriptLanguage (..),
  PMidgardTxInput (..),
  PMidgardTxOutput (..),
  PMidgardValue (..),
  PMidgardVersionedScript (..),
  PNativeTxFieldPreimageLengthsV1 (..),
 )
import Midgard.NativeTxFieldAccess (
  PFieldViewV1 (..),
  pchunkBytesK,
  pdecodeFieldArrayHeader,
  pdecodeFieldArrayHeaderAt,
  pemptyFieldCommitment,
  pencodeFieldArrayHeader,
  pencodeFieldPreimage,
  pfieldCommitment,
  pfieldCommitmentFromItems,
  pfieldItemAt,
  pfieldItemExtent,
  pfieldStride,
 )
import Plutarch.LedgerApi.AssocMap (PAssocMap (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Native Tx Preimages Tests"
    [ fieldItemsGoldenTests
    , envelopeTests
    , producerTests
    , decoderTests
    , mintTests
    , q24AdaMintedTests
    ]

--------------------------------------------------------------------------------
-- native-tx-field-items-v1-golden.test.ak
--------------------------------------------------------------------------------

fieldItemsGoldenTests :: TestTree
fieldItemsGoldenTests =
  testGroup
    "native-tx-field-items-v1-golden Aiken parity"
    ( map goldenFieldItemCase goldenFieldItemVectors
        <> goldenProducerRoundTripTests
        <> goldenItemEncoderTests
        <> goldenNegativeTests
        <> goldenCrossCuttingTests
    )

data GoldenFieldItemVector = GoldenFieldItemVector
  { goldenItemName :: String
  , goldenItemFieldIndex :: Integer
  , goldenItemStride :: Integer
  , goldenItemBytes :: [BS.ByteString]
  , goldenItemPreimage :: BS.ByteString
  , goldenItemCommitment :: BS.ByteString
  , goldenItemHeaderWidth :: Integer
  , goldenItemExtents :: [(Integer, Integer)]
  }

goldenFieldItemCase :: GoldenFieldItemVector -> TestTree
goldenFieldItemCase vector =
  testCase (goldenItemName vector) $ holds (goldenFieldItemMatches vector)

goldenFieldItemMatches :: forall s. GoldenFieldItemVector -> Term s PBool
goldenFieldItemMatches vector =
  plet
    ( pcon $
        PWholeView
          (pconstant (goldenItemPreimage vector))
          (pconstant (fromIntegral (length (goldenItemBytes vector))))
          (pconstant (goldenItemStride vector))
    )
    $ \view ->
      pall' $
        [ pencodeFieldPreimage # itemsT (goldenItemBytes vector)
            #== pconstant (goldenItemPreimage vector)
        , pfieldCommitment # pconstant (goldenItemPreimage vector)
            #== pconstant (goldenItemCommitment vector)
        , pfieldCommitmentFromItems # itemsT (goldenItemBytes vector)
            #== pconstant (goldenItemCommitment vector)
        , ppairIs
            (pdecodeFieldArrayHeader # pconstant (goldenItemPreimage vector))
            (goldenItemHeaderWidth vector)
            (fromIntegral (length (goldenItemBytes vector)))
        , pfieldStride # pconstant (goldenItemFieldIndex vector)
            #== pconstant (goldenItemStride vector)
        ]
          <> [ ppairIs (pfieldItemExtent # view # pconstant index) offset len
             | (index, (offset, len)) <- zip [0 ..] (goldenItemExtents vector)
             ]
          <> [ pfieldItemAt # view # pconstant index #== pconstant item
             | (index, item) <- zip [0 ..] (goldenItemBytes vector)
             ]

ppairIs :: forall s. Term s (PPair PInteger PInteger) -> Integer -> Integer -> Term s PBool
ppairIs pair expectedFirst expectedSecond =
  pmatch pair $ \(PPair first second) ->
    first #== pconstant expectedFirst #&& second #== pconstant expectedSecond

goldenItemsHex :: String -> BS.ByteString
goldenItemsHex = Base16.decodeLenient . BS8.pack

gv :: String -> Integer -> Integer -> [String] -> String -> String -> Integer -> [[Integer]] -> GoldenFieldItemVector
gv name fieldIndex stride itemHex preimageHex commitmentHex headerWidth extents =
  GoldenFieldItemVector
    name
    fieldIndex
    stride
    (map goldenItemsHex itemHex)
    (goldenItemsHex preimageHex)
    (goldenItemsHex commitmentHex)
    headerWidth
    (map goldenExtent extents)
  where
    goldenExtent [offset, len] = (offset, len)
    goldenExtent _ = error "invalid golden field-item extent"

goldenFieldItemVectors :: [GoldenFieldItemVector]
goldenFieldItemVectors =
  [ gv "golden_f0_empty_matches_typescript" 0 40 [] "80" "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0" 1 []
  , gv "golden_f0_single_matches_typescript" 0 40 ["825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb190000"] "815826825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb190000" "0e2116b60e68dfb2a1f3c469ee4427050922a4d46650e55abb825630557f134f" 1 [[3,38]]
  , gv "golden_f0_index_boundaries_matches_typescript" 0 40 ["82582041484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a190000","82582060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b3239190017","8258207f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a5158190018","8258209ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970771900ff","825820bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f96190100","825820dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb519ffff"] "86582682582041484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a190000582682582060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323919001758268258207f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a515819001858268258209ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970771900ff5826825820bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f961901005826825820dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb519ffff" "624f216aaa343abb76bed8295181cd24bd58b2cb3e8c0a7dce2d24335b0fd341" 1 [[3,38],[43,38],[83,38],[123,38],[163,38],[203,38]]
  , gv "golden_f1_empty_matches_typescript" 1 40 [] "80" "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0" 1 []
  , gv "golden_f1_single_matches_typescript" 1 40 ["825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb190000"] "815826825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb190000" "0e2116b60e68dfb2a1f3c469ee4427050922a4d46650e55abb825630557f134f" 1 [[3,38]]
  , gv "golden_f1_pair_matches_typescript" 1 40 ["8258201a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3190001","8258203940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b1219ffff"] "8258268258201a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf319000158268258203940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b1219ffff" "e067f8ded4bee00b24c3255dfd2bd99049bf3110ceeb0fd3eb4891d2018584d0" 1 [[3,38],[43,38]]
  , gv "golden_f2_empty_matches_typescript" 2 0 [] "80" "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0" 1 []
  , gv "golden_f2_address_and_value_matches_typescript" 2 0 ["a200581d60222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8df01821a001e8480a0"] "815829a200581d60222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8df01821a001e8480a0" "a5574a7b36cebf27e84f0557dc966552702d60d7c0deef077b130a5cb4b3dc68" 1 [[3,41]]
  , gv "golden_f2_with_multiasset_value_matches_typescript" 2 0 ["a200581d6041484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe01821a0016e360a1581c60676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161da24007434d494405"] "81584fa200581d6041484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe01821a0016e360a1581c60676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161da24007434d494405" "144ec68be15e0ade6c97726d32834b207b520cf1441df78379b51ae1c3000b40" 1 [[3,79]]
  , gv "golden_f2_with_script_ref_matches_typescript" 2 0 ["a300581d607f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c01821a002dc6c0a003820358289ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8af"] "815856a300581d607f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c01821a002dc6c0a003820358289ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8af" "49046b6e9924bbde1ffa8c0fb5de7b24615372d2c4d373f986b4f4dde459ee8c" 1 [[3,86]]
  , gv "golden_f2_with_datum_and_script_ref_matches_typescript" 2 0 ["a400581d60bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a01821a003d0900a00243d87980038218805818dce3eaf1f8ff060d141b222930373e454c535a61686f767d"] "81584ca400581d60bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a01821a003d0900a00243d87980038218805818dce3eaf1f8ff060d141b222930373e454c535a61686f767d" "a8e6754743e0c13f0bd40759b629f3df895a8a0ab6730574dd73d926b2f06d54" 1 [[3,76]]
  , gv "golden_f2_datum_canonicity_boundaries_matches_typescript" 2 0 ["a300581d60585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e1501821a000f4240a00243d87980","a300581d60777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d3401821a000f4241a002491bffffffffffffffff","a300581d60969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c5301821a000f4242a0024bc249010000000000000000","a300581d60b5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b7201821a000f4243a0024bc349010000000000000000","a300581d60d4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a9101821a000f4244a00246d905789f00ff","a300581d60f3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b001821a000f4245a00248d8668218809f00ff"] "86582ea300581d60585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e1501821a000f4240a00243d879805834a300581d60777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d3401821a000f4241a002491bffffffffffffffff5836a300581d60969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c5301821a000f4242a0024bc2490100000000000000005836a300581d60b5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b7201821a000f4243a0024bc3490100000000000000005831a300581d60d4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a9101821a000f4244a00246d905789f00ff5833a300581d60f3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b001821a000f4245a00248d8668218809f00ff" "792fd427527b5cdc8001dffdbb2c40ba07697d0788da0ac3c62ce06b6c400a97" 1 [[3,46],[51,52],[105,54],[161,54],[217,49],[268,51]]
  , gv "golden_f3_empty_matches_typescript" 3 30 [] "80" "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0" 1 []
  , gv "golden_f3_single_matches_typescript" 3 30 ["222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8df"] "81581c222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8df" "30af757ef3138b5cf4d29d813e1178a2e7e30761774236218150688ca441d881" 1 [[3,28]]
  , gv "golden_f3_two_byte_header_matches_typescript" 3 30 ["41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe","60676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d","7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c","9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b","bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a","dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299","fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8","1a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7","3940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6","585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e15","777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d34","969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c53","b5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b72","d4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91","f3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0","121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cf","31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7ee","50575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d","6f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c","8e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b","adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a","ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b8289","ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8","0a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7"] "9818581c41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe581c60676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d581c7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c581c9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b581cbdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a581cdce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299581cfb020910171e252c333a41484f565d646b727980878e959ca3aab1b8581c1a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7581c3940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6581c585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e15581c777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d34581c969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c53581cb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b72581cd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91581cf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0581c121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cf581c31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7ee581c50575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d581c6f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c581c8e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b581cadb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a581cccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b8289581cebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8581c0a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7" "42864a8f9b132e54d3f20539b0e666d68376a16d23d09e43fac718b805f0f1dd" 2 [[4,28],[34,28],[64,28],[94,28],[124,28],[154,28],[184,28],[214,28],[244,28],[274,28],[304,28],[334,28],[364,28],[394,28],[424,28],[454,28],[484,28],[514,28],[544,28],[574,28],[604,28],[634,28],[664,28],[694,28]]
  , gv "golden_f4_empty_matches_typescript" 4 30 [] "80" "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0" 1 []
  , gv "golden_f4_single_matches_typescript" 4 30 ["222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8df"] "81581c222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8df" "30af757ef3138b5cf4d29d813e1178a2e7e30761774236218150688ca441d881" 1 [[3,28]]
  , gv "golden_f4_pair_matches_typescript" 4 30 ["a5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b62","c4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81"] "82581ca5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b62581cc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81" "34f1e5ac877c6ff0989c1529830c89264c67ab8d386fdb5bffaaac34f4d3937c" 1 [[3,28],[33,28]]
  , gv "golden_f5_empty_matches_typescript" 5 0 [] "80" "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0" 1 []
  , gv "golden_f5_single_policy_single_asset_matches_typescript" 5 0 ["82581c222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfa1434d494405"] "81582582581c222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfa1434d494405" "09076a453010ecb84249a9a6805b02084e5fb9b46245de7027dc67cbbef59fc5" 1 [[3,37]]
  , gv "golden_f5_ordered_assets_matches_typescript" 5 0 ["82581c41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fea5400141410241420342414104582060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323906"] "81584f82581c41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fea5400141410241420342414104582060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323906" "f45bf4838470d9a83678cf8a99b3cf6563b67a739e39c31e968510153bbc5d25" 1 [[3,79]]
  , gv "golden_f5_burn_negative_quantity_matches_typescript" 5 0 ["82581c7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353ca1444275726e3b001ffffffffffffe"] "81582e82581c7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353ca1444275726e3b001ffffffffffffe" "f6278ec1a970d87f4adfd93a071ea1250cdbd79147eebdc5333aa6c04df0d7ea" 1 [[3,46]]
  , gv "golden_f5_multi_policy_matches_typescript" 5 0 ["82581c9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545ba1414101","82581cbdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737aa1414202"] "82582382581c9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545ba1414101582382581cbdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737aa1414202" "e0204c286985aace7fe6c6db00b55ab9d10d64333b71498e2141aaaf818a55b7" 1 [[3,35],[40,35]]
  , gv "golden_f6_empty_matches_typescript" 6 0 [] "80" "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0" 1 []
  , gv "golden_f6_native_cardano_matches_typescript" 6 0 ["820058208200581c222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8df"] "815824820058208200581c222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8df" "f5a475b5b25fb5256a4c5efba0b61afef70ab3310f6a6fc9ca5fedaaffda4a56" 1 [[3,36]]
  , gv "golden_f6_plutus_v3_matches_typescript" 6 0 ["8203582841484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b52"] "81582c8203582841484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b52" "e321369ff46e1623f0ca1f8787f0ff4851fa5518ee1357e7aace04f275cc69e8" 1 [[3,44]]
  , gv "golden_f6_midgard_v1_matches_typescript" 6 0 ["821880584060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b1219"] "815845821880584060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b1219" "13c9cc6eec2a948749ba77e4a6f04fa42fbdeff6395401c0d56fe8796d2a15be" 1 [[3,69]]
  , gv "golden_f6_all_languages_matches_typescript" 6 0 ["820058208200581c7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c","820358189ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f","82188059012cbdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3ea"] "835824820058208200581c7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c581c820358189ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f59013282188059012cbdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3ea" "458983b47b540144eaf9106b969a76b82cc203fc154402485621f0301e7f5620" 1 [[3,36],[41,28],[72,306]]
  , gv "golden_f7_empty_matches_typescript" 7 103 [] "80" "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0" 1 []
  , gv "golden_f7_single_matches_typescript" 7 103 ["825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb584041484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa"] "815865825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb584041484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa" "84889e21cc2270a082135ab551fd0336780b157b418dd8fd9e898f77a32f9f40" 1 [[3,101]]
  , gv "golden_f7_triple_matches_typescript" 7 103 ["82582060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323958407f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a3138","8258209ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970775840bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f76","825820dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb55840fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4"] "83586582582060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323958407f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a313858658258209ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970775840bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f765865825820dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb55840fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4" "8c4e7220f1f418e28e3c02acffd64d3c6bbdde0aaaf796c76c0d59447710937b" 1 [[3,101],[106,101],[209,101]]
  , gv "golden_f8_empty_matches_typescript" 8 0 [] "80" "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0" 1 []
  , gv "golden_f8_all_purposes_matches_typescript" 8 0 ["84000043d87980821903e81907d0","84010143d87980821903e91907d1","84020243d87980821903ea1907d2","84030343d87980821903eb1907d3","84040443d87980821903ec1907d4","84050543d87980821903ed1907d5","84060643d87980821903ee1907d6"] "874e84000043d87980821903e81907d04e84010143d87980821903e91907d14e84020243d87980821903ea1907d24e84030343d87980821903eb1907d34e84040443d87980821903ec1907d44e84050543d87980821903ed1907d54e84060643d87980821903ee1907d6" "b13455a9b2e012b2513812553c293379c0875864bbc6cc7c8427fec178e570da" 1 [[2,14],[17,14],[32,14],[47,14],[62,14],[77,14],[92,14]]
  , gv "golden_f8_scalar_boundaries_matches_typescript" 8 0 ["84000043d87980820017","8401174443d8798082181818ff","84031818491bffffffffffffffff8219010019ffff","840619ffff48d8668218809f00ff821a000100001b0000000100000000"] "844a84000043d879808200174d8401174443d8798082181818ff5584031818491bffffffffffffffff8219010019ffff581d840619ffff48d8668218809f00ff821a000100001b0000000100000000" "c6d38234cac014c72b820f74f81991a00901632d84d7b57ab4c0a36847a11344" 1 [[2,10],[13,13],[27,21],[50,29]]
  ]

goldenProducerRoundTripTests :: [TestTree]
goldenProducerRoundTripTests =
  [ testCase "golden_f0_producer_round_trip_matches_typescript" $
      holds $
        pall'
          [ pencodeInputPreimage # (pdecodeMidgardTxInputsPreimageCbor # pconstant preimage)
              #== pconstant preimage
          | preimage <- goldenFieldPreimages 0
          ]
  , testCase "golden_f1_producer_round_trip_matches_typescript" $
      holds $
        pall'
          [ pencodeInputPreimage # (pdecodeMidgardTxInputsPreimageCbor # pconstant preimage)
              #== pconstant preimage
          | preimage <- goldenFieldPreimages 1
          ]
  , testCase "golden_f2_producer_round_trip_matches_typescript" $
      holds $
        pall'
          [ pencodeOutputPreimage # (pdecodeMidgardTxOutputsPreimageCbor # pconstant preimage)
              #== pconstant preimage
          | preimage <- goldenFieldPreimages 2
          ]
  , testCase "golden_f3_producer_round_trip_matches_typescript" $
      holds $
        pall'
          [ pencodeHash28ListPreimage # (pdecodeMidgardTxHash28ListPreimageCbor # pconstant preimage)
              #== pconstant preimage
          | preimage <- goldenFieldPreimages 3
          ]
  , testCase "golden_f4_producer_round_trip_matches_typescript" $
      holds $
        pall'
          [ pencodeHash28ListPreimage # (pdecodeMidgardTxHash28ListPreimageCbor # pconstant preimage)
              #== pconstant preimage
          | preimage <- goldenFieldPreimages 4
          ]
  , testCase "golden_f5_producer_round_trip_matches_typescript" $
      holds $
        pall'
          [ pencodeMintPreimage # (pdecodeMidgardTxMintPreimageCbor # pconstant preimage)
              #== pconstant preimage
          | preimage <- goldenFieldPreimages 5
          ]
  , testCase "golden_f6_producer_round_trip_matches_typescript" $
      holds $
        pall'
          [ pencodeScriptWitnessPreimage # (pdecodeMidgardTxScriptWitnessesPreimageCbor # pconstant preimage)
              #== pconstant preimage
          | preimage <- goldenFieldPreimages 6
          ]
  , testCase "golden_f7_producer_round_trip_matches_typescript" $
      holds $
        pall'
          [ pencodeAddressWitnessPreimage # (pdecodeMidgardTxAddressWitnessesPreimageCbor # pconstant preimage)
              #== pconstant preimage
          | preimage <- goldenFieldPreimages 7
          ]
  , testCase "golden_f8_producer_round_trip_matches_typescript" $
      holds $
        pall'
          [ pencodeRedeemerWitnessPreimage # (pdecodeMidgardTxRedeemerWitnessesPreimageCbor # pconstant preimage)
              #== pconstant preimage
          | preimage <- goldenFieldPreimages 8
          ]
  ]

goldenFieldPreimages :: Integer -> [BS.ByteString]
goldenFieldPreimages fieldIndex =
  [ goldenItemPreimage vector
  | vector <- goldenFieldItemVectors
  , goldenItemFieldIndex vector == fieldIndex
  ]

goldenItemEncoderTests :: [TestTree]
goldenItemEncoderTests =
  [ testCase "golden_f0_item_encoder_matches_typescript" $
      holds $
        pall'
          [ pencodeMidgardTxInput # goldenInputTerm input #== pconstant (goldenItemsHex expected)
          | (input, expected) <- goldenF0InputCases
          ]
  , testCase "golden_f1_item_encoder_matches_typescript" $
      holds $
        pall'
          [ pencodeMidgardTxInput # goldenInputTerm input #== pconstant (goldenItemsHex expected)
          | (input, expected) <- goldenF1InputCases
          ]
  , testCase "golden_f6_item_encoder_matches_typescript" $
      holds $
        pall'
          [ pencodeMidgardVersionedScript # goldenScriptTerm script #== pconstant (goldenItemsHex expected)
          | (script, expected) <- goldenScriptCases
          ]
  , testCase "golden_f7_item_encoder_matches_typescript" $
      holds $
        pall'
          [ pencodeMidgardAddressWitness # goldenAddressWitnessTerm witness #== pconstant (goldenItemsHex expected)
          | (witness, expected) <- goldenAddressWitnessCases
          ]
  , testCase "golden_f8_item_encoder_matches_typescript" $
      holds $
        pall'
          [ pencodeMidgardRedeemerWitness # goldenRedeemerTerm redeemer #== pconstant (goldenItemsHex expected)
          | (redeemer, expected) <- goldenRedeemerCases
          ]
  ]

goldenF0InputCases, goldenF1InputCases :: [(Input, String)]
goldenF0InputCases =
  [ (Input (goldenItemsHex "222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb") 0, "825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb190000")
  , (Input (goldenItemsHex "41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a") 0, "82582041484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a190000")
  , (Input (goldenItemsHex "60676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b3239") 23, "82582060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b3239190017")
  , (Input (goldenItemsHex "7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a5158") 24, "8258207f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a5158190018")
  , (Input (goldenItemsHex "9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b62697077") 255, "8258209ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970771900ff")
  , (Input (goldenItemsHex "bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f96") 256, "825820bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f96190100")
  , (Input (goldenItemsHex "dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5") 65_535, "825820dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb519ffff")
  ]
goldenF1InputCases =
  [ (Input (goldenItemsHex "222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb") 0, "825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb190000")
  , (Input (goldenItemsHex "1a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3") 1, "8258201a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3190001")
  , (Input (goldenItemsHex "3940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b12") 65_535, "8258203940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b1219ffff")
  ]

goldenScriptCases :: [(Scr, String)]
goldenScriptCases =
  [ (Scr NativeCardano (goldenItemsHex "8200581c222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8df"), "820058208200581c222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8df")
  , (Scr PlutusV3 (goldenItemsHex "41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b52"), "8203582841484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b52")
  , (Scr MidgardV1 (goldenItemsHex "60676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b1219"), "821880584060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b1219")
  , (Scr NativeCardano (goldenItemsHex "8200581c7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c"), "820058208200581c7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c")
  , (Scr PlutusV3 (goldenItemsHex "9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f"), "820358189ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f")
  , (Scr MidgardV1 (goldenItemsHex "bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3ea"), "82188059012cbdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3ea")
  ]

goldenAddressWitnessCases :: [(Aw, String)]
goldenAddressWitnessCases =
  [ (Aw (goldenItemsHex "222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb") (goldenItemsHex "41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa"), "825820222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb584041484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa")
  , (Aw (goldenItemsHex "60676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b3239") (goldenItemsHex "7f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a3138"), "82582060676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323958407f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a3138")
  , (Aw (goldenItemsHex "9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b62697077") (goldenItemsHex "bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f76"), "8258209ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970775840bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f76")
  , (Aw (goldenItemsHex "dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5") (goldenItemsHex "fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4"), "825820dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb55840fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4")
  ]

goldenRedeemerCases :: [(Rw, String)]
goldenRedeemerCases =
  [ (Rw 0 0 (goldenItemsHex "d87980") 1000 2000, "84000043d87980821903e81907d0")
  , (Rw 1 1 (goldenItemsHex "d87980") 1001 2001, "84010143d87980821903e91907d1")
  , (Rw 2 2 (goldenItemsHex "d87980") 1002 2002, "84020243d87980821903ea1907d2")
  , (Rw 3 3 (goldenItemsHex "d87980") 1003 2003, "84030343d87980821903eb1907d3")
  , (Rw 4 4 (goldenItemsHex "d87980") 1004 2004, "84040443d87980821903ec1907d4")
  , (Rw 5 5 (goldenItemsHex "d87980") 1005 2005, "84050543d87980821903ed1907d5")
  , (Rw 6 6 (goldenItemsHex "d87980") 1006 2006, "84060643d87980821903ee1907d6")
  , (Rw 0 0 (goldenItemsHex "d87980") 0 23, "84000043d87980820017")
  , (Rw 1 23 (goldenItemsHex "43d87980") 24 255, "8401174443d8798082181818ff")
  , (Rw 3 24 (goldenItemsHex "1bffffffffffffffff") 256 65_535, "84031818491bffffffffffffffff8219010019ffff")
  , (Rw 6 65_535 (goldenItemsHex "d8668218809f00ff") 65_536 4_294_967_296, "840619ffff48d8668218809f00ff821a000100001b0000000100000000")
  ]

goldenInputTerm :: forall s. Input -> Term s PMidgardTxInput
goldenInputTerm (Input txId index) =
  pcon $
    PMidgardTxInput
      { ptxInput'txId = pdata (pconstant txId)
      , ptxInput'outputIndex = pdata (pconstant index)
      }

goldenScriptTerm :: forall s. Scr -> Term s PMidgardVersionedScript
goldenScriptTerm (Scr language bytes) =
  pcon $
    PMidgardVersionedScript
      { pversionedScript'language = pdata (goldenLanguageTerm language)
      , pversionedScript'scriptBytes = pdata (pconstant bytes)
      }

goldenAddressWitnessTerm :: forall s. Aw -> Term s PMidgardAddressWitness
goldenAddressWitnessTerm (Aw key signature) =
  pcon $
    PMidgardAddressWitness
      { paddressWitness'verificationKey = pdata (pconstant key)
      , paddressWitness'signature = pdata (pconstant signature)
      }

goldenRedeemerTerm :: forall s. Rw -> Term s PMidgardRedeemerWitness
goldenRedeemerTerm (Rw purpose index cbor memory steps) =
  pcon $
    PMidgardRedeemerWitness
      { predeemerWitness'purpose = pdata (goldenPurposeTerm purpose)
      , predeemerWitness'index = pdata (pconstant index)
      , predeemerWitness'redeemerCbor = pdata (pconstant cbor)
      , predeemerWitness'executionUnits =
          pdata . pcon $
            PMidgardExecutionUnits
              { pexecutionUnits'memory = pdata (pconstant memory)
              , pexecutionUnits'steps = pdata (pconstant steps)
              }
      }

goldenLanguageTerm :: forall s. Lang -> Term s PMidgardScriptLanguage
goldenLanguageTerm NativeCardano = pcon PNativeCardanoScript
goldenLanguageTerm PlutusV3 = pcon PPlutusV3Script
goldenLanguageTerm MidgardV1 = pcon PMidgardV1Script

goldenPurposeTerm :: forall s. Integer -> Term s PMidgardRedeemerPurpose
goldenPurposeTerm 0 = pcon PSpendRedeemer
goldenPurposeTerm 1 = pcon PMintRedeemer
goldenPurposeTerm 2 = pcon PCertRedeemer
goldenPurposeTerm 3 = pcon PRewardRedeemer
goldenPurposeTerm 4 = pcon PVoteRedeemer
goldenPurposeTerm 5 = pcon PProposeRedeemer
goldenPurposeTerm 6 = pcon PReceiveRedeemer
goldenPurposeTerm purpose = error ("invalid golden redeemer purpose: " <> show purpose)

goldenNegativeTests :: [TestTree]
goldenNegativeTests =
  [ testCase "golden_f3_f4_item_width_rejects_27_bytes" $
      pfails $
        plengthBS
          # (pencodeHash28ListPreimage # itemsT [goldenItemsHex "222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8"])
  , testCase "golden_f3_f4_item_width_rejects_29_bytes" $
      pfails $
        plengthBS
          # (pencodeHash28ListPreimage # itemsT [goldenItemsHex "222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6"])
  , testCase "golden_f0_f1_item_rejects_a_short_tx_id" $
      pfails $
        plengthBS
          # ( pencodeMidgardTxInput
                # goldenInputTerm
                  (Input (goldenItemsHex "222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4") 0)
            )
  , testCase "golden_f7_item_rejects_a_short_verification_key" $
      pfails $
        plengthBS
          # ( pencodeMidgardAddressWitness
                # goldenAddressWitnessTerm
                  ( Aw
                      (goldenItemsHex "222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4")
                      (goldenItemsHex "41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa")
                  )
            )
  , testCase "golden_f7_item_rejects_a_short_signature" $
      pfails $
        plengthBS
          # ( pencodeMidgardAddressWitness
                # goldenAddressWitnessTerm
                  ( Aw
                      (goldenItemsHex "222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb")
                      (goldenItemsHex "41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3")
                  )
            )
  , testCase "golden_f5_asset_names_reject_descending_order" $
      pfails $
        plengthBS
          # ( pencodeMintPolicyItem
                # pconstant (B (goldenItemsHex "9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b"))
                # pconstant (Map [(B "AA", I 1), (B "B", I 2)])
            )
  , testCase "golden_f5_asset_names_reject_a_repeat" $
      pfails $
        plengthBS
          # ( pencodeMintPolicyItem
                # pconstant (B (goldenItemsHex "9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b"))
                # pconstant (Map [(B "A", I 1), (B "A", I 2)])
            )
  , testCase "golden_f5_policy_ids_reject_descending_order" $
      pfails $
        plengthBS
          # ( pencodeMintPreimage
                # pconstant
                  ( Map
                      [ (B (goldenItemsHex "bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a"), Map [(B "A", I 1)])
                      , (B (goldenItemsHex "9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b"), Map [(B "A", I 2)])
                      ]
                  )
            )
  , testCase "golden_f5_policy_ids_reject_a_repeat" $
      pfails $
        plengthBS
          # ( pencodeMintPreimage
                # pconstant
                  ( Map
                      [ (B (goldenItemsHex "9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b"), Map [(B "A", I 1)])
                      , (B (goldenItemsHex "9ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b"), Map [(B "A", I 2)])
                      ]
                  )
            )
  ]

goldenCrossCuttingTests :: [TestTree]
goldenCrossCuttingTests =
  [ testCase "golden_fixed_output_index_matches_typescript" $
      holds $
        pall'
          [ pencodeFixedOutputIndex # pconstant index #== pconstant (goldenItemsHex encoded)
          | (index, encoded) <-
              [ (0, "190000")
              , (23, "190017")
              , (24, "190018")
              , (255, "1900ff")
              , (256, "190100")
              , (65_535, "19ffff")
              ]
          ]
  , testCase "golden_script_language_tags_match_typescript" $
      holds $
        pall'
          [ psliceBS # 1 # pconstant width # (pencodeMidgardVersionedScript # goldenScriptTerm script)
              #== pconstant (goldenItemsHex expected)
          | (script, width, expected) <- goldenLanguageTagCases
          ]
  , testCase "golden_redeemer_purpose_tags_match_typescript" $
      holds $
        pall'
          [ pencodeMidgardRedeemerWitness
              # goldenRedeemerTerm (Rw purpose 1 (goldenItemsHex "d87980") 2 3)
              #== pconstant (goldenItemsHex expected)
          | (purpose, expected) <- zip [0 .. 6] goldenPurposeTagEncodings
          ]
  , testCase "canonical_redeemer_bridge_golden_vector_is_stable" $
      holds canonicalRedeemerBridgeMatches
  , testCase "golden_field_preimage_lengths_match_typescript" $
      holds $
        pencodeNativeTxFieldPreimageLengthsV1 # goldenFieldLengthsTerm
          #== pconstant (goldenItemsHex "890b161821182c18371842184d18581863")
  , testCase "golden_straddle_read_matches_typescript" $
      holds goldenStraddleMatches
  ]

goldenLanguageTagCases :: [(Scr, Integer, String)]
goldenLanguageTagCases =
  [ (Scr NativeCardano (goldenItemsHex "8200581ce9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6"), 1, "00")
  , (Scr PlutusV3 (goldenItemsHex "080f161d242b3239"), 1, "03")
  , (Scr MidgardV1 (goldenItemsHex "272e353c434a5158"), 2, "1880")
  ]

goldenPurposeTagEncodings :: [String]
goldenPurposeTagEncodings =
  [ "84000143d87980820203"
  , "84010143d87980820203"
  , "84020143d87980820203"
  , "84030143d87980820203"
  , "84040143d87980820203"
  , "84050143d87980820203"
  , "84060143d87980820203"
  ]

canonicalRedeemerBridgeMatches :: forall s. Term s PBool
canonicalRedeemerBridgeMatches =
  plet
    (pdecodeMidgardTxRedeemerWitnessesPreimageCbor # pconstant canonicalRedeemerBridgeCbor)
    $ \decoded ->
      decoded #== canonicalRedeemerBridgeWitnesses
        #&& (pencodeRedeemerWitnessPreimage # decoded #== pconstant canonicalRedeemerBridgeCbor)

canonicalRedeemerBridgeWitnesses :: forall s. Term s (PBuiltinList (PAsData PMidgardRedeemerWitness))
canonicalRedeemerBridgeWitnesses =
  pcons
    # pdata (goldenRedeemerTerm (Rw 0 0 (goldenItemsHex "d87980") 5 7))
    #$ pcons
      # pdata (goldenRedeemerTerm (Rw 3 1 (goldenItemsHex "182a") 11 13))
      # pnil

canonicalRedeemerBridgeCbor :: BS.ByteString
canonicalRedeemerBridgeCbor = goldenItemsHex "824a84000043d879808205074984030142182a820b0d"

goldenFieldLengthsTerm :: forall s. Term s PNativeTxFieldPreimageLengthsV1
goldenFieldLengthsTerm =
  pcon $
    PNativeTxFieldPreimageLengthsV1
      { plengths'spendInputs = 11
      , plengths'referenceInputs = 22
      , plengths'outputs = 33
      , plengths'requiredObservers = 44
      , plengths'requiredSigners = 55
      , plengths'mint = 66
      , plengths'addressWitnesses = 88
      , plengths'scriptWitnesses = 77
      , plengths'redeemers = 99
      }

goldenStraddleBlock, goldenStraddlePreimage, goldenStraddleCommitment :: BS.ByteString
goldenStraddleBlock = goldenItemsHex "58268258201f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f819000058268258203e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb0209101719000158268258205d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f3619000258268258207c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e5519000358268258209ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d741900045826825820bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c931900055826825820d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb21900065826825820f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad11900075826825820171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f01900085826825820363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f190009"
goldenStraddlePreimage = goldenItemsHex "990190" <> BS.concat (replicate 40 goldenStraddleBlock)
goldenStraddleCommitment = goldenItemsHex "c33cac158cd252aeb86e3fafb6776fd03e7afacffa7923c05878afca870b4ef1"

goldenStraddleChunks, goldenStraddleDigests :: [BS.ByteString]
goldenStraddleChunks =
  [ BS.take 15_900 goldenStraddlePreimage
  , BS.drop 15_900 goldenStraddlePreimage
  ]
goldenStraddleDigests =
  map
    goldenItemsHex
    [ "3d472a8b6608fd6572a3de26a9f95a37c86c9ba739c39b5b8314f860bc908806"
    , "7243f0ea84ad415d83212fc24569d54be7e81f50198a257f93de50cfcb3d7cb2"
    ]

goldenStraddleMatches :: forall s. Term s PBool
goldenStraddleMatches =
  plet
    ( pcon $
        PChunkedView
          (itemsT goldenStraddleChunks)
          (itemsT goldenStraddleDigests)
          400
          40
    )
    $ \view ->
      pall'
        [ plengthBS # pconstant goldenStraddlePreimage #== 16_003
        , pchunkBytesK #== 15_900
        , pfieldCommitment # pconstant goldenStraddlePreimage #== pconstant goldenStraddleCommitment
        , plengthBS # pconstant (head goldenStraddleChunks) #== 15_900
        , plengthBS # pconstant (goldenStraddleChunks !! 1) #== 103
        , pblake2b_256 # pconstant (head goldenStraddleChunks) #== pconstant (head goldenStraddleDigests)
        , pblake2b_256 # pconstant (goldenStraddleChunks !! 1) #== pconstant (goldenStraddleDigests !! 1)
        , ppairIs (pfieldItemExtent # view # 396) 15_845 38
        , pfieldItemAt # view # 396 #== pconstant (goldenItemsHex "825820d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2190006")
        , ppairIs (pfieldItemExtent # view # 397) 15_885 38
        , pfieldItemAt # view # 397 #== pconstant (goldenItemsHex "825820f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1190007")
        , ppairIs (pfieldItemExtent # view # 398) 15_925 38
        , pfieldItemAt # view # 398 #== pconstant (goldenItemsHex "825820171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0190008")
        ]

--------------------------------------------------------------------------------
-- The §5.1 envelope
--------------------------------------------------------------------------------

envelopeTests :: TestTree
envelopeTests =
  testGroup
    "the §5.1 envelope"
    [ testCase "the array header is minimal at every width" $
        holds $
          pall'
            [ (pencodeFieldArrayHeader # pconstant n) #== pconstant expected
            | (n, expected) <-
                [ (0, "\x80")
                , (1, "\x81")
                , (23, "\x97")
                , (24, "\x98\x18")
                , (255, "\x98\xff")
                , (256, "\x99\x01\x00")
                , (65535, "\x99\xff\xff")
                ]
            ]
    , testCase "the array header rejects a count outside the grammar" $
        mapM_
          (\n -> pfails $ pencodeFieldArrayHeader # pconstant n)
          [-1, 65536]
    , testCase "the header round-trips at every width" $
        holds $
          pall'
            [ pheaderIs (pencodeFieldArrayHeader # pconstant n) n
            | n <- [0, 1, 23, 24, 255, 256, 65535]
            ]
    , -- Non-minimal spellings must reject, or a field gets two preimages.
      testCase "the header decoder rejects a non-minimal width" $
        mapM_
          (\bytes -> pfails $ pheaderCountOf (pconstant bytes))
          [ "\x98\x00" -- 0 in the one-byte form
          , "\x98\x17" -- 23 in the one-byte form
          , "\x99\x00\x00" -- 0 in the two-byte form
          , "\x99\x00\xff" -- 255 in the two-byte form
          ]
    , -- Well-formed CBOR, outside §5.1.
      testCase "the header decoder rejects the four-byte 9a form" $
        pfails $ pheaderCountOf (pconstant "\x9a\x00\x01\x00\x00")
    , testCase "the header decoder rejects a non-array head" $
        mapM_
          (\bytes -> pfails $ pheaderCountOf (pconstant bytes))
          ["\x9f", "\xa0", "\x40", "\x00"]
    , testCase "the header decoder reads at an offset" $
        holds $
          psndOf (pdecodeFieldArrayHeaderAt # pconstant "\xff\xff\x98\x2a" # 2) #== 42
    , testCase "an empty field is exactly 80" $
        holds $ (pencodeFieldPreimage # itemsT []) #== pconstant "\x80"
    , testCase "the preimage matches an independent encoding" $
        holds $
          pall'
            [ (pencodeFieldPreimage # itemsT items) #== pconstant (encodePreimage items)
            | items <- allItemLists
            ]
    , -- §4: plain blake2b_256 over the bytes, no domain tag and no field index.
      testCase "the field commitment is a plain hash of the preimage bytes" $
        holds $
          pall'
            [ (pfieldCommitment # pconstant (encodePreimage items))
              #== pconstant (blake2b256 (encodePreimage items))
            | items <- allItemLists
            ]
    , testCase "the item-wise commitment agrees with the byte-wise one" $
        holds $
          pall'
            [ (pfieldCommitmentFromItems # itemsT items)
              #== (pfieldCommitment #$ pencodeFieldPreimage # itemsT items)
            | items <- allItemLists
            ]
    , -- The pin the Aiken test file also proves against the producer.
      testCase "the empty-field commitment literal matches the producer" $
        holds $ pemptyFieldCommitment #== (pfieldCommitmentFromItems # itemsT [])
    ]

--------------------------------------------------------------------------------
-- Producers
--------------------------------------------------------------------------------

producerTests :: TestTree
producerTests =
  testGroup
    "producers"
    [ testCase "the input preimage matches an independent encoding" $
        holds $
          (pencodeInputPreimage # inputsT defaultInputs)
            #== pconstant (encodePreimage (map encodeInput defaultInputs))
    , testCase "the output preimage matches an independent encoding" $
        holds $
          (pencodeOutputPreimage # outputsT defaultOutputs)
            #== pconstant (encodePreimage (map encodeOutput defaultOutputs))
    , testCase "the hash28 preimage matches an independent encoding" $
        holds $
          (pencodeHash28ListPreimage # itemsT defaultHashes)
            #== pconstant (encodePreimage defaultHashes)
    , -- §5.3 fixes the stride at 30, which only holds if every item is 28 bytes.
      testCase "the hash28 preimage rejects an item of any other width" $
        mapM_
          (\n -> pfails $ pencodeHash28ListPreimage # itemsT [BS.replicate n 0x01])
          [0, 27, 29, 32]
    , testCase "the address witness preimage matches an independent encoding" $
        holds $
          (pencodeAddressWitnessPreimage # addressWitnessesT defaultAws)
            #== pconstant (encodePreimage (map encodeAddressWitness defaultAws))
    , -- Fields 6 and 8 were raw concatenation under the retired counted scheme.
      testCase "the script witness preimage wraps each item in the envelope" $
        holds $
          (pencodeScriptWitnessPreimage # scriptsT defaultScripts)
            #== pconstant (encodePreimage (map encodeScript defaultScripts))
    , testCase "the redeemer witness preimage wraps each item in the envelope" $
        holds $
          (pencodeRedeemerWitnessPreimage # redeemersT defaultRedeemers)
            #== pconstant (encodePreimage (map encodeRedeemer defaultRedeemers))
    , testCase "every producer emits 80 for an empty field" $
        holds $
          pall'
            [ (pencodeInputPreimage # inputsT []) #== pconstant "\x80"
            , (pencodeOutputPreimage # outputsT []) #== pconstant "\x80"
            , (pencodeHash28ListPreimage # itemsT []) #== pconstant "\x80"
            , (pencodeAddressWitnessPreimage # addressWitnessesT []) #== pconstant "\x80"
            , (pencodeScriptWitnessPreimage # scriptsT []) #== pconstant "\x80"
            , (pencodeRedeemerWitnessPreimage # redeemersT []) #== pconstant "\x80"
            , (pencodeMintPreimage # pconstant emptyMint) #== pconstant "\x80"
            ]
    ]

--------------------------------------------------------------------------------
-- Decoders
--------------------------------------------------------------------------------

decoderTests :: TestTree
decoderTests =
  testGroup
    "decoders"
    [ testCase "the byte list round-trips" $
        holds $
          pall'
            [ (pdecodeMidgardTxByteListPreimageCbor # pconstant (encodePreimage items))
              #== itemsT items
            | items <- allItemLists
            ]
    , testCase "the hash28 list round-trips" $
        holds $
          (pdecodeMidgardTxHash28ListPreimageCbor # pconstant (encodePreimage defaultHashes))
            #== itemsT defaultHashes
    , -- The decoder must refuse what the encoder would.
      testCase "the hash28 decoder rejects an item of the wrong width" $
        pfails $
          pdecodeMidgardTxHash28ListPreimageCbor
            # pconstant (encodePreimage [BS.replicate 27 0x01])
    , testCase "the inputs round-trip" $
        holds $
          (pencodeInputPreimage #$ pdecodeMidgardTxInputsPreimageCbor # pconstant inputPreimage)
            #== pconstant inputPreimage
    , testCase "the outputs round-trip" $
        holds $
          (pencodeOutputPreimage #$ pdecodeMidgardTxOutputsPreimageCbor # pconstant outputPreimage)
            #== pconstant outputPreimage
    , testCase "the address witnesses round-trip" $
        holds $
          ( pencodeAddressWitnessPreimage
              #$ pdecodeMidgardTxAddressWitnessesPreimageCbor # pconstant awPreimage
          )
            #== pconstant awPreimage
    , testGroup
        "C20 field 7 malformed address witness boundaries"
        [ testCase "v1_c20_7_vkey_witness_arity_one_rejects" $
            rejectsAddressWitnessPreimage
              "8158238158201111111111111111111111111111111111111111111111111111111111111111"
        , testCase "v1_c20_7_vkey_witness_arity_three_rejects" $
            rejectsAddressWitnessPreimage
              "815866835820111111111111111111111111111111111111111111111111111111111111111158402222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222240"
        , testCase "v1_c20_7_vkey_length_31_rejects" $
            rejectsAddressWitnessPreimage
              "81586482581f11111111111111111111111111111111111111111111111111111111111111584022222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222"
        , testCase "v1_c20_7_vkey_length_33_rejects" $
            rejectsAddressWitnessPreimage
              "815866825821111111111111111111111111111111111111111111111111111111111111111111584022222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222"
        , testCase "v1_c20_7_signature_length_63_rejects" $
            rejectsAddressWitnessPreimage
              "8158648258201111111111111111111111111111111111111111111111111111111111111111583f222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222"
        , testCase "v1_c20_7_signature_length_65_rejects" $
            rejectsAddressWitnessPreimage
              "815866825820111111111111111111111111111111111111111111111111111111111111111158412222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222"
        ]
    , testCase "the script witnesses round-trip" $
        holds $
          ( pencodeScriptWitnessPreimage
              #$ pdecodeMidgardTxScriptWitnessesPreimageCbor # pconstant scriptPreimage
          )
            #== pconstant scriptPreimage
    , testCase "the redeemer witnesses round-trip" $
        holds $
          ( pencodeRedeemerWitnessPreimage
              #$ pdecodeMidgardTxRedeemerWitnessesPreimageCbor # pconstant redeemerPreimage
          )
            #== pconstant redeemerPreimage
    , -- Nothing may ride along behind the last item.
      testCase "every decoder rejects trailing bytes" $ do
        pfails $ pdecodeMidgardTxByteListPreimageCbor # pconstant (encodePreimage ["\x01"] <> "\x00")
        pfails $ pdecodeMidgardTxInputsPreimageCbor # pconstant (inputPreimage <> "\x00")
        pfails $ pdecodeMidgardTxOutputsPreimageCbor # pconstant (outputPreimage <> "\x00")
        pfails $ pdecodeMidgardTxAddressWitnessesPreimageCbor # pconstant (awPreimage <> "\x00")
        pfails $ pdecodeMidgardTxScriptWitnessesPreimageCbor # pconstant (scriptPreimage <> "\x00")
        pfails $ pdecodeMidgardTxRedeemerWitnessesPreimageCbor # pconstant (redeemerPreimage <> "\x00")
    , testCase "every decoder rejects a count larger than the items present" $ do
        pfails $ pdecodeMidgardTxByteListPreimageCbor # pconstant ("\x82" <> BS.drop 1 (encodePreimage ["\x01"]))
        pfails $ pdecodeMidgardTxInputsPreimageCbor # pconstant ("\x83" <> BS.drop 1 inputPreimage)
    , -- Fields 6 and 8 read through an offset decoder, so each item carries its
      -- own "consumed exactly this item" check.
      testCase "the script witness decoder rejects a padded item" $
        pfails $
          pdecodeMidgardTxScriptWitnessesPreimageCbor
            # pconstant (encodePreimage [encodeScript (Scr PlutusV3 "\x01") <> "\x00"])
    , testCase "the redeemer witness decoder rejects a padded item" $
        pfails $
          pdecodeMidgardTxRedeemerWitnessesPreimageCbor
            # pconstant (encodePreimage [encodeRedeemer defaultRedeemer <> "\x00"])
    ]

rejectsAddressWitnessPreimage :: String -> Assertion
rejectsAddressWitnessPreimage fixture =
  pfails $
    pdecodeMidgardTxAddressWitnessesPreimageCbor
      # pconstant (goldenItemsHex fixture)

--------------------------------------------------------------------------------
-- Field 5 — mint
--------------------------------------------------------------------------------

mintTests :: TestTree
mintTests =
  testGroup
    "mint"
    [ testCase "the mint preimage matches an independent encoding" $
        holds $
          pall'
            [ (pencodeMintPreimage # pconstant (mintData m)) #== pconstant (encodeMint m)
            | m <- allMints
            ]
    , -- The retired raw-map form (a0 when empty) is prohibited.
      testCase "an empty mint encodes as 80 and not as a0" $
        holds $ (pencodeMintPreimage # pconstant emptyMint) #== pconstant "\x80"
    , testCase "every mint round-trips" $
        holds $
          pall'
            [ (pdecodeMidgardTxMintPreimageCbor # pconstant (encodeMint m))
              #== pconstant (mintData m)
            | m <- allMints
            ]
    , testCase "an empty mint decodes back to the empty list and not an empty map" $
        holds $ (pdecodeMidgardTxMintPreimageCbor # pconstant "\x80") #== pconstant emptyMint
    , testCase "every mint passes the in-place verifier" $
        holds $
          pall'
            [ pverifyCanonicalMintPreimageCbor
              # pconstant (encodeMint m)
              # pconstant (blake2b256 (encodeMint m))
            | m <- allMints
            ]
    , testCase "the in-place verifier rejects a wrong commitment" $
        pfails $
          pverifyCanonicalMintPreimageCbor
            # pconstant (encodeMint defaultMint)
            # pconstant (BS.replicate 32 0x00)
    , -- Ordering is enforced on the producer, so an encoder cannot emit bytes
      -- that never decode.
      testCase "the encoder rejects policies out of canonical order" $
        pfails $ pencodeMintPreimage # pconstant (Map [policyEntry pidB, policyEntry pidA])
    , testCase "the encoder rejects a duplicated policy" $
        pfails $ pencodeMintPreimage # pconstant (Map [policyEntry pidA, policyEntry pidA])
    , testCase "the encoder rejects asset names out of canonical order" $
        pfails $
          pencodeMintPreimage
            # pconstant (Map [(B pidA, Map [(B "\x02", I 1), (B "\x01", I 1)])])
    , testCase "the encoder rejects a duplicated asset name" $
        pfails $
          pencodeMintPreimage
            # pconstant (Map [(B pidA, Map [(B "\x01", I 1), (B "\x01", I 1)])])
    , testCase "the encoder rejects a policy with no assets" $
        pfails $ pencodeMintPreimage # pconstant (Map [(B pidA, Map [])])
    , testCase "the encoder rejects a zero quantity" $
        pfails $ pencodeMintPreimage # pconstant (Map [(B pidA, Map [(B "\x01", I 0)])])
    , testCase "the encoder rejects a policy id that is not 28 bytes" $
        pfails $
          pencodeMintPreimage
            # pconstant (Map [(B (BS.replicate 27 0x01), Map [(B "\x01", I 1)])])
    , testCase "the encoder rejects an asset name over 32 bytes" $
        pfails $
          pencodeMintPreimage
            # pconstant (Map [(B pidA, Map [(B (BS.replicate 33 0x01), I 1)])])
    , -- The same rules again on the way in, since mint never re-encodes.
      testCase "the decoder rejects policies out of canonical order" $ do
        pfails $ pdecodeMidgardTxMintPreimageCbor # pconstant descendingPolicies
        pfails $ pverifyCanonicalMintPreimageCbor # pconstant descendingPolicies # pconstant (blake2b256 descendingPolicies)
    , testCase "the decoder rejects asset names out of canonical order" $ do
        pfails $ pdecodeMidgardTxMintPreimageCbor # pconstant descendingAssets
        pfails $ pverifyCanonicalMintPreimageCbor # pconstant descendingAssets # pconstant (blake2b256 descendingAssets)
    , testCase "the decoder rejects a policy with no assets" $
        pfails $ pdecodeMidgardTxMintPreimageCbor # pconstant emptyPolicy
    , testCase "the decoder rejects a zero quantity" $
        pfails $ pdecodeMidgardTxMintPreimageCbor # pconstant zeroQuantity
    , -- The canonical byte reader, which the mint item decoder uses in place of
      -- the general one.
      testCase "the decoder rejects a non-minimal asset name width" $
        pfails $ pdecodeMidgardTxMintPreimageCbor # pconstant nonMinimalName
    , testCase "the decoder rejects a non-minimal map header" $
        pfails $ pdecodeMidgardTxMintPreimageCbor # pconstant nonMinimalMapHeader
    , testCase "the decoder rejects trailing bytes inside a policy item" $
        pfails $ pdecodeMidgardTxMintPreimageCbor # pconstant paddedPolicyItem
    , testCase "a negative quantity is admitted, since a burn is a mint" $
        holds $
          (pdecodeMidgardTxMintPreimageCbor # pconstant (encodeMint burnMint))
            #== pconstant (mintData burnMint)
    , -- Canonical CBOR map order: shorter encoded key first, then lexicographic.
      testCase "key ordering is by encoded length before content" $
        holds $
          pall'
            [ pcanonicalBytesKeyPrecedes # pconstant "\x01" # pconstant "\x02"
            , pnot #$ pcanonicalBytesKeyPrecedes # pconstant "\x02" # pconstant "\x01"
            , pnot #$ pcanonicalBytesKeyPrecedes # pconstant "\x01" # pconstant "\x01"
            , -- A short key precedes a long one whatever the bytes say.
              pcanonicalBytesKeyPrecedes # pconstant "\xff" # pconstant "\x00\x00"
            , pnot #$ pcanonicalBytesKeyPrecedes # pconstant "\x00\x00" # pconstant "\xff"
            , -- The header width jumps at 24 bytes.
              pcanonicalBytesKeyPrecedes
                # pconstant (BS.replicate 23 0xff)
                # pconstant (BS.replicate 24 0x00)
            ]
    ]

--------------------------------------------------------------------------------
-- structural-na-q24-ada-minted.test.ak
--------------------------------------------------------------------------------

q24AdaMintedTests :: TestTree
q24AdaMintedTests =
  testGroup
    "Q24 ADA Minted Aiken Parity"
    [ testCase "q24_ada_mint_entry_serialises_to_the_expected_violating_cbor" $
        holds q24MintEntriesSerialiseExactly
    , testCase "q24_ada_policy_mint_entry_rejected_by_preimage_encoder" $
        pfails $ pencodeMintPreimage # pconstant q24AdaMint
    , testCase "q24_ada_policy_mint_entry_rejected_by_compact_item_encoder" $
        pfails $ plength # (pmintPolicyItems # pconstant q24AdaMint)
    , testCase "q24_ada_policy_mint_preimage_bytes_rejected_by_canonical_verifier" $
        pfails $
          pverifyCanonicalMintPreimageCbor
            # pconstant q24AdaMintFieldPreimageCbor
            # (pfieldCommitment # pconstant q24AdaMintFieldPreimageCbor)
    , testCase "q24_retired_raw_map_mint_preimage_is_refused" $
        pfails $
          pverifyCanonicalMintPreimageCbor
            # pconstant q24RealMintDataCbor
            # (pfieldCommitment # pconstant q24RealMintDataCbor)
    , testCase "q24_twenty_seven_byte_policy_id_rejected_by_preimage_encoder" $
        pfails $ pencodeMintPreimage # pconstant q24ShortPolicyMint
    , testCase "q24_positive_control_28_byte_policy_encodes_and_verifies" $
        holds q24RealPolicyControl
    , testCase "q24_positive_control_empty_mint_encodes_and_verifies" $
        holds q24EmptyMintControl
    ]

q24MintEntriesSerialiseExactly :: forall s. Term s PBool
q24MintEntriesSerialiseExactly =
  pserialiseData # pconstant @PData q24AdaMint #== pconstant q24AdaMintDataCbor
    #&& pserialiseData # pconstant @PData q24RealMint #== pconstant q24RealMintDataCbor

q24RealPolicyControl :: forall s. Term s PBool
q24RealPolicyControl =
  plet (pmintPolicyItems # pconstant q24RealMint) $ \items ->
    pall'
      [ pencodeMintPreimage # pconstant q24RealMint
          #== pconstant q24RealMintFieldPreimageCbor
      , plength # items #== 1
      , pverifyCanonicalMintPreimageCbor
          # pconstant q24RealMintFieldPreimageCbor
          # (pfieldCommitmentFromItems # items)
      ]

q24EmptyMintControl :: forall s. Term s PBool
q24EmptyMintControl =
  pall'
    [ pencodeMintPreimage # pconstant q24EmptyMint #== pconstant "\x80"
    , pverifyCanonicalMintPreimageCbor
        # pconstant "\x80"
        # (pfieldCommitmentFromItems #$ pmintPolicyItems # pconstant q24EmptyMint)
    ]

q24MintData :: BS.ByteString -> BS.ByteString -> Integer -> Data
q24MintData policyId assetName quantity =
  Map [(B policyId, Map [(B assetName, I quantity)])]

q24AdaMint, q24RealMint, q24ShortPolicyMint, q24EmptyMint :: Data
q24AdaMint = q24MintData "" "" 5
q24RealMint = q24MintData q24RealPolicyId "\x01" 5
q24ShortPolicyMint = q24MintData q24ShortPolicyId "\x01" 5
q24EmptyMint = List []

q24RealPolicyId, q24ShortPolicyId :: BS.ByteString
q24RealPolicyId = goldenItemsHex "01010101010101010101010101010101010101010101010101010101"
q24ShortPolicyId = goldenItemsHex "0101010101010101010101010101010101010101010101010101ff"

q24AdaMintDataCbor, q24RealMintDataCbor, q24AdaMintFieldPreimageCbor, q24RealMintFieldPreimageCbor :: BS.ByteString
q24AdaMintDataCbor = goldenItemsHex "a140a14005"
q24RealMintDataCbor = goldenItemsHex "a1581c01010101010101010101010101010101010101010101010101010101a1410105"
q24AdaMintFieldPreimageCbor = goldenItemsHex "81458240a14005"
q24RealMintFieldPreimageCbor = goldenItemsHex "81582382581c01010101010101010101010101010101010101010101010101010101a1410105"

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

allItemLists :: [[BS.ByteString]]
allItemLists =
  [ []
  , ["\x01"]
  , ["", "\x01\x02", BS.replicate 40 0x03]
  , replicate 24 "\x07" -- forces the 98 header
  , replicate 300 "\x08" -- forces the 99 header
  ]

defaultHashes :: [BS.ByteString]
defaultHashes = [BS.replicate 28 0xaa, BS.replicate 28 0xbb]

data Input = Input BS.ByteString Integer

defaultInputs :: [Input]
defaultInputs = [Input (BS.replicate 32 0x01) 0, Input (BS.replicate 32 0x02) 7]

data Aw = Aw BS.ByteString BS.ByteString

defaultAws :: [Aw]
defaultAws = [Aw (BS.replicate 32 0x11) (BS.replicate 64 0x22)]

data Lang = NativeCardano | PlutusV3 | MidgardV1

data Scr = Scr Lang BS.ByteString

defaultScripts :: [Scr]
defaultScripts = [Scr NativeCardano "\x01", Scr PlutusV3 "\x02\x03", Scr MidgardV1 (BS.replicate 30 0x04)]

data Rw = Rw Integer Integer BS.ByteString Integer Integer

defaultRedeemer :: Rw
defaultRedeemer = Rw 0 1 "\x01\x02" 1000 2000000

defaultRedeemers :: [Rw]
defaultRedeemers = [defaultRedeemer, Rw 6 0 "" 1 1]

{- | An output with nothing optional set — the encoding detail lives in
"Testing.NativeTxComponents"; here it only has to be a real item.
-}
data Out = Out BS.ByteString Integer

defaultOutputs :: [Out]
defaultOutputs = [Out (BS.replicate 28 0xaa) 1000000, Out (BS.replicate 28 0xbb) 2]

-- | A mint: policies, each with named quantities.
type Mint = [(BS.ByteString, [(BS.ByteString, Integer)])]

defaultMint :: Mint
defaultMint = [(pidA, [("\x01", 5), ("\x02", 6)]), (pidB, [("", 1)])]

burnMint :: Mint
burnMint = [(pidA, [("\x01", -5)])]

allMints :: [Mint]
allMints =
  [ []
  , [(pidA, [("", 1)])]
  , defaultMint
  , burnMint
  , [(pidA, [(BS.replicate 23 0x00, 1), (BS.replicate 24 0x00, 1)])]
  , [(pidA, [("\x01", 4294967296)])]
  ]

pidA :: BS.ByteString
pidA = BS.replicate 28 0x01

pidB :: BS.ByteString
pidB = BS.replicate 28 0x02

policyEntry :: BS.ByteString -> (Data, Data)
policyEntry policyId = (B policyId, Map [(B "\x01", I 1)])

--------------------------------------------------------------------------------
-- Reference encoders, written from the format
--------------------------------------------------------------------------------

encodePreimage :: [BS.ByteString] -> BS.ByteString
encodePreimage items = arrayHeader (length items) <> mconcat (map definiteBytes items)

arrayHeader :: Int -> BS.ByteString
arrayHeader n
  | n <= 23 = BS.pack [fromIntegral (128 + n)]
  | n <= 255 = BS.pack [0x98, fromIntegral n]
  | n <= 65535 = BS.pack [0x99] <> be 2 (fromIntegral n)
  | otherwise = error "reference arrayHeader: out of fixture range"

mapHeader :: Int -> BS.ByteString
mapHeader n
  | n <= 23 = BS.pack [fromIntegral (160 + n)]
  | n <= 255 = BS.pack [0xb8, fromIntegral n]
  | otherwise = error "reference mapHeader: out of fixture range"

encodeInput :: Input -> BS.ByteString
encodeInput (Input txId index) = "\x82" <> definiteBytes txId <> "\x19" <> be 2 index

encodeOutput :: Out -> BS.ByteString
encodeOutput (Out paymentHash lovelace) =
  "\xa2\x00"
    <> definiteBytes (BS.pack [0x60] <> paymentHash)
    <> "\x01\x82"
    <> cborInt lovelace
    <> mapHeader 0

encodeAddressWitness :: Aw -> BS.ByteString
encodeAddressWitness (Aw key signature) =
  "\x82" <> definiteBytes key <> definiteBytes signature

langTag :: Lang -> Integer
langTag NativeCardano = 0
langTag PlutusV3 = 3
langTag MidgardV1 = 128

encodeScript :: Scr -> BS.ByteString
encodeScript (Scr lang bytes) = "\x82" <> cborInt (langTag lang) <> definiteBytes bytes

encodeRedeemer :: Rw -> BS.ByteString
encodeRedeemer (Rw purpose index cbor memory steps) =
  "\x84"
    <> cborInt purpose
    <> cborInt index
    <> definiteBytes cbor
    <> "\x82"
    <> cborInt memory
    <> cborInt steps

encodeMintPolicyItem :: (BS.ByteString, [(BS.ByteString, Integer)]) -> BS.ByteString
encodeMintPolicyItem (policyId, assets) =
  "\x82"
    <> definiteBytes policyId
    <> mapHeader (length assets)
    <> mconcat [definiteBytes name <> cborInt quantity | (name, quantity) <- assets]

encodeMint :: Mint -> BS.ByteString
encodeMint = encodePreimage . map encodeMintPolicyItem

mintData :: Mint -> Data
mintData [] = emptyMint
mintData policies =
  Map [(B policyId, Map [(B name, I q) | (name, q) <- assets]) | (policyId, assets) <- policies]

-- | An empty mint is the @Data@ list @[]@, not an empty map.
emptyMint :: Data
emptyMint = List []

--------------------------------------------------------------------------------
-- Hand-built malformed mint preimages
--------------------------------------------------------------------------------

descendingPolicies :: BS.ByteString
descendingPolicies =
  encodePreimage [encodeMintPolicyItem (pidB, [("\x01", 1)]), encodeMintPolicyItem (pidA, [("\x01", 1)])]

descendingAssets :: BS.ByteString
descendingAssets = encodePreimage [encodeMintPolicyItem (pidA, [("\x02", 1), ("\x01", 1)])]

emptyPolicy :: BS.ByteString
emptyPolicy = encodePreimage [encodeMintPolicyItem (pidA, [])]

zeroQuantity :: BS.ByteString
zeroQuantity = encodePreimage [encodeMintPolicyItem (pidA, [("\x01", 0)])]

-- | A one-byte asset name spelt in the @58 01@ form, which canonical CBOR forbids.
nonMinimalName :: BS.ByteString
nonMinimalName =
  encodePreimage
    ["\x82" <> definiteBytes pidA <> mapHeader 1 <> "\x58\x01\x01" <> cborInt 1]

-- | A one-entry asset map spelt in the @b8 01@ form.
nonMinimalMapHeader :: BS.ByteString
nonMinimalMapHeader =
  encodePreimage
    ["\x82" <> definiteBytes pidA <> "\xb8\x01" <> definiteBytes "\x01" <> cborInt 1]

paddedPolicyItem :: BS.ByteString
paddedPolicyItem = encodePreimage [encodeMintPolicyItem (pidA, [("\x01", 1)]) <> "\x00"]

--------------------------------------------------------------------------------
-- CBOR primitives, independent of the port
--------------------------------------------------------------------------------

cborInt :: Integer -> BS.ByteString
cborInt n
  | n >= 0 = major 0 n
  | otherwise = major 1 (-1 - n)
  where
    major base v
      | v <= 23 = BS.pack [fromIntegral (base * 32 + v)]
      | v <= 255 = BS.pack [fromIntegral (base * 32 + 24), fromIntegral v]
      | v <= 65535 = BS.pack [fromIntegral (base * 32 + 25)] <> be 2 v
      | v <= 4294967295 = BS.pack [fromIntegral (base * 32 + 26)] <> be 4 v
      | otherwise = BS.pack [fromIntegral (base * 32 + 27)] <> be 8 v

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | len <= 23 = BS.pack [fromIntegral (64 + len)] <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | otherwise = error "reference definiteBytes: out of fixture range"
  where
    len = BS.length bytes

be :: Int -> Integer -> BS.ByteString
be width n =
  BS.pack [fromIntegral (n `div` (256 ^ i) `mod` 256) | i <- reverse [0 .. width - 1]]

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

--------------------------------------------------------------------------------
-- Building the Plutarch values
--------------------------------------------------------------------------------

plist :: forall s a. PIsListLike PBuiltinList a => [Term s a] -> Term s (PBuiltinList a)
plist = foldr (\x acc -> pcons # x # acc) pnil

itemsT :: forall s. [BS.ByteString] -> Term s (PBuiltinList PByteString)
itemsT = plist . map pconstant

inputsT :: forall s. [Input] -> Term s (PBuiltinList (PAsData PMidgardTxInput))
inputsT = plist . map inputT
  where
    inputT (Input txId index) =
      pdata . pcon $
        PMidgardTxInput
          { ptxInput'txId = pdata (pconstant txId)
          , ptxInput'outputIndex = pdata (pconstant index)
          }

outputsT :: forall s. [Out] -> Term s (PBuiltinList (PAsData PMidgardTxOutput))
outputsT = plist . map outT
  where
    outT (Out paymentHash lovelace) =
      pdata . pcon $
        PMidgardTxOutput
          { ptxOutput'address =
              pdata . pcon $
                PMidgardAddress
                  { paddress'protected = pdata (pconstant False)
                  , paddress'networkId = pdata (pconstant 0)
                  , paddress'paymentCredential =
                      pdata (pcon (PMidgardPubKeyCredential (pdata (pconstant paymentHash))))
                  , paddress'stakeCredential = pdata (pcon PDNothing)
                  }
          , ptxOutput'value =
              pdata . pcon $
                PMidgardValue
                  { pvalue'lovelace = pdata (pconstant lovelace)
                  , pvalue'assets = pdata (pcon (PAssocMap pnil))
                  }
          , ptxOutput'datumCbor = pdata (pcon PDNothing)
          , ptxOutput'scriptRef = pdata (pcon PDNothing)
          }

addressWitnessesT :: forall s. [Aw] -> Term s (PBuiltinList (PAsData PMidgardAddressWitness))
addressWitnessesT = plist . map awT
  where
    awT (Aw key signature) =
      pdata . pcon $
        PMidgardAddressWitness
          { paddressWitness'verificationKey = pdata (pconstant key)
          , paddressWitness'signature = pdata (pconstant signature)
          }

scriptsT :: forall s. [Scr] -> Term s (PBuiltinList (PAsData PMidgardVersionedScript))
scriptsT = plist . map scrT
  where
    scrT (Scr lang bytes) =
      pdata . pcon $
        PMidgardVersionedScript
          { pversionedScript'language = pdata (langT lang)
          , pversionedScript'scriptBytes = pdata (pconstant bytes)
          }
    langT NativeCardano = pcon PNativeCardanoScript
    langT PlutusV3 = pcon PPlutusV3Script
    langT MidgardV1 = pcon PMidgardV1Script

redeemersT :: forall s. [Rw] -> Term s (PBuiltinList (PAsData PMidgardRedeemerWitness))
redeemersT = plist . map rwT
  where
    rwT (Rw purpose index cbor memory steps) =
      pdata . pcon $
        PMidgardRedeemerWitness
          { predeemerWitness'purpose = pdata (purposeT purpose)
          , predeemerWitness'index = pdata (pconstant index)
          , predeemerWitness'redeemerCbor = pdata (pconstant cbor)
          , predeemerWitness'executionUnits =
              pdata . pcon $
                PMidgardExecutionUnits
                  { pexecutionUnits'memory = pdata (pconstant memory)
                  , pexecutionUnits'steps = pdata (pconstant steps)
                  }
          }
    purposeT 0 = pcon PSpendRedeemer
    purposeT 6 = pcon PReceiveRedeemer
    purposeT n = error ("unused redeemer purpose in fixtures: " <> show n)

inputPreimage :: BS.ByteString
inputPreimage = encodePreimage (map encodeInput defaultInputs)

outputPreimage :: BS.ByteString
outputPreimage = encodePreimage (map encodeOutput defaultOutputs)

awPreimage :: BS.ByteString
awPreimage = encodePreimage (map encodeAddressWitness defaultAws)

scriptPreimage :: BS.ByteString
scriptPreimage = encodePreimage (map encodeScript defaultScripts)

redeemerPreimage :: BS.ByteString
redeemerPreimage = encodePreimage (map encodeRedeemer defaultRedeemers)

--------------------------------------------------------------------------------
-- Assertion helpers
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

pall' :: forall s. [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

-- | The count a header decodes to, forced.
pheaderCountOf :: forall s. Term s PByteString -> Term s PInteger
pheaderCountOf bytes = psndOf (pdecodeFieldArrayHeader # bytes)

-- | The header decodes to this count, and to an offset at its own end.
pheaderIs :: forall s. Term s PByteString -> Integer -> Term s PBool
pheaderIs bytes n =
  pmatch (pdecodeFieldArrayHeader # bytes) $ \(PPair offset count) ->
    count #== pconstant n #&& offset #== plengthBS # bytes

psndOf :: forall s a b. Term s (PPair a b) -> Term s b
psndOf p = pmatch p $ \(PPair _ b) -> b
