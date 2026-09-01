{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxCarriageWireGolden
Description : Exact §8.6/§8.8 wire vectors from Aiken and TypeScript.
-}
module Testing.NativeTxCarriageWireGolden (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Data (pasByteStr, pasInt, pasList, pserialiseData)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PPubKeyHash)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Aiken.Cbor (pdeserialise)
import Midgard.NativeTxCarriage (PFieldPreimageCertificateMintRedeemerV1 (..))
import Midgard.NativeTxFieldAccess (
  PFieldCarriageV1 (..),
  PFieldPreimageCertificateV1 (..),
 )
import Testing.Eval (passertEval, pfails)

-- The production view is Scott-encoded for internal use. This mirror pins the
-- public Aiken Data ABI without changing that runtime representation.
data PFieldViewWireV1 (s :: S)
  = PWholeViewWire
      { pwholeWire'bytes :: Term s (PAsData PByteString)
      , pwholeWire'count :: Term s (PAsData PInteger)
      , pwholeWire'stride :: Term s (PAsData PInteger)
      }
  | PChunkedViewWire
      { pchunkedWire'chunks :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      , pchunkedWire'chunkDigests :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
      , pchunkedWire'count :: Term s (PAsData PInteger)
      , pchunkedWire'stride :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFieldViewWireV1)

tests :: TestTree
tests =
  testGroup
    "Native Tx Carriage Wire V1 Golden Aiken Parity"
    [ testCase "golden_carriage_inline_chunked_preimage_round_trips" $
        passertEval $ roundTrips inlineChunked carriageInlineChunkedCbor
    , testCase "golden_carriage_inline_63_byte_preimage_round_trips" $
        passertEval $ roundTrips (inlineValue inline63) carriageInline63Cbor
    , testCase "golden_carriage_inline_64_byte_preimage_round_trips" $
        passertEval $ roundTrips (inlineValue inline64) carriageInline64Cbor
    , testCase "golden_carriage_inline_empty_field_round_trips" $
        passertEval $ roundTrips (inlineValue "\x80") carriageInlineEmptyCbor
    , testCase "golden_carriage_raw_utxo_round_trips" $
        passertEval $ roundTrips (pcon $ PRawUtxo $ pdata 3) carriageRawUtxoCbor
    , testCase "golden_carriage_certified_three_chunks_round_trips" $
        passertEval $ roundTrips certifiedThreeChunks carriageCertifiedThreeChunksCbor
    , testCase "golden_view_whole_empty_field_round_trips" $
        passertEval $ roundTrips wholeEmptyView viewWholeEmptyCbor
    , testCase "golden_view_chunked_three_chunk_corner_round_trips" $
        passertEval $ roundTrips chunkedCornerView viewChunkedCornerCbor
    , testCase "golden_certificate_three_chunk_corner_round_trips" $
        passertEval $ roundTrips certificateCorner certificateCornerCbor
    , testCase "golden_mint_redeemer_certify_chunked_arguments_round_trips" $
        passertEval $ roundTrips certifyChunked mintCertifyChunkedCbor
    , testCase "golden_mint_redeemer_certify_short_arguments_round_trips" $
        passertEval $ roundTrips certifyShort mintCertifyShortCbor
    , testCase "golden_mint_redeemer_retire_round_trips" $
        passertEval $ roundTrips (pcon PRetire) mintRetireCbor
    , testCase "golden_carriage_trailing_bytes_rejects" $
        passertEval $ pnot # parses carriageTrailingBytesCbor
    , testCase "golden_carriage_constructor_index_out_of_range_parses_as_cbor" $
        passertEval $ parses carriageConstructorOutOfRangeCbor
    , testCase "golden_carriage_constructor_index_out_of_range_rejects" $
        rejects carriageSchema carriageConstructorOutOfRangeCbor
    , testCase "golden_carriage_inline_preimage_as_integer_parses_as_cbor" $
        passertEval $ parses carriageInlineIntegerCbor
    , testCase "golden_carriage_inline_preimage_as_integer_rejects" $
        rejects carriageSchema carriageInlineIntegerCbor
    , testCase "golden_carriage_inline_extra_field_parses_as_cbor" $
        passertEval $ parses carriageInlineExtraFieldCbor
    , testCase "golden_carriage_inline_extra_field_rejects" $
        rejects carriageSchema carriageInlineExtraFieldCbor
    , testCase "golden_view_trailing_bytes_rejects" $
        passertEval $ pnot # parses viewTrailingBytesCbor
    , testCase "golden_view_constructor_index_out_of_range_parses_as_cbor" $
        passertEval $ parses viewConstructorOutOfRangeCbor
    , testCase "golden_view_constructor_index_out_of_range_rejects" $
        rejects viewSchema viewConstructorOutOfRangeCbor
    , testCase "golden_view_whole_missing_stride_parses_as_cbor" $
        passertEval $ parses viewWholeMissingStrideCbor
    , testCase "golden_view_whole_missing_stride_rejects" $
        rejects viewSchema viewWholeMissingStrideCbor
    , testCase "golden_view_whole_count_as_bytes_parses_as_cbor" $
        passertEval $ parses viewWholeCountAsBytesCbor
    , testCase "golden_view_whole_count_as_bytes_rejects" $
        rejects viewSchema viewWholeCountAsBytesCbor
    , testCase "golden_view_chunked_chunks_as_bytes_parses_as_cbor" $
        passertEval $ parses viewChunkedChunksAsBytesCbor
    , testCase "golden_view_chunked_chunks_as_bytes_rejects" $
        rejects viewSchema viewChunkedChunksAsBytesCbor
    , testCase "golden_certificate_trailing_bytes_rejects" $
        passertEval $ pnot # parses certificateTrailingBytesCbor
    , testCase "golden_certificate_missing_chunk_digests_parses_as_cbor" $
        passertEval $ parses certificateMissingDigestsCbor
    , testCase "golden_certificate_missing_chunk_digests_rejects" $
        rejects certificateSchema certificateMissingDigestsCbor
    , testCase "golden_certificate_field_index_as_bytes_parses_as_cbor" $
        passertEval $ parses certificateFieldIndexAsBytesCbor
    , testCase "golden_certificate_field_index_as_bytes_rejects" $
        rejects certificateSchema certificateFieldIndexAsBytesCbor
    , testCase "golden_mint_redeemer_trailing_bytes_rejects" $
        passertEval $ pnot # parses mintTrailingBytesCbor
    , testCase "golden_mint_redeemer_constructor_index_out_of_range_parses_as_cbor" $
        passertEval $ parses mintConstructorOutOfRangeCbor
    , testCase "golden_mint_redeemer_constructor_index_out_of_range_rejects" $
        rejects mintRedeemerSchema mintConstructorOutOfRangeCbor
    , testCase "golden_mint_redeemer_certify_indices_as_bytes_parses_as_cbor" $
        passertEval $ parses mintIndicesAsBytesCbor
    , testCase "golden_mint_redeemer_certify_indices_as_bytes_rejects" $
        rejects mintRedeemerSchema mintIndicesAsBytesCbor
    , testCase "golden_mint_redeemer_certify_missing_output_index_parses_as_cbor" $
        passertEval $ parses mintMissingOutputIndexCbor
    , testCase "golden_mint_redeemer_certify_missing_output_index_rejects" $
        rejects mintRedeemerSchema mintMissingOutputIndexCbor
    , testCase "golden_mint_redeemer_retire_extra_field_parses_as_cbor" $
        passertEval $ parses mintRetireExtraFieldCbor
    , testCase "golden_mint_redeemer_retire_extra_field_rejects" $
        rejects mintRedeemerSchema mintRetireExtraFieldCbor
    ]

roundTrips :: forall a s. PIsData a => Term s a -> BS.ByteString -> Term s PBool
roundTrips value bytes =
  withDecoded bytes $ \decoded ->
    let rebuilt = pforgetData (pdata value)
     in decoded #== rebuilt #&& pserialiseData # rebuilt #== pconstant bytes

parses :: forall s. BS.ByteString -> Term s PBool
parses bytes =
  pmatch (pdeserialise # pconstant bytes) $ \case
    PNothing -> pconstant False
    PJust _ -> pconstant True

withDecoded :: forall s. BS.ByteString -> (Term s PData -> Term s PBool) -> Term s PBool
withDecoded bytes continuation =
  pmatch (pdeserialise # pconstant bytes) $ \case
    PNothing -> perror
    PJust value -> continuation value

rejects :: (forall s. Term s PData -> Term s PBool) -> BS.ByteString -> Assertion
rejects schema bytes = pfails $ withDecoded bytes schema

data FieldShape = BytesField | IntegerField | ByteListField | IntegerListField

carriageSchema, viewSchema, certificateSchema, mintRedeemerSchema :: forall s. Term s PData -> Term s PBool
carriageSchema value = forceConstructor value [(0, [BytesField]), (1, [IntegerField]), (2, [IntegerField, IntegerListField])]
viewSchema value = forceConstructor value [(0, [BytesField, IntegerField, IntegerField]), (1, [ByteListField, ByteListField, IntegerField, IntegerField])]
certificateSchema value = forceConstructor value [(0, [BytesField, BytesField, IntegerField, IntegerField, ByteListField])]
mintRedeemerSchema value = forceConstructor value [(0, [BytesField, BytesField, IntegerListField, IntegerField]), (1, [])]

forceConstructor :: forall s. Term s PData -> [(Integer, [FieldShape])] -> Term s PBool
forceConstructor value alternatives =
  pmatch (pasConstr # value) $ \(PBuiltinPair tag fields) ->
    foldr
      (\(expectedTag, shapes) rest -> pif (tag #== pconstant expectedTag) (forceFields fields shapes) rest)
      perror
      alternatives

forceFields :: forall s. Term s (PBuiltinList PData) -> [FieldShape] -> Term s PBool
forceFields fields shapes =
  pif
    (plength # fields #== pconstant (fromIntegral $ length shapes))
    (pand'List [forceField shape (pelemAt # pconstant index # fields) | (index, shape) <- zip [0 ..] shapes])
    perror

forceField :: forall s. FieldShape -> Term s PData -> Term s PBool
forceField BytesField value = plengthBS # (pasByteStr # value) #>= 0
forceField IntegerField value = pasInt # value #== pasInt # value
forceField ByteListField value =
  pall # plam (\item -> plengthBS # (pasByteStr # item) #>= 0) # (pasList # value)
forceField IntegerListField value =
  pall # plam (\item -> pasInt # item #== pasInt # item) # (pasList # value)

inlineValue :: forall s. BS.ByteString -> Term s PFieldCarriageV1
inlineValue bytes = pcon $ PInline $ pdata (pconstant bytes)

inlineChunked :: forall s. Term s PFieldCarriageV1
inlineChunked = inlineValue inlineChunkedPreimage

certifiedThreeChunks :: forall s. Term s PFieldCarriageV1
certifiedThreeChunks =
  pcon $
    PCertified
      { pcertified'certRefInputIndex = pdata 0
      , pcertified'chunkRefInputIndices = pdata (pconstant [1 :: Integer, 2, 3])
      }

wholeEmptyView :: forall s. Term s PFieldViewWireV1
wholeEmptyView =
  pcon $
    PWholeViewWire
      { pwholeWire'bytes = pdata (pconstant "\x80")
      , pwholeWire'count = pdata 0
      , pwholeWire'stride = pdata 40
      }

chunkedCornerView :: forall s. Term s PFieldViewWireV1
chunkedCornerView =
  pcon $
    PChunkedViewWire
      { pchunkedWire'chunks = pdata (byteStringDataList [chunkedViewFirstChunk, "\x01\x02\x03"])
      , pchunkedWire'chunkDigests = pdata (byteStringDataList [BS.replicate 32 0x33, BS.replicate 32 0x44])
      , pchunkedWire'count = pdata 819
      , pchunkedWire'stride = pdata 40
      }

certificateCorner :: forall s. Term s PFieldPreimageCertificateV1
certificateCorner =
  pcon $
    PFieldPreimageCertificateV1
      { pcert'owner = pdata (punsafeCoerce @PPubKeyHash $ pconstant @PByteString $ BS.replicate 28 0x11)
      , pcert'txId = pdata (pconstant $ BS.replicate 32 0x22)
      , pcert'fieldIndex = pdata 5
      , pcert'totalLength = pdata 32_763
      , pcert'chunkDigests = pdata (byteStringDataList [BS.replicate 32 0x33, BS.replicate 32 0x44, BS.replicate 32 0x55])
      }

certifyChunked, certifyShort :: forall s. Term s PFieldPreimageCertificateMintRedeemerV1
certifyChunked =
  pcon $
    PCertify
      { pcertify'compactCbor = pdata (pconstant certifyChunkedCompact)
      , pcertify'witnessSetCompactCbor = pdata (pconstant certifyChunkedWitness)
      , pcertify'chunkRefInputIndices = pdata (pconstant [1 :: Integer, 2, 3])
      , pcertify'outputIndex = pdata 0
      }
certifyShort =
  pcon $
    PCertify
      { pcertify'compactCbor = pdata (pconstant $ hex "a10203")
      , pcertify'witnessSetCompactCbor = pdata (pconstant $ hex "b204")
      , pcertify'chunkRefInputIndices = pdata (pconstant [0 :: Integer, 1])
      , pcertify'outputIndex = pdata 2
      }

byteStringDataList :: forall s. [BS.ByteString] -> Term s (PBuiltinList (PAsData PByteString))
byteStringDataList =
  foldr
    (\bytes rest -> pcons # pdata (pconstant bytes) # rest)
    pnil

inlineChunkedPreimage, inline63, inline64, chunkedViewFirstChunk, certifyChunkedCompact, certifyChunkedWitness :: BS.ByteString
inlineChunkedPreimage = hex "939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bc"
inline63 = hex "636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e15"
inline64 = hex "737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c"
chunkedViewFirstChunk = hex "232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3"
certifyChunkedCompact = hex "141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9"
certifyChunkedWitness = hex "434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f26"

carriageInlineChunkedCbor, carriageInline63Cbor, carriageInline64Cbor, carriageInlineEmptyCbor, carriageRawUtxoCbor, carriageCertifiedThreeChunksCbor :: BS.ByteString
carriageInlineChunkedCbor = hex "d8799f5f5840939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c50535a61686f767d848b9299a0a7aeb5bcffff"
carriageInline63Cbor = hex "d8799f583f636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e15ff"
carriageInline64Cbor = hex "d8799f5840737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252cff"
carriageInlineEmptyCbor = hex "d8799f4180ff"
carriageRawUtxoCbor = hex "d87a9f03ff"
carriageCertifiedThreeChunksCbor = hex "d87b9f009f010203ffff"

viewWholeEmptyCbor, viewChunkedCornerCbor, certificateCornerCbor, mintCertifyChunkedCbor, mintCertifyShortCbor, mintRetireCbor :: BS.ByteString
viewWholeEmptyCbor = hex "d8799f4180001828ff"
viewChunkedCornerCbor = hex "d87a9f9f5f5840232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dc41e3ff43010203ff9f5820333333333333333333333333333333333333333333333333333333333333333358204444444444444444444444444444444444444444444444444444444444444444ff1903331828ff"
certificateCornerCbor = hex "d8799f581c111111111111111111111111111111111111111111111111111111115820222222222222222222222222222222222222222222222222222222222222222205197ffb9f582033333333333333333333333333333333333333333333333333333333333333335820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555ffff"
mintCertifyChunkedCbor = hex "d8799f5f5840141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cd5824d4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9ff5f5840434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc46030a11181f26ff9f010203ff00ff"
mintCertifyShortCbor = hex "d8799f43a1020342b2049f0001ff02ff"
mintRetireCbor = hex "d87a80"

carriageTrailingBytesCbor, carriageConstructorOutOfRangeCbor, carriageInlineIntegerCbor, carriageInlineExtraFieldCbor :: BS.ByteString
carriageTrailingBytesCbor = hex "d8799f4180ff00"
carriageConstructorOutOfRangeCbor = hex "d87c9f4180ff"
carriageInlineIntegerCbor = hex "d8799f05ff"
carriageInlineExtraFieldCbor = hex "d8799f418043010203ff"

viewTrailingBytesCbor, viewConstructorOutOfRangeCbor, viewWholeMissingStrideCbor, viewWholeCountAsBytesCbor, viewChunkedChunksAsBytesCbor :: BS.ByteString
viewTrailingBytesCbor = hex "d8799f4180001828ff00"
viewConstructorOutOfRangeCbor = hex "d87b9f4180001828ff"
viewWholeMissingStrideCbor = hex "d8799f418000ff"
viewWholeCountAsBytesCbor = hex "d8799f4180430102031828ff"
viewChunkedChunksAsBytesCbor = hex "d87a9f430102039f58203333333333333333333333333333333333333333333333333333333333333333ff011828ff"

certificateTrailingBytesCbor, certificateMissingDigestsCbor, certificateFieldIndexAsBytesCbor :: BS.ByteString
certificateTrailingBytesCbor = hex "d8799f581c111111111111111111111111111111111111111111111111111111115820222222222222222222222222222222222222222222222222222222222222222205197ffb9f582033333333333333333333333333333333333333333333333333333333333333335820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555ffff00"
certificateMissingDigestsCbor = hex "d8799f581c111111111111111111111111111111111111111111111111111111115820222222222222222222222222222222222222222222222222222222222222222205197ffbff"
certificateFieldIndexAsBytesCbor = hex "d8799f581c11111111111111111111111111111111111111111111111111111111582022222222222222222222222222222222222222222222222222222222222222224105197ffb9f582033333333333333333333333333333333333333333333333333333333333333335820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555ffff"

mintTrailingBytesCbor, mintConstructorOutOfRangeCbor, mintIndicesAsBytesCbor, mintMissingOutputIndexCbor, mintRetireExtraFieldCbor :: BS.ByteString
mintTrailingBytesCbor = hex "d87a8000"
mintConstructorOutOfRangeCbor = hex "d87b80"
mintIndicesAsBytesCbor = hex "d8799f43a1020342b2044301020302ff"
mintMissingOutputIndexCbor = hex "d8799f43a1020342b2049f0001ffff"
mintRetireExtraFieldCbor = hex "d87a9f00ff"

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient
