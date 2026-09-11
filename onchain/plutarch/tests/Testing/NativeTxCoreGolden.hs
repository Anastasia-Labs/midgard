{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Testing.NativeTxCoreGolden (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.String (fromString)
import PlutusLedgerApi.V3 (Data (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Compact (
  pdecodeNativeTxCompactV1,
  pencodeNativeTxCompactV1,
  pnativeTxFullHashV1,
  pverifyNativeTxCompactCborV1,
  pverifyNativeTxCompactV1,
 )
import Midgard.FraudProofs.NativeTx.Components (
  pdecodeMidgardTxOutputCbor,
  pencodeMidgardTxOutput,
 )
import Midgard.FraudProofs.NativeTx.Preimages (
  pdecodeMidgardTxAddressWitnessesPreimageCbor,
  pdecodeMidgardTxByteListPreimageCbor,
  pdecodeMidgardTxInputsPreimageCbor,
  pdecodeMidgardTxMintPreimageCbor,
  pdecodeMidgardTxOutputsPreimageCbor,
  pdecodeMidgardTxRedeemerWitnessesPreimageCbor,
  pdecodeMidgardTxScriptWitnessesPreimageCbor,
  pencodeMintPreimage,
 )
import Midgard.FraudProofs.NativeTx.Transaction (
  pdecodeMidgardTransactionV1,
  pencodeMidgardTransactionV1,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardTxInput (..),
  PMidgardTxOutput (..),
  PMidgardValue (..),
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Native Tx Core Golden Aiken Parity"
    [ testCase "golden_midgard_core_native_full_tx_decodes" $
        passertEval $
          pencodeMidgardTransactionV1 # (pdecodeMidgardTransactionV1 # pconstant fullTxCbor)
            #== pconstant fullTxCbor
    , testCase "golden_midgard_core_native_preimage_cbor_decoders" $
        passertEval preimageDecodersGolden
    , testCase "golden_midgard_core_native_compact_tx_decodes" $
        passertEval compactTransactionGolden
    , testCase "decode_midgard_value_preserves_unsorted_asset_order" $
        passertEval unsortedAssetOrderGolden
    , testCase "empty_mint_map_data_rejects_canonical_preimage_encoding" $
        pfails $ pencodeMintPreimage # pconstant @PData (Map [])
    , testCase "v1_full_transaction_hash_matches_typescript_domain_vector" $
        passertEval $
          pnativeTxFullHashV1 # pconstant "\x80"
            #== phex "00039b53f81dc1d7d1e55c12c2369f2cbe7aa4c2943f990ddf7faee2377b3178"
    ]

preimageDecodersGolden :: forall s. Term s PBool
preimageDecodersGolden =
  plet (pdecodeMidgardTxInputsPreimageCbor # pconstant spendPreimageCbor) $ \spendInputs ->
    plet (pdecodeMidgardTxInputsPreimageCbor # pconstant referencePreimageCbor) $ \referenceInputs ->
      plet (pdecodeMidgardTxOutputsPreimageCbor # pconstant outputsPreimageCbor) $ \outputs ->
        plet (pfromData $ phead # spendInputs) $ \spendInput ->
          plet (pfromData $ phead # referenceInputs) $ \referenceInput ->
            plet (pfromData $ phead # outputs) $ \output ->
              pmatch spendInput $ \PMidgardTxInput {ptxInput'txId, ptxInput'outputIndex} ->
                pmatch referenceInput $ \PMidgardTxInput {ptxInput'txId = referenceTxId, ptxInput'outputIndex = referenceIndex} ->
                  pmatch output $ \PMidgardTxOutput {ptxOutput'value} ->
                    plet (pfromData ptxOutput'value) $ \value ->
                      pmatch value $ \PMidgardValue {pvalue'lovelace, pvalue'assets} ->
                        pall'
                          [ plength # spendInputs #== 1
                          , pblake2b_256 # pconstant spendPreimageCbor
                              #== phex "214c88efa8d641e100907432a62d5a6090c840f5c845f4d97785e72842df9795"
                          , pfromData ptxInput'txId #== phex "1111111111111111111111111111111111111111111111111111111111111111"
                          , pfromData ptxInput'outputIndex #== 0
                          , plength # referenceInputs #== 1
                          , pblake2b_256 # pconstant referencePreimageCbor
                              #== phex "c7d58dc93d2a5850ba53d9c3b54199c68bb009fc43d808d98860416d2e3bc6e0"
                          , pfromData referenceTxId #== phex "2222222222222222222222222222222222222222222222222222222222222222"
                          , pfromData referenceIndex #== 1
                          , plength # outputs #== 1
                          , pencodeMidgardTxOutput # output #== pconstant goldenOutputCbor
                          , pfromData pvalue'lovelace #== 2_000_000
                          , pforgetData pvalue'assets
                              #== pconstant @PData (Map [(B goldenAssetUnit, I 5)])
                          , pnull # (pdecodeMidgardTxByteListPreimageCbor # pconstant emptyPreimageCbor)
                          , pblake2b_256 # pconstant emptyPreimageCbor #== pconstant emptyPreimageHash
                          , pdecodeMidgardTxMintPreimageCbor # pconstant emptyPreimageCbor
                              #== pconstant @PData (List [])
                          , pnull # (pdecodeMidgardTxAddressWitnessesPreimageCbor # pconstant emptyPreimageCbor)
                          , pnull # (pdecodeMidgardTxScriptWitnessesPreimageCbor # pconstant emptyPreimageCbor)
                          , pnull # (pdecodeMidgardTxRedeemerWitnessesPreimageCbor # pconstant emptyPreimageCbor)
                          ]

compactTransactionGolden :: forall s. Term s PBool
compactTransactionGolden =
  plet (pdecodeNativeTxCompactV1 # pconstant compactTxCbor) $ \compact ->
    plet (pverifyNativeTxCompactV1 # pconstant goldenTxId # compact # pconstant compactTxCbor) $ \verified ->
      plet (pverifyNativeTxCompactCborV1 # pconstant goldenTxId # pconstant compactTxCbor) $ \verifiedFromCbor ->
        pmatch verified $ \PVerifiedMidgardNativeTxCompact {pverified'txId, pverified'version, pverified'txCompact} ->
          pmatch verifiedFromCbor $ \PVerifiedMidgardNativeTxCompact {pverified'txId = cborTxId, pverified'version = cborVersion, pverified'txCompact = cborCompact} ->
            pall'
              [ pverified'txId #== cborTxId
              , pverified'version #== cborVersion
              , pencodeNativeTxCompactV1 # pverified'txCompact
                  #== pencodeNativeTxCompactV1 # cborCompact
              , compactFieldsMatch pverified'txCompact
              ]

unsortedAssetOrderGolden :: forall s. Term s PBool
unsortedAssetOrderGolden =
  plet (pdecodeMidgardTxOutputCbor # pconstant unsortedOutputCbor) $ \output ->
    pmatch output $ \PMidgardTxOutput {ptxOutput'value} ->
      plet (pfromData ptxOutput'value) $ \value ->
        pmatch value $ \PMidgardValue {pvalue'lovelace, pvalue'assets} ->
          pall'
            [ pfromData pvalue'lovelace #== 0
            , pforgetData pvalue'assets
                #== pconstant
                  @PData
                  ( Map
                      [ (B (unsortedPolicyId <> "\x02"), I 1)
                      , (B (unsortedPolicyId <> "\x01"), I 2)
                      ]
                  )
            , pencodeMidgardTxOutput # output #== pconstant unsortedOutputCbor
            ]

compactFieldsMatch :: forall s. Term s PNativeTxCompact -> Term s PBool
compactFieldsMatch compact =
  pmatch compact $ \PNativeTxCompact {pcompact'body, pcompact'witnessSetHash, pcompact'validityCode} ->
    pmatch pcompact'body $ \PNativeTxBodyCompact {pbodyCompact'spendInputsHash, pbodyCompact'referenceInputsHash, pbodyCompact'outputsHash} ->
      pall'
        [ pcompact'validityCode #== 0
        , pbodyCompact'spendInputsHash #== phex "214c88efa8d641e100907432a62d5a6090c840f5c845f4d97785e72842df9795"
        , pbodyCompact'referenceInputsHash #== phex "c7d58dc93d2a5850ba53d9c3b54199c68bb009fc43d808d98860416d2e3bc6e0"
        , pbodyCompact'outputsHash #== phex "90c0662cc2568cf510b3d965486a0a7a4494e811045b237c945158b3e14d16fb"
        , pcompact'witnessSetHash #== phex "52408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c"
        ]

pall' :: forall s. [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

phex :: forall s. String -> Term s PByteString
phex = pconstant . Base16.decodeLenient . fromString

goldenAssetUnit :: BS.ByteString
goldenAssetUnit = Base16.decodeLenient "7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a4d4944"

unsortedPolicyId, unsortedOutputCbor :: BS.ByteString
unsortedPolicyId = BS.replicate 28 0x01
unsortedOutputCbor =
  "\xa2\x00\x58\x1d\x60"
    <> BS.replicate 28 0xaa
    <> "\x01\x82\x00\xa1\x58\x1c"
    <> unsortedPolicyId
    <> "\xa2\x41\x02\x01\x41\x01\x02"

fullTxCbor, compactTxCbor, goldenTxId, spendPreimageCbor, referencePreimageCbor, outputsPreimageCbor, goldenOutputCbor, emptyPreimageCbor, emptyPreimageHash :: BS.ByteString
fullTxCbor = Base16.decodeLenient "84018c5829815826825820111111111111111111111111111111111111111111111111111111111111111119000058298158268258202222222222222222222222222222222222222222222222222222222222222222190001585081584da200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a001e8480a1581c7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7aa1434d494405182a2018644180418041805820333333333333333333333333333333333333333333333333333333333333333358204444444444444444444444444444444444444444444444444444444444444444008341804180418000"
compactTxCbor = Base16.decodeLenient "84018c5820214c88efa8d641e100907432a62d5a6090c840f5c845f4d97785e72842df97955820c7d58dc93d2a5850ba53d9c3b54199c68bb009fc43d808d98860416d2e3bc6e0582090c0662cc2568cf510b3d965486a0a7a4494e811045b237c945158b3e14d16fb182a201864582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582033333333333333333333333333333333333333333333333333333333333333335820444444444444444444444444444444444444444444444444444444444444444400582052408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c00"
goldenTxId = Base16.decodeLenient "f0edf52dff58dd7fac556b76007da81ed62655351622f4ce9c8ec492d66a22ec"
spendPreimageCbor = Base16.decodeLenient "8158268258201111111111111111111111111111111111111111111111111111111111111111190000"
referencePreimageCbor = Base16.decodeLenient "8158268258202222222222222222222222222222222222222222222222222222222222222222190001"
outputsPreimageCbor = Base16.decodeLenient "81584da200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a001e8480a1581c7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7aa1434d494405"
goldenOutputCbor = BS.drop 3 outputsPreimageCbor
emptyPreimageCbor = Base16.decodeLenient "80"
emptyPreimageHash = Base16.decodeLenient "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"
