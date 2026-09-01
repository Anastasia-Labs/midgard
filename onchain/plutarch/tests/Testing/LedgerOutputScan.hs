{-# LANGUAGE OverloadedStrings #-}

module Testing.LedgerOutputScan (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.LedgerOutputScan
import Midgard.ValidationMerkle (pemptyFrontier)
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.LedgerOutputScan"
  [ testCase "scans_required_fields_without_revealing_the_complete_output" $ passertEvalNoTrace scansRequiredFields
  , testCase "derives_the_exact_cardano_value_size_for_multi_asset_output" $ passertEvalNoTrace derivesValueSize
  , testCase "advances_a_large_datum_only_to_authenticated_chunk_boundaries" $ passertEvalNoTrace advancesLargeDatum
  , testCase "rejects_trailing_bytes_at_the_terminal_edge" $ passertEvalNoTrace rejectsTrailingBytes
  , testCase "decodes_and_reencodes_the_typescript_terminal_control" $ passertEvalNoTrace decodesTerminalControl
  , testCase "rejects_non_minimal_cbor_and_malformed_control_state" $ passertEvalNoTrace rejectsMalformed
  , testCase "rejects_inline_datum_without_plutus_data_bytes" $ passertEvalNoTrace rejectsEmptyDatum
  ]

scansRequiredFields :: forall s. Term s PBool
scansRequiredFields =
  plet (byteLength outputWithoutOptionalFields) $ \total ->
  plet (expectJust $ pstepV1 # pinitialControlV1 # total # outputWithoutOptionalFields # 0) $ \valueControl ->
  plet (expectJust $ pstepV1 # valueControl # total # outputWithoutOptionalFields # cursor valueControl) $ \optionalControl ->
  plet (expectJust $ pfinishV1 # optionalControl # total) $ \terminal ->
  pmatch valueControl $ \v -> pmatch optionalControl $ \o ->
    pfromData (pscan'address v) #== address
      #&& pfromData (pscan'lovelace o) #== 5_000_000
      #&& pfromData (pscan'cardanoValueSize o) #== 5
      #&& pfromData (pscan'assetCount o) #== 0
      #&& pterminalIsExactV1 # terminal # total

derivesValueSize :: forall s. Term s PBool
derivesValueSize =
  plet (byteLength multiAssetOutput) $ \total ->
  plet (expectJust $ pstepV1 # pinitialControlV1 # total # multiAssetOutput # 0) $ \valueControl ->
  plet (expectJust $ pstepV1 # valueControl # total # multiAssetOutput # cursor valueControl) $ \policyControl ->
  plet (expectJust $ pstepV1 # policyControl # total # multiAssetOutput # cursor policyControl) $ \firstAsset ->
  plet (expectJust $ pstepV1 # firstAsset # total # multiAssetOutput # cursor firstAsset) $ \secondAsset ->
  plet (expectJust $ pstepV1 # secondAsset # total # multiAssetOutput # cursor secondAsset) $ \optionalControl ->
  plet (expectJust $ pfinishV1 # optionalControl # total) $ \terminal -> pmatch optionalControl $ \o ->
    pfromData (pscan'cardanoValueSize o) #== 46
      #&& pfromData (pscan'assetCount o) #== 2
      #&& pterminalIsExactV1 # terminal # total

advancesLargeDatum :: forall s. Term s PBool
advancesLargeDatum =
  plet (largeDatumPrefix <> (preplicateBS # 5_000 # (pintegerToByte # 90))) $ \output ->
  plet (plengthBS # output) $ \total ->
  plet (expectJust $ pstepV1 # pinitialControlV1 # total # largeDatumPrefix # 0) $ \valueControl ->
  plet (expectJust $ pstepV1 # valueControl # total # largeDatumPrefix # cursor valueControl) $ \optionalControl ->
  plet (expectJust $ pstepV1 # optionalControl # total # largeDatumPrefix # cursor optionalControl) $ \datumControl ->
  plet (expectJust $ pstepV1 # datumControl # total # pconstant "\x00" # 0) $ \secondChunk ->
  plet (expectJust $ pstepV1 # secondChunk # total # pconstant "\x00" # 0) $ \datumComplete ->
  plet (expectJust $ pfinishV1 # datumComplete # total) $ \terminal ->
  pmatch datumControl $ \d -> pmatch secondChunk $ \s -> pmatch datumComplete $ \complete ->
    pfromData (pscan'datumOffset d) #== 45
      #&& pfromData (pscan'datumLength d) #== 5_000
      #&& pfromData (pscan'cursor s) #== 4_095
      #&& pfromData (pscan'payloadRemaining s) #== 950
      #&& pfromData (pscan'cursor complete) #== total
      #&& pterminalIsExactV1 # terminal # total

rejectsTrailingBytes :: forall s. Term s PBool
rejectsTrailingBytes =
  plet (byteLength outputWithoutOptionalFields + 1) $ \total ->
  plet (expectJust $ pstepV1 # pinitialControlV1 # total # outputWithoutOptionalFields # 0) $ \valueControl ->
  plet (expectJust $ pstepV1 # valueControl # total # outputWithoutOptionalFields # cursor valueControl) $ \optionalControl ->
    pfinishV1 # optionalControl # total #== pcon PNothing

decodesTerminalControl :: forall s. Term s PBool
decodesTerminalControl = plet (pdecodeControlV1 # terminalControlCbor) $ \control -> pmatch control $ \c ->
  pfromData (pscan'stage c) #== pstageTerminal
    #&& pfromData (pscan'cursor c) #== 11_249
    #&& pfromData (pscan'mapEntryCount c) #== 4
    #&& pfromData (pscan'optionalFieldCount c) #== 2
    #&& pfromData (pscan'lovelace c) #== 8_000_000
    #&& pfromData (pscan'cardanoValueSize c) #== 46
    #&& pfromData (pscan'assetCount c) #== 2
    #&& pfromData (pscan'datumOffset c) #== 84
    #&& pfromData (pscan'datumLength c) #== 5_159
    #&& pfromData (pscan'referenceScriptLanguage c) #== 3
    #&& pfromData (pscan'referenceScriptItemOffset c) #== 5_244
    #&& pfromData (pscan'referenceScriptOffset c) #== 5_249
    #&& pfromData (pscan'referenceScriptLength c) #== 6_000
    #&& pencodeControlV1 # control #== terminalControlCbor

rejectsMalformed :: forall s. Term s PBool
rejectsMalformed =
  pstepV1 # pinitialControlV1 # byteLength nonMinimalOutputMap # nonMinimalOutputMap # 0 #== pcon PNothing
    #&& pstepV1 # malformedControl # byteLength outputWithoutOptionalFields # outputWithoutOptionalFields # 0 #== pcon PNothing

rejectsEmptyDatum :: forall s. Term s PBool
rejectsEmptyDatum =
  plet (byteLength emptyInlineDatum) $ \total ->
  plet (expectJust $ pstepV1 # pinitialControlV1 # total # emptyInlineDatum # 0) $ \valueControl ->
  plet (expectJust $ pstepV1 # valueControl # total # emptyInlineDatum # cursor valueControl) $ \optionalControl ->
    pstepV1 # optionalControl # total # emptyInlineDatum # cursor optionalControl #== pcon PNothing

malformedControl :: forall s. Term s PLedgerOutputScanControlV1
malformedControl = pcon $ PLedgerOutputScanControlV1
  (pdata pversion) (pdata pstageRequiredFields) (pdata 0) (pdata 0) (pdata 0)
  (pdata $ pconstant "") (pdata 0) (pdata 0) (pdata 0) (pdata 0) (pdata 0)
  (pdata $ pconstant "") (pdata $ pconstant "") (pdata $ pconstant "")
  (pdata 1) (pdata pemptyFrontier) (pdata $ -1) (pdata 0) (pdata 0)
  (pdata $ -1) (pdata $ -1) (pdata $ -1) (pdata 0)

cursor :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PInteger
cursor control = pmatch control $ \c -> pfromData $ pscan'cursor c

expectJust :: forall s a. Term s (PMaybe a) -> Term s a
expectJust value = pmatch value $ \case PNothing -> perror; PJust result -> result

byteLength :: forall s. Term s PByteString -> Term s PInteger
byteLength value = plengthBS # value

address, outputWithoutOptionalFields, nonMinimalOutputMap, emptyInlineDatum,
  multiAssetOutput, largeDatumPrefix, terminalControlCbor :: forall s. Term s PByteString
address = bytes "6011111111111111111111111111111111111111111111111111111111"
outputWithoutOptionalFields = bytes "a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"
nonMinimalOutputMap = bytes "b80200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"
emptyInlineDatum = bytes "a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a00240"
multiAssetOutput = bytes "a200581d601111111111111111111111111111111111111111111111111111111101821a007a1200a1581c55555555555555555555555555555555555555555555555555555555a241ff07420000182a"
largeDatumPrefix = bytes "a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a002591388"
terminalControlCbor = bytes "970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820fdd05992e96e478560b718d45058402827072f35e5220f396e2569800a2b76fe1854191427000319147c191481191770"

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
