{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxComponents
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/fraud-proofs/native-tx/components.ak@.

The per-item codecs — one input, one address, one value, one output, one
witness. Everything above this layer is these functions run over a list and
hashed, so a divergence here moves every commitment above it.

The reference encoders below are written from the format rather than from the
port, and every "matches an independent encoding" case compares bytes against
them. The round trips are the weaker check and are here for the decoders'
sake, not the encoders'.

Four things these tests exist to pin.

**The output index is deliberately non-minimal.** @19 0000@, not @00@. The
minimal spelling has to /reject/, or an input item would have two encodings and
the fixed 40-byte stride the format is built around would not hold.

**Address type and payload length cross-check.** A 29-byte payload must carry a
type of 6 or 7 and a 57-byte one a type of 3 or less. Without both directions,
the same bytes read as two different addresses.

**Script language tags are 0, 3 and 128 — not the constructor indices 0, 1, 2.**
A port that wrote the constructor index would round-trip against itself and
disagree with everything else.

**A policy may appear in more than one group.** @count_policy_groups@ counts
runs, so assets @[A, B, A]@ encode as three groups, and that /is/ admitted: the
encoder and decoder agree, and the entry order is what the format commits to.
Two orderings of the same asset multiset therefore have different encodings,
which is a property of the format, not a defect in it.
-}
module Testing.NativeTxComponents (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Function (on)
import Data.List (groupBy)
import Data.Maybe (catMaybes)
import PlutusLedgerApi.V3 (Data (..))
import Test.Tasty
import Test.Tasty.HUnit

import Aiken.Cbor (pdeserialise)
import Plutarch.LedgerApi.AssocMap (PAssocMap (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Components (
  passetUnitFromPolicyAsset,
  pdecodeMidgardAddressBytes,
  pdecodeMidgardAddressWitnessCbor,
  pdecodeMidgardRedeemerWitnessAt,
  pdecodeMidgardRedeemerWitnessData,
  pdecodeMidgardTxInputCbor,
  pdecodeMidgardTxOutputCbor,
  pdecodeMidgardTxOutputData,
  pdecodeMidgardVersionedScriptAt,
  pdecodeMidgardVersionedScriptData,
  pencodeFixedOutputIndex,
  pencodeMidgardAddress,
  pencodeMidgardAddressWitness,
  pencodeMidgardRedeemerWitness,
  pencodeMidgardRedeemerWitnessData,
  pencodeMidgardTxInput,
  pencodeMidgardTxOutput,
  pencodeMidgardValue,
  pencodeMidgardVersionedScript,
  pmidgardScriptLanguageToTag,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardAddressWitness (..),
  PMidgardAssets,
  PMidgardCredential (..),
  PMidgardExecutionUnits (..),
  PMidgardRedeemerPurpose (..),
  PMidgardRedeemerWitness (..),
  PMidgardScriptLanguage (..),
  PMidgardTxInput (..),
  PMidgardTxOutput (..),
  PMidgardValue (..),
  PMidgardVersionedScript (..),
 )
import Midgard.LedgerOutput (pdecodeCanonicalOutput)
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Native Tx Components Tests"
    [ inputTests
    , addressTests
    , valueTests
    , scriptTests
    , outputTests
    , q25NegativeOutputValueTests
    , witnessTests
    ]

--------------------------------------------------------------------------------
-- Inputs
--------------------------------------------------------------------------------

inputTests :: TestTree
inputTests =
  testGroup
    "inputs"
    [ -- The whole point of the fixed form: index 0 is three bytes, not one.
      testCase "the output index is always the fixed three-byte form" $
        holds $
          pall'
            [ (pencodeFixedOutputIndex # 0) #== pconstant "\x19\x00\x00"
            , (pencodeFixedOutputIndex # 23) #== pconstant "\x19\x00\x17"
            , (pencodeFixedOutputIndex # 256) #== pconstant "\x19\x01\x00"
            , (pencodeFixedOutputIndex # 65535) #== pconstant "\x19\xff\xff"
            ]
    , testCase "the input matches an independent encoding" $
        holds $ (pencodeMidgardTxInput # inputT defaultInput) #== pconstant (encodeInput defaultInput)
    , testCase "the input round-trips" $
        holds $
          (pdecodeMidgardTxInputCbor # pconstant (encodeInput defaultInput)) #== inputT defaultInput
    , testCase "an index at each header boundary round-trips" $
        holds $
          pall'
            [ (pdecodeMidgardTxInputCbor # pconstant (encodeInput i)) #== inputT i
            | n <- [0, 1, 23, 24, 255, 256, 65535]
            , let i = defaultInput {iIndex = n}
            ]
    , -- Rejecting the minimal form is what keeps the encoding unique.
      testCase "the decoder rejects a minimally encoded output index" $
        pfails $ pdecodeMidgardTxInputCbor # pconstant ("\x82" <> definiteBytes (bytes32 1) <> "\x00")
    , testCase "the decoder rejects the one-byte 18 XX form" $
        pfails $ pdecodeMidgardTxInputCbor # pconstant ("\x82" <> definiteBytes (bytes32 1) <> "\x18\x20")
    , testCase "the decoder rejects the four-byte form" $
        pfails $
          pdecodeMidgardTxInputCbor
            # pconstant ("\x82" <> definiteBytes (bytes32 1) <> "\x1a\x00\x00\x00\x01")
    , testCase "the encoder rejects an output index above 65535" $
        pfails $ pencodeMidgardTxInput # inputT defaultInput {iIndex = 65536}
    , testCase "the encoder rejects a negative output index" $
        pfails $ pencodeMidgardTxInput # inputT defaultInput {iIndex = -1}
    , testCase "the encoder rejects a transaction id that is not 32 bytes" $
        pfails $ pencodeMidgardTxInput # inputT defaultInput {iTxId = BS.replicate 31 0x01}
    , testCase "the decoder rejects a transaction id that is not 32 bytes" $
        pfails $
          pdecodeMidgardTxInputCbor
            # pconstant ("\x82" <> definiteBytes (BS.replicate 31 0x01) <> "\x19\x00\x00")
    , testCase "the decoder rejects trailing bytes" $
        pfails $ pdecodeMidgardTxInputCbor # pconstant (encodeInput defaultInput <> "\x00")
    ]

--------------------------------------------------------------------------------
-- Addresses
--------------------------------------------------------------------------------

addressTests :: TestTree
addressTests =
  testGroup
    "addresses"
    [ testCase "every address shape matches an independent encoding" $
        holds $
          pall'
            [ (pencodeMidgardAddress # addrT a) #== pconstant (encodeAddress a)
            | a <- allAddresses
            ]
    , -- The header's high nibble is the type and the low one the network id
      -- with the protected bit on top.
      testCase "the header nibbles are type, then network with the protected bit" $
        holds $
          pall'
            [ (pencodeMidgardAddress # addrT a) #== pconstant (BS.pack [h] <> body)
            | (a, h, body) <-
                [ (Addr False 0 (PubKey h28a) (Just (PubKey h28b)), 0x00, h28a <> h28b)
                , (Addr False 1 (Script h28a) (Just (PubKey h28b)), 0x11, h28a <> h28b)
                , (Addr False 0 (PubKey h28a) (Just (Script h28b)), 0x20, h28a <> h28b)
                , (Addr False 1 (Script h28a) (Just (Script h28b)), 0x31, h28a <> h28b)
                , (Addr False 0 (PubKey h28a) Nothing, 0x60, h28a)
                , (Addr False 1 (Script h28a) Nothing, 0x71, h28a)
                , (Addr True 0 (PubKey h28a) (Just (PubKey h28b)), 0x08, h28a <> h28b)
                , (Addr True 1 (Script h28a) Nothing, 0x79, h28a)
                ]
            ]
    , testCase "every address shape round-trips" $
        holds $
          pall'
            [ (pdecodeMidgardAddressBytes # pconstant (encodeAddress a)) #== addrT a
            | a <- allAddresses
            ]
    , testCase "the encoder rejects a network id that is not 0 or 1" $
        pfails $ pencodeMidgardAddress # addrT (Addr False 2 (PubKey h28a) Nothing)
    , testCase "the decoder rejects a network id that is not 0 or 1" $
        pfails $ pdecodeMidgardAddressBytes # pconstant (BS.pack [0x62] <> h28a)
    , testCase "the encoder rejects a credential hash that is not 28 bytes" $
        pfails $ pencodeMidgardAddress # addrT (Addr False 0 (PubKey (BS.replicate 27 0x01)) Nothing)
    , -- Both directions of the length/type cross-check.
      testCase "the decoder rejects a 29-byte payload whose type expects a stake credential" $
        pfails $ pdecodeMidgardAddressBytes # pconstant (BS.pack [0x00] <> h28a)
    , testCase "the decoder rejects a 57-byte payload whose type expects none" $
        pfails $ pdecodeMidgardAddressBytes # pconstant (BS.pack [0x70] <> h28a <> h28b)
    , testCase "the decoder rejects a payload of any other length" $
        mapM_
            (\n -> pfails $ pdecodeMidgardAddressBytes # pconstant (BS.replicate n 0x00))
            [0, 1, 28, 30, 56, 58]
    , testCase "the protected bit survives the round trip on its own" $
        holds $
          pnot
            #$ (pencodeMidgardAddress # addrT (Addr True 0 (PubKey h28a) Nothing))
            #== (pencodeMidgardAddress # addrT (Addr False 0 (PubKey h28a) Nothing))
    ]

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

valueTests :: TestTree
valueTests =
  testGroup
    "values"
    [ testCase "every value matches an independent encoding" $
        holds $
          pall'
            [ (pencodeMidgardValue # valueT v) #== pconstant (encodeValue v)
            | v <- allValues
            ]
    , -- Adjacent entries under one policy become one group, so the map header
      -- counts policies and not assets.
      testCase "adjacent entries under one policy become a single group" $
        holds $
          (pencodeMidgardValue # valueT twoTokensOnePolicy)
            #== pconstant
              ( "\x82"
                  <> cborInt 7
                  <> mapHeader 1
                  <> definiteBytes pidA
                  <> mapHeader 2
                  <> (definiteBytes "\x01" <> cborInt 5)
                  <> (definiteBytes "\x02" <> cborInt 6)
              )
    , -- Runs, not distinct policies: A B A is three groups, and it round-trips.
      testCase "a policy split by another policy encodes as two groups" $
        holds $
          (pencodeMidgardValue # valueT splitPolicy)
            #== pconstant
              ( "\x82"
                  <> cborInt 1
                  <> mapHeader 3
                  <> (definiteBytes pidA <> mapHeader 1 <> definiteBytes "\x01" <> cborInt 5)
                  <> (definiteBytes pidB <> mapHeader 1 <> definiteBytes "\x02" <> cborInt 6)
                  <> (definiteBytes pidA <> mapHeader 1 <> definiteBytes "\x03" <> cborInt 7)
              )
    , testCase "an empty asset list encodes as an empty map" $
        holds $
          (pencodeMidgardValue # valueT (Val 42 [])) #== pconstant ("\x82" <> cborInt 42 <> mapHeader 0)
    , testCase "every value round-trips through the Data decoder" $
        holds $
          pall'
            [ (pdecodeMidgardTxOutputData # pconstant (outputData (outOf v))) #== outT (outOf v)
            | v <- allValues
            , let outOf x = Out defaultAddr x Nothing Nothing
            ]
    , testCase "the encoder rejects a negative lovelace" $
        pfails $ pencodeMidgardValue # valueT (Val (-1) [])
    , testCase "the encoder rejects a zero quantity" $
        pfails $ pencodeMidgardValue # valueT (Val 1 [(pidA <> "\x01", 0)])
    , testCase "the encoder rejects a negative quantity" $
        pfails $ pencodeMidgardValue # valueT (Val 1 [(pidA <> "\x01", -1)])
    , testCase "the encoder rejects a unit shorter than a policy id" $
        pfails $ pencodeMidgardValue # valueT (Val 1 [(BS.replicate 27 0x01, 1)])
    , testCase "the encoder rejects a unit whose asset name is over 32 bytes" $
        pfails $ pencodeMidgardValue # valueT (Val 1 [(pidA <> BS.replicate 33 0x01, 1)])
    , testCase "the Data decoder rejects a policy with no tokens" $
        pfails $
          pdecodeMidgardTxOutputData
            # pconstant (outputDataWith (List [I 1, Map [(B pidA, Map [])]]))
    , testCase "the Data decoder rejects a zero quantity" $
        pfails $
          pdecodeMidgardTxOutputData
            # pconstant (outputDataWith (List [I 1, Map [(B pidA, Map [(B "\x01", I 0)])]]))
    , testCase "the Data decoder rejects a policy id that is not 28 bytes" $
        pfails $
          pdecodeMidgardTxOutputData
            # pconstant
              (outputDataWith (List [I 1, Map [(B (BS.replicate 27 0x01), Map [(B "\x01", I 1)])]]))
    , testCase "the Data decoder rejects a negative lovelace" $
        pfails $ pdecodeMidgardTxOutputData # pconstant (outputDataWith (List [I (-1), Map []]))
    , testCase "the unit builder concatenates a policy id and an asset name" $
        holds $
          (passetUnitFromPolicyAsset # pconstant pidA # pconstant "\x01") #== pconstant (pidA <> "\x01")
    , testCase "the unit builder rejects a policy id that is not 28 bytes" $
        pfails $ passetUnitFromPolicyAsset # pconstant (BS.replicate 27 0x01) # pconstant "\x01"
    , testCase "the unit builder rejects an asset name over 32 bytes" $
        pfails $ passetUnitFromPolicyAsset # pconstant pidA # pconstant (BS.replicate 33 0x01)
    ]

--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------

scriptTests :: TestTree
scriptTests =
  testGroup
    "scripts"
    [ -- 0, 3, 128 — not the constructor indices 0, 1, 2.
      testCase "the language tags are 0, 3 and 128" $
        holds $
          pall'
            [ (pmidgardScriptLanguageToTag # pcon PNativeCardanoScript) #== 0
            , (pmidgardScriptLanguageToTag # pcon PPlutusV3Script) #== 3
            , (pmidgardScriptLanguageToTag # pcon PMidgardV1Script) #== 128
            ]
    , testCase "every language matches an independent encoding" $
        holds $
          pall'
            [ (pencodeMidgardVersionedScript # scriptT s) #== pconstant (encodeScript s)
            | s <- allScripts
            ]
    , -- 128 needs the two-byte form; 0 and 3 do not.
      testCase "the Midgard language takes the two-byte 18 80 form" $
        holds $
          (pencodeMidgardVersionedScript # scriptT (Scr MidgardV1 "\xde\xad"))
            #== pconstant ("\x82\x18\x80" <> definiteBytes "\xde\xad")
    , testCase "every language round-trips at the byte decoder" $
        holds $
          pall'
            [ psndOf (pdecodeMidgardVersionedScriptAt # pconstant (encodeScript s) # 0) #== scriptT s
            | s <- allScripts
            ]
    , testCase "the byte decoder reports the offset past the script" $
        holds $
          pall'
            [ pfstOf (pdecodeMidgardVersionedScriptAt # pconstant (encodeScript s) # 0)
              #== pconstant (fromIntegral (BS.length (encodeScript s)))
            | s <- allScripts
            ]
    , testCase "every language round-trips at the Data decoder" $
        holds $
          pall'
            [ (pdecodeMidgardVersionedScriptData # pconstant (scriptData s)) #== scriptT s
            | s <- allScripts
            ]
    , testCase "the byte decoder rejects an unknown tag" $
        mapM_
            ( \tag ->
                pfails $
                  psndOf (pdecodeMidgardVersionedScriptAt # pconstant ("\x82" <> tag <> definiteBytes "\x01") # 0)
            )
            ["\x01", "\x02", "\x04", "\x17", "\x18\x7f", "\x18\xff"]
    , testCase "the Data decoder rejects an unknown tag" $
        pfails $ pdecodeMidgardVersionedScriptData # pconstant (List [I 1, B "\x01"])
    , testCase "the Data decoder rejects a wrong arity" $
        pfails $ pdecodeMidgardVersionedScriptData # pconstant (List [I 0, B "\x01", B "\x02"])
    ]

--------------------------------------------------------------------------------
-- Outputs
--------------------------------------------------------------------------------

outputTests :: TestTree
outputTests =
  testGroup
    "outputs"
    [ testCase "every output shape matches an independent encoding" $
        holds $
          pall'
            [ (pencodeMidgardTxOutput # outT o) #== pconstant (encodeOutput o)
            | o <- allOutputs
            ]
    , -- The optional entries are absent rather than null, so the header is
      -- a2, a3 or a4 and the keys stay ascending.
      testCase "the map header counts only the entries that are present" $
        holds $
          pall'
            [ (pencodeMidgardTxOutput # outT o) #== pconstant (BS.pack [tag] <> BS.drop 1 (encodeOutput o))
            | (o, tag) <-
                [ (baseOutput, 0xa2)
                , (baseOutput {oDatum = Just "\x01"}, 0xa3)
                , (baseOutput {oScript = Just (Scr PlutusV3 "\x02")}, 0xa3)
                , (baseOutput {oDatum = Just "\x01", oScript = Just (Scr PlutusV3 "\x02")}, 0xa4)
                ]
            ]
    , testCase "every output shape round-trips at the byte decoder" $
        holds $
          pall'
            [ (pdecodeMidgardTxOutputCbor # pconstant (encodeOutput o)) #== outT o
            | o <- allOutputs
            ]
    , testCase "every output shape round-trips at the Data decoder" $
        holds $
          pall'
            [ (pdecodeMidgardTxOutputData # pconstant (outputData o)) #== outT o
            | o <- allOutputs
            ]
    , testCase "the byte decoder rejects trailing bytes" $
        mapM_
            (\o -> pfails $ pdecodeMidgardTxOutputCbor # pconstant (encodeOutput o <> "\x00"))
            allOutputs
    , testCase "the byte decoder rejects a map header outside a2..a4" $
        mapM_
            ( \tag ->
                pfails $
                  pdecodeMidgardTxOutputCbor # pconstant (BS.pack [tag] <> BS.drop 1 (encodeOutput baseOutput))
            )
            [0xa1, 0xa5, 0xb8]
    , testCase "the byte decoder rejects a wrong first key" $
        pfails $
          pdecodeMidgardTxOutputCbor
            # pconstant ("\xa2\x04" <> BS.drop 2 (encodeOutput baseOutput))
    , testCase "the byte decoder rejects a three-entry map with an unknown extra key" $
        pfails $
          pdecodeMidgardTxOutputCbor
            # pconstant
              ( "\xa3"
                  <> BS.drop 1 (encodeOutput baseOutput)
                  <> "\x04"
                  <> definiteBytes "\x01"
              )
    , testCase "the byte decoder rejects a four-entry map that omits the datum" $
        pfails $
          pdecodeMidgardTxOutputCbor
            # pconstant
              ( "\xa4"
                  <> BS.drop 1 (encodeOutput baseOutput)
                  <> "\x03"
                  <> encodeScript (Scr PlutusV3 "\x02")
                  <> "\x03"
                  <> encodeScript (Scr PlutusV3 "\x02")
              )
    , testCase "the Data decoder rejects a three-entry map with an unknown extra key" $
        pfails $
          pdecodeMidgardTxOutputData
            # pconstant
              ( Map
                  [ (I 0, B (encodeAddress defaultAddr))
                  , (I 1, valueData defaultValue)
                  , (I 4, B "\x01")
                  ]
              )
    , testCase "the Data decoder rejects a four-entry map with the last two keys swapped" $
        pfails $
          pdecodeMidgardTxOutputData
            # pconstant
              ( Map
                  [ (I 0, B (encodeAddress defaultAddr))
                  , (I 1, valueData defaultValue)
                  , (I 3, scriptData (Scr PlutusV3 "\x02"))
                  , (I 2, B "\x01")
                  ]
              )
    , testCase "the Data decoder rejects a five-entry map" $
        pfails $
          pdecodeMidgardTxOutputData
            # pconstant
              ( Map
                  [ (I 0, B (encodeAddress defaultAddr))
                  , (I 1, valueData defaultValue)
                  , (I 2, B "\x01")
                  , (I 3, scriptData (Scr PlutusV3 "\x02"))
                  , (I 4, B "\x02")
                  ]
              )
    , testCase "the Data decoder rejects a one-entry map" $
        pfails $ pdecodeMidgardTxOutputData # pconstant (Map [(I 0, B (encodeAddress defaultAddr))])
    ]

--------------------------------------------------------------------------------
-- structural-na-q25-negative-output-value.test.ak
--------------------------------------------------------------------------------

q25NegativeOutputValueTests :: TestTree
q25NegativeOutputValueTests =
  testGroup
    "Q25 Negative Output Value Aiken Parity"
    [ testCase "q25_negative_lovelace_rejected_by_raw_cbor_output_decoder" $
        pfails $ q25RawOutputLovelace q25NegativeLovelaceOutput #< 0
    , testCase "q25_negative_lovelace_rejected_by_data_output_decoder" $
        pfails $ q25DataOutputLovelace q25NegativeLovelaceOutput #< 0
    , testCase "q25_negative_lovelace_rejected_by_ledger_output_v1" $
        holds $
          pdecodeCanonicalOutput # pconstant q25NegativeLovelaceOutput
            #== pcon PNothing
    , testCase "q25_negative_lovelace_rejected_by_canonical_value_encoder" $
        pfails $
          pnot # (pencodeMidgardValue # valueT (Val (-1) []) #== pconstant "")
    , testCase "q25_zero_asset_quantity_rejected_by_raw_cbor_output_decoder" $
        pfails $ q25RawOutputLovelace q25ZeroQuantityOutput #== 0
    , testCase "q25_zero_asset_quantity_rejected_by_data_output_decoder" $
        pfails $ q25DataOutputLovelace q25ZeroQuantityOutput #== 0
    , testCase "q25_negative_asset_quantity_rejected_by_raw_cbor_output_decoder" $
        pfails $ q25RawOutputLovelace q25NegativeQuantityOutput #== 0
    , testCase "q25_negative_asset_quantity_rejected_by_data_output_decoder" $
        pfails $ q25DataOutputLovelace q25NegativeQuantityOutput #== 0
    , testCase "q25_nonpositive_asset_quantities_rejected_by_ledger_output_v1" $
        holds $
          pdecodeCanonicalOutput # pconstant q25ZeroQuantityOutput #== pcon PNothing
            #&& pdecodeCanonicalOutput # pconstant q25NegativeQuantityOutput #== pcon PNothing
    , testCase "q25_negative_asset_quantity_rejected_by_canonical_value_encoder" $
        pfails $
          pnot
            # ( pencodeMidgardValue
                  # valueT (Val 0 [(q25PolicyId <> "\x01", -1)])
                  #== pconstant ""
              )
    , testCase "q25_positive_control_zero_lovelace_output_decodes" $
        holds $
          q25PositiveOutputDecodes
            q25ZeroLovelaceOutput
            (Out q25Address (Val 0 []) Nothing Nothing)
    , testCase "q25_positive_control_positive_asset_quantity_output_decodes" $
        holds $
          q25PositiveOutputDecodes
            q25PositiveQuantityOutput
            (Out q25Address (Val 0 [(q25PolicyId <> "\x01", 1)]) Nothing Nothing)
    , testCase "q25_positive_control_reencodes_to_the_same_bytes" $
        holds q25PositiveValueReencodes
    ]

q25RawOutputLovelace :: forall s. BS.ByteString -> Term s PInteger
q25RawOutputLovelace outputCbor =
  q25OutputLovelace (pdecodeMidgardTxOutputCbor # pconstant outputCbor)

q25DataOutputLovelace :: forall s. BS.ByteString -> Term s PInteger
q25DataOutputLovelace outputCbor =
  q25OutputLovelace (pdecodeMidgardTxOutputData # q25AsData outputCbor)

q25OutputLovelace :: forall s. Term s PMidgardTxOutput -> Term s PInteger
q25OutputLovelace output =
  pmatch output $ \(PMidgardTxOutput _ value _ _) ->
    pmatch (pfromData value) $ \(PMidgardValue lovelace _) ->
      pfromData lovelace

q25AsData :: forall s. BS.ByteString -> Term s PData
q25AsData outputCbor =
  pmatch (pdeserialise # pconstant outputCbor) $ \case
    PNothing -> perror
    PJust dat -> dat

q25PositiveOutputDecodes :: forall s. BS.ByteString -> Out -> Term s PBool
q25PositiveOutputDecodes outputCbor expected =
  plet (pdecodeMidgardTxOutputCbor # pconstant outputCbor) $ \output ->
    pall'
      [ output #== outT expected
      , pdecodeMidgardTxOutputData # q25AsData outputCbor #== output
      , pdecodeCanonicalOutput # pconstant outputCbor #== pcon (PJust output)
      ]

q25PositiveValueReencodes :: forall s. Term s PBool
q25PositiveValueReencodes =
  pmatch (pdecodeMidgardTxOutputCbor # pconstant q25PositiveQuantityOutput) $
    \(PMidgardTxOutput _ value _ _) ->
      pencodeMidgardValue # pfromData value
        #== pconstant q25PositiveQuantityValueCbor

q25Address :: Addr
q25Address = Addr False 0 (PubKey h28a) Nothing

q25PolicyId :: BS.ByteString
q25PolicyId = BS.replicate 28 0x01

q25NegativeLovelaceOutput, q25ZeroLovelaceOutput, q25ZeroQuantityOutput, q25NegativeQuantityOutput, q25PositiveQuantityOutput, q25PositiveQuantityValueCbor :: BS.ByteString
q25NegativeLovelaceOutput = q25Hex "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018220a0"
q25ZeroLovelaceOutput = q25Hex "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0"
q25ZeroQuantityOutput = q25Hex "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c01010101010101010101010101010101010101010101010101010101a1410100"
q25NegativeQuantityOutput = q25Hex "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c01010101010101010101010101010101010101010101010101010101a1410120"
q25PositiveQuantityOutput = q25Hex "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c01010101010101010101010101010101010101010101010101010101a1410101"
q25PositiveQuantityValueCbor = q25Hex "8200a1581c01010101010101010101010101010101010101010101010101010101a1410101"

q25Hex :: BS.ByteString -> BS.ByteString
q25Hex = Base16.decodeLenient

--------------------------------------------------------------------------------
-- Witnesses
--------------------------------------------------------------------------------

witnessTests :: TestTree
witnessTests =
  testGroup
    "witnesses"
    [ testCase "the address witness matches an independent encoding" $
        holds $
          (pencodeMidgardAddressWitness # addressWitnessT defaultAw)
            #== pconstant (encodeAddressWitness defaultAw)
    , testCase "the address witness round-trips" $
        holds $
          (pdecodeMidgardAddressWitnessCbor # pconstant (encodeAddressWitness defaultAw))
            #== addressWitnessT defaultAw
    , testCase "the address witness encoder rejects a key that is not 32 bytes" $
        pfails $ pencodeMidgardAddressWitness # addressWitnessT (Aw (BS.replicate 31 0x01) sig64)
    , testCase "the address witness encoder rejects a signature that is not 64 bytes" $
        pfails $ pencodeMidgardAddressWitness # addressWitnessT (Aw key32 (BS.replicate 63 0x02))
    , testCase "the address witness decoder rejects a key that is not 32 bytes" $
        pfails $
          pdecodeMidgardAddressWitnessCbor
            # pconstant ("\x82" <> definiteBytes (BS.replicate 31 0x01) <> definiteBytes sig64)
    , testCase "the address witness decoder rejects trailing bytes" $
        pfails $
          pdecodeMidgardAddressWitnessCbor # pconstant (encodeAddressWitness defaultAw <> "\x00")
    , -- The purposes are 0..6 in declared order, and the seventh has no L1
      -- counterpart.
      testCase "every redeemer purpose matches an independent encoding" $
        holds $
          pall'
            [ (pencodeMidgardRedeemerWitness # redeemerT r) #== pconstant (encodeRedeemer r)
            | r <- allRedeemers
            ]
    , testCase "the redeemer's Data form matches an independent encoding" $
        holds $
          pall'
            [ (pencodeMidgardRedeemerWitnessData # redeemerT r) #== pconstant (redeemerData r)
            | r <- allRedeemers
            ]
    , testCase "every redeemer round-trips at the byte decoder" $
        holds $
          pall'
            [ psndOf (pdecodeMidgardRedeemerWitnessAt # pconstant (encodeRedeemer r) # 0) #== redeemerT r
            | r <- allRedeemers
            ]
    , testCase "the byte decoder reports the offset past the redeemer" $
        holds $
          pall'
            [ pfstOf (pdecodeMidgardRedeemerWitnessAt # pconstant (encodeRedeemer r) # 0)
              #== pconstant (fromIntegral (BS.length (encodeRedeemer r)))
            | r <- allRedeemers
            ]
    , -- The offset decoder is the one that reads out of a longer preimage, so
      -- it must start where it is told and stop where the item ends.
      testCase "the byte decoder reads a redeemer at a non-zero offset" $
        holds $
          psndOf
            ( pdecodeMidgardRedeemerWitnessAt
                # pconstant ("\xff\xff\xff" <> encodeRedeemer defaultRw)
                # 3
            )
            #== redeemerT defaultRw
    , testCase "every redeemer round-trips at the Data decoder" $
        holds $
          pall'
            [ (pdecodeMidgardRedeemerWitnessData # pconstant (redeemerData r)) #== redeemerT r
            | r <- allRedeemers
            ]
    , testCase "the encoder rejects a negative index" $
        pfails $ pencodeMidgardRedeemerWitness # redeemerT defaultRw {rIndex = -1}
    , testCase "the encoder rejects negative execution units" $
        mapM_
            (\r -> pfails $ pencodeMidgardRedeemerWitness # redeemerT r)
            [defaultRw {rMemory = -1}, defaultRw {rSteps = -1}]
    , testCase "the Data encoder rejects a negative index" $
        pfails $ pencodeMidgardRedeemerWitnessData # redeemerT defaultRw {rIndex = -1}
    , testCase "the byte decoder rejects an unknown purpose tag" $
        pfails $
          psndOf
            ( pdecodeMidgardRedeemerWitnessAt
                # pconstant ("\x84\x07" <> cborInt 0 <> definiteBytes "\x01" <> "\x82" <> cborInt 1 <> cborInt 2)
                # 0
            )
    , testCase "the Data decoder rejects an unknown purpose tag" $
        pfails $
          pdecodeMidgardRedeemerWitnessData
            # pconstant (List [I 7, I 0, B "\x01", List [I 1, I 2]])
    , testCase "the Data decoder rejects a wrong arity" $
        pfails $ pdecodeMidgardRedeemerWitnessData # pconstant (List [I 0, I 0, B "\x01"])
    , testCase "the Data decoder rejects wrong execution units" $
        pfails $
          pdecodeMidgardRedeemerWitnessData
            # pconstant (List [I 0, I 0, B "\x01", List [I 1, I 2, I 3]])
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

-- | A transaction input.
data Input = Input {iTxId :: BS.ByteString, iIndex :: Integer}

defaultInput :: Input
defaultInput = Input (bytes32 1) 7

-- | A Midgard credential.
data Cred = PubKey BS.ByteString | Script BS.ByteString

-- | A Midgard address.
data Addr = Addr
  { aProtected :: Bool
  , aNetwork :: Integer
  , aPayment :: Cred
  , aStake :: Maybe Cred
  }

{- | Every address shape: both credential kinds on both sides, both networks,
and the protected bit on and off.
-}
allAddresses :: [Addr]
allAddresses =
  [ Addr protected net payment stake
  | protected <- [False, True]
  , net <- [0, 1]
  , payment <- [PubKey h28a, Script h28a]
  , stake <- [Nothing, Just (PubKey h28b), Just (Script h28b)]
  ]

defaultAddr :: Addr
defaultAddr = Addr False 0 (PubKey h28a) Nothing

-- | A Midgard value: lovelace plus a flat unit-keyed asset list.
data Val = Val {vLovelace :: Integer, vAssets :: [(BS.ByteString, Integer)]}

twoTokensOnePolicy :: Val
twoTokensOnePolicy = Val 7 [(pidA <> "\x01", 5), (pidA <> "\x02", 6)]

-- | The same policy on both sides of another one — three groups, not two.
splitPolicy :: Val
splitPolicy = Val 1 [(pidA <> "\x01", 5), (pidB <> "\x02", 6), (pidA <> "\x03", 7)]

defaultValue :: Val
defaultValue = twoTokensOnePolicy

allValues :: [Val]
allValues =
  [ Val 0 []
  , Val 1000000 []
  , twoTokensOnePolicy
  , splitPolicy
  , Val 24 [(pidA, 1)] -- an empty asset name
  , Val 65536 [(pidA <> BS.replicate 32 0x07, 4294967296)]
  ]

-- | A Midgard script language.
data Lang = NativeCardano | PlutusV3 | MidgardV1

-- | A versioned script.
data Scr = Scr {sLang :: Lang, sBytes :: BS.ByteString}

allScripts :: [Scr]
allScripts =
  [ Scr NativeCardano "\x01\x02"
  , Scr PlutusV3 "\x03"
  , Scr MidgardV1 (BS.replicate 40 0x04)
  ]

-- | A transaction output.
data Out = Out
  { oAddr :: Addr
  , oValue :: Val
  , oDatum :: Maybe BS.ByteString
  , oScript :: Maybe Scr
  }

baseOutput :: Out
baseOutput = Out defaultAddr defaultValue Nothing Nothing

allOutputs :: [Out]
allOutputs =
  [ baseOutput
  , baseOutput {oDatum = Just "\x01\x02\x03"}
  , baseOutput {oScript = Just (Scr MidgardV1 "\x04\x05")}
  , baseOutput
      { oAddr = Addr True 1 (Script h28a) (Just (PubKey h28b))
      , oValue = splitPolicy
      , oDatum = Just (BS.replicate 40 0x06)
      , oScript = Just (Scr PlutusV3 "\x07")
      }
  ]

-- | An address witness.
data Aw = Aw BS.ByteString BS.ByteString

defaultAw :: Aw
defaultAw = Aw key32 sig64

-- | A redeemer witness.
data Rw = Rw
  { rPurpose :: Integer
  , rIndex :: Integer
  , rCbor :: BS.ByteString
  , rMemory :: Integer
  , rSteps :: Integer
  }

defaultRw :: Rw
defaultRw = Rw 0 3 "\x01\x02" 1000 2000000

allRedeemers :: [Rw]
allRedeemers = [defaultRw {rPurpose = p} | p <- [0 .. 6]] <> [Rw 6 0 "" 0 0]

key32 :: BS.ByteString
key32 = BS.replicate 32 0x11

sig64 :: BS.ByteString
sig64 = BS.replicate 64 0x22

h28a :: BS.ByteString
h28a = BS.replicate 28 0xaa

h28b :: BS.ByteString
h28b = BS.replicate 28 0xbb

pidA :: BS.ByteString
pidA = BS.replicate 28 0x01

pidB :: BS.ByteString
pidB = BS.replicate 28 0x02

--------------------------------------------------------------------------------
-- The independent reference encoders
--------------------------------------------------------------------------------

encodeInput :: Input -> BS.ByteString
encodeInput (Input txId index) =
  "\x82" <> definiteBytes txId <> "\x19" <> be 2 index

encodeAddress :: Addr -> BS.ByteString
encodeAddress (Addr protected net payment stake) =
  BS.pack [fromIntegral header] <> credHash payment <> maybe "" credHash stake
  where
    addressType :: Integer
    addressType = case stake of
      Just s -> (if isScript payment then 1 else 0) + (if isScript s then 2 else 0)
      Nothing -> if isScript payment then 7 else 6
    header = addressType * 16 + net + (if protected then 8 else 0)

credHash :: Cred -> BS.ByteString
credHash (PubKey h) = h
credHash (Script h) = h

isScript :: Cred -> Bool
isScript (PubKey _) = False
isScript (Script _) = True

encodeValue :: Val -> BS.ByteString
encodeValue (Val lovelace assets) =
  "\x82" <> cborInt lovelace <> encodePolicyAssets assets

{- | The grouping pass, written the other way round from the port: group the
adjacent runs first, then write them.
-}
encodePolicyAssets :: [(BS.ByteString, Integer)] -> BS.ByteString
encodePolicyAssets assets =
  mapHeader (length groups)
    <> mconcat
      [ definiteBytes policyId
        <> mapHeader (length tokens)
        <> mconcat [definiteBytes name <> cborInt quantity | (name, quantity) <- tokens]
      | (policyId, tokens) <- groups
      ]
  where
    groups =
      [ (BS.take 28 (fst (head run)), [(BS.drop 28 unit, quantity) | (unit, quantity) <- run])
      | run <- groupBy ((==) `on` (BS.take 28 . fst)) assets
      ]

encodeScript :: Scr -> BS.ByteString
encodeScript (Scr lang bytes) = "\x82" <> cborInt (langTag lang) <> definiteBytes bytes

langTag :: Lang -> Integer
langTag NativeCardano = 0
langTag PlutusV3 = 3
langTag MidgardV1 = 128

encodeOutput :: Out -> BS.ByteString
encodeOutput (Out addr value datum scriptRef) =
  BS.pack [fromIntegral (0xa0 + entryCount)] <> required <> extras
  where
    required =
      "\x00" <> definiteBytes (encodeAddress addr) <> "\x01" <> encodeValue value
    extras =
      mconcat . catMaybes $
        [ ("\x02" <>) . definiteBytes <$> datum
        , ("\x03" <>) . encodeScript <$> scriptRef
        ]
    entryCount =
      2 + length (catMaybes [() <$ datum, () <$ scriptRef]) :: Int

encodeAddressWitness :: Aw -> BS.ByteString
encodeAddressWitness (Aw key signature) =
  "\x82" <> definiteBytes key <> definiteBytes signature

encodeRedeemer :: Rw -> BS.ByteString
encodeRedeemer (Rw purpose index cbor memory steps) =
  "\x84"
    <> cborInt purpose
    <> cborInt index
    <> definiteBytes cbor
    <> "\x82"
    <> cborInt memory
    <> cborInt steps

--------------------------------------------------------------------------------
-- The independent reference Data forms
--------------------------------------------------------------------------------

valueData :: Val -> Data
valueData (Val lovelace assets) =
  List
    [ I lovelace
    , Map
        [ (B policyId, Map [(B name, I quantity) | (name, quantity) <- tokens])
        | run <- groupBy ((==) `on` (BS.take 28 . fst)) assets
        , let policyId = BS.take 28 (fst (head run))
        , let tokens = [(BS.drop 28 unit, quantity) | (unit, quantity) <- run]
        ]
    ]

scriptData :: Scr -> Data
scriptData (Scr lang bytes) = List [I (langTag lang), B bytes]

outputData :: Out -> Data
outputData (Out addr value datum scriptRef) =
  Map $
    [(I 0, B (encodeAddress addr)), (I 1, valueData value)]
      <> catMaybes
        [ (\d -> (I 2, B d)) <$> datum
        , (\s -> (I 3, scriptData s)) <$> scriptRef
        ]

-- | An output whose value slot carries arbitrary 'Data', for the reject cases.
outputDataWith :: Data -> Data
outputDataWith value = Map [(I 0, B (encodeAddress defaultAddr)), (I 1, value)]

redeemerData :: Rw -> Data
redeemerData (Rw purpose index cbor memory steps) =
  List [I purpose, I index, B cbor, List [I memory, I steps]]

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

mapHeader :: Int -> BS.ByteString
mapHeader n
  | n <= 23 = BS.pack [fromIntegral (160 + n)]
  | n <= 255 = BS.pack [0xb8, fromIntegral n]
  | otherwise = error "reference mapHeader: out of fixture range"

be :: Int -> Integer -> BS.ByteString
be width n =
  BS.pack [fromIntegral (n `div` (256 ^ i) `mod` 256) | i <- reverse [0 .. width - 1]]

bytes32 :: Int -> BS.ByteString
bytes32 n = BS.replicate 32 (fromIntegral n)

--------------------------------------------------------------------------------
-- Building the Plutarch values
--------------------------------------------------------------------------------

inputT :: forall s. Input -> Term s PMidgardTxInput
inputT (Input txId index) =
  pcon $
    PMidgardTxInput
      { ptxInput'txId = pdata (pconstant txId)
      , ptxInput'outputIndex = pdata (pconstant index)
      }

credT :: forall s. Cred -> Term s PMidgardCredential
credT (PubKey h) = pcon (PMidgardPubKeyCredential (pdata (pconstant h)))
credT (Script h) = pcon (PMidgardScriptCredential (pdata (pconstant h)))

addrT :: forall s. Addr -> Term s PMidgardAddress
addrT (Addr protected net payment stake) =
  pcon $
    PMidgardAddress
      { paddress'protected = pdata (pconstant protected)
      , paddress'networkId = pdata (pconstant net)
      , paddress'paymentCredential = pdata (credT payment)
      , paddress'stakeCredential = pdata (maybeD (credT <$> stake))
      }

valueT :: forall s. Val -> Term s PMidgardValue
valueT (Val lovelace assets) =
  pcon $
    PMidgardValue
      { pvalue'lovelace = pdata (pconstant lovelace)
      , pvalue'assets = pdata (assetsT assets)
      }

assetsT :: forall s. [(BS.ByteString, Integer)] -> Term s PMidgardAssets
assetsT =
  pcon
    . PAssocMap
    . foldr
      (\(unit, quantity) acc -> pcons # (ppairDataBuiltin # pdata (pconstant unit) # pdata (pconstant quantity)) # acc)
      (pcon PNil)

langT :: forall s. Lang -> Term s PMidgardScriptLanguage
langT NativeCardano = pcon PNativeCardanoScript
langT PlutusV3 = pcon PPlutusV3Script
langT MidgardV1 = pcon PMidgardV1Script

scriptT :: forall s. Scr -> Term s PMidgardVersionedScript
scriptT (Scr lang bytes) =
  pcon $
    PMidgardVersionedScript
      { pversionedScript'language = pdata (langT lang)
      , pversionedScript'scriptBytes = pdata (pconstant bytes)
      }

outT :: forall s. Out -> Term s PMidgardTxOutput
outT (Out addr value datum scriptRef) =
  pcon $
    PMidgardTxOutput
      { ptxOutput'address = pdata (addrT addr)
      , ptxOutput'value = pdata (valueT value)
      , ptxOutput'datumCbor = pdata (maybeD (pconstant <$> datum))
      , ptxOutput'scriptRef = pdata (maybeD (scriptT <$> scriptRef))
      }

addressWitnessT :: forall s. Aw -> Term s PMidgardAddressWitness
addressWitnessT (Aw key signature) =
  pcon $
    PMidgardAddressWitness
      { paddressWitness'verificationKey = pdata (pconstant key)
      , paddressWitness'signature = pdata (pconstant signature)
      }

purposeT :: forall s. Integer -> Term s PMidgardRedeemerPurpose
purposeT 0 = pcon PSpendRedeemer
purposeT 1 = pcon PMintRedeemer
purposeT 2 = pcon PCertRedeemer
purposeT 3 = pcon PRewardRedeemer
purposeT 4 = pcon PVoteRedeemer
purposeT 5 = pcon PProposeRedeemer
purposeT 6 = pcon PReceiveRedeemer
purposeT n = error ("no such redeemer purpose: " <> show n)

redeemerT :: forall s. Rw -> Term s PMidgardRedeemerWitness
redeemerT (Rw purpose index cbor memory steps) =
  pcon $
    PMidgardRedeemerWitness
      { predeemerWitness'purpose = pdata (purposeT purpose)
      , predeemerWitness'index = pdata (pconstant index)
      , predeemerWitness'redeemerCbor = pdata (pconstant cbor)
      , predeemerWitness'executionUnits =
          pdata $
            pcon
              ( PMidgardExecutionUnits
                  { pexecutionUnits'memory = pdata (pconstant memory)
                  , pexecutionUnits'steps = pdata (pconstant steps)
                  }
              )
      }

maybeD :: forall s a. PIsData a => Maybe (Term s a) -> Term s (PMaybeData a)
maybeD = maybe (pcon PDNothing) (pcon . PDJust . pdata)

--------------------------------------------------------------------------------
-- Assertion helpers
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

pall' :: forall s. [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

pfstOf :: forall s a b. Term s (PPair a b) -> Term s a
pfstOf p = pmatch p $ \(PPair a _) -> a

psndOf :: forall s a b. Term s (PPair a b) -> Term s b
psndOf p = pmatch p $ \(PPair _ b) -> b
