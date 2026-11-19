{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxC20Identity
Description : Exact C20-6/C20-7 full-preimage identity vectors.

These are the two compact C20 identity rows from @native-tx-v1.test.ak@. The
fixtures are rebuilt from the wire format, then decoded and authenticated by
the production full-field verifier. Generic producer round trips do not pin
these exact language tags, script bytes, verification keys, or signatures.
-}
module Testing.NativeTxC20Identity (tests) where

import Data.ByteString qualified as BS
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Compact (
  pdecodeNativeTxFieldPreimageLengthsV1,
  pencodeNativeTxFieldPreimageLengthsV1,
  pencodeNativeTxWitnessSetCompact,
  pnativeTxIdForVersion,
  pnativeTxProofCommitmentV1,
 )
import Midgard.FraudProofs.NativeTx.Preimages (
  pdecodeMidgardTxAddressWitnessesPreimageCbor,
  pdecodeMidgardTxScriptWitnessesPreimageCbor,
  pencodeAddressWitnessPreimage,
  pencodeScriptWitnessPreimage,
 )
import Midgard.FraudProofs.NativeTx.Transaction (pverifyMidgardTransactionFieldPreimageV1)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddressWitness (..),
  PMidgardScriptLanguage (..),
  PMidgardVersionedScript (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxWitnessSetCompact (..),
 )
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Native Tx C20 Full Preimage Identity"
    [ testCase "v1_c20_6_full_preimage_reveals_native_and_plutus_script_witnesses" $
        passertEval $
          plet (pdecodeMidgardTxScriptWitnessesPreimageCbor # pconstant scriptPreimage) $ \decoded ->
            pand'List
              [ decoded #== scriptTerms
              , pencodeScriptWitnessPreimage # decoded #== pconstant scriptPreimage
              , verifyField 6 scriptPreimage [] scripts
              ]
    , testCase "v1_c20_7_full_preimage_reveals_exact_signer_identities" $
        passertEval $
          plet (pdecodeMidgardTxAddressWitnessesPreimageCbor # pconstant addressPreimage) $ \decoded ->
            pand'List
              [ decoded #== addressTerms
              , pencodeAddressWitnessPreimage # decoded #== pconstant addressPreimage
              , verifyField 7 addressPreimage witnesses []
              ]
    , testCase "v1_c20_6_c20_7_field_directory_and_witness_tuple_are_exact" $
        passertEval $
          pand'List
            [ pencodeNativeTxWitnessSetCompact # c20WitnessSet
                #== pconstant c20WitnessSetCbor
            , pencodeNativeTxFieldPreimageLengthsV1 # c20Lengths
                #== pconstant "\x89\x00\x00\x00\x00\x00\x00\x06\x07\x08"
            , pmatch
                ( pdecodeNativeTxFieldPreimageLengthsV1
                    # pconstant "\x89\x00\x00\x00\x00\x00\x00\x06\x07\x08"
                )
                ( \PNativeTxFieldPreimageLengthsV1
                     { plengths'scriptWitnesses
                     , plengths'addressWitnesses
                     } ->
                      plengths'scriptWitnesses #== 6
                        #&& plengths'addressWitnesses #== 7
                )
            ]
    , testCase "v1_c20_7_field_seven_substitution_binds_the_field_commitment" $
        pfails $
          pand'List
            [ verifyField 7 committedAddressPreimage [committedAddressWitness] []
            , verifyField 7 substitutedAddressPreimage [committedAddressWitness] []
            ]
    , testCase "v1_c20_6_field_six_substitution_binds_the_field_commitment" $
        pfails $
          pand'List
            [ verifyField 6 committedScriptPreimage [] [committedScript]
            , verifyField 6 substitutedScriptPreimage [] [committedScript]
            ]
    ]

verifyField ::
  forall s.
  Integer ->
  BS.ByteString ->
  [Witness] ->
  [Script] ->
  Term s PBool
verifyField fieldIndex preimage addrWits scriptWits =
  pverifyMidgardTransactionFieldPreimageV1
    # (pnativeTxIdForVersion # 1 # pconstant bodyCompactCbor)
    # ( pnativeTxProofCommitmentV1
          # pconstant compactCbor
          # pconstant witnessSetCbor
          # pconstant lengthsCbor
      )
    # pconstant compactCbor
    # pconstant witnessSetCbor
    # pconstant lengthsCbor
    # pconstant fieldIndex
    # pconstant preimage
  where
    addrPreimage = encodeAddressPreimage addrWits
    scriptsPreimage = encodeScriptPreimage scriptWits
    witnessSetCbor =
      BS.concat
        [ "\x83"
        , defBytes32 (blake2b256 addrPreimage)
        , defBytes32 (blake2b256 scriptsPreimage)
        , defBytes32 emptyCommitment
        ]
    compactCbor =
      BS.concat
        [ "\x84\x01"
        , bodyCompactCbor
        , defBytes32 (blake2b256 witnessSetCbor)
        , "\x00"
        ]
    lengthsCbor =
      BS.cons 0x89 $
        BS.concat $
          map cborInt
            [ 1
            , 1
            , 1
            , 1
            , 1
            , 1
            , fromIntegral (BS.length scriptsPreimage)
            , fromIntegral (BS.length addrPreimage)
            , 1
            ]

--------------------------------------------------------------------------------
-- Exact C20 identities
--------------------------------------------------------------------------------

data ScriptLanguage = NativeCardano | PlutusV3 | MidgardV1

data Script = Script ScriptLanguage BS.ByteString

scripts :: [Script]
scripts =
  [ Script NativeCardano ("\x82\x00\x58\x1c" <> BS.replicate 28 0x55)
  , Script PlutusV3 (BS.replicate 32 0x66)
  , Script MidgardV1 (BS.replicate 24 0x77)
  ]

scriptPreimage :: BS.ByteString
scriptPreimage = encodeScriptPreimage scripts

scriptTerms :: forall s. Term s (PBuiltinList (PAsData PMidgardVersionedScript))
scriptTerms = foldr (\script rest -> pcons # scriptTerm script # rest) pnil scripts
  where
    scriptTerm (Script language bytes) =
      pdata . pcon $
        PMidgardVersionedScript
          { pversionedScript'language = pdata (languageTerm language)
          , pversionedScript'scriptBytes = pdata (pconstant bytes)
          }
    languageTerm NativeCardano = pcon PNativeCardanoScript
    languageTerm PlutusV3 = pcon PPlutusV3Script
    languageTerm MidgardV1 = pcon PMidgardV1Script

data Witness = Witness BS.ByteString BS.ByteString

witnesses :: [Witness]
witnesses =
  [ committedAddressWitness
  , substitutedAddressWitness
  ]

committedAddressWitness, substitutedAddressWitness :: Witness
committedAddressWitness = Witness (BS.replicate 32 0x11) (BS.replicate 64 0x22)
substitutedAddressWitness = Witness (BS.replicate 32 0x33) (BS.replicate 64 0x44)

committedAddressPreimage, substitutedAddressPreimage :: BS.ByteString
committedAddressPreimage = encodeAddressPreimage [committedAddressWitness]
substitutedAddressPreimage = encodeAddressPreimage [substitutedAddressWitness]

addressPreimage :: BS.ByteString
addressPreimage = encodeAddressPreimage witnesses

addressTerms :: forall s. Term s (PBuiltinList (PAsData PMidgardAddressWitness))
addressTerms = foldr (\witness rest -> pcons # witnessTerm witness # rest) pnil witnesses
  where
    witnessTerm (Witness key signature) =
      pdata . pcon $
        PMidgardAddressWitness
          { paddressWitness'verificationKey = pdata (pconstant key)
          , paddressWitness'signature = pdata (pconstant signature)
          }

committedScript, substitutedScript :: Script
committedScript = Script PlutusV3 (BS.replicate 97 0xab)
substitutedScript = Script PlutusV3 (BS.replicate 97 0xcd)

committedScriptPreimage, substitutedScriptPreimage :: BS.ByteString
committedScriptPreimage = encodeScriptPreimage [committedScript]
substitutedScriptPreimage = encodeScriptPreimage [substitutedScript]

-- The source test's single-item ABI fixture. The record order is address then
-- script, while the field-length wire below is script then address.
c20WitnessSet :: forall s. Term s PNativeTxWitnessSetCompact
c20WitnessSet =
  pcon $
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash = pdata (pconstant c20AddressCommitment)
      , pwitnessSetCompact'scriptTxWitsHash = pdata (pconstant c20ScriptCommitment)
      , pwitnessSetCompact'redeemerTxWitsHash = pdata (pconstant c20RedeemerHash)
      }

c20Lengths :: forall s. Term s PNativeTxFieldPreimageLengthsV1
c20Lengths =
  pcon $
    PNativeTxFieldPreimageLengthsV1
      { plengths'spendInputs = 0
      , plengths'referenceInputs = 0
      , plengths'outputs = 0
      , plengths'requiredObservers = 0
      , plengths'requiredSigners = 0
      , plengths'mint = 0
      , plengths'addressWitnesses = 7
      , plengths'scriptWitnesses = 6
      , plengths'redeemers = 8
      }

c20WitnessSetCbor :: BS.ByteString
c20WitnessSetCbor =
  "\x83"
    <> defBytes32 c20AddressCommitment
    <> defBytes32 c20ScriptCommitment
    <> defBytes32 c20RedeemerHash

c20AddressCommitment, c20ScriptCommitment, c20RedeemerHash :: BS.ByteString
c20AddressCommitment = blake2b256 ("\x81" <> definiteBytes c20AddressItem)
c20ScriptCommitment = blake2b256 ("\x81" <> definiteBytes c20ScriptItem)
c20RedeemerHash = BS.replicate 32 0x44

c20ScriptItem, c20AddressItem :: BS.ByteString
c20ScriptItem =
  "\x83\x00\x58\x1c" <> BS.replicate 28 0x11 <> "\x41\x00"
c20AddressItem =
  "\x82\x58\x20"
    <> BS.replicate 32 0x22
    <> "\x58\x40"
    <> BS.replicate 64 0x33

--------------------------------------------------------------------------------
-- Independent reference encoding
--------------------------------------------------------------------------------

encodeScriptPreimage :: [Script] -> BS.ByteString
encodeScriptPreimage values =
  arrayHeader (length values) <> BS.concat (map (definiteBytes . encodeScript) values)
  where
    encodeScript (Script language bytes) =
      "\x82" <> languageTag language <> definiteBytes bytes
    languageTag NativeCardano = "\x00"
    languageTag PlutusV3 = "\x03"
    languageTag MidgardV1 = "\x18\x80"

encodeAddressPreimage :: [Witness] -> BS.ByteString
encodeAddressPreimage values =
  arrayHeader (length values) <> BS.concat (map (definiteBytes . encodeWitness) values)
  where
    encodeWitness (Witness key signature) =
      "\x82" <> definiteBytes key <> definiteBytes signature

bodyCompactCbor :: BS.ByteString
bodyCompactCbor =
  BS.concat
    [ "\x8c"
    , defBytes32 emptyCommitment
    , defBytes32 emptyCommitment
    , defBytes32 emptyCommitment
    , "\x00\x20\x20"
    , defBytes32 emptyCommitment
    , defBytes32 emptyCommitment
    , defBytes32 emptyCommitment
    , defBytes32 (BS.replicate 32 0)
    , defBytes32 (BS.replicate 32 0)
    , "\x18\xff"
    ]

emptyCommitment :: BS.ByteString
emptyCommitment = blake2b256 "\x80"

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

arrayHeader :: Int -> BS.ByteString
arrayHeader n
  | n <= 23 = BS.singleton (fromIntegral (0x80 + n))
  | otherwise = error "arrayHeader: fixture count out of range"

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | n <= 23 = BS.cons (fromIntegral (0x40 + n)) bytes
  | n <= 0xff = BS.pack [0x58, fromIntegral n] <> bytes
  | otherwise = BS.pack [0x59, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)] <> bytes
  where
    n = BS.length bytes

defBytes32 :: BS.ByteString -> BS.ByteString
defBytes32 bytes = "\x58\x20" <> bytes

cborInt :: Integer -> BS.ByteString
cborInt n
  | n <= 23 = BS.singleton (fromIntegral n)
  | n <= 0xff = BS.pack [0x18, fromIntegral n]
  | otherwise = BS.pack [0x19, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]
