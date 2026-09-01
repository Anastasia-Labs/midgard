{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxC20Field6Maximum
Description : Exact C20 field-6 Cardano boundary fixture parity.

The TypeScript fixture predates the uniform section 5.1 item envelope: its
field-6 preimage contains raw 44-byte versioned-script CBOR values. Current
Aiken and Plutarch production code wrap every item as a definite byte string,
so the old positive Aiken test is red (the exact Aiken selector reports
@expect tag == 90@). These tests pin both byte forms and make that production
rejection explicit without restoring the retired wire format.
-}
module Testing.NativeTxC20Field6Maximum (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import Data.List (sortOn)
import Data.Word (Word8)
import Plutarch.Prelude
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import PlutusTx.Builtins qualified as Builtins
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.NativeTx.Transaction (
  pverifyMidgardTransactionFieldPreimageV1,
 )
import Midgard.FraudProofs.NativeTx.Preimages (
  pdecodeMidgardTxScriptWitnessesPreimageCbor,
 )
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
  testGroup
    "Native Tx C20 Field 6 Maximum"
    [ testCase "v1_c20_6_accepted_cardano_maximum_matches_typescript_boundary_fixture (pinned stale bytes)" $ do
        length maximumScripts @?= 224
        assertBool "every native script is exactly 40 bytes" $
          all ((== 40) . BS.length) maximumScripts
        assertBool "every versioned-script item is exactly 44 bytes" $
          all ((== 44) . BS.length . versionedScriptItem) maximumScripts
        BS.length staleRawPreimage @?= 9_858
        blake2b256 staleRawPreimage
          @?= hex "149831e894a70eabc4f171af54b47bb7f67e1530f782272a610d874a04f3b406"
        BS.length canonicalEnvelopedPreimage @?= 10_306
        blake2b256 canonicalEnvelopedPreimage
          @?= hex "39728233c559f2fdc7876f58aad839f313ea4af375a4e91d60e63e1a55adbe2e"
    , testCase "v1_c20_6_accepted_cardano_maximum_matches_typescript_boundary_fixture production rejects the retired raw envelope" $
        pfails (verifyAtField 6)
    , testCase "v1_c20_6_maximum_fixture_script_preimage_rejects_at_observer_position" $
        pfails (verifyAtField 3)
    , testCase "v1_c20_6_adjacent_script_count_exceeds_the_cardano_envelope (stale field-size pins)" $ do
        let accepted = syntheticEnvelopedPreimage 224
            adjacent = syntheticEnvelopedPreimage 225
        -- The exact Aiken selector reports False, False, True, True. The two
        -- old sizes omit one definite-bytes wrapper per script; the signed
        -- Cardano boundary itself remains exact.
        BS.length accepted @?= 10_306
        BS.length adjacent @?= 10_352
        assertBool "the accepted raw-era field-size pin is stale" $
          BS.length accepted /= 9_858
        assertBool "the adjacent raw-era field-size pin is stale" $
          BS.length adjacent /= 9_858 + 44
        assertBool "the accepted signed transaction fits Cardano" $
          (16_338 :: Int) <= 16_384
        assertBool "the adjacent signed transaction exceeds Cardano" $
          (16_410 :: Int) > 16_384
    , testCase "C20 field-6 mutation source accepts the committed script order" $
        passertEvalNoTrace (verifyC20ScriptPreimage honestC20ScriptPreimage)
    , testCase "v1_c20_6_duplicate_script_witness_rejects" $
        pfails (verifyC20ScriptPreimage duplicateC20ScriptPreimage)
    , testCase "v1_c20_6_reordered_script_witnesses_reject" $
        pfails (verifyC20ScriptPreimage reorderedC20ScriptPreimage)
    , testCase "v1_c20_6_omitted_script_witness_rejects" $
        pfails (verifyC20ScriptPreimage omittedC20ScriptPreimage)
    , testCase "v1_c20_6_script_witness_arity_one_rejects" $
        pfails $
          pdecodeMidgardTxScriptWitnessesPreimageCbor # pconstant "\x81\x81\x00"
    , testCase "v1_c20_6_script_witness_arity_three_rejects" $
        pfails $
          pdecodeMidgardTxScriptWitnessesPreimageCbor # pconstant "\x81\x83\x03\x41\xaa\x40"
    , -- Tags 1, 4, and 127 are already in NativeTxComponents' unknown-tag
      -- table. Tag 129 is the uncovered boundary immediately above Midgard's
      -- valid two-byte tag 128.
      testCase "v1_c20_6_language_tag_129_rejects" $
        pfails $
          pdecodeMidgardTxScriptWitnessesPreimageCbor # pconstant "\x81\x44\x82\x18\x81\x40"
    , testCase "v1_c20_6_noncanonical_language_tag_encoding_is_not_canonical" $ do
        canonicalSingleScriptPreimage @?= "\x81\x46\x82\x03\x43\x01\x02\x03"
        BS.length rawNoncanonicalSingleScriptPreimage @?= 8
        passertEvalNoTrace $
          verifyC20SingleScriptPreimage canonicalSingleScriptPreimage canonicalSingleScriptPreimage
    , testCase "v1_c20_6_noncanonical_language_tag_encoding_aborts" $
        pfails $
          verifyC20SingleScriptPreimage envelopedNoncanonicalSingleScriptPreimage envelopedNoncanonicalSingleScriptPreimage
    ]

verifyAtField :: forall s. Integer -> Term s PBool
verifyAtField fieldIndex =
  pverifyMidgardTransactionFieldPreimageV1
    # phex transactionIdHex
    # phex transactionCommitmentHex
    # phex compactCborHex
    # phex witnessSetCompactCborHex
    # phex fieldPreimageLengthsCborHex
    # pconstant fieldIndex
    # pconstant staleRawPreimage

verifyC20ScriptPreimage :: forall s. BS.ByteString -> Term s PBool
verifyC20ScriptPreimage preimage =
  pverifyMidgardTransactionFieldPreimageV1
    # pconstant c20MutationTxId
    # pconstant c20MutationCommitment
    # pconstant c20MutationCompactCbor
    # pconstant c20MutationWitnessSetCbor
    # pconstant c20MutationLengthsCbor
    # 6
    # pconstant preimage

verifyC20SingleScriptPreimage :: forall s.
  BS.ByteString -> BS.ByteString -> Term s PBool
verifyC20SingleScriptPreimage claimedShape preimage =
  let (txId, commitment, compact, witnessSet, lengths) =
        c20SingleScriptSource (BS.length claimedShape)
   in pverifyMidgardTransactionFieldPreimageV1
        # pconstant txId
        # pconstant commitment
        # pconstant compact
        # pconstant witnessSet
        # pconstant lengths
        # 6
        # pconstant preimage

-- The CML witness set is sorted by the native Cardano script hash.
maximumScripts :: [BS.ByteString]
maximumScripts = sortOn nativeScriptHash [nativeScript index | index <- [0 .. 223]]

nativeScript :: Int -> BS.ByteString
nativeScript index =
  hex "8201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d820519"
    <> word16be (20_000 + index)

nativeScriptHash :: BS.ByteString -> BS.ByteString
nativeScriptHash = blake2b224 . BS.cons 0

versionedScriptItem :: BS.ByteString -> BS.ByteString
versionedScriptItem script = "\x82\x00\x58\x28" <> script

-- The retired TypeScript/Aiken shape: an array followed by raw item CBOR.
staleRawPreimage :: BS.ByteString
staleRawPreimage = "\x98\xe0" <> foldMap versionedScriptItem maximumScripts

-- Current section 5.1 shape: the same items, each wrapped as definite bytes.
canonicalEnvelopedPreimage :: BS.ByteString
canonicalEnvelopedPreimage =
  "\x98\xe0" <> foldMap (("\x58\x2c" <>) . versionedScriptItem) maximumScripts

-- The source adjacent-count control constructs scripts in expiry-slot order.
syntheticEnvelopedPreimage :: Int -> BS.ByteString
syntheticEnvelopedPreimage count =
  BS.pack [0x98, fromIntegral count]
    <> foldMap
      (("\x58\x2c" <>) . versionedScriptItem . nativeScript)
      [0 .. count - 1]

--------------------------------------------------------------------------------
-- Exact two-witness C20 mutation source
--------------------------------------------------------------------------------

c20NativeScriptItem, c20PlutusScriptItem :: BS.ByteString
c20NativeScriptItem =
  "\x82\x00\x58\x20\x82\x00\x58\x1c" <> BS.replicate 28 0x55
c20PlutusScriptItem = "\x82\x03\x58\x20" <> BS.replicate 32 0x66

c20AddressWitnessItem :: Word8 -> Word8 -> BS.ByteString
c20AddressWitnessItem keyByte signatureByte =
  "\x82\x58\x20"
    <> BS.replicate 32 keyByte
    <> "\x58\x40"
    <> BS.replicate 64 signatureByte

honestC20ScriptPreimage, duplicateC20ScriptPreimage,
  reorderedC20ScriptPreimage, omittedC20ScriptPreimage :: BS.ByteString
honestC20ScriptPreimage = encodePreimage [c20NativeScriptItem, c20PlutusScriptItem]
duplicateC20ScriptPreimage = encodePreimage [c20NativeScriptItem, c20NativeScriptItem]
reorderedC20ScriptPreimage = encodePreimage [c20PlutusScriptItem, c20NativeScriptItem]
omittedC20ScriptPreimage = encodePreimage [c20NativeScriptItem]

c20AddressPreimage :: BS.ByteString
c20AddressPreimage =
  encodePreimage
    [ c20AddressWitnessItem 0x11 0x22
    , c20AddressWitnessItem 0x33 0x44
    ]

c20MutationTxId, c20MutationCommitment, c20MutationCompactCbor,
  c20MutationWitnessSetCbor, c20MutationLengthsCbor :: BS.ByteString
c20MutationTxId =
  blake2b256 ("MidgardNativeTxBodyV1" <> "\x01" <> c20MutationBodyCbor)
c20MutationCommitment =
  blake2b256 $
    "MidgardNativeTxProofSourceV1\x01\x83"
      <> definiteBytes c20MutationCompactCbor
      <> definiteBytes c20MutationWitnessSetCbor
      <> definiteBytes c20MutationLengthsCbor
c20MutationCompactCbor =
  "\x84\x01"
    <> c20MutationBodyCbor
    <> defBytes32 (blake2b256 c20MutationWitnessSetCbor)
    <> "\x00"
c20MutationWitnessSetCbor =
  "\x83"
    <> defBytes32 (blake2b256 c20AddressPreimage)
    <> defBytes32 (blake2b256 honestC20ScriptPreimage)
    <> defBytes32 emptyFieldCommitment
c20MutationLengthsCbor =
  "\x89"
    <> foldMap cborInt [1, 1, 1, 1, 1, 1, 77, 207, 1]

c20MutationBodyCbor :: BS.ByteString
c20MutationBodyCbor =
  "\x8c"
    <> foldMap defBytes32 (replicate 3 emptyFieldCommitment)
    <> "\x00\x20\x20"
    <> foldMap defBytes32 (replicate 3 emptyFieldCommitment)
    <> foldMap defBytes32 (replicate 2 zeroHash)
    <> "\x18\xff"

emptyFieldCommitment, zeroHash :: BS.ByteString
emptyFieldCommitment = blake2b256 "\x80"
zeroHash = BS.replicate 32 0

encodePreimage :: [BS.ByteString] -> BS.ByteString
encodePreimage items =
  BS.singleton (fromIntegral (0x80 + length items))
    <> foldMap definiteBytes items

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | len <= 23 = BS.singleton (fromIntegral (0x40 + len)) <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | len <= 65_535 = "\x59" <> word16be len <> bytes
  | otherwise = error "C20 fixture byte string exceeds the supported range"
  where
    len = BS.length bytes

defBytes32 :: BS.ByteString -> BS.ByteString
defBytes32 bytes = "\x58\x20" <> bytes

cborInt :: Int -> BS.ByteString
cborInt value
  | value <= 23 = BS.singleton (fromIntegral value)
  | value <= 255 = BS.pack [0x18, fromIntegral value]
  | value <= 65_535 = "\x19" <> word16be value
  | otherwise = error "C20 fixture integer exceeds the supported range"

canonicalSingleScriptPreimage, rawNoncanonicalSingleScriptPreimage,
  envelopedNoncanonicalSingleScriptPreimage :: BS.ByteString
canonicalSingleScriptPreimage = "\x81\x46\x82\x03\x43\x01\x02\x03"
rawNoncanonicalSingleScriptPreimage = "\x81\x82\x18\x03\x43\x01\x02\x03"
envelopedNoncanonicalSingleScriptPreimage = "\x81\x48\x82\x18\x03\x43\x01\x02\x03"

c20SingleScriptSource :: Int ->
  (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString)
c20SingleScriptSource claimedScriptLength =
  ( txId
  , commitment
  , compact
  , witnessSet
  , lengths
  )
  where
    witnessSet =
      "\x83"
        <> defBytes32 emptyFieldCommitment
        <> defBytes32 (blake2b256 canonicalSingleScriptPreimage)
        <> defBytes32 emptyFieldCommitment
    compact =
      "\x84\x01"
        <> c20MutationBodyCbor
        <> defBytes32 (blake2b256 witnessSet)
        <> "\x00"
    lengths =
      "\x89"
        <> foldMap cborInt [1, 1, 1, 1, 1, 1, claimedScriptLength, 1, 1]
    txId = blake2b256 ("MidgardNativeTxBodyV1" <> "\x01" <> c20MutationBodyCbor)
    commitment =
      blake2b256 $
        "MidgardNativeTxProofSourceV1\x01\x83"
          <> definiteBytes compact
          <> definiteBytes witnessSet
          <> definiteBytes lengths

word16be :: Int -> BS.ByteString
word16be value = BS.pack [fromIntegral (value `div` 256), fromIntegral value]

blake2b224 :: BS.ByteString -> BS.ByteString
blake2b224 = fromBuiltin . Builtins.blake2b_224 . toBuiltin

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

phex :: String -> Term s PByteString
phex = pconstant . hex

hex :: String -> BS.ByteString
hex = Base16.decodeLenient . BSC.pack

transactionIdHex, transactionCommitmentHex, compactCborHex,
  witnessSetCompactCborHex, fieldPreimageLengthsCborHex :: String
transactionIdHex = "9e6740ff958462051886f6d64a5fb7d03ee47dc50bd627c5765734f1c32f4bde"
transactionCommitmentHex = "7c0bebc279806e82bfdda06d46c06e6917e132e13325e48d2f3eb5ad7b7509da"
compactCborHex = "84018c5820114094118138473ad4d828ed3aa3b5767604cf846235863510ded7f7fb5d36655820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f5820598bbaa08e9cc6dc4d9634b23089ead14091f45c5e1165dcf8a6288be95a1b001a000d570d201927105820e127f848e4bda8c1e9b42ddf4c89dfbd1479301dd90551baeff900fdfcec2e975820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff58205c66a9cb2310e13ca74d861c7d29f5b996030ad755920f1dfc2c325ec3cb015d00"
witnessSetCompactCborHex = "8358209c3c9f949b41759fc4d9ea024e36e2aa7659f3d5dbe41611256f4dfc80a9a62d5820ad4dcd868783831d5bd321d25528c3295f55b1ea6c8d61d85c3216be9a73ea3d5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"
fieldPreimageLengthsCborHex = "89182701182c191a420101192682186801"
