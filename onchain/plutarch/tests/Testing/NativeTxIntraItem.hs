{-# LANGUAGE OverloadedStrings #-}

module Testing.NativeTxIntraItem (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Builtin.ByteString (pintegerToByteString, pmostSignificantFirst)
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.NativeTxIntraItem
import Midgard.LedgerOutput (pdecodeCanonicalOutput)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAssets,
  PMidgardTxOutput (..),
  PMidgardValue (..),
 )
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests = testGroup "Midgard.NativeTxIntraItem"
  [ valueTests
  , testGroup "core"
      [ bytesCase "whole nested datum" nestedDatum [] nestedDatum
      , bytesCase "first nested child" nestedDatum [0] (hex "00")
      , bytesCase "nested constructor child" nestedDatum [1] (hex "d8799f44010203041901f4ff")
      , bytesCase "nested bytes child" nestedDatum [1, 0] (hex "4401020304")
      , bytesCase "nested integer child" nestedDatum [1, 1] (hex "1901f4")
      , nothingCase "constructor is not integer" $ pdatumIntegerAtV1 # pconstant nestedDatum # 0
      , integerCase "constructor alternative zero" (pdatumAlternativeAtV1 # pconstant nestedDatum # 0) 0
      , extentNothingCase "outer child out of range" nestedDatum [2]
      , extentNothingCase "inner child out of range" nestedDatum [1, 2]
      , extentNothingCase "path through leaf" nestedDatum [0, 0]
      , extentNothingCase "negative path index" nestedDatum [-1]
      , extentNothingCase "truncated container" (hex "d8799f00") [0]
      , bytesCase "map key zero" mapDatum [0] (hex "01")
      , bytesCase "map value zero" mapDatum [1] (hex "4401020304")
      , bytesCase "map key one" mapDatum [2] (hex "02")
      , bytesCase "map value one" mapDatum [3] (hex "9f00ff")
      , bytesCase "map nested list child" mapDatum [3, 0] (hex "00")
      , extentNothingCase "map child out of range" mapDatum [4]
      , integerCase "positive bignum" (pdatumIntegerAtV1 # pconstant (hex "c249010000000000000000") # 0) 18446744073709551616
      , integerCase "negative bignum" (pdatumIntegerAtV1 # pconstant (hex "c349010000000000000000") # 0) (-18446744073709551617)
      , integerCase "uint 500" (pdatumIntegerAtV1 # pconstant (hex "1901f4") # 0) 500
      , integerCase "negative one" (pdatumIntegerAtV1 # pconstant (hex "20") # 0) (-1)
      , integerCase "max uint64" (pdatumIntegerAtV1 # pconstant (hex "1bffffffffffffffff") # 0) 18446744073709551615
      , nothingCase "non-minimal bignum" $ pdatumIntegerAtV1 # pconstant (hex "c2480100000000000000") # 0
      , integerCase "alternative 128" (pdatumAlternativeAtV1 # pconstant (hex "d86682188080") # 0) 128
      , integerCase "alternative 65535" (pdatumAlternativeAtV1 # pconstant (hex "d8668219ffff80") # 0) 65535
      , integerCase "alternative 0" (pdatumAlternativeAtV1 # pconstant (hex "d87980") # 0) 0
      , integerCase "alternative 6" (pdatumAlternativeAtV1 # pconstant (hex "d87f80") # 0) 6
      , integerCase "alternative 7" (pdatumAlternativeAtV1 # pconstant (hex "d9050080") # 0) 7
      , integerCase "alternative 127" (pdatumAlternativeAtV1 # pconstant (hex "d9057880") # 0) 127
      , nothingCase "alternative 128 in wrong form" $ pdatumAlternativeAtV1 # pconstant (hex "d9057980") # 0
      , nothingCase "generic alternative below 128" $ pdatumAlternativeAtV1 # pconstant (hex "d866820080") # 0
      , testGroup "malformed typed heads" $ concatMap malformedCases
          [hex "d9007980", hex "d866", hex "d8668219008080"]
      , integerCase "nested bignum access" (pdatumIntegerAtV1 # pconstant nestedBignum # 3) 18446744073709551616
      , bytesCase "nested bignum child" nestedBignum [0] (hex "c249010000000000000000")
      , bytesCase "nested high-alternative child" nestedHighAlternative [0] (hex "d86682188080")
      , integerCase "nested high alternative" (pdatumAlternativeAtV1 # pconstant nestedHighAlternative # 3) 128
      , testCase "stitches canonical byte-string chunks" $ passertEvalNoTrace stitchesChunks
      ]
  , testGroup "wide budget"
      [ testCase "reaches the last leaf of a wide datum" $ passertEvalNoTrace reachesWideLeaf
      ]
  ]

valueTests :: TestTree
valueTests = testGroup "Midgard.NativeTxIntraItem.value"
  [ testGroup "core"
      [ testCase "sweeps all 64 units in order" $ passertEvalNoTrace sweepsWideValue
      , testCase "value_bookmark_agrees_with_the_materialising_decoder" $
          passertEvalNoTrace bookmarkAgreesWithMaterialisingDecoder
      , testCase "reports absent units and continues" $ passertEvalNoTrace reportsAbsence
      , testCase "opens ada-only value" $ passertEvalNoTrace opensAdaOnly
      , testCase "hand-built wide fixture is canonical output" $ passertEvalNoTrace wideFixtureCanonical
      ]
  , testGroup "refusals"
      [ testCase "repeated lookup" $ pfails repeatedLookup
      , testCase "backwards lookup" $ pfails backwardsLookup
      , testCase "unordered policies" $ pfails $ pforceOpen unorderedPolicies
      , testCase "duplicate policy" $ pfails $ pforceOpen duplicatePolicies
      , testCase "unordered asset names" $ pfails $ pforceOpen unorderedNames
      , testCase "zero quantity" $ pfails $ pforceOpen zeroQuantity
      , testCase "empty policy group" $ pfails $ pforceOpen emptyPolicyGroup
      , testCase "short policy id" $ pfails $ pforceOpen shortPolicyGroup
      , testCase "non-canonical address width" $ pfails $ pforceOpen nonCanonicalAddressWidth
      , testCase "non-output map head" $ pfails $ pforceOpen $ rawOutput 0xa5 0 0x58 29 1 adaOnlyValue
      , testCase "first key is not address" $ pfails $ pforceOpen $ rawOutput 0xa2 2 0x58 29 1 adaOnlyValue
      , testCase "non-two-byte address wrapper" $ pfails $ pforceOpen $ rawOutput 0xa2 0 0x57 29 1 adaOnlyValue
      , testCase "second key is not value" $ pfails $ pforceOpen $ rawOutput 0xa2 0 0x58 29 2 adaOnlyValue
      , testCase "value is not a pair" $ pfails $ pforceOpen $ rawOutput 0xa2 0 0x58 29 1 (hex "8301a0")
      , testCase "non-minimal one-byte interior head" $ pfails $ pforceOpen $ rawOutput 0xa2 0 0x58 29 1 (hex "821801a0")
      , testCase "non-minimal two-byte interior head" $ pfails $ pforceOpen $ rawOutput 0xa2 0 0x58 29 1 (hex "82190001a0")
      , testCase "non-minimal four-byte interior head" $ pfails $ pforceOpen $ rawOutput 0xa2 0 0x58 29 1 (hex "821a00000001a0")
      , testCase "non-minimal eight-byte interior head" $ pfails $ pforceOpen $ rawOutput 0xa2 0 0x58 29 1 (hex "821b0000000000000001a0")
      , testCase "indefinite interior head" $ pfails $ pforceOpen $ rawOutput 0xa2 0 0x58 29 1 (hex "821fffffffffffffffffa0")
      , testCase "wrong-major interior head" $ pfails $ pforceOpen $ rawOutput 0xa2 0 0x58 29 1 (hex "8241a0")
      , testCase "oversized asset name" $ pfails $ pforceOpen oversizedNameGroup
      , testCase "short lookup policy" $ pfails shortLookupPolicy
      , testCase "oversized lookup name" $ pfails oversizedLookupName
      ]
  ]

wideFixtureCanonical :: forall s. Term s PBool
wideFixtureCanonical = pmatch (pdecodeCanonicalOutput # pconstant wideValueItem) $ \case
  PNothing -> pconstant False
  PJust _ -> pconstant True

sweepsWideValue :: forall s. Term s PBool
sweepsWideValue =
  pfix (\self -> plam $ \index total bookmark ->
    pif (index #>= 64)
      (total #== 2080 #&& pvalueLovelace # bookmark #== 5_000_000) $
      pmatch
        (pvalueQuantityOf # bookmark # ppolicyTerm (pdiv # index # 8) # passetTerm (pmod # index # 8))
        $ \(PPair quantity advanced) -> self # (index + 1) # (total + quantity) # advanced)
    # 0 # 0 # (popenValueBookmark # pconstant wideValueItem)

bookmarkAgreesWithMaterialisingDecoder :: forall s. Term s PBool
bookmarkAgreesWithMaterialisingDecoder =
  pmatch (pdecodeCanonicalOutput # pconstant wideValueItem) $ \case
    PNothing -> pconstant False
    PJust output -> pmatch output $ \PMidgardTxOutput {ptxOutput'value} ->
      pmatch (pfromData ptxOutput'value) $ \PMidgardValue {pvalue'lovelace, pvalue'assets} ->
        pfix
          ( \self -> plam $ \index agreed bookmark ->
              pif
                (index #>= 64)
                ( agreed
                    #&& pfromData pvalue'lovelace
                    #== pvalueLovelace # (popenValueBookmark # pconstant wideValueItem)
                )
                ( plet (ppolicyTerm (pdiv # index # 8)) $ \policy ->
                    plet (passetTerm (pmod # index # 8)) $ \asset ->
                      pmatch (pvalueQuantityOf # bookmark # policy # asset) $ \(PPair quantity advanced) ->
                        self
                          # (index + 1)
                          # (agreed #&& quantity #== pmaterialisedQuantity (policy <> asset) (pfromData pvalue'assets))
                          # advanced
                )
          )
          # 0
          # pconstant True
          # (popenValueBookmark # pconstant wideValueItem)

pmaterialisedQuantity :: forall s. Term s PByteString -> Term s PMidgardAssets -> Term s PInteger
pmaterialisedQuantity unit assets =
  pfix
    ( \self -> plam $ \entries ->
        pelimList
          ( \entry rest ->
              pif
                (pfromData (pfstBuiltin # entry) #== unit)
                (pfromData (psndBuiltin # entry))
                (self # rest)
          )
          0
          entries
    )
    # pto assets

reportsAbsence :: forall s. Term s PBool
reportsAbsence =
  plet (popenValueBookmark # pconstant wideValueItem) $ \opened ->
    pmatch (pvalueQuantityOf # opened # pconstant (policyId 0) # pconstant (assetName 9)) $ \(PPair missing afterMissing) ->
      pmatch (pvalueQuantityOf # afterMissing # pconstant (policyId 2) # pconstant (assetName 3)) $ \(PPair present afterPresent) ->
        pmatch (pvalueQuantityOf # afterPresent # pconstant (policyId 9) # pconstant (assetName 0)) $ \(PPair missingPolicy _) ->
          missing #== 0 #&& present #== 20 #&& missingPolicy #== 0

opensAdaOnly :: forall s. Term s PBool
opensAdaOnly = plet (popenValueBookmark # pconstant (outputItem 1_500_000 [])) $ \opened ->
  pmatch (pvalueQuantityOf # opened # pconstant (policyId 0) # pconstant (assetName 0)) $ \(PPair quantity _) ->
    pvalueLovelace # opened #== 1_500_000 #&& quantity #== 0

repeatedLookup :: forall s. Term s PBool
repeatedLookup = plet (popenValueBookmark # pconstant wideValueItem) $ \opened ->
  pmatch (pvalueQuantityOf # opened # pconstant (policyId 0) # pconstant (assetName 0)) $ \(PPair _ advanced) ->
    plet (pvalueQuantityOf # advanced # pconstant (policyId 0) # pconstant (assetName 0)) $ \_ -> pconstant True

backwardsLookup :: forall s. Term s PBool
backwardsLookup = plet (popenValueBookmark # pconstant wideValueItem) $ \opened ->
  pmatch (pvalueQuantityOf # opened # pconstant (policyId 2) # pconstant (assetName 0)) $ \(PPair _ advanced) ->
    plet (pvalueQuantityOf # advanced # pconstant (policyId 1) # pconstant (assetName 7)) $ \_ -> pconstant True

pforceOpen :: forall s. BS.ByteString -> Term s PBool
pforceOpen item = plet (popenValueBookmark # pconstant item) $ \_ -> pconstant True

shortLookupPolicy, oversizedLookupName :: forall s. Term s PBool
shortLookupPolicy = plet (popenValueBookmark # pconstant wideValueItem) $ \opened ->
  plet (pvalueQuantityOf # opened # pconstant (hex "b0b0b0b0") # pconstant (assetName 0)) $ \_ -> pconstant True
oversizedLookupName = plet (popenValueBookmark # pconstant wideValueItem) $ \opened ->
  plet (pvalueQuantityOf # opened # pconstant (policyId 0) # pconstant (BS.replicate 33 0xcc)) $ \_ -> pconstant True

ppolicyTerm :: forall s. Term s PInteger -> Term s PByteString
ppolicyTerm index = pintegerToByteString # pmostSignificantFirst # 1 # index <> pconstant (BS.replicate 27 0xb0)

passetTerm :: forall s. Term s PInteger -> Term s PByteString
passetTerm index = pconstant (BS.replicate 3 0xc0) <> pintegerToByteString # pmostSignificantFirst # 1 # index

bytesCase :: String -> BS.ByteString -> [Integer] -> BS.ByteString -> TestTree
bytesCase name datum path expected = testCase name $ passertEvalNoTrace $ pbytesAtPath datum path expected

integerCase :: String -> (forall s. Term s (PMaybe PInteger)) -> Integer -> TestTree
integerCase name value expected = testCase name $ passertEvalNoTrace $ pintegerMaybe value expected

nothingCase :: forall a. String -> (forall s. Term s (PMaybe a)) -> TestTree
nothingCase name value = testCase name $ passertEvalNoTrace $ pisNothing value

extentNothingCase :: String -> BS.ByteString -> [Integer] -> TestTree
extentNothingCase name datum path = nothingCase name $
  pdatumChildExtentV1 # pconstant datum # pintList path

malformedCases :: BS.ByteString -> [TestTree]
malformedCases datum =
  [ nothingCase "alternative" $ pdatumAlternativeAtV1 # pconstant datum # 0
  , nothingCase "integer" $ pdatumIntegerAtV1 # pconstant datum # 0
  , nothingCase "bytes" $ pdatumBytesAtV1 # pconstant datum # 0
  ]

stitchesChunks :: forall s. Term s PBool
stitchesChunks =
  let payload = BS.replicate 64 0xaa <> BS.singleton 0xbb
      datum = hex "5f5840" <> BS.replicate 64 0xaa <> hex "41bbff"
   in pmatch (pdatumBytesAtV1 # pconstant datum # 0) $ \case
        PNothing -> pconstant False
        PJust bytes -> bytes #== pconstant payload

reachesWideLeaf :: forall s. Term s PBool
reachesWideLeaf = pmatch (pdatumChildExtentV1 # pconstant wideDatum # pintList [23]) $ \case
  PNothing -> pconstant False
  PJust extent -> pmatch extent $ \(PPair offset _) ->
    pmatch (pdatumBytesAtV1 # pconstant wideDatum # offset) $ \case
      PNothing -> pconstant False
      PJust bytes -> bytes #== pconstant (wideLeaf 23)

pbytesAtPath :: forall s. BS.ByteString -> [Integer] -> BS.ByteString -> Term s PBool
pbytesAtPath datum path expected = pmatch
  (pdatumChildBytesV1 # pconstant datum # pintList path) $ \case
    PNothing -> pconstant False
    PJust bytes -> bytes #== pconstant expected

pintegerMaybe :: forall s. Term s (PMaybe PInteger) -> Integer -> Term s PBool
pintegerMaybe value expected = pmatch value $ \case
  PNothing -> pconstant False
  PJust integer -> integer #== pconstant expected

pisNothing :: forall s a. Term s (PMaybe a) -> Term s PBool
pisNothing value = pmatch value $ \case PNothing -> pconstant True; PJust _ -> pconstant False

pintList :: forall s. [Integer] -> Term s (PBuiltinList PInteger)
pintList = foldr (\value rest -> pcons # pconstant value # rest) pnil

nestedDatum, nestedBignum, nestedHighAlternative, mapDatum :: BS.ByteString
nestedDatum = hex "d8799f00d8799f44010203041901f4ffff"
nestedBignum = hex "d8799fc249010000000000000000ff"
nestedHighAlternative = hex "d8799fd86682188080ff"
mapDatum = hex "a2014401020304029f00ff"

wideDatum :: BS.ByteString
wideDatum = hex "9f" <> BS.concat [hex "5820" <> wideLeaf i | i <- [0 .. 23]] <> hex "ff"

wideLeaf :: Int -> BS.ByteString
wideLeaf index = BS.singleton (fromIntegral index) <> BS.replicate 31 0xd0

type AssetGroup = (BS.ByteString, [(BS.ByteString, Integer)])

wideValueItem :: BS.ByteString
wideValueItem = outputItem 5_000_000
  [ (policyId policy, [(assetName asset, quantityOf policy asset) | asset <- [0 .. 7]])
  | policy <- [0 .. 7]
  ]

unorderedPolicies, duplicatePolicies, unorderedNames, zeroQuantity :: BS.ByteString
unorderedPolicies = outputItem 1 [group 1, group 0]
duplicatePolicies = outputItem 1 [group 0, group 0]
unorderedNames = outputItem 1 [(policyId 0, [(assetName 1, 1), (assetName 0, 2)])]
zeroQuantity = outputItem 1 [(policyId 0, [(assetName 0, 0)])]

emptyPolicyGroup, shortPolicyGroup, oversizedNameGroup, nonCanonicalAddressWidth :: BS.ByteString
emptyPolicyGroup = outputItem 1 [(policyId 0, [])]
shortPolicyGroup = outputItem 1 [(BS.replicate 27 0xb0, [(assetName 0, 1)])]
oversizedNameGroup = outputItem 1 [(policyId 0, [(BS.replicate 33 0xcc, 1)])]
nonCanonicalAddressWidth =
  hex "a200581c" <> BS.replicate 28 0xaa <> hex "018201a0"

adaOnlyValue :: BS.ByteString
adaOnlyValue = hex "8201a0"

rawOutput :: Int -> Int -> Int -> Int -> Int -> BS.ByteString -> BS.ByteString
rawOutput mapHeader addressKey wrapper declared valueKey valueBytes =
  BS.pack [fromIntegral mapHeader, fromIntegral addressKey, fromIntegral wrapper, fromIntegral declared]
    <> BS.replicate declared 0xaa <> BS.singleton (fromIntegral valueKey) <> valueBytes

group :: Int -> AssetGroup
group policy = (policyId policy, [(assetName 0, quantityOf policy 0)])

policyId :: Int -> BS.ByteString
policyId index = BS.singleton (fromIntegral index) <> BS.replicate 27 0xb0

assetName :: Int -> BS.ByteString
assetName index = BS.replicate 3 0xc0 <> BS.singleton (fromIntegral index)

quantityOf :: Int -> Int -> Integer
quantityOf policy asset = fromIntegral (1 + policy * 8 + asset)

addressPayload :: BS.ByteString
addressPayload = BS.singleton 0x60 <> BS.replicate 28 0xaa

outputItem :: Integer -> [AssetGroup] -> BS.ByteString
outputItem lovelace groups =
  hex "a200" <> bytesItem addressPayload <> hex "0182" <> uintItem lovelace
    <> mapHead (length groups) <> BS.concat (map encodeGroup groups)

encodeGroup :: AssetGroup -> BS.ByteString
encodeGroup (policy, assets) =
  bytesItem policy <> mapHead (length assets)
    <> BS.concat [bytesItem name <> uintItem quantity | (name, quantity) <- assets]

bytesItem :: BS.ByteString -> BS.ByteString
bytesItem bytes
  | BS.length bytes <= 23 = BS.singleton (0x40 + fromIntegral (BS.length bytes)) <> bytes
  | otherwise = hex "58" <> BS.singleton (fromIntegral $ BS.length bytes) <> bytes

mapHead :: Int -> BS.ByteString
mapHead count = BS.singleton (0xa0 + fromIntegral count)

uintItem :: Integer -> BS.ByteString
uintItem value
  | value <= 23 = BS.singleton (fromIntegral value)
  | value <= 0xff = BS.pack [0x18, fromIntegral value]
  | value <= 0xffff = BS.pack [0x19, fromIntegral (value `div` 256), fromIntegral value]
  | otherwise = BS.pack
      [ 0x1a
      , fromIntegral (value `div` 0x1000000)
      , fromIntegral (value `div` 0x10000)
      , fromIntegral (value `div` 0x100)
      , fromIntegral value
      ]

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient
