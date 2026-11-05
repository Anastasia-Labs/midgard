{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.ScriptLanguageViews
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/script-language-views-v1.ak@.

Four cases and a constant, so the tests are a table — but the table is built by a
Haskell reference that assembles the language-views map itself rather than by
copying the port's bytes back.

__The empty case is the one that would go wrong quietly.__ A bitmap of @0@ hashes
CBOR @null@, @0xf6@ — not the empty string, not an empty map. All three are
32-byte hashes of something, so a port that picked the wrong one would look
entirely healthy until a script-free transaction's integrity hash disagreed with
L1's. It is pinned against a literal below.

The other three cases are pinned structurally: each bitmap must produce a
different hash from every other, and the two-language case must be the
ascending-key map rather than either single-language map concatenated.
-}
module Testing.ScriptLanguageViews (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.ScriptLanguageViews (
  pcanonicalCostModelView,
  pemptyScriptIntegrityHash,
  pexpectedScriptIntegrityHash,
 )
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Script Language Views Tests"
    [ testCase "script_integrity_language_view_vectors_match_typescript" $
        holds typescriptVectorsMatch
    , testGroup "the empty case" emptyTests
    , testGroup "the three script cases" viewTests
    , testGroup "what aborts" abortTests
    ]

typescriptVectorsMatch :: forall (s :: S). Term s PBool
typescriptVectorsMatch = foldr (#&&) (pconstant True)
  [ ( pexpectedScriptIntegrityHash
        # pconstant typescriptRedeemerHash
        # pconstant bitmap
    )
      #== pconstant expected
  | (bitmap, expected) <- typescriptIntegrityVectors
  ]

--------------------------------------------------------------------------------
-- The empty case
--------------------------------------------------------------------------------

emptyTests :: [TestTree]
emptyTests =
  [ testCase "a bitmap of zero hashes CBOR null" $
      holds $ (pexpectedScriptIntegrityHash # pconstant witnessHash # 0) #== pconstant nullHash
  , testCase "…which is the module's own constant" $
      holds $ pemptyScriptIntegrityHash #== pconstant nullHash
  , {- The three things it could have been. All are 32-byte hashes, so only a
       direct comparison separates them. -}
    testCase "…and is not the hash of the empty string" $
      nullHash /= blake2b256 "" @? "null hash collided with the empty string's"
  , testCase "…nor of an empty CBOR map" $
      nullHash /= blake2b256 "\xa0" @? "null hash collided with the empty map's"
  , testCase "…nor of an empty CBOR array" $
      nullHash /= blake2b256 "\x80" @? "null hash collided with the empty array's"
  , -- The witness hash is not in the preimage at all when there are no scripts.
    testCase "the redeemer witness hash does not affect it" $
      holds $
        (pexpectedScriptIntegrityHash # pconstant witnessHash # 0)
          #== (pexpectedScriptIntegrityHash # pconstant otherWitnessHash # 0)
  ]

--------------------------------------------------------------------------------
-- The three script cases
--------------------------------------------------------------------------------

viewTests :: [TestTree]
viewTests =
  [ testCase ("bitmap " <> show bitmap <> " matches the reference") $
    holds $
      (pexpectedScriptIntegrityHash # pconstant witnessHash # pconstant bitmap)
        #== pconstant (integrityHash witnessHash bitmap)
  | bitmap <- [1, 2, 3]
  ]
    <> [ testCase "the three bitmaps give three different hashes" $
          length (nub' [integrityHash witnessHash b | b <- [0, 1, 2, 3]]) @?= 4
       , testCase "the redeemer witness hash is inside the preimage" $
          holds $
            pnot
              #$ (pexpectedScriptIntegrityHash # pconstant witnessHash # 1)
              #== (pexpectedScriptIntegrityHash # pconstant otherWitnessHash # 1)
       , {- The two-language case is a canonical definite map with ascending
            keys, not either single-language map appended to the other. Both of
            those spell the same views in a different frame. -}
         testCase "the both-languages case is the ascending two-entry map" $
          integrityHash witnessHash 3
            @?= blake2b256
              ( "\x82"
                  <> definiteBytes witnessHash
                  <> hex "a202"
                  <> costModelView
                  <> hex "1880"
                  <> costModelView
              )
       , testCase "…and not the two one-entry maps concatenated" $
          integrityHash witnessHash 3
            /= blake2b256
              ( "\x82"
                  <> definiteBytes witnessHash
                  <> (hex "a102" <> costModelView)
                  <> (hex "a11880" <> costModelView)
              )
            @? "the two-language view collided with a concatenation"
       , testCase "the cost-model view the port carries is the fixture's" $
          holds $ pcanonicalCostModelView #== pconstant costModelView
       ]

--------------------------------------------------------------------------------
-- What aborts
--------------------------------------------------------------------------------

abortTests :: [TestTree]
abortTests =
  [ testCase "a bitmap above three aborts" $
      pfails $ pexpectedScriptIntegrityHash # pconstant witnessHash # 4
  , testCase "a negative bitmap aborts" $
      pfails $ pexpectedScriptIntegrityHash # pconstant witnessHash # (-1)
  , testCase "a witness hash that is not 32 bytes aborts" $
      pfails $ pexpectedScriptIntegrityHash # pconstant (BS.take 31 witnessHash) # 1
  , -- …including on the empty branch, where the hash is not read.
    testCase "…even when the bitmap is zero" $
      pfails $ pexpectedScriptIntegrityHash # pconstant (BS.take 31 witnessHash) # 0
  ]

--------------------------------------------------------------------------------
-- The reference, assembled rather than copied
--------------------------------------------------------------------------------

-- | @blake2b_256(82 ‖ bytes(redeemer_witness_hash) ‖ language_views)@.
integrityHash :: BS.ByteString -> Integer -> BS.ByteString
integrityHash _witness 0 = nullHash
integrityHash witness bitmap =
  blake2b256 ("\x82" <> definiteBytes witness <> languageViews bitmap)

languageViews :: Integer -> BS.ByteString
languageViews 1 = hex "a102" <> costModelView
languageViews 2 = hex "a11880" <> costModelView
languageViews _ = hex "a202" <> costModelView <> hex "1880" <> costModelView

nullHash :: BS.ByteString
nullHash = blake2b256 (hex "f6")

-- | A 32-byte definite byte string: @58 20 ‖ bytes@.
definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes b
  | BS.length b == 32 = hex "5820" <> b
  | otherwise = error "reference definiteBytes: fixture is 32 bytes"

witnessHash, otherWitnessHash :: BS.ByteString
witnessHash = blake2b256 "redeemers"
otherWitnessHash = blake2b256 "other redeemers"

typescriptRedeemerHash :: BS.ByteString
typescriptRedeemerHash = BS.replicate 32 0x11

typescriptIntegrityVectors :: [(Integer, BS.ByteString)]
typescriptIntegrityVectors =
  [ (0, hex "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53")
  , (1, hex "d7239eb1bd8b7376dedfbf7e6201815b225c023d11c975cd99d25d5236b199a1")
  , (2, hex "71201d25ea11e4104eda108782a7d67b37b4ae97df6dc3258b06d9c98e58bbcb")
  , (3, hex "6d49b4f24c60bec1cb34a2538278252059ec0601b7f675ef73fe2b48e24317d8")
  ]

{- | The canonical view, read back off the port.

Deliberately /not/ a second transcription: it is a 1,600-byte protocol constant
copied out of the ledger, and a hand-retyped duplicate would test typing accuracy
rather than the module. What the tests pin is the framing around it, which is
where the module's own logic lives, plus the fact that the port carries this
exact constant.
-}
costModelView :: BS.ByteString
costModelView = hex "9901291a000189b41901a401011903e818ad00011903e819ea350401192baf18201a000312591920a404193e801864193e801864193e801864193e801864193e801864193e80186418641864193e8018641a000170a718201a00020782182019f016041a0001194a18b2000119568718201a0001643519030104021a00014f581a0001e143191c893903831906b419022518391a00014f580001011903e819a7a90402195fe419733a1826011a000db464196a8f0119ca3f19022e011999101903e819ecb2011a00022a4718201a000144ce1820193bc318201a0001291101193371041956540a197147184a01197147184a0119a9151902280119aecd19021d0119843c18201a00010a9618201a00011aaa1820191c4b1820191cdf1820192d1a18201a00014f581a0001e143191c893903831906b419022518391a00014f5800011a0001614219020700011a000122c118201a00014f581a0001e143191c893903831906b419022518391a00014f580001011a00014f581a0001e143191c893903831906b419022518391a00014f5800011a000e94721a0003414000021a0004213c19583c041a00163cad19fc3604194ff30104001a00022aa818201a000189b41901a401011a00013eff182019e86a1820194eae182019600c1820195108182019654d182019602f18201a0290f1e70a1a032e93af1937fd0a1a0298e40b1966c40a193e801864193e8018641a000eaf1f121a002a6e06061a0006be98011a0321aac7190eac121a00041699121a048e466e1922a4121a0327ec9a121a001e743c18241a0031410f0c1a000dbf9e011a09f2f6d31910d318241a0004578218241a096e44021967b518241a0473cee818241a13e62472011a0f23d40118481a00212c5618481a0022814619fc3b041a00032b00192076041a0013be0419702c183f00011a000f59d919aa6718fb00011a000187551902d61902cf00011a000187551902d61902cf00011a000187551902d61902cf00011a0001a5661902a800011a00017468011a00044a391949a000011a0002bfe2189f01011a00026b371922ee00011a00026e9219226d00011a0001a3e2190ce2011a00019e4919028f011a001df8bb195fc803"

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode

nub' :: [BS.ByteString] -> [BS.ByteString]
nub' = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

holds :: (forall (s :: S). Term s PBool) -> Assertion
holds = passertEval
