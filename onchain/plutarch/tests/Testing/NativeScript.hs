{-# LANGUAGE OverloadedStrings #-}

module Testing.NativeScript (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.NativeScript
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.NativeScript"
  [ testCase "native_script_v1_reconstructs_and_evaluates_a_signature_script" $
      passertEvalNoTrace reconstructsSignature
  , testGroup "native_script_v1_pins_every_cardano_tag_and_arity"
      [ canonicalCase "signature" sigScript (-1) (-1) True True
      , canonicalCase "all" (hex "820180") (-1) (-1) False True
      , canonicalCase "any" (hex "820280") (-1) (-1) False False
      , canonicalCase "at least" (hex "83030080") (-1) (-1) False True
      , canonicalCase "after" (hex "820400") 0 (-1) False True
      , canonicalCase "before" (hex "820500") (-1) 0 False True
      , testCase "unknown tag" $ passertEvalNoTrace $
          pisNothing $ pcheckNativeScriptV1 # pconstant (hex "820600") # (-1) # (-1) # pnil
      ]
  , testCase "native_script_v1_rejects_a_non_canonical_indefinite_script" $
      passertEvalNoTrace rejectsIndefinite
  , testCase "native_script_v1_applies_validity_bounds_and_at_least" $
      passertEvalNoTrace appliesValidityAndAtLeast
  , testGroup "native_script_v1_accepts_depth_16_and_rejects_depth_17"
      [ testCase "accepts depth 16" $ passertEvalNoTrace $ pisJust $ pcheck (nestAll 15)
      , testCase "rejects depth 17" $ passertEvalNoTrace $ pisNothing $ pcheck (nestAll 16)
      ]
  , testGroup "native_script_v1_rejects_more_than_32_nodes"
      [ testCase "accepts 32 nodes" $ passertEvalNoTrace accepts32Nodes
      , testCase "rejects 33 nodes" $ passertEvalNoTrace $ pisNothing $ pcheckWith 0 (anyAfterScripts 32)
      ]
  ]

canonicalCase :: String -> BS.ByteString -> Integer -> Integer -> Bool -> Bool -> TestTree
canonicalCase name script start end hasSigner expected = testCase name $ passertEvalNoTrace $
  pcanonicalWith (pconstant script) (pconstant start) (pconstant end)
    (if hasSigner then psignerList else pnil) (pconstant expected)

reconstructsSignature :: forall s. Term s PBool
reconstructsSignature = pmatch (pcheckNativeScriptV1 # pconstant sigScript # (-1) # (-1) # psignerList) $ \case
  PNothing -> pconstant False
  PJust checked -> pmatch checked $ \c ->
    pfromData (pnativeProof'canonicalCbor c) #== pconstant sigScript
      #&& pfromData (pnativeProof'valid c)
      #&& pfromData (pnativeProof'nodeCount c) #== 1
      #&& pfromData (pnativeProof'depth c) #== 1

pcanonicalWith :: forall s. Term s PByteString -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList PByteString) -> Term s PBool -> Term s PBool
pcanonicalWith script start end signers expected =
  pmatch (pcheckNativeScriptV1 # script # start # end # signers) $ \case
    PNothing -> pconstant False
    PJust checked -> pmatch checked $ \c ->
      pfromData (pnativeProof'canonicalCbor c) #== script
        #&& pfromData (pnativeProof'valid c) #== expected
        #&& pfromData (pnativeProof'nodeCount c) #== 1
        #&& pfromData (pnativeProof'depth c) #== 1

rejectsIndefinite :: forall s. Term s PBool
rejectsIndefinite = pisNothing $
  pcheckNativeScriptV1 # pconstant (hex "9f00" <> encodeBytes signer <> hex "ff") # (-1) # (-1) # psignerList

appliesValidityAndAtLeast :: forall s. Term s PBool
appliesValidityAndAtLeast =
  pmatch (pcheckNativeScriptV1 # pconstant atLeastScript # 100 # 200 # psignerList) $ \case
    PNothing -> pconstant False
    PJust checked -> pmatch checked $ \c ->
      pfromData (pnativeProof'valid c)
        #&& pfromData (pnativeProof'nodeCount c) #== 4
        #&& pfromData (pnativeProof'depth c) #== 2

accepts32Nodes :: forall s. Term s PBool
accepts32Nodes = pmatch (pcheckWith 0 $ anyAfterScripts 31) $ \case
  PNothing -> pconstant False
  PJust checked -> pmatch checked $ \c -> pfromData (pnativeProof'nodeCount c) #== 32

pcheck :: forall s. BS.ByteString -> Term s (PMaybe PNativeScriptProofV1)
pcheck = pcheckWith (-1)

pcheckWith :: forall s. Integer -> BS.ByteString -> Term s (PMaybe PNativeScriptProofV1)
pcheckWith start script = pcheckNativeScriptV1 # pconstant script # pconstant start # (-1) # psignerList

pisNothing :: forall s a. Term s (PMaybe a) -> Term s PBool
pisNothing value = pmatch value $ \case PNothing -> pconstant True; PJust _ -> pconstant False

pisJust :: forall s a. Term s (PMaybe a) -> Term s PBool
pisJust value = pmatch value $ \case PNothing -> pconstant False; PJust _ -> pconstant True

psignerList :: forall s. Term s (PBuiltinList PByteString)
psignerList = pcons # pconstant signer # pnil

signer, sigScript, atLeastScript :: BS.ByteString
signer = BS.replicate 28 0xaa
sigScript = hex "8200" <> encodeBytes signer
atLeastScript = hex "83030283" <> sigScript <> hex "82041864" <> hex "820518c8"

nestAll :: Int -> BS.ByteString
nestAll count = BS.concat (replicate count $ hex "820181") <> sigScript

anyAfterScripts :: Int -> BS.ByteString
anyAfterScripts count = hex "8202" <> encodeArrayHeader count <> BS.concat (replicate count $ hex "820400")

encodeArrayHeader :: Int -> BS.ByteString
encodeArrayHeader count
  | count <= 23 = BS.singleton $ fromIntegral (0x80 + count)
  | otherwise = BS.pack [0x98, fromIntegral count]

encodeBytes :: BS.ByteString -> BS.ByteString
encodeBytes value
  | BS.length value <= 23 = BS.singleton (fromIntegral $ 0x40 + BS.length value) <> value
  | otherwise = BS.pack [0x58, fromIntegral $ BS.length value] <> value

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient
