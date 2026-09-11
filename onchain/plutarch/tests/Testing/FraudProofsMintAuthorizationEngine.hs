{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsMintAuthorizationEngine (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.MintAuthorization.Engine
import Testing.Eval (passertEvalNoTrace, pfailsNoTraceWithoutHoistChecks)

tests :: TestTree
tests =
  testGroup
    "Mint-authorization engine"
    [ testCase "reads a canonical mint item" $
        passertEvalNoTrace $
          ppolicyIdOfMintItemV1 # item canonicalAssets #== policyA
    , testCase "reads a burn mint item" $
        passertEvalNoTrace $
          ppolicyIdOfMintItemV1 # item burnAssets #== policyA
    , testCase "rejects out-of-order asset names" $
        pfailsNoTraceWithoutHoistChecks $
          ppolicyIdOfMintItemV1 # item outOfOrderAssets
    , testCase "rejects an oversized asset name" $
        pfailsNoTraceWithoutHoistChecks $
          ppolicyIdOfMintItemV1 # item oversizedAssets
    , testCase "rejects trailing mint-item bytes" $
        pfailsNoTraceWithoutHoistChecks $
          ppolicyIdOfMintItemV1 # item trailingAssets
    , testCase "rejects an empty asset map" $
        pfailsNoTraceWithoutHoistChecks $
          ppolicyIdOfMintItemV1 # item emptyAssets
    , testCase "refuses past the node budget" $
        passertEvalNoTrace $
          pevaluateNativeScriptV1
            # nodeLimitPayload
            # pnil
            # (-1)
            # (-1)
            #== pcon PScriptNodeLimitV1
    , testCase "refuses trailing script bytes" $
        passertEvalNoTrace $
          pevaluateNativeScriptV1
            # trailingScript
            # pnil
            # (-1)
            # (-1)
            #== pcon PScriptMalformedV1
    , testCase "refuses an empty payload" $
        passertEvalNoTrace $
          pevaluateNativeScriptV1
            # pconstant ""
            # pnil
            # (-1)
            # (-1)
            #== pcon PScriptMalformedV1
    , testCase "refuses an unknown tag" $
        passertEvalNoTrace $
          pevaluateNativeScriptV1
            # hex "820700"
            # pnil
            # (-1)
            # (-1)
            #== pcon PScriptMalformedV1
    , testCase "finds timelocks unsatisfied without an interval" $
        passertEvalNoTrace $
          pevaluateNativeScriptV1
            # hex "820400"
            # pnil
            # (-1)
            # (-1)
            #== evaluated False
            #&& pevaluateNativeScriptV1
            # hex "820500"
            # pnil
            # (-1)
            # (-1)
            #== evaluated False
    , testCase "finds zero-of-zero satisfied" $
        passertEvalNoTrace $
          pevaluateNativeScriptV1
            # hex "83030080"
            # pnil
            # (-1)
            # (-1)
            #== evaluated True
    , testCase "evaluates nested containers" $
        passertEvalNoTrace $
          pevaluateNativeScriptV1
            # nestedPayload
            # (pcons # policyA # pnil)
            # (-1)
            # (-1)
            #== evaluated True
    ]

evaluated :: forall s. Bool -> Term s PNativeScriptVerdictV1
evaluated = pcon . PScriptEvaluatedV1 . pdata . pconstant

policyA, absentKey :: forall s. Term s PByteString
policyA = pconstant $ BS.replicate 28 0x42
absentKey = pconstant $ BS.replicate 28 0x99

item :: forall s. Term s PByteString -> Term s PByteString
item assets = hex "82581c" <> policyA <> assets

canonicalAssets, burnAssets, outOfOrderAssets, oversizedAssets, trailingAssets, emptyAssets :: forall s. Term s PByteString
canonicalAssets = hex "a241410142414202"
burnAssets = hex "a1414120"
outOfOrderAssets = hex "a242414202414101"
oversizedAssets = hex "a15821" <> pconstant (BS.replicate 33 0x41) <> hex "01"
trailingAssets = hex "a141410100"
emptyAssets = hex "a0"

signatureScript :: forall s. Term s PByteString -> Term s PByteString
signatureScript key = hex "8200581c" <> key

nodeLimitPayload, trailingScript, nestedPayload :: forall s. Term s PByteString
nodeLimitPayload = pconstant (BS.concat $ replicate 16_384 "\x82\x01\x81") <> signatureScript absentKey
trailingScript = signatureScript absentKey <> hex "00"
nestedPayload = hex "820282820181" <> signatureScript absentKey <> signatureScript policyA

hex :: forall s. BS.ByteString -> Term s PByteString
hex = pconstant . Base16.decodeLenient
