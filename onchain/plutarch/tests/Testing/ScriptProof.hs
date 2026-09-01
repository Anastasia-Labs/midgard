{-# LANGUAGE OverloadedStrings #-}

module Testing.ScriptProof (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils ((#/=))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardVersionedScript)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardExecutionUnits (..), PMidgardRedeemerPurpose (..), PMidgardRedeemerWitness (..),
  PMidgardScriptLanguage (..), PMidgardVersionedScript (..),
 )
import Midgard.ScriptProof
import Midgard.ValidationMerkle (pappendLeaf, pemptyFrontier, pfrontierCommitment)
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.ScriptProof"
  [ testCase "midgard_v1_script_tag_and_hash_prefix_vector_is_stable" $ passertEvalNoTrace midgardHashVector
  , testGroup "script_proof_source_and_redeemer_vectors_are_stable"
      [ vector "versioned script" (pversionedScriptHash # pscript) "8b8c11dcad0af38c40d742ed155b4c938acc5507a0ecbcfcea36496a"
      , vector "inline source" (psourceLeafHash # 0 # bytes "00" # pscript) "6b4984caceb70e0446b5d02a9b01068b7f409c76a54cf4e3b2b78f965b9eecc6"
      , vector "redeemer" (predeemerLeafHash # 0 # predeemer) "e42aed2342a26c9334ac80aea22c66b8f649cf6be5a5a4a70c6f33cbd8bda8ab"
      , vector "purpose" ppurposeLeaf "24c90c22834ab9ec656bee1db27d5421515010ec4c6b8928a63f65aecb5e367b"
      , vector "execution" (pexecutionLeafHash # 3 # ppurposeLeaf
          # (psourceLeafHash # 0 # bytes "00" # pscript) # (predeemerLeafHash # 0 # predeemer))
          "b5f8846e6888a5ebab1619bd6738ce3591323a63eb41fc06954729c5f4418d21"
      , vector "output" (poutputLeafHash # 2 # bytes "0102") "f9a3fa502da0ee4fe7048a78088ca4b89a72fe3aa0d313e12fbae7305e171727"
      , vector "output descriptor" (poutputDescriptorLeafHash # 2 # bytes "820102") "7d1979d9a1af11ab5b13ceeb3ae750046f31ea6ed7776f99cb2d41b253e87fd4"
      , vector "context item" (pcontextItemLeafHash # 0 # 2
          # bytes "66b39a3f329165f6ab15f249df38d5e8bc99230853ce5c16976b4780af1ed029" # 176 # 218)
          "2758fe3ec7c263c1630eab8cb4bb7f431cd403838a25b46bfe3848b0481daef3"
      , testCase "reference source agrees with descriptor hash" $ passertEvalNoTrace referenceSourceAgrees
      , testGroup "canonical reference key"
          [ testCase "accepts canonical" $ passertEvalNoTrace $ pcanonicalReferenceSourceKey # preferenceKey
          , testCase "rejects short transaction id" $ passertEvalNoTrace $ pnot # (pcanonicalReferenceSourceKey # pshortReferenceKey)
          , testCase "rejects trailing bytes" $ passertEvalNoTrace $ pnot # (pcanonicalReferenceSourceKey # (preferenceKey <> bytes "00"))
          , testCase "rejects nonminimal index" $ passertEvalNoTrace $ pnot # (pcanonicalReferenceSourceKey # pnonminimalReferenceKey)
          , testCase "rejects uint16 overflow" $ passertEvalNoTrace $ pnot # (pcanonicalReferenceSourceKey # poverflowReferenceKey)
          ]
      ]
  , testCase "signer_leaf_and_seven_leaf_frontier_root_match_typescript" $ passertEvalNoTrace signerFrontierVector
  , testCase "inline_script_source_binds_raw_field_six_and_rejects_field_seven" $ passertEvalNoTrace inlineSourceBindsFieldSix
  , testCase "redeemer_purpose_tags_are_stable" $ passertEvalNoTrace purposeTagsStable
  ]

vector :: String -> (forall s. Term s PByteString) -> BS.ByteString -> TestTree
vector name actual expected = testCase name $ passertEvalNoTrace $ actual #== bytes expected

midgardHashVector :: forall s. Term s PBool
midgardHashVector = planguageTag # pcon PMidgardV1Script #== 128
  #&& pversionedScriptHash # pcon (PMidgardVersionedScript (pdata $ pcon PMidgardV1Script) (pdata $ bytes "010203"))
    #== bytes "760b621a49505853e1f4562d126e185f78483932825e0fb077a1ed80"

pscript :: forall s. Term s PMidgardVersionedScript
pscript = pcon $ PMidgardVersionedScript (pdata $ pcon PPlutusV3Script) (pdata $ bytes "010203")

predeemer :: forall s. Term s PMidgardRedeemerWitness
predeemer = pcon $ PMidgardRedeemerWitness
  (pdata $ pcon PSpendRedeemer) (pdata 2) (pdata $ bytes "d87980")
  (pdata $ pcon $ PMidgardExecutionUnits (pdata 3) (pdata 4))

ppurposeLeaf :: forall s. Term s PByteString
ppurposeLeaf = ppurposeLeafHash # 0 # 2 # (pversionedScriptHash # pscript) # bytes "0102"

preferenceKey :: forall s. Term s PByteString
preferenceKey = bytes "825820444444444444444444444444444444444444444444444444444444444444444402"

pshortReferenceKey, pnonminimalReferenceKey, poverflowReferenceKey :: forall s. Term s PByteString
pshortReferenceKey = bytes "82581f4444444444444444444444444444444444444444444444444444444444444402"
pnonminimalReferenceKey = bytes "82582044444444444444444444444444444444444444444444444444444444444444441802"
poverflowReferenceKey = bytes "82582044444444444444444444444444444444444444444444444444444444444444441a00010000"

referenceSourceAgrees :: forall s. Term s PBool
referenceSourceAgrees = plet (pencodeMidgardVersionedScript # pscript) $ \scriptCbor ->
  psourceLeafHash # 1 # preferenceKey # pscript
    #== preferenceSourceLeafHash # preferenceKey # 3 # (pversionedScriptHash # pscript)
      # (plengthBS # scriptCbor) # (Bounded.pfromBytes # 2 # 2 # scriptCbor)

signerFrontierVector :: forall s. Term s PBool
signerFrontierVector = plet (psignerLeafHash # bytes "11111111111111111111111111111111111111111111111111111111") $ \signerLeaf ->
  plet (pappendLeaf # 6
      # (pappendLeaf # 5
        # (pappendLeaf # 4
          # (pappendLeaf # 3
            # (pappendLeaf # 2
              # (pappendLeaf # 1
                # (pappendLeaf # 0 # pemptyFrontier # bytes "6b4984caceb70e0446b5d02a9b01068b7f409c76a54cf4e3b2b78f965b9eecc6")
                # bytes "e42aed2342a26c9334ac80aea22c66b8f649cf6be5a5a4a70c6f33cbd8bda8ab")
              # bytes "24c90c22834ab9ec656bee1db27d5421515010ec4c6b8928a63f65aecb5e367b")
            # bytes "b5f8846e6888a5ebab1619bd6738ce3591323a63eb41fc06954729c5f4418d21")
          # signerLeaf)
        # bytes "f9a3fa502da0ee4fe7048a78088ca4b89a72fe3aa0d313e12fbae7305e171727")
      # bytes "2758fe3ec7c263c1630eab8cb4bb7f431cd403838a25b46bfe3848b0481daef3") $ \frontier ->
    signerLeaf #== bytes "9e4bab3a1b4ca49640fe5c54486aac6a1183fb7da45eec6b30d46382d8f3418b"
      #&& pfrontierCommitment # 7 # frontier #== bytes "3f81171ae98f8745f125cbe28461e23204fe4f39e609e9ac4be7537b9dac126f"

inlineSourceBindsFieldSix :: forall s. Term s PBool
inlineSourceBindsFieldSix = plet (pencodeMidgardVersionedScript # pscript) $ \scriptCbor ->
  plet (pversionedScriptHash # pscript) $ \scriptHash ->
  plet (psourceLeafHash # 0 # bytes "00" # pscript) $ \canonical ->
    canonical #== pinlineSourceLeafHash # 0 # 3 # scriptHash # (plengthBS # scriptCbor)
      # (Bounded.pfromBytes # 6 # 0 # scriptCbor)
      #&& canonical #/= pinlineSourceLeafHash # 0 # 3 # scriptHash # (plengthBS # scriptCbor)
        # (Bounded.pfromBytes # 7 # 0 # scriptCbor)

purposeTagsStable :: forall s. Term s PBool
purposeTagsStable = predeemerPurposeTag # pcon PSpendRedeemer #== 0
  #&& predeemerPurposeTag # pcon PMintRedeemer #== 1
  #&& predeemerPurposeTag # pcon PCertRedeemer #== 2
  #&& predeemerPurposeTag # pcon PRewardRedeemer #== 3
  #&& predeemerPurposeTag # pcon PVoteRedeemer #== 4
  #&& predeemerPurposeTag # pcon PProposeRedeemer #== 5
  #&& predeemerPurposeTag # pcon PReceiveRedeemer #== 6

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
