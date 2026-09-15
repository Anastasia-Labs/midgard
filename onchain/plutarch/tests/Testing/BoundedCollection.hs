{-# LANGUAGE OverloadedStrings #-}

module Testing.BoundedCollection (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.BoundedCollection
import Midgard.ValidationMerkle (PFrontierPeak (..))
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.BoundedCollection"
  [ testCase "canonical_cross_language_collection_vector" $
      passertEvalNoTrace canonicalCrossLanguageCollectionVector
  , testCase "exact_item_membership_is_accepted" $
      passertEvalNoTrace $ pverifyBoundedCollectionItem # collectionCommitment # itemProof 2
  , testCase "cross_field_replay_fails_closed" $
      passertEvalNoTrace $ pnot # (pverifyBoundedCollectionItem # collectionCommitment # itemProof 1)
  ]

canonicalCrossLanguageCollectionVector :: forall s. Term s PBool
canonicalCrossLanguageCollectionVector =
  phashBoundedCollectionItem # 2 # 0 # 1 # itemA #== leafA
    #&& phashBoundedCollectionItem # 2 # 1 # 2 # itemBb #== leafBb
    #&& phashBoundedCollectionItem # 2 # 2 # 3 # itemCcc #== leafCcc
    #&& pboundedCollectionCommitment # 2 # 3 # frontier #== collectionCommitment

itemProof :: forall s. Term s PInteger -> Term s PItemProofV1
itemProof fieldIndex = pcon $ PItemProofV1
  (pdata pboundedCollectionVersion)
  (pdata fieldIndex)
  (pdata 3)
  (pdata 2)
  (pdata 3)
  (pdata itemCcc)
  (pdata frontier)
  (pdata pnil)

frontier :: forall s. Term s (PBuiltinList (PAsData PFrontierPeak))
frontier =
  pcons # pdata (pcon $ PFrontierPeak (pdata 0) (pdata leafCcc))
    #$ pcons # pdata (pcon $ PFrontierPeak (pdata 1) (pdata pairAb)) # pnil

leafA, leafBb, leafCcc, pairAb, collectionCommitment, itemA, itemBb, itemCcc ::
  forall s. Term s PByteString
leafA = bytes "252f1f362ac201ba6803bcd0a22c3295bc562721b5e518caa65c966d306d4589"
leafBb = bytes "c06b6c3d255adb5089eb6599c97027cec2b48f04e4b7bf6af3c6d2db2c7c95ab"
leafCcc = bytes "4224350be47eecf833606847b9419d5e0555ac664ee7e9506c94ca516a799cad"
pairAb = bytes "c457e03d9d60de55d41e221961f6214696ad2fe8adb62e810ec7adc2feba1a54"
collectionCommitment = bytes "6b79e49eabb6aaa1e49ea511db08d438c16c022292394f5d782b7ab6cd6713d6"
itemA = bytes "c4e238f2d3c5c3b535341bfcb66f449e2170ed3506d1f15bf7d496aa3dca97a9"
itemBb = bytes "d0a6f8f5a8dfc2a6455d90dd6be9f18fbab15e0a9aef55003cb3af5e61404864"
itemCcc = bytes "330b2e4f898b5dc2e191c9479ec5834c3109359bfd58ba7de6f9a8cd04119b93"

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
