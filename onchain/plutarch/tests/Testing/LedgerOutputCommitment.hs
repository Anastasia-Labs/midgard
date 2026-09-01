{-# LANGUAGE OverloadedStrings #-}

module Testing.LedgerOutputCommitment (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Test.Tasty
import Test.Tasty.HUnit
import Plutarch.Prelude

import Midgard.BoundedItem (PChunkProofV1 (..), pversion)
import Midgard.CekData (PDataSummaryV1 (..))
import Midgard.LedgerOutputCommitment
import Midgard.ValidationMerkle (PFrontierPeak (..), pappendLeaf, pfrontierCommitment)
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests = testGroup "Midgard.LedgerOutputCommitment"
  [ testCase "canonical_cross_language_descriptor_vector" $ passertEvalNoTrace canonicalDescriptorVector
  , testCase "canonical_cross_language_asset_frontier_vector" $ passertEvalNoTrace canonicalAssetFrontierVector
  , testCase "every_bounded_output_chunk_is_authenticated" $ passertEvalNoTrace everyBoundedOutputChunkIsAuthenticated
  , testCase "descriptor_substitution_fails_closed" $ passertEvalNoTrace descriptorSubstitutionFailsClosed
  , testCase "exact_cardano_value_size_bound_is_supported" $ passertEvalNoTrace exactCardanoValueSizeBoundIsSupported
  , testCase "cardano_value_above_mainnet_bound_fails_closed" $
      pfails $ pencodeLedgerOutputCommitment # descriptorFixture (pmaxCardanoValueCborBytes + 1) 7
  ]

canonicalDescriptorVector :: forall s. Term s PBool
canonicalDescriptorVector = plet (descriptorFixture 5 7) $ \descriptor ->
  poutputItemCommitment # 7 # (preplicateBS # 5_000 # (pintegerToByte # 90)) #== itemCommitment
    #&& pencodeLedgerOutputCommitment # descriptor #== expectedDescriptor
    #&& pdecodeLedgerOutputCommitment # expectedDescriptor #== descriptor

canonicalAssetFrontierVector :: forall s. Term s PBool
canonicalAssetFrontierVector =
  plet (passetLeafHash # (preplicateBS # 28 # (pintegerToByte # 85)) # pconstant "\x01\x02" # 42) $ \longerName ->
  plet (passetLeafHash # (preplicateBS # 28 # (pintegerToByte # 85)) # pconstant "\x03" # 7) $ \shorterName ->
  plet (pappendLeaf # 0 # pnil # shorterName) $ \firstPeaks ->
  plet (pappendLeaf # 1 # firstPeaks # longerName) $ \peaks ->
    longerName #== bytes "9ed4bad2bc3c66d009d021f8fa57eb32d6035c71aa6f1c415e8f66bf35c6661c"
      #&& shorterName #== bytes "d4fab6956ed316d6c8094e2f8ba31c07f9b8e11d6b9ed1236541a102187a58ce"
      #&& pfrontierCommitment # 2 # peaks #== bytes "1af1c3eeb6379f3bb3dac82d19faffa1318a2a64f739b38c5fc5640b58196cc5"

everyBoundedOutputChunkIsAuthenticated :: forall s. Term s PBool
everyBoundedOutputChunkIsAuthenticated = plet (pdecodeLedgerOutputCommitment # expectedDescriptor) $ \descriptor ->
  pverifyOutputChunk # descriptor # chunkProof 0 4_095 secondChunkHash
    #&& pverifyOutputChunk # descriptor # chunkProof 1 905 firstChunkHash

descriptorSubstitutionFailsClosed :: forall s. Term s PBool
descriptorSubstitutionFailsClosed =
  pnot # (pverifyOutputChunk # descriptorFixture 5 8 # chunkProof 0 4_095 secondChunkHash)

exactCardanoValueSizeBoundIsSupported :: forall s. Term s PBool
exactCardanoValueSizeBoundIsSupported = plet (descriptorFixture pmaxCardanoValueCborBytes 7) $ \descriptor ->
  pdecodeLedgerOutputCommitment # (pencodeLedgerOutputCommitment # descriptor) #== descriptor

descriptorFixture :: forall s. Term s PInteger -> Term s PInteger -> Term s PLedgerOutputCommitmentV1
descriptorFixture cardanoValueSize outputIndex = pcon $ PLedgerOutputCommitmentV1
  (pdata pledgerOutputCommitmentVersion) (pdata outputIndex) (pdata 5_000) (pdata itemCommitment)
  (pdata $ bytes "6011111111111111111111111111111111111111111111111111111111")
  (pdata 5_000_000) (pdata 0)
  (pdata $ bytes "b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd")
  (pdata cardanoValueSize) (pdata $ -1) (pdata $ pconstant "") (pdata 0) (pdata $ pconstant "")
  (pdata $ summary 0x22 101 202) (pdata $ summary 0x33 103 204) (pdata $ summary 0x44 3 4)

summary :: forall s. Word -> Term s PInteger -> Term s PInteger -> Term s PDataSummaryV1
summary byte cborLength memory = pcon $ PDataSummaryV1
  (pdata $ pconstant $ BS.replicate 32 $ fromIntegral byte) (pdata cborLength) (pdata memory)

chunkProof :: forall s. Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PChunkProofV1
chunkProof chunkIndex chunkLength sibling = pcon $ PChunkProofV1
  (pdata pversion) (pdata poutputFieldIndex) (pdata 7) (pdata 5_000) (pdata chunkIndex)
  (pdata $ preplicateBS # chunkLength # (pintegerToByte # 90)) (pdata outputFrontier) (pdata $ pcons # pdata sibling # pnil)

outputFrontier :: forall s. Term s (PBuiltinList (PAsData PFrontierPeak))
outputFrontier = pcons # pdata
  (pcon $ PFrontierPeak (pdata 1) (pdata $ bytes "2ef9190c09f85ce5dd740c0b009582d71f6fcf7f8045daafbb88e9df13028704")) # pnil

expectedDescriptor, firstChunkHash, secondChunkHash, itemCommitment :: forall s. Term s PByteString
expectedDescriptor = bytes "900107191388582013e167684e9dc284acc6ebbe972cd2cf0763d03bba558bae463825f3f35990d6581d60111111111111111111111111111111111111111111111111111111111a004c4b40005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358202222222222222222222222222222222222222222222222222222222222222222186518ca8358203333333333333333333333333333333333333333333333333333333333333333186718cc83582044444444444444444444444444444444444444444444444444444444444444440304"
firstChunkHash = bytes "cace1f183183522da906fabee1bafe52ab2e01a31d719845b0e055a23f1b1061"
secondChunkHash = bytes "b7d94afeaec000c32c2354c892e3e83989bc98985ade659f423d39da45913334"
itemCommitment = bytes "13e167684e9dc284acc6ebbe972cd2cf0763d03bba558bae463825f3f35990d6"

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
