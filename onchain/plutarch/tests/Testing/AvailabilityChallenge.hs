{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.AvailabilityChallenge
Description : Aiken-parity tests for @lib/midgard/availability-challenge.ak@.
-}
module Testing.AvailabilityChallenge (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PTokenName (..))
import Plutarch.Prelude
import PlutusLedgerApi.V1.Value (TokenName (..))
import PlutusLedgerApi.V3 (PubKeyHash (..), TxId (..), TxOutRef (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.AvailabilityChallenge
import Midgard.ValidationMerkle qualified as ValidationMerkle
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Availability Challenge Library"
    [ assetNameTests
    , responseTimingTests
    , geometryTests
    , parameterTests
    , commitmentTests
    , reserveTests
    , accumulatorTests
    , publicationTests
    ]

assetNameTests :: TestTree
assetNameTests =
  testGroup
    "asset names"
    [ testCase "bond and challenge names are distinct domain-prefixed 32-byte names" $
        passertEval $
          let bondName = pto $ pdaBondAssetNameV1 (pconstant fixtureOutRef)
              challengeName = pto $ pchallengeAssetNameV1 (pconstant fixtureOutRef)
           in pand'List
                [ plengthBS # bondName #== 32
                , plengthBS # challengeName #== 32
                , psliceBS # 0 # 4 # bondName #== pconstant "DABN"
                , psliceBS # 0 # 4 # challengeName #== pconstant "DACH"
                , psliceBS # 4 # 28 # bondName #== psliceBS # 4 # 28 # challengeName
                ]
    , testCase "tranche indices 0, 1 and 63 match the target little-endian wire" $
        passertEval $
          let challengeName = pchallengeAssetNameV1 (pconstant fixtureOutRef)
              tranche0 = pto $ ptrancheAssetNameV1 challengeName 0
              tranche1 = pto $ ptrancheAssetNameV1 challengeName 1
              tranche63 = pto $ ptrancheAssetNameV1 challengeName 63
           in pand'List
                [ plengthBS # tranche0 #== 32
                , psliceBS # 0 # 2 # tranche0 #== pconstant "DT"
                , psliceBS # 30 # 2 # tranche0 #== pconstant "\NUL\NUL"
                , psliceBS # 30 # 2 # tranche1 #== pconstant "\SOH\NUL"
                , psliceBS # 30 # 2 # tranche63 #== pconstant "?\NUL"
                ]
    , testCase "tranche index -1 is rejected" $
        pfails $ ptrancheAssetNameV1 (pchallengeAssetNameV1 $ pconstant fixtureOutRef) (-1)
    , testCase "tranche index 64 is rejected" $
        pfails $ ptrancheAssetNameV1 (pchallengeAssetNameV1 $ pconstant fixtureOutRef) 64
    , testCase "terminal accumulator preserves the challenge hash suffix" $
        passertEval $
          let challengeName = pto $ pchallengeAssetNameV1 (pconstant fixtureOutRef)
              terminalName = pto $ pterminalAccumulatorAssetNameV1 (pcon $ PTokenName challengeName)
           in terminalName #== pconstant "DACT" <> psliceBS # 4 # 28 # challengeName
    , testCase "derived names reject a malformed challenge asset name" $
        pfails $ pterminalAccumulatorAssetNameV1 (pconstant $ TokenName "short")
    ]

responseTimingTests :: TestTree
responseTimingTests =
  testGroup
    "response timing"
    [ testCase "non-positive payloads have no response window" $
        passertEval $
          presponseWindowMsV1 0 #== pcon PNothing
            #&& presponseWindowMsV1 (-1) #== pcon PNothing
    , testCase "one byte and 64 KiB use the one-hour window" $
        passertEval $
          presponseWindowMsV1 1 #== pcon (PJust psmallResponseWindowMsV1)
            #&& presponseWindowMsV1 65_536 #== pcon (PJust psmallResponseWindowMsV1)
    , testCase "64 KiB plus one and 64 MiB use the 48-hour window" $
        passertEval $
          presponseWindowMsV1 65_537 #== pcon (PJust pfullResponseWindowMsV1)
            #&& presponseWindowMsV1 67_108_864 #== pcon (PJust pfullResponseWindowMsV1)
    , testCase "payloads above 64 MiB have no response window" $
        passertEval $ presponseWindowMsV1 67_108_865 #== pcon PNothing
    , testCase "deadline adds the selected window" $
        passertEval $
          presponseDeadlineV1 1 200 #== pcon (PJust $ 200 + psmallResponseWindowMsV1)
            #&& presponseDeadlineV1 65_537 200 #== pcon (PJust $ 200 + pfullResponseWindowMsV1)
    , testCase "negative opening time has no deadline" $
        passertEval $ presponseDeadlineV1 1 (-1) #== pcon PNothing
    ]

geometryTests :: TestTree
geometryTests =
  testGroup
    "canonical geometry"
    [ testCase "accepts the authenticated 16-tranche geometry" $
        passertEval $ presponseGeometryIsCanonicalV1 # canonicalGeometry
    , testCase "rejects non-positive and oversized chunks" $
        passertEval $
          pnot # (presponseGeometryIsCanonicalV1 # geometry 0 trancheBytes 16)
            #&& pnot # (presponseGeometryIsCanonicalV1 # geometry 15_149 trancheBytes 16)
    , testCase "rejects tranche lengths outside the payload bounds" $
        passertEval $
          pnot # (presponseGeometryIsCanonicalV1 # geometry chunkBytes 65_535 64)
            #&& pnot # (presponseGeometryIsCanonicalV1 # geometry chunkBytes 67_108_865 1)
    , testCase "rejects zero or more than 64 tranches" $
        passertEval $
          pnot # (presponseGeometryIsCanonicalV1 # geometry chunkBytes trancheBytes 0)
            #&& pnot # (presponseGeometryIsCanonicalV1 # geometry chunkBytes trancheBytes 65)
    , testCase "rejects a tranche count that cannot cover 64 MiB" $
        passertEval $ pnot # (presponseGeometryIsCanonicalV1 # geometry chunkBytes trancheBytes 15)
    ]

parameterTests :: TestTree
parameterTests =
  testGroup
    "canonical parameters"
    [ testCase "accepts the exact test fixture fee budget" $
        passertEval $ pparametersAreCanonicalV1 # canonicalParameters
    , testCase "requires equal positive DA and challenger bonds" $
        passertEval $
          pnot # (pparametersAreCanonicalV1 # parameters 0 0 500_000 500_000 500_000 1_000_000 1_200_000)
            #&& pnot # (pparametersAreCanonicalV1 # parameters 10_000_000_000 9_999_999_999 500_000 500_000 500_000 1_000_000 1_200_000)
    , testCase "requires every fee limit to be positive" $
        passertEval $
          pnot # (pparametersAreCanonicalV1 # parameters 10_000_000_000 10_000_000_000 0 500_000 500_000 1_000_000 1_200_000)
            #&& pnot # (pparametersAreCanonicalV1 # parameters 10_000_000_000 10_000_000_000 500_000 0 500_000 1_000_000 1_200_000)
            #&& pnot # (pparametersAreCanonicalV1 # parameters 10_000_000_000 10_000_000_000 500_000 500_000 0 1_000_000 1_200_000)
            #&& pnot # (pparametersAreCanonicalV1 # parameters 10_000_000_000 10_000_000_000 500_000 500_000 500_000 0 1_200_000)
            #&& pnot # (pparametersAreCanonicalV1 # parameters 10_000_000_000 10_000_000_000 500_000 500_000 500_000 1_000_000 0)
    , testCase "requires both worst-case fee reserves to remain below the bond" $
        passertEval $
          pnot # (pparametersAreCanonicalV1 # parameters 1_000_000 1_000_000 1 500_000 500_000 1 1)
    ]

commitmentTests :: TestTree
commitmentTests =
  testGroup
    "canonical commitment"
    [ testCase "accepts the exact one-byte commitment" $
        passertEval $ pcommitmentIsCanonicalV1 canonicalCommitment canonicalParameters
    , testCase "rejects wrong version or malformed identities" $
        passertEval $
          pnot # pcommitmentIsCanonicalV1 (commitment 0 identity28 hash28 owner28 1 canonicalGeometry canonicalDescriptors) canonicalParameters
            #&& pnot # pcommitmentIsCanonicalV1 (commitment 1 identity27 hash28 owner28 1 canonicalGeometry canonicalDescriptors) canonicalParameters
            #&& pnot # pcommitmentIsCanonicalV1 (commitment 1 identity28 hash27 owner28 1 canonicalGeometry canonicalDescriptors) canonicalParameters
            #&& pnot # pcommitmentIsCanonicalV1 (commitment 1 identity28 hash28 owner27 1 canonicalGeometry canonicalDescriptors) canonicalParameters
    , testCase "rejects an unsupported payload or substituted geometry" $
        passertEval $
          pnot # pcommitmentIsCanonicalV1 (commitment 1 identity28 hash28 owner28 0 canonicalGeometry canonicalDescriptors) canonicalParameters
            #&& pnot # pcommitmentIsCanonicalV1 (commitment 1 identity28 hash28 owner28 1 (geometry 14_021 trancheBytes 16) canonicalDescriptors) canonicalParameters
    , testCase "rejects missing, gapped, or miscounted descriptors" $
        passertEval $
          pnot # pcommitmentIsCanonicalV1 (commitment 1 identity28 hash28 owner28 1 canonicalGeometry pnil) canonicalParameters
            #&& pnot # pcommitmentIsCanonicalV1 (commitment 1 identity28 hash28 owner28 1 canonicalGeometry $ singletonDescriptor 1 0 1 1 hash32 hash32) canonicalParameters
            #&& pnot # pcommitmentIsCanonicalV1 (commitment 1 identity28 hash28 owner28 1 canonicalGeometry $ singletonDescriptor 0 1 1 1 hash32 hash32) canonicalParameters
            #&& pnot # pcommitmentIsCanonicalV1 (commitment 1 identity28 hash28 owner28 1 canonicalGeometry $ singletonDescriptor 0 0 1 2 hash32 hash32) canonicalParameters
    , testCase "rejects malformed descriptor commitments" $
        passertEval $
          pnot # pcommitmentIsCanonicalV1 (commitment 1 identity28 hash28 owner28 1 canonicalGeometry $ singletonDescriptor 0 0 1 1 hash31 hash32) canonicalParameters
            #&& pnot # pcommitmentIsCanonicalV1 (commitment 1 identity28 hash28 owner28 1 canonicalGeometry $ singletonDescriptor 0 0 1 1 hash32 hash31) canonicalParameters
    ]

reserveTests :: TestTree
reserveTests =
  testGroup
    "bond reserve allocation"
    [ testCase "terminal reserve is the larger close-or-timeout fee" $
        passertEval $
          pterminalAccumulatorReserveLovelaceV1 canonicalParameters #== 1_200_000
            #&& pterminalAccumulatorReserveLovelaceV1 (parameters 10_000_000_000 10_000_000_000 500_000 500_000 500_000 1_300_000 1_200_000) #== 1_300_000
    , testCase "one tranche receives its publication, settlement, and distributable reserves" $
        passertEval $
          ptrancheInitialLovelacesV1 canonicalCommitment canonicalParameters
            #== pcon (PJust $ pcons # 9_998_800_000 # pnil)
    , testCase "indexed allocation is total at both ends" $
        passertEval $
          ptrancheInitialLovelaceV1 canonicalCommitment canonicalParameters 0 #== pcon (PJust 9_998_800_000)
            #&& ptrancheInitialLovelaceV1 canonicalCommitment canonicalParameters 1 #== pcon PNothing
            #&& ptrancheInitialLovelaceV1 canonicalCommitment canonicalParameters (-1) #== pcon PNothing
    , testCase "invalid commitments have no tranche allocation" $
        passertEval $
          ptrancheInitialLovelacesV1 (commitment 0 identity28 hash28 owner28 1 canonicalGeometry canonicalDescriptors) canonicalParameters
            #== pcon PNothing
    , testCase "canonical commitment reserves the terminal fee exactly" $
        passertEval $
          pterminalAccumulatorInitialLovelaceV1 canonicalCommitment canonicalParameters
            #== pcon (PJust 1_200_000)
    ]

accumulatorTests :: TestTree
accumulatorTests =
  testGroup
    "domain-separated accumulators"
    [ testCase "maximum first leaf matches the Aiken cross-language vector" $
        passertEval $
          let chunk = pconstant maximumChunk
           in pchunkLeafHashV1 0 0 0 14_020 (pblake2b_256 # chunk)
                #== pconstant maximumLeafHash
    , testCase "attestation and published commitment domains are distinct" $
        passertEval $
          let attestation = pattestationMessageV1 canonicalCommitment
              published = ppublishedTerminalCommitmentV1 canonicalCommitment
           in plengthBS # attestation #== 32
                #&& plengthBS # published #== 32
                #&& pnot # (attestation #== published)
    , testCase "terminal start and tranche start produce 32-byte accumulators" $
        passertEval $
          plengthBS # pterminalAccumulatorStartV1 canonicalCommitment challengeName #== 32
            #&& plengthBS # ptrancheStartAccumulatorV1 (pconstant identity28) (pconstant hash28) canonicalDescriptor #== 32
    , testCase "terminal fold commits to the terminal status" $
        passertEval $
          let published = pcon $ PPublishedTranche (pdata $ pconstant hash32)
              timedOut = pcon $ PTimedOutTranche (pdata 1) (pdata $ pconstant hash32)
           in pnot # (pfoldTerminalAccumulatorV1 (pconstant hash32) 0 published #== pfoldTerminalAccumulatorV1 (pconstant hash32) 0 timedOut)
    , testCase "tranche step commits to chunk bytes and previous accumulator" $
        passertEval $
          let first = ptrancheStepAccumulatorV1 (pconstant identity28) (pconstant hash28) 0 0 (pconstant "a") (pconstant hash32)
              changedChunk = ptrancheStepAccumulatorV1 (pconstant identity28) (pconstant hash28) 0 0 (pconstant "b") (pconstant hash32)
              changedPrevious = ptrancheStepAccumulatorV1 (pconstant identity28) (pconstant hash28) 0 0 (pconstant "a") (pconstant hash31)
           in plengthBS # first #== 32
                #&& pnot # (first #== changedChunk)
                #&& pnot # (first #== changedPrevious)
    ]

publicationTests :: TestTree
publicationTests =
  testGroup
    "publication advancement"
    [ testCase "a committed final chunk advances Active to Receipt" $
        passertEval $
          ppublicationAdvancesActiveTrancheV1 oneChunkActive oneChunkPublication 1 7
            #== pcon (PJust oneChunkReceipt)
    , testCase "a committed non-final chunk advances Active to Active" $
        passertEval $
          ppublicationAdvancesActiveTrancheV1 twoChunkActive firstOfTwoPublication 1 7
            #== pcon (PJust firstOfTwoAdvanced)
    , testCase "a Receipt cannot be advanced" $
        passertEval $
          isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkReceipt oneChunkPublication 1 7
    , testCase "rejects a substituted identity, header, or challenge" $
        passertEval $
          pand'List
            [ isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity27) (pconstant hash28) challengeName 0 0 0 1 (pconstant "\x01") oneChunkHash oneChunkStart oneChunkNext oneChunkFrontier pnil) 1 7
            , isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity28) (pconstant hash27) challengeName 0 0 0 1 (pconstant "\x01") oneChunkHash oneChunkStart oneChunkNext oneChunkFrontier pnil) 1 7
            , isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity28) (pconstant hash28) otherChallengeName 0 0 0 1 (pconstant "\x01") oneChunkHash oneChunkStart oneChunkNext oneChunkFrontier pnil) 1 7
            ]
    , testCase "rejects a substituted tranche position or declared length" $
        passertEval $
          pand'List
            [ isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity28) (pconstant hash28) challengeName 1 0 0 1 (pconstant "\x01") oneChunkHash oneChunkStart oneChunkNext oneChunkFrontier pnil) 1 7
            , isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity28) (pconstant hash28) challengeName 0 1 0 1 (pconstant "\x01") oneChunkHash oneChunkStart oneChunkNext oneChunkFrontier pnil) 1 7
            , isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity28) (pconstant hash28) challengeName 0 0 1 1 (pconstant "\x01") oneChunkHash oneChunkStart oneChunkNext oneChunkFrontier pnil) 1 7
            , isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity28) (pconstant hash28) challengeName 0 0 0 2 (pconstant "\x01") oneChunkHash oneChunkStart oneChunkNext oneChunkFrontier pnil) 1 7
            ]
    , testCase "rejects altered chunk bytes, hash, or accumulator chain" $
        passertEval $
          pand'List
            [ isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity28) (pconstant hash28) challengeName 0 0 0 1 (pconstant "\x02") oneChunkHash oneChunkStart oneChunkNext oneChunkFrontier pnil) 1 7
            , isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity28) (pconstant hash28) challengeName 0 0 0 1 (pconstant "\x01") (pconstant hash32) oneChunkStart oneChunkNext oneChunkFrontier pnil) 1 7
            , isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity28) (pconstant hash28) challengeName 0 0 0 1 (pconstant "\x01") oneChunkHash (pconstant hash32) oneChunkNext oneChunkFrontier pnil) 1 7
            , isNoAdvance $ ppublicationAdvancesActiveTrancheV1 oneChunkActive (oneChunkPublicationWith (pconstant identity28) (pconstant hash28) challengeName 0 0 0 1 (pconstant "\x01") oneChunkHash oneChunkStart (pconstant hash32) oneChunkFrontier pnil) 1 7
            ]
    , testCase "rejects a malformed Merkle proof" $
        passertEval $
          isNoAdvance $
            ppublicationAdvancesActiveTrancheV1
              oneChunkActive
              (oneChunkPublicationWith (pconstant identity28) (pconstant hash28) challengeName 0 0 0 1 (pconstant "\x01") oneChunkHash oneChunkStart oneChunkNext oneChunkFrontier (pcons # pdata (pconstant hash32) # pnil))
              1
              7
    , testCase "rejects a negative carrier output index or wrong final accumulator" $
        passertEval $
          isNoAdvance (ppublicationAdvancesActiveTrancheV1 oneChunkActive oneChunkPublication 1 (-1))
            #&& isNoAdvance (ppublicationAdvancesActiveTrancheV1 wrongTerminalActive oneChunkPublication 1 7)
    ]

isNoAdvance :: forall s. Term s (PMaybe PTrancheDatumV1) -> Term s PBool
isNoAdvance result = pmatch result $ \case
  PNothing -> pconstant True
  PJust _ -> pconstant False

oneChunkHash :: forall s. Term s PByteString
oneChunkHash = pblake2b_256 # pconstant "\x01"

oneChunkLeaf :: forall s. Term s PByteString
oneChunkLeaf = pchunkLeafHashV1 0 0 0 1 oneChunkHash

oneChunkFrontier :: forall s. Term s (PBuiltinList (PAsData ValidationMerkle.PFrontierPeak))
oneChunkFrontier =
  pcons
    # pdata (pcon $ ValidationMerkle.PFrontierPeak (pdata 0) (pdata oneChunkLeaf))
    # pnil

descriptorTerm ::
  forall s.
  Integer -> Integer -> Integer -> Integer -> Term s PByteString -> Term s PByteString -> Term s PTrancheDescriptorV1
descriptorTerm index offset byteLength chunkCount chunkCommitment terminalAccumulator =
  pcon $
    PTrancheDescriptorV1
      (pdata $ pconstant index)
      (pdata $ pconstant offset)
      (pdata $ pconstant byteLength)
      (pdata $ pconstant chunkCount)
      (pdata chunkCommitment)
      (pdata terminalAccumulator)

oneChunkDescriptorShape :: forall s. Term s PTrancheDescriptorV1
oneChunkDescriptorShape = descriptorTerm 0 0 1 1 (pconstant hash32) (pconstant hash32)

oneChunkStart :: forall s. Term s PByteString
oneChunkStart = ptrancheStartAccumulatorV1 (pconstant identity28) (pconstant hash28) oneChunkDescriptorShape

oneChunkNext :: forall s. Term s PByteString
oneChunkNext = ptrancheStepAccumulatorV1 (pconstant identity28) (pconstant hash28) 0 0 (pconstant "\x01") oneChunkStart

oneChunkDescriptor :: forall s. Term s PTrancheDescriptorV1
oneChunkDescriptor =
  descriptorTerm
    0
    0
    1
    1
    (ValidationMerkle.pfrontierCommitment # 1 # oneChunkFrontier)
    oneChunkNext

activeWithDescriptor :: forall s. Term s PTrancheDescriptorV1 -> Term s PByteString -> Term s PTrancheDatumV1
activeWithDescriptor selectedDescriptor accumulator =
  pcon $
    PActiveTranche
      (pdata $ pconstant identity28)
      (pdata $ pconstant hash28)
      (pdata challengeName)
      (pdata selectedDescriptor)
      (pdata 0)
      (pdata accumulator)
      (pdata $ pcon PDNothing)
      (pdata psmallResponseWindowMsV1)
      (pdata $ pconstant owner28)

oneChunkActive :: forall s. Term s PTrancheDatumV1
oneChunkActive = activeWithDescriptor oneChunkDescriptor oneChunkStart

wrongTerminalActive :: forall s. Term s PTrancheDatumV1
wrongTerminalActive =
  activeWithDescriptor
    (descriptorTerm 0 0 1 1 (ValidationMerkle.pfrontierCommitment # 1 # oneChunkFrontier) (pconstant hash32))
    oneChunkStart

oneChunkReceipt :: forall s. Term s PTrancheDatumV1
oneChunkReceipt =
  pcon $
    PReceipt
      (pdata $ pconstant identity28)
      (pdata $ pconstant hash28)
      (pdata challengeName)
      (pdata oneChunkDescriptor)
      (pdata oneChunkNext)
      (pdata 7)
      (pdata $ pconstant owner28)

oneChunkPublication :: forall s. Term s PPublicationDatumV1
oneChunkPublication =
  oneChunkPublicationWith
    (pconstant identity28)
    (pconstant hash28)
    challengeName
    0
    0
    0
    1
    (pconstant "\x01")
    oneChunkHash
    oneChunkStart
    oneChunkNext
    oneChunkFrontier
    pnil

oneChunkPublicationWith ::
  forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PTokenName ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s (PBuiltinList (PAsData ValidationMerkle.PFrontierPeak)) ->
  Term s (PBuiltinList (PAsData PByteString)) ->
  Term s PPublicationDatumV1
oneChunkPublicationWith deployment header challenge trancheIndex chunkIndex chunkOffset chunkLength chunk chunkHash previousAccumulator nextAccumulator frontier siblings =
  pcon $
    PPublicationDatumV1
      (pdata deployment)
      (pdata header)
      (pdata challenge)
      (pdata trancheIndex)
      (pdata chunkIndex)
      (pdata chunkOffset)
      (pdata chunkLength)
      (pdata chunkHash)
      (pdata frontier)
      (pdata siblings)
      (pdata previousAccumulator)
      (pdata nextAccumulator)
      (pdata chunk)

twoChunkHash0, twoChunkHash1, twoChunkLeaf0, twoChunkLeaf1 :: forall s. Term s PByteString
twoChunkHash0 = pblake2b_256 # pconstant "a"
twoChunkHash1 = pblake2b_256 # pconstant "b"
twoChunkLeaf0 = pchunkLeafHashV1 0 0 0 1 twoChunkHash0
twoChunkLeaf1 = pchunkLeafHashV1 0 1 1 1 twoChunkHash1

twoChunkFrontier :: forall s. Term s (PBuiltinList (PAsData ValidationMerkle.PFrontierPeak))
twoChunkFrontier =
  pcons
    # pdata (pcon $ ValidationMerkle.PFrontierPeak (pdata 1) (pdata $ ValidationMerkle.phashBranch # twoChunkLeaf0 # twoChunkLeaf1))
    # pnil

twoChunkDescriptorShape :: forall s. Term s PTrancheDescriptorV1
twoChunkDescriptorShape = descriptorTerm 0 0 2 2 (pconstant hash32) (pconstant hash32)

twoChunkStart, twoChunkNext :: forall s. Term s PByteString
twoChunkStart = ptrancheStartAccumulatorV1 (pconstant identity28) (pconstant hash28) twoChunkDescriptorShape
twoChunkNext = ptrancheStepAccumulatorV1 (pconstant identity28) (pconstant hash28) 0 0 (pconstant "a") twoChunkStart

twoChunkDescriptor :: forall s. Term s PTrancheDescriptorV1
twoChunkDescriptor =
  descriptorTerm 0 0 2 2 (ValidationMerkle.pfrontierCommitment # 2 # twoChunkFrontier) (pconstant hash32)

twoChunkActive :: forall s. Term s PTrancheDatumV1
twoChunkActive = activeWithDescriptor twoChunkDescriptor twoChunkStart

firstOfTwoPublication :: forall s. Term s PPublicationDatumV1
firstOfTwoPublication =
  oneChunkPublicationWith
    (pconstant identity28)
    (pconstant hash28)
    challengeName
    0
    0
    0
    1
    (pconstant "a")
    twoChunkHash0
    twoChunkStart
    twoChunkNext
    twoChunkFrontier
    (pcons # pdata twoChunkLeaf1 # pnil)

firstOfTwoAdvanced :: forall s. Term s PTrancheDatumV1
firstOfTwoAdvanced =
  pcon $
    PActiveTranche
      (pdata $ pconstant identity28)
      (pdata $ pconstant hash28)
      (pdata challengeName)
      (pdata twoChunkDescriptor)
      (pdata 1)
      (pdata twoChunkNext)
      (pdata $ pcon $ PDJust $ pdata 7)
      (pdata psmallResponseWindowMsV1)
      (pdata $ pconstant owner28)

fixtureOutRef :: TxOutRef
fixtureOutRef = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0xaa) 7

chunkBytes, trancheBytes :: Integer
chunkBytes = 14_020
trancheBytes = 4 * 1024 * 1024

geometry :: forall s. Integer -> Integer -> Integer -> Term s PResponseGeometryV1
geometry chunkLength trancheLength trancheCount =
  pcon $
    PResponseGeometryV1
      (pdata $ pconstant chunkLength)
      (pdata $ pconstant trancheLength)
      (pdata $ pconstant trancheCount)

canonicalGeometry :: forall s. Term s PResponseGeometryV1
canonicalGeometry = geometry chunkBytes trancheBytes 16

parameters ::
  forall s.
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Term s PParametersV1
parameters daBond challengerBond openFee publicationFee settlementFee closeFee timeoutFee =
  pcon $
    PParametersV1
      (pdata canonicalGeometry)
      (pdata $ pconstant daBond)
      (pdata $ pconstant challengerBond)
      (pdata $ pconstant openFee)
      (pdata $ pconstant publicationFee)
      (pdata $ pconstant settlementFee)
      (pdata $ pconstant closeFee)
      (pdata $ pconstant timeoutFee)

canonicalParameters :: forall s. Term s PParametersV1
canonicalParameters = parameters 10_000_000_000 10_000_000_000 500_000 500_000 500_000 1_000_000 1_200_000

singletonDescriptor ::
  forall s.
  Integer -> Integer -> Integer -> Integer -> BS.ByteString -> BS.ByteString -> Term s (PBuiltinList (PAsData PTrancheDescriptorV1))
singletonDescriptor index offset byteLength chunkCount chunkCommitment terminalAccumulator =
  pcons
    # pdata (descriptor index offset byteLength chunkCount chunkCommitment terminalAccumulator)
    # pnil

descriptor ::
  forall s.
  Integer -> Integer -> Integer -> Integer -> BS.ByteString -> BS.ByteString -> Term s PTrancheDescriptorV1
descriptor index offset byteLength chunkCount chunkCommitment terminalAccumulator =
  pcon $
    PTrancheDescriptorV1
      (pdata $ pconstant index)
      (pdata $ pconstant offset)
      (pdata $ pconstant byteLength)
      (pdata $ pconstant chunkCount)
      (pdata $ pconstant chunkCommitment)
      (pdata $ pconstant terminalAccumulator)

canonicalDescriptor :: forall s. Term s PTrancheDescriptorV1
canonicalDescriptor = descriptor 0 0 1 1 hash32 hash32

canonicalDescriptors :: forall s. Term s (PBuiltinList (PAsData PTrancheDescriptorV1))
canonicalDescriptors = singletonDescriptor 0 0 1 1 hash32 hash32

commitment ::
  forall s.
  Integer -> BS.ByteString -> BS.ByteString -> PubKeyHash -> Integer -> Term s PResponseGeometryV1 -> Term s (PBuiltinList (PAsData PTrancheDescriptorV1)) -> Term s PCommitmentV1
commitment version deploymentIdentity headerHash owner payloadLength responseGeometry descriptors =
  pcon $
    PCommitmentV1
      (pdata $ pconstant version)
      (pdata $ pconstant deploymentIdentity)
      (pdata $ pconstant headerHash)
      (pdata $ pconstant payloadLength)
      (pdata responseGeometry)
      (pdata descriptors)
      (pdata $ pconstant owner)

canonicalCommitment :: forall s. Term s PCommitmentV1
canonicalCommitment = commitment 1 identity28 hash28 owner28 1 canonicalGeometry canonicalDescriptors

identity28, identity27, hash28, hash27, hash32, hash31 :: BS.ByteString
identity28 = BS.replicate 28 0x11
identity27 = BS.replicate 27 0x11
hash28 = BS.replicate 28 0x22
hash27 = BS.replicate 27 0x22
hash32 = BS.replicate 32 0x33
hash31 = BS.replicate 31 0x33

owner28, owner27 :: PubKeyHash
owner28 = PubKeyHash $ toBuiltin $ BS.replicate 28 0x44
owner27 = PubKeyHash $ toBuiltin $ BS.replicate 27 0x44

challengeName :: forall s. Term s PTokenName
challengeName = pconstant $ TokenName $ toBuiltin $ BS.replicate 32 0x55

otherChallengeName :: forall s. Term s PTokenName
otherChallengeName = pconstant $ TokenName $ toBuiltin $ BS.replicate 32 0x56

maximumChunk, maximumLeafHash :: BS.ByteString
maximumChunk = BS.replicate 14_020 42
maximumLeafHash = Base16.decodeLenient $ BSC.pack "2b89672abc40b0ba8c2d4db8cd236b2ca6bda31ab8db93517f88e88dff114675"
