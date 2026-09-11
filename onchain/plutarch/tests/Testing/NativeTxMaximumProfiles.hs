{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxMaximumProfiles
Description : Cross-language maximum native-transaction profiles.

The fixture files are exact copies of the TypeScript vectors which generate
the corresponding Aiken suites.  The assertions are deliberately evaluated in
small groups: the size-balanced transaction is over 16 KiB, and compiling one
monolithic conjunction needlessly multiplies GHC and Plutarch compiler memory.
-}
module Testing.NativeTxMaximumProfiles (tests) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import Data.List (find)
import GHC.Generics (Generic)
import Plutarch.Builtin.ByteString (pintegerToByteString, pmostSignificantFirst)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.NativeTx.Compact (
  pdecodeNativeTxFieldPreimageLengthsV1,
  pdecodeNativeTxWitnessSetCompact,
  pnativeTxCanonicalSizeV1,
  pverifyNativeTxCompactCborV1,
 )
import Midgard.FraudProofs.NativeTx.Components (
  pencodeMidgardAddressWitness,
  pencodeMidgardRedeemerWitness,
 )
import Midgard.FraudProofs.NativeTx.Preimages (
  pdecodeMidgardTxAddressWitnessesPreimageCbor,
  pdecodeMidgardTxRedeemerWitnessesPreimageCbor,
  pencodeAddressWitnessPreimage,
  pencodeRedeemerWitnessPreimage,
 )
import Midgard.FraudProofs.NativeTx.Transaction (
  pdecodeMidgardTransactionV1,
  pencodeMidgardTransactionV1,
  pverifyMidgardTransactionFieldPreimageV1,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddressWitness (..),
  PMidgardExecutionUnits (..),
  PMidgardRedeemerPurpose (..),
  PMidgardRedeemerWitness (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.BoundedCollection qualified as BoundedCollection
import Midgard.BoundedItem qualified as BoundedItem
import Midgard.NativeTxFieldAccess qualified as NativeField
import Midgard.ValidationMachineFieldDoor qualified as FieldDoor
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepEvidenceV1 (..),
  PValidationOneStepWitnessV1 (..),
  pencodeCompactBindingWitness,
  pencodeTransactionFieldScanWitness,
  pverifyCanonicalDecodeOneStepV1,
 )
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (..),
  PValidationSourceKind (..),
  PValidationVerdict (..),
  phashLedgerDelta,
  phashValidationContext,
  phashWorkWitness,
  pmachineVersion,
 )
import Testing.Eval (passertEvalNoTraceWithoutHoistChecks, pfails)
import Testing.FieldOpening qualified as FieldFixture

data Fixture = Fixture
  { fullTxCborHex :: String
  , compactTxCborHex :: String
  , compactBodyCborHex :: String
  , txIdHex :: String
  , counts :: ProfileCounts
  , sizes :: ProfileSizes
  , mintPolicyIdsInTxInfoOrder :: [String]
  , redeemerPointers :: [String]
  , preimages :: ProfilePreimages
  , hashes :: ProfileHashes
  , targetFullTxCborBytes :: Maybe Integer
  , fullTxCborToleranceBytes :: Maybe Integer
  , maxFee :: Maybe String
  , maxListLength :: Maybe Integer
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data ProfileCounts = ProfileCounts
  { spendInputs :: Integer
  , referenceInputs :: Integer
  , outputs :: Integer
  , mintPolicies :: Integer
  , spendRedeemers :: Integer
  , mintRedeemers :: Integer
  , observerRedeemers :: Integer
  , receiveRedeemers :: Integer
  , totalRedeemers :: Integer
  , requiredSigners :: Maybe Integer
  , addrWitnesses :: Maybe Integer
  , scriptWitnesses :: Maybe Integer
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data ProfileSizes = ProfileSizes
  { fullTxCborBytes :: Integer
  , compactTxCborBytes :: Integer
  , compactBodyCborBytes :: Integer
  , fee :: String
  , preimages :: ProfilePreimageSizes
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data ProfilePreimageSizes = ProfilePreimageSizes
  { spendInputs :: Integer
  , referenceInputs :: Integer
  , outputs :: Integer
  , requiredObservers :: Integer
  , requiredSigners :: Integer
  , mint :: Integer
  , addrTxWits :: Integer
  , scriptTxWits :: Integer
  , redeemerTxWits :: Integer
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data ProfilePreimages = ProfilePreimages
  { spendInputsCborHex :: String
  , referenceInputsCborHex :: String
  , outputsCborHex :: String
  , requiredObserversCborHex :: String
  , requiredSignersCborHex :: String
  , mintCborHex :: String
  , addrTxWitsCborHex :: String
  , scriptTxWitsCborHex :: String
  , redeemerTxWitsCborHex :: String
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data ProfileHashes = ProfileHashes
  { spendInputsHashHex :: String
  , referenceInputsHashHex :: String
  , outputsHashHex :: String
  , requiredObserversHashHex :: String
  , requiredSignersHashHex :: String
  , mintHashHex :: String
  , addrTxWitsHashHex :: String
  , scriptTxWitsHashHex :: String
  , redeemerTxWitsHashHex :: String
  , witnessSetHashHex :: String
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data Expected = Expected
  { expectedTxIdHex :: String
  , expectedCounts :: ProfileCounts
  , expectedSizes :: ProfileSizes
  , expectedMintPolicyIds :: [String]
  , expectedRedeemerPointers :: [String]
  , expectedHashes :: ProfileHashes
  , expectedTargetBytes :: Maybe Integer
  , expectedToleranceBytes :: Maybe Integer
  , expectedMaxFee :: Maybe String
  , expectedMaxListLength :: Maybe Integer
  }

data C20AddressFixture = C20AddressFixture
  { transaction_id :: String
  , transaction_commitment :: String
  , collection_commitment :: String
  , preimage_hash :: String
  , compact_cbor :: String
  , witness_set_compact_cbor :: String
  , field_preimage_lengths_cbor :: String
  , address_witnesses_preimage_cbor :: String
  , vkey_witness_count :: Integer
  , field_bytes :: Integer
  , signed_cardano_bytes :: Integer
  , adjacent_signed_cardano_bytes :: Integer
  , canonical_bytes :: Integer
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data MaximumFoldVector = MaximumFoldVector
  { maximumLabel :: String
  , maximumTransactionId :: String
  , maximumTransactionCommitment :: String
  , maximumCompactCbor :: String
  , maximumWitnessSetCbor :: String
  , maximumLengthsCbor :: String
  , maximumFieldPreimage :: String
  , maximumContextCbor :: String
  , maximumFieldIndex :: Integer
  , maximumItemCount :: Integer
  , maximumItemIndex :: Integer
  , maximumTerminalChunkIndex :: Integer
  , maximumEncodedLength :: Integer
  , maximumPreWorkRoot :: String
  , maximumPostWorkRoot :: String
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

tests :: TestTree
tests =
  testGroup "Native Tx Maximum Profiles"
    [ profileCase
        "high_cardinality_lucid_midgard_native_tx_decodes"
        "tests/fixtures/native-high-cardinality.json"
        highCardinalityExpected
    , profileCase
        "size_balanced_lucid_midgard_native_tx_decodes"
        "tests/fixtures/native-size-balanced-15_5k.json"
        sizeBalancedExpected
    , maximumFoldCase "maximum_cardano_inline_datum_terminal_fold_matches_typescript" "maximum-inline-datum" $ \vector -> do
        BS.length maximumInlineDatumTerminalChunk @?= 3_936
        passertEvalNoTraceWithoutHoistChecks (maximumInlineDatumTerminalFold vector)
    , maximumFoldCase "maximum_cardano_spend_redeemer_field_matches_typescript_terminal_commitment" "maximum-spend-redeemers" $ \vector -> do
        passertEvalNoTraceWithoutHoistChecks maximumSpendRedeemerDecodeRoundTrip
        passertEvalNoTraceWithoutHoistChecks maximumSpendRedeemerShape
        passertEvalNoTraceWithoutHoistChecks (maximumSpendRedeemerPreimageVector vector)
        passertEvalNoTraceWithoutHoistChecks (maximumSpendRedeemerCommitment vector)
    , maximumFoldCase "maximum_cardano_spend_redeemer_terminal_fold_matches_typescript" "maximum-spend-redeemers" $
        \vector -> passertEvalNoTraceWithoutHoistChecks (maximumSpendRedeemerTerminalFold vector)
    , testGroup "C20 field 7 Cardano maximum TypeScript fixture"
        [ c20AddressCase "v1_c20_7_maximum_fixture_decodes_and_round_trips" $
            \fixture -> passertEvalNoTraceWithoutHoistChecks (c20AddressDecodeRoundTrip fixture)
        , c20AddressCase "v1_c20_7_maximum_fixture_commitments_are_exact" $
            \fixture -> passertEvalNoTraceWithoutHoistChecks (c20AddressCommitments fixture)
        , c20AddressCase "v1_c20_7_accepted_cardano_maximum_matches_typescript_boundary_fixture production rejects the retired counted commitment" $
            \fixture -> do
              passertEvalNoTraceWithoutHoistChecks (c20AddressCanonicalSize fixture)
              -- The generated witness set still carries the retired counted
              -- root; current Aiken and Plutarch require the flat hash.
              pfails (c20AddressAtField fixture 7)
        , c20AddressCase "v1_c20_7_maximum_fixture_witness_preimage_rejects_at_signer_position" $
            \fixture -> pfails (c20AddressAtField fixture 4)
        , c20AddressCase "v1_c20_7_adjacent_witness_count_exceeds_the_cardano_envelope" $
            \fixture -> passertEvalNoTraceWithoutHoistChecks (c20AddressAdjacentCount fixture)
        ]
    ]

maximumFoldCase :: String -> String -> (MaximumFoldVector -> Assertion) -> TestTree
maximumFoldCase name label assertion = testCase name $ do
  decoded <-
    eitherDecodeFileStrict' "tests/fixtures/validation-machine-field-terminal-v1.json"
      :: IO (Either String [MaximumFoldVector])
  case decoded of
    Left err -> assertFailure err
    Right vectors -> case find ((== label) . maximumLabel) vectors of
      Nothing -> assertFailure $ "missing maximum fold vector: " <> label
      Just vector -> assertion vector

profileCase :: String -> FilePath -> Expected -> TestTree
profileCase caseName fixturePath expected = testCase caseName $ do
  fixture <- loadFixture fixturePath
  let fullTx = hex fixture.fullTxCborHex
      compactTx = hex fixture.compactTxCborHex
      compactBody = hex fixture.compactBodyCborHex
  assertBool "full transaction is outside the compact profile" $
    BS.length fullTx > BS.length compactTx
  fixture.txIdHex @?= expected.expectedTxIdHex
  fixture.counts @?= expected.expectedCounts
  fixture.sizes @?= expected.expectedSizes
  fixture.mintPolicyIdsInTxInfoOrder @?= expected.expectedMintPolicyIds
  fixture.redeemerPointers @?= expected.expectedRedeemerPointers
  fixture.hashes @?= expected.expectedHashes
  fixture.targetFullTxCborBytes @?= expected.expectedTargetBytes
  fixture.fullTxCborToleranceBytes @?= expected.expectedToleranceBytes
  fixture.maxFee @?= expected.expectedMaxFee
  fixture.maxListLength @?= expected.expectedMaxListLength
  fromIntegral (BS.length fullTx) @?= expected.expectedSizes.fullTxCborBytes
  fromIntegral (BS.length compactTx) @?= expected.expectedSizes.compactTxCborBytes
  fromIntegral (BS.length compactBody) @?= expected.expectedSizes.compactBodyCborBytes
  assertPreimageVectors fixture
  case (expected.expectedTargetBytes, expected.expectedToleranceBytes) of
    (Nothing, Nothing) -> pure ()
    (Just target, Just tolerance) ->
      assertBool "full transaction is outside the Aiken size-balanced window" $
        expected.expectedSizes.fullTxCborBytes >= target - tolerance
          && expected.expectedSizes.fullTxCborBytes <= target + tolerance
    _ -> assertFailure "target and tolerance must either both be present or both be absent"
  passertEvalNoTraceWithoutHoistChecks $
    pencodeMidgardTransactionV1 # (pdecodeMidgardTransactionV1 # pconstant fullTx)
      #== pconstant fullTx

assertPreimageVectors :: Fixture -> Assertion
assertPreimageVectors fixture = do
  let actual = profilePreimageBytes fixture.preimages
      expectedLengths = profilePreimageLengths fixture.sizes.preimages
  map (fromIntegral . BS.length) actual @?= expectedLengths

profilePreimageBytes :: ProfilePreimages -> [BS.ByteString]
profilePreimageBytes p =
  map
    hex
    [ p.spendInputsCborHex
    , p.referenceInputsCborHex
    , p.outputsCborHex
    , p.requiredObserversCborHex
    , p.requiredSignersCborHex
    , p.mintCborHex
    , p.addrTxWitsCborHex
    , p.scriptTxWitsCborHex
    , p.redeemerTxWitsCborHex
    ]

profilePreimageLengths :: ProfilePreimageSizes -> [Integer]
profilePreimageLengths p =
  [ p.spendInputs
  , p.referenceInputs
  , p.outputs
  , p.requiredObservers
  , p.requiredSigners
  , p.mint
  , p.addrTxWits
  , p.scriptTxWits
  , p.redeemerTxWits
  ]

loadFixture :: FilePath -> IO Fixture
loadFixture path = either fail pure =<< eitherDecodeFileStrict' path

c20AddressCase :: String -> (C20AddressFixture -> Assertion) -> TestTree
c20AddressCase name assertion = testCase name $ do
  fixture <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/native-c20-field7-maximum.json"
  let preimage = hex fixture.address_witnesses_preimage_cbor
  fromIntegral (BS.length preimage) @?= fixture.field_bytes
  fixture.vkey_witness_count @?= 124
  fixture.signed_cardano_bytes @?= 16_351
  fixture.adjacent_signed_cardano_bytes @?= 16_482
  fixture.canonical_bytes @?= 16_685
  assertion fixture

c20AddressDecodeRoundTrip :: forall s. C20AddressFixture -> Term s PBool
c20AddressDecodeRoundTrip fixture =
  let preimage = phex fixture.address_witnesses_preimage_cbor
   in plet (pdecodeMidgardTxAddressWitnessesPreimageCbor # preimage) $ \decoded ->
        plet (pfromData $ pelemAt # 0 # decoded) $ \first ->
          plet (pfromData $ pelemAt # 123 # decoded) $ \last ->
            pmatch first $ \PMidgardAddressWitness {paddressWitness'verificationKey = firstKey} ->
              pmatch last $ \PMidgardAddressWitness {paddressWitness'verificationKey = lastKey} ->
                pand'List
                  [ plength # decoded #== pconstant fixture.vkey_witness_count
                  , pall
                      # plam
                        ( \witnessData ->
                            pmatch (pfromData witnessData) $ \PMidgardAddressWitness
                              { paddressWitness'verificationKey
                              , paddressWitness'signature
                              } ->
                                plengthBS # pfromData paddressWitness'verificationKey #== 32
                                  #&& plengthBS # pfromData paddressWitness'signature #== 64
                        )
                      # decoded
                  , pfromData firstKey
                      #== phex "fc01d2918e4aab0ab1ccbd479e975f23acd834a3578f688c7dfbdcc2ba0a63c4"
                  , pfromData lastKey
                      #== phex "fa14eba7e0bec653a0fcdb28a961b7649a7772b28665d83ffc111d8a38cef017"
                  , pencodeAddressWitnessPreimage # decoded #== preimage
                  ]

c20AddressCommitments :: forall s. C20AddressFixture -> Term s PBool
c20AddressCommitments fixture =
  let preimage = phex fixture.address_witnesses_preimage_cbor
   in plet (pdecodeMidgardTxAddressWitnessesPreimageCbor # preimage) $ \decoded ->
        plet
          ( pmap
              # plam (\witnessData -> pencodeMidgardAddressWitness # pfromData witnessData)
              # decoded
          )
          $ \itemCbors ->
            plet (pdecodeNativeTxWitnessSetCompact # phex fixture.witness_set_compact_cbor) $ \witnessSet ->
              plet (pdecodeNativeTxFieldPreimageLengthsV1 # phex fixture.field_preimage_lengths_cbor) $ \lengths ->
                pmatch witnessSet $ \PNativeTxWitnessSetCompact {pwitnessSetCompact'addrTxWitsHash} ->
                  pmatch lengths $ \PNativeTxFieldPreimageLengthsV1 {plengths'addressWitnesses} ->
                    pand'List
                      [ pall # plam (\item -> plengthBS # item #== 101) # itemCbors
                      , plengthBS # preimage #== pconstant fixture.field_bytes
                      , plengths'addressWitnesses #== pconstant fixture.field_bytes
                      , pblake2b_256 # preimage #== phex fixture.preimage_hash
                      , NativeField.pfieldCommitmentFromItems # itemCbors
                          #== phex fixture.preimage_hash
                      , pfromData pwitnessSetCompact'addrTxWitsHash
                          #== phex fixture.collection_commitment
                      , pnot
                          # ( phex fixture.preimage_hash
                                #== phex fixture.collection_commitment
                            )
                      ]

c20AddressAdjacentCount :: forall s. C20AddressFixture -> Term s PBool
c20AddressAdjacentCount fixture =
  plet
    (pencodeAddressWitnessPreimage # c20SyntheticVkeyWitnesses 124)
    $ \accepted ->
      plet
        (pencodeAddressWitnessPreimage # c20SyntheticVkeyWitnesses 125)
        $ \adjacent ->
          pand'List
            [ plengthBS # accepted #== pconstant fixture.field_bytes
            , plengthBS # adjacent #== pconstant (fixture.field_bytes + 103)
            , pconstant @PInteger fixture.signed_cardano_bytes #<= 16_384
            , pconstant @PInteger fixture.adjacent_signed_cardano_bytes #> 16_384
            ]

c20SyntheticVkeyWitnesses :: forall s. Integer -> Term s (PBuiltinList (PAsData PMidgardAddressWitness))
c20SyntheticVkeyWitnesses count =
  pfix
    ( \self -> plam $ \index ->
        pif
          (index #>= pconstant count)
          pnil
          ( pcons
              # pdata
                ( pcon $ PMidgardAddressWitness
                    ( pdata $
                        pconstant (BS.replicate 28 0xc7)
                          <> pintegerToByteString # pmostSignificantFirst # 4 # index
                    )
                    ( pdata $
                        pconstant (BS.replicate 60 0xc7)
                          <> pintegerToByteString # pmostSignificantFirst # 4 # index
                    )
                )
              # (self # (index + 1))
          )
    )
    # 0

c20AddressCanonicalSize :: forall s. C20AddressFixture -> Term s PBool
c20AddressCanonicalSize fixture =
  plet
    (pverifyNativeTxCompactCborV1 # phex fixture.transaction_id # phex fixture.compact_cbor)
    $ \verified ->
      plet (pdecodeNativeTxFieldPreimageLengthsV1 # phex fixture.field_preimage_lengths_cbor) $ \lengths ->
        pmatch verified $ \PVerifiedMidgardNativeTxCompact {pverified'txCompact} ->
          pnativeTxCanonicalSizeV1 # pverified'txCompact # lengths
            #== pconstant fixture.canonical_bytes

c20AddressAtField :: forall s. C20AddressFixture -> Integer -> Term s PBool
c20AddressAtField fixture fieldIndex =
  pverifyMidgardTransactionFieldPreimageV1
    # phex fixture.transaction_id
    # phex fixture.transaction_commitment
    # phex fixture.compact_cbor
    # phex fixture.witness_set_compact_cbor
    # phex fixture.field_preimage_lengths_cbor
    # pconstant fieldIndex
    # phex fixture.address_witnesses_preimage_cbor

purposeTag :: Term s PMidgardRedeemerPurpose -> Term s PInteger
purposeTag purpose = pmatch purpose $ \case
  PSpendRedeemer -> 0
  PMintRedeemer -> 1
  PCertRedeemer -> 2
  PRewardRedeemer -> 3
  PVoteRedeemer -> 4
  PProposeRedeemer -> 5
  PReceiveRedeemer -> 6

phex :: String -> Term s PByteString
phex = pconstant . hex

hex :: String -> BS.ByteString
hex = Base16.decodeLenient . BSC.pack

--------------------------------------------------------------------------------
-- Cardano maximum inline datum and redeemer profiles
--------------------------------------------------------------------------------

maximumInlineDatumTerminalFold :: forall s. MaximumFoldVector -> Term s PBool
maximumInlineDatumTerminalFold vector =
  let transactionId = phex $ maximumTransactionId vector
      transactionCommitment = phex $ maximumTransactionCommitment vector
      compactCbor = phex $ maximumCompactCbor vector
      witnessSetCbor = phex $ maximumWitnessSetCbor vector
      lengthsCbor = phex $ maximumLengthsCbor vector
      contextCbor = phex $ maximumContextCbor vector
      fieldPreimage = hex $ maximumFieldPreimage vector
      carriage = FieldFixture.certifiedCarriageFor $ FieldFixture.chunksOf fieldPreimage
      door = pcon $ FieldDoor.PMachineFieldDoorV1
        (FieldFixture.inputsT $ FieldFixture.certifiedReferenceInputs
          (hex $ maximumTransactionId vector) 2 fieldPreimage)
        (pdata $ pconstant FieldFixture.certificatePolicy)
   in
  plet
    ( pencodeTransactionFieldScanWitness
        # compactCbor
        # witnessSetCbor
        # lengthsCbor
        # contextCbor
        # 2 # 0 # 3 # 1 # 1
    )
    $ \workCbor ->
  plet
    ( pencodeTransactionFieldScanWitness
        # compactCbor
        # witnessSetCbor
        # lengthsCbor
        # contextCbor
        # 3 # 0 # 0 # (-1) # 0
    )
    $ \successorWorkCbor ->
  plet
    ( maximumState
        transactionId
        transactionCommitment
        contextCbor
        (pcon PCanonicalDecode)
        40
        workCbor
    )
    $ \pre ->
  plet
    ( maximumState
        transactionId
        transactionCommitment
        contextCbor
        (pcon PCanonicalDecode)
        41
        successorWorkCbor
    )
    $ \post ->
  plet
    ( pcon $ BoundedCollection.PItemProofV1
        (pdata BoundedCollection.pboundedCollectionVersion)
        (pdata 2)
        (pdata 1)
        (pdata 0)
        (pdata 16_221)
        (pdata $ phex "0b98a091df9a97ac8b6c858f8ac3a50125986f551df3011575575dc58ae422e9")
        ( pdata $ peakList
            [ (0, "a68306b1789aa1bfe5ea824fa0eafe7b7a7f35a9b5b8bad7897d694383c5d99d")
            ]
        )
        (pdata pnil)
    )
    $ \_collectionProof ->
  plet
    ( pcon $ BoundedItem.PChunkProofV1
        (pdata BoundedItem.pversion)
        (pdata 2)
        (pdata 0)
        (pdata 16_221)
        (pdata 3)
        (pdata $ pconstant maximumInlineDatumTerminalChunk)
        ( pdata $ peakList
            [ (2, "e28dcceb8c768fd8cfc2c05cb0f713bad5b2e47fb5791ded037f9fe5cf59338b")
            ]
        )
        ( pdata $ byteStringList
            [ "5d5a9d6259d53c1663a3e797b3ba87e4b738b2231ded6225a47b2fb68a3f9837"
            , "a427ef63d23f12f91b7c02e0febef3a98754273827f46a36e5545c92e693edf6"
            ]
        )
    )
    $ \_chunkProof ->
  plet (maximumEvidence workCbor post 2 0 carriage) $ \evidence ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
    pfromData (pmachineState'workRoot preState)
      #== phex (maximumPreWorkRoot vector)
      #&& (pfromData (pmachineState'workRoot postState) #== phex (maximumPostWorkRoot vector))
      #&& (pverifyCanonicalDecodeOneStepV1 # pre # evidence # door)

maximumSpendRedeemerDecodeRoundTrip :: forall s. Term s PBool
maximumSpendRedeemerDecodeRoundTrip =
  plet maximumSpendRedeemers $ \redeemers ->
  plet (pencodeRedeemerWitnessPreimage # redeemers) $ \preimage ->
  plet (pdecodeMidgardTxRedeemerWitnessesPreimageCbor # preimage) $ \decoded ->
    plength # decoded
      #== 296
      #&& (pencodeRedeemerWitnessPreimage # decoded #== preimage)

maximumSpendRedeemerShape :: forall s. Term s PBool
maximumSpendRedeemerShape =
  plet (pdecodeMidgardTxRedeemerWitnessesPreimageCbor #$ pencodeRedeemerWitnessPreimage # maximumSpendRedeemers) $ \decoded ->
    maximumRedeemersAreSequential # decoded # 1

maximumSpendRedeemerPreimageVector :: forall s. MaximumFoldVector -> Term s PBool
maximumSpendRedeemerPreimageVector vector =
  plet (pencodeRedeemerWitnessPreimage # maximumSpendRedeemers) $ \preimage ->
    plengthBS # preimage
      #== pconstant (fromIntegral $ BS.length $ hex $ maximumFieldPreimage vector)
      #&& (preimage #== pconstant (hex $ maximumFieldPreimage vector))

maximumSpendRedeemerCommitment :: forall s. MaximumFoldVector -> Term s PBool
maximumSpendRedeemerCommitment vector =
  plet maximumSpendRedeemers $ \redeemers ->
  plet
    ( pmap
        # plam (\redeemer -> pencodeMidgardRedeemerWitness # pfromData redeemer)
        # redeemers
    )
    $ \itemCbors ->
    NativeField.pfieldCommitmentFromItems # itemCbors
      #== (pblake2b_256 # pconstant (hex $ maximumFieldPreimage vector))

maximumSpendRedeemerTerminalFold :: forall s. MaximumFoldVector -> Term s PBool
maximumSpendRedeemerTerminalFold vector =
  let transactionId = phex $ maximumTransactionId vector
      transactionCommitment = phex $ maximumTransactionCommitment vector
      compactCbor = phex $ maximumCompactCbor vector
      witnessSetCbor = phex $ maximumWitnessSetCbor vector
      lengthsCbor = phex $ maximumLengthsCbor vector
      contextCbor = phex $ maximumContextCbor vector
      carriage = pcon $ NativeField.PInline
        (pdata $ pencodeRedeemerWitnessPreimage # maximumSpendRedeemers)
      door = pcon $ FieldDoor.PMachineFieldDoorV1
        pnil
        (pdata $ pconstant FieldFixture.certificatePolicy)
   in
  plet
    ( pencodeTransactionFieldScanWitness
        # compactCbor
        # witnessSetCbor
        # lengthsCbor
        # contextCbor
        # 8 # 295 # 0 # 296 # pconstant (maximumEncodedLength vector)
    )
    $ \workCbor ->
  plet
    ( pencodeCompactBindingWitness
        # transactionId
        # transactionCommitment
        # compactCbor
        # witnessSetCbor
        # lengthsCbor
        # contextCbor
    )
    $ \successorWorkCbor ->
  plet
    ( maximumState
        transactionId
        transactionCommitment
        contextCbor
        (pcon PCanonicalDecode)
        40
        workCbor
    )
    $ \pre ->
  plet
    ( maximumState
        transactionId
        transactionCommitment
        contextCbor
        (pcon PCompactBinding)
        41
        successorWorkCbor
    )
    $ \post ->
  plet
    ( pcon $ BoundedCollection.PItemProofV1
        (pdata BoundedCollection.pboundedCollectionVersion)
        (pdata 8)
        (pdata 296)
        (pdata 295)
        (pdata 18)
        (pdata $ phex "0b7517c996b4be98c145b61a84789c337d9a394529322f0e4ff2b00825a13fe5")
        ( pdata $ peakList
            [ (3, "599fb8883e9753ebff787e9ba693c9d266a94e2e4c2412f5cc8def17a37efc4b")
            , (5, "d3ee1a26b14495f4b4e5196a4035453be00e5947a29d7a106e78df0ffb840942")
            , (8, "baff8cd322326841f8dbf9a8fa0464a67cdf9c4161e429fbb569e0346adffde1")
            ]
        )
        ( pdata $ byteStringList
            [ "b2a18a9249e13b7f2c75032bf73d6d447b196d970097d8276f450ddcbd45ff21"
            , "8277569ed239c02bb472113a2804537ce7e9977f6fc3e8e01104f1c38c696c18"
            , "07c8bf5bbf9c9e2b477a1c8ccfe1a7d4ce62846164c5fbebc6481c4da4fc6f22"
            ]
        )
    )
    $ \_collectionProof ->
  plet
    ( pcon $ BoundedItem.PChunkProofV1
        (pdata BoundedItem.pversion)
        (pdata 8)
        (pdata 295)
        (pdata 18)
        (pdata 0)
        (pdata $ phex "840019012843d87980821906411a0004d2f5")
        ( pdata $ peakList
            [ (0, "bd5765879c3e766f6cbc89ea728e263b73af278ed4091e26cabaf5b7fb04d91e")
            ]
        )
        (pdata pnil)
    )
    $ \_chunkProof ->
  plet (maximumEvidence workCbor post 8 295 carriage) $ \evidence ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
    pfromData (pmachineState'workRoot preState)
      #== phex (maximumPreWorkRoot vector)
      #&& (pfromData (pmachineState'workRoot postState) #== phex (maximumPostWorkRoot vector))
      #&& (pverifyCanonicalDecodeOneStepV1 # pre # evidence # door)

maximumState :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PValidationPhase ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PValidationMachineStateV1
maximumState transactionId transactionCommitment context phase counter workCbor =
  pcon $ PValidationMachineStateV1
    (pdata pmachineVersion)
    (pdata transactionId)
    (pdata transactionId)
    (pdata transactionCommitment)
    (pdata $ phashValidationContext # context)
    (pdata $ pcon PForced)
    (pdata transactionCommitment)
    (pdata phase)
    (pdata counter)
    (pdata $ phashWorkWitness # phase # counter # workCbor)
    (pdata 0)
    (pdata 0)
    (pdata $ pcon PPending)
    (pdata $ pconstant zeroHash)
    (pdata $ phashLedgerDelta # pconstant "\x80")

maximumEvidence :: forall s.
  Term s PByteString ->
  Term s PValidationMachineStateV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s NativeField.PFieldCarriageV1 ->
  Term s PValidationOneStepEvidenceV1
maximumEvidence workCbor post fieldIndex itemIndex carriage =
  pcon $ PValidationOneStepEvidenceV1
    (pdata $ pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post))
    (pdata $ pcon $ PTransactionFieldChunkWitness
      (pdata fieldIndex) (pdata itemIndex) (pdata carriage))

maximumSpendRedeemers :: forall s. Term s (PBuiltinList (PAsData PMidgardRedeemerWitness))
maximumSpendRedeemers =
  foldr
    ( \index rest ->
        pcons
          # pdata
            ( pcon $ PMidgardRedeemerWitness
                (pdata $ pcon PSpendRedeemer)
                (pdata $ pconstant index)
                (pdata $ phex "d87980")
                (pdata $ pcon $ PMidgardExecutionUnits (pdata 1_601) (pdata 316_149))
            )
          # rest
    )
    pnil
    [1 .. 296]

maximumRedeemersAreSequential :: forall s.
  Term s (PBuiltinList (PAsData PMidgardRedeemerWitness) :--> PInteger :--> PBool)
maximumRedeemersAreSequential = pfix $ \self -> plam $ \redeemers expectedIndex ->
  pelimList
    ( \redeemerData rest ->
        pmatch (pfromData redeemerData) $ \redeemer ->
        pmatch (pfromData $ predeemerWitness'executionUnits redeemer) $ \units ->
          purposeTag (pfromData $ predeemerWitness'purpose redeemer)
            #== 0
            #&& (pfromData (predeemerWitness'index redeemer) #== expectedIndex)
            #&& (pfromData (predeemerWitness'redeemerCbor redeemer) #== phex "d87980")
            #&& (pfromData (pexecutionUnits'memory units) #== 1_601)
            #&& (pfromData (pexecutionUnits'steps units) #== 316_149)
            #&& (self # rest # (expectedIndex + 1))
    )
    (expectedIndex #== 297)
    redeemers

peakList :: forall s. [(Integer, String)] -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
peakList = foldr
  (\(height, hashValue) rest -> pcons # pdata (pcon $ Merkle.PFrontierPeak (pdata $ pconstant height) (pdata $ phex hashValue)) # rest)
  pnil

byteStringList :: forall s. [String] -> Term s (PBuiltinList (PAsData PByteString))
byteStringList = foldr (\value rest -> pcons # pdata (phex value) # rest) pnil

maximumInlineDatumTerminalChunk :: BS.ByteString
maximumInlineDatumTerminalChunk =
  BS.replicate 41 0x5a
    <> mconcat (replicate 59 ("\x58\x40" <> BS.replicate 64 0x5a))
    <> "\xff"

zeroHash :: BS.ByteString
zeroHash = BS.replicate 32 0

highCardinalityExpected :: Expected
highCardinalityExpected = Expected
  { expectedTxIdHex = "9bfe8f0ec50ab4b1822452f6fa1f881e6ae280d03e00ee2592a08abf9bf84bea"
  , expectedCounts = ProfileCounts
      { spendInputs = 8
      , referenceInputs = 4
      , outputs = 12
      , mintPolicies = 6
      , spendRedeemers = 3
      , mintRedeemers = 6
      , observerRedeemers = 2
      , receiveRedeemers = 2
      , totalRedeemers = 13
      , requiredSigners = Nothing
      , addrWitnesses = Nothing
      , scriptWitnesses = Nothing
      }
  , expectedSizes = ProfileSizes
      { fullTxCborBytes = 2_936
      , compactTxCborBytes = 314
      , compactBodyCborBytes = 277
      , fee = "0"
      , preimages = ProfilePreimageSizes
          { spendInputs = 321
          , referenceInputs = 161
          , outputs = 1_132
          , requiredObservers = 61
          , requiredSigners = 31
          , mint = 253
          , addrTxWits = 104
          , scriptTxWits = 640
          , redeemerTxWits = 135
          }
      }
  , expectedMintPolicyIds =
      [ "1717e4e1e346932539ff3c8ec8ab14ae5d9ec67d640e90fba75ddf90"
      , "1e17a3ac6aa59edc53d143460cce20514d7ec6129fb5139b30a7f8f4"
      , "35acb16c87c65a724675f3166eec068a621ecfa4fc28c2611febf781"
      , "9df1a46687c1957165a9b13392d823de39e1fc19a9e31ec7d3a56c21"
      , "c012db50e7f3e256aa5ea8806397141c7a8e571fc3abcf537e773a55"
      , "f9a2778be9b42cdd59e74568bbf243aca5591210e3ed2bbb90c7f0ff"
      ]
  , expectedRedeemerPointers =
      [ "0:1", "0:4", "0:7"
      , "1:0", "1:1", "1:2", "1:3", "1:4", "1:5"
      , "3:0", "3:1", "6:0", "6:1"
      ]
  , expectedHashes = ProfileHashes
      { spendInputsHashHex = "99272cc37b5ef7d39d69a25f035251d47bf8b2ba5037cca8c80e83062e129795"
      , referenceInputsHashHex = "71ccf64e13bf69772ad3d039a7b98df9df56cf72507fab67d250f4f3bfa96289"
      , outputsHashHex = "c7d0ae52b0b329f17db87ecd9606869e58d255ddd5d6abdf1d9ddb49c659df11"
      , requiredObserversHashHex = "6970d0585775a9435729f42623c7474df384fe7a93358131a4d530183d8c4896"
      , requiredSignersHashHex = "ddc9251016435006deab0c3aeab4defff8a4fc6847122e046f6b25d529fa5bfc"
      , mintHashHex = "7424389d0cc642a47af1e29a1da80de5c92935e4ed49123adb1757c4376a8ef0"
      , addrTxWitsHashHex = "d14987c4b00e9a65f67f19ce4dae270f49a5dd1341db181a1d0a381a1c0e4715"
      , scriptTxWitsHashHex = "a23d592d41138b395b6e5a05221f347d63ec995ffb180a2c4e7b9435b6bc59f1"
      , redeemerTxWitsHashHex = "28fc840d662bb07dbd0f8f029674915d7b2a1e77d16ebf588e547882509a6f50"
      , witnessSetHashHex = "774348f5a59cb13a14ce8d2b7cd8d7e6e8be482493fcc783ec474d9f542dc51f"
      }
  , expectedTargetBytes = Nothing
  , expectedToleranceBytes = Nothing
  , expectedMaxFee = Nothing
  , expectedMaxListLength = Nothing
  }

sizeBalancedExpected :: Expected
sizeBalancedExpected = Expected
  { expectedTxIdHex = "bae88529bd16928b3651b517a9a2e786a7ac772413b23c0515708bd02ba66299"
  , expectedCounts = ProfileCounts
      { spendInputs = 48
      , referenceInputs = 32
      , outputs = 48
      , mintPolicies = 24
      , spendRedeemers = 8
      , mintRedeemers = 24
      , observerRedeemers = 18
      , receiveRedeemers = 18
      , totalRedeemers = 68
      , requiredSigners = Just 17
      , addrWitnesses = Just 17
      , scriptWitnesses = Just 68
      }
  , expectedSizes = ProfileSizes
      { fullTxCborBytes = 16_176
      , compactTxCborBytes = 318
      , compactBodyCborBytes = 281
      , fee = "5000000"
      , preimages = ProfilePreimageSizes
          { spendInputs = 1_922
          , referenceInputs = 1_282
          , outputs = 4_371
          , requiredObservers = 541
          , requiredSigners = 511
          , mint = 1_035
          , addrTxWits = 1_752
          , scriptTxWits = 3_896
          , redeemerTxWits = 758
          }
      }
  , expectedMintPolicyIds =
      [ "13495cddfa3f76c2b61db08debe7436dde852483a259a28fecbce523"
      , "225b4b1401614c6570fe24713245f766ebd010d83ef3b656ecea525a"
      , "2a08243fe21da6635e6f5944757ec87381bcc50c1ef23065944adab1"
      , "3114815749ea2205e6a019fb12c32c31dd19a3c1c338ef77a0510c24"
      , "32009a8d48d57459d8ac81bb25aee8a8e3d9dd8c62b6059c298af7b4"
      , "33573af50e11f8cc800326cb7a0ebf75f9ad183d1e2fbc9d7fd858d3"
      , "40a0003944d5d0fd52c524486559f44a1b31b5f29b8027326489fb64"
      , "4e58ccbf6836164904505373c78977fb5e07e7edbdfe02bcb8895162"
      , "4f2652177ca58e8ddce164863596611967fad0ef227a4d4b9820d3c8"
      , "609d5a4d16f0e2369d220994840523a1e43671c3aeee97dfaa9e194d"
      , "6cb7f19594cd456e7ada909053e4eee93af7d5666f0e72f5482567be"
      , "831bbb60ca6687e168a87db1b1df0d1aefe06063520e500c27db35d7"
      , "84d6e094405ff5f7aeef6d6ede3307dcf958a62e344ad65ecc1c667b"
      , "8519882cc348c15d228a0d21941efa4d8d2fa12133f3262edb933f36"
      , "8cde7ceea1325bd7c26a019b1f11224b6ba227599257e7751956449f"
      , "9c069bfd6f028262c78c16c0ec6a9742e1bf347a4268b27ac3dba91b"
      , "c0bfde0cd6bbb2d7cac51689bc3bb7803418c4a1201e20d9fe6b61dc"
      , "e276e3153393fa5851b37f5fa6de0c4123fba9bc8fecc114e080a617"
      , "e45b5eb0149232ee9438ee676396b133a060d041693aa6b67cf8fd7f"
      , "e97c2c6330990eca8e58ca215634be4a0973b2ec20fe39b7d334330e"
      , "ef967c1f236616b1d26f2995282173a91d9d02337efa95c14c35400a"
      , "f281143eaa25332f84b0bd91b9917f93ea96a80605bc24e7bfb79472"
      , "f4166dea635550bd3a4bec029269ae32c3462aea297e4f4ee093c6aa"
      , "fd5eb0e838e645b1715745c602e984d0d877904bd6faeb3f4bc21250"
      ]
  , expectedRedeemerPointers =
      pointerRange "0" 40 47
        <> pointerRange "1" 0 23
        <> pointerRange "3" 0 17
        <> pointerRange "6" 0 17
  , expectedHashes = ProfileHashes
      { spendInputsHashHex = "eb7a16d70ae555416ffca6da2a0eec46dff3ddbb66fe26f961ad9b2d248d666e"
      , referenceInputsHashHex = "d499d5e4a101dc2c505403598f5bd80fa1f5c1562e5c94ce226fdaef4345e135"
      , outputsHashHex = "59430a79bca7b5b29f12a7d17cf3bb42d3d499e85eff7bfa5a68132eb71bc78f"
      , requiredObserversHashHex = "744a1689162fd10c5cfe48d675fe921e38358947f6869c11cda679d1a2a26008"
      , requiredSignersHashHex = "588f11ee838626ba103a91e54ea682d5ee3232348b66d37740779a71e87e40b9"
      , mintHashHex = "32b6b7ef38a2a065daad7fe9fa3b7e27ed0c744b6a1af02bf75d07e7115ccca9"
      , addrTxWitsHashHex = "d46f7b76594c72003b5827f34613dc63f394f32ba0315f26afdeb07dcbd56e49"
      , scriptTxWitsHashHex = "1987e4154ac5b40418359cf940d9cc5f0b4506d954f394b3c55d36f94a414099"
      , redeemerTxWitsHashHex = "d781aff72da9f7bb3739bb85dd58e0305ebce50db65dfb3b76e35245226ff506"
      , witnessSetHashHex = "450fee2a8f7384896d2552016257597c62a5e0d4aee28877fb89372793025ce6"
      }
  , expectedTargetBytes = Just 16_128
  , expectedToleranceBytes = Just 128
  , expectedMaxFee = Just "10000000"
  , expectedMaxListLength = Just 255
  }

pointerRange :: String -> Integer -> Integer -> [String]
pointerRange tag first lastIndex =
  map ((tag <> ":") <>) $ map show [first .. lastIndex]
