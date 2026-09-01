{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsQ1xSpendInputCardinality
Description : Cardinality-boundary parity for
              @validators/fraud-proofs/q1x-spend-input-cardinality.test.ak@.

The controls assert that each fixture really occupies the claimed §5.1/§5.3
envelope.  The measured rows then drive the production validators with the
same last-item selections and full absence walks as the Aiken suite.
-}
module Testing.FraudProofsQ1xSpendInputCardinality (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.DoubleSpend (doubleSpendStep04Validator)
import Midgard.Validators.FraudProofs.MissingNativeScriptTx (missingNativeScriptTxStep06Validator)
import Midgard.Validators.FraudProofs.MissingSignature (missingSignatureStep04Validator)
import Midgard.Validators.FraudProofs.NoInput (noInputStep02Validator)
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Q1x Spend Input Cardinality"
    [ testCase "q1x_f6_no_input_step_02_control_at_one_spend_input" $
        spendControl minimalSpendFixture
    , testCase "q1x_f6_no_input_step_02_at_one_spend_input" $
        psucceeds $ runNoInputStep02 minimalSpendFixture
    , testCase "q1x_f6_no_input_step_02_control_at_the_admissible_maximum" $
        spendControl maximalSpendFixture
    , testCase "q1x_f6_no_input_step_02_at_the_admissible_maximum" $
        psucceeds $ runNoInputStep02 maximalSpendFixture
    , testCase "q1x_f6_double_spend_step_04_control_at_one_spend_input" $
        spendControl minimalSpendFixture
    , testCase "q1x_f6_double_spend_step_04_at_one_spend_input" $
        psucceeds $ runDoubleSpendStep04 minimalSpendFixture Nothing
    , testCase "q1x_f6_double_spend_step_04_control_at_the_admissible_maximum" $
        spendControl maximalSpendFixture
    , testCase "q1x_f6_double_spend_step_04_at_the_admissible_maximum" $
        psucceeds $ runDoubleSpendStep04 maximalSpendFixture Nothing
    , testCase "q1x_f6_missing_native_script_tx_step_06_control_at_one_script_witness" $
        scriptControl minimalScriptFixture
    , testCase "q1x_f6_missing_native_script_tx_step_06_at_one_script_witness" $
        psucceeds $ runMissingNativeScriptStep06 minimalScriptFixture absentScriptHash
    , testCase "q1x_f6_missing_native_script_tx_step_06_control_at_the_admissible_maximum" $
        scriptControl maximalScriptFixture
    , testCase "q1x_f6_missing_native_script_tx_step_06_at_the_admissible_maximum" $
        psucceeds $ runMissingNativeScriptStep06 maximalScriptFixture absentScriptHash
    , testCase "q1x_f6_missing_signature_step_04_control_at_one_address_witness" $
        addressControl minimalAddressFixture
    , testCase "q1x_f6_missing_signature_step_04_at_one_address_witness" $
        psucceeds $ runMissingSignatureStep04 minimalAddressFixture absentVerificationKey
    , testCase "q1x_f6_missing_signature_step_04_control_at_the_admissible_maximum" $
        addressControl maximalAddressFixture
    , testCase "q1x_f6_missing_signature_step_04_at_the_admissible_maximum" $
        psucceeds $ runMissingSignatureStep04 maximalAddressFixture absentVerificationKey
    , testCase "q1x_f6_fixture_sits_at_the_admissible_cardinality" maximalSpendFixtureControl
    , testCase "q1x_f6_step_at_the_admissible_maximum_refuses_a_tampered_preimage" $
        pfails $
          runDoubleSpendStep04
            maximalSpendFixture
            ( Just
                ( spendInputsPreimage
                    ((sfTx maximalSpendFixture) {tSpendInputs = init (sfInputs maximalSpendFixture)})
                )
            )
    , testCase "q1x_f6_compact_structure_length_is_cardinality_independent" compactLengthControl
    , testCase "q1x_f6_step_06_at_the_admissible_maximum_refuses_a_present_script" $
        pfails $
          runMissingNativeScriptStep06
            maximalScriptFixture
            (versionedScriptHashOf 0 (last (scScripts maximalScriptFixture)))
    , testCase "q1x_f6_step_04_at_the_admissible_maximum_refuses_a_present_signature" $
        pfails $
          runMissingSignatureStep04
            maximalAddressFixture
            (last (acVerificationKeys maximalAddressFixture))
    , testCase "q1x_f6_address_witness_fixture_sits_at_the_admissible_cardinality" $
        maximalAddressFixtureControl
    ]

admissibleSpendInputCount, minimalSpendInputCount :: Int
admissibleSpendInputCount = 296
minimalSpendInputCount = 1

admissibleScriptWitnessCount, minimalScriptWitnessCount :: Int
admissibleScriptWitnessCount = 224
minimalScriptWitnessCount = 1

admissibleAddressWitnessCount, minimalAddressWitnessCount :: Int
admissibleAddressWitnessCount = 318
minimalAddressWitnessCount = 1

spendInputStride, addressWitnessStride, maxTier1PreimageBytes, maxAggregateFieldBytes :: Int
spendInputStride = 40
addressWitnessStride = 103
maxTier1PreimageBytes = 14_336
maxAggregateFieldBytes = 32_768

fieldHeaderBytes :: Int -> Int
fieldHeaderBytes count
  | count <= 23 = 1
  | count <= 255 = 2
  | otherwise = 3

--------------------------------------------------------------------------------
-- Spend-input cardinality (Q11 step-02 and Q10 step-04)
--------------------------------------------------------------------------------

data SpendFixture = SpendFixture
  { sfTx :: Tx
  , sfInputs :: [(BS.ByteString, Integer)]
  , sfPreimage :: BS.ByteString
  , sfCompact :: BS.ByteString
  , sfTxId :: BS.ByteString
  }

spendFixture :: Int -> SpendFixture
spendFixture count =
  SpendFixture
    { sfTx = tx
    , sfInputs = inputs
    , sfPreimage = spendInputsPreimage tx
    , sfCompact = compactOf tx
    , sfTxId = txIdOf tx
    }
  where
    inputs = [(BS.replicate 32 0x31, fromIntegral index) | index <- [0 .. count - 1]]
    tx = tx1 {tSpendInputs = inputs}

minimalSpendFixture, maximalSpendFixture :: SpendFixture
minimalSpendFixture = spendFixture minimalSpendInputCount
maximalSpendFixture = spendFixture admissibleSpendInputCount

spendControl :: SpendFixture -> Assertion
spendControl fixture = do
  let count = length (sfInputs fixture)
  BS.length (sfPreimage fixture) @?= fieldHeaderBytes count + count * spendInputStride
  assertBool "the L1 thread fixture has no input" (not (null [threadInput]))

maximalSpendFixtureControl :: Assertion
maximalSpendFixtureControl = do
  length (sfInputs maximalSpendFixture) @?= admissibleSpendInputCount
  admissibleSpendInputCount @?= 296
  BS.length (sfPreimage maximalSpendFixture) @?= 3 + 296 * spendInputStride
  BS.length (sfPreimage maximalSpendFixture) @?= 11_843
  assertBool "field-0 preimage exceeds tier 1" (BS.length (sfPreimage maximalSpendFixture) <= maxTier1PreimageBytes)

compactLengthControl :: Assertion
compactLengthControl = do
  BS.length (sfCompact minimalSpendFixture) @?= BS.length (sfCompact maximalSpendFixture)
  assertBool "cardinality did not change the compact commitment" (sfCompact minimalSpendFixture /= sfCompact maximalSpendFixture)

runNoInputStep02 :: forall s. SpendFixture -> Term s PUnit
runNoInputStep02 fixture =
  noInputStep02Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just inputState))
          (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, bodyOpening (sfCompact fixture) (sfPreimage fixture), PD.I disputedIndex]])
          [threadInput]
          [stepOutput nextScript (Just outputState)]
          referenceInputs
          []
          mempty
      )
  where
    disputedIndex = fromIntegral (length (sfInputs fixture) - 1)
    disputedInput = last (sfInputs fixture)
    inputState = PD.Constr 0 [PD.B (sfTxId fixture), PD.B prevUtxosRoot, PD.B phasRoot]
    outputState = PD.Constr 0 [inputData disputedInput, PD.B prevUtxosRoot, PD.B phasRoot]

runDoubleSpendStep04 :: forall s. SpendFixture -> Maybe BS.ByteString -> Term s PUnit
runDoubleSpendStep04 fixture mPreimage =
  doubleSpendStep04Validator
    # pdata (pconstant ctPolicy)
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant certificatePolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (PD.Constr 0 [PD.B (sfTxId fixture), inputData disputedInput])))
          ( PD.Constr
              1
              [ PD.Constr
                  0
                  [ PD.I 0
                  , PD.I 0
                  , PD.I 0
                  , bodyOpening (sfCompact fixture) (maybe (sfPreimage fixture) id mPreimage)
                  , PD.I disputedIndex
                  ]
              ]
          )
          [threadInput]
          [convictionOutput fraudProofAddress threadName]
          referenceInputs
          [fraudProofMintEntry threadName]
          (singleton fpPolicy (TokenName (toBuiltin threadName)) 1)
      )
  where
    disputedIndex = fromIntegral (length (sfInputs fixture) - 1)
    disputedInput = last (sfInputs fixture)

--------------------------------------------------------------------------------
-- Native-script witness cardinality (Q17 step-06)
--------------------------------------------------------------------------------

data ScriptFixture = ScriptFixture
  { scTxId :: BS.ByteString
  , scWitnessSetHash :: BS.ByteString
  , scCompact :: BS.ByteString
  , scWitnessSetHashes :: (BS.ByteString, BS.ByteString, BS.ByteString)
  , scPreimage :: BS.ByteString
  , scScripts :: [BS.ByteString]
  }

scriptFixture :: Int -> ScriptFixture
scriptFixture count =
  ScriptFixture
    { scTxId = txIdOf tx
    , scWitnessSetHash = witnessSetHash
    , scCompact = compactWith tx witnessSetHash
    , scWitnessSetHashes = hashes
    , scPreimage = preimage
    , scScripts = scripts
    }
  where
    scripts = [scriptWitnessBytes index | index <- [0 .. count - 1]]
    tx = tx1 {tScripts = [(0, bytes) | bytes <- scripts]}
    preimage = scriptWitnessCollectionPreimage [(0, bytes) | bytes <- scripts]
    hashes = witnessSetHashesOf tx
    witnessSetHash = blake2b256 (witnessSetCborFrom hashes)

scriptWitnessBytes :: Int -> BS.ByteString
scriptWitnessBytes index =
  BS.pack [0x82, 0x00, 0x58, 0x1c]
    <> BS.replicate 22 0x30
    <> BS.pack [fromIntegral index, fromIntegral (index `div` 256)]

absentScriptHash :: BS.ByteString
absentScriptHash =
  versionedScriptHashOf
    0
    (BS.pack [0x82, 0x01, 0x81, 0x82, 0x00, 0x58, 0x1c] <> BS.replicate 22 0x31)

minimalScriptFixture, maximalScriptFixture :: ScriptFixture
minimalScriptFixture = scriptFixture minimalScriptWitnessCount
maximalScriptFixture = scriptFixture admissibleScriptWitnessCount

scriptControl :: ScriptFixture -> Assertion
scriptControl fixture = do
  assertBool "field-6 preimage did not contain its items" (BS.length (scPreimage fixture) > length (scScripts fixture))
  length (scScripts fixture) @?= declaredFieldCount (scPreimage fixture)
  BS.length (scWitnessSetHash fixture) @?= 32
  assertBool "the L1 thread fixture has no input" (not (null [threadInput]))

runMissingNativeScriptStep06 :: forall s. ScriptFixture -> BS.ByteString -> Term s PUnit
runMissingNativeScriptStep06 fixture expectedMissingHash =
  missingNativeScriptTxStep06Validator
    # pdata (pconstant ctPolicy)
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant certificatePolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just state))
          ( PD.Constr
              1
              [ PD.Constr
                  0
                  [ PD.I 0
                  , PD.I 0
                  , PD.I 0
                  , witnessOpeningRaw (scCompact fixture) (scWitnessSetHashes fixture) (scPreimage fixture)
                  ]
              ]
          )
          [threadInput]
          [convictionOutput fraudProofAddress threadName]
          referenceInputs
          [fraudProofMintEntry threadName]
          (singleton fpPolicy (TokenName (toBuiltin threadName)) 1)
      )
  where
    state = PD.Constr 0 [PD.B expectedMissingHash, PD.B (scTxId fixture), PD.B (scWitnessSetHash fixture)]

--------------------------------------------------------------------------------
-- Address-witness cardinality (Q16 step-04)
--------------------------------------------------------------------------------

data AddressFixture = AddressFixture
  { acTxId :: BS.ByteString
  , acWitnessSetHash :: BS.ByteString
  , acCompact :: BS.ByteString
  , acWitnessSetHashes :: (BS.ByteString, BS.ByteString, BS.ByteString)
  , acPreimage :: BS.ByteString
  , acVerificationKeys :: [BS.ByteString]
  }

addressFixture :: Int -> AddressFixture
addressFixture count =
  AddressFixture
    { acTxId = txIdOf tx
    , acWitnessSetHash = witnessSetHash
    , acCompact = compactWith tx witnessSetHash
    , acWitnessSetHashes = hashes
    , acPreimage = preimage
    , acVerificationKeys = verificationKeys
    }
  where
    tx = tx1 {tWitnesses = []}
    verificationKeys = [distinctVerificationKey index | index <- [0 .. count - 1]]
    preimage = arrayHeader count <> BS.concat [wrapItem (addressWitnessItem key) | key <- verificationKeys]
    hashes = (blake2b256 preimage, blake2b256 (scriptWitnessesPreimage tx), hash32 0x13)
    witnessSetHash = blake2b256 (witnessSetCborFrom hashes)

distinctVerificationKey :: Int -> BS.ByteString
distinctVerificationKey index = BS.replicate 31 0x40 <> BS.singleton (fromIntegral index)

absentVerificationKey :: BS.ByteString
absentVerificationKey = BS.replicate 32 0x70

sampleWitnessSignature :: BS.ByteString
sampleWitnessSignature = BS.replicate 32 0x50 <> BS.replicate 32 0x60

addressWitnessItem :: BS.ByteString -> BS.ByteString
addressWitnessItem verificationKey =
  "\x82" <> defBytes32 verificationKey <> "\x58\x40" <> sampleWitnessSignature

minimalAddressFixture, maximalAddressFixture :: AddressFixture
minimalAddressFixture = addressFixture minimalAddressWitnessCount
maximalAddressFixture = addressFixture admissibleAddressWitnessCount

addressControl :: AddressFixture -> Assertion
addressControl fixture = do
  let count = length (acVerificationKeys fixture)
  BS.length (acPreimage fixture) @?= fieldHeaderBytes count + count * addressWitnessStride
  assertBool "the L1 thread fixture has no input" (not (null [threadInput]))

maximalAddressFixtureControl :: Assertion
maximalAddressFixtureControl = do
  length (acVerificationKeys maximalAddressFixture) @?= admissibleAddressWitnessCount
  admissibleAddressWitnessCount @?= 318
  BS.length (acPreimage maximalAddressFixture) @?= 3 + 318 * addressWitnessStride
  BS.length (acPreimage maximalAddressFixture) @?= 32_757
  assertBool "field-7 preimage exceeds the aggregate cap" (BS.length (acPreimage maximalAddressFixture) <= maxAggregateFieldBytes)
  assertBool "field-7 preimage unexpectedly fits tier 1" (BS.length (acPreimage maximalAddressFixture) > maxTier1PreimageBytes)

runMissingSignatureStep04 :: forall s. AddressFixture -> BS.ByteString -> Term s PUnit
runMissingSignatureStep04 fixture missingVerificationKey =
  missingSignatureStep04Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just state))
          ( PD.Constr
              1
              [ PD.Constr
                  0
                  [ PD.I 0
                  , PD.I 0
                  , PD.I 0
                  , witnessOpeningRaw (acCompact fixture) (acWitnessSetHashes fixture) (acPreimage fixture)
                  ]
              ]
          )
          [threadInput]
          [convictionOutput fraudProofAddress threadName]
          referenceInputs
          [fraudProofMintEntry threadName]
          (singleton fpPolicy (TokenName (toBuiltin threadName)) 1)
      )
  where
    state = PD.Constr 0 [PD.B missingVerificationKey, PD.B (acTxId fixture), PD.B (acWitnessSetHash fixture)]

declaredFieldCount :: BS.ByteString -> Int
declaredFieldCount bytes =
  case BS.unpack (BS.take 3 bytes) of
    first : _ | first >= 0x80 && first <= 0x97 -> fromIntegral (first - 0x80)
    0x98 : count : _ -> fromIntegral count
    0x99 : hi : lo : _ -> fromIntegral hi * 256 + fromIntegral lo
    _ -> error "non-canonical field array header"
