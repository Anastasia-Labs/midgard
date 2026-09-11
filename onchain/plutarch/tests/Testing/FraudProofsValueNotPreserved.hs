{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsValueNotPreserved (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.ValueNotPreserved
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
    testGroup
        "Value Not Preserved Fraud Proof Tests"
        [ testGroup "step 01" step01Tests
        , testGroup "step 02" step02Tests
        , testGroup "step 03" step03Tests
        , testGroup "step 04" step04Tests
        ]

step01Tests :: [TestTree]
step01Tests =
    [ testCase "value_not_preserved_step_01_binds_and_freezes_the_claim" $
        psucceeds $
            runStep01 tokenInflatedTx tokenDescriptor tokenClaim
    , testCase "value_not_preserved_step_01_rejects_a_rejected_transaction" $
        pfails $
            runStep01 tokenInflatedTx{vValidity = 1} tokenDescriptor tokenClaim
    , testCase "value_not_preserved_step_01_rejects_a_malformed_claim" $
        pfails $
            runStep01 tokenInflatedTx tokenDescriptor malformedTokenClaim
    ]

step02Tests :: [TestTree]
step02Tests =
    [ testCase "value_not_preserved_step_02_folds_a_token_spent_input" $
        psucceeds $
            runFold tokenInflatedTx tokenDescriptor tokenClaim tokenWitness 40
    , testCase "value_not_preserved_step_02_folds_an_ada_spent_input" $
        psucceeds $
            runFold adaInflatedTx adaDescriptor adaClaim adaWitness 10_000_000
    , testCase "value_not_preserved_step_02_rejects_a_forged_descriptor" $
        pfails $
            runFold tokenInflatedTx tokenDescriptor tokenClaim forgedDescriptorWitness 40
    , testCase "value_not_preserved_step_02_rejects_a_truncated_asset_walk" $
        pfails $
            runFold tokenInflatedTx tokenDescriptor tokenClaim truncatedTokenWitness 40
    , testCase "value_not_preserved_step_02_rejects_a_forged_leaf_quantity" $
        pfails $
            runFold tokenInflatedTx tokenDescriptor tokenClaim forgedQuantityWitness 400
    , testCase "value_not_preserved_step_02_finishes_after_the_last_input" $
        psucceeds $
            runFinish tokenInflatedTx tokenDescriptor tokenClaim 1 40
    , testCase "value_not_preserved_step_02_rejects_a_premature_finish" $
        pfails $
            runFinish tokenInflatedTx tokenDescriptor tokenClaim 0 0
    ]

step03Tests :: [TestTree]
step03Tests =
    [ testCase "value_not_preserved_step_03_completes_the_token_fold" $
        psucceeds $
            runStep03 tokenInflatedTx tokenClaim 40 (Just $ vMintPreimage tokenInflatedTx) (-10)
    , testCase "value_not_preserved_step_03_completes_the_ada_fold" $
        psucceeds $
            runStep03 adaInflatedTx adaClaim 10_000_000 Nothing (-500_000)
    , testCase "value_not_preserved_step_03_folds_mint_against_outputs" $
        psucceeds $
            runStep03 mintedPaidOutTx tokenClaim 0 (Just $ vMintPreimage mintedPaidOutTx) 0
    , testCase "value_not_preserved_step_03_rejects_a_substituted_outputs_preimage" $
        pfails $
            runStep03WithOutputs tokenInflatedTx (vOutputsPreimage tokenBalancedTx) tokenClaim 40 (Just $ vMintPreimage tokenInflatedTx) 0
    , testCase "value_not_preserved_step_03_rejects_a_missing_mint_carriage" $
        pfails $
            runStep03 mintedPaidOutTx tokenClaim 0 Nothing (-25)
    ]

step04Tests :: [TestTree]
step04Tests =
    [ testCase "value_not_preserved_step_04_convicts_an_inflated_token" $
        psucceeds $
            runStep04 tokenClaim inflatedDirection (-10) valueThreadName
    , testCase "value_not_preserved_step_04_convicts_a_deflated_token" $
        psucceeds $
            runStep04 tokenClaim deflatedDirection 10 valueThreadName
    , testCase "value_not_preserved_step_04_convicts_inflated_ada" $
        psucceeds $
            runStep04 adaClaim inflatedDirection (-500_000) valueThreadName
    , testCase "value_not_preserved_step_04_rejects_a_balanced_fold" $
        pfails $
            runStep04 tokenClaim inflatedDirection 0 valueThreadName
    , testCase "value_not_preserved_step_04_rejects_a_direction_mismatch" $
        pfails $
            runStep04 tokenClaim inflatedDirection 10 valueThreadName
    , testCase "value_not_preserved_step_04_rejects_a_wrong_category" $
        pfails $
            runStep04 tokenClaim inflatedDirection (-10) ("\x00\x00\x00\x04" <> valueHeaderHash)
    ]

runStep01 :: forall s. ValueTx -> BS.ByteString -> PD.Data -> Term s PUnit
runStep01 tx descriptor claim =
    valueNotPreservedStep01Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant hubOracleHash)
        # pconstant context
  where
    root = singleEntryPhasRoot spentInputKey descriptor
    rawTxRoot = singleEntryPhasRoot (txId tx) (source tx)
    headerTxRoot = commitCountedRoot transactionsDomain rawTxRoot l2Count
    expected = step02State tx claim root 0 0
    args = PD.Constr 0 [bareInclusionArgs (txId tx) (source tx) rawTxRoot, claim, inflatedDirection]
    context =
        spendContext
            (stepDatum Nothing)
            (continueAction args)
            [threadInputWithName valueThreadName]
            [stepOutputWithName nextScript (Just expected) valueThreadName]
            (referenceInputsWithTransactionAndUtxosRoots headerTxRoot root root)
            [phasEntry rawTxRoot (txId tx) (source tx)]
            mempty

runFold :: forall s. ValueTx -> BS.ByteString -> PD.Data -> PD.Data -> Integer -> Term s PUnit
runFold tx descriptor claim witness expectedQuantity =
    valueNotPreservedStep02Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant
            ( continueContext
                (step02State tx claim root 0 0)
                (PD.Constr 0 [PD.Constr 0 [PD.I 0, PD.I 0, spendInputsOpening tx, witness]])
                stepScript
                (step02State tx claim root 1 expectedQuantity)
            )
  where
    root = singleEntryPhasRoot spentInputKey descriptor

runFinish :: forall s. ValueTx -> BS.ByteString -> PD.Data -> Integer -> Integer -> Term s PUnit
runFinish tx descriptor claim cursor delta =
    valueNotPreservedStep02Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant
            ( continueContext
                (step02State tx claim root cursor delta)
                (PD.Constr 1 [PD.I 0, PD.I 0, spendInputsOpening tx])
                nextScript
                (step03State tx claim delta)
            )
  where
    root = singleEntryPhasRoot spentInputKey descriptor

runStep03 :: forall s. ValueTx -> PD.Data -> Integer -> Maybe BS.ByteString -> Integer -> Term s PUnit
runStep03 tx = runStep03WithOutputs tx (vOutputsPreimage tx)

runStep03WithOutputs :: forall s. ValueTx -> BS.ByteString -> PD.Data -> Integer -> Maybe BS.ByteString -> Integer -> Term s PUnit
runStep03WithOutputs tx outputs claim delta mintCarriage finalDelta =
    valueNotPreservedStep03Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant
            ( continueContext
                (step03State tx claim delta)
                ( PD.Constr
                    0
                    [ PD.I 0
                    , PD.I 0
                    , PD.B (compact tx)
                    , inlineCarriage outputs
                    , maybe none (some . inlineCarriage) mintCarriage
                    ]
                )
                nextScript
                (PD.Constr 0 [PD.B (txId tx), claim, inflatedDirection, PD.I finalDelta])
            )

runStep04 :: forall s. PD.Data -> PD.Data -> Integer -> BS.ByteString -> Term s PUnit
runStep04 claim direction delta assetName =
    valueNotPreservedStep04Validator
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pdata (pconstant ctPolicy)
        # pconstant
            ( spendContext
                (stepDatum $ Just $ PD.Constr 0 [PD.B (txId tokenInflatedTx), claim, direction, PD.I delta])
                (continueAction $ PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0])
                [threadInputWithName assetName]
                [convictionOutput fraudProofAddress assetName]
                []
                [fraudProofMintEntry assetName]
                (singleton fpPolicy (TokenName $ toBuiltin assetName) 1)
            )

continueContext :: PD.Data -> PD.Data -> BS.ByteString -> PD.Data -> ScriptContext
continueContext inputState action outputScript outputState =
    spendContext
        (stepDatum $ Just inputState)
        (continueAction action)
        [threadInputWithName valueThreadName]
        [stepOutputWithName outputScript (Just outputState) valueThreadName]
        []
        []
        mempty

step02State :: ValueTx -> PD.Data -> BS.ByteString -> Integer -> Integer -> PD.Data
step02State tx claim root cursor delta =
    PD.Constr 0 [PD.B (txId tx), claim, inflatedDirection, PD.I (vFee tx), PD.B root, PD.I cursor, PD.I delta]

step03State :: ValueTx -> PD.Data -> Integer -> PD.Data
step03State tx claim delta = PD.Constr 0 [PD.B (txId tx), claim, inflatedDirection, PD.I (vFee tx), PD.I delta]

spendInputsOpening :: ValueTx -> PD.Data
spendInputsOpening tx = PD.Constr 0 [PD.B (compact tx), inlineCarriage spentInputsPreimage]

inlineCarriage :: BS.ByteString -> PD.Data
inlineCarriage preimage = PD.Constr 0 [PD.B preimage]

some :: PD.Data -> PD.Data
some value = PD.Constr 0 [value]

none :: PD.Data
none = PD.Constr 1 []

continueAction :: PD.Data -> PD.Data
continueAction action = PD.Constr 1 [action]

tokenClaim, malformedTokenClaim, adaClaim, inflatedDirection, deflatedDirection :: PD.Data
tokenClaim = PD.Constr 1 [PD.B claimedPolicy, PD.B claimedName]
malformedTokenClaim = PD.Constr 1 [PD.B $ BS.replicate 27 0xaa, PD.B claimedName]
adaClaim = PD.Constr 0 []
inflatedDirection = PD.Constr 0 []
deflatedDirection = PD.Constr 1 []

claimedPolicy, decoyPolicy, claimedName, decoyName :: BS.ByteString
claimedPolicy = BS.replicate 28 0xaa
decoyPolicy = BS.replicate 28 0xbb
claimedName = "VAL"
decoyName = "DEC"

spentInput :: (BS.ByteString, Integer)
spentInput = (BS.replicate 32 0x77, 2)

spentInputKey :: BS.ByteString
spentInputKey = encodedInput spentInput

spentInputsPreimage :: BS.ByteString
spentInputsPreimage = "\x81" <> wrapItem spentInputKey

data ValueTx = ValueTx
    { vOutputsPreimage :: BS.ByteString
    , vMintPreimage :: BS.ByteString
    , vFee :: Integer
    , vValidity :: Integer
    }

valueTx :: BS.ByteString -> BS.ByteString -> Integer -> ValueTx
valueTx output mint fee = ValueTx ("\x81" <> wrapItem output) mint fee 0

tokenInflatedTx, tokenBalancedTx, mintedPaidOutTx, adaInflatedTx :: ValueTx
tokenInflatedTx = valueTx (outputCbor 4_000_000 [(claimedPolicy, claimedName, 50)]) "\x80" 1_000_000
tokenBalancedTx = valueTx (outputCbor 9_000_000 [(claimedPolicy, claimedName, 40), (decoyPolicy, decoyName, 7)]) "\x80" 1_000_000
mintedPaidOutTx = valueTx (outputCbor 4_000_000 [(claimedPolicy, claimedName, 25)]) (mintPreimage claimedPolicy claimedName 25) 1_000_000
adaInflatedTx = valueTx (outputCbor 9_500_000 []) "\x80" 1_000_000

compactBody :: ValueTx -> BS.ByteString
compactBody tx =
    BS.concat
        [ "\x8c"
        , defBytes32 $ blake2b256 spentInputsPreimage
        , defBytes32 $ blake2b256 "\x80"
        , defBytes32 $ blake2b256 $ vOutputsPreimage tx
        , cborInt $ vFee tx
        , cborInt 0
        , cborInt 65_536
        , defBytes32 $ blake2b256 "\x80"
        , defBytes32 $ blake2b256 "\x80"
        , defBytes32 $ blake2b256 $ vMintPreimage tx
        , defBytes32 $ hash32 0x07
        , defBytes32 $ hash32 0x08
        , cborInt 1
        ]

compact :: ValueTx -> BS.ByteString
compact tx = "\x84\x01" <> compactBody tx <> defBytes32 (blake2b256 sourceWitnessSet) <> cborInt (vValidity tx)

sourceWitnessSet :: BS.ByteString
sourceWitnessSet = witnessSetCborOf tx1

source :: ValueTx -> BS.ByteString
source tx = sourceCborFor (txId tx) (compact tx) sourceWitnessSet (fieldPreimageLengthsCborOf tx1)

txId :: ValueTx -> BS.ByteString
txId tx = blake2b256 $ "MidgardNativeTxBodyV1" <> "\x01" <> compactBody tx

outputCbor :: Integer -> [(BS.ByteString, BS.ByteString, Integer)] -> BS.ByteString
outputCbor lovelace assets =
    "\xa2\x00"
        <> wrapItem (pubKeyAddressBytes prover)
        <> "\x01\x82"
        <> cborInt lovelace
        <> policyMap assets

policyMap :: [(BS.ByteString, BS.ByteString, Integer)] -> BS.ByteString
policyMap [] = "\xa0"
policyMap entries =
    BS.singleton (0xa0 + fromIntegral (length entries))
        <> BS.concat ["\x58\x1c" <> policy <> "\xa1" <> wrapItem name <> cborInt quantity | (policy, name, quantity) <- entries]

mintPreimage :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
mintPreimage policy name quantity =
    "\x81" <> wrapItem ("\x82\x58\x1c" <> policy <> "\xa1" <> wrapItem name <> cborInt quantity)

tokenDescriptor, adaDescriptor :: BS.ByteString
tokenDescriptor = descriptorCbor 10_000_000 tokenAssets
adaDescriptor = descriptorCbor 10_000_000 []

tokenAssets :: [(BS.ByteString, BS.ByteString, Integer)]
tokenAssets = [(claimedPolicy, claimedName, 40), (decoyPolicy, decoyName, 7)]

descriptorCbor :: Integer -> [(BS.ByteString, BS.ByteString, Integer)] -> BS.ByteString
descriptorCbor lovelace assets =
    BS.concat
        [ "\x90\x01\x02\x00"
        , defBytes32 $ hash32 0x51
        , wrapItem $ pubKeyAddressBytes prover
        , cborInt lovelace
        , cborInt $ fromIntegral $ length assets
        , defBytes32 $ frontierCommitment assets
        , "\x00\x20\x40\x00\x40"
        , summary 0x52
        , summary 0x53
        , summary 0x54
        ]
  where
    summary seed = "\x83" <> defBytes32 (hash32 seed) <> "\x00\x00"

assetLeaf :: (BS.ByteString, BS.ByteString, Integer) -> BS.ByteString
assetLeaf (policy, name, quantity) =
    blake2b256 $ "MidgardLedgerOutputAssetLeafV1\x83" <> wrapItem policy <> wrapItem name <> cborInt quantity

frontierCommitment :: [(BS.ByteString, BS.ByteString, Integer)] -> BS.ByteString
frontierCommitment [] = blake2b256 $ "MidgardValidationMerkleFrontierV1\x00\x80"
frontierCommitment [asset] =
    let leaf = assetLeaf asset
     in blake2b256 $ "MidgardValidationMerkleFrontierV1\x01\x81\x82\x00" <> defBytes32 leaf
frontierCommitment [first, second] =
    let root = blake2b256 $ "MidgardValidationMerkleBranchV1" <> assetLeaf first <> assetLeaf second
     in blake2b256 $ "MidgardValidationMerkleFrontierV1\x02\x81\x82\x01" <> defBytes32 root
frontierCommitment _ = error "fixture supports at most two assets"

tokenWitness, truncatedTokenWitness, forgedQuantityWitness, forgedDescriptorWitness, adaWitness :: PD.Data
tokenWitness = witness tokenDescriptor tokenAssets
truncatedTokenWitness = witnessWithOpenings tokenDescriptor tokenAssets [head tokenAssets]
forgedQuantityWitness =
    witnessWithOpenings tokenDescriptor tokenAssets [(claimedPolicy, claimedName, 400), tokenAssets !! 1]
forgedDescriptorWitness = witness adaDescriptor tokenAssets
adaWitness = PD.Constr 0 [PD.B adaDescriptor, emptyProof, PD.List [], PD.List []]

witness :: BS.ByteString -> [(BS.ByteString, BS.ByteString, Integer)] -> PD.Data
witness descriptor assets = witnessWithOpenings descriptor assets assets

witnessWithOpenings :: BS.ByteString -> [(BS.ByteString, BS.ByteString, Integer)] -> [(BS.ByteString, BS.ByteString, Integer)] -> PD.Data
witnessWithOpenings descriptor committed opened =
    PD.Constr 0 [PD.B descriptor, emptyProof, PD.List peaks, PD.List $ zipWith opening [0 :: Int ..] opened]
  where
    leaves = map assetLeaf committed
    root = case leaves of
        [first, second] -> blake2b256 $ "MidgardValidationMerkleBranchV1" <> first <> second
        [leaf] -> leaf
        _ -> BS.empty
    peaks = if null committed then [] else [PD.Constr 0 [PD.I (if length committed == 2 then 1 else 0), PD.B root]]
    opening index (policy, name, quantity) =
        PD.Constr 0 [PD.B policy, PD.B name, PD.I quantity, PD.List $ map PD.B $ siblings index leaves]
    siblings 0 [_, second] = [second]
    siblings 1 [first, _] = [first]
    siblings _ _ = []

valueHeaderHash, valueThreadName :: BS.ByteString
valueHeaderHash = BS.replicate 28 0xaa
valueThreadName = "\x00\x00\x00\x19" <> valueHeaderHash
