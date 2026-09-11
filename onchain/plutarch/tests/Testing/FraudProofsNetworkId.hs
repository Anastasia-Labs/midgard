{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsNetworkId (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Bits (shiftR, (.&.))
import Data.Word (Word8)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (
  Credential (..),
  Datum (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptContext,
  ScriptHash (..),
  ScriptPurpose (..),
  TokenName (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.ChunkedInclusion (
  PPublishedProofCarriage,
  ppublishedChunkMembership,
  ppublishedChunkNonMembership,
 )
import Midgard.Validators.FraudProofs.NetworkId (
  networkIdStep01Validator,
  networkIdStep02Validator,
 )
import Testing.Eval (
  pfailsNoTraceWithoutHoistChecks,
  psucceedsNoTraceWithoutHoistChecks,
 )
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Network ID fraud proof"
    [ testGroup "step-01 transaction and output claims" step01Tests
    , testGroup "step-02 transaction and output claims" step02Tests
    , testGroup "post-UTxO claims" postUtxoTests
    , testGroup "published post-UTxO claims" publishedPostUtxoTests
    ]

psucceeds, pfails :: (forall s. Term s a) -> Assertion
psucceeds = psucceedsNoTraceWithoutHoistChecks
pfails = pfailsNoTraceWithoutHoistChecks

step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds an accepted transaction claim" $
      psucceeds $ runStep01 1 0 transactionFault
  , testCase "binds an accepted output claim" $
      psucceeds $ runStep01 0 0 (outputFault 0)
  , testCase "rejects a transaction the block marked invalid" $
      pfails $ runStep01WithValidity 1 0 transactionFault 1
  , testCase "rejects an unsupported deployment network" $
      pfails $ runStep01 1 7 transactionFault
  , testCase "rejects a negative output index" $
      pfails $ runStep01 0 0 (outputFault (-1))
  , testCase "rejects post-UTxO evidence beside a transaction claim" $
      pfails $ step01Validator 0 $ transactionContext 1 0 transactionFault 0 (Just introducedPostPlaceholder)
  ]

step02Tests :: [TestTree]
step02Tests =
  [ testCase "convicts an explicit transaction network mismatch" $
      psucceeds $ runTransactionFinalize 1 0 Nothing
  , testCase "rejects an absent transaction network" $
      pfails $ runTransactionFinalize 255 0 Nothing
  , testCase "rejects a matching transaction network" $
      pfails $ runTransactionFinalize 0 0 Nothing
  , testCase "rejects unused output evidence for a transaction claim" $
      pfails $ runTransactionFinalize 1 0 (Just $ outputOpening 1 False)
  , testCase "convicts a wrong output network" $
      psucceeds $ runOutputFinalize 1 False 0 0
  , testCase "convicts a protected wrong-network output" $
      psucceeds $ runOutputFinalize 1 True 0 0
  , testCase "convicts foreign logical network 2" $
      psucceeds $ runOutputFinalize 2 False 0 0
  , testCase "convicts protected foreign logical network 7" $
      psucceeds $ runOutputFinalize 7 True 0 0
  , testCase "rejects a matching output network" $
      pfails $ runOutputFinalize 0 False 0 0
  , testCase "rejects a protected matching output network" $
      pfails $ runOutputFinalize 0 True 0 0
  , testCase "rejects an out-of-range output index" $
      pfails $ runOutputFinalize 1 False 12 0
  , testCase "rejects a forged outputs preimage" $
      pfails $ runOutputFinalizeWithCommitted 0 False 1 False 0 0
  , testCase "convicts the last output in a high-cardinality opening" $
      psucceeds runHighCardinalityOutputFinalize
  ]

--------------------------------------------------------------------------------
-- Wire encodings
--------------------------------------------------------------------------------

some, none :: PD.Data -> PD.Data
some value = PD.Constr 0 [value]
none _ = PD.Constr 1 []

transactionFault :: PD.Data
transactionFault = PD.Constr 0 []

outputFault :: Integer -> PD.Data
outputFault outputIndex = PD.Constr 1 [PD.I outputIndex]

postUtxoFault :: Integer -> PD.Data
postUtxoFault networkId = PD.Constr 2 [PD.I networkId]

introducedPostPlaceholder :: PD.Data
introducedPostPlaceholder =
  PD.Constr
    0
    [ PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I 1
    , outRefData
    , PD.B ""
    , PD.Constr 0 [emptyProof, PD.I 0]
    , PD.Constr 0 []
    ]

step01Args :: PD.Data -> Maybe PD.Data -> Maybe PD.Data -> PD.Data
step01Args fault txInclusion postMembership =
  PD.Constr
    0
    [ maybe (none fault) some txInclusion
    , maybe (none fault) some postMembership
    , fault
    ]

step02State :: BS.ByteString -> Integer -> Integer -> PD.Data -> Maybe PD.Data -> PD.Data
step02State badTxId committedNetwork expectedNetwork fault postUtxo =
  PD.Constr
    0
    [ PD.B badTxId
    , PD.I committedNetwork
    , PD.I expectedNetwork
    , fault
    , maybe (none fault) some postUtxo
    ]

step02Args :: Maybe PD.Data -> Maybe PD.Data -> PD.Data
step02Args outputsOpening predecessor =
  PD.Constr
    0
    [ PD.I 0
    , PD.I 0
    , PD.I 0
    , maybe (none transactionFault) some outputsOpening
    , maybe (none transactionFault) some predecessor
    ]

outRefData :: PD.Data
outRefData = outRefDataFor 0xcc

outRefDataFor :: Word8 -> PD.Data
outRefDataFor byte = PD.Constr 0 [PD.B (BS.replicate 32 byte), PD.I 7]

--------------------------------------------------------------------------------
-- Step 01
--------------------------------------------------------------------------------

runStep01 :: forall s. Integer -> Integer -> PD.Data -> Term s PUnit
runStep01 committedNetwork expectedNetwork fault =
  runStep01WithValidity committedNetwork expectedNetwork fault 0

runStep01WithValidity :: forall s. Integer -> Integer -> PD.Data -> Integer -> Term s PUnit
runStep01WithValidity committedNetwork expectedNetwork fault validity =
  step01Validator expectedNetwork $
    transactionContext committedNetwork expectedNetwork fault validity Nothing

step01Validator :: forall s. Integer -> ScriptContext -> Term s PUnit
step01Validator expectedNetwork ctx =
  networkIdStep01Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant expectedNetwork
    # pconstant ctx

transactionContext :: Integer -> Integer -> PD.Data -> Integer -> Maybe PD.Data -> ScriptContext
transactionContext committedNetwork expectedNetwork fault validity postMembership =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [step01Args fault (Just $ inclusionArgs txId source rawRoot) postMembership])
    [threadInput]
    [stepOutput nextScript $ Just expectedState]
    (referenceInputsWithTransactionsRoot countedRoot)
    [phasEntry rawRoot txId source]
    mempty
  where
    body = compactBodyFor committedNetwork (outputsPreimage tx1)
    txId = bodyId body
    compact = compactFromBody body validity
    source = sourceCborFor txId compact (witnessSetCborOf tx1) (fieldPreimageLengthsCborOf tx1)
    rawRoot = singleEntryPhasRoot txId source
    countedRoot = commitCountedRoot transactionsDomain rawRoot l2Count
    expectedState = step02State txId committedNetwork expectedNetwork fault Nothing

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

runTransactionFinalize :: forall s. Integer -> Integer -> Maybe PD.Data -> Term s PUnit
runTransactionFinalize committedNetwork expectedNetwork opening =
  step02Validator $
    finalizeContext
      (step02State txId committedNetwork expectedNetwork transactionFault Nothing)
      (step02Args opening Nothing)
  where
    txId = bodyId $ compactBodyFor committedNetwork (outputsPreimage tx1)

runOutputFinalize :: forall s. Integer -> Bool -> Integer -> Integer -> Term s PUnit
runOutputFinalize observedNetwork protected outputIndex expectedNetwork =
  runOutputFinalizeWithCommitted observedNetwork protected observedNetwork protected outputIndex expectedNetwork

runOutputFinalizeWithCommitted :: forall s. Integer -> Bool -> Integer -> Bool -> Integer -> Integer -> Term s PUnit
runOutputFinalizeWithCommitted committedNetwork committedProtected openedNetwork openedProtected outputIndex expectedNetwork =
  step02Validator $
    finalizeContext
      (step02State txId 0 expectedNetwork (outputFault outputIndex) Nothing)
      (step02Args (Just opening) Nothing)
  where
    committedPreimage = singleOutputPreimage committedNetwork committedProtected
    body = compactBodyFor 0 committedPreimage
    txId = bodyId body
    compact = compactFromBody body 0
    opening = bodyOpening compact (singleOutputPreimage openedNetwork openedProtected)

runHighCardinalityOutputFinalize :: forall s. Term s PUnit
runHighCardinalityOutputFinalize =
  step02Validator $
    finalizeContext
      (step02State txId 0 0 (outputFault 11) Nothing)
      (step02Args (Just $ bodyOpening compact preimage) Nothing)
  where
    outputs = replicate 11 (networkOutputCbor 0 False) <> [networkOutputCbor 1 False]
    preimage = arrayHeader (length outputs) <> BS.concat (map wrapItem outputs)
    body = compactBodyFor 0 preimage
    txId = bodyId body
    compact = compactFromBody body 0

step02Validator :: forall s. ScriptContext -> Term s PUnit
step02Validator ctx =
  networkIdStep02Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx

finalizeContext :: PD.Data -> PD.Data -> ScriptContext
finalizeContext state args =
  spendContext
    (stepDatum $ Just state)
    (PD.Constr 1 [args])
    [threadInput]
    [convictionOutput fraudProofAddress threadName]
    []
    [fraudProofMintEntry threadName]
    (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

outputOpening :: Integer -> Bool -> PD.Data
outputOpening networkId protected =
  let preimage = singleOutputPreimage networkId protected
      body = compactBodyFor 0 preimage
   in bodyOpening (compactFromBody body 0) preimage

--------------------------------------------------------------------------------
-- Independent compact and output encoders
--------------------------------------------------------------------------------

compactBodyFor :: Integer -> BS.ByteString -> BS.ByteString
compactBodyFor networkId outputPreimage =
  BS.concat
    [ "\x8c"
    , defBytes32 $ blake2b256 $ spendInputsPreimage tx1
    , defBytes32 $ blake2b256 $ referenceInputsPreimage tx1
    , defBytes32 $ blake2b256 outputPreimage
    , cborInt $ tFee tx1
    , cborInt $ tValidityStart tx1
    , cborInt $ tValidityEnd tx1
    , defBytes32 $ hash32 0x04
    , defBytes32 $ blake2b256 $ requiredSignersPreimage tx1
    , defBytes32 $ hash32 0x06
    , defBytes32 $ hash32 0x07
    , defBytes32 $ hash32 0x08
    , cborInt networkId
    ]

compactFromBody :: BS.ByteString -> Integer -> BS.ByteString
compactFromBody body validity =
  BS.concat ["\x84", cborInt 1, body, defBytes32 (witnessSetHashOf tx1), cborInt validity]

bodyId :: BS.ByteString -> BS.ByteString
bodyId body = blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> body)

singleOutputPreimage :: Integer -> Bool -> BS.ByteString
singleOutputPreimage networkId protected =
  arrayHeader 1 <> wrapItem (networkOutputCbor networkId protected)

networkOutputCbor :: Integer -> Bool -> BS.ByteString
networkOutputCbor networkId protected =
  midgardOutputCbor address 2_000_000 Nothing
  where
    rawNetwork = networkId + if protected then 8 else 0
    address = BS.cons (fromIntegral $ 0x60 + rawNetwork) (keyHashFor 0)

--------------------------------------------------------------------------------
-- Post-UTxO route
--------------------------------------------------------------------------------

postUtxoTests :: [TestTree]
postUtxoTests =
  [ testCase "step 01 authenticates a post-UTxO descriptor" $
      psucceeds $ runPostStep01 defaultPost01
  , testCase "step 01 rejects malformed descriptor CBOR" $
      pfails $ runPostStep01 defaultPost01 {p1ClaimedDescriptor = "\x80"}
  , testCase "step 01 rejects a forged descriptor value" $
      pfails $
        runPostStep01
          defaultPost01
            { p1ClaimedDescriptor = descriptorAtNetwork 3 False
            , p1ObservedNetwork = 3
            }
  , testCase "step 01 rejects a forged ledger key" $
      pfails $ runPostStep01 defaultPost01 {p1ClaimedOutRef = outRefDataFor 0xdd}
  , testCase "step 01 rejects a forged post-state root" $
      pfails $ runPostStep01 defaultPost01 {p1HeaderPostRoot = otherRoot}
  , testCase "step 01 freezes a network-changing predecessor" $
      psucceeds $
        runPostStep01
          defaultPost01
            { p1HeaderPrevRoot = rootForDescriptor $ descriptorAtNetwork 0 False
            , p1Predecessor = changedClaim $ descriptorAtNetwork 0 False
            }
  , testCase "step 01 freezes an introduction over a nonempty predecessor" $
      psucceeds $ runPostStep01 defaultPost01 {p1HeaderPrevRoot = otherRoot}
  , testCase "step 02 convicts an authenticated introduced foreign UTxO" $
      psucceeds $ runIntroducedFinalize 2 emptyMerkleRoot emptyMerkleRoot ledgerKey
  , testCase "step 02 rejects a matching introduced UTxO" $
      pfails $ runIntroducedFinalize 0 emptyMerkleRoot emptyMerkleRoot ledgerKey
  , testCase "step 02 convicts an expected-to-foreign mutation" $
      psucceeds $ runChangedFinalize 2 0 Nothing
  , testCase "step 02 rejects an inherited wrong-network UTxO" $
      pfails $ runChangedFinalize 2 2 Nothing
  , testCase "step 02 rejects a foreign-to-foreign mutation" $
      pfails $ runChangedFinalize 2 3 Nothing
  , testCase "step 02 rejects a forged predecessor root" $
      pfails $ runChangedFinalize 2 0 (Just (otherRoot, ledgerKey, descriptorAtNetwork 0 False))
  , testCase "step 02 rejects a forged predecessor key" $
      pfails $ runChangedFinalize 2 0 (Just (rootForDescriptor $ descriptorAtNetwork 0 False, forgedLedgerKey, descriptorAtNetwork 0 False))
  , testCase "step 02 rejects a forged predecessor value" $
      pfails $ runChangedFinalize 2 0 (Just (rootForDescriptor $ descriptorAtNetwork 0 False, ledgerKey, descriptorAtNetwork 1 False))
  , testCase "step 02 rejects a forged predecessor proof" $
      pfails runForgedPredecessorProof
  , testCase "step 02 rejects a cross-step predecessor-kind swap" $
      pfails $ runPredecessorKindSwap
  ]

data Post01 = Post01
  { p1ClaimedOutRef :: PD.Data
  , p1ClaimedDescriptor :: BS.ByteString
  , p1HeaderPrevRoot :: BS.ByteString
  , p1HeaderPostRoot :: BS.ByteString
  , p1WithdrawalRoot :: BS.ByteString
  , p1WithdrawalKey :: BS.ByteString
  , p1WithdrawalValue :: BS.ByteString
  , p1ObservedNetwork :: Integer
  , p1Predecessor :: PD.Data
  }

defaultPost01 :: Post01
defaultPost01 =
  Post01
    { p1ClaimedOutRef = outRefData
    , p1ClaimedDescriptor = foreignDescriptor
    , p1HeaderPrevRoot = emptyMerkleRoot
    , p1HeaderPostRoot = foreignRoot
    , p1WithdrawalRoot = foreignRoot
    , p1WithdrawalKey = ledgerKey
    , p1WithdrawalValue = foreignDescriptor
    , p1ObservedNetwork = 2
    , p1Predecessor = introducedClaim
    }

runPostStep01 :: forall s. Post01 -> Term s PUnit
runPostStep01 p = runPostStep01WithEvidence p redeemerCarriedMembership refs withdrawals
  where
    refs = referenceInputsWithUtxosRoots (p1HeaderPrevRoot p) (p1HeaderPostRoot p)
    withdrawals = [phasEntry (p1WithdrawalRoot p) (p1WithdrawalKey p) (p1WithdrawalValue p)]

runPostStep01WithEvidence :: forall s. Post01 -> PD.Data -> [TxInInfo] -> [(ScriptPurpose, Redeemer)] -> Term s PUnit
runPostStep01WithEvidence p membershipCarriage refs withdrawals =
  step01Validator 0 $
    spendContext
      (stepDatum Nothing)
      (PD.Constr 1 [step01Args fault Nothing (Just membership)])
      [threadInput]
      [stepOutput nextScript $ Just expectedState]
      refs
      withdrawals
      mempty
  where
    fault = postUtxoFault $ p1ObservedNetwork p
    membership =
      PD.Constr
        0
        [ PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 1
        , p1ClaimedOutRef p
        , PD.B $ p1ClaimedDescriptor p
        , membershipCarriage
        , p1Predecessor p
        ]
    expectedPost =
      PD.Constr
        0
        [ p1ClaimedOutRef p
        , PD.B $ p1ClaimedDescriptor p
        , PD.B $ p1HeaderPrevRoot p
        , p1Predecessor p
        ]
    expectedState =
      step02State
        (txIdFromOutRef $ p1ClaimedOutRef p)
        255
        0
        fault
        (Just expectedPost)

introducedClaim :: PD.Data
introducedClaim = PD.Constr 0 []

changedClaim :: BS.ByteString -> PD.Data
changedClaim previousDescriptor = PD.Constr 1 [PD.B previousDescriptor]

redeemerCarriedMembership :: PD.Data
redeemerCarriedMembership = PD.Constr 0 [emptyProof, PD.I 0]

introducedCarriage :: PD.Data
introducedCarriage = PD.Constr 0 [redeemerCarriedNonMembership]

changedCarriage :: PD.Data
changedCarriage = PD.Constr 1 [redeemerCarriedMembership]

changedCarriageWithProof :: PD.Data -> PD.Data
changedCarriageWithProof proof = PD.Constr 1 [PD.Constr 0 [proof, PD.I 0]]

runIntroducedFinalize :: forall s. Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Term s PUnit
runIntroducedFinalize observedNetwork previousRoot claimedRoot claimedKey =
  runPostFinalize
    observedNetwork
    (descriptorAtNetwork observedNetwork False)
    previousRoot
    introducedClaim
    introducedCarriage
    [pexcludesEntry claimedRoot claimedKey]

runChangedFinalize :: forall s. Integer -> Integer -> Maybe (BS.ByteString, BS.ByteString, BS.ByteString) -> Term s PUnit
runChangedFinalize observedNetwork previousNetwork forgedClaim =
  runPostFinalize
    observedNetwork
    postDescriptor
    previousRoot
    (changedClaim previousDescriptor)
    changedCarriage
    [phasEntry claimedRoot claimedKey claimedValue]
  where
    postDescriptor = descriptorAtNetwork observedNetwork False
    previousDescriptor = descriptorAtNetwork previousNetwork False
    previousRoot = rootForDescriptor previousDescriptor
    (claimedRoot, claimedKey, claimedValue) =
      maybe (previousRoot, ledgerKey, previousDescriptor) id forgedClaim

runPredecessorKindSwap :: forall s. Term s PUnit
runPredecessorKindSwap =
  runPostFinalize
    2
    foreignDescriptor
    (rootForDescriptor $ descriptorAtNetwork 0 False)
    introducedClaim
    changedCarriage
    [phasEntry (rootForDescriptor $ descriptorAtNetwork 0 False) ledgerKey (descriptorAtNetwork 0 False)]

runForgedPredecessorProof :: forall s. Term s PUnit
runForgedPredecessorProof =
  runPostFinalize
    2
    foreignDescriptor
    previousRoot
    (changedClaim previousDescriptor)
    (changedCarriageWithProof forgedProof)
    [phasEntry previousRoot ledgerKey previousDescriptor]
  where
    previousDescriptor = descriptorAtNetwork 0 False
    previousRoot = rootForDescriptor previousDescriptor
    forgedProof =
      PD.List
        [ PD.Constr
            0
            [ PD.I 0
            , PD.B $ BS.replicate 32 0x11
            , PD.B $ BS.replicate 32 0x22
            ]
        ]

runPostFinalize :: forall s. Integer -> BS.ByteString -> BS.ByteString -> PD.Data -> PD.Data -> [(ScriptPurpose, Redeemer)] -> Term s PUnit
runPostFinalize observedNetwork postDescriptor previousRoot predecessorClaim predecessorCarriage withdrawals =
  runPostFinalizeWithRefs observedNetwork postDescriptor previousRoot predecessorClaim predecessorCarriage withdrawals []

runPostFinalizeWithRefs :: forall s. Integer -> BS.ByteString -> BS.ByteString -> PD.Data -> PD.Data -> [(ScriptPurpose, Redeemer)] -> [TxInInfo] -> Term s PUnit
runPostFinalizeWithRefs observedNetwork postDescriptor previousRoot predecessorClaim predecessorCarriage withdrawals refs =
  step02Validator $
    spendContext
      (stepDatum $ Just state)
      (PD.Constr 1 [step02Args Nothing (Just predecessorCarriage)])
      [threadInput]
      [convictionOutput fraudProofAddress threadName]
      refs
      (fraudProofMintEntry threadName : withdrawals)
      (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)
  where
    fault = postUtxoFault observedNetwork
    postState = PD.Constr 0 [outRefData, PD.B postDescriptor, PD.B previousRoot, predecessorClaim]
    state = step02State (BS.replicate 32 0xcc) 255 0 fault (Just postState)

txIdFromOutRef :: PD.Data -> BS.ByteString
txIdFromOutRef (PD.Constr 0 [PD.B txId, PD.I _]) = txId
txIdFromOutRef _ = error "invalid output reference fixture"

ledgerKey, forgedLedgerKey :: BS.ByteString
ledgerKey = encodedLedgerKey 0xcc
forgedLedgerKey = encodedLedgerKey 0xdd

encodedLedgerKey :: Word8 -> BS.ByteString
encodedLedgerKey byte = "\x82" <> defBytes32 (BS.replicate 32 byte) <> "\x19\x00\x07"

rootForDescriptor :: BS.ByteString -> BS.ByteString
rootForDescriptor = singleEntryPhasRoot ledgerKey

foreignDescriptor, foreignRoot, emptyMerkleRoot :: BS.ByteString
foreignDescriptor = descriptorAtNetwork 2 False
foreignRoot = rootForDescriptor foreignDescriptor
emptyMerkleRoot = blake2b256 ""

descriptorAtNetwork :: Integer -> Bool -> BS.ByteString
descriptorAtNetwork networkId protected =
  BS.take 42 canonicalDescriptor
    <> BS.singleton (fromIntegral $ 0x60 + networkId + if protected then 8 else 0)
    <> BS.drop 43 canonicalDescriptor

canonicalDescriptor :: BS.ByteString
canonicalDescriptor =
  Base16.decodeLenient
    "900107191388582013e167684e9dc284acc6ebbe972cd2cf0763d03bba558bae463825f3f35990d6581d60111111111111111111111111111111111111111111111111111111111a004c4b40005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358202222222222222222222222222222222222222222222222222222222222222222186518ca8358203333333333333333333333333333333333333333333333333333333333333333186718cc83582044444444444444444444444444444444444444444444444444444444444444440304"

--------------------------------------------------------------------------------
-- Published proof carriage
--------------------------------------------------------------------------------

publishedPostUtxoTests :: [TestTree]
publishedPostUtxoTests =
  [ testCase "step 01 authenticates a maximum 64-step published membership" $
      psucceeds $ runPublishedPostStep01 [2, 3, 4, 5] [2, 3, 4, 5]
  , testCase "step 01 rejects reordered published chunks" $
      pfails $ runPublishedPostStep01 [2, 4, 3, 5] [2, 4, 3, 5]
  , testCase "step 01 rejects a cross-step chunk-order mismatch" $
      pfails $ runPublishedPostStep01 [2, 3, 4, 5] [2, 4, 3, 5]
  , testCase "step 02 authenticates a maximum 64-step published predecessor membership" $
      psucceeds $ runPublishedChanged [0, 1, 2, 3] [0, 1, 2, 3]
  , testCase "step 02 authenticates a maximum 64-step published predecessor non-membership" $
      psucceeds runPublishedIntroduced
  , testCase "step 02 rejects reordered published predecessor chunks" $
      pfails $ runPublishedChanged [0, 2, 1, 3] [0, 2, 1, 3]
  , testCase "step 02 rejects a cross-step predecessor chunk-order mismatch" $
      pfails $ runPublishedChanged [0, 1, 2, 3] [0, 2, 1, 3]
  ]

runPublishedPostStep01 :: forall s. [Integer] -> [Integer] -> Term s PUnit
runPublishedPostStep01 carriageIndices claimIndices =
  pif directCheck validatorCheck perror
  where
    steps = adversarialBranchSteps 64
    chunks = proofChunkReferenceInputs 2 steps
    root = membershipRootOf ledgerKey foreignDescriptor steps
    refs = referenceInputsWithUtxosRoots emptyMerkleRoot root <> chunks
    carriage = publishedMembershipCarriage carriageIndices
    claim = chunkClaim 0 root ledgerKey (blake2b256 foreignDescriptor) claimIndices
    directCheck =
      ppublishedChunkMembership
        # pconstant refs
        # publishedProofCarriageT carriageIndices
        # pconstant root
        # pconstant ledgerKey
        # pconstant foreignDescriptor
    validatorCheck =
      runPostStep01WithEvidence
        defaultPost01 {p1HeaderPostRoot = root}
        carriage
        refs
        [chunkEntry claim]

runPublishedChanged :: forall s. [Integer] -> [Integer] -> Term s PUnit
runPublishedChanged carriageIndices claimIndices =
  pif directCheck validatorCheck perror
  where
    previousDescriptor = descriptorAtNetwork 0 False
    steps = adversarialBranchSteps 64
    root = membershipRootOf ledgerKey previousDescriptor steps
    chunks = proofChunkReferenceInputs 0 steps
    carriage = publishedChangedCarriage carriageIndices
    claim = chunkClaim 0 root ledgerKey (blake2b256 previousDescriptor) claimIndices
    directCheck =
      ppublishedChunkMembership
        # pconstant chunks
        # publishedProofCarriageT carriageIndices
        # pconstant root
        # pconstant ledgerKey
        # pconstant previousDescriptor
    validatorCheck =
      runPostFinalizeWithRefs
        2
        foreignDescriptor
        root
        (changedClaim previousDescriptor)
        carriage
        [chunkEntry claim]
        chunks

runPublishedIntroduced :: forall s. Term s PUnit
runPublishedIntroduced = pif directCheck validatorCheck perror
  where
    steps = adversarialNonMembershipSteps 63
    root = nonMembershipRootOf ledgerKey 0 steps
    chunks = proofChunkReferenceInputs 0 steps
    indices = [0, 1, 2, 3]
    carriage = publishedIntroducedCarriage indices
    claim = chunkClaim 1 root ledgerKey (BS.replicate 32 0) indices
    directCheck =
      ppublishedChunkNonMembership
        # pconstant chunks
        # publishedProofCarriageT indices
        # pconstant root
        # pconstant ledgerKey
    validatorCheck =
      runPostFinalizeWithRefs
        2
        foreignDescriptor
        root
        introducedClaim
        carriage
        [chunkEntry claim]
        chunks

publishedProofCarriageData :: [Integer] -> PD.Data
publishedProofCarriageData indices = PD.Constr 0 [PD.List $ map PD.I indices]

publishedProofCarriageT :: forall s. [Integer] -> Term s PPublishedProofCarriage
publishedProofCarriageT indices =
  pfromData $ punsafeCoerce $ pconstant @PData $ publishedProofCarriageData indices

publishedMembershipCarriage :: [Integer] -> PD.Data
publishedMembershipCarriage indices = PD.Constr 1 [publishedProofCarriageData indices]

publishedChangedCarriage :: [Integer] -> PD.Data
publishedChangedCarriage indices = PD.Constr 1 [publishedMembershipCarriage indices]

publishedIntroducedCarriage :: [Integer] -> PD.Data
publishedIntroducedCarriage indices =
  PD.Constr 0 [PD.Constr 1 [publishedProofCarriageData indices]]

chunkClaim :: Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString -> [Integer] -> PD.Data
chunkClaim mode root key valueHash indices =
  PD.Constr
    0
    [ PD.Constr mode []
    , PD.B root
    , PD.B key
    , PD.B valueHash
    , PD.List $ map PD.I indices
    ]

chunkEntry :: PD.Data -> (ScriptPurpose, Redeemer)
chunkEntry claim =
  ( Rewarding $ ScriptCredential $ ScriptHash $ toBuiltin chunkedVerifyHash
  , Redeemer $ dataToBuiltinData claim
  )

chunkedVerifyHash :: BS.ByteString
chunkedVerifyHash = Base16.decodeLenient "dfd0e01fe351bd1d6f75a1ba728d06fb8b11d56bc3bf9ee98e025040"

data ProofStepRef
  = BranchStep Integer BS.ByteString
  | LeafStep Integer BS.ByteString BS.ByteString

stepData :: ProofStepRef -> PD.Data
stepData (BranchStep skip neighbors) = PD.Constr 0 [PD.I skip, PD.B neighbors]
stepData (LeafStep skip key value) = PD.Constr 2 [PD.I skip, PD.B key, PD.B value]

adversarialBranchSteps :: Int -> [ProofStepRef]
adversarialBranchSteps count =
  [BranchStep 0 $ branchNeighbors seed | seed <- [count - 1, count - 2 .. 0]]

adversarialNonMembershipSteps :: Int -> [ProofStepRef]
adversarialNonMembershipSteps branchCount =
  adversarialBranchSteps branchCount
    <> [LeafStep 0 (blake2b256 "q35-absence") (BS.replicate 32 0x6a)]

branchNeighbors :: Int -> BS.ByteString
branchNeighbors seed = a <> b <> c <> d
  where
    a = blake2b256 $ cborInt $ fromIntegral seed
    b = blake2b256 a
    c = blake2b256 b
    d = blake2b256 c

membershipRootOf :: BS.ByteString -> BS.ByteString -> [ProofStepRef] -> BS.ByteString
membershipRootOf key value = foldMembershipRoot (blake2b256 key) (blake2b256 value) 0

foldMembershipRoot :: BS.ByteString -> BS.ByteString -> Int -> [ProofStepRef] -> BS.ByteString
foldMembershipRoot path valueHash cursor [] = combine (suffix path cursor) valueHash
foldMembershipRoot path valueHash cursor (BranchStep skip neighbors : rest) =
  branchRoot path cursor skip neighbors $ foldMembershipRoot path valueHash nextCursor rest
  where
    nextCursor = cursor + 1 + fromIntegral skip
foldMembershipRoot _ _ _ (LeafStep {} : _) = BS.replicate 32 0

nonMembershipRootOf :: BS.ByteString -> Int -> [ProofStepRef] -> BS.ByteString
nonMembershipRootOf pathBytes = foldNonMembershipRoot (blake2b256 pathBytes)

foldNonMembershipRoot :: BS.ByteString -> Int -> [ProofStepRef] -> BS.ByteString
foldNonMembershipRoot _ cursor [LeafStep _ key value] = combine (suffix key cursor) value
foldNonMembershipRoot path cursor (BranchStep skip neighbors : rest) =
  branchRoot path cursor skip neighbors $ foldNonMembershipRoot path nextCursor rest
  where
    nextCursor = cursor + 1 + fromIntegral skip
foldNonMembershipRoot _ _ _ = BS.replicate 32 0

branchRoot :: BS.ByteString -> Int -> Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString
branchRoot path cursor skip neighbors childRoot =
  combine
    (nibbles path cursor $ nextCursor - 1)
    ( merkle16
        (nibble path $ nextCursor - 1)
        childRoot
        (BS.take 32 neighbors)
        (BS.take 32 $ BS.drop 32 neighbors)
        (BS.take 32 $ BS.drop 64 neighbors)
        (BS.take 32 $ BS.drop 96 neighbors)
    )
  where
    nextCursor = cursor + 1 + fromIntegral skip

combine :: BS.ByteString -> BS.ByteString -> BS.ByteString
combine left right = blake2b256 $ left <> right

nibble :: BS.ByteString -> Int -> Int
nibble path index
  | even index = fromIntegral (BS.index path $ index `div` 2) `shiftR` 4
  | otherwise = fromIntegral (BS.index path $ index `div` 2) .&. 0x0f

nibbles :: BS.ByteString -> Int -> Int -> BS.ByteString
nibbles path start end = BS.pack [fromIntegral $ nibble path i | i <- [start .. end - 1]]

suffix :: BS.ByteString -> Int -> BS.ByteString
suffix path cursor
  | even cursor = BS.cons 0xff $ BS.drop (cursor `div` 2) path
  | otherwise =
      BS.pack [0, fromIntegral $ nibble path cursor]
        <> BS.drop ((cursor + 1) `div` 2) path

merkle16 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle16 branch root n8 n4 n2 n1
  | branch <= 7 = combine (merkle8 branch root n4 n2 n1) n8
  | otherwise = combine n8 (merkle8 (branch - 8) root n4 n2 n1)

merkle8 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle8 branch root n4 n2 n1
  | branch <= 3 = combine (merkle4 branch root n2 n1) n4
  | otherwise = combine n4 (merkle4 (branch - 4) root n2 n1)

merkle4 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle4 branch root n2 n1
  | branch <= 1 = combine (merkle2 branch root n1) n2
  | otherwise = combine n2 (merkle2 (branch - 2) root n1)

merkle2 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle2 branch root n1
  | branch == 0 = combine root n1
  | otherwise = combine n1 root

proofChunkReferenceInputs :: Integer -> [ProofStepRef] -> [TxInInfo]
proofChunkReferenceInputs offset steps =
  [ chunkRefInput (offset + fromIntegral index) chunk
  | (index, chunk) <- zip [0 :: Int ..] $ chunksOf 16 steps
  ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size items = take size items : chunksOf size (drop size items)

chunkRefInput :: Integer -> [ProofStepRef] -> TxInInfo
chunkRefInput index steps =
  TxInInfo
    (TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x11) index)
    ( TxOut
        (scriptHashAddress $ ScriptHash $ toBuiltin $ BS.replicate 28 0x90)
        mempty
        (OutputDatum $ Datum $ dataToBuiltinData $ PD.Constr 0 [PD.List $ map stepData steps])
        Nothing
    )
