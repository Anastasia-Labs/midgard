{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsMinAda (tests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.Word (Word8)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (
  Credential (..),
  Datum (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (..),
  ScriptPurpose (..),
  TokenName (..),
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (builtinDataToData, dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import MerkleTree.Validators.Membership (membershipStakeValidator, nonMembershipStakeValidator)
import Midgard.FraudProofs.ChunkedInclusion (
  PPublishedProofCarriage,
  ppublishedChunkMembership,
  ppublishedChunkNonMembership,
 )
import Midgard.ValidationMachine (
  pcoinsPerUtxoByte,
  pminAdaLovelaceV1,
  poutputMeetsMinAdaV1,
 )
import Midgard.Validators.FraudProofs.MinAda
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Testing.Eval (passertEvalNoTrace, pfailsNoTraceWithoutHoistChecks, psucceedsNoTraceWithoutHoistChecks)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Min-Ada fraud proof"
    [ testCase "step 01 binds an accepted transaction output" $ psucceeds $ runTxStep01 (-1) 0 0
    , testCase "step 01 rejects an invalid transaction leaf" $ pfails $ runTxStep01 (-1) 0 1
    , testCase "step 01 rejects a negative output index" $ pfails $ runTxStep01 (-1) (-1) 0
    , testCase "forced source binds the exact min-Ada output" $ psucceeds $ runForcedStep01 1 45 0 0 1 Nothing
    , testCase "forced source rejects a different reason" $ pfails $ runForcedStep01 1 26 0 0 1 Nothing
    , testCase "forced source rejects another output coordinate" $ pfails $ runForcedStep01 1 45 1 0 1 Nothing
    , testCase "forced source rejects a negative output coordinate" $ pfails $ runForcedStep01 1 45 (-1) (-1) 1 Nothing
    , testCase "forced source rejects the accepted direction" $ pfails $ runForcedStep01 1 45 0 0 0 Nothing
    , testCase "forced source rejects accepted native validity" $ pfails $ runForcedStep01 0 45 0 0 1 Nothing
    , testCase "forced source rejects an existing prior state" $ pfails $ runForcedStep01 1 45 0 0 1 (Just terminalState)
    , testCase "yield handshake accepts the selected unique spend" $ psucceeds $ runYieldMutation id
    , testCase "yield handshake rejects nonzero withdrawal" $ pfails $ runYieldMutation $ \tx -> tx{txInfoWdrl = AssocMap.unsafeFromList [(yieldCredential, 1)]}
    , testCase "yield handshake rejects missing withdrawal" $ pfails $ runYieldMutation $ \tx -> tx{txInfoWdrl = AssocMap.empty}
    , testCase "yield handshake rejects missing rewarding redeemer" $ pfails $ runYieldMutation $ \tx -> tx{txInfoRedeemers = AssocMap.unsafeFromList $ filter ((/= Rewarding yieldCredential) . fst) $ AssocMap.toList $ txInfoRedeemers tx}
    , testCase "yield handshake rejects another arm's role NFT" $ pfails $ runYieldMutation $ \tx -> tx{txInfoReferenceInputs = [i{txInInfoResolved = (txInInfoResolved i){txOutValue = adaValue 2_000_000 <> singleton certificatePolicy (TokenName $ toBuiltin ("V1FpMinAdaS02TxYield" :: BS.ByteString)) 1}} | i <- txInfoReferenceInputs tx]}
    , testCase "yield handshake rejects missing reference script" $ pfails $ runYieldMutation $ \tx -> tx{txInfoReferenceInputs = [i{txInInfoResolved = (txInInfoResolved i){txOutReferenceScript = Nothing}} | i <- txInfoReferenceInputs tx]}
    , testCase "yield rejects two dispatcher inputs" $ pfails $ runYieldMutation $ \tx -> tx{txInfoInputs = txInfoInputs tx <> txInfoInputs tx}
    , testCase "yield rejects a missing dispatcher redeemer" $ pfails $ runYieldMutation $ \tx -> tx{txInfoRedeemers = AssocMap.unsafeFromList $ filter ((/= Spending ownRef) . fst) $ AssocMap.toList $ txInfoRedeemers tx}
    , testCase "step 01 binds a post-UTxO descriptor" $ psucceeds $ runPostStep01 defaultPost
    , testCase "step 02 rejects a forged post value" $ pfails $ runPostMembership defaultMembership{pmDescriptor = descriptorAt 0}
    , testCase "step 02 rejects a forged post key" $ pfails $ runPostMembership defaultMembership{pmOutRef = outRefDataFor 0xdd}
    , testCase "step 02 rejects a forged post root" $ pfails $ runPostMembership defaultMembership{pmStatePostRoot = otherRoot}
    , testCase "step 01 binds a maximum post claim without walking it" $ psucceeds runMaximumPostStep01
    , testCase "step 02 authenticates maximum published post membership" $ psucceeds $ runPublishedPostMembership [0, 1, 2, 3]
    , testCase "step 02 rejects reordered published post chunks" $ pfails $ runPublishedPostMembership [0, 2, 1, 3]
    , testCase "transaction convicts one lovelace below the exact floor" $ psucceeds $ runTxPredicate 0 (-1) nextScript
    , testCase "transaction rejects the exact floor" $ pfails $ runTxPredicate 0 0 nextScript
    , testCase "transaction rejects one lovelace above the floor" $ pfails $ runTxPredicate 0 1 nextScript
    , testCase "transaction rejects a forged or malformed output opening" $ pfails $ runInvalidOpening
    , testCase "wrongful rejection convicts at the exact floor" $ psucceeds $ runTxPredicate 1 0 nextScript
    , testCase "wrongful rejection convicts above the floor" $ psucceeds $ runTxPredicate 1 1 nextScript
    , testCase "honest rejection refuses below the floor" $ pfails $ runTxPredicate 1 (-1) nextScript
    , testCase "unknown verdict direction refuses" $ pfails $ runTxPredicate 2 (-1) nextScript
    , testCase "UTxO convicts a new underfunded post member" $ psucceeds runPostConviction
    , testCase "UTxO rejects the exact floor" $ pfails $ runPostPredicate (descriptorAt 0) nextScript
    , testCase "UTxO rejects an inherited underfunded member" $ pfails $ runPredecessor defaultPredecessor{prStateRoot = postRoot, prClaimedRoot = postRoot}
    , testCase "UTxO rejects a forged predecessor root" $ pfails $ runPredecessor defaultPredecessor{prClaimedRoot = otherRoot}
    , testCase "UTxO rejects a forged predecessor key" $ pfails $ runPredecessor defaultPredecessor{prClaimedKey = ledgerKeyFor 0xee}
    , testCase "UTxO rejects malformed descriptor bytes" $ pfails $ runPostPredicate "\x80" nextScript
    , testCase "UTxO authenticates maximum descriptor predicate" $ psucceeds $ runPostPredicate maximumDescriptor nextScript
    , testCase "UTxO authenticates maximum published predecessor non-membership" $ psucceeds $ runPublishedPredecessor [0, 1, 2, 3] maximumAbsentRoot
    , testCase "UTxO rejects reordered published predecessor chunks" $ pfails $ runPublishedPredecessor [0, 2, 1, 3] maximumAbsentRoot
    , testCase "UTxO rejects a forged published predecessor root" $ pfails $ runPublishedPredecessor [0, 1, 2, 3] otherRoot
    , testCase "five partially packed chunks carry genuine proofs within the general bound" $ passertEvalNoTrace fiveChunkProofsAreValid
    , testCase "post membership rejects five genuine published chunks" $
        pfails $
          runPublishedPostMembershipWithChunks (partiallyPackedChunks maximumSteps) [0, 1, 2, 3, 4]
    , testCase "post membership rejects duplicate chunk indices" $ pfails $ runPublishedPostMembership [0, 1, 1, 3]
    , testCase "predecessor rejects duplicate chunk indices" $ pfails $ runPublishedPredecessor [0, 1, 1, 3] maximumAbsentRoot
    , testCase "post membership rejects a negative chunk index" $ pfails $ runPublishedPostMembership [0, -1, 2, 3]
    , testCase "post membership rejects an out-of-range chunk index" $ pfails $ runPublishedPostMembership [0, 1, 2, 4]
    , testCase "predecessor rejects a negative chunk index" $ pfails $ runPublishedPredecessor [0, -1, 2, 3] maximumAbsentRoot
    , testCase "predecessor rejects an out-of-range chunk index" $ pfails $ runPublishedPredecessor [0, 1, 2, 4] maximumAbsentRoot
    , testCase "published empty predecessor sentinel needs no chunks" $ psucceeds $ runPublishedPredecessorWithChunks [] [] emptyRoot
    , testCase "UTxO rejects more than four published chunks" $ pfails runFivePublishedChunks
    , testCase "transaction cannot continue to the UTxO predecessor step" $ pfails $ runTxPredicate 0 (-1) stepScript
    , testCase "UTxO cannot skip the predecessor step" $ pfails $ runPostPredicate (descriptorAt (-1)) stepScript
    , testCase "predecessor step rejects a wrong terminal successor" $ pfails $ runPredecessor defaultPredecessor{prSuccessor = stepScript}
    , testCase "family binds the exact production parameter snapshot" $ psucceeds productionSnapshot
    , testCase "step 05 finalizes only an adjudicated thread" $ psucceeds runStep05
    , testCase "steps preserve prover cancellation" $ psucceeds runCancellations
    ]

psucceeds, pfails :: (forall s. Term s a) -> Assertion
psucceeds = psucceedsNoTraceWithoutHoistChecks
pfails = pfailsNoTraceWithoutHoistChecks

some :: PD.Data -> PD.Data
some x = PD.Constr 0 [x]

none :: PD.Data
none = PD.Constr 1 []

txFault :: Integer -> PD.Data
txFault outputIndex = PD.Constr 0 [PD.I outputIndex]

utxoFault :: PD.Data
utxoFault = PD.Constr 1 []

terminalState :: PD.Data
terminalState = PD.Constr 0 []

step01Args :: Maybe PD.Data -> Maybe PD.Data -> PD.Data -> PD.Data
step01Args inclusion membership fault =
  PD.Constr 0 [none, maybe none some inclusion, maybe none some membership, fault]

step02State :: BS.ByteString -> PD.Data -> Maybe PD.Data -> PD.Data
step02State badTxId fault post =
  PD.Constr 0 [PD.B "", PD.Constr 0 [], PD.B "", PD.I 0, PD.B badTxId, fault, maybe none some post]

step02Args :: Maybe PD.Data -> Maybe PD.Data -> PD.Data
step02Args opening membership =
  PD.Constr 0 [PD.B "", PD.B "", PD.I 0, PD.I 0, PD.I 0, maybe none some opening, maybe none some membership]

step03State :: BS.ByteString -> BS.ByteString -> PD.Data
step03State descriptor previousRoot =
  PD.Constr 2 [PD.B descriptor, PD.B ledgerKey, PD.B previousRoot]

step04State :: BS.ByteString -> PD.Data
step04State previousRoot = PD.Constr 0 [PD.B ledgerKey, PD.B previousRoot]

continueArgs :: PD.Data
continueArgs = PD.Constr 0 [PD.B "", PD.I 0, PD.I 0]

step04Args :: PD.Data -> PD.Data
step04Args carriage = PD.Constr 0 [PD.I 0, PD.I 0, carriage]

step05Args :: PD.Data
step05Args = PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0]

step01, step02, step03, step04, step05 :: forall s. ScriptContext -> Term s PUnit
step01 ctx =
  minAdaStep01Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step02 ctx =
  minAdaStep02Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx
step03 ctx =
  minAdaStep03Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant ctx
step04 ctx =
  minAdaStep04Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant ctx
step05 ctx =
  minAdaStep05Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant ctx

-- Transaction route

outputAt :: Integer -> BS.ByteString
outputAt delta =
  let base = midgardOutputCbor (pubKeyAddressBytes $ keyHashFor 0) 2_000_000 Nothing
      floorLovelace = 4_310 * (160 + fromIntegral (BS.length base))
      output = midgardOutputCbor (pubKeyAddressBytes $ keyHashFor 0) (floorLovelace + delta) Nothing
   in if BS.length output == BS.length base then output else error "min-Ada fixture changed CBOR width"

outputPreimage :: BS.ByteString -> BS.ByteString
outputPreimage output = arrayHeader 1 <> wrapItem output

compactBodyFor :: BS.ByteString -> BS.ByteString
compactBodyFor output =
  BS.concat
    [ "\x8c"
    , defBytes32 $ blake2b256 $ spendInputsPreimage tx1
    , defBytes32 $ blake2b256 $ referenceInputsPreimage tx1
    , defBytes32 $ blake2b256 $ outputPreimage output
    , cborInt $ tFee tx1
    , cborInt $ tValidityStart tx1
    , cborInt $ tValidityEnd tx1
    , defBytes32 $ hash32 0x04
    , defBytes32 $ blake2b256 $ requiredSignersPreimage tx1
    , defBytes32 $ hash32 0x06
    , defBytes32 $ hash32 0x07
    , defBytes32 $ hash32 0x08
    , cborInt 1
    ]

compactFor :: BS.ByteString -> Integer -> BS.ByteString
compactFor output validity =
  "\x84\x01" <> compactBodyFor output <> defBytes32 (witnessSetHashOf tx1) <> cborInt validity

txIdFor :: BS.ByteString -> BS.ByteString
txIdFor output = blake2b256 $ "MidgardNativeTxBodyV1\x01" <> compactBodyFor output

runTxStep01 :: forall s. Integer -> Integer -> Integer -> Term s PUnit
runTxStep01 delta outputIndex validity = step01 context
 where
  output = outputAt delta
  compact = compactFor output validity
  txId = txIdFor output
  source = sourceCborFor txId compact (witnessSetCborOf tx1) (fieldPreimageLengthsCborOf tx1)
  root = singleEntryPhasRoot txId source
  fault = txFault outputIndex
  context =
    spendContext
      (stepDatum Nothing)
      (PD.Constr 1 [step01Args (Just $ inclusionArgs txId source root) Nothing fault])
      [threadInput]
      [stepOutput nextScript $ Just $ step02State txId fault Nothing]
      (referenceInputsWithTransactionsRoot $ commitCountedRoot transactionsDomain root l2Count)
      [phasEntry root txId source]
      mempty

runTxPredicate :: forall s. Integer -> Integer -> BS.ByteString -> Term s PUnit
runTxPredicate direction delta successor =
  minAdaStep03Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin stepScript)
    # pdata (pconstant $ ScriptHash $ toBuiltin successor)
    # pdata (pconstant ctPolicy)
    # pconstant context
 where
  context =
    spendContext
      (stepDatum $ Just $ PD.Constr 1 [PD.I direction, PD.I $ fromIntegral $ BS.length $ outputAt delta, PD.I $ outputLovelace delta])
      (PD.Constr 1 [continueArgs])
      [threadInput]
      [stepOutput nextScript $ Just terminalState]
      []
      []
      mempty

-- Invoke both halves of the ledger's rewarding/spending conjunction, retaining
-- proof reference/redeemer indices and adding the authenticated role at the end.
runUtxoYield :: forall s. ScriptContext -> Term s PUnit
runUtxoYield original =
  plet (step02 context) $ \_ ->
    minAdaUtxoYieldValidator
      # pdata (pconstant $ ScriptHash $ toBuiltin stepScript)
      # pconstant (asRewarding context)
 where
  context = yieldedContext "V1FpMinAdaS02UtxoYield" original

yieldedContext :: BS.ByteString -> ScriptContext -> ScriptContext
yieldedContext role (ScriptContext tx redeemer info) = ScriptContext nextTx nextRedeemer info
 where
  datum = case info of SpendingScript _ (Just d) -> d; _ -> error "expected spend datum"
  refIndex = fromIntegral $ length $ txInfoReferenceInputs tx
  nextRedeemer = case redeemer of
    Redeemer d -> case builtinDataToData d of
      PD.Constr 1 [PD.Constr 0 [grammar, walk, i, o, _, opening, membership]] ->
        Redeemer $ dataToBuiltinData $ PD.Constr 1 [PD.Constr 0 [grammar, walk, i, o, PD.I refIndex, opening, membership]]
      _ -> error "expected step-02 continue"
  credential = ScriptCredential $ ScriptHash $ toBuiltin stepScript
  roleInput =
    TxInInfo (TxOutRef (TxId $ toBuiltin $ hash32 0x89) 0) $
      TxOut
        (scriptHashAddress $ ScriptHash $ toBuiltin otherScript)
        (adaValue 2_000_000 <> singleton certificatePolicy (TokenName $ toBuiltin role) 1)
        NoOutputDatum
        (Just $ ScriptHash $ toBuiltin stepScript)
  nextTx =
    tx
      { txInfoInputs = [input{txInInfoResolved = (txInInfoResolved input){txOutDatum = OutputDatum datum}} | input <- txInfoInputs tx]
      , txInfoReferenceInputs = txInfoReferenceInputs tx <> [roleInput]
      , txInfoWdrl = AssocMap.unsafeFromList [(credential, 0)]
      , txInfoRedeemers =
          AssocMap.unsafeFromList $
            AssocMap.toList (txInfoRedeemers tx)
              <> [(Spending ownRef, nextRedeemer), (Rewarding credential, Redeemer $ dataToBuiltinData terminalState)]
      }

runInvalidOpening :: forall s. Term s PUnit
runInvalidOpening =
  minAdaTxYieldValidator
    # pdata (pconstant $ ScriptHash $ toBuiltin stepScript)
    # pdata (pconstant certificatePolicy)
    # pconstant (asRewarding $ yieldedContext "V1FpMinAdaS02TxYield" context)
 where
  output = outputAt (-1)
  context =
    spendContext
      (stepDatum $ Just $ step02State (txIdFor output) (txFault 0) Nothing)
      (PD.Constr 1 [step02Args (Just $ bodyOpening (compactFor output 0) $ outputPreimage "\xff") Nothing])
      [threadInput]
      [stepOutput stepScript $ Just terminalState]
      []
      []
      mempty

-- Post-UTxO route

outRefDataFor :: Word8 -> PD.Data
outRefDataFor byte = PD.Constr 0 [PD.B $ BS.replicate 32 byte, PD.I 0]

outRefData :: PD.Data
outRefData = outRefDataFor 0xcc

ledgerKeyFor :: Word8 -> BS.ByteString
ledgerKeyFor byte = "\x82" <> defBytes32 (BS.replicate 32 byte) <> "\x19\x00\x00"

ledgerKey :: BS.ByteString
ledgerKey = ledgerKeyFor 0xcc

emptyRoot, postRoot :: BS.ByteString
emptyRoot = blake2b256 ""
postRoot = singleEntryPhasRoot ledgerKey (descriptorAt (-1))

postState :: PD.Data -> BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
postState outRef descriptor currentRoot previousRoot =
  PD.Constr 0 [outRef, PD.B descriptor, PD.B currentRoot, PD.B previousRoot]

postMembershipArgs :: PD.Data -> BS.ByteString -> PD.Data
postMembershipArgs outRef descriptor =
  PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 1, outRef, PD.B descriptor]

data Post01 = Post01
  { p01OutRef :: PD.Data
  , p01Descriptor :: BS.ByteString
  , p01PostRoot :: BS.ByteString
  , p01PreviousRoot :: BS.ByteString
  }

defaultPost :: Post01
defaultPost = Post01 outRefData (descriptorAt (-1)) postRoot emptyRoot

runPostStep01 :: forall s. Post01 -> Term s PUnit
runPostStep01 p = step01 context
 where
  expectedPost = postState (p01OutRef p) (p01Descriptor p) (p01PostRoot p) (p01PreviousRoot p)
  context =
    spendContext
      (stepDatum Nothing)
      (PD.Constr 1 [step01Args Nothing (Just $ postMembershipArgs (p01OutRef p) $ p01Descriptor p) utxoFault])
      [threadInput]
      [stepOutput nextScript $ Just $ step02State (BS.replicate 32 0xcc) utxoFault (Just expectedPost)]
      (referenceInputsWithUtxosRoots (p01PreviousRoot p) (p01PostRoot p))
      []
      mempty

data PostMembership = PostMembership
  { pmOutRef :: PD.Data
  , pmDescriptor :: BS.ByteString
  , pmStatePostRoot :: BS.ByteString
  , pmPreviousRoot :: BS.ByteString
  , pmClaimedRoot :: BS.ByteString
  , pmClaimedKey :: BS.ByteString
  , pmClaimedValue :: BS.ByteString
  }

defaultMembership :: PostMembership
defaultMembership =
  PostMembership outRefData (descriptorAt (-1)) postRoot emptyRoot postRoot ledgerKey (descriptorAt (-1))

redeemerCarriedMembership :: PD.Data
redeemerCarriedMembership = PD.Constr 0 [emptyProof, PD.I 0]

runPostMembership :: forall s. PostMembership -> Term s PUnit
runPostMembership p =
  plet (runUtxoYield context) $ \_ ->
    membershipStakeValidator # pconstant (proofContext membershipRedeemer)
 where
  post = postState (pmOutRef p) (pmDescriptor p) (pmStatePostRoot p) (pmPreviousRoot p)
  nextState = PD.Constr 2 [PD.B $ pmDescriptor p, PD.B $ ledgerKeyOf $ pmOutRef p, PD.B $ pmPreviousRoot p]
  context =
    spendContext
      (stepDatum $ Just $ step02State (txIdOfRef $ pmOutRef p) utxoFault (Just post))
      (PD.Constr 1 [step02Args Nothing $ Just redeemerCarriedMembership])
      [threadInput]
      [stepOutput nextScript $ Just nextState]
      []
      [phasEntry (pmClaimedRoot p) (pmClaimedKey p) (pmClaimedValue p)]
      mempty
  membershipRedeemer =
    PD.List [PD.B $ pmClaimedRoot p, PD.B $ pmClaimedKey p, PD.B $ pmClaimedValue p, emptyProof]

txIdOfRef :: PD.Data -> BS.ByteString
txIdOfRef (PD.Constr 0 [PD.B txId, PD.I _]) = txId
txIdOfRef _ = error "invalid output reference fixture"

ledgerKeyOf :: PD.Data -> BS.ByteString
ledgerKeyOf ref = case txIdOfRef ref of
  txId -> "\x82" <> defBytes32 txId <> "\x19\x00\x00"

runMaximumPostStep01 :: forall s. Term s PUnit
runMaximumPostStep01 = runPostStep01 $ Post01 outRefData maximumDescriptor maximumPostRoot emptyRoot

maximumSteps :: [ProofStepRef]
maximumSteps = adversarialBranchSteps 64

maximumPostRoot :: BS.ByteString
maximumPostRoot = membershipRootOf ledgerKey maximumDescriptor maximumSteps

runPublishedPostMembership :: forall s. [Integer] -> Term s PUnit
runPublishedPostMembership = runPublishedPostMembershipWithChunks (proofChunkReferenceInputs 0 maximumSteps)

runPublishedPostMembershipWithChunks :: forall s. [TxInInfo] -> [Integer] -> Term s PUnit
runPublishedPostMembershipWithChunks chunks indices = validatorCheck
 where
  carriage = publishedMembershipCarriage indices
  validatorCheck =
    runUtxoYield $
      spendContext
        (stepDatum $ Just $ step02State (BS.replicate 32 0xcc) utxoFault $ Just post)
        (PD.Constr 1 [step02Args Nothing $ Just carriage])
        [threadInput]
        [stepOutput nextScript $ Just $ step03State maximumDescriptor emptyRoot]
        chunks
        []
        mempty
  post = postState outRefData maximumDescriptor maximumPostRoot emptyRoot

runPostPredicate :: forall s. BS.ByteString -> BS.ByteString -> Term s PUnit
runPostPredicate descriptor successor =
  minAdaStep03Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin successor)
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant context
 where
  context =
    spendContext
      (stepDatum $ Just $ step03State descriptor emptyRoot)
      (PD.Constr 1 [continueArgs])
      [threadInput]
      [stepOutput nextScript $ Just $ step04State emptyRoot]
      []
      []
      mempty

data Predecessor = Predecessor
  { prStateRoot :: BS.ByteString
  , prClaimedRoot :: BS.ByteString
  , prClaimedKey :: BS.ByteString
  , prSuccessor :: BS.ByteString
  }

defaultPredecessor :: Predecessor
defaultPredecessor = Predecessor emptyRoot emptyRoot ledgerKey nextScript

runPredecessor :: forall s. Predecessor -> Term s PUnit
runPredecessor p =
  plet
    ( minAdaStep04Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin $ prSuccessor p)
        # pdata (pconstant ctPolicy)
        # pconstant context
    )
    $ \_ -> nonMembershipStakeValidator # pconstant (proofContext exclusionRedeemer)
 where
  context =
    spendContext
      (stepDatum $ Just $ step04State $ prStateRoot p)
      (PD.Constr 1 [step04Args redeemerCarriedNonMembership])
      [threadInput]
      [stepOutput nextScript $ Just terminalState]
      []
      [pexcludesEntry (prClaimedRoot p) (prClaimedKey p)]
      mempty
  exclusionRedeemer = PD.List [PD.B $ prClaimedRoot p, PD.B $ prClaimedKey p, emptyProof]

runPostConviction :: forall s. Term s PUnit
runPostConviction =
  plet (runPostMembership defaultMembership) $ \_ ->
    plet (runPostPredicate (descriptorAt (-1)) nextScript) $ \_ ->
      runPredecessor defaultPredecessor

maximumAbsentSteps :: [ProofStepRef]
maximumAbsentSteps = adversarialNonMembershipSteps 63

maximumAbsentRoot :: BS.ByteString
maximumAbsentRoot = nonMembershipRootOf ledgerKey 0 maximumAbsentSteps

runPublishedPredecessor :: forall s. [Integer] -> BS.ByteString -> Term s PUnit
runPublishedPredecessor = runPublishedPredecessorWithChunks (proofChunkReferenceInputs 0 maximumAbsentSteps)

runPublishedPredecessorWithChunks :: forall s. [TxInInfo] -> [Integer] -> BS.ByteString -> Term s PUnit
runPublishedPredecessorWithChunks chunks indices stateRoot = validatorCheck
 where
  carriage = publishedNonMembershipCarriage indices
  validatorCheck =
    step04 $
      spendContext
        (stepDatum $ Just $ step04State stateRoot)
        (PD.Constr 1 [step04Args carriage])
        [threadInput]
        [stepOutput nextScript $ Just terminalState]
        chunks
        []
        mempty

runFivePublishedChunks :: forall s. Term s PUnit
runFivePublishedChunks =
  runPublishedPredecessorWithChunks
    (partiallyPackedChunks maximumAbsentSteps)
    [0, 1, 2, 3, 4]
    maximumAbsentRoot

-- The exact same genuine 64-step proof fits in five partially packed chunks.
-- The general route accepts it; only the min-Ada-specific four-chunk limit
-- should reject it. A delegated withdrawal cannot bypass this limit.
partiallyPackedChunks :: [ProofStepRef] -> [TxInInfo]
partiallyPackedChunks steps =
  [chunkRefInput index chunk | (index, chunk) <- zip [0 ..] $ chunksOf 13 steps]

fiveChunkProofsAreValid :: forall s. Term s PBool
fiveChunkProofsAreValid =
  ppublishedChunkNonMembership
    # pconstant (partiallyPackedChunks maximumAbsentSteps)
    # publishedProofCarriageT [0, 1, 2, 3, 4]
    # pconstant maximumAbsentRoot
    # pconstant ledgerKey
    #&& ppublishedChunkMembership
    # pconstant (partiallyPackedChunks maximumSteps)
    # publishedProofCarriageT [0, 1, 2, 3, 4]
    # pconstant maximumPostRoot
    # pconstant ledgerKey
    # pconstant maximumDescriptor

-- Terminal and deployment snapshot

productionSnapshot :: forall s. Term s PBool
productionSnapshot =
  let maximumLength = 16_384
      floorLovelace = 71_304_640
   in pand'List
        [ pcoinsPerUtxoByte #== 4_310
        , pminAdaLovelaceV1 # pcoinsPerUtxoByte # maximumLength #== floorLovelace
        , poutputMeetsMinAdaV1 # pcoinsPerUtxoByte # maximumLength # floorLovelace
        , pnot #$ poutputMeetsMinAdaV1 # pcoinsPerUtxoByte # maximumLength # (floorLovelace - 1)
        , poutputMeetsMinAdaV1 # 0 # maximumLength # (floorLovelace - 1)
        ]

runStep05 :: forall s. Term s PUnit
runStep05 = step05 context
 where
  context =
    spendContext
      (stepDatum $ Just terminalState)
      (PD.Constr 1 [step05Args])
      [threadInput]
      [convictionOutput fraudProofAddress threadName]
      []
      [fraudProofMintEntry threadName]
      (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

runCancellations :: forall s. Term s PUnit
runCancellations =
  plet (step01 cancellation) $ \_ ->
    plet (step02 cancellation) $ \_ ->
      plet (step03 cancellation) $ \_ ->
        plet (step04 cancellation) $ \_ ->
          step05 cancellation
 where
  cancellation =
    spendContext
      (stepDatum Nothing)
      cancelRedeemer
      [threadInput]
      []
      []
      [cancelMintEntry threadName]
      mempty

proofContext :: PD.Data -> ScriptContext
proofContext proofRedeemer =
  asRewarding $
    spendContext
      (stepDatum Nothing)
      proofRedeemer
      []
      []
      []
      []
      mempty

-- Independent descriptor and published-proof fixtures

descriptorAt :: Integer -> BS.ByteString
descriptorAt delta = descriptorCbor (fromIntegral $ BS.length $ outputAt delta) (outputLovelace delta)

outputLovelace :: Integer -> Integer
outputLovelace delta = 4_310 * (160 + fromIntegral (BS.length $ outputAt delta)) + delta

maximumDescriptor :: BS.ByteString
maximumDescriptor = descriptorCbor 16_384 (71_304_640 - 1)

descriptorCbor :: Integer -> Integer -> BS.ByteString
descriptorCbor totalLength lovelace =
  BS.concat
    [ "\x90"
    , cborInt 1
    , cborInt 0
    , cborInt totalLength
    , defBytes32 $ hash32 0x31
    , wrapItem $ pubKeyAddressBytes $ keyHashFor 0
    , cborInt lovelace
    , cborInt 0
    , defBytes32 $ hash32 0x32
    , cborInt 0
    , cborInt (-1)
    , "\x40"
    , cborInt 0
    , "\x40"
    , summary 0x33
    , summary 0x34
    , summary 0x35
    ]
 where
  summary seed = "\x83" <> defBytes32 (hash32 seed) <> "\x00\x00"

publishedProofCarriageData :: [Integer] -> PD.Data
publishedProofCarriageData indices = PD.Constr 0 [PD.List $ map PD.I indices]

publishedProofCarriageT :: forall s. [Integer] -> Term s PPublishedProofCarriage
publishedProofCarriageT indices =
  pfromData $ punsafeCoerce $ pconstant @PData $ publishedProofCarriageData indices

publishedMembershipCarriage :: [Integer] -> PD.Data
publishedMembershipCarriage indices = PD.Constr 1 [publishedProofCarriageData indices]

publishedNonMembershipCarriage :: [Integer] -> PD.Data
publishedNonMembershipCarriage indices = PD.Constr 1 [publishedProofCarriageData indices]

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
adversarialNonMembershipSteps count =
  adversarialBranchSteps count <> [LeafStep 0 (blake2b256 "q27-absence") (BS.replicate 32 0x6a)]

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
foldMembershipRoot _ _ _ (LeafStep{} : _) = BS.replicate 32 0

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
  | otherwise = BS.pack [0, fromIntegral $ nibble path cursor] <> BS.drop ((cursor + 1) `div` 2) path

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
  [chunkRefInput (offset + fromIntegral index) chunk | (index, chunk) <- zip [0 :: Int ..] $ chunksOf 16 steps]

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

-- Raw Aiken/SDK wire constructors keep the forced admission fixtures independent
-- of the Plutarch data derivation being tested.
runForcedStep01 :: forall s. Integer -> Integer -> Integer -> Integer -> Integer -> Maybe PD.Data -> Term s PUnit
runForcedStep01 validity reason reasonIndex claimedIndex direction prior =
  step01 $
    spendContext
      (stepDatum prior)
      (PD.Constr 1 [PD.Constr 0 [some forced, none, none, txFault claimedIndex]])
      [threadInputWithName name]
      [stepOutputWithName nextScript (Just state) name]
      []
      []
      mempty
 where
  output = outputAt 0
  txId = txIdFor output
  source = PD.Constr 0 [PD.B $ compactFor output validity, PD.B $ witnessSetCborOf tx1, PD.B $ fieldPreimageLengthsCborOf tx1]
  key = PD.Constr 0 [PD.B $ hash32 0x77, PD.I 0]
  leaf = PD.Constr 0 [PD.B txId, source, PD.Constr 1 [PD.Constr reason [PD.I reasonIndex]]]
  rawRoot = singleEntryPhasRoot (serialise key) (serialise leaf)
  root = commitCountedRoot 1 rawRoot 1
  membership = membershipProof 1 root rawRoot 1 key leaf
  header =
    PD.Constr 0 $
      [PD.B "", PD.B "", PD.B "", PD.B root]
        <> replicate 5 (PD.B "")
        <> [PD.I 0, PD.I 1]
        <> replicate 11 (PD.I 0)
        <> [PD.B "", PD.B "", PD.I 1]
  name = BS.pack [0, 0, 0, 5] <> blake2b224 (serialise header)
  forced = PD.Constr 0 [PD.I 0, PD.I 0, header, membership, PD.I direction]
  state = PD.Constr 0 [PD.B "", PD.Constr 0 [], PD.B "", PD.I direction, PD.B txId, txFault claimedIndex, none]

yieldCredential :: Credential
yieldCredential = ScriptCredential $ ScriptHash $ toBuiltin stepScript

runYieldMutation :: forall s. (TxInfo -> TxInfo) -> Term s PUnit
runYieldMutation alter = plet (step02 context) $ \_ ->
  minAdaUtxoYieldValidator
    # pdata (pconstant $ ScriptHash $ toBuiltin stepScript)
    # pconstant (asRewarding context)
 where
  ScriptContext tx r info =
    yieldedContext "V1FpMinAdaS02UtxoYield" $
      spendContext
        (stepDatum $ Just $ step02State (BS.replicate 32 0xcc) utxoFault $ Just $ postState outRefData (descriptorAt (-1)) postRoot emptyRoot)
        (PD.Constr 1 [step02Args Nothing $ Just redeemerCarriedMembership])
        [threadInput]
        [stepOutput nextScript $ Just $ step03State (descriptorAt (-1)) emptyRoot]
        []
        [phasEntry postRoot ledgerKey (descriptorAt (-1))]
        mempty
  context = ScriptContext (alter tx) r info
