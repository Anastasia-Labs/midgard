{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsNoReferenceInput
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proofs/no-reference-input/step-0{1,2,3,4}.ak@.

The @no-input@ proof one §2.5 slot over: step-02 opens field 1 rather than field
0, and the same two absences follow.

Like @no-input@, both absence steps use the shared non-membership carriage. The
cases below drive the direct arm, while the shared carriage suite pins the
equivalent published-chunk route.

The __slot__ is the other. Field 0 and field 1 commit identically for identical
items (§4 removed field-index domain separation), so the only thing separating
this family from @no-input@ is the constant its step-02 passes the door. The
fixture's 'tx3' has different spend inputs and reference inputs, which is what
makes reading the wrong one observable.

__The two keys, again.__ Step-03 keys the ledger MPF by the node's CBOR encoding
of a transaction input; step-04 keys the transactions MPF by the raw 32-byte
transaction id. Each has a case handing the delegated @pexcludes@ walk the other
step's key.
-}
module Testing.FraudProofsNoReferenceInput (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (
  Address,
  Credential (..),
  Redeemer (..),
  ScriptContext,
  ScriptHash (..),
  ScriptPurpose (..),
  TokenName (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.NoReferenceInput (
  noReferenceInputStep01Validator,
  noReferenceInputStep02Validator,
  noReferenceInputStep03Validator,
  noReferenceInputStep04Validator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "No Reference Input Fraud Proof Tests"
    [ testGroup "step-01" step01Tests
    , testGroup "step-02" step02Tests
    , testGroup "step-03" step03Tests
    , testGroup "step-04" step04Tests
    , testGroup "forced pre-state proof" forcedTests
    ]

--------------------------------------------------------------------------------
-- Step 01
--------------------------------------------------------------------------------

step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds the transaction and forwards both roots" $
      psucceeds $
        runStep01 defaultStep01
  , testCase "rejects an output at a script that is not step-02's" $
      pfails $
        runStep01 defaultStep01 {s1OutputScript = otherScript}
  , testCase "rejects a state naming another initial-ledger root" $
      pfails $
        runStep01 defaultStep01 {s1OutputState = Just (state02 tx1Id otherRoot phasRoot)}
  , testCase "rejects a state naming another transactions root" $
      pfails $
        runStep01 defaultStep01 {s1OutputState = Just (state02 tx1Id prevUtxosRoot otherRoot)}
  , testCase "rejects a raw root the header does not commit" $
      pfails $
        runStep01 defaultStep01 {s1PhasRoot = otherRoot}
  , testCase "rejects a transaction marked invalid" $
      pfails $
        runStep01 defaultStep01 {s1SourceCbor = sourceCborOf tx1}
  , testCase "a cancel burning the thread token succeeds" $
      psucceeds $
        runCancel True
  , testCase "a cancel that does not burn the thread token fails" $
      pfails $
        runCancel False
  , testCase "a minting purpose fails" $
      pfails $
        step01Of (asMinting (contextStep01 defaultStep01))
  ]

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

step02Tests :: [TestTree]
step02Tests =
  [ testCase "reads the named reference input and forwards it with both roots" $
      psucceeds $
        runStep02 defaultStep02
  , -- The slot. tx3 references a different output than it spends, so a step
    -- that opened field 0 would authenticate against the wrong commitment.
    testCase "opens field 1, not field 0" $
      psucceeds $
        runStep02
          defaultStep02
            { s2StateTxId = tx3Id
            , s2OpeningCbor = tx3Cbor
            , s2Preimage = Just (referenceInputsPreimage tx3)
            , s2OutputState = Just (state03 sharedInputRef prevUtxosRoot phasRoot)
            }
  , testCase "refuses the sibling slot's preimage" $
      pfails $
        runStep02
          defaultStep02
            { s2StateTxId = tx3Id
            , s2OpeningCbor = tx3Cbor
            , s2Preimage = Just (spendInputsPreimage tx3)
            }
  , testCase "rejects an opening of a transaction the thread did not anchor" $
      pfails $
        runStep02 defaultStep02 {s2OpeningCbor = tx3Cbor}
  , testCase "rejects an index past the end of the collection" $
      pfails $
        runStep02 defaultStep02 {s2BadIndex = 1}
  , testCase "rejects an output at a script that is not step-03's" $
      pfails $
        runStep02 defaultStep02 {s2OutputScript = otherScript}
  , testCase "rejects a state that alters the initial-ledger root" $
      pfails $
        runStep02 defaultStep02 {s2OutputState = Just (state03 sharedInputRef otherRoot phasRoot)}
  , testCase "rejects a state that alters the transactions root" $
      pfails $
        runStep02
          defaultStep02 {s2OutputState = Just (state03 sharedInputRef prevUtxosRoot otherRoot)}
  ]

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

step03Tests :: [TestTree]
step03Tests =
  [ testCase "proves absence from the initial ledger and carries the tx id on" $
      psucceeds $
        runStep03 defaultStep03
  , testCase "accepts genesis-empty direct non-membership" $
      psucceeds $
        runStep03
          defaultStep03
            { s3StateRoot = emptyMerkleRoot
            , s3ClaimRoot = emptyMerkleRoot
            }
  , testCase "accepts a maximum four-chunk published non-membership claim" $
      psucceeds $
        runStep03Published [0, 1, 2, 3] [0, 1, 2, 3]
  , testCase "rejects a reordered published non-membership claim" $
      pfails $
        runStep03Published [0, 1, 2, 3] [0, 2, 1, 3]
  , testCase "rejects a pexcludes claim under another root" $
      pfails $
        runStep03 defaultStep03 {s3ClaimRoot = otherRoot}
  , testCase "rejects a pexcludes claim under another key" $
      pfails $
        runStep03 defaultStep03 {s3ClaimKey = Just (BS.replicate 32 0x7f)}
  , -- Step-04's key would prove the wrong absence.
    testCase "rejects a pexcludes claim keyed by the raw transaction id" $
      pfails $
        runStep03 defaultStep03 {s3ClaimKey = Just (fst sharedInputRef)}
  , -- The proof the redeemer carries is the one the walk must have run on.
    testCase "rejects a claim whose proof is not the redeemer's" $
      pfails $
        runStep03 defaultStep03 {s3ClaimProof = Just (PD.List [PD.I 0])}
  , testCase "rejects an output at a script that is not step-04's" $
      pfails $
        runStep03 defaultStep03 {s3OutputScript = otherScript}
  , testCase "rejects a state that alters the transactions root" $
      pfails $
        runStep03 defaultStep03 {s3OutputState = Just (state04 (fst sharedInputRef) otherRoot)}
  ]

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

step04Tests :: [TestTree]
step04Tests =
  [ testCase "proves absence from the transactions root and convicts" $
      psucceeds $
        runStep04 defaultStep04
  , testCase "accepts a maximum four-chunk published non-membership claim" $
      psucceeds $
        runStep04Published [0, 1, 2, 3] [0, 1, 2, 3]
  , testCase "rejects a reordered published non-membership claim" $
      pfails $
        runStep04Published [0, 1, 2, 3] [0, 2, 1, 3]
  , testCase "rejects a pexcludes claim under another root" $
      pfails $
        runStep04 defaultStep04 {s4ClaimRoot = otherRoot}
  , testCase "rejects a pexcludes claim under another key" $
      pfails $
        runStep04 defaultStep04 {s4ClaimKey = Just (BS.replicate 32 0x7f)}
  , -- Step-03's key would prove the wrong absence.
    testCase "rejects a pexcludes claim keyed by the encoded input" $
      pfails $
        runStep04 defaultStep04 {s4ClaimKey = Just (encodedInput sharedInputRef)}
  , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
      pfails $
        runStep04 defaultStep04 {s4FraudProofAddress = otherAddress}
  , testCase "rejects a conviction under a name that is not the thread's" $
      pfails $
        runStep04 defaultStep04 {s4FraudProofName = otherThreadName}
  ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

state02 :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
state02 txId ledgerRoot txsRoot = PD.Constr 0 [PD.B txId, PD.B ledgerRoot, PD.B txsRoot]

state03 :: (BS.ByteString, Integer) -> BS.ByteString -> BS.ByteString -> PD.Data
state03 ref ledgerRoot txsRoot = PD.Constr 0 [inputData ref, PD.B ledgerRoot, PD.B txsRoot]

state04 :: BS.ByteString -> BS.ByteString -> PD.Data
state04 txId txsRoot = PD.Constr 0 [PD.B txId, PD.B txsRoot]

--------------------------------------------------------------------------------
-- Driving step 01
--------------------------------------------------------------------------------

data Step01 = Step01
  { s1OutputScript :: BS.ByteString
  , s1OutputState :: Maybe PD.Data
  , s1PhasRoot :: BS.ByteString
  , s1SourceCbor :: BS.ByteString
  }

defaultStep01 :: Step01
defaultStep01 =
  Step01
    { s1OutputScript = nextScript
    , s1OutputState = Just (state02 tx1Id prevUtxosRoot phasRoot)
    , s1PhasRoot = phasRoot
    , s1SourceCbor = sourceCborWithValidity tx1 0
    }

runStep01 :: forall s. Step01 -> Term s PUnit
runStep01 = step01Of . contextStep01

step01Of :: forall s. ScriptContext -> Term s PUnit
step01Of ctx =
  noReferenceInputStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx

contextStep01 :: Step01 -> ScriptContext
contextStep01 s =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [PD.Constr 0 [PD.Constr 0 [inclusionArgs tx1Id (s1SourceCbor s) (s1PhasRoot s)]]])
    [threadInput]
    [stepOutput (s1OutputScript s) (s1OutputState s)]
    referenceInputs
    [phasEntry (s1PhasRoot s) tx1Id (s1SourceCbor s)]
    mempty

runCancel :: forall s. Bool -> Term s PUnit
runCancel burns =
  step01Of
    ( spendContext
        (stepDatum Nothing)
        cancelRedeemer
        [threadInput]
        []
        referenceInputs
        [cancelMintEntry (if burns then threadName else otherThreadName)]
        mempty
    )

--------------------------------------------------------------------------------
-- Driving step 02
--------------------------------------------------------------------------------

data Step02 = Step02
  { s2StateTxId :: BS.ByteString
  , s2OpeningCbor :: BS.ByteString
  , s2Preimage :: Maybe BS.ByteString
  , s2BadIndex :: Integer
  , s2OutputScript :: BS.ByteString
  , s2OutputState :: Maybe PD.Data
  }

defaultStep02 :: Step02
defaultStep02 =
  Step02
    { s2StateTxId = tx1Id
    , s2OpeningCbor = compactWithValidity tx1 (witnessSetHashOf tx1) 0
    , s2Preimage = Nothing
    , s2BadIndex = 0
    , s2OutputScript = nextScript
    , s2OutputState = Just (state03 sharedInputRef prevUtxosRoot phasRoot)
    }

runStep02 :: forall s. Step02 -> Term s PUnit
runStep02 s =
  noReferenceInputStep02Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (state02 (s2StateTxId s) prevUtxosRoot phasRoot)))
          ( PD.Constr
              1
              [ PD.Constr
                  0
                  [ PD.I 0
                  , PD.I 0
                  , bodyOpening (s2OpeningCbor s) preimage
                  , PD.I (s2BadIndex s)
                  ]
              ]
          )
          [threadInput]
          [stepOutput (s2OutputScript s) (s2OutputState s)]
          referenceInputs
          []
          mempty
      )
  where
    preimage = maybe (referenceInputsPreimage tx1) id (s2Preimage s)

--------------------------------------------------------------------------------
-- Driving step 03
--------------------------------------------------------------------------------

data Step03 = Step03
  { s3StateRoot :: BS.ByteString
  , s3ClaimRoot :: BS.ByteString
  , s3ClaimKey :: Maybe BS.ByteString
  , s3ClaimProof :: Maybe PD.Data
  , s3OutputScript :: BS.ByteString
  , s3OutputState :: Maybe PD.Data
  }

defaultStep03 :: Step03
defaultStep03 =
  Step03
    { s3StateRoot = prevUtxosRoot
    , s3ClaimRoot = prevUtxosRoot
    , s3ClaimKey = Nothing
    , s3ClaimProof = Nothing
    , s3OutputScript = nextScript
    , s3OutputState = Just (state04 (fst sharedInputRef) phasRoot)
    }

runStep03 :: forall s. Step03 -> Term s PUnit
runStep03 s =
  noReferenceInputStep03Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (state03 sharedInputRef (s3StateRoot s) phasRoot)))
          (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, redeemerCarriedNonMembership]])
          [threadInput]
          [stepOutput (s3OutputScript s) (s3OutputState s)]
          referenceInputs
          [ pexcludesEntryWith
              (s3ClaimRoot s)
              (maybe (encodedInput sharedInputRef) id (s3ClaimKey s))
              (maybe emptyProof id (s3ClaimProof s))
          ]
          mempty
      )

runStep03Published :: forall s. [Integer] -> [Integer] -> Term s PUnit
runStep03Published carriageIndices claimIndices =
  noReferenceInputStep03Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (state03 sharedInputRef prevUtxosRoot phasRoot)))
          ( PD.Constr
              1
              [ PD.Constr
                  0
                  [ PD.I 0
                  , PD.I 0
                  , publishedNonMembershipCarriage carriageIndices
                  ]
              ]
          )
          [threadInput]
          [stepOutput nextScript (Just (state04 (fst sharedInputRef) phasRoot))]
          referenceInputs
          [chunkedNonMembershipEntry prevUtxosRoot (encodedInput sharedInputRef) claimIndices]
          mempty
      )

--------------------------------------------------------------------------------
-- Driving step 04
--------------------------------------------------------------------------------

data Step04 = Step04
  { s4ClaimRoot :: BS.ByteString
  , s4ClaimKey :: Maybe BS.ByteString
  , s4FraudProofAddress :: Address
  , s4FraudProofName :: BS.ByteString
  }

defaultStep04 :: Step04
defaultStep04 =
  Step04
    { s4ClaimRoot = phasRoot
    , s4ClaimKey = Nothing
    , s4FraudProofAddress = fraudProofAddress
    , s4FraudProofName = threadName
    }

runStep04 :: forall s. Step04 -> Term s PUnit
runStep04 s =
  noReferenceInputStep04Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (state04 (fst sharedInputRef) phasRoot)))
          (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, redeemerCarriedNonMembership]])
          [threadInput]
          [convictionOutput (s4FraudProofAddress s) (s4FraudProofName s)]
          referenceInputs
          [ fraudProofMintEntry (s4FraudProofName s)
          , pexcludesEntryWith
              (s4ClaimRoot s)
              (maybe (fst sharedInputRef) id (s4ClaimKey s))
              emptyProof
          ]
          (singleton fpPolicy (TokenName (toBuiltin (s4FraudProofName s))) 1)
      )

runStep04Published :: forall s. [Integer] -> [Integer] -> Term s PUnit
runStep04Published carriageIndices claimIndices =
  noReferenceInputStep04Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (state04 (fst sharedInputRef) phasRoot)))
          ( PD.Constr
              1
              [ PD.Constr
                  0
                  [ PD.I 0
                  , PD.I 0
                  , PD.I 0
                  , publishedNonMembershipCarriage carriageIndices
                  ]
              ]
          )
          [threadInput]
          [convictionOutput fraudProofAddress threadName]
          referenceInputs
          [ fraudProofMintEntry threadName
          , chunkedNonMembershipEntry phasRoot (fst sharedInputRef) claimIndices
          ]
          (singleton fpPolicy (TokenName (toBuiltin threadName)) 1)
      )

publishedNonMembershipCarriage :: [Integer] -> PD.Data
publishedNonMembershipCarriage indices =
  PD.Constr 1 [PD.Constr 0 [PD.List (map PD.I indices)]]

chunkedNonMembershipEntry :: BS.ByteString -> BS.ByteString -> [Integer] -> (ScriptPurpose, Redeemer)
chunkedNonMembershipEntry root key indices =
  ( Rewarding (ScriptCredential (ScriptHash (toBuiltin chunkedVerifyHash)))
  , Redeemer $
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.Constr 1 []
          , PD.B root
          , PD.B key
          , PD.B (BS.replicate 32 0)
          , PD.List (map PD.I indices)
          ]
  )

chunkedVerifyHash, emptyMerkleRoot :: BS.ByteString
chunkedVerifyHash =
  Base16.decodeLenient "ea8d998a1396392158fa85afb0d202df7bd6d6ede7d3fbc05f55acd6"
emptyMerkleRoot =
  Base16.decodeLenient "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

-- Independent target Data producers for the forced event/transition path.
forcedKey, forcedEvent, forcedSubject, forcedState02, eventProof, traceProof :: PD.Data
forcedKey = PD.Constr 0 [PD.B $ BS.replicate 32 0x77, PD.I 0]
forcedEvent = PD.Constr 1 [forcedKey]
forcedSubject = inputSubject 1 0
inputSubject :: Integer -> Integer -> PD.Data
inputSubject kind index =
  PD.Constr
    0
    [ PD.I 1
    , PD.I 1
    , PD.I 1
    , PD.B forcedTxId
    , PD.B $ serialise forcedKey
    , PD.Constr 0 [PD.Constr 18 [PD.I kind, PD.I index]]
    ]
forcedState02 = PD.Constr 1 [forcedSubject, forcedEvent, PD.B eventRoot, PD.I 1, PD.B traceRoot, PD.I 1]
eventProof = snd $ countedProof 5 forcedEvent (PD.Constr 0 [PD.I 0, PD.Constr 1 []])
traceProof = snd $ countedProof 4 (PD.I 0) transition

countedProof :: Integer -> PD.Data -> PD.Data -> (BS.ByteString, PD.Data)
countedProof domain key value = (root, membershipProof domain root rawRoot 1 key value)
  where
    rawRoot = singleEntryPhasRoot (serialise key) (serialise value)
    root = commitCountedRoot domain rawRoot 1

eventRoot, traceRoot, ledgerRoot, ledgerValueHash :: BS.ByteString
eventRoot = fst $ countedProof 5 forcedEvent (PD.Constr 0 [PD.I 0, PD.Constr 1 []])
traceRoot = fst $ countedProof 4 (PD.I 0) transition
ledgerValueHash = blake2b256 "committed ledger output"
ledgerRoot = blake2b256 $ BS.cons 0xff (blake2b256 (encodedInput sharedInputRef) <> ledgerValueHash)

transition, selectedInput, noSelectedInput, ledgerMembership :: PD.Data
transition = PD.Constr 0 [PD.I 1, PD.I 0, forcedEvent, PD.Constr 1 [], PD.B ledgerRoot, PD.B ledgerRoot]
selectedInput = PD.Constr 0 [inputData sharedInputRef]
noSelectedInput = PD.Constr 1 []
ledgerMembership = PD.Constr 0 [PD.Constr 0 [PD.B ledgerValueHash, PD.List []]]

forcedState03 :: PD.Data -> PD.Data
forcedState03 selected = PD.Constr 1 [forcedEvent, PD.B traceRoot, PD.I 1, PD.I 0, selected]
forcedState04 :: PD.Data -> BS.ByteString -> PD.Data
forcedState04 selected root = PD.Constr 1 [selected, PD.B root]

replaceDataField :: Int -> PD.Data -> PD.Data -> PD.Data
replaceDataField index replacement (PD.Constr tag fields) =
  PD.Constr tag [if n == index then replacement else field | (n, field) <- zip [0 ..] fields]
replaceDataField _ _ _ = error "constructor fixture required"

runForced01 :: forall s. Integer -> Integer -> Maybe PD.Data -> Term s PUnit
runForced01 kind direction inputState =
  step01Of $
    spendContext
      (stepDatum inputState)
      (PD.Constr 1 [PD.Constr 0 [PD.Constr 1 [PD.I 0, PD.I 0, header, proof, PD.I direction]]])
      [threadInputWithName name]
      [stepOutputWithName nextScript (Just expected) name]
      []
      []
      mempty
  where
    source =
      PD.Constr
        0
        [ PD.B $ compactWithValidity forcedTx (witnessSetHashOf forcedTx) 1
        , PD.B $ witnessSetCborOf forcedTx
        , PD.B $ fieldPreimageLengthsCborOf forcedTx
        ]
    leaf = PD.Constr 0 [PD.B forcedTxId, source, PD.Constr 1 [PD.Constr 18 [PD.I kind, PD.I 0]]]
    (root, proof) = countedProof 1 forcedKey leaf
    header =
      PD.Constr 0 $
        [PD.B "", PD.B "", PD.B "", PD.B root, PD.B "", PD.B "", PD.B traceRoot, PD.B eventRoot, PD.B ""]
          ++ [PD.I 0, PD.I 1, PD.I 0, PD.I 0, PD.I 1, PD.I 1]
          ++ replicate 7 (PD.I 0)
          ++ [PD.B "", PD.B "", PD.I 1]
    name = BS.pack [0, 0, 0, 5] <> blake2b224 (serialise header)
    expected = replaceDataField 0 (inputSubject kind 0) forcedState02

runForced02 :: forall s. PD.Data -> PD.Data -> PD.Data -> Term s PUnit
runForced02 state proof expected =
  noReferenceInputStep02Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant context
  where
    opening = bodyOpening (compactWithValidity forcedTx (witnessSetHashOf forcedTx) 1) (referenceInputsPreimage forcedTx)
    context =
      spendContext
        (stepDatum $ Just state)
        (PD.Constr 1 [PD.Constr 1 [PD.I 0, PD.I 0, opening, proof]])
        [threadInput]
        [stepOutput nextScript $ Just expected]
        []
        []
        mempty

runForced03 :: forall s. PD.Data -> PD.Data -> PD.Data -> Term s PUnit
runForced03 state proof expected =
  noReferenceInputStep03Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant context
  where
    context =
      spendContext
        (stepDatum $ Just state)
        (PD.Constr 1 [PD.Constr 1 [PD.I 0, PD.I 0, proof]])
        [threadInput]
        [stepOutput nextScript $ Just expected]
        []
        []
        mempty

runForced04 :: forall s. PD.Data -> PD.Data -> Term s PUnit
runForced04 state proof =
  noReferenceInputStep04Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant context
  where
    context =
      spendContext
        (stepDatum $ Just state)
        (PD.Constr 1 [PD.Constr 1 [PD.I 0, PD.I 0, PD.I 0, proof]])
        [threadInput]
        [convictionOutput fraudProofAddress threadName]
        []
        [fraudProofMintEntry threadName]
        (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

forcedTests :: [TestTree]
forcedTests =
  [ testCase "binds the rejected forced reference input and event roots" $ psucceeds $ runForced01 1 1 Nothing
  , testCase "refuses a spend-input rejection in the reference family" $ pfails $ runForced01 0 1 Nothing
  , testCase "refuses a substituted verdict direction" $ pfails $ runForced01 1 0 Nothing
  , testCase "refuses a prior forced thread state" $ pfails $ runForced01 1 1 (Just $ PD.I 0)
  , testCase "selects the exact rejected input and authenticates its event" $ psucceeds $ runForced02 forcedState02 eventProof (forcedState03 selectedInput)
  , testCase "forwards absent selection for an out-of-range rejection" $ psucceeds $ runForced02 (replaceDataField 0 (inputSubject 1 99) forcedState02) eventProof (forcedState03 noSelectedInput)
  , testCase "forwards absent selection for a negative rejection coordinate" $ psucceeds $ runForced02 (replaceDataField 0 (inputSubject 1 (-1)) forcedState02) eventProof (forcedState03 noSelectedInput)
  , testCase "refuses a changed event root" $ pfails $ runForced02 (replaceDataField 2 (PD.B otherRoot) forcedState02) eventProof (forcedState03 selectedInput)
  , testCase "refuses a substituted event key" $ pfails $ runForced02 (replaceDataField 1 (PD.Constr 1 [inputData otherInputRef]) forcedState02) eventProof (forcedState03 selectedInput)
  , testCase "refuses an event index outside the trace count" $ pfails $ runForced02 (replaceDataField 5 (PD.I 0) forcedState02) eventProof (forcedState03 selectedInput)
  , testCase "binds the event transition pre-state" $ psucceeds $ runForced03 (forcedState03 selectedInput) traceProof (forcedState04 selectedInput ledgerRoot)
  , testCase "refuses a changed transition root" $ pfails $ runForced03 (replaceDataField 1 (PD.B otherRoot) $ forcedState03 selectedInput) traceProof (forcedState04 selectedInput ledgerRoot)
  , testCase "refuses a changed transition index" $ pfails $ runForced03 (replaceDataField 3 (PD.I 1) $ forcedState03 selectedInput) traceProof (forcedState04 selectedInput ledgerRoot)
  , testCase "refuses a changed transition event" $ pfails $ runForced03 (replaceDataField 0 (PD.Constr 1 [inputData otherInputRef]) $ forcedState03 selectedInput) traceProof (forcedState04 selectedInput ledgerRoot)
  , testCase "convicts when the rejected input exists in the pre-state" $ psucceeds $ runForced04 (forcedState04 selectedInput ledgerRoot) ledgerMembership
  , testCase "refuses membership under another pre-state root" $ pfails $ runForced04 (forcedState04 selectedInput otherRoot) ledgerMembership
  , testCase "refuses missing membership for a selected input" $ pfails $ runForced04 (forcedState04 selectedInput ledgerRoot) noSelectedInput
  , testCase "convicts a rejection naming no input without membership" $ psucceeds $ runForced04 (forcedState04 noSelectedInput ledgerRoot) noSelectedInput
  , testCase "refuses surplus membership for an absent selection" $ pfails $ runForced04 (forcedState04 noSelectedInput ledgerRoot) ledgerMembership
  ]

-- Distinct field commitments ensure the forced route really opens field 1.
forcedTx :: Tx
forcedTx = tx1 {tSpendInputs = [otherInputRef]}
forcedTxId :: BS.ByteString
forcedTxId = txIdOf forcedTx
