{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsWithdrawnReferenceInput
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proofs/withdrawn-reference-input/step-0{1,2,3}.ak@.

The one family whose conviction is a __presence__ rather than an absence: the
disputed transaction referenced an output, and a withdrawal event in the same
block's withdrawals tree had already taken that output off L2.

That makes it short — three steps, no second tree to rule out — and it moves the
weight onto the counted-root machinery instead. Three things have to line up and
each gets its own case below:

* the event's __validity__ must be @WithdrawalIsValid@, because an event the
  operator itself rejected never removed anything;
* its __@l2_outref@__ must be the reference input the thread named, both halves;
* it must be __in the tree the header committed__, which is a counted-root unwrap
  (domain, root, count) followed by an MPF membership walk over the canonically
  serialised key and value.

The tree here holds one entry, so its raw root is
@combine(suffix(path, 0), blake2b_256(value))@ and the proof is empty — built in
"Testing.FraudProofsFixture" from the walk's definition rather than from the port.
-}
module Testing.FraudProofsWithdrawnReferenceInput (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.WithdrawnReferenceInput (
  withdrawnReferenceInputStep01Validator,
  withdrawnReferenceInputStep02Validator,
  withdrawnReferenceInputStep03Validator,
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
    "Withdrawn Reference Input Fraud Proof Tests"
    [ testGroup "the fixture" fixtureTests
    , testGroup "step-01" step01Tests
    , testGroup "step-02" step02Tests
    , testGroup "step-03" step03Tests
    ]

--------------------------------------------------------------------------------
-- The fixture
--------------------------------------------------------------------------------

fixtureTests :: [TestTree]
fixtureTests =
  [ -- The block's withdrawal has to take off L2 exactly the output the disputed
    -- transaction references, or every positive case below proves nothing.
    testCase "the block's withdrawal names the reference input tx1 carries" $
      assertBool "tx1 does not reference the withdrawn output" $
        sharedInputRef `elem` tReferenceInputs tx1
  , testCase "the header's withdrawals root is the counted commitment" $
      headerWithdrawalsRoot
        @?= commitCountedRoot withdrawalsDomain withdrawalsPhasRoot withdrawalCount
  , {- The counted scheme's whole point: the root commits the /count/ too, so the
       same tree presented with a different size is a different root. -}
    testCase "the same tree under a different count is a different root" $
      assertBool "the count does not reach the commitment" $
        headerWithdrawalsRoot
          /= commitCountedRoot withdrawalsDomain withdrawalsPhasRoot (withdrawalCount + 1)
  , testCase "and under a different domain likewise" $
      assertBool "the domain does not reach the commitment" $
        headerWithdrawalsRoot
          /= commitCountedRoot transactionsDomain withdrawalsPhasRoot withdrawalCount
  , testCase "the one-entry raw root is the leaf combination" $
      withdrawalsPhasRoot
        @?= blake2b256
          (BS.cons 0xff (blake2b256 withdrawalKeyBytes <> blake2b256 withdrawalValueBytes))
  ]

--------------------------------------------------------------------------------
-- step-01
--------------------------------------------------------------------------------

step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds the transaction and picks up the counted withdrawals commitment" $
      psucceeds $ step01 (context01 default01)
  , testCase "rejects an output at a script that is not step-02's" $
      pfails $ step01 (context01 default01 {w1OutputScript = otherScript})
  , -- Both halves travel, and a state carrying only one of them is refused.
    testCase "rejects a state naming another withdrawals root" $
      pfails $
        step01
          (context01 default01 {w1OutputState = Just (state02 tx1Id otherRoot withdrawalCount)})
  , testCase "rejects a state naming another withdrawal count" $
      pfails $
        step01
          ( context01
              default01
                {w1OutputState = Just (state02 tx1Id headerWithdrawalsRoot (withdrawalCount + 1))}
          )
  , testCase "rejects an inclusion proof against a root the header does not commit" $
      pfails $ step01 (context01 default01 {w1PhasRoot = otherRoot})
  ]

--------------------------------------------------------------------------------
-- step-02
--------------------------------------------------------------------------------

step02Tests :: [TestTree]
step02Tests =
  [ testCase "names the reference input and carries the commitment forward" $
      psucceeds $ step02 (context02 default02)
  , -- §4 removed field-index domain separation, so field 0's preimage over the
    -- same items would commit identically. tx1's two collections are equal, so
    -- the case that tells the slots apart uses tx3, whose are not.
    testCase "rejects field 0's preimage in field 1's slot" $
      pfails $
        step02
          ( context02
              default02
                { w2StateTxId = tx3Id
                , w2OpeningCbor = tx3Cbor
                , w2Preimage = Just (spendInputsPreimage tx3)
                , w2OutputState = Just (state03 (inputData otherInputRef))
                }
          )
  , testCase "accepts field 1's preimage in field 1's slot" $
      psucceeds $
        step02
          ( context02
              default02
                { w2StateTxId = tx3Id
                , w2OpeningCbor = tx3Cbor
                , w2Preimage = Just (referenceInputsPreimage tx3)
                }
          )
  , testCase "rejects a preimage the transaction never committed" $
      pfails $ step02 (context02 default02 {w2Preimage = Just (referenceInputsPreimage txEmpty)})
  , testCase "rejects an index past the end of the collection" $
      pfails $ step02 (context02 default02 {w2InputIndex = 1})
  , testCase "rejects a state that drops the withdrawal count" $
      pfails $
        step02
          ( context02
              default02
                { w2OutputState =
                    Just
                      ( PD.Constr
                          0
                          [ inputData sharedInputRef
                          , PD.B headerWithdrawalsRoot
                          , PD.I (withdrawalCount + 1)
                          ]
                      )
                }
          )
  , testCase "rejects an output at a script that is not step-03's" $
      pfails $ step02 (context02 default02 {w2OutputScript = otherScript})
  ]

--------------------------------------------------------------------------------
-- step-03
--------------------------------------------------------------------------------

step03Tests :: [TestTree]
step03Tests =
  [ testCase "convicts on a valid withdrawal of the referenced output" $
      psucceeds $ step03 (context03 default03)
  , -- An event the operator marked invalid never took the output off L2, so
    -- referencing it afterwards is no fault at all.
    testCase "refuses a withdrawal the block marked invalid" $
      pfails $ step03 (context03 (withEvent sharedInputRef nonExistentUtxo))
  , -- The event has to be about the output the thread named. Both halves are
    -- checked separately, so both are exercised separately.
    testCase "refuses a withdrawal of another transaction's output" $
      pfails $ step03 (context03 (withEvent otherInputRef valid))
  , testCase "refuses a withdrawal of another index of the same transaction" $
      pfails $ step03 (context03 (withEvent (fst sharedInputRef, 1) valid))
  , -- The counted unwrap: domain, root and count are all bound to what the
    -- thread carries, and each is a separate refusal.
    testCase "refuses a witness under another root domain" $
      pfails $ step03 (context03 default03 {w3Domain = transactionsDomain})
  , testCase "refuses a witness naming another counted root" $
      pfails $ step03 (context03 default03 {w3WitnessRoot = Just otherRoot})
  , testCase "refuses a witness naming another count" $
      pfails $ step03 (context03 default03 {w3WitnessCount = Just (withdrawalCount + 1)})
  , {- The raw root is what the walk runs against, and the counted root is what
       commits it. A witness carrying a raw root that does not commit to the
       counted one it also carries fails the consistency check, not the walk. -}
    testCase "refuses a raw root the counted root does not commit" $
      pfails $ step03 (context03 default03 {w3PhasRoot = Just otherRoot})
  , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
      pfails $ step03 (context03 default03 {w3FraudProofAddress = otherAddress})
  , testCase "rejects a conviction under a name that is not the thread's" $
      pfails $ step03 (context03 default03 {w3FraudProofName = otherThreadName})
  ]

--------------------------------------------------------------------------------
-- Withdrawal validities
--------------------------------------------------------------------------------

valid, nonExistentUtxo :: PD.Data
valid = PD.Constr 0 []
nonExistentUtxo = PD.Constr 1 []

{- | A step-03 case whose block commits a different withdrawal event.

Changing the event changes the value bytes, hence the raw root, hence the counted
root the header carries — so the whole chain has to be rebuilt around it, and the
thread's state has to name the new counted root. That is what makes these cases
tests of the /event/ rather than of the commitment.
-}
withEvent :: (BS.ByteString, Integer) -> PD.Data -> Step03
withEvent outref validity =
  default03
    { w3Event = value
    , w3StateRoot = commitCountedRoot withdrawalsDomain rawRoot withdrawalCount
    , w3PhasRoot = Just rawRoot
    , w3WitnessRoot = Just (commitCountedRoot withdrawalsDomain rawRoot withdrawalCount)
    }
  where
    value = withdrawalInfoData outref validity
    rawRoot = singleEntryPhasRoot withdrawalKeyBytes (serialise value)

--------------------------------------------------------------------------------
-- Thread states
--------------------------------------------------------------------------------

state02 :: BS.ByteString -> BS.ByteString -> Integer -> PD.Data
state02 txId root count = PD.Constr 0 [PD.B txId, PD.B root, PD.I count]

state03 :: PD.Data -> PD.Data
state03 input = PD.Constr 0 [input, PD.B headerWithdrawalsRoot, PD.I withdrawalCount]

--------------------------------------------------------------------------------
-- Driving the validators
--------------------------------------------------------------------------------

step01, step02, step03 :: forall s. ScriptContext -> Term s PUnit
step01 ctx =
  withdrawnReferenceInputStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step02 ctx =
  withdrawnReferenceInputStep02Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx
step03 ctx =
  withdrawnReferenceInputStep03Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant ctx

--------------------------------------------------------------------------------
-- step-01's context
--------------------------------------------------------------------------------

data Step01 = Step01
  { w1OutputScript :: BS.ByteString
  , w1OutputState :: Maybe PD.Data
  , w1PhasRoot :: BS.ByteString
  }

default01 :: Step01
default01 =
  Step01
    { w1OutputScript = nextScript
    , w1OutputState = Just (state02 tx1Id headerWithdrawalsRoot withdrawalCount)
    , w1PhasRoot = phasRoot
    }

context01 :: Step01 -> ScriptContext
context01 s =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [bareInclusionArgs tx1Id tx1Cbor (w1PhasRoot s)])
    [threadInput]
    [stepOutput (w1OutputScript s) (w1OutputState s)]
    referenceInputs
    [phasEntry (w1PhasRoot s) tx1Id tx1Cbor]
    mempty

--------------------------------------------------------------------------------
-- step-02's context
--------------------------------------------------------------------------------

data Step02 = Step02
  { w2StateTxId :: BS.ByteString
  , w2OpeningCbor :: BS.ByteString
  , w2Preimage :: Maybe BS.ByteString
  , w2InputIndex :: Integer
  , w2OutputScript :: BS.ByteString
  , w2OutputState :: Maybe PD.Data
  }

default02 :: Step02
default02 =
  Step02
    { w2StateTxId = tx1Id
    , w2OpeningCbor = tx1Cbor
    , w2Preimage = Nothing
    , w2InputIndex = 0
    , w2OutputScript = nextScript
    , w2OutputState = Just (state03 (inputData sharedInputRef))
    }

context02 :: Step02 -> ScriptContext
context02 s =
  spendContext
    (stepDatum (Just (state02 (w2StateTxId s) headerWithdrawalsRoot withdrawalCount)))
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , bodyOpening (w2OpeningCbor s) preimage
            , PD.I (w2InputIndex s)
            ]
        ]
    )
    [threadInput]
    [stepOutput (w2OutputScript s) (w2OutputState s)]
    referenceInputs
    []
    mempty
  where
    preimage = maybe (referenceInputsPreimage tx1) id (w2Preimage s)

--------------------------------------------------------------------------------
-- step-03's context
--------------------------------------------------------------------------------

data Step03 = Step03
  { w3Event :: PD.Data
  , w3StateRoot :: BS.ByteString
  , w3Domain :: Integer
  , w3WitnessRoot :: Maybe BS.ByteString
  , w3WitnessCount :: Maybe Integer
  , w3PhasRoot :: Maybe BS.ByteString
  , w3FraudProofAddress :: Address
  , w3FraudProofName :: BS.ByteString
  }

default03 :: Step03
default03 =
  Step03
    { w3Event = withdrawalInfoData sharedInputRef valid
    , w3StateRoot = headerWithdrawalsRoot
    , w3Domain = withdrawalsDomain
    , w3WitnessRoot = Nothing
    , w3WitnessCount = Nothing
    , w3PhasRoot = Nothing
    , w3FraudProofAddress = fraudProofAddress
    , w3FraudProofName = threadName
    }

context03 :: Step03 -> ScriptContext
context03 s =
  spendContext
    ( stepDatum
        ( Just
            ( PD.Constr
                0
                [ inputData sharedInputRef
                , PD.B (w3StateRoot s)
                , PD.I withdrawalCount
                ]
            )
        )
    )
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , membershipProof
                (w3Domain s)
                (maybe (w3StateRoot s) id (w3WitnessRoot s))
                (maybe withdrawalsPhasRoot id (w3PhasRoot s))
                (maybe withdrawalCount id (w3WitnessCount s))
                withdrawalId
                (w3Event s)
            , PD.I 0
            ]
        ]
    )
    [threadInput]
    [convictionOutput (w3FraudProofAddress s) (w3FraudProofName s)]
    referenceInputs
    [fraudProofMintEntry (w3FraudProofName s)]
    (singleton fpPolicy (TokenName (toBuiltin (w3FraudProofName s))) 1)
