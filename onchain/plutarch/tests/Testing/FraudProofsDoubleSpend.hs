{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsDoubleSpend
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proofs/double-spend/step-0{1,2,3,4}.ak@.

Four validators, one per L1 transaction of the proof, driven end to end over a
single-transaction block fixture built here from the format rather than taken
from the port.

What the four steps own between them is two guards, and the tests are organised
around making each of them fail on its own.

__Distinctness is step-02's.__ Two /identical/ transactions are one transaction —
a block committing the same canonical bytes twice commits one leaf — so a prover
that binds the same transaction twice must be refused, and it is refused there
and nowhere else. The fixture makes that reachable: tx1 and tx2 differ only in
fee, so they share every spend input and differ in canonical id, and the "same
transaction twice" case is one line away from the honest one.

__The disputed input is carried, not re-derived.__ Step-03 reads it out of tx1's
authenticated field 0 and puts it in thread state; step-04 reads tx2's field 0 at
its own index and compares. A challenge against a valid block dies at that
comparison, because distinct transactions of a valid block spend disjoint inputs
— which is the case @rejects a second transaction spending a different input@
reproduces.

Everything the door needs is positional: the field index is a compiled-in
literal and the commitment is derived from the compact structures the /verified
id/ authenticates, so no test here ever hands a validator a free-standing field
hash. Under §4's plain hashing there is no such thing as one that names a slot.

The block fixture — canonical compact CBOR, the §3 transaction id, the header's
counted @transactions_root@ — is "Testing.FraudProofsFixture", written from the
spec rather than from the port so that a change on either side fails a test
instead of two copies agreeing.
-}
module Testing.FraudProofsDoubleSpend (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.DoubleSpend (
  doubleSpendStep01Validator,
  doubleSpendStep02Validator,
  doubleSpendStep03Validator,
  doubleSpendStep04Validator,
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
    "Double Spend Fraud Proof Tests"
    [ testGroup "the fixture" fixtureTests
    , testGroup "step-01" step01Tests
    , testGroup "step-02" step02Tests
    , testGroup "step-03" step03Tests
    , testGroup "step-04" step04Tests
    , testGroup "cancel and purpose" sharedTests
    ]

--------------------------------------------------------------------------------
-- The fixture
--------------------------------------------------------------------------------

{- | The family is only provable against a block that really holds two distinct
transactions spending one input, so the fixture's own shape is asserted before
anything is driven through it.
-}
fixtureTests :: [TestTree]
fixtureTests =
  [ testCase "the two conflicting transactions have different ids" $
      assertBool "tx1 and tx2 share an id" (tx1Id /= tx2Id)
  , testCase "...and identical spend inputs" $
      spendInputsOf tx1 @?= spendInputsOf tx2
  , testCase "the honest third transaction spends a different input" $
      assertBool "tx3 shares tx1's input" (spendInputsOf tx3 /= spendInputsOf tx1)
  , testCase "the header commits the raw root under the counted scheme" $
      headerTransactionsRoot @?= commitCountedRoot transactionsDomain phasRoot l2Count
  ]

--------------------------------------------------------------------------------
-- Step 01
--------------------------------------------------------------------------------

{- | Binds the first conflicting transaction to the block and forwards its id.
The id is the whole of the state, because step-03 re-opens field 0 through the
door from it.
-}
step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds a committed transaction and forwards its id" $
      psucceeds $ runStep01 defaultStep01
  , -- The next hop is a compiled-in parameter, so a thread cannot be diverted
    -- to a script of the prover's choosing.
    testCase "rejects an output at a script that is not step-02's" $
      pfails $ runStep01 defaultStep01 {s1OutputScript = otherScript}
  , testCase "rejects a state carrying anything but the verified id" $
      pfails $ runStep01 defaultStep01 {s1OutputState = Just (PD.Constr 0 [PD.B tx2Id])}
  , testCase "rejects a state with an extra field" $
      pfails $
        runStep01 defaultStep01 {s1OutputState = Just (PD.Constr 0 [PD.B tx1Id, PD.B tx1Id])}
  , -- What makes this the *first* step: a thread already carrying state is one
    -- this validator was not initialised for.
    testCase "rejects a thread that already carries state" $
      pfails $ runStep01 defaultStep01 {s1InputState = Just (PD.Constr 0 [PD.B tx1Id])}
  , -- The evidence still has to be evidence. The raw root the args name must be
    -- the one the header committed under the counted scheme.
    testCase "rejects a raw root the header does not commit" $
      pfails $ runStep01 defaultStep01 {s1PhasRoot = otherRoot}
  , testCase "rejects compact bytes that re-derive to another id" $
      pfails $ runStep01 defaultStep01 {s1Cbor = tx2Cbor}
  ]

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

{- | Binds the second transaction and enforces distinctness.

The distinctness case is the one that matters and it is cheap to get wrong: a
port that dropped the check would still pass every other test here, because
every other test uses two genuinely different transactions.
-}
step02Tests :: [TestTree]
step02Tests =
  [ testCase "binds a second, distinct transaction and forwards both ids" $
      psucceeds $ runStep02 defaultStep02
  , testCase "rejects the same transaction bound twice" $
      pfails $
        runStep02
          defaultStep02
            { s2InputState = Just (PD.Constr 0 [PD.B tx2Id])
            , s2OutputState = Just (PD.Constr 0 [PD.B tx2Id, PD.B tx2Id])
            }
  , testCase "rejects an output at a script that is not step-03's" $
      pfails $ runStep02 defaultStep02 {s2OutputScript = otherScript}
  , -- Both ids travel, in declaration order: tx1 then tx2.
    testCase "rejects a state with the two ids transposed" $
      pfails $
        runStep02 defaultStep02 {s2OutputState = Just (PD.Constr 0 [PD.B tx2Id, PD.B tx1Id])}
  , testCase "rejects a state that drops the first id" $
      pfails $ runStep02 defaultStep02 {s2OutputState = Just (PD.Constr 0 [PD.B tx2Id])}
  , testCase "rejects a thread carrying no prior state" $
      pfails $ runStep02 defaultStep02 {s2InputState = Nothing}
  ]

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

{- | Opens tx1's field 0 through the door and forwards the disputed reference.

The anchor is thread state, not the redeemer, which is what the substituted-id
case below shows: an opening whose bytes belong to tx2 does not authenticate
against tx1's anchor even though those bytes are perfectly genuine.
-}
step03Tests :: [TestTree]
step03Tests =
  [ testCase "opens tx1's field 0 and forwards the named input" $
      psucceeds $ runStep03 defaultStep03
  , testCase "rejects an opening of a transaction the thread did not anchor" $
      pfails $ runStep03 defaultStep03 {s3OpeningCbor = tx2Cbor}
  , -- §7.3: an out-of-range read aborts rather than clamping. A clamped read
    -- would let two different indices name the same input.
    testCase "rejects an input index past the end of the collection" $
      pfails $ runStep03 defaultStep03 {s3InputIndex = 3}
  , testCase "rejects a negative input index" $
      pfails $ runStep03 defaultStep03 {s3InputIndex = -1}
  , -- The preimage is authenticated against the commitment the door derives
    -- from tx1's compact structures, so a fabricated collection is refused.
    testCase "rejects a preimage the transaction does not commit" $
      pfails $ runStep03 defaultStep03 {s3Preimage = Just (spendInputsPreimage tx3)}
  , testCase "rejects an output at a script that is not step-04's" $
      pfails $ runStep03 defaultStep03 {s3OutputScript = otherScript}
  , testCase "rejects a state naming an input the collection does not hold" $
      pfails $
        runStep03
          defaultStep03
            { s3OutputState = Just (PD.Constr 0 [PD.B tx2Id, inputData otherInputRef])
            }
  , -- The second id has to survive step-03 untouched: step-04 is what uses it.
    testCase "rejects a state that drops the second id" $
      pfails $
        runStep03 defaultStep03 {s3OutputState = Just (PD.Constr 0 [inputData sharedInputRef])}
  ]

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

{- | Closes the family: tx2's field 0 must hold the very reference step-03
carried.

This is where a challenge against a valid block dies, and the last case is that
challenge: tx3 is a perfectly ordinary transaction of the same block that spends
a different input, so the comparison fails and no conviction is minted.
-}
step04Tests :: [TestTree]
step04Tests =
  [ testCase "convicts when tx2 spends the carried input" $
      psucceeds $ runStep04 defaultStep04
  , testCase "rejects a second transaction spending a different input" $
      pfails $
        runStep04
          defaultStep04
            { s4StateTxId = tx3Id
            , s4OpeningCbor = tx3Cbor
            , s4Preimage = Just (spendInputsPreimage tx3)
            }
  , testCase "rejects an opening of a transaction the thread did not anchor" $
      pfails $ runStep04 defaultStep04 {s4OpeningCbor = tx1Cbor}
  , testCase "rejects an input index past the end of the collection" $
      pfails $ runStep04 defaultStep04 {s4InputIndex = 3}
  , -- The conviction is what the step exists to produce, so its shape is
    -- checked as tightly as the evidence.
    testCase "rejects a conviction parked anywhere but the fraud-proof address" $
      pfails $ runStep04 defaultStep04 {s4FraudProofAddress = otherAddress}
  , testCase "rejects a conviction under a name that is not the thread's" $
      pfails $ runStep04 defaultStep04 {s4FraudProofName = otherThreadName}
  , testCase "rejects a fraud-proof mint redeemer naming another thread" $
      pfails $ runStep04 defaultStep04 {s4MintRedeemerName = otherThreadName}
  ]

--------------------------------------------------------------------------------
-- Shared surface
--------------------------------------------------------------------------------

{- | @Cancel@ and @else(_) { fail }@ are identical in all four steps, so they are
driven once each here rather than four times over.
-}
sharedTests :: [TestTree]
sharedTests =
  [ testCase "a cancel burning the thread token succeeds" $
      psucceeds $ runCancel True
  , testCase "a cancel that does not burn the thread token fails" $
      pfails $ runCancel False
  , testCase "a minting purpose fails" $
      pfails $ step01Of (asMinting (contextStep01 defaultStep01))
  , testCase "a rewarding purpose fails" $
      pfails $ step01Of (asRewarding (contextStep01 defaultStep01))
  ]

--------------------------------------------------------------------------------
-- Driving step 01
--------------------------------------------------------------------------------

data Step01 = Step01
  { s1InputState :: Maybe PD.Data
  , s1OutputScript :: BS.ByteString
  , s1OutputState :: Maybe PD.Data
  , s1PhasRoot :: BS.ByteString
  , s1Cbor :: BS.ByteString
  , s1TxId :: BS.ByteString
  }

defaultStep01 :: Step01
defaultStep01 =
  Step01
    { s1InputState = Nothing
    , s1OutputScript = nextScript
    , s1OutputState = Just (PD.Constr 0 [PD.B tx1Id])
    , s1PhasRoot = phasRoot
    , s1Cbor = tx1Cbor
    , s1TxId = tx1Id
    }

runStep01 :: forall s. Step01 -> Term s PUnit
runStep01 = step01Of . contextStep01

step01Of :: forall s. ScriptContext -> Term s PUnit
step01Of ctx =
  doubleSpendStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx

contextStep01 :: Step01 -> ScriptContext
contextStep01 s =
  spendContext
    (stepDatum (s1InputState s))
    (PD.Constr 1 [inclusionArgs (s1TxId s) (s1Cbor s) (s1PhasRoot s)])
    [threadInput]
    [stepOutput (s1OutputScript s) (s1OutputState s)]
    referenceInputs
    [phasEntry (s1PhasRoot s) (s1TxId s) (s1Cbor s)]
    mempty

--------------------------------------------------------------------------------
-- Driving step 02
--------------------------------------------------------------------------------

data Step02 = Step02
  { s2InputState :: Maybe PD.Data
  , s2OutputScript :: BS.ByteString
  , s2OutputState :: Maybe PD.Data
  }

defaultStep02 :: Step02
defaultStep02 =
  Step02
    { s2InputState = Just (PD.Constr 0 [PD.B tx1Id])
    , s2OutputScript = nextScript
    , s2OutputState = Just (PD.Constr 0 [PD.B tx1Id, PD.B tx2Id])
    }

runStep02 :: forall s. Step02 -> Term s PUnit
runStep02 s =
  doubleSpendStep02Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant
      ( spendContext
          (stepDatum (s2InputState s))
          (PD.Constr 1 [inclusionArgs tx2Id tx2Cbor phasRoot])
          [threadInput]
          [stepOutput (s2OutputScript s) (s2OutputState s)]
          referenceInputs
          [phasEntry phasRoot tx2Id tx2Cbor]
          mempty
      )

--------------------------------------------------------------------------------
-- Driving step 03
--------------------------------------------------------------------------------

data Step03 = Step03
  { s3OpeningCbor :: BS.ByteString
  , s3Preimage :: Maybe BS.ByteString
  , s3InputIndex :: Integer
  , s3OutputScript :: BS.ByteString
  , s3OutputState :: Maybe PD.Data
  }

defaultStep03 :: Step03
defaultStep03 =
  Step03
    { s3OpeningCbor = tx1Cbor
    , s3Preimage = Nothing
    , s3InputIndex = 0
    , s3OutputScript = nextScript
    , s3OutputState = Just (PD.Constr 0 [PD.B tx2Id, inputData sharedInputRef])
    }

runStep03 :: forall s. Step03 -> Term s PUnit
runStep03 s =
  doubleSpendStep03Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (PD.Constr 0 [PD.B tx1Id, PD.B tx2Id])))
          ( PD.Constr
              1
              [ PD.Constr
                  0
                  [ PD.I 0
                  , PD.I 0
                  , bodyOpening (s3OpeningCbor s) (maybe (spendInputsPreimage tx1) id (s3Preimage s))
                  , PD.I (s3InputIndex s)
                  ]
              ]
          )
          [threadInput]
          [stepOutput (s3OutputScript s) (s3OutputState s)]
          referenceInputs
          []
          mempty
      )

--------------------------------------------------------------------------------
-- Driving step 04
--------------------------------------------------------------------------------

data Step04 = Step04
  { s4StateTxId :: BS.ByteString
  , s4CarriedInput :: (BS.ByteString, Integer)
  , s4OpeningCbor :: BS.ByteString
  , s4Preimage :: Maybe BS.ByteString
  , s4InputIndex :: Integer
  , s4FraudProofAddress :: Address
  , s4FraudProofName :: BS.ByteString
  , s4MintRedeemerName :: BS.ByteString
  }

defaultStep04 :: Step04
defaultStep04 =
  Step04
    { s4StateTxId = tx2Id
    , s4CarriedInput = sharedInputRef
    , s4OpeningCbor = tx2Cbor
    , s4Preimage = Nothing
    , s4InputIndex = 0
    , s4FraudProofAddress = fraudProofAddress
    , s4FraudProofName = threadName
    , s4MintRedeemerName = threadName
    }

runStep04 :: forall s. Step04 -> Term s PUnit
runStep04 s =
  doubleSpendStep04Validator
    # pdata (pconstant ctPolicy)
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant certificatePolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (PD.Constr 0 [PD.B (s4StateTxId s), inputData (s4CarriedInput s)])))
          ( PD.Constr
              1
              [ PD.Constr
                  0
                  [ PD.I 0
                  , PD.I 0
                  , PD.I 0
                  , bodyOpening (s4OpeningCbor s) (maybe (spendInputsPreimage tx1) id (s4Preimage s))
                  , PD.I (s4InputIndex s)
                  ]
              ]
          )
          [threadInput]
          [convictionOutput (s4FraudProofAddress s) (s4FraudProofName s)]
          referenceInputs
          [fraudProofMintEntry (s4MintRedeemerName s)]
          (singleton fpPolicy (TokenName (toBuiltin (s4FraudProofName s))) 1)
      )

--------------------------------------------------------------------------------
-- Cancel
--------------------------------------------------------------------------------

{- | @Cancel@ abandons the thread. The computation-thread policy's
@BurnForCancellation@ redeemer must have run on this thread's own token.
-}
runCancel :: forall s. Bool -> Term s PUnit
runCancel burns =
  doubleSpendStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant
      ( spendContext
          (stepDatum Nothing)
          cancelRedeemer
          [threadInput]
          []
          referenceInputs
          [cancelMintEntry (if burns then threadName else otherThreadName)]
          mempty
      )
