{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsNoInput
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proofs/no-input/step-0{1,2,3,4}.ak@.

Four validators over "Testing.FraudProofsFixture"'s committed block.

__Non-existence is two absences, and the tests keep them apart.__ An output
either predates the block or was produced inside it, so step-03's absence from
@prev_utxos_root@ and step-04's absence from @transactions_root@ each prove
nothing alone. Both steps are driven separately here, and each has a case where
its own absence proof is refused while everything else about the transaction is
honest.

__The two keys are different, and neither is the obvious one.__ Step-03 keys the
ledger MPF by the node's CBOR encoding of a transaction input — a definite
two-element array — and step-04 keys the transactions MPF by the raw 32-byte
transaction id. Both are checked against what the delegated @pexcludes@
withdrawal was actually invoked on, so a step that used the wrong encoding would
be refused rather than silently proving something else. The two "rejects a
@pexcludes@ claim the redeemer did not name" cases are that check.

__Both roots are thread state, and both were authenticated upstream.__
@prev_utxos_root@ comes off the challenged header and the raw transactions root
comes off the carriage /after/ step-01 checked it against the header's counted
@transactions_root@. Neither can be re-derived later, which is why the tests that
move them are step-03's and step-04's rather than step-01's.
-}
module Testing.FraudProofsNoInput (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.NoInput (
  noInputStep01Validator,
  noInputStep02Validator,
  noInputStep03Validator,
  noInputStep04Validator,
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
    "No Input Fraud Proof Tests"
    [ testGroup "step-01" step01Tests
    , testGroup "step-02" step02Tests
    , testGroup "step-03" step03Tests
    , testGroup "step-04" step04Tests
    ]

--------------------------------------------------------------------------------
-- Step 01
--------------------------------------------------------------------------------

{- | Binds the disputed transaction and forwards its id together with the two
roots the rest of the proof runs against.
-}
step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds the transaction and forwards both roots" $
      psucceeds $ runStep01 defaultStep01
  , testCase "rejects an output at a script that is not step-02's" $
      pfails $ runStep01 defaultStep01 {s1OutputScript = otherScript}
  , -- The prev-utxos root travels off the *header*, so a state naming a
    -- different one is not the state the header supports.
    testCase "rejects a state naming another initial-ledger root" $
      pfails $
        runStep01
          defaultStep01
            { s1OutputState = Just (state02 tx1Id otherRoot phasRoot)
            }
  , -- ...and the transactions root travels off the carriage, whose raw root
    -- `pass_native_tx_to_next_step` already checked against the header.
    testCase "rejects a state naming another transactions root" $
      pfails $
        runStep01
          defaultStep01
            { s1OutputState = Just (state02 tx1Id prevUtxosRoot otherRoot)
            }
  , testCase "rejects a raw root the header does not commit" $
      pfails $ runStep01 defaultStep01 {s1PhasRoot = otherRoot}
  , testCase "a cancel burning the thread token succeeds" $
      psucceeds $ runCancel True
  , testCase "a cancel that does not burn the thread token fails" $
      pfails $ runCancel False
  , testCase "a minting purpose fails" $
      pfails $ step01Of (asMinting (contextStep01 defaultStep01))
  ]

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

{- | Reads the disputed input out of the transaction's authenticated field 0 and
carries it forward with both roots untouched.
-}
step02Tests :: [TestTree]
step02Tests =
  [ testCase "reads the named input and forwards it with both roots" $
      psucceeds $ runStep02 defaultStep02
  , testCase "rejects an opening of a transaction the thread did not anchor" $
      pfails $ runStep02 defaultStep02 {s2OpeningCbor = tx3Cbor}
  , -- §7.3: an out-of-range read aborts rather than clamping.
    testCase "rejects an input index past the end of the collection" $
      pfails $ runStep02 defaultStep02 {s2BadInputIndex = 1}
  , testCase "rejects a preimage the transaction does not commit" $
      pfails $ runStep02 defaultStep02 {s2Preimage = spendInputsPreimage tx3}
  , testCase "rejects an output at a script that is not step-03's" $
      pfails $ runStep02 defaultStep02 {s2OutputScript = otherScript}
  , testCase "rejects a state naming an input the collection does not hold" $
      pfails $
        runStep02
          defaultStep02
            {s2OutputState = Just (state03 otherInputRef prevUtxosRoot phasRoot)}
  , -- Both roots have to survive this step: step-03 and step-04 each use one.
    testCase "rejects a state that alters the initial-ledger root" $
      pfails $
        runStep02
          defaultStep02
            {s2OutputState = Just (state03 sharedInputRef otherRoot phasRoot)}
  , testCase "rejects a state that alters the transactions root" $
      pfails $
        runStep02
          defaultStep02
            {s2OutputState = Just (state03 sharedInputRef prevUtxosRoot otherRoot)}
  ]

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

{- | The first absence: the disputed input is not in the block's initial ledger.

The key is the /encoded input/, not the transaction id, and the test that moves
the delegated claim to a different key is what pins that.
-}
step03Tests :: [TestTree]
step03Tests =
  [ testCase "proves absence from the initial ledger and carries the tx id on" $
      psucceeds $ runStep03 defaultStep03
  , -- The delegated `pexcludes` walk must have been invoked on this step's own
    -- root and key. A claim naming anything else proves something the step did
    -- not ask for.
    testCase "rejects a pexcludes claim under another root" $
      pfails $ runStep03 defaultStep03 {s3ClaimRoot = otherRoot}
  , testCase "rejects a pexcludes claim under another key" $
      pfails $ runStep03 defaultStep03 {s3ClaimKey = Just (BS.replicate 32 0x7f)}
  , -- The ledger MPF is keyed by the encoded input; the raw transaction id is
    -- step-04's key, and using it here would prove the wrong absence.
    testCase "rejects a pexcludes claim keyed by the raw transaction id" $
      pfails $ runStep03 defaultStep03 {s3ClaimKey = Just (fst sharedInputRef)}
  , testCase "rejects an output at a script that is not step-04's" $
      pfails $ runStep03 defaultStep03 {s3OutputScript = otherScript}
  , -- Only the producing transaction's id survives: the output index is dropped
    -- because a transaction that does not exist produced no output at any index.
    testCase "rejects a state carrying the whole input instead of its tx id" $
      pfails $
        runStep03
          defaultStep03
            {s3OutputState = Just (PD.Constr 0 [inputData sharedInputRef, PD.B phasRoot])}
  , testCase "rejects a state that alters the transactions root" $
      pfails $
        runStep03
          defaultStep03
            {s3OutputState = Just (state04 (fst sharedInputRef) otherRoot)}
  ]

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

{- | The second absence, and the conviction: no transaction of the same block
produced the missing input.
-}
step04Tests :: [TestTree]
step04Tests =
  [ testCase "proves absence from the transactions root and convicts" $
      psucceeds $ runStep04 defaultStep04
  , testCase "rejects a pexcludes claim under another root" $
      pfails $ runStep04 defaultStep04 {s4ClaimRoot = otherRoot}
  , testCase "rejects a pexcludes claim under another key" $
      pfails $ runStep04 defaultStep04 {s4ClaimKey = Just (BS.replicate 32 0x7f)}
  , -- The transactions MPF is keyed by the raw id; the encoded input is
    -- step-03's key.
    testCase "rejects a pexcludes claim keyed by the encoded input" $
      pfails $ runStep04 defaultStep04 {s4ClaimKey = Just (encodedInput sharedInputRef)}
  , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
      pfails $ runStep04 defaultStep04 {s4FraudProofAddress = otherAddress}
  , testCase "rejects a conviction under a name that is not the thread's" $
      pfails $ runStep04 defaultStep04 {s4FraudProofName = otherThreadName}
  ]

--------------------------------------------------------------------------------
-- Thread states
--------------------------------------------------------------------------------

-- | @no_input/step_02.State@.
state02 :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
state02 txId ledgerRoot txsRoot = PD.Constr 0 [PD.B txId, PD.B ledgerRoot, PD.B txsRoot]

-- | @no_input/step_03.State@.
state03 :: (BS.ByteString, Integer) -> BS.ByteString -> BS.ByteString -> PD.Data
state03 ref ledgerRoot txsRoot = PD.Constr 0 [inputData ref, PD.B ledgerRoot, PD.B txsRoot]

-- | @no_input/step_04.State@.
state04 :: BS.ByteString -> BS.ByteString -> PD.Data
state04 txId txsRoot = PD.Constr 0 [PD.B txId, PD.B txsRoot]

--------------------------------------------------------------------------------
-- Driving step 01
--------------------------------------------------------------------------------

data Step01 = Step01
  { s1OutputScript :: BS.ByteString
  , s1OutputState :: Maybe PD.Data
  , s1PhasRoot :: BS.ByteString
  }

defaultStep01 :: Step01
defaultStep01 =
  Step01
    { s1OutputScript = nextScript
    , s1OutputState = Just (state02 tx1Id prevUtxosRoot phasRoot)
    , s1PhasRoot = phasRoot
    }

runStep01 :: forall s. Step01 -> Term s PUnit
runStep01 = step01Of . contextStep01

step01Of :: forall s. ScriptContext -> Term s PUnit
step01Of ctx =
  noInputStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx

contextStep01 :: Step01 -> ScriptContext
contextStep01 s =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [inclusionArgs tx1Id tx1Cbor (s1PhasRoot s)])
    [threadInput]
    [stepOutput (s1OutputScript s) (s1OutputState s)]
    referenceInputs
    [phasEntry (s1PhasRoot s) tx1Id tx1Cbor]
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
  { s2OpeningCbor :: BS.ByteString
  , s2Preimage :: BS.ByteString
  , s2BadInputIndex :: Integer
  , s2OutputScript :: BS.ByteString
  , s2OutputState :: Maybe PD.Data
  }

defaultStep02 :: Step02
defaultStep02 =
  Step02
    { s2OpeningCbor = tx1Cbor
    , s2Preimage = spendInputsPreimage tx1
    , s2BadInputIndex = 0
    , s2OutputScript = nextScript
    , s2OutputState = Just (state03 sharedInputRef prevUtxosRoot phasRoot)
    }

runStep02 :: forall s. Step02 -> Term s PUnit
runStep02 s =
  noInputStep02Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (state02 tx1Id prevUtxosRoot phasRoot)))
          ( PD.Constr
              1
              [ PD.Constr
                  0
                  [ PD.I 0
                  , PD.I 0
                  , bodyOpening (s2OpeningCbor s) (s2Preimage s)
                  , PD.I (s2BadInputIndex s)
                  ]
              ]
          )
          [threadInput]
          [stepOutput (s2OutputScript s) (s2OutputState s)]
          referenceInputs
          []
          mempty
      )

--------------------------------------------------------------------------------
-- Driving step 03
--------------------------------------------------------------------------------

data Step03 = Step03
  { s3ClaimRoot :: BS.ByteString
  , s3ClaimKey :: Maybe BS.ByteString
  , s3OutputScript :: BS.ByteString
  , s3OutputState :: Maybe PD.Data
  }

defaultStep03 :: Step03
defaultStep03 =
  Step03
    { s3ClaimRoot = prevUtxosRoot
    , s3ClaimKey = Nothing
    , s3OutputScript = nextScript
    , s3OutputState = Just (state04 (fst sharedInputRef) phasRoot)
    }

runStep03 :: forall s. Step03 -> Term s PUnit
runStep03 s =
  noInputStep03Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (state03 sharedInputRef prevUtxosRoot phasRoot)))
          (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, redeemerCarriedNonMembership]])
          [threadInput]
          [stepOutput (s3OutputScript s) (s3OutputState s)]
          referenceInputs
          [ pexcludesEntry
              (s3ClaimRoot s)
              (maybe (encodedInput sharedInputRef) id (s3ClaimKey s))
          ]
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
  noInputStep04Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (state04 (fst sharedInputRef) phasRoot)))
          (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, redeemerCarriedNonMembership, PD.I 0]])
          [threadInput]
          [convictionOutput (s4FraudProofAddress s) (s4FraudProofName s)]
          referenceInputs
          [ fraudProofMintEntry (s4FraudProofName s)
          , pexcludesEntry (s4ClaimRoot s) (maybe (fst sharedInputRef) id (s4ClaimKey s))
          ]
          (singleton fpPolicy (TokenName (toBuiltin (s4FraudProofName s))) 1)
      )
