{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsZeroInput
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proofs/zero-input/step-0{1,2}.ak@.

Two validators over "Testing.FraudProofsFixture"'s committed block: step-01 binds
the disputed transaction and forwards its id, step-02 opens field 0 and convicts
when it holds no items.

__The whole family turns on one number being read positionally.__ §4 removed
field-index domain separation, so the empty field has a single commitment shared
by all nine slots. A step that compared a forwarded spend-inputs commitment
against that constant would prove "some field of this transaction is empty" — and
would convict an honest operator whose transaction merely has, say, no required
signers. The port reads the item count through the door instead, where the
commitment is derived positionally from the compact structures the verified id
authenticates.

That distinction is what the last two cases here exist for: 'txEmpty' spends
nothing and is convicted, while 'tx1' — which spends one input but whose /other/
fields are empty — is refused. A commitment-comparing port passes the first and
fails the second.
-}
module Testing.FraudProofsZeroInput (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.ZeroInput (
  zeroInputStep01Validator,
  zeroInputStep02Validator,
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
    "Zero Input Fraud Proof Tests"
    [ testGroup "the fixture" fixtureTests
    , testGroup "step-01" step01Tests
    , testGroup "step-02" step02Tests
    ]

fixtureTests :: [TestTree]
fixtureTests =
  [ testCase "the empty transaction spends nothing" $
      spendInputsOf txEmpty @?= []
  , -- §5.1: the empty field is the single byte `80`, a definite array of zero.
    testCase "its field-0 preimage is one byte" $
      spendInputsPreimage txEmpty @?= BS.pack [0x80]
  , testCase "the honest transaction spends one input" $
      length (spendInputsOf tx1) @?= 1
  ]

--------------------------------------------------------------------------------
-- Step 01
--------------------------------------------------------------------------------

step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds the disputed transaction and forwards its id" $
      psucceeds $ runStep01 defaultStep01
  , testCase "rejects an output at a script that is not step-02's" $
      pfails $ runStep01 defaultStep01 {s1OutputScript = otherScript}
  , testCase "rejects a state carrying another transaction's id" $
      pfails $ runStep01 defaultStep01 {s1OutputState = Just (PD.Constr 0 [PD.B tx1Id])}
  , -- No block roots travel: this family concludes from the transaction alone,
    -- so a state that carries one is not the state step-02 reads.
    testCase "rejects a state carrying block roots as well" $
      pfails $
        runStep01
          defaultStep01 {s1OutputState = Just (PD.Constr 0 [PD.B txEmptyId, PD.B phasRoot])}
  , testCase "rejects a raw root the header does not commit" $
      pfails $ runStep01 defaultStep01 {s1PhasRoot = otherRoot}
  , testCase "rejects compact bytes that re-derive to another id" $
      pfails $ runStep01 defaultStep01 {s1Cbor = tx1Cbor}
  , testCase "a cancel burning the thread token succeeds" $
      psucceeds $ runCancel True
  , testCase "a cancel that does not burn the thread token fails" $
      pfails $ runCancel False
  , testCase "a minting purpose fails" $
      pfails $ step01Of (asMinting (contextStep01 defaultStep01))
  , testCase "a rewarding purpose fails" $
      pfails $ step01Of (asRewarding (contextStep01 defaultStep01))
  ]

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

step02Tests :: [TestTree]
step02Tests =
  [ testCase "convicts a transaction whose field 0 is empty" $
      psucceeds $ runStep02 defaultStep02
  , -- The case the whole family is about: a transaction that genuinely spends
    -- an input is refused, even though every *other* field of it is empty and
    -- hashes to the same constant an empty field 0 would.
    testCase "rejects a transaction that spends an input" $
      pfails $
        runStep02
          defaultStep02
            { s2StateTxId = tx1Id
            , s2OpeningCbor = tx1Cbor
            , s2Preimage = spendInputsPreimage tx1
            }
  , -- The opening is anchored to thread state, so genuine bytes belonging to
    -- another transaction do not authenticate.
    testCase "rejects an opening of a transaction the thread did not anchor" $
      pfails $ runStep02 defaultStep02 {s2OpeningCbor = tx1Cbor}
  , -- The preimage still has to be the one the transaction committed. An empty
    -- field 0 offered for a transaction whose field 0 is not empty fails the
    -- door's hash check, which is where the positional slot is enforced.
    testCase "rejects an empty preimage the transaction does not commit" $
      pfails $
        runStep02 defaultStep02 {s2StateTxId = tx1Id, s2OpeningCbor = tx1Cbor}
  , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
      pfails $ runStep02 defaultStep02 {s2FraudProofAddress = otherAddress}
  , testCase "rejects a conviction under a name that is not the thread's" $
      pfails $ runStep02 defaultStep02 {s2FraudProofName = otherThreadName}
  , testCase "rejects a fraud-proof mint redeemer naming another thread" $
      pfails $ runStep02 defaultStep02 {s2MintRedeemerName = otherThreadName}
  ]

--------------------------------------------------------------------------------
-- Driving step 01
--------------------------------------------------------------------------------

data Step01 = Step01
  { s1OutputScript :: BS.ByteString
  , s1OutputState :: Maybe PD.Data
  , s1PhasRoot :: BS.ByteString
  , s1Cbor :: BS.ByteString
  , s1TxId :: BS.ByteString
  }

defaultStep01 :: Step01
defaultStep01 =
  Step01
    { s1OutputScript = nextScript
    , s1OutputState = Just (PD.Constr 0 [PD.B txEmptyId])
    , s1PhasRoot = phasRoot
    , s1Cbor = txEmptyCbor
    , s1TxId = txEmptyId
    }

runStep01 :: forall s. Step01 -> Term s PUnit
runStep01 = step01Of . contextStep01

step01Of :: forall s. ScriptContext -> Term s PUnit
step01Of ctx =
  zeroInputStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx

contextStep01 :: Step01 -> ScriptContext
contextStep01 s =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [inclusionArgs (s1TxId s) (s1Cbor s) (s1PhasRoot s)])
    [threadInput]
    [stepOutput (s1OutputScript s) (s1OutputState s)]
    referenceInputs
    [phasEntry (s1PhasRoot s) (s1TxId s) (s1Cbor s)]
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
  , s2Preimage :: BS.ByteString
  , s2FraudProofAddress :: Address
  , s2FraudProofName :: BS.ByteString
  , s2MintRedeemerName :: BS.ByteString
  }

defaultStep02 :: Step02
defaultStep02 =
  Step02
    { s2StateTxId = txEmptyId
    , s2OpeningCbor = txEmptyCbor
    , s2Preimage = spendInputsPreimage txEmpty
    , s2FraudProofAddress = fraudProofAddress
    , s2FraudProofName = threadName
    , s2MintRedeemerName = threadName
    }

runStep02 :: forall s. Step02 -> Term s PUnit
runStep02 s =
  zeroInputStep02Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant
      ( spendContext
          (stepDatum (Just (PD.Constr 0 [PD.B (s2StateTxId s)])))
          ( PD.Constr
              1
              [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, bodyOpening (s2OpeningCbor s) (s2Preimage s)]]
          )
          [threadInput]
          [convictionOutput (s2FraudProofAddress s) (s2FraudProofName s)]
          referenceInputs
          [fraudProofMintEntry (s2MintRedeemerName s)]
          (singleton fpPolicy (TokenName (toBuiltin (s2FraudProofName s))) 1)
      )
