{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsMinFee
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proofs/min-fee/step-0{1,2}.ak@.

The family that cannot finalize, and the suite says so out loud rather than
leaving it to be discovered.

Aiken's @get_min_transaction_fee@ is a stub returning @0@ for every transaction —
@TODO: This will need execution traces to calculate it@ — so step-02's conclusion
is @fee < 0@. The port reproduces the stub, and the cases below pin the
consequence from both sides: every honest fee is refused, and the only thing that
convicts is a fee no honest transaction has. If the execution traces ever land,
these are the tests that will need to change, which is exactly where the reminder
belongs.

__Step-01's half is real__, and most of the suite is about it. It authenticates
the transaction against the block's counted @transactions_root@ like every other
family, and then writes the verified compact structure into the thread as @Data@.
That encoding is rebuilt here from §2.5's field layout, because it is the datum an
SDK has to produce: the port keeps 'Midgard.FraudProofs.NativeTx.Types.PNativeTxCompact'
Scott-encoded and hands step-01 a bespoke encoder, so nothing but a test comparing
against independently written bytes would catch the two drifting apart.
-}
module Testing.FraudProofsMinFee (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.MinFee (
  minFeeStep01Validator,
  minFeeStep02Validator,
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
    "Min Fee Fraud Proof Tests"
    [ testGroup "step-01" step01Tests
    , testGroup "step-02" step02Tests
    ]

--------------------------------------------------------------------------------
-- step-01
--------------------------------------------------------------------------------

step01Tests :: [TestTree]
step01Tests =
  [ {- The whole point of this group: the state step-01 writes is the compact
       structure's canonical @Data@ encoding, rebuilt below from §2.5's field
       layout rather than from the port's encoder. -}
    testCase "binds the transaction and forwards its compact form and fee" $
      psucceeds $ step01 (context01 default01)
  , testCase "…for a transaction with different field commitments too" $
      psucceeds $
        step01
          ( context01
              default01 {f1Tx = tx3, f1OutputState = Just (state02 (compactData tx3) (tFee tx3))}
          )
  , testCase "rejects a state naming a fee the body does not declare" $
      pfails $
        step01
          (context01 default01 {f1OutputState = Just (state02 (compactData tx1) (tFee tx1 + 1))})
  , -- Each of the three top-level fields, because a nesting mistake in the
    -- encoder would still produce three of something.
    testCase "rejects a state whose compact structure names another witness set" $
      pfails $
        step01
          ( context01
              default01
                { f1OutputState =
                    Just (state02 (compactDataWith tx1 (witnessSetHashOf tx3) 3) (tFee tx1))
                }
          )
  , testCase "rejects a state whose compact structure names another validity code" $
      pfails $
        step01
          ( context01
              default01
                { f1OutputState =
                    Just (state02 (compactDataWith tx1 (witnessSetHashOf tx1) 0) (tFee tx1))
                }
          )
  , testCase "rejects a state whose body is spliced flat instead of nested" $
      pfails $
        step01
          ( context01
              default01
                { f1OutputState =
                    Just
                      ( PD.Constr
                          0
                          [ PD.Constr 0 (bodyFields tx1 <> [PD.B (witnessSetHashOf tx1), PD.I 3])
                          , PD.I (tFee tx1)
                          ]
                      )
                }
          )
  , testCase "rejects an output at a script that is not step-02's" $
      pfails $ step01 (context01 default01 {f1OutputScript = otherScript})
  , testCase "rejects an inclusion proof against a root the header does not commit" $
      pfails $ step01 (context01 default01 {f1PhasRoot = otherRoot})
  ]

--------------------------------------------------------------------------------
-- step-02
--------------------------------------------------------------------------------

step02Tests :: [TestTree]
step02Tests =
  [ {- The stub, from both sides. @get_min_transaction_fee@ returns 0 for every
       transaction, so the conclusion is @fee < 0@ and no honest fee reaches it.
       Aiken's step-02 refuses these too; a port that convicted here would be the
       divergence. -}
    testCase "refuses the fixture transaction's own fee" $
      pfails $ step02 (context02 (adjudicate (compactData tx1) (tFee tx1)))
  , testCase "refuses a fee of one" $
      pfails $ step02 (context02 (adjudicate (compactData tx1) 1))
  , testCase "refuses a fee of zero" $
      pfails $ step02 (context02 (adjudicate (compactData tx1) 0))
  , -- The only thing that convicts, which is what pins the stub's value at 0.
    testCase "convicts only a fee below zero" $
      psucceeds $ step02 (context02 (adjudicate (compactData tx1) (-1)))
  , -- The compact structure is carried but never read, so it cannot change the
    -- verdict — the state is opaque to this step by construction.
    testCase "the compact structure does not change the verdict" $
      psucceeds $ step02 (context02 (adjudicate (compactData tx3) (-1)))
  , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
      pfails $
        step02
          (context02 (adjudicate (compactData tx1) (-1)) {f2FraudProofAddress = otherAddress})
  , testCase "rejects a conviction under a name that is not the thread's" $
      pfails $
        step02
          (context02 (adjudicate (compactData tx1) (-1)) {f2FraudProofName = otherThreadName})
  ]

--------------------------------------------------------------------------------
-- The compact structure's Data encoding, rebuilt from §2.5
--------------------------------------------------------------------------------

{- | @NativeTxCompact@ as @Data@: the twelve-field body nested inside, then the
witness-set hash and the validity code.

The version byte the compact /CBOR/ carries is not part of this structure — it
frames the encoding, not the record — which is the kind of thing an encoder
written twice gets right only if both copies are written from the format.
-}
compactData :: Tx -> PD.Data
compactData tx = compactDataWith tx (witnessSetHashOf tx) 3

compactDataWith :: Tx -> BS.ByteString -> Integer -> PD.Data
compactDataWith tx witnessSetHash validityCode =
  PD.Constr
    0
    [ PD.Constr 0 (bodyFields tx)
    , PD.B witnessSetHash
    , PD.I validityCode
    ]

-- | @NativeTxBodyCompact@'s twelve fields, in §2.5's order.
bodyFields :: Tx -> [PD.Data]
bodyFields tx =
  [ PD.B (blake2b256 (spendInputsPreimage tx))
  , PD.B (blake2b256 (referenceInputsPreimage tx))
  , PD.B (blake2b256 (outputsPreimage tx))
  , PD.I (tFee tx)
  , PD.I (tValidityStart tx)
  , PD.I (tValidityEnd tx)
  , PD.B (hash32 0x04)
  , PD.B (blake2b256 (requiredSignersPreimage tx))
  , PD.B (hash32 0x06)
  , PD.B (hash32 0x07)
  , PD.B (hash32 0x08)
  , PD.I 1
  ]

--------------------------------------------------------------------------------
-- Thread state
--------------------------------------------------------------------------------

state02 :: PD.Data -> Integer -> PD.Data
state02 compact fee = PD.Constr 0 [compact, PD.I fee]

--------------------------------------------------------------------------------
-- Driving the validators
--------------------------------------------------------------------------------

step01, step02 :: forall s. ScriptContext -> Term s PUnit
step01 ctx =
  minFeeStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step02 ctx =
  minFeeStep02Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant ctx

--------------------------------------------------------------------------------
-- step-01's context
--------------------------------------------------------------------------------

data Step01 = Step01
  { f1Tx :: Tx
  , f1OutputScript :: BS.ByteString
  , f1OutputState :: Maybe PD.Data
  , f1PhasRoot :: BS.ByteString
  }

default01 :: Step01
default01 =
  Step01
    { f1Tx = tx1
    , f1OutputScript = nextScript
    , f1OutputState = Just (state02 (compactData tx1) (tFee tx1))
    , f1PhasRoot = phasRoot
    }

context01 :: Step01 -> ScriptContext
context01 s =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [bareInclusionArgs txId cbor (f1PhasRoot s)])
    [threadInput]
    [stepOutput (f1OutputScript s) (f1OutputState s)]
    referenceInputs
    [phasEntry (f1PhasRoot s) txId cbor]
    mempty
  where
    txId = txIdOf (f1Tx s)
    cbor = compactOf (f1Tx s)

--------------------------------------------------------------------------------
-- step-02's context
--------------------------------------------------------------------------------

data Step02 = Step02
  { f2State :: PD.Data
  , f2FraudProofAddress :: Address
  , f2FraudProofName :: BS.ByteString
  }

adjudicate :: PD.Data -> Integer -> Step02
adjudicate compact fee =
  Step02
    { f2State = state02 compact fee
    , f2FraudProofAddress = fraudProofAddress
    , f2FraudProofName = threadName
    }

context02 :: Step02 -> ScriptContext
context02 s =
  spendContext
    (stepDatum (Just (f2State s)))
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0]])
    [threadInput]
    [convictionOutput (f2FraudProofAddress s) (f2FraudProofName s)]
    referenceInputs
    [fraudProofMintEntry (f2FraudProofName s)]
    (singleton fpPolicy (TokenName (toBuiltin (f2FraudProofName s))) 1)
