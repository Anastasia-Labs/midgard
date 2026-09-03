{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsInvalidRange
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proofs/invalid-range/step-0{1,2}.ak@.

A committed transaction whose validity interval is not covered by the block's, or
whose interval is unsatisfiable outright.

Two steps, and almost all the content is in the normalisation between them, so
that is what the suite is built around.

* __Two sentinels, five shapes.__ A native body carries its interval as two
  integers with @-1@ meaning "unbounded", which is four combinations — and the
  bounded-bounded case splits again on whether the lower exceeds the upper. Every
  one of the five is driven end to end below, both to the state step-01 writes
  and through step-02's verdict.

* __Exclusive on the wire, inclusive in the type.__ The body's
  @validity_interval_end@ is exclusive and every bounded constructor holds an
  inclusive upper, so step-01 subtracts one. The off-by-one cases below sit
  exactly on the boundary, because that is where a port that dropped the
  subtraction still looks right everywhere else.

* __An unbounded range is not a fault.__ @Always@ makes step-02 abort rather than
  refuse, which is faithful to Aiken's @fail@ — a thread that got there was built
  on a premise this family cannot be about.

The fixture's block runs from 100 to 200, and its transactions carry
@[0, 65536)@, so the default fixture transaction is already convictable.
-}
module Testing.FraudProofsInvalidRange (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (Address, ScriptContext, ScriptHash (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.InvalidRange (
  invalidRangeStep01Validator,
  invalidRangeStep02Validator,
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
    "Invalid Range Fraud Proof Tests"
    [ testGroup "step-01 normalisation" normalisationTests
    , testGroup "step-01" step01Tests
    , testGroup "step-02" step02Tests
    ]

--------------------------------------------------------------------------------
-- step-01's normalisation, all five shapes
--------------------------------------------------------------------------------

{- | Each case drives step-01 against a transaction with the named interval and
asserts the /exact/ normalised range it writes, by requiring that state on the
output and letting the step refuse anything else.
-}
normalisationTests :: [TestTree]
normalisationTests =
  [ testCase "both bounds present becomes a closed range with an inclusive upper" $
      psucceeds $ step01 (context01 (binding (ranged 1 300 400) (closedRange 300 399)))
  , -- The subtraction, pinned on the boundary. A port that forgot it would write
    -- 400 here and this is the case that would catch it.
    testCase "the exclusive upper is not carried through unchanged" $
      pfails $ step01 (context01 (binding (ranged 2 300 400) (closedRange 300 400)))
  , testCase "an absent lower becomes FromNegInf on the inclusive upper" $
      psucceeds $ step01 (context01 (binding (ranged 3 (-1) 400) (fromNegInf 399)))
  , testCase "an absent upper becomes ToPosInf on the lower" $
      psucceeds $ step01 (context01 (binding (ranged 4 300 (-1)) (toPosInf 300)))
  , testCase "both absent becomes Always" $
      psucceeds $ step01 (context01 (binding (ranged 5 (-1) (-1)) always))
  , {- The unsatisfiable case, and it is unsatisfiable /after/ the subtraction:
       @[300, 300)@ is empty, so the inclusive upper is 299 and the lower exceeds
       it. A range one tick wider is an ordinary closed range. -}
    testCase "an empty interval becomes InvalidRange" $
      psucceeds $ step01 (context01 (binding (ranged 6 300 300) invalidRange))
  , testCase "the narrowest satisfiable interval is still a closed range" $
      psucceeds $ step01 (context01 (binding (ranged 7 300 301) (closedRange 300 300)))
  , -- A lower past the upper is InvalidRange too, not a closed range read
    -- backwards.
    testCase "a reversed interval becomes InvalidRange" $
      psucceeds $ step01 (context01 (binding (ranged 8 400 300) invalidRange))
  ]

--------------------------------------------------------------------------------
-- step-01
--------------------------------------------------------------------------------

step01Tests :: [TestTree]
step01Tests =
  [ testCase "binds the transaction and pairs the block's bounds with its range" $
      psucceeds $ step01 (context01 default01)
  , testCase "rejects an output at a script that is not step-02's" $
      pfails $ step01 (context01 default01 {r1OutputScript = otherScript})
  , testCase "rejects a state naming bounds the header does not carry" $
      pfails $
        step01 (context01 default01 {r1OutputState = Just (state02 0 200 (closedRange 0 65535))})
  , -- Aiken writes `expect None = m_input_state_data` here, where the other
    -- families bind that field and ignore it.
    testCase "rejects a thread whose state is already written" $
      pfails $ step01 (context01 default01 {r1InputState = Just (state02 100 200 always)})
  , testCase "rejects an inclusion proof against a root the header does not commit" $
      pfails $ step01 (context01 default01 {r1PhasRoot = otherRoot})
  ]

--------------------------------------------------------------------------------
-- step-02
--------------------------------------------------------------------------------

step02Tests :: [TestTree]
step02Tests =
  [ -- The block runs [100, 200).
    testCase "convicts a range starting before the block" $
      psucceeds $ step02 (context02 (verdict (closedRange 99 150)))
  , testCase "convicts a range ending at or after the block's end" $
      psucceeds $ step02 (context02 (verdict (closedRange 150 200)))
  , testCase "refuses a range strictly inside the block" $
      pfails $ step02 (context02 (verdict (closedRange 100 199)))
  , {- The two boundaries, which is where an inclusive/exclusive mix-up lives.
       The lower is inclusive so 100 is inside; the upper is exclusive so 199 is
       the last covered tick and 200 is not. -}
    testCase "the block's lower bound is inside it" $
      pfails $ step02 (context02 (verdict (closedRange 100 150)))
  , testCase "the tick before the block's upper bound is inside it" $
      pfails $ step02 (context02 (verdict (closedRange 150 199)))
  , -- Unbounded ends are not adjudicated: only the end that exists is compared.
    testCase "convicts a FromNegInf reaching past the block's end" $
      psucceeds $ step02 (context02 (verdict (fromNegInf 200)))
  , testCase "refuses a FromNegInf ending inside the block" $
      pfails $ step02 (context02 (verdict (fromNegInf 199)))
  , testCase "convicts a ToPosInf starting before the block" $
      psucceeds $ step02 (context02 (verdict (toPosInf 99)))
  , testCase "refuses a ToPosInf starting inside the block" $
      pfails $ step02 (context02 (verdict (toPosInf 100)))
  , -- An unsatisfiable range is a fault whatever the block's bounds are.
    testCase "convicts an unsatisfiable range" $
      psucceeds $ step02 (context02 (verdict invalidRange))
  , -- …and an unbounded one is not a fault at all, so the step aborts.
    testCase "aborts on an unbounded range rather than refusing" $
      pfails $ step02 (context02 (verdict always))
  , testCase "rejects a conviction parked anywhere but the fraud-proof address" $
      pfails $ step02 (context02 (verdict invalidRange) {r2FraudProofAddress = otherAddress})
  , testCase "rejects a conviction under a name that is not the thread's" $
      pfails $ step02 (context02 (verdict invalidRange) {r2FraudProofName = otherThreadName})
  ]

--------------------------------------------------------------------------------
-- Normalised ranges
--------------------------------------------------------------------------------

closedRange :: Integer -> Integer -> PD.Data
closedRange lower upper = PD.Constr 0 [PD.I lower, PD.I upper]

fromNegInf, toPosInf :: Integer -> PD.Data
fromNegInf upper = PD.Constr 1 [PD.I upper]
toPosInf lower = PD.Constr 2 [PD.I lower]

always, invalidRange :: PD.Data
always = PD.Constr 3 []
invalidRange = PD.Constr 4 []

--------------------------------------------------------------------------------
-- The block's bounds, and transactions with chosen intervals
--------------------------------------------------------------------------------

-- | Slots 16 and 17 of the fixture's header.
blockValidFrom, blockValidTo :: Integer
blockValidFrom = 100
blockValidTo = 200

{- | A fixture transaction with a named interval.

The fee is what separates them: it reaches the compact body, so two transactions
with different fees have different canonical ids and can be committed as distinct
leaves of the same block.
-}
ranged :: Integer -> Integer -> Integer -> Tx
ranged n start end =
  tx1 {tFee = 10_000_000 + n, tValidityStart = start, tValidityEnd = end}

--------------------------------------------------------------------------------
-- Thread state
--------------------------------------------------------------------------------

state02 :: Integer -> Integer -> PD.Data -> PD.Data
state02 validFrom validTo range = PD.Constr 0 [PD.I validFrom, PD.I validTo, range]

--------------------------------------------------------------------------------
-- Driving the validators
--------------------------------------------------------------------------------

step01, step02 :: forall s. ScriptContext -> Term s PUnit
step01 ctx =
  invalidRangeStep01Validator
    # pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step02 ctx =
  invalidRangeStep02Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant ctx

--------------------------------------------------------------------------------
-- step-01's context
--------------------------------------------------------------------------------

data Step01 = Step01
  { r1Tx :: Tx
  , r1InputState :: Maybe PD.Data
  , r1OutputScript :: BS.ByteString
  , r1OutputState :: Maybe PD.Data
  , r1PhasRoot :: BS.ByteString
  }

default01 :: Step01
default01 = binding tx1 (closedRange 0 65535)

-- | A step-01 case binding @tx@ and requiring the named normalised range.
binding :: Tx -> PD.Data -> Step01
binding tx range =
  Step01
    { r1Tx = tx
    , r1InputState = Nothing
    , r1OutputScript = nextScript
    , r1OutputState = Just (state02 blockValidFrom blockValidTo range)
    , r1PhasRoot = phasRoot
    }

context01 :: Step01 -> ScriptContext
context01 s =
  spendContext
    (stepDatum (r1InputState s))
    (PD.Constr 1 [inclusionArgs txId cbor (r1PhasRoot s)])
    [threadInput]
    [stepOutput (r1OutputScript s) (r1OutputState s)]
    referenceInputs
    [phasEntry (r1PhasRoot s) txId cbor]
    mempty
  where
    txId = txIdOf (r1Tx s)
    cbor = compactOf (r1Tx s)

--------------------------------------------------------------------------------
-- step-02's context
--------------------------------------------------------------------------------

data Step02 = Step02
  { r2Range :: PD.Data
  , r2FraudProofAddress :: Address
  , r2FraudProofName :: BS.ByteString
  }

-- | A step-02 case adjudicating the named range against the block's bounds.
verdict :: PD.Data -> Step02
verdict range =
  Step02
    { r2Range = range
    , r2FraudProofAddress = fraudProofAddress
    , r2FraudProofName = threadName
    }

context02 :: Step02 -> ScriptContext
context02 s =
  spendContext
    (stepDatum (Just (state02 blockValidFrom blockValidTo (r2Range s))))
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0]])
    [threadInput]
    [convictionOutput (r2FraudProofAddress s) (r2FraudProofName s)]
    referenceInputs
    [fraudProofMintEntry (r2FraudProofName s)]
    (singleton fpPolicy (TokenName (toBuiltin (r2FraudProofName s))) 1)
