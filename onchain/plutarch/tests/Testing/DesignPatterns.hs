{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.DesignPatterns
Description : Tests for the Plutarch ports of the non-linked-list
              @aiken-design-patterns@ modules Midgard uses.

Four functions, one per module. Each is small, and each carries a check that is
the whole point of the pattern — the index must resolve to the UTxO actually
being spent, the delegate's redeemer must have been computed for /this/ input,
and so on. Those are what the negative cases target.
-}
module Testing.DesignPatterns (tests) where

import Numeric (showHex)

import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusLedgerApi.V3 (
  Credential (..),
  OutputDatum (..),
  POSIXTime (..),
  Redeemer (..),
  ScriptHash (..),
  ScriptPurpose (Rewarding, Spending),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins (BuiltinData, dataToBuiltinData)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import DesignPatterns.MerkelizedValidator (pdelegatedCompute)
import DesignPatterns.ParameterValidation (papplyPrehashedParam)
import DesignPatterns.SingularUtxoIndexer (poneToOne)
import DesignPatterns.ValidityRangeNormalization (
  PNormalizedTimeRange (..),
  pnormalizeTimeRange,
 )
import Midgard.Common.Utils (
  pgetInclusiveLowerBoundOfInterval,
  pgetInclusiveUpperBoundOfInterval,
 )
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Design Patterns Tests"
    [ testGroup
        "singularUtxoIndexer.oneToOne"
        [ testCase "resolves the indexed input and output" $
            passertEval $ runOneToOne 0 0 (outRefN 0) (pconstant True)
        , testCase "rejects an index pointing at a different input" $
            pfails $ runOneToOne 1 0 (outRefN 0) (pconstant True)
        , testCase "rejects when double satisfaction is not prevented" $
            pfails $ runOneToOne 0 0 (outRefN 0) (pconstant False)
        , testCase "rejects an out-of-range output index" $
            pfails $ runOneToOne 0 9 (outRefN 0) (pconstant True)
        ]
    , testGroup
        "validityRangeNormalization.normalizeTimeRange"
        [ testCase "an unbounded range is Always" $
            passertEval $ isAlways (Interval (LowerBound NegInf True) (UpperBound PosInf True))
        , testCase "inclusive finite bounds are preserved" $
            passertEval $ isClosed (finite 100 True 200 True) 100 200
        , testCase "an exclusive lower bound moves up by one" $
            passertEval $ isClosed (finite 100 False 200 True) 101 200
        , testCase "an exclusive upper bound moves down by one" $
            passertEval $ isClosed (finite 100 True 200 False) 100 199
        , testCase "an inverted range is invalid" $
            passertEval $ isInvalid (finite 200 True 100 True)
        , testCase "a degenerate point range is invalid" $
            passertEval $ isInvalid (finite 100 True 100 True)
        , -- The two open-ended shapes, which nothing tested until
          -- `withdrawn-reference-input` and `invalid-range` forced this type
          -- from Scott to `Constr`. They are the arms the branch-selection
          -- hazard silently swallowed; see below.
          testCase "a range unbounded below is FromNegInf" $
            passertEval $ isFromNegInf (Interval (LowerBound NegInf True) (upper 200 True)) 200
        , testCase "a range unbounded above is ToPosInf" $
            passertEval $ isToPosInf (Interval (lower 100 True) (UpperBound PosInf True)) 100
        ]
    , {- Both readers, on every arm that can return a bound.

         These are direct tests for what the README's branch-selection hazard
         section names as the one candidate it judged safe *because* of its Scott
         encoding. Moving `NormalizedTimeRange` to `DeriveAsDataStruct` made it
         unsafe overnight: both readers had two arms with identical bodies,
         Plutarch selected neither, and the wildcard `perror` swallowed valid
         ranges. Three validator suites caught it only indirectly, which is why
         the readers now have cases of their own. -}
      testGroup
        "utils.get_inclusive_{lower,upper}_bound_of_interval"
        [ testCase "the upper bound of a closed range" $
            passertEval $
              pgetInclusiveUpperBoundOfInterval # pconstant (finite 100 True 200 True) #== 200
        , testCase "the upper bound of a range unbounded below" $
            passertEval $
              pgetInclusiveUpperBoundOfInterval
                # pconstant (Interval (LowerBound NegInf True) (upper 200 True))
                #== 200
        , testCase "the lower bound of a closed range" $
            passertEval $
              pgetInclusiveLowerBoundOfInterval # pconstant (finite 100 True 200 True) #== 100
        , testCase "the lower bound of a range unbounded above" $
            passertEval $
              pgetInclusiveLowerBoundOfInterval
                # pconstant (Interval (lower 100 True) (UpperBound PosInf True))
                #== 100
        , testCase "an unbounded range has no upper bound to report" $
            pfails $
              pgetInclusiveUpperBoundOfInterval
                # pconstant (Interval (LowerBound NegInf True) (UpperBound PosInf True))
        , testCase "an unbounded range has no lower bound to report" $
            pfails $
              pgetInclusiveLowerBoundOfInterval
                # pconstant (Interval (LowerBound NegInf True) (UpperBound PosInf True))
        , testCase "an inverted range has neither" $
            pfails $ pgetInclusiveUpperBoundOfInterval # pconstant (finite 200 True 100 True)
        ]
    , testGroup
        "parameterValidation.applyPrehashedParam"
        [ testCase "is deterministic for the same inputs" $
            passertEval $
              papplyPrehashedParam # 3 # pconstant "beef" # pconstant "cafe"
                #== papplyPrehashedParam # 3 # pconstant "beef" # pconstant "cafe"
        , testCase "a different parameter gives a different hash" $
            passertEval $
              pnot
                #$ papplyPrehashedParam
                # 3
                # pconstant "beef"
                # pconstant "cafe"
                #== papplyPrehashedParam
                # 3
                # pconstant "beef"
                # pconstant "d00d"
        , testCase "a different Plutus version gives a different hash" $
            passertEval $
              pnot
                #$ papplyPrehashedParam
                # 2
                # pconstant "beef"
                # pconstant "cafe"
                #== papplyPrehashedParam
                # 3
                # pconstant "beef"
                # pconstant "cafe"
        , testCase "the result is 28 bytes" $
            passertEval $
              plengthBS
                # pto (papplyPrehashedParam # 3 # pconstant "beef" # pconstant "cafe")
                #== 28
        ]
    , testGroup
        "merkelizedValidator.delegatedCompute"
        [ testCase "returns the delegate's result for a matching input" $
            passertEval $ runDelegated 7 (computation 7 42) 0 #== 42
        , testCase "rejects a result computed for a different input" $
            pfails $ runDelegated 7 (computation 8 42) 0
        , testCase "rejects a redeemer index pointing at another purpose" $
            pfails $ runDelegated 7 (computation 7 42) 1
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (replicate (56 - length h) '0' <> h)
  where
    h = showHex n ""

delegateHash :: ScriptHash
delegateHash = ScriptHash (unCurrencySymbol (policyFor 1))

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

mkIn :: Integer -> TxInInfo
mkIn n =
  TxInInfo
    (outRefN n)
    (TxOut (scriptHashAddress delegateHash) (mkAdaValue 2_000_000) NoOutputDatum Nothing)

mkOut :: TxOut
mkOut = TxOut (scriptHashAddress delegateHash) (mkAdaValue 1_000_000) NoOutputDatum Nothing

finite :: Integer -> Bool -> Integer -> Bool -> Interval POSIXTime
finite lo loInc hi hiInc = Interval (lower lo loInc) (upper hi hiInc)

lower :: Integer -> Bool -> LowerBound POSIXTime
lower t inclusive = LowerBound (Finite (POSIXTime t)) inclusive

upper :: Integer -> Bool -> UpperBound POSIXTime
upper t inclusive = UpperBound (Finite (POSIXTime t)) inclusive

-- | @ComputationRedeemer { input_arg, result }@ — a record, so @Constr 0@.
computation :: Integer -> Integer -> BuiltinData
computation inputArg result =
  dataToBuiltinData (PD.Constr 0 [PD.I inputArg, PD.I result])

--------------------------------------------------------------------------------
-- Applying the terms
--------------------------------------------------------------------------------

runOneToOne :: forall s. Integer -> Integer -> TxOutRef -> Term s PBool -> Term s PBool
runOneToOne inIx outIx ownRef dsPrevented =
  poneToOne
    (pconstant inIx)
    (pconstant outIx)
    (pconstant ownRef)
    (pconstant [mkIn 0, mkIn 1])
    (pconstant [mkOut])
    dsPrevented
    (\_in _out -> pconstant True)

normalized :: forall s. Interval POSIXTime -> Term s PNormalizedTimeRange
normalized i = pnormalizeTimeRange # pconstant i

isAlways :: forall s. Interval POSIXTime -> Term s PBool
isAlways i = pmatch (normalized i) $ \case
  PAlways -> pconstant True
  _ -> pconstant False

isInvalid :: forall s. Interval POSIXTime -> Term s PBool
isInvalid i = pmatch (normalized i) $ \case
  PInvalidRange -> pconstant True
  _ -> pconstant False

isClosed :: forall s. Interval POSIXTime -> Term s PInteger -> Term s PInteger -> Term s PBool
isClosed i lo hi = pmatch (normalized i) $ \case
  PClosedRange l u -> pand' # (pfromData l #== lo) # (pfromData u #== hi)
  _ -> pconstant False

isFromNegInf :: forall s. Interval POSIXTime -> Term s PInteger -> Term s PBool
isFromNegInf i hi = pmatch (normalized i) $ \case
  PFromNegInf u -> pfromData u #== hi
  _ -> pconstant False

isToPosInf :: forall s. Interval POSIXTime -> Term s PInteger -> Term s PBool
isToPosInf i lo = pmatch (normalized i) $ \case
  PToPosInf l -> pfromData l #== lo
  _ -> pconstant False

{- | Applies 'pdelegatedCompute' against a redeemer list whose entry 0 is the
delegate's withdrawal and entry 1 is an unrelated spend.
-}
runDelegated :: forall s. Integer -> BuiltinData -> Integer -> Term s PInteger
runDelegated functionInput delegateRedeemer redeemerIndex =
  pdelegatedCompute
    (pconstant @PInteger functionInput)
    (pdata (pconstant delegateHash))
    ( pconstant
        [ (Rewarding (ScriptCredential delegateHash), Redeemer delegateRedeemer)
        , (Spending (outRefN 0), Redeemer (dataToBuiltinData (PD.I 0)))
        ]
    )
    (pconstant redeemerIndex)
    asInteger
    asInteger
  where
    -- The delegate's redeemer carries raw Data; the caller decides the type.
    asInteger :: Term s PData -> Term s PInteger
    asInteger d = pfromData (punsafeCoerce d :: Term s (PAsData PInteger))
