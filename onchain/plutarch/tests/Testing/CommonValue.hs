{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CommonValue
Description : Tests for the @cardano/assets@ ports in "Midgard.Common.Value".

@from_asset_list@ is a validating conversion, and its three failure modes are
what these tests are for. The fourth case matters just as much in the other
direction: policy entries out of order must be /accepted/, because Aiken accepts
them, and a port that required sorted policies would reject transactions the
Aiken implementation allows.
-}
module Testing.CommonValue (tests) where

import Data.ByteString qualified as BS

import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..))
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PValuePairs)
import Midgard.Common.Value (
  pfromAssetList,
  pmergeValues,
  pnegateValue,
  pvalueIsNonNegative,
  pvalueWithoutAsset,
 )
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Common Value Tests"
    [ testGroup
        "fromAssetList"
        [ testCase "converts a well-formed asset list" $
            passertEval $ roundTrips [(polA, [("aa", 5), ("bb", 7)])]
        , -- Aiken inserts policies one at a time, so any policy order is fine.
          testCase "accepts policy entries that are not in ascending order" $
            passertEval $ roundTrips [(polB, [("aa", 1)]), (polA, [("aa", 2)])]
        , testCase "accepts a single policy with a single token" $
            passertEval $ roundTrips [(polA, [("aa", 1)])]
        , testCase "rejects a policy with an empty token list" $
            pfails $ convert [(polA, [])]
        , testCase "rejects token names that are not ascending" $
            pfails $ convert [(polA, [("bb", 1), ("aa", 2)])]
        , testCase "rejects duplicate token names" $
            pfails $ convert [(polA, [("aa", 1), ("aa", 2)])]
        , testCase "rejects a zero quantity" $
            pfails $ convert [(polA, [("aa", 0)])]
        , testCase "rejects a zero quantity among non-zero ones" $
            pfails $ convert [(polA, [("aa", 1), ("bb", 0), ("cc", 2)])]
        , testCase "rejects the same policy appearing twice" $
            pfails $ convert [(polA, [("aa", 1)]), (polA, [("bb", 2)])]
        ]
    , testGroup
        "valueIsNonNegative"
        [ testCase "accepts all-positive quantities" $
            passertEval $ nonNegative [(polA, [("aa", 1), ("bb", 2)])]
        , testCase "accepts an empty value" $
            passertEval $ nonNegative []
        , testCase "rejects a negative quantity" $
            passertEval $ pnot #$ nonNegative [(polA, [("aa", 1), ("bb", -1)])]
        , testCase "rejects a negative quantity under a later policy" $
            passertEval $ pnot #$ nonNegative [(polA, [("aa", 1)]), (polB, [("bb", -3)])]
        ]
    , testGroup
        "merge and negate — Aiken's subtraction"
        [ -- The shape the withdrawal validator uses: target - accumulator.
          testCase "a target strictly above the accumulator leaves a surplus" $
            passertEval $ remainderNonNegative [(polA, [("aa", 10)])] [(polA, [("aa", 4)])]
        , testCase "an exact match leaves zero, which is non-negative" $
            passertEval $ remainderNonNegative [(polA, [("aa", 4)])] [(polA, [("aa", 4)])]
        , testCase "an accumulator above the target leaves a deficit" $
            passertEval $ pnot #$ remainderNonNegative [(polA, [("aa", 4)])] [(polA, [("aa", 5)])]
        , -- This is the case that `punionWith (-)` would get wrong: an asset
          -- present only in the accumulator must come through negative.
          testCase "an asset absent from the target is a deficit" $
            passertEval $ pnot #$ remainderNonNegative [(polA, [("aa", 4)])] [(polB, [("bb", 1)])]
        , testCase "an asset absent from the accumulator is a surplus" $
            passertEval $ remainderNonNegative [(polA, [("aa", 4)]), (polB, [("bb", 1)])] [(polA, [("aa", 4)])]
        ]
    , testGroup
        "valueWithoutAsset — Aiken's add(policy, name, -1) on an entry of one"
        [ testCase "removes the name and leaves the rest of its policy" $
            passertEval $
              without [(polA, [("aa", 1), ("bb", 2)])] polA "aa"
                #== buildValue [(polA, [("bb", 2)])]
        , testCase "drops the policy that held nothing else" $
            passertEval $ without [(polA, [("aa", 1)])] polA "aa" #== buildValue []
        , testCase "leaves every other policy where it was" $
            passertEval $
              without [(polA, [("aa", 1)]), (polB, [("bb", 3)])] polA "aa"
                #== buildValue [(polB, [("bb", 3)])]
        , -- The two cases below are where this diverges from @assets.add@, which
          -- would leave a @-1@ behind. Both are unreachable through the port:
          -- every call site requires @quantity_of == 1@ first, and neither of
          -- these values has the asset at all.
          testCase "a value without the policy comes back unchanged" $
            passertEval $
              without [(polB, [("bb", 3)])] polA "aa" #== buildValue [(polB, [("bb", 3)])]
        , testCase "a policy without the name comes back unchanged" $
            passertEval $
              without [(polA, [("bb", 3)])] polA "aa" #== buildValue [(polA, [("bb", 3)])]
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

polA, polB :: CurrencySymbol
polA = CurrencySymbol (toBuiltinBS (BS.replicate 28 0x01))
polB = CurrencySymbol (toBuiltinBS (BS.replicate 28 0x02))

toBuiltinBS :: BS.ByteString -> PlutusTx.Builtins.BuiltinByteString
toBuiltinBS = PlutusTx.Builtins.toBuiltin

-- | An asset list as @Data@: @Pairs<PolicyId, Pairs<AssetName, Int>>@.
assetListData :: [(CurrencySymbol, [(BS.ByteString, Integer)])] -> PD.Data
assetListData entries =
  PD.Map
    [ ( PD.B (fromBuiltinBS (unCurrencySymbol policy))
      , PD.Map [(PD.B name, PD.I qty) | (name, qty) <- tokens]
      )
    | (policy, tokens) <- entries
    ]
  where
    fromBuiltinBS = PlutusTx.Builtins.fromBuiltin

pvaluePairs :: forall s. [(CurrencySymbol, [(BS.ByteString, Integer)])] -> Term s PValuePairs
pvaluePairs entries =
  pfromData (punsafeCoerce (pconstant @PData (assetListData entries)))

convert ::
  forall s. [(CurrencySymbol, [(BS.ByteString, Integer)])] -> Term s Value.PSortedValue
convert entries = pfromAssetList # pvaluePairs entries

{- | Converts, then checks the result agrees with a directly built value.

Comparing against a separately constructed 'Value.PSortedValue' is what makes
this a real check rather than "it did not crash": it pins the resulting
representation, including that the policies come out sorted.
-}
roundTrips ::
  forall s. [(CurrencySymbol, [(BS.ByteString, Integer)])] -> Term s PBool
roundTrips entries = convert entries #== expected
  where
    expected =
      foldr
        (\v acc -> pmergeValues # v # acc)
        (pfromAssetList # pvaluePairs [])
        [ Value.psingletonSortedValue
          # pconstant policy
          # pconstant (TokenName (toBuiltinBS name))
          # pconstant qty
        | (policy, tokens) <- entries
        , (name, qty) <- tokens
        ]

nonNegative ::
  forall s. [(CurrencySymbol, [(BS.ByteString, Integer)])] -> Term s PBool
nonNegative entries = pvalueIsNonNegative # buildValue entries

{- | @value_is_nonnegative(merge(target, negate(accumulator)))@ — the exact
composition @validate_initial_payout_accumulator_value@ performs.
-}
remainderNonNegative ::
  forall s.
  [(CurrencySymbol, [(BS.ByteString, Integer)])] ->
  [(CurrencySymbol, [(BS.ByteString, Integer)])] ->
  Term s PBool
remainderNonNegative target accumulator =
  pvalueIsNonNegative
    #$ pmergeValues
    # buildValue target
    #$ pnegateValue
    # buildValue accumulator

-- | The value with one asset entry removed, as the port removes it.
without ::
  forall s.
  [(CurrencySymbol, [(BS.ByteString, Integer)])] ->
  CurrencySymbol ->
  BS.ByteString ->
  Term s Value.PSortedValue
without entries policy name =
  pvalueWithoutAsset
    # buildValue entries
    # pdata (pconstant policy)
    # pdata (pconstant (TokenName (toBuiltinBS name)))

{- | Builds a value directly, bypassing 'pfromAssetList'.

Used where the test needs a value with negative or zero quantities, which
'pfromAssetList' rejects by design.
-}
buildValue ::
  forall s. [(CurrencySymbol, [(BS.ByteString, Integer)])] -> Term s Value.PSortedValue
buildValue entries =
  foldr
    (\v acc -> pmergeValues # v # acc)
    (pfromAssetList # pvaluePairs [])
    [ Value.psingletonSortedValue
      # pconstant policy
      # pconstant (TokenName (toBuiltinBS name))
      # pconstant qty
    | (policy, tokens) <- entries
    , (name, qty) <- tokens
    ]
