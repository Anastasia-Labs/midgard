{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CommonUtils
Description : Direct tests for @lib/midgard/common/utils.test.ak@.
-}
module Testing.CommonUtils (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Common.Utils (pzipFoldl, pzipFoldr)
import Testing.Eval (passertEval)

tests :: TestTree
tests =
  testGroup
    "Common Utils Tests"
    [ testCase "zip_foldl" $
        passertEval $ (pzipFoldl # integers [1, 2, 3] # integers [1, 2, 3, 4] # pnil # prependPair) #== pairs [(3, 3), (2, 2), (1, 1)]
    , testCase "zip_foldr" $
        passertEval $ (pzipFoldr # integers [1, 2, 3] # integers [1, 2, 3, 4] # pnil # prependPair) #== pairs [(1, 1), (2, 2), (3, 3)]
    , testCase "both folds stop when the first list is exhausted" $
        passertEval $
          (pzipFoldl # integers [1] # integers [1, 2] # pnil # prependPair) #== pairs [(1, 1)]
            #&& ((pzipFoldr # integers [1] # integers [1, 2] # pnil # prependPair) #== pairs [(1, 1)])
    , testCase "both folds stop when the second list is exhausted" $
        passertEval $
          (pzipFoldl # integers [1, 2] # integers [1] # pnil # prependPair) #== pairs [(1, 1)]
            #&& ((pzipFoldr # integers [1, 2] # integers [1] # pnil # prependPair) #== pairs [(1, 1)])
    ]

prependPair ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
        :--> PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
    )
prependPair = plam $ \a b acc -> pcons # (ppairDataBuiltin # pdata a # pdata b) # acc

integers :: forall (s :: S). [Integer] -> Term s (PBuiltinList PInteger)
integers = foldr (\n rest -> pcons # pconstant n # rest) pnil

pairs ::
  forall (s :: S).
  [(Integer, Integer)] ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
pairs =
  foldr
    (\(a, b) rest -> pcons # (ppairDataBuiltin # pdata (pconstant a) # pdata (pconstant b)) # rest)
    pnil
