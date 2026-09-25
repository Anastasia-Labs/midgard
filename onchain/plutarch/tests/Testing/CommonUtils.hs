{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CommonUtils
Description : Direct tests for @lib/midgard/common/utils.test.ak@.
-}
module Testing.CommonUtils (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PRedeemer)
import Plutarch.Prelude
import PlutusLedgerApi.V3 qualified as Ledger
import PlutusCore.Data qualified as PD

import Midgard.Common.Utils (pgetUniqueMintRedeemer, pzipFoldl, pzipFoldr)
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Common Utils Tests"
    [ testGroup "unique mint redeemer"
        [ testCase "selects the exact policy among other purposes" $
            passertEval $ mintLookup [otherMint, sameHashReward, ownMint] #== pdata (pconstant payload)
        , testCase "missing mint purpose fails" $ pfails $ mintLookup [otherMint, sameHashReward]
        , testCase "duplicate matching purposes fail" $ pfails $ mintLookup [ownMint, ownMint]
        ]
    , testCase "zip_foldl" $
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

policy :: Ledger.CurrencySymbol
policy = Ledger.CurrencySymbol "policy"

payload :: Ledger.Redeemer
payload = Ledger.Redeemer (Ledger.dataToBuiltinData (PD.I 42))

ownMint, otherMint, sameHashReward :: (Ledger.ScriptPurpose, Ledger.Redeemer)
ownMint = (Ledger.Minting policy, payload)
otherMint = (Ledger.Minting (Ledger.CurrencySymbol "other"), payload)
sameHashReward = (Ledger.Rewarding (Ledger.ScriptCredential (Ledger.ScriptHash "policy")), payload)

mintLookup :: [(Ledger.ScriptPurpose, Ledger.Redeemer)] -> Term s (PAsData PRedeemer)
mintLookup entries = pgetUniqueMintRedeemer # pconstant entries # pconstant policy
