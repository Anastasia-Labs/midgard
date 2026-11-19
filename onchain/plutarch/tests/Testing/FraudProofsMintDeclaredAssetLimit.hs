{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsMintDeclaredAssetLimit (tests) where

import Data.ByteString qualified as BS
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.MintDeclaredAssetLimit
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Testing.Eval (passertEvalNoTrace, pfails)

txId, policyA, policyB, checkpoint :: BS.ByteString
txId = BS.pack [0 .. 31]
policyA = BS.replicate 28 0
policyB = BS.replicate 28 1
checkpoint = BS.replicate 32 0xaa

goldenFoldState :: BS.ByteString
goldenFoldState =
  BS.pack [0xd8, 0x79, 0x9f, 0xd8, 0x79, 0x9f, 0x01, 0x01, 0x01, 0x58, 0x20]
    <> txId
    <> BS.pack [0x41, 0x01, 0xd8, 0x79, 0x9f, 0xd9, 0x05, 0x17, 0x9f, 0x00, 0xff, 0xff, 0xff, 0x00, 0x58, 0x1c]
    <> policyA
    <> BS.pack [0x03, 0x58, 0x20]
    <> checkpoint
    <> BS.pack [0x02, 0x40, 0x58, 0x1c]
    <> policyA
    <> BS.pack [0x18, 0x25, 0x01, 0x02, 0x41, 0x00, 0x00, 0xff]

accepted :: forall s. Term s PVerdictSubject
accepted = pcon $ PVerdictSubject (pdata 1) (pdata 0) (pdata 0) (pdata $ pconstant txId) (pdata $ pconstant "") (pdata $ pcon PDNothing)

rejected :: forall s. Integer -> Term s PVerdictSubject
rejected index = pcon $ PVerdictSubject (pdata 1) (pdata 1) (pdata 1) (pdata $ pconstant txId) (pdata $ phexByteStr "01") (pdata $ pcon $ PDJust $ pdata $ pcon $ PMintDeclaredAssetLimit $ pdata $ pconstant index)

rejectedOther :: forall s. Term s PVerdictSubject
rejectedOther = pcon $ PVerdictSubject (pdata 1) (pdata 1) (pdata 1) (pdata $ pconstant txId) (pdata $ phexByteStr "01") (pdata $ pcon $ PDJust $ pdata $ pcon $ POutputNonCanonical $ pdata 0)

singleton, triple, crossingTarget, boundaryTarget :: BS.ByteString -> BS.ByteString
singleton policy = BS.pack [0x82, 0x58, 0x1c] <> policy <> BS.pack [0xa1, 0x40, 0x01]
triple policy = BS.pack [0x82, 0x58, 0x1c] <> policy <> BS.pack [0xa3, 0x40, 0x01, 0x41, 0x00, 0x01, 0x42, 0x00, 0x00, 0x01]
crossingTarget policy = BS.pack [0x82, 0x58, 0x1c] <> policy <> BS.pack [0xb9, 0x40, 0x01, 0x00]
boundaryTarget policy = BS.pack [0x82, 0x58, 0x1c] <> policy <> BS.pack [0xb9, 0x3f, 0xff, 0x00]

complete :: forall s. Term s PFoldStateV1 -> Integer -> BS.ByteString -> Term s PFoldStateV1
complete state index item = pmatch (pconsumeAssetsV1 # (pbeginPolicyV1 # state # pconstant index # pconstant item) # pconstant index # pconstant item # pstagedFoldBudget) $ \(PPair consumed _) -> consumed

initial :: forall s. Term s PVerdictSubject -> Integer -> BS.ByteString -> Term s PFoldStateV1
initial subject index target = pinitialFoldV1 # (pbindPolicyV1 # subject # pconstant index) # pconstant target # pconstant checkpoint

tests :: TestTree
tests =
  testGroup
    "Mint declared asset limit"
    [ testCase "reads exact policy header" $
        passertEvalNoTrace $
          pmatch (ppolicyHeaderV1 # pconstant (singleton policyA)) $ \PPolicyHeaderV1{..} ->
            pfromData ppolicyHeader'policyId
              #== pconstant policyA
              #&& pfromData ppolicyHeader'declaredCount
              #== 1
              #&& pfromData ppolicyHeader'assetsOffset
              #== 32
    , testCase "reads the machine array head" $
        let item = BS.pack [0x98, 0x02, 0x58, 0x1c] <> policyA <> BS.pack [0xa1, 0x40, 0x01]
         in passertEvalNoTrace $
              pmatch (complete (initial (rejected 0) 0 item) 0 item) $ \PFoldStateV1{pfoldState'outcome} ->
                pmatch (ppolicyHeaderV1 # pconstant item) $ \PPolicyHeaderV1{ppolicyHeader'declaredCount} ->
                  pfromData ppolicyHeader'declaredCount #== 1 #&& pfromData pfoldState'outcome #== poutcomeNonCrossing
    , testCase "accepted crossing uses header before body" $
        let target = crossingTarget policyB
            before = complete (initial accepted 1 target) 0 (singleton policyA)
         in passertEvalNoTrace $
              pmatch before $ \PFoldStateV1{pfoldState'accumulatedCount, pfoldState'previousPolicy, pfoldState'activePolicy} ->
                pmatch (pbeginPolicyV1 # before # 1 # pconstant target) $ \terminal@PFoldStateV1{pfoldState'outcome} ->
                  pfromData pfoldState'accumulatedCount
                    #== 1
                    #&& pfromData pfoldState'previousPolicy
                    #== pconstant policyA
                    #&& pfromData pfoldState'activePolicy
                    #== pconstant ""
                    #&& pfromData pfoldState'outcome
                    #== poutcomeCrossing
                    #&& (pterminalContradictionV1 #$ pdecisionV1 # pcon terminal)
    , testCase "forced non-crossing completes target item" $
        let item = triple policyA
         in passertEvalNoTrace $
              pmatch (complete (initial (rejected 0) 0 item) 0 item) $ \terminal@PFoldStateV1{pfoldState'outcome, pfoldState'accumulatedCount, pfoldState'activePolicy, pfoldState'assetsRemaining} ->
                pfromData pfoldState'outcome
                  #== poutcomeNonCrossing
                  #&& pfromData pfoldState'accumulatedCount
                  #== 3
                  #&& pfromData pfoldState'activePolicy
                  #== pconstant ""
                  #&& pfromData pfoldState'assetsRemaining
                  #== 0
                  #&& (pterminalContradictionV1 #$ pdecisionV1 # pcon terminal)
    , testCase "resumes inside a policy item" $
        let item = triple policyA
            opened = pbeginPolicyV1 # initial (rejected 0) 0 item # 0 # pconstant item
         in passertEvalNoTrace $
              pmatch (pconsumeAssetsV1 # opened # 0 # pconstant item # 2) $ \(PPair partial left) ->
                pmatch (pconsumeAssetsV1 # partial # 0 # pconstant item # 5) $ \(PPair resumed _) ->
                  pmatch partial $ \PFoldStateV1{pfoldState'outcome, pfoldState'activePolicy, pfoldState'assetsRemaining, pfoldState'policyAssetCursor, pfoldState'previousAsset, pfoldState'accumulatedCount} ->
                    left
                      #== 0
                      #&& pfromData pfoldState'outcome
                      #== poutcomeScanning
                      #&& pfromData pfoldState'activePolicy
                      #== pconstant policyA
                      #&& pfromData pfoldState'assetsRemaining
                      #== 1
                      #&& pfromData pfoldState'policyAssetCursor
                      #== 2
                      #&& pfromData pfoldState'previousAsset
                      #== phexByteStr "00"
                      #&& pfromData pfoldState'accumulatedCount
                      #== 2
                      #&& resumed
                      #== complete (initial (rejected 0) 0 item) 0 item
    , testCase "refuses opening over an open policy" $
        let item = triple policyA
            opened = pbeginPolicyV1 # initial (rejected 0) 0 item # 0 # pconstant item
         in pfails $ pbeginPolicyV1 # opened # 0 # pconstant item
    , testCase "refuses consuming a closed policy" $
        let item = singleton policyA
            closed = complete (initial (rejected 0) 0 item) 0 item
         in passertEvalNoTrace $
              pmatch (pconsumeAssetV1 # closed # 0 # pconstant item) $ \PFoldStateV1{pfoldState'accumulatedCount} ->
                pfromData pfoldState'accumulatedCount #== 1
    , testCase "accepts the exact boundary" $
        let target = boundaryTarget policyB
            before = complete (initial (rejected 1) 1 target) 0 (singleton policyA)
         in passertEvalNoTrace $
              pmatch before $ \PFoldStateV1{pfoldState'accumulatedCount} ->
                pmatch (pbeginPolicyV1 # before # 1 # pconstant target) $ \PFoldStateV1{pfoldState'outcome, pfoldState'activePolicy, pfoldState'assetsRemaining} ->
                  pfromData pfoldState'accumulatedCount
                    + 16_383
                      #== pmaxDistinctAssetCount
                      #&& pfromData pfoldState'outcome
                      #== poutcomeScanning
                      #&& pfromData pfoldState'activePolicy
                      #== pconstant policyB
                      #&& pfromData pfoldState'assetsRemaining
                      #== 16_383
    , testCase "crosses one past the boundary" $
        let target = crossingTarget policyB
            before = complete (initial accepted 1 target) 0 (singleton policyA)
         in passertEvalNoTrace $ pmatch (pbeginPolicyV1 # before # 1 # pconstant target) $ \PFoldStateV1{pfoldState'outcome} -> pfromData pfoldState'outcome #== poutcomeCrossing
    , testCase "honest accepted non-crossing refuses" $
        let item = singleton policyA
            terminal = complete (initial accepted 0 item) 0 item
         in passertEvalNoTrace $ pnot # (pterminalContradictionV1 #$ pdecisionV1 # terminal)
    , testCase "honest rejection crossing refuses" $
        let target = crossingTarget policyB
            before = complete (initial (rejected 1) 1 target) 0 (singleton policyA)
         in passertEvalNoTrace $ pnot # (pterminalContradictionV1 #$ pdecisionV1 #$ pbeginPolicyV1 # before # 1 # pconstant target)
    , testCase "refuses an unfinished decision" $
        let item = triple policyA
            opened = pbeginPolicyV1 # initial (rejected 0) 0 item # 0 # pconstant item
         in pfails $ pdecisionV1 # opened
    , testCase "refuses reason coordinate substitution" $ pfails $ pbindPolicyV1 # rejected 4 # 3
    , testCase "refuses other reason" $ pfails $ pbindPolicyV1 # rejectedOther # 0
    , testCase "refuses negative coordinate" $ pfails $ pbindPolicyV1 # accepted # (-1)
    , testCase "refuses item past the bound coordinate" $
        let item = singleton policyA
         in pfails $ pbeginPolicyV1 # initial accepted 0 item # 1 # pconstant item
    , testCase "refuses target item substitution" $
        let expected = singleton policyA
            substituted = singleton policyB
         in pfails $ pbeginPolicyV1 # initial accepted 0 expected # 0 # pconstant substituted
    , testCase "refuses declared count substitution" $
        let expected = triple policyA
            substituted = singleton policyA
         in pfails $ pbeginPolicyV1 # initial accepted 0 expected # 0 # pconstant substituted
    , testCase "refuses malformed checkpoint" $
        let item = singleton policyA
         in pfails $ pinitialFoldV1 # (pbindPolicyV1 # accepted # 0) # pconstant item # phexByteStr "aaaa"
    , testCase "refuses zero quantity in non-crossing item" $
        let item = BS.pack [0x82, 0x58, 0x1c] <> policyA <> BS.pack [0xa1, 0x40, 0x00]
         in pfails $ complete (initial (rejected 0) 0 item) 0 item
    , testCase "refuses unordered assets" $
        let item = BS.pack [0x82, 0x58, 0x1c] <> policyA <> BS.pack [0xa2, 0x41, 0x00, 0x01, 0x40, 0x01]
         in pfails $ complete (initial (rejected 0) 0 item) 0 item
    , testCase "refuses wide asset name" $
        let item = BS.pack [0x82, 0x58, 0x1c] <> policyA <> BS.pack [0xa1, 0x58, 0x21] <> BS.replicate 33 0 <> BS.pack [0x01]
         in pfails $ complete (initial (rejected 0) 0 item) 0 item
    , testCase "refuses trailing bytes after the last asset" $
        let item = BS.pack [0x82, 0x58, 0x1c] <> policyA <> BS.pack [0xa1, 0x40, 0x01, 0x00]
         in pfails $ complete (initial (rejected 0) 0 item) 0 item
    , testCase "refuses body shorter than declared" $
        let item = BS.pack [0x82, 0x58, 0x1c] <> policyA <> BS.pack [0xa2, 0x40, 0x01]
         in pfails $ complete (initial (rejected 0) 0 item) 0 item
    , testCase "refuses out-of-order prior policy" $
        let target = singleton policyA
            mutated = pmatch (initial accepted 1 target) $ \s -> pcon s{pfoldState'previousPolicy = pdata $ pconstant policyB}
         in pfails $ pbeginPolicyV1 # mutated # 1 # pconstant target
    , testCase "refuses a prior first crossing" $
        let target = singleton policyB
            prior = BS.pack [0x82, 0x58, 0x1c] <> policyA <> BS.pack [0xa2, 0x40, 0x01, 0x41, 0x00, 0x01]
            mutated = pmatch (initial accepted 1 target) $ \s -> pcon s{pfoldState'accumulatedCount = pdata 16_383}
         in pfails $ pbeginPolicyV1 # mutated # 0 # pconstant prior
    , testCase "fold state matches the shared golden vector" $
        let item = triple policyA
            opened = pbeginPolicyV1 # initial (rejected 0) 0 item # 0 # pconstant item
         in passertEvalNoTrace $
              pmatch (pconsumeAssetsV1 # opened # 0 # pconstant item # 2) $
                \(PPair partial _) -> pencodeFoldStateV1 # partial #== pconstant goldenFoldState
    ]
