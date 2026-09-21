{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsSpendInputSignerMissing (tests) where

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.SpendInputSignerMissing
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
  testGroup
    "Spend-input signer missing rule"
    [ testCase "accepted_missing_signer_closes" $ passertEvalNoTrace $ pterminalV1 # (pscanVerdictV1 # acceptedSubject # pconstant True)
    , testCase "accepted_present_signer_is_honest" $ pfails $ requireTrue $ pterminalV1 # (pscanVerdictV1 # acceptedSubject # pconstant False)
    , testCase "forced_present_signer_closes" $ passertEvalNoTrace forcedPresentCloses
    , testCase "forced_missing_signer_is_honest" $ pfails $ requireTrue $ pterminalV1 # (pscanVerdictV1 # forcedSubject 3 # pconstant True)
    , testCase "forced_wrong_coordinate_is_refused" $ pfails $ boundIndex (pbindSpendInputV1 # forcedSubject 3 # 2 # priorRoot # witnessSetHash) #== 2
    , testCase "forced_no_signer_required_closes" $ passertEvalNoTrace forcedDirectCloses
    , testCase "accepted_subject_cannot_take_the_direct_exit" $ pfails $ verdictRequired (pdirectVerdictV1 # (pbindSpendInputV1 # acceptedSubject # 3 # priorRoot # witnessSetHash)) #== pconstant False
    , testCase "accepted_unrequired_signer_never_closes" $ pfails $ requireTrue $ pterminalV1 # unrequiredAcceptedVerdict
    ]

forcedPresentCloses :: forall s. Term s PBool
forcedPresentCloses =
  plet (forcedSubject 3) $ \subject ->
    plet (pbindSpendInputV1 # subject # 3 # priorRoot # witnessSetHash) $ \_ ->
      pterminalV1 # (pscanVerdictV1 # subject # pconstant False)

forcedDirectCloses :: forall s. Term s PBool
forcedDirectCloses =
  plet (pdirectVerdictV1 # (pbindSpendInputV1 # forcedSubject 3 # 3 # priorRoot # witnessSetHash)) $ \verdict ->
    pnot
      # verdictRequired verdict
      #&& pnot
      # verdictMissing verdict
      #&& pterminalV1
      # verdict

acceptedSubject :: forall s. Term s Subject.PVerdictSubject
acceptedSubject = subject 0 0 (pconstant "") (pcon PDNothing)

forcedSubject :: forall s. Integer -> Term s Subject.PVerdictSubject
forcedSubject inputIndex =
  subject
    1
    1
    (phexByteStr "81")
    (pcon $ PDJust $ pdata $ pcon $ PSpendInputSignerMissing $ pdata $ pconstant inputIndex)

subject :: forall s. Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s (PMaybeData PRejectionReasonV1) -> Term s Subject.PVerdictSubject
subject direction sourceKind sourceKey reason =
  pcon $
    Subject.PVerdictSubject
      (pdata 1)
      (pdata direction)
      (pdata sourceKind)
      (pdata txId)
      (pdata sourceKey)
      (pdata reason)

unrequiredAcceptedVerdict :: forall s. Term s PVerdictV1
unrequiredAcceptedVerdict = pcon $ PVerdictV1 (pdata acceptedSubject) (pdata $ pconstant False) (pdata $ pconstant True)

boundIndex :: forall s. Term s PBoundSpendInputV1 -> Term s PInteger
boundIndex value = pmatch value $ \PBoundSpendInputV1{pboundSpendInput'inputIndex} -> pfromData pboundSpendInput'inputIndex

verdictRequired :: forall s. Term s PVerdictV1 -> Term s PBool
verdictRequired value = pmatch value $ \PVerdictV1{pverdict'signerRequired} -> pfromData pverdict'signerRequired

verdictMissing :: forall s. Term s PVerdictV1 -> Term s PBool
verdictMissing value = pmatch value $ \PVerdictV1{pverdict'signerMissing} -> pfromData pverdict'signerMissing

requireTrue :: forall s. Term s PBool -> Term s PBool
requireTrue condition = pif condition (pconstant True) perror

txId, priorRoot, witnessSetHash :: forall s. Term s PByteString
txId = phexByteStr "0101010101010101010101010101010101010101010101010101010101010101"
priorRoot = phexByteStr "0202020202020202020202020202020202020202020202020202020202020202"
witnessSetHash = phexByteStr "0303030303030303030303030303030303030303030303030303030303030303"
