{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsProtectedOutputSignerMissing (tests) where

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.ProtectedOutputSignerMissing
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
  testGroup
    "Protected-output signer missing rule"
    [ testCase "accepted missing signer closes" $ passertEvalNoTrace $ pterminalV1 # (pscanVerdictV1 # acceptedSubject # pconstant False)
    , testCase "accepted present signer is honest" $ pfails $ requireTrue $ pterminalV1 # (pscanVerdictV1 # acceptedSubject # pconstant True)
    , testCase "forced present signer closes" $ passertEvalNoTrace forcedPresentCloses
    , testCase "forced missing signer is honest" $ pfails $ requireTrue $ pterminalV1 # (pscanVerdictV1 # forcedSubject 4 # pconstant False)
    , testCase "forced wrong output coordinate is refused" $ pfails $ boundIndex (pbindOutputV1 # forcedSubject 4 # 3) #== 3
    , testCase "forced foreign reason constructor is refused" $ pfails $ boundIndex (pbindOutputV1 # foreignForcedSubject # 4) #== 4
    , testCase "accepted subject binds any in-range coordinate" $ passertEvalNoTrace $ boundIndex (pbindOutputV1 # acceptedSubject # 7) #== 7
    , testCase "negative output coordinate is refused" $ pfails $ boundIndex (pbindOutputV1 # acceptedSubject # (-1)) #== (-1)
    , testCase "accepted subject with a reason is refused" $ pfails $ boundIndex (pbindOutputV1 # acceptedSubjectWithReason # 0) #== 0
    , testCase "forced no signer required closes" $ passertEvalNoTrace forcedDirectCloses
    , testCase "accepted subject cannot take the direct exit" $ pfails $ verdictRequired (pdirectVerdictV1 # (pbindOutputV1 # acceptedSubject # 4)) #== pconstant False
    , testCase "accepted unrequired signer never closes" $ pfails $ requireTrue $ pterminalV1 # unrequiredAcceptedVerdict
    ]

forcedPresentCloses :: forall s. Term s PBool
forcedPresentCloses =
  plet (forcedSubject 4) $ \subject ->
    plet (pbindOutputV1 # subject # 4) $ \_ ->
      pterminalV1 # (pscanVerdictV1 # subject # pconstant True)

forcedDirectCloses :: forall s. Term s PBool
forcedDirectCloses =
  plet (pdirectVerdictV1 # (pbindOutputV1 # forcedSubject 4 # 4)) $ \verdict ->
    pnot
      # verdictRequired verdict
      #&& pnot
      # verdictPresent verdict
      #&& pterminalV1
      # verdict

acceptedSubject :: forall s. Term s Subject.PVerdictSubject
acceptedSubject = subject 0 0 (pconstant "") (pcon PDNothing)

forcedSubject :: forall s. Integer -> Term s Subject.PVerdictSubject
forcedSubject outputIndex =
  subject
    1
    1
    (phexByteStr "81")
    (pcon $ PDJust $ pdata $ pcon $ PProtectedOutputSignerMissing $ pdata $ pconstant outputIndex)

foreignForcedSubject :: forall s. Term s Subject.PVerdictSubject
foreignForcedSubject =
  subject
    1
    1
    (phexByteStr "81")
    (pcon $ PDJust $ pdata $ pcon $ PSpendInputSignerMissing $ pdata 4)

acceptedSubjectWithReason :: forall s. Term s Subject.PVerdictSubject
acceptedSubjectWithReason =
  subject
    0
    0
    (pconstant "")
    (pcon $ PDJust $ pdata $ pcon $ PProtectedOutputSignerMissing $ pdata 0)

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
unrequiredAcceptedVerdict = pcon $ PVerdictV1 (pdata acceptedSubject) (pdata $ pconstant False) (pdata $ pconstant False)

boundIndex :: forall s. Term s PBoundOutputV1 -> Term s PInteger
boundIndex value = pmatch value $ \PBoundOutputV1{pboundOutput'outputIndex} -> pfromData pboundOutput'outputIndex

verdictRequired :: forall s. Term s PVerdictV1 -> Term s PBool
verdictRequired value = pmatch value $ \PVerdictV1{pverdict'signerRequired} -> pfromData pverdict'signerRequired

verdictPresent :: forall s. Term s PVerdictV1 -> Term s PBool
verdictPresent value = pmatch value $ \PVerdictV1{pverdict'signerPresent} -> pfromData pverdict'signerPresent

requireTrue :: forall s. Term s PBool -> Term s PBool
requireTrue condition = pif condition (pconstant True) perror

txId :: forall s. Term s PByteString
txId = phexByteStr "0101010101010101010101010101010101010101010101010101010101010101"
