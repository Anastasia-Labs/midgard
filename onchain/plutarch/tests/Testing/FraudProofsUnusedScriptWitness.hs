{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsUnusedScriptWitness (tests) where

import Data.ByteString qualified as BS
import Plutarch.LedgerApi.Utils (PMaybeData (PDJust, PDNothing))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.UnusedScriptWitness
import Midgard.RejectionReason (PRejectionReasonV1 (PUnusedScriptWitness))
import Midgard.ScriptProof (pinlineSourceLeafHash, ppurposeLeafHash)
import Midgard.ValidationMerkle (PFrontierPeak (..), pappendLeaf)
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
  testGroup
    "Unused-script-witness rule"
    [ testCase "binds exact forced rejection coordinate" $ passertEvalNoTrace bindsForcedCoordinate
    , testCase "middle purpose match marks witness used" $ passertEvalNoTrace purposeMatchMarksUsed
    , testCase "complete absence marks witness unused" $ passertEvalNoTrace purposeAbsenceMarksUnused
    , testCase "earlier duplicate shadows accused witness" $ passertEvalNoTrace earlierDuplicateShadows
    , testCase "wrongful acceptance unused convicts" $ passertEvalNoTrace wrongfulAcceptanceConvicts
    , testCase "wrongful rejection used convicts" $ passertEvalNoTrace wrongfulRejectionConvicts
    , testCase "purpose scan before alternates complete refuses" $ pfails purposeBeforeAlternates
    , testCase "alternate walk cannot overrun coordinate" $ pfails alternateOverrun
    , testCase "decision requires complete alternate walk" $ pfails prematureDecision
    , testCase "scan batch and initial checkpoint encoding are pinned" $ passertEvalNoTrace batchAndGolden
    ]

bindsForcedCoordinate :: forall s. Term s PBool
bindsForcedCoordinate =
  pmatch (pbindWitnessV1 # psubject 1 1 # h32 # 1 # 1) $ \PBoundWitnessV1{pboundWitness'scriptIndex} ->
    pfromData pboundWitness'scriptIndex #== 1

purposeMatchMarksUsed :: forall s. Term s PBool
purposeMatchMarksUsed =
  plet (pscan 1 targetHash) $ \state ->
    pmatch state $ \PReverseScanV1{preverseScan'used} ->
      pfromData preverseScan'used #&& pmatch (pdecisionV1 # state) (\PDecisionV1{pdecision'unused} -> pnot # pfromData pdecision'unused)

purposeAbsenceMarksUnused :: forall s. Term s PBool
purposeAbsenceMarksUnused =
  plet (pscan 0 otherHash) $ \state ->
    pmatch (pdecisionV1 # state) $ \PDecisionV1{pdecision'unused} -> pfromData pdecision'unused

earlierDuplicateShadows :: forall s. Term s PBool
earlierDuplicateShadows =
  plet (pscanWithAlternate targetHash targetHash) $ \state ->
    pmatch state $ \PReverseScanV1{preverseScan'shadowed, preverseScan'used} ->
      pfromData preverseScan'shadowed
        #&& pnot
        # pfromData preverseScan'used
        #&& pmatch (pdecisionV1 # state) (\PDecisionV1{pdecision'unused} -> pfromData pdecision'unused)

wrongfulAcceptanceConvicts :: forall s. Term s PBool
wrongfulAcceptanceConvicts = pterminalContradictionV1 # (pdecisionV1 # pscan 0 otherHash)

wrongfulRejectionConvicts :: forall s. Term s PBool
wrongfulRejectionConvicts = pterminalContradictionV1 # (pdecisionV1 # pscan 1 targetHash)

purposeBeforeAlternates :: forall s. Term s PReverseScanV1
purposeBeforeAlternates =
  plet (ppurposeLeafHash # 0 # 0 # targetHash # subjectBytes) $ \purposeLeaf ->
    pscanPurposeV1
      # (pinitialReverseScanV1 # pauthenticated 0 1 (psourcePeaks targetHash) (pappendLeaf # 0 # pnil # purposeLeaf))
      # ppurposeOpening targetHash

alternateOverrun :: forall s. Term s PReverseScanV1
alternateOverrun =
  plet (pscanEarlier otherHash) $ \complete ->
    pauthenticateEarlierSourceV1 # complete # psourceOpening otherHash targetHash

prematureDecision :: forall s. Term s PDecisionV1
prematureDecision =
  plet (ppurposeLeafHash # 0 # 0 # targetHash # subjectBytes) $ \purposeLeaf ->
    pdecisionV1 # (pinitialReverseScanV1 # pauthenticated 0 1 (psourcePeaks otherHash) (pappendLeaf # 0 # pnil # purposeLeaf))

batchAndGolden :: forall s. Term s PBool
batchAndGolden =
  pmaximumScanBatch
    #== 24
    #&& pencodeReverseScanV1
    # (pinitialReverseScanV1 # goldenAuthenticated)
    #== phexByteStr
      "d8799fd8799fd8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff582033333333333333333333333333333333333333333333333333333333333333330101ff5820333333333333333333333333333333333333333333333333333333333333333303581c111111111111111111111111111111111111111111111111111111110a58203333333333333333333333333333333333333333333333333333333333333333029fd8799f0158203333333333333333333333333333333333333333333333333333333333333333ffff019fd8799f0058203333333333333333333333333333333333333333333333333333333333333333ffffff0000d87980d8798058200f7f6c7b0a0b8b3507c96310eb9a38807ead1a0832623262ed24aebe8aa7499cff"

pscan :: forall s. Term s PInteger -> Term s PByteString -> Term s PReverseScanV1
pscan direction selectedHash =
  plet (ppurposeLeafHash # 0 # 0 # selectedHash # subjectBytes) $ \purposeLeaf ->
    pscanPurposeV1
      # (pinitialReverseScanV1 # pauthenticated direction 0 (pappendLeaf # 0 # pnil # accusedLeaf) (pappendLeaf # 0 # pnil # purposeLeaf))
      # ppurposeOpening selectedHash

pscanWithAlternate :: forall s. Term s PByteString -> Term s PByteString -> Term s PReverseScanV1
pscanWithAlternate earlierHash selectedHash =
  plet (ppurposeLeafHash # 0 # 0 # selectedHash # subjectBytes) $ \purposeLeaf ->
    pscanPurposeV1
      # ( pauthenticateEarlierSourceV1
            # (pinitialReverseScanV1 # pauthenticated 0 1 (psourcePeaks earlierHash) (pappendLeaf # 0 # pnil # purposeLeaf))
            # psourceOpening earlierHash targetHash
        )
      # (pcon $ PPurposeOpeningV1 (pdata 0) (pdata 0) (pdata 0) (pdata selectedHash) (pdata subjectBytes) (pdata pnil))

pscanEarlier :: forall s. Term s PByteString -> Term s PReverseScanV1
pscanEarlier earlierHash =
  pauthenticateEarlierSourceV1
    # (pinitialReverseScanV1 # pauthenticated 0 1 (psourcePeaks earlierHash) pnil)
    # psourceOpening earlierHash targetHash

pauthenticated ::
  forall s.
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PFrontierPeak)) ->
  Term s (PBuiltinList (PAsData PFrontierPeak)) ->
  Term s PAuthenticatedWitnessV1
pauthenticated direction scriptIndex sourcePeaks purposePeaks =
  pcon $
    PAuthenticatedWitnessV1
      (pdata $ pbindWitnessV1 # psubject direction scriptIndex # h32 # 1 # scriptIndex)
      (pdata h32)
      (pdata 3)
      (pdata targetHash)
      (pdata 1)
      (pdata h32)
      (pdata $ pif (scriptIndex #== 0) 1 2)
      (pdata sourcePeaks)
      (pdata $ pif (plength # purposePeaks #== 0) 0 1)
      (pdata purposePeaks)

psourcePeaks :: forall s. Term s PByteString -> Term s (PBuiltinList (PAsData PFrontierPeak))
psourcePeaks earlierHash =
  plet (pinlineSourceLeafHash # 0 # 3 # earlierHash # 1 # h32) $ \earlierLeaf ->
    pappendLeaf # 1 # (pappendLeaf # 0 # pnil # earlierLeaf) # (pinlineSourceLeafHash # 1 # 3 # targetHash # 1 # h32)

psourceOpening :: forall s. Term s PByteString -> Term s PByteString -> Term s PSourceOpeningV1
psourceOpening earlierHash siblingHash =
  pcon $
    PSourceOpeningV1
      (pdata 0)
      (pdata 3)
      (pdata earlierHash)
      (pdata 1)
      (pdata h32)
      (pdata $ pcons # pdata (pinlineSourceLeafHash # 1 # 3 # siblingHash # 1 # h32) # pnil)

ppurposeOpening :: forall s. Term s PByteString -> Term s PPurposeOpeningV1
ppurposeOpening selectedHash =
  pcon $ PPurposeOpeningV1 (pdata 0) (pdata 0) (pdata 0) (pdata selectedHash) (pdata subjectBytes) (pdata pnil)

accusedLeaf :: forall s. Term s PByteString
accusedLeaf = pinlineSourceLeafHash # 0 # 3 # targetHash # 1 # h32

goldenAuthenticated :: forall s. Term s PAuthenticatedWitnessV1
goldenAuthenticated =
  pcon $
    PAuthenticatedWitnessV1
      (pdata $ pbindWitnessV1 # psubject 0 1 # h32 # 1 # 1)
      (pdata h32)
      (pdata 3)
      (pdata targetHash)
      (pdata 10)
      (pdata h32)
      (pdata 2)
      (pdata $ pcons # pdata (pcon $ PFrontierPeak (pdata 1) (pdata h32)) # pnil)
      (pdata 1)
      (pdata $ pcons # pdata (pcon $ PFrontierPeak (pdata 0) (pdata h32)) # pnil)

psubject :: forall s. Term s PInteger -> Term s PInteger -> Term s Subject.PVerdictSubject
psubject direction scriptIndex =
  pcon $
    Subject.PVerdictSubject
      (pdata 1)
      (pdata direction)
      (pdata $ pif (direction #== 0) 0 1)
      (pdata $ pconstant txId)
      (pdata $ pif (direction #== 0) (pconstant "") (phexByteStr "01"))
      ( pdata $
          pif
            (direction #== 0)
            (pcon PDNothing)
            (pcon $ PDJust $ pdata $ pcon $ PUnusedScriptWitness $ pdata scriptIndex)
      )

targetHash, otherHash, h32, subjectBytes :: forall s. Term s PByteString
targetHash = pconstant $ BS.replicate 28 0x11
otherHash = pconstant $ BS.replicate 28 0x22
h32 = pconstant $ BS.replicate 32 0x33
subjectBytes = phexByteStr "aa"

txId :: BS.ByteString
txId = BS.pack [0 .. 31]
