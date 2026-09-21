{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsMissingRedeemer (tests) where

import Data.ByteString qualified as BS
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.MissingRedeemer
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.RejectionReason (PRejectionReasonV1 (PRedeemerMissing))
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
  testGroup
    "Missing-redeemer rule"
    [ testCase "purpose kind mapping is consensus exact" $ passertEvalNoTrace purposeKindMapping
    , testCase "only Plutus languages can require a redeemer" $ passertEvalNoTrace languageMapping
    , testCase "complete absence and present terminals are opposite" $ passertEvalNoTrace terminalPolarity
    , testCase "forced presence contradicts for every purpose kind" $ passertEvalNoTrace forcedPresence
    , testCase "alternate pointer does not satisfy target" $ passertEvalNoTrace alternatePointer
    , testCase "binds accepted purpose and witness root" $ passertEvalNoTrace acceptedBinding
    , testCase "forced purpose authenticates exact reason" $ passertEvalNoTrace forcedBinding
    , testCase "native language is not redeemer bearing" $ passertEvalNoTrace $ pnot # (pisRedeemerBearingLanguageV1 # 2)
    , testCase "forced wrong purpose coordinate refuses" $ pfails $ pbindPurposeV1 # forcedSubject 0 0 # h32 # h32 # 1 # 0 # 1
    , testCase "wrong witness hash width refuses" $ pfails $ pbindPurposeV1 # acceptedSubject # phexByteStr "00" # h32 # 1 # 0 # 0
    , testCase "wrong trace root width refuses" $ pfails $ pbindPurposeV1 # acceptedSubject # h32 # phexByteStr "00" # 1 # 0 # 0
    , testCase "nonpositive trace count refuses" $ pfails $ pbindPurposeV1 # acceptedSubject # h32 # h32 # 0 # 0 # 0
    , testCase "exact pointer is found for every purpose kind" $ passertEvalNoTrace exactPointers
    , testCase "malformed pointer item refuses" $ pfails $ pscanItemV1 # scan False 0 1 # 0 # phexByteStr "8100"
    , testCase "skipped item index refuses" $ pfails $ pscanItemV1 # scan False 0 2 # 1 # pointer 0 0
    , testCase "scan past item count refuses" $ pfails $ pscanItemV1 # scan False 0 0 # 0 # pointer 0 0
    , testCase "scan after presence refuses" $ pfails $ pscanItemV1 # scan True 0 1 # 0 # pointer 0 0
    , testCase "premature absence decision refuses" $ pfails $ pdecisionV1 # scan False 0 1
    , testCase "out of range purpose kind refuses" $ pfails $ pbindPurposeV1 # acceptedSubject # h32 # h32 # 1 # 4 # 0
    , testCase "honest accepted presence is not a contradiction" $ passertEvalNoTrace $ pnot # (pterminalContradictionV1 # decision False False)
    , testCase "honest forced absence is not a contradiction" $ passertEvalNoTrace $ pnot # (pterminalContradictionV1 # decision True True)
    ]

purposeKindMapping :: forall s. Term s PBool
purposeKindMapping =
  isTag 0 0
    #&& isTag 1 1
    #&& isTag 2 3
    #&& isTag 3 6
    #&& pmatch (predeemerTagForPurposeKindV1 # 4) (\case PNothing -> pconstant True; PJust _ -> pconstant False)
 where
  isTag kind tag = pmatch (predeemerTagForPurposeKindV1 # kind) (\case PNothing -> pconstant False; PJust actual -> actual #== tag)

languageMapping :: forall s. Term s PBool
languageMapping =
  pisRedeemerBearingLanguageV1
    # 3
    #&& pisRedeemerBearingLanguageV1
    # 128
    #&& pnot
    # (pisRedeemerBearingLanguageV1 # 0)
    #&& pnot
    # (pisRedeemerBearingLanguageV1 # 2)

terminalPolarity :: forall s. Term s PBool
terminalPolarity =
  pterminalContradictionV1
    # decision False True
    #&& pnot
    # (pterminalContradictionV1 # decision False False)
    #&& pterminalContradictionV1
    # decision True False
    #&& pnot
    # (pterminalContradictionV1 # decision True True)

forcedPresence :: forall s. Term s PBool
forcedPresence =
  pterminalContradictionV1
    # decisionForKind True 0 False
    #&& pterminalContradictionV1
    # decisionForKind True 1 False
    #&& pterminalContradictionV1
    # decisionForKind True 2 False
    #&& pterminalContradictionV1
    # decisionForKind True 3 False

alternatePointer :: forall s. Term s PBool
alternatePointer =
  pmatch (pscanItemV1 # scan False 0 1 # 0 # pointer 1 0) $ \PScanStateV1{pscanState'found} ->
    pnot # pfromData pscanState'found

acceptedBinding :: forall s. Term s PBool
acceptedBinding =
  pmatch (bound False 2 7) $ \PBoundPurposeV1{..} ->
    pfromData pboundPurpose'purposeKind
      #== 2
      #&& pfromData pboundPurpose'purposeIndex
      #== 7
      #&& pfromData pboundPurpose'witnessSetHash
      #== h32
      #&& pfromData pboundPurpose'validationTracesRoot
      #== h32

forcedBinding :: forall s. Term s PBool
forcedBinding =
  pmatch (bound True 3 9) $ \PBoundPurposeV1{pboundPurpose'purposeKind, pboundPurpose'purposeIndex} ->
    pfromData pboundPurpose'purposeKind #== 3 #&& pfromData pboundPurpose'purposeIndex #== 9

exactPointers :: forall s. Term s PBool
exactPointers =
  found 0 0 #&& found 1 1 #&& found 2 3 #&& found 3 6
 where
  found kind tag = pmatch (pscanItemV1 # scanForKind False kind 0 1 # 0 # pointer tag 0) $ \PScanStateV1{pscanState'found} -> pfromData pscanState'found

acceptedSubject :: forall s. Term s Subject.PVerdictSubject
acceptedSubject =
  pcon $
    Subject.PVerdictSubject
      (pdata 1)
      (pdata 0)
      (pdata 0)
      (pdata h32)
      (pdata $ pconstant "")
      (pdata $ pcon PDNothing)

forcedSubject :: forall s. Integer -> Integer -> Term s Subject.PVerdictSubject
forcedSubject kind index =
  pcon $
    Subject.PVerdictSubject
      (pdata 1)
      (pdata 1)
      (pdata 1)
      (pdata h32)
      (pdata $ phexByteStr "01")
      (pdata $ pcon $ PDJust $ pdata $ pcon $ PRedeemerMissing (pdata $ pconstant kind) (pdata $ pconstant index))

bound :: forall s. Bool -> Integer -> Integer -> Term s PBoundPurposeV1
bound forced kind index =
  pbindPurposeV1
    # (if forced then forcedSubject kind index else acceptedSubject)
    # h32
    # h32
    # 1
    # pconstant kind
    # pconstant index

authenticatedForKind :: forall s. Bool -> Integer -> Integer -> Term s PAuthenticatedPurposeV1
authenticatedForKind forced kind index =
  pmatch (predeemerTagForPurposeKindV1 # pconstant kind) $ \case
    PNothing -> perror
    PJust tag ->
      pcon $
        PAuthenticatedPurposeV1
          (pdata $ bound forced kind index)
          (pdata 1)
          (pdata tag)
          (pdata h28)
          (pdata 0)
          (pdata 3)
          (pdata h32)

scanForKind :: forall s. Bool -> Integer -> Integer -> Integer -> Term s PScanStateV1
scanForKind found kind cursor itemCount =
  pcon $
    PScanStateV1
      (pdata $ authenticatedForKind False kind 0)
      (pdata h32)
      (pdata $ pconstant cursor)
      (pdata $ pconstant itemCount)
      (pdata $ pconstant found)

scan :: forall s. Bool -> Integer -> Integer -> Term s PScanStateV1
scan found cursor itemCount = scanForKind found 0 cursor itemCount

decisionForKind :: forall s. Bool -> Integer -> Bool -> Term s PDecisionStateV1
decisionForKind forced kind missing =
  pcon $ PDecisionStateV1 (pdata $ bound forced kind 0) (pdata $ pconstant missing)

decision :: forall s. Bool -> Bool -> Term s PDecisionStateV1
decision forced missing = decisionForKind forced 0 missing

pointer :: forall s. Integer -> Integer -> Term s PByteString
pointer tag index
  | tag < 24 && index < 24 = pconstant $ BS.pack [0x84, fromInteger tag, fromInteger index]
  | otherwise = perror

h32 :: forall s. Term s PByteString
h32 = pconstant $ BS.replicate 32 1

h28 :: forall s. Term s PByteString
h28 = pconstant $ BS.replicate 28 2
