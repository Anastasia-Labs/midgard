{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsMissingScriptSource (tests) where

import Data.ByteString qualified as BS
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.MissingScriptSource
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.RejectionReason (PRejectionReasonV1 (PRedeemerMissing, PScriptSourceMissing))
import Midgard.ScriptProof (psourceDescriptorLeafHash)
import Midgard.ValidationMerkle (pappendLeaf)
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
  testGroup
    "Missing-script-source rule"
    [ testCase "binds all purpose kinds" $ passertEvalNoTrace bindsAllPurposeKinds
    , testCase "refuses substituted purpose coordinate" $ pfails $ bound True 2 7 2 8
    , testCase "refuses another reason constructor" $ pfails $ pbindPurposeV1 # wrongReasonSubject # h32 # 1 # h32 # 0 # 0
    , testCase "complete absence scans witness sources" $ passertEvalNoTrace completeAbsence
    , testCase "alternate source location finds required hash" $ passertEvalNoTrace resolvedPresence
    , testCase "wrongful acceptance absence convicts" $ passertEvalNoTrace $ pterminalContradictionV1 # absentComplete False
    , testCase "wrongful rejection presence convicts" $ passertEvalNoTrace $ pterminalContradictionV1 # presentComplete True
    , testCase "honest acceptance presence refuses" $ passertEvalNoTrace $ pnot # (pterminalContradictionV1 # presentComplete False)
    , testCase "honest rejection absence refuses" $ passertEvalNoTrace $ pnot # (pterminalContradictionV1 # absentComplete True)
    , testCase "batch advance agrees with source scan" $ passertEvalNoTrace batchAgrees
    , testCase "batch advance refuses malformed initial checkpoint" $ pfails $ padvanceScanV1 # malformedCheckpoint # (pcons # pdata (descriptor witnessLocation otherScriptHash) # pnil) # scanHash # finalHash
    , testCase "empty batch hands scan back unchanged" $ passertEvalNoTrace emptyBatchIsIdentity
    , testCase "prefix scan finalizes at scan limit" $ passertEvalNoTrace prefixFinalizesAtLimit
    , testCase "prefix scan refuses sources beyond limit" $ pfails $ pscanSourceV1 # absentComplete False # descriptor witnessLocation otherScriptHash # finalHash
    , testCase "partial scan cannot finalize" $ pfails $ pterminalContradictionV1 # initial False requiredScriptHash witnessLocation
    , testCase "substituted source membership refuses" $ pfails $ pscanSourceV1 # initial False requiredScriptHash witnessLocation # substitutedDescriptor # finalHash
    , testCase "alternate source index refuses" $ pfails $ pscanSourceV1 # initial False requiredScriptHash witnessLocation # pmatch (descriptor witnessLocation otherScriptHash) (\d@PSourceDescriptorV1{} -> pcon d{psourceDescriptor'sourceIndex = pdata 1}) # finalHash
    , testCase "alternate source location refuses" $ pfails $ pscanSourceV1 # initial False requiredScriptHash witnessLocation # descriptor resolvedLocation otherScriptHash # finalHash
    , testCase "malformed checkpoint refuses" $ pfails $ pscanSourceV1 # malformedCheckpoint # descriptor witnessLocation otherScriptHash # finalHash
    , testCase "transaction source count is bounded by scan limit" $ pfails $ pauthenticateTransactionSourcesV1 # purpose False requiredScriptHash witnessLocation otherScriptHash # 2
    , testCase "resolved count must complete the split" $ pfails $ pauthenticateResolvedSourcesV1 # (pauthenticateTransactionSourcesV1 # purpose False requiredScriptHash witnessLocation otherScriptHash # 1) # 1
    , testCase "initial state checkpoint is authentic" $ passertEvalNoTrace $ pstateIsAuthenticV1 # initial False requiredScriptHash witnessLocation
    ]

bindsAllPurposeKinds :: forall s. Term s PBool
bindsAllPurposeKinds =
  kindOf (bound False 0 0 0 0)
    #== 0
    #&& kindOf (bound False 1 0 1 0)
    #== 1
    #&& kindOf (bound False 2 0 2 0)
    #== 2
    #&& kindOf (bound False 3 0 3 0)
    #== 3
 where
  kindOf value = pmatch value $ \PBoundPurposeV1{pboundPurpose'purposeKind} -> pfromData pboundPurpose'purposeKind

completeAbsence :: forall s. Term s PBool
completeAbsence =
  pmatch (absentComplete False) $ \PSourceScanStateV1{psourceScan'cursor, psourceScan'found, psourceScan'nextExpectedScriptHash} ->
    pfromData psourceScan'cursor
      #== 1
      #&& pnot
      # pfromData
        psourceScan'found
      #&& pfromData
        psourceScan'nextExpectedScriptHash
      #== finalHash
      #&& pscanCompleteV1
      # absentComplete False

resolvedPresence :: forall s. Term s PBool
resolvedPresence =
  pmatch (presentCompleteAt resolvedLocation) $ \PSourceScanStateV1{psourceScan'found} ->
    pfromData psourceScan'found

batchAgrees :: forall s. Term s PBool
batchAgrees =
  padvanceScanV1
    # initial False requiredScriptHash witnessLocation
    # (pcons # pdata (descriptor witnessLocation otherScriptHash) # pnil)
    # scanHash
    # finalHash
    #== pscanSourceV1
    # initial False requiredScriptHash witnessLocation
    # descriptor witnessLocation otherScriptHash
    # finalHash

emptyBatchIsIdentity :: forall s. Term s PBool
emptyBatchIsIdentity =
  padvanceScanV1
    # initial False requiredScriptHash witnessLocation
    # pnil
    # scanHash
    # finalHash
    #== initial False requiredScriptHash witnessLocation

prefixFinalizesAtLimit :: forall s. Term s PBool
prefixFinalizesAtLimit =
  pmatch (presentComplete True) $ \PSourceScanStateV1{psourceScan'nextExpectedScriptHash} ->
    pfromData psourceScan'nextExpectedScriptHash #== finalHash #&& pscanCompleteV1 # presentComplete True

absentComplete :: forall s. Bool -> Term s PSourceScanStateV1
absentComplete forced =
  pscanSourceV1
    # initial forced requiredScriptHash witnessLocation
    # descriptor witnessLocation otherScriptHash
    # finalHash

presentComplete :: forall s. Bool -> Term s PSourceScanStateV1
presentComplete forced = presentCompleteAtFor forced witnessLocation

presentCompleteAt :: forall s. Integer -> Term s PSourceScanStateV1
presentCompleteAt = presentCompleteAtFor False

presentCompleteAtFor :: forall s. Bool -> Integer -> Term s PSourceScanStateV1
presentCompleteAtFor forced location =
  pscanSourceV1
    # initialWithSource forced requiredScriptHash location requiredScriptHash
    # descriptor location requiredScriptHash
    # finalHash

initial :: forall s. Bool -> Term s PByteString -> Integer -> Term s PSourceScanStateV1
initial forced required location = initialWithSource forced required location otherScriptHash

initialWithSource :: forall s. Bool -> Term s PByteString -> Integer -> Term s PByteString -> Term s PSourceScanStateV1
initialWithSource forced required location sourceHash = pinitialScanV1 # resolved forced required location sourceHash # scanHash

resolved :: forall s. Bool -> Term s PByteString -> Integer -> Term s PByteString -> Term s PAuthenticatedResolvedSourcesV1
resolved forced required location sourceHash =
  let transactionCount = if location == witnessLocation then 1 else 0
      resolvedCount = 1 - transactionCount
   in pauthenticateResolvedSourcesV1
        # (pauthenticateTransactionSourcesV1 # purpose forced required location sourceHash # pconstant transactionCount)
        # pconstant resolvedCount

purpose :: forall s. Bool -> Term s PByteString -> Integer -> Term s PByteString -> Term s PAuthenticatedPurposeV1
purpose forced required location sourceHash =
  pcon $
    PAuthenticatedPurposeV1
      (pdata $ bound forced 0 0 0 0)
      (pdata h32)
      (pdata required)
      (pdata 1)
      (pdata 1)
      (pdata $ pappendLeaf # 0 # pnil # sourceLeaf location sourceHash)

descriptor :: forall s. Integer -> Term s PByteString -> Term s PSourceDescriptorV1
descriptor location scriptHash =
  pcon $
    PSourceDescriptorV1
      (pdata 0)
      (pdata $ pconstant location)
      (pdata $ sourceKeyFor location)
      (pdata 3)
      (pdata scriptHash)
      (pdata 10)
      (pdata itemCommitment)
      (pdata pnil)

substitutedDescriptor :: forall s. Term s PSourceDescriptorV1
substitutedDescriptor =
  pmatch (descriptor witnessLocation otherScriptHash) $ \d@PSourceDescriptorV1{} ->
    pcon d{psourceDescriptor'itemCommitment = pdata h32}

sourceLeaf :: forall s. Integer -> Term s PByteString -> Term s PByteString
sourceLeaf location scriptHash =
  psourceDescriptorLeafHash
    # pconstant location
    # sourceKeyFor location
    # 3
    # scriptHash
    # 10
    # itemCommitment

malformedCheckpoint :: forall s. Term s PSourceScanStateV1
malformedCheckpoint =
  pmatch (initial False requiredScriptHash witnessLocation) $ \state@PSourceScanStateV1{} ->
    pcon state{psourceScan'checkpointHash = pdata h32}

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
      (pdata forcedSourceKey)
      (pdata $ pcon $ PDJust $ pdata $ pcon $ PScriptSourceMissing (pdata $ pconstant kind) (pdata $ pconstant index))

wrongReasonSubject :: forall s. Term s Subject.PVerdictSubject
wrongReasonSubject =
  pcon $
    Subject.PVerdictSubject
      (pdata 1)
      (pdata 1)
      (pdata 1)
      (pdata h32)
      (pdata forcedSourceKey)
      (pdata $ pcon $ PDJust $ pdata $ pcon $ PRedeemerMissing (pdata 0) (pdata 0))

bound :: forall s. Bool -> Integer -> Integer -> Integer -> Integer -> Term s PBoundPurposeV1
bound forced reasonKind reasonIndex purposeKind purposeIndex =
  pbindPurposeV1
    # (if forced then forcedSubject reasonKind reasonIndex else acceptedSubject)
    # h32
    # 1
    # h32
    # pconstant purposeKind
    # pconstant purposeIndex

witnessLocation, resolvedLocation :: Integer
witnessLocation = 0
resolvedLocation = 1

h32, itemCommitment, forcedSourceKey :: forall s. Term s PByteString
h32 = pconstant $ BS.replicate 32 1
itemCommitment = pconstant $ BS.replicate 32 2
forcedSourceKey = phexByteStr "01"

sourceKeyFor :: forall s. Integer -> Term s PByteString
sourceKeyFor location
  | location == witnessLocation = phexByteStr "01"
  | otherwise = phexByteStr "8258204444444444444444444444444444444444444444444444444444444444444444190002"

requiredScriptHash, otherScriptHash, scanHash, finalHash :: forall s. Term s PByteString
requiredScriptHash = pconstant $ BS.replicate 28 3
otherScriptHash = pconstant $ BS.replicate 28 4
scanHash = pconstant $ BS.replicate 28 5
finalHash = pconstant $ BS.replicate 28 6
