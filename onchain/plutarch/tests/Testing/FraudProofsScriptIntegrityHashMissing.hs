{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsScriptIntegrityHashMissing (tests) where

import Data.ByteString qualified as BS
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardVersionedScript)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardScriptLanguage (..),
  PMidgardVersionedScript (..),
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.ScriptIntegrityHashMissing
import Midgard.RejectionReason (PRejectionReasonV1 (PScriptIntegrityHashMissing))
import Midgard.ScriptLanguageViews (pemptyScriptIntegrityHash)
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
  testGroup
    "Script-integrity-hash-missing rule"
    [ testCase "wrongful acceptance requires an effectful absent hash" $ passertEvalNoTrace effectfulAbsentHash
    , testCase "honest nonzero integrity hash never matches" $ passertEvalNoTrace honestNonzeroHash
    , testCase "script language scan distinguishes native from effectful scripts" $ passertEvalNoTrace languageScan
    , testCase "short integrity hash refuses closed" $ pfails $ pfaultHoldsV1 # phexByteStr "00" # pconstant True # pconstant False
    , testCase "zero hash is not the canonical absence commitment" $ passertEvalNoTrace zeroHashIsPresent
    , testCase "staged and direct limits are pinned" $ passertEvalNoTrace $ pstagedBatchLimit #== 32 #&& pdirectFieldItemLimit #== 64
    , testCase "script grammar phase preserves its checkpoint" $ passertEvalNoTrace $ phaseCheckpoint (pcon $ PScriptGrammar $ pdata h32) #== h32
    , testCase "script scan phase preserves checkpoint and finding" $ passertEvalNoTrace scriptScanPhase
    , testCase "script complete and redeemer grammar phases preserve findings" $ passertEvalNoTrace terminalPhases
    , testCase "decision state encoder matches the target wire" $ passertEvalNoTrace decisionWire
    , testCase "terminal polarity follows the shared subject" $ passertEvalNoTrace terminalPolarity
    ]

effectfulAbsentHash :: forall s. Term s PBool
effectfulAbsentHash =
  pfaultHoldsV1
    # pemptyScriptIntegrityHash
    # pconstant True
    # pconstant False
    #&& pfaultHoldsV1
    # pemptyScriptIntegrityHash
    # pconstant False
    # pconstant True
    #&& pnot
    # (pfaultHoldsV1 # pemptyScriptIntegrityHash # pconstant False # pconstant False)

honestNonzeroHash :: forall s. Term s PBool
honestNonzeroHash =
  pnot
    # (pfaultHoldsV1 # h32 # pconstant True # pconstant False)
    #&& pnot
    # (pfaultHoldsV1 # h32 # pconstant False # pconstant True)

languageScan :: forall s. Term s PBool
languageScan =
  pnot
    # (pcontainsNonNativeScriptItemsV1 # pnil)
    #&& pnot
    # (pcontainsNonNativeScriptItemsV1 # (pcons # nativeItem # pnil))
    #&& pcontainsNonNativeScriptItemsV1
    # (pcons # nativeItem # (pcons # plutusItem # pnil))
    #&& pcontainsNonNativeScriptItemsV1
    # (pcons # midgardItem # pnil)

zeroHashIsPresent :: forall s. Term s PBool
zeroHashIsPresent =
  pnot # (pfaultHoldsV1 # pconstant (BS.replicate 32 0) # pconstant True # pconstant True)

phaseCheckpoint :: forall s. Term s PStagedPhaseV1 -> Term s PByteString
phaseCheckpoint phase = pmatch phase $ \case
  PScriptGrammar checkpoint -> pfromData checkpoint
  _ -> perror

scriptScanPhase :: forall s. Term s PBool
scriptScanPhase =
  pmatch (pcon $ PScriptScan (pdata h32) (pdata $ pconstant True)) $ \case
    PScriptScan checkpoint found -> pfromData checkpoint #== h32 #&& pfromData found
    _ -> pconstant False

terminalPhases :: forall s. Term s PBool
terminalPhases =
  pmatch (pcon $ PScriptComplete $ pdata $ pconstant True) $ \case
    PScriptComplete found ->
      pfromData found
        #&& pmatch
          (pcon $ PRedeemerGrammar (pdata h32) (pdata $ pconstant True))
          ( \case
              PRedeemerGrammar checkpoint found' -> pfromData checkpoint #== h32 #&& pfromData found'
              _ -> pconstant False
          )
    _ -> pconstant False

decisionWire :: forall s. Term s PBool
decisionWire =
  pencodeDecisionStateV1
    # pcon
      ( PDecisionStateV1
          (pdata acceptedSubject)
          (pdata pemptyScriptIntegrityHash)
          (pdata $ pconstant True)
          (pdata $ pconstant False)
      )
    #== pconstant expectedDecisionWire

terminalPolarity :: forall s. Term s PBool
terminalPolarity =
  Subject.pterminalContradiction
    # acceptedSubject
    # pconstant True
    #&& Subject.pterminalContradiction
    # forcedSubject
    # pconstant False
    #&& pnot
    # (Subject.pterminalContradiction # acceptedSubject # pconstant False)
    #&& pnot
    # (Subject.pterminalContradiction # forcedSubject # pconstant True)

scriptItem :: forall s. PMidgardScriptLanguage s -> BS.ByteString -> Term s PByteString
scriptItem language bytes =
  pencodeMidgardVersionedScript
    # pcon
      ( PMidgardVersionedScript
          (pdata $ pcon language)
          (pdata $ pconstant bytes)
      )

nativeItem, plutusItem, midgardItem :: forall s. Term s PByteString
nativeItem = scriptItem PNativeCardanoScript "\x00"
plutusItem = scriptItem PPlutusV3Script "\x01"
midgardItem = scriptItem PMidgardV1Script "\x02"

acceptedSubject :: forall s. Term s Subject.PVerdictSubject
acceptedSubject =
  pcon $
    Subject.PVerdictSubject
      (pdata 1)
      (pdata 0)
      (pdata 0)
      (pdata $ pconstant txId)
      (pdata $ pconstant "")
      (pdata $ pcon PDNothing)

forcedSubject :: forall s. Term s Subject.PVerdictSubject
forcedSubject =
  pcon $
    Subject.PVerdictSubject
      (pdata 1)
      (pdata 1)
      (pdata 1)
      (pdata $ pconstant txId)
      (pdata $ phexByteStr "01")
      (pdata $ pcon $ PDJust $ pdata $ pcon PScriptIntegrityHashMissing)

h32 :: forall s. Term s PByteString
h32 = pconstant $ BS.replicate 32 1

txId :: BS.ByteString
txId = BS.pack [0 .. 31]

expectedDecisionWire :: BS.ByteString
expectedDecisionWire =
  BS.concat
    [ "\x84\x58\x28\x86\x01\x00\x00\x58\x20"
    , txId
    , "\x40\x80\x58\x20"
    , "\x01\xf4\xb7\x88\x59\x3d\x4f\x70\xde\x2a\x45\xc2\xe1\xe8\x70\x88"
    , "\xbf\xbd\xfa\x29\x57\x7a\xe1\xb6\x2a\xba\x60\xe0\x95\xe3\xab\x53"
    , "\xd8\x7a\x80\xd8\x79\x80"
    ]
