{-# LANGUAGE OverloadedStrings #-}

module Testing.RedeemerItemProof (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.BoundedItem (PChunkProofV1 (..), pfromBytes, phashChunk)
import Midgard.CekData (PDataSummaryV1 (..))
import Midgard.CekDataTraverse (PDataTraverseControlV1 (..))
import Midgard.RedeemerItemProof
import Midgard.ValidationMerkle (PFrontierPeak, pappendLeaf, pemptyFrontier)
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.RedeemerItemProof"
  [ testCase "redeemer_item_descriptor_authenticates_header_tail_and_exact_metadata" $
      passertEvalNoTrace descriptorAuthenticatesExactMetadata
  , testGroup "redeemer_item_rejects_mutated_header_tail_and_chunk_evidence"
      [ testCase "authenticated malformed header" $ passertEvalNoTrace rejectsMalformedHeader
      , testCase "authenticated malformed tail" $ passertEvalNoTrace rejectsMalformedTail
      , testCase "substituted chunk proof" $ passertEvalNoTrace rejectsSubstitutedChunk
      , testCase "initial item count changes hash" $ passertEvalNoTrace initialItemCountChangesHash
      , testCase "tail item count changes hash" $ passertEvalNoTrace tailItemCountChangesHash
      ]
  , testCase "redeemer_item_terminal_data_summary_agrees_with_typescript" $ passertEvalNoTrace terminalSummaryAgrees
  ]

descriptorAuthenticatesExactMetadata :: forall s. Term s PBool
descriptorAuthenticatesExactMetadata =
  plet pinitial $ \initial ->
  plet (pexpectAdvanced $ pheaderProofStepV1 # initial # pwitness PRedeemerItemOpenHeader (pproofFor smallRedeemer)) $ \tailControl ->
  plet (pexpectAdvanced $ ptailProofStepV1 # tailControl # pwitness PRedeemerItemOpenTail (pproofFor smallRedeemer)) $ \terminal ->
  plet (pexpectJust $ pdescriptorV1 # terminal) $ \descriptor ->
  pmatch terminal $ \c -> pmatch descriptor $ \d ->
    pfromData (predeemerControl'stage c) #== pstageTerminal
      #&& pfromData (predeemerDescriptor'itemIndex d) #== 0
      #&& pfromData (predeemerDescriptor'itemCount d) #== 1
      #&& pfromData (predeemerDescriptor'totalLength d) #== plengthBS # smallRedeemer
      #&& pfromData (predeemerDescriptor'itemCommitment d) #== pitemCommitment smallRedeemer
      #&& pfromData (predeemerDescriptor'purposeTag d) #== 0
      #&& pfromData (predeemerDescriptor'pointerIndex d) #== 0
      #&& pfromData (predeemerDescriptor'dataOffset d) #== 4
      #&& pfromData (predeemerDescriptor'dataLength d) #== 1
      #&& pfromData (predeemerDescriptor'executionMemory d) #== 10
      #&& pfromData (predeemerDescriptor'executionSteps d) #== 20

rejectsMalformedHeader :: forall s. Term s PBool
rejectsMalformedHeader =
  pheaderProofStepV1 # pinitialFor wrongHeader # pwitness PRedeemerItemOpenHeader (pproofFor wrongHeader)
    #== pcon (PJust $ pcon PRedeemerItemProofInvalid)

rejectsMalformedTail :: forall s. Term s PBool
rejectsMalformedTail =
  plet (pexpectAdvanced $ pheaderProofStepV1 # pinitial # pwitness PRedeemerItemOpenHeader (pproofFor smallRedeemer)) $ \tailControl ->
    ptailProofStepV1 # pwrongTailControl tailControl # pwitness PRedeemerItemOpenTail (pproofFor wrongTail)
      #== pcon (PJust $ pcon PRedeemerItemProofInvalid)

rejectsSubstitutedChunk :: forall s. Term s PBool
rejectsSubstitutedChunk =
  pheaderProofStepV1 # pinitial # pwitness PRedeemerItemOpenHeader (pproofFor substitutedRedeemer)
    #== pcon PNothing

initialItemCountChangesHash :: forall s. Term s PBool
initialItemCountChangesHash = phashControlV1 # pinitial #/= phashControlV1 # pwithItemCount pinitial 2

tailItemCountChangesHash :: forall s. Term s PBool
tailItemCountChangesHash =
  plet (pexpectAdvanced $ pheaderProofStepV1 # pinitial # pwitness PRedeemerItemOpenHeader (pproofFor smallRedeemer)) $ \tailControl ->
    phashControlV1 # tailControl #/= phashControlV1 # pwithItemCount tailControl 2

terminalSummaryAgrees :: forall s. Term s PBool
terminalSummaryAgrees = plet (pdecodeControlV1 # terminalControlCbor) $ \terminal ->
  plet (pexpectJust $ pfinalizeV1 # terminal) $ \summary ->
  plet (pwrongTerminal terminal) $ \wrongTerminal ->
  pmatch terminal $ \c -> pmatch summary $ \s ->
    pfromData (predeemerControl'mode c) #== pmodeData
      #&& pfromData (predeemerControl'stage c) #== pstageTerminal
      #&& pfromData (predeemerControl'dataOffset c) #== 4
      #&& pfromData (predeemerControl'dataLength c) #== 1
      #&& pfromData (psummary'root s) #== bytes "0c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da2"
      #&& pfromData (psummary'cborLength s) #== 1
      #&& pfromData (psummary'memory s) #== 5
      #&& pcontrolIsWellFormed # wrongTerminal
      #&& phashControlV1 # wrongTerminal #/= phashControlV1 # terminal
      #&& pencodeControlV1 # terminal #== terminalControlCbor

pwrongTerminal :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PRedeemerItemProofControlV1
pwrongTerminal control = pmatch control $ \c -> pmatch (pfromData $ predeemerControl'traversal c) $ \case
  PDNothing -> perror
  PDJust traversalData -> plet (pfromData traversalData) $ \traversal -> pmatch traversal $ \t ->
    pmatch (pfromData $ ptraverse'result t) $ \case
      PDNothing -> perror
      PDJust summaryData -> plet (pfromData summaryData) $ \summary -> pmatch summary $ \s ->
        plet (pcon $ PDataSummaryV1 (pdata $ preplicateBS # 32 # (pintegerToByte # 85))
          (psummary'cborLength s) (psummary'memory s)) $ \wrongSummary ->
        plet (pcon $ PDataTraverseControlV1
          (ptraverse'version t) (ptraverse'stage t) (ptraverse'sourceStart t) (ptraverse'sourceLength t)
          (ptraverse'offset t) (ptraverse'frameRoot t) (ptraverse'pendingLargeExpectedChildren t)
          (ptraverse'integer t) (ptraverse'bytes t) (pdata $ pcon $ PDJust $ pdata wrongSummary)) $ \wrongTraversal ->
          pcon $ PRedeemerItemProofControlV1
            (predeemerControl'version c) (predeemerControl'mode c) (predeemerControl'stage c)
            (predeemerControl'itemIndex c) (predeemerControl'itemCount c) (predeemerControl'totalLength c)
            (predeemerControl'itemCommitment c) (predeemerControl'expectedPurposeTag c)
            (predeemerControl'expectedPointerIndex c) (predeemerControl'purposeTag c)
            (predeemerControl'pointerIndex c) (predeemerControl'dataOffset c) (predeemerControl'dataLength c)
            (predeemerControl'executionMemory c) (predeemerControl'executionSteps c)
            (pdata $ pcon $ PDJust $ pdata wrongTraversal)

pexpectJust :: forall s a. Term s (PMaybe a) -> Term s a
pexpectJust value = pmatch value $ \case PNothing -> perror; PJust result -> result

pexpectAdvanced :: forall s. Term s (PMaybe PRedeemerItemProofStepResultV1) -> Term s PRedeemerItemProofControlV1
pexpectAdvanced value = pmatch value $ \case
  PNothing -> perror
  PJust result -> pmatch result $ \case
    PRedeemerItemProofInvalid -> perror
    PRedeemerItemProofAdvanced control -> pfromData control

pinitial :: forall s. Term s PRedeemerItemProofControlV1
pinitial = pinitialFor smallRedeemer

pinitialFor :: forall s. Term s PByteString -> Term s PRedeemerItemProofControlV1
pinitialFor redeemer = pinitialControlV1 # pmodeDescriptor # 0 # 1 # (plengthBS # redeemer)
  # pitemCommitment redeemer # 0 # 0

pitemCommitment :: forall s. Term s PByteString -> Term s PByteString
pitemCommitment redeemer = pfromBytes # predeemerFieldIndex # 0 # redeemer

pfrontierFor :: forall s. Term s PByteString -> Term s (PBuiltinList (PAsData PFrontierPeak))
pfrontierFor redeemer = pappendLeaf # 0 # pemptyFrontier # (phashChunk # predeemerFieldIndex # 0 # 0 # redeemer)

pproofFor :: forall s. Term s PByteString -> Term s PChunkProofV1
pproofFor redeemer = pcon $ PChunkProofV1
  (pdata 1) (pdata predeemerFieldIndex) (pdata 0) (pdata $ plengthBS # redeemer) (pdata 0)
  (pdata redeemer) (pdata $ pfrontierFor redeemer) (pdata pnil)

pwitness :: forall s. PRedeemerItemProofActionV1 s -> Term s PChunkProofV1 -> Term s PRedeemerItemProofWitnessV1
pwitness action proof = pcon $ PRedeemerItemProofWitnessV1 (pdata $ pcon action)
  (pdata $ pcon $ PDJust $ pdata proof) (pdata $ pcon PDNothing)

pwrongTailControl :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PRedeemerItemProofControlV1
pwrongTailControl control = pmatch control $ \c -> pcon $ PRedeemerItemProofControlV1
  (predeemerControl'version c) (predeemerControl'mode c) (predeemerControl'stage c)
  (predeemerControl'itemIndex c) (predeemerControl'itemCount c) (pdata $ plengthBS # wrongTail)
  (pdata $ pitemCommitment wrongTail) (predeemerControl'expectedPurposeTag c)
  (predeemerControl'expectedPointerIndex c) (predeemerControl'purposeTag c)
  (predeemerControl'pointerIndex c) (predeemerControl'dataOffset c) (predeemerControl'dataLength c)
  (predeemerControl'executionMemory c) (predeemerControl'executionSteps c) (predeemerControl'traversal c)

pwithItemCount :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PInteger -> Term s PRedeemerItemProofControlV1
pwithItemCount control count = pmatch control $ \c -> pcon $ PRedeemerItemProofControlV1
  (predeemerControl'version c) (predeemerControl'mode c) (predeemerControl'stage c)
  (predeemerControl'itemIndex c) (pdata count) (predeemerControl'totalLength c)
  (predeemerControl'itemCommitment c) (predeemerControl'expectedPurposeTag c)
  (predeemerControl'expectedPointerIndex c) (predeemerControl'purposeTag c)
  (predeemerControl'pointerIndex c) (predeemerControl'dataOffset c) (predeemerControl'dataLength c)
  (predeemerControl'executionMemory c) (predeemerControl'executionSteps c) (predeemerControl'traversal c)

terminalControlCbor :: forall s. Term s PByteString
terminalControlCbor = bytes "900101030001085820f7f0d1013b70311a09ce7afa0e0ff24d93f67af1ce449ce73e699c88058f93ed2020000004010a14d8799f8a010704010140d87a80d87a80d87a80d8799f8358200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ffff"

smallRedeemer, wrongHeader, wrongTail, substitutedRedeemer :: forall s. Term s PByteString
smallRedeemer = bytes "8400004100820a14"
wrongHeader = bytes "8300004100820a14"
wrongTail = bytes "8400004100830a1415"
substitutedRedeemer = bytes "8400004100820a15"

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
