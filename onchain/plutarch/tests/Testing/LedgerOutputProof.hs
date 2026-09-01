{-# LANGUAGE OverloadedStrings #-}

module Testing.LedgerOutputProof (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.Blake2b224Trace qualified as Blake
import Midgard.BoundedItem (PChunkProofV1 (..), pcommitment, phashChunk)
import Midgard.CekData (PDataSummaryV1 (..), pemptyDataPairSummaryV1, pmapDataSummaryV1, psemanticDataSummaryV1)
import Midgard.CekData qualified as CekData
import Midgard.CekDataFrame qualified as Frame
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pledgerOutputCommitmentVersion)
import Midgard.LedgerOutputProof
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerOutputValue qualified as Value
import Midgard.ValidationMerkle (PFrontierPeak, pappendLeaf, pemptyFrontier, pfrontierCommitment)
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.LedgerOutputProof"
  [ testGroup "authenticates_a_complete_output_without_a_reference_script"
      [ testCase "terminal control" $ passertEvalNoTrace authenticatesCompleteOutput
      , testCase "value summary" $ passertEvalNoTrace authenticatesValueSummary
      , testCase "cardano summary" $ passertEvalNoTrace authenticatesCardanoSummary
      , testCase "midgard summary" $ passertEvalNoTrace authenticatesMidgardSummary
      , testCase "exact descriptor" $ passertEvalNoTrace acceptsExactDescriptor
      , testCase "lovelace substitution" $ passertEvalNoTrace rejectsLovelaceSubstitution
      , testCase "summary substitution" $ passertEvalNoTrace rejectsSummarySubstitution
      ]
  , testCase "derives_the_first_script_hash_block_from_the_output_chunk" $ passertEvalNoTrace derivesFirstScriptHashBlock
  , testGroup "authenticates_the_inline_datum_semantics_from_output_chunks"
      [ testCase "opens the authenticated datum" $ passertEvalNoTrace authenticatesInlineDatumHead
      , testCase "finalizes the authenticated datum" $ passertEvalNoTrace authenticatesInlineDatumFold
      , testCase "publishes the spend datum summary" $ passertEvalNoTrace authenticatesInlineDatum
      ]
  , testCase "reports_authenticated_invalid_output_bytes" $ passertEvalNoTrace reportsInvalidOutput
  , testCase "fails_closed_for_a_substituted_chunk" $ passertEvalNoTrace rejectsSubstitutedChunk
  , testCase "fails_closed_for_a_substituted_reference_script_item_chunk" $ passertEvalNoTrace rejectsSubstitutedReferenceChunk
  , testCase "fails_closed_for_a_substituted_inline_datum_item_chunk" $ passertEvalNoTrace rejectsSubstitutedDatumChunk
  , testCase "decodes_the_long_typescript_terminal_control_canonically" $ passertEvalNoTrace decodesLongTerminalControl
  ]

authenticatesCompleteOutput :: forall s. Term s PBool
authenticatesCompleteOutput =
  plet pnoReferenceTerminal $ \terminal ->
  pmatch terminal $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \scan ->
    pcontrolIsWellFormed # terminal
      #&& pterminalIsExactV1 # terminal
      #&& pfromData (Scan.pscan'address scan) #== bytes "7811111111111111111111111111111111111111111111111111111111"
      #&& pfromData (Scan.pscan'lovelace scan) #== 0
      #&& pfromData (pproof'scriptHash c) #== pcon PDNothing

authenticatesValueSummary :: forall s. Term s PBool
authenticatesValueSummary =
  pvalueSummaryV1 # pnoReferenceTerminal #== pcon (PJust $ psemanticDataSummaryV1 # pconstant (PD.Map []))

authenticatesCardanoSummary :: forall s. Term s PBool
authenticatesCardanoSummary =
  pcardanoTxOutSummaryV1 # pnoReferenceTerminal
    #== pcon (PJust $ psemanticDataSummaryV1 # pconstant cardanoTxOutData)

authenticatesMidgardSummary :: forall s. Term s PBool
authenticatesMidgardSummary =
  pmatch (pcardanoTxOutSummaryV1 # pnoReferenceTerminal) $ \case
    PNothing -> pconstant False
    PJust cardano -> pmatch (pmidgardTxOutSummaryV1 # pnoReferenceTerminal) $ \case
      PNothing -> pconstant False
      PJust midgard -> midgard #== psemanticDataSummaryV1 # pconstant midgardTxOutData #&& cardano #/= midgard

acceptsExactDescriptor :: forall s. Term s PBool
acceptsExactDescriptor = plet pnoReferenceTerminal $ \terminal ->
  plet (pexpectJust $ pcardanoTxOutSummaryV1 # terminal) $ \cardano ->
  plet (pexpectJust $ pmidgardTxOutSummaryV1 # terminal) $ \midgard ->
  plet (pexpectJust $ pcardanoSpendDatumSummaryV1 # terminal) $ \spend ->
    pdescriptorIsExactV1 # terminal # pdescriptor terminal midgard spend 0 cardano

rejectsLovelaceSubstitution :: forall s. Term s PBool
rejectsLovelaceSubstitution = plet pnoReferenceTerminal $ \terminal ->
  plet (pexpectJust $ pcardanoTxOutSummaryV1 # terminal) $ \cardano ->
  plet (pexpectJust $ pmidgardTxOutSummaryV1 # terminal) $ \midgard ->
  plet (pexpectJust $ pcardanoSpendDatumSummaryV1 # terminal) $ \spend ->
    pnot # (pdescriptorIsExactV1 # terminal # pdescriptor terminal midgard spend 1 cardano)

rejectsSummarySubstitution :: forall s. Term s PBool
rejectsSummarySubstitution = plet pnoReferenceTerminal $ \terminal ->
  plet (pexpectJust $ pmidgardTxOutSummaryV1 # terminal) $ \midgard ->
  plet (pexpectJust $ pcardanoSpendDatumSummaryV1 # terminal) $ \spend ->
    pnot # (pdescriptorIsExactV1 # terminal # pdescriptor terminal midgard spend 0 midgard)

derivesFirstScriptHashBlock :: forall s. Term s PBool
derivesFirstScriptHashBlock =
  plet (pproofFor 0 smallReferenceOutput) $ \proof ->
  plet preferenceReady $ \referenceReady ->
  plet (padvanced $ preferenceCommitmentStep # referenceReady # pchunks proof) $ \referenceCommitted ->
  plet (padvanced $ preferenceCommitmentStep # referenceCommitted # pnoWitness) $ \hashReady ->
  plet (padvanced $ phashControlStep hashReady (pchunks proof)) $ \hashRound ->
  pmatch referenceCommitted $ \r -> pmatch hashRound $ \h ->
  pmatch (pfromData $ pproof'scriptHash h) $ \case
    PDNothing -> pconstant False
    PDJust hashControl -> pmatch (pfromData hashControl) $ \hc ->
      pfromData (pproof'referenceScriptCount r) #== 1
        #&& preferenceScriptItemCommitmentV1 # referenceCommitted #== pcon PNothing
        #&& pfromData (pproof'stage h) #== pstageScriptHash
        #&& pfromData (Blake.pctl'stage hc) #== Blake.pstageRound
        #&& pfromData (Blake.pctl'activeBlockLength hc) #== 4
        #&& psliceBS # 0 # 4 # pfromData (Blake.pctl'activeBlock hc) #== bytes "036b6b6b"

authenticatesInlineDatumHead :: forall s. Term s PBool
authenticatesInlineDatumHead =
  plet (pproofFor 0 smallDatumOutput) $ \proof ->
  plet pdatumReady $ \datumReady ->
  plet (padvanced $ pdatumControlStepWith pdatumHeadStep datumReady $ pdatumWitness (pcon $ Traverse.PHeadSequence $ pdata 0) (pcon $ PDJust $ pdata proof)) $ \datumOpened ->
    datumOpened #== pdatumOpened

authenticatesInlineDatumFold :: forall s. Term s PBool
authenticatesInlineDatumFold =
  plet (Frame.pinitialSmallConstrFrameV1 # 0 # pconstant "" # 0) $ \frame ->
  plet (padvanced $ pdatumControlStepWith pdatumFoldStep pdatumOpened $ pdatumWitness
      (pcon $ Traverse.PFinalizeFrame (pdata frame) (pdata $ pcon PDNothing)) (pcon PDNothing)) $ \datumAuthenticated ->
    datumAuthenticated #== pdatumAuthenticated

authenticatesInlineDatum :: forall s. Term s PBool
authenticatesInlineDatum =
  plet (padvanced $ pdatumControlStepWith pdatumFoldStep pdatumAuthenticated pnoWitness) $ \terminal ->
  plet (psemanticDataSummaryV1 # pconstant (PD.Constr 0 [PD.Constr 0 []])) $ \expected ->
    pcardanoSpendDatumSummaryV1 # terminal #== pcon (PJust expected)
      #&& pterminalIsExactV1 # terminal

reportsInvalidOutput :: forall s. Term s PBool
reportsInvalidOutput =
  plet (pproofFor 0 malformedNonminimalOutput) $ \proof ->
    pstructureStep # pinitialFor 0 malformedNonminimalOutput # pchunks proof
      #== pcon (PJust $ pcon PLedgerOutputProofInvalidOutput)

rejectsSubstitutedChunk :: forall s. Term s PBool
rejectsSubstitutedChunk =
  plet (pproofFor 0 noReferenceOutput) $ \proof ->
  plet (psubstituteChunk proof malformedNonminimalOutput) $ \substituted ->
    pstructureStep # pinitialFor 0 noReferenceOutput # pchunks substituted #== pcon PNothing

rejectsSubstitutedReferenceChunk :: forall s. Term s PBool
rejectsSubstitutedReferenceChunk =
  plet (pproofFor 0 smallReferenceOutput) $ \proof ->
  plet preferenceReady $ \referenceReady ->
  plet (psubstituteChunk proof $ preplicateBS # (plengthBS # smallReferenceOutput) # (pintegerToByte # 0)) $ \substituted ->
    preferenceCommitmentStep # referenceReady # pchunks substituted #== pcon PNothing

rejectsSubstitutedDatumChunk :: forall s. Term s PBool
rejectsSubstitutedDatumChunk =
  plet (pproofFor 0 smallDatumOutput) $ \proof ->
  plet pdatumReady $ \datumReady ->
  plet (psubstituteChunk proof $ preplicateBS # (plengthBS # smallDatumOutput) # (pintegerToByte # 0)) $ \substituted ->
    pdatumControlStepWith pdatumHeadStep datumReady (pdatumWitness (pcon $ Traverse.PHeadSequence $ pdata 0)
      (pcon $ PDJust $ pdata substituted)) #== pcon PNothing

decodesLongTerminalControl :: forall s. Term s PBool
decodesLongTerminalControl = plet (pdecodeControlV1 # longTerminalControl) $ \control ->
  plet (pexpectJust $ pcardanoSpendDatumSummaryV1 # control) $ \spend ->
  plet (pexpectJust $ preferenceScriptItemCommitmentV1 # control) $ \itemCommitment ->
  plet (pexpectJust $ pcardanoTxOutSummaryV1 # control) $ \cardano ->
  plet (pexpectJust $ pmidgardTxOutSummaryV1 # control) $ \midgard ->
  pmatch control $ \c -> pmatch spend $ \sp -> pmatch cardano $ \ca -> pmatch midgard $ \mi ->
    pterminalIsExactV1 # control
      #&& pfromData (pproof'referenceScriptCount c) #== 2
      #&& plengthBS # pfromData (psummary'root sp) #== 32
      #&& plengthBS # itemCommitment #== 32
      #&& plengthBS # pfromData (psummary'root ca) #== 32
      #&& plengthBS # pfromData (psummary'root mi) #== 32
      #&& pfromData (psummary'root ca) #/= pfromData (psummary'root mi)
      #&& preferenceScriptDigestV1 # control #== pcon (PJust $ bytes "634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098")
      #&& pencodeControlV1 # control #== longTerminalControl

pdescriptor :: forall s. Term s PLedgerOutputProofControlV1 -> Term s PDataSummaryV1 ->
  Term s PDataSummaryV1 -> Term s PInteger -> Term s PDataSummaryV1 -> Term s PLedgerOutputCommitmentV1
pdescriptor control midgard spend lovelaceDelta cardanoField = pmatch control $ \c ->
  pmatch (pfromData $ pproof'outputScan c) $ \scan -> pcon $ PLedgerOutputCommitmentV1
    (pdata pledgerOutputCommitmentVersion) (pproof'outputIndex c) (pproof'totalLength c) (pproof'itemCommitment c)
    (Scan.pscan'address scan) (pdata $ pfromData (Scan.pscan'lovelace scan) + lovelaceDelta) (Scan.pscan'assetCount scan)
    (pdata $ pfrontierCommitment # pfromData (Scan.pscan'assetCount scan) # pfromData (Scan.pscan'assetPeaks scan))
    (Scan.pscan'cardanoValueSize scan) (pdata $ -1) (pdata $ pconstant "") (pdata 0) (pdata $ pconstant "")
    (pdata cardanoField) (pdata midgard) (pdata spend)

pinitialFor :: forall s. Term s PInteger -> Term s PByteString -> Term s PLedgerOutputProofControlV1
pinitialFor index output = pinitialControlV1 # index # (plengthBS # output) # pcommitmentFor index output

pcommitmentFor :: forall s. Term s PInteger -> Term s PByteString -> Term s PByteString
pcommitmentFor index output = pcommitment # poutputFieldIndex # index # (plengthBS # output) # pfrontierFor index output

pfrontierFor :: forall s. Term s PInteger -> Term s PByteString -> Term s (PBuiltinList (PAsData PFrontierPeak))
pfrontierFor index output = pappendLeaf # 0 # pemptyFrontier # (phashChunk # poutputFieldIndex # index # 0 # output)

pproofFor :: forall s. Term s PInteger -> Term s PByteString -> Term s PChunkProofV1
pproofFor index output = pcon $ PChunkProofV1
  (pdata 1) (pdata poutputFieldIndex) (pdata index) (pdata $ plengthBS # output) (pdata 0)
  (pdata output) (pdata $ pfrontierFor index output) (pdata pnil)

psubstituteChunk :: forall s. Term s PChunkProofV1 -> Term s PByteString -> Term s PChunkProofV1
psubstituteChunk proof chunk = pmatch proof $ \p -> pcon $ PChunkProofV1
  (pchunkProof'version p) (pchunkProof'fieldIndex p) (pchunkProof'itemIndex p) (pchunkProof'totalLength p)
  (pchunkProof'chunkIndex p) (pdata chunk) (pchunkProof'frontier p) (pchunkProof'siblings p)

pnoWitness :: forall s. Term s PLedgerOutputProofWitnessV1
pnoWitness = pcon PLedgerOutputProofNoWitness

pchunks :: forall s. Term s PChunkProofV1 -> Term s PLedgerOutputProofWitnessV1
pchunks proof = pcon $ PLedgerOutputProofChunks (pdata proof) (pdata $ pcon PDNothing)

pdatumWitness :: forall s. Term s Traverse.PDataTraverseActionV1 -> Term s (PMaybeData PChunkProofV1) -> Term s PLedgerOutputProofWitnessV1
pdatumWitness action proof = pcon $ PLedgerOutputProofDatum (pdata action) (pdata proof) (pdata $ pcon PDNothing)

pnoReferenceTerminal, preferenceReady, pdatumReady, pdatumOpened, pdatumAuthenticated :: forall s. Term s PLedgerOutputProofControlV1
pnoReferenceTerminal = pproofControl pstageTerminal noReferenceOutput pnoReferenceScan (pcon PDNothing)
preferenceReady = pproofControl pstageReferenceScriptCommitment smallReferenceOutput preferenceScan (pcon PDNothing)
pdatumReady = pproofControl pstageDatumTraversal smallDatumOutput pdatumScan
  (pcon $ PDJust $ pdata $ Traverse.pinitialControlV1 # 39 # 3)
pdatumOpened = pproofControl pstageDatumTraversal smallDatumOutput pdatumScan
  (pcon $ PDJust $ pdata pdatumOpenedTraverse)
pdatumAuthenticated = pproofControl pstageDatumTraversal smallDatumOutput pdatumScan
  (pcon $ PDJust $ pdata pdatumTerminalTraverse)

pdatumOpenedTraverse, pdatumTerminalTraverse :: forall s. Term s Traverse.PDataTraverseControlV1
pdatumOpenedTraverse =
  plet (Frame.pinitialSmallConstrFrameV1 # 0 # pconstant "" # 0) $ \frame ->
    pcon $ Traverse.PDataTraverseControlV1
      (pdata Traverse.pversion) (pdata Traverse.pstageFold) (pdata 39) (pdata 3) (pdata 3)
      (pdata $ Frame.phashFrameV1 # frame) (pdata $ pcon PDNothing) (pdata $ pcon PDNothing)
      (pdata $ pcon PDNothing) (pdata $ pcon PDNothing)

pdatumTerminalTraverse = pcon $ Traverse.PDataTraverseControlV1
  (pdata Traverse.pversion) (pdata Traverse.pstageTerminal) (pdata 39) (pdata 3) (pdata 3)
  (pdata $ pconstant "") (pdata $ pcon PDNothing) (pdata $ pcon PDNothing) (pdata $ pcon PDNothing)
  (pdata $ pcon $ PDJust $ pdata $ CekData.psmallConstrDataSummaryV1 # 0 # CekData.pemptyDataListSummaryV1)

pproofControl :: forall s. Term s PInteger -> Term s PByteString -> Term s Scan.PLedgerOutputScanControlV1 ->
  Term s (PMaybeData Traverse.PDataTraverseControlV1) -> Term s PLedgerOutputProofControlV1
pproofControl stage output scan datumControl = pcon $ PLedgerOutputProofControlV1
  (pdata 1) (pdata stage) (pdata 0) (pdata $ plengthBS # output) (pdata $ pcommitmentFor 0 output)
  (pdata scan) (pdata $ pcon $ PDJust $ pdata pemptyValueTerminal) (pdata datumControl)
  (pdata 0) (pdata pemptyFrontier) (pdata $ pcon PDNothing) (pdata $ pcon PDNothing)

pemptyValueTerminal :: forall s. Term s Value.PLedgerOutputValueControlV1
pemptyValueTerminal = pcon $ Value.PLedgerOutputValueControlV1
  (pdata Value.pversion) (pdata Value.pstageTerminal) (pdata 0) (pdata $ pconstant "")
  (pdata pemptyDataPairSummaryV1) (pdata pemptyDataPairSummaryV1)
  (pdata $ pcon $ PDJust $ pdata $ pmapDataSummaryV1 # pemptyDataPairSummaryV1)

pnoReferenceScan, preferenceScan, pdatumScan :: forall s. Term s Scan.PLedgerOutputScanControlV1
pnoReferenceScan = pscanTerminal 37 2 0 (-1) 0 (-1) (-1) (-1) 0
preferenceScan = pscanTerminal 44 3 1 (-1) 0 3 38 41 3
pdatumScan = pscanTerminal 42 3 1 39 3 (-1) (-1) (-1) 0

pscanTerminal :: forall s. Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s Scan.PLedgerOutputScanControlV1
pscanTerminal cursor mapCount optionalCount datumOffset datumLength language itemOffset scriptOffset scriptLength =
  pcon $ Scan.PLedgerOutputScanControlV1
    (pdata Scan.pversion) (pdata Scan.pstageTerminal) (pdata cursor) (pdata mapCount) (pdata optionalCount)
    (pdata $ bytes "7811111111111111111111111111111111111111111111111111111111")
    (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
    (pdata $ pconstant "") (pdata $ pconstant "") (pdata $ pconstant "")
    (pdata 0) (pdata pemptyFrontier) (pdata datumOffset) (pdata datumLength) (pdata 0)
    (pdata language) (pdata itemOffset) (pdata scriptOffset) (pdata scriptLength)

padvanced :: forall s. Term s (PMaybe PLedgerOutputProofStepResultV1) -> Term s PLedgerOutputProofControlV1
padvanced result = pmatch result $ \case
  PJust stepResult -> pmatch stepResult $ \case
    PLedgerOutputProofAdvanced control -> pfromData control
    _ -> perror
  PNothing -> perror

pdatumControlStepWith :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> Traverse.PDataTraverseControlV1 :--> PMaybe PLedgerOutputProofStepResultV1) ->
  Term s PLedgerOutputProofControlV1 -> Term s PLedgerOutputProofWitnessV1 -> Term s (PMaybe PLedgerOutputProofStepResultV1)
pdatumControlStepWith step control witness = pmatch control $ \c -> pmatch (pfromData $ pproof'datum c) $ \case
  PDNothing -> pcon PNothing
  PDJust datumControl -> step # control # witness # pfromData datumControl

phashControlStep :: forall s. Term s PLedgerOutputProofControlV1 -> Term s PLedgerOutputProofWitnessV1 -> Term s (PMaybe PLedgerOutputProofStepResultV1)
phashControlStep control witness = pmatch control $ \c -> pmatch (pfromData $ pproof'scriptHash c) $ \case
  PDNothing -> pcon PNothing
  PDJust hashControl -> phashStep # control # witness # pfromData hashControl

pexpectJust :: forall s a. Term s (PMaybe a) -> Term s a
pexpectJust value = pmatch value $ \case PNothing -> perror; PJust result -> result

cardanoTxOutData, midgardTxOutData :: PD.Data
cardanoTxOutData = txOutData 0
midgardTxOutData = txOutData 1

txOutData :: Integer -> PD.Data
txOutData addressConstructor = PD.Constr 0
  [ PD.Constr addressConstructor [PD.Constr 1 [PD.B $ BS.replicate 28 0x11], PD.Constr 1 []]
  , PD.Map []
  , PD.Constr 0 []
  , PD.Constr 1 []
  ]

noReferenceOutput, smallReferenceOutput, smallDatumOutput, malformedNonminimalOutput, longTerminalControl :: forall s. Term s PByteString
noReferenceOutput = bytes "a200581d7811111111111111111111111111111111111111111111111111111111018200a0"
smallReferenceOutput = bytes "a300581d7811111111111111111111111111111111111111111111111111111111018200a0038203436b6b6b"
smallDatumOutput = bytes "a300581d7811111111111111111111111111111111111111111111111111111111018200a00243d87980"
malformedNonminimalOutput = bytes "b80200581d7811111111111111111111111111111111111111111111111111111111018200a0"
longTerminalControl = bytes "8c010600192bf15820a023c9459077b4fc906660cacfa81a46eea15b9ad1f21fb20fbd745d2678f9ec970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820fdd05992e96e478560b718d45058402827072f35e5220f396e2569800a2b76fe1854191427000319147c191481191770d8799f8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f835820f1f2c712b57f2a363ba09d8673a2cf47b8688568395388f1baf86ce7f477c23618301852ffffd8799f8a0107185419142719142740d87a80d87a80d87a80d8799f8358202aa2efa1446b0d53ad8a806d29396c82aca037037c095811d7948f5304d3896219142719138cffff02818201582028f935b37d798dd5f68f23fa40e9d9dd02037d6b1e1fa7ad7edfdcb84b63a26cd8799f8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000ffd87a80"

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
