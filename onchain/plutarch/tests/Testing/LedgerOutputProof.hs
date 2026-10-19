{-# LANGUAGE OverloadedStrings #-}

module Testing.LedgerOutputProof (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Builtin.Crypto (pblake2b_256)
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
import Midgard.CekDataInteger qualified as Integer
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.CekSourceBlob qualified as Blob
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pledgerOutputCommitmentVersion)
import Midgard.LedgerOutputCommitment qualified as Commitment
import Midgard.LedgerOutputProof
import Midgard.LedgerOutputProofDatum qualified as Datum
import Midgard.LedgerOutputProofDescriptor qualified as Descriptor
import Midgard.LedgerOutputProofRaw qualified as Raw
import Midgard.LedgerOutputProofStages qualified as Stages
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerOutputValue qualified as Value
import Midgard.ValidationMerkle (PFrontierPeak, pappendLeaf, pemptyFrontier, pfrontierCommitment)
import Testing.Eval (passertEvalNoTraceWithoutHoistChecks, pfailsNoTraceWithoutHoistChecks)

tests :: TestTree
tests =
    testGroup
        "Midgard.LedgerOutputProof"
        [ narrowDatumTests
        , narrowStageTests
        , narrowDescriptorTests
        , rawFrameTests
        , windowAndFactTests
        , testGroup
            "authenticates_a_complete_output_without_a_reference_script"
            [ testCase "terminal control" $ passertEvalNoTraceWithoutHoistChecks authenticatesCompleteOutput
            , testCase "value summary" $ passertEvalNoTraceWithoutHoistChecks authenticatesValueSummary
            , testCase "cardano summary" $ passertEvalNoTraceWithoutHoistChecks authenticatesCardanoSummary
            , testCase "midgard summary" $ passertEvalNoTraceWithoutHoistChecks authenticatesMidgardSummary
            , testCase "exact descriptor" $ passertEvalNoTraceWithoutHoistChecks acceptsExactDescriptor
            , testCase "lovelace substitution" $ passertEvalNoTraceWithoutHoistChecks rejectsLovelaceSubstitution
            , testCase "summary substitution" $ passertEvalNoTraceWithoutHoistChecks rejectsSummarySubstitution
            ]
        , testCase "derives_the_first_script_hash_block_from_the_output_chunk" $ passertEvalNoTraceWithoutHoistChecks derivesFirstScriptHashBlock
        , testGroup
            "authenticates_the_inline_datum_semantics_from_output_chunks"
            [ testCase "opens the authenticated datum" $ passertEvalNoTraceWithoutHoistChecks authenticatesInlineDatumHead
            , testCase "finalizes the authenticated datum" $ passertEvalNoTraceWithoutHoistChecks authenticatesInlineDatumFold
            , testCase "publishes the spend datum summary" $ passertEvalNoTraceWithoutHoistChecks authenticatesInlineDatum
            ]
        , testCase "reports_authenticated_invalid_output_bytes" $ passertEvalNoTraceWithoutHoistChecks reportsInvalidOutput
        , testCase "fails_closed_for_a_substituted_chunk" $ passertEvalNoTraceWithoutHoistChecks rejectsSubstitutedChunk
        , testCase "fails_closed_for_a_substituted_reference_script_item_chunk" $ passertEvalNoTraceWithoutHoistChecks rejectsSubstitutedReferenceChunk
        , testCase "fails_closed_for_a_substituted_inline_datum_item_chunk" $ passertEvalNoTraceWithoutHoistChecks rejectsSubstitutedDatumChunk
        , testCase "decodes_the_long_typescript_terminal_control_canonically" $ passertEvalNoTraceWithoutHoistChecks decodesLongTerminalControl
        ]

authenticatesCompleteOutput :: forall s. Term s PBool
authenticatesCompleteOutput =
    plet pnoReferenceTerminal $ \terminal ->
        pmatch terminal $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \scan ->
            pcontrolIsWellFormed
                # terminal
                #&& pterminalIsExactV1
                # terminal
                #&& pfromData (Scan.pscan'address scan)
                #== bytes "7811111111111111111111111111111111111111111111111111111111"
                #&& pfromData (Scan.pscan'lovelace scan)
                #== 0
                #&& pfromData (pproof'scriptHash c)
                #== pcon PDNothing

authenticatesValueSummary :: forall s. Term s PBool
authenticatesValueSummary =
    pvalueSummaryV1 # pnoReferenceTerminal #== pcon (PJust $ psemanticDataSummaryV1 # pconstant (PD.Map []))

authenticatesCardanoSummary :: forall s. Term s PBool
authenticatesCardanoSummary =
    pcardanoTxOutSummaryV1
        # pnoReferenceTerminal
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
        plet (pattach preferenceReady smallReferenceOutput proof) $ \referenceReady ->
            plet (padvanced $ preferenceCommitmentStep # referenceReady # pcon (PLedgerOutputProofWindow $ pdata smallReferenceOutput)) $ \referenceCommitted ->
                plet (padvanced $ preferenceCommitmentStep # referenceCommitted # pnoWitness) $ \hashReady ->
                    plet (padvanced $ phashControlStep hashReady (pcon $ PLedgerOutputProofWindow $ pdata smallReferenceOutput)) $ \hashRound ->
                        pmatch referenceCommitted $ \r -> pmatch hashRound $ \h ->
                            pmatch (pfromData $ pproof'scriptHash h) $ \case
                                PDNothing -> pconstant False
                                PDJust hashControl -> pmatch (pfromData hashControl) $ \hc ->
                                    pfromData (pproof'referenceScriptCount r)
                                        #== 1
                                        #&& preferenceScriptItemCommitmentV1
                                        # referenceCommitted
                                        #== pcon PNothing
                                        #&& pfromData (pproof'stage h)
                                        #== pstageScriptHash
                                        #&& pfromData (Blake.pctl'stage hc)
                                        #== Blake.pstageRound
                                        #&& pfromData (Blake.pctl'activeBlockLength hc)
                                        #== 4
                                        #&& psliceBS
                                        # 0
                                        # 4
                                        # pfromData (Blake.pctl'activeBlock hc)
                                        #== bytes "036b6b6b"

authenticatesInlineDatumHead :: forall s. Term s PBool
authenticatesInlineDatumHead =
    plet (pproofFor 0 smallDatumOutput) $ \proof ->
        plet (pattach pdatumReady smallDatumOutput proof) $ \datumReady ->
            plet (padvanced $ pdatumControlStepWith pdatumHeadStep datumReady $ pdatumWitness (pcon $ Traverse.PHeadSequence $ pdata 0) (pcon $ PDJust $ pdata smallDatumOutput)) $ \datumOpened ->
                datumOpened #== pdatumOpened

authenticatesInlineDatumFold :: forall s. Term s PBool
authenticatesInlineDatumFold =
    plet (Frame.pinitialSmallConstrFrameV1 # 0 # pconstant "" # 0) $ \frame ->
        plet
            ( padvanced $
                pdatumControlStepWith pdatumFoldStep pdatumOpened $
                    pdatumWitness
                        (pcon $ Traverse.PFinalizeFrame (pdata frame) (pdata $ pcon PDNothing))
                        (pcon PDNothing)
            )
            $ \datumAuthenticated ->
                datumAuthenticated #== pdatumAuthenticated

authenticatesInlineDatum :: forall s. Term s PBool
authenticatesInlineDatum =
    plet (padvanced $ pdatumControlStepWith pdatumFoldStep pdatumAuthenticated pnoWitness) $ \terminal ->
        plet (psemanticDataSummaryV1 # pconstant (PD.Constr 0 [PD.Constr 0 []])) $ \expected ->
            pcardanoSpendDatumSummaryV1
                # terminal
                #== pcon (PJust expected)
                #&& pterminalIsExactV1
                # terminal

reportsInvalidOutput :: forall s. Term s PBool
reportsInvalidOutput =
    plet (pproofFor 0 malformedNonminimalOutput) $ \proof ->
        pstructureStep
            # pinitialFor 0 malformedNonminimalOutput
            # pchunks proof
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
                pstepV1 # referenceReady # pspanWitness smallReferenceOutput substituted #== pcon PNothing

rejectsSubstitutedDatumChunk :: forall s. Term s PBool
rejectsSubstitutedDatumChunk =
    plet (pproofFor 0 smallDatumOutput) $ \proof ->
        plet pdatumReady $ \datumReady ->
            plet (psubstituteChunk proof $ preplicateBS # (plengthBS # smallDatumOutput) # (pintegerToByte # 0)) $ \substituted ->
                pstepV1 # datumReady # pspanWitness smallDatumOutput substituted #== pcon PNothing

decodesLongTerminalControl :: forall s. Term s PBool
decodesLongTerminalControl = plet (pdecodeControlV1 # longTerminalControl) $ \control ->
    plet (pexpectJust $ pcardanoSpendDatumSummaryV1 # control) $ \spend ->
        plet (pexpectJust $ preferenceScriptItemCommitmentV1 # control) $ \itemCommitment ->
            plet (pexpectJust $ pcardanoTxOutSummaryV1 # control) $ \cardano ->
                plet (pexpectJust $ pmidgardTxOutSummaryV1 # control) $ \midgard ->
                    pmatch control $ \c -> pmatch spend $ \sp -> pmatch cardano $ \ca -> pmatch midgard $ \mi ->
                        pterminalIsExactV1
                            # control
                            #&& pfromData (pproof'referenceScriptCount c)
                            #== 2
                            #&& plengthBS
                            # pfromData (psummary'root sp)
                            #== 32
                            #&& plengthBS
                            # itemCommitment
                            #== 32
                            #&& plengthBS
                            # pfromData (psummary'root ca)
                            #== 32
                            #&& plengthBS
                            # pfromData (psummary'root mi)
                            #== 32
                            #&& pfromData (psummary'root ca)
                            #/= pfromData (psummary'root mi)
                            #&& preferenceScriptDigestV1
                            # control
                            #== pcon (PJust $ bytes "634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098")
                            #&& pencodeControlV1
                            # control
                            #== longTerminalControl

pdescriptor ::
    forall s.
    Term s PLedgerOutputProofControlV1 ->
    Term s PDataSummaryV1 ->
    Term s PDataSummaryV1 ->
    Term s PInteger ->
    Term s PDataSummaryV1 ->
    Term s PLedgerOutputCommitmentV1
pdescriptor control midgard spend lovelaceDelta cardanoField = pmatch control $ \c ->
    pmatch (pfromData $ pproof'outputScan c) $ \scan ->
        pcon $
            PLedgerOutputCommitmentV1
                (pdata pledgerOutputCommitmentVersion)
                (pproof'outputIndex c)
                (pproof'totalLength c)
                (pproof'itemCommitment c)
                (Scan.pscan'address scan)
                (pdata $ pfromData (Scan.pscan'lovelace scan) + lovelaceDelta)
                (Scan.pscan'assetCount scan)
                (pdata $ pfrontierCommitment # pfromData (Scan.pscan'assetCount scan) # pfromData (Scan.pscan'assetPeaks scan))
                (Scan.pscan'cardanoValueSize scan)
                (pdata $ -1)
                (pdata $ pconstant "")
                (pdata 0)
                (pdata $ pconstant "")
                (pdata cardanoField)
                (pdata midgard)
                (pdata spend)

pinitialFor :: forall s. Term s PInteger -> Term s PByteString -> Term s PLedgerOutputProofControlV1
pinitialFor index output = pinitialControlV1 # index # (plengthBS # output) # pcommitmentFor index output

pcommitmentFor :: forall s. Term s PInteger -> Term s PByteString -> Term s PByteString
pcommitmentFor index output = pcommitment # poutputFieldIndex # index # (plengthBS # output) # pfrontierFor index output

pfrontierFor :: forall s. Term s PInteger -> Term s PByteString -> Term s (PBuiltinList (PAsData PFrontierPeak))
pfrontierFor index output = pappendLeaf # 0 # pemptyFrontier # (phashChunk # poutputFieldIndex # index # 0 # output)

pproofFor :: forall s. Term s PInteger -> Term s PByteString -> Term s PChunkProofV1
pproofFor index output =
    pcon $
        PChunkProofV1
            (pdata 1)
            (pdata poutputFieldIndex)
            (pdata index)
            (pdata $ plengthBS # output)
            (pdata 0)
            (pdata output)
            (pdata $ pfrontierFor index output)
            (pdata pnil)

psubstituteChunk :: forall s. Term s PChunkProofV1 -> Term s PByteString -> Term s PChunkProofV1
psubstituteChunk proof chunk = pmatch proof $ \p ->
    pcon $
        PChunkProofV1
            (pchunkProof'version p)
            (pchunkProof'fieldIndex p)
            (pchunkProof'itemIndex p)
            (pchunkProof'totalLength p)
            (pchunkProof'chunkIndex p)
            (pdata chunk)
            (pchunkProof'frontier p)
            (pchunkProof'siblings p)

pnoWitness :: forall s. Term s PLedgerOutputProofWitnessV1
pnoWitness = pcon PLedgerOutputProofNoWitness

pchunks :: forall s. Term s PChunkProofV1 -> Term s PLedgerOutputProofWitnessV1
pchunks proof = pcon $ PLedgerOutputProofChunks (pdata proof) (pdata $ pcon PDNothing)

pdatumWitness :: forall s. Term s Traverse.PDataTraverseActionV1 -> Term s (PMaybeData PByteString) -> Term s PLedgerOutputProofWitnessV1
pdatumWitness action window = pcon $ PLedgerOutputProofDatum (pdata action) (pdata window)

pnoReferenceTerminal, preferenceReady, pdatumReady, pdatumOpened, pdatumAuthenticated :: forall s. Term s PLedgerOutputProofControlV1
pnoReferenceTerminal = pproofControl pstageTerminal noReferenceOutput pnoReferenceScan (pcon PDNothing)
preferenceReady = pproofControl pstageReferenceScriptCommitment smallReferenceOutput preferenceScan (pcon PDNothing)
pdatumReady =
    pproofControl
        pstageDatumTraversal
        smallDatumOutput
        pdatumScan
        (pcon $ PDJust $ pdata $ Traverse.pinitialControlV1 # 39 # 3)
pdatumOpened =
    pwithDatumWindow $
        pproofControl
            pstageDatumTraversal
            smallDatumOutput
            pdatumScan
            (pcon $ PDJust $ pdata pdatumOpenedTraverse)
pdatumAuthenticated =
    pwithDatumWindow $
        pproofControl
            pstageDatumTraversal
            smallDatumOutput
            pdatumScan
            (pcon $ PDJust $ pdata pdatumTerminalTraverse)

pdatumOpenedTraverse, pdatumTerminalTraverse :: forall s. Term s Traverse.PDataTraverseControlV1
pdatumOpenedTraverse =
    plet (Frame.pinitialSmallConstrFrameV1 # 0 # pconstant "" # 0) $ \frame ->
        pcon $
            Traverse.PDataTraverseControlV1
                (pdata Traverse.pversion)
                (pdata Traverse.pstageFold)
                (pdata 39)
                (pdata 3)
                (pdata 3)
                (pdata $ Frame.phashFrameV1 # frame)
                (pdata $ pcon PDNothing)
                (pdata $ pcon PDNothing)
                (pdata $ pcon PDNothing)
                (pdata $ pcon PDNothing)
pdatumTerminalTraverse =
    pcon $
        Traverse.PDataTraverseControlV1
            (pdata Traverse.pversion)
            (pdata Traverse.pstageTerminal)
            (pdata 39)
            (pdata 3)
            (pdata 3)
            (pdata $ pconstant "")
            (pdata $ pcon PDNothing)
            (pdata $ pcon PDNothing)
            (pdata $ pcon PDNothing)
            (pdata $ pcon $ PDJust $ pdata $ CekData.psmallConstrDataSummaryV1 # 0 # CekData.pemptyDataListSummaryV1)

pproofControl ::
    forall s.
    Term s PInteger ->
    Term s PByteString ->
    Term s Scan.PLedgerOutputScanControlV1 ->
    Term s (PMaybeData Traverse.PDataTraverseControlV1) ->
    Term s PLedgerOutputProofControlV1
pproofControl stage output scan datumControl =
    pcon $
        PLedgerOutputProofControlV1
            (pdata 1)
            (pdata stage)
            (pdata 0)
            (pdata $ plengthBS # output)
            (pdata $ pcommitmentFor 0 output)
            (pdata scan)
            (pdata $ pcon $ PDJust $ pdata pemptyValueTerminal)
            (pdata datumControl)
            (pdata 0)
            (pdata pemptyFrontier)
            (pdata $ pcon PDNothing)
            (pdata $ pcon PDNothing)
            (pdata $ pcon PDNothing)
            (pdata $ pcon PDNothing)
            (pdata $ pcon PDNothing)
            (pdata $ pcon PDNothing)
            (pdata $ pcon PDNothing)

pemptyValueTerminal :: forall s. Term s Value.PLedgerOutputValueControlV1
pemptyValueTerminal =
    pcon $
        Value.PLedgerOutputValueControlV1
            (pdata Value.pversion)
            (pdata Value.pstageTerminal)
            (pdata 0)
            (pdata $ pconstant "")
            (pdata pemptyDataPairSummaryV1)
            (pdata pemptyDataPairSummaryV1)
            (pdata $ pcon $ PDJust $ pdata $ pmapDataSummaryV1 # pemptyDataPairSummaryV1)

pnoReferenceScan, preferenceScan, pdatumScan :: forall s. Term s Scan.PLedgerOutputScanControlV1
pnoReferenceScan = pscanTerminal 37 2 0 (-1) 0 (-1) (-1) (-1) 0
preferenceScan = pscanTerminal 44 3 1 (-1) 0 3 38 41 3
pdatumScan = pscanTerminal 42 3 1 39 3 (-1) (-1) (-1) 0

pscanTerminal ::
    forall s.
    Term s PInteger ->
    Term s PInteger ->
    Term s PInteger ->
    Term s PInteger ->
    Term s PInteger ->
    Term s PInteger ->
    Term s PInteger ->
    Term s PInteger ->
    Term s PInteger ->
    Term s Scan.PLedgerOutputScanControlV1
pscanTerminal cursor mapCount optionalCount datumOffset datumLength language itemOffset scriptOffset scriptLength =
    pcon $
        Scan.PLedgerOutputScanControlV1
            (pdata Scan.pversion)
            (pdata Scan.pstageTerminal)
            (pdata cursor)
            (pdata mapCount)
            (pdata optionalCount)
            (pdata $ bytes "7811111111111111111111111111111111111111111111111111111111")
            (pdata 0)
            (pdata 1)
            (pdata 0)
            (pdata 0)
            (pdata 0)
            (pdata $ pconstant "")
            (pdata $ pconstant "")
            (pdata $ pconstant "")
            (pdata 0)
            (pdata pemptyFrontier)
            (pdata datumOffset)
            (pdata datumLength)
            (pdata 0)
            (pdata language)
            (pdata itemOffset)
            (pdata scriptOffset)
            (pdata scriptLength)

padvanced :: forall s. Term s (PMaybe PLedgerOutputProofStepResultV1) -> Term s PLedgerOutputProofControlV1
padvanced result = pmatch result $ \case
    PJust stepResult -> pmatch stepResult $ \case
        PLedgerOutputProofAdvanced control -> pfromData control
        _ -> perror
    PNothing -> perror

pdatumControlStepWith ::
    forall s.
    Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> Traverse.PDataTraverseControlV1 :--> PMaybe PLedgerOutputProofStepResultV1) ->
    Term s PLedgerOutputProofControlV1 ->
    Term s PLedgerOutputProofWitnessV1 ->
    Term s (PMaybe PLedgerOutputProofStepResultV1)
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
txOutData addressConstructor =
    PD.Constr
        0
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
longTerminalControl = bytes "91010600192bf15820a023c9459077b4fc906660cacfa81a46eea15b9ad1f21fb20fbd745d2678f9ec970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820fdd05992e96e478560b718d45058402827072f35e5220f396e2569800a2b76fe1854191427000319147c191481191770d8799f8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f8358201779e0023b9273af30dcfadb5e5ec557763a8c2c3af34f57fb2a2ad59f50ada018301852ffffd8799f8a0107185419142719142740d87a80d87a80d87a80d8799f8358202aa2efa1446b0d53ad8a806d29396c82aca037037c095811d7948f5304d3896219142719138cffff02818201582028f935b37d798dd5f68f23fa40e9d9dd02037d6b1e1fa7ad7edfdcb84b63a26cd8799f8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000ffd87a80d87a80d87a80d87a80d87a80d87a80"

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient

pspanWitness :: forall s. Term s PByteString -> Term s PChunkProofV1 -> Term s PLedgerOutputProofWitnessV1
pspanWitness output proof = pcon $ PLedgerOutputProofSpanAttach (pdata 0) (pdata $ plengthBS # output) (pdata proof) (pdata $ pcon PDNothing)

pattach :: forall s. Term s PLedgerOutputProofControlV1 -> Term s PByteString -> Term s PChunkProofV1 -> Term s PLedgerOutputProofControlV1
pattach control output proof = pmatch control $ \c ->
    plet (pexpectJust $ pauthenticatedOutputSpanForOutput (pfromData $ pproof'outputIndex c) (pfromData $ pproof'totalLength c) (pfromData $ pproof'itemCommitment c) 0 (plengthBS # output) (pchunks proof)) $ \authenticated ->
        pcon c{pproof'spanWindow = pdata $ pcon $ PDJust $ pdata $ pcon $ PLedgerOutputSpanWindowV1 (pdata 0) (pdata $ plengthBS # output) (pdata $ pblake2b_256 # authenticated)}

pwithDatumWindow :: forall s. Term s PLedgerOutputProofControlV1 -> Term s PLedgerOutputProofControlV1
pwithDatumWindow control = pmatch control $ \c -> pcon c{pproof'spanWindow = pdata $ pcon $ PDJust $ pdata $ pcon $ PLedgerOutputSpanWindowV1 (pdata 0) (pdata $ plengthBS # smallDatumOutput) (pdata $ pblake2b_256 # smallDatumOutput)}

windowAndFactTests :: TestTree
windowAndFactTests =
    testGroup
        "span windows and descriptor facts"
        [ testCase "initial control uses target 17-field encoding" $
            passertEvalNoTraceWithoutHoistChecks $
                pencodeInitialControlV1
                    # 0
                    # (plengthBS # noReferenceOutput)
                    # pcommitmentFor 0 noReferenceOutput
                    #== pencodeControlV1
                    # pinitialFor 0 noReferenceOutput
        , testCase "initial wire starts with a definite 17-item array" $
            passertEvalNoTraceWithoutHoistChecks $
                psliceBS # 0 # 1 # (pencodeControlV1 # pinitialFor 0 noReferenceOutput) #== bytes "91"
        , testCase "span attachment is forbidden during structure" $
            passertEvalNoTraceWithoutHoistChecks $
                pstepV1 # pinitialFor 0 noReferenceOutput # pspanWitness noReferenceOutput (pproofFor 0 noReferenceOutput) #== pcon PNothing
        , testCase "span attachment is forbidden at terminal" $
            passertEvalNoTraceWithoutHoistChecks $
                pstepV1 # pnoReferenceTerminal # pspanWitness noReferenceOutput (pproofFor 0 noReferenceOutput) #== pcon PNothing
        , testCase "span attachment binds output identity" $
            passertEvalNoTraceWithoutHoistChecks $
                pstepV1 # pdatumReady # pspanWitness smallDatumOutput (pproofFor 1 smallDatumOutput) #== pcon PNothing
        , testCase "attached span survives canonical control roundtrip" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (pattach pdatumReady smallDatumOutput (pproofFor 0 smallDatumOutput)) $
                    \c -> pdecodeControlV1 # (pencodeControlV1 # c) #== c
        , testCase "datum window requires an earlier attachment" $
            passertEvalNoTraceWithoutHoistChecks $
                pdatumControlStepWith pdatumHeadStep pdatumReady (pdatumWitness (pcon $ Traverse.PHeadSequence $ pdata 0) (pcon $ PDJust $ pdata smallDatumOutput)) #== pcon PNothing
        , testCase "reference window requires an earlier attachment" $
            passertEvalNoTraceWithoutHoistChecks $
                preferenceCommitmentStep # preferenceReady # pcon (PLedgerOutputProofWindow $ pdata smallReferenceOutput) #== pcon PNothing
        , testCase "substituted window bytes fail at consumption" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (pattach preferenceReady smallReferenceOutput (pproofFor 0 smallReferenceOutput)) $ \c ->
                    preferenceCommitmentStep # c # pcon (PLedgerOutputProofWindow $ pdata $ preplicateBS # (plengthBS # smallReferenceOutput) # (pintegerToByte # 0)) #== pcon PNothing
        , testCase "window slices the authenticated range" $
            passertEvalNoTraceWithoutHoistChecks $
                pboundWindowBytesV1 # ptestWindow # 11 # 2 # pconstant "abcd" #== pcon (PJust $ pconstant "bc")
        , testCase "window rejects an out-of-range request" $
            passertEvalNoTraceWithoutHoistChecks $
                pboundWindowBytesV1 # ptestWindow # 9 # 2 # pconstant "abcd" #== pcon PNothing
        , testCase "window rejects an empty request" $
            passertEvalNoTraceWithoutHoistChecks $
                pboundWindowBytesV1 # ptestWindow # 10 # 0 # pconstant "abcd" #== pcon PNothing
        , testCase "window rejects a digest substitution" $
            passertEvalNoTraceWithoutHoistChecks $
                pboundWindowBytesV1 # ptestWindow # 11 # 2 # pconstant "abce" #== pcon PNothing
        , testCase "facts attach leaves then scan then reference" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (pexpectJust $ pfactAttachV1 # pnoReferenceTerminal # pconstant "descriptor") $ \leaves ->
                    plet (pexpectJust $ pfactAttachV1 # leaves # pconstant "descriptor") $ \scan ->
                        plet (pexpectJust $ pfactAttachV1 # scan # pconstant "descriptor") $ \complete ->
                            pfactV1
                                # leaves
                                # 2
                                #/= pcon PDNothing
                                #&& pfactV1
                                # leaves
                                # 3
                                #/= pcon PDNothing
                                #&& pfactV1
                                # leaves
                                # 0
                                #== pcon PDNothing
                                #&& pfactV1
                                # leaves
                                # 1
                                #== pcon PDNothing
                                #&& pfactV1
                                # scan
                                # 0
                                #/= pcon PDNothing
                                #&& pfactV1
                                # scan
                                # 1
                                #== pcon PDNothing
                                #&& pfactsCompleteV1
                                # complete
                                #&& pfactAttachV1
                                # complete
                                # pconstant "descriptor"
                                #== pcon PNothing
        , testCase "terminal facts bind one exact descriptor and summary triple" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (pexpectJust $ pterminalClaimedSummariesV1 # pnoReferenceTerminal) $ \pair -> pmatch pair $ \(PPair value datum) ->
                    plet pcompleteFacts $ \control ->
                        pfactsAreExactV1
                            # control
                            # pconstant "descriptor"
                            # value
                            # datum
                            #&& pnot
                            # (pfactsAreExactV1 # control # pconstant "other" # value # datum)
                            #&& pnot
                            # (pfactsAreExactV1 # control # pconstant "descriptor" # pconstant (PD.I 0) # datum)
                            #&& pnot
                            # (pfactsAreExactV1 # control # pconstant "descriptor" # value # pconstant (PD.I 0))
        , testCase "fact commitment uses target Data-list encoding" $
            passertEvalNoTraceWithoutHoistChecks $
                pfactCommitmentV1
                    # 0
                    # pconstant "d"
                    # pconstant (PD.I 7)
                    # pconstant (PD.Constr 1 [])
                    #== pblake2b_256
                    # (pserialiseData # pconstant (PD.List [PD.B "d", PD.I 7, PD.Constr 1 []]))
        , testCase "fact commitments are forbidden before terminal" $
            passertEvalNoTraceWithoutHoistChecks $
                pnot # (pcontrolIsWellFormed # (pwithFactV1 # pdatumReady # 0 # (pblake2b_256 # pconstant "d")))
        , testCase "fact commitments require 32 bytes" $
            passertEvalNoTraceWithoutHoistChecks $
                pnot # (pcontrolIsWellFormed # (pwithFactV1 # pnoReferenceTerminal # 0 # pconstant "short"))
        , testCase "fact attachment refuses an unfinished traversal" $
            passertEvalNoTraceWithoutHoistChecks $
                pfactAttachV1 # pdatumReady # pconstant "descriptor" #== pcon PNothing
        , testCase "fact-bearing control roundtrips exactly" $
            passertEvalNoTraceWithoutHoistChecks $
                pdecodeControlV1 # (pencodeControlV1 # pcompleteFacts) #== pcompleteFacts
        , testCase "partial leaf group is refused" $
            pfailsNoTraceWithoutHoistChecks $
                pfactAttachV1 # (pwithFactV1 # pnoReferenceTerminal # 2 # (pblake2b_256 # pconstant "d")) # pconstant "descriptor"
        ]

ptestWindow :: forall s. Term s (PMaybeData PLedgerOutputSpanWindowV1)
ptestWindow = pcon $ PDJust $ pdata $ pcon $ PLedgerOutputSpanWindowV1 (pdata 10) (pdata 4) (pdata $ pblake2b_256 # pconstant "abcd")

pcompleteFacts :: forall s. Term s PLedgerOutputProofControlV1
pcompleteFacts = plet (pexpectJust $ pfactAttachV1 # pnoReferenceTerminal # pconstant "descriptor") $ \first ->
    plet (pexpectJust $ pfactAttachV1 # first # pconstant "descriptor") $ \second ->
        pexpectJust $ pfactAttachV1 # second # pconstant "descriptor"

rawFrameTests :: TestTree
rawFrameTests =
    testGroup
        "raw output-proof frame"
        [ testCase "roundtrips the exact target terminal golden" $
            passertEvalNoTraceWithoutHoistChecks $
                Raw.pencode # (Raw.popen # longTerminalControl) #== longTerminalControl
        , testCase "reads the target initial output identity" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (Raw.popen # (pencodeInitialControlV1 # 65535 # 16384 # (pblake2b_256 # pconstant "item"))) $ \frame ->
                    Raw.pinteger # frame # 2 #== 65535 #&& Raw.pinteger # frame # 3 #== 16384
        , testCase "replaces exactly the requested raw field" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (Raw.popen # longTerminalControl) $ \frame ->
                    plet (Raw.preplace # frame # 2 # pconstant (PD.I 5)) $ \changed ->
                        pmatch (pdecodeControlV1 # longTerminalControl) $ \control ->
                            pdecodeControlV1 # (Raw.pencode # changed) #== pcon control{pproof'outputIndex = pdata 5}
        , testCase "roundtrips a raw span window" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (Raw.preplace # (Raw.popen # longTerminalControl) # 12 # (Raw.pspanWindowData # 10 # 4 # (pblake2b_256 # pconstant "abcd"))) $ \frame ->
                    Raw.pspanWindow # frame #== ptestWindow
        , testCase "roundtrips a raw descriptor fact" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (pblake2b_256 # pconstant "fact") $ \digest ->
                    Raw.pfact # (Raw.preplace # (Raw.popen # longTerminalControl) # 15 # (Raw.pfactData # digest)) # 2 #== pcon (PDJust $ pdata digest)
        , testCase "rejects maps in the raw grammar" $
            pfailsNoTraceWithoutHoistChecks $
                Raw.pencodeItem # pconstant (PD.Map [])
        , testCase "rejects a foreign constructor" $
            pfailsNoTraceWithoutHoistChecks $
                Raw.pencodeItem # pconstant (PD.Constr 2 [])
        , testCase "rejects a malformed optional value" $
            pfailsNoTraceWithoutHoistChecks $
                Raw.pencodeItem # pconstant (PD.Constr 0 [])
        , testCase "rejects an out-of-range replacement" $
            pfailsNoTraceWithoutHoistChecks $
                Raw.preplace # (Raw.popen # longTerminalControl) # 17 # pconstant (PD.I 0)
        , testCase "rejects a negative replacement" $
            pfailsNoTraceWithoutHoistChecks $
                Raw.preplace # (Raw.popen # longTerminalControl) # (-1) # pconstant (PD.I 0)
        , testCase "rejects an out-of-range fact role" $
            pfailsNoTraceWithoutHoistChecks $
                Raw.pfact # (Raw.popen # longTerminalControl) # 4
        , testCase "rejects the obsolete twelve-field control" $
            pfailsNoTraceWithoutHoistChecks $
                Raw.popen # (bytes "8c" <> (psliceBS # 1 # (plengthBS # longTerminalControl - 16) # longTerminalControl))
        , testCase "rejects an indefinite control array" $
            pfailsNoTraceWithoutHoistChecks $
                Raw.popen # (bytes "9f" <> (psliceBS # 1 # (plengthBS # longTerminalControl - 1) # longTerminalControl) <> bytes "ff")
        ]

narrowStageTests :: TestTree
narrowStageTests =
    testGroup
        "narrow output-proof stages"
        [ testCase "structure matches the typed transition" $
            passertEvalNoTraceWithoutHoistChecks $
                pstageAgrees Stages.pstructure pstructureStep (pinitialFor 0 noReferenceOutput) (pchunks $ pproofFor 0 noReferenceOutput)
        , testCase "structure headers match the typed transition" $
            passertEvalNoTraceWithoutHoistChecks $
                pstageAgrees Stages.pstructureHeaders pstructureStep (pinitialFor 0 noReferenceOutput) (pchunks $ pproofFor 0 noReferenceOutput)
        , testCase "structure finish installs the initial value control" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (pinitialFor 0 noReferenceOutput) $ \initial -> pmatch initial $ \c ->
                    pstageAgrees Stages.pstructureFinish pstructureStep (pcon c{pproof'outputScan = pdata pnoReferenceScan}) pnoWitness
        , testCase "value finish preserves every unrelated frame item" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch pnoReferenceTerminal $
                    \c -> pstageAgrees Stages.pvalueFold (plam $ \c w -> pmatch c $ \fields -> pmatch (pfromData $ pproof'value fields) $ \case PDNothing -> perror; PDJust v -> pvalueFoldStep # c # w # pfromData v) (pcon c{pproof'stage = pdata pstageValueFold}) pnoWitness
        , testCase "span attachment agrees with the typed proof" $
            passertEvalNoTraceWithoutHoistChecks $
                pstageAgrees Stages.pspanAttach pstepV1 preferenceReady (pspanWitness smallReferenceOutput $ pproofFor 0 smallReferenceOutput)
        , testCase "reference commitment agrees with the typed proof" $
            passertEvalNoTraceWithoutHoistChecks $
                pstageAgrees
                    Stages.preferenceScript
                    preferenceCommitmentStep
                    (pattach preferenceReady smallReferenceOutput $ pproofFor 0 smallReferenceOutput)
                    (pcon $ PLedgerOutputProofWindow $ pdata smallReferenceOutput)
        , testCase "script hash agrees with the typed proof" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (pattach preferenceReady smallReferenceOutput $ pproofFor 0 smallReferenceOutput) $ \attached ->
                    plet (padvanced $ preferenceCommitmentStep # attached # pcon (PLedgerOutputProofWindow $ pdata smallReferenceOutput)) $ \committed ->
                        plet (padvanced $ preferenceCommitmentStep # committed # pnoWitness) $ \ready ->
                            pstageAgrees Stages.pscriptHash (plam phashControlStep) ready (pcon $ PLedgerOutputProofWindow $ pdata smallReferenceOutput)
        , testCase "span attachment rejects forged membership"
            $ passertEvalNoTraceWithoutHoistChecks
            $ pmatch
                ( Stages.pspanAttach
                    # (Raw.popen # (pencodeControlV1 # pdatumReady))
                    # pspanWitness smallDatumOutput (pproofFor 1 smallDatumOutput)
                )
            $ \case PNothing -> pconstant @PBool True; _ -> pconstant @PBool False
        , testCase "structure rejects prematurely attached facts" $
            pfailsNoTraceWithoutHoistChecks $
                Stages.pstructure
                    # (Raw.preplace # (Raw.popen # (pencodeControlV1 # pinitialFor 0 noReferenceOutput)) # 13 # (Raw.pfactData # (pblake2b_256 # pconstant "fact")))
                    # pchunks (pproofFor 0 noReferenceOutput)
        , testCase "fact attach requires the next canonical role group"
            $ passertEvalNoTraceWithoutHoistChecks
            $ pmatch
                ( Stages.pfactAttach
                    # (Raw.popen # (pencodeControlV1 # pnoReferenceTerminal))
                    # pconstant @(PBuiltinList PInteger) [0]
                    # pconstant "descriptor"
                    # pconstant (PD.I 1)
                    # pconstant (PD.Constr 1 [])
                )
            $ \case
                PNothing -> pconstant @PBool True
                _ -> pconstant @PBool False
        , testCase "raw and typed fact attachment agree" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (pexpectJust $ pterminalClaimedSummariesV1 # pnoReferenceTerminal) $ \pair -> pmatch pair $ \(PPair value datum) ->
                    plet
                        ( pexpectJust $
                            Stages.pfactAttach
                                # (Raw.popen # (pencodeControlV1 # pnoReferenceTerminal))
                                # pconstant @(PBuiltinList PInteger) [2, 3]
                                # pconstant "descriptor"
                                # value
                                # datum
                        )
                        $ \rawNext ->
                            Raw.pencode # rawNext #== pencodeControlV1 # (pexpectJust $ pfactAttachV1 # pnoReferenceTerminal # pconstant "descriptor")
        , testCase "raw exact fact gate binds the same final triple" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (pexpectJust $ pterminalClaimedSummariesV1 # pnoReferenceTerminal) $ \pair -> pmatch pair $ \(PPair value datum) ->
                    plet (Raw.popen # (pencodeControlV1 # pcompleteFacts)) $ \raw ->
                        Stages.pfactsAreExact
                            # raw
                            # pconstant "descriptor"
                            # value
                            # datum
                            #&& pnot
                            # (Stages.pfactsAreExact # raw # pconstant "substituted" # value # datum)
        ]

pstageAgrees :: forall s. Term s (Raw.PFrame :--> PLedgerOutputProofWitnessV1 :--> PMaybe Stages.PStepResult) -> Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> PMaybe PLedgerOutputProofStepResultV1) -> Term s PLedgerOutputProofControlV1 -> Term s PLedgerOutputProofWitnessV1 -> Term s PBool
pstageAgrees narrow typed control witness =
    pmatch (narrow # (Raw.popen # (pencodeControlV1 # control)) # witness) $ \case
        PNothing -> typed # control # witness #== pcon PNothing
        PJust result -> pmatch result $ \case
            Stages.PAdvanced next -> Raw.pencode # next #== pencodeControlV1 # (padvanced $ typed # control # witness)
            Stages.PInvalidOutput -> typed # control # witness #== pcon (PJust $ pcon PLedgerOutputProofInvalidOutput)
            Stages.PInvalidReferenceScript -> typed # control # witness #== pcon (PJust $ pcon PLedgerOutputProofInvalidReferenceScript)
            Stages.PNativeScriptNodeLimit -> typed # control # witness #== pcon (PJust $ pcon PLedgerOutputProofNativeScriptNodeLimit)
            Stages.PNativeScriptDepthLimit -> typed # control # witness #== pcon (PJust $ pcon PLedgerOutputProofNativeScriptDepthLimit)

narrowDescriptorTests :: TestTree
narrowDescriptorTests =
    testGroup
        "descriptor attestation roles"
        [ testCase "four roles conjoin to the complete descriptor predicate" $
            passertEvalNoTraceWithoutHoistChecks $
                plet pnoReferenceTerminal $ \control ->
                    plet (Raw.popen # (pencodeControlV1 # control)) $ \raw ->
                        plet (pexpectJust $ pvalueSummaryV1 # control) $ \value ->
                            plet (pexpectJust $ pcardanoTxOutSummaryV1 # control) $ \cardano ->
                                plet (pdescriptor control (pexpectJust $ pmidgardTxOutSummaryV1 # control) (pexpectJust $ pcardanoSpendDatumSummaryV1 # control) 0 cardano) $ \descriptor ->
                                    plet (Commitment.pencodeLedgerOutputCommitment # descriptor) $ \cbor ->
                                        Descriptor.pvalueSummaryIsPinned
                                            # raw
                                            # value
                                            #&& Descriptor.pdatumSummaryIsPinned
                                            # raw
                                            # pcon PDNothing
                                            #&& Descriptor.preferenceScriptIsPinned
                                            # raw
                                            # cbor
                                            #&& Descriptor.pscanFactsArePinned
                                            # raw
                                            # cbor
                                            # value
                                            # pcon PDNothing
                                            #&& pdescriptorIsExactV1
                                            # control
                                            # descriptor
        , testCase "value role rejects a substituted leaf summary" $
            passertEvalNoTraceWithoutHoistChecks $
                pnot
                    # ( Descriptor.pvalueSummaryIsPinned
                            # (Raw.popen # (pencodeControlV1 # pnoReferenceTerminal))
                            # (psemanticDataSummaryV1 # pconstant (PD.I 123))
                      )
        , testCase "datum role rejects a false datum-presence claim" $
            passertEvalNoTraceWithoutHoistChecks $
                pnot
                    # ( Descriptor.pdatumSummaryIsPinned
                            # (Raw.popen # (pencodeControlV1 # pnoReferenceTerminal))
                            # pcon (PDJust $ pdata $ psemanticDataSummaryV1 # pconstant (PD.I 0))
                      )
        , testCase "datum role pins authenticated inline datum summary" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (padvanced $ pdatumControlStepWith pdatumFoldStep pdatumAuthenticated pnoWitness) $ \terminal ->
                    Descriptor.pdatumSummaryIsPinned
                        # (Raw.popen # (pencodeControlV1 # terminal))
                        # pcon (PDJust $ pdata $ psemanticDataSummaryV1 # pconstant (PD.Constr 0 []))
        , testCase "scan role refuses a changed lovelace fact" $
            passertEvalNoTraceWithoutHoistChecks $
                plet pnoReferenceTerminal $ \control ->
                    plet
                        ( pdescriptor
                            control
                            (pexpectJust $ pmidgardTxOutSummaryV1 # control)
                            (pexpectJust $ pcardanoSpendDatumSummaryV1 # control)
                            1
                            (pexpectJust $ pcardanoTxOutSummaryV1 # control)
                        )
                        $ \wrong ->
                            pnot
                                # ( Descriptor.pscanFactsArePinned
                                        # (Raw.popen # (pencodeControlV1 # control))
                                        # (Commitment.pencodeLedgerOutputCommitment # wrong)
                                        # (pexpectJust $ pvalueSummaryV1 # control)
                                        # pcon PDNothing
                                  )
        , testCase "every descriptor role refuses a nonterminal scan" $
            pfailsNoTraceWithoutHoistChecks $
                Descriptor.pterminalScan # (Raw.popen # (pencodeControlV1 # pinitialFor 0 noReferenceOutput))
        ]

-- Compare each narrow datum role with the typed traversal transition, including
-- the untouched raw frame items. Claims are generated from the pre-state.
narrowDatumTests :: TestTree
narrowDatumTests =
    testGroup
        "narrow datum stages"
        [ testCase "streamed large constructor agrees at every stage" $ passertEvalNoTraceWithoutHoistChecks largeDatumAgreement
        , testCase "sequence head agrees" $
            passertEvalNoTraceWithoutHoistChecks $
                pstageAgrees (plam $ \c w -> Datum.pheadSequence # c # pforgetData (pdata w)) (plam $ pdatumControlStepWith pdatumHeadStep) (pwithDatumWindow pdatumReady) (pdatumWitness (pcon $ Traverse.PHeadSequence $ pdata 0) (pcon $ PDJust $ pdata smallDatumOutput))
        , testCase "frame finalization agrees" $
            passertEvalNoTraceWithoutHoistChecks $
                pstageAgrees (plam $ \c w -> Datum.pfinalizeFrame # c # pforgetData (pdata w)) (plam $ pdatumControlStepWith pdatumFoldStep) pdatumOpened (pdatumWitness (pcon $ Traverse.PFinalizeFrame (pdata $ Frame.pinitialSmallConstrFrameV1 # 0 # pconstant "" # 0) (pdata $ pcon PDNothing)) (pcon PDNothing))
        , testCase "finish agrees" $
            passertEvalNoTraceWithoutHoistChecks $
                pstageAgrees (plam $ \c w -> Datum.pfinish # c # pforgetData (pdata w)) (plam $ pdatumControlStepWith pdatumFoldStep) pdatumAuthenticated pnoWitness
        , testCase "head refuses mutated window bytes" $
            pfailsNoTraceWithoutHoistChecks $
                Datum.pheadSequence # (Raw.popen # (pencodeControlV1 # pwithDatumWindow pdatumReady)) # pforgetData (pdata $ pdatumWitness (pcon $ Traverse.PHeadSequence $ pdata 0) (pcon $ PDJust $ pdata $ bytes "00" <> (psliceBS # 1 # 41 # smallDatumOutput)))
        , testCase "head refuses a missing span attachment" $
            pfailsNoTraceWithoutHoistChecks $
                Datum.pheadSequence # (Raw.popen # (pencodeControlV1 # pdatumReady)) # pforgetData (pdata $ pdatumWitness (pcon $ Traverse.PHeadSequence $ pdata 0) (pcon $ PDJust $ pdata smallDatumOutput))
        , testCase "finish refuses active traversal" $
            pfailsNoTraceWithoutHoistChecks $
                Datum.pfinish # (Raw.popen # (pencodeControlV1 # pdatumReady)) # pforgetData (pdata pnoWitness)
        , testCase "integer attestation binds prefix and scalar" $
            passertEvalNoTraceWithoutHoistChecks $
                plet integerOpened $ \control -> plet (honestScalar $ activeDatum control) $ \claim ->
                    Datum.pintegerScalarClaimIsExact
                        # (Raw.popen # (pencodeControlV1 # control))
                        # claim
                        #&& pnot
                        # (Datum.pintegerScalarClaimIsExact # (Raw.popen # (pencodeControlV1 # control)) # pconstant (PD.Constr 0 [PD.I 0, PD.I 1, PD.I 1, PD.B "", PD.I 0]))
        , testCase "bytes attestation refuses integer control" $
            pfailsNoTraceWithoutHoistChecks $
                Datum.pbytesScalarClaimIsExact # (Raw.popen # (pencodeControlV1 # integerOpened)) # honestScalar (activeDatum integerOpened)
        ]

activeDatum :: forall s. Term s PLedgerOutputProofControlV1 -> Term s Traverse.PDataTraverseControlV1
activeDatum control = pmatch control $ \c -> pmatch (pfromData $ pproof'datum c) $ \case PDNothing -> perror; PDJust d -> pfromData d

honestScalar :: forall s. Term s Traverse.PDataTraverseControlV1 -> Term s PData
honestScalar current = pmatch current $ \c ->
    pforgetData $
        pconstrBuiltin
            # 0
            # ( pcons
                    # pforgetData (Traverse.ptraverse'sourceStart c)
                    # ( pcons
                            # pforgetData (Traverse.ptraverse'sourceLength c)
                            # ( pcons
                                    # pforgetData (Traverse.ptraverse'offset c)
                                    # ( pcons
                                            # pforgetData (Traverse.ptraverse'frameRoot c)
                                            # ( pcons
                                                    # pif
                                                        (pfromData (Traverse.ptraverse'stage c) #== Traverse.pstageInteger)
                                                        (pmatch (pfromData $ Traverse.ptraverse'integer c) $ \case PDNothing -> perror; PDJust scalar -> pforgetData scalar)
                                                        (pmatch (pfromData $ Traverse.ptraverse'bytes c) $ \case PDNothing -> perror; PDJust scalar -> pforgetData scalar)
                                                    # pnil
                                              )
                                      )
                              )
                      )
              )

sameDatumStep :: forall s. Term s (PLedgerOutputProofControlV1 :--> Traverse.PDataTraverseActionV1 :--> PByteString :--> PLedgerOutputProofControlV1)
sameDatumStep = phoistAcyclic $ plam $ \control action output ->
    plet (activeDatum control) $ \datum -> pmatch datum $ \d ->
        plet (pdatumWitness action $ pmatch (Traverse.pnextSourceSpanV1 # datum) $ \case PNothing -> pcon PDNothing; PJust _ -> pcon $ PDJust $ pdata output) $ \witness ->
            plet (padvanced $ pdatumControlStepWith pdatumStep control witness) $ \next ->
                plet (Raw.popen # (pencodeControlV1 # control)) $ \raw ->
                    plet (pforgetData $ pdata witness) $ \wd ->
                        plet (pfromData $ Traverse.ptraverse'stage d) $ \stage ->
                            plet
                                ( pmatch action $ \case
                                    Traverse.PHeadScalar _ -> Datum.pheadScalar # raw # wd
                                    Traverse.PHeadSequence _ -> Datum.pheadSequence # raw # wd
                                    Traverse.PHeadMap -> Datum.pheadMap # raw # wd
                                    Traverse.PHeadLargeConstructor _ _ -> Datum.pheadLargeConstructor # raw # wd
                                    Traverse.PAttachScalar _ -> pif (stage #== Traverse.pstageInteger) (Datum.pattachInteger # raw # wd # honestScalar datum) (Datum.pattachBytes # raw # wd # honestScalar datum)
                                    Traverse.PFoldList _ _ _ _ -> Datum.pfoldList # raw # wd
                                    Traverse.PFoldMap _ _ _ _ _ _ -> Datum.pfoldMap # raw # wd
                                    Traverse.PFinalizeFrame _ _ -> Datum.pfinalizeFrame # raw # wd
                                    Traverse.PNoAction ->
                                        pif (stage #== Traverse.pstageInteger) (Datum.padvanceInteger # raw # wd # honestScalar datum) $
                                            pif (stage #== Traverse.pstageBytes) (Datum.padvanceBytes # raw # wd # honestScalar datum) $
                                                pif (stage #== Traverse.pstageLargeConstructor) (Datum.padvanceLargeConstructor # raw # wd) $
                                                    pif (stage #== Traverse.pstageLargeFields) (Datum.padvanceLargeFields # raw # wd) (Datum.pclose # raw # wd)
                                )
                                $ \narrow ->
                                    pmatch narrow $ \case
                                        PNothing -> perror
                                        PJust result -> pmatch result $ \case
                                            Stages.PAdvanced rawNext -> pif (Raw.pencode # rawNext #== pencodeControlV1 # next) next perror
                                            _ -> perror

largeDatumOutput :: forall s. Term s PByteString
largeDatumOutput = bytes "a300581d7811111111111111111111111111111111111111111111111111111111018200a00251d86682c2490100000000000000009f01ff"

largeDatumReady :: forall s. Term s PLedgerOutputProofControlV1
largeDatumReady =
    pattach
        (pproofControl pstageDatumTraversal largeDatumOutput (pscanTerminal 56 3 1 39 17 (-1) (-1) (-1) 0) (pcon $ PDJust $ pdata $ Traverse.pinitialControlV1 # 39 # 17))
        largeDatumOutput
        (pproofFor 0 largeDatumOutput)

integerOpened :: forall s. Term s PLedgerOutputProofControlV1
integerOpened = plet (pproofControl pstageDatumTraversal (bytes "01") (pscanTerminal 1 3 1 0 1 (-1) (-1) (-1) 0) (pcon $ PDJust $ pdata $ Traverse.pinitialControlV1 # 0 # 1)) $ \ready ->
    padvanced $ pdatumControlStepWith pdatumHeadStep (pattach ready (bytes "01") (pproofFor 0 $ bytes "01")) (pdatumWitness (pcon $ Traverse.PHeadScalar $ pdata 1) (pcon $ PDJust $ pdata $ bytes "01"))

advanceDatumWhile :: forall s. Term s (PLedgerOutputProofControlV1 :--> PInteger :--> PInteger :--> PLedgerOutputProofControlV1)
advanceDatumWhile = phoistAcyclic $ pfix $ \self -> plam $ \control stage fuel ->
    pmatch (activeDatum control) $ \d ->
        pif (fuel #<= 0) perror $
            pif
                (pfromData (Traverse.ptraverse'stage d) #== stage)
                (self # (sameDatumStep # control # pcon Traverse.PNoAction # largeDatumOutput) # stage # (fuel - 1))
                control

advanceDatumInteger :: forall s. Term s (PLedgerOutputProofControlV1 :--> PInteger :--> PLedgerOutputProofControlV1)
advanceDatumInteger = phoistAcyclic $ pfix $ \self -> plam $ \control fuel ->
    pmatch (activeDatum control) $ \d -> pmatch (pfromData $ Traverse.ptraverse'integer d) $ \case
        PDNothing -> perror
        PDJust scalar -> pmatch (pfromData scalar) $ \i ->
            pif (fuel #<= 0) perror $
                pif
                    (pfromData (Integer.pint'stage i) #== Integer.pstageTerminal)
                    control
                    (self # (sameDatumStep # control # pcon Traverse.PNoAction # largeDatumOutput) # (fuel - 1))

largeDatumAgreement :: forall s. Term s PBool
largeDatumAgreement =
    plet (sameDatumStep # largeDatumReady # pcon (Traverse.PHeadLargeConstructor (pdata 11) (pdata 1)) # largeDatumOutput) $ \opened ->
        plet (advanceDatumWhile # opened # Traverse.pstageLargeConstructor # 256) $ \largeFields ->
            pmatch (activeDatum largeFields) $ \fields -> pmatch (pfromData $ Traverse.ptraverse'integer fields) $ \case
                PDNothing -> perror
                PDJust integer -> pmatch (pfromData integer) $ \i -> pmatch (pfromData $ Integer.pint'blob i) $ \case
                    PDNothing -> perror
                    PDJust blob ->
                        plet (Frame.pinitialLargeConstrFrameV1 # (pexpectJust $ Blob.pfinalizeV1 # pfromData blob) # pfromData (Integer.pint'sourceLength i) # pfromData (Integer.pint'memory i) # pfromData (Traverse.ptraverse'frameRoot fields) # 1) $ \frame ->
                            plet (sameDatumStep # largeFields # pcon Traverse.PNoAction # largeDatumOutput) $ \fieldsOpened ->
                                plet (sameDatumStep # fieldsOpened # pcon (Traverse.PHeadScalar $ pdata 1) # largeDatumOutput) $ \fieldOpened ->
                                    plet (advanceDatumInteger # fieldOpened # 256) $ \terminalInteger ->
                                        pmatch (activeDatum terminalInteger) $ \td -> pmatch (pfromData $ Traverse.ptraverse'integer td) $ \case
                                            PDNothing -> perror
                                            PDJust scalar ->
                                                plet (pexpectJust $ Integer.pfinalizeV1 # pfromData scalar) $ \summary ->
                                                    plet (sameDatumStep # terminalInteger # pcon (Traverse.PAttachScalar $ pdata $ pcon $ PDJust $ pdata frame) # largeDatumOutput) $ \attached ->
                                                        plet (sameDatumStep # attached # pcon Traverse.PNoAction # largeDatumOutput) $ \closed ->
                                                            plet (pexpectJust $ Frame.pappendChildV1 # frame # summary) $ \full ->
                                                                plet (sameDatumStep # closed # pcon (Traverse.PFoldList (pdata full) (pdata 0) (pdata summary) (pdata pnil)) # largeDatumOutput) $ \folded ->
                                                                    plet (pexpectJust $ Frame.pfoldListChildV1 # full # 0 # summary # pnil) $ \foldedFrame ->
                                                                        plet (sameDatumStep # folded # pcon (Traverse.PFinalizeFrame (pdata foldedFrame) (pdata $ pcon PDNothing)) # largeDatumOutput) $ \terminal ->
                                                                            plet (pexpectJust $ Traverse.pfinalizeV1 # activeDatum terminal) $ \result -> pmatch result $ \r ->
                                                                                pfromData (psummary'root r) #== bytes "844cdd8ac8dc97d87e4ed149da121054504365b523034a804a12c014d55c2c44"
