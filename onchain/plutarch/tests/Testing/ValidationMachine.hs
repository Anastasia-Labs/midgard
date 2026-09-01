module Testing.ValidationMachine (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import PlutusCore.Data qualified as PD
import Plutarch.Builtin.Crypto (pblake2b_224, pblake2b_256)
import Plutarch.LedgerApi.AssocMap (PAssocMap (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.MerkleTree.Merkling (pnull_hash)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Aiken.Cbor (pdeserialise)
import Midgard.BoundedCollection qualified as BoundedCollection
import Midgard.BoundedItem qualified as Bounded
import Midgard.CekConstant qualified as CekConstant
import Midgard.CekData qualified as CekData
import Midgard.CekMachine qualified as CekMachine
import Midgard.CekProof qualified as CekProof
import Midgard.Blake2b224Trace qualified as Blake2b224
import Midgard.FraudProofs.NativeTx.Codec qualified as Codec
import Midgard.FraudProofs.NativeTx.Compact qualified as NativeCompact
import Midgard.FraudProofs.NativeTx.Transaction qualified as NativeTransaction
import Midgard.FraudProofs.NativeTx.Components (
  pencodeMidgardAddressWitness,
  pencodeMidgardTxInput,
  pencodeMidgardTxOutput,
  pencodeMidgardVersionedScript,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardAddressWitness (..),
  PMidgardCredential (..),
  PMidgardTxInput (..),
  PMidgardTxOutput (..),
  PMidgardValue (..),
  PMidgardScriptLanguage (..),
  PMidgardVersionedScript (..),
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxWitnessSetCompact (..),
 )
import Midgard.MpfProofFold qualified as ProofFold
import Midgard.MpfProof qualified as MpfProof
import Midgard.MpfProof.Types (PProof (..), PProofStep (..))
import Midgard.NativeTxFieldAccess qualified as NativeField
import Midgard.NativeScriptScan qualified as NativeScriptScan
import Midgard.RedeemerItemProof qualified as RedeemerItemProof
import Midgard.ScriptContext qualified as ScriptContext
import Midgard.ScriptLanguageViews qualified as ScriptLanguageViews
import Midgard.LedgerOutputProof qualified as LedgerOutputProof
import Midgard.LedgerOutputScan qualified as LedgerOutputScan
import Midgard.LedgerOutputValue qualified as LedgerOutputValue
import Midgard.LedgerOutputCommitment qualified as OutputCommitment
import Midgard.ValidationMachine
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ScriptProof qualified as ScriptProof
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (..),
  PValidationSourceKind (..),
  PValidationVerdict (..),
  phashRejectionCode,
  phashLedgerDelta,
  phashValidationContext,
  phashWorkWitness,
  pmachineVersion,
 )
import Testing.Eval (passertEvalNoTrace, passertEvalNoTraceWithoutHoistChecks, pfails)

tests :: TestTree
tests = testGroup "Midgard.ValidationMachine"
  [ testCase "validation_tail_auxiliary_v1_typescript_corpus_is_exact" $
      passertEvalNoTrace validationTailAuxiliaryCorpusAbi
  , testCase "canonical_validation_controls_v1_typescript_abi_vectors" $
      passertEvalNoTrace canonicalValidationControlsAbiVectors
  , testCase "terminal_acceptance_and_rejection_v1_typescript_vectors_are_exact" $
      passertEvalNoTrace terminalAcceptanceAndRejectionAbiVectors
  , testCase "value_accumulator_update_and_control_v1_abi_is_exact" $
      passertEvalNoTrace valueAccumulatorUpdateAndControlAbi
  , testCase "ledger_delta_control_operation_and_frontier_v1_abi_is_exact" $
      passertEvalNoTrace ledgerDeltaControlOperationAndFrontierAbi
  , testCase "validation auxiliary witness constructor ABI matches Aiken" $
      passertEvalNoTrace validationAuxiliaryWitnessAbi
  , testCase "validation_auxiliary_v1_rejects_adjacent_tag_40" $
      pfails malformedAuxiliaryAdjacentTag
  , testCase "validation_auxiliary_v1_rejects_wrong_constructor_arity" $
      pfails malformedAuxiliaryWrongArity
  , testCase "validation_auxiliary_v1_accepts_the_canonical_constructor_zero" $
      passertEvalNoTrace auxiliaryCanonicalConstructorZero
  , testCase "validation_tail_auxiliary_v1_rejects_adjacent_tag_40" $
      pfails malformedAuxiliaryAdjacentTag
  , testCase "validation_tail_auxiliary_v1_rejects_wrong_arity" $
      pfails malformedTailAuxiliaryWrongArity
  , testCase "native V1 optional POSIX-time sentinel and adjacent invalid values are exact" $
      passertEvalNoTrace validityIntervalSentinels
  , testCase "purpose kind to redeemer pointer mapping is exact" $
      passertEvalNoTrace purposeKindMapping
  , testCase "observer purpose scan control CBOR is canonical" $
      passertEvalNoTrace observerScanEncoding
  , testCase "receive purpose scan control CBOR is canonical" $
      passertEvalNoTrace receiveScanEncoding
  , testCase "resolved input accumulator vector is stable" $
      passertEvalNoTrace resolvedInputAccumulatorVector
  , testCase "mint asset leaf vector is stable" $
      passertEvalNoTrace mintAssetLeafVector
  , testCase "ledger delta operation frontier vectors are stable" $
      passertEvalNoTrace ledgerDeltaOperationVectors
  , testCase "parameterized minimum Ada boundary matches target snapshot" $
      passertEvalNoTrace parameterizedMinAdaBoundary
  , testCase "CanonicalDecode advances and rejects empty fields exactly" $
      passertEvalNoTrace canonicalDecodeEmptyTransitions
  , testCase "CanonicalDecode authenticates a bounded item chunk" $
      passertEvalNoTrace canonicalDecodeChunkTransition
  , testCase "CanonicalDecode authenticates a complete bounded item" $
      passertEvalNoTrace canonicalDecodeItemTransition
  , testCase "canonical_decode_rejects_a_legacy_field_seven_script_proof" $
      passertEvalNoTrace canonicalDecodeRejectsLegacyFieldSevenScriptProof
  , testGroup "validation-machine-v1 maximum field-terminal TypeScript vectors"
      [ maximumFieldTerminalCase "maximum_spend_input_field_terminal_matches_typescript" maximumSpendInputFieldTerminal
      , maximumFieldTerminalCase "maximum_reference_input_field_terminal_matches_typescript" maximumReferenceInputFieldTerminal
      , maximumFieldTerminalCase "maximum_observer_field_terminal_matches_typescript" maximumObserverFieldTerminal
      , maximumFieldTerminalCase "maximum_output_field_terminal_matches_typescript" maximumOutputFieldTerminal
      , maximumFieldTerminalCase "maximum_required_signer_field_terminal_matches_typescript" maximumRequiredSignerFieldTerminal
      , maximumFieldTerminalCase "maximum_mint_field_terminal_matches_typescript" maximumMintFieldTerminal
      ]
  , testCase "early phase witness encoders match canonical CBOR vectors" $
      passertEvalNoTrace earlyPhaseWitnessEncodingVectors
  , testCase "early phase witness encoders abort on malformed controls" $
      pfails malformedInputSetsWitness
  , testCase "transaction resolution schedule is stable and key ordered" $
      passertEvalNoTrace transactionResolutionScheduleOrdering
  , testCase "resolve-input witness optional proof encoding is canonical" $
      passertEvalNoTrace resolveInputsWitnessEncoding
  , testCase "resolve-input witness rejects malformed fixed-width fields" $
      pfails malformedResolveInputsWitness
  , testCase "resolve_inputs_v1_rejects_malformed_accumulator" $
      pfails malformedResolveInputsWitness
  , testCase "Phase-A native-script scan witness matches canonical CBOR" $
      passertEvalNoTrace phaseANativeScriptsWitnessEncoding
  , testCase "Phase-A native-script scan witness rejects invalid result tag" $
      pfails malformedPhaseANativeScriptsWitness
  , testCase "validation context and input-set controls decode exactly" $
      passertEvalNoTrace controlDecodingVectors
  , testCase "validation context rejects an unsupported network" $
      pfails malformedValidationContext
  , testCase "signatures control witness decodes frontier peaks exactly" $
      passertEvalNoTrace signaturesControlDecoding
  , testCase "Phase-A native control decodes nested signer continuation" $
      passertEvalNoTrace phaseANativeControlDecoding
  , testCase "preconditions and resolve-input controls decode exactly" $
      passertEvalNoTrace handoffControlDecoding
  , testCase "value accumulator codec is canonical" $
      passertEvalNoTrace valueAccumulatorCodec
  , testCase "value accumulator rejects nonzero count above seen count" $
      pfails malformedValueAccumulator
  , testCase "ledger-delta pending mutation and control codecs round-trip" $
      passertEvalNoTrace ledgerDeltaControlCodecs
  , testCase "ledger-delta stage-zero mutation rejects an active fold" $
      pfails malformedLedgerDeltaPendingMutation
  , testCase "native-script aggregate and script-integrity codecs are exact" $
      passertEvalNoTrace nativeScriptsControlCodecs
  , testCase "script_sources_v1_rejects_negative_stage" $
      pfails malformedScriptSourcesStage
  , testCase "native_scripts_v1_rejects_short_integrity_hash" $
      pfails malformedScriptIntegrityHash
  , testCase "value-and-mint control and initial witness round-trip" $
      passertEvalNoTrace valueAndMintControlCodec
  , testCase "CEK context control codec preserves all 25 fields" $
      passertEvalNoTrace cekContextControlCodec
  , testCase "CEK sub-control codecs match Aiken ABI vectors" $
      passertEvalNoTrace cekSubControlAbiVectors
  , testCase "CEK final context hash rejects empty summaries" $
      pfails malformedCekFinalContext
  , testCase "cek_context_v1_rejects_empty_final_summary" $
      pfails malformedCekFinalContext
  , testCase "CEK execution witness envelope is canonical" $
      passertEvalNoTrace cekWitnessEnvelope
  , testCase "CEK execution witness rejects an out-of-range cursor" $
      pfails malformedCekWitnessCursor
  , testCase "value asset mutation preserves MPF and count semantics" $
      passertEvalNoTrace valueAssetMutationSemantics
  , testCase "CEK observer summaries match context and Aiken vectors" $
      passertEvalNoTrace cekObserverSummaryVectors
  , testGroup "validation-machine-v1 CEK observer parity"
      [ testCase "cek_context_observer_cardano_maximum_224_first_item_and_terminal_agree" $
          passertEvalNoTraceWithoutHoistChecks cekObserverCardanoMaximumFirstAndTerminal
      , testCase "cek_context_observer_rejects_malformed_proofs_order_and_successors" $
          passertEvalNoTrace cekObserverRejectsMalformedProofsOrderAndSuccessors
      , testCase "cek_context_observer_complete_summary_relation_is_exact" $
          passertEvalNoTrace cekObserverCompleteSummaryRelationIsExact
      ]
  , testCase "CEK observer prepend rejects a non-credential hash" $
      pfails malformedCekObserverHash
  , testCase "CEK context well-formedness preserves the Aiken stage matrix" $
      passertEvalNoTrace cekContextWellFormedStageMatrix
  , testCase "CEK redeemer context validates active and completed bindings" $
      passertEvalNoTrace cekRedeemerContextBindings
  , testCase "CEK context successor binds the exact next work witness" $
      passertEvalNoTrace cekContextSuccessorBinding
  , testCase "CEK stage 0 authenticates an effectful execution selection" $
      passertEvalNoTrace cekExecutionSelectionTransition
  , testGroup "validation-machine-v1 CEK context ABI and selection parity"
      [ testCase "cek_effectful_selection_rejects_a_51_byte_program_envelope" $
          passertEvalNoTrace cekOversizedProgramEnvelopeRejected
      , testCase "maximum_native_then_effectful_cek_selection_is_bounded" $
          passertEvalNoTrace maximumNativeThenEffectfulCekSelectionIsBounded
      , testCase "cross_language_cek_context_control_vectors" $
          passertEvalNoTrace crossLanguageCekContextControlVectors
      , testCase "plutus_v3_receive_selection_rejects_with_an_exact_noop" $
          passertEvalNoTrace plutusV3ReceiveSelectionRejectsWithExactNoop
      ]
  , testCase "CEK core stepping binds budgets and the continuing state" $
      passertEvalNoTrace cekCoreStepTransition
  , testCase "CEK context dispatcher routes auxiliary witnesses by stage" $
      passertEvalNoTrace cekContextDispatcherRouting
  , testCase "CEK completion branch advances an empty execution set" $
      passertEvalNoTrace cekTopLevelCompletion
  , testCase "CEK one-step envelope enforces the structural successor" $
      passertEvalNoTrace cekOneStepStructuralEnvelope
  , testCase "Value-and-Mint stages 0 and 1 initialize replay exactly" $
      passertEvalNoTrace valueAndMintInitialStages
  , testCase "Value-and-Mint stage 2 opens and completes an asset-free input" $
      passertEvalNoTrace valueAndMintInputReplay
  , testCase "Value-and-Mint stage 2 authenticates and accumulates an input asset" $
      passertEvalNoTrace valueAndMintInputAssetReplay
  , testCase "Value-and-Mint stage 3 opens and completes an asset-free output" $
      passertEvalNoTrace valueAndMintOutputReplay
  , testCase "Value-and-Mint stage 3 authenticates and subtracts an output asset" $
      passertEvalNoTrace valueAndMintOutputAssetReplay
  , testCase "Value-and-Mint stage 4 authenticates and applies a mint asset" $
      passertEvalNoTrace valueAndMintMintReplay
  , testCase "Value-and-Mint stage 5 seeds the exact Ledger-Delta witness" $
      passertEvalNoTrace valueAndMintFinalize
  , testCase "Compact Binding independently authenticates the proof source" $
      passertEvalNoTrace compactBindingTransition
  , testCase "Static Ledger Rules advances an exact valid proof context" $
      passertEvalNoTrace staticRulesAcceptedTransition
  , testCase "Static Ledger Rules rejects a network mismatch without rewriting the delta" $
      passertEvalNoTrace staticRulesRejectedTransition
  , testCase "Static Ledger Rules rejects a cleared claimed delta root" $
      passertEvalNoTrace staticRulesClearedDeltaCannotReject
  , testCase "Static Ledger Rules rejects a rewritten claimed delta root" $
      passertEvalNoTrace staticRulesRewrittenDeltaCannotReject
  , testCase "Static Ledger Rules cannot prove a valid transaction rejected" $
      passertEvalNoTrace staticRulesValidCannotReject
  , testCase "Static Ledger Rules permits a forced-invalid empty claimed delta" $
      passertEvalNoTrace staticRulesForcedInvalidEmptyDelta
  , testCase "InputSets rejects empty inputs and a wrong-phase successor exactly" $
      passertEvalNoTrace inputSetsEmptyTransitions
  , testCase "InputSets authenticates bounded disjoint spend and reference inputs" $
      passertEvalNoTrace inputSetsDisjointTransition
  , testCase "InputSets rejects spend/reference overlap exactly" $
      passertEvalNoTrace inputSetsDuplicateTransition
  , testCase "InputSets rejects a malformed final validity interval exactly" $
      passertEvalNoTrace inputSetsMalformedValidityTransition
  , testCase "Signatures advances empty scans and hands off with the unevaluated sentinel" $
      passertEvalNoTrace signaturesEmptyTransitions
  , testCase "Signatures rejects a missing required signer exactly" $
      passertEvalNoTrace signaturesMissingRequiredTransition
  , testCase "Signatures rejects an invalid address signature exactly" $
      passertEvalNoTrace signaturesInvalidAddressTransition
  , testCase "Phase-A native scripts advances empty and selects nonempty fields" $
      passertEvalNoTrace phaseANativeEnvelopeTransitions
  , testCase "Phase-A native scripts completes a non-native item immediately" $
      passertEvalNoTrace phaseANativeNonNativeTransition
  , testCase "Phase-A native scripts rejects a malformed item header exactly" $
      passertEvalNoTrace phaseANativeMalformedItemTransition
  , testCase "Phase-A native scripts evaluates and finalizes an after timelock" $
      passertEvalNoTrace phaseANativeTimelockTransition
  , testCase "Phase-A native scripts splits timelock head and payload scans" $
      passertEvalNoTrace phaseANativeSplitTimelockTransition
  , testCase "Phase-A native scripts handles empty and framed container payloads" $
      passertEvalNoTrace phaseANativeContainerPayloadTransitions
  , testCase "Phase-A native scripts evaluates a signature against an empty signer set" $
      passertEvalNoTrace phaseANativeEmptySignaturePayloadTransition
  , testCase "Phase-A native scripts rejects an unsatisfied script exactly" $
      passertEvalNoTrace phaseANativeUnsatisfiedScriptTransition
  , testCase "Phase-A native scripts reduces a completed container frame" $
      passertEvalNoTrace phaseANativeFrameReductionTransition
  , testCase "Phase-A script preconditions finalize empty and reject missing integrity" $
      passertEvalNoTrace phaseAScriptPreconditionsFinalizeTransitions
  , testCase "Phase-A script preconditions authenticates and finalizes an observer" $
      passertEvalNoTrace phaseAScriptPreconditionsObserverTransition
  , testCase "Phase-A script preconditions rejects duplicate observers exactly" $
      passertEvalNoTrace phaseAScriptPreconditionsDuplicateObserverTransition
  , testCase "ResolveInputsInitial validates the interval before ledger lookup" $
      passertEvalNoTrace resolveInputsInitialTransitions
  , testCase "ResolveInputsFinish finalizes into canonical ScriptSources state" $
      passertEvalNoTrace resolveInputsFinishTransition
  , testCase "stage_differential_zero_finish_routes_agree" $
      passertEvalNoTrace scriptSourcesStageZeroEmptyFinishTransition
  , testCase "stage_differential_zero_begin_routes_agree" $
      passertEvalNoTrace scriptSourcesStageZeroBeginTransition
  , testCase "ScriptSourcesStageZero streams and advances an inline source hash" $
      passertEvalNoTrace scriptSourcesStageZeroHashTransitions
  , testCase "ScriptSourcesStageZero commits a terminal inline source identity" $
      passertEvalNoTrace scriptSourcesStageZeroTerminalTransition
  , testCase "ScriptSourcesStageOneRawFinish advances only to the canonical stage-two state" $
      passertEvalNoTrace scriptSourcesStageOneRawFinishTransition
  , testCase "ScriptSourcesStageOneRawFinish binds canonical successor encoding" $
      passertEvalNoTrace scriptSourcesStageOneRawFinishCanonicalEncoding
  , testCase "ScriptSourcesStageOneRedeemerBegin opens an authenticated redeemer item" $
      passertEvalNoTrace scriptSourcesStageOneRedeemerBeginTransition
  , testCase "ScriptSourcesStageOneRedeemerStep advances the authenticated item control" $
      passertEvalNoTrace scriptSourcesStageOneRedeemerStepTransition
  , testCase "ScriptSourcesStageOne enforces its redeemer auxiliary family" $
      do
        passertEvalNoTraceWithoutHoistChecks (scriptSourcesStageOneRedeemerFamilyGuard False)
        passertEvalNoTraceWithoutHoistChecks (scriptSourcesStageOneRedeemerFamilyGuard True)
  , testCase "ScriptSourcesStageTwo initializes replay from the committed schedule" $
      passertEvalNoTrace scriptSourcesStageTwoReplayInitializationTransition
  , testCase "ScriptSourcesStageThreeFinish advances a completed replay exactly" $
      passertEvalNoTrace scriptSourcesStageThreeFinishTransition
  , testCase "ScriptSourcesStageThreeReplay commits a reference script source" $
      passertEvalNoTrace scriptSourcesStageThreeReferenceReplayTransition
  , testCase "ScriptSourcesStageThreeReplay commits a script spend purpose" $
      passertEvalNoTrace scriptSourcesStageThreeSpendReplayTransition
  , testCase "ScriptSourcesStageFour skips an authenticated empty output field" $
      passertEvalNoTrace scriptSourcesStageFourEmptyTransition
  , testCase "ScriptSourcesStageFour authenticates and completes an output frontier" $
      passertEvalNoTrace scriptSourcesStageFourOutputTransition
  , testCase "ScriptSourcesStageFour rejects a forged output item commitment" $
      passertEvalNoTrace (scriptSourcesStageFourForgedOutputTransition True)
  , testCase "ScriptSourcesStageFour rejects a forged output item length" $
      passertEvalNoTrace (scriptSourcesStageFourForgedOutputTransition False)
  , testCase "ScriptSourcesStageFiveFinish advances a completed output scan" $
      passertEvalNoTrace scriptSourcesStageFiveFinishTransition
  , testCase "ScriptSourcesStageFive routes output proof witnesses exactly" $
      passertEvalNoTrace scriptSourcesStageFiveRouting
  , testCase "ScriptSourcesStageSix skips an authenticated empty mint field" $
      passertEvalNoTrace scriptSourcesStageSixEmptyTransition
  , testCase "ScriptSourcesStageSix finishes a completed mint fold" $
      passertEvalNoTrace scriptSourcesStageSixFinishTransition
  , testCase "ScriptSourcesStageSix mint begin preserves the flat-commitment tripwire" $
      passertEvalNoTrace scriptSourcesStageSixBeginTripwire
  , testCase "ScriptSourcesStageSix streams a mint asset into its frontier" $
      passertEvalNoTrace scriptSourcesStageSixAssetTransition
  , testCase "ScriptSourcesStageSix authenticates a mint chunk boundary" $
      passertEvalNoTrace (scriptSourcesStageSixBoundaryTransition $ pconstant False)
  , testCase "ScriptSourcesStageSix fails when boundary data is withheld" $
      pfails (scriptSourcesStageSixBoundaryTransition $ pconstant True)
  , testCase "ScriptSourcesStageSix routes mint fold states exactly" $
      passertEvalNoTrace scriptSourcesStageSixRouting
  , testCase "stage_differential_seven_observer_routes_agree" $
      passertEvalNoTrace scriptSourcesStageSevenObserverTransition
  , testCase "ScriptSourcesStageSeven finishes canonical observer purposes" $
      passertEvalNoTrace scriptSourcesStageSevenObserverFinishTransition
  , testCase "ScriptSourcesStageSeven finish binds canonical successor encoding" $
      passertEvalNoTraceWithoutHoistChecks scriptSourcesStageSevenFinishCanonicalEncoding
  , testCase "ScriptSourcesStageSeven observer binds canonical successor encoding" $
      passertEvalNoTrace scriptSourcesStageSevenObserverCanonicalEncoding
  , testCase "ScriptSourcesStageSeven receive binds canonical successor encoding" $
      passertEvalNoTrace scriptSourcesStageSevenReceiveCanonicalEncoding
  , testCase "stage_differential_seven_receive_routes_agree" $
      passertEvalNoTrace (scriptSourcesStageSevenReceiveTransition 0)
  , testCase "ScriptSourcesStageSeven appends a protected receive purpose" $
      passertEvalNoTrace (scriptSourcesStageSevenReceiveTransition 1)
  , testCase "ScriptSourcesStageSeven rescans receive sources for distinctness" $
      passertEvalNoTrace (scriptSourcesStageSevenReceiveTransition 2)
  , testCase "ScriptSourcesStageSeven finishes a protected receive purpose scan" $
      passertEvalNoTrace (scriptSourcesStageSevenReceiveTransition 3)
  , testCase "ScriptSourcesStageSeven advances an empty receive scan" $
      passertEvalNoTrace scriptSourcesStageSevenEmptyFinishTransition
  , testCase "script_sources_prepares_more_than_sixteen_purposes_for_discovery" $
      passertEvalNoTrace scriptSourcesPreparesMoreThanSixteenPurposes
  , testCase "script_sources_discovers_more_than_sixteen_purposes" $
      passertEvalNoTrace scriptSourcesDiscoversMoreThanSixteenPurposes
  , testCase "stage_differential_eight_finish_routes_agree" $
      passertEvalNoTrace scriptSourcesStageEightFinishTransition
  , testCase "stage_differential_eight_purpose_routes_agree" $
      passertEvalNoTrace scriptSourcesStageEightPurposeTransition
  , testCase "stage_differential_nine_missing_routes_agree" $
      passertEvalNoTrace scriptSourcesStageNineMissingTransition
  , testCase "script_sources_stage_nine_accepts_an_exact_mismatch" $
      passertEvalNoTrace scriptSourcesStageNineMismatchTransition
  , testCase "script_sources_stage_nine_accepts_an_exact_native_match" $
      passertEvalNoTrace scriptSourcesStageNineNativeMatchTransition
  , testCase "script_sources_stage_nine_accepts_an_exact_effectful_match" $
      passertEvalNoTrace scriptSourcesStageNineEffectfulMatchTransition
  , testCase "stage_differential_ten_missing_routes_agree" $
      passertEvalNoTraceWithoutHoistChecks scriptSourcesStageTenMissingTransition
  , testGroup
      "script_sources_stage_ten_redeemer_family_guards"
      [ testCase "scan begin is mismatch-family only" $
          passertEvalNoTrace (scriptSourcesStageTenMatchTransition 0)
      , testCase "item header is mismatch-family only" $
          passertEvalNoTrace (scriptSourcesStageTenMatchTransition 1)
      , testCase "terminal tail is match-family only" $
          passertEvalNoTrace (scriptSourcesStageTenMatchTransition 2)
      ]
  , testCase "ScriptSourcesStageTen advances past a redeemer mismatch" $
      passertEvalNoTrace (scriptSourcesStageTenMatchTransition 3)
  , testCase "stage_differential_eleven_finish_routes_agree" $
      passertEvalNoTrace scriptSourcesStageElevenAuditTransitions
  , testCase "ScriptSourcesStageEleven rejects an unused inline source exactly" $
      passertEvalNoTrace scriptSourcesStageElevenUnusedInlineTransition
  , testCase "ScriptSourcesStageTwelve opens an authenticated redeemer audit" $
      passertEvalNoTrace scriptSourcesStageTwelveBeginTransition
  , testCase "ScriptSourcesStageTwelve enforces its redeemer auxiliary family" $
      passertEvalNoTrace scriptSourcesStageTwelveFamilyGuard
  , testGroup
      "stage_differential_twelve_finish_routes_agree"
      [ testCase "accepting routes agree" $
          passertEvalNoTraceWithoutHoistChecks (scriptSourcesStageTwelveFinishTransition True)
      , testCase "rejecting routes agree" $
          passertEvalNoTraceWithoutHoistChecks (scriptSourcesStageTwelveFinishTransition False)
      ]
  , testCase "stage_eight_finish_pending_redeemer_hash_divergence_is_unreachable" $
      passertEvalNoTraceWithoutHoistChecks scriptSourcesStageEightPendingHashDivergence
  , testCase "stage_ten_missing_pending_redeemer_hash_divergence_is_unreachable" $
      passertEvalNoTraceWithoutHoistChecks scriptSourcesStageTenPendingHashDivergence
  , testCase "stage_twelve_finish_pending_redeemer_hash_divergence_is_unreachable" $
      passertEvalNoTraceWithoutHoistChecks scriptSourcesStageTwelvePendingHashDivergence
  , testCase "ScriptSources aggregate routes every stage and excludes output proofs" $
      passertEvalNoTrace scriptSourcesAggregateRouting
  , testCase "ScriptSources aggregate validates shared discovery and observer controls" $
      passertEvalNoTrace scriptSourcesAggregateControlPredicates
  , testCase "ScriptSources aggregate binds its control and structural envelope" $
      passertEvalNoTrace scriptSourcesAggregateEnvelope
  , testCase "NativeScripts terminal handoff binds the exact ScriptIntegrity witness" $
      passertEvalNoTrace nativeScriptsTerminalTransition
  , testCase "NativeScripts effectful languages update their bitmap exactly" $
      passertEvalNoTrace nativeScriptsLanguageBitmap
  , testCase "NativeScripts authenticates and advances an effectful descriptor" $
      passertEvalNoTrace nativeScriptsEffectfulTransition
  , testCase "NativeScripts authenticates a native descriptor and opens Phase-A scanning" $
      passertEvalNoTrace nativeScriptsNativeTransition
  , testCase "ScriptIntegrity stage 0 authenticates the proof source" $
      passertEvalNoTrace scriptIntegrityAuthenticationTransition
  , testCase "ScriptIntegrity verifies compact, witness-set, and final hashes" $
      passertEvalNoTrace scriptIntegrityRemainingTransitions
  , testCase "ScriptSourcesOutputProofBegin opens an authenticated output" $
      passertEvalNoTrace scriptSourcesOutputProofBeginTransition
  , testCase "ScriptSourcesOutputProofStep carries an authenticated advanced result" $
      passertEvalNoTrace scriptSourcesOutputProofStepTransition
  , testCase "ScriptSourcesOutputProofFinalize commits an authorized output descriptor" $
      passertEvalNoTrace scriptSourcesOutputProofFinalizeTransition
  , testCase "ScriptSourcesOutputProofFinalize rejects an unsigned protected pubkey output" $
      passertEvalNoTrace scriptSourcesOutputProofFinalizeMissingSignerTransition
  , testCase "ResolveInputsLookupOpen authenticates membership and non-membership" $
      passertEvalNoTrace resolveInputsLookupOpeningTransitions
  , testCase "ResolveInputsMembershipStep streams an authenticated output proof" $
      passertEvalNoTrace resolveInputsMembershipStepTransition
  , testCase "ResolveInputsMembershipFinalize authorizes and consumes an input" $
      passertEvalNoTrace resolveInputsMembershipFinalizeTransition
  , testCase "Ledger-Delta authenticates deletion and insertion operations" $
      passertEvalNoTrace ledgerDeltaOperationTransitions
  , testCase "Ledger-Delta replays reference and spend inputs exactly" $
      passertEvalNoTrace ledgerDeltaReplayTransitions
  , testCase "Ledger-Delta replay and output phase handoffs are exact" $
      passertEvalNoTrace ledgerDeltaPhaseHandoffs
  , testCase "Ledger-Delta authenticates an output descriptor and initializes its fold" $
      passertEvalNoTrace ledgerDeltaOutputTransition
  , testCase "Ledger-Delta advances only an authenticated proof frame" $
      passertEvalNoTrace ledgerDeltaProofFrameTransition
  , testCase "Ledger-Delta finalizes the root mutation and terminal commitment" $
      passertEvalNoTrace ledgerDeltaFinalTransitions
  , testCase "CEK stages 1 and 2 authenticate reference and spend inputs" $
      passertEvalNoTrace cekResolvedContextTransitions
  , testCase "CEK stages 3 and 4 authenticate outputs and signers" $
      passertEvalNoTrace cekOutputAndSignerContextTransitions
  , testCase "CEK stage 5 verifies and finalizes observer contexts" $
      passertEvalNoTrace cekObserverContextTransitions
  , testCase "CEK stage 6 initializes empty and nonempty mint contexts" $
      passertEvalNoTrace cekMintContextInitTransitions
  , testCase "CEK stage 8 groups, rolls over, and finalizes mint policies" $
      passertEvalNoTrace cekMintContextItemTransitions
  , testCase "CEK stage 9 authenticates redeemer and purpose selection" $
      passertEvalNoTrace cekRedeemerContextSelectionTransitions
  , testCase "CEK stage 9 completes a terminal redeemer proof" $
      passertEvalNoTrace cekRedeemerContextStepTransitions
  , testCase "CEK stage 10 finalizes a general script context" $
      passertEvalNoTrace cekContextFinalizeTransition
  , testCase "CEK stage 10 authenticates a Cardano spend descriptor" $
      passertEvalNoTrace cekContextFinalizeSpendTransition
  , testCase "CEK stage 11 assembles and binds transaction-info tail fields" $
      passertEvalNoTrace cekContextAssembleTransition
  , testCase "CEK stage 12 finalizes transaction info from compact scalars" $
      passertEvalNoTrace cekTxInfoFinalizeTransition
  , testCase "CEK stage 13 seeds the exact initial execution state" $
      passertEvalNoTrace cekContextSeedTransition
  , testGroup "validation-machine-v1 CEK maximum mint and one-step context fixtures"
      [ testCase "cek_context_maximum_mint_initializes_from_authenticated_frontier" $
          passertEvalNoTraceWithoutHoistChecks cekContextMaximumMintInitializesFromAuthenticatedFrontier
      , testCase "cek_context_maximum_mint_authenticates_last_asset_membership" $
          passertEvalNoTraceWithoutHoistChecks cekContextMaximumMintAuthenticatesLastAssetMembership
      , testCase "cek_context_seed_fits_one_step" $
          passertEvalNoTrace cekContextSeedFitsOneStep
      , testCase "cek_context_assemble_fits_one_step" $
          passertEvalNoTrace cekContextAssembleFitsOneStep
      , testCase "cek_context_tx_info_finalize_fits_one_step" $
          passertEvalNoTrace cekContextTxInfoFinalizeFitsOneStep
      ]
  ]

validationAuxiliaryWitnessAbi :: forall s. Term s PBool
validationAuxiliaryWitnessAbi =
  plet
    ( pcon $ Bounded.PChunkProofV1
        (pdata 1) (pdata 6) (pdata 0) (pdata 0)
        (pdata 0) (pdata $ pconstant "") (pdata pnil) (pdata pnil)
    )
    $ \chunkProof ->
  plet
    ( pcon $ PCekFinalContextControlV1
        (pdata cekEmptySummary) (pdata cekEmptySummary) (pdata cekEmptySummary)
    )
    $ \finalControl ->
    pand'List
      [ auxiliaryConstructorIs 0 0 (pcon PNoAuxiliaryWitness)
      , auxiliaryConstructorIs 19 1
          (pcon $ PCekContextFinalizeWitness $ pdata pinitialCekRedeemerContextControlV1)
      , auxiliaryConstructorIs 23 1
          (pcon $ PCekContextSeedWitness $ pdata finalControl)
      , auxiliaryConstructorIs 39 2
          ( pcon $ PMintFoldAssetWitness
              (pdata chunkProof) (pdata $ pcon PDNothing)
          )
      , signerProofConstructorIs 0 0 (pcon PNoSignerSetProof)
      , signerProofConstructorIs 5 6
          ( pcon $ PSignerBetweenProof
              (pdata pnil) (pdata 0) (pdata $ pconstant "")
              (pdata pnil) (pdata $ pconstant "") (pdata pnil)
          )
      ]

validationTailAuxiliaryCorpusAbi :: forall s. Term s PBool
validationTailAuxiliaryCorpusAbi =
  plet
    (pcon $ PValueAssetMutationWitnessV1 (pdata $ pconstant False) (pdata 0) (pdata pnil))
    $ \mutation ->
  plet
    ( pcon $ ProofFold.PProofFrameV1
        (pdata 1) (pdata 0) (pdata 0) (pdata 1)
        (pdata $ pcon $ PBranch (pdata 0) (pdata $ pconstant ""))
    )
    $ \proofFrame ->
  plet
    ( pcon $ PLedgerDeltaOperationProofV1
        (pdata pemptyProofDescriptor) (pdata 0) (pdata pnil) (pdata 0) (pdata pnil)
    )
    $ \operationProof ->
  plet
    ( pcon $ Bounded.PChunkProofV1
        (pdata 1) (pdata 5) (pdata 0) (pdata 1)
        (pdata 0) (pdata $ phexByteStr "12") (pdata pnil) (pdata pnil)
    )
    $ \chunkProof ->
  plet
    ( pcon $ PValueInputAssetWitness
        (pdata 0) (pdata $ phexByteStr "01") (pdata $ cekHash 0x41)
        (pdata $ phexByteStr "80") (pdata 0)
        (pdata $ preplicateBS # 28 # (pintegerToByte # 0x42))
        (pdata $ phexByteStr "abcd") (pdata 5)
        (pdata pnil) (pdata pnil) (pdata mutation)
    )
    $ \inputAsset ->
  plet
    ( pcon $ PValueOutputAssetWitness
        (pdata 1) (pdata $ phexByteStr "80") (pdata 0)
        (pdata $ preplicateBS # 28 # (pintegerToByte # 0x43))
        (pdata $ phexByteStr "beef") (pdata 7)
        (pdata pnil) (pdata pnil) (pdata mutation)
    )
    $ \outputAsset ->
  plet
    ( pcon $ PValueMintAssetWitness
        (pdata 2)
        (pdata $ preplicateBS # 28 # (pintegerToByte # 0x44))
        (pdata $ phexByteStr "cafe") (pdata $ pconstant (-5))
        (pdata pnil) (pdata mutation)
    )
    $ \mintAsset ->
  plet
    ( pcon $ PLedgerDeltaReplayWitness
        (pdata 1) (pdata $ phexByteStr "02")
        (pdata $ cekHash 0x45) (pdata $ phexByteStr "03")
    )
    $ \replay ->
  plet
    ( pcon $ PLedgerDeltaOutputWitness
        (pdata 3) (pdata $ phexByteStr "8100")
        (pdata $ pcons # pdata (cekHash 0x46) # pnil)
    )
    $ \output ->
  plet
    ( pcon $ PLedgerDeltaProofFrameWitness
        (pdata proofFrame) (pdata $ pcons # pdata (cekHash 0x47) # pnil)
    )
    $ \frame ->
  plet
    ( pcon $ PLedgerDeltaOperationWitness
        (pdata 0) (pdata $ phexByteStr "01") (pdata $ pconstant "")
        (pdata operationProof)
    )
    $ \operation ->
  plet
    ( pcon $ PValueOutputDescriptorWitness
        (pdata 4) (pdata $ phexByteStr "8101")
        (pdata $ pcons # pdata (cekHash 0x48) # pnil)
    )
    $ \descriptor ->
  plet
    (pcon $ PMintFoldAssetWitness (pdata chunkProof) (pdata $ pcon PDNothing))
    $ \mintFold ->
  plet
    ( pcons # pforgetData (pdata inputAsset)
        #$ pcons # pforgetData (pdata outputAsset)
        #$ pcons # pforgetData (pdata mintAsset)
        #$ pcons # pforgetData (pdata replay)
        #$ pcons # pforgetData (pdata output)
        #$ pcons # pforgetData (pdata frame)
        #$ pcons # pforgetData (pdata operation)
        #$ pcons # pforgetData (pdata descriptor)
        #$ pcons # pforgetData (pdata mintFold)
        # pnil
    )
    $ \items ->
      pand'List
        [ auxiliaryConstructorIs 24 11 inputAsset
        , auxiliaryConstructorIs 25 9 outputAsset
        , auxiliaryConstructorIs 26 6 mintAsset
        , auxiliaryConstructorIs 27 4 replay
        , auxiliaryConstructorIs 28 3 output
        , auxiliaryConstructorIs 34 2 frame
        , auxiliaryConstructorIs 35 4 operation
        , auxiliaryConstructorIs 38 3 descriptor
        , auxiliaryConstructorIs 39 2 mintFold
        , pblake2b_256 # (pserialiseData # (plistData # items))
            #== phexByteStr "8916ad7c26d34eafe62c93ed9c36be30d880fb102b918bb37f0b6d3dc27111e1"
        ]

terminalAcceptanceAndRejectionAbiVectors :: forall s. Term s PBool
terminalAcceptanceAndRejectionAbiVectors =
  plet (preplicateBS # 32 # (pintegerToByte # 0x71)) $ \operationHash ->
  plet (preplicateBS # 32 # (pintegerToByte # 0x72)) $ \postLedgerRoot ->
  plet (preplicateBS # 32 # (pintegerToByte # 0x73)) $ \priorLedgerRoot ->
  plet
    ( pencodeTerminalAcceptanceWitnessV1
        # postLedgerRoot # 2
        # (pcons # pdata (pcon $ Merkle.PFrontierPeak (pdata 1) (pdata operationHash)) # pnil)
    )
    $ \acceptance ->
  plet
    ( pencodeTerminalRejectionWitness
        # phexByteStr "455f56414c55455f4e4f545f505245534552564544"
        # priorLedgerRoot
    )
    $ \rejection ->
      pand'List
        [ acceptance
            #== phexByteStr "840140582072727272727272727272727272727272727272727272727272727272727272725827820281820158207171717171717171717171717171717171717171717171717171717171717171"
        , rejection
            #== phexByteStr "840255455f56414c55455f4e4f545f505245534552564544582073737373737373737373737373737373737373737373737373737373737373734180"
        , phashWorkWitness # pcon PTerminal # 9 # acceptance
            #== phexByteStr "0b3defd802c8cc6ee1112724ef19532be5b8f61817ab0282a56db645a2b20948"
        , phashWorkWitness # pcon PTerminal # 9 # rejection
            #== phexByteStr "6b15a4122dc6437ca54930248e9df11979d21dc484d5ea373373b43c489f1ce6"
        ]

valueAccumulatorUpdateAndControlAbi :: forall s. Term s PBool
valueAccumulatorUpdateAndControlAbi =
  plet tailValueAccumulator $ \accumulator ->
  plet
    ( pcon $ PValueAndMintControlV1
        (pdata tailNativeControl) (pdata 3) (pdata $ cekHash 0x55)
        (pdata 4) (pdata 5) (pdata $ cekHash 0x56)
        (pdata $ cekHash 0x57) (pdata $ cekHash 0x58)
        (pdata 6) (pdata 7) (pdata 8) (pdata accumulator)
    )
    $ \control ->
  plet (pencodeValueAndMintControlV1 # control) $ \controlCbor ->
    pand'List
      [ pencodeValueAccumulatorV1 # accumulator
          #== phexByteStr "8407582054545454545454545454545454545454545454545454545454545454545454540201"
      , valueAccumulatorUpdateConstructorIs 0 1
          (pcon $ PValueAccumulatorUpdated $ pdata accumulator)
      , valueAccumulatorUpdateConstructorIs 1 0 (pcon PValueAccumulatorAssetLimitExceeded)
      , valueAccumulatorUpdateConstructorIs 2 0 (pcon PValueAccumulatorMutationInvalid)
      , pblake2b_256 # controlCbor
          #== phexByteStr "d30dfeaa4f1f3323bf2824a1051ef943fee27a31779e171678abe4c05ba2b2e0"
      , pmatch (pdeserialise # controlCbor) $ \case
          PNothing -> pconstant False
          PJust dat -> plength # (pasList # dat) #== 12
      ]

ledgerDeltaControlOperationAndFrontierAbi :: forall s. Term s PBool
ledgerDeltaControlOperationAndFrontierAbi =
  plet tailPendingMutation $ \pending ->
  plet (pencodeLedgerDeltaPendingMutationV1 # pending) $ \pendingCbor ->
  plet
    ( pcon $ PLedgerDeltaControlV1
        (pdata 0) (pdata $ cekHash 0x61) (pdata 0) (pdata pnil)
        (pdata 1) (pdata $ cekHash 0x62) (pdata 0)
        (pdata $ cekHash 0x63) (pdata $ cekHash 0x64) (pdata $ cekHash 0x65)
        (pdata 0) (pdata 0) (pdata pnil) (pdata pendingCbor)
    )
    $ \control ->
  plet (pencodeLedgerDeltaControlV1 # control) $ \controlCbor ->
    pand'List
      [ pendingCbor #== phexByteStr "8a01000142010242030445840100008020404000"
      , pblake2b_256 # controlCbor
          #== phexByteStr "92e07c0c935ac73750a521ed638aed060414828d766774495885b56a04f5481b"
      , pmatch (pdeserialise # controlCbor) $ \case
          PNothing -> pconstant False
          PJust dat -> plength # (pasList # dat) #== 14
      , ledgerDeltaOperationVectors
      ]

valueAccumulatorUpdateConstructorIs :: forall s.
  Term s PInteger ->
  Term s PInteger ->
  Term s PValueAccumulatorUpdateV1 ->
  Term s PBool
valueAccumulatorUpdateConstructorIs expectedTag expectedFields update =
  pmatch (pasConstr # pforgetData (pdata update)) $ \(PBuiltinPair tag fields) ->
    tag #== expectedTag #&& plength # fields #== expectedFields

tailValueAccumulator :: forall s. Term s PValueAccumulatorV1
tailValueAccumulator = pcon $ PValueAccumulatorV1
  (pdata 7) (pdata $ cekHash 0x54) (pdata 2) (pdata 1)

tailPendingMutation :: forall s. Term s PLedgerDeltaPendingMutationV1
tailPendingMutation = pcon $ PLedgerDeltaPendingMutationV1
  (pdata 0)
  (pdata 1)
  (pdata $ phexByteStr "0102")
  (pdata $ phexByteStr "0304")
  (pdata pemptyProofDescriptor)
  ( pdata $ pcon $ ProofFold.PProofFoldControlV1
      { ProofFold.pfoldControl'nextFrameIndex = pdata (-1)
      , ProofFold.pfoldControl'expectedNextCursor = pdata 0
      , ProofFold.pfoldControl'includingRoot = pdata $ pconstant ""
      , ProofFold.pfoldControl'excludingRoot = pdata $ pconstant ""
      }
  )

tailNativeControl :: forall s. Term s PNativeScriptsControlV1
tailNativeControl = pcon $ PNativeScriptsControlV1
  (pdata $ phexByteStr "01")
  (pdata $ phexByteStr "02")
  (pdata $ phexByteStr "03")
  (pdata $ phexByteStr "04")
  (pdata 0) (pdata $ cekHash 0x51) (pdata 0) (pdata pnil)
  (pdata 0) (pdata $ cekHash 0x52) (pdata 0) (pdata pnil)
  (pdata 0) (pdata pnil) (pdata 0) (pdata pnil)
  (pdata 0) (pdata pnil) (pdata pnil)
  (pdata 0) (pdata pnil) (pdata 0) (pdata pnil)
  (pdata 0) (pdata 0) (pdata $ cekHash 0x53)

canonicalValidationControlsAbiVectors :: forall s. Term s PBool
canonicalValidationControlsAbiVectors =
  plet (pconstant validationAuxiliaryCorpusBytes) $ \corpusCbor ->
  pmatch (pdeserialise # corpusCbor) $ \case
    PNothing -> pconstant False
    PJust corpusData ->
      pand'List
        [ pvalidationAuxiliaryCorpusIsExact # (pasList # corpusData) # 0
        , pserialiseData # corpusData #== corpusCbor
        , pblake2b_256 # corpusCbor
            #== phexByteStr "9e3d884b15fa7d04c150e26adac1f0d4415dc94c48f6e7585a58d080ba31980f"
        , validationAuxiliaryWitnessAbi
        , resolveInputsWitnessEncoding
        , nativeScriptsControlCodecs
        , cekSubControlAbiVectors
        ]

pvalidationAuxiliaryCorpusIsExact :: forall s.
  Term s (PBuiltinList PData :--> PInteger :--> PBool)
pvalidationAuxiliaryCorpusIsExact = phoistAcyclic $ pfix $ \self ->
  plam $ \items expectedTag ->
    pelimList
      ( \item rest ->
          pmatch (pvalidationAuxiliaryWitnessFromData # item) $ \_ ->
          pmatch (pasConstr # item) $ \(PBuiltinPair tag _) ->
            tag #== expectedTag #&& self # rest # (expectedTag + 1)
      )
      (expectedTag #== 40)
      items

validationAuxiliaryCorpusBytes :: BS.ByteString
validationAuxiliaryCorpusBytes =
  Base16.decodeLenient $
      "9fd87980d87a9fd8799f01000100015820111111111111111111111111111111111111111111111111111111111111111180"
      <> "80ffd8799f010000010041128080ffffd87b9fd8799f01000100015820111111111111111111111111111111111111111111"
      <> "11111111111111111111118080ffd8799f010000010041128080ffd87980ffd87c9fd8799f010000010041128080ffd87a80"
      <> "d87980ffd87d9fd8799f41020100000000ffffd87e9f00410358202020202020202020202020202020202020202020202020"
      <> "202020202020202020410480d87980ffd87f9f01410558202121212121212121212121212121212121212121212121212121"
      <> "21212121212180ffd905009f0041065820222222222222222222222222222222222222222222222222222222222222222241"
      <> "07ffd905019f000058202323232323232323232323232323232323232323232323232323232323232323410980ffd905029f"
      <> "0000410a00582024242424242424242424242424242424242424242424242424242424242424240158202525252525252525"
      <> "25252525252525252525252525252525252525252525252580ffd905039f0001015820262626262626262626262626262626"
      <> "262626262626262626262626262626262680ffd905049f000000005820272727272727272727272727272727272727272727"
      <> "2727272727272727272727410b800000410c0158203030303030303030303030303030303030303030303030303030303030"
      <> "303030805820282828282828282828282828282828282828282828282828282828282828282880d8799f0100000100411280"
      <> "80ffffd905059fd8799fd8799f00005820141414141414141414141414141414141414141414141414141414141414141458"
      <> "2015151515151515151515151515151515151515151515151515151515151515155820161616161616161616161616161616"
      <> "1616161616161616161616161616161616000000ffd8799f0000582014141414141414141414141414141414141414141414"
      <> "1414141414141414141458201515151515151515151515151515151515151515151515151515151515151515582016161616"
      <> "16161616161616161616161616161616161616161616161616161616000000ffd87f80ffffd905069f0000410e410f80ffd9"
      <> "05079f00411080ffd905089f80005820292929292929292929292929292929292929292929292929292929292929292980ff"
      <> "d905099f00411141120180ffd9050a9fd8799f00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea09381571694527914"
      <> "0f31aec8cbaba2000000ff4040d8799f400000ffd8799f400000ffff00010158202a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a"
      <> "2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a8000000058202b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b"
      <> "2b2b2b411380ffd9050b9fd87a80d8799f010000000101582013131313131313131313131313131313131313131313131313"
      <> "131313131313130000000000000000d87a80ffd8799fd87c80d87a80d87a80ffffd9050c9fd8799f00d8799f5820bbcb3bff"
      <> "6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ff4040d8799f400000ffd8799f400000ffffff"
      <> "d9050d9fd8799f00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ff40"
      <> "40d8799f400000ffd8799f400000ffff004114411580ffd9050e9fd8799fd8799f40000000ffd8799f400000ffd8799f4000"
      <> "00ffffffd9050f9fd8799fd8799f40000000ffd8799f400000ffd8799f400000ffffffd905109fd8799fd8799f400000ffd8"
      <> "799f400000ffd8799f400000ffffffd905119f00411658202c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c"
      <> "2c2c2c2c2c2c41170041184119018080d8799fd879800080ffffd905129f00411a00411b411c018080d8799fd879800080ff"
      <> "ffd905139f00411d411e0180d8799fd879800080ffffd905149f01411f58202d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d"
      <> "2d2d2d2d2d2d2d2d2d2d2d2d2d4120ffd905159f00412180ffd905169fd8799f010001000158201111111111111111111111"
      <> "1111111111111111111111111111111111111111118080ffffd905179fd8799f010001000158201111111111111111111111"
      <> "1111111111111111111111111111111111111111118080ff4122ffd905189f000158202e2e2e2e2e2e2e2e2e2e2e2e2e2e2e"
      <> "2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e80ffd905199fd87980ffd9051a9f4122d87980ffd9051b9fd8799f01000001d879"
      <> "9f0040ffff80ffd9051c9f0041234124d8799fd8799f01000080ff00800080ffffd9051d9fd8799f010000010041128080ff"
      <> "d87a80ffd9051e9f0000000058202f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f41258000"
      <> "0041260158203030303030303030303030303030303030303030303030303030303030303030805820313131313131313131"
      <> "313131313131313131313131313131313131313131313180d87a8080ffd9051f9f00412780ffd905209fd8799f0100000100"
      <> "41128080ffd87a80ffff"

malformedAuxiliaryAdjacentTag :: forall s. Term s PUnit
malformedAuxiliaryAdjacentTag = forceAuxiliaryData $ PD.Constr 40 []

malformedAuxiliaryWrongArity :: forall s. Term s PUnit
malformedAuxiliaryWrongArity = forceAuxiliaryData $ PD.Constr 0 [PD.I 0]

malformedTailAuxiliaryWrongArity :: forall s. Term s PUnit
malformedTailAuxiliaryWrongArity = forceAuxiliaryData $ PD.Constr 24 [PD.I 0]

forceAuxiliaryData :: forall s. PD.Data -> Term s PUnit
forceAuxiliaryData dat =
  pmatch
    (pvalidationAuxiliaryWitnessFromData # pconstant @PData dat)
    (const $ pconstant ())

auxiliaryCanonicalConstructorZero :: forall s. Term s PBool
auxiliaryCanonicalConstructorZero =
  (pvalidationAuxiliaryWitnessFromData # pconstant @PData (PD.Constr 0 []))
    #== pcon PNoAuxiliaryWitness

auxiliaryConstructorIs :: forall s.
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationAuxiliaryWitnessV1 ->
  Term s PBool
auxiliaryConstructorIs expectedTag expectedFields witness =
  pmatch (pasConstr # pforgetData (pdata witness)) $ \(PBuiltinPair tag fields) ->
    tag #== expectedTag #&& plength # fields #== expectedFields

signerProofConstructorIs :: forall s.
  Term s PInteger ->
  Term s PInteger ->
  Term s PSignerSetProofV1 ->
  Term s PBool
signerProofConstructorIs expectedTag expectedFields proof =
  pmatch (pasConstr # pforgetData (pdata proof)) $ \(PBuiltinPair tag fields) ->
    tag #== expectedTag #&& plength # fields #== expectedFields

validityIntervalSentinels :: forall s. Term s PBool
validityIntervalSentinels = pand'List
  [ pnot #$ pvalidityIntervalIsMalformed # (-1) # (-1)
  , pnot #$ pvalidityIntervalIsMalformed # 0 # 0
  , pnot #$ pvalidityIntervalIsMalformed # 0 # 1
  , pvalidityIntervalIsMalformed # (-2) # (-1)
  , pvalidityIntervalIsMalformed # (-1) # (-2)
  , pvalidityIntervalIsMalformed # 1 # 0
  ]

purposeKindMapping :: forall s. Term s PBool
purposeKindMapping = pand'List
  [ predeemerTagForPurposeKindV1 # 0 #== pcon (PJust 0)
  , predeemerTagForPurposeKindV1 # 1 #== pcon (PJust 1)
  , predeemerTagForPurposeKindV1 # 2 #== pcon (PJust 3)
  , predeemerTagForPurposeKindV1 # 3 #== pcon (PJust 6)
  , predeemerTagForPurposeKindV1 # (-1) #== pcon PNothing
  , predeemerTagForPurposeKindV1 # 4 #== pcon PNothing
  , predeemerPointerMatchesPurposeV1 # 0 # 7 # 0 # 7
  , predeemerPointerMatchesPurposeV1 # 1 # 7 # 1 # 7
  , predeemerPointerMatchesPurposeV1 # 2 # 7 # 3 # 7
  , predeemerPointerMatchesPurposeV1 # 3 # 7 # 6 # 7
  , pnot #$ predeemerPointerMatchesPurposeV1 # 0 # 7 # 0 # 8
  , pnot #$ predeemerPointerMatchesPurposeV1 # 1 # 7 # 1 # 8
  , pnot #$ predeemerPointerMatchesPurposeV1 # 2 # 7 # 3 # 8
  , pnot #$ predeemerPointerMatchesPurposeV1 # 3 # 7 # 6 # 8
  , pnot #$ predeemerPointerMatchesPurposeV1 # (-1) # 7 # 0 # 7
  , pnot #$ predeemerPointerMatchesPurposeV1 # 4 # 7 # 6 # 7
  , pnot #$ predeemerPointerMatchesPurposeV1 # 0 # 7 # (-1) # 7
  , pnot #$ predeemerPointerMatchesPurposeV1 # 0 # 7 # 2 # 7
  , pnot #$ predeemerPointerMatchesPurposeV1 # 0 # 7 # 4 # 7
  , pnot #$ predeemerPointerMatchesPurposeV1 # 0 # 7 # 5 # 7
  ]

observerScanEncoding :: forall s. Term s PBool
observerScanEncoding =
  plet (pencodeObserverPurposeScanControl # pemptyObserverPurposeScanControl) $ \encoded ->
  pmatch (pdeserialise # encoded) $ \case
    PNothing -> pconstant False
    PJust dat -> encoded #== pconstant "\x83\x00\x40\x00"
      #&& plength # (pasList # dat) #== 3

receiveScanEncoding :: forall s. Term s PBool
receiveScanEncoding =
  pencodeReceivePurposeScanControl # pemptyReceivePurposeScanControl
    #== pconstant "\x86\x00\x80\x00\x40\x40\x80"

resolvedInputAccumulatorVector :: forall s. Term s PBool
resolvedInputAccumulatorVector =
  plet pinitialResolutionAccumulator $ \initial ->
    initial #== phexByteStr "07eb401e2f7e5de17444414ec48a5d9dca455dea72f4675cc2b08bf5b4e39979"
      #&& presolvedInputAccumulatorSuccessor
        # initial # 0 # phexByteStr "010203" # phexByteStr "040506"
        #== phexByteStr "97e2dbdabf1ac8b5046e02f46c8d081ade2d81296b174bf77b9b8c69bd59c9c0"

mintAssetLeafVector :: forall s. Term s PBool
mintAssetLeafVector =
  pmintAssetLeafHash
    # (preplicateBS # 28 # (pintegerToByte # 17))
    # phexByteStr "abcd"
    # (-7)
    #== phexByteStr "4813bd9aad26eea82fa41280aefd50c848041a2a6cf27be416e1873c2876a479"

ledgerDeltaOperationVectors :: forall s. Term s PBool
ledgerDeltaOperationVectors =
  plet pemptyProofDescriptor $ \descriptor ->
  plet (pledgerDeltaOperationLeafHash # 0 # phexByteStr "010203" # pconstant "" # descriptor) $ \deletion ->
  plet (pledgerDeltaOperationLeafHash # 1 # phexByteStr "0405" # phexByteStr "060708" # descriptor) $ \insertion ->
  plet (Merkle.pbuildFrontier #$ pcons # pdata deletion #$ pcons # pdata insertion # pnil) $ \frontier ->
  pmatch frontier $ \built ->
    pand'List
      [ deletion #== phexByteStr "d70952a4347195627444cfbb1874f6857de1ad78f095460b76fc826cd267a589"
      , insertion #== phexByteStr "f8bc7029f5f58f0436ebdf6cbbb85bd9adac05d5f6dc1b9238c8166a517aa8db"
      , Merkle.pfrontierCommitment # Merkle.pbuiltFrontier'count built # Merkle.pbuiltFrontier'peaks built
          #== phexByteStr "b6d017c71f3fc974f620b22764385bf9ad56ee5627009e57dbeb9418e486dcb2"
      ]

parameterizedMinAdaBoundary :: forall s. Term s PBool
parameterizedMinAdaBoundary =
  plet (plengthBS # (pencodeMidgardTxOutput # minAdaOutput)) $ \outputBytes ->
  plet (pminAdaLovelaceV1 # 4_310 # outputBytes) $ \floor ->
    pand'List
      [ pminAdaOutputOverheadBytes #== 160
      , floor #== 4_310 * (160 + outputBytes)
      , poutputMeetsMinAdaV1 # 4_310 # outputBytes # floor
      , pnot #$ poutputMeetsMinAdaV1 # 4_310 # outputBytes # (floor - 1)
      , poutputMeetsMinAdaV1 # 4_310 # outputBytes # (floor + 1)
      , pminAdaLovelaceV1 # 4_310 # (outputBytes + 1) - floor #== 4_310
      , pminAdaLovelaceV1 # 4_310 # 0 #== 689_600
      ]

minAdaOutput :: forall s. Term s PMidgardTxOutput
minAdaOutput = pcon $ PMidgardTxOutput
  (pdata $ pcon $ PMidgardAddress
    (pdata $ pconstant False)
    (pdata 0)
    (pdata $ pcon $ PMidgardPubKeyCredential $ pdata $ pconstant $ BS.replicate 28 0xaa)
    (pdata $ pcon PDNothing))
  (pdata $ pcon $ PMidgardValue (pdata 2_000_000) (pdata $ pcon $ PAssocMap pnil))
  (pdata $ pcon PDNothing)
  (pdata $ pcon PDNothing)

earlyPhaseWitnessEncodingVectors :: forall s. Term s PBool
earlyPhaseWitnessEncodingVectors = pand'List
  [ pencodeStaticRulesWitness
      # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
      #== phexByteStr "844101410241034104"
  , pencodeInputSetsScanWitness
      # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
      # 0 # (-1) # 0 # 1
      # (preplicateBS # 38 # (pintegerToByte # 0xaa))
      # (preplicateBS # 32 # (pintegerToByte # 0xbb))
      #== ( phexByteStr "8a4101410241034104002000015826"
              <> (preplicateBS # 38 # (pintegerToByte # 0xaa))
              <> phexByteStr "5820"
              <> (preplicateBS # 32 # (pintegerToByte # 0xbb))
          )
  , pencodeSignaturesScanWitness
      # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
      # (preplicateBS # 32 # (pintegerToByte # 0xcc))
      # 1 # (-1) # 2 # 0 # 1 # phexByteStr "05" # pconstant "" # 0 # pnil # 1
      #== ( phexByteStr "8f41014102410341045820"
              <> (preplicateBS # 32 # (pintegerToByte # 0xcc))
              <> phexByteStr "0120020001410540008001"
          )
  , pencodePhaseAScriptPreconditionsWitness
      # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
      # (preplicateBS # 32 # (pintegerToByte # 0xcc))
      # 2
      # (preplicateBS # 32 # (pintegerToByte # 0xdd))
      # pconstant True # 3 # 1
      # (preplicateBS # 28 # (pintegerToByte # 0xee))
      #== ( phexByteStr "8b41014102410341045820"
              <> (preplicateBS # 32 # (pintegerToByte # 0xcc))
              <> phexByteStr "025820"
              <> (preplicateBS # 32 # (pintegerToByte # 0xdd))
              <> phexByteStr "01581c"
              <> (preplicateBS # 28 # (pintegerToByte # 0xee))
              <> phexByteStr "0301"
          )
  , pencodeTerminalRejectionWitness
      # phexByteStr "455252"
      # (preplicateBS # 32 # (pintegerToByte # 0xff))
      #== ( phexByteStr "8402434552525820"
              <> (preplicateBS # 32 # (pintegerToByte # 0xff))
              <> phexByteStr "4180"
          )
  ]

malformedInputSetsWitness :: forall s. Term s PByteString
malformedInputSetsWitness =
  pencodeInputSetsScanWitness
    # pconstant "" # pconstant "" # pconstant "" # pconstant ""
    # (-2) # 0 # 0 # 0 # pconstant "" # (preplicateBS # 32 # (pintegerToByte # 0))

transactionResolutionScheduleOrdering :: forall s. Term s PBool
transactionResolutionScheduleOrdering =
  plet (pencodeMidgardTxInput # scheduleInputA) $ \keyA ->
  plet (pencodeMidgardTxInput # scheduleInputB) $ \keyB ->
  plet
    (presolutionScheduleNodeHash # 1 # keyA
      # (presolutionScheduleNodeHash # 0 # keyB # pemptyResolutionScheduleHash))
    $ \orderedExpected ->
  plet
    (presolutionScheduleNodeHash # 0 # keyA
      # (presolutionScheduleNodeHash # 1 # keyA # pemptyResolutionScheduleHash))
    $ \equalKeyExpected ->
      pand'List
        [ ptransactionResolutionScheduleHash # pnil # pnil #== pemptyResolutionScheduleHash
        , ptransactionResolutionScheduleHash
            # (pcons # pdata scheduleInputB # pnil)
            # (pcons # pdata scheduleInputA # pnil)
            #== orderedExpected
        , ptransactionResolutionScheduleHash
            # (pcons # pdata scheduleInputB #$ pcons # pdata scheduleInputA # pnil)
            # pnil
            #== ptransactionResolutionScheduleHash
              # (pcons # pdata scheduleInputA #$ pcons # pdata scheduleInputB # pnil)
              # pnil
        , ptransactionResolutionScheduleHash
            # (pcons # pdata scheduleInputA # pnil)
            # (pcons # pdata scheduleInputA # pnil)
            #== equalKeyExpected
        ]

scheduleInputA, scheduleInputB :: forall s. Term s PMidgardTxInput
scheduleInputA = pcon $ PMidgardTxInput
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xaa))
  (pdata 0)
scheduleInputB = pcon $ PMidgardTxInput
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xbb))
  (pdata 1)

resolveInputsWitnessEncoding :: forall s. Term s PBool
resolveInputsWitnessEncoding =
  plet activeResolveOutputProof $ \active ->
  plet (pencodeResolveInputOutputProof # active) $ \activeCbor ->
  plet
    ( phexByteStr "8b4101410241034104005820"
        <> (preplicateBS # 32 # (pintegerToByte # 0xaa))
        <> phexByteStr "5820"
        <> (preplicateBS # 32 # (pintegerToByte # 0xbb))
        <> phexByteStr "005820"
        <> (preplicateBS # 32 # (pintegerToByte # 0xcc))
    )
    $ \prefix ->
  plet (phexByteStr "5820" <> (preplicateBS # 32 # (pintegerToByte # 0xdd))) $ \suffix ->
    pand'List
      [ pencodeResolveInputsWitness
          # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
          # 0
          # (preplicateBS # 32 # (pintegerToByte # 0xaa))
          # (preplicateBS # 32 # (pintegerToByte # 0xbb))
          # 0
          # (preplicateBS # 32 # (pintegerToByte # 0xcc))
          # pcon PDNothing
          # (preplicateBS # 32 # (pintegerToByte # 0xdd))
          #== prefix <> phexByteStr "4100" <> suffix
      , pencodeResolveInputsWitness
          # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
          # 0
          # (preplicateBS # 32 # (pintegerToByte # 0xaa))
          # (preplicateBS # 32 # (pintegerToByte # 0xbb))
          # 0
          # (preplicateBS # 32 # (pintegerToByte # 0xcc))
          # (pcon $ PDJust $ pdata active)
          # (preplicateBS # 32 # (pintegerToByte # 0xdd))
          #== prefix <> (Codec.pencodeDefiniteBytes # activeCbor) <> suffix
      , psliceBS # 0 # 1 # activeCbor #== phexByteStr "85"
      , pdecodeResolveInputOutputProof # activeCbor #== active
      ]

activeResolveOutputProof :: forall s. Term s PResolveInputOutputProofV1
activeResolveOutputProof = pcon $ PResolveInputOutputProofV1
  (pdata 0)
  (pdata $ phexByteStr "05")
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xee))
  (pdata $ phexByteStr "06")
  (pdata $ LedgerOutputProof.pinitialControlV1
    # 0 # 1 # (preplicateBS # 32 # (pintegerToByte # 0x11)))

malformedResolveInputsWitness :: forall s. Term s PByteString
malformedResolveInputsWitness =
  pencodeResolveInputsWitness
    # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
    # 0 # (preplicateBS # 31 # (pintegerToByte # 0x41))
    # (preplicateBS # 32 # (pintegerToByte # 0x42)) # 0
    # (preplicateBS # 32 # (pintegerToByte # 0x43)) # pcon PDNothing
    # (preplicateBS # 32 # (pintegerToByte # 0x44))

phaseANativeScriptsWitnessEncoding :: forall s. Term s PBool
phaseANativeScriptsWitnessEncoding =
  pencodePhaseANativeScriptsScanWitness
    # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
    # (preplicateBS # 32 # (pintegerToByte # 0xaa))
    # 0 # (-1) # 0 # 0 # 0 # pconstant "" # 0 # pconstant "" # 0 # 0 # (-1)
    # 0 # pnil # phexByteStr "05"
    #== ( phexByteStr "9f41014102410341045820"
            <> (preplicateBS # 32 # (pintegerToByte # 0xaa))
            <> phexByteStr "00200000004000400000200082804105ff"
        )

malformedPhaseANativeScriptsWitness :: forall s. Term s PByteString
malformedPhaseANativeScriptsWitness =
  pencodePhaseANativeScriptsScanWitness
    # pconstant "" # pconstant "" # pconstant "" # pconstant ""
    # (preplicateBS # 32 # (pintegerToByte # 0))
    # 0 # 0 # 0 # 0 # 0 # pconstant "" # 0 # pconstant "" # 0 # 0 # 2
    # 0 # pnil # pconstant ""

controlDecodingVectors :: forall s. Term s PBool
controlDecodingVectors =
  plet validContextCbor $ \contextCbor ->
  plet
    ( pencodeInputSetsScanWitness
        # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # contextCbor
        # (-1) # 0 # 0 # 0 # pconstant ""
        # (preplicateBS # 32 # (pintegerToByte # 0xaa))
    )
    $ \inputSetsCbor ->
      pand'List
        [ pdecodeValidationContext # contextCbor
            #== pcon (PValidationContextV1 (pdata 1_000) (pdata 0) (pdata 44) (pdata 155_381) (pdata 7))
        , pinputSetsControlFromWitness # inputSetsCbor
            #== pcon
              ( PInputSetsControlV1
                  (pdata $ phexByteStr "01")
                  (pdata $ phexByteStr "02")
                  (pdata $ phexByteStr "03")
                  (pdata contextCbor)
                  (pdata (-1))
                  (pdata 0)
                  (pdata 0)
                  (pdata 0)
                  (pdata $ pconstant "")
                  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xaa))
              )
        ]

validContextCbor :: forall s. Term s PByteString
validContextCbor =
  pconstant "\x87"
    <> Codec.pcborInt 1
    <> (Codec.pencodeDefiniteBytes # pconstant "midgard-consensus-v1")
    <> Codec.pcborInt 1_000
    <> Codec.pcborInt 0
    <> Codec.pcborInt 44
    <> Codec.pcborInt 155_381
    <> Codec.pcborInt 7

malformedValidationContext :: forall s. Term s PValidationContextV1
malformedValidationContext =
  pdecodeValidationContext
    # ( pconstant "\x87"
          <> Codec.pcborInt 1
          <> (Codec.pencodeDefiniteBytes # pconstant "midgard-consensus-v1")
          <> Codec.pcborInt 1_000
          <> Codec.pcborInt 2
          <> Codec.pcborInt 44
          <> Codec.pcborInt 155_381
          <> Codec.pcborInt 7
      )

signaturesControlDecoding :: forall s. Term s PBool
signaturesControlDecoding =
  plet
    (pcons # pdata (pcon $ Merkle.PFrontierPeak (pdata 0)
      (pdata $ preplicateBS # 32 # (pintegerToByte # 0xcc))) # pnil)
    $ \peaks ->
  plet
    ( pencodeSignaturesScanWitness
        # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
        # (preplicateBS # 32 # (pintegerToByte # 0xaa))
        # 0 # 1 # (-1) # 1 # 0 # phexByteStr "05"
        # (preplicateBS # 28 # (pintegerToByte # 0xbb))
        # 1 # peaks # 0
    )
    $ \witnessCbor ->
      psignaturesControlFromWitness # witnessCbor
        #== pcon
          ( PSignaturesControlV1
              (pdata $ phexByteStr "01")
              (pdata $ phexByteStr "02")
              (pdata $ phexByteStr "03")
              (pdata $ phexByteStr "04")
              (pdata $ preplicateBS # 32 # (pintegerToByte # 0xaa))
              (pdata 0)
              (pdata 1)
              (pdata (-1))
              (pdata 1)
              (pdata 0)
              (pdata $ phexByteStr "05")
              (pdata $ preplicateBS # 28 # (pintegerToByte # 0xbb))
              (pdata 1)
              (pdata peaks)
              (pdata 0)
          )

phaseANativeControlDecoding :: forall s. Term s PBool
phaseANativeControlDecoding =
  plet
    (pcons # pdata (pcon $ Merkle.PFrontierPeak (pdata 0)
      (pdata $ preplicateBS # 32 # (pintegerToByte # 0xcc))) # pnil)
    $ \peaks ->
  plet
    ( pencodePhaseANativeScriptsScanWitness
        # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
        # (preplicateBS # 32 # (pintegerToByte # 0xaa))
        # 2 # 1 # 0 # 0 # 12
        # (preplicateBS # 32 # (pintegerToByte # 0xbb))
        # 7 # (preplicateBS # 32 # (pintegerToByte # 0xdd))
        # 1 # 3 # 1 # 1 # peaks # phexByteStr "0506"
    )
    $ \witnessCbor ->
      pphaseANativeControlFromWitness # witnessCbor
        #== pcon
          ( PPhaseANativeScriptsControlV1
              (pdata $ phexByteStr "01")
              (pdata $ phexByteStr "02")
              (pdata $ phexByteStr "03")
              (pdata $ phexByteStr "04")
              (pdata $ preplicateBS # 32 # (pintegerToByte # 0xaa))
              (pdata 2)
              (pdata 1)
              (pdata 0)
              (pdata 0)
              (pdata 12)
              (pdata $ preplicateBS # 32 # (pintegerToByte # 0xbb))
              (pdata 7)
              (pdata $ preplicateBS # 32 # (pintegerToByte # 0xdd))
              (pdata 1)
              (pdata 3)
              (pdata 1)
              (pdata 1)
              (pdata peaks)
              (pdata $ phexByteStr "0506")
          )

handoffControlDecoding :: forall s. Term s PBool
handoffControlDecoding =
  plet
    ( pencodePhaseAScriptPreconditionsWitness
        # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
        # (preplicateBS # 32 # (pintegerToByte # 0xaa))
        # 2 # (preplicateBS # 32 # (pintegerToByte # 0xbb))
        # pconstant True # 3 # 1
        # (preplicateBS # 28 # (pintegerToByte # 0xcc))
    )
    $ \preconditionsCbor ->
  plet
    ( pencodeResolveInputsWitness
        # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
        # 7
        # (preplicateBS # 32 # (pintegerToByte # 0xdd))
        # (preplicateBS # 32 # (pintegerToByte # 0xee))
        # 2
        # (preplicateBS # 32 # (pintegerToByte # 0xbb))
        # pcon PDNothing
        # (preplicateBS # 32 # (pintegerToByte # 0xaa))
    )
    $ \resolveCbor ->
      pand'List
        [ pphaseAScriptPreconditionsControlFromWitness # preconditionsCbor
            #== pcon
              ( PPhaseAScriptPreconditionsControlV1
                  (pdata $ phexByteStr "01")
                  (pdata $ phexByteStr "02")
                  (pdata $ phexByteStr "03")
                  (pdata $ phexByteStr "04")
                  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xaa))
                  (pdata 2)
                  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xbb))
                  (pdata 1)
                  (pdata 3)
                  (pdata 1)
                  (pdata $ preplicateBS # 28 # (pintegerToByte # 0xcc))
              )
        , presolveInputsControlFromWitness # resolveCbor
            #== pcon
              ( PResolveInputsControlV1
                  (pdata $ phexByteStr "01")
                  (pdata $ phexByteStr "02")
                  (pdata $ phexByteStr "03")
                  (pdata $ phexByteStr "04")
                  (pdata 7)
                  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xdd))
                  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xee))
                  (pdata 2)
                  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xbb))
                  (pdata $ pcon PDNothing)
                  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xaa))
              )
        ]

valueAccumulatorCodec :: forall s. Term s PBool
valueAccumulatorCodec =
  plet
    ( pcon $ PValueAccumulatorV1
        (pdata (-7))
        (pdata $ preplicateBS # 32 # (pintegerToByte # 0xaa))
        (pdata 3)
        (pdata 2)
    )
    $ \accumulator ->
  plet (pencodeValueAccumulatorV1 # accumulator) $ \encoded ->
    encoded
      #== ( phexByteStr "84265820"
              <> (preplicateBS # 32 # (pintegerToByte # 0xaa))
              <> phexByteStr "0302"
          )
      #&& pvalueAccumulatorFromCbor # encoded #== accumulator

malformedValueAccumulator :: forall s. Term s PValueAccumulatorV1
malformedValueAccumulator =
  pvalueAccumulatorFromCbor
    # ( phexByteStr "84005820"
          <> (preplicateBS # 32 # (pintegerToByte # 0))
          <> phexByteStr "0102"
      )

ledgerDeltaControlCodecs :: forall s. Term s PBool
ledgerDeltaControlCodecs =
  plet emptyLedgerDeltaPendingMutation $ \pending ->
  plet (pencodeLedgerDeltaPendingMutationV1 # pending) $ \pendingCbor ->
  plet (emptyLedgerDeltaControl pendingCbor) $ \control ->
  plet (pencodeLedgerDeltaControlV1 # control) $ \controlCbor ->
  plet (preplicateBS # 32 # (pintegerToByte # 0xff)) $ \postRoot ->
    pand'List
      [ pledgerDeltaPendingMutationFromCbor # pendingCbor #== pending
      , pledgerDeltaControlFromWitness # controlCbor #== control
      , pencodeTerminalAcceptanceWitnessV1 # postRoot # 0 # pnil
          #== ( phexByteStr "8401405820"
                  <> postRoot
                  <> phexByteStr "43820080"
              )
      ]

emptyLedgerDeltaPendingMutation :: forall s. Term s PLedgerDeltaPendingMutationV1
emptyLedgerDeltaPendingMutation = pcon $ PLedgerDeltaPendingMutationV1
  (pdata 0)
  (pdata 1)
  (pdata $ phexByteStr "05")
  (pdata $ phexByteStr "06")
  (pdata pemptyProofDescriptor)
  ( pdata $ pcon $ ProofFold.PProofFoldControlV1
      { ProofFold.pfoldControl'nextFrameIndex = pdata (-1)
      , ProofFold.pfoldControl'expectedNextCursor = pdata 0
      , ProofFold.pfoldControl'includingRoot = pdata $ pconstant ""
      , ProofFold.pfoldControl'excludingRoot = pdata $ pconstant ""
      }
  )

emptyLedgerDeltaControl :: forall s. Term s PByteString -> Term s PLedgerDeltaControlV1
emptyLedgerDeltaControl pendingCbor = pcon $ PLedgerDeltaControlV1
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xaa))
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xbb))
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xcc))
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xdd))
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0xee))
  (pdata 0)
  (pdata 0)
  (pdata pnil)
  (pdata pendingCbor)

malformedLedgerDeltaPendingMutation :: forall s. Term s PLedgerDeltaPendingMutationV1
malformedLedgerDeltaPendingMutation =
  plet
    ( pcon $ PLedgerDeltaPendingMutationV1
        (pdata 0)
        (pdata 0)
        (pdata $ phexByteStr "05")
        (pdata $ pconstant "")
        (pdata pemptyProofDescriptor)
        ( pdata $ pcon $ ProofFold.PProofFoldControlV1
            { ProofFold.pfoldControl'nextFrameIndex = pdata 0
            , ProofFold.pfoldControl'expectedNextCursor = pdata 0
            , ProofFold.pfoldControl'includingRoot = pdata $ pconstant ""
            , ProofFold.pfoldControl'excludingRoot = pdata $ pconstant ""
            }
        )
    )
    $ \pending ->
      pledgerDeltaPendingMutationFromCbor # (pencodeLedgerDeltaPendingMutationV1 # pending)

nativeScriptsControlCodecs :: forall s. Term s PBool
nativeScriptsControlCodecs =
  plet (emptyNativeScriptsControl 0) $ \control ->
  plet (pencodeNativeScriptsControlV1 # control) $ \controlCbor ->
  plet (preplicateBS # 32 # (pintegerToByte # 0xaa)) $ \firstHash ->
  plet (preplicateBS # 32 # (pintegerToByte # 0xbb)) $ \secondHash ->
    pand'List
      [ psliceBS # 0 # 2 # controlCbor #== phexByteStr "981a"
      , pnativeScriptsControlFromWitness # controlCbor #== control
      , pnativeScriptsControlIsWellFormed # control
      , pnot #$ pnativeScriptsControlIsWellFormed # (emptyNativeScriptsControl 4)
      , pencodeScriptIntegrityWitnessV1 # control
          #== pconstant "\x82" <> (Codec.pencodeDefiniteBytes # controlCbor) <> phexByteStr "00"
      , pencodeScriptIntegrityCompactWitnessV1 # control
          #== pconstant "\x82" <> (Codec.pencodeDefiniteBytes # controlCbor) <> phexByteStr "01"
      , pencodeScriptIntegrityWitnessSetWitnessV1 # control # firstHash # secondHash
          #== pconstant "\x84" <> (Codec.pencodeDefiniteBytes # controlCbor)
            <> phexByteStr "025820" <> firstHash <> phexByteStr "5820" <> secondHash
      , pencodeScriptIntegrityFinalizeWitnessV1 # control # firstHash # secondHash
          #== pconstant "\x84" <> (Codec.pencodeDefiniteBytes # controlCbor)
            <> phexByteStr "035820" <> firstHash <> phexByteStr "5820" <> secondHash
      , pencodeLedgerDeltaWitnessV1 # control # firstHash # secondHash
          #== pencodeLedgerDeltaControlV1
            # ( pcon $ PLedgerDeltaControlV1
                  (pdata 0)
                  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x11))
                  (pdata 0)
                  (pdata pnil)
                  (pdata 0)
                  (pdata secondHash)
                  (pdata 0)
                  (pdata pinitialResolutionAccumulator)
                  (pdata secondHash)
                  (pdata firstHash)
                  (pdata 0)
                  (pdata 0)
                  (pdata pnil)
                  (pdata $ pconstant "")
              )
      ]

malformedScriptSourcesStage :: forall s. Term s PByteString
malformedScriptSourcesStage =
  pencodeScriptSourcesWitness
    # phexByteStr "01" # phexByteStr "02" # phexByteStr "03" # phexByteStr "04"
    # 0 # (cekHash 0x45) # 0 # (cekHash 0x46) # pnil
    # (-1) # 0 # pnil # 0 # pnil
    # 0 # (cekHash 0x47) # (cekHash 0x48) # 0 # 0 # pnil
    # 0 # 0 # pnil # 0 # pemptyReceivePurposeScanControl
    # 0 # 0 # pemptyObserverPurposeScanControl # pemptyMintFoldControl # (cekHash 0x49)

malformedScriptIntegrityHash :: forall s. Term s PByteString
malformedScriptIntegrityHash =
  pencodeScriptIntegrityWitnessSetWitnessV1
    # emptyNativeScriptsControl 0
    # phexByteStr "00"
    # cekHash 0x4e

emptyNativeScriptsControl :: forall s. Term s PInteger -> Term s PNativeScriptsControlV1
emptyNativeScriptsControl languageBitmap = nativeScriptsControlFixture (phexByteStr "01") languageBitmap 0

nativeScriptsControlWithRedeemerCount :: forall s.
  Term s PInteger -> Term s PInteger -> Term s PNativeScriptsControlV1
nativeScriptsControlWithRedeemerCount languageBitmap redeemerCount =
  nativeScriptsControlFixture (phexByteStr "01") languageBitmap redeemerCount

nativeScriptsControlWithCompactCbor :: forall s.
  Term s PByteString -> Term s PNativeScriptsControlV1
nativeScriptsControlWithCompactCbor compactCbor = nativeScriptsControlFixture compactCbor 0 0

nativeScriptsControlWithResolvedItem :: forall s.
  Term s PByteString -> Term s PNativeScriptsControlV1
nativeScriptsControlWithResolvedItem leaf = pcon $ PNativeScriptsControlV1
  (pdata $ phexByteStr "01")
  (pdata $ phexByteStr "02")
  (pdata $ phexByteStr "03")
  (pdata $ phexByteStr "04")
  (pdata 1)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x11))
  (pdata 1)
  (pdata $ pcons # pdata (pcon $ Merkle.PFrontierPeak (pdata 0) (pdata leaf)) # pnil)
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x22))
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x33))

nativeScriptsControlForContext :: forall s.
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PNativeScriptsControlV1
nativeScriptsControlForContext resolvedCount spendCount resolvedPeaks signerCount signerCommitment outputCount outputPeaks =
  pcon $ PNativeScriptsControlV1
    (pdata $ phexByteStr "01")
    (pdata $ phexByteStr "02")
    (pdata $ phexByteStr "03")
    (pdata $ phexByteStr "04")
    (pdata resolvedCount)
    (pdata $ preplicateBS # 32 # (pintegerToByte # 0x11))
    (pdata spendCount)
    (pdata resolvedPeaks)
    (pdata signerCount)
    (pdata signerCommitment)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata outputCount)
    (pdata outputPeaks)
    (pdata outputPeaks)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata 0)
    (pdata $ preplicateBS # 32 # (pintegerToByte # 0x33))

nativeScriptsControlWithMint :: forall s.
  Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PNativeScriptsControlV1
nativeScriptsControlWithMint mintCount mintPeaks = pcon $ PNativeScriptsControlV1
  (pdata $ phexByteStr "01")
  (pdata $ phexByteStr "02")
  (pdata $ phexByteStr "03")
  (pdata $ phexByteStr "04")
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x11))
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata pnil)
  (pdata mintCount)
  (pdata mintPeaks)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x33))

nativeScriptsControlForRedeemerSelection :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PNativeScriptsControlV1
nativeScriptsControlForRedeemerSelection redeemerLeaf purposeLeaf = pcon $ PNativeScriptsControlV1
  (pdata $ phexByteStr "01")
  (pdata $ phexByteStr "02")
  (pdata $ phexByteStr "03")
  (pdata $ phexByteStr "04")
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x11))
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 1)
  (pdata $ cekSinglePeak redeemerLeaf)
  (pdata 1)
  (pdata $ cekSinglePeak purposeLeaf)
  (pdata 0)
  (pdata pnil)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x33))

nativeScriptsControlFixture :: forall s.
  Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PNativeScriptsControlV1
nativeScriptsControlFixture compactCbor languageBitmap redeemerCount = pcon $ PNativeScriptsControlV1
  (pdata compactCbor)
  (pdata $ phexByteStr "02")
  (pdata $ phexByteStr "03")
  (pdata $ phexByteStr "04")
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x11))
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x22))
  (pdata 0)
  (pdata pnil)
  (pdata redeemerCount)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata pnil)
  (pdata 0)
  (pdata languageBitmap)
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x33))

valueAndMintControlCodec :: forall s. Term s PBool
valueAndMintControlCodec =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  plet (pencodeValueAndMintWitnessV1 # nativeControl) $ \witnessCbor ->
  plet
    ( pcon $ PValueAndMintControlV1
        (pdata nativeControl)
        (pdata 0)
        (pdata pemptyResolutionScheduleHash)
        (pdata 0)
        (pdata 0)
        (pdata $ preplicateBS # 32 # (pintegerToByte # 0))
        (pdata pinitialResolutionAccumulator)
        (pdata pemptyResolutionScheduleHash)
        (pdata 0)
        (pdata 0)
        (pdata 0)
        (pdata pinitialValueAccumulator)
    )
    $ \expected ->
      pand'List
        [ psliceBS # 0 # 1 # witnessCbor #== phexByteStr "8c"
        , pvalueAndMintControlFromWitness # witnessCbor #== expected
        , pencodeValueAndMintControlV1 # expected #== witnessCbor
        , pencodeValueAccumulatorV1 # pinitialValueAccumulator
            #== pconstant "\x84\x00"
              <> (Codec.pencodeDefiniteBytes # pnull_hash)
              <> pconstant "\x00\x00"
        ]

cekContextControlCodec :: forall s. Term s PBool
cekContextControlCodec =
  plet cekNonemptySequence $ \sequenceSummary ->
  plet cekNonemptySummary $ \summary ->
  plet (preplicateBS # 32 # (pintegerToByte # 0xaa)) $ \programRoot ->
  plet (preplicateBS # 32 # (pintegerToByte # 0xbb)) $ \envelopeHash ->
  plet (preplicateBS # 28 # (pintegerToByte # 0xcc)) $ \scriptHash ->
  plet (preplicateBS # 32 # (pintegerToByte # 0xee)) $ \redeemerLeaf ->
  plet (preplicateBS # 32 # (pintegerToByte # 0xff)) $ \redeemerControlHash ->
  plet (preplicateBS # 28 # (pintegerToByte # 0x33)) $ \mintPolicy ->
  plet
    ( pcon $ PCekContextControlV1
        (pdata 1)
        (pdata 3)
        (pdata programRoot)
        (pdata envelopeHash)
        (pdata 2)
        (pdata 7)
        (pdata scriptHash)
        (pdata $ phexByteStr "ddee")
        (pdata redeemerLeaf)
        (pdata redeemerControlHash)
        (pdata 10)
        (pdata 11)
        (pdata sequenceSummary)
        (pdata sequenceSummary)
        (pdata sequenceSummary)
        (pdata sequenceSummary)
        (pdata 12)
        (pdata sequenceSummary)
        (pdata $ phexByteStr "0102")
        (pdata summary)
        (pdata 13)
        (pdata mintPolicy)
        (pdata sequenceSummary)
        (pdata sequenceSummary)
        (pdata summary)
    )
    $ \control ->
      plet (pencodeCekContextControlV1 # control) $ \encoded ->
      plet
        ( pconstant "\x84"
            <> (Codec.pencodeDefiniteBytes # (preplicateBS # 32 # (pintegerToByte # 0x52)))
            <> pconstant "\x01\x01\x01"
        )
        $ \encodedSequence ->
        plet
          ( pconstant "\x83"
              <> (Codec.pencodeDefiniteBytes # (preplicateBS # 32 # (pintegerToByte # 0x51)))
              <> pconstant "\x01\x01"
          )
          $ \encodedSummary ->
            pand'List
              [ pcekContextControlFromCbor # encoded #== control
              , encoded
                  #== phexByteStr "98190103"
                    <> (Codec.pencodeDefiniteBytes # programRoot)
                    <> (Codec.pencodeDefiniteBytes # envelopeHash)
                    <> phexByteStr "0207"
                    <> (Codec.pencodeDefiniteBytes # scriptHash)
                    <> phexByteStr "42ddee"
                    <> (Codec.pencodeDefiniteBytes # redeemerLeaf)
                    <> (Codec.pencodeDefiniteBytes # redeemerControlHash)
                    <> phexByteStr "0a0b"
                    <> encodedSequence <> encodedSequence <> encodedSequence <> encodedSequence
                    <> phexByteStr "0c" <> encodedSequence <> phexByteStr "420102"
                    <> encodedSummary <> phexByteStr "0d"
                    <> (Codec.pencodeDefiniteBytes # mintPolicy)
                    <> encodedSequence <> encodedSequence <> encodedSummary
              ]

cekSubControlAbiVectors :: forall s. Term s PBool
cekSubControlAbiVectors =
  plet cekNonemptySequence $ \sequenceSummary ->
  plet cekNonemptySummary $ \summary ->
  plet cekEmptySummary $ \emptySummary ->
  plet
    ( pcon $ PCekRedeemerContextControlV1
        (pdata 1)
        (pdata sequenceSummary)
        (pdata $ preplicateBS # 32 # (pintegerToByte # 0x53))
        (pdata $ preplicateBS # 32 # (pintegerToByte # 0x54))
        (pdata emptySummary)
        (pdata summary)
    )
    $ \redeemerControl ->
    plet (pcon $ PCekFinalContextControlV1 (pdata summary) (pdata summary) (pdata summary)) $ \finalControl ->
    plet (pcon $ PCekContextPartsControlV1 (pdata sequenceSummary) (pdata summary) (pdata summary)) $ \partsControl ->
    plet (pcon $ PCekTxInfoAssemblyControlV1 (pdata sequenceSummary) (pdata summary) (pdata summary)) $ \assemblyControl ->
      pand'List
        [ pencodeCekRedeemerContextControlV1 # redeemerControl
            #== phexByteStr "8601845820525252525252525252525252525252525252525252525252525252525252525201010158205353535353535353535353535353535353535353535353535353535353535353582054545454545454545454545454545454545454545454545454545454545454548340000083582051515151515151515151515151515151515151515151515151515151515151510101"
        , phashCekRedeemerContextControlV1 # redeemerControl
            #== phexByteStr "3dfab23fb96dece2da964d3b0b62ef26006400b04b676b6ccfc18ac5da438c10"
        , pencodeCekFinalContextControlV1 # finalControl
            #== phexByteStr "83835820515151515151515151515151515151515151515151515151515151515151515101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101"
        , phashCekFinalContextControlV1 # finalControl
            #== phexByteStr "a4fdda392c9324034244f6b4674441a320d90d819521ccc9b62ff37c0dfdc10b"
        , pencodeCekContextPartsControlV1 # partsControl
            #== phexByteStr "8384582052525252525252525252525252525252525252525252525252525252525252520101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101"
        , phashCekContextPartsControlV1 # partsControl
            #== phexByteStr "1e8e3ea65ea7e762512207ea4276022ce321332ee4c2f6bdf1b7329bd1baa962"
        , pencodeCekTxInfoAssemblyControlV1 # assemblyControl
            #== phexByteStr "8384582052525252525252525252525252525252525252525252525252525252525252520101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101"
        , phashCekTxInfoAssemblyControlV1 # assemblyControl
            #== phexByteStr "7e848c60ea1a41e1d8d90d38c9034b78ce2c0b55e5e0cba7620bb0d3f909e674"
        , phashCekRedeemerContextControlV1 # pinitialCekRedeemerContextControlV1
            #== phexByteStr "c56f80a85b46a479d9a766ce7050e825484f42da6453a3ec217afc6b03d5f938"
        ]

malformedCekFinalContext :: forall s. Term s PByteString
malformedCekFinalContext =
  phashCekFinalContextControlV1
    # (pcon $ PCekFinalContextControlV1 (pdata cekEmptySummary) (pdata cekEmptySummary) (pdata cekEmptySummary))

cekWitnessEnvelope :: forall s. Term s PBool
cekWitnessEnvelope =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  plet (pencodeNativeScriptsControlV1 # nativeControl) $ \nativeCbor ->
  plet (preplicateBS # 32 # (pintegerToByte # 0xaa)) $ \activeHash ->
  plet (preplicateBS # 32 # (pintegerToByte # 0xbb)) $ \envelopeHash ->
    pencodeCekWitnessV1
      # nativeControl
      # phexByteStr "0102"
      # 0
      # 3
      # 4
      # activeHash
      # 5
      # 6
      # envelopeHash
      #== pconstant "\x89"
        <> (Codec.pencodeDefiniteBytes # nativeCbor)
        <> phexByteStr "4201020003045820"
        <> activeHash
        <> phexByteStr "5820"
        <> envelopeHash
        <> phexByteStr "0506"

malformedCekWitnessCursor :: forall s. Term s PByteString
malformedCekWitnessCursor =
  pencodeCekWitnessV1
    # emptyNativeScriptsControl 0
    # pconstant ""
    # 1
    # 0
    # 0
    # pconstant ""
    # 0
    # 0
    # pconstant ""

valueAssetMutationSemantics :: forall s. Term s PBool
valueAssetMutationSemantics =
  plet (preplicateBS # 28 # (pintegerToByte # 0x44)) $ \unit ->
  plet (valueMutation (pconstant False) 0) $ \insertion ->
  plet (papplyValueAssetMutation # pinitialValueAccumulator # unit # 5 # insertion) $ \inserted ->
    pmatch inserted $ \case
      PValueAccumulatorUpdated insertedData ->
        plet (pfromData insertedData) $ \insertedAccumulator ->
        plet (valueMutation (pconstant True) 5) $ \removal ->
        plet (papplyValueAssetMutation # insertedAccumulator # unit # (-5) # removal) $ \removed ->
        plet
          ( MpfProof.pinsertRoot
              # pnull_hash
              # unit
              # phexByteStr "05"
              # (pcon $ PProof pnil)
          )
          $ \expectedInsertedRoot ->
          pand'List
            [ pmatch expectedInsertedRoot $ \case
                PNothing -> pconstant False
                PJust expectedRoot -> pmatch insertedAccumulator $ \accumulator ->
                  pand'List
                    [ pfromData (pvalueAccumulator'assetRoot accumulator) #== expectedRoot
                    , pfromData (pvalueAccumulator'seenAssetCount accumulator) #== 1
                    , pfromData (pvalueAccumulator'nonzeroAssetCount accumulator) #== 1
                    ]
            , pmatch removed $ \case
                PValueAccumulatorUpdated removedData -> pmatch (pfromData removedData) $ \accumulator ->
                  pand'List
                    [ pfromData (pvalueAccumulator'seenAssetCount accumulator) #== 1
                    , pfromData (pvalueAccumulator'nonzeroAssetCount accumulator) #== 0
                    ]
                _ -> pconstant False
            , valueMutationIsInvalid $ papplyValueAssetMutation # pinitialValueAccumulator # unit # 0 # insertion
            , valueMutationIsInvalid $ papplyValueAssetMutation # pinitialValueAccumulator # pconstant "short" # 1 # insertion
            , valueMutationIsInvalid $ papplyValueAssetMutation # pinitialValueAccumulator # unit # 1 # (valueMutation (pconstant False) 1)
            , valueMutationHitLimit
                $ papplyValueAssetMutation
                  # (pcon $ PValueAccumulatorV1 (pdata 0) (pdata pnull_hash) (pdata 16_384) (pdata 0))
                  # unit
                  # 1
                  # insertion
            ]
      _ -> pconstant False
  where
    valueMutation :: Term s PBool -> Term s PInteger -> Term s PValueAssetMutationWitnessV1
    valueMutation wasPresent oldDelta = pcon $ PValueAssetMutationWitnessV1
      (pdata wasPresent)
      (pdata oldDelta)
      (pdata pnil)

    valueMutationIsInvalid :: Term s PValueAccumulatorUpdateV1 -> Term s PBool
    valueMutationIsInvalid update = pmatch update $ \case
      PValueAccumulatorMutationInvalid -> pconstant True
      _ -> pconstant False

    valueMutationHitLimit :: Term s PValueAccumulatorUpdateV1 -> Term s PBool
    valueMutationHitLimit update = pmatch update $ \case
      PValueAccumulatorAssetLimitExceeded -> pconstant True
      _ -> pconstant False

cekObserverSummaryVectors :: forall s. Term s PBool
cekObserverSummaryVectors =
  plet (preplicateBS # 28 # (pintegerToByte # 0xaa)) $ \observerHash ->
  plet (pprependCekObserverItemV1 # observerHash # pconstant False # CekData.pemptyDataPairSummaryV1) $ \cardanoItems ->
  plet (pprependCekObserverItemV1 # observerHash # pconstant True # CekData.pemptyDataListSummaryV1) $ \midgardItems ->
  plet
    ( pcon $ CekData.PDataSequenceSummaryV1
        (pdata $ phexByteStr "354faff8c56e5c2db595d2b0682336623a4de555e8426762630783d26921ab5d")
        (pdata 224)
        (pdata 7840)
        (pdata 9184)
    )
    $ \completeItems ->
      pand'List
        [ pmatch (ScriptContext.pobserverCollectionSummaryV1 # (pcons # observerHash # pnil) # pconstant False) $ \case
            PNothing -> pconstant False
            PJust expected -> pfinalizeCekObserverItemsV1 # cardanoItems # pconstant False #== expected
        , pmatch (ScriptContext.pobserverCollectionSummaryV1 # (pcons # observerHash # pnil) # pconstant True) $ \case
            PNothing -> pconstant False
            PJust expected -> pfinalizeCekObserverItemsV1 # midgardItems # pconstant True #== expected
        , pmatch (pfinalizeCekObserverItemsV1 # completeItems # pconstant False) $ \summary ->
            pand'List
              [ pfromData (CekData.psummary'root summary)
                  #== phexByteStr "61eff6b0a693f2da6f3fd45ae1b3d402937e3ab0de94da2fcbd5ed67ab61a726"
              , pfromData (CekData.psummary'cborLength summary) #== 7842
              , pfromData (CekData.psummary'memory summary) #== 9188
              ]
        ]

malformedCekObserverHash :: forall s. Term s CekData.PDataSequenceSummaryV1
malformedCekObserverHash =
  pprependCekObserverItemV1
    # pconstant "short"
    # pconstant False
    # CekData.pemptyDataPairSummaryV1

cekContextWellFormedStageMatrix :: forall s. Term s PBool
cekContextWellFormedStageMatrix =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
    pand'List
      [ pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 0 3 True
      , pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 1 3 True
      , pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 2 3 True
      , pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 3 3 True
      , pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 4 3 True
      , pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 5 3 True
      , pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 6 3 True
      , pnot #$ pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 7 3 True
      , pnot #$ pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 8 3 True
      , pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 9 3 True
      , pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 13 3 True
      , pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 0 128 True
      , pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 6 128 True
      , pnot #$ pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 0 4 True
      , pnot #$ pcekContextControlIsWellFormed # nativeControl # cekContextStageFixture 6 3 False
      ]

cekContextStageFixture :: forall s. Integer -> Integer -> Bool -> Term s PCekContextControlV1
cekContextStageFixture stage languageTag exactObserverSummary =
  cekContextStageFixtureWithRedeemerHash
    stage
    languageTag
    exactObserverSummary
    ( if stage == 0
        then pconstant ""
        else preplicateBS # 32 # (pintegerToByte # 0x77)
    )

cekContextStageFixtureWithRedeemerHash :: forall s.
  Integer -> Integer -> Bool -> Term s PByteString -> Term s PCekContextControlV1
cekContextStageFixtureWithRedeemerHash stage languageTag exactObserverSummary redeemerControlHash =
  let observerItems = if languageTag == 128
        then CekData.pemptyDataListSummaryV1
        else CekData.pemptyDataPairSummaryV1
      observerSummary
        | stage < 6 = cekEmptySummary
        | exactObserverSummary = pfinalizeCekObserverItemsV1 # observerItems # pconstant (languageTag == 128)
        | otherwise = cekNonemptySummary
      mintSummary = if stage >= 9
        then CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1
        else cekEmptySummary
   in pcon $ PCekContextControlV1
        (pdata $ pconstant stage)
        (pdata $ pconstant languageTag)
        (pdata $ preplicateBS # 32 # (pintegerToByte # 0x11))
        (pdata $ preplicateBS # 32 # (pintegerToByte # 0x22))
        (pdata 0)
        (pdata 0)
        (pdata $ preplicateBS # 28 # (pintegerToByte # 0x33))
        (pdata $ pconstant "")
        (pdata $ preplicateBS # 32 # (pintegerToByte # 0x44))
        (pdata redeemerControlHash)
        (pdata 0)
        (pdata 0)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata 0)
        (pdata observerItems)
        (pdata $ pconstant "")
        (pdata observerSummary)
        (pdata 0)
        (pdata $ pconstant "")
        (pdata CekData.pemptyDataPairSummaryV1)
        (pdata CekData.pemptyDataPairSummaryV1)
        (pdata mintSummary)

cekRedeemerContextBindings :: forall s. Term s PBool
cekRedeemerContextBindings =
  plet (redeemerContextFixture 1 cekNonemptySequence (pconstant "") (pconstant "") cekNonemptySummary) $ \completed ->
  plet (phashCekRedeemerContextControlV1 # completed) $ \completedHash ->
  plet (cekContextStageFixtureWithRedeemerHash 10 3 True completedHash) $ \boundContext ->
  plet (nativeScriptsControlWithRedeemerCount 0 1) $ \nativeControl ->
  plet
    ( redeemerContextFixture
        0
        CekData.pemptyDataPairSummaryV1
        (preplicateBS # 32 # (pintegerToByte # 0xaa))
        (preplicateBS # 32 # (pintegerToByte # 0xbb))
        cekEmptySummary
    )
    $ \active ->
      pand'List
        [ pcekRedeemerContextControlIsWellFormed # 0 # pinitialCekRedeemerContextControlV1
        , pcekRedeemerContextControlIsWellFormed # 1 # active
        , pnot #$ pcekRedeemerContextControlIsWellFormed # 0 # active
        , pcekRedeemerContextControlIsWellFormed # 1 # completed
        , pcompletedCekRedeemerContextMatches # nativeControl # boundContext # completed
        , pnot
            #$ pcompletedCekRedeemerContextMatches
            # nativeControl
            # cekContextStageFixture 10 3 True
            # completed
        , pnot
            #$ pcekRedeemerContextControlIsWellFormed
            # 1
            # redeemerContextFixture
              0
              CekData.pemptyDataPairSummaryV1
              (pconstant "")
              (preplicateBS # 32 # (pintegerToByte # 0xbb))
              cekEmptySummary
        ]

redeemerContextFixture :: forall s.
  Integer ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s PByteString ->
  Term s PByteString ->
  Term s CekData.PDataSummaryV1 ->
  Term s PCekRedeemerContextControlV1
redeemerContextFixture cursor mapItems activeScanHash activeRedeemerLeaf currentRedeemer =
  pcon $ PCekRedeemerContextControlV1
    (pdata $ pconstant cursor)
    (pdata mapItems)
    (pdata activeScanHash)
    (pdata activeRedeemerLeaf)
    (pdata cekEmptySummary)
    (pdata currentRedeemer)

cekContextSuccessorBinding :: forall s. Term s PBool
cekContextSuccessorBinding =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  plet (cekContextStageFixture 0 3 True) $ \contextControl ->
  plet (cekStateFixture (pcon PCek) 40 (cekHash 0xaa) 7 8) $ \pre ->
  plet
    ( phashWorkWitness
        # pcon PCek
        # 41
        # ( pencodeCekWitnessV1
              # nativeControl
              # (pencodeCekContextControlV1 # contextControl)
              # 0
              # 3
              # 4
              # pconstant ""
              # 0
              # 0
              # (preplicateBS # 32 # (pintegerToByte # 0x22))
          )
    )
    $ \expectedWorkRoot ->
    plet (cekStateFixture (pcon PCek) 41 expectedWorkRoot 7 8) $ \post ->
    plet
      (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata post))
      $ \witness ->
        pcekContextSuccessorIsExact # pre # witness # nativeControl # contextControl # 0 # 3 # 4
          #&& pnot
            # ( pcekContextSuccessorIsExact
                  # pre
                  # ( pcon $ PValidationOneStepWitnessV1
                        (pdata $ pconstant "")
                        (pdata $ cekStateFixture (pcon PCek) 41 expectedWorkRoot 9 8)
                    )
                  # nativeControl
                  # contextControl
                  # 0
                  # 3
                  # 4
              )

cekExecutionSelectionTransition :: forall s. Term s PBool
cekExecutionSelectionTransition =
  plet (cekHash 0xa1) $ \termRoot ->
  plet (phexByteStr "8501830101005820" <> termRoot <> phexByteStr "0101") $ \envelopeCbor ->
  plet (phexByteStr "8203582a" <> envelopeCbor) $ \scriptCbor ->
  plet (cekHash 0xa2) $ \redeemerLeaf ->
  plet (preplicateBS # 28 # (pintegerToByte # 0xa3)) $ \scriptHash ->
  plet (phexByteStr "00") $ \subject ->
  plet (Bounded.pfromBytes # 6 # 0 # scriptCbor) $ \itemCommitment ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # subject) $ \purposeLeaf ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # subject # 3 # scriptHash
        # (plengthBS # scriptCbor) # itemCommitment
    )
    $ \sourceLeaf ->
  plet
    (ScriptProof.pexecutionLeafHash # 3 # purposeLeaf # sourceLeaf # redeemerLeaf)
    $ \executionLeaf ->
  plet
    (nativeScriptsControlForExecutionSelection purposeLeaf sourceLeaf redeemerLeaf executionLeaf)
    $ \nativeControl ->
  plet (CekProof.phashProgramEnvelopeV1 # 1 # 1 # 0 # termRoot # 1 # 1) $ \envelopeHash ->
  plet
    (cekExecutionContextFixture termRoot envelopeHash scriptHash subject redeemerLeaf)
    $ \nextContext ->
  plet
    ( pcon $ Bounded.PChunkProofV1
        (pdata Bounded.pversion)
        (pdata 6)
        (pdata 0)
        (pdata $ plengthBS # scriptCbor)
        (pdata 0)
        (pdata scriptCbor)
        (pdata $ cekSinglePeak $ Bounded.phashChunk # 6 # 0 # 0 # scriptCbor)
        (pdata pnil)
    )
    $ \firstChunkProof ->
  plet (cekStateFixture (pcon PCek) 98 (cekHash 0xaa) 7 8) $ \pre ->
  plet
    ( phashWorkWitness
        # pcon PCek
        # 99
        # ( pencodeCekWitnessV1
              # nativeControl # (pencodeCekContextControlV1 # nextContext)
              # 0 # 3 # 4 # pconstant "" # 0 # 0 # envelopeHash
          )
    )
    $ \nextWorkRoot ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        (pdata $ pconstant "")
        (pdata $ cekStateFixture (pcon PCek) 99 nextWorkRoot 7 8)
    )
    $ \witness ->
    pverifyCekExecutionSelection
      # pre # witness # nativeControl # 0 # 3 # 4
      # 0 # 3 # 0 # 0 # scriptHash # subject # pnil
      # 0 # 0 # subject # (plengthBS # scriptCbor) # itemCommitment # pnil
      # redeemerLeaf # pnil # firstChunkProof

cekOversizedProgramEnvelopeRejected :: forall s. Term s PBool
cekOversizedProgramEnvelopeRejected =
  plet
    (preplicateBS # (CekProof.pmaxProgramEnvelopeCborBytes + 1) # (pintegerToByte # 0))
    $ \oversized ->
      plengthBS # oversized #== 51
        #&& pmatch (CekProof.pinspectProgramEnvelopeV1 # oversized) (\case
          PNothing -> pconstant True
          PJust _ -> pconstant False
        )

maximumNativeThenEffectfulCekSelectionIsBounded :: forall s. Term s PBool
maximumNativeThenEffectfulCekSelectionIsBounded =
  plet
    ( preplicateBS # 8_192 # (pintegerToByte # 0)
        <> preplicateBS # 8_187 # (pintegerToByte # 0)
    )
    $ \nativeScriptBytes ->
  plet
    ( pcon $ PMidgardVersionedScript
        (pdata $ pcon PNativeCardanoScript)
        (pdata nativeScriptBytes)
    )
    $ \nativeScript ->
  plet (pencodeMidgardVersionedScript # nativeScript) $ \nativeScriptCbor ->
  plet (Bounded.pfromBytes # 6 # 0 # nativeScriptCbor) $ \nativeCommitment ->
  plet (psliceBS # 0 # Bounded.pchunkBytes # nativeScriptCbor) $ \chunk0 ->
  plet (psliceBS # Bounded.pchunkBytes # Bounded.pchunkBytes # nativeScriptCbor) $ \chunk1 ->
  plet (psliceBS # (Bounded.pchunkBytes * 2) # Bounded.pchunkBytes # nativeScriptCbor) $ \chunk2 ->
  plet (psliceBS # (Bounded.pchunkBytes * 3) # Bounded.pchunkBytes # nativeScriptCbor) $ \chunk3 ->
  plet (psliceBS # (Bounded.pchunkBytes * 4) # 4 # nativeScriptCbor) $ \chunk4 ->
  plet (Bounded.phashChunk # 6 # 0 # 0 # chunk0) $ \hash0 ->
  plet (Bounded.phashChunk # 6 # 0 # 1 # chunk1) $ \hash1 ->
  plet (Bounded.phashChunk # 6 # 0 # 2 # chunk2) $ \hash2 ->
  plet (Bounded.phashChunk # 6 # 0 # 3 # chunk3) $ \hash3 ->
  plet (Bounded.phashChunk # 6 # 0 # 4 # chunk4) $ \hash4 ->
  plet
    ( Merkle.pbuildFrontier
        #$ pcons # pdata hash0
        #$ pcons # pdata hash1
        #$ pcons # pdata hash2
        #$ pcons # pdata hash3
        #$ pcons # pdata hash4
        # pnil
    )
    $ \chunkFrontier ->
  pmatch chunkFrontier $ \builtChunks ->
  plet
    ( pcon $ Bounded.PChunkProofV1
        (pdata Bounded.pversion)
        (pdata 6)
        (pdata 0)
        (pdata $ plengthBS # nativeScriptCbor)
        (pdata 0)
        (pdata chunk0)
        (pdata $ Merkle.pbuiltFrontier'peaks builtChunks)
        ( pdata
            $ pcons # pdata hash1
            #$ pcons # pdata (Merkle.phashBranch # hash2 # hash3)
            # pnil
        )
    )
    $ \firstChunkProof ->
  plet (CekProof.pencodeProgramEnvelopeV1 # 1 # 1 # 0 # cekHash 0xaa # 3 # 144) $ \programEnvelope ->
  plet
    ( pcon $ PMidgardVersionedScript
        (pdata $ pcon PPlutusV3Script)
        (pdata programEnvelope)
    )
    $ \effectfulScript ->
  plet (pencodeMidgardVersionedScript # effectfulScript) $ \effectfulScriptCbor ->
  plet (ScriptProof.pversionedScriptHash # nativeScript) $ \nativeHash ->
  plet (ScriptProof.pversionedScriptHash # effectfulScript) $ \effectfulHash ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # phexByteStr "00" # 0 # nativeHash
        # (plengthBS # nativeScriptCbor) # nativeCommitment
    )
    $ \nativeSource ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # phexByteStr "01" # 3 # effectfulHash
        # (plengthBS # effectfulScriptCbor)
        # (Bounded.pfromBytes # 6 # 1 # effectfulScriptCbor)
    )
    $ \effectfulSource ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # nativeHash # phexByteStr "00") $ \nativePurpose ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 1 # effectfulHash # phexByteStr "01") $ \effectfulPurpose ->
  plet (ScriptProof.pexecutionLeafHash # 0 # nativePurpose # nativeSource # pconstant "") $ \nativeExecution ->
  plet (ScriptProof.pexecutionLeafHash # 3 # effectfulPurpose # effectfulSource # cekHash 0xcc) $ \effectfulExecution ->
  plet (Merkle.pbuildFrontier #$ pcons # pdata nativeSource #$ pcons # pdata effectfulSource # pnil) $ \sourceFrontier ->
  plet (Merkle.pbuildFrontier #$ pcons # pdata nativePurpose #$ pcons # pdata effectfulPurpose # pnil) $ \purposeFrontier ->
  plet (Merkle.pbuildFrontier #$ pcons # pdata nativeExecution #$ pcons # pdata effectfulExecution # pnil) $ \executionFrontier ->
  pmatch sourceFrontier $ \builtSources ->
  pmatch purposeFrontier $ \builtPurposes ->
  pmatch executionFrontier $ \builtExecutions ->
  plet
    ( nativeScriptsControlForTwoExecutionSelections
        (Merkle.pbuiltFrontier'peaks builtSources)
        (Merkle.pbuiltFrontier'peaks builtPurposes)
        (Merkle.pbuiltFrontier'peaks builtExecutions)
    )
    $ \nativeControl ->
  plet (pencodeCekWitnessV1 # nativeControl # pconstant "" # 0 # 0 # 0 # pconstant "" # 0 # 0 # pconstant "") $ \workWitnessCbor ->
  plet (cekStateFixture (pcon PCek) 43 (phashWorkWitness # pcon PCek # 43 # workWitnessCbor) 0 0) $ \pre ->
  plet (pencodeCekWitnessV1 # nativeControl # pconstant "" # 1 # 0 # 0 # pconstant "" # 0 # 0 # pconstant "") $ \nextWorkWitnessCbor ->
  plet (cekStateFixture (pcon PCek) 44 (phashWorkWitness # pcon PCek # 44 # nextWorkWitnessCbor) 0 0) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workWitnessCbor) (pdata post)) $ \transition ->
  plet
    ( pcon $ PNativeExecutionScanWitness
        (pdata 0) (pdata 0) (pdata 0) (pdata 0)
        (pdata nativeHash) (pdata $ phexByteStr "00")
        (pdata $ pcons # pdata effectfulPurpose # pnil)
        (pdata 0) (pdata 0) (pdata $ phexByteStr "00")
        (pdata $ plengthBS # nativeScriptCbor) (pdata nativeCommitment)
        (pdata $ pcons # pdata effectfulSource # pnil)
        (pdata $ pconstant "")
        (pdata $ pcons # pdata effectfulExecution # pnil)
        (pdata firstChunkProof)
    )
    $ \auxiliary ->
  plet (pcon $ PValidationOneStepEvidenceV1 (pdata transition) (pdata auxiliary)) $ \evidence ->
    pand'List
      [ plengthBS # nativeScriptBytes #== 16_379
      , plengthBS # nativeScriptCbor #== 16_384
      , plengthBS # chunk0 #== Bounded.pchunkBytes
      , plengthBS # (pserialiseData # pforgetData (pdata evidence)) #< 16_384
      , pverifyCekExecutionSelection
          # pre # transition # nativeControl # 0 # 0 # 0
          # 0 # 0 # 0 # 0 # nativeHash # phexByteStr "00"
          # (pcons # pdata effectfulPurpose # pnil)
          # 0 # 0 # phexByteStr "00" # (plengthBS # nativeScriptCbor)
          # nativeCommitment # (pcons # pdata effectfulSource # pnil)
          # pconstant "" # (pcons # pdata effectfulExecution # pnil)
          # firstChunkProof
      ]

crossLanguageCekContextControlVectors :: forall s. Term s PBool
crossLanguageCekContextControlVectors =
  plet
    ( pcon $ PCekContextControlV1
        (pdata 0)
        (pdata 3)
        (pdata $ cekHash 0xaa)
        (pdata $ cekHash 0xbb)
        (pdata 0)
        (pdata 0)
        (pdata $ preplicateBS # 28 # (pintegerToByte # 0xbb))
        (pdata $ phexByteStr "00")
        (pdata $ cekHash 0xcc)
        (pdata $ pconstant "")
        (pdata 0)
        (pdata 0)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata 0)
        (pdata CekData.pemptyDataPairSummaryV1)
        (pdata $ pconstant "")
        (pdata cekEmptySummary)
        (pdata 0)
        (pdata $ pconstant "")
        (pdata CekData.pemptyDataPairSummaryV1)
        (pdata CekData.pemptyDataPairSummaryV1)
        (pdata cekEmptySummary)
    )
    $ \context ->
      pand'List
        [ pencodeCekContextControlV1 # context
            #== phexByteStr "981900035820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa5820bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb0000581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb41005820cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc4000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa5600000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba200000040834000000040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba200000083400000"
        , phashCekRedeemerContextControlV1 # pinitialCekRedeemerContextControlV1
            #== phexByteStr "c56f80a85b46a479d9a766ce7050e825484f42da6453a3ec217afc6b03d5f938"
        ]

plutusV3ReceiveSelectionRejectsWithExactNoop :: forall s. Term s PBool
plutusV3ReceiveSelectionRejectsWithExactNoop =
  plet (CekProof.pencodeProgramEnvelopeV1 # 1 # 1 # 0 # cekHash 0xaa # 3 # 144) $ \programEnvelope ->
  plet
    ( pcon $ PMidgardVersionedScript
        (pdata $ pcon PPlutusV3Script)
        (pdata programEnvelope)
    )
    $ \script ->
  plet (pencodeMidgardVersionedScript # script) $ \scriptCbor ->
  plet (Bounded.pfromBytes # 6 # 0 # scriptCbor) $ \scriptItemCommitment ->
  plet (ScriptProof.pversionedScriptHash # script) $ \scriptHash ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # phexByteStr "00" # 3 # scriptHash
        # (plengthBS # scriptCbor) # scriptItemCommitment
    )
    $ \sourceLeaf ->
  plet (ScriptProof.ppurposeLeafHash # 3 # 0 # scriptHash # scriptHash) $ \purposeLeaf ->
  plet (ScriptProof.pexecutionLeafHash # 3 # purposeLeaf # sourceLeaf # cekHash 0xcc) $ \executionLeaf ->
  plet (nativeScriptsControlForExecutionSelection purposeLeaf sourceLeaf (cekHash 0xcc) executionLeaf) $ \nativeControl ->
  plet
    ( pcon $ Bounded.PChunkProofV1
        (pdata Bounded.pversion)
        (pdata 6)
        (pdata 0)
        (pdata $ plengthBS # scriptCbor)
        (pdata 0)
        (pdata scriptCbor)
        (pdata $ cekSinglePeak $ Bounded.phashChunk # 6 # 0 # 0 # scriptCbor)
        (pdata pnil)
    )
    $ \firstChunkProof ->
  plet (pencodeCekWitnessV1 # nativeControl # pconstant "" # 0 # 0 # 0 # pconstant "" # 0 # 0 # pconstant "") $ \workWitnessCbor ->
  plet (cekStateFixture (pcon PCek) 40 (phashWorkWitness # pcon PCek # 40 # workWitnessCbor) 0 0) $ \pre ->
  plet (inputSetsExactRejection pre $ pconstant "E_PLUTUS_SCRIPT_INVALID") $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workWitnessCbor) (pdata post)) $ \transition ->
    pverifyCekExecutionSelection
      # pre # transition # nativeControl # 0 # 0 # 0
      # 0 # 3 # 3 # 0 # scriptHash # scriptHash # pnil
      # 0 # 0 # phexByteStr "00" # (plengthBS # scriptCbor)
      # scriptItemCommitment # pnil # cekHash 0xcc # pnil # firstChunkProof

nativeScriptsControlForTwoExecutionSelections :: forall s.
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PNativeScriptsControlV1
nativeScriptsControlForTwoExecutionSelections sourcePeaks purposePeaks executionPeaks =
  pcon $ PNativeScriptsControlV1
    (pdata $ phexByteStr "01")
    (pdata $ phexByteStr "02")
    (pdata $ phexByteStr "03")
    (pdata $ phexByteStr "04")
    (pdata 0)
    (pdata $ cekHash 0x11)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
    (pdata 2)
    (pdata sourcePeaks)
    (pdata 0)
    (pdata pnil)
    (pdata 2)
    (pdata purposePeaks)
    (pdata 0)
    (pdata pnil)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 2)
    (pdata executionPeaks)
    (pdata 2)
    (pdata 1)
    (pdata $ cekHash 0x33)

nativeScriptsControlForExecutionSelection :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PNativeScriptsControlV1
nativeScriptsControlForExecutionSelection purposeLeaf sourceLeaf redeemerLeaf executionLeaf =
  pcon $ PNativeScriptsControlV1
    (pdata $ phexByteStr "01")
    (pdata $ phexByteStr "02")
    (pdata $ phexByteStr "03")
    (pdata $ phexByteStr "04")
    (pdata 0)
    (pdata $ cekHash 0x11)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
    (pdata 1)
    (pdata $ cekSinglePeak sourceLeaf)
    (pdata 1)
    (pdata $ cekSinglePeak redeemerLeaf)
    (pdata 1)
    (pdata $ cekSinglePeak purposeLeaf)
    (pdata 0)
    (pdata pnil)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 1)
    (pdata $ cekSinglePeak executionLeaf)
    (pdata 1)
    (pdata 1)
    (pdata $ cekHash 0x33)

cekExecutionContextFixture :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PCekContextControlV1
cekExecutionContextFixture termRoot envelopeHash scriptHash subject redeemerLeaf =
  pcon $ PCekContextControlV1
    (pdata 0)
    (pdata 3)
    (pdata termRoot)
    (pdata envelopeHash)
    (pdata 0)
    (pdata 0)
    (pdata scriptHash)
    (pdata subject)
    (pdata redeemerLeaf)
    (pdata $ pconstant "")
    (pdata 0)
    (pdata 0)
    (pdata CekData.pemptyDataListSummaryV1)
    (pdata CekData.pemptyDataListSummaryV1)
    (pdata CekData.pemptyDataListSummaryV1)
    (pdata CekData.pemptyDataListSummaryV1)
    (pdata 0)
    (pdata CekData.pemptyDataPairSummaryV1)
    (pdata $ pconstant "")
    (pdata cekEmptySummary)
    (pdata 0)
    (pdata $ pconstant "")
    (pdata CekData.pemptyDataPairSummaryV1)
    (pdata CekData.pemptyDataPairSummaryV1)
    (pdata cekEmptySummary)

cekCoreStepTransition :: forall s. Term s PBool
cekCoreStepTransition =
  plet (cekHash 0xb1) $ \functionRoot ->
  plet (cekHash 0xb2) $ \argumentRoot ->
  plet CekProof.pemptyEnvironmentRootV1 $ \environmentRoot ->
  plet CekProof.pemptyContinuationRootV1 $ \continuationRoot ->
  plet
    ( cekCoreStateFixture
        CekMachine.pmodeCompute
        (CekProof.phashApplicationTermV1 # functionRoot # argumentRoot)
        environmentRoot continuationRoot 0 10 11
    )
    $ \corePre ->
  plet
    ( CekProof.phashApplyArgumentContinuationV1
        # argumentRoot # environmentRoot # continuationRoot
    )
    $ \nextContinuation ->
  plet
    ( cekCoreStateFixture
        CekMachine.pmodeCompute functionRoot environmentRoot nextContinuation
        0 16_010 111
    )
    $ \corePost ->
  plet
    (nativeScriptsControlForExecutionSelection (cekHash 0xc1) (cekHash 0xc2) (cekHash 0xc3) (cekHash 0xc4))
    $ \nativeControl ->
  plet (cekHash 0xc5) $ \programEnvelopeHash ->
  plet
    ( phashWorkWitness
        # pcon PCek
        # 101
        # ( pencodeCekWitnessV1
              # nativeControl # pconstant "" # 0 # 3 # 4
              # (CekMachine.phashStateV1 # corePost)
              # 20_000 # 200 # programEnvelopeHash
          )
    )
    $ \nextWorkRoot ->
  plet (cekStateFixture (pcon PCek) 100 (cekHash 0xaa) 13 15) $ \pre ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        (pdata $ pconstant "")
        (pdata $ cekStateFixture (pcon PCek) 101 nextWorkRoot 16_013 115)
    )
    $ \witness ->
    pverifyCekCoreAdvanced
      # pre # witness # nativeControl # 0 # 3 # 4
      # (CekMachine.phashStateV1 # corePre)
      # 20_000 # 200 # programEnvelopeHash
      # corePre # corePost # pconstant True

cekCoreStateFixture :: forall s.
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s CekMachine.PMachineStateV1
cekCoreStateFixture mode focus environment continuation auxiliary cpu memory =
  pcon $ CekMachine.PMachineStateV1
    (pdata mode)
    (pdata 0)
    (pdata focus)
    (pdata environment)
    (pdata continuation)
    (pdata auxiliary)
    (pdata cpu)
    (pdata memory)

cekContextDispatcherRouting :: forall s. Term s PBool
cekContextDispatcherRouting =
  plet (pcon PNoAuxiliaryWitness) $ \noAuxiliary ->
  plet
    ( pcon $ PCekContextSeedWitness
        $ pdata $ pcon $ PCekFinalContextControlV1
          (pdata cekEmptySummary) (pdata cekEmptySummary) (pdata cekEmptySummary)
    )
    $ \seedAuxiliary ->
    pand'List
      [ pcekContextAuxiliaryMatchesStage
          # (cekContextStageFixture 6 3 True) # noAuxiliary
      , pnot #$ pcekContextAuxiliaryMatchesStage
          # (cekContextStageFixture 6 3 True) # seedAuxiliary
      , pnot #$ pcekContextAuxiliaryMatchesStage
          # (cekContextStageFixture 7 3 True) # noAuxiliary
      , pcekContextAuxiliaryMatchesStage
          # (cekContextStageFixture 13 3 True) # seedAuxiliary
      ]

cekTopLevelCompletion :: forall s. Term s PBool
cekTopLevelCompletion =
  pmatch (cekObserverProofSource NativeField.pemptyFieldCommitment) $ \(PPair nativeControl transactionId) ->
  pmatch nativeControl $ \native ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pnativeControl'compactCbor native)
        # pfromData (pnativeControl'witnessSetCompactCbor native)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
    )
    $ \transactionCommitment ->
  plet (phashValidationContext # pfromData (pnativeControl'contextCbor native)) $ \contextHash ->
  plet
    ( phashWorkWitness
        # pcon PValueAndMint # 91 # (pencodeValueAndMintWitnessV1 # nativeControl)
    )
    $ \nextWorkRoot ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PCek) 90 (cekHash 0xaa) 7 8
    )
    $ \pre ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PValueAndMint) 91 nextWorkRoot 7 8
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata post)) $ \witness ->
    pverifyCekCompleted # pre # witness # pcon PNoAuxiliaryWitness # nativeControl

cekOneStepStructuralEnvelope :: forall s. Term s PBool
cekOneStepStructuralEnvelope =
  plet (phexByteStr "80") $ \workWitnessCbor ->
  plet (cekHash 0x31) $ \transactionId ->
  plet (cekHash 0x32) $ \transactionCommitment ->
  plet (cekHash 0x33) $ \contextHash ->
  plet (phashWorkWitness # pcon PCek # 20 # workWitnessCbor) $ \workRoot ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PCek) 20 workRoot 7 8
    )
    $ \pre ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PCek) 21 (cekHash 0x34) 7 8
    )
    $ \post ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PCek) 22 (cekHash 0x34) 7 8
    )
    $ \wrongCounterPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workWitnessCbor) (pdata post)) $ \witness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pnot
        # ( pstructuralTransitionIsValid
              # pre
              # (pcon $ PValidationOneStepWitnessV1 (pdata workWitnessCbor) (pdata wrongCounterPost))
          )

valueAndMintInitialStages :: forall s. Term s PBool
valueAndMintInitialStages =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  pmatch nativeControl $ \native ->
  plet (pfromData $ pnativeControl'resolutionScheduleHash native) $ \scheduleHash ->
  plet
    (valueAndMintStageFixture nativeControl 0 pemptyResolutionScheduleHash pemptyResolutionScheduleHash)
    $ \stageZero ->
  plet (valueAndMintStageFixture nativeControl 1 scheduleHash scheduleHash) $ \stageOne ->
  plet (valueAndMintStageFixture nativeControl 2 scheduleHash scheduleHash) $ \stageTwo ->
  plet (cekStateFixture (pcon PValueAndMint) 30 (cekHash 0xa1) 7 8) $ \preZero ->
  plet
    ( phashWorkWitness
        # pcon PValueAndMint # 31 # (pencodeValueAndMintControlV1 # stageOne)
    )
    $ \stageOneRoot ->
  plet (cekStateFixture (pcon PValueAndMint) 31 stageOneRoot 7 8) $ \postZeroAndPreOne ->
  plet
    ( phashWorkWitness
        # pcon PValueAndMint # 32 # (pencodeValueAndMintControlV1 # stageTwo)
    )
    $ \stageTwoRoot ->
  plet (cekStateFixture (pcon PValueAndMint) 32 stageTwoRoot 7 8) $ \postOne ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postZeroAndPreOne))
    $ \stageZeroWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postOne))
    $ \stageOneWitness ->
    pvalueAndMintStageZero
      # preZero # stageZeroWitness # pcon PNoAuxiliaryWitness # stageZero
      #&& pvalueAndMintStageOne
        # postZeroAndPreOne # stageOneWitness # pcon PNoAuxiliaryWitness # stageOne

valueAndMintStageFixture :: forall s.
  Term s PNativeScriptsControlV1 ->
  Integer ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PValueAndMintControlV1
valueAndMintStageFixture nativeControl stage scheduleHash remainingHash =
  pcon $ PValueAndMintControlV1
    (pdata nativeControl)
    (pdata $ pconstant stage)
    (pdata scheduleHash)
    (pdata 0)
    (pdata 0)
    (pdata $ preplicateBS # 32 # (pintegerToByte # 0))
    (pdata pinitialResolutionAccumulator)
    (pdata remainingHash)
    (pdata 0)
    (pdata 0)
    (pdata 0)
    (pdata pinitialValueAccumulator)

valueAndMintInputReplay :: forall s. Term s PBool
valueAndMintInputReplay =
  plet (cekDescriptorForIndex 0) $ \descriptorCbor ->
  plet cekSpendSubject $ \key ->
  plet pemptyResolutionScheduleHash $ \emptySchedule ->
  plet (presolutionScheduleNodeHash # 0 # key # emptySchedule) $ \scheduleHash ->
  plet
    ( presolvedInputAccumulatorSuccessor
        # pinitialResolutionAccumulator # 0 # key # descriptorCbor
    )
    $ \resolvedAccumulator ->
  plet (valueReplayNativeControl resolvedAccumulator scheduleHash) $ \nativeControl ->
  plet
    ( pcon $ PValueAccumulatorV1
        (pdata 1_234_567) (pdata pnull_hash) (pdata 0) (pdata 0)
    )
    $ \nextAccumulator ->
  plet
    ( valueReplayControl
        nativeControl 2 scheduleHash 0 0
        (preplicateBS # 32 # (pintegerToByte # 0))
        pinitialResolutionAccumulator scheduleHash pinitialValueAccumulator
    )
    $ \beforeReplay ->
  plet
    ( valueReplayControl
        nativeControl 2 scheduleHash 1 0
        (preplicateBS # 32 # (pintegerToByte # 0))
        resolvedAccumulator emptySchedule nextAccumulator
    )
    $ \afterReplay ->
  plet
    ( valueReplayControl
        nativeControl 3 scheduleHash 1 0
        (preplicateBS # 32 # (pintegerToByte # 0))
        resolvedAccumulator emptySchedule nextAccumulator
    )
    $ \afterFinish ->
  plet (cekStateFixture (pcon PValueAndMint) 40 (cekHash 0xa1) 7 8) $ \preReplay ->
  plet
    (phashWorkWitness # pcon PValueAndMint # 41 # (pencodeValueAndMintControlV1 # afterReplay))
    $ \afterReplayRoot ->
  plet (cekStateFixture (pcon PValueAndMint) 41 afterReplayRoot 7 8) $ \postReplayAndPreFinish ->
  plet
    (phashWorkWitness # pcon PValueAndMint # 42 # (pencodeValueAndMintControlV1 # afterFinish))
    $ \afterFinishRoot ->
  plet (cekStateFixture (pcon PValueAndMint) 42 afterFinishRoot 7 8) $ \postFinish ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postReplayAndPreFinish))
    $ \replayWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postFinish))
    $ \finishWitness ->
    pvalueAndMintStageTwoReplay
      # preReplay # replayWitness # beforeReplay # 0 # key # emptySchedule # descriptorCbor
      #&& pvalueAndMintStageTwoFinish
        # postReplayAndPreFinish # finishWitness # pcon PNoAuxiliaryWitness # afterReplay

valueReplayControl :: forall s.
  Term s PNativeScriptsControlV1 ->
  Integer ->
  Term s PByteString ->
  Integer ->
  Integer ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PValueAccumulatorV1 ->
  Term s PValueAndMintControlV1
valueReplayControl nativeControl stage scheduleHash cursor assetCursor valueHash replayAccumulator remainingHash accumulator =
  pcon $ PValueAndMintControlV1
    (pdata nativeControl)
    (pdata $ pconstant stage)
    (pdata scheduleHash)
    (pdata $ pconstant cursor)
    (pdata $ pconstant assetCursor)
    (pdata valueHash)
    (pdata replayAccumulator)
    (pdata remainingHash)
    (pdata 0)
    (pdata 0)
    (pdata 0)
    (pdata accumulator)

valueReplayNativeControl :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PNativeScriptsControlV1
valueReplayNativeControl resolvedAccumulator scheduleHash =
  pcon $ PNativeScriptsControlV1
    (pdata $ phexByteStr "01")
    (pdata $ phexByteStr "02")
    (pdata $ phexByteStr "03")
    (pdata $ phexByteStr "04")
    (pdata 1)
    (pdata resolvedAccumulator)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata pnil)
    (pdata 0)
    (pdata 0)
    (pdata scheduleHash)

valueAndMintInputAssetReplay :: forall s. Term s PBool
valueAndMintInputAssetReplay =
  plet (preplicateBS # 28 # (pintegerToByte # 0x44)) $ \policyId ->
  plet (pconstant "token") $ \assetName ->
  plet 5 $ \quantity ->
  plet (OutputCommitment.passetLeafHash # policyId # assetName # quantity) $ \assetLeaf ->
  plet (cekSinglePeak assetLeaf) $ \assetPeaks ->
  plet (valueDescriptorForAsset policyId assetName quantity assetPeaks) $ \descriptorCbor ->
  plet cekSpendSubject $ \key ->
  plet pemptyResolutionScheduleHash $ \emptySchedule ->
  plet (presolutionScheduleNodeHash # 0 # key # emptySchedule) $ \scheduleHash ->
  plet
    ( presolvedInputAccumulatorSuccessor
        # pinitialResolutionAccumulator # 0 # key # descriptorCbor
    )
    $ \resolvedAccumulator ->
  plet (valueReplayNativeControl resolvedAccumulator scheduleHash) $ \nativeControl ->
  plet
    ( pcon $ PValueAccumulatorV1
        (pdata 1_234_567) (pdata pnull_hash) (pdata 0) (pdata 0)
    )
    $ \lovelaceAccumulator ->
  plet
    (pcon $ PValueAssetMutationWitnessV1 (pdata $ pconstant False) (pdata 0) (pdata pnil))
    $ \mutation ->
  pmatch
    ( papplyValueAssetMutation
        # lovelaceAccumulator # (policyId <> assetName) # quantity # mutation
    )
    $ \case
      PValueAccumulatorUpdated nextAccumulatorData ->
        plet (pfromData nextAccumulatorData) $ \nextAccumulator ->
        plet
          ( valueReplayControl
              nativeControl 2 scheduleHash 0 0
              (preplicateBS # 32 # (pintegerToByte # 0))
              pinitialResolutionAccumulator scheduleHash pinitialValueAccumulator
          )
          $ \beforeReplay ->
        plet
          ( valueReplayControl
              nativeControl 2 scheduleHash 0 1 (pblake2b_256 # descriptorCbor)
              pinitialResolutionAccumulator scheduleHash lovelaceAccumulator
          )
          $ \beforeAsset ->
        plet
          ( valueReplayControl
              nativeControl 2 scheduleHash 1 0
              (preplicateBS # 32 # (pintegerToByte # 0))
              resolvedAccumulator emptySchedule nextAccumulator
          )
          $ \afterAsset ->
        plet (cekStateFixture (pcon PValueAndMint) 45 (cekHash 0xa1) 7 8) $ \preReplay ->
        plet
          (phashWorkWitness # pcon PValueAndMint # 46 # (pencodeValueAndMintControlV1 # beforeAsset))
          $ \beforeAssetRoot ->
        plet (cekStateFixture (pcon PValueAndMint) 46 beforeAssetRoot 7 8) $ \postReplayAndPreAsset ->
        plet
          (phashWorkWitness # pcon PValueAndMint # 47 # (pencodeValueAndMintControlV1 # afterAsset))
          $ \afterAssetRoot ->
        plet (cekStateFixture (pcon PValueAndMint) 47 afterAssetRoot 7 8) $ \postAsset ->
        plet
          ( pcon $ PValidationOneStepWitnessV1
              (pdata $ pconstant "") (pdata postReplayAndPreAsset)
          )
          $ \replayWitness ->
        plet
          (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postAsset))
          $ \assetWitness ->
        plet
          ( pcon $ PResolvedInputReplayWitness
              (pdata 0) (pdata key) (pdata emptySchedule) (pdata descriptorCbor)
          )
          $ \replayAuxiliary ->
        plet
          ( pcon $ PValueInputAssetWitness
              (pdata 0) (pdata key) (pdata emptySchedule) (pdata descriptorCbor)
              (pdata 0) (pdata policyId) (pdata assetName) (pdata quantity)
              (pdata assetPeaks) (pdata pnil) (pdata mutation)
          )
          $ \assetAuxiliary ->
          pvalueAndMintStageTwo
            # preReplay # replayWitness # replayAuxiliary # beforeReplay
            #&& pvalueAndMintStageTwo
              # postReplayAndPreAsset # assetWitness # assetAuxiliary # beforeAsset
      _ -> pconstant False

valueDescriptorForAsset :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PByteString
valueDescriptorForAsset _policyId _assetName _quantity assetPeaks =
  plet
    (phexByteStr "a200581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a142223307")
    $ \outputCbor ->
  pmatch (ScriptContext.ptxOutSummaryV1 # outputCbor # pconstant False) $ \case
    PNothing -> perror
    PJust cardano ->
      pmatch (ScriptContext.ptxOutSummaryV1 # outputCbor # pconstant True) $ \case
        PNothing -> perror
        PJust midgard ->
          pmatch (ScriptContext.pspendDatumSummaryV1 # outputCbor) $ \case
            PNothing -> perror
            PJust spend ->
              OutputCommitment.pencodeLedgerOutputCommitment
                # ( pcon $ OutputCommitment.PLedgerOutputCommitmentV1
                      (pdata OutputCommitment.pledgerOutputCommitmentVersion)
                      (pdata 0)
                      (pdata $ plengthBS # outputCbor)
                      (pdata $ Bounded.pfromBytes # 2 # 0 # outputCbor)
                      (pdata $ phexByteStr "68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                      (pdata 1_234_567)
                      (pdata 1)
                      (pdata $ Merkle.pfrontierCommitment # 1 # assetPeaks)
                      (pdata 0)
                      (pdata $ -1)
                      (pdata $ pconstant "")
                      (pdata 0)
                      (pdata $ pconstant "")
                      (pdata cardano)
                      (pdata midgard)
                      (pdata spend)
                  )

valueAndMintOutputReplay :: forall s. Term s PBool
valueAndMintOutputReplay =
  plet (cekDescriptorForIndex 0) $ \descriptorCbor ->
  plet (ScriptProof.poutputDescriptorLeafHash # 0 # descriptorCbor) $ \descriptorLeaf ->
  plet (cekSinglePeak descriptorLeaf) $ \descriptorPeaks ->
  plet (valueOutputNativeControl descriptorPeaks) $ \nativeControl ->
  plet
    ( pcon $ PValueAccumulatorV1
        (pdata $ -1_234_567) (pdata pnull_hash) (pdata 0) (pdata 0)
    )
    $ \nextAccumulator ->
  plet (valueOutputControl nativeControl 3 0 0 (cekHash 0) pinitialValueAccumulator) $ \beforeOutput ->
  plet (valueOutputControl nativeControl 3 1 0 (cekHash 0) nextAccumulator) $ \afterOutput ->
  plet (valueOutputControl nativeControl 4 1 0 (cekHash 0) nextAccumulator) $ \afterFinish ->
  plet (cekStateFixture (pcon PValueAndMint) 50 (cekHash 0xa1) 7 8) $ \preOutput ->
  plet
    (phashWorkWitness # pcon PValueAndMint # 51 # (pencodeValueAndMintControlV1 # afterOutput))
    $ \afterOutputRoot ->
  plet (cekStateFixture (pcon PValueAndMint) 51 afterOutputRoot 7 8) $ \postOutputAndPreFinish ->
  plet
    (phashWorkWitness # pcon PValueAndMint # 52 # (pencodeValueAndMintControlV1 # afterFinish))
    $ \afterFinishRoot ->
  plet (cekStateFixture (pcon PValueAndMint) 52 afterFinishRoot 7 8) $ \postFinish ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postOutputAndPreFinish))
    $ \outputWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postFinish))
    $ \finishWitness ->
    pvalueAndMintStageThreeDescriptor
      # preOutput # outputWitness # beforeOutput # 0 # descriptorCbor # pnil
      #&& pvalueAndMintStageThreeFinish
        # postOutputAndPreFinish # finishWitness # pcon PNoAuxiliaryWitness # afterOutput

valueOutputControl :: forall s.
  Term s PNativeScriptsControlV1 ->
  Integer -> Integer -> Integer ->
  Term s PByteString -> Term s PValueAccumulatorV1 ->
  Term s PValueAndMintControlV1
valueOutputControl nativeControl stage outputCursor assetCursor valueHash accumulator =
  pcon $ PValueAndMintControlV1
    (pdata nativeControl)
    (pdata $ pconstant stage)
    (pdata pemptyResolutionScheduleHash)
    (pdata 0)
    (pdata 0)
    (pdata valueHash)
    (pdata pinitialResolutionAccumulator)
    (pdata pemptyResolutionScheduleHash)
    (pdata $ pconstant outputCursor)
    (pdata $ pconstant assetCursor)
    (pdata 0)
    (pdata accumulator)

valueOutputNativeControl :: forall s.
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) -> Term s PNativeScriptsControlV1
valueOutputNativeControl descriptorPeaks =
  pcon $ PNativeScriptsControlV1
    (pdata $ phexByteStr "01") (pdata $ phexByteStr "02")
    (pdata $ phexByteStr "03") (pdata $ phexByteStr "04")
    (pdata 0) (pdata pinitialResolutionAccumulator) (pdata 0) (pdata pnil)
    (pdata 0) (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
    (pdata 0) (pdata pnil) (pdata 0) (pdata pnil)
    (pdata 0) (pdata pnil)
    (pdata 1) (pdata descriptorPeaks) (pdata descriptorPeaks)
    (pdata 0) (pdata pnil) (pdata 0) (pdata pnil)
    (pdata 0) (pdata 0) (pdata pemptyResolutionScheduleHash)

valueAndMintOutputAssetReplay :: forall s. Term s PBool
valueAndMintOutputAssetReplay =
  plet (preplicateBS # 28 # (pintegerToByte # 0x44)) $ \policyId ->
  plet (pconstant "token") $ \assetName ->
  plet 5 $ \quantity ->
  plet (OutputCommitment.passetLeafHash # policyId # assetName # quantity) $ \assetLeaf ->
  plet (cekSinglePeak assetLeaf) $ \assetPeaks ->
  plet (valueDescriptorForAsset policyId assetName quantity assetPeaks) $ \descriptorCbor ->
  plet (ScriptProof.poutputDescriptorLeafHash # 0 # descriptorCbor) $ \descriptorLeaf ->
  plet (cekSinglePeak descriptorLeaf) $ \descriptorPeaks ->
  plet (valueOutputNativeControl descriptorPeaks) $ \nativeControl ->
  plet
    (pcon $ PValueAccumulatorV1 (pdata 1_234_567) (pdata pnull_hash) (pdata 0) (pdata 0))
    $ \baseAccumulator ->
  plet
    (pcon $ PValueAssetMutationWitnessV1 (pdata $ pconstant False) (pdata 0) (pdata pnil))
    $ \insertion ->
  pmatch
    (papplyValueAssetMutation # baseAccumulator # (policyId <> assetName) # quantity # insertion)
    $ \case
      PValueAccumulatorUpdated inputAccumulatorData ->
        plet (pfromData inputAccumulatorData) $ \inputAccumulator ->
        pmatch inputAccumulator $ \inputValue ->
        plet
          ( pcon $ PValueAccumulatorV1
              (pdata 0)
              (pvalueAccumulator'assetRoot inputValue)
              (pvalueAccumulator'seenAssetCount inputValue)
              (pvalueAccumulator'nonzeroAssetCount inputValue)
          )
          $ \beforeAssetAccumulator ->
        plet
          (pcon $ PValueAssetMutationWitnessV1 (pdata $ pconstant True) (pdata quantity) (pdata pnil))
          $ \removal ->
        pmatch
          ( papplyValueAssetMutation
              # beforeAssetAccumulator # (policyId <> assetName) # (0 - quantity) # removal
          )
          $ \case
            PValueAccumulatorUpdated nextAccumulatorData ->
              plet (pfromData nextAccumulatorData) $ \nextAccumulator ->
              plet (valueOutputControl nativeControl 3 0 0 (cekHash 0) inputAccumulator) $ \beforeOutput ->
              plet
                ( valueOutputControl
                    nativeControl 3 0 1 (pblake2b_256 # descriptorCbor) beforeAssetAccumulator
                )
                $ \beforeAsset ->
              plet (valueOutputControl nativeControl 3 1 0 (cekHash 0) nextAccumulator) $ \afterAsset ->
              plet (cekStateFixture (pcon PValueAndMint) 55 (cekHash 0xa1) 7 8) $ \preOutput ->
              plet
                (phashWorkWitness # pcon PValueAndMint # 56 # (pencodeValueAndMintControlV1 # beforeAsset))
                $ \beforeAssetRoot ->
              plet (cekStateFixture (pcon PValueAndMint) 56 beforeAssetRoot 7 8) $ \postOutputAndPreAsset ->
              plet
                (phashWorkWitness # pcon PValueAndMint # 57 # (pencodeValueAndMintControlV1 # afterAsset))
                $ \afterAssetRoot ->
              plet (cekStateFixture (pcon PValueAndMint) 57 afterAssetRoot 7 8) $ \postAsset ->
              plet
                (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postOutputAndPreAsset))
                $ \outputWitness ->
              plet
                (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postAsset))
                $ \assetWitness ->
              plet
                (pcon $ PValueOutputDescriptorWitness (pdata 0) (pdata descriptorCbor) (pdata pnil))
                $ \outputAuxiliary ->
              plet
                ( pcon $ PValueOutputAssetWitness
                    (pdata 0) (pdata descriptorCbor) (pdata 0)
                    (pdata policyId) (pdata assetName) (pdata quantity)
                    (pdata assetPeaks) (pdata pnil) (pdata removal)
                )
                $ \assetAuxiliary ->
                pvalueAndMintStageThree
                  # preOutput # outputWitness # outputAuxiliary # beforeOutput
                  #&& pvalueAndMintStageThree
                    # postOutputAndPreAsset # assetWitness # assetAuxiliary # beforeAsset
            _ -> pconstant False
      _ -> pconstant False

valueAndMintMintReplay :: forall s. Term s PBool
valueAndMintMintReplay =
  plet (preplicateBS # 28 # (pintegerToByte # 0x55)) $ \policyId ->
  plet (pconstant "minted") $ \assetName ->
  plet 7 $ \quantity ->
  plet (pmintAssetLeafHash # policyId # assetName # quantity) $ \mintLeaf ->
  plet (cekSinglePeak mintLeaf) $ \mintPeaks ->
  plet (nativeScriptsControlWithMint 1 mintPeaks) $ \nativeControl ->
  plet
    (pcon $ PValueAssetMutationWitnessV1 (pdata $ pconstant False) (pdata 0) (pdata pnil))
    $ \mutation ->
  pmatch
    (papplyValueAssetMutation # pinitialValueAccumulator # (policyId <> assetName) # quantity # mutation)
    $ \case
      PValueAccumulatorUpdated nextAccumulatorData ->
        plet (pfromData nextAccumulatorData) $ \nextAccumulator ->
        plet (valueMintControl nativeControl 4 0 pinitialValueAccumulator) $ \beforeMint ->
        plet (valueMintControl nativeControl 4 1 nextAccumulator) $ \afterMint ->
        plet (valueMintControl nativeControl 5 1 nextAccumulator) $ \afterFinish ->
        plet (cekStateFixture (pcon PValueAndMint) 60 (cekHash 0xa1) 7 8) $ \preMint ->
        plet
          (phashWorkWitness # pcon PValueAndMint # 61 # (pencodeValueAndMintControlV1 # afterMint))
          $ \afterMintRoot ->
        plet (cekStateFixture (pcon PValueAndMint) 61 afterMintRoot 7 8) $ \postMintAndPreFinish ->
        plet
          (phashWorkWitness # pcon PValueAndMint # 62 # (pencodeValueAndMintControlV1 # afterFinish))
          $ \afterFinishRoot ->
        plet (cekStateFixture (pcon PValueAndMint) 62 afterFinishRoot 7 8) $ \postFinish ->
        plet
          (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postMintAndPreFinish))
          $ \mintWitness ->
        plet
          (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata postFinish))
          $ \finishWitness ->
        plet
          ( pcon $ PValueMintAssetWitness
              (pdata 0) (pdata policyId) (pdata assetName) (pdata quantity)
              (pdata pnil) (pdata mutation)
          )
          $ \mintAuxiliary ->
          pvalueAndMintStageFour
            # preMint # mintWitness # mintAuxiliary # beforeMint
            #&& pvalueAndMintStageFour
              # postMintAndPreFinish # finishWitness # pcon PNoAuxiliaryWitness # afterMint
      _ -> pconstant False

valueMintControl :: forall s.
  Term s PNativeScriptsControlV1 -> Integer -> Integer ->
  Term s PValueAccumulatorV1 -> Term s PValueAndMintControlV1
valueMintControl nativeControl stage mintCursor accumulator =
  pcon $ PValueAndMintControlV1
    (pdata nativeControl)
    (pdata $ pconstant stage)
    (pdata pemptyResolutionScheduleHash)
    (pdata 0) (pdata 0) (pdata $ cekHash 0)
    (pdata pinitialResolutionAccumulator)
    (pdata pemptyResolutionScheduleHash)
    (pdata 0) (pdata 0)
    (pdata $ pconstant mintCursor)
    (pdata accumulator)

valueAndMintFinalize :: forall s. Term s PBool
valueAndMintFinalize =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  pmatch nativeControl $ \native ->
  pmatch cekCompactFixture $ \(PNativeTxCompact body _ _) ->
  pmatch body $ \txBody ->
  plet
    ( pcon $ PValueAccumulatorV1
        (pdata $ pbodyCompact'fee txBody) (pdata pnull_hash) (pdata 0) (pdata 0)
    )
    $ \balancedAccumulator ->
  plet
    ( pcon $ PValueAndMintControlV1
        (pdata nativeControl)
        (pdata 5)
        (pdata pemptyResolutionScheduleHash)
        (pnativeControl'resolvedInputCount native)
        (pdata 0)
        (pdata $ cekHash 0)
        (pnativeControl'resolvedInputsAccumulator native)
        (pdata pemptyResolutionScheduleHash)
        (pnativeControl'outputCount native)
        (pdata 0)
        (pnativeControl'mintCount native)
        (pdata balancedAccumulator)
    )
    $ \control ->
  plet (cekStateFixture (pcon PValueAndMint) 70 (cekHash 0xa1) 7 8) $ \pre ->
  pmatch pre $ \preState ->
  plet
    ( phashWorkWitness
        # pcon PLedgerDelta # 71
        # ( pencodeLedgerDeltaWitnessV1
              # nativeControl # pfromData (pmachineState'priorLedgerRoot preState)
              # pemptyResolutionScheduleHash
          )
    )
    $ \nextWorkRoot ->
  plet (cekStateFixture (pcon PLedgerDelta) 71 nextWorkRoot 7 8) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata post)) $ \witness ->
    pvalueAndMintStageFive
      # pre # witness # pcon PNoAuxiliaryWitness # control # body

compactBindingTransition :: forall s. Term s PBool
compactBindingTransition =
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata $ cekHash 0x21) (pdata $ cekHash 0x22) (pdata $ cekHash 0x23)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet
    ( pcon $ PNativeTxFieldPreimageLengthsV1
        0 0 0 0 0 0 0 0 0
    )
    $ \fieldLengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # fieldLengths) $ \fieldLengthsCbor ->
  pmatch cekCompactFixture $ \(PNativeTxCompact body _ validityCode) ->
  plet
    (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) validityCode)
    $ \compact ->
  pmatch compact $ \(PNativeTxCompact txBody _ _) ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # txBody) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # compactCbor # witnessSetCbor # fieldLengthsCbor
    )
    $ \commitment ->
  plet validContextCbor $ \contextCbor ->
  plet
    ( pencodeCompactBindingWitness
        # transactionId # commitment # compactCbor # witnessSetCbor
        # fieldLengthsCbor # contextCbor
    )
    $ \workCbor ->
  plet
    ( cekBoundStateFixture
        transactionId commitment (phashValidationContext # contextCbor)
        (pcon PCompactBinding) 9
        (phashWorkWitness # pcon PCompactBinding # 9 # workCbor)
        0 0
    )
    $ \pre ->
  pmatch pre $ \preState ->
  plet
    ( pcon $ PValidationMachineStateV1
        (pmachineState'machineVersion preState)
        (pmachineState'eventKeyHash preState)
        (pmachineState'transactionId preState)
        (pmachineState'transactionCommitment preState)
        (pmachineState'validationContextHash preState)
        (pmachineState'sourceKind preState)
        (pmachineState'priorLedgerRoot preState)
        (pdata $ pcon PStaticLedgerRules)
        (pdata 10)
        ( pdata $
            phashWorkWitness # pcon PStaticLedgerRules # 10
              # ( pencodeStaticRulesWitness
                    # compactCbor # witnessSetCbor # fieldLengthsCbor # contextCbor
                )
        )
        (pmachineState'executionCpu preState)
        (pmachineState'executionMemory preState)
        (pmachineState'verdict preState)
        (pmachineState'rejectionCodeHash preState)
        (pmachineState'ledgerDeltaRoot preState)
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata transition) (pdata $ pcon PNoAuxiliaryWitness)
    )
    $ \evidence ->
    pverifyCompactBindingSemanticsV1 # pre # evidence
      #&& pverifyCompactBindingOneStepV1 # pre # evidence

staticRulesAcceptedTransition :: forall s. Term s PBool
staticRulesAcceptedTransition =
  staticRulesTransition 0 False staticRulesNonemptyClaimedDeltaRoot
    staticRulesNonemptyClaimedDeltaRoot True

staticRulesRejectedTransition :: forall s. Term s PBool
staticRulesRejectedTransition =
  staticRulesTransition 1 True staticRulesNonemptyClaimedDeltaRoot
    staticRulesNonemptyClaimedDeltaRoot True

staticRulesClearedDeltaCannotReject :: forall s. Term s PBool
staticRulesClearedDeltaCannotReject =
  staticRulesTransition 1 True staticRulesNonemptyClaimedDeltaRoot
    staticRulesEmptyClaimedDeltaRoot False

staticRulesRewrittenDeltaCannotReject :: forall s. Term s PBool
staticRulesRewrittenDeltaCannotReject =
  staticRulesTransition 1 True staticRulesNonemptyClaimedDeltaRoot
    (cekHash 0xbb) False

staticRulesValidCannotReject :: forall s. Term s PBool
staticRulesValidCannotReject =
  staticRulesTransition 0 True staticRulesNonemptyClaimedDeltaRoot
    staticRulesNonemptyClaimedDeltaRoot False

staticRulesForcedInvalidEmptyDelta :: forall s. Term s PBool
staticRulesForcedInvalidEmptyDelta =
  staticRulesTransition 1 True staticRulesEmptyClaimedDeltaRoot
    staticRulesEmptyClaimedDeltaRoot True

staticRulesNonemptyClaimedDeltaRoot :: forall s. Term s PByteString
staticRulesNonemptyClaimedDeltaRoot =
  plet pemptyProofDescriptor $ \descriptor ->
  plet (pledgerDeltaOperationLeafHash # 0 # phexByteStr "010203" # pconstant "" # descriptor) $ \deletion ->
  plet (pledgerDeltaOperationLeafHash # 1 # phexByteStr "0405" # phexByteStr "060708" # descriptor) $ \insertion ->
  pmatch (Merkle.pbuildFrontier #$ pcons # pdata deletion #$ pcons # pdata insertion # pnil) $ \frontier ->
    Merkle.pfrontierCommitment
      # Merkle.pbuiltFrontier'count frontier
      # Merkle.pbuiltFrontier'peaks frontier

staticRulesEmptyClaimedDeltaRoot :: forall s. Term s PByteString
staticRulesEmptyClaimedDeltaRoot = Merkle.pfrontierCommitment # 0 # pnil

staticRulesTransition :: forall s.
  Integer ->
  Bool ->
  Term s PByteString ->
  Term s PByteString ->
  Bool ->
  Term s PBool
staticRulesTransition networkId rejected claimedDeltaRoot successorDeltaRoot expected =
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata $ cekHash 0x31) (pdata $ cekHash 0x32) (pdata $ cekHash 0x33)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet
    (pcon $ PNativeTxFieldPreimageLengthsV1 0 0 0 0 0 0 0 0 0)
    $ \fieldLengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # fieldLengths) $ \fieldLengthsCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        1_000_000
        (-1)
        200
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        (cekHash 0x37)
        (cekHash 0x38)
        (pconstant networkId)
    )
    $ \body ->
  plet
    (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0)
    $ \compact ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet
    (NativeCompact.pnativeTxProofCommitmentV1 # compactCbor # witnessSetCbor # fieldLengthsCbor)
    $ \commitment ->
  plet validContextCbor $ \contextCbor ->
  plet
    (pencodeStaticRulesWitness # compactCbor # witnessSetCbor # fieldLengthsCbor # contextCbor)
    $ \workCbor ->
  plet
    ( cekBoundStateFixture
        transactionId commitment (phashValidationContext # contextCbor)
        (pcon PStaticLedgerRules) 20
        (phashWorkWitness # pcon PStaticLedgerRules # 20 # workCbor)
        0 0
    )
    $ \basePre ->
  pmatch basePre $ \base ->
  plet
    ( pcon $ PValidationMachineStateV1
        (pmachineState'machineVersion base)
        (pmachineState'eventKeyHash base)
        (pmachineState'transactionId base)
        (pmachineState'transactionCommitment base)
        (pmachineState'validationContextHash base)
        (pdata $ pcon PForced)
        (pmachineState'priorLedgerRoot base)
        (pmachineState'phase base)
        (pmachineState'programCounter base)
        (pmachineState'workRoot base)
        (pmachineState'executionCpu base)
        (pmachineState'executionMemory base)
        (pmachineState'verdict base)
        (pmachineState'rejectionCodeHash base)
        (pdata claimedDeltaRoot)
    )
    $ \pre ->
  pmatch pre $ \preState ->
  plet
    ( if rejected
        then
          let rejectionCode = pconstant "E_NETWORK_ID_MISMATCH"
           in pcon $ PValidationMachineStateV1
                (pmachineState'machineVersion preState)
                (pmachineState'eventKeyHash preState)
                (pmachineState'transactionId preState)
                (pmachineState'transactionCommitment preState)
                (pmachineState'validationContextHash preState)
                (pmachineState'sourceKind preState)
                (pmachineState'priorLedgerRoot preState)
                (pdata $ pcon PTerminal)
                (pdata 21)
                ( pdata $
                    phashWorkWitness # pcon PTerminal # 21
                      # ( pencodeTerminalRejectionWitness
                            # rejectionCode # pfromData (pmachineState'priorLedgerRoot preState)
                        )
                )
                (pmachineState'executionCpu preState)
                (pmachineState'executionMemory preState)
                (pdata $ pcon PRejected)
                (pdata $ phashRejectionCode # rejectionCode)
                (pdata successorDeltaRoot)
        else
          pcon $ PValidationMachineStateV1
            (pmachineState'machineVersion preState)
            (pmachineState'eventKeyHash preState)
            (pmachineState'transactionId preState)
            (pmachineState'transactionCommitment preState)
            (pmachineState'validationContextHash preState)
            (pmachineState'sourceKind preState)
            (pmachineState'priorLedgerRoot preState)
            (pdata $ pcon PInputSets)
            (pdata 21)
            ( pdata $
                phashWorkWitness # pcon PInputSets # 21
                  # ( pencodeInputSetsScanWitness
                        # compactCbor # witnessSetCbor # fieldLengthsCbor # contextCbor
                        # 0 # 0 # 0 # 0 # pconstant "" # pemptyResolutionScheduleHash
                    )
            )
            (pmachineState'executionCpu preState)
            (pmachineState'executionMemory preState)
            (pmachineState'verdict preState)
            (pmachineState'rejectionCodeHash preState)
            (pmachineState'ledgerDeltaRoot preState)
    )
    $ \post ->
  pmatch post $ \postState ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata transition) (pdata $ pcon PNoAuxiliaryWitness))
    $ \evidence ->
    (pverifyStaticLedgerRulesOneStepV1 # pre # evidence #== pconstant expected)
      #&& if expected
        then
          pverifyStaticLedgerRulesSemanticsV1 # pre # evidence
            #&& pfromData (pmachineState'ledgerDeltaRoot postState)
              #== pfromData (pmachineState'ledgerDeltaRoot preState)
        else pconstant True

inputSetsSingletonProof :: forall s.
  Term s PInteger ->
  Term s PByteString ->
  Term s (PPair BoundedCollection.PItemProofV1 Bounded.PChunkProofV1)
inputSetsSingletonProof fieldIndex key =
  plet (Bounded.pfromBytes # fieldIndex # 0 # key) $ \itemCommitment ->
  plet (Bounded.phashChunk # fieldIndex # 0 # 0 # key) $ \chunkLeaf ->
  plet (cekSinglePeak chunkLeaf) $ \chunkPeaks ->
  plet
    ( BoundedCollection.phashBoundedCollectionItem
        # fieldIndex # 0 # (plengthBS # key) # itemCommitment
    )
    $ \itemLeaf ->
  plet (cekSinglePeak itemLeaf) $ \itemPeaks ->
    pcon $ PPair
      ( pcon $ BoundedCollection.PItemProofV1
          (pdata BoundedCollection.pboundedCollectionVersion)
          (pdata fieldIndex)
          (pdata 1)
          (pdata 0)
          (pdata $ plengthBS # key)
          (pdata itemCommitment)
          (pdata itemPeaks)
          (pdata pnil)
      )
      ( pcon $ Bounded.PChunkProofV1
          (pdata Bounded.pversion)
          (pdata fieldIndex)
          (pdata 0)
          (pdata $ plengthBS # key)
          (pdata 0)
          (pdata key)
          (pdata chunkPeaks)
          (pdata pnil)
      )

inputSetsSingletonCommitment :: forall s.
  Term s PInteger -> Term s PByteString -> Term s PByteString
inputSetsSingletonCommitment fieldIndex key =
  plet (Bounded.pfromBytes # fieldIndex # 0 # key) $ \itemCommitment ->
  plet
    ( BoundedCollection.phashBoundedCollectionItem
        # fieldIndex # 0 # (plengthBS # key) # itemCommitment
    )
    $ \itemLeaf ->
    BoundedCollection.pboundedCollectionCommitment
      # fieldIndex # 1 # cekSinglePeak itemLeaf

inputSetsControlFixture :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s (PPair PInputSetsControlV1 PByteString)
inputSetsControlFixture
  spendCommitment referenceCommitment validityStart validityEnd
  spendCount referenceCount spendSeen referenceSeen previousKey scheduleHash =
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet
    (pcon $ PNativeTxFieldPreimageLengthsV1 0 0 0 0 0 0 0 0 0)
    $ \fieldLengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # fieldLengths) $ \fieldLengthsCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        spendCommitment
        referenceCommitment
        NativeField.pemptyFieldCommitment
        1_000_000
        validityStart
        validityEnd
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        (cekHash 0x71)
        (cekHash 0x72)
        0
    )
    $ \body ->
  plet (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0) $ \compact ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet validContextCbor $ \contextCbor ->
    pcon $ PPair
      ( pcon $ PInputSetsControlV1
          (pdata compactCbor)
          (pdata witnessSetCbor)
          (pdata fieldLengthsCbor)
          (pdata contextCbor)
          (pdata spendCount)
          (pdata referenceCount)
          (pdata spendSeen)
          (pdata referenceSeen)
          (pdata previousKey)
          (pdata scheduleHash)
      )
      transactionId

inputSetsWitnessCbor :: forall s. Term s PInputSetsControlV1 -> Term s PByteString
inputSetsWitnessCbor control = pmatch control $ \c ->
  pencodeInputSetsScanWitness
    # pfromData (pinputSets'compactCbor c)
    # pfromData (pinputSets'witnessSetCompactCbor c)
    # pfromData (pinputSets'fieldPreimageLengthsCbor c)
    # pfromData (pinputSets'contextCbor c)
    # pfromData (pinputSets'spendCount c)
    # pfromData (pinputSets'referenceCount c)
    # pfromData (pinputSets'spendSeen c)
    # pfromData (pinputSets'referenceSeen c)
    # pfromData (pinputSets'previousKey c)
    # pfromData (pinputSets'resolutionScheduleHash c)

inputSetsStateFixture :: forall s.
  Term s PByteString ->
  Term s PInputSetsControlV1 ->
  Term s PInteger ->
  Term s PValidationMachineStateV1
inputSetsStateFixture transactionId control counter = pmatch control $ \c ->
  plet (inputSetsWitnessCbor control) $ \workCbor ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pinputSets'compactCbor c)
        # pfromData (pinputSets'witnessSetCompactCbor c)
        # pfromData (pinputSets'fieldPreimageLengthsCbor c)
    )
    $ \transactionCommitment ->
    cekBoundStateFixture
      transactionId transactionCommitment
      (phashValidationContext # pfromData (pinputSets'contextCbor c))
      (pcon PInputSets) counter
      (phashWorkWitness # pcon PInputSets # counter # workCbor)
      0 0

inputSetsExactRejection :: forall s.
  Term s PValidationMachineStateV1 ->
  Term s PByteString ->
  Term s PValidationMachineStateV1
inputSetsExactRejection pre rejectionCode = pmatch pre $ \preState ->
  pcon $ PValidationMachineStateV1
    (pmachineState'machineVersion preState)
    (pmachineState'eventKeyHash preState)
    (pmachineState'transactionId preState)
    (pmachineState'transactionCommitment preState)
    (pmachineState'validationContextHash preState)
    (pmachineState'sourceKind preState)
    (pmachineState'priorLedgerRoot preState)
    (pdata $ pcon PTerminal)
    (pdata $ pfromData (pmachineState'programCounter preState) + 1)
    ( pdata $
        phashWorkWitness # pcon PTerminal
          # (pfromData (pmachineState'programCounter preState) + 1)
          # ( pencodeTerminalRejectionWitness
                # rejectionCode # pfromData (pmachineState'priorLedgerRoot preState)
            )
    )
    (pmachineState'executionCpu preState)
    (pmachineState'executionMemory preState)
    (pdata $ pcon PRejected)
    (pdata $ phashRejectionCode # rejectionCode)
    (pmachineState'ledgerDeltaRoot preState)

inputSetsSignaturesPost :: forall s.
  Term s PValidationMachineStateV1 ->
  Term s PInputSetsControlV1 ->
  Term s PByteString ->
  Term s PValidationMachineStateV1
inputSetsSignaturesPost pre control scheduleHash =
  pmatch pre $ \preState ->
  pmatch control $ \c ->
  plet
    ( pencodeSignaturesScanWitness
        # pfromData (pinputSets'compactCbor c)
        # pfromData (pinputSets'witnessSetCompactCbor c)
        # pfromData (pinputSets'fieldPreimageLengthsCbor c)
        # pfromData (pinputSets'contextCbor c)
        # scheduleHash # 0 # 0 # 0 # 0 # 0
        # pconstant "" # pconstant "" # 0 # pnil # 0
    )
    $ \workCbor ->
    pcon $ PValidationMachineStateV1
      (pmachineState'machineVersion preState)
      (pmachineState'eventKeyHash preState)
      (pmachineState'transactionId preState)
      (pmachineState'transactionCommitment preState)
      (pmachineState'validationContextHash preState)
      (pmachineState'sourceKind preState)
      (pmachineState'priorLedgerRoot preState)
      (pdata $ pcon PSignatures)
      (pdata $ pfromData (pmachineState'programCounter preState) + 1)
      ( pdata $
          phashWorkWitness # pcon PSignatures
            # (pfromData (pmachineState'programCounter preState) + 1) # workCbor
      )
      (pmachineState'executionCpu preState)
      (pmachineState'executionMemory preState)
      (pmachineState'verdict preState)
      (pmachineState'rejectionCodeHash preState)
      (pmachineState'ledgerDeltaRoot preState)

inputSetsItemEvidence :: forall s.
  Term s PValidationOneStepWitnessV1 ->
  Term s BoundedCollection.PItemProofV1 ->
  Term s Bounded.PChunkProofV1 ->
  Term s PValidationOneStepEvidenceV1
inputSetsItemEvidence witness collectionProof chunkProof =
  pcon $ PValidationOneStepEvidenceV1
    (pdata witness)
    (pdata $ pcon $ PTransactionFieldChunkWitness (pdata collectionProof) (pdata chunkProof))

inputSetsEmptyTransitions :: forall s. Term s PBool
inputSetsEmptyTransitions =
  pmatch
    ( inputSetsControlFixture
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        (-1) (-1) 0 0 0 0 (pconstant "") pemptyResolutionScheduleHash
    )
    $ \(PPair control transactionId) ->
  plet (inputSetsStateFixture transactionId control 2) $ \pre ->
  plet (inputSetsExactRejection pre $ pconstant "E_EMPTY_INPUTS") $ \rejection ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ inputSetsWitnessCbor control) (pdata rejection)) $ \witness ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata witness) (pdata $ pcon PNoAuxiliaryWitness))
    $ \evidence ->
  pmatch rejection $ \rejectedState ->
  plet
    ( pcon $ PValidationMachineStateV1
        (pmachineState'machineVersion rejectedState)
        (pmachineState'eventKeyHash rejectedState)
        (pmachineState'transactionId rejectedState)
        (pmachineState'transactionCommitment rejectedState)
        (pmachineState'validationContextHash rejectedState)
        (pmachineState'sourceKind rejectedState)
        (pmachineState'priorLedgerRoot rejectedState)
        (pdata $ pcon PStaticLedgerRules)
        (pmachineState'programCounter rejectedState)
        (pmachineState'workRoot rejectedState)
        (pmachineState'executionCpu rejectedState)
        (pmachineState'executionMemory rejectedState)
        (pmachineState'verdict rejectedState)
        (pmachineState'rejectionCodeHash rejectedState)
        (pmachineState'ledgerDeltaRoot rejectedState)
    )
    $ \wrongPhase ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ inputSetsWitnessCbor control) (pdata wrongPhase)) $ \wrongWitness ->
    pand'List
      [ pverifyInputSetsEmptySemanticsV1 # pre # witness
      , pverifyInputSetsOneStepV1 # pre # evidence
      , pnot # (pverifyInputSetsEmptySemanticsV1 # pre # wrongWitness)
      ]

inputSetsDisjointTransition :: forall s. Term s PBool
inputSetsDisjointTransition =
  plet (pencodeMidgardTxInput # pcon (PMidgardTxInput (pdata $ cekHash 0xaa) (pdata 0))) $ \spendKey ->
  plet (pencodeMidgardTxInput # pcon (PMidgardTxInput (pdata $ cekHash 0xbb) (pdata 1))) $ \referenceKey ->
  plet (inputSetsSingletonCommitment 0 spendKey) $ \spendCommitment ->
  plet (inputSetsSingletonCommitment 1 referenceKey) $ \referenceCommitment ->
  plet (presolutionScheduleNodeHash # 1 # referenceKey # pemptyResolutionScheduleHash) $ \firstSchedule ->
  plet (presolutionScheduleNodeHash # 0 # spendKey # firstSchedule) $ \fullSchedule ->
  pmatch
    ( inputSetsControlFixture spendCommitment referenceCommitment (-1) (-1)
        (-1) (-1) 0 0 (pconstant "") pemptyResolutionScheduleHash
    )
    $ \(PPair firstControl transactionId) ->
  pmatch
    ( inputSetsControlFixture spendCommitment referenceCommitment (-1) (-1)
        (-1) 1 0 1 referenceKey firstSchedule
    )
    $ \(PPair secondControl _) ->
  plet (inputSetsStateFixture transactionId firstControl 2) $ \firstPre ->
  plet (inputSetsStateFixture transactionId secondControl 3) $ \secondPre ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ inputSetsWitnessCbor firstControl) (pdata secondPre)) $ \firstWitness ->
  pmatch (inputSetsSingletonProof 1 referenceKey) $ \(PPair referenceCollection referenceChunk) ->
  plet (inputSetsSignaturesPost secondPre secondControl fullSchedule) $ \signaturesPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ inputSetsWitnessCbor secondControl) (pdata signaturesPost)) $ \secondWitness ->
  pmatch (inputSetsSingletonProof 0 spendKey) $ \(PPair spendCollection spendChunk) ->
    pand'List
      [ pverifyInputSetsItemSemanticsV1
          # firstPre # firstWitness # referenceCollection # referenceChunk
      , pverifyInputSetsItemSemanticsV1
          # secondPre # secondWitness # spendCollection # spendChunk
      , pverifyInputSetsOneStepV1
          # secondPre # inputSetsItemEvidence secondWitness spendCollection spendChunk
      , spendKey #< referenceKey
      ]

inputSetsDuplicateTransition :: forall s. Term s PBool
inputSetsDuplicateTransition =
  plet (pencodeMidgardTxInput # pcon (PMidgardTxInput (pdata $ cekHash 0xaa) (pdata 0))) $ \key ->
  plet (inputSetsSingletonCommitment 0 key) $ \spendCommitment ->
  plet (inputSetsSingletonCommitment 1 key) $ \referenceCommitment ->
  plet (presolutionScheduleNodeHash # 1 # key # pemptyResolutionScheduleHash) $ \firstSchedule ->
  pmatch
    ( inputSetsControlFixture spendCommitment referenceCommitment (-1) (-1)
        (-1) (-1) 0 0 (pconstant "") pemptyResolutionScheduleHash
    )
    $ \(PPair firstControl transactionId) ->
  pmatch
    ( inputSetsControlFixture spendCommitment referenceCommitment (-1) (-1)
        (-1) 1 0 1 key firstSchedule
    )
    $ \(PPair secondControl _) ->
  plet (inputSetsStateFixture transactionId firstControl 2) $ \firstPre ->
  plet (inputSetsStateFixture transactionId secondControl 3) $ \secondPre ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ inputSetsWitnessCbor firstControl) (pdata secondPre)) $ \firstWitness ->
  pmatch (inputSetsSingletonProof 1 key) $ \(PPair referenceCollection referenceChunk) ->
  plet (inputSetsExactRejection secondPre $ pconstant "E_DUPLICATE_INPUT_IN_TX") $ \rejection ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ inputSetsWitnessCbor secondControl) (pdata rejection)) $ \secondWitness ->
  pmatch (inputSetsSingletonProof 0 key) $ \(PPair spendCollection spendChunk) ->
    pverifyInputSetsItemSemanticsV1
      # firstPre # firstWitness # referenceCollection # referenceChunk
      #&& pverifyInputSetsItemSemanticsV1
        # secondPre # secondWitness # spendCollection # spendChunk

inputSetsMalformedValidityTransition :: forall s. Term s PBool
inputSetsMalformedValidityTransition =
  plet (pencodeMidgardTxInput # pcon (PMidgardTxInput (pdata $ cekHash 0xaa) (pdata 0))) $ \key ->
  plet (inputSetsSingletonCommitment 0 key) $ \spendCommitment ->
  pmatch
    ( inputSetsControlFixture spendCommitment NativeField.pemptyFieldCommitment 10 9
        (-1) 0 0 0 (pconstant "") pemptyResolutionScheduleHash
    )
    $ \(PPair control transactionId) ->
  plet (inputSetsStateFixture transactionId control 2) $ \pre ->
  plet (inputSetsExactRejection pre $ pconstant "E_INVALID_VALIDITY_INTERVAL_FORMAT") $ \rejection ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ inputSetsWitnessCbor control) (pdata rejection)) $ \witness ->
  pmatch (inputSetsSingletonProof 0 key) $ \(PPair collectionProof chunkProof) ->
    pverifyInputSetsItemSemanticsV1
      # pre # witness # collectionProof # chunkProof

signaturesControlFixture :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PInteger ->
  Term s (PPair PSignaturesControlV1 PByteString)
signaturesControlFixture
  addressCommitment scriptCommitment requiredCommitment stage addressCount
  requiredCount addressSeen requiredSeen signerCount signerPeaks invalidSeen =
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata addressCommitment)
        (pdata scriptCommitment)
        (pdata NativeField.pemptyFieldCommitment)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet (pcon $ PNativeTxFieldPreimageLengthsV1 0 0 0 0 0 0 0 0 0) $ \fieldLengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # fieldLengths) $ \fieldLengthsCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        1_000_000
        (-1)
        (-1)
        NativeField.pemptyFieldCommitment
        requiredCommitment
        NativeField.pemptyFieldCommitment
        (cekHash 0x81)
        (cekHash 0x82)
        0
    )
    $ \body ->
  plet (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0) $ \compact ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet validContextCbor $ \contextCbor ->
    pcon $ PPair
      ( pcon $ PSignaturesControlV1
          (pdata compactCbor)
          (pdata witnessSetCbor)
          (pdata fieldLengthsCbor)
          (pdata contextCbor)
          (pdata pemptyResolutionScheduleHash)
          (pdata stage)
          (pdata addressCount)
          (pdata requiredCount)
          (pdata addressSeen)
          (pdata requiredSeen)
          (pdata $ pconstant "")
          (pdata $ pconstant "")
          (pdata signerCount)
          (pdata signerPeaks)
          (pdata invalidSeen)
      )
      transactionId

signaturesWitnessCbor :: forall s. Term s PSignaturesControlV1 -> Term s PByteString
signaturesWitnessCbor control = pmatch control $ \c ->
  pencodeSignaturesScanWitness
    # pfromData (psignatures'compactCbor c)
    # pfromData (psignatures'witnessSetCompactCbor c)
    # pfromData (psignatures'fieldPreimageLengthsCbor c)
    # pfromData (psignatures'contextCbor c)
    # pfromData (psignatures'resolutionScheduleHash c)
    # pfromData (psignatures'stage c)
    # pfromData (psignatures'addressCount c)
    # pfromData (psignatures'requiredCount c)
    # pfromData (psignatures'addressSeen c)
    # pfromData (psignatures'requiredSeen c)
    # pfromData (psignatures'previousOrderKey c)
    # pfromData (psignatures'previousSignerHash c)
    # pfromData (psignatures'signerCount c)
    # pfromData (psignatures'signerPeaks c)
    # pfromData (psignatures'invalidSignatureSeen c)

signaturesStateFixture :: forall s.
  Term s PByteString ->
  Term s PSignaturesControlV1 ->
  Term s PInteger ->
  Term s PValidationMachineStateV1
signaturesStateFixture transactionId control counter = pmatch control $ \c ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (psignatures'compactCbor c)
        # pfromData (psignatures'witnessSetCompactCbor c)
        # pfromData (psignatures'fieldPreimageLengthsCbor c)
    )
    $ \commitment ->
  plet (signaturesWitnessCbor control) $ \workCbor ->
    cekBoundStateFixture
      transactionId commitment
      (phashValidationContext # pfromData (psignatures'contextCbor c))
      (pcon PSignatures) counter
      (phashWorkWitness # pcon PSignatures # counter # workCbor)
      0 0

signaturesPhaseAPost :: forall s.
  Term s PValidationMachineStateV1 ->
  Term s PSignaturesControlV1 ->
  Term s PInteger ->
  Term s PValidationMachineStateV1
signaturesPhaseAPost pre control result =
  pmatch pre $ \preState ->
  pmatch control $ \c ->
  plet
    ( pencodePhaseANativeScriptsScanWitness
        # pfromData (psignatures'compactCbor c)
        # pfromData (psignatures'witnessSetCompactCbor c)
        # pfromData (psignatures'fieldPreimageLengthsCbor c)
        # pfromData (psignatures'contextCbor c)
        # pfromData (psignatures'resolutionScheduleHash c)
        # 0 # 0 # 0 # 0 # 0 # pconstant ""
        # 0 # pconstant "" # 0 # 0 # result
        # pfromData (psignatures'signerCount c)
        # pfromData (psignatures'signerPeaks c)
        # pconstant ""
    )
    $ \workCbor ->
    pcon $ PValidationMachineStateV1
      (pmachineState'machineVersion preState)
      (pmachineState'eventKeyHash preState)
      (pmachineState'transactionId preState)
      (pmachineState'transactionCommitment preState)
      (pmachineState'validationContextHash preState)
      (pmachineState'sourceKind preState)
      (pmachineState'priorLedgerRoot preState)
      (pdata $ pcon PPhaseANativeScripts)
      (pdata $ pfromData (pmachineState'programCounter preState) + 1)
      ( pdata $
          phashWorkWitness # pcon PPhaseANativeScripts
            # (pfromData (pmachineState'programCounter preState) + 1) # workCbor
      )
      (pmachineState'executionCpu preState)
      (pmachineState'executionMemory preState)
      (pmachineState'verdict preState)
      (pmachineState'rejectionCodeHash preState)
      (pmachineState'ledgerDeltaRoot preState)

signaturesEmptyTransitions :: forall s. Term s PBool
signaturesEmptyTransitions =
  pmatch
    ( signaturesControlFixture
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment 0 0 0 0 0 0 pnil 0
    )
    $ \(PPair addressControl transactionId) ->
  pmatch
    ( signaturesControlFixture
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment 1 0 0 0 0 0 pnil 0
    )
    $ \(PPair requiredControl _) ->
  pmatch
    ( signaturesControlFixture
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment 2 0 0 0 0 0 pnil 0
    )
    $ \(PPair handoffControl _) ->
  plet (signaturesStateFixture transactionId addressControl 4) $ \addressPre ->
  plet (signaturesStateFixture transactionId requiredControl 5) $ \requiredPre ->
  plet (signaturesStateFixture transactionId handoffControl 6) $ \handoffPre ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ signaturesWitnessCbor addressControl) (pdata requiredPre)) $ \addressWitness ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ signaturesWitnessCbor requiredControl) (pdata handoffPre)) $ \requiredWitness ->
  plet (signaturesPhaseAPost handoffPre handoffControl (-1)) $ \phaseAPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ signaturesWitnessCbor handoffControl) (pdata phaseAPost)) $ \handoffWitness ->
  plet (signaturesPhaseAPost handoffPre handoffControl 0) $ \falseVerdictPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ signaturesWitnessCbor handoffControl) (pdata falseVerdictPost)) $ \falseVerdictWitness ->
  plet (signaturesPhaseAPost handoffPre handoffControl 1) $ \trueVerdictPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ signaturesWitnessCbor handoffControl) (pdata trueVerdictPost)) $ \trueVerdictWitness ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata handoffWitness) (pdata $ pcon PNoAuxiliaryWitness)
    )
    $ \handoffEvidence ->
    pand'List
      [ pverifySignaturesAdvanceSemanticsV1 # addressPre # addressWitness
      , pverifySignaturesAdvanceSemanticsV1 # requiredPre # requiredWitness
      , pverifySignaturesHandoffSemanticsV1 # handoffPre # handoffWitness
      , pverifySignaturesOneStepV1 # handoffPre # handoffEvidence
      , pnot # (pverifySignaturesHandoffSemanticsV1 # handoffPre # falseVerdictWitness)
      , pnot # (pverifySignaturesHandoffSemanticsV1 # handoffPre # trueVerdictWitness)
      ]

signaturesMissingRequiredTransition :: forall s. Term s PBool
signaturesMissingRequiredTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x91)) $ \signerHash ->
  plet (inputSetsSingletonCommitment 4 signerHash) $ \requiredCommitment ->
  pmatch
    ( signaturesControlFixture
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        requiredCommitment 0 0 (-1) 0 0 0 pnil 0
    )
    $ \(PPair addressControl transactionId) ->
  pmatch
    ( signaturesControlFixture
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        requiredCommitment 1 0 (-1) 0 0 0 pnil 0
    )
    $ \(PPair requiredControl _) ->
  plet (signaturesStateFixture transactionId addressControl 4) $ \addressPre ->
  plet (signaturesStateFixture transactionId requiredControl 5) $ \requiredPre ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ signaturesWitnessCbor addressControl) (pdata requiredPre)) $ \addressWitness ->
  plet (inputSetsExactRejection requiredPre $ pconstant "E_MISSING_REQUIRED_WITNESS") $ \rejection ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ signaturesWitnessCbor requiredControl) (pdata rejection)) $ \requiredWitness ->
  pmatch (inputSetsSingletonProof 4 signerHash) $ \(PPair collectionProof chunkProof) ->
  plet (pcon $ PEmptySignerSetProof $ pdata pnil) $ \nonMembershipProof ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata requiredWitness)
        ( pdata $ pcon $ PRequiredSignerItemWitness
            (pdata collectionProof) (pdata chunkProof) (pdata nonMembershipProof)
        )
    )
    $ \evidence ->
    pverifySignaturesAdvanceSemanticsV1 # addressPre # addressWitness
      #&& pverifyRequiredSignerItemSemanticsV1
        # requiredPre # requiredWitness # collectionProof # chunkProof # nonMembershipProof
      #&& pverifySignaturesOneStepV1 # requiredPre # evidence

signaturesInvalidAddressTransition :: forall s. Term s PBool
signaturesInvalidAddressTransition =
  plet
    ( pcon $ PMidgardAddressWitness
        (pdata $ cekHash 0)
        (pdata $ preplicateBS # 64 # (pintegerToByte # 0))
    )
    $ \addressWitnessValue ->
  plet (pencodeMidgardAddressWitness # addressWitnessValue) $ \addressWitnessCbor ->
  plet (inputSetsSingletonCommitment 7 addressWitnessCbor) $ \addressCommitment ->
  plet (pblake2b_224 # cekHash 0) $ \signerHash ->
  plet (cekSinglePeak $ ScriptProof.psignerLeafHash # signerHash) $ \signerPeaks ->
  pmatch
    ( signaturesControlFixture
        addressCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment 0 (-1) 0 0 0 0 pnil 0
    )
    $ \(PPair addressControl transactionId) ->
  pmatch
    ( signaturesControlFixture
        addressCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment 1 1 0 1 0 1 signerPeaks 1
    )
    $ \(PPair requiredControl _) ->
  plet (signaturesStateFixture transactionId addressControl 4) $ \addressPre ->
  plet (signaturesStateFixture transactionId requiredControl 5) $ \requiredPre ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ signaturesWitnessCbor addressControl) (pdata requiredPre)) $ \addressWitness ->
  pmatch (inputSetsSingletonProof 7 addressWitnessCbor) $ \(PPair collectionProof chunkProof) ->
  plet (inputSetsExactRejection requiredPre $ pconstant "E_INVALID_SIGNATURE") $ \rejection ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ signaturesWitnessCbor requiredControl) (pdata rejection)) $ \requiredWitness ->
    pverifySignatureAddressItemSemanticsV1
      # addressPre # addressWitness # collectionProof # chunkProof
      #&& pverifySignaturesAdvanceSemanticsV1 # requiredPre # requiredWitness

phaseANativeControlFixture :: forall s.
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PPair PPhaseANativeScriptsControlV1 PByteString)
phaseANativeControlFixture
  scriptCommitment validityStart validityEnd stage scriptCount scriptSeen
  containsNonNative itemLength itemCommitment cursor nodeCount result =
  phaseANativeControlFixtureWithRedeemer
    scriptCommitment NativeField.pemptyFieldCommitment
    validityStart validityEnd stage scriptCount scriptSeen containsNonNative
    itemLength itemCommitment cursor nodeCount result

phaseANativeControlFixtureWithRedeemer :: forall s.
  Term s PByteString -> Term s PByteString ->
  Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s PInteger -> Term s PByteString -> Term s PInteger ->
  Term s PInteger -> Term s PInteger ->
  Term s (PPair PPhaseANativeScriptsControlV1 PByteString)
phaseANativeControlFixtureWithRedeemer
  scriptCommitment redeemerCommitment validityStart validityEnd stage
  scriptCount scriptSeen containsNonNative itemLength itemCommitment
  cursor nodeCount result =
  phaseANativeControlFixtureWithOutputs
    scriptCommitment redeemerCommitment NativeField.pemptyFieldCommitment
    validityStart validityEnd stage scriptCount scriptSeen containsNonNative
    itemLength itemCommitment cursor nodeCount result

phaseANativeControlFixtureWithOutputs :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PByteString ->
  Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s PInteger -> Term s PByteString -> Term s PInteger ->
  Term s PInteger -> Term s PInteger ->
  Term s (PPair PPhaseANativeScriptsControlV1 PByteString)
phaseANativeControlFixtureWithOutputs
  scriptCommitment redeemerCommitment outputsCommitment validityStart validityEnd
  stage scriptCount scriptSeen containsNonNative itemLength itemCommitment
  cursor nodeCount result =
  phaseANativeControlFixtureWithOutputsAndObservers
    scriptCommitment redeemerCommitment outputsCommitment
    NativeField.pemptyFieldCommitment validityStart validityEnd stage
    scriptCount scriptSeen containsNonNative itemLength itemCommitment
    cursor nodeCount result

phaseANativeControlFixtureWithOutputsAndObservers :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PByteString ->
  Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s PInteger -> Term s PByteString -> Term s PInteger ->
  Term s PInteger -> Term s PInteger ->
  Term s (PPair PPhaseANativeScriptsControlV1 PByteString)
phaseANativeControlFixtureWithOutputsAndObservers
  scriptCommitment redeemerCommitment outputsCommitment observerCommitment
  validityStart validityEnd stage scriptCount scriptSeen containsNonNative
  itemLength itemCommitment cursor nodeCount result =
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata NativeField.pemptyFieldCommitment)
        (pdata scriptCommitment)
        (pdata redeemerCommitment)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet (pcon $ PNativeTxFieldPreimageLengthsV1 0 0 0 0 0 0 0 0 0) $ \fieldLengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # fieldLengths) $ \fieldLengthsCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        outputsCommitment
        1_000_000 validityStart validityEnd
        observerCommitment
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        (cekHash 0xa1) (cekHash 0xa2) 0
    )
    $ \body ->
  plet (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0) $ \compact ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet validContextCbor $ \contextCbor ->
    pcon $ PPair
      ( pcon $ PPhaseANativeScriptsControlV1
          (pdata compactCbor)
          (pdata witnessSetCbor)
          (pdata fieldLengthsCbor)
          (pdata contextCbor)
          (pdata pemptyResolutionScheduleHash)
          (pdata stage)
          (pdata scriptCount)
          (pdata scriptSeen)
          (pdata containsNonNative)
          (pdata itemLength)
          (pdata itemCommitment)
          (pdata cursor)
          (pdata $ pconstant "")
          (pdata 0)
          (pdata nodeCount)
          (pdata result)
          (pdata 0)
          (pdata pnil)
          (pdata $ pconstant "")
      )
      transactionId

phaseANativeWitnessCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PByteString
phaseANativeWitnessCbor control = pmatch control $ \c ->
  pencodePhaseANativeScriptsScanWitness
    # pfromData (pphaseANative'compactCbor c)
    # pfromData (pphaseANative'witnessSetCompactCbor c)
    # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    # pfromData (pphaseANative'contextCbor c)
    # pfromData (pphaseANative'resolutionScheduleHash c)
    # pfromData (pphaseANative'stage c)
    # pfromData (pphaseANative'scriptCount c)
    # pfromData (pphaseANative'scriptSeen c)
    # pfromData (pphaseANative'containsNonNativeScript c)
    # pfromData (pphaseANative'itemLength c)
    # pfromData (pphaseANative'itemCommitment c)
    # pfromData (pphaseANative'cursor c)
    # pfromData (pphaseANative'stackRoot c)
    # pfromData (pphaseANative'stackDepth c)
    # pfromData (pphaseANative'nodeCount c)
    # pfromData (pphaseANative'result c)
    # pfromData (pphaseANative'signerCount c)
    # pfromData (pphaseANative'signerPeaks c)
    # pfromData (pphaseANative'continuationCbor c)

phaseANativeStateFixture :: forall s.
  Term s PByteString ->
  Term s PPhaseANativeScriptsControlV1 ->
  Term s PInteger ->
  Term s PValidationMachineStateV1
phaseANativeStateFixture transactionId control counter = pmatch control $ \c ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \commitment ->
  plet (phaseANativeWitnessCbor control) $ \workCbor ->
    cekBoundStateFixture
      transactionId commitment
      (phashValidationContext # pfromData (pphaseANative'contextCbor c))
      (pcon PPhaseANativeScripts) counter
      (phashWorkWitness # pcon PPhaseANativeScripts # counter # workCbor)
      0 0

phaseAPreconditionsPost :: forall s.
  Term s PValidationMachineStateV1 ->
  Term s PPhaseANativeScriptsControlV1 ->
  Term s PValidationMachineStateV1
phaseAPreconditionsPost pre control =
  pmatch pre $ \preState ->
  pmatch control $ \c ->
  plet
    ( pencodePhaseAScriptPreconditionsWitness
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
        # pfromData (pphaseANative'contextCbor c)
        # pfromData (pphaseANative'resolutionScheduleHash c)
        # pfromData (pphaseANative'signerCount c)
        # (Merkle.pfrontierCommitment # pfromData (pphaseANative'signerCount c) # pfromData (pphaseANative'signerPeaks c))
        # (pfromData (pphaseANative'containsNonNativeScript c) #== 1)
        # 0 # 0 # pconstant ""
    )
    $ \workCbor ->
    pcon $ PValidationMachineStateV1
      (pmachineState'machineVersion preState)
      (pmachineState'eventKeyHash preState)
      (pmachineState'transactionId preState)
      (pmachineState'transactionCommitment preState)
      (pmachineState'validationContextHash preState)
      (pmachineState'sourceKind preState)
      (pmachineState'priorLedgerRoot preState)
      (pdata $ pcon PPhaseAScriptPreconditions)
      (pdata $ pfromData (pmachineState'programCounter preState) + 1)
      ( pdata $
          phashWorkWitness # pcon PPhaseAScriptPreconditions
            # (pfromData (pmachineState'programCounter preState) + 1) # workCbor
      )
      (pmachineState'executionCpu preState)
      (pmachineState'executionMemory preState)
      (pmachineState'verdict preState)
      (pmachineState'rejectionCodeHash preState)
      (pmachineState'ledgerDeltaRoot preState)

phaseANativeEnvelopeTransitions :: forall s. Term s PBool
phaseANativeEnvelopeTransitions =
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair emptyControl emptyTransactionId) ->
  plet (phaseANativeStateFixture emptyTransactionId emptyControl 7) $ \emptyPre ->
  plet (phaseAPreconditionsPost emptyPre emptyControl) $ \emptyPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor emptyControl) (pdata emptyPost)) $ \emptyWitness ->
  plet (phexByteStr "82004482041864") $ \nativeItemCbor ->
  plet (inputSetsSingletonCommitment 6 nativeItemCbor) $ \scriptCommitment ->
  plet (Bounded.pfromBytes # 6 # 0 # nativeItemCbor) $ \itemCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 0 (-1) 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair itemControl transactionId) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 1 1 0 0 7 itemCommitment 3 0 (-1)
    )
    $ \(PPair tokenControl _) ->
  plet (phaseANativeStateFixture transactionId itemControl 8) $ \itemPre ->
  plet (phaseANativeStateFixture transactionId tokenControl 9) $ \tokenPre ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor itemControl) (pdata tokenPre)) $ \itemWitness ->
  pmatch (inputSetsSingletonProof 6 nativeItemCbor) $ \(PPair collectionProof chunkProof) ->
    pverifyPhaseANativeAdvanceSemanticsV1 # emptyPre # emptyWitness
      #&& pverifyPhaseANativeItemSemanticsV1
        # itemPre # itemWitness # collectionProof # chunkProof

phaseANativeNonNativeTransition :: forall s. Term s PBool
phaseANativeNonNativeTransition =
  plet (phexByteStr "820340") $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 0 (-1) 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair control transactionId) ->
  plet (phaseANativeStateFixture transactionId control 10) $ \pre ->
  plet (presetPhaseANativeControl # control # 1 # 1 # 1) $ \completedControl ->
  plet (phaseAPreconditionsPost pre completedControl) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor control) (pdata post)) $ \witness ->
  pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair collectionProof chunkProof) ->
    pverifyPhaseANativeItemSemanticsV1
      # pre # witness # collectionProof # chunkProof

phaseANativeMalformedItemTransition :: forall s. Term s PBool
phaseANativeMalformedItemTransition =
  plet (phexByteStr "01") $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 0 (-1) 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair control transactionId) ->
  plet (phaseANativeStateFixture transactionId control 11) $ \pre ->
  plet (inputSetsExactRejection pre $ pconstant "E_INVALID_FIELD_TYPE") $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor control) (pdata post)) $ \witness ->
  pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair collectionProof chunkProof) ->
    pverifyPhaseANativeItemSemanticsV1
      # pre # witness # collectionProof # chunkProof

phaseANativeTimelockTransition :: forall s. Term s PBool
phaseANativeTimelockTransition =
  plet (phexByteStr "82004482041864") $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  plet (Bounded.pfromBytes # 6 # 0 # itemCbor) $ \itemCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment 100 (-1) 0 (-1) 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair itemControl transactionId) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment 100 (-1) 1 1 0 0 7 itemCommitment 3 0 (-1)
    )
    $ \(PPair tokenControl _) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment 100 (-1) 2 1 0 0 7 itemCommitment 7 1 1
    )
    $ \(PPair resultControl _) ->
  plet (phaseANativeStateFixture transactionId itemControl 12) $ \itemPre ->
  plet (phaseANativeStateFixture transactionId tokenControl 13) $ \tokenPre ->
  plet (phaseANativeStateFixture transactionId resultControl 14) $ \resultPre ->
  plet (presetPhaseANativeControl # resultControl # 1 # 1 # 0) $ \completedControl ->
  plet (phaseAPreconditionsPost resultPre completedControl) $ \post ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor itemControl) (pdata tokenPre))
    $ \itemWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor tokenControl) (pdata resultPre))
    $ \tokenWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor resultControl) (pdata post))
    $ \finalizeWitness ->
  pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair collectionProof chunkProof) ->
    pand'List
      [ pverifyPhaseANativeItemSemanticsV1
          # itemPre # itemWitness # collectionProof # chunkProof
      , pverifyPhaseANativeTokenSemanticsV1
          # tokenPre # tokenWitness # chunkProof # pcon PDNothing
          # pcon PNoSignerSetProof
      , pverifyPhaseANativeTimelockTokenSemanticsV1
          # tokenPre # tokenWitness # chunkProof # pcon PDNothing
      , pverifyPhaseANativeAdvanceSemanticsV1 # resultPre # finalizeWitness
      ]

phaseANativeSplitTimelockTransition :: forall s. Term s PBool
phaseANativeSplitTimelockTransition =
  plet (phexByteStr "82004482041864") $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  plet (Bounded.pfromBytes # 6 # 0 # itemCbor) $ \itemCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment 100 (-1) 1 1 0 0 7 itemCommitment 3 0 (-1)
    )
    $ \(PPair headControl transactionId) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment 100 (-1) 7 1 0 0 7 itemCommitment 5 1 (-1)
    )
    $ \(PPair payloadControl _) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment 100 (-1) 2 1 0 0 7 itemCommitment 7 1 1
    )
    $ \(PPair resultControl _) ->
  plet (phaseANativeStateFixture transactionId headControl 15) $ \headPre ->
  plet (phaseANativeStateFixture transactionId payloadControl 16) $ \payloadPre ->
  plet (phaseANativeStateFixture transactionId resultControl 17) $ \resultPre ->
  plet (presetPhaseANativeControl # resultControl # 1 # 1 # 0) $ \completedControl ->
  plet (phaseAPreconditionsPost resultPre completedControl) $ \post ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor headControl) (pdata payloadPre))
    $ \headWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor payloadControl) (pdata resultPre))
    $ \payloadWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor resultControl) (pdata post))
    $ \finalizeWitness ->
  pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair _ chunkProof) ->
  plet
    ( pcon $ PNativeScriptTokenWitness
        (pdata chunkProof) (pdata $ pcon PDNothing) (pdata $ pcon PNoSignerSetProof)
    )
    $ \headAuxiliary ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata headWitness) (pdata headAuxiliary))
    $ \headEvidence ->
    pand'List
      [ pverifyPhaseANativeTokenHeadSemanticsV1
          # headPre # headWitness # chunkProof # pcon PDNothing
      , pverifyPhaseANativeTimelockPayloadSemanticsV1
          # payloadPre # payloadWitness # chunkProof # pcon PDNothing
      , pverifyPhaseANativeAdvanceSemanticsV1 # resultPre # finalizeWitness
      , pverifyPhaseANativeScriptsSemanticsV1 # headPre # headEvidence
      , pnot # (pverifyPhaseANativeScriptsOneStepV1 # headPre # headEvidence)
      ]

phaseANativeContainerPayloadTransitions :: forall s. Term s PBool
phaseANativeContainerPayloadTransitions = pand'List
  [ phaseANativeEmptyAllPayloadTransition
  , phaseANativeFramedAllPayloadTransition
  , phaseANativeEmptyAtLeastPayloadTransition
  ]

phaseANativeEmptyAllPayloadTransition :: forall s. Term s PBool
phaseANativeEmptyAllPayloadTransition =
  plet (phexByteStr "820043820180") $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  plet (Bounded.pfromBytes # 6 # 0 # itemCbor) $ \itemCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 1 1 0 0 6 itemCommitment 3 0 (-1)
    )
    $ \(PPair headControl transactionId) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 4 1 0 0 6 itemCommitment 5 1 (-1)
    )
    $ \(PPair payloadControl _) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 2 1 0 0 6 itemCommitment 6 1 1
    )
    $ \(PPair resultControl _) ->
  plet (phaseANativeStateFixture transactionId headControl 18) $ \headPre ->
  plet (phaseANativeStateFixture transactionId payloadControl 19) $ \payloadPre ->
  plet (phaseANativeStateFixture transactionId resultControl 20) $ \resultPre ->
  plet (phaseANativeStateFixture transactionId resultControl 19) $ \fullResultPre ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor headControl) (pdata payloadPre))
    $ \headWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor payloadControl) (pdata resultPre))
    $ \payloadWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor headControl) (pdata fullResultPre))
    $ \fullTokenWitness ->
  pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair _ chunkProof) ->
    pand'List
      [ pverifyPhaseANativeTokenHeadSemanticsV1
          # headPre # headWitness # chunkProof # pcon PDNothing
      , pverifyPhaseANativeAllOrAnyEmptyContainerPayloadSemanticsV1
          # payloadPre # payloadWitness # chunkProof # pcon PDNothing
      , pverifyPhaseANativeEmptyContainerTokenSemanticsV1
          # headPre # fullTokenWitness # chunkProof # pcon PDNothing
      ]

phaseANativeFramedAllPayloadTransition :: forall s. Term s PBool
phaseANativeFramedAllPayloadTransition =
  plet (phexByteStr "82004782018182041864") $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  plet (Bounded.pfromBytes # 6 # 0 # itemCbor) $ \itemCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment 100 (-1) 1 1 0 0 10 itemCommitment 3 0 (-1)
    )
    $ \(PPair headControl transactionId) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment 100 (-1) 4 1 0 0 10 itemCommitment 5 1 (-1)
    )
    $ \(PPair payloadControl _) ->
  plet
    ( pcon $ NativeScriptScan.PNativeScriptTokenV1
        (pdata NativeScriptScan.pallNode) (pdata 6) (pdata 1) (pdata 0)
        (pdata $ pconstant "") (pdata 0)
    )
    $ \token ->
  pmatch (NativeScriptScan.pframeForTokenV1 # token # pconstant "") $ \case
    PNothing -> pconstant False
    PJust frame ->
      plet
        ( pphaseANativeSetExecution # payloadControl # 1 # 6
            # (NativeScriptScan.phashFrameV1 # frame) # 1 # 1 # (-1)
        )
        $ \frameControl ->
      plet (phaseANativeStateFixture transactionId headControl 21) $ \headPre ->
      plet (phaseANativeStateFixture transactionId payloadControl 22) $ \payloadPre ->
      plet (phaseANativeStateFixture transactionId frameControl 23) $ \framePre ->
      plet (phaseANativeStateFixture transactionId frameControl 22) $ \fullFramePre ->
      plet
        (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor headControl) (pdata payloadPre))
        $ \headWitness ->
      plet
        (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor payloadControl) (pdata framePre))
        $ \payloadWitness ->
      plet
        (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor headControl) (pdata fullFramePre))
        $ \fullTokenWitness ->
      pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair _ chunkProof) ->
        pand'List
          [ pverifyPhaseANativeTokenHeadSemanticsV1
              # headPre # headWitness # chunkProof # pcon PDNothing
          , pverifyPhaseANativeAllOrAnyContainerFramePayloadSemanticsV1
              # payloadPre # payloadWitness # chunkProof # pcon PDNothing
          , pverifyPhaseANativeContainerFrameTokenSemanticsV1
              # headPre # fullTokenWitness # chunkProof # pcon PDNothing
          ]

phaseANativeEmptyAtLeastPayloadTransition :: forall s. Term s PBool
phaseANativeEmptyAtLeastPayloadTransition =
  plet (phexByteStr "82004483030080") $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  plet (Bounded.pfromBytes # 6 # 0 # itemCbor) $ \itemCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 1 1 0 0 7 itemCommitment 3 0 (-1)
    )
    $ \(PPair headControl transactionId) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 6 1 0 0 7 itemCommitment 5 1 (-1)
    )
    $ \(PPair payloadControl _) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 2 1 0 0 7 itemCommitment 7 1 1
    )
    $ \(PPair resultControl _) ->
  plet (phaseANativeStateFixture transactionId headControl 24) $ \headPre ->
  plet (phaseANativeStateFixture transactionId payloadControl 25) $ \payloadPre ->
  plet (phaseANativeStateFixture transactionId resultControl 26) $ \resultPre ->
  plet (phaseANativeStateFixture transactionId resultControl 25) $ \fullResultPre ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor headControl) (pdata payloadPre))
    $ \headWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor payloadControl) (pdata resultPre))
    $ \payloadWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor headControl) (pdata fullResultPre))
    $ \fullTokenWitness ->
  pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair _ chunkProof) ->
    pand'List
      [ pverifyPhaseANativeTokenHeadSemanticsV1
          # headPre # headWitness # chunkProof # pcon PDNothing
      , pverifyPhaseANativeAtLeastEmptyContainerPayloadSemanticsV1
          # payloadPre # payloadWitness # chunkProof # pcon PDNothing
      , pverifyPhaseANativeEmptyContainerTokenSemanticsV1
          # headPre # fullTokenWitness # chunkProof # pcon PDNothing
      ]

phaseANativeEmptySignaturePayloadTransition :: forall s. Term s PBool
phaseANativeEmptySignaturePayloadTransition =
  plet
    (phexByteStr "820058208200581c00000000000000000000000000000000000000000000000000000000")
    $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  plet (Bounded.pfromBytes # 6 # 0 # itemCbor) $ \itemCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 1 1 0 0 36 itemCommitment 4 0 (-1)
    )
    $ \(PPair headControl transactionId) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 3 1 0 0 36 itemCommitment 6 1 (-1)
    )
    $ \(PPair payloadControl _) ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 2 1 0 0 36 itemCommitment 36 1 0
    )
    $ \(PPair resultControl _) ->
  plet (phaseANativeStateFixture transactionId headControl 27) $ \headPre ->
  plet (phaseANativeStateFixture transactionId payloadControl 28) $ \payloadPre ->
  plet (phaseANativeStateFixture transactionId resultControl 29) $ \resultPre ->
  plet (phaseANativeStateFixture transactionId resultControl 28) $ \fullResultPre ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor headControl) (pdata payloadPre))
    $ \headWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor payloadControl) (pdata resultPre))
    $ \payloadWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata $ phaseANativeWitnessCbor headControl) (pdata fullResultPre))
    $ \fullTokenWitness ->
  pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair _ chunkProof) ->
    pand'List
      [ pverifyPhaseANativeTokenHeadSemanticsV1
          # headPre # headWitness # chunkProof # pcon PDNothing
      , pverifyPhaseANativeSignaturePayloadSemanticsV1
          # payloadPre # payloadWitness # chunkProof # pcon PDNothing
          # pcon (PEmptySignerSetProof $ pdata pnil)
      , pverifyPhaseANativeSignatureEmptyPayloadSemanticsV1
          # payloadPre # payloadWitness # chunkProof # pcon PDNothing # pnil
      , pverifyPhaseANativeSignatureTokenSemanticsV1
          # headPre # fullTokenWitness # chunkProof # pcon PDNothing
          # pcon (PEmptySignerSetProof $ pdata pnil)
      ]

phaseANativeUnsatisfiedScriptTransition :: forall s. Term s PBool
phaseANativeUnsatisfiedScriptTransition =
  plet
    (phexByteStr "820058208200581c00000000000000000000000000000000000000000000000000000000")
    $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  plet (Bounded.pfromBytes # 6 # 0 # itemCbor) $ \itemCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 2 1 0 0 36 itemCommitment 36 1 0
    )
    $ \(PPair resultControl transactionId) ->
  plet (phaseANativeStateFixture transactionId resultControl 29) $ \pre ->
  plet (inputSetsExactRejection pre $ pconstant "E_NATIVE_SCRIPT_INVALID") $ \post ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        (pdata $ phaseANativeWitnessCbor resultControl) (pdata post)
    )
    $ \witness ->
    pverifyPhaseANativeAdvanceSemanticsV1 # pre # witness

phaseANativeFrameReductionTransition :: forall s. Term s PBool
phaseANativeFrameReductionTransition =
  plet (phexByteStr "82004782018182041864") $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  plet (Bounded.pfromBytes # 6 # 0 # itemCbor) $ \itemCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment 100 (-1) 1 1 0 0 10 itemCommitment 6 1 (-1)
    )
    $ \(PPair baseControl transactionId) ->
  plet
    ( pcon $ NativeScriptScan.PNativeScriptTokenV1
        (pdata NativeScriptScan.pallNode) (pdata 6) (pdata 1) (pdata 0)
        (pdata $ pconstant "") (pdata 0)
    )
    $ \containerToken ->
  pmatch (NativeScriptScan.pframeForTokenV1 # containerToken # pconstant "") $ \case
    PNothing -> pconstant False
    PJust frame ->
      plet
        ( pphaseANativeSetExecution # baseControl # 2 # 10
            # (NativeScriptScan.phashFrameV1 # frame) # 1 # 2 # 1
        )
        $ \childResultControl ->
      plet
        (pphaseANativeSetExecution # childResultControl # 2 # 10 # pconstant "" # 0 # 2 # 1)
        $ \reducedControl ->
      plet (phaseANativeStateFixture transactionId childResultControl 30) $ \pre ->
      plet (phaseANativeStateFixture transactionId reducedControl 31) $ \post ->
      plet
        ( pcon $ PValidationOneStepWitnessV1
            (pdata $ phaseANativeWitnessCbor childResultControl) (pdata post)
        )
        $ \witness ->
        pverifyPhaseANativeFrameSemanticsV1 # pre # witness # frame

phaseAScriptPreconditionsControlFixture :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s (PPair PPhaseAScriptPreconditionsControlV1 PByteString)
phaseAScriptPreconditionsControlFixture
  observerCommitment scriptCommitment scriptIntegrityHash containsNonNative =
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata NativeField.pemptyFieldCommitment)
        (pdata scriptCommitment)
        (pdata NativeField.pemptyFieldCommitment)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet (pcon $ PNativeTxFieldPreimageLengthsV1 0 0 0 0 0 0 0 0 0) $ \fieldLengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # fieldLengths) $ \fieldLengthsCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        1_000_000 (-1) (-1)
        observerCommitment
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        scriptIntegrityHash (cekHash 0xa2) 0
    )
    $ \body ->
  plet (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0) $ \compact ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet validContextCbor $ \contextCbor ->
  plet (Merkle.pfrontierCommitment # 0 # pnil) $ \signerCommitment ->
    pcon $ PPair
      ( pcon $ PPhaseAScriptPreconditionsControlV1
          (pdata compactCbor) (pdata witnessSetCbor) (pdata fieldLengthsCbor)
          (pdata contextCbor) (pdata pemptyResolutionScheduleHash)
          (pdata 0) (pdata signerCommitment) (pdata containsNonNative)
          (pdata 0) (pdata 0) (pdata $ pconstant "")
      )
      transactionId

phaseAScriptPreconditionsStateFixture :: forall s.
  Term s PByteString ->
  Term s PPhaseAScriptPreconditionsControlV1 ->
  Term s PInteger ->
  Term s PValidationMachineStateV1
phaseAScriptPreconditionsStateFixture transactionId control counter = pmatch control $ \c ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pphaseAPreconditions'compactCbor c)
        # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
        # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
    )
    $ \commitment ->
  plet
    ( pencodePhaseAScriptPreconditionsWitness
        # pfromData (pphaseAPreconditions'compactCbor c)
        # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
        # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
        # pfromData (pphaseAPreconditions'contextCbor c)
        # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
        # pfromData (pphaseAPreconditions'signerCount c)
        # pfromData (pphaseAPreconditions'signerFrontierCommitment c)
        # (pfromData (pphaseAPreconditions'containsNonNativeScript c) #== 1)
        # pfromData (pphaseAPreconditions'observerCount c)
        # pfromData (pphaseAPreconditions'observerSeen c)
        # pfromData (pphaseAPreconditions'previousObserver c)
    )
    $ \workCbor ->
    cekBoundStateFixture
      transactionId commitment
      (phashValidationContext # pfromData (pphaseAPreconditions'contextCbor c))
      (pcon PPhaseAScriptPreconditions) counter
      (phashWorkWitness # pcon PPhaseAScriptPreconditions # counter # workCbor)
      0 0

phaseAScriptPreconditionsResolvePost :: forall s.
  Term s PValidationMachineStateV1 ->
  Term s PPhaseAScriptPreconditionsControlV1 ->
  Term s PValidationMachineStateV1
phaseAScriptPreconditionsResolvePost pre control =
  pmatch pre $ \preState ->
  pmatch control $ \c ->
  plet
    ( pencodeResolveInputsWitness
        # pfromData (pphaseAPreconditions'compactCbor c)
        # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
        # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
        # pfromData (pphaseAPreconditions'contextCbor c)
        # 0 # pinitialResolutionAccumulator
        # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
        # pfromData (pphaseAPreconditions'signerCount c)
        # pfromData (pphaseAPreconditions'signerFrontierCommitment c)
        # pcon PDNothing
        # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
    )
    $ \workCbor ->
    pcon $ PValidationMachineStateV1
      (pmachineState'machineVersion preState) (pmachineState'eventKeyHash preState)
      (pmachineState'transactionId preState) (pmachineState'transactionCommitment preState)
      (pmachineState'validationContextHash preState) (pmachineState'sourceKind preState)
      (pmachineState'priorLedgerRoot preState) (pdata $ pcon PResolveInputs)
      (pdata $ pfromData (pmachineState'programCounter preState) + 1)
      ( pdata $ phashWorkWitness # pcon PResolveInputs
          # (pfromData (pmachineState'programCounter preState) + 1) # workCbor
      )
      (pmachineState'executionCpu preState) (pmachineState'executionMemory preState)
      (pmachineState'verdict preState) (pmachineState'rejectionCodeHash preState)
      (pmachineState'ledgerDeltaRoot preState)

phaseAScriptPreconditionsFinalizeTransitions :: forall s. Term s PBool
phaseAScriptPreconditionsFinalizeTransitions =
  plet (pconstant $ BS.replicate 32 0) $ \zeroHash ->
  pmatch
    ( phaseAScriptPreconditionsControlFixture
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment zeroHash 0
    )
    $ \(PPair emptyControl emptyTransactionId) ->
  plet (phaseAScriptPreconditionsStateFixture emptyTransactionId emptyControl 32) $ \emptyPre ->
  plet (phaseAScriptPreconditionsResolvePost emptyPre emptyControl) $ \emptyPost ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        (pdata $ pmatch emptyControl $ \c ->
          pencodePhaseAScriptPreconditionsWitness
            # pfromData (pphaseAPreconditions'compactCbor c)
            # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
            # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
            # pfromData (pphaseAPreconditions'contextCbor c)
            # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
            # 0 # pfromData (pphaseAPreconditions'signerFrontierCommitment c)
            # pconstant False # 0 # 0 # pconstant ""
        )
        (pdata emptyPost)
    )
    $ \emptyWitness ->
  pmatch emptyControl $ \emptyFields ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # emptyTransactionId
        # pfromData (pphaseAPreconditions'compactCbor emptyFields)
        # pfromData (pphaseAPreconditions'witnessSetCompactCbor emptyFields)
        # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor emptyFields)
    )
    $ \(PPair emptyVerified _) ->
  pmatch
    ( phaseAScriptPreconditionsControlFixture
        NativeField.pemptyFieldCommitment (cekHash 0xb1) zeroHash 1
    )
    $ \(PPair invalidControl invalidTransactionId) ->
  plet (phaseAScriptPreconditionsStateFixture invalidTransactionId invalidControl 34) $ \invalidPre ->
  plet (inputSetsExactRejection invalidPre $ pconstant "E_INVALID_FIELD_TYPE") $ \invalidPost ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        (pdata $ pmatch invalidControl $ \c ->
          pencodePhaseAScriptPreconditionsWitness
            # pfromData (pphaseAPreconditions'compactCbor c)
            # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
            # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
            # pfromData (pphaseAPreconditions'contextCbor c)
            # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
            # 0 # pfromData (pphaseAPreconditions'signerFrontierCommitment c)
            # pconstant True # 0 # 0 # pconstant ""
        )
        (pdata invalidPost)
    )
    $ \invalidWitness ->
  pmatch invalidControl $ \invalidFields ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # invalidTransactionId
        # pfromData (pphaseAPreconditions'compactCbor invalidFields)
        # pfromData (pphaseAPreconditions'witnessSetCompactCbor invalidFields)
        # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor invalidFields)
    )
    $ \(PPair invalidVerified _) ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata emptyWitness) (pdata $ pcon PNoAuxiliaryWitness))
    $ \emptyEvidence ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata invalidWitness) (pdata $ pcon PNoAuxiliaryWitness))
    $ \invalidEvidence ->
    pand'List
      [ pphaseAScriptPreconditionsControlIsBound
          # emptyPre # emptyWitness # emptyControl # NativeField.pemptyFieldCommitment
      , pphaseAScriptPreconditionsFinalize
          # emptyPre # emptyWitness # emptyControl # emptyVerified # pconstant False # 0
      , pphaseAScriptPreconditionsControlIsBound
          # invalidPre # invalidWitness # invalidControl # NativeField.pemptyFieldCommitment
      , pphaseAScriptPreconditionsFinalize
          # invalidPre # invalidWitness # invalidControl # invalidVerified # pconstant False # 0
      , pverifyPhaseAScriptPreconditionsOneStepV1 # emptyPre # emptyEvidence
      , pverifyPhaseAScriptPreconditionsSemanticsV1 # invalidPre # invalidEvidence
      ]

phaseAScriptPreconditionsObserverTransition :: forall s. Term s PBool
phaseAScriptPreconditionsObserverTransition =
  plet (pconstant $ BS.replicate 28 0x11) $ \observerHash ->
  plet (pconstant $ BS.replicate 32 0) $ \zeroHash ->
  plet (inputSetsSingletonCommitment 3 observerHash) $ \observerCommitment ->
  pmatch
    ( phaseAScriptPreconditionsControlFixture
        observerCommitment NativeField.pemptyFieldCommitment zeroHash 0
    )
    $ \(PPair initialControl transactionId) ->
  plet
    (pphaseAScriptPreconditionsWithObserver # initialControl # 1 # 1 # observerHash)
    $ \completeControl ->
  plet (phaseAScriptPreconditionsStateFixture transactionId initialControl 36) $ \initialPre ->
  plet (phaseAScriptPreconditionsStateFixture transactionId completeControl 37) $ \completePre ->
  plet (phaseAScriptPreconditionsResolvePost completePre completeControl) $ \resolvePost ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        ( pdata $ pmatch initialControl $ \c ->
            pencodePhaseAScriptPreconditionsWitness
              # pfromData (pphaseAPreconditions'compactCbor c)
              # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
              # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
              # pfromData (pphaseAPreconditions'contextCbor c)
              # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
              # 0 # pfromData (pphaseAPreconditions'signerFrontierCommitment c)
              # pconstant False # 0 # 0 # pconstant ""
        )
        (pdata completePre)
    )
    $ \itemWitness ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        ( pdata $ pmatch completeControl $ \c ->
            pencodePhaseAScriptPreconditionsWitness
              # pfromData (pphaseAPreconditions'compactCbor c)
              # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
              # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
              # pfromData (pphaseAPreconditions'contextCbor c)
              # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
              # 0 # pfromData (pphaseAPreconditions'signerFrontierCommitment c)
              # pconstant False # 1 # 1 # observerHash
        )
        (pdata resolvePost)
    )
    $ \finalizeWitness ->
  pmatch (inputSetsSingletonProof 3 observerHash) $ \(PPair collectionProof chunkProof) ->
  plet
    ( pcon $ PTransactionFieldChunkWitness
        (pdata collectionProof) (pdata chunkProof)
    )
    $ \itemAuxiliary ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata itemWitness) (pdata itemAuxiliary))
    $ \itemEvidence ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata finalizeWitness) (pdata $ pcon PNoAuxiliaryWitness)
    )
    $ \finalizeEvidence ->
    pand'List
      [ pverifyPhaseAScriptPreconditionsOneStepV1 # initialPre # itemEvidence
      , pverifyPhaseAScriptPreconditionsSemanticsV1 # completePre # finalizeEvidence
      ]

phaseAScriptPreconditionsDuplicateObserverTransition :: forall s. Term s PBool
phaseAScriptPreconditionsDuplicateObserverTransition =
  plet (pconstant $ BS.replicate 28 0xaa) $ \observerHash ->
  plet (phaseAScriptPreconditionsDuplicateObserverCommitment observerHash) $ \observerCommitment ->
  plet (pconstant $ BS.replicate 32 0) $ \zeroHash ->
  pmatch
    ( phaseAScriptPreconditionsControlFixture
        observerCommitment NativeField.pemptyFieldCommitment zeroHash 0
    )
    $ \(PPair initialControl transactionId) ->
  plet
    (pphaseAScriptPreconditionsWithObserver # initialControl # 2 # 1 # observerHash)
    $ \secondControl ->
  plet (phaseAScriptPreconditionsStateFixture transactionId initialControl 38) $ \initialPre ->
  plet (phaseAScriptPreconditionsStateFixture transactionId secondControl 39) $ \secondPre ->
  plet (inputSetsExactRejection secondPre $ pconstant "E_INVALID_FIELD_TYPE") $ \rejection ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        (pdata $ phaseAScriptPreconditionsWitnessCbor initialControl) (pdata secondPre)
    )
    $ \firstWitness ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        (pdata $ phaseAScriptPreconditionsWitnessCbor secondControl) (pdata rejection)
    )
    $ \secondWitness ->
  pmatch
    (phaseAScriptPreconditionsDuplicateObserverProof 0 observerHash)
    $ \(PPair firstCollectionProof firstChunkProof) ->
  pmatch
    (phaseAScriptPreconditionsDuplicateObserverProof 1 observerHash)
    $ \(PPair secondCollectionProof secondChunkProof) ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata firstWitness)
        ( pdata $ pcon $ PTransactionFieldChunkWitness
            (pdata firstCollectionProof) (pdata firstChunkProof)
        )
    )
    $ \firstEvidence ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata secondWitness)
        ( pdata $ pcon $ PTransactionFieldChunkWitness
            (pdata secondCollectionProof) (pdata secondChunkProof)
        )
    )
    $ \secondEvidence ->
    pverifyPhaseAScriptPreconditionsOneStepV1 # initialPre # firstEvidence
      #&& pverifyPhaseAScriptPreconditionsOneStepV1 # secondPre # secondEvidence

phaseAScriptPreconditionsDuplicateObserverCommitment :: forall s.
  Term s PByteString -> Term s PByteString
phaseAScriptPreconditionsDuplicateObserverCommitment observerHash =
  plet (Bounded.pfromBytes # 3 # 0 # observerHash) $ \firstItemCommitment ->
  plet (Bounded.pfromBytes # 3 # 1 # observerHash) $ \secondItemCommitment ->
  plet
    (BoundedCollection.phashBoundedCollectionItem
      # 3 # 0 # 28 # firstItemCommitment)
    $ \firstLeaf ->
  plet
    (BoundedCollection.phashBoundedCollectionItem
      # 3 # 1 # 28 # secondItemCommitment)
    $ \secondLeaf ->
  pmatch
    (Merkle.pbuildFrontier #$ pcons # pdata firstLeaf #$ pcons # pdata secondLeaf # pnil)
    $ \frontier ->
    BoundedCollection.pboundedCollectionCommitment
      # 3 # 2 # Merkle.pbuiltFrontier'peaks frontier

phaseAScriptPreconditionsDuplicateObserverProof :: forall s.
  Integer ->
  Term s PByteString ->
  Term s (PPair BoundedCollection.PItemProofV1 Bounded.PChunkProofV1)
phaseAScriptPreconditionsDuplicateObserverProof itemIndex observerHash =
  plet (Bounded.pfromBytes # 3 # 0 # observerHash) $ \firstItemCommitment ->
  plet (Bounded.pfromBytes # 3 # 1 # observerHash) $ \secondItemCommitment ->
  plet
    (BoundedCollection.phashBoundedCollectionItem
      # 3 # 0 # 28 # firstItemCommitment)
    $ \firstLeaf ->
  plet
    (BoundedCollection.phashBoundedCollectionItem
      # 3 # 1 # 28 # secondItemCommitment)
    $ \secondLeaf ->
  pmatch
    (Merkle.pbuildFrontier #$ pcons # pdata firstLeaf #$ pcons # pdata secondLeaf # pnil)
    $ \collectionFrontier ->
  plet (pconstant itemIndex) $ \itemIndex' ->
  plet (if itemIndex == 0 then firstItemCommitment else secondItemCommitment) $ \itemCommitment ->
  plet (if itemIndex == 0 then secondLeaf else firstLeaf) $ \collectionSibling ->
  plet (Bounded.phashChunk # 3 # itemIndex' # 0 # observerHash) $ \chunkLeaf ->
    pcon $ PPair
      ( pcon $ BoundedCollection.PItemProofV1
          (pdata BoundedCollection.pboundedCollectionVersion)
          (pdata 3)
          (pdata 2)
          (pdata itemIndex')
          (pdata 28)
          (pdata itemCommitment)
          (pdata $ Merkle.pbuiltFrontier'peaks collectionFrontier)
          (pdata $ pcons # pdata collectionSibling # pnil)
      )
      ( pcon $ Bounded.PChunkProofV1
          (pdata Bounded.pversion)
          (pdata 3)
          (pdata itemIndex')
          (pdata 28)
          (pdata 0)
          (pdata observerHash)
          (pdata $ cekSinglePeak chunkLeaf)
          (pdata pnil)
      )

phaseAScriptPreconditionsWitnessCbor :: forall s.
  Term s PPhaseAScriptPreconditionsControlV1 -> Term s PByteString
phaseAScriptPreconditionsWitnessCbor control = pmatch control $ \c ->
  pencodePhaseAScriptPreconditionsWitness
    # pfromData (pphaseAPreconditions'compactCbor c)
    # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
    # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
    # pfromData (pphaseAPreconditions'contextCbor c)
    # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
    # pfromData (pphaseAPreconditions'signerCount c)
    # pfromData (pphaseAPreconditions'signerFrontierCommitment c)
    # (pfromData (pphaseAPreconditions'containsNonNativeScript c) #== 1)
    # pfromData (pphaseAPreconditions'observerCount c)
    # pfromData (pphaseAPreconditions'observerSeen c)
    # pfromData (pphaseAPreconditions'previousObserver c)

resolveInputsInitialControlFixture :: forall s.
  Term s PInteger -> Term s (PPair PResolveInputsControlV1 PByteString)
resolveInputsInitialControlFixture validityStart =
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet (pcon $ PNativeTxFieldPreimageLengthsV1 0 0 0 0 0 0 0 0 0) $ \fieldLengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # fieldLengths) $ \fieldLengthsCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        1_000_000 validityStart (-1)
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment
        (pconstant $ BS.replicate 32 0) (cekHash 0xa2) 0
    )
    $ \body ->
  plet (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0) $ \compact ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet (Merkle.pfrontierCommitment # 0 # pnil) $ \signerCommitment ->
    pcon $ PPair
      ( pcon $ PResolveInputsControlV1
          (pdata compactCbor) (pdata witnessSetCbor) (pdata fieldLengthsCbor)
          (pdata validContextCbor) (pdata 0) (pdata pinitialResolutionAccumulator)
          (pdata pemptyResolutionScheduleHash) (pdata 0) (pdata signerCommitment)
          (pdata $ pcon PDNothing) (pdata pemptyResolutionScheduleHash)
      )
      transactionId

resolveInputsStateFixture :: forall s.
  Term s PByteString -> Term s PResolveInputsControlV1 -> Term s PInteger ->
  Term s PValidationMachineStateV1
resolveInputsStateFixture transactionId control counter = pmatch control $ \c ->
  plet
    ( pencodeResolveInputsWitness
        # pfromData (presolveInputs'compactCbor c)
        # pfromData (presolveInputs'witnessSetCompactCbor c)
        # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
        # pfromData (presolveInputs'contextCbor c)
        # pfromData (presolveInputs'cursor c)
        # pfromData (presolveInputs'accumulator c)
        # pfromData (presolveInputs'remainingScheduleHash c)
        # pfromData (presolveInputs'signerCount c)
        # pfromData (presolveInputs'signerFrontierCommitment c)
        # pfromData (presolveInputs'pending c)
        # pfromData (presolveInputs'resolutionScheduleHash c)
    )
    $ \workCbor ->
    cekBoundStateFixture
      transactionId
      ( NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (presolveInputs'compactCbor c)
          # pfromData (presolveInputs'witnessSetCompactCbor c)
          # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
      )
      (phashValidationContext # pfromData (presolveInputs'contextCbor c))
      (pcon PResolveInputs) counter
      (phashWorkWitness # pcon PResolveInputs # counter # workCbor)
      0 0

resolveInputsAdvanceControl :: forall s.
  Term s PResolveInputsControlV1 -> Term s PResolveInputsControlV1
resolveInputsAdvanceControl control = pmatch control $ \c ->
  pcon $ PResolveInputsControlV1
    (presolveInputs'compactCbor c)
    (presolveInputs'witnessSetCompactCbor c)
    (presolveInputs'fieldPreimageLengthsCbor c)
    (presolveInputs'contextCbor c)
    (pdata 1)
    (presolveInputs'accumulator c)
    (presolveInputs'remainingScheduleHash c)
    (presolveInputs'signerCount c)
    (presolveInputs'signerFrontierCommitment c)
    (pdata $ pcon PDNothing)
    (presolveInputs'resolutionScheduleHash c)

resolveInputsInitialTransitions :: forall s. Term s PBool
resolveInputsInitialTransitions =
  pmatch (resolveInputsInitialControlFixture (-1)) $ \(PPair validControl validTxId) ->
  plet (resolveInputsStateFixture validTxId validControl 40) $ \validPre ->
  plet (resolveInputsStateFixture validTxId (resolveInputsAdvanceControl validControl) 41) $ \validPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ pmatch validControl $ \c ->
          pencodeResolveInputsWitness
            # pfromData (presolveInputs'compactCbor c)
            # pfromData (presolveInputs'witnessSetCompactCbor c)
            # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
            # pfromData (presolveInputs'contextCbor c)
            # 0 # pfromData (presolveInputs'accumulator c)
            # pfromData (presolveInputs'remainingScheduleHash c)
            # pfromData (presolveInputs'signerCount c)
            # pfromData (presolveInputs'signerFrontierCommitment c)
            # pcon PDNothing
            # pfromData (presolveInputs'resolutionScheduleHash c))
          (pdata validPost)) $ \validWitness ->
  pmatch (resolveInputsInitialControlFixture 8) $ \(PPair invalidControl invalidTxId) ->
  plet (resolveInputsStateFixture invalidTxId invalidControl 42) $ \invalidPre ->
  plet (inputSetsExactRejection invalidPre $ pconstant "E_VALIDITY_INTERVAL_MISMATCH") $ \invalidPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ pmatch invalidControl $ \c ->
          pencodeResolveInputsWitness
            # pfromData (presolveInputs'compactCbor c)
            # pfromData (presolveInputs'witnessSetCompactCbor c)
            # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
            # pfromData (presolveInputs'contextCbor c)
            # 0 # pfromData (presolveInputs'accumulator c)
            # pfromData (presolveInputs'remainingScheduleHash c)
            # pfromData (presolveInputs'signerCount c)
            # pfromData (presolveInputs'signerFrontierCommitment c)
            # pcon PDNothing
            # pfromData (presolveInputs'resolutionScheduleHash c))
          (pdata invalidPost)) $ \invalidWitness ->
    pand'List
      [ pstructuralTransitionIsValid # validPre # validWitness
      , pverifyResolveInputsInitialSemanticsV1 # validPre # validWitness
      , pstructuralTransitionIsValid # invalidPre # invalidWitness
      , pverifyResolveInputsInitialSemanticsV1 # invalidPre # invalidWitness
      ]

resolveInputsScriptSourcesCbor :: forall s.
  Term s PResolveInputsControlV1 -> Term s PByteString
resolveInputsScriptSourcesCbor control = resolveInputsScriptSourcesCborAtStage control 0

resolveInputsScriptSourcesCborAtStage :: forall s.
  Term s PResolveInputsControlV1 -> Term s PInteger -> Term s PByteString
resolveInputsScriptSourcesCborAtStage control stage = pmatch control $ \c ->
  pencodeScriptSourcesWitness
    # pfromData (presolveInputs'compactCbor c)
    # pfromData (presolveInputs'witnessSetCompactCbor c)
    # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
    # pfromData (presolveInputs'contextCbor c)
    # (pfromData (presolveInputs'cursor c) - 1)
    # pfromData (presolveInputs'accumulator c)
    # pfromData (presolveInputs'signerCount c)
    # pfromData (presolveInputs'signerFrontierCommitment c)
    # pnil
    # stage # 0 # pnil # 0 # pnil
    # 0 # pinitialResolutionAccumulator # pemptyResolutionScheduleHash
    # 0 # 0 # pnil # 0 # 0 # pnil # 0
    # pemptyReceivePurposeScanControl
    # 0 # 0 # pemptyObserverPurposeScanControl
    # pemptyMintFoldControl
    # pfromData (presolveInputs'resolutionScheduleHash c)

resolveInputsFinishPost :: forall s.
  Term s PValidationMachineStateV1 -> Term s PResolveInputsControlV1 ->
  Term s PValidationMachineStateV1
resolveInputsFinishPost pre control = pmatch pre $ \preState ->
  plet (resolveInputsScriptSourcesCbor control) $ \workCbor ->
  pcon $ PValidationMachineStateV1
    (pmachineState'machineVersion preState)
    (pmachineState'eventKeyHash preState)
    (pmachineState'transactionId preState)
    (pmachineState'transactionCommitment preState)
    (pmachineState'validationContextHash preState)
    (pmachineState'sourceKind preState)
    (pmachineState'priorLedgerRoot preState)
    (pdata $ pcon PScriptSources)
    (pdata $ pfromData (pmachineState'programCounter preState) + 1)
    ( pdata $ phashWorkWitness # pcon PScriptSources
        # (pfromData (pmachineState'programCounter preState) + 1) # workCbor
    )
    (pmachineState'executionCpu preState)
    (pmachineState'executionMemory preState)
    (pmachineState'verdict preState)
    (pmachineState'rejectionCodeHash preState)
    (pmachineState'ledgerDeltaRoot preState)

resolveInputsFinishTransition :: forall s. Term s PBool
resolveInputsFinishTransition =
  pmatch (resolveInputsInitialControlFixture (-1)) $ \(PPair initialControl transactionId) ->
  plet (resolveInputsAdvanceControl initialControl) $ \control ->
  plet (resolveInputsStateFixture transactionId control 44) $ \pre ->
  plet (resolveInputsFinishPost pre control) $ \post ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        ( pdata $ pmatch control $ \c ->
            pencodeResolveInputsWitness
              # pfromData (presolveInputs'compactCbor c)
              # pfromData (presolveInputs'witnessSetCompactCbor c)
              # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
              # pfromData (presolveInputs'contextCbor c)
              # 1 # pfromData (presolveInputs'accumulator c)
              # pfromData (presolveInputs'remainingScheduleHash c)
              # pfromData (presolveInputs'signerCount c)
              # pfromData (presolveInputs'signerFrontierCommitment c)
              # pcon PDNothing
              # pfromData (presolveInputs'resolutionScheduleHash c)
        )
        (pdata post)
    )
    $ \witness ->
    pand'List
      [ pencodeMintFoldControl # pemptyMintFoldControl
          #== phexByteStr "8c200040400040000000400080"
      , psliceBS # 0 # 2 # (resolveInputsScriptSourcesCbor control)
          #== phexByteStr "981e"
      , pstructuralTransitionIsValid # pre # witness
      , pverifyResolveInputsFinishSemanticsV1 # pre # witness
      ]

scriptSourcesStateForCbor :: forall s.
  Term s PValidationMachineStateV1 -> Term s PByteString ->
  Term s PValidationMachineStateV1
scriptSourcesStateForCbor pre workCbor = pmatch pre $ \preState ->
  plet (pfromData (pmachineState'programCounter preState) + 1) $ \nextCounter ->
    pcon $ PValidationMachineStateV1
      (pmachineState'machineVersion preState)
      (pmachineState'eventKeyHash preState)
      (pmachineState'transactionId preState)
      (pmachineState'transactionCommitment preState)
      (pmachineState'validationContextHash preState)
      (pmachineState'sourceKind preState)
      (pmachineState'priorLedgerRoot preState)
      (pdata $ pcon PScriptSources)
      (pdata nextCounter)
      (pdata $ phashWorkWitness # pcon PScriptSources # nextCounter # workCbor)
      (pmachineState'executionCpu preState)
      (pmachineState'executionMemory preState)
      (pmachineState'verdict preState)
      (pmachineState'rejectionCodeHash preState)
      (pmachineState'ledgerDeltaRoot preState)

scriptSourcesStageZeroEmptyFinishTransition :: forall s. Term s PBool
scriptSourcesStageZeroEmptyFinishTransition =
  pmatch (resolveInputsInitialControlFixture (-1)) $ \(PPair initialControl transactionId) ->
  plet (resolveInputsAdvanceControl initialControl) $ \control ->
  plet (resolveInputsStateFixture transactionId control 54) $ \resolvePre ->
  plet (resolveInputsFinishPost resolvePre control) $ \pre ->
  plet (resolveInputsScriptSourcesCbor control) $ \stageZeroCbor ->
  plet (resolveInputsScriptSourcesCborAtStage control 1) $ \stageOneCbor ->
  plet (resolveInputsScriptSourcesCborAtStage control 2) $ \wrongStageCbor ->
  plet (scriptSourcesStateForCbor pre stageOneCbor) $ \post ->
  plet (scriptSourcesStateForCbor pre wrongStageCbor) $ \wrongPost ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata stageZeroCbor) (pdata post))
    $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata stageZeroCbor) (pdata wrongPost))
    $ \wrongWitness ->
  plet
    ( pcon $ PInlineSourceHashControlV1
        (pdata 1) (pdata 0) (pdata 1) (pdata 3)
        (pdata 1) (pdata 0) (pdata 1)
        (pdata $ cekHash 0x7a)
        (pdata $ Blake2b224.pinitialControlV1 # 1)
    )
    $ \inlineControl ->
  plet (pencodeInlineSourceHashControlV1 # inlineControl) $ \inlineCbor ->
    pand'List
      [ psliceBS # 0 # 1 # inlineCbor #== phexByteStr "89"
      , pdecodeInlineSourceHashControlV1 # inlineCbor #== inlineControl
      , pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageZeroFinishSemanticsV1 # pre # witness
      , pverifyScriptSourcesStageZeroSemanticsV1
          # pre # witness # pcon PNoAuxiliaryWitness
      , pnot # (pverifyScriptSourcesStageZeroFinishSemanticsV1 # pre # wrongWitness)
      , pnot
          # ( pverifyScriptSourcesStageZeroSemanticsV1
                # pre # wrongWitness # pcon PNoAuxiliaryWitness
            )
      ]

scriptSourcesStageZeroCborFromPhase :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger -> Term s PByteString
scriptSourcesStageZeroCborFromPhase control sourceTotalCount =
  scriptSourcesStageZeroCborFromPhaseWithFrontier control 0 pnil sourceTotalCount

scriptSourcesStageZeroCborFromPhaseWithFrontier :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) -> Term s PInteger ->
  Term s PByteString
scriptSourcesStageZeroCborFromPhaseWithFrontier
  control sourceCount sourcePeaks sourceTotalCount =
  scriptSourcesCborFromPhaseAtStage control 0 sourceCount sourcePeaks sourceTotalCount

scriptSourcesCborFromPhaseAtStage :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) -> Term s PInteger ->
  Term s PByteString
scriptSourcesCborFromPhaseAtStage
  control stage sourceCount sourcePeaks sourceTotalCount =
  scriptSourcesCborFromPhaseAtStageWithRedeemers
    control stage sourceCount sourcePeaks sourceTotalCount 0 pnil 0

scriptSourcesCborFromPhaseAtStageWithRedeemers :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) -> Term s PInteger ->
  Term s PInteger -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PInteger -> Term s PByteString
scriptSourcesCborFromPhaseAtStageWithRedeemers
  control stage sourceCount sourcePeaks sourceTotalCount
  redeemerCount redeemerPeaks redeemerTotalCount = pmatch control $ \c ->
  pencodeScriptSourcesWitness
    # pfromData (pphaseANative'compactCbor c)
    # pfromData (pphaseANative'witnessSetCompactCbor c)
    # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    # pfromData (pphaseANative'contextCbor c)
    # 0 # pinitialResolutionAccumulator
    # pfromData (pphaseANative'signerCount c)
    # ( Merkle.pfrontierCommitment
          # pfromData (pphaseANative'signerCount c)
          # pfromData (pphaseANative'signerPeaks c)
      )
    # pnil # stage # sourceCount # sourcePeaks # redeemerCount # redeemerPeaks
    # 0 # pinitialResolutionAccumulator # pemptyResolutionScheduleHash
    # 0 # 0 # pnil # 0 # 0 # pnil # 0
    # pemptyReceivePurposeScanControl
    # sourceTotalCount # redeemerTotalCount # pemptyObserverPurposeScanControl
    # pemptyMintFoldControl
    # pfromData (pphaseANative'resolutionScheduleHash c)

scriptSourcesReplayInitializationCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger ->
  Term s PByteString -> Term s PByteString -> Term s PByteString
scriptSourcesReplayInitializationCbor
  control stage replayRemainingScheduleHash resolutionScheduleHash = pmatch control $ \c ->
  pencodeScriptSourcesWitness
    # pfromData (pphaseANative'compactCbor c)
    # pfromData (pphaseANative'witnessSetCompactCbor c)
    # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    # pfromData (pphaseANative'contextCbor c)
    # 1 # pinitialResolutionAccumulator
    # pfromData (pphaseANative'signerCount c)
    # ( Merkle.pfrontierCommitment
          # pfromData (pphaseANative'signerCount c)
          # pfromData (pphaseANative'signerPeaks c)
      )
    # pnil # stage # 0 # pnil # 0 # pnil
    # 0 # pinitialResolutionAccumulator # replayRemainingScheduleHash
    # 0 # 0 # pnil # 0 # 0 # pnil # 0
    # pemptyReceivePurposeScanControl
    # 0 # 0 # pemptyObserverPurposeScanControl
    # pemptyMintFoldControl # resolutionScheduleHash

scriptSourcesReplayFinishCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger ->
  Term s PByteString -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PByteString -> Term s PByteString
scriptSourcesReplayFinishCbor
  control stage resolvedAccumulator resolvedItemPeaks resolutionScheduleHash =
  pmatch control $ \c ->
  pencodeScriptSourcesWitness
    # pfromData (pphaseANative'compactCbor c)
    # pfromData (pphaseANative'witnessSetCompactCbor c)
    # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    # pfromData (pphaseANative'contextCbor c)
    # 1 # resolvedAccumulator
    # pfromData (pphaseANative'signerCount c)
    # ( Merkle.pfrontierCommitment
          # pfromData (pphaseANative'signerCount c)
          # pfromData (pphaseANative'signerPeaks c)
      )
    # resolvedItemPeaks # stage # 0 # pnil # 0 # pnil
    # 1 # resolvedAccumulator # pemptyResolutionScheduleHash
    # 0 # 0 # pnil # 0 # 0 # pnil # 0
    # pemptyReceivePurposeScanControl
    # 0 # 0 # pemptyObserverPurposeScanControl
    # pemptyMintFoldControl # resolutionScheduleHash

scriptSourcesReplayCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger ->
  Term s PByteString -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PInteger -> Term s PByteString -> Term s PByteString ->
  Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) -> Term s PInteger ->
  Term s PByteString -> Term s PByteString
scriptSourcesReplayCbor
  control stage resolvedAccumulator resolvedItemPeaks replayCursor replayAccumulator
  replayRemainingScheduleHash spendIndex sourceCount sourcePeaks purposeCount
  purposePeaks sourceTotalCount resolutionScheduleHash = pmatch control $ \c ->
  pencodeScriptSourcesWitness
    # pfromData (pphaseANative'compactCbor c)
    # pfromData (pphaseANative'witnessSetCompactCbor c)
    # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    # pfromData (pphaseANative'contextCbor c)
    # 1 # resolvedAccumulator
    # pfromData (pphaseANative'signerCount c)
    # ( Merkle.pfrontierCommitment
          # pfromData (pphaseANative'signerCount c)
          # pfromData (pphaseANative'signerPeaks c)
      )
    # resolvedItemPeaks # stage # sourceCount # sourcePeaks # 0 # pnil
    # replayCursor # replayAccumulator # replayRemainingScheduleHash
    # spendIndex # purposeCount # purposePeaks # 0 # 0 # pnil # 0
    # pemptyReceivePurposeScanControl
    # sourceTotalCount # 0 # pemptyObserverPurposeScanControl
    # pemptyMintFoldControl # resolutionScheduleHash

scriptSourcesReplayDescriptor :: forall s.
  Term s PByteString -> Term s PInteger -> Term s PByteString ->
  Term s PInteger -> Term s PByteString -> Term s PByteString
scriptSourcesReplayDescriptor
  address referenceScriptLanguage referenceScriptHash referenceScriptTotalLength
  referenceScriptItemCommitment =
  plet
    (phexByteStr "a200581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a142223307")
    $ \outputCbor ->
  pmatch (ScriptContext.ptxOutSummaryV1 # outputCbor # pconstant False) $ \case
    PNothing -> perror
    PJust cardano ->
      pmatch (ScriptContext.ptxOutSummaryV1 # outputCbor # pconstant True) $ \case
        PNothing -> perror
        PJust midgard ->
          pmatch (ScriptContext.pspendDatumSummaryV1 # outputCbor) $ \case
            PNothing -> perror
            PJust spend ->
              OutputCommitment.pencodeLedgerOutputCommitment
                # ( pcon $ OutputCommitment.PLedgerOutputCommitmentV1
                      (pdata OutputCommitment.pledgerOutputCommitmentVersion)
                      (pdata 2)
                      (pdata $ plengthBS # outputCbor)
                      (pdata $ Bounded.pfromBytes # 2 # 2 # outputCbor)
                      (pdata address)
                      (pdata 1_234_567)
                      (pdata 0)
                      (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
                      (pdata 0)
                      (pdata referenceScriptLanguage)
                      (pdata referenceScriptHash)
                      (pdata referenceScriptTotalLength)
                      (pdata referenceScriptItemCommitment)
                      (pdata cardano)
                      (pdata midgard)
                      (pdata spend)
                  )

scriptSourcesStageFourCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) -> Term s PInteger ->
  Term s PByteString
scriptSourcesStageFourCbor control stage outputCount outputPeaks outputTotalCount =
  pmatch control $ \c ->
  pencodeScriptSourcesWitness
    # pfromData (pphaseANative'compactCbor c)
    # pfromData (pphaseANative'witnessSetCompactCbor c)
    # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    # pfromData (pphaseANative'contextCbor c)
    # 0 # pinitialResolutionAccumulator
    # pfromData (pphaseANative'signerCount c)
    # ( Merkle.pfrontierCommitment
          # pfromData (pphaseANative'signerCount c)
          # pfromData (pphaseANative'signerPeaks c)
      )
    # pnil # stage # 0 # pnil # 0 # pnil
    # 0 # pinitialResolutionAccumulator # pemptyResolutionScheduleHash
    # 0 # 0 # pnil # 0 # outputCount # outputPeaks # outputTotalCount
    # pemptyReceivePurposeScanControl
    # 0 # 0 # pemptyObserverPurposeScanControl
    # pemptyMintFoldControl # pemptyResolutionScheduleHash

scriptSourcesStageFiveCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) -> Term s PInteger ->
  Term s PReceivePurposeScanControlV1 -> Term s PByteString
scriptSourcesStageFiveCbor
  control outputCursor outputCount outputPeaks outputTotalCount receiveScan =
  pmatch control $ \c ->
  pencodeScriptSourcesWitness
    # pfromData (pphaseANative'compactCbor c)
    # pfromData (pphaseANative'witnessSetCompactCbor c)
    # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    # pfromData (pphaseANative'contextCbor c)
    # 0 # pinitialResolutionAccumulator
    # pfromData (pphaseANative'signerCount c)
    # ( Merkle.pfrontierCommitment
          # pfromData (pphaseANative'signerCount c)
          # pfromData (pphaseANative'signerPeaks c)
      )
    # pnil # 5 # 0 # pnil # 0 # pnil
    # 0 # pinitialResolutionAccumulator # pemptyResolutionScheduleHash
    # 0 # 0 # pnil # outputCursor # outputCount # outputPeaks # outputTotalCount
    # receiveScan
    # 0 # 0 # pemptyObserverPurposeScanControl
    # pemptyMintFoldControl # pemptyResolutionScheduleHash

scriptSourcesStateFromPhase :: forall s.
  Term s PByteString -> Term s PPhaseANativeScriptsControlV1 ->
  Term s PInteger -> Term s PByteString -> Term s PValidationMachineStateV1
scriptSourcesStateFromPhase transactionId control counter workCbor = pmatch control $ \c ->
  cekBoundStateFixture
    transactionId
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    (phashValidationContext # pfromData (pphaseANative'contextCbor c))
    (pcon PScriptSources) counter
    (phashWorkWitness # pcon PScriptSources # counter # workCbor)
    0 0

scriptSourcesStageOneRawFinishTransition :: forall s. Term s PBool
scriptSourcesStageOneRawFinishTransition =
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 1 0 pnil 0) $ \workCbor ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 2 0 pnil 0) $ \nextWorkCbor ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 3 0 pnil 0) $ \wrongWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 64 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 65 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 65 wrongWorkCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageOneFinishRawSemanticsV1 # pre # witness
      #&& pnot # (pverifyScriptSourcesStageOneFinishRawSemanticsV1 # pre # wrongWitness)

scriptSourcesStageOneRawFinishCanonicalEncoding :: forall s. Term s PBool
scriptSourcesStageOneRawFinishCanonicalEncoding =
  plet (preplicateBS # 32 # (pintegerToByte # 0xaa)) $ \termRoot ->
  plet (CekProof.pencodeProgramEnvelopeV1 # 1 # 1 # 0 # termRoot # 3 # 144) $
    \scriptBytes ->
  plet
    (pconstant "\x82" <> Codec.pcborInt 3 <> (Codec.pencodeDefiniteBytes # scriptBytes))
    $ \scriptCbor ->
  plet (inputSetsSingletonCommitment 6 scriptCbor) $ \scriptCommitment ->
  plet (pblake2b_224 # (phexByteStr "03" <> scriptBytes)) $ \scriptHash ->
  plet
    ( ScriptProof.pinlineSourceLeafHash
        # 0 # 3 # scriptHash # (plengthBS # scriptCbor)
        # (Bounded.pfromBytes # 6 # 0 # scriptCbor)
    )
    $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  pmatch phaseControl $ \phaseFields ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 1 1 sourcePeaks 1) $ \workCbor ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 2 1 sourcePeaks 1) $ \nextWorkCbor ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 3 1 sourcePeaks 1) $ \mutatedWorkCbor ->
  plet (scriptSourcesSerialiseDataReencoding nextWorkCbor) $ \reencodedWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 10 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 11 nextWorkCbor) $ \post ->
  plet
    (scriptSourcesStateFromPhase transactionId phaseControl 11 reencodedWorkCbor)
    $ \reencodedPost ->
  plet
    (scriptSourcesStateFromPhase transactionId phaseControl 11 mutatedWorkCbor)
    $ \mutatedPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata reencodedPost))
    $ \reencodedWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata mutatedPost))
    $ \mutatedWitness ->
    pand'List
      [ plengthBS # pfromData (pphaseANative'compactCbor phaseFields) #> 64
      , pverifyScriptSourcesStageOneFinishRawSemanticsV1 # pre # witness
      , pnot
          # ( pverifyScriptSourcesStageOneFinishRawSemanticsV1
                # pre # reencodedWitness
            )
      , pnot
          # ( pverifyScriptSourcesStageOneFinishRawSemanticsV1
                # pre # mutatedWitness
            )
      ]

scriptSourcesStageOneRedeemerBeginTransition :: forall s. Term s PBool
scriptSourcesStageOneRedeemerBeginTransition =
  plet (phexByteStr "8400004100820a14") $ \redeemerCbor ->
  plet (inputSetsSingletonCommitment 8 redeemerCbor) $ \redeemerCommitment ->
  pmatch
    ( phaseANativeControlFixtureWithRedeemer
        NativeField.pemptyFieldCommitment redeemerCommitment
        (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 1 0 pnil 0) $ \workCbor ->
  pmatch (inputSetsSingletonProof 8 redeemerCbor) $ \(PPair collectionProof _) ->
  pmatch collectionProof $ \item ->
  plet
    ( RedeemerItemProof.pinitialControlV1
        # RedeemerItemProof.pmodeData # 0 # 1
        # pfromData (BoundedCollection.pitemProof'itemLength item)
        # pfromData (BoundedCollection.pitemProof'itemCommitment item)
        # (-1) # (-1)
    )
    $ \itemControl ->
  plet (RedeemerItemProof.phashControlV1 # itemControl) $ \pendingHash ->
  plet
    (scriptSourcesCborFromPhaseAtStageWithRedeemers phaseControl 1 0 pnil 0 0 pnil 1)
    $ \nextBaseCbor ->
  plet (pscriptSourcesStageZeroControlFromWitness # nextBaseCbor) $ \nextControl ->
  plet
    (pencodeScriptSourcesRedeemerItemWitness # nextControl # pendingHash)
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 66 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 67 nextWorkCbor) $ \post ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post))
    $ \witness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageOneRedeemerBeginSemanticsV1
        # pre # witness # collectionProof

scriptSourcesStageOneRedeemerStepTransition :: forall s. Term s PBool
scriptSourcesStageOneRedeemerStepTransition =
  plet (phexByteStr "8400004100820a14") $ \redeemerCbor ->
  plet (inputSetsSingletonCommitment 8 redeemerCbor) $ \redeemerCommitment ->
  pmatch
    ( phaseANativeControlFixtureWithRedeemer
        NativeField.pemptyFieldCommitment redeemerCommitment
        (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  pmatch (inputSetsSingletonProof 8 redeemerCbor) $ \(PPair collectionProof chunkProof) ->
  pmatch collectionProof $ \item ->
  plet
    ( RedeemerItemProof.pinitialControlV1
        # RedeemerItemProof.pmodeData # 0 # 1
        # pfromData (BoundedCollection.pitemProof'itemLength item)
        # pfromData (BoundedCollection.pitemProof'itemCommitment item)
        # (-1) # (-1)
    )
    $ \itemControl ->
  plet
    ( pcon $ RedeemerItemProof.PRedeemerItemProofWitnessV1
        (pdata $ pcon RedeemerItemProof.PRedeemerItemOpenHeader)
        (pdata $ pcon $ PDJust $ pdata chunkProof)
        (pdata $ pcon PDNothing)
    )
    $ \itemWitness ->
  plet
    ( pcon $ RedeemerItemProof.PRedeemerItemProofControlV1
        (pdata RedeemerItemProof.pversion)
        (pdata RedeemerItemProof.pmodeData)
        (pdata RedeemerItemProof.pstageTail)
        (pdata 0) (pdata 1)
        (BoundedCollection.pitemProof'itemLength item)
        (BoundedCollection.pitemProof'itemCommitment item)
        (pdata $ -1) (pdata $ -1)
        (pdata 0) (pdata 0) (pdata 4) (pdata 1)
        (pdata $ -1) (pdata $ -1)
        (pdata $ pcon PDNothing)
    )
    $ \nextItemControl ->
  plet
    (scriptSourcesCborFromPhaseAtStageWithRedeemers phaseControl 1 0 pnil 0 0 pnil 1)
    $ \baseCbor ->
  plet (pscriptSourcesStageZeroControlFromWitness # baseCbor) $ \baseControl ->
  plet
    ( pencodeScriptSourcesRedeemerItemWitness
        # baseControl # (RedeemerItemProof.phashControlV1 # itemControl)
    )
    $ \workCbor ->
  plet
    ( pencodeScriptSourcesRedeemerItemWitness
        # baseControl # (RedeemerItemProof.phashControlV1 # nextItemControl)
    )
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 68 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 69 nextWorkCbor) $ \post ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post))
    $ \witness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageOneRedeemerHeaderSemanticsV1
        # pre # witness # itemControl # itemWitness

scriptSourcesStageOneRedeemerFamilyGuard :: forall s. Bool -> Term s PBool
scriptSourcesStageOneRedeemerFamilyGuard expectRedeemerRejection =
  plet (phexByteStr "8400004100820a14") $ \redeemerCbor ->
  plet (inputSetsSingletonCommitment 8 redeemerCbor) $ \redeemerCommitment ->
  plet (Bounded.pfromBytes # 8 # 0 # redeemerCbor) $ \itemCommitment ->
  plet (ScriptProof.predeemerItemLeafHash # 0 # itemCommitment) $ \redeemerLeaf ->
  plet (cekSinglePeak redeemerLeaf) $ \redeemerPeaks ->
  pmatch
    ( phaseANativeControlFixtureWithRedeemer
        NativeField.pemptyFieldCommitment redeemerCommitment
        (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesCborFromPhaseAtStageWithRedeemers
        phaseControl 1 0 pnil 0 1 redeemerPeaks 1
    )
    $ \workCbor ->
  plet
    ( scriptSourcesCborFromPhaseAtStageWithRedeemers
        phaseControl 2 0 pnil 0 1 redeemerPeaks 1
    )
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 70 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 71 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet (pcon PNoAuxiliaryWitness) $ \noAuxiliary ->
    if expectRedeemerRejection
      then
        pnot
          # ( pverifyScriptSourcesStageOneRedeemerSemanticsV1
                # pre # witness # noAuxiliary
            )
      else pverifyScriptSourcesStageOneSemanticsV1 # pre # witness # noAuxiliary

scriptSourcesStageTwoReplayInitializationTransition :: forall s. Term s PBool
scriptSourcesStageTwoReplayInitializationTransition =
  plet (cekHash 0x91) $ \resolutionScheduleHash ->
  plet (cekHash 0x92) $ \wrongReplayHead ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesReplayInitializationCbor
        phaseControl 2 pemptyResolutionScheduleHash resolutionScheduleHash
    )
    $ \workCbor ->
  plet
    ( scriptSourcesReplayInitializationCbor
        phaseControl 3 resolutionScheduleHash resolutionScheduleHash
    )
    $ \nextWorkCbor ->
  plet
    ( scriptSourcesReplayInitializationCbor
        phaseControl 3 wrongReplayHead resolutionScheduleHash
    )
    $ \wrongWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 70 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 71 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 71 wrongWorkCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageTwoSemanticsV1
        # pre # witness # pcon PNoAuxiliaryWitness
      #&& pnot
        # ( pverifyScriptSourcesStageTwoSemanticsV1
              # pre # wrongWitness # pcon PNoAuxiliaryWitness
          )

scriptSourcesStageThreeFinishTransition :: forall s. Term s PBool
scriptSourcesStageThreeFinishTransition =
  plet (cekHash 0x93) $ \resolvedAccumulator ->
  plet (cekSinglePeak $ cekHash 0x94) $ \resolvedItemPeaks ->
  plet (cekHash 0x95) $ \resolutionScheduleHash ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesReplayFinishCbor
        phaseControl 3 resolvedAccumulator resolvedItemPeaks resolutionScheduleHash
    )
    $ \workCbor ->
  plet
    ( scriptSourcesReplayFinishCbor
        phaseControl 4 resolvedAccumulator resolvedItemPeaks resolutionScheduleHash
    )
    $ \nextWorkCbor ->
  plet
    ( scriptSourcesReplayFinishCbor
        phaseControl 5 resolvedAccumulator resolvedItemPeaks resolutionScheduleHash
    )
    $ \wrongWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 72 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 73 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 73 wrongWorkCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageThreeFinishSemanticsV1 # pre # witness
      #&& pnot # (pverifyScriptSourcesStageThreeFinishSemanticsV1 # pre # wrongWitness)

scriptSourcesStageThreeReferenceReplayTransition :: forall s. Term s PBool
scriptSourcesStageThreeReferenceReplayTransition =
  plet (phexByteStr "68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") $ \address ->
  plet (preplicateBS # 28 # (pintegerToByte # 0x41)) $ \scriptHash ->
  plet (cekHash 0x42) $ \scriptCommitment ->
  plet
    (scriptSourcesReplayDescriptor address 3 scriptHash 7 scriptCommitment)
    $ \value ->
  plet
    ( pconstant "\x82\x58\x20"
        <> preplicateBS # 32 # (pintegerToByte # 0x44)
        <> Codec.pcborInt 2
    )
    $ \key ->
  plet pemptyResolutionScheduleHash $ \nextScheduleHash ->
  plet (presolutionScheduleNodeHash # 1 # key # nextScheduleHash) $ \scheduleHash ->
  plet
    (presolvedInputAccumulatorSuccessor # pinitialResolutionAccumulator # 1 # key # value)
    $ \resolvedAccumulator ->
  plet
    (ScriptProof.presolvedContextItemLeafHash # 1 # 0 # key # value)
    $ \resolvedLeaf ->
  plet (cekSinglePeak resolvedLeaf) $ \resolvedPeaks ->
  plet
    (ScriptProof.preferenceSourceLeafHash # key # 3 # scriptHash # 7 # scriptCommitment)
    $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesReplayCbor
        phaseControl 3 resolvedAccumulator pnil 0 pinitialResolutionAccumulator
        scheduleHash 0 0 pnil 0 pnil 0 scheduleHash
    )
    $ \workCbor ->
  plet
    ( scriptSourcesReplayCbor
        phaseControl 3 resolvedAccumulator resolvedPeaks 1 resolvedAccumulator
        nextScheduleHash 0 1 sourcePeaks 0 pnil 1 scheduleHash
    )
    $ \nextWorkCbor ->
  plet
    ( scriptSourcesReplayCbor
        phaseControl 3 resolvedAccumulator resolvedPeaks 1 resolvedAccumulator
        nextScheduleHash 0 0 pnil 0 pnil 0 scheduleHash
    )
    $ \wrongWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 74 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 75 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 75 wrongWorkCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageThreeReplaySemanticsV1
        # pre # witness # 1 # key # nextScheduleHash # value
      #&& pnot
        # ( pverifyScriptSourcesStageThreeReplaySemanticsV1
              # pre # wrongWitness # 1 # key # nextScheduleHash # value
          )

scriptSourcesStageThreeSpendReplayTransition :: forall s. Term s PBool
scriptSourcesStageThreeSpendReplayTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0xaa)) $ \scriptHash ->
  plet (preplicateBS # 1 # (pintegerToByte # 0x78) <> scriptHash) $ \address ->
  plet
    ( scriptSourcesReplayDescriptor
        address (-1) (pconstant "") 0 (pconstant "")
    )
    $ \value ->
  plet cekSpendSubject $ \key ->
  plet pemptyResolutionScheduleHash $ \nextScheduleHash ->
  plet (presolutionScheduleNodeHash # 0 # key # nextScheduleHash) $ \scheduleHash ->
  plet
    (presolvedInputAccumulatorSuccessor # pinitialResolutionAccumulator # 0 # key # value)
    $ \resolvedAccumulator ->
  plet
    (ScriptProof.presolvedContextItemLeafHash # 0 # 0 # key # value)
    $ \resolvedLeaf ->
  plet (cekSinglePeak resolvedLeaf) $ \resolvedPeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # key) $ \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesReplayCbor
        phaseControl 3 resolvedAccumulator pnil 0 pinitialResolutionAccumulator
        scheduleHash 0 0 pnil 0 pnil 0 scheduleHash
    )
    $ \workCbor ->
  plet
    ( scriptSourcesReplayCbor
        phaseControl 3 resolvedAccumulator resolvedPeaks 1 resolvedAccumulator
        nextScheduleHash 1 0 pnil 1 purposePeaks 0 scheduleHash
    )
    $ \nextWorkCbor ->
  plet
    ( scriptSourcesReplayCbor
        phaseControl 3 resolvedAccumulator resolvedPeaks 1 resolvedAccumulator
        nextScheduleHash 1 0 pnil 0 pnil 0 scheduleHash
    )
    $ \wrongWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 76 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 77 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 77 wrongWorkCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageThreeReplaySemanticsV1
        # pre # witness # 0 # key # nextScheduleHash # value
      #&& pnot
        # ( pverifyScriptSourcesStageThreeReplaySemanticsV1
              # pre # wrongWitness # 0 # key # nextScheduleHash # value
          )

scriptSourcesStageFourEmptyTransition :: forall s. Term s PBool
scriptSourcesStageFourEmptyTransition =
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 4 0 pnil 0) $ \workCbor ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 5 0 pnil 0) $ \nextWorkCbor ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 6 0 pnil 0) $ \wrongWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 78 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 79 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 79 wrongWorkCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageFourSemanticsV1
        # pre # witness # pcon PNoAuxiliaryWitness
      #&& pnot
        # ( pverifyScriptSourcesStageFourSemanticsV1
              # pre # wrongWitness # pcon PNoAuxiliaryWitness
          )

scriptSourcesStageFourOutputTransition :: forall s. Term s PBool
scriptSourcesStageFourOutputTransition =
  plet (phexByteStr "01") $ \outputCbor ->
  plet (inputSetsSingletonCommitment 2 outputCbor) $ \outputsCommitment ->
  pmatch (inputSetsSingletonProof 2 outputCbor) $ \(PPair collectionProof _) ->
  pmatch collectionProof $ \item ->
  plet
    ( ScriptProof.poutputItemLeafHash
        # 0 # pfromData (BoundedCollection.pitemProof'itemCommitment item)
    )
    $ \outputLeaf ->
  plet (cekSinglePeak outputLeaf) $ \outputPeaks ->
  pmatch
    ( phaseANativeControlFixtureWithOutputs
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        outputsCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageFourCbor phaseControl 4 0 pnil 0) $ \initialWorkCbor ->
  plet (scriptSourcesStageFourCbor phaseControl 4 1 outputPeaks 1) $ \completeWorkCbor ->
  plet (scriptSourcesStageFourCbor phaseControl 5 1 outputPeaks 1) $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 80 initialWorkCbor) $ \initialPre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 81 completeWorkCbor) $ \completeState ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 82 nextWorkCbor) $ \post ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata initialWorkCbor) (pdata completeState))
    $ \appendWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata completeWorkCbor) (pdata post))
    $ \finishWitness ->
  plet
    (pcon $ PTransactionRedeemerItemBeginWitness $ pdata collectionProof)
    $ \appendAuxiliary ->
    pstructuralTransitionIsValid # initialPre # appendWitness
      #&& pverifyScriptSourcesStageFourSemanticsV1
        # initialPre # appendWitness # appendAuxiliary
      #&& pstructuralTransitionIsValid # completeState # finishWitness
      #&& pverifyScriptSourcesStageFourSemanticsV1
        # completeState # finishWitness # pcon PNoAuxiliaryWitness

scriptSourcesStageFourForgedOutputTransition :: forall s. Bool -> Term s PBool
scriptSourcesStageFourForgedOutputTransition forgeCommitment =
  plet (phexByteStr "01") $ \outputCbor ->
  plet (inputSetsSingletonCommitment 2 outputCbor) $ \outputsCommitment ->
  pmatch (inputSetsSingletonProof 2 outputCbor) $ \(PPair collectionProof _) ->
  pmatch collectionProof $ \item ->
  plet
    ( pcon $ BoundedCollection.PItemProofV1
        (BoundedCollection.pitemProof'version item)
        (BoundedCollection.pitemProof'fieldIndex item)
        (BoundedCollection.pitemProof'itemCount item)
        (BoundedCollection.pitemProof'itemIndex item)
        ( pdata $
            if forgeCommitment
              then pfromData (BoundedCollection.pitemProof'itemLength item)
              else pfromData (BoundedCollection.pitemProof'itemLength item) + 1
        )
        ( pdata $
            if forgeCommitment
              then preplicateBS # 31 # (pintegerToByte # 0) <> phexByteStr "ff"
              else pfromData (BoundedCollection.pitemProof'itemCommitment item)
        )
        (BoundedCollection.pitemProof'frontier item)
        (BoundedCollection.pitemProof'siblings item)
    )
    $ \forgedProof ->
  plet
    ( ScriptProof.poutputItemLeafHash
        # 0 # pfromData (BoundedCollection.pitemProof'itemCommitment item)
    )
    $ \outputLeaf ->
  plet (cekSinglePeak outputLeaf) $ \outputPeaks ->
  pmatch
    ( phaseANativeControlFixtureWithOutputs
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        outputsCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageFourCbor phaseControl 4 0 pnil 0) $ \workCbor ->
  plet (scriptSourcesStageFourCbor phaseControl 4 1 outputPeaks 1) $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 83 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 84 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet (pcon $ PTransactionRedeemerItemBeginWitness $ pdata forgedProof) $ \auxiliary ->
    pnot # (pverifyScriptSourcesStageFourSemanticsV1 # pre # witness # auxiliary)

scriptSourcesStageFiveFinishTransition :: forall s. Term s PBool
scriptSourcesStageFiveFinishTransition =
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 5 0 pnil 0) $ \workCbor ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 6 0 pnil 0) $ \nextWorkCbor ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 7 0 pnil 0) $ \wrongWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 83 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 84 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 84 wrongWorkCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageFiveFinishSemanticsV1 # pre # witness
      #&& pnot # (pverifyScriptSourcesStageFiveFinishSemanticsV1 # pre # wrongWitness)

scriptSourcesStageFiveRouting :: forall s. Term s PBool
scriptSourcesStageFiveRouting =
  plet (pcon PDNothing) $ \noProof ->
  plet
    ( pcon $ PLedgerOutputProofBeginWitness
        (pdata 0) (pdata 1) (pdata $ cekHash 0x55) (pdata pnil)
    )
    $ \beginWitness ->
  plet
    ( pcon $ PLedgerOutputProofStepWitness
        (pdata $ pcon LedgerOutputProof.PLedgerOutputProofNoWitness)
    )
    $ \stepWitness ->
  plet
    ( pcon $ PLedgerOutputProofFinalizeWitness
        (pdata resolveInputsNoReferenceDescriptor) (pdata $ pcon PNoSignerSetProof)
    )
    $ \finalizeWitness ->
  plet
    (pdata $ LedgerOutputProof.pinitialControlV1 # 0 # 1 # cekHash 0x55)
    $ \activeProof ->
  plet (pdata resolveInputsNoReferenceTerminal) $ \terminalProof ->
    pand'List
      [ pscriptSourcesStageFiveBranchV1
          # 1 # 1 # noProof # pcon PNoAuxiliaryWitness #== 1
      , pscriptSourcesStageFiveBranchV1
          # 1 # 1 # noProof # finalizeWitness #== 0
      , pscriptSourcesStageFiveBranchV1
          # 0 # 1 # noProof # beginWitness #== 2
      , pscriptSourcesStageFiveBranchV1
          # 0 # 1 # pcon (PDJust activeProof) # stepWitness #== 3
      , pscriptSourcesStageFiveBranchV1
          # 0 # 1 # pcon (PDJust terminalProof) # finalizeWitness #== 4
      ]

scriptSourcesStageSixCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger ->
  Term s PMintFoldControlV1 -> Term s PByteString
scriptSourcesStageSixCbor control stage mintFold =
  scriptSourcesStageSixMintCbor control stage 0 pnil mintFold

scriptSourcesStageSixMintCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PMintFoldControlV1 -> Term s PByteString
scriptSourcesStageSixMintCbor control stage purposeCount purposePeaks mintFold =
  pmatch control $ \c ->
  pencodeScriptSourcesWitness
    # pfromData (pphaseANative'compactCbor c)
    # pfromData (pphaseANative'witnessSetCompactCbor c)
    # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    # pfromData (pphaseANative'contextCbor c)
    # 0 # pinitialResolutionAccumulator
    # pfromData (pphaseANative'signerCount c)
    # ( Merkle.pfrontierCommitment
          # pfromData (pphaseANative'signerCount c)
          # pfromData (pphaseANative'signerPeaks c)
      )
    # pnil # stage # 0 # pnil # 0 # pnil
    # 0 # pinitialResolutionAccumulator # pemptyResolutionScheduleHash
    # 0 # purposeCount # purposePeaks # 0 # 0 # pnil # 0
    # pemptyReceivePurposeScanControl
    # 0 # 0 # pemptyObserverPurposeScanControl
    # mintFold # pemptyResolutionScheduleHash

scriptSourcesStageSixEmptyTransition :: forall s. Term s PBool
scriptSourcesStageSixEmptyTransition =
  plet
    ( pcon $ PMintFoldControlV1
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata 0) (pdata 0)
        (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \completeFold ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageSixCbor phaseControl 6 pemptyMintFoldControl) $ \workCbor ->
  plet (scriptSourcesStageSixCbor phaseControl 7 completeFold) $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 91 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 92 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageSixEmptySemanticsV1
        # pre # witness # pcon PNoAuxiliaryWitness

scriptSourcesStageSixFinishTransition :: forall s. Term s PBool
scriptSourcesStageSixFinishTransition =
  plet (cekSinglePeak $ cekHash 0x66) $ \assetPeaks ->
  plet
    ( pcon $ PMintFoldControlV1
        (pdata 1) (pdata 1)
        (pdata $ phexByteStr "11111111111111111111111111111111111111111111111111111111")
        (pdata $ pconstant "") (pdata 0) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata 0) (pdata $ pconstant "")
        (pdata 1) (pdata assetPeaks)
    )
    $ \completeFold ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageSixCbor phaseControl 6 completeFold) $ \workCbor ->
  plet (scriptSourcesStageSixCbor phaseControl 7 completeFold) $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 93 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 94 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageSixFinishSemanticsV1
        # pre # witness # pcon PNoAuxiliaryWitness

scriptSourcesStageSixBeginTripwire :: forall s. Term s PBool
scriptSourcesStageSixBeginTripwire =
  plet
    (phexByteStr "82581c11111111111111111111111111111111111111111111111111111111a1410124")
    $ \policyItemCbor ->
  pmatch (inputSetsSingletonProof 5 policyItemCbor) $ \(PPair collectionProof chunkProof) ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageSixCbor phaseControl 6 pemptyMintFoldControl) $ \workCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 95 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 96 workCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pnot
          # ( pverifyScriptSourcesStageSixBeginSemanticsV1
                # pre # witness # collectionProof # chunkProof
            )
      , pnot
          # ( BoundedCollection.pverifyBoundedCollectionItem
                # NativeField.pemptyFieldCommitment # collectionProof
            )
      ]

scriptSourcesStageSixAssetTransition :: forall s. Term s PBool
scriptSourcesStageSixAssetTransition =
  plet
    (phexByteStr "82581c11111111111111111111111111111111111111111111111111111111a1410124")
    $ \policyItemCbor ->
  plet (phexByteStr "11111111111111111111111111111111111111111111111111111111") $ \policyId ->
  pmatch (inputSetsSingletonProof 5 policyItemCbor) $ \(PPair collectionProof chunkProof) ->
  pmatch collectionProof $ \item ->
  plet
    (cekSinglePeak $ ScriptProof.ppurposeLeafHash # 1 # 0 # policyId # policyId)
    $ \purposePeaks ->
  plet (cekSinglePeak $ pmintAssetLeafHash # policyId # phexByteStr "01" # (-5)) $
    \assetPeaks ->
  plet
    ( pcon $ PMintFoldControlV1
        (pdata 1) (pdata 0) (pdata $ pconstant "") (pdata policyId)
        (BoundedCollection.pitemProof'itemLength item)
        (BoundedCollection.pitemProof'itemCommitment item)
        (pdata 32) (pdata 1) (pdata 0) (pdata $ pconstant "")
        (pdata 0) (pdata pnil)
    )
    $ \activeFold ->
  plet
    ( pcon $ PMintFoldControlV1
        (pdata 1) (pdata 1) (pdata policyId) (pdata $ pconstant "")
        (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata 0) (pdata 0)
        (pdata $ pconstant "") (pdata 1) (pdata assetPeaks)
    )
    $ \completedFold ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageSixMintCbor phaseControl 6 1 purposePeaks activeFold) $ \workCbor ->
  plet (scriptSourcesStageSixMintCbor phaseControl 6 1 purposePeaks completedFold) $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 97 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 98 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageSixAssetSemanticsV1
        # pre # witness # chunkProof # pcon PDNothing

scriptSourcesStageSixBoundaryTransition :: forall s. Term s PBool -> Term s PBool
scriptSourcesStageSixBoundaryTransition withholdNext =
  plet (phexByteStr "11111111111111111111111111111111111111111111111111111111") $ \policyId ->
  plet (preplicateBS # 4093 # (pintegerToByte # 0) <> phexByteStr "410124") $ \itemCbor ->
  plet (psliceBS # 0 # 4095 # itemCbor) $ \firstChunk ->
  plet (psliceBS # 4095 # 1 # itemCbor) $ \secondChunk ->
  plet (Bounded.phashChunk # 5 # 0 # 0 # firstChunk) $ \firstHash ->
  plet (Bounded.phashChunk # 5 # 0 # 1 # secondChunk) $ \secondHash ->
  plet
    (Merkle.pbuildFrontier #$ pcons # pdata firstHash #$ pcons # pdata secondHash # pnil)
    $ \chunkFrontier ->
  pmatch chunkFrontier $ \built ->
  plet
    (Bounded.pcommitment # 5 # 0 # 4096 # Merkle.pbuiltFrontier'peaks built)
    $ \itemCommitment ->
  plet
    ( pcon $ Bounded.PChunkProofV1
        (pdata Bounded.pversion) (pdata 5) (pdata 0) (pdata 4096) (pdata 0)
        (pdata firstChunk) (pdata $ Merkle.pbuiltFrontier'peaks built)
        (pdata $ pcons # pdata secondHash # pnil)
    )
    $ \firstProof ->
  plet
    ( pcon $ Bounded.PChunkProofV1
        (pdata Bounded.pversion) (pdata 5) (pdata 0) (pdata 4096) (pdata 1)
        (pdata secondChunk) (pdata $ Merkle.pbuiltFrontier'peaks built)
        (pdata $ pcons # pdata firstHash # pnil)
    )
    $ \secondProof ->
  plet
    (cekSinglePeak $ ScriptProof.ppurposeLeafHash # 1 # 0 # policyId # policyId)
    $ \purposePeaks ->
  plet (cekSinglePeak $ pmintAssetLeafHash # policyId # phexByteStr "01" # (-5)) $
    \assetPeaks ->
  plet
    ( pcon $ PMintFoldControlV1
        (pdata 1) (pdata 0) (pdata $ pconstant "") (pdata policyId)
        (pdata 4096) (pdata itemCommitment) (pdata 4093) (pdata 1)
        (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \activeFold ->
  plet
    ( pcon $ PMintFoldControlV1
        (pdata 1) (pdata 1) (pdata policyId) (pdata $ pconstant "")
        (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata 0) (pdata 0)
        (pdata $ pconstant "") (pdata 1) (pdata assetPeaks)
    )
    $ \completedFold ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageSixMintCbor phaseControl 6 1 purposePeaks activeFold) $ \workCbor ->
  plet (scriptSourcesStageSixMintCbor phaseControl 6 1 purposePeaks completedFold) $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 99 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 100 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    ( pif withholdNext
        (pcon PDNothing)
        (pcon $ PDJust $ pdata secondProof)
    )
    $ \nextProof ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesStageSixAssetSemanticsV1
        # pre # witness # firstProof # nextProof

scriptSourcesStageSixRouting :: forall s. Term s PBool
scriptSourcesStageSixRouting =
  plet
    ( pcon $ PMintFoldControlV1
        (pdata 1) (pdata 1)
        (pdata $ phexByteStr "11111111111111111111111111111111111111111111111111111111")
        (pdata $ pconstant "") (pdata 0) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata 0) (pdata $ pconstant "")
        (pdata 0) (pdata pnil)
    )
    $ \completedFold ->
  plet
    ( pcon $ PMintFoldControlV1
        (pdata 1) (pdata 0) (pdata $ pconstant "")
        (pdata $ phexByteStr "11111111111111111111111111111111111111111111111111111111")
        (pdata 2) (pdata $ cekHash 0x77) (pdata 1) (pdata 1)
        (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \activeFold ->
    pand'List
      [ pscriptSourcesStageSixBranchV1
          # NativeField.pemptyFieldCommitment # pemptyMintFoldControl
          # pcon PNoAuxiliaryWitness #== 1
      , pscriptSourcesStageSixBranchV1
          # (cekHash 0x78) # completedFold # pcon PNoAuxiliaryWitness #== 2
      , pscriptSourcesStageSixBranchV1
          # (cekHash 0x78) # pemptyMintFoldControl # pcon PNoAuxiliaryWitness #== 3
      , pscriptSourcesStageSixBranchV1
          # (cekHash 0x78) # activeFold # pcon PNoAuxiliaryWitness #== 4
      ]

scriptSourcesStageSevenMintFold :: forall s. Term s PMintFoldControlV1
scriptSourcesStageSevenMintFold =
  pcon $ PMintFoldControlV1
    (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata $ pconstant "")
    (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata 0) (pdata 0)
    (pdata $ pconstant "") (pdata 0) (pdata pnil)

scriptSourcesStageSevenCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PObserverPurposeScanControlV1 ->
  Term s PByteString
scriptSourcesStageSevenCbor control purposeCount purposePeaks observerScan =
  scriptSourcesStageSevenStateCbor
    control 7 purposeCount purposePeaks 0 0 pnil
    pemptyReceivePurposeScanControl observerScan

scriptSourcesStageSevenStateCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PReceivePurposeScanControlV1 ->
  Term s PObserverPurposeScanControlV1 -> Term s PByteString
scriptSourcesStageSevenStateCbor
  control stage purposeCount purposePeaks outputCursor outputCount outputPeaks
  receiveScan observerScan =
  plet
    ( scriptSourcesBaseStateCbor
        control stage purposeCount purposePeaks outputCursor outputCount outputPeaks
        receiveScan observerScan
    )
    $ \base ->
  pif (stage #>= 8)
    ( pencodeScriptSourcesDiscoveryWitness
        # (pscriptSourcesStageZeroControlFromWitness # base)
        # stage # pemptyScriptDiscoveryControl
    )
    base

scriptSourcesBaseStateCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PReceivePurposeScanControlV1 ->
  Term s PObserverPurposeScanControlV1 -> Term s PByteString
scriptSourcesBaseStateCbor
  control stage purposeCount purposePeaks outputCursor outputCount outputPeaks
  receiveScan observerScan =
  pmatch control $ \c ->
  pencodeScriptSourcesWitness
    # pfromData (pphaseANative'compactCbor c)
    # pfromData (pphaseANative'witnessSetCompactCbor c)
    # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    # pfromData (pphaseANative'contextCbor c)
    # 0 # pinitialResolutionAccumulator
    # pfromData (pphaseANative'signerCount c)
    # ( Merkle.pfrontierCommitment
          # pfromData (pphaseANative'signerCount c)
          # pfromData (pphaseANative'signerPeaks c)
      )
    # pnil # stage # 0 # pnil # 0 # pnil
    # 0 # pinitialResolutionAccumulator # pemptyResolutionScheduleHash
    # 0 # purposeCount # purposePeaks
    # outputCursor # outputCount # outputPeaks # outputCount
    # receiveScan
    # 0 # 0 # observerScan
    # scriptSourcesStageSevenMintFold # pemptyResolutionScheduleHash

scriptSourcesDiscoveryStateCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PScriptDiscoveryControlV1 -> Term s PByteString
scriptSourcesDiscoveryStateCbor control stage purposeCount purposePeaks discovery =
  plet
    ( scriptSourcesBaseStateCbor
        control stage purposeCount purposePeaks 0 0 pnil
        pemptyReceivePurposeScanControl pemptyObserverPurposeScanControl
    )
    $ \base ->
    pencodeScriptSourcesDiscoveryWitness
      # (pscriptSourcesStageZeroControlFromWitness # base)
      # stage # discovery

scriptSourcesDiscoverySourceStateCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger ->
  Term s PInteger -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PInteger -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PScriptDiscoveryControlV1 -> Term s PByteString
scriptSourcesDiscoverySourceStateCbor
  control stage sourceCount sourcePeaks purposeCount purposePeaks discovery =
  scriptSourcesDiscoveryFullStateCbor
    control stage sourceCount sourcePeaks 0 pnil purposeCount purposePeaks discovery

scriptSourcesDiscoveryFullStateCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInteger ->
  Term s PInteger -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PInteger -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PInteger -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PScriptDiscoveryControlV1 -> Term s PByteString
scriptSourcesDiscoveryFullStateCbor
  control stage sourceCount sourcePeaks redeemerCount redeemerPeaks
  purposeCount purposePeaks discovery =
  pmatch control $ \c ->
  plet
    ( pencodeScriptSourcesWitness
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
        # pfromData (pphaseANative'contextCbor c)
        # 0 # pinitialResolutionAccumulator
        # pfromData (pphaseANative'signerCount c)
        # ( Merkle.pfrontierCommitment
              # pfromData (pphaseANative'signerCount c)
              # pfromData (pphaseANative'signerPeaks c)
          )
        # pnil # stage # sourceCount # sourcePeaks # redeemerCount # redeemerPeaks
        # 0 # pinitialResolutionAccumulator # pemptyResolutionScheduleHash
        # 0 # purposeCount # purposePeaks # 0 # 0 # pnil # 0
        # pemptyReceivePurposeScanControl
        # sourceCount # redeemerCount # pemptyObserverPurposeScanControl
        # scriptSourcesStageSevenMintFold # pemptyResolutionScheduleHash
    )
    $ \base ->
    pencodeScriptSourcesDiscoveryWitness
      # (pscriptSourcesStageZeroControlFromWitness # base)
      # stage # discovery

scriptSourcesStageSevenObserverTransition :: forall s. Term s PBool
scriptSourcesStageSevenObserverTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x31)) $ \observerHash ->
  plet (inputSetsSingletonCommitment 3 observerHash) $ \observerCommitment ->
  pmatch (inputSetsSingletonProof 3 observerHash) $ \(PPair collectionProof chunkProof) ->
  plet
    (cekSinglePeak $ ScriptProof.ppurposeLeafHash # 2 # 0 # observerHash # observerHash)
    $ \purposePeaks ->
  plet
    (pcon $ PObserverPurposeScanControlV1 (pdata 1) (pdata 1) (pdata observerHash))
    $ \nextObserverScan ->
  pmatch
    ( phaseANativeControlFixtureWithOutputsAndObservers
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment observerCommitment
        (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesStageSevenCbor phaseControl 0 pnil pemptyObserverPurposeScanControl)
    $ \workCbor ->
  plet
    (scriptSourcesStageSevenCbor phaseControl 1 purposePeaks nextObserverScan)
    $ \nextWorkCbor ->
  plet
    (scriptSourcesStageSevenCbor phaseControl 0 pnil nextObserverScan)
    $ \wrongWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 101 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 102 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 102 wrongWorkCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageSevenObserverSemanticsV1
          # pre # witness # collectionProof # chunkProof
      , pverifyScriptSourcesStageSevenSemanticsV1
          # pre # witness
          # pcon (PTransactionFieldChunkWitness (pdata collectionProof) (pdata chunkProof))
      , pnot
          # ( pverifyScriptSourcesStageSevenObserverSemanticsV1
                # pre # wrongWitness # collectionProof # chunkProof
            )
      , pnot
          # ( pverifyScriptSourcesStageSevenSemanticsV1
                # pre # wrongWitness
                # pcon (PTransactionFieldChunkWitness (pdata collectionProof) (pdata chunkProof))
            )
      ]

scriptSourcesStageSevenObserverFinishTransition :: forall s. Term s PBool
scriptSourcesStageSevenObserverFinishTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0xaa)) $ \observerHash ->
  plet (inputSetsSingletonCommitment 3 observerHash) $ \observerCommitment ->
  plet
    (cekSinglePeak $ ScriptProof.ppurposeLeafHash # 2 # 0 # observerHash # observerHash)
    $ \purposePeaks ->
  plet
    (pcon $ PObserverPurposeScanControlV1 (pdata 1) (pdata 1) (pdata observerHash))
    $ \observerScan ->
  pmatch
    ( phaseANativeControlFixtureWithOutputsAndObservers
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment observerCommitment
        (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageSevenCbor phaseControl 1 purposePeaks observerScan) $ \workCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 8 1 purposePeaks 0 0 pnil
        pemptyReceivePurposeScanControl pemptyObserverPurposeScanControl
    )
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 19 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 20 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageSevenFinishSemanticsV1 # pre # witness
      , pverifyScriptSourcesStageSevenSemanticsV1
          # pre # witness # pcon PNoAuxiliaryWitness
      ]

scriptSourcesStageSevenFinishCanonicalEncoding :: forall s. Term s PBool
scriptSourcesStageSevenFinishCanonicalEncoding =
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesStageSevenCbor phaseControl 0 pnil pemptyObserverPurposeScanControl)
    $ \workCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 8 0 pnil 0 0 pnil
        pemptyReceivePurposeScanControl pemptyObserverPurposeScanControl
    )
    $ \nextWorkCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 9 0 pnil 0 0 pnil
        pemptyReceivePurposeScanControl pemptyObserverPurposeScanControl
    )
    $ \mutatedWorkCbor ->
  plet (scriptSourcesSerialiseDataReencoding nextWorkCbor) $ \reencodedWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 18 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 19 nextWorkCbor) $ \post ->
  plet
    (scriptSourcesStateFromPhase transactionId phaseControl 19 reencodedWorkCbor)
    $ \reencodedPost ->
  plet
    (scriptSourcesStateFromPhase transactionId phaseControl 19 mutatedWorkCbor)
    $ \mutatedPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata reencodedPost))
    $ \reencodedWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata mutatedPost))
    $ \mutatedWitness ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata witness) (pdata $ pcon PNoAuxiliaryWitness))
    $ \evidence ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata reencodedWitness) (pdata $ pcon PNoAuxiliaryWitness)
    )
    $ \reencodedEvidence ->
    pand'List
      [ psliceBS # 0 # 1 # nextWorkCbor #== phexByteStr "98"
      , psliceBS # 0 # 1 # reencodedWorkCbor #== phexByteStr "9f"
      , pverifyScriptSourcesStageSevenFinishSemanticsV1 # pre # witness
      , pverifyScriptSourcesOneStepV1 # pre # evidence
      , pnot
          # ( pverifyScriptSourcesStageSevenFinishSemanticsV1
                # pre # reencodedWitness
            )
      , pnot # (pverifyScriptSourcesOneStepV1 # pre # reencodedEvidence)
      , pnot
          # ( pverifyScriptSourcesStageSevenFinishSemanticsV1
                # pre # mutatedWitness
            )
      ]

scriptSourcesStageSevenObserverCanonicalEncoding :: forall s. Term s PBool
scriptSourcesStageSevenObserverCanonicalEncoding =
  plet (preplicateBS # 28 # (pintegerToByte # 0xaa)) $ \observerHash ->
  plet (inputSetsSingletonCommitment 3 observerHash) $ \observerCommitment ->
  pmatch (inputSetsSingletonProof 3 observerHash) $ \(PPair collectionProof chunkProof) ->
  plet
    (cekSinglePeak $ ScriptProof.ppurposeLeafHash # 2 # 0 # observerHash # observerHash)
    $ \purposePeaks ->
  plet
    (pcon $ PObserverPurposeScanControlV1 (pdata 1) (pdata 1) (pdata observerHash))
    $ \observerScan ->
  pmatch
    ( phaseANativeControlFixtureWithOutputsAndObservers
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment observerCommitment
        (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesStageSevenCbor phaseControl 0 pnil pemptyObserverPurposeScanControl)
    $ \workCbor ->
  plet
    (scriptSourcesStageSevenCbor phaseControl 1 purposePeaks observerScan)
    $ \nextWorkCbor ->
  plet
    (scriptSourcesStageSevenCbor phaseControl 0 pnil observerScan)
    $ \mutatedWorkCbor ->
  plet (scriptSourcesSerialiseDataReencoding nextWorkCbor) $ \reencodedWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 18 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 19 nextWorkCbor) $ \post ->
  plet
    (scriptSourcesStateFromPhase transactionId phaseControl 19 reencodedWorkCbor)
    $ \reencodedPost ->
  plet
    (scriptSourcesStateFromPhase transactionId phaseControl 19 mutatedWorkCbor)
    $ \mutatedPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata reencodedPost))
    $ \reencodedWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata mutatedPost))
    $ \mutatedWitness ->
    pand'List
      [ pverifyScriptSourcesStageSevenObserverSemanticsV1
          # pre # witness # collectionProof # chunkProof
      , pnot
          # ( pverifyScriptSourcesStageSevenObserverSemanticsV1
                # pre # reencodedWitness # collectionProof # chunkProof
            )
      , pnot
          # ( pverifyScriptSourcesStageSevenObserverSemanticsV1
                # pre # mutatedWitness # collectionProof # chunkProof
            )
      ]

scriptSourcesStageSevenReceiveCanonicalEncoding :: forall s. Term s PBool
scriptSourcesStageSevenReceiveCanonicalEncoding =
  plet
    (phexByteStr "a200581d78aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0")
    $ \outputCbor ->
  plet
    (phexByteStr "78aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    $ \address ->
  plet (preplicateBS # 28 # (pintegerToByte # 0xaa)) $ \scriptHash ->
  plet (resolveInputsNoReferenceDescriptorFor outputCbor address) $ \descriptorCbor ->
  plet (inputSetsSingletonCommitment 2 outputCbor) $ \outputsCommitment ->
  pmatch (inputSetsSingletonProof 2 outputCbor) $ \(PPair collectionProof _) ->
  pmatch collectionProof $ \item ->
  plet
    (ScriptProof.poutputItemLeafHash # 0 # pfromData (BoundedCollection.pitemProof'itemCommitment item))
    $ \outputLeaf ->
  plet (cekSinglePeak outputLeaf) $ \outputPeaks ->
  plet
    (cekSinglePeak $ ScriptProof.ppurposeLeafHash # 3 # 0 # scriptHash # scriptHash)
    $ \sourcePeaks ->
  plet
    (cekSinglePeak $ ScriptProof.poutputDescriptorLeafHash # 0 # descriptorCbor)
    $ \descriptorPeaks ->
  plet
    ( pcon $ PReceivePurposeScanControlV1
        (pdata 1) (pdata sourcePeaks) (pdata 0)
        (pdata $ pconstant "") (pdata $ pconstant "") (pdata descriptorPeaks)
    )
    $ \initialScan ->
  plet
    ( pcon $ PReceivePurposeScanControlV1
        (pdata 1) (pdata sourcePeaks) (pdata 0)
        (pdata $ pconstant "") (pdata scriptHash) (pdata descriptorPeaks)
    )
    $ \selectedScan ->
  pmatch
    ( phaseANativeControlFixtureWithOutputs
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        outputsCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 7 0 pnil 0 1 outputPeaks initialScan
        pemptyObserverPurposeScanControl
    )
    $ \workCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 7 0 pnil 1 1 outputPeaks selectedScan
        pemptyObserverPurposeScanControl
    )
    $ \nextWorkCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 7 0 pnil 0 1 outputPeaks selectedScan
        pemptyObserverPurposeScanControl
    )
    $ \mutatedWorkCbor ->
  plet (scriptSourcesSerialiseDataReencoding nextWorkCbor) $ \reencodedWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 15 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 16 nextWorkCbor) $ \post ->
  plet
    (scriptSourcesStateFromPhase transactionId phaseControl 16 reencodedWorkCbor)
    $ \reencodedPost ->
  plet
    (scriptSourcesStateFromPhase transactionId phaseControl 16 mutatedWorkCbor)
    $ \mutatedPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata reencodedPost))
    $ \reencodedWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata mutatedPost))
    $ \mutatedWitness ->
    pand'List
      [ pverifyScriptSourcesStageSevenReceiveSemanticsV1
          # pre # witness # 3 # 0 # scriptHash # scriptHash # pnil
      , pnot
          # ( pverifyScriptSourcesStageSevenReceiveSemanticsV1
                # pre # reencodedWitness # 3 # 0 # scriptHash # scriptHash # pnil
            )
      , pnot
          # ( pverifyScriptSourcesStageSevenReceiveSemanticsV1
                # pre # mutatedWitness # 3 # 0 # scriptHash # scriptHash # pnil
            )
      ]

scriptSourcesSerialiseDataReencoding :: forall s.
  Term s PByteString -> Term s PByteString
scriptSourcesSerialiseDataReencoding workCbor =
  pmatch (pdeserialise # workCbor) $ \case
    PNothing -> perror
    PJust workData -> pserialiseData # (plistData # (pasList # workData))

scriptSourcesStageSevenReceiveTransition :: forall s. Integer -> Term s PBool
scriptSourcesStageSevenReceiveTransition stepToVerify =
  plet (preplicateBS # 28 # (pintegerToByte # 0x41)) $ \scriptHash ->
  plet (ScriptProof.ppurposeLeafHash # 3 # 0 # scriptHash # scriptHash) $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (cekSinglePeak $ cekHash 0x42) $ \outputPeaks ->
  plet (cekSinglePeak $ cekHash 0x43) $ \descriptorPeaks ->
  plet
    ( pcon $ PReceivePurposeScanControlV1
        (pdata 1) (pdata sourcePeaks) (pdata 0)
        (pdata $ pconstant "") (pdata $ pconstant "") (pdata descriptorPeaks)
    )
    $ \initialScan ->
  plet
    ( pcon $ PReceivePurposeScanControlV1
        (pdata 1) (pdata sourcePeaks) (pdata 0)
        (pdata $ pconstant "") (pdata scriptHash) (pdata descriptorPeaks)
    )
    $ \selectedScan ->
  plet
    ( pcon $ PReceivePurposeScanControlV1
        (pdata 1) (pdata sourcePeaks) (pdata 1)
        (pdata scriptHash) (pdata $ pconstant "") (pdata descriptorPeaks)
    )
    $ \appendedScan ->
  plet
    ( pcon $ PReceivePurposeScanControlV1
        (pdata 0) (pdata pnil) (pdata 0)
        (pdata $ pconstant "") (pdata $ pconstant "") (pdata descriptorPeaks)
    )
    $ \finishedScan ->
  plet (cekSinglePeak sourceLeaf) $ \purposePeaks ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 7 0 pnil 0 1 outputPeaks initialScan
        pemptyObserverPurposeScanControl
    )
    $ \initialCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 7 0 pnil 1 1 outputPeaks selectedScan
        pemptyObserverPurposeScanControl
    )
    $ \selectedCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 7 0 pnil 0 1 outputPeaks selectedScan
        pemptyObserverPurposeScanControl
    )
    $ \wrongSelectedCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 7 1 purposePeaks 0 1 outputPeaks appendedScan
        pemptyObserverPurposeScanControl
    )
    $ \appendedCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 7 1 purposePeaks 1 1 outputPeaks appendedScan
        pemptyObserverPurposeScanControl
    )
    $ \rescannedCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 8 1 purposePeaks 1 1 outputPeaks finishedScan
        pemptyObserverPurposeScanControl
    )
    $ \finishedCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 103 initialCbor) $ \initialState ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 104 selectedCbor) $ \selectedState ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 104 wrongSelectedCbor) $ \wrongSelectedState ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 105 appendedCbor) $ \appendedState ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 106 rescannedCbor) $ \rescannedState ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 107 finishedCbor) $ \finishedState ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata initialCbor) (pdata selectedState))
    $ \selectWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata initialCbor) (pdata wrongSelectedState))
    $ \wrongSelectWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata selectedCbor) (pdata appendedState))
    $ \appendWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata appendedCbor) (pdata rescannedState))
    $ \rescanWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata rescannedCbor) (pdata finishedState))
    $ \finishWitness ->
  plet
    ( pcon $ PScriptPurposeScanWitness
        (pdata 3) (pdata 0) (pdata scriptHash) (pdata scriptHash) (pdata pnil)
    )
    $ \auxiliary ->
    if stepToVerify == 0
      then
        pstructuralTransitionIsValid # initialState # selectWitness
          #&& pverifyScriptSourcesStageSevenReceiveSemanticsV1
            # initialState # selectWitness # 3 # 0 # scriptHash # scriptHash # pnil
          #&& pverifyScriptSourcesStageSevenSemanticsV1
            # initialState # selectWitness # auxiliary
          #&& pnot
            # ( pverifyScriptSourcesStageSevenReceiveSemanticsV1
                  # initialState # wrongSelectWitness # 3 # 0
                  # scriptHash # scriptHash # pnil
              )
          #&& pnot
            # ( pverifyScriptSourcesStageSevenSemanticsV1
                  # initialState # wrongSelectWitness # auxiliary
              )
      else if stepToVerify == 1
        then
          pstructuralTransitionIsValid # selectedState # appendWitness
            #&& pverifyScriptSourcesStageSevenFinishSemanticsV1
              # selectedState # appendWitness
            #&& pverifyScriptSourcesStageSevenSemanticsV1
              # selectedState # appendWitness # pcon PNoAuxiliaryWitness
        else if stepToVerify == 2
          then
            pstructuralTransitionIsValid # appendedState # rescanWitness
              #&& pverifyScriptSourcesStageSevenReceiveSemanticsV1
                # appendedState # rescanWitness # 3 # 0 # scriptHash # scriptHash # pnil
              #&& pverifyScriptSourcesStageSevenSemanticsV1
                # appendedState # rescanWitness # auxiliary
          else
            pconstant (stepToVerify == 3)
              #&& pstructuralTransitionIsValid # rescannedState # finishWitness
              #&& pverifyScriptSourcesStageSevenFinishSemanticsV1
                # rescannedState # finishWitness
              #&& pverifyScriptSourcesStageSevenSemanticsV1
                # rescannedState # finishWitness # pcon PNoAuxiliaryWitness

scriptSourcesStageSevenEmptyFinishTransition :: forall s. Term s PBool
scriptSourcesStageSevenEmptyFinishTransition =
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesStageSevenCbor phaseControl 0 pnil pemptyObserverPurposeScanControl)
    $ \workCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 8 0 pnil 0 0 pnil
        pemptyReceivePurposeScanControl pemptyObserverPurposeScanControl
    )
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 106 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 107 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageSevenFinishSemanticsV1 # pre # witness
      , pverifyScriptSourcesStageSevenSemanticsV1
          # pre # witness # pcon PNoAuxiliaryWitness
      ]

indexedSpendPurposeLeaves :: forall s. Int -> Int -> [Term s PByteString]
indexedSpendPurposeLeaves start count =
  [ ScriptProof.ppurposeLeafHash
      # 0 # pconstant index # scriptHash # Codec.pcborInt (pconstant index)
  | index <- [toInteger start .. toInteger (start + count - 1)]
  ]
  where
    scriptHash = preplicateBS # 28 # (pintegerToByte # 0xaa)

byteStringDataList :: forall s. [Term s PByteString] -> Term s (PBuiltinList (PAsData PByteString))
byteStringDataList = foldr (\item rest -> pcons # pdata item # rest) pnil

purposeParentLayer :: forall s. [Term s PByteString] -> [Term s PByteString]
purposeParentLayer [] = []
purposeParentLayer (left : right : rest) =
  (Merkle.phashBranch # left # right) : purposeParentLayer rest
purposeParentLayer _ = error "power-of-two purpose layer must contain complete pairs"

firstPurposeSiblings :: forall s. [Term s PByteString] -> [Term s PByteString]
firstPurposeSiblings [_] = []
firstPurposeSiblings leaves@(_ : sibling : _) =
  sibling : firstPurposeSiblings (purposeParentLayer leaves)
firstPurposeSiblings _ = error "purpose tree must be a non-empty power of two"

scriptSourcesPreparesMoreThanSixteenPurposes :: forall s. Term s PBool
scriptSourcesPreparesMoreThanSixteenPurposes =
  pmatch (Merkle.pbuildFrontier # byteStringDataList (indexedSpendPurposeLeaves 0 17)) $
    \(Merkle.PBuiltFrontier purposeCount purposePeaks) ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesStageSevenCbor phaseControl purposeCount purposePeaks pemptyObserverPurposeScanControl)
    $ \workCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 8 purposeCount purposePeaks 0 0 pnil
        pemptyReceivePurposeScanControl pemptyObserverPurposeScanControl
    )
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 18 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 19 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata witness) (pdata $ pcon PNoAuxiliaryWitness))
    $ \evidence ->
      purposeCount #== 17
        #&& pverifyScriptSourcesOneStepV1 # pre # evidence

scriptSourcesDiscoversMoreThanSixteenPurposes :: forall s. Term s PBool
scriptSourcesDiscoversMoreThanSixteenPurposes =
  let leaves = indexedSpendPurposeLeaves 0 17
      siblings = byteStringDataList (firstPurposeSiblings (take 16 leaves))
   in pmatch (Merkle.pbuildFrontier # byteStringDataList leaves) $
        \(Merkle.PBuiltFrontier purposeCount purposePeaks) ->
      plet (preplicateBS # 28 # (pintegerToByte # 0xaa)) $ \scriptHash ->
      plet (Codec.pcborInt 0) $ \subject ->
      plet
        ( pcon $ PScriptDiscoveryControlV1
            (pdata 0) (pdata 0) (pdata 0) (pdata 0) (pdata 0)
            (pdata scriptHash) (pdata subject)
            (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
            (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
        )
        $ \selectedPurpose ->
      pmatch
        ( phaseANativeControlFixture
            NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
              (pconstant "") 0 0 (-1)
        )
        $ \(PPair phaseControl transactionId) ->
      plet
        (scriptSourcesDiscoveryStateCbor phaseControl 8 purposeCount purposePeaks pemptyScriptDiscoveryControl)
        $ \workCbor ->
      plet
        (scriptSourcesDiscoveryStateCbor phaseControl 9 purposeCount purposePeaks selectedPurpose)
        $ \nextWorkCbor ->
      plet (scriptSourcesStateFromPhase transactionId phaseControl 19 workCbor) $ \pre ->
      plet (scriptSourcesStateFromPhase transactionId phaseControl 20 nextWorkCbor) $ \post ->
      plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
      plet
        (pcon $ PScriptPurposeScanWitness (pdata 0) (pdata 0) (pdata scriptHash) (pdata subject) (pdata siblings))
        $ \auxiliary ->
      plet (pcon $ PValidationOneStepEvidenceV1 (pdata witness) (pdata auxiliary)) $ \evidence ->
        pand'List
          [ purposeCount #== 17
          , pverifyScriptSourcesStageEightPurposeSemanticsV1
              # pre # witness # 0 # 0 # scriptHash # subject # siblings
          , pverifyScriptSourcesOneStepV1 # pre # evidence
          ]

scriptSourcesStageEightFinishTransition :: forall s. Term s PBool
scriptSourcesStageEightFinishTransition =
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 8 0 pnil 0 0 pnil
        pemptyReceivePurposeScanControl pemptyObserverPurposeScanControl
    )
    $ \workCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 11 0 pnil 0 0 pnil
        pemptyReceivePurposeScanControl pemptyObserverPurposeScanControl
    )
    $ \nextWorkCbor ->
  plet
    ( scriptSourcesStageSevenStateCbor
        phaseControl 12 0 pnil 0 0 pnil
        pemptyReceivePurposeScanControl pemptyObserverPurposeScanControl
    )
    $ \wrongWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 108 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 109 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 109 wrongWorkCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageEightFinishSemanticsV1 # pre # witness
      , pverifyScriptSourcesStageEightSemanticsV1
          # pre # witness # pcon PNoAuxiliaryWitness
      , pnot # (pverifyScriptSourcesStageEightFinishSemanticsV1 # pre # wrongWitness)
      , pnot
          # ( pverifyScriptSourcesStageEightSemanticsV1
                # pre # wrongWitness # pcon PNoAuxiliaryWitness
            )
      ]

scriptSourcesStageEightPendingHashDivergence :: forall s. Term s PBool
scriptSourcesStageEightPendingHashDivergence =
  plet (cekHash 0x8a) $ \pendingHash ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata pendingHash) (pdata 0) (pdata pnil)
    )
    $ \pendingDiscovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesDiscoveryStateCbor phaseControl 8 0 pnil pendingDiscovery)
    $ \workCbor ->
  plet
    (scriptSourcesDiscoveryStateCbor phaseControl 11 0 pnil pemptyScriptDiscoveryControl)
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 19 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 20 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pverifyScriptSourcesStageEightFinishSemanticsV1 # pre # witness
      #&& pnot
        # (pverifyScriptSources # pre # witness # pcon PNoAuxiliaryWitness)

scriptSourcesStageEightPurposeTransition :: forall s. Term s PBool
scriptSourcesStageEightPurposeTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x51)) $ \scriptHash ->
  plet (phexByteStr "0102") $ \subject ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # subject) $ \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 0) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata subject)
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \selectedDiscovery ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 0) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0103")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \wrongDiscovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoveryStateCbor
        phaseControl 8 1 purposePeaks pemptyScriptDiscoveryControl
    )
    $ \workCbor ->
  plet
    ( scriptSourcesDiscoveryStateCbor
        phaseControl 9 1 purposePeaks selectedDiscovery
    )
    $ \nextWorkCbor ->
  plet
    ( scriptSourcesDiscoveryStateCbor
        phaseControl 9 1 purposePeaks wrongDiscovery
    )
    $ \wrongWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 110 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 111 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 111 wrongWorkCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
  plet
    ( pcon $ PScriptPurposeScanWitness
        (pdata 0) (pdata 0) (pdata scriptHash) (pdata subject) (pdata pnil)
    )
    $ \auxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageEightPurposeSemanticsV1
          # pre # witness # 0 # 0 # scriptHash # subject # pnil
      , pverifyScriptSourcesStageEightSemanticsV1 # pre # witness # auxiliary
      , pnot
          # ( pverifyScriptSourcesStageEightPurposeSemanticsV1
                # pre # wrongWitness # 0 # 0 # scriptHash # subject # pnil
            )
      , pnot
          # ( pverifyScriptSourcesStageEightSemanticsV1
                # pre # wrongWitness # auxiliary
            )
      ]

scriptSourcesStageNineMissingTransition :: forall s. Term s PBool
scriptSourcesStageNineMissingTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x61)) $ \scriptHash ->
  plet (ScriptProof.ppurposeLeafHash # 3 # 0 # scriptHash # scriptHash) $ \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 0) (pdata 0) (pdata 3) (pdata 0)
        (pdata scriptHash) (pdata scriptHash)
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \discovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesDiscoveryStateCbor phaseControl 9 1 purposePeaks discovery)
    $ \workCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 112 workCbor) $ \pre ->
  plet
    (inputSetsExactRejection pre $ pconstant "E_MISSING_REQUIRED_WITNESS")
    $ \post ->
  plet
    (inputSetsExactRejection pre $ pconstant "E_INVALID_FIELD_TYPE")
    $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
  plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageNineMissingSemanticsV1 # pre # witness
      , pverifyScriptSourcesStageNineSemanticsV1 # pre # witness # auxiliary
      , pnot # (pverifyScriptSourcesStageNineMissingSemanticsV1 # pre # wrongWitness)
      , pnot
          # ( pverifyScriptSourcesStageNineSemanticsV1
                # pre # wrongWitness # auxiliary
            )
      ]

scriptSourcesStageNineMismatchTransition :: forall s. Term s PBool
scriptSourcesStageNineMismatchTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x71)) $ \selectedHash ->
  plet (preplicateBS # 28 # (pintegerToByte # 0x72)) $ \sourceHash ->
  plet (phexByteStr "00") $ \sourceKey ->
  plet (cekHash 0x73) $ \itemCommitment ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # sourceKey # 3 # sourceHash # 10 # itemCommitment
    )
    $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # selectedHash # phexByteStr "0102") $
    \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 0) (pdata 0) (pdata 0) (pdata 0)
        (pdata selectedHash) (pdata $ phexByteStr "0102")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \discovery ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
        (pdata selectedHash) (pdata $ phexByteStr "0102")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \nextDiscovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoverySourceStateCbor
        phaseControl 9 1 sourcePeaks 1 purposePeaks discovery
    )
    $ \workCbor ->
  plet
    ( scriptSourcesDiscoverySourceStateCbor
        phaseControl 9 1 sourcePeaks 1 purposePeaks nextDiscovery
    )
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 113 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 114 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 114 workCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
  plet
    ( pcon $ PScriptSourceScanWitness
        (pdata 0) (pdata 0) (pdata sourceKey) (pdata 3)
        (pdata sourceHash) (pdata 10) (pdata itemCommitment) (pdata pnil)
    )
    $ \auxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageNineMismatchSemanticsV1
          # pre # witness # 0 # 0 # sourceKey # 3 # sourceHash # 10
          # itemCommitment # pnil
      , pverifyScriptSourcesStageNineSemanticsV1 # pre # witness # auxiliary
      , pnot
          # ( pverifyScriptSourcesStageNineEffectfulMatchSemanticsV1
                # pre # witness # 0 # 0 # sourceKey # 3 # sourceHash # 10
                # itemCommitment # pnil
            )
      , pnot # (pverifyScriptSourcesStageNineMissingSemanticsV1 # pre # witness)
      , pnot
          # ( pverifyScriptSourcesStageNineMismatchSemanticsV1
                # pre # wrongWitness # 0 # 0 # sourceKey # 3 # sourceHash # 10
                # itemCommitment # pnil
            )
      , pnot
          # ( pverifyScriptSourcesStageNineSemanticsV1
                # pre # wrongWitness # auxiliary
            )
      ]

scriptSourcesStageNineNativeMatchTransition :: forall s. Term s PBool
scriptSourcesStageNineNativeMatchTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x74)) $ \scriptHash ->
  plet (phexByteStr "00") $ \sourceKey ->
  plet (cekHash 0x75) $ \itemCommitment ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # sourceKey # 0 # scriptHash # 10 # itemCommitment
    )
    $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # phexByteStr "0102") $
    \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet
    (ScriptProof.pexecutionLeafHash # 0 # purposeLeaf # sourceLeaf # pconstant "")
    $ \executionLeaf ->
  plet (cekSinglePeak executionLeaf) $ \executionPeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 0) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \discovery ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 1) (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 1) (pdata 0) (pdata $ pconstant "")
        (pdata 1) (pdata executionPeaks)
    )
    $ \nextDiscovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoverySourceStateCbor
        phaseControl 9 1 sourcePeaks 1 purposePeaks discovery
    )
    $ \workCbor ->
  plet
    ( scriptSourcesDiscoverySourceStateCbor
        phaseControl 8 1 sourcePeaks 1 purposePeaks nextDiscovery
    )
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 115 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 116 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 116 workCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
  plet
    ( pcon $ PScriptSourceScanWitness
        (pdata 0) (pdata 0) (pdata sourceKey) (pdata 0)
        (pdata scriptHash) (pdata 10) (pdata itemCommitment) (pdata pnil)
    )
    $ \auxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageNineNativeMatchSemanticsV1
          # pre # witness # 0 # 0 # sourceKey # scriptHash # 10
          # itemCommitment # pnil
      , pverifyScriptSourcesStageNineSemanticsV1 # pre # witness # auxiliary
      , pnot
          # ( pverifyScriptSourcesStageNineMismatchSemanticsV1
                # pre # witness # 0 # 0 # sourceKey # 0 # scriptHash # 10
                # itemCommitment # pnil
            )
      , pnot
          # ( pverifyScriptSourcesStageNineEffectfulMatchSemanticsV1
                # pre # witness # 0 # 0 # sourceKey # 0 # scriptHash # 10
                # itemCommitment # pnil
            )
      , pnot
          # ( pverifyScriptSourcesStageNineNativeMatchSemanticsV1
                # pre # wrongWitness # 0 # 0 # sourceKey # scriptHash # 10
                # itemCommitment # pnil
            )
      , pnot
          # ( pverifyScriptSourcesStageNineSemanticsV1
                # pre # wrongWitness # auxiliary
            )
      ]

scriptSourcesStageNineEffectfulMatchTransition :: forall s. Term s PBool
scriptSourcesStageNineEffectfulMatchTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x76)) $ \scriptHash ->
  plet (phexByteStr "00") $ \sourceKey ->
  plet (cekHash 0x77) $ \itemCommitment ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # sourceKey # 3 # scriptHash # 10 # itemCommitment
    )
    $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # phexByteStr "0102") $
    \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 0) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \discovery ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata 0) (pdata 3) (pdata sourceLeaf)
        (pdata 1) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \nextDiscovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoverySourceStateCbor
        phaseControl 9 1 sourcePeaks 1 purposePeaks discovery
    )
    $ \workCbor ->
  plet
    ( scriptSourcesDiscoverySourceStateCbor
        phaseControl 10 1 sourcePeaks 1 purposePeaks nextDiscovery
    )
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 117 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 118 nextWorkCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 118 workCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
    $ \wrongWitness ->
  plet
    ( pcon $ PScriptSourceScanWitness
        (pdata 0) (pdata 0) (pdata sourceKey) (pdata 3)
        (pdata scriptHash) (pdata 10) (pdata itemCommitment) (pdata pnil)
    )
    $ \auxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageNineEffectfulMatchSemanticsV1
          # pre # witness # 0 # 0 # sourceKey # 3 # scriptHash # 10
          # itemCommitment # pnil
      , pverifyScriptSourcesStageNineSemanticsV1 # pre # witness # auxiliary
      , pnot
          # ( pverifyScriptSourcesStageNineMismatchSemanticsV1
                # pre # witness # 0 # 0 # sourceKey # 3 # scriptHash # 10
                # itemCommitment # pnil
            )
      , pnot
          # ( pverifyScriptSourcesStageNineNativeMatchSemanticsV1
                # pre # witness # 0 # 0 # sourceKey # scriptHash # 10
                # itemCommitment # pnil
            )
      , pnot
          # ( pverifyScriptSourcesStageNineEffectfulMatchSemanticsV1
                # pre # wrongWitness # 0 # 0 # sourceKey # 3 # scriptHash # 10
                # itemCommitment # pnil
            )
      , pnot
          # ( pverifyScriptSourcesStageNineSemanticsV1
                # pre # wrongWitness # auxiliary
            )
      ]

scriptSourcesStageTenRedeemer :: forall s. Term s PByteString
scriptSourcesStageTenRedeemer = phexByteStr "8400004100820a14"

scriptSourcesStageTenChunkProof :: forall s. Term s Bounded.PChunkProofV1
scriptSourcesStageTenChunkProof =
  plet scriptSourcesStageTenRedeemer $ \redeemer ->
  plet (Bounded.phashChunk # 8 # 0 # 0 # redeemer) $ \chunkLeaf ->
  pcon $ Bounded.PChunkProofV1
    (pdata 1) (pdata 8) (pdata 0) (pdata $ plengthBS # redeemer) (pdata 0)
    (pdata redeemer) (pdata $ cekSinglePeak chunkLeaf) (pdata pnil)

scriptSourcesStageTenItemWitness :: forall s.
  RedeemerItemProof.PRedeemerItemProofActionV1 s ->
  Term s RedeemerItemProof.PRedeemerItemProofWitnessV1
scriptSourcesStageTenItemWitness action =
  pcon $ RedeemerItemProof.PRedeemerItemProofWitnessV1
    (pdata $ pcon action)
    (pdata $ pcon $ PDJust $ pdata scriptSourcesStageTenChunkProof)
    (pdata $ pcon PDNothing)

scriptSourcesStageTenTailControl :: forall s.
  Term s PByteString -> Term s RedeemerItemProof.PRedeemerItemProofControlV1
scriptSourcesStageTenTailControl itemCommitment =
  pcon $ RedeemerItemProof.PRedeemerItemProofControlV1
    (pdata 1) (pdata 0) (pdata 1) (pdata 0) (pdata 1) (pdata 8)
    (pdata itemCommitment) (pdata $ -1) (pdata $ -1) (pdata 0) (pdata 0)
    (pdata 4) (pdata 1) (pdata $ -1) (pdata $ -1) (pdata $ pcon PDNothing)

scriptSourcesStageTenMissingTransition :: forall s. Term s PBool
scriptSourcesStageTenMissingTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x78)) $ \scriptHash ->
  plet (cekHash 0x79) $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # phexByteStr "0102") $
    \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata 0) (pdata 3) (pdata sourceLeaf)
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \discovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 10 1 sourcePeaks 0 pnil 1 purposePeaks discovery
    )
    $ \workCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 119 workCbor) $ \pre ->
  plet
    (inputSetsExactRejection pre $ pconstant "E_MISSING_REQUIRED_WITNESS")
    $ \post ->
  plet (inputSetsExactRejection pre $ pconstant "E_INVALID_FIELD_TYPE") $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost)) $
    \wrongWitness ->
  plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageTenMissingSemanticsV1 # pre # witness
      , pverifyScriptSourcesStageTenSemanticsV1 # pre # witness # auxiliary
      , pnot # (pverifyScriptSourcesStageTenMatchSemanticsV1 # pre # witness # auxiliary)
      , pnot # (pverifyScriptSourcesStageTenMismatchSemanticsV1 # pre # witness # auxiliary)
      , pnot # (pverifyScriptSourcesStageTenMissingSemanticsV1 # pre # wrongWitness)
      , pnot # (pverifyScriptSourcesStageTenSemanticsV1 # pre # wrongWitness # auxiliary)
      ]

scriptSourcesStageTenPendingHashDivergence :: forall s. Term s PBool
scriptSourcesStageTenPendingHashDivergence =
  plet (preplicateBS # 28 # (pintegerToByte # 0x78)) $ \scriptHash ->
  plet (cekHash 0x79) $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # phexByteStr "0102") $
    \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata 0) (pdata 3) (pdata sourceLeaf)
        (pdata 0) (pdata 0) (pdata $ cekHash 0x8b) (pdata 0) (pdata pnil)
    )
    $ \discovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 10 1 sourcePeaks 0 pnil 1 purposePeaks discovery
    )
    $ \workCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 31 workCbor) $ \pre ->
  plet (inputSetsExactRejection pre $ pconstant "E_MISSING_REQUIRED_WITNESS") $
    \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pverifyScriptSourcesStageTenMissingSemanticsV1 # pre # witness
      #&& pnot
        # (pverifyScriptSources # pre # witness # pcon PNoAuxiliaryWitness)

scriptSourcesStageTenMatchTransition :: forall s. Integer -> Term s PBool
scriptSourcesStageTenMatchTransition transitionIndex = case transitionIndex of
  0 -> scriptSourcesStageTenBeginTransition
  1 -> scriptSourcesStageTenHeaderTransition
  2 -> scriptSourcesStageTenTerminalMatchTransition
  _ -> scriptSourcesStageTenMismatchTransition

scriptSourcesStageTenBeginTransition :: forall s. Term s PBool
scriptSourcesStageTenBeginTransition =
  plet scriptSourcesStageTenRedeemer $ \redeemer ->
  plet (Bounded.pfromBytes # 8 # 0 # redeemer) $ \itemCommitment ->
  plet
    ( RedeemerItemProof.pinitialControlV1
        # RedeemerItemProof.pmodeDescriptor # 0 # 1 # (plengthBS # redeemer)
        # itemCommitment # (-1) # (-1)
    )
    $ \itemControl ->
  plet (preplicateBS # 28 # (pintegerToByte # 0x7a)) $ \scriptHash ->
  plet (cekHash 0x7b) $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # phexByteStr "0102") $
    \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet (ScriptProof.predeemerItemLeafHash # 0 # itemCommitment) $ \redeemerLeaf ->
  plet (cekSinglePeak redeemerLeaf) $ \redeemerPeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata 0) (pdata 3) (pdata sourceLeaf)
        (pdata 1) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \selectedSource ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata 0) (pdata 3) (pdata sourceLeaf)
        (pdata 1) (pdata 0) (pdata $ RedeemerItemProof.phashControlV1 # itemControl)
        (pdata 0) (pdata pnil)
    )
    $ \selectedItem ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 10 1 sourcePeaks 1 redeemerPeaks 1 purposePeaks selectedSource
    )
    $ \beginCbor ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 10 1 sourcePeaks 1 redeemerPeaks 1 purposePeaks selectedItem
    )
    $ \itemCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 120 beginCbor) $ \beginPre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 121 itemCbor) $ \itemState ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata beginCbor) (pdata itemState)) $
    \beginTransition ->
  plet
    ( pcon $ PRedeemerScanBeginWitness
        (pdata 0) (pdata 1) (pdata $ plengthBS # redeemer)
        (pdata itemCommitment) (pdata pnil)
    )
    $ \auxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # beginPre # beginTransition
      , pverifyPreparedScriptSourcesStageTenBeginTransitionV1
          # beginPre # beginTransition # 0 # 1 # (plengthBS # redeemer)
          # itemCommitment # pnil
      , pverifyScriptSourcesStageTenSemanticsV1
          # beginPre # beginTransition # auxiliary
      , pverifyScriptSourcesStageTenMismatchSemanticsV1
          # beginPre # beginTransition # auxiliary
      , pnot
          # ( pverifyScriptSourcesStageTenMatchSemanticsV1
                # beginPre # beginTransition # auxiliary
            )
      ]

scriptSourcesStageTenHeaderTransition :: forall s. Term s PBool
scriptSourcesStageTenHeaderTransition =
  plet scriptSourcesStageTenRedeemer $ \redeemer ->
  plet (Bounded.pfromBytes # 8 # 0 # redeemer) $ \itemCommitment ->
  plet
    ( RedeemerItemProof.pinitialControlV1
        # RedeemerItemProof.pmodeDescriptor # 0 # 1 # (plengthBS # redeemer)
        # itemCommitment # (-1) # (-1)
    )
    $ \itemControl ->
  plet
    (scriptSourcesStageTenItemWitness RedeemerItemProof.PRedeemerItemOpenHeader)
    $ \headerWitness ->
  plet (scriptSourcesStageTenTailControl itemCommitment) $ \tailControl ->
  plet (preplicateBS # 28 # (pintegerToByte # 0x7a)) $ \scriptHash ->
  plet (cekHash 0x7b) $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # phexByteStr "0102") $ \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet (ScriptProof.predeemerItemLeafHash # 0 # itemCommitment) $ \redeemerLeaf ->
  plet (cekSinglePeak redeemerLeaf) $ \redeemerPeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata 0) (pdata 3) (pdata sourceLeaf)
        (pdata 1) (pdata 0) (pdata $ RedeemerItemProof.phashControlV1 # itemControl)
        (pdata 0) (pdata pnil)
    )
    $ \selectedItem ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata 0) (pdata 3) (pdata sourceLeaf)
        (pdata 1) (pdata 0) (pdata $ RedeemerItemProof.phashControlV1 # tailControl)
        (pdata 0) (pdata pnil)
    )
    $ \selectedTail ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesDiscoveryFullStateCbor phaseControl 10 1 sourcePeaks 1 redeemerPeaks 1 purposePeaks selectedItem)
    $ \itemCbor ->
  plet
    (scriptSourcesDiscoveryFullStateCbor phaseControl 10 1 sourcePeaks 1 redeemerPeaks 1 purposePeaks selectedTail)
    $ \tailCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 121 itemCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 122 tailCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata itemCbor) (pdata post)) $ \transition ->
  plet
    ( pcon $ PRedeemerItemStepWitness
        (pdata $ pcon PDNothing) (pdata itemControl) (pdata headerWitness)
    )
    $ \auxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # pre # transition
      , pverifyPreparedScriptSourcesStageTenAdvanceTransitionV1
          # pre # transition # itemControl # headerWitness
      , pverifyScriptSourcesStageTenSemanticsV1 # pre # transition # auxiliary
      , pverifyScriptSourcesStageTenMismatchSemanticsV1
          # pre # transition # auxiliary
      , pnot
          # (pverifyScriptSourcesStageTenMatchSemanticsV1 # pre # transition # auxiliary)
      ]

scriptSourcesStageTenTerminalMatchTransition :: forall s. Term s PBool
scriptSourcesStageTenTerminalMatchTransition =
  plet scriptSourcesStageTenRedeemer $ \redeemer ->
  plet (Bounded.pfromBytes # 8 # 0 # redeemer) $ \itemCommitment ->
  plet (scriptSourcesStageTenTailControl itemCommitment) $ \tailControl ->
  plet (scriptSourcesStageTenItemWitness RedeemerItemProof.PRedeemerItemOpenTail) $ \tailWitness ->
  plet (preplicateBS # 28 # (pintegerToByte # 0x7a)) $ \scriptHash ->
  plet (cekHash 0x7b) $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # phexByteStr "0102") $ \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet (ScriptProof.predeemerItemLeafHash # 0 # itemCommitment) $ \redeemerLeaf ->
  plet (cekSinglePeak redeemerLeaf) $ \redeemerPeaks ->
  plet (ScriptProof.pexecutionLeafHash # 3 # purposeLeaf # sourceLeaf # redeemerLeaf) $ \executionLeaf ->
  plet (cekSinglePeak executionLeaf) $ \executionPeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata 0) (pdata 3) (pdata sourceLeaf)
        (pdata 1) (pdata 0) (pdata $ RedeemerItemProof.phashControlV1 # tailControl)
        (pdata 0) (pdata pnil)
    )
    $ \selectedTail ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 1) (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 1) (pdata 1) (pdata $ pconstant "")
        (pdata 1) (pdata executionPeaks)
    )
    $ \completed ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesDiscoveryFullStateCbor phaseControl 10 1 sourcePeaks 1 redeemerPeaks 1 purposePeaks selectedTail)
    $ \workCbor ->
  plet
    (scriptSourcesDiscoveryFullStateCbor phaseControl 8 1 sourcePeaks 1 redeemerPeaks 1 purposePeaks completed)
    $ \doneCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 122 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 123 doneCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
  plet
    ( pcon $ PRedeemerItemStepWitness
        (pdata $ pcon PDNothing) (pdata tailControl) (pdata tailWitness)
    )
    $ \auxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # pre # transition
      , pverifyPreparedScriptSourcesStageTenMatchTransitionV1
          # pre # transition # tailControl # tailWitness
      , pverifyScriptSourcesStageTenSemanticsV1 # pre # transition # auxiliary
      , pverifyScriptSourcesStageTenMatchSemanticsV1 # pre # transition # auxiliary
      , pnot
          # ( pverifyScriptSourcesStageTenMismatchSemanticsV1
                # pre # transition # auxiliary
            )
      ]

scriptSourcesStageTenMismatchTransition :: forall s. Term s PBool
scriptSourcesStageTenMismatchTransition =
  plet scriptSourcesStageTenRedeemer $ \redeemer ->
  plet (Bounded.pfromBytes # 8 # 0 # redeemer) $ \itemCommitment ->
  plet (scriptSourcesStageTenTailControl itemCommitment) $ \tailControl ->
  plet
    (scriptSourcesStageTenItemWitness RedeemerItemProof.PRedeemerItemOpenTail)
    $ \tailWitness ->
  plet (preplicateBS # 28 # (pintegerToByte # 0x7c)) $ \scriptHash ->
  plet (cekHash 0x7d) $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 1 # scriptHash # phexByteStr "0102") $
    \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet (ScriptProof.predeemerItemLeafHash # 0 # itemCommitment) $ \redeemerLeaf ->
  plet (cekSinglePeak redeemerLeaf) $ \redeemerPeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 1)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata 0) (pdata 3) (pdata sourceLeaf)
        (pdata 1) (pdata 0) (pdata $ RedeemerItemProof.phashControlV1 # tailControl)
        (pdata 0) (pdata pnil)
    )
    $ \discovery ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 1) (pdata 1) (pdata 0) (pdata 1)
        (pdata scriptHash) (pdata $ phexByteStr "0102")
        (pdata 0) (pdata 3) (pdata sourceLeaf)
        (pdata 1) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \nextDiscovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 10 1 sourcePeaks 1 redeemerPeaks 1 purposePeaks discovery
    )
    $ \workCbor ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 10 1 sourcePeaks 1 redeemerPeaks 1 purposePeaks nextDiscovery
    )
    $ \nextCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 124 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 125 nextCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
  pand'List
      [ pstructuralTransitionIsValid # pre # transition
      , pverifyPreparedScriptSourcesStageTenTerminalMismatchTransitionV1
          # pre # transition # tailControl # tailWitness
      ]

scriptSourcesStageElevenAuditTransitions :: forall s. Term s PBool
scriptSourcesStageElevenAuditTransitions =
  plet (preplicateBS # 28 # (pintegerToByte # 0x7e)) $ \scriptHash ->
  plet (phexByteStr "00") $ \sourceKey ->
  plet (cekHash 0x7f) $ \itemCommitment ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # sourceKey # 3 # scriptHash # 10 # itemCommitment
    )
    $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # phexByteStr "0102") $
    \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet (cekSinglePeak sourceLeaf) $ \executionPeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 1) (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 1) (pdata 0) (pdata $ pconstant "")
        (pdata 1) (pdata executionPeaks)
    )
    $ \discovery ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 1) (pdata 1) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 1) (pdata 0) (pdata $ pconstant "")
        (pdata 1) (pdata executionPeaks)
    )
    $ \audited ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 11 1 sourcePeaks 0 pnil 1 purposePeaks discovery
    )
    $ \workCbor ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 11 1 sourcePeaks 0 pnil 1 purposePeaks audited
    )
    $ \auditedCbor ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 12 1 sourcePeaks 0 pnil 1 purposePeaks audited
    )
    $ \finishedCbor ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 11 1 sourcePeaks 0 pnil 1 purposePeaks audited
    )
    $ \wrongFinishedCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 126 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 127 auditedCbor) $ \auditedState ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 128 finishedCbor) $ \finishedState ->
  plet
    (scriptSourcesStateFromPhase transactionId phaseControl 128 wrongFinishedCbor)
    $ \wrongFinishedState ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata auditedState)) $
    \sourceWitness ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata auditedCbor) (pdata finishedState)) $
    \finishWitness ->
  plet
    (pcon $ PValidationOneStepWitnessV1 (pdata auditedCbor) (pdata wrongFinishedState))
    $ \wrongFinishWitness ->
  plet
    ( pcon $ PScriptSourceScanWitness
        (pdata 0) (pdata 0) (pdata sourceKey) (pdata 3)
        (pdata scriptHash) (pdata 10) (pdata itemCommitment) (pdata pnil)
    )
    $ \sourceAuxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # pre # sourceWitness
      , pverifyScriptSourcesStageElevenSourceSemanticsV1
          # pre # sourceWitness # 0 # 0 # sourceKey # 3 # scriptHash # 10
          # itemCommitment # pnil
      , pverifyScriptSourcesStageElevenSemanticsV1
          # pre # sourceWitness # sourceAuxiliary
      , pstructuralTransitionIsValid # auditedState # finishWitness
      , pverifyScriptSourcesStageElevenFinishSemanticsV1
          # auditedState # finishWitness
      , pverifyScriptSourcesStageElevenSemanticsV1
          # auditedState # finishWitness # pcon PNoAuxiliaryWitness
      , pnot
          # ( pverifyScriptSourcesStageElevenFinishSemanticsV1
                # auditedState # wrongFinishWitness
            )
      , pnot
          # ( pverifyScriptSourcesStageElevenSemanticsV1
                # auditedState # wrongFinishWitness # pcon PNoAuxiliaryWitness
            )
      ]

scriptSourcesStageElevenUnusedInlineTransition :: forall s. Term s PBool
scriptSourcesStageElevenUnusedInlineTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x80)) $ \scriptHash ->
  plet (phexByteStr "00") $ \sourceKey ->
  plet (cekHash 0x81) $ \itemCommitment ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # sourceKey # 3 # scriptHash # 10 # itemCommitment
    )
    $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \discovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 11 1 sourcePeaks 0 pnil 0 pnil discovery
    )
    $ \workCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 129 workCbor) $ \pre ->
  plet (inputSetsExactRejection pre $ pconstant "E_INVALID_FIELD_TYPE") $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    ( pcon $ PScriptSourceScanWitness
        (pdata 0) (pdata 0) (pdata sourceKey) (pdata 3)
        (pdata scriptHash) (pdata 10) (pdata itemCommitment) (pdata pnil)
    )
    $ \auxiliary ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesStageElevenSourceSemanticsV1
          # pre # witness # 0 # 0 # sourceKey # 3 # scriptHash # 10
          # itemCommitment # pnil
      , pverifyScriptSourcesStageElevenSemanticsV1 # pre # witness # auxiliary
      ]

scriptSourcesStageTwelveBeginTransition :: forall s. Term s PBool
scriptSourcesStageTwelveBeginTransition =
  plet scriptSourcesStageTenRedeemer $ \redeemer ->
  plet (Bounded.pfromBytes # 8 # 0 # redeemer) $ \itemCommitment ->
  plet
    ( RedeemerItemProof.pinitialControlV1
        # RedeemerItemProof.pmodeDescriptor # 0 # 1 # (plengthBS # redeemer)
        # itemCommitment # (-1) # (-1)
    )
    $ \itemControl ->
  plet (ScriptProof.predeemerItemLeafHash # 0 # itemCommitment) $ \redeemerLeaf ->
  plet (cekSinglePeak redeemerLeaf) $ \redeemerPeaks ->
  plet (cekHash 0x82) $ \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet (cekSinglePeak redeemerLeaf) $ \executionPeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 1) (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 1) (pdata $ pconstant "")
        (pdata 1) (pdata executionPeaks)
    )
    $ \discovery ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 1) (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 1) (pdata $ RedeemerItemProof.phashControlV1 # itemControl)
        (pdata 1) (pdata executionPeaks)
    )
    $ \selected ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 12 0 pnil 1 redeemerPeaks 1 purposePeaks discovery
    )
    $ \workCbor ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 12 0 pnil 1 redeemerPeaks 1 purposePeaks selected
    )
    $ \nextCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 130 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 131 nextCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyPreparedScriptSourcesStageTwelveBeginTransitionV1
          # pre # witness # 0 # 1 # (plengthBS # redeemer)
          # itemCommitment # pnil
      ]

scriptSourcesStageTwelveFamilyGuard :: forall s. Term s PBool
scriptSourcesStageTwelveFamilyGuard =
  plet
    ( RedeemerItemProof.pinitialControlV1
        # RedeemerItemProof.pmodeDescriptor # 0 # 1 # 8 # (cekHash 0x83) # (-1) # (-1)
    )
    $ \itemControl ->
  plet (scriptSourcesStageTenItemWitness RedeemerItemProof.PRedeemerItemOpenHeader) $
    \itemWitness ->
  plet
    ( pcon $ PRedeemerScanBeginWitness
        (pdata 0) (pdata 1) (pdata 8) (pdata $ cekHash 0x83) (pdata pnil)
    )
    $ \beginAuxiliary ->
  plet
    ( pcon $ PRedeemerItemStepWitness
        (pdata $ pcon PDNothing) (pdata itemControl) (pdata itemWitness)
    )
    $ \itemAuxiliary ->
  plet
    ( pcon $ PRedeemerItemStepWitness
        (pdata $ pcon $ PDJust $ pdata pinitialCekRedeemerContextControlV1)
        (pdata itemControl) (pdata itemWitness)
    )
    $ \wrongFamily ->
    pand'List
      [ pscriptSourcesStageTwelveRedeemerAuxiliaryIsFamily # beginAuxiliary
      , pscriptSourcesStageTwelveRedeemerAuxiliaryIsFamily # itemAuxiliary
      , pnot # (pscriptSourcesStageTwelveRedeemerAuxiliaryIsFamily # wrongFamily)
      , pnot
          # ( pscriptSourcesStageTwelveRedeemerAuxiliaryIsFamily
                # pcon PNoAuxiliaryWitness
            )
      ]

scriptSourcesStageTwelveFinishTransition :: forall s. Bool -> Term s PBool
scriptSourcesStageTwelveFinishTransition accepting =
  plet (cekHash 0x84) $ \sourceLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (cekHash 0x85) $ \purposeLeaf ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 1) (pdata 1) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 1) (pdata sourcePeaks)
    )
    $ \discovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 12 1 sourcePeaks 0 pnil 1 purposePeaks discovery
    )
    $ \workCbor ->
  plet (pscriptSourcesStageTwelveControlFromWitness # workCbor) $ \control ->
  plet (pencodeNativeScriptsWitnessV1 # control # 0 # 0) $ \nativeCbor ->
  plet (pencodeNativeScriptsWitnessV1 # control # 1 # 0) $ \wrongNativeCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 132 workCbor) $ \pre ->
  pmatch phaseControl $ \phaseFields ->
  plet
    ( cekBoundStateFixture
        transactionId
        ( NativeCompact.pnativeTxProofCommitmentV1
            # pfromData (pphaseANative'compactCbor phaseFields)
            # pfromData (pphaseANative'witnessSetCompactCbor phaseFields)
            # pfromData (pphaseANative'fieldPreimageLengthsCbor phaseFields)
        )
        (phashValidationContext # pfromData (pphaseANative'contextCbor phaseFields))
        (pcon PNativeScripts) 133
        (phashWorkWitness # pcon PNativeScripts # 133 # nativeCbor)
        0 0
    )
    $ \post ->
  plet
    ( cekBoundStateFixture
        transactionId
        ( NativeCompact.pnativeTxProofCommitmentV1
            # pfromData (pphaseANative'compactCbor phaseFields)
            # pfromData (pphaseANative'witnessSetCompactCbor phaseFields)
            # pfromData (pphaseANative'fieldPreimageLengthsCbor phaseFields)
        )
        (phashValidationContext # pfromData (pphaseANative'contextCbor phaseFields))
        (pcon PNativeScripts) 133
        (phashWorkWitness # pcon PNativeScripts # 133 # wrongNativeCbor)
        0 0
    )
    $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost)) $
    \wrongWitness ->
    if accepting
      then
        pverifyScriptSourcesStageTwelveFinishSemanticsV1 # pre # witness
          #&& ( pverifyScriptSourcesStageTwelveSemanticsV1
                  # pre # witness # pcon PNoAuxiliaryWitness
              )
      else
        pnot # (pverifyScriptSourcesStageTwelveFinishSemanticsV1 # pre # wrongWitness)
          #&& pnot
            # ( pverifyScriptSourcesStageTwelveSemanticsV1
                  # pre # wrongWitness # pcon PNoAuxiliaryWitness
              )

scriptSourcesStageTwelvePendingHashDivergence :: forall s. Term s PBool
scriptSourcesStageTwelvePendingHashDivergence =
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ cekHash 0x8c) (pdata 0) (pdata pnil)
    )
    $ \discovery ->
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    ( scriptSourcesDiscoveryFullStateCbor
        phaseControl 12 0 pnil 0 pnil 0 pnil discovery
    )
    $ \workCbor ->
  plet (pscriptSourcesStageTwelveControlFromWitness # workCbor) $ \control ->
  plet (pencodeNativeScriptsWitnessV1 # control # 0 # 0) $ \nativeCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 32 workCbor) $ \pre ->
  pmatch phaseControl $ \phaseFields ->
  plet
    ( cekBoundStateFixture
        transactionId
        ( NativeCompact.pnativeTxProofCommitmentV1
            # pfromData (pphaseANative'compactCbor phaseFields)
            # pfromData (pphaseANative'witnessSetCompactCbor phaseFields)
            # pfromData (pphaseANative'fieldPreimageLengthsCbor phaseFields)
        )
        (phashValidationContext # pfromData (pphaseANative'contextCbor phaseFields))
        (pcon PNativeScripts) 33
        (phashWorkWitness # pcon PNativeScripts # 33 # nativeCbor)
        0 0
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pverifyScriptSourcesStageTwelveFinishSemanticsV1 # pre # witness
      #&& pnot
        # (pverifyScriptSources # pre # witness # pcon PNoAuxiliaryWitness)

scriptSourcesAggregateRouting :: forall s. Term s PBool
scriptSourcesAggregateRouting =
  pand'List
    [ pscriptSourcesStageBranchV1 # pconstant True # 0 #== 1
    , pscriptSourcesStageBranchV1 # pconstant True # 5 #== 6
    , pscriptSourcesStageBranchV1 # pconstant True # 12 #== 13
    , pscriptSourcesStageBranchV1 # pconstant False # 4 #== 5
    , pscriptSourcesStageBranchV1 # pconstant False # 5 #== 0
    , pscriptSourcesStageBranchV1 # pconstant False # 6 #== 7
    , pscriptSourcesStageBranchV1 # pconstant True # (-1) #== 0
    , pscriptSourcesStageBranchV1 # pconstant True # 13 #== 0
    ]

scriptSourcesAggregateControlPredicates :: forall s. Term s PBool
scriptSourcesAggregateControlPredicates =
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 0) (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
        (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata $ -1) (pdata 2) (pdata $ pconstant "")
        (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)
    )
    $ \invalidDiscovery ->
  plet
    ( pcon $ PObserverPurposeScanControlV1
        (pdata 1) (pdata 0) (pdata $ pconstant "")
    )
    $ \activeObserver ->
    pand'List
      [ pscriptDiscoveryControlIsWellFormed # pemptyScriptDiscoveryControl
      , pnot # (pscriptDiscoveryControlIsWellFormed # invalidDiscovery)
      , pscriptSourcesObserverScanIsWellFormed
          # 8 # pemptyObserverPurposeScanControl
      , pscriptSourcesObserverScanIsWellFormed # 7 # activeObserver
      , pnot # (pscriptSourcesObserverScanIsWellFormed # 8 # activeObserver)
      ]

scriptSourcesAggregateEnvelope :: forall s. Term s PBool
scriptSourcesAggregateEnvelope =
  pmatch
    ( phaseANativeControlFixture
        NativeField.pemptyFieldCommitment (-1) (-1) 0 0 0 0 0
          (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageZeroCborFromPhase phaseControl 0) $ \workCbor ->
  plet (scriptSourcesCborFromPhaseAtStage phaseControl 1 0 pnil 0) $ \nextCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 133 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 134 nextCbor) $ \post ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 133 nextCbor) $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost)) $ \wrongWitness ->
  plet (pscriptSourcesControlFromWitness # workCbor) $ \control ->
  pmatch control $ \c ->
    pand'List
      [ pfromData (pscriptSources'stage c) #== 0
      , pscriptSourcesControlIsBound # pre # witness # control
      , pverifyPreparedScriptSourcesOneStepV1
          # pre # witness # pconstant True
      , pnot
          # ( pverifyPreparedScriptSourcesOneStepV1
                # pre # witness # pconstant False
            )
      , pnot
          # ( pverifyPreparedScriptSourcesOneStepV1
                # pre # wrongWitness # pconstant True
            )
      ]

nativeScriptsTerminalTransition :: forall s. Term s PBool
nativeScriptsTerminalTransition =
  pmatch (cekObserverProofSource NativeField.pemptyFieldCommitment) $ \(PPair control transactionId) ->
  pmatch control $ \c ->
  plet (pencodeNativeScriptsControlV1 # control) $ \workCbor ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pnativeControl'compactCbor c)
        # pfromData (pnativeControl'witnessSetCompactCbor c)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor c)
    )
    $ \transactionCommitment ->
  plet (phashValidationContext # pfromData (pnativeControl'contextCbor c)) $ \contextHash ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PNativeScripts) 134
        (phashWorkWitness # pcon PNativeScripts # 134 # workCbor) 0 0
    )
    $ \pre ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PScriptIntegrity) 135
        ( phashWorkWitness
            # pcon PScriptIntegrity # 135
            # (pencodeScriptIntegrityWitnessV1 # control)
        )
        0 0
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata witness) (pdata $ pcon PNoAuxiliaryWitness)
    )
    $ \evidence ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pnativeScriptsControlIsBound # pre # witness # control
      , pverifyNativeScriptsTerminalSemanticsV1 # pre # witness
      , pverifyNativeScripts # pre # witness # pcon PNoAuxiliaryWitness
      , pverifyNativeScriptsOneStepV1 # pre # evidence
      ]

nativeScriptsLanguageBitmap :: forall s. Term s PBool
nativeScriptsLanguageBitmap =
  pand'List
    [ pnativeScriptsNextLanguageBitmap # 0 # 3 #== 1
    , pnativeScriptsNextLanguageBitmap # 1 # 3 #== 1
    , pnativeScriptsNextLanguageBitmap # 2 # 3 #== 3
    , pnativeScriptsNextLanguageBitmap # 3 # 3 #== 3
    , pnativeScriptsNextLanguageBitmap # 0 # 128 #== 2
    , pnativeScriptsNextLanguageBitmap # 1 # 128 #== 3
    , pnativeScriptsNextLanguageBitmap # 2 # 128 #== 2
    , pnativeScriptsNextLanguageBitmap # 3 # 128 #== 3
    ]

scriptIntegrityAuthenticationTransition :: forall s. Term s PBool
scriptIntegrityAuthenticationTransition =
  pmatch (cekObserverProofSource NativeField.pemptyFieldCommitment) $ \(PPair control transactionId) ->
  pmatch control $ \c ->
  plet (pencodeScriptIntegrityWitnessV1 # control) $ \workCbor ->
  plet (pencodeScriptIntegrityCompactWitnessV1 # control) $ \nextCbor ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pnativeControl'compactCbor c)
        # pfromData (pnativeControl'witnessSetCompactCbor c)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor c)
    )
    $ \transactionCommitment ->
  plet (phashValidationContext # pfromData (pnativeControl'contextCbor c)) $ \contextHash ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PScriptIntegrity) 135
        (phashWorkWitness # pcon PScriptIntegrity # 135 # workCbor) 0 0
    )
    $ \pre ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PScriptIntegrity) 136
        (phashWorkWitness # pcon PScriptIntegrity # 136 # nextCbor) 0 0
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  pmatch (pscriptIntegrityControlAndStage # workCbor) $ \(PPair decoded stage) ->
    pand'List
      [ stage #== 0
      , pencodeNativeScriptsControlV1 # decoded #== pencodeNativeScriptsControlV1 # control
      , pnativeResolvedContextIsWellFormed # decoded
      , pstructuralTransitionIsValid # pre # witness
      , pverifyScriptIntegrityAuthentication
          # pre # witness # pcon PNoAuxiliaryWitness
      , pverifyScriptIntegrityAuthenticationSemanticsV1 # pre # witness
      ]

data MaximumFieldTerminalVector = MaximumFieldTerminalVector
  { maximumTransactionId :: String
  , maximumTransactionCommitment :: String
  , maximumCompactCbor :: String
  , maximumWitnessSetCbor :: String
  , maximumLengthsCbor :: String
  , maximumFieldIndex :: Integer
  , maximumItemCount :: Integer
  , maximumItemIndex :: Integer
  , maximumItemLength :: Integer
  , maximumEncodedLength :: Integer
  , maximumPreWorkRoot :: String
  , maximumPostWorkRoot :: String
  , maximumItemCommitment :: String
  , maximumCollectionPeaks :: [(Integer, String)]
  , maximumCollectionSiblings :: [String]
  , maximumChunk :: String
  , maximumChunkHash :: String
  }

maximumFieldTerminalCase :: String -> MaximumFieldTerminalVector -> TestTree
maximumFieldTerminalCase name vector = testCase name $
  passertEvalNoTraceWithoutHoistChecks (verifyMaximumFieldTerminal vector)

verifyMaximumFieldTerminal :: forall s. MaximumFieldTerminalVector -> Term s PBool
verifyMaximumFieldTerminal vector =
  let transactionId = maximumHex (maximumTransactionId vector)
      transactionCommitment = maximumHex (maximumTransactionCommitment vector)
      compactCbor = maximumHex (maximumCompactCbor vector)
      witnessSetCbor = maximumHex (maximumWitnessSetCbor vector)
      lengthsCbor = maximumHex (maximumLengthsCbor vector)
      contextCbor = maximumHex "8701546d6964676172642d636f6e73656e7375732d763118640000001864"
      fieldIndex = pconstant (maximumFieldIndex vector)
      itemCount = pconstant (maximumItemCount vector)
      itemIndex = pconstant (maximumItemIndex vector)
      itemLength = pconstant (maximumItemLength vector)
      encodedLength = pconstant (maximumEncodedLength vector)
      chunk = maximumHex (maximumChunk vector)
      collectionProof = pcon $ BoundedCollection.PItemProofV1
        (pdata BoundedCollection.pboundedCollectionVersion) (pdata fieldIndex) (pdata itemCount)
        (pdata itemIndex) (pdata itemLength)
        (pdata $ maximumHex $ maximumItemCommitment vector)
        (pdata $ maximumPeakList $ maximumCollectionPeaks vector)
        (pdata $ maximumHashList $ maximumCollectionSiblings vector)
      chunkProof = pcon $ Bounded.PChunkProofV1
        (pdata Bounded.pversion) (pdata fieldIndex) (pdata itemIndex)
        (pdata itemLength) (pdata 0) (pdata chunk)
        (pdata $ maximumPeakList [(0, maximumChunkHash vector)])
        (pdata pnil)
   in plet
        ( pencodeTransactionFieldScanWitness
            # compactCbor # witnessSetCbor # lengthsCbor # contextCbor
            # fieldIndex # itemIndex # 0 # itemCount # encodedLength
        )
        $ \workCbor ->
      plet
        ( pencodeTransactionFieldScanWitness
            # compactCbor # witnessSetCbor # lengthsCbor # contextCbor
            # (fieldIndex + 1) # 0 # 0 # (-1) # 0
        )
        $ \nextWorkCbor ->
      plet
        ( maximumFieldTerminalState
            transactionId transactionCommitment contextCbor 40
            (phashWorkWitness # pcon PCanonicalDecode # 40 # workCbor)
        )
        $ \pre ->
      plet
        ( maximumFieldTerminalState
            transactionId transactionCommitment contextCbor 41
            (phashWorkWitness # pcon PCanonicalDecode # 41 # nextWorkCbor)
        )
        $ \post ->
      plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
        pand'List
          [ NativeCompact.pnativeTxProofCommitmentV1 # compactCbor # witnessSetCbor # lengthsCbor
              #== transactionCommitment
          , maximumFieldStateWorkRoot pre
              #== maximumHex (maximumPreWorkRoot vector)
          , maximumFieldStateWorkRoot post
              #== maximumHex (maximumPostWorkRoot vector)
          , itemIndex + 1 #== itemCount
          , Bounded.pchunkCount # itemLength #== 1
          , pverifyCanonicalDecodeChunkSemanticsV1
              # pre # transition # collectionProof # chunkProof
          , NativeTransaction.pverifyMidgardTransactionFieldChunkV1
              # transactionId # transactionCommitment # compactCbor # witnessSetCbor
              # lengthsCbor # collectionProof # chunkProof
          ]

maximumFieldTerminalState :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PByteString ->
  Term s PInteger -> Term s PByteString -> Term s PValidationMachineStateV1
maximumFieldTerminalState transactionId transactionCommitment contextCbor counter workRoot =
  pcon $ PValidationMachineStateV1
    (pdata pmachineVersion) (pdata transactionId) (pdata transactionId)
    (pdata transactionCommitment) (pdata $ phashValidationContext # contextCbor)
    (pdata $ pcon PForced) (pdata transactionCommitment)
    (pdata $ pcon PCanonicalDecode) (pdata counter) (pdata workRoot)
    (pdata 0) (pdata 0) (pdata $ pcon PPending)
    (pdata $ preplicateBS # 32 # (pintegerToByte # 0))
    (pdata $ phashLedgerDelta # phexByteStr "80")

maximumFieldStateWorkRoot :: forall s. Term s PValidationMachineStateV1 -> Term s PByteString
maximumFieldStateWorkRoot state =
  pmatch state $ \fields -> pfromData (pmachineState'workRoot fields)

maximumHex :: forall s. String -> Term s PByteString
maximumHex = pconstant . Base16.decodeLenient . BSC.pack

maximumPeakList :: forall s. [(Integer, String)] -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
maximumPeakList = foldr
  (\(height, hashValue) rest -> pcons # pdata (pcon $ Merkle.PFrontierPeak (pdata $ pconstant height) (pdata $ maximumHex hashValue)) # rest)
  pnil

maximumHashList :: forall s. [String] -> Term s (PBuiltinList (PAsData PByteString))
maximumHashList = foldr (\hashValue rest -> pcons # pdata (maximumHex hashValue) # rest) pnil

maximumSpendInputFieldTerminal :: MaximumFieldTerminalVector
maximumSpendInputFieldTerminal = MaximumFieldTerminalVector
  "5616fc26bfef26893b4d4413a9b75af4275feed78dd7e13000d52af2b221402b"
  "c91b1c78baee450b8138a85aa4febfd6d9b5e6e208633fef29d4cec4e2c63d0b"
  "84018c5820fa49d0bce0fd16e8a8f20703162027ce5d24e4de5872def31c5e89f46d27cdb25820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58204b9a64944d619f32cd8c14e88f53c67857b717dd0686b1a20838c8e815308dce1a000d5e1920205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820ecaa2a4ca3d5110e0d124e3b3da2012a20c326fd6bdc25c1b71e67e9b9a04a9e00"
  "835820bdb04d390efaca1a689981865803b28a488f2c518ce905626962c7a7cebb7be25820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"
  "891942bb01183001010101186801" 0 434 433 38 17043
  "99ce6f53c84ad64e689f582577913b117d7f86257bffb96308554678d1506973"
  "d0354c24ccee8206b9926ca2662ea50639e0a114a5b60f4173eb986a94792cef"
  "dba9086489131c8444128fddbf2a864d16f628c13cac5b157d35b2bffeb8f12c"
  [(1,"dd12647adc66bb3ef04f3e994637f7057ca5c4b96b7a58cde6e4f925a34c4d83"),(4,"8539f11555b31aa517cb051cdf1c6849258e396576dc70474c16bd92b53efa01"),(5,"e0c3b95882dc7bdf380e0aa80c42b4a02cb52e77e4627db5e9a0399102039c25"),(7,"548fabcfbe39b4adee5ef017869a04015edfbc9fe2a2c8489a86a8059e956515"),(8,"2ec5b0bf539013547a4e08389d40946112938257232a275eabda35f47a512ef8")]
  ["7857404d8ac012dc3b41cc157772693f62616b0fecb0714d7526254f4850a8f0"]
  "82582000000000000000000000000000000000000000000000000000000000000000001901b1"
  "cb3168562b9c7affbe10efaa7474882ba25cc037186ac9075b2106d9f68f8d2e"

maximumReferenceInputFieldTerminal :: MaximumFieldTerminalVector
maximumReferenceInputFieldTerminal = MaximumFieldTerminalVector
  "cee18ffae3c1e118db1b046c5cc2da1e06cc8c611fe1afd2e6355149e869e3dc"
  "81de50f4c6b825a90ce4d70bdc89e7062494af859cfeda854dbe335a61c329f0"
  "84018c5820114094118138473ad4d828ed3aa3b5767604cf846235863510ded7f7fb5d36655820e8d8722d2b57d87875a3aead6c1b8ea4aa999d1ad7d8340a712ae2dee01a228458204ddc79a7ae5ce6f67c3282833863d15ffe34a3dcad707bb6b921a31bf9c77b3b1a000d5e4520205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820d79bc2560eef235bd2a538c7f6110513f9bca34ff66948b14aab16b2c21f5ec600"
  "83582058a2b8a985737738bebe056e227d4b84b4a97c9534a63afd2b10925d2e28b8935820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"
  "891827194295182c01010101186801" 1 433 432 38 17005
  "b7ccf4d203ce485dc61512b6680f6a7710f0fc9a57a5732fb838e1ada6b60d3b"
  "63b56d7b9c2368b667349fa33f722915691adba711645dce39a556a280a965af"
  "36b4dd3bf311b81d7f82428358cefdd48f2a8a896b105454ac98d24120580a51"
  [(0,"f25dce7b75dc95fac3a66ae5014c50b4e3aa3ae235aa70f5cf03d59888aa490d"),(4,"b9dfbe97f48da08a03316da8df54bc031cbbc6acfaf038ee04cf0e33d8ec12f6"),(5,"9328ea0ba44c359f4a243f6573f819a44ed4d70b086a61d0f5aebdf7261bd857"),(7,"266cda5f3304f6975845924b5ceb99aeb2f7379d619bac20f87ea4dd5049d6a5"),(8,"8606d2748f4812aa20f053dfc7660fb46b7f80a2fe4fb75309dc26cf8b0bd9b7")]
  [] "82582000000000000000000000000000000000000000000000000000000000000000001901b1"
  "4a6e597090be615a74af8ce771e0d4db02d004da070c991b1dfea5f355a87d78"

maximumObserverFieldTerminal :: MaximumFieldTerminalVector
maximumObserverFieldTerminal = MaximumFieldTerminalVector
  "9e6740ff958462051886f6d64a5fb7d03ee47dc50bd627c5765734f1c32f4bde"
  "7c0bebc279806e82bfdda06d46c06e6917e132e13325e48d2f3eb5ad7b7509da"
  "84018c5820114094118138473ad4d828ed3aa3b5767604cf846235863510ded7f7fb5d36655820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f5820598bbaa08e9cc6dc4d9634b23089ead14091f45c5e1165dcf8a6288be95a1b001a000d570d201927105820e127f848e4bda8c1e9b42ddf4c89dfbd1479301dd90551baeff900fdfcec2e975820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff58205c66a9cb2310e13ca74d861c7d29f5b996030ad755920f1dfc2c325ec3cb015d00"
  "8358209c3c9f949b41759fc4d9ea024e36e2aa7659f3d5dbe41611256f4dfc80a9a62d5820ad4dcd868783831d5bd321d25528c3295f55b1ea6c8d61d85c3216be9a73ea3d5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"
  "89182701182c191a420101192682186801" 3 224 223 28 6692
  "f07593faa6515f95d422e6ab0cdad5c58fc1946bf7bbd43d6a78c02fd1db6566"
  "ff51cd1448de36fccb43040b44c7977b7a0d6e96c11c41ad3bb2b4ebe4794136"
  "eea077178a4c1efc0237062dd79d0e88e25f226a75c50e2941386c551c8dbdf1"
  [(5,"b3d0869ae6f779e2a3bd2657550bad972479f7bd2bf30525d7d6ea1f6e273219"),(6,"8b8872929417945fc60a184c86b36e6cb8b3f79f063a2b1bd74276e3e6dd4561"),(7,"4f23682ce5aac57bdd356897e085c19da0956f3c43a4d0a590e06931693ec7aa")]
  ["37a247add2ab2bb8dd1ca90bc7340ccbd288795241423781485f83f1d2404441","12cbd11747b0ee2b75df2dbf849eb190bfa2ad508af765c0ef769438b6b697e4","761dcb1c5ea56450765f089f956ac800528d0d03b602a5f2ebdbcfdc391aeb92","38c8a30b4b417afa1434d182e99e1c5f595d57dfd0643c0e49e8c1975d60884c","637359bdbf37e81101c411bd6f8f72efd3bf2983764dfe73da65ebc09e999582"]
  "ffab1dd64f82b6991818c1ecc5047d52ce5d00f6fdbc5023e2980167"
  "770f825142ee9790250392a5b0310c8e2a8e833eaa6c62ca6f7223b5ed9e2288"

maximumOutputFieldTerminal :: MaximumFieldTerminalVector
maximumOutputFieldTerminal = MaximumFieldTerminalVector
  "851486b3f437bcae3712e1a2f0dbfab86062e4f0e8a3ed207607e1c0581c29e3"
  "23ad248d5c89787009031bf83f08588bf60d221f57370f8e3bb99b35e3d5fa2a"
  "84018c5820114094118138473ad4d828ed3aa3b5767604cf846235863510ded7f7fb5d36655820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f5820171d600ea53700101a59de7d6eb4b7c321fcc68f0e73c7952b0b18a266128e501a000d5ce520205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff582017cc85b1ee02772972e50a3306e76f232174a2ad9c1e108f257a1f0204b4455800"
  "835820f2556fc5ee4526e1b69dd8bcf77c5dbc7cab7187bbaec526c0d69004ecc8c0d95820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"
  "8918270119499901010101186801" 2 438 437 45 18794
  "dbff2136c653b5f01f9cc44095f2d9e72e93bbf5f87791c978c7424e2cad6030"
  "3e54da6a779af01fccaba5ffe794ec0f678f22c415bf5a7c3309b3726ab5ff5c"
  "0597306fb7c06665c796780ac8c2c3dff11acb9d8081451b0b6841225b66502e"
  [(1,"603f5cf5dfedbc32a9328b2dfe2229daebd97e00b6756614b6899268d5747194"),(2,"e95bebbf7cce121543e7466c88586ea885fe8c0c99de76cc6cc48bae690c9d43"),(4,"e753575ac7a8c06a27eef2344a3285467598b0c729a765ffd3e32611c0716c96"),(5,"4d9b3fd0b424101091fc3ab48971affc2e4175bb453e1689eafee7536c927e8b"),(7,"584997780ccd6abd1945725ef648852655a35c32ceb034331a238c9553e7b33e"),(8,"a44f00d0abbdb4e8898c53b61abcd9843747bd08a27b6cb7eb83997649b5e109")]
  ["471ddd2fe200fcacca47dfcab169d7a4a18309ba64e17771302b51785dc68b78"]
  "a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821b000000091c0a049ba0"
  "877b968e1a24a48a08d35b9abed8cd49537127d4dd1fc1adb183d3ba2ceddedf"

maximumRequiredSignerFieldTerminal :: MaximumFieldTerminalVector
maximumRequiredSignerFieldTerminal = MaximumFieldTerminalVector
  "0297901027ac0e7df5aeefe14961ca4fcebcdf27d69bc9d9ab2638ee2c86b71e"
  "c0ef3ffbbef5147e9ebc16ace943689df4f1ba1ef4521fc62af33fc8bb0d4b67"
  "84018c5820114094118138473ad4d828ed3aa3b5767604cf846235863510ded7f7fb5d36655820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f582010b1e0f0eedf2fd59f6e885d21a7291fbeeaa435b794343c0d44c6222886d53f1a000d594920205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820df206ad9e3c76e42f59400a5bf8ffb6d74ef4d4c82dad8b11b36df6de3db39325820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820a41a40b9671b9e0bf7705ea9fe39e4ff4162d25391a7837f17ee3c48aaf446b700"
  "8358206368be82071b67667a46025edba8a97b1311d8f561553682bd05d001c1d4492f5820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"
  "89182701183001190e8a01011931e601" 4 124 123 28 3692
  "a9daba44283f35f0f8bd4a463aab8de3a8004e0c25518ed8fae31f0c8459a61d"
  "2645969168012ec73dacba2a2fad3932d0af7eb0ee6721a5050d2eb21783e160"
  "feb8f7de321dc04604f1f371dbb874ae0937cb89cbc9b3725f65b394f66ba84c"
  [(2,"e5016ee8c62694b78e233d759c1f54ecde200985d94499af5f2500a3f101b57f"),(3,"1bca74d8d5ff0a6459d8244e030e1b389887d60bc07bfad14910aa7a785cb39b"),(4,"fe5959d59591a8e3e3f8b26a17fd836db4581e8f12cd3e35cce3f7dd1e9dfe49"),(5,"6962c3759551b9b2ddad0e534754ac08b55ccfd284d8655fa6883138a8fcf418"),(6,"680d5c5c242fc8a1d04d9ab55fa448e1bb43fecb5216067c636794c56c32dd65")]
  ["599aac19a5434816ee7892b6f6e6874b812b77f395d7a4c5bc26faa0f4ed3c45","e0fb7274ace519849848f853a11596545a2f0fa28045a6e79daca66443e1d91a"]
  "38abf94805d076d7253d8386794096ec3d48fe233bc45d5edf97ab19"
  "a413f53279be814fc18bb395723dbf929bddfb04c628af191ff9adebc0fa282a"

maximumMintFieldTerminal :: MaximumFieldTerminalVector
maximumMintFieldTerminal = MaximumFieldTerminalVector
  "fb8384370f1f3b2c4543567ee9d2bd0d3c9a4152b505c8eecd540cf99e78bcd2"
  "346193b5f63533c46919d8dddd78bcb99777a9a5d87c00ca49a26242c0e4e086"
  "84018c5820114094118138473ad4d828ed3aa3b5767604cf846235863510ded7f7fb5d36655820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f582077da2ade1bedecb84bafe4fabaefeccaf5da90cd82634773f3626f2ad5e92e291a000d5d95201927105820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea7458207dc0480f6a10748bc18aed966ef23ca7dd70cd929f5a88b056d427e2f9453c32582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820cfe3cf8c48a624b593eb8f53bdf83543b3db912f088fbc07b40d2b800cbf6efd00"
  "8358205a38624e9c4dd250767e307ff82f25d727953cbe76e83ed2361505d1bfde4e2658201ae334ef559fb72274ae60b8a4b57232ad0bc17b9fcbb6c4cc51aef26774c6e65820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"
  "891827011915b2010119155619165a186801" 5 130 129 43 5420
  "fb84741314c90a947da6e44eee2d225d2f80a61bf0ab36e84b378f41881ad49a"
  "9c2c4e915e7d0f502fa3070c90c9eafb2f59526f9e02c21e8a2cbf756457e83a"
  "534ff6685dd10a576be7dec4ecb1cf2f239a5fdcb751db98ccc1b93ebd4e5c04"
  [(1,"ce7d37cda58da9e9e61128e546de8b86657a6ba8b3412ff6b0ac9768220facc1"),(7,"a4e70fd3e6e67a34ff688b3ee548e87d066c538c85340620668e3b53e09d7110")]
  ["7dee9561c439c28a1058042f1cccd273783fe91f56440e1b9c44e306d0aee12b"]
  "82581cffab1dd64f82b6991818c1ecc5047d52ce5d00f6fdbc5023e2980167a1494d696467617264563101"
  "ae892cdb843a795de543205f99a43ea2d0f946bcab042d2c405bd786dbad75da"

canonicalDecodeEmptyTransitions :: forall s. Term s PBool
canonicalDecodeEmptyTransitions =
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment 1_000_000 (-1) 200
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment (cekHash 0x37) (cekHash 0x38) 0
    )
    $ \body ->
  plet (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0) $ \compact ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet validContextCbor $ \contextCbor ->
  plet (pcon $ PNativeTxFieldPreimageLengthsV1 1 1 1 1 1 1 1 1 1) $ \validLengths ->
  plet (pcon $ PNativeTxFieldPreimageLengthsV1 0 1 1 1 1 1 1 1 1) $ \invalidLengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # validLengths) $ \validLengthsCbor ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # invalidLengths) $ \invalidLengthsCbor ->
  plet
    (canonicalDecodeEmptyCase transactionId compactCbor witnessSetCbor validLengthsCbor contextCbor True)
    $ \valid ->
  plet
    (canonicalDecodeEmptyCase transactionId compactCbor witnessSetCbor invalidLengthsCbor contextCbor False)
    $ \invalid ->
    valid #&& invalid

canonicalDecodeEmptyCase :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PByteString ->
  Term s PByteString -> Term s PByteString -> Bool -> Term s PBool
canonicalDecodeEmptyCase transactionId compactCbor witnessSetCbor lengthsCbor contextCbor valid =
  plet
    (NativeCompact.pnativeTxProofCommitmentV1 # compactCbor # witnessSetCbor # lengthsCbor)
    $ \commitment ->
  plet
    ( pencodeTransactionFieldScanWitness
        # compactCbor # witnessSetCbor # lengthsCbor # contextCbor
        # 0 # 0 # 0 # (-1) # 0
    )
    $ \workCbor ->
  plet
    ( cekBoundStateFixture
        transactionId commitment (phashValidationContext # contextCbor)
        (pcon PCanonicalDecode) 0
        (phashWorkWitness # pcon PCanonicalDecode # 0 # workCbor) 0 0
    )
    $ \pre ->
  plet
    ( if valid
        then
          cekBoundStateFixture
            transactionId commitment (phashValidationContext # contextCbor)
            (pcon PCanonicalDecode) 1
            ( phashWorkWitness # pcon PCanonicalDecode # 1
                # ( pencodeTransactionFieldScanWitness
                      # compactCbor # witnessSetCbor # lengthsCbor # contextCbor
                      # 1 # 0 # 0 # (-1) # 0
                  )
            )
            0 0
        else inputSetsExactRejection pre $ pconstant "E_FIELD_PREIMAGE_SIZE"
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata witness) (pdata $ pcon PNoAuxiliaryWitness))
    $ \evidence ->
  plet (pcanonicalDecodeControlFromWitness # workCbor) $ \control ->
    pstructuralTransitionIsValid # pre # witness
      #&& pcanonicalScanFieldSuccessorIsExact
        # pre # post # compactCbor # witnessSetCbor # lengthsCbor # contextCbor # 0
        #== pconstant valid
      #&& pverifyCanonicalDecodeEmpty # pre # witness # control
      #&& pverifyCanonicalDecodeEmptySemanticsV1 # pre # witness
      #&& pverifyCanonicalDecode # pre # witness # pcon PNoAuxiliaryWitness
      #&& pverifyCanonicalDecodeOneStepV1 # pre # evidence
      #&& pverifyCanonicalDecodeSemanticsV1 # pre # evidence

canonicalDecodeChunkTransition :: forall s. Term s PBool
canonicalDecodeChunkTransition =
  plet (phexByteStr "01") $ \itemCbor ->
  pmatch (inputSetsSingletonProof 0 itemCbor) $ \(PPair collectionProof chunkProof) ->
  plet (inputSetsSingletonCommitment 0 itemCbor) $ \fieldCommitment ->
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        fieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment 1_000_000 (-1) 200
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment (cekHash 0x37) (cekHash 0x38) 0
    )
    $ \body ->
  plet (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0) $ \compact ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet (pcon $ PNativeTxFieldPreimageLengthsV1 3 1 1 1 1 1 1 1 1) $ \lengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # lengths) $ \lengthsCbor ->
  plet validContextCbor $ \contextCbor ->
  plet
    (NativeCompact.pnativeTxProofCommitmentV1 # compactCbor # witnessSetCbor # lengthsCbor)
    $ \commitment ->
  plet
    ( pencodeTransactionFieldScanWitness
        # compactCbor # witnessSetCbor # lengthsCbor # contextCbor
        # 0 # 0 # 0 # (-1) # 0
    )
    $ \workCbor ->
  plet
    ( cekBoundStateFixture
        transactionId commitment (phashValidationContext # contextCbor)
        (pcon PCanonicalDecode) 7
        (phashWorkWitness # pcon PCanonicalDecode # 7 # workCbor) 0 0
    )
    $ \pre ->
  plet
    ( cekBoundStateFixture
        transactionId commitment (phashValidationContext # contextCbor)
        (pcon PCanonicalDecode) 8
        ( phashWorkWitness # pcon PCanonicalDecode # 8
            # ( pencodeTransactionFieldScanWitness
                  # compactCbor # witnessSetCbor # lengthsCbor # contextCbor
                  # 1 # 0 # 0 # (-1) # 0
              )
        )
        0 0
    )
    $ \post ->
  plet (inputSetsExactRejection pre $ pconstant "E_FIELD_PREIMAGE_SIZE") $ \wrongPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost)) $ \wrongWitness ->
  plet
    (pcon $ PTransactionFieldChunkWitness (pdata collectionProof) (pdata chunkProof))
    $ \auxiliary ->
  plet (pcon $ PValidationOneStepEvidenceV1 (pdata witness) (pdata auxiliary)) $ \evidence ->
  plet (pcanonicalDecodeControlFromWitness # workCbor) $ \control ->
    pand'List
      [ pcanonicalArgumentHeaderSize # 1 #== 1
      , pmatch (ptransactionFieldItemEncodedLength # 0 # 1) $ \case
          PJust encodedLength -> encodedLength #== 2
          PNothing -> pconstant False
      , pstructuralTransitionIsValid # pre # witness
      , pstructuralTransitionIsValid # pre # wrongWitness
      , pverifyCanonicalDecodeChunk
          # pre # witness # control # collectionProof # chunkProof
      , pverifyCanonicalDecodeChunkSemanticsV1
          # pre # witness # collectionProof # chunkProof
      , pverifyCanonicalDecode # pre # witness # auxiliary
      , pverifyCanonicalDecodeOneStepV1 # pre # evidence
      , pverifyCanonicalDecodeSemanticsV1 # pre # evidence
      , pnot
          # ( pverifyCanonicalDecodeChunkSemanticsV1
                # pre # wrongWitness # collectionProof # chunkProof
            )
      ]

canonicalDecodeRejectsLegacyFieldSevenScriptProof :: forall s. Term s PBool
canonicalDecodeRejectsLegacyFieldSevenScriptProof =
  plet
    ( pencodeMidgardVersionedScript
        # ( pcon $ PMidgardVersionedScript
              (pdata $ pcon PPlutusV3Script)
              (pdata $ phexByteStr "010203")
          )
    )
    $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \correctFieldCommitment ->
  plet (Bounded.phashChunk # 7 # 0 # 0 # itemCbor) $ \legacyChunkHash ->
  plet (cekSinglePeak legacyChunkHash) $ \legacyChunkFrontier ->
  plet
    ( Bounded.pcommitment
        # 6 # 0 # (plengthBS # itemCbor) # legacyChunkFrontier
    )
    $ \poisonedItemCommitment ->
  plet
    ( BoundedCollection.phashBoundedCollectionItem
        # 6 # 0 # (plengthBS # itemCbor) # poisonedItemCommitment
    )
    $ \poisonedItemHash ->
  plet (cekSinglePeak poisonedItemHash) $ \poisonedCollectionFrontier ->
  plet
    ( pcon $ BoundedCollection.PItemProofV1
        (pdata BoundedCollection.pboundedCollectionVersion)
        (pdata 6)
        (pdata 1)
        (pdata 0)
        (pdata $ plengthBS # itemCbor)
        (pdata poisonedItemCommitment)
        (pdata poisonedCollectionFrontier)
        (pdata pnil)
    )
    $ \collectionProof ->
  plet
    ( pcon $ Bounded.PChunkProofV1
        (pdata Bounded.pversion)
        (pdata 6)
        (pdata 0)
        (pdata $ plengthBS # itemCbor)
        (pdata 0)
        (pdata itemCbor)
        (pdata legacyChunkFrontier)
        (pdata pnil)
    )
    $ \chunkProof ->
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata NativeField.pemptyFieldCommitment)
        (pdata correctFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment 0 (-1) (-1)
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment (cekHash 0x37) (cekHash 0x38) 0
    )
    $ \body ->
  plet (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0) $ \compact ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet
    ( pcon $ PNativeTxFieldPreimageLengthsV1
        1 1 1 1 1 1 1 (2 + plengthBS # itemCbor) 1
    )
    $ \lengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # lengths) $ \lengthsCbor ->
  plet validContextCbor $ \contextCbor ->
  plet
    (NativeCompact.pnativeTxProofCommitmentV1 # compactCbor # witnessSetCbor # lengthsCbor)
    $ \transactionCommitment ->
  plet
    ( pencodeTransactionFieldScanWitness
        # compactCbor # witnessSetCbor # lengthsCbor # contextCbor
        # 6 # 0 # 0 # (-1) # 0
    )
    $ \workCbor ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment (phashValidationContext # contextCbor)
        (pcon PCanonicalDecode) 7
        (phashWorkWitness # pcon PCanonicalDecode # 7 # workCbor) 0 0
    )
    $ \pre ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment (phashValidationContext # contextCbor)
        (pcon PCanonicalDecode) 8
        ( phashWorkWitness # pcon PCanonicalDecode # 8
            # ( pencodeTransactionFieldScanWitness
                  # compactCbor # witnessSetCbor # lengthsCbor # contextCbor
                  # 7 # 0 # 0 # (-1) # 0
              )
        )
        0 0
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
    pand'List
      [ pnot # (Bounded.pfromBytes # 6 # 0 # itemCbor #== poisonedItemCommitment)
      , pnot
          # ( pverifyCanonicalDecodeChunkSemanticsV1
                # pre # transition # collectionProof # chunkProof
            )
      , pnot
          # ( pverifyCanonicalDecodeItemSemanticsV1
                # pre # transition # collectionProof # itemCbor
            )
      , pnot
          # ( pverifyCanonicalDecodeItemSemanticsV1
                # pre # transition # collectionProof # phexByteStr "ff"
            )
      ]

canonicalDecodeItemTransition :: forall s. Term s PBool
canonicalDecodeItemTransition =
  plet (phexByteStr "01") $ \itemCbor ->
  pmatch (inputSetsSingletonProof 0 itemCbor) $ \(PPair collectionProof _) ->
  plet (inputSetsSingletonCommitment 0 itemCbor) $ \fieldCommitment ->
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        fieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment 1_000_000 (-1) 200
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment (cekHash 0x37) (cekHash 0x38) 0
    )
    $ \body ->
  plet (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0) $ \compact ->
  plet (NativeCompact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (NativeCompact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet (pcon $ PNativeTxFieldPreimageLengthsV1 3 1 1 1 1 1 1 1 1) $ \lengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # lengths) $ \lengthsCbor ->
  plet validContextCbor $ \contextCbor ->
  plet
    (NativeCompact.pnativeTxProofCommitmentV1 # compactCbor # witnessSetCbor # lengthsCbor)
    $ \commitment ->
  plet
    ( pencodeTransactionFieldScanWitness
        # compactCbor # witnessSetCbor # lengthsCbor # contextCbor
        # 0 # 0 # 0 # (-1) # 0
    )
    $ \workCbor ->
  plet
    ( cekBoundStateFixture
        transactionId commitment (phashValidationContext # contextCbor)
        (pcon PCanonicalDecode) 9
        (phashWorkWitness # pcon PCanonicalDecode # 9 # workCbor) 0 0
    )
    $ \pre ->
  plet
    ( cekBoundStateFixture
        transactionId commitment (phashValidationContext # contextCbor)
        (pcon PCanonicalDecode) 10
        ( phashWorkWitness # pcon PCanonicalDecode # 10
            # ( pencodeTransactionFieldScanWitness
                  # compactCbor # witnessSetCbor # lengthsCbor # contextCbor
                  # 1 # 0 # 0 # (-1) # 0
              )
        )
        0 0
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ PTransactionFieldItemWitness (pdata collectionProof) (pdata itemCbor))
    $ \auxiliary ->
  plet (pcon $ PValidationOneStepEvidenceV1 (pdata witness) (pdata auxiliary)) $ \evidence ->
  plet (pbindCanonicalDecodeItemSourceV1 # pre # witness) $ \source ->
  plet (pobserveCanonicalDecodeItemV1 # witness # collectionProof # itemCbor) $ \observation ->
  plet (pverifyCanonicalDecodeItemObservationV1 # witness # source # observation) $ \proof ->
  pmatch proof $ \proofFields ->
    pand'List
      [ pfromData (pcanonicalProof'activeItemCount proofFields) #== 1
      , pfromData (pcanonicalProof'itemEncodingIsValid proofFields)
      , pfromData (pcanonicalProof'nextEncodedLength proofFields) #== 3
      , pverifyCanonicalDecodeItemSuccessorV1 # pre # witness # source # proof
      , pverifyCanonicalDecodeItemFromSourceV1
          # pre # witness # source # collectionProof # itemCbor
      , pverifyCanonicalDecodeItemSemanticsV1
          # pre # witness # collectionProof # itemCbor
      , pverifyCanonicalDecode # pre # witness # auxiliary
      , pverifyCanonicalDecodeOneStepV1 # pre # evidence
      , pverifyCanonicalDecodeSemanticsV1 # pre # evidence
      , pnot
          # ( pverifyCanonicalDecode # pre # witness
                # pcon (PTransactionRedeemerItemBeginWitness $ pdata collectionProof)
            )
      , pnot
          # ( pverifyCanonicalDecodeItemFromSourceV1
                # pre # witness # source # collectionProof # phexByteStr "02"
            )
      ]

scriptIntegrityRemainingTransitions :: forall s. Term s PBool
scriptIntegrityRemainingTransitions =
  plet (ScriptLanguageViews.pexpectedScriptIntegrityHash # cekHash 0x33 # 0) $ \integrityHash ->
  pmatch
    (cekObserverProofSourceWithIntegrity NativeField.pemptyFieldCommitment integrityHash 0)
    $ \(PPair control transactionId) ->
  pmatch control $ \c ->
  plet (pblake2b_256 # pfromData (pnativeControl'witnessSetCompactCbor c)) $ \witnessSetHash ->
  plet (pencodeScriptIntegrityCompactWitnessV1 # control) $ \compactCbor ->
  plet
    (pencodeScriptIntegrityWitnessSetWitnessV1 # control # integrityHash # witnessSetHash)
    $ \witnessSetCbor ->
  plet
    (pencodeScriptIntegrityFinalizeWitnessV1 # control # integrityHash # cekHash 0x33)
    $ \finalizeCbor ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pnativeControl'compactCbor c)
        # pfromData (pnativeControl'witnessSetCompactCbor c)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor c)
    )
    $ \transactionCommitment ->
  plet (phashValidationContext # pfromData (pnativeControl'contextCbor c)) $ \contextHash ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash (pcon PScriptIntegrity) 136
        (phashWorkWitness # pcon PScriptIntegrity # 136 # compactCbor) 0 0
    )
    $ \compactPre ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash (pcon PScriptIntegrity) 137
        (phashWorkWitness # pcon PScriptIntegrity # 137 # witnessSetCbor) 0 0
    )
    $ \witnessSetPre ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash (pcon PScriptIntegrity) 138
        (phashWorkWitness # pcon PScriptIntegrity # 138 # finalizeCbor) 0 0
    )
    $ \finalizePre ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash (pcon PCek) 139
        ( phashWorkWitness # pcon PCek # 139
            # ( pencodeCekWitnessV1
                  # control # pconstant "" # 0 # 0 # 0
                  # pconstant "" # 0 # 0 # pconstant ""
              )
        )
        0 0
    )
    $ \validPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata compactCbor) (pdata witnessSetPre)) $ \compactWitness ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata witnessSetCbor) (pdata finalizePre)) $ \witnessSetWitness ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata finalizeCbor) (pdata validPost)) $ \finalizeWitness ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata compactWitness) (pdata $ pcon PNoAuxiliaryWitness))
    $ \compactEvidence ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata witnessSetWitness) (pdata $ pcon PNoAuxiliaryWitness))
    $ \witnessSetEvidence ->
  plet
    (pcon $ PValidationOneStepEvidenceV1 (pdata finalizeWitness) (pdata $ pcon PNoAuxiliaryWitness))
    $ \finalizeEvidence ->
  pmatch (cekObserverProofSource NativeField.pemptyFieldCommitment) $ \(PPair invalidControl invalidTransactionId) ->
  pmatch invalidControl $ \invalidFields ->
  plet
    ( pencodeScriptIntegrityFinalizeWitnessV1
        # invalidControl # cekHash 0x16 # cekHash 0x33
    )
    $ \invalidCbor ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pnativeControl'compactCbor invalidFields)
        # pfromData (pnativeControl'witnessSetCompactCbor invalidFields)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor invalidFields)
    )
    $ \invalidCommitment ->
  plet
    ( cekBoundStateFixture
        invalidTransactionId invalidCommitment
        (phashValidationContext # pfromData (pnativeControl'contextCbor invalidFields))
        (pcon PScriptIntegrity) 140
        (phashWorkWitness # pcon PScriptIntegrity # 140 # invalidCbor) 0 0
    )
    $ \invalidPre ->
  plet (inputSetsExactRejection invalidPre $ pconstant "E_INVALID_FIELD_TYPE") $ \invalidPost ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata invalidCbor) (pdata invalidPost)) $ \invalidWitness ->
    pand'List
      [ pverifyScriptIntegrityCompactSemanticsV1 # compactPre # compactWitness
      , pverifyScriptIntegrityWitnessSetSemanticsV1 # witnessSetPre # witnessSetWitness
      , pverifyScriptIntegrityFinalizeSemanticsV1 # finalizePre # finalizeWitness
      , pverifyScriptIntegrityFinalizeSemanticsV1 # invalidPre # invalidWitness
      , pverifyScriptIntegrityOneStepV1 # compactPre # compactEvidence
      , pverifyScriptIntegrityOneStepV1 # witnessSetPre # witnessSetEvidence
      , pverifyScriptIntegrityOneStepV1 # finalizePre # finalizeEvidence
      ]

nativeScriptsExecutionControl :: forall s.
  Term s PNativeScriptsControlV1 ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PInteger -> Term s PInteger -> Term s PNativeScriptsControlV1
nativeScriptsExecutionControl base sourcePeaks purposePeaks executionPeaks cursor bitmap =
  pmatch base $ \c -> pcon $ PNativeScriptsControlV1
    (pnativeControl'compactCbor c)
    (pnativeControl'witnessSetCompactCbor c)
    (pnativeControl'fieldPreimageLengthsCbor c)
    (pnativeControl'contextCbor c)
    (pnativeControl'resolvedInputCount c)
    (pnativeControl'resolvedInputsAccumulator c)
    (pnativeControl'spendInputCount c)
    (pnativeControl'resolvedItemPeaks c)
    (pnativeControl'signerCount c)
    (pnativeControl'signerFrontierCommitment c)
    (pdata 1) (pdata sourcePeaks)
    (pnativeControl'redeemerCount c) (pnativeControl'redeemerPeaks c)
    (pdata 1) (pdata purposePeaks)
    (pnativeControl'outputCount c) (pnativeControl'outputPeaks c)
    (pnativeControl'outputDescriptorPeaks c)
    (pnativeControl'mintCount c) (pnativeControl'mintPeaks c)
    (pdata 1) (pdata executionPeaks) (pdata cursor) (pdata bitmap)
    (pnativeControl'resolutionScheduleHash c)

nativeScriptsEffectfulTransition :: forall s. Term s PBool
nativeScriptsEffectfulTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x91)) $ \scriptHash ->
  plet (phexByteStr "0102") $ \subject ->
  plet (phexByteStr "00") $ \sourceKey ->
  plet (cekHash 0x92) $ \itemCommitment ->
  plet (cekHash 0x93) $ \redeemerLeaf ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # subject) $ \purposeLeaf ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # sourceKey # 3 # scriptHash # 10 # itemCommitment
    )
    $ \sourceLeaf ->
  plet (ScriptProof.pexecutionLeafHash # 3 # purposeLeaf # sourceLeaf # redeemerLeaf) $ \executionLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet (cekSinglePeak executionLeaf) $ \executionPeaks ->
  pmatch (cekObserverProofSource NativeField.pemptyFieldCommitment) $ \(PPair base transactionId) ->
  plet
    (nativeScriptsExecutionControl base sourcePeaks purposePeaks executionPeaks 0 0)
    $ \control ->
  plet
    (nativeScriptsExecutionControl base sourcePeaks purposePeaks executionPeaks 1 1)
    $ \nextControl ->
  pmatch base $ \baseFields ->
  plet (pencodeNativeScriptsControlV1 # control) $ \workCbor ->
  plet (pencodeNativeScriptsControlV1 # nextControl) $ \nextCbor ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pnativeControl'compactCbor baseFields)
        # pfromData (pnativeControl'witnessSetCompactCbor baseFields)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor baseFields)
    )
    $ \transactionCommitment ->
  plet (phashValidationContext # pfromData (pnativeControl'contextCbor baseFields)) $ \contextHash ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PNativeScripts) 136
        (phashWorkWitness # pcon PNativeScripts # 136 # workCbor) 0 0
    )
    $ \pre ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PNativeScripts) 137
        (phashWorkWitness # pcon PNativeScripts # 137 # nextCbor) 0 0
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    ( pcon $ PNativeExecutionDescriptorWitness
        (pdata 0) (pdata 3) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata subject) (pdata pnil)
        (pdata 0) (pdata 0) (pdata sourceKey) (pdata 10)
        (pdata itemCommitment) (pdata pnil) (pdata redeemerLeaf) (pdata pnil)
        (pdata $ pcon PDNothing) (pdata pnil)
    )
    $ \auxiliary ->
  plet (pcon $ PValidationOneStepEvidenceV1 (pdata witness) (pdata auxiliary)) $ \evidence ->
    pand'List
      [ pstructuralTransitionIsValid # pre # witness
      , pverifyNativeScriptsEffectfulSemanticsV1
          # pre # witness # 0 # 3 # 0 # 0 # scriptHash # subject # pnil
          # 0 # 0 # sourceKey # 10 # itemCommitment # pnil # redeemerLeaf # pnil
      , pverifyNativeScripts # pre # witness # auxiliary
      , pverifyNativeScriptsOneStepV1 # pre # evidence
      ]

nativeScriptsNativeTransition :: forall s. Term s PBool
nativeScriptsNativeTransition =
  plet (phexByteStr "82004100") $ \scriptCbor ->
  pmatch (inputSetsSingletonProof 6 scriptCbor) $ \(PPair collectionProof firstChunkProof) ->
  pmatch collectionProof $ \item ->
  plet (preplicateBS # 28 # (pintegerToByte # 0x94)) $ \scriptHash ->
  plet (phexByteStr "0304") $ \subject ->
  plet (phexByteStr "00") $ \sourceKey ->
  plet (pfromData $ BoundedCollection.pitemProof'itemLength item) $ \scriptTotalLength ->
  plet (pfromData $ BoundedCollection.pitemProof'itemCommitment item) $ \itemCommitment ->
  plet (ScriptProof.ppurposeLeafHash # 0 # 0 # scriptHash # subject) $ \purposeLeaf ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # 0 # sourceKey # 0 # scriptHash # scriptTotalLength # itemCommitment
    )
    $ \sourceLeaf ->
  plet (ScriptProof.pexecutionLeafHash # 0 # purposeLeaf # sourceLeaf # pconstant "") $ \executionLeaf ->
  plet (cekSinglePeak sourceLeaf) $ \sourcePeaks ->
  plet (cekSinglePeak purposeLeaf) $ \purposePeaks ->
  plet (cekSinglePeak executionLeaf) $ \executionPeaks ->
  pmatch (cekObserverProofSource NativeField.pemptyFieldCommitment) $ \(PPair base transactionId) ->
  plet
    (nativeScriptsExecutionControl base sourcePeaks purposePeaks executionPeaks 0 0)
    $ \control ->
  pmatch base $ \baseFields ->
  plet (pencodeNativeScriptsControlV1 # control) $ \workCbor ->
  plet
    ( pencodePhaseANativeScriptsScanWitness
        # pfromData (pnativeControl'compactCbor baseFields)
        # pfromData (pnativeControl'witnessSetCompactCbor baseFields)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor baseFields)
        # pfromData (pnativeControl'contextCbor baseFields)
        # (pblake2b_256 # workCbor)
        # 1 # 1 # 0 # 0 # scriptTotalLength # itemCommitment
        # 3 # pconstant "" # 0 # 0 # (-1)
        # pfromData (pnativeControl'signerCount baseFields) # pnil # workCbor
    )
    $ \phaseAWorkCbor ->
  plet
    ( NativeCompact.pnativeTxProofCommitmentV1
        # pfromData (pnativeControl'compactCbor baseFields)
        # pfromData (pnativeControl'witnessSetCompactCbor baseFields)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor baseFields)
    )
    $ \transactionCommitment ->
  plet (phashValidationContext # pfromData (pnativeControl'contextCbor baseFields)) $ \contextHash ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PNativeScripts) 138
        (phashWorkWitness # pcon PNativeScripts # 138 # workCbor) 0 0
    )
    $ \pre ->
  plet
    ( cekBoundStateFixture
        transactionId transactionCommitment contextHash
        (pcon PPhaseANativeScripts) 139
        (phashWorkWitness # pcon PPhaseANativeScripts # 139 # phaseAWorkCbor) 0 0
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    ( pcon $ PNativeExecutionDescriptorWitness
        (pdata 0) (pdata 0) (pdata 0) (pdata 0)
        (pdata scriptHash) (pdata subject) (pdata pnil)
        (pdata 0) (pdata 0) (pdata sourceKey) (pdata scriptTotalLength)
        (pdata itemCommitment) (pdata pnil) (pdata $ pconstant "") (pdata pnil)
        (pdata $ pcon $ PDJust $ pdata firstChunkProof) (pdata pnil)
    )
    $ \auxiliary ->
    pverifyNativeScriptsNativeSemanticsV1
      # pre # witness # 0 # 0 # 0 # scriptHash # subject # pnil
      # 0 # 0 # sourceKey # scriptTotalLength # itemCommitment # pnil
      # pconstant "" # pnil # firstChunkProof # pnil
      #&& pverifyNativeScripts # pre # witness # auxiliary

scriptSourcesOutputProofBeginTransition :: forall s. Term s PBool
scriptSourcesOutputProofBeginTransition =
  plet resolveInputsOutputCbor $ \outputCbor ->
  plet (inputSetsSingletonCommitment 2 outputCbor) $ \outputsCommitment ->
  pmatch (inputSetsSingletonProof 2 outputCbor) $ \(PPair collectionProof _) ->
  pmatch collectionProof $ \item ->
  plet
    (ScriptProof.poutputItemLeafHash # 0 # pfromData (BoundedCollection.pitemProof'itemCommitment item))
    $ \outputLeaf ->
  plet (cekSinglePeak outputLeaf) $ \outputPeaks ->
  plet
    ( LedgerOutputProof.pinitialControlV1
        # 0 # pfromData (BoundedCollection.pitemProof'itemLength item)
        # pfromData (BoundedCollection.pitemProof'itemCommitment item)
    )
    $ \outputProof ->
  pmatch
    ( phaseANativeControlFixtureWithOutputs
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        outputsCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageFourCbor phaseControl 5 1 outputPeaks 1) $ \workCbor ->
  plet (pscriptSourcesStageZeroControlFromWitness # workCbor) $ \control ->
  plet
    (pencodeScriptSourcesOutputProofWitness # control # outputProof)
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 85 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 86 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptSourcesOutputProofBeginSemanticsV1
        # pre # witness # 0
        # pfromData (BoundedCollection.pitemProof'itemLength item)
        # pfromData (BoundedCollection.pitemProof'itemCommitment item)
        # pnil
      #&& pnot
        # ( pverifyScriptSourcesOutputProofBeginSemanticsV1
              # pre # witness # 1
              # pfromData (BoundedCollection.pitemProof'itemLength item)
              # pfromData (BoundedCollection.pitemProof'itemCommitment item)
              # pnil
          )

scriptSourcesOutputProofStepTransition :: forall s. Term s PBool
scriptSourcesOutputProofStepTransition =
  plet resolveInputsOutputCbor $ \outputCbor ->
  plet (inputSetsSingletonCommitment 2 outputCbor) $ \outputsCommitment ->
  pmatch (inputSetsSingletonProof 2 outputCbor) $ \(PPair collectionProof _) ->
  pmatch collectionProof $ \item ->
  plet
    (ScriptProof.poutputItemLeafHash # 0 # pfromData (BoundedCollection.pitemProof'itemCommitment item))
    $ \outputLeaf ->
  plet (cekSinglePeak outputLeaf) $ \outputPeaks ->
  plet
    ( LedgerOutputProof.pinitialControlV1
        # 0 # pfromData (BoundedCollection.pitemProof'itemLength item)
        # pfromData (BoundedCollection.pitemProof'itemCommitment item)
    )
    $ \outputProof ->
  pmatch
    ( phaseANativeControlFixtureWithOutputs
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        outputsCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageFourCbor phaseControl 5 1 outputPeaks 1) $ \baseCbor ->
  plet (pscriptSourcesStageZeroControlFromWitness # baseCbor) $ \control ->
  plet
    (pencodeScriptSourcesOutputProofWitness # control # outputProof)
    $ \workCbor ->
  plet (pscriptSourcesStageZeroControlFromWitness # workCbor) $ \activeControl ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 87 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 88 workCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet
    (pcon $ LedgerOutputProof.PLedgerOutputProofAdvanced $ pdata outputProof)
    $ \result ->
    pstructuralTransitionIsValid # pre # witness
      #&& pscriptSourcesOutputProofResult
        # pre # witness # activeControl # outputProof # result

scriptSourcesOutputProofFinalizeTransition :: forall s. Term s PBool
scriptSourcesOutputProofFinalizeTransition =
  plet resolveInputsNoReferenceOutput $ \outputCbor ->
  plet resolveInputsNoReferenceDescriptor $ \descriptorCbor ->
  plet (inputSetsSingletonCommitment 2 outputCbor) $ \outputsCommitment ->
  pmatch (inputSetsSingletonProof 2 outputCbor) $ \(PPair collectionProof _) ->
  pmatch collectionProof $ \item ->
  plet
    (ScriptProof.poutputItemLeafHash # 0 # pfromData (BoundedCollection.pitemProof'itemCommitment item))
    $ \outputLeaf ->
  plet (cekSinglePeak outputLeaf) $ \outputPeaks ->
  plet
    (cekSinglePeak $ ScriptProof.poutputDescriptorLeafHash # 0 # descriptorCbor)
    $ \descriptorPeaks ->
  plet (phexByteStr "11111111111111111111111111111111111111111111111111111111") $ \scriptHash ->
  plet
    (cekSinglePeak $ ScriptProof.ppurposeLeafHash # 3 # 0 # scriptHash # scriptHash)
    $ \sourcePeaks ->
  plet
    ( pcon $ PReceivePurposeScanControlV1
        (pdata 1) (pdata sourcePeaks) (pdata 0) (pdata $ pconstant "")
        (pdata $ pconstant "") (pdata descriptorPeaks)
    )
    $ \nextReceiveScan ->
  pmatch
    ( phaseANativeControlFixtureWithOutputs
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        outputsCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesStageFiveCbor phaseControl 0 1 outputPeaks 1 pemptyReceivePurposeScanControl)
    $ \baseWorkCbor ->
  plet (pscriptSourcesStageZeroControlFromWitness # baseWorkCbor) $ \control ->
  plet
    (pencodeScriptSourcesOutputProofWitness # control # resolveInputsNoReferenceTerminal)
    $ \workCbor ->
  plet
    (scriptSourcesStageFiveCbor phaseControl 1 1 outputPeaks 1 nextReceiveScan)
    $ \nextWorkCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 89 workCbor) $ \pre ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 90 nextWorkCbor) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
    pand'List
      [ LedgerOutputProof.pdescriptorIsExactV1
          # resolveInputsNoReferenceTerminal
          # (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor)
      , pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesOutputProofFinalizeSemanticsV1
          # pre # witness # descriptorCbor # pcon PNoSignerSetProof
      ]

scriptSourcesOutputProofFinalizeMissingSignerTransition :: forall s. Term s PBool
scriptSourcesOutputProofFinalizeMissingSignerTransition =
  plet
    (phexByteStr "a200581d6811111111111111111111111111111111111111111111111111111111018200a0")
    $ \outputCbor ->
  plet
    (phexByteStr "6811111111111111111111111111111111111111111111111111111111")
    $ \address ->
  plet (resolveInputsNoReferenceDescriptorFor outputCbor address) $ \descriptorCbor ->
  plet (resolveInputsNoReferenceTerminalFor outputCbor address) $ \outputProof ->
  plet (inputSetsSingletonCommitment 2 outputCbor) $ \outputsCommitment ->
  pmatch (inputSetsSingletonProof 2 outputCbor) $ \(PPair collectionProof _) ->
  pmatch collectionProof $ \item ->
  plet
    (ScriptProof.poutputItemLeafHash # 0 # pfromData (BoundedCollection.pitemProof'itemCommitment item))
    $ \outputLeaf ->
  plet (cekSinglePeak outputLeaf) $ \outputPeaks ->
  pmatch
    ( phaseANativeControlFixtureWithOutputs
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        outputsCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet
    (scriptSourcesStageFiveCbor phaseControl 0 1 outputPeaks 1 pemptyReceivePurposeScanControl)
    $ \baseWorkCbor ->
  plet (pscriptSourcesStageZeroControlFromWitness # baseWorkCbor) $ \control ->
  plet
    (pencodeScriptSourcesOutputProofWitness # control # outputProof)
    $ \workCbor ->
  plet (scriptSourcesStateFromPhase transactionId phaseControl 91 workCbor) $ \pre ->
  plet (inputSetsExactRejection pre $ pconstant "E_MISSING_REQUIRED_WITNESS") $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \witness ->
  plet (pcon $ PEmptySignerSetProof $ pdata pnil) $ \signerProof ->
    pand'List
      [ LedgerOutputProof.pdescriptorIsExactV1
          # outputProof
          # (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor)
      , pstructuralTransitionIsValid # pre # witness
      , pverifyScriptSourcesOutputProofFinalizeSemanticsV1
          # pre # witness # descriptorCbor # signerProof
      ]

scriptSourcesStageZeroBeginTransition :: forall s. Term s PBool
scriptSourcesStageZeroBeginTransition =
  plet (phexByteStr "820340") $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  plet (scriptSourcesStageZeroCborFromPhase phaseControl 0) $ \workCbor ->
  plet (scriptSourcesStageZeroCborFromPhase phaseControl 1) $ \nextBaseCbor ->
  pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair collectionProof chunkProof) ->
  pmatch collectionProof $ \item ->
  pmatch chunkProof $ \chunk ->
  pmatch
    ( NativeScriptScan.pversionedScriptHeaderV1
        # pfromData (Bounded.pchunkProof'chunk chunk)
        # pfromData (BoundedCollection.pitemProof'itemLength item)
    )
    $ \case
      PNothing -> pconstant False
      PJust header -> pmatch header $ \h ->
        plet
          ( pcon $ PInlineSourceHashControlV1
              (pdata 1) (pdata 0) (pdata 1)
              (NativeScriptScan.pheader'languageTag h)
              (NativeScriptScan.pheader'payloadOffset h)
              (NativeScriptScan.pheader'payloadLength h)
              (BoundedCollection.pitemProof'itemLength item)
              (BoundedCollection.pitemProof'itemCommitment item)
              ( pdata $ Blake2b224.pinitialControlV1
                  # (pfromData (NativeScriptScan.pheader'payloadLength h) + 1)
              )
          )
          $ \pending ->
        plet (pencodeInlineSourceHashControlV1 # pending) $ \pendingCbor ->
        plet (pscriptSourcesStageZeroControlFromWitness # nextBaseCbor) $ \nextBase ->
        plet
          (pencodeScriptSourcesPendingSourceWitness # nextBase # pendingCbor)
          $ \nextWorkCbor ->
        plet (scriptSourcesStateFromPhase transactionId phaseControl 56 workCbor) $ \pre ->
        plet (scriptSourcesStateFromPhase transactionId phaseControl 57 nextWorkCbor) $ \post ->
        plet (scriptSourcesStateFromPhase transactionId phaseControl 57 nextBaseCbor) $ \wrongPost ->
        plet
          (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post))
          $ \witness ->
        plet
          (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata wrongPost))
          $ \wrongWitness ->
        plet
          ( pcon $ PTransactionFieldChunkWitness
              (pdata collectionProof) (pdata chunkProof)
          )
          $ \auxiliary ->
          pand'List
            [ pstructuralTransitionIsValid # pre # witness
            , pverifyScriptSourcesStageZeroBeginSemanticsV1
                # pre # witness # collectionProof # chunkProof
            , pverifyScriptSourcesStageZeroSemanticsV1
                # pre # witness # auxiliary
            , pnot
                # ( pverifyScriptSourcesStageZeroBeginSemanticsV1
                      # pre # wrongWitness # collectionProof # chunkProof
                  )
            , pnot
                # ( pverifyScriptSourcesStageZeroSemanticsV1
                      # pre # wrongWitness # auxiliary
                  )
            ]

scriptSourcesInlinePending :: forall s.
  Term s BoundedCollection.PItemProofV1 -> Term s NativeScriptScan.PVersionedScriptHeaderV1 ->
  Term s Blake2b224.PBlake2b224TraceControlV1 -> Term s PInlineSourceHashControlV1
scriptSourcesInlinePending collectionProof header hashControl =
  pmatch collectionProof $ \item ->
  pmatch header $ \h ->
    pcon $ PInlineSourceHashControlV1
      (pdata 1) (pdata 0) (pdata 1)
      (NativeScriptScan.pheader'languageTag h)
      (NativeScriptScan.pheader'payloadOffset h)
      (NativeScriptScan.pheader'payloadLength h)
      (BoundedCollection.pitemProof'itemLength item)
      (BoundedCollection.pitemProof'itemCommitment item)
      (pdata hashControl)

scriptSourcesPendingWorkCbor :: forall s.
  Term s PPhaseANativeScriptsControlV1 -> Term s PInlineSourceHashControlV1 ->
  Term s PByteString
scriptSourcesPendingWorkCbor phaseControl pending =
  plet (scriptSourcesStageZeroCborFromPhase phaseControl 1) $ \baseCbor ->
  plet (pscriptSourcesStageZeroControlFromWitness # baseCbor) $ \baseControl ->
    pencodeScriptSourcesPendingSourceWitness
      # baseControl # (pencodeInlineSourceHashControlV1 # pending)

scriptSourcesStageZeroHashTransitions :: forall s. Term s PBool
scriptSourcesStageZeroHashTransitions =
  plet (phexByteStr "820341aa") $ \itemCbor ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair collectionProof chunkProof) ->
  pmatch collectionProof $ \item ->
  pmatch chunkProof $ \chunk ->
  pmatch
    ( NativeScriptScan.pversionedScriptHeaderV1
        # pfromData (Bounded.pchunkProof'chunk chunk)
        # pfromData (BoundedCollection.pitemProof'itemLength item)
    )
    $ \case
      PNothing -> pconstant False
      PJust header -> pmatch header $ \h ->
        plet
          (Blake2b224.pinitialControlV1 # (pfromData (NativeScriptScan.pheader'payloadLength h) + 1))
          $ \initialHash ->
        plet (scriptSourcesInlinePending collectionProof (pcon h) initialHash) $ \initialPending ->
        pmatch
          (pinlineSourceHashBlockV1 # initialPending # chunkProof # pcon PDNothing)
          $ \case
            PNothing -> pconstant False
            PJust block ->
              pmatch (Blake2b224.pstepV1 # initialHash # pcon (PJust block)) $ \case
                PNothing -> pconstant False
                PJust roundHash ->
                  pmatch (Blake2b224.pstepV1 # roundHash # pcon PNothing) $ \case
                    PNothing -> pconstant False
                    PJust nextRoundHash ->
                      plet (scriptSourcesInlinePending collectionProof (pcon h) roundHash) $ \roundPending ->
                      plet (scriptSourcesInlinePending collectionProof (pcon h) nextRoundHash) $ \nextRoundPending ->
                      plet (scriptSourcesPendingWorkCbor phaseControl initialPending) $ \initialWork ->
                      plet (scriptSourcesPendingWorkCbor phaseControl roundPending) $ \roundWork ->
                      plet (scriptSourcesPendingWorkCbor phaseControl nextRoundPending) $ \nextRoundWork ->
                      plet
                        (scriptSourcesStateFromPhase transactionId phaseControl 58 initialWork)
                        $ \blockPre ->
                      plet
                        (scriptSourcesStateFromPhase transactionId phaseControl 59 roundWork)
                        $ \blockPost ->
                      plet
                        (pcon $ PValidationOneStepWitnessV1 (pdata initialWork) (pdata blockPost))
                        $ \blockWitness ->
                      plet
                        (scriptSourcesStateFromPhase transactionId phaseControl 60 roundWork)
                        $ \advancePre ->
                      plet
                        (scriptSourcesStateFromPhase transactionId phaseControl 61 nextRoundWork)
                        $ \advancePost ->
                      plet
                        (pcon $ PValidationOneStepWitnessV1 (pdata roundWork) (pdata advancePost))
                        $ \advanceWitness ->
                        pand'List
                          [ block #== phexByteStr "03aa"
                          , pstructuralTransitionIsValid # blockPre # blockWitness
                          , pverifyScriptSourcesStageZeroHashBlockSemanticsV1
                              # blockPre # blockWitness # chunkProof # pcon PDNothing
                          , pverifyScriptSourcesStageZeroSemanticsV1
                              # blockPre # blockWitness
                              # ( pcon $ PScriptSourceHashBlockWitness
                                    (pdata chunkProof) (pdata $ pcon PDNothing)
                                )
                          , pstructuralTransitionIsValid # advancePre # advanceWitness
                          , pverifyScriptSourcesStageZeroHashAdvanceSemanticsV1
                              # advancePre # advanceWitness
                          , pverifyScriptSourcesStageZeroSemanticsV1
                              # advancePre # advanceWitness # pcon PNoAuxiliaryWitness
                          ]

scriptSourcesStageZeroTerminalTransition :: forall s. Term s PBool
scriptSourcesStageZeroTerminalTransition =
  plet (phexByteStr "820341aa") $ \itemCbor ->
  plet (phexByteStr "03aa") $ \scriptBytes ->
  plet (pblake2b_224 # scriptBytes) $ \scriptHash ->
  plet (inputSetsSingletonCommitment 6 itemCbor) $ \scriptCommitment ->
  pmatch
    ( phaseANativeControlFixture
        scriptCommitment (-1) (-1) 0 0 0 0 0 (pconstant "") 0 0 (-1)
    )
    $ \(PPair phaseControl transactionId) ->
  pmatch (inputSetsSingletonProof 6 itemCbor) $ \(PPair collectionProof chunkProof) ->
  pmatch collectionProof $ \item ->
  pmatch chunkProof $ \chunk ->
  pmatch
    ( NativeScriptScan.pversionedScriptHeaderV1
        # pfromData (Bounded.pchunkProof'chunk chunk)
        # pfromData (BoundedCollection.pitemProof'itemLength item)
    )
    $ \case
      PNothing -> pconstant False
      PJust header -> pmatch header $ \h ->
        plet
          ( pcon $ Blake2b224.PBlake2b224TraceControlV1
              (pdata Blake2b224.pblake2b224TraceVersion)
              (pdata Blake2b224.pstageTerminal)
              (pdata 2) (pdata 2)
              (pdata $ scriptHash <> (preplicateBS # 36 # (pintegerToByte # 0)))
              (pdata $ pconstant "") (pdata 0)
              (pdata $ pconstant "") (pdata 0)
          )
          $ \terminalHash ->
        plet (scriptSourcesInlinePending collectionProof (pcon h) terminalHash) $ \pending ->
        plet (scriptSourcesPendingWorkCbor phaseControl pending) $ \workCbor ->
        plet
          ( ScriptProof.pinlineSourceLeafHash
              # 0 # pfromData (NativeScriptScan.pheader'languageTag h)
              # scriptHash
              # pfromData (BoundedCollection.pitemProof'itemLength item)
              # pfromData (BoundedCollection.pitemProof'itemCommitment item)
          )
          $ \sourceLeaf ->
        plet (Merkle.pappendLeaf # 0 # pnil # sourceLeaf) $ \sourcePeaks ->
        plet
          (scriptSourcesStageZeroCborFromPhaseWithFrontier phaseControl 1 sourcePeaks 1)
          $ \nextWorkCbor ->
        plet (scriptSourcesStateFromPhase transactionId phaseControl 62 workCbor) $ \pre ->
        plet (scriptSourcesStateFromPhase transactionId phaseControl 63 nextWorkCbor) $ \post ->
        plet
          (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post))
          $ \witness ->
          pstructuralTransitionIsValid # pre # witness
            #&& pverifyScriptSourcesStageZeroHashTerminalSemanticsV1 # pre # witness
            #&& pverifyScriptSourcesStageZeroSemanticsV1
              # pre # witness # pcon PNoAuxiliaryWitness

resolveInputsWithSchedule :: forall s.
  Term s PResolveInputsControlV1 -> Term s PByteString ->
  Term s PResolveInputsControlV1
resolveInputsWithSchedule control scheduleHash = pmatch control $ \c ->
  pcon $ PResolveInputsControlV1
    (presolveInputs'compactCbor c)
    (presolveInputs'witnessSetCompactCbor c)
    (presolveInputs'fieldPreimageLengthsCbor c)
    (presolveInputs'contextCbor c)
    (presolveInputs'cursor c)
    (presolveInputs'accumulator c)
    (pdata scheduleHash)
    (presolveInputs'signerCount c)
    (presolveInputs'signerFrontierCommitment c)
    (presolveInputs'pending c)
    (pdata scheduleHash)

resolveInputsWithPendingProof :: forall s.
  Term s PResolveInputsControlV1 -> Term s PResolveInputOutputProofV1 ->
  Term s PResolveInputsControlV1
resolveInputsWithPendingProof control pending = pmatch control $ \c ->
  pcon $ PResolveInputsControlV1
    (presolveInputs'compactCbor c)
    (presolveInputs'witnessSetCompactCbor c)
    (presolveInputs'fieldPreimageLengthsCbor c)
    (presolveInputs'contextCbor c)
    (presolveInputs'cursor c)
    (presolveInputs'accumulator c)
    (presolveInputs'remainingScheduleHash c)
    (presolveInputs'signerCount c)
    (presolveInputs'signerFrontierCommitment c)
    (pdata $ pcon $ PDJust $ pdata pending)
    (presolveInputs'resolutionScheduleHash c)

resolveInputsStateFixtureAtRoot :: forall s.
  Term s PByteString -> Term s PResolveInputsControlV1 -> Term s PInteger ->
  Term s PByteString -> Term s PValidationMachineStateV1
resolveInputsStateFixtureAtRoot transactionId control counter priorRoot =
  pmatch (resolveInputsStateFixture transactionId control counter) $ \state ->
    pcon $ PValidationMachineStateV1
      (pmachineState'machineVersion state)
      (pmachineState'eventKeyHash state)
      (pmachineState'transactionId state)
      (pmachineState'transactionCommitment state)
      (pmachineState'validationContextHash state)
      (pmachineState'sourceKind state)
      (pdata priorRoot)
      (pmachineState'phase state)
      (pmachineState'programCounter state)
      (pmachineState'workRoot state)
      (pmachineState'executionCpu state)
      (pmachineState'executionMemory state)
      (pmachineState'verdict state)
      (pmachineState'rejectionCodeHash state)
      (pmachineState'ledgerDeltaRoot state)

resolveInputsControlCbor :: forall s.
  Term s PResolveInputsControlV1 -> Term s PByteString
resolveInputsControlCbor control = pmatch control $ \c ->
  pencodeResolveInputsWitness
    # pfromData (presolveInputs'compactCbor c)
    # pfromData (presolveInputs'witnessSetCompactCbor c)
    # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
    # pfromData (presolveInputs'contextCbor c)
    # pfromData (presolveInputs'cursor c)
    # pfromData (presolveInputs'accumulator c)
    # pfromData (presolveInputs'remainingScheduleHash c)
    # pfromData (presolveInputs'signerCount c)
    # pfromData (presolveInputs'signerFrontierCommitment c)
    # pfromData (presolveInputs'pending c)
    # pfromData (presolveInputs'resolutionScheduleHash c)

resolveInputsLookupOpeningTransitions :: forall s. Term s PBool
resolveInputsLookupOpeningTransitions =
  plet
    ( pencodeMidgardTxInput
        # pcon (PMidgardTxInput (pdata $ cekHash 0xa1) (pdata 0))
    )
    $ \key ->
  plet (cekDescriptorForIndex 0) $ \descriptorCbor ->
  plet (presolutionScheduleNodeHash # 0 # key # pemptyResolutionScheduleHash) $ \scheduleHash ->
  pmatch (resolveInputsInitialControlFixture (-1)) $ \(PPair initialControl transactionId) ->
  plet (resolveInputsWithSchedule (resolveInputsAdvanceControl initialControl) scheduleHash) $ \control ->
  pmatch
    ( MpfProof.pinsertRoot
        # pnull_hash # key # descriptorCbor # (pcon $ PProof pnil)
    )
    $ \case
      PNothing -> pconstant False
      PJust singletonRoot ->
        plet (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
        pmatch descriptor $ \output ->
        plet
          ( pcon $ PResolveInputOutputProofV1
              (pdata 0) (pdata key) (pdata pemptyResolutionScheduleHash)
              (pdata descriptorCbor)
              ( pdata $ LedgerOutputProof.pinitialControlV1
                  # pfromData (OutputCommitment.poutputCommitment'outputIndex output)
                  # pfromData (OutputCommitment.poutputCommitment'totalLength output)
                  # pfromData (OutputCommitment.poutputCommitment'itemCommitment output)
              )
          )
          $ \pending ->
        plet (resolveInputsStateFixtureAtRoot transactionId control 46 singletonRoot) $ \memberPre ->
        plet
          ( resolveInputsStateFixtureAtRoot transactionId
              (resolveInputsWithPendingProof control pending) 47 singletonRoot
          )
          $ \memberPost ->
        plet
          ( pcon $ PValidationOneStepWitnessV1
              (pdata $ resolveInputsControlCbor control) (pdata memberPost)
          )
          $ \memberWitness ->
        plet (resolveInputsStateFixtureAtRoot transactionId control 48 pnull_hash) $ \absentPre ->
        plet (inputSetsExactRejection absentPre $ pconstant "E_INPUT_NOT_FOUND") $ \absentPost ->
        plet
          ( pcon $ PValidationOneStepWitnessV1
              (pdata $ resolveInputsControlCbor control) (pdata absentPost)
          )
          $ \absentWitness ->
          pand'List
            [ pstructuralTransitionIsValid # memberPre # memberWitness
            , pverifyResolveInputsMembershipBeginSemanticsV1
                # memberPre # memberWitness # 0 # key # pemptyResolutionScheduleHash
                # descriptorCbor # (pcon $ PProof pnil) # pcon PNoSignerSetProof
            , pstructuralTransitionIsValid # absentPre # absentWitness
            , pverifyResolveInputsNonMembershipSemanticsV1
                # absentPre # absentWitness # 0 # key # pemptyResolutionScheduleHash
                # (pcon $ PProof pnil)
            ]

resolveInputsOutputCbor :: forall s. Term s PByteString
resolveInputsOutputCbor =
  phexByteStr "a200581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a142223307"

resolveInputsOutputChunkProof :: forall s. Term s Bounded.PChunkProofV1
resolveInputsOutputChunkProof =
  plet
    ( Bounded.phashChunk
        # OutputCommitment.poutputFieldIndex # 0 # 0 # resolveInputsOutputCbor
    )
    $ \leaf ->
  plet (Merkle.pappendLeaf # 0 # Merkle.pemptyFrontier # leaf) $ \frontier ->
    pcon $ Bounded.PChunkProofV1
      (pdata Bounded.pversion)
      (pdata OutputCommitment.poutputFieldIndex)
      (pdata 0)
      (pdata $ plengthBS # resolveInputsOutputCbor)
      (pdata 0)
      (pdata resolveInputsOutputCbor)
      (pdata frontier)
      (pdata pnil)

resolveInputsMembershipStepTransition :: forall s. Term s PBool
resolveInputsMembershipStepTransition =
  plet
    ( pencodeMidgardTxInput
        # pcon (PMidgardTxInput (pdata $ cekHash 0xa1) (pdata 0))
    )
    $ \key ->
  plet (cekDescriptorForIndex 0) $ \descriptorCbor ->
  plet (presolutionScheduleNodeHash # 0 # key # pemptyResolutionScheduleHash) $ \scheduleHash ->
  pmatch (resolveInputsInitialControlFixture (-1)) $ \(PPair initialControl transactionId) ->
  plet (resolveInputsWithSchedule (resolveInputsAdvanceControl initialControl) scheduleHash) $ \control ->
  plet (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
  pmatch descriptor $ \output ->
  plet
    ( LedgerOutputProof.pinitialControlV1
        # pfromData (OutputCommitment.poutputCommitment'outputIndex output)
        # pfromData (OutputCommitment.poutputCommitment'totalLength output)
        # pfromData (OutputCommitment.poutputCommitment'itemCommitment output)
    )
    $ \initialOutputProof ->
  plet
    ( pcon $ PResolveInputOutputProofV1
        (pdata 0) (pdata key) (pdata pemptyResolutionScheduleHash)
        (pdata descriptorCbor) (pdata initialOutputProof)
    )
    $ \pending ->
  plet
    ( pcon $ LedgerOutputProof.PLedgerOutputProofChunks
        (pdata resolveInputsOutputChunkProof) (pdata $ pcon PDNothing)
    )
    $ \proofWitness ->
  pmatch (LedgerOutputProof.pstructureStep # initialOutputProof # proofWitness) $ \case
    PNothing -> pconstant False
    PJust result -> pmatch result $ \case
      LedgerOutputProof.PLedgerOutputProofAdvanced nextOutputProofD ->
        plet
          ( pcon $ PResolveInputOutputProofV1
              (pdata 0) (pdata key) (pdata pemptyResolutionScheduleHash)
              (pdata descriptorCbor) nextOutputProofD
          )
          $ \nextPending ->
        plet (resolveInputsWithPendingProof control pending) $ \activeControl ->
        plet (resolveInputsWithPendingProof control nextPending) $ \nextControl ->
        plet (resolveInputsStateFixture transactionId activeControl 50) $ \pre ->
        plet (resolveInputsStateFixture transactionId nextControl 51) $ \post ->
        plet
          ( pcon $ PValidationOneStepWitnessV1
              (pdata $ resolveInputsControlCbor activeControl) (pdata post)
          )
          $ \witness ->
          pstructuralTransitionIsValid # pre # witness
            #&& presolveInputsControlIsBound # pre # witness # activeControl
            #&& presolveMembershipProofResult
              # pre # witness # activeControl # pending
              # (pcon $ LedgerOutputProof.PLedgerOutputProofAdvanced nextOutputProofD)
      _ -> pconstant False

resolveInputsNoReferenceOutput :: forall s. Term s PByteString
resolveInputsNoReferenceOutput =
  phexByteStr "a200581d7811111111111111111111111111111111111111111111111111111111018200a0"

resolveInputsNoReferenceDescriptor :: forall s. Term s PByteString
resolveInputsNoReferenceDescriptor =
  resolveInputsNoReferenceDescriptorFor
    resolveInputsNoReferenceOutput
    (phexByteStr "7811111111111111111111111111111111111111111111111111111111")

resolveInputsNoReferenceDescriptorFor :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PByteString
resolveInputsNoReferenceDescriptorFor outputCbor address =
  pmatch (ScriptContext.ptxOutSummaryV1 # outputCbor # pconstant False) $ \case
    PNothing -> perror
    PJust cardano ->
      pmatch (ScriptContext.ptxOutSummaryV1 # outputCbor # pconstant True) $ \case
        PNothing -> perror
        PJust midgard ->
          pmatch (ScriptContext.pspendDatumSummaryV1 # outputCbor) $ \case
            PNothing -> perror
            PJust spend ->
              OutputCommitment.pencodeLedgerOutputCommitment
                # ( pcon $ OutputCommitment.PLedgerOutputCommitmentV1
                      (pdata OutputCommitment.pledgerOutputCommitmentVersion)
                      (pdata 0)
                      (pdata $ plengthBS # outputCbor)
                      (pdata $ Bounded.pfromBytes # 2 # 0 # outputCbor)
                      (pdata address)
                      (pdata 0)
                      (pdata 0)
                      (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
                      (pdata 1)
                      (pdata $ -1)
                      (pdata $ pconstant "")
                      (pdata 0)
                      (pdata $ pconstant "")
                      (pdata cardano)
                      (pdata midgard)
                      (pdata spend)
                  )

resolveInputsNoReferenceTerminal :: forall s. Term s LedgerOutputProof.PLedgerOutputProofControlV1
resolveInputsNoReferenceTerminal =
  resolveInputsNoReferenceTerminalFor
    resolveInputsNoReferenceOutput
    (phexByteStr "7811111111111111111111111111111111111111111111111111111111")

resolveInputsNoReferenceTerminalFor :: forall s.
  Term s PByteString -> Term s PByteString ->
  Term s LedgerOutputProof.PLedgerOutputProofControlV1
resolveInputsNoReferenceTerminalFor outputCbor address =
  plet
    ( pcon $ LedgerOutputScan.PLedgerOutputScanControlV1
        (pdata LedgerOutputScan.pversion) (pdata LedgerOutputScan.pstageTerminal)
        (pdata 37) (pdata 2) (pdata 0)
        (pdata address)
        (pdata 0) (pdata 1) (pdata 0) (pdata 0) (pdata 0)
        (pdata $ pconstant "") (pdata $ pconstant "") (pdata $ pconstant "")
        (pdata 0) (pdata pnil) (pdata $ -1) (pdata 0) (pdata 0)
        (pdata $ -1) (pdata $ -1) (pdata $ -1) (pdata 0)
    )
    $ \scan ->
  plet
    ( pcon $ LedgerOutputValue.PLedgerOutputValueControlV1
        (pdata LedgerOutputValue.pversion) (pdata LedgerOutputValue.pstageTerminal)
        (pdata 0) (pdata $ pconstant "")
        (pdata CekData.pemptyDataPairSummaryV1)
        (pdata CekData.pemptyDataPairSummaryV1)
        ( pdata $ pcon $ PDJust $ pdata
            $ CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1
        )
    )
    $ \value ->
    pcon $ LedgerOutputProof.PLedgerOutputProofControlV1
      (pdata LedgerOutputProof.pversion) (pdata LedgerOutputProof.pstageTerminal)
      (pdata 0) (pdata $ plengthBS # outputCbor)
      (pdata $ Bounded.pfromBytes # 2 # 0 # outputCbor)
      (pdata scan)
      (pdata $ pcon $ PDJust $ pdata value)
      (pdata $ pcon PDNothing)
      (pdata 0) (pdata pnil)
      (pdata $ pcon PDNothing) (pdata $ pcon PDNothing)

resolveInputsAfterFinalizedProof :: forall s.
  Term s PResolveInputsControlV1 -> Term s PByteString -> Term s PByteString ->
  Term s PResolveInputsControlV1
resolveInputsAfterFinalizedProof control key descriptorCbor = pmatch control $ \c ->
  pcon $ PResolveInputsControlV1
    (presolveInputs'compactCbor c)
    (presolveInputs'witnessSetCompactCbor c)
    (presolveInputs'fieldPreimageLengthsCbor c)
    (presolveInputs'contextCbor c)
    (pdata $ pfromData (presolveInputs'cursor c) + 1)
    ( pdata $ presolvedInputAccumulatorSuccessor
        # pfromData (presolveInputs'accumulator c) # 0 # key # descriptorCbor
    )
    (pdata pemptyResolutionScheduleHash)
    (presolveInputs'signerCount c)
    (presolveInputs'signerFrontierCommitment c)
    (pdata $ pcon PDNothing)
    (presolveInputs'resolutionScheduleHash c)

resolveInputsMembershipFinalizeTransition :: forall s. Term s PBool
resolveInputsMembershipFinalizeTransition =
  plet
    ( pencodeMidgardTxInput
        # pcon (PMidgardTxInput (pdata $ cekHash 0xa1) (pdata 0))
    )
    $ \key ->
  plet resolveInputsNoReferenceDescriptor $ \descriptorCbor ->
  plet (presolutionScheduleNodeHash # 0 # key # pemptyResolutionScheduleHash) $ \scheduleHash ->
  pmatch (resolveInputsInitialControlFixture (-1)) $ \(PPair initialControl transactionId) ->
  plet (resolveInputsWithSchedule (resolveInputsAdvanceControl initialControl) scheduleHash) $ \control ->
  plet
    ( pcon $ PResolveInputOutputProofV1
        (pdata 0) (pdata key) (pdata pemptyResolutionScheduleHash)
        (pdata descriptorCbor) (pdata resolveInputsNoReferenceTerminal)
    )
    $ \pending ->
  plet (resolveInputsWithPendingProof control pending) $ \activeControl ->
  plet (resolveInputsAfterFinalizedProof control key descriptorCbor) $ \nextControl ->
  plet (resolveInputsStateFixture transactionId activeControl 52) $ \pre ->
  plet (resolveInputsStateFixture transactionId nextControl 53) $ \post ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        (pdata $ resolveInputsControlCbor activeControl) (pdata post)
    )
    $ \witness ->
    pand'List
      [ LedgerOutputProof.pdescriptorIsExactV1
          # resolveInputsNoReferenceTerminal
          # (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor)
      , pstructuralTransitionIsValid # pre # witness
      , pverifyResolveInputsMembershipFinalizeSemanticsV1
          # pre # witness # descriptorCbor # pcon PNoSignerSetProof
      ]

ledgerDeltaControlFixture :: forall s.
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PByteString ->
  Term s PLedgerDeltaControlV1
ledgerDeltaControlFixture
  stage resolvedCount resolvedAccumulator outputCount outputPeaks replaySchedule
  replayCursor replayAccumulator replayRemaining currentRoot outputCursor
  operationCount operationPeaks pendingCbor =
    pcon $ PLedgerDeltaControlV1
      (pdata resolvedCount)
      (pdata resolvedAccumulator)
      (pdata outputCount)
      (pdata outputPeaks)
      (pdata stage)
      (pdata replaySchedule)
      (pdata replayCursor)
      (pdata replayAccumulator)
      (pdata replayRemaining)
      (pdata currentRoot)
      (pdata outputCursor)
      (pdata operationCount)
      (pdata operationPeaks)
      (pdata pendingCbor)

ledgerDeltaBaseControl :: forall s.
  Term s PInteger -> Term s PByteString -> Term s PLedgerDeltaControlV1
ledgerDeltaBaseControl stage pendingCbor =
  ledgerDeltaControlFixture
    stage 0 pinitialResolutionAccumulator 0 pnil pemptyResolutionScheduleHash
    0 pinitialResolutionAccumulator pemptyResolutionScheduleHash (cekHash 5)
    0 0 pnil pendingCbor

ledgerDeltaStateFixture :: forall s.
  Term s PLedgerDeltaControlV1 ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PValidationMachineStateV1
ledgerDeltaStateFixture control counter deltaRoot =
  pcon $ PValidationMachineStateV1
    (pdata pmachineVersion)
    (pdata $ cekHash 1)
    (pdata $ cekHash 2)
    (pdata $ cekHash 3)
    (pdata $ cekHash 4)
    (pdata $ pcon PNormal)
    (pdata $ cekHash 5)
    (pdata $ pcon PLedgerDelta)
    (pdata counter)
    (pdata $ phashWorkWitness # pcon PLedgerDelta # counter # (pencodeLedgerDeltaControlV1 # control))
    (pdata 0)
    (pdata 0)
    (pdata $ pcon PPending)
    (pdata $ cekHash 0)
    (pdata deltaRoot)

ledgerDeltaSuccessorFixture :: forall s.
  Term s PValidationMachineStateV1 ->
  Term s PLedgerDeltaControlV1 ->
  Term s PValidationMachineStateV1
ledgerDeltaSuccessorFixture pre control = pmatch pre $ \state ->
  pcon $ PValidationMachineStateV1
    (pmachineState'machineVersion state)
    (pmachineState'eventKeyHash state)
    (pmachineState'transactionId state)
    (pmachineState'transactionCommitment state)
    (pmachineState'validationContextHash state)
    (pmachineState'sourceKind state)
    (pmachineState'priorLedgerRoot state)
    (pdata $ pcon PLedgerDelta)
    (pdata $ pfromData (pmachineState'programCounter state) + 1)
    ( pdata $
        phashWorkWitness
          # pcon PLedgerDelta
          # (pfromData (pmachineState'programCounter state) + 1)
          # (pencodeLedgerDeltaControlV1 # control)
    )
    (pmachineState'executionCpu state)
    (pmachineState'executionMemory state)
    (pmachineState'verdict state)
    (pmachineState'rejectionCodeHash state)
    (pmachineState'ledgerDeltaRoot state)

ledgerDeltaWitnessFixture :: forall s.
  Term s PLedgerDeltaControlV1 ->
  Term s PValidationMachineStateV1 ->
  Term s PValidationOneStepWitnessV1
ledgerDeltaWitnessFixture control post =
  pcon $ PValidationOneStepWitnessV1
    (pdata $ pencodeLedgerDeltaControlV1 # control)
    (pdata post)

ledgerDeltaPendingFixture :: forall s.
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s ProofFold.PProofDescriptorV1 ->
  Term s ProofFold.PProofFoldControlV1 ->
  Term s PLedgerDeltaPendingMutationV1
ledgerDeltaPendingFixture authorization kind key value descriptor foldControl =
  pcon $ PLedgerDeltaPendingMutationV1
    (pdata authorization)
    (pdata kind)
    (pdata key)
    (pdata value)
    (pdata descriptor)
    (pdata foldControl)

ledgerDeltaEmptyFold :: forall s. Term s ProofFold.PProofFoldControlV1
ledgerDeltaEmptyFold = pcon $ ProofFold.PProofFoldControlV1
  (pdata (-1)) (pdata 0) (pdata $ pconstant "") (pdata $ pconstant "")

ledgerDeltaOperationTransitions :: forall s. Term s PBool
ledgerDeltaOperationTransitions =
  ledgerDeltaOperationTransition 0 #&& ledgerDeltaOperationTransition 1

ledgerDeltaOperationTransition :: forall s. Integer -> Term s PBool
ledgerDeltaOperationTransition kind =
  let stage = if kind == 0 then 0 else 1
      key = if kind == 0
        then phexByteStr "64656c6574652d6b6579"
        else pencodeMidgardTxInput # pcon (PMidgardTxInput (pdata $ cekHash 2) (pdata 0))
      value = if kind == 0 then pconstant "" else phexByteStr "696e736572742d76616c7565"
   in plet pemptyProofDescriptor $ \descriptor ->
      plet (pledgerDeltaOperationLeafHash # pconstant kind # key # value # descriptor) $ \leaf ->
      plet (Merkle.pbuildFrontier #$ pcons # pdata leaf # pnil) $ \frontier ->
      pmatch frontier $ \built ->
      plet (ledgerDeltaBaseControl (pconstant stage) (pconstant "")) $ \control ->
      plet
        ( ledgerDeltaPendingFixture 0 (pconstant kind) key value descriptor ledgerDeltaEmptyFold )
        $ \pending ->
      plet
        ( ledgerDeltaControlFixture
            (pconstant stage) 0 pinitialResolutionAccumulator 0 pnil
            pemptyResolutionScheduleHash 0 pinitialResolutionAccumulator
            pemptyResolutionScheduleHash (cekHash 5) 0 0 pnil
            (pencodeLedgerDeltaPendingMutationV1 # pending)
        )
        $ \nextControl ->
      plet
        ( ledgerDeltaStateFixture control 90
            (Merkle.pfrontierCommitment # Merkle.pbuiltFrontier'count built # Merkle.pbuiltFrontier'peaks built)
        )
        $ \pre ->
      plet (ledgerDeltaSuccessorFixture pre nextControl) $ \post ->
      plet (ledgerDeltaWitnessFixture control post) $ \witness ->
      plet
        ( pcon $ PLedgerDeltaOperationProofV1
            (pdata descriptor)
            (pdata $ Merkle.pbuiltFrontier'count built)
            (pdata $ Merkle.pbuiltFrontier'peaks built)
            (pdata 0)
            (pdata pnil)
        )
        $ \proof ->
        pverifyLedgerDeltaOperationSemanticsV1
          # pre # witness # pconstant kind # key # value # proof

ledgerDeltaReplayTransitions :: forall s. Term s PBool
ledgerDeltaReplayTransitions =
  ledgerDeltaReferenceReplay #&& ledgerDeltaSpendReplay

ledgerDeltaReferenceReplay :: forall s. Term s PBool
ledgerDeltaReferenceReplay =
  plet
    (pencodeMidgardTxInput # pcon (PMidgardTxInput (pdata $ cekHash 2) (pdata 0)))
    $ \key ->
  plet (cekDescriptorForIndex 0) $ \value ->
  plet (presolutionScheduleNodeHash # 1 # key # pemptyResolutionScheduleHash) $ \scheduleHash ->
  plet
    ( presolvedInputAccumulatorSuccessor
        # pinitialResolutionAccumulator # 1 # key # value
    )
    $ \resolvedAccumulator ->
  plet
    ( ledgerDeltaControlFixture
        0 1 resolvedAccumulator 0 pnil scheduleHash 0 pinitialResolutionAccumulator
        scheduleHash (cekHash 5) 0 0 pnil (pconstant "")
    )
    $ \control ->
  plet
    ( ledgerDeltaControlFixture
        0 1 resolvedAccumulator 0 pnil scheduleHash 1 resolvedAccumulator
        pemptyResolutionScheduleHash (cekHash 5) 0 0 pnil (pconstant "")
    )
    $ \nextControl ->
  plet (ledgerDeltaStateFixture control 91 (Merkle.pfrontierCommitment # 0 # pnil)) $ \pre ->
  plet (ledgerDeltaWitnessFixture control $ ledgerDeltaSuccessorFixture pre nextControl) $ \witness ->
    pverifyLedgerDeltaReplaySemanticsV1
      # pre # witness # 1 # key # pemptyResolutionScheduleHash # value

ledgerDeltaSpendReplay :: forall s. Term s PBool
ledgerDeltaSpendReplay =
  plet
    (pencodeMidgardTxInput # pcon (PMidgardTxInput (pdata $ cekHash 2) (pdata 0)))
    $ \key ->
  plet (cekDescriptorForIndex 0) $ \value ->
  plet pemptyProofDescriptor $ \descriptor ->
  pmatch (ProofFold.pinitialFoldControlV1 # key # value # descriptor) $ \case
    PNothing -> pconstant False
    PJust foldControl ->
      plet (presolutionScheduleNodeHash # 0 # key # pemptyResolutionScheduleHash) $ \scheduleHash ->
      plet
        ( presolvedInputAccumulatorSuccessor
            # pinitialResolutionAccumulator # 0 # key # value
        )
        $ \resolvedAccumulator ->
      plet
        (ledgerDeltaPendingFixture 0 0 key (pconstant "") descriptor ledgerDeltaEmptyFold)
        $ \pending ->
      plet
        (ledgerDeltaPendingFixture 1 0 key value descriptor foldControl)
        $ \nextPending ->
      plet
        ( ledgerDeltaControlFixture
            0 1 resolvedAccumulator 0 pnil scheduleHash 0 pinitialResolutionAccumulator
            scheduleHash
            (pmatch foldControl $ \fold -> pfromData $ ProofFold.pfoldControl'includingRoot fold)
            0 0 pnil (pencodeLedgerDeltaPendingMutationV1 # pending)
        )
        $ \control ->
      plet
        ( ledgerDeltaControlFixture
            0 1 resolvedAccumulator 0 pnil scheduleHash 1 resolvedAccumulator
            pemptyResolutionScheduleHash
            (pmatch foldControl $ \fold -> pfromData $ ProofFold.pfoldControl'includingRoot fold)
            0 0 pnil (pencodeLedgerDeltaPendingMutationV1 # nextPending)
        )
        $ \nextControl ->
      plet (ledgerDeltaStateFixture control 92 (Merkle.pfrontierCommitment # 0 # pnil)) $ \pre ->
      plet (ledgerDeltaWitnessFixture control $ ledgerDeltaSuccessorFixture pre nextControl) $ \witness ->
        pverifyLedgerDeltaReplaySemanticsV1
          # pre # witness # 0 # key # pemptyResolutionScheduleHash # value

ledgerDeltaPhaseHandoffs :: forall s. Term s PBool
ledgerDeltaPhaseHandoffs =
  ledgerDeltaPhaseHandoff 0 1 #&& ledgerDeltaPhaseHandoff 1 2

ledgerDeltaPhaseHandoff :: forall s. Integer -> Integer -> Term s PBool
ledgerDeltaPhaseHandoff stage nextStage =
  plet (ledgerDeltaBaseControl (pconstant stage) (pconstant "")) $ \control ->
  plet (ledgerDeltaBaseControl (pconstant nextStage) (pconstant "")) $ \nextControl ->
  plet (ledgerDeltaStateFixture control (pconstant $ 93 + stage) (Merkle.pfrontierCommitment # 0 # pnil)) $ \pre ->
  plet (ledgerDeltaWitnessFixture control $ ledgerDeltaSuccessorFixture pre nextControl) $ \witness ->
    if stage == 0
      then pverifyLedgerDeltaReplayFinishSemanticsV1 # pre # witness
      else pverifyLedgerDeltaOutputFinishSemanticsV1 # pre # witness

ledgerDeltaOutputTransition :: forall s. Term s PBool
ledgerDeltaOutputTransition =
  plet
    (pencodeMidgardTxInput # pcon (PMidgardTxInput (pdata $ cekHash 2) (pdata 0)))
    $ \key ->
  plet (cekDescriptorForIndex 0) $ \value ->
  plet pemptyProofDescriptor $ \descriptor ->
  pmatch (ProofFold.pinitialFoldControlV1 # key # value # descriptor) $ \case
    PNothing -> pconstant False
    PJust foldControl ->
      plet (ScriptProof.poutputDescriptorLeafHash # 0 # value) $ \descriptorLeaf ->
      plet (Merkle.pbuildFrontier #$ pcons # pdata descriptorLeaf # pnil) $ \frontier ->
      pmatch frontier $ \built ->
      plet (ledgerDeltaPendingFixture 0 1 key value descriptor ledgerDeltaEmptyFold) $ \pending ->
      plet (ledgerDeltaPendingFixture 1 1 key value descriptor foldControl) $ \nextPending ->
      plet
        ( ledgerDeltaControlFixture
            1 0 pinitialResolutionAccumulator 1 (Merkle.pbuiltFrontier'peaks built)
            pemptyResolutionScheduleHash 0 pinitialResolutionAccumulator
            pemptyResolutionScheduleHash pnull_hash 0 0 pnil
            (pencodeLedgerDeltaPendingMutationV1 # pending)
        )
        $ \control ->
      plet
        ( ledgerDeltaControlFixture
            1 0 pinitialResolutionAccumulator 1 (Merkle.pbuiltFrontier'peaks built)
            pemptyResolutionScheduleHash 0 pinitialResolutionAccumulator
            pemptyResolutionScheduleHash pnull_hash 1 0 pnil
            (pencodeLedgerDeltaPendingMutationV1 # nextPending)
        )
        $ \nextControl ->
      plet (ledgerDeltaStateFixture control 95 (Merkle.pfrontierCommitment # 0 # pnil)) $ \pre ->
      plet (ledgerDeltaWitnessFixture control $ ledgerDeltaSuccessorFixture pre nextControl) $ \witness ->
        pverifyLedgerDeltaOutputSemanticsV1 # pre # witness # 0 # value # pnil

ledgerDeltaProofFrameTransition :: forall s. Term s PBool
ledgerDeltaProofFrameTransition =
  plet (phexByteStr "616273656e742d6b6579") $ \key ->
  plet (phexByteStr "696e7365727465642d76616c7565") $ \value ->
  plet (cekHash 1 <> cekHash 2 <> cekHash 3 <> cekHash 0) $ \neighbors ->
  plet
    ( pcon $ ProofFold.PProofFrameV1
        (pdata 1) (pdata 0) (pdata 0) (pdata 1)
        (pdata $ pcon $ PBranch (pdata 0) (pdata neighbors))
    )
    $ \frame ->
  plet (Merkle.pbuildFrontier #$ pcons # pdata (ProofFold.pproofFrameLeafHashV1 # frame) # pnil) $ \frontier ->
  pmatch frontier $ \built ->
  plet
    ( pcon $ ProofFold.PProofDescriptorV1
        (pdata 1) (pdata 1) (pdata 1) (pdata $ Merkle.pbuiltFrontier'peaks built)
    )
    $ \descriptor ->
  pmatch (ProofFold.pinitialFoldControlV1 # key # value # descriptor) $ \case
    PNothing -> pconstant False
    PJust foldControl ->
      pmatch (ProofFold.pfoldProofFrameV1 # key # descriptor # foldControl # frame # pnil) $ \case
        PNothing -> pconstant False
        PJust nextFoldControl ->
          plet (ledgerDeltaPendingFixture 1 1 key value descriptor foldControl) $ \pending ->
          plet (ledgerDeltaPendingFixture 1 1 key value descriptor nextFoldControl) $ \nextPending ->
          plet
            ( ledgerDeltaControlFixture
                1 0 pinitialResolutionAccumulator 0 pnil pemptyResolutionScheduleHash
                0 pinitialResolutionAccumulator pemptyResolutionScheduleHash pnull_hash
                0 0 pnil (pencodeLedgerDeltaPendingMutationV1 # pending)
            )
            $ \control ->
          plet
            ( ledgerDeltaControlFixture
                1 0 pinitialResolutionAccumulator 0 pnil pemptyResolutionScheduleHash
                0 pinitialResolutionAccumulator pemptyResolutionScheduleHash pnull_hash
                0 0 pnil (pencodeLedgerDeltaPendingMutationV1 # nextPending)
            )
            $ \nextControl ->
          plet (ledgerDeltaStateFixture control 96 (Merkle.pfrontierCommitment # 0 # pnil)) $ \pre ->
          plet (ledgerDeltaWitnessFixture control $ ledgerDeltaSuccessorFixture pre nextControl) $ \witness ->
            pverifyLedgerDeltaProofFrameSemanticsV1 # pre # witness # frame # pnil

ledgerDeltaFinalTransitions :: forall s. Term s PBool
ledgerDeltaFinalTransitions =
  ledgerDeltaFinalizeTransition #&& ledgerDeltaTerminalTransition

ledgerDeltaFinalizeTransition :: forall s. Term s PBool
ledgerDeltaFinalizeTransition =
  plet (phexByteStr "696e736572742d6b6579") $ \key ->
  plet (phexByteStr "696e7365727465642d76616c7565") $ \value ->
  plet pemptyProofDescriptor $ \descriptor ->
  pmatch (ProofFold.pinitialFoldControlV1 # key # value # descriptor) $ \case
    PNothing -> pconstant False
    PJust foldControl ->
      pmatch foldControl $ \fold ->
      plet (ledgerDeltaPendingFixture 1 1 key value descriptor foldControl) $ \pending ->
      plet (pledgerDeltaOperationLeafHash # 1 # key # value # descriptor) $ \operationLeaf ->
      plet (Merkle.pbuildFrontier #$ pcons # pdata operationLeaf # pnil) $ \frontier ->
      pmatch frontier $ \built ->
      plet
        ( ledgerDeltaControlFixture
            1 0 pinitialResolutionAccumulator 0 pnil pemptyResolutionScheduleHash
            0 pinitialResolutionAccumulator pemptyResolutionScheduleHash
            (pfromData $ ProofFold.pfoldControl'excludingRoot fold)
            0 0 pnil (pencodeLedgerDeltaPendingMutationV1 # pending)
        )
        $ \control ->
      plet
        ( ledgerDeltaControlFixture
            1 0 pinitialResolutionAccumulator 0 pnil pemptyResolutionScheduleHash
            0 pinitialResolutionAccumulator pemptyResolutionScheduleHash
            (pfromData $ ProofFold.pfoldControl'includingRoot fold)
            0 1 (Merkle.pbuiltFrontier'peaks built) (pconstant "")
        )
        $ \nextControl ->
      plet
        ( ledgerDeltaStateFixture control 97
            (Merkle.pfrontierCommitment # Merkle.pbuiltFrontier'count built # Merkle.pbuiltFrontier'peaks built)
        )
        $ \pre ->
      plet (ledgerDeltaWitnessFixture control $ ledgerDeltaSuccessorFixture pre nextControl) $ \witness ->
        pverifyLedgerDeltaFinalizeSemanticsV1 # pre # witness

ledgerDeltaTerminalTransition :: forall s. Term s PBool
ledgerDeltaTerminalTransition =
  plet (ledgerDeltaBaseControl 2 (pconstant "")) $ \control ->
  plet (ledgerDeltaStateFixture control 98 (Merkle.pfrontierCommitment # 0 # pnil)) $ \pre ->
  pmatch pre $ \state ->
  pmatch control $ \c ->
  plet
    ( pcon $ PValidationMachineStateV1
        (pmachineState'machineVersion state)
        (pmachineState'eventKeyHash state)
        (pmachineState'transactionId state)
        (pmachineState'transactionCommitment state)
        (pmachineState'validationContextHash state)
        (pmachineState'sourceKind state)
        (pmachineState'priorLedgerRoot state)
        (pdata $ pcon PTerminal)
        (pdata $ pfromData (pmachineState'programCounter state) + 1)
        ( pdata $
            phashWorkWitness
              # pcon PTerminal
              # (pfromData (pmachineState'programCounter state) + 1)
              # ( pencodeTerminalAcceptanceWitnessV1
                    # pfromData (pledgerDelta'currentLedgerRoot c) # 0 # pnil
                )
        )
        (pmachineState'executionCpu state)
        (pmachineState'executionMemory state)
        (pdata $ pcon PAccepted)
        (pdata $ cekHash 0)
        (pmachineState'ledgerDeltaRoot state)
    )
    $ \post ->
  plet (ledgerDeltaWitnessFixture control post) $ \witness ->
  plet (pcon $ PValidationOneStepEvidenceV1 (pdata witness) (pdata $ pcon PNoAuxiliaryWitness)) $ \evidence ->
    pverifyLedgerDeltaTerminalSemanticsV1 # pre # witness
      #&& pverifyLedgerDeltaOneStepV1 # pre # evidence

cekResolvedContextTransitions :: forall s. Term s PBool
cekResolvedContextTransitions =
  cekResolvedContextTransition 1 1 2 0
    #&& cekResolvedContextTransition 0 2 3 1

cekResolvedContextTransition :: forall s. Integer -> Integer -> Integer -> Integer -> Term s PBool
cekResolvedContextTransition sourceKind stage nextStage spendCount =
  plet cekSpendSubject $ \subject ->
  plet (cekDescriptorForIndex 2) $ \descriptorCbor ->
  plet
    ( ScriptProof.presolvedContextItemLeafHash
        # pconstant sourceKind # 0 # subject # descriptorCbor
    )
    $ \leaf ->
  plet (cekSinglePeak leaf) $ \peaks ->
  plet
    ( nativeScriptsControlForContext
        1 (pconstant spendCount) peaks 0
        (Merkle.pfrontierCommitment # 0 # pnil) 0 pnil
    )
    $ \nativeControl ->
  pmatch
    ( ScriptContext.pprependResolvedDescriptorTxInInfoV1
        # 1 # peaks # pconstant spendCount # pconstant sourceKind # 0
        # subject # descriptorCbor # pnil # pconstant False
        # CekData.pemptyDataListSummaryV1
    )
    $ \case
      PNothing -> pconstant False
      PJust nextItems ->
        let references = if sourceKind == 1 then nextItems else CekData.pemptyDataListSummaryV1
            spends = if sourceKind == 0 then nextItems else CekData.pemptyDataListSummaryV1
            current = cekContextWithTestCollections stage CekData.pemptyDataListSummaryV1
              CekData.pemptyDataListSummaryV1 CekData.pemptyDataListSummaryV1 CekData.pemptyDataListSummaryV1
            afterItem = cekContextWithTestCollections stage references spends
              CekData.pemptyDataListSummaryV1 CekData.pemptyDataListSummaryV1
            afterAdvance = cekContextWithTestCollections nextStage references spends
              CekData.pemptyDataListSummaryV1 CekData.pemptyDataListSummaryV1
            itemVerifier = if sourceKind == 1
              then pverifyCekReferenceContextItem
              else pverifyCekSpendContextItem
            advanceVerifier = if sourceKind == 1
              then pverifyCekReferenceContextAdvance
              else pverifyCekSpendContextAdvance
         in plet (cekStateFixture (pcon PCek) 75 (cekHash 0xaa) 7 8) $ \itemPre ->
            plet (cekContextTransitionWitness nativeControl afterItem 76 7 8) $ \itemWitness ->
            plet (cekStateFixture (pcon PCek) 76 (cekHash 0xab) 7 8) $ \advancePre ->
            plet (cekContextTransitionWitness nativeControl afterAdvance 77 7 8) $ \advanceWitness ->
            plet
              ( pcon $ PCekResolvedContextItemWitness
                  (pdata $ pconstant sourceKind) (pdata 0) (pdata subject)
                  (pdata descriptorCbor) (pdata pnil)
              )
              $ \itemAuxiliary ->
              itemVerifier
                # itemPre # itemWitness # nativeControl # current
                # pconstant sourceKind # 0 # subject # descriptorCbor # pnil # 0 # 3 # 4
                #&& advanceVerifier
                  # advancePre # advanceWitness # nativeControl # afterItem # 0 # 3 # 4
                #&& ( if sourceKind == 1
                        then pverifyCekReferenceContextStep
                        else pverifyCekSpendContextStep
                    )
                  # itemPre # itemWitness # itemAuxiliary # nativeControl # current
                  # 0 # 3 # 4
                #&& ( if sourceKind == 1
                        then pverifyCekReferenceContextStep
                        else pverifyCekSpendContextStep
                    )
                  # advancePre # advanceWitness # pcon PNoAuxiliaryWitness
                  # nativeControl # afterItem # 0 # 3 # 4

cekOutputAndSignerContextTransitions :: forall s. Term s PBool
cekOutputAndSignerContextTransitions =
  cekOutputContextTransition #&& cekSignerContextTransition

cekOutputContextTransition :: forall s. Term s PBool
cekOutputContextTransition =
  plet (cekDescriptorForIndex 0) $ \descriptorCbor ->
  plet (ScriptProof.poutputDescriptorLeafHash # 0 # descriptorCbor) $ \leaf ->
  plet (cekSinglePeak leaf) $ \peaks ->
  plet
    ( nativeScriptsControlForContext
        0 0 pnil 0 (Merkle.pfrontierCommitment # 0 # pnil) 1 peaks
    )
    $ \nativeControl ->
  pmatch
    ( ScriptContext.pprependOutputDescriptorV1
        # 1 # peaks # 0 # descriptorCbor # pnil # pconstant False
        # CekData.pemptyDataListSummaryV1
    )
    $ \case
      PNothing -> pconstant False
      PJust nextItems ->
        plet
          ( cekContextWithTestCollections 3 CekData.pemptyDataListSummaryV1
              CekData.pemptyDataListSummaryV1 CekData.pemptyDataListSummaryV1
              CekData.pemptyDataListSummaryV1
          )
          $ \current ->
        plet
          ( cekContextWithTestCollections 3 CekData.pemptyDataListSummaryV1
              CekData.pemptyDataListSummaryV1 nextItems CekData.pemptyDataListSummaryV1
          )
          $ \afterItem ->
        plet
          ( cekContextWithTestCollections 4 CekData.pemptyDataListSummaryV1
              CekData.pemptyDataListSummaryV1 nextItems CekData.pemptyDataListSummaryV1
          )
          $ \afterAdvance ->
        plet (cekStateFixture (pcon PCek) 78 (cekHash 0xaa) 7 8) $ \itemPre ->
        plet (cekContextTransitionWitness nativeControl afterItem 79 7 8) $ \itemWitness ->
        plet (cekStateFixture (pcon PCek) 79 (cekHash 0xab) 7 8) $ \advancePre ->
        plet (cekContextTransitionWitness nativeControl afterAdvance 80 7 8) $ \advanceWitness ->
        plet
          (pcon $ PCekOutputContextItemWitness (pdata 0) (pdata descriptorCbor) (pdata pnil))
          $ \itemAuxiliary ->
          pverifyCekOutputContextItem
            # itemPre # itemWitness # nativeControl # current
            # 0 # descriptorCbor # pnil # 0 # 3 # 4
            #&& pverifyCekOutputContextAdvance
              # advancePre # advanceWitness # nativeControl # afterItem # 0 # 3 # 4
            #&& pverifyCekOutputContextStep
              # itemPre # itemWitness # itemAuxiliary # nativeControl # current # 0 # 3 # 4
            #&& pverifyCekOutputContextStep
              # advancePre # advanceWitness # pcon PNoAuxiliaryWitness
              # nativeControl # afterItem # 0 # 3 # 4

cekSignerContextTransition :: forall s. Term s PBool
cekSignerContextTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x66)) $ \signerHash ->
  plet (ScriptProof.psignerLeafHash # signerHash) $ \leaf ->
  plet (cekSinglePeak leaf) $ \peaks ->
  plet (Merkle.pfrontierCommitment # 1 # peaks) $ \commitment ->
  plet
    (nativeScriptsControlForContext 0 0 pnil 1 commitment 0 pnil)
    $ \nativeControl ->
  pmatch
    ( ScriptContext.pprependSignerV1
        # 1 # commitment # peaks # 0 # signerHash # pnil
        # CekData.pemptyDataListSummaryV1
    )
    $ \case
      PNothing -> pconstant False
      PJust nextItems ->
        plet
          ( cekContextWithTestCollections 4 CekData.pemptyDataListSummaryV1
              CekData.pemptyDataListSummaryV1 CekData.pemptyDataListSummaryV1
              CekData.pemptyDataListSummaryV1
          )
          $ \current ->
        plet
          ( cekContextWithTestCollections 4 CekData.pemptyDataListSummaryV1
              CekData.pemptyDataListSummaryV1 CekData.pemptyDataListSummaryV1 nextItems
          )
          $ \afterItem ->
        plet
          ( cekContextWithTestCollections 5 CekData.pemptyDataListSummaryV1
              CekData.pemptyDataListSummaryV1 CekData.pemptyDataListSummaryV1 nextItems
          )
          $ \afterAdvance ->
        plet (cekStateFixture (pcon PCek) 81 (cekHash 0xaa) 7 8) $ \itemPre ->
        plet (cekContextTransitionWitness nativeControl afterItem 82 7 8) $ \itemWitness ->
        plet (cekStateFixture (pcon PCek) 82 (cekHash 0xab) 7 8) $ \advancePre ->
        plet (cekContextTransitionWitness nativeControl afterAdvance 83 7 8) $ \advanceWitness ->
          pverifyCekSignerContextItem
            # itemPre # itemWitness # nativeControl # current
            # peaks # 0 # signerHash # pnil # 0 # 3 # 4
            #&& pverifyCekSignerContextAdvance
              # advancePre # advanceWitness # nativeControl # afterItem # 0 # 3 # 4

cekContextWithTestCollections :: forall s.
  Integer ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s PCekContextControlV1
cekContextWithTestCollections stage references spends outputs signers =
  pmatch (cekContextStageFixture stage 3 True) $ \c ->
    pcon $ PCekContextControlV1
      (pcekContext'stage c)
      (pcekContext'languageTag c)
      (pcekContext'programTermRoot c)
      (pcekContext'programEnvelopeHash c)
      (pcekContext'purposeKind c)
      (pcekContext'purposeIndex c)
      (pcekContext'scriptHash c)
      (pcekContext'subject c)
      (pcekContext'redeemerLeaf c)
      (pcekContext'redeemerContextControlHash c)
      (pcekContext'executionMemoryLimit c)
      (pcekContext'executionCpuLimit c)
      (pdata references)
      (pdata spends)
      (pdata outputs)
      (pdata signers)
      (pcekContext'observerCount c)
      (pcekContext'observerItems c)
      (pcekContext'previousObserver c)
      (pcekContext'observerSummary c)
      (pcekContext'mintCursor c)
      (pcekContext'currentMintPolicy c)
      (pcekContext'currentMintAssets c)
      (pcekContext'mintPolicies c)
      (pcekContext'mintSummary c)

cekSinglePeak :: forall s.
  Term s PByteString -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
cekSinglePeak root =
  pcons # pdata (pcon $ Merkle.PFrontierPeak (pdata 0) (pdata root)) # pnil

cekObserverContextTransitions :: forall s. Term s PBool
cekObserverContextTransitions =
  cekObserverContextItemTransition #&& cekObserverContextAdvanceTransitions

cekObserverCardanoMaximumFirstAndTerminal :: forall s. Term s PBool
cekObserverCardanoMaximumFirstAndTerminal =
  let vector = maximumObserverFieldTerminal
      transactionId = maximumHex (maximumTransactionId vector)
      observerHash = maximumHex (maximumChunk vector)
      collectionProof = maximumObserverCollectionProof
        (pconstant $ maximumItemCount vector) (pconstant $ maximumItemIndex vector)
        (pconstant $ maximumItemLength vector)
      chunkProof = maximumObserverChunkProof (pconstant $ maximumItemLength vector)
   in plet maximumObserverNativeControl $ \nativeControl ->
      plet (cekContextStageFixture 5 3 True) $ \contextControl ->
      plet
        (pprependCekObserverItemV1 # observerHash # pconstant False # CekData.pemptyDataPairSummaryV1)
        $ \firstItems ->
      plet (cekContextWithTestObservers 5 224 firstItems observerHash cekEmptySummary) $ \firstControl ->
      plet
        ( pcon $ CekData.PDataSequenceSummaryV1
            (pdata $ phexByteStr "354faff8c56e5c2db595d2b0682336623a4de555e8426762630783d26921ab5d")
            (pdata 224) (pdata 7840) (pdata 9184)
        )
        $ \completeItems ->
      plet
        ( cekContextWithTestObservers 5 224 completeItems
            (phexByteStr "008b91b576da13635f7150f248cd133c23ac0a697f1ccae85617ea85")
            cekEmptySummary
        )
        $ \completeControl ->
      plet (pfinalizeCekObserverItemsV1 # completeItems # pconstant False) $ \observerSummary ->
      plet
        ( cekContextWithTestObservers 6 224 completeItems
            (phexByteStr "008b91b576da13635f7150f248cd133c23ac0a697f1ccae85617ea85")
            observerSummary
        )
        $ \finalControl ->
      plet (cekStateFixtureWithTransactionId transactionId (pcon PCek) 45 (cekHash 0xaa) 0 0) $ \firstPre ->
      plet (cekObserverTransitionWitness nativeControl firstControl 46) $ \firstWitness ->
      plet (cekStateFixtureWithTransactionId transactionId (pcon PCek) 46 (cekHash 0xab) 0 0) $ \terminalPre ->
      plet (cekObserverTransitionWitness nativeControl finalControl 47) $ \terminalWitness ->
      plet
        (cekContextWithTestObservers 5 225 firstItems observerHash cekEmptySummary)
        $ \mismatchedCountControl ->
      plet
        ( maximumObserverCollectionProof 225 (pconstant $ maximumItemIndex vector)
            (pconstant $ maximumItemLength vector)
        )
        $ \mismatchedProof ->
      plet (cekObserverTransitionWitness nativeControl mismatchedCountControl 48) $ \mismatchedWitness ->
        pand'List
          [ observerSummary
              #== pcon (CekData.PDataSummaryV1
                    (pdata $ phexByteStr "61eff6b0a693f2da6f3fd45ae1b3d402937e3ab0de94da2fcbd5ed67ab61a726")
                    (pdata 7842) (pdata 9188))
          , pverifyCekObserverContextItem
              # firstPre # firstWitness # nativeControl # contextControl
              # collectionProof # chunkProof # 0 # 0 # 0
          , pverifyCekObserverContextAdvance
              # terminalPre # terminalWitness # nativeControl # completeControl # 0 # 0 # 0
          , pnot
              # ( pverifyCekObserverContextItem
                    # firstPre # mismatchedWitness # nativeControl # contextControl
                    # mismatchedProof # chunkProof # 0 # 0 # 0
                )
          ]

maximumObserverCollectionProof :: forall s.
  Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s BoundedCollection.PItemProofV1
maximumObserverCollectionProof itemCount itemIndex itemLength =
  let vector = maximumObserverFieldTerminal
   in pcon $ BoundedCollection.PItemProofV1
        (pdata BoundedCollection.pboundedCollectionVersion) (pdata 3)
        (pdata itemCount) (pdata itemIndex) (pdata itemLength)
        (pdata $ maximumHex $ maximumItemCommitment vector)
        (pdata $ maximumPeakList $ maximumCollectionPeaks vector)
        (pdata $ maximumHashList $ maximumCollectionSiblings vector)

maximumObserverChunkProof :: forall s. Term s PInteger -> Term s Bounded.PChunkProofV1
maximumObserverChunkProof totalLength =
  let vector = maximumObserverFieldTerminal
   in pcon $ Bounded.PChunkProofV1
        (pdata Bounded.pversion) (pdata 3) (pdata 223) (pdata totalLength)
        (pdata 0) (pdata $ maximumHex $ maximumChunk vector)
        (pdata $ maximumPeakList [(0, maximumChunkHash vector)]) (pdata pnil)

maximumObserverNativeControl :: forall s. Term s PNativeScriptsControlV1
maximumObserverNativeControl =
  let vector = maximumObserverFieldTerminal
   in pcon $ PNativeScriptsControlV1
        (pdata $ maximumHex $ maximumCompactCbor vector)
        (pdata $ maximumHex $ maximumWitnessSetCbor vector)
        (pdata $ maximumHex $ maximumLengthsCbor vector)
        (pdata $ phexByteStr "04")
        (pdata 0) (pdata $ cekHash 0x21) (pdata 0) (pdata pnil)
        (pdata 0) (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
        (pdata 0) (pdata pnil) (pdata 0) (pdata pnil)
        (pdata 0) (pdata pnil) (pdata 0) (pdata pnil) (pdata pnil)
        (pdata 0) (pdata pnil) (pdata 0) (pdata pnil)
        (pdata 0) (pdata 0) (pdata $ cekHash 0x22)

cekObserverRejectsMalformedProofsOrderAndSuccessors :: forall s. Term s PBool
cekObserverRejectsMalformedProofsOrderAndSuccessors =
  plet (preplicateBS # 27 # (pintegerToByte # 0) <> phexByteStr "01") $ \observer0 ->
  plet (preplicateBS # 27 # (pintegerToByte # 0) <> phexByteStr "02") $ \observer1 ->
  plet (Bounded.pfromBytes # 3 # 0 # observer0) $ \commitment0 ->
  plet (Bounded.pfromBytes # 3 # 1 # observer1) $ \commitment1 ->
  plet (BoundedCollection.phashBoundedCollectionItem # 3 # 0 # 28 # commitment0) $ \leaf0 ->
  plet (BoundedCollection.phashBoundedCollectionItem # 3 # 1 # 28 # commitment1) $ \leaf1 ->
  plet (Merkle.pbuildFrontier #$ pcons # pdata leaf0 #$ pcons # pdata leaf1 # pnil) $ \frontier ->
  pmatch frontier $ \built ->
  plet (BoundedCollection.pboundedCollectionCommitment # 3 # 2 # Merkle.pbuiltFrontier'peaks built) $ \observerCommitment ->
  pmatch (cekObserverProofSource observerCommitment) $ \(PPair nativeControl transactionId) ->
  plet (observerTwoItemCollectionProof 1 commitment1 (Merkle.pbuiltFrontier'peaks built) leaf0 3 2 28) $ \proof1 ->
  plet (observerTwoItemChunkProof 1 observer1 commitment1 3 28) $ \chunk1 ->
  plet (observerTwoItemCollectionProof 0 commitment0 (Merkle.pbuiltFrontier'peaks built) leaf1 3 2 28) $ \proof0 ->
  plet (observerTwoItemChunkProof 0 observer0 commitment0 3 28) $ \chunk0 ->
  plet (cekContextStageFixture 5 3 True) $ \contextControl ->
  plet (pprependCekObserverItemV1 # observer1 # pconstant False # CekData.pemptyDataPairSummaryV1) $ \firstItems ->
  plet (cekContextWithTestObservers 5 2 firstItems observer1 cekEmptySummary) $ \firstControl ->
  plet (cekContextWithTestObservers 5 2 firstItems observer0 cekEmptySummary) $ \malformedSuccessor ->
  plet (pprependCekObserverItemV1 # observer0 # pconstant False # firstItems) $ \nextItems ->
  plet (cekContextWithTestObservers 5 2 firstItems observer0 cekEmptySummary) $ \wrongOrderControl ->
  plet (cekContextWithTestObservers 5 2 nextItems observer0 cekEmptySummary) $ \completeControl ->
  plet (pfinalizeCekObserverItemsV1 # nextItems # pconstant False) $ \exactSummary ->
  pmatch exactSummary $ \summaryFields ->
  plet (pcon $ CekData.PDataSummaryV1 (pdata $ cekHash 0xaa) (CekData.psummary'cborLength summaryFields) (CekData.psummary'memory summaryFields)) $ \wrongSummary ->
  plet (cekContextWithTestObservers 6 2 nextItems observer0 wrongSummary) $ \wrongSummaryControl ->
  plet (cekStateFixtureWithTransactionId transactionId (pcon PCek) 50 (cekHash 0xaa) 0 0) $ \pre ->
  plet (cekObserverTransitionWitness nativeControl firstControl 51) $ \validWitness ->
  plet (cekObserverTransitionWitness nativeControl malformedSuccessor 51) $ \malformedWitness ->
  plet (cekStateFixtureWithTransactionId transactionId (pcon PCek) 57 (cekHash 0xab) 0 0) $ \orderPre ->
  plet (cekObserverTransitionWitness nativeControl completeControl 58) $ \orderWitness ->
  plet (cekStateFixtureWithTransactionId transactionId (pcon PCek) 58 (cekHash 0xac) 0 0) $ \summaryPre ->
  plet (cekObserverTransitionWitness nativeControl wrongSummaryControl 59) $ \summaryWitness ->
    pand'List
      [ pverifyCekObserverContextItem # pre # validWitness # nativeControl # contextControl # proof1 # chunk1 # 0 # 0 # 0
      , pnot # (pverifyCekObserverContextItem # pre # malformedWitness # nativeControl # contextControl # proof1 # chunk1 # 0 # 0 # 0)
      , pnot # (pverifyCekObserverContextItem # pre # validWitness # nativeControl # contextControl # (observerTwoItemCollectionProof 0 commitment1 (Merkle.pbuiltFrontier'peaks built) leaf0 3 2 28) # chunk1 # 0 # 0 # 0)
      , pnot # (pverifyCekObserverContextItem # pre # validWitness # nativeControl # contextControl # (observerTwoItemCollectionProof 1 commitment1 (Merkle.pbuiltFrontier'peaks built) leaf0 3 1 28) # chunk1 # 0 # 0 # 0)
      , pnot # (pverifyCekObserverContextItem # pre # validWitness # nativeControl # contextControl # (observerTwoItemCollectionProof 1 commitment1 (Merkle.pbuiltFrontier'peaks built) leaf0 2 2 28) # (observerTwoItemChunkProof 1 observer1 commitment1 2 28) # 0 # 0 # 0)
      , pnot # (pverifyCekObserverContextItem # pre # validWitness # nativeControl # contextControl # (observerTwoItemCollectionProof 1 commitment1 (Merkle.pbuiltFrontier'peaks built) leaf0 3 2 27) # chunk1 # 0 # 0 # 0)
      , pnot # (pverifyCekObserverContextItem # pre # validWitness # nativeControl # contextControl # proof1 # (observerTwoItemChunkProof 1 observer1 commitment1 3 27) # 0 # 0 # 0)
      , pnot # (pverifyCekObserverContextItem # orderPre # orderWitness # nativeControl # wrongOrderControl # proof0 # chunk0 # 0 # 0 # 0)
      , pnot # (pverifyCekObserverContextAdvance # summaryPre # summaryWitness # nativeControl # completeControl # 0 # 0 # 0)
      ]

observerTwoItemCollectionProof :: forall s.
  Integer -> Term s PByteString -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  Term s PByteString -> Integer -> Integer -> Integer -> Term s BoundedCollection.PItemProofV1
observerTwoItemCollectionProof itemIndex commitment peaks sibling fieldIndex itemCount itemLength =
  pcon $ BoundedCollection.PItemProofV1
    (pdata BoundedCollection.pboundedCollectionVersion) (pdata $ pconstant fieldIndex)
    (pdata $ pconstant itemCount) (pdata $ pconstant itemIndex)
    (pdata $ pconstant itemLength) (pdata commitment) (pdata peaks)
    (pdata $ pcons # pdata sibling # pnil)

observerTwoItemChunkProof :: forall s.
  Integer -> Term s PByteString -> Term s PByteString -> Integer -> Integer -> Term s Bounded.PChunkProofV1
observerTwoItemChunkProof itemIndex bytes _ fieldIndex totalLength =
  pcon $ Bounded.PChunkProofV1
    (pdata Bounded.pversion) (pdata $ pconstant fieldIndex) (pdata $ pconstant itemIndex)
    (pdata $ pconstant totalLength) (pdata 0) (pdata bytes)
    (pdata $ cekSinglePeak $ Bounded.phashChunk # pconstant fieldIndex # pconstant itemIndex # 0 # bytes)
    (pdata pnil)

cekObserverTransitionWitness :: forall s.
  Term s PNativeScriptsControlV1 -> Term s PCekContextControlV1 ->
  Term s PInteger -> Term s PValidationOneStepWitnessV1
cekObserverTransitionWitness nativeControl nextContext counter =
  plet
    ( phashWorkWitness # pcon PCek # counter
        # ( pencodeCekWitnessV1
              # nativeControl # (pencodeCekContextControlV1 # nextContext)
              # 0 # 0 # 0 # pconstant "" # 0 # 0 # cekHash 0x22
          )
    )
    $ \workRoot ->
      pcon $ PValidationOneStepWitnessV1
        (pdata $ pconstant "")
        (pdata $ cekStateFixture (pcon PCek) counter workRoot 0 0)

cekObserverCompleteSummaryRelationIsExact :: forall s. Term s PBool
cekObserverCompleteSummaryRelationIsExact =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  plet (pfinalizeCekObserverItemsV1 # CekData.pemptyDataPairSummaryV1 # pconstant False) $ \exactSummary ->
  pmatch exactSummary $ \summary ->
  plet
    ( pcon $ CekData.PDataSummaryV1
        (pdata $ cekHash 0xaa)
        (CekData.psummary'cborLength summary)
        (CekData.psummary'memory summary)
    )
    $ \malformedSummary ->
  plet (cekContextWithTestObservers 6 0 CekData.pemptyDataPairSummaryV1 (pconstant "") malformedSummary) $ \malformedControl ->
  plet (cekContextStageFixture 9 3 True) $ \successorControl ->
    pnot # (pcekContextControlIsWellFormed # nativeControl # malformedControl)
      #&& pcekContextControlIsWellFormed # nativeControl # successorControl

cekObserverContextItemTransition :: forall s. Term s PBool
cekObserverContextItemTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x55)) $ \observerHash ->
  plet (Bounded.pfromBytes # 3 # 0 # observerHash) $ \itemCommitment ->
  plet (Bounded.phashChunk # 3 # 0 # 0 # observerHash) $ \chunkLeaf ->
  plet (cekSinglePeak chunkLeaf) $ \chunkPeaks ->
  plet
    ( BoundedCollection.phashBoundedCollectionItem
        # 3 # 0 # 28 # itemCommitment
    )
    $ \itemLeaf ->
  plet (cekSinglePeak itemLeaf) $ \itemPeaks ->
  plet
    (BoundedCollection.pboundedCollectionCommitment # 3 # 1 # itemPeaks)
    $ \observerCommitment ->
  plet
    ( pcon $ BoundedCollection.PItemProofV1
        (pdata BoundedCollection.pboundedCollectionVersion)
        (pdata 3)
        (pdata 1)
        (pdata 0)
        (pdata 28)
        (pdata itemCommitment)
        (pdata itemPeaks)
        (pdata pnil)
    )
    $ \collectionProof ->
  plet
    ( pcon $ Bounded.PChunkProofV1
        (pdata Bounded.pversion)
        (pdata 3)
        (pdata 0)
        (pdata 28)
        (pdata 0)
        (pdata observerHash)
        (pdata chunkPeaks)
        (pdata pnil)
    )
    $ \chunkProof ->
  pmatch (cekObserverProofSource observerCommitment) $ \(PPair nativeControl txId) ->
  plet (cekContextStageFixture 5 3 True) $ \current ->
  plet (pprependCekObserverItemV1 # observerHash # pconstant False # CekData.pemptyDataPairSummaryV1) $ \nextItems ->
  plet (cekContextWithTestObservers 5 1 nextItems observerHash cekEmptySummary) $ \nextContext ->
  plet (cekStateFixtureWithTransactionId txId (pcon PCek) 82 (cekHash 0xaa) 7 8) $ \pre ->
  plet (cekContextTransitionWitness nativeControl nextContext 83 7 8) $ \witness ->
  plet
    (pcon $ PTransactionFieldChunkWitness (pdata collectionProof) (pdata chunkProof))
    $ \auxiliary ->
    pverifyCekObserverContextItem
      # pre # witness # nativeControl # current # collectionProof # chunkProof # 0 # 3 # 4
      #&& pverifyCekObserverContextStep
        # pre # witness # auxiliary # nativeControl # current # 0 # 3 # 4

cekObserverContextAdvanceTransitions :: forall s. Term s PBool
cekObserverContextAdvanceTransitions =
  pmatch (cekObserverProofSource NativeField.pemptyFieldCommitment) $ \(PPair emptyNative emptyTxId) ->
  plet (cekContextStageFixture 5 3 True) $ \emptyCurrent ->
  plet (cekContextStageFixture 6 3 True) $ \emptyNext ->
  plet (cekStateFixtureWithTransactionId emptyTxId (pcon PCek) 83 (cekHash 0xaa) 7 8) $ \emptyPre ->
  plet (cekContextTransitionWitness emptyNative emptyNext 84 7 8) $ \emptyWitness ->
  plet (preplicateBS # 28 # (pintegerToByte # 0x55)) $ \observerHash ->
  plet (pprependCekObserverItemV1 # observerHash # pconstant False # CekData.pemptyDataPairSummaryV1) $ \observerItems ->
  pmatch (cekObserverProofSource $ cekHash 0x56) $ \(PPair nonemptyNative nonemptyTxId) ->
  plet
    (cekContextWithTestObservers 5 1 observerItems observerHash cekEmptySummary)
    $ \nonemptyCurrent ->
  plet
    ( cekContextWithTestObservers
        6 1 observerItems observerHash
        (pfinalizeCekObserverItemsV1 # observerItems # pconstant False)
    )
    $ \nonemptyNext ->
  plet (cekStateFixtureWithTransactionId nonemptyTxId (pcon PCek) 84 (cekHash 0xab) 7 8) $ \nonemptyPre ->
  plet (cekContextTransitionWitness nonemptyNative nonemptyNext 85 7 8) $ \nonemptyWitness ->
    pverifyCekObserverContextAdvance
      # emptyPre # emptyWitness # emptyNative # emptyCurrent # 0 # 3 # 4
      #&& pverifyCekObserverContextStep
        # emptyPre # emptyWitness # pcon PNoAuxiliaryWitness
        # emptyNative # emptyCurrent # 0 # 3 # 4
      #&& pverifyCekObserverContextAdvance
        # nonemptyPre # nonemptyWitness # nonemptyNative # nonemptyCurrent # 0 # 3 # 4
      #&& pverifyCekObserverContextStep
        # nonemptyPre # nonemptyWitness # pcon PNoAuxiliaryWitness
        # nonemptyNative # nonemptyCurrent # 0 # 3 # 4
      #&& pnot
        # ( pverifyCekObserverContextAdvance
              # nonemptyPre # nonemptyWitness # nonemptyNative # emptyCurrent # 0 # 3 # 4
          )

cekObserverProofSource :: forall s.
  Term s PByteString -> Term s (PPair PNativeScriptsControlV1 PByteString)
cekObserverProofSource observerCommitment =
  cekObserverProofSourceWithIntegrity observerCommitment (cekHash 0x16) 0

cekObserverProofSourceWithIntegrity :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s (PPair PNativeScriptsControlV1 PByteString)
cekObserverProofSourceWithIntegrity observerCommitment scriptIntegrityHash languageBitmap =
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata $ cekHash 0x31)
        (pdata $ cekHash 0x32)
        (pdata $ cekHash 0x33)
    )
    $ \witnessSet ->
  plet (NativeCompact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        (cekHash 0x10)
        (cekHash 0x11)
        (cekHash 0x12)
        0
        (-1)
        (-1)
        observerCommitment
        (cekHash 0x14)
        (cekHash 0x15)
        scriptIntegrityHash
        (cekHash 0x17)
        0
    )
    $ \body ->
  plet
    (pcon $ PNativeTxCompact body (pblake2b_256 # witnessCbor) 0)
    $ \compact ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet
    (pcon $ PNativeTxFieldPreimageLengthsV1 1 1 1 1 1 1 1 1 1)
    $ \lengths ->
  plet (NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # lengths) $ \lengthsCbor ->
  plet (cekObserverBodyCborWithIntegrity observerCommitment scriptIntegrityHash) $ \bodyCbor ->
  plet
    (pblake2b_256 #$ pconstant "MidgardNativeTxBodyV1" <> Codec.pcborInt 1 <> bodyCbor)
    $ \txId ->
      pcon $ PPair
        ( pcon $ PNativeScriptsControlV1
            (pdata compactCbor)
            (pdata witnessCbor)
            (pdata lengthsCbor)
            (pdata $ phexByteStr "04")
            (pdata 0)
            (pdata $ cekHash 0x21)
            (pdata 0)
            (pdata pnil)
            (pdata 0)
            (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
            (pdata 0)
            (pdata pnil)
            (pdata 0)
            (pdata pnil)
            (pdata 0)
            (pdata pnil)
            (pdata 0)
            (pdata pnil)
            (pdata pnil)
            (pdata 0)
            (pdata pnil)
            (pdata 0)
            (pdata pnil)
            (pdata 0)
            (pdata languageBitmap)
            (pdata $ cekHash 0x22)
        )
        txId

cekObserverBodyCborWithIntegrity :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PByteString
cekObserverBodyCborWithIntegrity observerCommitment scriptIntegrityHash =
  pconstant "\x8c"
    <> Codec.pencodeDefiniteBytes # cekHash 0x10
    <> Codec.pencodeDefiniteBytes # cekHash 0x11
    <> Codec.pencodeDefiniteBytes # cekHash 0x12
    <> Codec.pcborInt 0
    <> Codec.pcborInt (-1)
    <> Codec.pcborInt (-1)
    <> Codec.pencodeDefiniteBytes # observerCommitment
    <> Codec.pencodeDefiniteBytes # cekHash 0x14
    <> Codec.pencodeDefiniteBytes # cekHash 0x15
    <> Codec.pencodeDefiniteBytes # scriptIntegrityHash
    <> Codec.pencodeDefiniteBytes # cekHash 0x17
    <> Codec.pcborInt 0

cekContextWithTestObservers :: forall s.
  Integer ->
  Integer ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s PByteString ->
  Term s CekData.PDataSummaryV1 ->
  Term s PCekContextControlV1
cekContextWithTestObservers stage observerCount observerItems previousObserver observerSummary =
  pmatch (cekContextStageFixture stage 3 True) $ \c ->
    pcon $ PCekContextControlV1
      (pcekContext'stage c)
      (pcekContext'languageTag c)
      (pcekContext'programTermRoot c)
      (pcekContext'programEnvelopeHash c)
      (pcekContext'purposeKind c)
      (pcekContext'purposeIndex c)
      (pcekContext'scriptHash c)
      (pcekContext'subject c)
      (pcekContext'redeemerLeaf c)
      (pcekContext'redeemerContextControlHash c)
      (pcekContext'executionMemoryLimit c)
      (pcekContext'executionCpuLimit c)
      (pcekContext'referenceItems c)
      (pcekContext'spendItems c)
      (pcekContext'outputItems c)
      (pcekContext'signerItems c)
      (pdata $ pconstant observerCount)
      (pdata observerItems)
      (pdata previousObserver)
      (pdata observerSummary)
      (pcekContext'mintCursor c)
      (pcekContext'currentMintPolicy c)
      (pcekContext'currentMintAssets c)
      (pcekContext'mintPolicies c)
      (pcekContext'mintSummary c)

cekContextMaximumMintInitializesFromAuthenticatedFrontier :: forall s. Term s PBool
cekContextMaximumMintInitializesFromAuthenticatedFrontier =
  pmatch (Merkle.pbuildFrontier # byteStringDataList maximumMintAssetLeaves) $
    \(Merkle.PBuiltFrontier mintCount mintPeaks) ->
  plet (nativeScriptsControlWithMint mintCount mintPeaks) $ \nativeControl ->
  plet (cekContextStageFixture 6 3 True) $ \current ->
  plet (cekContextStageFixture 8 3 True) $ \nextContext ->
  plet (cekStateFixture (pcon PCek) 51 (cekHash 0xaa) 0 0) $ \pre ->
  plet (cekObserverTransitionWitness nativeControl nextContext 52) $ \witness ->
    pverifyCekMintContextInit
      # pre # witness # nativeControl # current # 0 # 0 # 0

cekContextMaximumMintAuthenticatesLastAssetMembership :: forall s. Term s PBool
cekContextMaximumMintAuthenticatesLastAssetMembership =
  pmatch (Merkle.pbuildFrontier # byteStringDataList maximumMintAssetLeaves) $
    \(Merkle.PBuiltFrontier mintCount mintPeaks) ->
  plet (nativeScriptsControlWithMint mintCount mintPeaks) $ \nativeControl ->
  plet (cekContextStageFixture 8 3 True) $ \current ->
  plet (maximumMintAssetName 127) $ \lastAsset ->
  plet (cekPrependMintAsset lastAsset 1 CekData.pemptyDataPairSummaryV1) $ \assetItems ->
  plet
    ( cekContextWithTestMint
        8 1 (preplicateBS # 28 # (pintegerToByte # 0xaa)) assetItems
        CekData.pemptyDataPairSummaryV1 cekEmptySummary
    )
    $ \nextContext ->
  plet (cekStateFixture (pcon PCek) 53 (cekHash 0xaa) 0 0) $ \pre ->
  plet (cekObserverTransitionWitness nativeControl nextContext 54) $ \witness ->
    pverifyCekMintContextItem
      # pre # witness # nativeControl # current
      # 127 # (preplicateBS # 28 # (pintegerToByte # 0xaa)) # lastAsset # 1
      # byteStringDataList maximumMintLastAssetSiblings # 0 # 0 # 0

maximumMintAssetName :: forall s. Integer -> Term s PByteString
maximumMintAssetName assetIndex =
  preplicateBS # 31 # (pintegerToByte # 0)
    <> preplicateBS # 1 # (pintegerToByte # pconstant assetIndex)

maximumMintAssetLeaves :: forall s. [Term s PByteString]
maximumMintAssetLeaves =
  [maximumMintAssetLeaf assetIndex | assetIndex <- [0 .. 127]]

maximumMintAssetLeaf :: forall s. Integer -> Term s PByteString
maximumMintAssetLeaf assetIndex =
  pmintAssetLeafHash
    # (preplicateBS # 28 # (pintegerToByte # 0xaa))
    # maximumMintAssetName assetIndex
    # 1

maximumMintSubtreeRoot :: forall s. Integer -> Integer -> Term s PByteString
maximumMintSubtreeRoot start size
  | size == 1 = maximumMintAssetLeaf start
  | otherwise =
      let half = size `div` 2
       in Merkle.phashBranch
            # maximumMintSubtreeRoot start half
            # maximumMintSubtreeRoot (start + half) half

maximumMintLastAssetSiblings :: forall s. [Term s PByteString]
maximumMintLastAssetSiblings =
  [ maximumMintSubtreeRoot 126 1
  , maximumMintSubtreeRoot 124 2
  , maximumMintSubtreeRoot 120 4
  , maximumMintSubtreeRoot 112 8
  , maximumMintSubtreeRoot 96 16
  , maximumMintSubtreeRoot 64 32
  , maximumMintSubtreeRoot 0 64
  ]

cekMintContextInitTransitions :: forall s. Term s PBool
cekMintContextInitTransitions =
  plet (nativeScriptsControlWithMint 0 pnil) $ \emptyNative ->
  plet
    ( cekContextWithTestMint
        6 0 (pconstant "") CekData.pemptyDataPairSummaryV1
        CekData.pemptyDataPairSummaryV1 cekEmptySummary
    )
    $ \current ->
  plet
    ( cekContextWithTestMint
        9 0 (pconstant "") CekData.pemptyDataPairSummaryV1
        CekData.pemptyDataPairSummaryV1
        (CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1)
    )
    $ \emptyNext ->
  plet (cekStateFixture (pcon PCek) 84 (cekHash 0xaa) 7 8) $ \emptyPre ->
  plet (cekContextTransitionWitness emptyNative emptyNext 85 7 8) $ \emptyWitness ->
  plet (cekHash 0x41) $ \mintLeaf ->
  plet (cekSinglePeak mintLeaf) $ \mintPeaks ->
  plet (nativeScriptsControlWithMint 1 mintPeaks) $ \nonemptyNative ->
  plet
    ( cekContextWithTestMint
        8 0 (pconstant "") CekData.pemptyDataPairSummaryV1
        CekData.pemptyDataPairSummaryV1 cekEmptySummary
    )
    $ \nonemptyNext ->
  plet (cekStateFixture (pcon PCek) 85 (cekHash 0xab) 7 8) $ \nonemptyPre ->
  plet (cekContextTransitionWitness nonemptyNative nonemptyNext 86 7 8) $ \nonemptyWitness ->
    pverifyCekMintContextInit
      # emptyPre # emptyWitness # emptyNative # current # 0 # 3 # 4
      #&& pverifyCekMintContextInit
        # nonemptyPre # nonemptyWitness # nonemptyNative # current # 0 # 3 # 4

cekMintContextItemTransitions :: forall s. Term s PBool
cekMintContextItemTransitions =
  cekMintFirstAndFinalizeTransition
    #&& cekMintExistingPolicyTransition
    #&& cekMintPolicyRolloverTransition

cekMintFirstAndFinalizeTransition :: forall s. Term s PBool
cekMintFirstAndFinalizeTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x22)) $ \policy ->
  plet (phexByteStr "7a") $ \assetName ->
  plet (pmintAssetLeafHash # policy # assetName # 5) $ \leaf ->
  plet (cekSinglePeak leaf) $ \peaks ->
  plet (nativeScriptsControlWithMint 1 peaks) $ \nativeControl ->
  plet
    ( cekContextWithTestMint
        8 0 (pconstant "") CekData.pemptyDataPairSummaryV1
        CekData.pemptyDataPairSummaryV1 cekEmptySummary
    )
    $ \current ->
  plet (cekPrependMintAsset assetName 5 CekData.pemptyDataPairSummaryV1) $ \assets ->
  plet
    (cekContextWithTestMint 8 1 policy assets CekData.pemptyDataPairSummaryV1 cekEmptySummary)
    $ \afterItem ->
  plet (cekFinalizeMintPolicy policy assets CekData.pemptyDataPairSummaryV1) $ \policies ->
  plet
    ( cekContextWithTestMint
        9 1 (pconstant "") CekData.pemptyDataPairSummaryV1 policies
        (CekData.pmapDataSummaryV1 # policies)
    )
    $ \afterAdvance ->
  plet (cekStateFixture (pcon PCek) 86 (cekHash 0xaa) 7 8) $ \itemPre ->
  plet (cekContextTransitionWitness nativeControl afterItem 87 7 8) $ \itemWitness ->
  plet (cekStateFixture (pcon PCek) 87 (cekHash 0xab) 7 8) $ \advancePre ->
  plet (cekContextTransitionWitness nativeControl afterAdvance 88 7 8) $ \advanceWitness ->
    pverifyCekMintContextItem
      # itemPre # itemWitness # nativeControl # current
      # 0 # policy # assetName # 5 # pnil # 0 # 3 # 4
      #&& pverifyCekMintContextAdvance
        # advancePre # advanceWitness # nativeControl # afterItem # 0 # 3 # 4
      #&& pnot
        # ( pverifyCekMintContextItem
              # itemPre # itemWitness # nativeControl # current
              # 1 # policy # assetName # 5 # pnil # 0 # 3 # 4
          )

cekMintExistingPolicyTransition :: forall s. Term s PBool
cekMintExistingPolicyTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x22)) $ \policy ->
  plet (phexByteStr "61") $ \nextAsset ->
  plet (phexByteStr "7a") $ \previousAsset ->
  plet (pmintAssetLeafHash # policy # nextAsset # 7) $ \nextLeaf ->
  plet (pmintAssetLeafHash # policy # previousAsset # 5) $ \previousLeaf ->
  plet (cekTwoLeafPeaks nextLeaf previousLeaf) $ \peaks ->
  plet (nativeScriptsControlWithMint 2 peaks) $ \nativeControl ->
  plet (cekPrependMintAsset previousAsset 5 CekData.pemptyDataPairSummaryV1) $ \currentAssets ->
  plet
    (cekContextWithTestMint 8 1 policy currentAssets CekData.pemptyDataPairSummaryV1 cekEmptySummary)
    $ \current ->
  plet (cekPrependMintAsset nextAsset 7 currentAssets) $ \nextAssets ->
  plet
    (cekContextWithTestMint 8 2 policy nextAssets CekData.pemptyDataPairSummaryV1 cekEmptySummary)
    $ \nextContext ->
  plet (cekStateFixture (pcon PCek) 88 (cekHash 0xaa) 7 8) $ \pre ->
  plet (cekContextTransitionWitness nativeControl nextContext 89 7 8) $ \witness ->
    pverifyCekMintContextItem
      # pre # witness # nativeControl # current
      # 0 # policy # nextAsset # 7
      # (pcons # pdata previousLeaf # pnil) # 0 # 3 # 4

cekMintPolicyRolloverTransition :: forall s. Term s PBool
cekMintPolicyRolloverTransition =
  plet (preplicateBS # 28 # (pintegerToByte # 0x11)) $ \nextPolicy ->
  plet (preplicateBS # 28 # (pintegerToByte # 0x22)) $ \previousPolicy ->
  plet (phexByteStr "61") $ \nextAsset ->
  plet (phexByteStr "7a") $ \previousAsset ->
  plet (pmintAssetLeafHash # nextPolicy # nextAsset # 7) $ \nextLeaf ->
  plet (pmintAssetLeafHash # previousPolicy # previousAsset # 5) $ \previousLeaf ->
  plet (cekTwoLeafPeaks nextLeaf previousLeaf) $ \peaks ->
  plet (nativeScriptsControlWithMint 2 peaks) $ \nativeControl ->
  plet (cekPrependMintAsset previousAsset 5 CekData.pemptyDataPairSummaryV1) $ \previousAssets ->
  plet
    ( cekContextWithTestMint
        8 1 previousPolicy previousAssets CekData.pemptyDataPairSummaryV1 cekEmptySummary
    )
    $ \current ->
  plet (cekFinalizeMintPolicy previousPolicy previousAssets CekData.pemptyDataPairSummaryV1) $ \policies ->
  plet (cekPrependMintAsset nextAsset 7 CekData.pemptyDataPairSummaryV1) $ \nextAssets ->
  plet
    (cekContextWithTestMint 8 2 nextPolicy nextAssets policies cekEmptySummary)
    $ \nextContext ->
  plet (cekStateFixture (pcon PCek) 89 (cekHash 0xaa) 7 8) $ \pre ->
  plet (cekContextTransitionWitness nativeControl nextContext 90 7 8) $ \witness ->
    pverifyCekMintContextItem
      # pre # witness # nativeControl # current
      # 0 # nextPolicy # nextAsset # 7
      # (pcons # pdata previousLeaf # pnil) # 0 # 3 # 4

cekContextWithTestMint :: forall s.
  Integer ->
  Integer ->
  Term s PByteString ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s CekData.PDataSummaryV1 ->
  Term s PCekContextControlV1
cekContextWithTestMint stage cursor policy assets policies mintSummary =
  pmatch (cekContextStageFixture stage 3 True) $ \c ->
    pcon $ PCekContextControlV1
      (pcekContext'stage c)
      (pcekContext'languageTag c)
      (pcekContext'programTermRoot c)
      (pcekContext'programEnvelopeHash c)
      (pcekContext'purposeKind c)
      (pcekContext'purposeIndex c)
      (pcekContext'scriptHash c)
      (pcekContext'subject c)
      (pcekContext'redeemerLeaf c)
      (pcekContext'redeemerContextControlHash c)
      (pcekContext'executionMemoryLimit c)
      (pcekContext'executionCpuLimit c)
      (pcekContext'referenceItems c)
      (pcekContext'spendItems c)
      (pcekContext'outputItems c)
      (pcekContext'signerItems c)
      (pcekContext'observerCount c)
      (pcekContext'observerItems c)
      (pcekContext'previousObserver c)
      (pcekContext'observerSummary c)
      (pdata $ pconstant cursor)
      (pdata policy)
      (pdata assets)
      (pdata policies)
      (pdata mintSummary)

cekPrependMintAsset :: forall s.
  Term s PByteString ->
  Term s PInteger ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s CekData.PDataSequenceSummaryV1
cekPrependMintAsset assetName quantity tailSummary =
  CekData.pprependDataPairSummaryV1
    # (CekData.psemanticDataSummaryV1 # pforgetData (pdata assetName))
    # (CekData.psemanticDataSummaryV1 # pforgetData (pdata quantity))
    # tailSummary

cekFinalizeMintPolicy :: forall s.
  Term s PByteString ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s CekData.PDataSequenceSummaryV1 ->
  Term s CekData.PDataSequenceSummaryV1
cekFinalizeMintPolicy policy assets policies =
  CekData.pprependDataPairSummaryV1
    # (CekData.psemanticDataSummaryV1 # pforgetData (pdata policy))
    # (CekData.pmapDataSummaryV1 # assets)
    # policies

cekTwoLeafPeaks :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
cekTwoLeafPeaks left right =
  pcons
    # pdata (pcon $ Merkle.PFrontierPeak (pdata 1) (pdata $ Merkle.phashBranch # left # right))
    # pnil

cekRedeemerContextSelectionTransitions :: forall s. Term s PBool
cekRedeemerContextSelectionTransitions =
  cekRedeemerContextDataSelection #&& cekRedeemerContextDescriptorSelection

cekRedeemerContextDataSelection :: forall s. Term s PBool
cekRedeemerContextDataSelection =
  plet (preplicateBS # 28 # (pintegerToByte # 0x33)) $ \scriptHash ->
  pmatch (ScriptContext.pscriptPurposeSummaryV1 # 1 # scriptHash # pconstant "" # pconstant False) $ \case
    PNothing -> pconstant False
    PJust purpose ->
      cekRedeemerContextSelectionCase
        1
        RedeemerItemProof.pmodeData
        scriptHash
        (pconstant "")
        purpose
        91

cekRedeemerContextDescriptorSelection :: forall s. Term s PBool
cekRedeemerContextDescriptorSelection =
  cekRedeemerContextSelectionCase
    3
    RedeemerItemProof.pmodeDescriptor
    (preplicateBS # 28 # (pintegerToByte # 0x33))
    (pconstant "")
    cekEmptySummary
    93

cekRedeemerContextSelectionCase :: forall s.
  Integer ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s CekData.PDataSummaryV1 ->
  Integer ->
  Term s PBool
cekRedeemerContextSelectionCase purposeKind mode scriptHash subject purpose counter =
  plet (cekHash 0x70) $ \itemCommitment ->
  plet (ScriptProof.predeemerItemLeafHash # 0 # itemCommitment) $ \redeemerLeaf ->
  plet
    (ScriptProof.ppurposeLeafHash # pconstant purposeKind # 0 # scriptHash # subject)
    $ \purposeLeaf ->
  plet (nativeScriptsControlForRedeemerSelection redeemerLeaf purposeLeaf) $ \nativeControl ->
  plet pinitialCekRedeemerContextControlV1 $ \redeemers ->
  plet (phashCekRedeemerContextControlV1 # redeemers) $ \redeemerHash ->
  plet (cekContextStageFixtureWithRedeemerHash 9 3 True redeemerHash) $ \contextControl ->
  pmatch (predeemerTagForPurposeKindV1 # pconstant purposeKind) $ \case
    PNothing -> pconstant False
    PJust purposeTag ->
      plet
        ( RedeemerItemProof.pinitialControlV1
            # mode # 0 # 1 # 10 # itemCommitment # purposeTag # 0
        )
        $ \itemControl ->
      plet
        ( pcon $ PCekRedeemerContextControlV1
            (pdata 0)
            (pdata CekData.pemptyDataPairSummaryV1)
            (pdata $ RedeemerItemProof.phashControlV1 # itemControl)
            (pdata redeemerLeaf)
            (pdata purpose)
            (pdata cekEmptySummary)
        )
        $ \nextRedeemers ->
      plet (phashCekRedeemerContextControlV1 # nextRedeemers) $ \nextHash ->
      plet (cekContextStageFixtureWithRedeemerHash 9 3 True nextHash) $ \nextContext ->
      plet (cekStateFixture (pcon PCek) (pconstant counter) (cekHash 0xaa) 7 8) $ \pre ->
      plet (cekContextTransitionWitness nativeControl nextContext (pconstant $ counter + 1) 7 8) $ \witness ->
        pverifyCekRedeemerContextSelect
          # pre # witness # nativeControl # contextControl # redeemers
          # 0 # 1 # 10 # itemCommitment # pnil
          # 0 # pconstant purposeKind # 0 # scriptHash # subject # pnil
          # 0 # 3 # 4

cekRedeemerContextStepTransitions :: forall s. Term s PBool
cekRedeemerContextStepTransitions =
  plet (phexByteStr "8406004100820a14") $ \redeemerCbor ->
  plet (Bounded.pfromBytes # 8 # 1 # redeemerCbor) $ \itemCommitment ->
  plet (ScriptProof.predeemerItemLeafHash # 1 # itemCommitment) $ \redeemerLeaf ->
  plet
    ( pcon $ RedeemerItemProof.PRedeemerItemProofControlV1
        (pdata RedeemerItemProof.pversion)
        (pdata RedeemerItemProof.pmodeDescriptor)
        (pdata RedeemerItemProof.pstageTail)
        (pdata 1)
        (pdata 2)
        (pdata $ plengthBS # redeemerCbor)
        (pdata itemCommitment)
        (pdata 6)
        (pdata 0)
        (pdata 6)
        (pdata 0)
        (pdata 4)
        (pdata 1)
        (pdata $ -1)
        (pdata $ -1)
        (pdata $ pcon PDNothing)
    )
    $ \tailItemControl ->
  plet
    ( pcon $ RedeemerItemProof.PRedeemerItemProofControlV1
        (pdata RedeemerItemProof.pversion)
        (pdata RedeemerItemProof.pmodeDescriptor)
        (pdata RedeemerItemProof.pstageTerminal)
        (pdata 1)
        (pdata 2)
        (pdata $ plengthBS # redeemerCbor)
        (pdata itemCommitment)
        (pdata 6)
        (pdata 0)
        (pdata 6)
        (pdata 0)
        (pdata 4)
        (pdata 1)
        (pdata 10)
        (pdata 20)
        (pdata $ pcon PDNothing)
    )
    $ \terminalItemControl ->
  plet
    ( pcon $ PCekRedeemerContextControlV1
        (pdata 0)
        (pdata CekData.pemptyDataPairSummaryV1)
        (pdata $ RedeemerItemProof.phashControlV1 # tailItemControl)
        (pdata redeemerLeaf)
        (pdata cekEmptySummary)
        (pdata cekEmptySummary)
    )
    $ \tailRedeemers ->
  plet (nativeScriptsControlWithRedeemerCount 0 2) $ \nativeControl ->
  plet (phashCekRedeemerContextControlV1 # tailRedeemers) $ \tailHash ->
  plet (cekContextStageFixtureWithRedeemerHash 9 3 True tailHash) $ \tailContext ->
  plet
    ( pcon $ PCekRedeemerContextControlV1
        (pdata 1)
        (pdata CekData.pemptyDataPairSummaryV1)
        (pdata $ pconstant "")
        (pdata $ pconstant "")
        (pdata cekEmptySummary)
        (pdata cekEmptySummary)
    )
    $ \terminalRedeemers ->
  plet (phashCekRedeemerContextControlV1 # terminalRedeemers) $ \terminalHash ->
  plet (cekContextStageFixtureWithRedeemerHash 9 3 True terminalHash) $ \terminalContext ->
  plet (cekStateFixture (pcon PCek) 96 (cekHash 0xab) 7 8) $ \tailPre ->
  plet (cekContextTransitionWitness nativeControl terminalContext 97 7 8) $ \tailWitness ->
    pverifyCekRedeemerContextAdvanced
      # tailPre # tailWitness # nativeControl # tailContext
      # tailRedeemers # tailItemControl # terminalItemControl # 0 # 3 # 4

cekContextFinalizeTransition :: forall s. Term s PBool
cekContextFinalizeTransition =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  plet cekCompletedRedeemerControl $ \redeemers ->
  plet (phashCekRedeemerContextControlV1 # redeemers) $ \redeemerHash ->
  plet (cekContextFinalizeFixture 10 128 3 0 (pconstant "") redeemerHash) $ \contextControl ->
  pmatch
    ( ScriptContext.pscriptPurposeSummaryV1
        # 3
        # (preplicateBS # 28 # (pintegerToByte # 0x33))
        # pconstant ""
        # pconstant True
    )
    $ \case
      PNothing -> pconstant False
      PJust scriptInfo ->
        plet
          ( pcon $ PCekContextPartsControlV1
              (pdata CekData.pemptyDataPairSummaryV1)
              (pdata cekNonemptySummary)
              (pdata scriptInfo)
          )
          $ \parts ->
        plet (phashCekContextPartsControlV1 # parts) $ \partsHash ->
        plet (cekContextFinalizeFixture 11 128 3 0 (pconstant "") partsHash) $ \nextContext ->
        plet (cekStateFixture (pcon PCek) 45 (cekHash 0xaa) 7 8) $ \pre ->
        plet (cekContextTransitionWitness nativeControl nextContext 46 7 8) $ \witness ->
        plet (pcon $ PCekContextFinalizeWitness $ pdata redeemers) $ \auxiliary ->
          pverifyCekContextFinalizeControl
            # pre # witness # nativeControl # contextControl # redeemers # 0 # 3 # 4
            #&& pverifyCekContextFinalize
              # pre # witness # auxiliary # nativeControl # contextControl # 0 # 3 # 4
            #&& pnot
              # ( pverifyCekContextFinalizeControl
                    # pre
                    # witness
                    # nativeControl
                    # (cekContextFinalizeFixture 10 128 3 0 (pconstant "") (cekHash 0xee))
                    # redeemers
                    # 0
                    # 3
                    # 4
                )

cekContextFinalizeSpendTransition :: forall s. Term s PBool
cekContextFinalizeSpendTransition =
  plet cekSpendSubject $ \subject ->
  plet (cekDescriptorForIndex 2) $ \descriptorCbor ->
  plet (ScriptProof.presolvedContextItemLeafHash # 0 # 0 # subject # descriptorCbor) $ \leaf ->
  plet (nativeScriptsControlWithResolvedItem leaf) $ \nativeControl ->
  plet cekCompletedRedeemerControl $ \redeemers ->
  plet (phashCekRedeemerContextControlV1 # redeemers) $ \redeemerHash ->
  plet (cekContextFinalizeFixture 10 3 0 0 subject redeemerHash) $ \contextControl ->
  pmatch (ScriptContext.pcardanoSpendScriptInfoFromDescriptorV1 # subject # descriptorCbor) $ \case
    PNothing -> pconstant False
    PJust scriptInfo ->
      plet
        ( pcon $ PCekContextPartsControlV1
            (pdata CekData.pemptyDataPairSummaryV1)
            (pdata cekNonemptySummary)
            (pdata scriptInfo)
        )
        $ \parts ->
      plet (phashCekContextPartsControlV1 # parts) $ \partsHash ->
      plet (cekContextFinalizeFixture 11 3 0 0 subject partsHash) $ \nextContext ->
      plet (cekStateFixture (pcon PCek) 47 (cekHash 0xaa) 7 8) $ \pre ->
      plet (cekContextTransitionWitness nativeControl nextContext 48 7 8) $ \witness ->
      plet
        ( pcon $ PCekContextFinalizeSpendWitness
            (pdata redeemers) (pdata 0) (pdata subject) (pdata descriptorCbor) (pdata pnil)
        )
        $ \auxiliary ->
        pverifyCekContextFinalizeSpendControl
          # pre # witness # nativeControl # contextControl # redeemers
          # 0 # subject # descriptorCbor # pnil # 0 # 3 # 4
          #&& pverifyCekContextFinalize
            # pre # witness # auxiliary # nativeControl # contextControl # 0 # 3 # 4
          #&& pnot
            # ( pverifyCekContextFinalizeSpendControl
                  # pre
                  # witness
                  # (nativeScriptsControlWithResolvedItem $ cekHash 0xfe)
                  # contextControl
                  # redeemers
                  # 0
                  # subject
                  # descriptorCbor
                  # pnil
                  # 0
                  # 3
                  # 4
              )

cekContextTransitionWitness :: forall s.
  Term s PNativeScriptsControlV1 ->
  Term s PCekContextControlV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationOneStepWitnessV1
cekContextTransitionWitness nativeControl nextContext counter cpu memory =
  plet
    ( phashWorkWitness
        # pcon PCek
        # counter
        # ( pencodeCekWitnessV1
              # nativeControl
              # (pencodeCekContextControlV1 # nextContext)
              # 0
              # 3
              # 4
              # pconstant ""
              # 0
              # 0
              # (preplicateBS # 32 # (pintegerToByte # 0x22))
          )
    )
    $ \workRoot ->
      pcon $ PValidationOneStepWitnessV1
        (pdata $ pconstant "")
        (pdata $ cekStateFixture (pcon PCek) counter workRoot cpu memory)

cekCompletedRedeemerControl :: forall s. Term s PCekRedeemerContextControlV1
cekCompletedRedeemerControl =
  redeemerContextFixture
    0
    CekData.pemptyDataPairSummaryV1
    (pconstant "")
    (pconstant "")
    cekNonemptySummary

cekContextFinalizeFixture :: forall s.
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PCekContextControlV1
cekContextFinalizeFixture stage languageTag purposeKind purposeIndex subject redeemerControlHash =
  let observerItems = if languageTag == 128
        then CekData.pemptyDataListSummaryV1
        else CekData.pemptyDataPairSummaryV1
   in pcon $ PCekContextControlV1
        (pdata $ pconstant stage)
        (pdata $ pconstant languageTag)
        (pdata $ preplicateBS # 32 # (pintegerToByte # 0x11))
        (pdata $ preplicateBS # 32 # (pintegerToByte # 0x22))
        (pdata $ pconstant purposeKind)
        (pdata $ pconstant purposeIndex)
        (pdata $ preplicateBS # 28 # (pintegerToByte # 0x33))
        (pdata subject)
        (pdata $ preplicateBS # 32 # (pintegerToByte # 0x44))
        (pdata redeemerControlHash)
        (pdata 0)
        (pdata 0)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata CekData.pemptyDataListSummaryV1)
        (pdata 0)
        (pdata observerItems)
        (pdata $ pconstant "")
        (pdata $ pfinalizeCekObserverItemsV1 # observerItems # pconstant (languageTag == 128))
        (pdata 0)
        (pdata $ pconstant "")
        (pdata CekData.pemptyDataPairSummaryV1)
        (pdata CekData.pemptyDataPairSummaryV1)
        (pdata $ CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1)

cekSpendSubject :: forall s. Term s PByteString
cekSpendSubject =
  pencodeMidgardTxInput
    # ( pcon $ PMidgardTxInput
          (pdata $ preplicateBS # 32 # (pintegerToByte # 0xbb))
          (pdata 2)
      )

cekDescriptorForIndex :: forall s. Integer -> Term s PByteString
cekDescriptorForIndex outputIndex =
  plet
    (phexByteStr "a200581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a142223307")
    $ \outputCbor ->
  pmatch (ScriptContext.ptxOutSummaryV1 # outputCbor # pconstant False) $ \case
    PNothing -> perror
    PJust cardano ->
      pmatch (ScriptContext.ptxOutSummaryV1 # outputCbor # pconstant True) $ \case
        PNothing -> perror
        PJust midgard ->
          pmatch (ScriptContext.pspendDatumSummaryV1 # outputCbor) $ \case
            PNothing -> perror
            PJust spend ->
              OutputCommitment.pencodeLedgerOutputCommitment
                # ( pcon $ OutputCommitment.PLedgerOutputCommitmentV1
                      (pdata OutputCommitment.pledgerOutputCommitmentVersion)
                      (pdata $ pconstant outputIndex)
                      (pdata $ plengthBS # outputCbor)
                      (pdata $ Bounded.pfromBytes # 2 # pconstant outputIndex # outputCbor)
                      (pdata $ phexByteStr "68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                      (pdata 1_234_567)
                      (pdata 0)
                      (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
                      (pdata 0)
                      (pdata $ -1)
                      (pdata $ pconstant "")
                      (pdata 0)
                      (pdata $ pconstant "")
                      (pdata cardano)
                      (pdata midgard)
                      (pdata spend)
                  )

cekExactContextFixture :: forall s.
  Integer -> Term s PByteString -> Term s PCekContextControlV1
cekExactContextFixture stage controlHash =
  pmatch (cekContextFinalizeFixture stage 3 1 0 (pconstant "") controlHash) $ \c ->
    pcon $ PCekContextControlV1
      (pcekContext'stage c)
      (pcekContext'languageTag c)
      (pcekContext'programTermRoot c)
      (pcekContext'programEnvelopeHash c)
      (pcekContext'purposeKind c)
      (pcekContext'purposeIndex c)
      (pdata $ preplicateBS # 28 # (pintegerToByte # 0xaa))
      (pcekContext'subject c)
      (pcekContext'redeemerLeaf c)
      (pcekContext'redeemerContextControlHash c)
      (pcekContext'executionMemoryLimit c)
      (pcekContext'executionCpuLimit c)
      (pcekContext'referenceItems c)
      (pcekContext'spendItems c)
      (pcekContext'outputItems c)
      (pcekContext'signerItems c)
      (pcekContext'observerCount c)
      (pcekContext'observerItems c)
      (pcekContext'previousObserver c)
      (pcekContext'observerSummary c)
      (pcekContext'mintCursor c)
      (pcekContext'currentMintPolicy c)
      (pcekContext'currentMintAssets c)
      (pcekContext'mintPolicies c)
      (pcekContext'mintSummary c)

cekContextAssembleFitsOneStep :: forall s. Term s PBool
cekContextAssembleFitsOneStep =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  plet (CekData.psemanticDataSummaryV1 # pforgetData (pdata (0 :: Term s PInteger))) $ \redeemer ->
  pmatch
    ( ScriptContext.pscriptPurposeSummaryV1
        # 1 # (preplicateBS # 28 # (pintegerToByte # 0xaa))
        # pconstant "" # pconstant False
    )
    $ \case
      PNothing -> pconstant False
      PJust scriptInfo ->
        plet
          ( pcon $ PCekContextPartsControlV1
              (pdata CekData.pemptyDataPairSummaryV1)
              (pdata redeemer)
              (pdata scriptInfo)
          )
          $ \parts ->
        plet (phashCekContextPartsControlV1 # parts) $ \partsHash ->
        plet (cekExactContextFixture 11 partsHash) $ \contextControl ->
        plet
          ( pcon $ PCekTxInfoAssemblyControlV1
              ( pdata
                  $ ScriptContext.ptxInfoTailFieldsSummaryV1
                    # pconstant False
                    # (pfinalizeCekObserverItemsV1 # CekData.pemptyDataPairSummaryV1 # pconstant False)
                    # CekData.pemptyDataListSummaryV1
                    # (CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1)
                    # CekData.pemptyDataPairSummaryV1
                    # cekHash 2
              )
              (pdata redeemer)
              (pdata scriptInfo)
          )
          $ \assembly ->
        plet (phashCekTxInfoAssemblyControlV1 # assembly) $ \assemblyHash ->
        plet (cekExactContextFixture 12 assemblyHash) $ \nextContext ->
        plet (cekStateFixture (pcon PCek) 55 (cekHash 0xaa) 0 0) $ \pre ->
        plet (cekObserverTransitionWitness nativeControl nextContext 56) $ \witness ->
          pverifyCekContextAssembleControl
            # pre # witness # nativeControl # contextControl # parts # 0 # 0 # 0

cekContextTxInfoFinalizeFitsOneStep :: forall s. Term s PBool
cekContextTxInfoFinalizeFitsOneStep =
  pmatch (cekObserverProofSource NativeField.pemptyFieldCommitment) $ \(PPair nativeControl _) ->
  plet (CekData.psemanticDataSummaryV1 # pforgetData (pdata (0 :: Term s PInteger))) $ \redeemer ->
  pmatch
    ( ScriptContext.pscriptPurposeSummaryV1
        # 1 # (preplicateBS # 28 # (pintegerToByte # 0xaa))
        # pconstant "" # pconstant False
    )
    $ \case
      PNothing -> pconstant False
      PJust scriptInfo ->
        plet (pfinalizeCekObserverItemsV1 # CekData.pemptyDataPairSummaryV1 # pconstant False) $ \observerSummary ->
        plet (CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1) $ \mintSummary ->
        plet
          ( ScriptContext.ptxInfoTailFieldsSummaryV1
              # pconstant False # observerSummary # CekData.pemptyDataListSummaryV1
              # mintSummary # CekData.pemptyDataPairSummaryV1 # cekHash 2
          )
          $ \tailFields ->
        plet
          (pcon $ PCekTxInfoAssemblyControlV1 (pdata tailFields) (pdata redeemer) (pdata scriptInfo))
          $ \assembly ->
        plet (phashCekTxInfoAssemblyControlV1 # assembly) $ \assemblyHash ->
        plet (cekExactContextFixture 12 assemblyHash) $ \contextControl ->
        plet
          ( ScriptContext.ptxInfoFromTailSummaryV1
              # pconstant False
              # CekData.pemptyDataListSummaryV1 # CekData.pemptyDataListSummaryV1
              # CekData.pemptyDataListSummaryV1 # 0 # (-1) # (-1)
              # observerSummary # mintSummary # tailFields
          )
          $ \txInfo ->
        plet
          (pcon $ PCekFinalContextControlV1 (pdata txInfo) (pdata redeemer) (pdata scriptInfo))
          $ \finalContext ->
        plet (phashCekFinalContextControlV1 # finalContext) $ \finalHash ->
        plet (cekExactContextFixture 13 finalHash) $ \nextContext ->
        plet (cekStateFixture (pcon PCek) 55 (cekHash 0xaa) 0 0) $ \pre ->
        plet (cekObserverTransitionWitness nativeControl nextContext 56) $ \witness ->
          pverifyCekTxInfoFinalizeControl
            # pre # witness # nativeControl # contextControl # assembly # 0 # 0 # 0

cekContextAssembleTransition :: forall s. Term s PBool
cekContextAssembleTransition =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  plet
    (pcon $ PCekContextPartsControlV1 (pdata cekNonemptySequence) (pdata cekNonemptySummary) (pdata cekNonemptySummary))
    $ \partsControl ->
  plet (phashCekContextPartsControlV1 # partsControl) $ \partsHash ->
  plet (cekContextStageFixtureWithRedeemerHash 11 3 True partsHash) $ \contextControl ->
  plet (cekStateFixture (pcon PCek) 50 (cekHash 0xaa) 7 8) $ \pre ->
  plet
    ( pcon $ PCekTxInfoAssemblyControlV1
        ( pdata
            $ ScriptContext.ptxInfoTailFieldsSummaryV1
              # pconstant False
              # (CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1)
              # CekData.pemptyDataListSummaryV1
              # (CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1)
              # cekNonemptySequence
              # cekHash 2
        )
        (pdata cekNonemptySummary)
        (pdata cekNonemptySummary)
    )
    $ \assembly ->
  plet (phashCekTxInfoAssemblyControlV1 # assembly) $ \assemblyHash ->
  plet (cekContextStageFixtureWithRedeemerHash 12 3 True assemblyHash) $ \nextContext ->
  plet
    ( phashWorkWitness
        # pcon PCek
        # 51
        # ( pencodeCekWitnessV1
              # nativeControl
              # (pencodeCekContextControlV1 # nextContext)
              # 0
              # 3
              # 4
              # pconstant ""
              # 0
              # 0
              # (preplicateBS # 32 # (pintegerToByte # 0x22))
          )
    )
    $ \expectedWorkRoot ->
  plet (cekStateFixture (pcon PCek) 51 expectedWorkRoot 7 8) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata post)) $ \witness ->
    pverifyCekContextAssembleControl
      # pre # witness # nativeControl # contextControl # partsControl # 0 # 3 # 4
      #&& pnot
        # ( pverifyCekContextAssembleControl
              # pre
              # witness
              # nativeControl
              # cekContextStageFixture 11 3 True
              # partsControl
              # 0
              # 3
              # 4
          )

cekTxInfoFinalizeTransition :: forall s. Term s PBool
cekTxInfoFinalizeTransition =
  plet cekCompactFixture $ \compact ->
  plet (NativeCompact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet (nativeScriptsControlWithCompactCbor compactCbor) $ \nativeControl ->
  plet
    (pcon $ PCekTxInfoAssemblyControlV1 (pdata cekNonemptySequence) (pdata cekNonemptySummary) (pdata cekNonemptySummary))
    $ \assemblyControl ->
  plet (phashCekTxInfoAssemblyControlV1 # assemblyControl) $ \assemblyHash ->
  plet (cekContextStageFixtureWithRedeemerHash 12 3 True assemblyHash) $ \contextControl ->
  plet (cekStateFixture (pcon PCek) 60 (cekHash 0xaa) 7 8) $ \pre ->
  plet
    ( ScriptContext.ptxInfoFromTailSummaryV1
        # pconstant False
        # CekData.pemptyDataListSummaryV1
        # CekData.pemptyDataListSummaryV1
        # CekData.pemptyDataListSummaryV1
        # 100
        # (-1)
        # 200
        # (CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1)
        # (CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1)
        # cekNonemptySequence
    )
    $ \txInfo ->
  plet
    (pcon $ PCekFinalContextControlV1 (pdata txInfo) (pdata cekNonemptySummary) (pdata cekNonemptySummary))
    $ \finalContext ->
  plet (phashCekFinalContextControlV1 # finalContext) $ \finalHash ->
  plet (cekContextStageFixtureWithRedeemerHash 13 3 True finalHash) $ \nextContext ->
  plet
    ( phashWorkWitness
        # pcon PCek
        # 61
        # ( pencodeCekWitnessV1
              # nativeControl
              # (pencodeCekContextControlV1 # nextContext)
              # 0
              # 3
              # 4
              # pconstant ""
              # 0
              # 0
              # (preplicateBS # 32 # (pintegerToByte # 0x22))
          )
    )
    $ \expectedWorkRoot ->
  plet (cekStateFixture (pcon PCek) 61 expectedWorkRoot 7 8) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata post)) $ \witness ->
    pverifyCekTxInfoFinalizeControl
      # pre # witness # nativeControl # contextControl # assemblyControl # 0 # 3 # 4
      #&& pnot
        # ( pverifyCekTxInfoFinalizeControl
              # pre
              # witness
              # nativeControl
              # cekContextStageFixture 12 3 True
              # assemblyControl
              # 0
              # 3
              # 4
          )

cekCompactFixture :: forall s. Term s PNativeTxCompact
cekCompactFixture = pcon $ PNativeTxCompact
  ( pcon $ PNativeTxBodyCompact
      (cekHash 0x10)
      (cekHash 0x11)
      (cekHash 0x12)
      100
      (-1)
      200
      (cekHash 0x13)
      (cekHash 0x14)
      (cekHash 0x15)
      (cekHash 0x16)
      (cekHash 0x17)
      0
  )
  (cekHash 0x18)
  0

cekContextSeedFitsOneStep :: forall s. Term s PBool
cekContextSeedFitsOneStep =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  plet (CekData.psemanticDataSummaryV1 # pforgetData (pdata (0 :: Term s PInteger))) $ \redeemer ->
  pmatch
    ( ScriptContext.pscriptPurposeSummaryV1
        # 1 # (preplicateBS # 28 # (pintegerToByte # 0xaa))
        # pconstant "" # pconstant False
    )
    $ \case
      PNothing -> pconstant False
      PJust scriptInfo ->
        plet (pfinalizeCekObserverItemsV1 # CekData.pemptyDataPairSummaryV1 # pconstant False) $ \observerSummary ->
        plet (CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1) $ \mintSummary ->
        plet
          ( ScriptContext.ptxInfoTailFieldsSummaryV1
              # pconstant False # observerSummary # CekData.pemptyDataListSummaryV1
              # mintSummary # CekData.pemptyDataPairSummaryV1 # cekHash 2
          )
          $ \tailFields ->
        plet
          ( ScriptContext.ptxInfoFromTailSummaryV1
              # pconstant False
              # CekData.pemptyDataListSummaryV1 # CekData.pemptyDataListSummaryV1
              # CekData.pemptyDataListSummaryV1 # 0 # (-1) # (-1)
              # observerSummary # mintSummary # tailFields
          )
          $ \txInfo ->
        plet
          (pcon $ PCekFinalContextControlV1 (pdata txInfo) (pdata redeemer) (pdata scriptInfo))
          $ \finalContext ->
        plet (phashCekFinalContextControlV1 # finalContext) $ \finalHash ->
        plet (cekExactContextFixture 13 finalHash) $ \contextControl ->
        plet (ScriptContext.pscriptContextSummaryV1 # txInfo # redeemer # scriptInfo) $ \contextSummary ->
        plet (CekConstant.psemanticDataConstantRootV1 # contextSummary) $ \contextValue ->
        plet (CekProof.phashContextConstantTermV1 # contextValue) $ \contextTerm ->
        plet
          ( pcon $ CekMachine.PMachineStateV1
              (pdata CekMachine.pmodeCompute)
              (pdata 0)
              (pdata $ CekProof.phashApplicationTermV1 # (preplicateBS # 32 # (pintegerToByte # 0x11)) # contextTerm)
              (pdata CekProof.pemptyEnvironmentRootV1)
              (pdata CekProof.pemptyContinuationRootV1)
              (pdata 0) (pdata 0) (pdata 0)
          )
          $ \initialState ->
        plet
          ( phashWorkWitness # pcon PCek # 56
              # ( pencodeCekWitnessV1
                    # nativeControl # pconstant "" # 0 # 0 # 0
                    # (CekMachine.phashStateV1 # initialState)
                    # 0 # 0 # (preplicateBS # 32 # (pintegerToByte # 0x22))
                )
          )
          $ \workRoot ->
        plet (cekStateFixture (pcon PCek) 55 (cekHash 0xaa) 0 0) $ \pre ->
        plet
          ( pcon $ PValidationOneStepWitnessV1
              (pdata $ pconstant "")
              (pdata $ cekStateFixture (pcon PCek) 56 workRoot 0 0)
          )
          $ \witness ->
          pverifyCekContextSeedControl
            # pre # witness # nativeControl # contextControl # finalContext # 0 # 0 # 0

cekContextSeedTransition :: forall s. Term s PBool
cekContextSeedTransition =
  plet (emptyNativeScriptsControl 0) $ \nativeControl ->
  plet
    (pcon $ PCekFinalContextControlV1 (pdata cekNonemptySummary) (pdata cekNonemptySummary) (pdata cekNonemptySummary))
    $ \finalControl ->
  plet (phashCekFinalContextControlV1 # finalControl) $ \finalHash ->
  plet (cekContextStageFixtureWithRedeemerHash 13 3 True finalHash) $ \contextControl ->
  plet (cekStateFixture (pcon PCek) 70 (cekHash 0xaa) 7 8) $ \pre ->
  plet
    ( ScriptContext.pscriptContextSummaryV1
        # cekNonemptySummary
        # cekNonemptySummary
        # cekNonemptySummary
    )
    $ \contextSummary ->
  plet (CekConstant.psemanticDataConstantRootV1 # contextSummary) $ \contextValue ->
  plet (CekProof.phashContextConstantTermV1 # contextValue) $ \contextTerm ->
  plet
    ( pcon $ CekMachine.PMachineStateV1
        (pdata CekMachine.pmodeCompute)
        (pdata 0)
        (pdata $ CekProof.phashApplicationTermV1 # (preplicateBS # 32 # (pintegerToByte # 0x11)) # contextTerm)
        (pdata CekProof.pemptyEnvironmentRootV1)
        (pdata CekProof.pemptyContinuationRootV1)
        (pdata 0)
        (pdata 0)
        (pdata 0)
    )
    $ \initialState ->
  plet
    ( phashWorkWitness
        # pcon PCek
        # 71
        # ( pencodeCekWitnessV1
              # nativeControl
              # pconstant ""
              # 0
              # 3
              # 4
              # (CekMachine.phashStateV1 # initialState)
              # 0
              # 0
              # (preplicateBS # 32 # (pintegerToByte # 0x22))
          )
    )
    $ \expectedWorkRoot ->
  plet (cekStateFixture (pcon PCek) 71 expectedWorkRoot 7 8) $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata post)) $ \witness ->
    pverifyCekContextSeedControl
      # pre # witness # nativeControl # contextControl # finalControl # 0 # 3 # 4
      #&& pnot
        # ( pverifyCekContextSeedControl
              # pre
              # witness
              # nativeControl
              # cekContextStageFixture 13 3 True
              # finalControl
              # 0
              # 3
              # 4
          )

cekStateFixture :: forall s.
  Term s PValidationPhase ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationMachineStateV1
cekStateFixture = cekStateFixtureWithTransactionId (cekHash 2)

cekStateFixtureWithTransactionId :: forall s.
  Term s PByteString ->
  Term s PValidationPhase ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationMachineStateV1
cekStateFixtureWithTransactionId transactionId phase counter workRoot cpu memory = pcon $ PValidationMachineStateV1
  (pdata pmachineVersion)
  (pdata $ cekHash 1)
  (pdata transactionId)
  (pdata $ cekHash 3)
  (pdata $ cekHash 4)
  (pdata $ pcon PNormal)
  (pdata $ cekHash 5)
  (pdata phase)
  (pdata counter)
  (pdata workRoot)
  (pdata cpu)
  (pdata memory)
  (pdata $ pcon PPending)
  (pdata $ cekHash 0)
  (pdata $ cekHash 6)

cekBoundStateFixture :: forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PValidationPhase ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationMachineStateV1
cekBoundStateFixture transactionId transactionCommitment contextHash phase counter workRoot cpu memory =
  pcon $ PValidationMachineStateV1
    (pdata pmachineVersion)
    (pdata $ cekHash 1)
    (pdata transactionId)
    (pdata transactionCommitment)
    (pdata contextHash)
    (pdata $ pcon PNormal)
    (pdata $ cekHash 5)
    (pdata phase)
    (pdata counter)
    (pdata workRoot)
    (pdata cpu)
    (pdata memory)
    (pdata $ pcon PPending)
    (pdata $ cekHash 0)
    (pdata $ cekHash 6)

cekHash :: forall s. Integer -> Term s PByteString
cekHash byte = preplicateBS # 32 # (pintegerToByte # pconstant byte)

cekNonemptySummary :: forall s. Term s CekData.PDataSummaryV1
cekNonemptySummary = pcon $ CekData.PDataSummaryV1
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x51))
  (pdata 1)
  (pdata 1)

cekEmptySummary :: forall s. Term s CekData.PDataSummaryV1
cekEmptySummary = pcon $ CekData.PDataSummaryV1 (pdata $ pconstant "") (pdata 0) (pdata 0)

cekNonemptySequence :: forall s. Term s CekData.PDataSequenceSummaryV1
cekNonemptySequence = pcon $ CekData.PDataSequenceSummaryV1
  (pdata $ preplicateBS # 32 # (pintegerToByte # 0x52))
  (pdata 1)
  (pdata 1)
  (pdata 1)

pemptyProofDescriptor :: forall s. Term s ProofFold.PProofDescriptorV1
pemptyProofDescriptor = pcon $ ProofFold.PProofDescriptorV1
  { ProofFold.pproofDescriptor'version = pdata 1
  , ProofFold.pproofDescriptor'frameCount = pdata 0
  , ProofFold.pproofDescriptor'terminalCursor = pdata 0
  , ProofFold.pproofDescriptor'peaks = pdata pnil
  }
