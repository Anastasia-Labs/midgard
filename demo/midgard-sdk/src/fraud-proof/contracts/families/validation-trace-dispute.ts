import { Data, Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import {
  applyBlueprintParams,
  declaredParameters,
  deriveValidationTraceDeploymentId,
  type FaultProofBlueprint,
  getBlueprintValidator,
  getUnappliedScript,
  makeSpendingValidator,
  tryBuild,
} from "../blueprint.js";
import { buildSharedFaultProofContracts } from "../shared.js";
import {
  CEK_PROGRAM_MATERIAL_SPEND_TITLE,
  VALIDATION_TRACE_RESOLVER_COUNT,
} from "../titles.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES = {
  proofItem: "fraud_proofs/validation_trace/proof_item_v1.main.else",
  dispute: "fraud_proofs/validation_trace/dispute_v1.main.spend",
  source: "fraud_proofs/validation_trace/source_v1.main.spend",
  game: "fraud_proofs/validation_trace/game_v1.main.spend",
  boundary: "fraud_proofs/validation_trace/boundary_v1.main.spend",
  timeout: "fraud_proofs/validation_trace/timeout_v1.main.spend",
  award: "fraud_proofs/validation_trace/award_v1.main.spend",
  canonicalDecodeItemStages: {
    source:
      "fraud_proofs/validation_trace/canonical_decode_item_source_v1.main.spend",
    observe:
      "fraud_proofs/validation_trace/canonical_decode_item_observe_v1.main.spend",
    proof:
      "fraud_proofs/validation_trace/canonical_decode_item_proof_v1.main.spend",
    settlement:
      "fraud_proofs/validation_trace/canonical_decode_item_settlement_v1.main.spend",
  },
  scriptSourcesStageOneRedeemerStages: {
    envelope:
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_envelope_v1.main.spend",
    traversalNormalizer:
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_traversal_normalizer_v1.main.spend",
    outerNormalizer:
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_outer_normalizer_v1.main.spend",
    foldMapExecutor:
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_map_executor_v1.main.spend",
    finalizeFrameExecutor:
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finalize_frame_executor_v1.main.spend",
    settlement:
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_execution_settlement_v1.main.spend",
  },
  prepares: {
    canonicalDecode:
      "fraud_proofs/validation_trace/canonical_decode_v1.main.spend",
    compactBinding:
      "fraud_proofs/validation_trace/compact_binding_v1.main.spend",
    staticLedgerRules:
      "fraud_proofs/validation_trace/static_ledger_rules_v1.main.spend",
    inputSets: "fraud_proofs/validation_trace/input_sets_v1.main.spend",
    signatures: "fraud_proofs/validation_trace/signatures_v1.main.spend",
    phaseANativeScripts:
      "fraud_proofs/validation_trace/phase_a_native_scripts_v1.main.spend",
    phaseAScriptPreconditions:
      "fraud_proofs/validation_trace/phase_a_script_preconditions_v1.main.spend",
    resolveInputs: "fraud_proofs/validation_trace/resolve_inputs_v1.main.spend",
    scriptSources: "fraud_proofs/validation_trace/script_sources_v1.main.spend",
    nativeScripts: "fraud_proofs/validation_trace/native_scripts_v1.main.spend",
    scriptIntegrity:
      "fraud_proofs/validation_trace/script_integrity_v1.main.spend",
    cek: "fraud_proofs/validation_trace/cek_v1.main.spend",
    valueAndMint: "fraud_proofs/validation_trace/value_and_mint_v1.main.spend",
    ledgerDelta: "fraud_proofs/validation_trace/ledger_delta_v1.main.spend",
  },
  semantics: {
    canonicalDecodeEmpty:
      "fraud_proofs/validation_trace/canonical_decode_empty_semantic_v1.main.spend",
    canonicalDecodeItem:
      "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend",
    compactBinding:
      "fraud_proofs/validation_trace/compact_binding_semantic_v1.main.spend",
    staticLedgerRules:
      "fraud_proofs/validation_trace/static_ledger_rules_semantic_v1.main.spend",
    inputSetsEmpty:
      "fraud_proofs/validation_trace/input_sets_empty_semantic_v1.main.spend",
    inputSetsItem:
      "fraud_proofs/validation_trace/input_sets_item_semantic_v1.main.spend",
    signaturesAdvance:
      "fraud_proofs/validation_trace/signatures_advance_semantic_v1.main.spend",
    signaturesAddressItem:
      "fraud_proofs/validation_trace/signatures_address_item_semantic_v1.main.spend",
    signaturesRequiredItem:
      "fraud_proofs/validation_trace/signatures_required_item_semantic_v1.main.spend",
    signaturesHandoff:
      "fraud_proofs/validation_trace/signatures_handoff_semantic_v1.main.spend",
    phaseANativeScriptsAdvance:
      "fraud_proofs/validation_trace/phase_a_native_scripts_advance_semantic_v1.main.spend",
    phaseANativeScriptsItem:
      "fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1.main.spend",
    phaseANativeScriptsTokenHead:
      "fraud_proofs/validation_trace/phase_a_native_scripts_token_head_semantic_v1.main.spend",
    phaseANativeScriptsAllOrAnyContainerFramePayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1.main.spend",
    phaseANativeScriptsAllOrAnyEmptyContainerPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1.main.spend",
    phaseANativeScriptsAtLeastContainerFramePayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_container_frame_payload_semantic_v1.main.spend",
    phaseANativeScriptsAtLeastEmptyContainerPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_empty_container_payload_semantic_v1.main.spend",
    phaseANativeScriptsTimelockPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_timelock_payload_semantic_v1.main.spend",
    phaseANativeScriptsSignatureMembershipPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_membership_payload_semantic_v1.main.spend",
    phaseANativeScriptsSignatureEmptyPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_empty_payload_semantic_v1.main.spend",
    phaseANativeScriptsSignatureBelowFirstPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_below_first_payload_semantic_v1.main.spend",
    phaseANativeScriptsSignatureAboveLastPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_above_last_payload_semantic_v1.main.spend",
    phaseANativeScriptsSignatureBetweenPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_between_payload_semantic_v1.main.spend",
    phaseANativeScriptsFrame:
      "fraud_proofs/validation_trace/phase_a_native_scripts_frame_semantic_v1.main.spend",
    phaseAScriptPreconditions:
      "fraud_proofs/validation_trace/phase_a_script_preconditions_semantic_v1.main.spend",
    phaseAScriptPreconditionsItem:
      "fraud_proofs/validation_trace/phase_a_script_preconditions_item_semantic_v1.main.spend",
    resolveInputsInitial:
      "fraud_proofs/validation_trace/resolve_inputs_initial_semantic_v1.main.spend",
    resolveInputsFinish:
      "fraud_proofs/validation_trace/resolve_inputs_finish_semantic_v1.main.spend",
    resolveInputsMembershipBegin:
      "fraud_proofs/validation_trace/resolve_inputs_membership_begin_semantic_v1.main.spend",
    resolveInputsMembershipStep:
      "fraud_proofs/validation_trace/resolve_inputs_membership_step_semantic_v1.main.spend",
    resolveInputsMembershipFinalize:
      "fraud_proofs/validation_trace/resolve_inputs_membership_finalize_semantic_v1.main.spend",
    resolveInputsNonMembership:
      "fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1.main.spend",
    scriptSourcesNonOutput:
      "fraud_proofs/validation_trace/script_sources_non_output_semantic_v1.main.spend",
    scriptSourcesOutputProofBegin:
      "fraud_proofs/validation_trace/script_sources_output_proof_begin_semantic_v1.main.spend",
    scriptSourcesOutputProofStep:
      "fraud_proofs/validation_trace/script_sources_output_proof_step_semantic_v1.main.spend",
    scriptSourcesOutputProofFinalize:
      "fraud_proofs/validation_trace/script_sources_output_proof_finalize_semantic_v1.main.spend",
    scriptSourcesOutputProofFinish:
      "fraud_proofs/validation_trace/script_sources_output_proof_finish_semantic_v1.main.spend",
    scriptSourcesStageZeroBegin:
      "fraud_proofs/validation_trace/script_sources_stage_zero_begin_semantic_v1.main.spend",
    scriptSourcesStageZeroFinish:
      "fraud_proofs/validation_trace/script_sources_stage_zero_finish_semantic_v1.main.spend",
    scriptSourcesStageZeroHashBlock:
      "fraud_proofs/validation_trace/script_sources_stage_zero_hash_block_semantic_v1.main.spend",
    scriptSourcesStageZeroHashAdvance:
      "fraud_proofs/validation_trace/script_sources_stage_zero_hash_advance_semantic_v1.main.spend",
    scriptSourcesStageZeroHashTerminal:
      "fraud_proofs/validation_trace/script_sources_stage_zero_hash_terminal_semantic_v1.main.spend",
    scriptSourcesStageNineMismatch:
      "fraud_proofs/validation_trace/script_sources_stage_nine_mismatch_semantic_v1.main.spend",
    scriptSourcesStageNineNativeMatch:
      "fraud_proofs/validation_trace/script_sources_stage_nine_native_match_semantic_v1.main.spend",
    scriptSourcesStageNineEffectfulMatch:
      "fraud_proofs/validation_trace/script_sources_stage_nine_effectful_match_semantic_v1.main.spend",
    scriptSourcesStageNineMissing:
      "fraud_proofs/validation_trace/script_sources_stage_nine_missing_semantic_v1.main.spend",
    scriptSourcesStageOneFinish:
      "fraud_proofs/validation_trace/script_sources_stage_one_finish_semantic_v1.main.spend",
    scriptSourcesStageOneRedeemer:
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_semantic_v1.main.spend",
    scriptSourcesStageElevenFinish:
      "fraud_proofs/validation_trace/script_sources_stage_eleven_finish_semantic_v1.main.spend",
    scriptSourcesStageElevenSource:
      "fraud_proofs/validation_trace/script_sources_stage_eleven_source_semantic_v1.main.spend",
    scriptSourcesStageTwelveFinish:
      "fraud_proofs/validation_trace/script_sources_stage_twelve_finish_semantic_v1.main.spend",
    scriptSourcesStageTwelveRedeemer:
      "fraud_proofs/validation_trace/script_sources_stage_twelve_redeemer_semantic_v1.main.spend",
    scriptSourcesStageTenMissing:
      "fraud_proofs/validation_trace/script_sources_stage_ten_missing_semantic_v1.main.spend",
    scriptSourcesStageTenMismatch:
      "fraud_proofs/validation_trace/script_sources_stage_ten_mismatch_semantic_v1.main.spend",
    scriptSourcesStageTenMatch:
      "fraud_proofs/validation_trace/script_sources_stage_ten_match_semantic_v1.main.spend",
    scriptSourcesStageEightFinish:
      "fraud_proofs/validation_trace/script_sources_stage_eight_finish_semantic_v1.main.spend",
    scriptSourcesStageEightPurpose:
      "fraud_proofs/validation_trace/script_sources_stage_eight_purpose_semantic_v1.main.spend",
    scriptSourcesStageSevenObserver:
      "fraud_proofs/validation_trace/script_sources_stage_seven_observer_semantic_v1.main.spend",
    scriptSourcesStageSevenReceive:
      "fraud_proofs/validation_trace/script_sources_stage_seven_receive_semantic_v1.main.spend",
    scriptSourcesStageSevenFinish:
      "fraud_proofs/validation_trace/script_sources_stage_seven_finish_semantic_v1.main.spend",
    nativeScriptsTerminal:
      "fraud_proofs/validation_trace/native_scripts_terminal_semantic_v1.main.spend",
    nativeScriptsNative:
      "fraud_proofs/validation_trace/native_scripts_native_semantic_v1.main.spend",
    nativeScriptsEffectful:
      "fraud_proofs/validation_trace/native_scripts_effectful_semantic_v1.main.spend",
    scriptIntegrityAuthentication:
      "fraud_proofs/validation_trace/script_integrity_authentication_semantic_v1.main.spend",
    scriptIntegrityCompact:
      "fraud_proofs/validation_trace/script_integrity_compact_semantic_v1.main.spend",
    scriptIntegrityWitnessSet:
      "fraud_proofs/validation_trace/script_integrity_witness_set_semantic_v1.main.spend",
    scriptIntegrityFinalize:
      "fraud_proofs/validation_trace/script_integrity_finalize_semantic_v1.main.spend",
    cekFinish:
      "fraud_proofs/validation_trace/cek_finish_semantic_v1.main.spend",
    cekExecutionSelection:
      "fraud_proofs/validation_trace/cek_execution_selection_semantic_v1.main.spend",
    cekContextStep:
      "fraud_proofs/validation_trace/cek_context_step_semantic_v1.main.spend",
    cekCoreStep:
      "fraud_proofs/validation_trace/cek_core_step_semantic_v1.main.spend",
    valueAndMintBegin:
      "fraud_proofs/validation_trace/value_and_mint_begin_semantic_v1.main.spend",
    valueAndMintReplayBegin:
      "fraud_proofs/validation_trace/value_and_mint_replay_begin_semantic_v1.main.spend",
    valueAndMintReplayInput:
      "fraud_proofs/validation_trace/value_and_mint_replay_input_semantic_v1.main.spend",
    valueAndMintReplayAsset:
      "fraud_proofs/validation_trace/value_and_mint_replay_asset_semantic_v1.main.spend",
    valueAndMintReplayFinish:
      "fraud_proofs/validation_trace/value_and_mint_replay_finish_semantic_v1.main.spend",
    valueAndMintOutputDescriptor:
      "fraud_proofs/validation_trace/value_and_mint_output_descriptor_semantic_v1.main.spend",
    valueAndMintOutputAsset:
      "fraud_proofs/validation_trace/value_and_mint_output_asset_semantic_v1.main.spend",
    valueAndMintOutputFinish:
      "fraud_proofs/validation_trace/value_and_mint_output_finish_semantic_v1.main.spend",
    valueAndMintMintAsset:
      "fraud_proofs/validation_trace/value_and_mint_mint_asset_semantic_v1.main.spend",
    valueAndMintMintFinish:
      "fraud_proofs/validation_trace/value_and_mint_mint_finish_semantic_v1.main.spend",
    valueAndMintFinalize:
      "fraud_proofs/validation_trace/value_and_mint_finalize_semantic_v1.main.spend",
    ledgerDeltaOperation:
      "fraud_proofs/validation_trace/ledger_delta_operation_semantic_v1.main.spend",
    ledgerDeltaReplay:
      "fraud_proofs/validation_trace/ledger_delta_replay_semantic_v1.main.spend",
    ledgerDeltaReplayFinish:
      "fraud_proofs/validation_trace/ledger_delta_replay_finish_semantic_v1.main.spend",
    ledgerDeltaOutput:
      "fraud_proofs/validation_trace/ledger_delta_output_semantic_v1.main.spend",
    ledgerDeltaOutputFinish:
      "fraud_proofs/validation_trace/ledger_delta_output_finish_semantic_v1.main.spend",
    ledgerDeltaProofFrame:
      "fraud_proofs/validation_trace/ledger_delta_proof_frame_semantic_v1.main.spend",
    ledgerDeltaFinalize:
      "fraud_proofs/validation_trace/ledger_delta_finalize_semantic_v1.main.spend",
    ledgerDeltaTerminal:
      "fraud_proofs/validation_trace/ledger_delta_terminal_semantic_v1.main.spend",
  },
} as const;

export type ValidationTraceDisputeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly validationTraceDispute: FraudProofChain & {
    readonly cekProgramMaterial: SpendingValidator;
    readonly opener: SpendingValidator;
    readonly source: SpendingValidator;
    readonly game: SpendingValidator;
    readonly boundary: SpendingValidator;
    readonly timeout: SpendingValidator;
    readonly award: SpendingValidator;
    readonly proofItem: SpendingValidator;
    readonly canonicalDecodeItemStages: {
      readonly source: SpendingValidator;
      readonly observe: SpendingValidator;
      readonly proof: SpendingValidator;
      readonly settlement: SpendingValidator;
    };
    readonly scriptSourcesStageOneRedeemerStages: {
      readonly envelope: SpendingValidator;
      readonly traversalNormalizer: SpendingValidator;
      readonly outerNormalizer: SpendingValidator;
      readonly foldMapExecutor: SpendingValidator;
      readonly finalizeFrameExecutor: SpendingValidator;
      readonly settlement: SpendingValidator;
    };
    readonly prepareResolvers: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
    readonly semanticResolvers: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
    readonly resolvers: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildValidationTraceDisputeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildValidationTraceDisputeChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  fraudProofCataloguePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofCataloguePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
}): Effect.Effect<
  ValidationTraceDisputeFaultProofContracts["validationTraceDispute"],
  Error
> =>
  Effect.gen(function* () {
    const cekProgramMaterial = yield* tryBuild(
      "Failed to build immutable CEK program-material validator",
      () =>
        makeSpendingValidator(
          network,
          getUnappliedScript(blueprint, CEK_PROGRAM_MATERIAL_SPEND_TITLE),
        ),
    );
    const award = yield* tryBuild(
      "Failed to build validation-trace award validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.award,
            [
              computationThread.policyId,
              fraudProof.policyId,
              fraudProofTokenAddressData,
            ],
          ),
        ),
    );

    const deploymentId = deriveValidationTraceDeploymentId(
      fraudProofCataloguePolicyId,
    );
    const buildStageOneRedeemerExecutor = (
      title: string,
      label: string,
    ): Effect.Effect<SpendingValidator, Error> =>
      tryBuild(`Failed to build stage-one redeemer ${label} validator`, () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(blueprint, title, [
            deploymentId,
            computationThread.policyId,
          ]),
        ),
      );
    const stageOneRedeemerFoldMapExecutor =
      yield* buildStageOneRedeemerExecutor(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.foldMapExecutor,
        "fold-map executor",
      );
    const stageOneRedeemerFinalizeFrameExecutor =
      yield* buildStageOneRedeemerExecutor(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
          .scriptSourcesStageOneRedeemerStages.finalizeFrameExecutor,
        "finalize-frame executor",
      );
    const stageOneRedeemerOuterNormalizer = yield* tryBuild(
      "Failed to build stage-one redeemer outer normalizer validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
              .scriptSourcesStageOneRedeemerStages.outerNormalizer,
            [deploymentId, computationThread.policyId],
          ),
        ),
    );
    const stageOneRedeemerTraversalNormalizer = yield* tryBuild(
      "Failed to build stage-one redeemer traversal normalizer validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
              .scriptSourcesStageOneRedeemerStages.traversalNormalizer,
            [deploymentId, computationThread.policyId],
          ),
        ),
    );
    const stageOneRedeemerSettlement = yield* tryBuild(
      "Failed to build stage-one redeemer settlement validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
              .scriptSourcesStageOneRedeemerStages.settlement,
            [
              deploymentId,
              stageOneRedeemerTraversalNormalizer.spendingScriptHash,
              stageOneRedeemerOuterNormalizer.spendingScriptHash,
              stageOneRedeemerFoldMapExecutor.spendingScriptHash,
              stageOneRedeemerFinalizeFrameExecutor.spendingScriptHash,
              award.spendingScriptHash,
              computationThread.policyId,
            ],
          ),
        ),
    );
    const stageOneRedeemerEnvelope = yield* tryBuild(
      "Failed to build stage-one redeemer envelope validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
              .scriptSourcesStageOneRedeemerStages.envelope,
            [
              deploymentId,
              stageOneRedeemerTraversalNormalizer.spendingScriptHash,
              stageOneRedeemerOuterNormalizer.spendingScriptHash,
              stageOneRedeemerFoldMapExecutor.spendingScriptHash,
              stageOneRedeemerFinalizeFrameExecutor.spendingScriptHash,
              stageOneRedeemerSettlement.spendingScriptHash,
              computationThread.policyId,
            ],
          ),
        ),
    );
    const scriptSourcesStageOneRedeemerStages = {
      envelope: stageOneRedeemerEnvelope,
      traversalNormalizer: stageOneRedeemerTraversalNormalizer,
      outerNormalizer: stageOneRedeemerOuterNormalizer,
      foldMapExecutor: stageOneRedeemerFoldMapExecutor,
      finalizeFrameExecutor: stageOneRedeemerFinalizeFrameExecutor,
      settlement: stageOneRedeemerSettlement,
    } as const;

    const semanticTitles = Object.values(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
    );
    const proofItem = yield* tryBuild(
      "Failed to build validation-trace proof-item validator",
      () =>
        makeSpendingValidator(
          network,
          getUnappliedScript(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.proofItem,
          ),
        ),
    );
    const canonicalDecodeItemSettlement = yield* tryBuild(
      "Failed to build validation-trace canonical item settlement",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
              .canonicalDecodeItemStages.settlement,
            [award.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );
    const canonicalDecodeItemProof = yield* tryBuild(
      "Failed to build validation-trace canonical item proof verifier",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
              .canonicalDecodeItemStages.proof,
            [
              canonicalDecodeItemSettlement.spendingScriptHash,
              computationThread.policyId,
            ],
          ),
        ),
    );
    const canonicalDecodeItemObserve = yield* tryBuild(
      "Failed to build validation-trace canonical item observer",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
              .canonicalDecodeItemStages.observe,
            [
              canonicalDecodeItemProof.spendingScriptHash,
              computationThread.policyId,
              proofItem.spendingScriptHash,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );
    const canonicalDecodeItemSource = yield* tryBuild(
      "Failed to build validation-trace canonical item source binder",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES
              .canonicalDecodeItemStages.source,
            [
              canonicalDecodeItemObserve.spendingScriptHash,
              computationThread.policyId,
            ],
          ),
        ),
    );
    const canonicalDecodeItemStages = {
      source: canonicalDecodeItemSource,
      observe: canonicalDecodeItemObserve,
      proof: canonicalDecodeItemProof,
      settlement: canonicalDecodeItemSettlement,
    } as const;
    /**
     * Every parameter any semantic resolver declares, keyed by the name the
     * compiler recorded for it.
     *
     * The semantic family is deployed by iterating a title list, so the loop
     * cannot carry a hand-written argument list per title without drifting from
     * what the validators declare — which is precisely how the ten resolvers
     * that gained `field_preimage_certificate_policy_id` in #592 kept being
     * deployed with two arguments and became always-succeeds scripts (#605).
     * Resolving each declared parameter BY NAME makes the blueprint the only
     * authority on both the count and the order: a resolver that grows a
     * parameter is served automatically if the name is known, and refused
     * loudly if it is not. A count-only rule could not work here anyway — the
     * canonical-decode item resolver declares a parameter set that is
     * different entirely, not `award_script_hash` plus one (three names
     * before #620's transition-only subtraction dropped
     * `proof_item_script_hash`, two after).
     */
    const semanticResolverParameterValues = new Map<string, Data>([
      ["award_script_hash", award.spendingScriptHash],
      ["computation_thread_policy_id", computationThread.policyId],
      [
        "field_preimage_certificate_policy_id",
        fieldPreimageCertificatePolicyId,
      ],
      [
        "source_binder_script_hash",
        canonicalDecodeItemSource.spendingScriptHash,
      ],
      ["proof_item_script_hash", proofItem.spendingScriptHash],
      [
        "cek_program_material_script_hash",
        cekProgramMaterial.spendingScriptHash,
      ],
    ]);
    const semanticResolverParams = (title: string): readonly Data[] =>
      declaredParameters(getBlueprintValidator(blueprint, title)).map(
        (parameter) => {
          const value = semanticResolverParameterValues.get(parameter.title);
          if (value === undefined) {
            throw new Error(
              `Semantic resolver "${title}" declares parameter ` +
                `"${parameter.title}", which this deployment builder has no value ` +
                "for. Add it to the semantic-resolver parameter set rather than " +
                "deploying the resolver under-applied (#609).",
            );
          }
          return value;
        },
      );
    const builtSemanticResolvers: SpendingValidator[] = [];
    for (const [index, title] of semanticTitles.entries()) {
      builtSemanticResolvers.push(
        yield* tryBuild(
          `Failed to build validation-trace semantic resolver ${index.toString()}`,
          () =>
            makeSpendingValidator(
              network,
              applyBlueprintParams(
                blueprint,
                title,
                semanticResolverParams(title),
              ),
            ),
        ),
      );
    }
    if (builtSemanticResolvers.length !== 90) {
      return yield* Effect.fail(
        new Error("Validation-trace semantic resolver set is incomplete"),
      );
    }
    const baseSemanticResolvers = [
      builtSemanticResolvers[0]!,
      builtSemanticResolvers[1]!,
      builtSemanticResolvers[2]!,
      builtSemanticResolvers[3]!,
      builtSemanticResolvers[4]!,
      builtSemanticResolvers[5]!,
      builtSemanticResolvers[6]!,
      builtSemanticResolvers[7]!,
      builtSemanticResolvers[8]!,
      builtSemanticResolvers[9]!,
      builtSemanticResolvers[10]!,
      builtSemanticResolvers[11]!,
      builtSemanticResolvers[12]!,
      builtSemanticResolvers[13]!,
      builtSemanticResolvers[14]!,
      builtSemanticResolvers[15]!,
      builtSemanticResolvers[16]!,
      builtSemanticResolvers[17]!,
      builtSemanticResolvers[18]!,
      builtSemanticResolvers[19]!,
      builtSemanticResolvers[20]!,
      builtSemanticResolvers[21]!,
      builtSemanticResolvers[22]!,
      builtSemanticResolvers[23]!,
      builtSemanticResolvers[24]!,
      builtSemanticResolvers[25]!,
      builtSemanticResolvers[26]!,
      builtSemanticResolvers[27]!,
      builtSemanticResolvers[28]!,
      builtSemanticResolvers[29]!,
      builtSemanticResolvers[30]!,
      builtSemanticResolvers[31]!,
      builtSemanticResolvers[32]!,
      builtSemanticResolvers[33]!,
      builtSemanticResolvers[34]!,
      builtSemanticResolvers[35]!,
      builtSemanticResolvers[36]!,
      builtSemanticResolvers[37]!,
      builtSemanticResolvers[38]!,
      builtSemanticResolvers[39]!,
      builtSemanticResolvers[40]!,
      builtSemanticResolvers[41]!,
      builtSemanticResolvers[42]!,
      builtSemanticResolvers[43]!,
      builtSemanticResolvers[44]!,
      builtSemanticResolvers[45]!,
      builtSemanticResolvers[46]!,
      builtSemanticResolvers[47]!,
      builtSemanticResolvers[48]!,
      builtSemanticResolvers[49]!,
      builtSemanticResolvers[50]!,
      builtSemanticResolvers[51]!,
      builtSemanticResolvers[52]!,
      builtSemanticResolvers[53]!,
      builtSemanticResolvers[54]!,
      builtSemanticResolvers[55]!,
      builtSemanticResolvers[56]!,
      builtSemanticResolvers[57]!,
      builtSemanticResolvers[58]!,
      builtSemanticResolvers[59]!,
      builtSemanticResolvers[60]!,
      builtSemanticResolvers[61]!,
      builtSemanticResolvers[62]!,
      builtSemanticResolvers[63]!,
      builtSemanticResolvers[64]!,
      builtSemanticResolvers[65]!,
      builtSemanticResolvers[66]!,
      builtSemanticResolvers[67]!,
      builtSemanticResolvers[68]!,
      builtSemanticResolvers[69]!,
      builtSemanticResolvers[70]!,
      builtSemanticResolvers[71]!,
      builtSemanticResolvers[72]!,
      builtSemanticResolvers[73]!,
      builtSemanticResolvers[74]!,
      builtSemanticResolvers[75]!,
      builtSemanticResolvers[76]!,
      builtSemanticResolvers[77]!,
      builtSemanticResolvers[78]!,
      builtSemanticResolvers[79]!,
      builtSemanticResolvers[80]!,
      builtSemanticResolvers[81]!,
      builtSemanticResolvers[82]!,
      builtSemanticResolvers[83]!,
      builtSemanticResolvers[84]!,
      builtSemanticResolvers[85]!,
      builtSemanticResolvers[86]!,
      builtSemanticResolvers[87]!,
      builtSemanticResolvers[88]!,
      builtSemanticResolvers[89]!,
    ] as const;
    const semanticResolvers = [
      ...baseSemanticResolvers,
      stageOneRedeemerEnvelope,
    ] as const;
    const semanticResolverGroups = [
      [semanticResolvers[0], semanticResolvers[1]],
      [semanticResolvers[2]],
      [semanticResolvers[3]],
      [semanticResolvers[4], semanticResolvers[5]],
      [
        semanticResolvers[6],
        semanticResolvers[7],
        semanticResolvers[8],
        semanticResolvers[9],
      ],
      [
        semanticResolvers[10],
        semanticResolvers[11],
        semanticResolvers[12],
        semanticResolvers[13],
        semanticResolvers[14],
        semanticResolvers[15],
        semanticResolvers[16],
        semanticResolvers[17],
        semanticResolvers[18],
        semanticResolvers[19],
        semanticResolvers[20],
        semanticResolvers[21],
        semanticResolvers[22],
        semanticResolvers[23],
      ],
      [semanticResolvers[24], semanticResolvers[25]],
      [
        semanticResolvers[26],
        semanticResolvers[27],
        semanticResolvers[28],
        semanticResolvers[29],
        semanticResolvers[30],
        semanticResolvers[31],
      ],
      [
        semanticResolvers[32],
        semanticResolvers[33],
        semanticResolvers[34],
        semanticResolvers[35],
        semanticResolvers[36],
        semanticResolvers[37],
        semanticResolvers[38],
        semanticResolvers[39],
        semanticResolvers[40],
        semanticResolvers[41],
        semanticResolvers[42],
        semanticResolvers[43],
        semanticResolvers[44],
        semanticResolvers[45],
        semanticResolvers[46],
        semanticResolvers[47],
        semanticResolvers[48],
        semanticResolvers[49],
        semanticResolvers[50],
        semanticResolvers[51],
        semanticResolvers[52],
        semanticResolvers[53],
        semanticResolvers[54],
        semanticResolvers[55],
        semanticResolvers[56],
        semanticResolvers[57],
        semanticResolvers[58],
        semanticResolvers[59],
        semanticResolvers[90],
      ],
      [semanticResolvers[60], semanticResolvers[61], semanticResolvers[62]],
      [
        semanticResolvers[63],
        semanticResolvers[64],
        semanticResolvers[65],
        semanticResolvers[66],
      ],
      [
        semanticResolvers[67],
        semanticResolvers[68],
        semanticResolvers[69],
        semanticResolvers[70],
      ],
      [
        semanticResolvers[71],
        semanticResolvers[72],
        semanticResolvers[73],
        semanticResolvers[74],
        semanticResolvers[75],
        semanticResolvers[76],
        semanticResolvers[77],
        semanticResolvers[78],
        semanticResolvers[79],
        semanticResolvers[80],
        semanticResolvers[81],
      ],
      [
        semanticResolvers[82],
        semanticResolvers[83],
        semanticResolvers[84],
        semanticResolvers[85],
        semanticResolvers[86],
        semanticResolvers[87],
        semanticResolvers[88],
        semanticResolvers[89],
      ],
    ] as const;
    const semanticResolverHashesSchema = Data.Array(Data.Bytes());
    type SemanticResolverHashes = Data.Static<
      typeof semanticResolverHashesSchema
    >;
    const SemanticResolverHashes =
      semanticResolverHashesSchema as unknown as SemanticResolverHashes;

    const prepareTitles = Object.values(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares,
    );
    const builtPrepareResolvers: SpendingValidator[] = [];
    for (const [index, title] of prepareTitles.entries()) {
      const semanticResolverHashesData = Data.from(
        Data.to(
          semanticResolverGroups[index]!.map(
            ({ spendingScriptHash }) => spendingScriptHash,
          ),
          SemanticResolverHashes,
        ),
      ) as Data;
      builtPrepareResolvers.push(
        yield* tryBuild(
          `Failed to build validation-trace prepare resolver ${index.toString()}`,
          () =>
            makeSpendingValidator(
              network,
              applyBlueprintParams(blueprint, title, [
                semanticResolverHashesData,
                computationThread.policyId,
              ]),
            ),
        ),
      );
    }
    if (builtPrepareResolvers.length !== 14) {
      return yield* Effect.fail(
        new Error("Validation-trace prepare resolver set is incomplete"),
      );
    }
    const prepareResolvers = [
      builtPrepareResolvers[0]!,
      builtPrepareResolvers[1]!,
      builtPrepareResolvers[2]!,
      builtPrepareResolvers[3]!,
      builtPrepareResolvers[4]!,
      builtPrepareResolvers[5]!,
      builtPrepareResolvers[6]!,
      builtPrepareResolvers[7]!,
      builtPrepareResolvers[8]!,
      builtPrepareResolvers[9]!,
      builtPrepareResolvers[10]!,
      builtPrepareResolvers[11]!,
      builtPrepareResolvers[12]!,
      builtPrepareResolvers[13]!,
    ] as const;

    const resolvers = [
      prepareResolvers[0],
      prepareResolvers[1],
      prepareResolvers[2],
      prepareResolvers[3],
      prepareResolvers[4],
      prepareResolvers[5],
      prepareResolvers[6],
      prepareResolvers[7],
      prepareResolvers[8],
      prepareResolvers[9],
      prepareResolvers[10],
      prepareResolvers[11],
      prepareResolvers[12],
      prepareResolvers[13],
    ] as const;
    if (
      new Set(resolvers.map(({ spendingScriptHash }) => spendingScriptHash))
        .size !== VALIDATION_TRACE_RESOLVER_COUNT
    ) {
      return yield* Effect.fail(
        new Error("Validation-trace resolver hashes must be distinct"),
      );
    }
    const resolverHashesSchema = Data.Array(Data.Bytes());
    type ResolverHashes = Data.Static<typeof resolverHashesSchema>;
    const ResolverHashes = resolverHashesSchema as unknown as ResolverHashes;
    const resolverHashesData = Data.from(
      Data.to(
        resolvers.map(({ spendingScriptHash }) => spendingScriptHash),
        ResolverHashes,
      ),
    );

    const boundary = yield* tryBuild(
      "Failed to build validation-trace boundary validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.boundary,
            [resolverHashesData, computationThread.policyId],
          ),
        ),
    );
    const timeout = yield* tryBuild(
      "Failed to build validation-trace timeout validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.timeout,
            [
              computationThread.policyId,
              fraudProof.policyId,
              fraudProofTokenAddressData,
            ],
          ),
        ),
    );
    const game = yield* tryBuild(
      "Failed to build validation-trace midpoint game validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.game,
            [
              boundary.spendingScriptHash,
              timeout.spendingScriptHash,
              computationThread.policyId,
            ],
          ),
        ),
    );
    const source = yield* tryBuild(
      "Failed to build validation-trace source validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.source,
            [
              game.spendingScriptHash,
              award.spendingScriptHash,
              computationThread.policyId,
            ],
          ),
        ),
    );
    const dispute = yield* tryBuild(
      "Failed to build validation-trace dispute opener",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.dispute,
            [
              source.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: dispute,
      steps: [
        dispute,
        source,
        game,
        boundary,
        timeout,
        award,
        proofItem,
        ...semanticResolvers,
        stageOneRedeemerTraversalNormalizer,
        stageOneRedeemerOuterNormalizer,
        stageOneRedeemerFoldMapExecutor,
        stageOneRedeemerFinalizeFrameExecutor,
        stageOneRedeemerSettlement,
        ...Object.values(canonicalDecodeItemStages),
        ...prepareResolvers,
      ],
      opener: dispute,
      source,
      game,
      boundary,
      timeout,
      award,
      proofItem,
      cekProgramMaterial,
      canonicalDecodeItemStages,
      scriptSourcesStageOneRedeemerStages,
      prepareResolvers,
      semanticResolvers,
      resolvers,
    };
  });

export const buildValidationTraceDisputeFaultProofContracts = (
  params: BuildValidationTraceDisputeFaultProofContractsParams,
): Effect.Effect<ValidationTraceDisputeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const validationTraceDispute = yield* buildValidationTraceDisputeChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      validationTraceDispute,
    };
  });
