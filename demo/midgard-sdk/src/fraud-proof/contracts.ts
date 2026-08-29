import {
  Address,
  applyParamsToScript,
  Data,
  fromHex,
  MintingPolicy,
  mintingPolicyToId,
  Network,
  SpendingValidator as LucidSpendingValidator,
  toHex,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";

import {
  AddressData,
  addressDataFromBech32,
  AuthenticatedValidator,
  type FraudProofs,
  MintingValidator,
  SpendingValidator,
} from "@/common.js";

/**
 * One entry of a blueprint validator's `parameters[]`: the compiler's own
 * record of a `validator main(...)` parameter, in declaration order.
 */
export type FaultProofBlueprintParameter = {
  readonly title: string;
};

export type FaultProofBlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
  /**
   * The parameters the compiled script declares, in declaration order. Carried
   * (rather than dropped at parse time, as it was before #609) because it is
   * the only authority on how many terms must be applied before the script is
   * a complete validator — see {@link applyBlueprintParams}.
   */
  readonly parameters: readonly FaultProofBlueprintParameter[];
};

export type FaultProofBlueprint = {
  readonly validators: readonly FaultProofBlueprintValidator[];
};

export const DOUBLE_SPEND_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/double_spend/step_01.main.spend",
  step02: "fraud_proofs/double_spend/step_02.main.spend",
  step03: "fraud_proofs/double_spend/step_03.main.spend",
  step04: "fraud_proofs/double_spend/step_04.main.spend",
} as const;

export const NON_EXISTENT_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/no_input/step_01.main.spend",
  step02: "fraud_proofs/no_input/step_02.main.spend",
  step03: "fraud_proofs/no_input/step_03.main.spend",
  step04: "fraud_proofs/no_input/step_04.main.spend",
} as const;

export const NO_REFERENCE_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/no_reference_input/step_01.main.spend",
  step02: "fraud_proofs/no_reference_input/step_02.main.spend",
  step03: "fraud_proofs/no_reference_input/step_03.main.spend",
  step04: "fraud_proofs/no_reference_input/step_04.main.spend",
} as const;

export const INPUT_NO_IDX_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/input_no_idx/step_01.main.spend",
  step02: "fraud_proofs/input_no_idx/step_02.main.spend",
  step03: "fraud_proofs/input_no_idx/step_03.main.spend",
  step04: "fraud_proofs/input_no_idx/step_04.main.spend",
} as const;

export const REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/reference_input_no_idx/step_01.main.spend",
  step02: "fraud_proofs/reference_input_no_idx/step_02.main.spend",
  step03: "fraud_proofs/reference_input_no_idx/step_03.main.spend",
  step04: "fraud_proofs/reference_input_no_idx/step_04.main.spend",
} as const;

export const INVALID_RANGE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/invalid_range/step_01.main.spend",
  step02: "fraud_proofs/invalid_range/step_02.main.spend",
} as const;

export const INVALID_SIGNATURE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/invalid_signature/step_01.main.spend",
  step02: "fraud_proofs/invalid_signature/step_02.main.spend",
} as const;

export const ZERO_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/zero_input/step_01.main.spend",
  step02: "fraud_proofs/zero_input/step_02.main.spend",
} as const;

export const DA_HASH_PREIMAGE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/da_hash_preimage/step_01.main.spend",
  step02: "fraud_proofs/da_hash_preimage/step_02.main.spend",
} as const;

export const FABRICATED_DEPOSIT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/fabricated_deposit/step_01.main.spend",
  step02: "fraud_proofs/fabricated_deposit/step_02.main.spend",
  step03: "fraud_proofs/fabricated_deposit/step_03.main.spend",
  step04: "fraud_proofs/fabricated_deposit/step_04.main.spend",
} as const;

export const FABRICATED_WITHDRAWAL_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/fabricated_withdrawal/step_01.main.spend",
  step02: "fraud_proofs/fabricated_withdrawal/step_02.main.spend",
  step03: "fraud_proofs/fabricated_withdrawal/step_03.main.spend",
  step04: "fraud_proofs/fabricated_withdrawal/step_04.main.spend",
} as const;

export const NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/native_script_decoding/step_01.main.spend",
  step02: "fraud_proofs/native_script_decoding/step_02.main.spend",
  step03: "fraud_proofs/native_script_decoding/step_03.main.spend",
  step04: "fraud_proofs/native_script_decoding/step_04.main.spend",
} as const;

export const MISSING_SIGNATURE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/missing_signature/step_01.main.spend",
  step02: "fraud_proofs/missing_signature/step_02.main.spend",
  step03: "fraud_proofs/missing_signature/step_03.main.spend",
  step04: "fraud_proofs/missing_signature/step_04.main.spend",
} as const;

export const MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/missing_native_script_tx/step_01.main.spend",
  step02: "fraud_proofs/missing_native_script_tx/step_02.main.spend",
  step03: "fraud_proofs/missing_native_script_tx/step_03.main.spend",
  step04: "fraud_proofs/missing_native_script_tx/step_04.main.spend",
  step05: "fraud_proofs/missing_native_script_tx/step_05.main.spend",
  step06: "fraud_proofs/missing_native_script_tx/step_06.main.spend",
} as const;

export const WITHDRAWN_REFERENCE_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/withdrawn_reference_input/step_01.main.spend",
  step02: "fraud_proofs/withdrawn_reference_input/step_02.main.spend",
  step03: "fraud_proofs/withdrawn_reference_input/step_03.main.spend",
} as const;

export const CANONICAL_DECODABILITY_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/canonical_decodability/step_01.main.spend",
  step02: "fraud_proofs/canonical_decodability/step_02.main.spend",
} as const;

export const COMMITTED_FIELD_SHAPE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/committed_field_shape/step_01.main.spend",
  step02: "fraud_proofs/committed_field_shape/step_02.main.spend",
} as const;

export const MIN_FEE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/min_fee/step_01.main.spend",
  step02: "fraud_proofs/min_fee/step_02.main.spend",
} as const;

export const WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/withdrawal_mistag/step_01.main.spend",
  step02: "fraud_proofs/withdrawal_mistag/step_02.main.spend",
  step03: "fraud_proofs/withdrawal_mistag/step_03.main.spend",
  step04: "fraud_proofs/withdrawal_mistag/step_04.main.spend",
  step05: "fraud_proofs/withdrawal_mistag/step_05.main.spend",
} as const;

export const DOUBLE_WITHDRAW_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/double_withdraw/step_01.main.spend",
  step02: "fraud_proofs/double_withdraw/step_02.main.spend",
} as const;

export const CROSS_BLOCK_DUPLICATE_EVENT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/cross_block_duplicate_event/step_01.main.spend",
  step02: "fraud_proofs/cross_block_duplicate_event/step_02.main.spend",
} as const;

export const L2_TX_MISTAG_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/l2_tx_mistag/step_01.main.spend",
  step02: "fraud_proofs/l2_tx_mistag/step_02.main.spend",
} as const;

export const WITHDRAWN_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/withdrawn_input/step_01.main.spend",
  step02: "fraud_proofs/withdrawn_input/step_02.main.spend",
  step03: "fraud_proofs/withdrawn_input/step_03.main.spend",
} as const;

export const VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/value_not_preserved/step_01.main.spend",
  step02: "fraud_proofs/value_not_preserved/step_02.main.spend",
  step03: "fraud_proofs/value_not_preserved/step_03.main.spend",
  step04: "fraud_proofs/value_not_preserved/step_04.main.spend",
} as const;

export const INPUT_SET_UNIQUENESS_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/input_set_uniqueness/step_01.main.spend",
  step02: "fraud_proofs/input_set_uniqueness/step_02.main.spend",
} as const;

export const MINT_AUTHORIZATION_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/mint_authorization/step_01.main.spend",
  step02: "fraud_proofs/mint_authorization/step_02.main.spend",
  step03: "fraud_proofs/mint_authorization/step_03.main.spend",
  step04: "fraud_proofs/mint_authorization/step_04.main.spend",
  step05: "fraud_proofs/mint_authorization/step_05.main.spend",
} as const;

export const TRANSITION_TRACE_FAULT_PROOF_TITLES = {
  route: "fraud_proofs/transition_trace/route_v1.main.spend",
  control: "fraud_proofs/transition_trace/control_v1.main.spend",
  source: "fraud_proofs/transition_trace/source_v1.main.spend",
  withdrawal: "fraud_proofs/transition_trace/withdrawal_v1.main.spend",
  forced: "fraud_proofs/transition_trace/forced_v1.main.spend",
  accepted: "fraud_proofs/transition_trace/accepted_transaction_v1.main.spend",
  deposit: "fraud_proofs/transition_trace/deposit_v1.main.spend",
  l1Event: "fraud_proofs/transition_trace/l1_event_v1.main.spend",
  duplicate: "fraud_proofs/transition_trace/duplicate_v1.main.spend",
} as const;

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

export const CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1 =
  "user_events/cek_program_material_v1.spend.spend";

export const VALIDATION_TRACE_RESOLVER_COUNT_V1 = 14;

export const FAULT_PROOF_SHARED_TITLES = {
  computationThreadMint: "computation_thread.mint.mint",
  fraudProofMint: "fraud_proof.mint.mint",
  fraudProofSpend: "fraud_proof.spend.else",
  fieldPreimageCertificateMint:
    "field_preimage_certificate.field_preimage_certificate.mint",
} as const;

export type FraudProofChain = {
  readonly firstStep: SpendingValidator;
  readonly steps: readonly [SpendingValidator, ...SpendingValidator[]];
};

export type DoubleSpendFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly doubleSpend: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type NonExistentInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly nonExistentInput: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

/**
 * Q18 `no-reference-input`: a committed transaction references an input that
 * never existed in the block's prev ledger and was not produced in-block. The
 * chain mirrors `no_input`'s applied-parameter order step for step; only the
 * field lifted out of the bad transaction differs (reference inputs, not spend
 * inputs).
 */
export type NoReferenceInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly noReferenceInput: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

/**
 * Q13 `input-no-idx` (`nonExistentInputNoIndex`): a committed transaction
 * spends an output index its in-block producing transaction never created.
 */
export type InputNoIdxFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly nonExistentInputNoIndex: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

/**
 * Q31 `reference-input-no-idx`: a committed transaction *reads* an output index
 * its in-block producing transaction never created. The reference-input mirror
 * of `input-no-idx`: steps 01 and 02 are distinct scripts, while steps 03 and 04
 * compile to the same UPLC as that chain's and are therefore shared, as with the
 * `no_input`/`no_reference_input` pair.
 */
export type ReferenceInputNoIdxFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly referenceInputNoIdx: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type InvalidRangeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly invalidRange: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type InvalidSignatureFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly invalidSignature: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type ZeroInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly zeroInput: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type DaHashPreimageFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly daHashPreimage: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

/**
 * Q39 `fabricated-deposit`: a committed `deposits_root` leaf that is not the
 * authentic L1 deposit event pair.
 */
export type FabricatedDepositFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fabricatedDeposit: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

/**
 * Q40 `fabricated-withdrawal`: a committed `withdrawals_root` leaf that is not
 * the authentic L1 withdrawal order pair.
 */
export type FabricatedWithdrawalFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fabricatedWithdrawal: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type NativeScriptDecodingFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly nativeScriptDecoding: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type MissingSignatureFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly missingSignature: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type MissingNativeScriptTxFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly missingNativeScriptTx: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type WithdrawnReferenceInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly withdrawnReferenceInput: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type CanonicalDecodabilityFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly canonicalDecodability: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type CommittedFieldShapeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly committedFieldShape: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type MinFeeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly minFee: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type WithdrawalMistagFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly withdrawalMistag: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type DoubleWithdrawFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly doubleWithdraw: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type CrossBlockDuplicateEventFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly crossBlockDuplicateEvent: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type L2TxMistagFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly l2TxMistag: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type WithdrawnInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly withdrawnInput: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type ValueNotPreservedFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly valueNotPreserved: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type InputSetUniquenessFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly inputSetUniqueness: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type MintAuthorizationFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly mintAuthorization: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type TransitionTraceFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly transitionTrace: FraudProofChain & {
    readonly route: SpendingValidator;
    readonly finals: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
    readonly steps: readonly [
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

export type FaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly doubleSpend: DoubleSpendFaultProofContracts["doubleSpend"];
  readonly nonExistentInput: NonExistentInputFaultProofContracts["nonExistentInput"];
  readonly noReferenceInput: NoReferenceInputFaultProofContracts["noReferenceInput"];
  readonly invalidRange: InvalidRangeFaultProofContracts["invalidRange"];
  readonly invalidSignature: InvalidSignatureFaultProofContracts["invalidSignature"];
  readonly zeroInput: ZeroInputFaultProofContracts["zeroInput"];
  readonly transitionTrace: TransitionTraceFaultProofContracts["transitionTrace"];
  readonly validationTraceDispute: ValidationTraceDisputeFaultProofContracts["validationTraceDispute"];
  readonly daHashPreimage: DaHashPreimageFaultProofContracts["daHashPreimage"];
  readonly nonExistentInputNoIndex: InputNoIdxFaultProofContracts["nonExistentInputNoIndex"];
  readonly referenceInputNoIdx: ReferenceInputNoIdxFaultProofContracts["referenceInputNoIdx"];
  readonly fabricatedDeposit: FabricatedDepositFaultProofContracts["fabricatedDeposit"];
  readonly fabricatedWithdrawal: FabricatedWithdrawalFaultProofContracts["fabricatedWithdrawal"];
  readonly nativeScriptDecoding: NativeScriptDecodingFaultProofContracts["nativeScriptDecoding"];
  readonly missingSignature: MissingSignatureFaultProofContracts["missingSignature"];
  readonly missingNativeScriptTx: MissingNativeScriptTxFaultProofContracts["missingNativeScriptTx"];
  readonly withdrawnReferenceInput: WithdrawnReferenceInputFaultProofContracts["withdrawnReferenceInput"];
  readonly canonicalDecodability: CanonicalDecodabilityFaultProofContracts["canonicalDecodability"];
  readonly committedFieldShape: CommittedFieldShapeFaultProofContracts["committedFieldShape"];
  readonly minFee: MinFeeFaultProofContracts["minFee"];
  readonly withdrawalMistag: WithdrawalMistagFaultProofContracts["withdrawalMistag"];
  readonly doubleWithdraw: DoubleWithdrawFaultProofContracts["doubleWithdraw"];
  readonly crossBlockDuplicateEvent: CrossBlockDuplicateEventFaultProofContracts["crossBlockDuplicateEvent"];
  readonly l2TxMistag: L2TxMistagFaultProofContracts["l2TxMistag"];
  readonly withdrawnInput: WithdrawnInputFaultProofContracts["withdrawnInput"];
  readonly valueNotPreserved: ValueNotPreservedFaultProofContracts["valueNotPreserved"];
  readonly inputSetUniqueness: InputSetUniquenessFaultProofContracts["inputSetUniqueness"];
  readonly mintAuthorization: MintAuthorizationFaultProofContracts["mintAuthorization"];
};

/**
 * Manifest-restorable category chains. Shared minting/spending policies are
 * deliberately excluded because the deployment ABI does not persist enough
 * information to reconstruct them safely.
 */
export type FaultProofContractChains = Omit<
  FaultProofContracts,
  "computationThread" | "fraudProof"
>;

type SharedFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  /**
   * The §8.6 field-preimage certificate minting policy id. #592 gave the
   * step validators that consult a carried field preimage a trailing
   * `field_preimage_certificate_policy_id` parameter; the certificate
   * validator itself takes no parameters, so this id is a pure function of
   * the blueprint. It is derived here rather than accepted from callers
   * precisely because it cannot vary independently of the blueprint the
   * other contracts are built from — a caller-supplied value could only ever
   * agree or be wrong.
   */
  readonly fieldPreimageCertificatePolicyId: string;
};

export type BuildFaultProofContractsParams = {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofCataloguePolicyId: string;
};

export type BuildDoubleSpendFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildNonExistentInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildNoReferenceInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildInputNoIdxFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildReferenceInputNoIdxFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildInvalidRangeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildInvalidSignatureFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildZeroInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildDaHashPreimageFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildFabricatedDepositFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildFabricatedWithdrawalFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildNativeScriptDecodingFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildMissingSignatureFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildMissingNativeScriptTxFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildWithdrawnReferenceInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildCanonicalDecodabilityFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildCommittedFieldShapeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildMinFeeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildWithdrawalMistagFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildDoubleWithdrawFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildCrossBlockDuplicateEventFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildL2TxMistagFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildWithdrawnInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildValueNotPreservedFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildInputSetUniquenessFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildMintAuthorizationFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildTransitionTraceFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildValidationTraceDisputeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const deriveValidationTraceDeploymentIdV1 = (
  fraudProofCataloguePolicyId: string,
): string => {
  if (!/^[0-9a-fA-F]{56}$/u.test(fraudProofCataloguePolicyId)) {
    throw new Error(
      "Fraud-proof catalogue policy id must be exactly 28 bytes of hexadecimal",
    );
  }
  return toHex(blake2b(fromHex(fraudProofCataloguePolicyId), { dkLen: 32 }));
};

export const parseFaultProofBlueprint = (
  value: unknown,
): FaultProofBlueprint => {
  if (typeof value !== "object" || value === null) {
    throw new Error("Fault proof blueprint must be a JSON object");
  }

  const validators = (value as { readonly validators?: unknown }).validators;
  if (!Array.isArray(validators)) {
    throw new Error("Fault proof blueprint must contain validators[]");
  }

  return {
    validators: validators.map((validator, index) => {
      if (typeof validator !== "object" || validator === null) {
        throw new Error(`validators[${index}] must be an object`);
      }
      const candidate = validator as {
        readonly title?: unknown;
        readonly compiledCode?: unknown;
        readonly parameters?: unknown;
      };
      if (typeof candidate.title !== "string") {
        throw new Error(`validators[${index}].title must be a string`);
      }
      if (typeof candidate.compiledCode !== "string") {
        throw new Error(`validators[${index}].compiledCode must be a string`);
      }
      // A validator that takes no parameters omits the key entirely, so absent
      // means zero declared — never "unknown, skip the check".
      const rawParameters = candidate.parameters ?? [];
      if (!Array.isArray(rawParameters)) {
        throw new Error(
          `validators[${index}].parameters must be an array when present`,
        );
      }
      return {
        title: candidate.title,
        compiledCode: candidate.compiledCode,
        parameters: rawParameters.map((parameter, parameterIndex) => {
          const parameterTitle = (parameter as { readonly title?: unknown })
            .title;
          if (typeof parameterTitle !== "string") {
            throw new Error(
              `validators[${index}].parameters[${parameterIndex}].title must be a string`,
            );
          }
          return { title: parameterTitle };
        }),
      };
    }),
  };
};

const getBlueprintValidator = (
  blueprint: FaultProofBlueprint,
  title: string,
): FaultProofBlueprintValidator => {
  const found = blueprint.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`Validator with title "${title}" not found in blueprint`);
  }
  return found;
};

/**
 * The parameters a blueprint entry declares.
 *
 * A validator that takes none omits the key entirely — that is the compiler's
 * format, so ABSENT MEANS ZERO, never "unknown, skip the check". Read through
 * this accessor rather than the field so a caller handing us a raw `plutus.json`
 * object (where the key is simply missing on nullary validators) is checked by
 * the same rule as one that went through {@link parseFaultProofBlueprint}.
 */
const declaredParameters = (
  validator: FaultProofBlueprintValidator,
): readonly FaultProofBlueprintParameter[] => validator.parameters ?? [];

const describeDeclaredParameters = (
  validator: FaultProofBlueprintValidator,
): string =>
  declaredParameters(validator).length === 0
    ? "none"
    : declaredParameters(validator)
        .map((parameter) => parameter.title)
        .join(", ");

/**
 * The single place this package turns a blueprint entry into a deployable
 * script, and the only permitted caller of `applyParamsToScript` here.
 *
 * `applyParamsToScript` applies whatever list it is handed and never checks it
 * against the script's own declared arity. Applying too FEW terms is silent and
 * catastrophic: the remaining `validator main(...)` parameters stay as lambdas,
 * so the ledger's single Plutus V3 script-context application reduces to a
 * lambda VALUE instead of running the validator body. Evaluation terminates
 * without error, and the ledger reads "no error" as SUCCESS — the deployment is
 * an unconditional always-succeeds script whose Aiken guards never execute.
 * That is exactly how ten validation-trace semantic resolvers shipped after
 * #592 added their `field_preimage_certificate_policy_id` parameter (#605/#609).
 * Applying too MANY is a well-formed script with a wrong hash, which surfaces
 * days later as a credential that matches nothing on chain.
 *
 * Refusing both directions here converts that whole class into a build-time
 * failure at the load site, for every validator this package deploys.
 */
const applyBlueprintParams = (
  blueprint: FaultProofBlueprint,
  title: string,
  params: readonly Data[],
): string => {
  const validator = getBlueprintValidator(blueprint, title);
  if (declaredParameters(validator).length !== params.length) {
    throw new Error(
      `Blueprint validator "${title}" declares ` +
        `${declaredParameters(validator).length.toString()} parameter(s) ` +
        `(${describeDeclaredParameters(validator)}) but ` +
        `${params.length.toString()} were applied. Under-application deploys an ` +
        "always-succeeds script and over-application deploys a wrong hash; " +
        "apply exactly the declared parameters (#609).",
    );
  }
  const cacheKey = appliedScriptCacheKey(validator.compiledCode, params);
  const cached = appliedScriptCache.get(cacheKey);
  if (cached !== undefined) {
    return cached;
  }
  const applied = applyParamsToScript(validator.compiledCode, [...params]);
  appliedScriptCache.set(cacheKey, applied);
  return applied;
};

/**
 * `applyParamsToScript` is pure — the applied script is a function of nothing
 * but the compiled code and the CBOR of the parameters — and it dominates
 * contract construction (3–65 ms per validator, ~14 s across a full
 * fault-proof contract build). Memoizing on the exact inputs therefore cannot
 * change any deployed byte: a cache hit is a proof the inputs were identical.
 * The #609 arity guard above runs before the lookup on every call, cached or
 * not, so under-/over-application still fails closed.
 */
const appliedScriptCache = new Map<string, string>();

const appliedScriptCacheKey = (
  compiledCode: string,
  params: readonly Data[],
): string =>
  toHex(
    blake2b(
      new TextEncoder().encode(
        `${compiledCode}|${params.map((param) => Data.to(param)).join("|")}`,
      ),
      { dkLen: 32 },
    ),
  );

/**
 * The same fail-closed reading for validators deployed with no parameters at
 * all: a title that silently grows a parameter must not keep being deployed
 * bare, which is under-application by the whole parameter list.
 */
const getUnappliedScript = (
  blueprint: FaultProofBlueprint,
  title: string,
): string => {
  const validator = getBlueprintValidator(blueprint, title);
  if (declaredParameters(validator).length !== 0) {
    throw new Error(
      `Blueprint validator "${title}" declares ` +
        `${declaredParameters(validator).length.toString()} parameter(s) ` +
        `(${describeDeclaredParameters(validator)}) but is deployed with none ` +
        "applied, which is an always-succeeds script (#609).",
    );
  }
  return validator.compiledCode;
};

const makeMintingPolicy = (mintingScriptCBOR: string): MintingValidator => {
  const mintingScript: MintingPolicy = {
    type: "PlutusV3",
    script: mintingScriptCBOR,
  };
  return {
    mintingScriptCBOR,
    mintingScript,
    policyId: mintingPolicyToId(mintingScript),
  };
};

const makeSpendingValidator = (
  network: Network,
  spendingScriptCBOR: string,
): SpendingValidator => {
  const spendingScript: LucidSpendingValidator = {
    type: "PlutusV3",
    script: spendingScriptCBOR,
  };
  return {
    spendingScriptCBOR,
    spendingScript,
    spendingScriptAddress: validatorToAddress(network, spendingScript),
    spendingScriptHash: validatorToScriptHash(spendingScript),
  };
};

const makeAuthenticatedValidator = (
  network: Network,
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): AuthenticatedValidator => ({
  ...makeSpendingValidator(network, spendingScriptCBOR),
  ...makeMintingPolicy(mintingScriptCBOR),
});

const asAddressDataParam = (address: Address): Effect.Effect<Data, Error> =>
  addressDataFromBech32(address).pipe(
    Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    Effect.mapError(
      (cause) =>
        new Error(
          `Failed to encode fraud proof token address parameter: ${cause.message}`,
        ),
    ),
  );

const tryBuild = <A>(
  description: string,
  build: () => A,
): Effect.Effect<A, Error> =>
  Effect.try({
    try: build,
    catch: (cause) =>
      new Error(
        `${description}: ${cause instanceof Error ? cause.message : String(cause)}`,
      ),
  });

const buildSharedFaultProofContracts = ({
  blueprint,
  network,
  hubOraclePolicyId,
  fraudProofCataloguePolicyId,
}: BuildFaultProofContractsParams): Effect.Effect<
  SharedFaultProofContracts,
  Error
> =>
  Effect.gen(function* () {
    const computationThread = yield* tryBuild(
      "Failed to build computation-thread minting policy",
      () =>
        makeMintingPolicy(
          applyBlueprintParams(
            blueprint,
            FAULT_PROOF_SHARED_TITLES.computationThreadMint,
            [fraudProofCataloguePolicyId, hubOraclePolicyId],
          ),
        ),
    );

    const fraudProof = yield* tryBuild(
      "Failed to build fraud-proof token validator",
      () =>
        makeAuthenticatedValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FAULT_PROOF_SHARED_TITLES.fraudProofMint,
            [computationThread.policyId],
          ),
          getUnappliedScript(
            blueprint,
            FAULT_PROOF_SHARED_TITLES.fraudProofSpend,
          ),
        ),
    );

    const fraudProofTokenAddressData = yield* asAddressDataParam(
      fraudProof.spendingScriptAddress,
    );

    const fieldPreimageCertificate = yield* tryBuild(
      "Failed to build field-preimage certificate minting policy",
      () =>
        makeMintingPolicy(
          getUnappliedScript(
            blueprint,
            FAULT_PROOF_SHARED_TITLES.fieldPreimageCertificateMint,
          ),
        ),
    );

    return {
      computationThread,
      fraudProof,
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId: fieldPreimageCertificate.policyId,
    };
  });

const buildDoubleSpendChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
}): Effect.Effect<DoubleSpendFaultProofContracts["doubleSpend"], Error> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild("Failed to build double-spend step 04", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          DOUBLE_SPEND_FAULT_PROOF_TITLES.step04,
          [
            computationThread.policyId,
            fraudProof.policyId,
            fraudProofTokenAddressData,
            fieldPreimageCertificatePolicyId,
          ],
        ),
      ),
    );

    const step03 = yield* tryBuild("Failed to build double-spend step 03", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          DOUBLE_SPEND_FAULT_PROOF_TITLES.step03,
          [
            step04.spendingScriptHash,
            computationThread.policyId,
            fieldPreimageCertificatePolicyId,
          ],
        ),
      ),
    );

    const step02 = yield* tryBuild("Failed to build double-spend step 02", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          DOUBLE_SPEND_FAULT_PROOF_TITLES.step02,
          [
            step03.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ],
        ),
      ),
    );

    const step01 = yield* tryBuild("Failed to build double-spend step 01", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          DOUBLE_SPEND_FAULT_PROOF_TITLES.step01,
          [
            step02.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ],
        ),
      ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildNonExistentInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
}): Effect.Effect<
  NonExistentInputFaultProofContracts["nonExistentInput"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build non-existent-input step 04",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step04,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build non-existent-input step 03",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step03,
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build non-existent-input step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step02,
            [
              step03.spendingScriptHash,
              computationThread.policyId,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build non-existent-input step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step01,
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildNoReferenceInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
}): Effect.Effect<
  NoReferenceInputFaultProofContracts["noReferenceInput"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build no-reference-input step 04",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step04,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build no-reference-input step 03",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step03,
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build no-reference-input step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step02,
            [
              step03.spendingScriptHash,
              computationThread.policyId,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build no-reference-input step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step01,
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

/**
 * Applied-parameter order is taken from the compiled blueprint
 * (`fraud_proofs/input_no_idx/step_0{1..4}.main.spend`), which differs from the
 * `no_input` chain at steps 02/03: step 02 takes only the next-step hash and
 * the thread policy, step 03 re-enters the block binding and therefore also
 * takes the hub oracle, and step 04 takes the thread policy first.
 */
const buildInputNoIdxChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
}): Effect.Effect<
  InputNoIdxFaultProofContracts["nonExistentInputNoIndex"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild("Failed to build input-no-idx step 04", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          INPUT_NO_IDX_FAULT_PROOF_TITLES.step04,
          [
            computationThread.policyId,
            fraudProof.policyId,
            fraudProofTokenAddressData,
            fieldPreimageCertificatePolicyId,
          ],
        ),
      ),
    );

    const step03 = yield* tryBuild("Failed to build input-no-idx step 03", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          INPUT_NO_IDX_FAULT_PROOF_TITLES.step03,
          [
            step04.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ],
        ),
      ),
    );

    const step02 = yield* tryBuild("Failed to build input-no-idx step 02", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          INPUT_NO_IDX_FAULT_PROOF_TITLES.step02,
          [
            step03.spendingScriptHash,
            computationThread.policyId,
            fieldPreimageCertificatePolicyId,
          ],
        ),
      ),
    );

    const step01 = yield* tryBuild("Failed to build input-no-idx step 01", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          INPUT_NO_IDX_FAULT_PROOF_TITLES.step01,
          [
            step02.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ],
        ),
      ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

/**
 * Applied-parameter order is taken from the compiled blueprint
 * (`fraud_proofs/reference_input_no_idx/step_0{1..4}.main.spend`) and matches
 * `input-no-idx` position for position: step 02 takes the next-step hash and
 * the thread policy, step 03 re-enters the block binding and therefore also
 * takes the hub oracle, and step 04 takes the thread policy first. That order
 * must stay identical to `input-no-idx`'s: the two chains share their step-03
 * and step-04 scripts, so a divergent order here would fork those hashes.
 */
const buildReferenceInputNoIdxChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
}): Effect.Effect<
  ReferenceInputNoIdxFaultProofContracts["referenceInputNoIdx"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build reference-input-no-idx step 04",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step04,
            [
              computationThread.policyId,
              fraudProof.policyId,
              fraudProofTokenAddressData,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build reference-input-no-idx step 03",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step03,
            [
              step04.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build reference-input-no-idx step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step02,
            [
              step03.spendingScriptHash,
              computationThread.policyId,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build reference-input-no-idx step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step01,
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildInvalidRangeChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<InvalidRangeFaultProofContracts["invalidRange"], Error> =>
  Effect.gen(function* () {
    const invalidRangeStep02 = yield* tryBuild(
      "Failed to build invalid-range step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            INVALID_RANGE_FAULT_PROOF_TITLES.step02,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const invalidRangeStep01 = yield* tryBuild(
      "Failed to build invalid-range step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            INVALID_RANGE_FAULT_PROOF_TITLES.step01,
            [
              invalidRangeStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: invalidRangeStep01,
      steps: [invalidRangeStep01, invalidRangeStep02],
    };
  });

const buildInvalidSignatureChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
}): Effect.Effect<
  InvalidSignatureFaultProofContracts["invalidSignature"],
  Error
> =>
  Effect.gen(function* () {
    const invalidSignatureStep02 = yield* tryBuild(
      "Failed to build invalid-signature step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            INVALID_SIGNATURE_FAULT_PROOF_TITLES.step02,
            [
              computationThread.policyId,
              fraudProof.policyId,
              fraudProofTokenAddressData,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const invalidSignatureStep01 = yield* tryBuild(
      "Failed to build invalid-signature step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            INVALID_SIGNATURE_FAULT_PROOF_TITLES.step01,
            [
              invalidSignatureStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: invalidSignatureStep01,
      steps: [invalidSignatureStep01, invalidSignatureStep02],
    };
  });

const buildZeroInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
}): Effect.Effect<ZeroInputFaultProofContracts["zeroInput"], Error> =>
  Effect.gen(function* () {
    const zeroInputStep02 = yield* tryBuild(
      "Failed to build zero-input step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            ZERO_INPUT_FAULT_PROOF_TITLES.step02,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const zeroInputStep01 = yield* tryBuild(
      "Failed to build zero-input step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            ZERO_INPUT_FAULT_PROOF_TITLES.step01,
            [
              zeroInputStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: zeroInputStep01,
      steps: [zeroInputStep01, zeroInputStep02],
    };
  });

const buildDaHashPreimageChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<DaHashPreimageFaultProofContracts["daHashPreimage"], Error> =>
  Effect.gen(function* () {
    const daHashPreimageStep02 = yield* tryBuild(
      "Failed to build da-hash-preimage step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            DA_HASH_PREIMAGE_FAULT_PROOF_TITLES.step02,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const daHashPreimageStep01 = yield* tryBuild(
      "Failed to build da-hash-preimage step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            DA_HASH_PREIMAGE_FAULT_PROOF_TITLES.step01,
            [
              daHashPreimageStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: daHashPreimageStep01,
      steps: [daHashPreimageStep01, daHashPreimageStep02],
    };
  });

const buildFabricatedDepositChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  FabricatedDepositFaultProofContracts["fabricatedDeposit"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build fabricated-deposit step 04",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step04,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build fabricated-deposit step 03",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step03,
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build fabricated-deposit step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step02,
            [
              step03.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build fabricated-deposit step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step01,
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildFabricatedWithdrawalChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  FabricatedWithdrawalFaultProofContracts["fabricatedWithdrawal"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build fabricated-withdrawal step 04",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_WITHDRAWAL_FAULT_PROOF_TITLES.step04,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build fabricated-withdrawal step 03",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_WITHDRAWAL_FAULT_PROOF_TITLES.step03,
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build fabricated-withdrawal step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_WITHDRAWAL_FAULT_PROOF_TITLES.step02,
            [
              step03.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build fabricated-withdrawal step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_WITHDRAWAL_FAULT_PROOF_TITLES.step01,
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildFaultProofSpendingStep = (
  context: Pick<BuildFaultProofContractsParams, "blueprint" | "network">,
  title: string,
  params: readonly Data[],
  description: string,
): Effect.Effect<SpendingValidator, Error> =>
  tryBuild(description, () =>
    makeSpendingValidator(
      context.network,
      applyBlueprintParams(context.blueprint, title, params),
    ),
  );

const buildNativeScriptDecodingChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  NativeScriptDecodingFaultProofContracts["nativeScriptDecoding"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step04,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build native-script-decoding step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build native-script-decoding step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build native-script-decoding step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build native-script-decoding step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03, step04] };
  });

const buildMissingSignatureChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MissingSignatureFaultProofContracts["missingSignature"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-signature step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-signature step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-signature step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SIGNATURE_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-signature step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03, step04] };
  });

const buildMissingNativeScriptTxChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MissingNativeScriptTxFaultProofContracts["missingNativeScriptTx"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step06 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step06,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-tx step 06",
    );
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step05,
      [step06.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-native-script-tx step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step04,
      [
        step05.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-tx step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-native-script-tx step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build missing-native-script-tx step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_NATIVE_SCRIPT_TX_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-native-script-tx step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05, step06],
    };
  });

const buildWithdrawnReferenceInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  WithdrawnReferenceInputFaultProofContracts["withdrawnReferenceInput"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWN_REFERENCE_INPUT_FAULT_PROOF_TITLES.step03,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build withdrawn-reference-input step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWN_REFERENCE_INPUT_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build withdrawn-reference-input step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWN_REFERENCE_INPUT_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build withdrawn-reference-input step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03] };
  });

const buildCanonicalDecodabilityChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  CanonicalDecodabilityFaultProofContracts["canonicalDecodability"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      CANONICAL_DECODABILITY_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build canonical-decodability step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      CANONICAL_DECODABILITY_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build canonical-decodability step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

const buildCommittedFieldShapeChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  CommittedFieldShapeFaultProofContracts["committedFieldShape"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      COMMITTED_FIELD_SHAPE_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build committed-field-shape step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      COMMITTED_FIELD_SHAPE_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build committed-field-shape step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

const buildMinFeeChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MinFeeFaultProofContracts["minFee"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MIN_FEE_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build min-fee step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MIN_FEE_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build min-fee step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

const buildWithdrawalMistagChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  WithdrawalMistagFaultProofContracts["withdrawalMistag"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES.step05,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build withdrawal-mistag step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build withdrawal-mistag step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build withdrawal-mistag step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build withdrawal-mistag step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWAL_MISTAG_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build withdrawal-mistag step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05],
    };
  });

const buildDoubleWithdrawChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  DoubleWithdrawFaultProofContracts["doubleWithdraw"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      DOUBLE_WITHDRAW_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build double-withdraw step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      DOUBLE_WITHDRAW_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build double-withdraw step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

const buildCrossBlockDuplicateEventChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  CrossBlockDuplicateEventFaultProofContracts["crossBlockDuplicateEvent"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      CROSS_BLOCK_DUPLICATE_EVENT_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build cross-block-duplicate-event step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      CROSS_BLOCK_DUPLICATE_EVENT_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build cross-block-duplicate-event step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

const buildL2TxMistagChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  L2TxMistagFaultProofContracts["l2TxMistag"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      L2_TX_MISTAG_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build l2-tx-mistag step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      L2_TX_MISTAG_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build l2-tx-mistag step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

const buildWithdrawnInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  WithdrawnInputFaultProofContracts["withdrawnInput"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWN_INPUT_FAULT_PROOF_TITLES.step03,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build withdrawn-input step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWN_INPUT_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build withdrawn-input step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWN_INPUT_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build withdrawn-input step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03] };
  });

const buildValueNotPreservedChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  ValueNotPreservedFaultProofContracts["valueNotPreserved"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build value-not-preserved step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build value-not-preserved step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build value-not-preserved step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build value-not-preserved step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03, step04] };
  });

const buildInputSetUniquenessChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  InputSetUniquenessFaultProofContracts["inputSetUniqueness"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      INPUT_SET_UNIQUENESS_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build input-set-uniqueness step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      INPUT_SET_UNIQUENESS_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build input-set-uniqueness step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

const buildMintAuthorizationChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MintAuthorizationFaultProofContracts["mintAuthorization"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step05,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build mint-authorization step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step04,
      [
        step05.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build mint-authorization step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        step05.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build mint-authorization step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build mint-authorization step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build mint-authorization step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05],
    };
  });

const buildTransitionTraceChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  TransitionTraceFaultProofContracts["transitionTrace"],
  Error
> =>
  Effect.gen(function* () {
    const finalSpecs = [
      ["control", false],
      ["source", false],
      ["withdrawal", false],
      ["forced", false],
      ["accepted", false],
      ["deposit", true],
      ["l1Event", true],
      ["duplicate", false],
    ] as const;
    const builtFinals: SpendingValidator[] = [];
    for (const [name, needsHub] of finalSpecs) {
      builtFinals.push(
        yield* tryBuild(
          `Failed to build transition-trace ${name} final validator`,
          () =>
            makeSpendingValidator(
              network,
              applyBlueprintParams(
                blueprint,
                TRANSITION_TRACE_FAULT_PROOF_TITLES[name],
                [
                  computationThread.policyId,
                  fraudProof.policyId,
                  fraudProofTokenAddressData,
                  ...(needsHub ? [hubOraclePolicyId] : []),
                ],
              ),
            ),
        ),
      );
    }
    const finals = [
      builtFinals[0]!,
      builtFinals[1]!,
      builtFinals[2]!,
      builtFinals[3]!,
      builtFinals[4]!,
      builtFinals[5]!,
      builtFinals[6]!,
      builtFinals[7]!,
    ] as const;
    if (
      new Set(finals.map(({ spendingScriptHash }) => spendingScriptHash))
        .size !== finals.length
    ) {
      return yield* Effect.fail(
        new Error("Transition-trace final validator hashes must be distinct"),
      );
    }
    const finalHashesSchema = Data.Array(Data.Bytes());
    type FinalHashes = Data.Static<typeof finalHashesSchema>;
    const FinalHashes = finalHashesSchema as unknown as FinalHashes;
    const finalHashesData = Data.from(
      Data.to(
        finals.map(({ spendingScriptHash }) => spendingScriptHash),
        FinalHashes,
      ),
    );
    const route = yield* tryBuild(
      "Failed to build transition-trace route validator",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            TRANSITION_TRACE_FAULT_PROOF_TITLES.route,
            [finalHashesData, computationThread.policyId],
          ),
        ),
    );

    return {
      firstStep: route,
      route,
      finals,
      steps: [route, ...finals],
    };
  });

const buildValidationTraceDisputeChain = ({
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
          getUnappliedScript(blueprint, CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1),
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

    const deploymentId = deriveValidationTraceDeploymentIdV1(
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
        .size !== VALIDATION_TRACE_RESOLVER_COUNT_V1
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

export const buildFaultProofContracts = (
  params: BuildFaultProofContractsParams,
): Effect.Effect<FaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const doubleSpend = yield* buildDoubleSpendChain({
      ...params,
      ...shared,
    });
    const nonExistentInput = yield* buildNonExistentInputChain({
      ...params,
      ...shared,
    });
    const noReferenceInput = yield* buildNoReferenceInputChain({
      ...params,
      ...shared,
    });
    const invalidRange = yield* buildInvalidRangeChain({
      ...params,
      ...shared,
    });
    const invalidSignature = yield* buildInvalidSignatureChain({
      ...params,
      ...shared,
    });
    const zeroInput = yield* buildZeroInputChain({
      ...params,
      ...shared,
    });
    const transitionTrace = yield* buildTransitionTraceChain({
      ...params,
      ...shared,
    });
    const validationTraceDispute = yield* buildValidationTraceDisputeChain({
      ...params,
      ...shared,
    });
    const daHashPreimage = yield* buildDaHashPreimageChain({
      ...params,
      ...shared,
    });
    const nonExistentInputNoIndex = yield* buildInputNoIdxChain({
      ...params,
      ...shared,
    });
    const referenceInputNoIdx = yield* buildReferenceInputNoIdxChain({
      ...params,
      ...shared,
    });
    const fabricatedDeposit = yield* buildFabricatedDepositChain({
      ...params,
      ...shared,
    });
    const fabricatedWithdrawal = yield* buildFabricatedWithdrawalChain({
      ...params,
      ...shared,
    });
    const nativeScriptDecoding = yield* buildNativeScriptDecodingChain({
      ...params,
      ...shared,
    });
    const missingSignature = yield* buildMissingSignatureChain({
      ...params,
      ...shared,
    });
    const missingNativeScriptTx = yield* buildMissingNativeScriptTxChain({
      ...params,
      ...shared,
    });
    const withdrawnReferenceInput = yield* buildWithdrawnReferenceInputChain({
      ...params,
      ...shared,
    });
    const canonicalDecodability = yield* buildCanonicalDecodabilityChain({
      ...params,
      ...shared,
    });
    const committedFieldShape = yield* buildCommittedFieldShapeChain({
      ...params,
      ...shared,
    });
    const minFee = yield* buildMinFeeChain({ ...params, ...shared });
    const withdrawalMistag = yield* buildWithdrawalMistagChain({
      ...params,
      ...shared,
    });
    const doubleWithdraw = yield* buildDoubleWithdrawChain({
      ...params,
      ...shared,
    });
    const crossBlockDuplicateEvent = yield* buildCrossBlockDuplicateEventChain({
      ...params,
      ...shared,
    });
    const l2TxMistag = yield* buildL2TxMistagChain({ ...params, ...shared });
    const withdrawnInput = yield* buildWithdrawnInputChain({
      ...params,
      ...shared,
    });
    const valueNotPreserved = yield* buildValueNotPreservedChain({
      ...params,
      ...shared,
    });
    const inputSetUniqueness = yield* buildInputSetUniquenessChain({
      ...params,
      ...shared,
    });
    const mintAuthorization = yield* buildMintAuthorizationChain({
      ...params,
      ...shared,
    });

    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      doubleSpend,
      nonExistentInput,
      noReferenceInput,
      invalidRange,
      invalidSignature,
      zeroInput,
      transitionTrace,
      validationTraceDispute,
      daHashPreimage,
      nonExistentInputNoIndex,
      referenceInputNoIdx,
      fabricatedDeposit,
      fabricatedWithdrawal,
      nativeScriptDecoding,
      missingSignature,
      missingNativeScriptTx,
      withdrawnReferenceInput,
      canonicalDecodability,
      committedFieldShape,
      minFee,
      withdrawalMistag,
      doubleWithdraw,
      crossBlockDuplicateEvent,
      l2TxMistag,
      withdrawnInput,
      valueNotPreserved,
      inputSetUniqueness,
      mintAuthorization,
    };
  });

/** Project the full deployment registry to the first-step catalogue leaves. */
export const fraudProofContractsToFirstSteps = (
  contracts: FaultProofContractChains,
): FraudProofs => ({
  doubleSpend: contracts.doubleSpend.firstStep,
  nonExistentInput: contracts.nonExistentInput.firstStep,
  nonExistentInputNoIndex: contracts.nonExistentInputNoIndex.firstStep,
  invalidRange: contracts.invalidRange.firstStep,
  transitionTrace: contracts.transitionTrace.firstStep,
  zeroInput: contracts.zeroInput.firstStep,
  validationTraceDispute: {
    ...contracts.validationTraceDispute.firstStep,
    source: contracts.validationTraceDispute.source,
    game: contracts.validationTraceDispute.game,
    boundary: contracts.validationTraceDispute.boundary,
    timeout: contracts.validationTraceDispute.timeout,
    award: contracts.validationTraceDispute.award,
  },
  daHashPreimage: contracts.daHashPreimage.firstStep,
  noReferenceInput: contracts.noReferenceInput.firstStep,
  referenceInputNoIdx: contracts.referenceInputNoIdx.firstStep,
  invalidSignature: contracts.invalidSignature.firstStep,
  fabricatedDeposit: contracts.fabricatedDeposit.firstStep,
  fabricatedWithdrawal: contracts.fabricatedWithdrawal.firstStep,
  nativeScriptDecoding: contracts.nativeScriptDecoding.firstStep,
  missingSignature: contracts.missingSignature.firstStep,
  missingNativeScriptTx: contracts.missingNativeScriptTx.firstStep,
  withdrawnReferenceInput: contracts.withdrawnReferenceInput.firstStep,
  canonicalDecodability: contracts.canonicalDecodability.firstStep,
  committedFieldShape: contracts.committedFieldShape.firstStep,
  minFee: contracts.minFee.firstStep,
  withdrawalMistag: contracts.withdrawalMistag.firstStep,
  doubleWithdraw: contracts.doubleWithdraw.firstStep,
  crossBlockDuplicateEvent: contracts.crossBlockDuplicateEvent.firstStep,
  l2TxMistag: contracts.l2TxMistag.firstStep,
  withdrawnInput: contracts.withdrawnInput.firstStep,
  valueNotPreserved: contracts.valueNotPreserved.firstStep,
  inputSetUniqueness: contracts.inputSetUniqueness.firstStep,
  mintAuthorization: contracts.mintAuthorization.firstStep,
});

export const buildDoubleSpendFaultProofContracts = (
  params: BuildDoubleSpendFaultProofContractsParams,
): Effect.Effect<DoubleSpendFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const doubleSpend = yield* buildDoubleSpendChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      doubleSpend,
    };
  });

export const buildNonExistentInputFaultProofContracts = (
  params: BuildNonExistentInputFaultProofContractsParams,
): Effect.Effect<NonExistentInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const nonExistentInput = yield* buildNonExistentInputChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      nonExistentInput,
    };
  });

export const buildNoReferenceInputFaultProofContracts = (
  params: BuildNoReferenceInputFaultProofContractsParams,
): Effect.Effect<NoReferenceInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const noReferenceInput = yield* buildNoReferenceInputChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      noReferenceInput,
    };
  });

export const buildInputNoIdxFaultProofContracts = (
  params: BuildInputNoIdxFaultProofContractsParams,
): Effect.Effect<InputNoIdxFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const nonExistentInputNoIndex = yield* buildInputNoIdxChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      nonExistentInputNoIndex,
    };
  });

export const buildReferenceInputNoIdxFaultProofContracts = (
  params: BuildReferenceInputNoIdxFaultProofContractsParams,
): Effect.Effect<ReferenceInputNoIdxFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const referenceInputNoIdx = yield* buildReferenceInputNoIdxChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      referenceInputNoIdx,
    };
  });

export const buildInvalidRangeFaultProofContracts = (
  params: BuildInvalidRangeFaultProofContractsParams,
): Effect.Effect<InvalidRangeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const invalidRange = yield* buildInvalidRangeChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      invalidRange,
    };
  });

export const buildInvalidSignatureFaultProofContracts = (
  params: BuildInvalidSignatureFaultProofContractsParams,
): Effect.Effect<InvalidSignatureFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const invalidSignature = yield* buildInvalidSignatureChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      invalidSignature,
    };
  });

export const buildZeroInputFaultProofContracts = (
  params: BuildZeroInputFaultProofContractsParams,
): Effect.Effect<ZeroInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const zeroInput = yield* buildZeroInputChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      zeroInput,
    };
  });

export const buildDaHashPreimageFaultProofContracts = (
  params: BuildDaHashPreimageFaultProofContractsParams,
): Effect.Effect<DaHashPreimageFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const daHashPreimage = yield* buildDaHashPreimageChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      daHashPreimage,
    };
  });

export const buildFabricatedDepositFaultProofContracts = (
  params: BuildFabricatedDepositFaultProofContractsParams,
): Effect.Effect<FabricatedDepositFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const fabricatedDeposit = yield* buildFabricatedDepositChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      fabricatedDeposit,
    };
  });

export const buildFabricatedWithdrawalFaultProofContracts = (
  params: BuildFabricatedWithdrawalFaultProofContractsParams,
): Effect.Effect<FabricatedWithdrawalFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const fabricatedWithdrawal = yield* buildFabricatedWithdrawalChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      fabricatedWithdrawal,
    };
  });

export const buildNativeScriptDecodingFaultProofContracts = (
  params: BuildNativeScriptDecodingFaultProofContractsParams,
): Effect.Effect<NativeScriptDecodingFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const nativeScriptDecoding = yield* buildNativeScriptDecodingChain({
      ...params,
      ...shared,
    });
    return { ...shared, nativeScriptDecoding };
  });

export const buildMissingSignatureFaultProofContracts = (
  params: BuildMissingSignatureFaultProofContractsParams,
): Effect.Effect<MissingSignatureFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const missingSignature = yield* buildMissingSignatureChain({
      ...params,
      ...shared,
    });
    return { ...shared, missingSignature };
  });

export const buildMissingNativeScriptTxFaultProofContracts = (
  params: BuildMissingNativeScriptTxFaultProofContractsParams,
): Effect.Effect<MissingNativeScriptTxFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const missingNativeScriptTx = yield* buildMissingNativeScriptTxChain({
      ...params,
      ...shared,
    });
    return { ...shared, missingNativeScriptTx };
  });

export const buildWithdrawnReferenceInputFaultProofContracts = (
  params: BuildWithdrawnReferenceInputFaultProofContractsParams,
): Effect.Effect<WithdrawnReferenceInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const withdrawnReferenceInput = yield* buildWithdrawnReferenceInputChain({
      ...params,
      ...shared,
    });
    return { ...shared, withdrawnReferenceInput };
  });

export const buildCanonicalDecodabilityFaultProofContracts = (
  params: BuildCanonicalDecodabilityFaultProofContractsParams,
): Effect.Effect<CanonicalDecodabilityFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const canonicalDecodability = yield* buildCanonicalDecodabilityChain({
      ...params,
      ...shared,
    });
    return { ...shared, canonicalDecodability };
  });

export const buildCommittedFieldShapeFaultProofContracts = (
  params: BuildCommittedFieldShapeFaultProofContractsParams,
): Effect.Effect<CommittedFieldShapeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const committedFieldShape = yield* buildCommittedFieldShapeChain({
      ...params,
      ...shared,
    });
    return { ...shared, committedFieldShape };
  });

export const buildMinFeeFaultProofContracts = (
  params: BuildMinFeeFaultProofContractsParams,
): Effect.Effect<MinFeeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const minFee = yield* buildMinFeeChain({ ...params, ...shared });
    return { ...shared, minFee };
  });

export const buildWithdrawalMistagFaultProofContracts = (
  params: BuildWithdrawalMistagFaultProofContractsParams,
): Effect.Effect<WithdrawalMistagFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const withdrawalMistag = yield* buildWithdrawalMistagChain({
      ...params,
      ...shared,
    });
    return { ...shared, withdrawalMistag };
  });

export const buildDoubleWithdrawFaultProofContracts = (
  params: BuildDoubleWithdrawFaultProofContractsParams,
): Effect.Effect<DoubleWithdrawFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const doubleWithdraw = yield* buildDoubleWithdrawChain({
      ...params,
      ...shared,
    });
    return { ...shared, doubleWithdraw };
  });

export const buildCrossBlockDuplicateEventFaultProofContracts = (
  params: BuildCrossBlockDuplicateEventFaultProofContractsParams,
): Effect.Effect<CrossBlockDuplicateEventFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const crossBlockDuplicateEvent = yield* buildCrossBlockDuplicateEventChain({
      ...params,
      ...shared,
    });
    return { ...shared, crossBlockDuplicateEvent };
  });

export const buildL2TxMistagFaultProofContracts = (
  params: BuildL2TxMistagFaultProofContractsParams,
): Effect.Effect<L2TxMistagFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const l2TxMistag = yield* buildL2TxMistagChain({ ...params, ...shared });
    return { ...shared, l2TxMistag };
  });

export const buildWithdrawnInputFaultProofContracts = (
  params: BuildWithdrawnInputFaultProofContractsParams,
): Effect.Effect<WithdrawnInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const withdrawnInput = yield* buildWithdrawnInputChain({
      ...params,
      ...shared,
    });
    return { ...shared, withdrawnInput };
  });

export const buildValueNotPreservedFaultProofContracts = (
  params: BuildValueNotPreservedFaultProofContractsParams,
): Effect.Effect<ValueNotPreservedFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const valueNotPreserved = yield* buildValueNotPreservedChain({
      ...params,
      ...shared,
    });
    return { ...shared, valueNotPreserved };
  });

export const buildInputSetUniquenessFaultProofContracts = (
  params: BuildInputSetUniquenessFaultProofContractsParams,
): Effect.Effect<InputSetUniquenessFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const inputSetUniqueness = yield* buildInputSetUniquenessChain({
      ...params,
      ...shared,
    });
    return { ...shared, inputSetUniqueness };
  });

export const buildMintAuthorizationFaultProofContracts = (
  params: BuildMintAuthorizationFaultProofContractsParams,
): Effect.Effect<MintAuthorizationFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const mintAuthorization = yield* buildMintAuthorizationChain({
      ...params,
      ...shared,
    });
    return { ...shared, mintAuthorization };
  });

export const buildTransitionTraceFaultProofContracts = (
  params: BuildTransitionTraceFaultProofContractsParams,
): Effect.Effect<TransitionTraceFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const transitionTrace = yield* buildTransitionTraceChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      transitionTrace,
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
