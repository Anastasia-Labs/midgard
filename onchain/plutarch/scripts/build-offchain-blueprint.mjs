#!/usr/bin/env node

import { readFile, readdir, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const plutarchDir = path.resolve(scriptDir, "..");

const DIRECT_TITLES = new Map([
  [
    "fraud-proof-execution-native-script-invalid-accepted-inline-source.unapplied.plutus.json",
    "fraud_proofs/execution_native_script_invalid/accepted_inline_source.main.spend",
  ],
  [
    "fraud-proof-execution-native-script-invalid-accepted-reference-source.unapplied.plutus.json",
    "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main.spend",
  ],
  [
    "fraud-proof-execution-native-script-invalid-accepted-spend-prefix.unapplied.plutus.json",
    "fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.main.spend",
  ],
  [
    "fraud-proof-execution-native-script-invalid-accepted-mint-prefix.unapplied.plutus.json",
    "fraud_proofs/execution_native_script_invalid/accepted_mint_prefix.main.spend",
  ],
  [
    "fraud-proof-execution-native-script-invalid-accepted-observer-prefix.unapplied.plutus.json",
    "fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.main.spend",
  ],
  [
    "fraud-proof-execution-native-script-invalid-accepted-receive-prefix.unapplied.plutus.json",
    "fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.main.spend",
  ],
  [
    "fraud-proof-execution-native-script-invalid-accepted-reconstruction-init.unapplied.plutus.json",
    "fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.main.spend",
  ],
  [
    "fraud-proof-missing-redeemer-step-02a.unapplied.plutus.json",
    "fraud_proofs/missing_redeemer/step_02a.main.spend",
  ],
  [
    "fraud-proof-missing-redeemer-step-02b.unapplied.plutus.json",
    "fraud_proofs/missing_redeemer/step_02b.main.spend",
  ],
  [
    "fraud-proof-unused-redeemer-step-02a.unapplied.plutus.json",
    "fraud_proofs/unused_redeemer/step_02a.main.spend",
  ],
  [
    "fraud-proof-unused-redeemer-step-02b.unapplied.plutus.json",
    "fraud_proofs/unused_redeemer/step_02b.main.spend",
  ],
  [
    "fraud-proof-unused-redeemer-step-02c.unapplied.plutus.json",
    "fraud_proofs/unused_redeemer/step_02c.main.spend",
  ],
  [
    "fraud-proof-field-preimage-length-mismatch-step-02-accepted.unapplied.plutus.json",
    "fraud_proofs/field_preimage_length_mismatch/step_02_accepted.main.spend",
  ],
  [
    "fraud-proof-field-preimage-length-mismatch-step-02-forced.unapplied.plutus.json",
    "fraud_proofs/field_preimage_length_mismatch/step_02_forced.main.spend",
  ],
  [
    "fraud-proof-script-integrity-hash-missing-script-grammar.unapplied.plutus.json",
    "fraud_proofs/script_integrity_hash_missing/script_grammar.main.spend",
  ],
  [
    "fraud-proof-script-integrity-hash-missing-script-scan.unapplied.plutus.json",
    "fraud_proofs/script_integrity_hash_missing/script_scan.main.spend",
  ],
  [
    "fraud-proof-script-integrity-hash-missing-redeemer-grammar.unapplied.plutus.json",
    "fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main.spend",
  ],
  [
    "fraud-proof-validation-trace-value-and-mint-asset-fold-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/value_and_mint_asset_fold_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-seven-observer-item-yield-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_seven_observer_item_yield_v1.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-seven-observer-bound-yield-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_seven_observer_bound_yield_v1.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-redeemer-item-step-yield-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_redeemer_item_step_yield_v1.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-middle-stage-two-advance-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_two_advance.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-middle-stage-three-replay-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_replay.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-middle-stage-three-finish-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_finish.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-middle-stage-four-begin-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_begin.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-middle-stage-four-finish-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_finish.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-middle-stage-six-begin-policy-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_begin_policy.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-middle-stage-six-fold-asset-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_fold_asset.withdraw",
  ],
  [
    "fraud-proof-validation-trace-script-sources-middle-stage-six-finish-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_finish.withdraw",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-advance-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_advance_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-all-or-any-container-frame-payload-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-all-or-any-empty-container-payload-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-at-least-container-frame-payload-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_container_frame_payload_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-at-least-empty-container-payload-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_empty_container_payload_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-frame-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_frame_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-item-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-item-foreign-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.foreign.withdraw",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-item-native-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.native.withdraw",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-signature-above-last-payload-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_signature_above_last_payload_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-signature-below-first-payload-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_signature_below_first_payload_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_signature_between_payload_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-signature-empty-payload-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_signature_empty_payload_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-signature-membership-payload-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_signature_membership_payload_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-timelock-payload-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_timelock_payload_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-token-head-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_token_head_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-phase-a-native-scripts-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/phase_a_native_scripts_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-envelope-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_envelope_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-traversal-normalizer-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_traversal_normalizer_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-outer-normalizer-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_outer_normalizer_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-fold-map-executor-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_map_executor_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-finalize-frame-executor-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finalize_frame_executor_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-execution-settlement-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_execution_settlement_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-cek-envelope.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_envelope.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-cek-settlement.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_settlement.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-source-authenticator.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_source_authenticator.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-open-header-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_open_header_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-open-tail-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_open_tail_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-scalar-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_scalar_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-sequence-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_sequence_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-map-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_map_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-large-constructor-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_large_constructor_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-attach-integer-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_attach_integer_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-attach-bytes-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_attach_bytes_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-fold-list-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_list_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-integer-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_integer_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-bytes-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_bytes_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-large-constructor-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_large_constructor_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-large-fields-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_large_fields_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-close-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_close_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-finish-data-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finish_data_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-invalid-header-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_header_executor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-script-sources-stage-one-redeemer-invalid-tail-executor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_tail_executor.main.spend",
  ],

  [
    "fraud-proof-validation-trace-cek-context-step-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_step_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-control.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_control.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-settle.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_settle.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-reference.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_reference.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-spend.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_spend.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-output.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_output.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-signer.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_signer.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-mint-init.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_mint_init.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-mint-item.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_mint_item.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-assemble.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_assemble.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-tx-info.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_tx_info.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-seed.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_seed.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-redeemer-begin.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_redeemer_begin.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-redeemer-select-authenticate.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_redeemer_select_authenticate.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-redeemer-select-initialize.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_redeemer_select_initialize.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-redeemer-select-hash.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_redeemer_select_hash.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-redeemer-select-finish.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_redeemer_select_finish.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-finalize-authenticate.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_finalize_authenticate.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-finalize-spend.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_finalize_spend.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-finalize-mint.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_finalize_mint.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-finalize-withdraw.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_finalize_withdraw.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-finalize-observe.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_finalize_observe.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-finalize-midgard.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_finalize_midgard.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-observer-authenticate.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_observer_authenticate.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-observer-fold.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_observer_fold.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-item-bind.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_item_bind.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-item-return.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_item_return.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-item-hash.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_item_hash.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-item-finalize.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_item_finalize.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-item-selection-continue.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_item_selection_continue.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-item-selection-finish.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_item_selection_finish.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-item-data-continue.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_item_data_continue.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-item-data-finish-descriptor.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_item_data_finish_descriptor.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-context-item-data-finish-value.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_context_item_data_finish_value.main.spend",
  ],

  [
    "fraud-proof-validation-trace-cek-execution-selection-semantic-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_execution_selection_semantic_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-execution-selection-authenticate-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_execution_selection_yields.authenticate.withdraw",
  ],
  [
    "fraud-proof-validation-trace-cek-execution-selection-successor-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_execution_selection_yields.successor.withdraw",
  ],
  [
    "fraud-proof-validation-trace-cek-execution-selection-material-program-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_execution_selection_yields.material_program.withdraw",
  ],
  [
    "fraud-proof-validation-trace-cek-execution-selection-material-data-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_execution_selection_yields.material_data.withdraw",
  ],

  [
    "fraud-proof-validation-trace-cek-core-arm-compute.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_arm_compute.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-builtin-roots.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-semantic-result.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_semantic_result.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-builtin-budget.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-direct-scalar.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_direct_scalar.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-direct-structured.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_direct_structured.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-arm-machine.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_arm_machine.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-arm-map-conversion.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_arm_map_conversion.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-semantic-pair.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_semantic_pair.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-semantic-list-construct.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_semantic_list_construct.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-semantic-list-select.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_semantic_list_select.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-semantic-choose.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_semantic_choose.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-semantic-data-construct.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_semantic_data_construct.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-semantic-data-scalar.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_semantic_data_scalar.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-semantic-data-misc.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_semantic_data_misc.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-failure-known.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_failure_known.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-failure-budget.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_failure_budget.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-semantic-failure-roots.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_semantic_failure_roots.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-semantic-failure-material.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_semantic_failure_material.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-type-failure-roots.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_type_failure_roots.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-type-failure-kinds.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_type_failure_kinds.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-bls-budget.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_bls_budget.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-bls-roots.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_bls_roots.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-bls-final.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_bls_final.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-map-start-roots.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_map_start_roots.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-map-start-budget.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_map_start_budget.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-map-start-nodes.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_map_start_nodes.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-core-settle.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_core_settle.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-material-traversal-v1.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_material_traversal_v1.main.spend",
  ],
  [
    "fraud-proof-validation-trace-cek-material-traversal-program-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_material_traversal_yields.program.withdraw",
  ],
  [
    "fraud-proof-validation-trace-cek-material-traversal-data-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/cek_material_traversal_yields.data.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-advance-bytes-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_advance_bytes_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-advance-integer-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_advance_integer_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-attach-bytes-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_attach_bytes_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-attach-integer-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_attach_integer_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-close-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_close_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-finalize-frame-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_finalize_frame_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-finish-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_finish_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-fold-list-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_fold_list_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-fold-map-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_fold_map_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-head-large-constructor-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_head_large_constructor_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-head-map-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_head_map_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-head-scalar-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_head_scalar_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-head-sequence-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_head_sequence_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-large-constructor-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_large_constructor_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-datum-large-fields-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_datum_large_fields_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-native-script-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_native_script_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-reference-script-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_reference_script_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-scalar-bytes-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_scalar_bytes_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-scalar-integer-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_scalar_integer_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-script-hash-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_script_hash_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-span-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_span_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-structure-assets-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_structure_assets_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-structure-finish-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_structure_finish_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-structure-optional-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_structure_optional_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-structure-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_structure_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-proof-value-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_proof_value_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-descriptor-datum-summary-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_descriptor_datum_summary_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-descriptor-reference-script-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_descriptor_reference_script_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-descriptor-scan-facts-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_descriptor_scan_facts_yield.main.withdraw",
  ],
  [
    "fraud-proof-validation-trace-ledger-output-descriptor-value-summary-yield.unapplied.plutus.json",
    "fraud_proofs/validation_trace/ledger_output_descriptor_value_summary_yield.main.withdraw",
  ],

  [
    "fraud-proof-transition-trace-l2-open.unapplied.plutus.json",
    "fraud_proofs/transition_trace/accepted_transaction_yields.l2_open.withdraw",
  ],
  [
    "fraud-proof-transition-trace-l2-replay.unapplied.plutus.json",
    "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay.withdraw",
  ],
  [
    "fraud-proof-transition-trace-claim-structure.unapplied.plutus.json",
    "fraud_proofs/transition_trace/accepted_transaction_yields.claim_structure.withdraw",
  ],
  [
    "fraud-proof-transition-trace-claim-source.unapplied.plutus.json",
    "fraud_proofs/transition_trace/accepted_transaction_yields.claim_source.withdraw",
  ],
  [
    "fraud-proof-transition-trace-claim-endpoints.unapplied.plutus.json",
    "fraud_proofs/transition_trace/accepted_transaction_yields.claim_endpoints.withdraw",
  ],
  [
    "fraud-proof-transition-trace-output-scan.unapplied.plutus.json",
    "fraud_proofs/transition_trace/output_scan.scan_output.withdraw",
  ],
  [
    "fraud-proof-transition-trace-output-value.unapplied.plutus.json",
    "fraud_proofs/transition_trace/output_value.value_output.withdraw",
  ],
  [
    "fraud-proof-transition-trace-output-summaries.unapplied.plutus.json",
    "fraud_proofs/transition_trace/output_summaries.summaries.withdraw",
  ],
  [
    "fraud-proof-transition-trace-output-assembly.unapplied.plutus.json",
    "fraud_proofs/transition_trace/output_assembly.assembly.withdraw",
  ],
  [
    "fraud-proof-transition-trace-deposit-projection.unapplied.plutus.json",
    "fraud_proofs/transition_trace/deposit_yields.projection.withdraw",
  ],
  [
    "fraud-proof-transition-trace-deposit-value.unapplied.plutus.json",
    "fraud_proofs/transition_trace/deposit_value.value_output.withdraw",
  ],
  [
    "fraud-proof-transition-trace-deposit-summaries.unapplied.plutus.json",
    "fraud_proofs/transition_trace/deposit_summaries.summaries.withdraw",
  ],

  [
    "fraud-proof-value-not-preserved-union-accepted-source.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_accepted_source.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-forced-source.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_forced_source.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-event.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_event.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-pre-state.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_pre_state.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-inputs.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_inputs.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-input-value.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_input_value.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-assets.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_assets.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-field-grammar.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_field_grammar.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-outputs.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_outputs.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-output-scan.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_output_scan.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-mint.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_mint.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-update.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_update.main.spend",
  ],
  [
    "fraud-proof-value-not-preserved-union-terminal.unapplied.plutus.json",
    "fraud_proofs/value_not_preserved/union_terminal.main.spend",
  ],

  [
    "fraud-proof-mint-authorization-evaluate.unapplied.plutus.json",
    "fraud_proofs/mint_authorization/evaluate.main.spend",
  ],
  [
    "fraud-proof-mint-authorization-witness-scan.unapplied.plutus.json",
    "fraud_proofs/mint_authorization/witness_scan.main.spend",
  ],
  [
    "fraud-proof-min-ada-tx-yield.unapplied.plutus.json",
    "fraud_proofs/min_ada/step_02_yields.tx.withdraw",
  ],
  [
    "fraud-proof-min-ada-utxo-yield.unapplied.plutus.json",
    "fraud_proofs/min_ada/step_02_yields.utxo.withdraw",
  ],
  [
    "fraud-proof-network-id-forced-step.unapplied.plutus.json",
    "fraud_proofs/network_id/forced_step.main.spend",
  ],
  [
    "fraud-proof-network-id-forced-scan.unapplied.plutus.json",
    "fraud_proofs/network_id/forced_scan.main.spend",
  ],

  [
    "fraud-proof-missing-signature-forced-step.unapplied.plutus.json",
    "fraud_proofs/missing_signature/forced_step.main.spend",
  ],
  [
    "fraud-proof-missing-signature-forced-signer.unapplied.plutus.json",
    "fraud_proofs/missing_signature/forced_signer.main.spend",
  ],
  [
    "fraud-proof-missing-signature-forced-witness.unapplied.plutus.json",
    "fraud_proofs/missing_signature/forced_witness.main.spend",
  ],

  [
    "fraud-proof-mpf-chunked-proof-challenge.unapplied.plutus.json",
    "fraud_proofs/mpf_chunked_proof/challenge.main.spend",
  ],
  [
    "fraud-proof-native-script-decoding-step-03-advance-or-close.unapplied.plutus.json",
    "fraud_proofs/native_script_decoding/step_03_advance_or_close.main.spend",
  ],
  [
    "fraud-proof-native-script-decoding-step-03-bind-descriptor.unapplied.plutus.json",
    "fraud_proofs/native_script_decoding/step_03_bind_descriptor.main.spend",
  ],
  [
    "fraud-proof-native-script-decoding-step-03-open-subject.unapplied.plutus.json",
    "fraud_proofs/native_script_decoding/step_03_open_subject.main.spend",
  ],
  [
    "availability-challenge-mint.unapplied.plutus.json",
    "availability_challenge.availability_challenge.mint",
  ],
  [
    "availability-challenge-spend.unapplied.plutus.json",
    "availability_challenge.availability_challenge.spend",
  ],
  [
    "correction-lock-spend.unapplied.plutus.json",
    "correction_lock.spend.spend",
  ],
  [
    "state-queue-yield-commit.unapplied.plutus.json",
    "state_queue_yields.commit.withdraw",
  ],
  [
    "state-queue-yield-remove-unattested.unapplied.plutus.json",
    "state_queue_yields.remove_unattested.withdraw",
  ],
  [
    "state-queue-yield-remove-unavailable.unapplied.plutus.json",
    "state_queue_yields.remove_unavailable.withdraw",
  ],
  [
    "state-queue-yield-remove-fraudulent.unapplied.plutus.json",
    "state_queue_yields.remove_fraudulent.withdraw",
  ],
  [
    "state-queue-yield-merge.unapplied.plutus.json",
    "state_queue_yields.merge.withdraw",
  ],
  [
    "availability-challenge-yield-bond.unapplied.plutus.json",
    "availability_challenge_yields.bond.withdraw",
  ],
  [
    "availability-challenge-yield-open.unapplied.plutus.json",
    "availability_challenge_yields.open.withdraw",
  ],
  [
    "availability-challenge-yield-settle.unapplied.plutus.json",
    "availability_challenge_yields.settle.withdraw",
  ],
  [
    "availability-challenge-yield-close.unapplied.plutus.json",
    "availability_challenge_yields.close.withdraw",
  ],
  [
    "availability-challenge-yield-timeout.unapplied.plutus.json",
    "availability_challenge_yields.timeout.withdraw",
  ],
  [
    "active-operators-mint.unapplied.plutus.json",
    "operator_directory/active_operators.mint.mint",
  ],
  [
    "active-operators-spend.unapplied.plutus.json",
    "operator_directory/active_operators.spend.spend",
  ],
  [
    "cek-program-material-spend.plutus.json",
    "user_events/cek_program_material_v1.spend.spend",
  ],
  [
    "computation-thread-mint.unapplied.plutus.json",
    "computation_thread.mint.mint",
  ],
  [
    "da-attestation-mint.unapplied.plutus.json",
    "da_attestation.da_attestation.mint",
  ],
  [
    "da-attestation-spend.unapplied.plutus.json",
    "da_attestation.da_attestation.spend",
  ],
  [
    "da-params-governor-mint.unapplied.plutus.json",
    "da_params_governor.da_params_governor.mint",
  ],
  [
    "da-params-governor-spend.unapplied.plutus.json",
    "da_params_governor.da_params_governor.spend",
  ],
  ["deposit-mint.unapplied.plutus.json", "user_events/deposit.mint.mint"],
  ["deposit-spend.unapplied.plutus.json", "user_events/deposit.spend.spend"],
  [
    "field-preimage-certificate-mint.plutus.json",
    "field_preimage_certificate.field_preimage_certificate.mint",
  ],
  [
    "field-preimage-certificate-spend.plutus.json",
    "field_preimage_certificate.field_preimage_certificate.spend",
  ],
  [
    "fraud-proof-catalogue-mint.unapplied.plutus.json",
    "fraud_proof_catalogue.mint.mint",
  ],
  [
    "fraud-proof-catalogue-spend.plutus.json",
    "fraud_proof_catalogue.spend.else",
  ],
  ["fraud-proof-mint.unapplied.plutus.json", "fraud_proof.mint.mint"],
  ["fraud-proof-spend.plutus.json", "fraud_proof.spend.else"],
  ["hub-oracle-mint.unapplied.plutus.json", "hub_oracle.mint.mint"],
  ["membership-stake.plutus.json", "phas.membership.withdraw"],
  [
    "mpf-chunked-verify-withdraw.plutus.json",
    "mpf_chunked_verify.verify.withdraw",
  ],
  ["non-membership-stake.plutus.json", "pexcludes.exclusion.withdraw"],
  ["payout-mint.unapplied.plutus.json", "payout.mint.mint"],
  ["payout-spend.unapplied.plutus.json", "payout.spend.spend"],
  [
    "registered-operators-mint.unapplied.plutus.json",
    "operator_directory/registered_operators.mint.mint",
  ],
  [
    "registered-operators-spend.unapplied.plutus.json",
    "operator_directory/registered_operators.spend.spend",
  ],
  ["reserve-spend.unapplied.plutus.json", "reserve.spend.spend"],
  ["reserve-withdraw.plutus.json", "reserve.withdraw.else"],
  [
    "retired-operators-mint.unapplied.plutus.json",
    "operator_directory/retired_operators.mint.mint",
  ],
  [
    "retired-operators-spend.unapplied.plutus.json",
    "operator_directory/retired_operators.spend.spend",
  ],
  ["scheduler-mint.unapplied.plutus.json", "scheduler.mint.mint"],
  ["scheduler-spend.unapplied.plutus.json", "scheduler.spend.spend"],
  ["settlement-mint.unapplied.plutus.json", "settlement.mint.mint"],
  ["settlement-spend.unapplied.plutus.json", "settlement.spend.spend"],
  ["state-queue-mint.unapplied.plutus.json", "state_queue.mint.mint"],
  ["state-queue-spend.unapplied.plutus.json", "state_queue.spend.spend"],
  [
    "tx-field-preimage-spend.plutus.json",
    "user_events/tx_field_preimage_v1.spend.spend",
  ],
  [
    "tx-field-receipt-mint.unapplied.plutus.json",
    "user_events/tx_field_receipt_v1.mint.mint",
  ],
  [
    "tx-field-receipt-spend.plutus.json",
    "user_events/tx_field_receipt_spend_v1.spend.spend",
  ],
  ["tx-order-mint.unapplied.plutus.json", "user_events/tx_order_v1.mint.mint"],
  [
    "tx-order-spend.unapplied.plutus.json",
    "user_events/tx_order_v1.spend.spend",
  ],
  [
    "user-event-witness-publish.unapplied.plutus.json",
    "user_events/witness.main.publish",
  ],
  ["withdrawal-mint.unapplied.plutus.json", "user_events/withdrawal.mint.mint"],
  [
    "withdrawal-spend.unapplied.plutus.json",
    "user_events/withdrawal.spend.spend",
  ],
]);

// Deployment parameters reviewed against the fixed target Aiken ABI. The
// deployment verifier requires every exported title to retain this exact order.
const REVIEWED_PARAMETERS = {
  "computation_thread.mint.mint": [
    "fraud_proof_catalogue_script_hash",
    "hub_oracle_script_hash",
  ],
  "da_params_governor.da_params_governor.mint": [
    "init_ref",
    "max_committee_size",
    "max_owner_count",
  ],
  "da_params_governor.da_params_governor.spend": [
    "init_ref",
    "max_committee_size",
    "max_owner_count",
  ],
  "user_events/deposit.mint.mint": ["hub_oracle"],
  "user_events/deposit.spend.spend": ["hub_oracle"],
  "fraud_proofs/canonical_decodability/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/canonical_decodability/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proof_catalogue.mint.mint": ["hub_oracle_script_hash"],
  "fraud_proofs/committed_field_shape/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/committed_field_shape/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/da_hash_preimage/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/da_hash_preimage/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/double_spend/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/double_spend/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/double_spend/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/double_spend/step_04.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/double_withdraw/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/double_withdraw/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/fabricated_deposit/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/fabricated_deposit/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/fabricated_deposit/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/fabricated_deposit/step_04.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/fabricated_withdrawal/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/fabricated_withdrawal/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/fabricated_withdrawal/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/fabricated_withdrawal/step_04.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/input_no_idx/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/input_no_idx/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/input_no_idx/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/input_no_idx/step_04.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/l2_tx_mistag/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/l2_tx_mistag/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proof.mint.mint": ["computation_thread_script_hash"],
  "fraud_proofs/missing_native_script_tx/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/missing_native_script_tx/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_native_script_tx/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/missing_native_script_tx/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_native_script_tx/step_05.main.spend": [
    "step_06_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/missing_native_script_tx/step_06.main.spend": [
    "step_07_validator_script_hash",
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_native_script_tx/step_07.main.spend": [
    "step_08_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_native_script_tx/step_08.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_native_script_utxo/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/missing_native_script_utxo/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_native_script_utxo/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/missing_native_script_utxo/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/missing_native_script_utxo/step_05.main.spend": [
    "step_06_validator_script_hash",
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_native_script_utxo/step_06.main.spend": [
    "step_07_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_native_script_utxo/step_07.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/mpf_chunked_proof/challenge.main.spend": [
    "hub_oracle_script_hash",
  ],
  "fraud_proofs/native_script_decoding/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/native_script_decoding/step_02.main.spend": [
    "step_03_open_subject_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/native_script_decoding/step_03_advance_or_close.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/native_script_decoding/step_03_bind_descriptor.main.spend": [
    "step_03_advance_or_close_validator_script_hash",
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/native_script_decoding/step_03_open_subject.main.spend": [
    "step_03_bind_descriptor_validator_script_hash",
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/native_script_decoding/step_04.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/reference_input_no_idx/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/reference_input_no_idx/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/reference_input_no_idx/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/reference_input_no_idx/step_04.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/validation_trace/award_v1.main.spend": [
    "computation_thread_policy_id",
    "fraud_proof_policy_id",
    "fraud_proof_address",
  ],
  "fraud_proofs/validation_trace/boundary_v1.main.spend": [
    "one_step_resolver_script_hashes",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/validation_trace/canonical_decode_empty_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/canonical_decode_item_observe_v1.main.spend": [
    "proof_verifier_script_hash",
    "computation_thread_policy_id",
    "proof_item_script_hash",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/validation_trace/canonical_decode_item_proof_v1.main.spend": [
    "successor_verifier_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend":
    ["source_binder_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/canonical_decode_item_settlement_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/canonical_decode_item_source_v1.main.spend": [
    "observer_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/canonical_decode_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_finish_semantic_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/compact_binding_semantic_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/compact_binding_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/game_v1.main.spend": [
    "boundary_validator_script_hash",
    "timeout_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/validation_trace/input_sets_empty_semantic_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/input_sets_item_semantic_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/validation_trace/input_sets_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/ledger_delta_finalize_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/ledger_delta_operation_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/ledger_delta_output_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/ledger_delta_output_semantic_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/ledger_delta_proof_frame_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/ledger_delta_replay_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/ledger_delta_replay_semantic_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/ledger_delta_terminal_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/ledger_delta_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/script_integrity_authentication_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_integrity_compact_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_integrity_finalize_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_integrity_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/script_integrity_witness_set_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_zero_begin_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_stage_zero_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_zero_hash_advance_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_zero_hash_block_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_zero_hash_terminal_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/signatures_address_item_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/signatures_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/static_ledger_rules_semantic_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/static_ledger_rules_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/timeout_v1.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/withdrawn_input/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/withdrawn_input/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/withdrawn_input/step_03.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/withdrawn_reference_input/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/withdrawn_reference_input/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/withdrawn_reference_input/step_03.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "hub_oracle.mint.mint": ["init_utxo", "hub_oracle_asset_name"],
  "payout.mint.mint": ["hub_oracle"],
  "payout.spend.spend": ["hub_oracle"],
  "operator_directory/registered_operators.mint.mint": [
    "retired_operators_mint_script_hash",
    "hub_oracle_script_hash",
  ],
  "operator_directory/registered_operators.spend.spend": [
    "registered_operators_mint_script_hash",
  ],
  "reserve.spend.spend": ["hub_oracle"],
  "scheduler.mint.mint": ["hub_oracle_script_hash"],
  "scheduler.spend.spend": [
    "registered_operators_policy_id",
    "active_operators_address",
    "active_operators_policy_id",
    "scheduler_policy_id",
    "hub_oracle_script_hash",
  ],
  "user_events/tx_order_v1.mint.mint": [
    "hub_oracle",
    "field_preimage_certificate_policy_id",
  ],
  "user_events/tx_order_v1.spend.spend": ["hub_oracle"],
  "user_events/witness.main.publish": ["nonce"],
  "user_events/withdrawal.mint.mint": ["hub_oracle"],
  "user_events/withdrawal.spend.spend": ["hub_oracle"],
  "operator_directory/active_operators.mint.mint": [
    "hub_oracle_script_hash",
    "registered_operators_policy_id",
    "retired_operators_policy_id",
  ],
  "operator_directory/active_operators.spend.spend": [
    "active_operators_mint_script_hash",
    "hub_oracle_script_hash",
  ],
  "correction_lock.spend.spend": [
    "hub_oracle_policy_id",
    "availability_policy_id",
  ],
  "da_attestation.da_attestation.mint": [
    "da_params_policy_id",
    "reference_script_auth_policy_id",
    "availability_policy_id",
    "availability_parameters",
  ],
  "da_attestation.da_attestation.spend": [
    "da_params_policy_id",
    "reference_script_auth_policy_id",
    "availability_policy_id",
    "availability_parameters",
  ],
  "fraud_proofs/validation_trace/dispute_v1.main.spend": [
    "source_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/validation_trace/source_v1.main.spend": [
    "game_validator_script_hash",
    "award_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "operator_directory/retired_operators.mint.mint": ["hub_oracle_script_hash"],
  "operator_directory/retired_operators.spend.spend": [
    "retired_operators_mint_script_hash",
  ],
  "settlement.mint.mint": ["hub_oracle"],
  "settlement.spend.spend": ["hub_oracle", "settlement_policy_id"],
  "fraud_proofs/field_preimage_length_mismatch/step_01.main.spend": [
    "accepted_step_02_validator_script_hash",
    "forced_step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/field_preimage_length_mismatch/step_02_accepted.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/field_preimage_length_mismatch/step_02_forced.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/field_preimage_length_mismatch/step_03.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/field_item_width_illegal/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/field_item_width_illegal/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/field_item_width_illegal/step_03.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/observers_forbidden_on_untagged_network/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/observers_forbidden_on_untagged_network/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/observer_order_invalid/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/observer_order_invalid/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/observer_order_invalid/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/observer_order_invalid/step_04.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/redeemer_canonicity/step_01.main.spend": [
    "step_02_hash",
    "computation_thread_policy",
    "hub_oracle",
  ],
  "fraud_proofs/redeemer_canonicity/step_02.main.spend": [
    "step_03_hash",
    "computation_thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/redeemer_canonicity/step_03.main.spend": [
    "fraud_policy",
    "fraud_address",
    "computation_thread_policy",
  ],
  "fraud_proofs/distinct_asset_accumulation_limit/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/distinct_asset_accumulation_limit/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/distinct_asset_accumulation_limit/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/distinct_asset_accumulation_limit/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/distinct_asset_accumulation_limit/step_05.main.spend": [
    "step_06_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/distinct_asset_accumulation_limit/step_06.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/mint_declared_asset_limit/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/mint_declared_asset_limit/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/mint_declared_asset_limit/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/mint_declared_asset_limit/step_04.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/witness_script_decoding/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/witness_script_decoding/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/witness_script_decoding/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/witness_script_decoding/step_04.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/output_reference_script_decoding/step_01.main.spend": [
    "step_02_hash",
    "computation_thread_policy",
    "hub_oracle",
  ],
  "fraud_proofs/output_reference_script_decoding/step_02.main.spend": [
    "step_03_hash",
    "computation_thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/output_reference_script_decoding/step_03.main.spend": [
    "step_04_hash",
    "computation_thread_policy",
  ],
  "fraud_proofs/output_reference_script_decoding/step_04.main.spend": [
    "step_05_hash",
    "computation_thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/output_reference_script_decoding/step_05.main.spend": [
    "step_06_hash",
    "computation_thread_policy",
  ],
  "fraud_proofs/output_reference_script_decoding/step_06.main.spend": [
    "fraud_proof_policy",
    "fraud_proof_address",
    "computation_thread_policy",
  ],
  "fraud_proofs/execution_source_script_decoding/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/execution_source_script_decoding/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/execution_source_script_decoding/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/execution_source_script_decoding/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/execution_source_script_decoding/step_05.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/execution_native_script_invalid/step_01.main.spend": [
    "accepted_reconstruction_init_script_hash",
    "forced_step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/execution_native_script_invalid/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/execution_native_script_invalid/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/execution_native_script_invalid/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/execution_native_script_invalid/step_05.main.spend": [
    "step_06_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/execution_native_script_invalid/step_06.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.main.spend":
    ["accepted_spend_prefix_script_hash", "computation_thread_token_policy_id"],
  "fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.main.spend": [
    "accepted_mint_prefix_script_hash",
    "accepted_inline_source_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/execution_native_script_invalid/accepted_mint_prefix.main.spend": [
    "accepted_observer_prefix_script_hash",
    "accepted_inline_source_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.main.spend": [
    "accepted_receive_prefix_script_hash",
    "accepted_inline_source_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.main.spend": [
    "accepted_inline_source_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/execution_native_script_invalid/accepted_inline_source.main.spend": [
    "evaluator_step_03_script_hash",
    "accepted_reference_source_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main.spend": [
    "evaluator_step_03_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_redeemer/step_01.main.spend": [
    "step_02_hash",
    "computation_thread_policy",
    "hub_oracle",
  ],
  "fraud_proofs/missing_redeemer/step_02.main.spend": [
    "step_02a_hash",
    "computation_thread_policy",
  ],
  "fraud_proofs/missing_redeemer/step_02a.main.spend": [
    "step_02b_hash",
    "computation_thread_policy",
  ],
  "fraud_proofs/missing_redeemer/step_02b.main.spend": [
    "step_03_hash",
    "computation_thread_policy",
  ],
  "fraud_proofs/missing_redeemer/step_03.main.spend": [
    "step_04_hash",
    "thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/missing_redeemer/step_04.main.spend": [
    "step_05_hash",
    "thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/missing_redeemer/step_05.main.spend": [
    "fraud_policy",
    "fraud_address",
    "thread_policy",
  ],
  "fraud_proofs/unused_redeemer/step_01.main.spend": [
    "step_02_hash",
    "thread_policy",
    "hub_oracle",
  ],
  "fraud_proofs/unused_redeemer/step_02.main.spend": [
    "step_02a_hash",
    "thread_policy",
  ],
  "fraud_proofs/unused_redeemer/step_02a.main.spend": [
    "step_02b_hash",
    "thread_policy",
  ],
  "fraud_proofs/unused_redeemer/step_02b.main.spend": [
    "step_02c_hash",
    "thread_policy",
  ],
  "fraud_proofs/unused_redeemer/step_02c.main.spend": [
    "step_03_hash",
    "thread_policy",
  ],
  "fraud_proofs/unused_redeemer/step_03.main.spend": [
    "step_04_hash",
    "thread_policy",
  ],
  "fraud_proofs/unused_redeemer/step_04.main.spend": [
    "step_05_hash",
    "thread_policy",
  ],
  "fraud_proofs/unused_redeemer/step_05.main.spend": [
    "step_06_hash",
    "thread_policy",
  ],
  "fraud_proofs/unused_redeemer/step_06.main.spend": [
    "thread_policy",
    "fraud_policy",
    "fraud_address",
  ],
  "fraud_proofs/unused_script_witness/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/unused_script_witness/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/unused_script_witness/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/unused_script_witness/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/unused_script_witness/step_05.main.spend": [
    "step_06_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/unused_script_witness/step_06.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/missing_script_source/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/missing_script_source/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/missing_script_source/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/missing_script_source/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/missing_script_source/step_05.main.spend": [
    "step_06_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/missing_script_source/step_06.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/script_integrity_hash_missing/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/script_integrity_hash_missing/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/script_integrity_hash_missing/step_03.main.spend": [
    "step_04_validator_script_hash",
    "script_grammar_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/script_integrity_hash_missing/script_grammar.main.spend": [
    "script_scan_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/script_integrity_hash_missing/script_scan.main.spend": [
    "redeemer_grammar_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/script_integrity_hash_missing/step_04.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/protected_output_signer_missing/step_01.main.spend": [
    "step_02_hash",
    "computation_thread_policy",
    "hub_oracle",
  ],
  "fraud_proofs/protected_output_signer_missing/step_02.main.spend": [
    "step_03_hash",
    "step_05_hash",
    "computation_thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/protected_output_signer_missing/step_03.main.spend": [
    "step_04_hash",
    "computation_thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/protected_output_signer_missing/step_04.main.spend": [
    "step_05_hash",
    "computation_thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/protected_output_signer_missing/step_05.main.spend": [
    "fraud_proof_policy",
    "fraud_proof_address",
    "computation_thread_policy",
  ],
  "fraud_proofs/resolved_output_non_canonical/step_01.main.spend": [
    "step_02_hash",
    "thread_policy",
    "hub_oracle",
  ],
  "fraud_proofs/resolved_output_non_canonical/step_02.main.spend": [
    "step_03_hash",
    "thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/resolved_output_non_canonical/step_03.main.spend": [
    "step_04_hash",
    "thread_policy",
  ],
  "fraud_proofs/resolved_output_non_canonical/step_04.main.spend": [
    "step_05_hash",
    "thread_policy",
  ],
  "fraud_proofs/resolved_output_non_canonical/step_05.main.spend": [
    "fraud_policy",
    "fraud_address",
    "thread_policy",
  ],
  "fraud_proofs/spend_input_signer_missing/step_01.main.spend": [
    "step_02_hash",
    "thread_policy",
    "hub_oracle",
  ],
  "fraud_proofs/spend_input_signer_missing/step_02.main.spend": [
    "step_03_hash",
    "step_05_hash",
    "thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/spend_input_signer_missing/step_03.main.spend": [
    "step_04_hash",
    "thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/spend_input_signer_missing/step_04.main.spend": [
    "step_05_hash",
    "thread_policy",
    "certificate_policy",
  ],
  "fraud_proofs/spend_input_signer_missing/step_05.main.spend": [
    "fraud_proof_policy",
    "fraud_proof_address",
    "thread_policy",
  ],
  "fraud_proofs/receive_purpose_language/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/receive_purpose_language/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/receive_purpose_language/step_03.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/script_integrity_hash_mismatch/step_01.main.spend": [
    "step_02_hash",
    "thread_policy",
    "hub_oracle",
  ],
  "fraud_proofs/script_integrity_hash_mismatch/step_02.main.spend": [
    "step_03_hash",
    "thread_policy",
  ],
  "fraud_proofs/script_integrity_hash_mismatch/step_03.main.spend": [
    "step_04_hash",
    "thread_policy",
  ],
  "fraud_proofs/script_integrity_hash_mismatch/step_04.main.spend": [
    "step_05_hash",
    "thread_policy",
  ],
  "fraud_proofs/script_integrity_hash_mismatch/step_05.main.spend": [
    "thread_policy",
    "fraud_policy",
    "fraud_address",
  ],
  "fraud_proofs/validation_trace/script_sources_stage_seven_observer_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_stage_seven_receive_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_seven_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_seven_observer_item_yield_v1.main.withdraw":
    ["observer_dispatcher_script_hash", "field_preimage_certificate_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_seven_observer_bound_yield_v1.main.withdraw":
    ["observer_dispatcher_script_hash"],
  "fraud_proofs/validation_trace/script_sources_redeemer_item_step_yield_v1.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_stage_eight_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_eight_purpose_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_nine_missing_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_nine_mismatch_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_nine_native_match_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_nine_effectful_match_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_ten_missing_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_ten_match_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_stage_ten_mismatch_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_stage_eleven_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_eleven_source_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_twelve_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_twelve_redeemer_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/value_and_mint_asset_fold_yield.main.withdraw":
    [
      "replay_asset_dispatcher_script_hash",
      "output_asset_dispatcher_script_hash",
      "mint_asset_dispatcher_script_hash",
    ],
  "fraud_proofs/validation_trace/value_and_mint_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/value_and_mint_begin_semantic_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/value_and_mint_replay_begin_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/value_and_mint_replay_input_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/value_and_mint_replay_asset_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/value_and_mint_replay_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/value_and_mint_output_descriptor_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/value_and_mint_output_asset_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/value_and_mint_output_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/value_and_mint_mint_asset_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/value_and_mint_mint_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/value_and_mint_finalize_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_output_proof_begin_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_output_proof_begin_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_output_proof_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_output_proof_finish_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_two_advance.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_two_advance.else":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_replay.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_replay.else":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_finish.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_finish.else":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_begin.withdraw":
    ["dispatcher_script_hashes", "field_preimage_certificate_policy_id"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_begin.else":
    ["dispatcher_script_hashes", "field_preimage_certificate_policy_id"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_finish.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_finish.else":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_begin_policy.withdraw":
    ["dispatcher_script_hashes", "field_preimage_certificate_policy_id"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_begin_policy.else":
    ["dispatcher_script_hashes", "field_preimage_certificate_policy_id"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_fold_asset.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_fold_asset.else":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_finish.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_finish.else":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/script_sources_non_output_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_non_output_semantic_v1.main.else":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/resolve_inputs_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/resolve_inputs_v1.main.else": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/resolve_inputs_initial_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/resolve_inputs_initial_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/resolve_inputs_finish_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/resolve_inputs_finish_semantic_v1.main.else": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/resolve_inputs_membership_begin_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/resolve_inputs_membership_begin_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/native_scripts_effectful_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/native_scripts_effectful_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/native_scripts_native_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/native_scripts_native_semantic_v1.main.else": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/native_scripts_terminal_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/native_scripts_terminal_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/native_scripts_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/native_scripts_v1.main.else": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/phase_a_native_scripts_advance_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_advance_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_container_frame_payload_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_container_frame_payload_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_empty_container_payload_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_empty_container_payload_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_frame_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_frame_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1.main.else":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.foreign.withdraw":
    [
      "dispatcher_script_hash",
      "award_script_hash",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.foreign.else":
    [
      "dispatcher_script_hash",
      "award_script_hash",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.native.withdraw":
    [
      "dispatcher_script_hash",
      "award_script_hash",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.native.else":
    [
      "dispatcher_script_hash",
      "award_script_hash",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/phase_a_native_scripts_signature_above_last_payload_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_signature_above_last_payload_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_signature_below_first_payload_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_signature_below_first_payload_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_signature_between_payload_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_signature_between_payload_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_signature_empty_payload_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_signature_empty_payload_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_signature_membership_payload_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_signature_membership_payload_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_timelock_payload_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_timelock_payload_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_token_head_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_token_head_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_native_scripts_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/phase_a_native_scripts_v1.main.else": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/phase_a_script_preconditions_item_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/phase_a_script_preconditions_item_semantic_v1.main.else":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/phase_a_script_preconditions_semantic_v1.main.spend":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_script_preconditions_semantic_v1.main.else":
    ["award_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/phase_a_script_preconditions_v1.main.spend": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/phase_a_script_preconditions_v1.main.else": [
    "semantic_resolver_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/signatures_advance_semantic_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/signatures_advance_semantic_v1.main.else": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/signatures_handoff_semantic_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/signatures_handoff_semantic_v1.main.else": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/signatures_required_item_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/signatures_required_item_semantic_v1.main.else":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "field_preimage_certificate_policy_id",
    ],

  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_envelope_v1.main.spend":
    [
      "deployment_id",
      "traversal_normalizer_script_hash",
      "outer_normalizer_script_hash",
      "semantic_executor_script_hashes",
      "settlement_script_hash",
      "computation_thread_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_traversal_normalizer_v1.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_outer_normalizer_v1.main.spend":
    [
      "deployment_id",
      "computation_thread_policy_id",
      "source_authenticator_script_hash",
    ],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_map_executor_v1.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finalize_frame_executor_v1.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_execution_settlement_v1.main.spend":
    [
      "deployment_id",
      "expected_traversal_normalizer_script_hash",
      "expected_outer_normalizer_script_hash",
      "expected_executor_script_hashes",
      "expected_award_script_hash",
      "computation_thread_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_envelope.main.spend":
    [
      "deployment_id",
      "traversal_normalizer_script_hash",
      "outer_normalizer_script_hash",
      "semantic_executor_script_hashes",
      "settlement_script_hash",
      "computation_thread_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_settlement.main.spend":
    [
      "deployment_id",
      "expected_traversal_normalizer_script_hash",
      "expected_outer_normalizer_script_hash",
      "expected_executor_script_hashes",
      "return_script_hash",
      "computation_thread_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_source_authenticator.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_open_header_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_open_tail_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_scalar_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_sequence_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_map_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_large_constructor_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_attach_integer_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_attach_bytes_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_list_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_integer_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_bytes_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_large_constructor_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_large_fields_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_close_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finish_data_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_header_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_tail_executor.main.spend":
    ["deployment_id", "computation_thread_policy_id"],

  "fraud_proofs/validation_trace/cek_context_step_semantic_v1.main.spend": [
    "cek_context_control_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_control.main.spend": [
    "stage_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_settle.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_reference.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_spend.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_output.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_signer.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_mint_init.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_mint_item.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_assemble.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_tx_info.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_seed.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_redeemer_begin.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_redeemer_select_authenticate.main.spend":
    ["initialize_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/cek_context_redeemer_select_initialize.main.spend":
    ["hash_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/cek_context_redeemer_select_hash.main.spend": [
    "finish_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_redeemer_select_finish.main.spend":
    ["settle_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/cek_context_finalize_authenticate.main.spend":
    ["summary_script_hashes", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/cek_context_finalize_spend.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_finalize_mint.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_finalize_withdraw.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_finalize_observe.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_finalize_midgard.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_observer_authenticate.main.spend":
    [
      "fold_script_hash",
      "computation_thread_policy_id",
      "field_preimage_certificate_policy_id",
    ],
  "fraud_proofs/validation_trace/cek_context_observer_fold.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_item_bind.main.spend": [
    "item_entry_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_item_return.main.spend": [
    "return_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_item_hash.main.spend": [
    "finish_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_item_finalize.main.spend": [
    "finish_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_item_selection_continue.main.spend":
    ["settle_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/cek_context_item_selection_finish.main.spend":
    ["settle_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/cek_context_item_data_continue.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_context_item_data_finish_descriptor.main.spend":
    ["settle_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/cek_context_item_data_finish_value.main.spend":
    ["settle_script_hash", "computation_thread_policy_id"],

  "fraud_proofs/validation_trace/cek_execution_selection_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "cek_program_material_script_hash",
      "reference_script_auth_policy_id",
      "cek_material_traversal_script_hash",
    ],
  "fraud_proofs/validation_trace/cek_execution_selection_yields.authenticate.withdraw":
    ["dispatcher_script_hash"],
  "fraud_proofs/validation_trace/cek_execution_selection_yields.successor.withdraw":
    ["dispatcher_script_hash"],
  "fraud_proofs/validation_trace/cek_execution_selection_yields.material_program.withdraw":
    ["dispatcher_script_hash", "cek_program_material_script_hash"],
  "fraud_proofs/validation_trace/cek_execution_selection_yields.material_data.withdraw":
    ["dispatcher_script_hash", "cek_program_material_script_hash"],

  "fraud_proofs/validation_trace/cek_core_arm_compute.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend": [
    "budget_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_semantic_result.main.spend": [
    "budget_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend": [
    "semantics_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_direct_scalar.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_direct_structured.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_arm_machine.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_arm_map_conversion.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_semantic_pair.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_semantic_list_construct.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_semantic_list_select.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_semantic_choose.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_semantic_data_construct.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_semantic_data_scalar.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_semantic_data_misc.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_failure_known.main.spend": [
    "budget_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_failure_budget.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_semantic_failure_roots.main.spend": [
    "material_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_semantic_failure_material.main.spend":
    ["settle_script_hash", "computation_thread_policy_id"],
  "fraud_proofs/validation_trace/cek_core_type_failure_roots.main.spend": [
    "kinds_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_type_failure_kinds.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_bls_budget.main.spend": [
    "final_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_bls_roots.main.spend": [
    "final_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_bls_final.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_map_start_roots.main.spend": [
    "budget_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_map_start_budget.main.spend": [
    "nodes_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_map_start_nodes.main.spend": [
    "settle_script_hash",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_settle.main.spend": [
    "award_script_hash",
    "arm_hop_counts",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_core_step_semantic_v1.main.spend": [
    "arm_script_hashes",
    "computation_thread_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_material_traversal_v1.main.spend": [
    "award_script_hash",
    "computation_thread_policy_id",
    "reference_script_auth_policy_id",
  ],
  "fraud_proofs/validation_trace/cek_material_traversal_yields.program.withdraw":
    ["dispatcher_script_hash"],
  "fraud_proofs/validation_trace/cek_material_traversal_yields.data.withdraw": [
    "dispatcher_script_hash",
  ],
  "fraud_proofs/validation_trace/ledger_output_descriptor_datum_summary_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_descriptor_reference_script_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_descriptor_scan_facts_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_descriptor_value_summary_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_advance_bytes_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_advance_integer_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_attach_bytes_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_attach_integer_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_close_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_finalize_frame_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_finish_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_fold_list_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_fold_map_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_head_large_constructor_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_head_map_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_head_scalar_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_head_sequence_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_large_constructor_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_datum_large_fields_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_native_script_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_reference_script_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_scalar_bytes_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_scalar_integer_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_script_hash_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_span_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_structure_assets_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_structure_finish_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_structure_optional_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_structure_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/ledger_output_proof_value_yield.main.withdraw":
    ["dispatcher_script_hashes"],
  "fraud_proofs/validation_trace/resolve_inputs_membership_finalize_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/resolve_inputs_membership_step_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_output_proof_finalize_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/validation_trace/script_sources_output_proof_step_semantic_v1.main.spend":
    [
      "award_script_hash",
      "computation_thread_policy_id",
      "reference_script_auth_policy_id",
    ],
  "fraud_proofs/transition_trace/accepted_transaction_v1.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "reference_script_auth_policy_id",
  ],
  "fraud_proofs/transition_trace/accepted_transaction_yields.claim_endpoints.withdraw":
    ["dispatcher"],
  "fraud_proofs/transition_trace/accepted_transaction_yields.claim_source.withdraw":
    ["dispatcher"],
  "fraud_proofs/transition_trace/accepted_transaction_yields.claim_structure.withdraw":
    ["dispatcher"],
  "fraud_proofs/transition_trace/accepted_transaction_yields.l2_open.withdraw":
    ["dispatcher"],
  "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay.withdraw":
    ["dispatcher"],
  "fraud_proofs/transition_trace/control_v1.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/transition_trace/deposit_summaries.summaries.withdraw": [
    "dispatcher",
  ],
  "fraud_proofs/transition_trace/deposit_v1.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "reference_script_auth_policy_id",
  ],
  "fraud_proofs/transition_trace/deposit_value.value_output.withdraw": [
    "dispatcher",
  ],
  "fraud_proofs/transition_trace/deposit_yields.projection.withdraw": [
    "dispatcher",
    "hub_oracle",
  ],
  "fraud_proofs/transition_trace/duplicate_v1.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/transition_trace/forced_v1.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/transition_trace/l1_event_v1.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "hub_oracle",
  ],
  "fraud_proofs/transition_trace/output_assembly.assembly.withdraw": [
    "dispatcher",
  ],
  "fraud_proofs/transition_trace/output_scan.scan_output.withdraw": [
    "dispatcher",
  ],
  "fraud_proofs/transition_trace/output_summaries.summaries.withdraw": [
    "dispatcher",
  ],
  "fraud_proofs/transition_trace/output_value.value_output.withdraw": [
    "dispatcher",
  ],
  "fraud_proofs/transition_trace/route_v1.main.spend": [
    "final_validator_script_hashes",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/transition_trace/source_v1.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/transition_trace/withdrawal_v1.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],

  "fraud_proofs/value_not_preserved/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
    "accepted_source_script_hash",
    "forced_source_script_hash",
  ],
  "fraud_proofs/value_not_preserved/step_01.main.else": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
    "accepted_source_script_hash",
    "forced_source_script_hash",
  ],
  "fraud_proofs/value_not_preserved/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/value_not_preserved/step_02.main.else": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/value_not_preserved/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/value_not_preserved/step_03.main.else": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/value_not_preserved/step_04.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/step_04.main.else": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_accepted_source.main.spend": [
    "event_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/value_not_preserved/union_accepted_source.main.else": [
    "event_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/value_not_preserved/union_assets.main.spend": [
    "union_update_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_assets.main.else": [
    "union_update_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_event.main.spend": [
    "pre_state_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_event.main.else": [
    "pre_state_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_field_grammar.main.spend": [
    "outputs_hash",
    "mint_hash",
    "field_certificate_policy_id",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_field_grammar.main.else": [
    "outputs_hash",
    "mint_hash",
    "field_certificate_policy_id",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_forced_source.main.spend": [
    "event_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_forced_source.main.else": [
    "event_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_input_value.main.spend": [
    "union_assets_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_input_value.main.else": [
    "union_assets_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_inputs.main.spend": [
    "input_value_hash",
    "field_grammar_hash",
    "field_certificate_policy_id",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_inputs.main.else": [
    "input_value_hash",
    "field_grammar_hash",
    "field_certificate_policy_id",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_mint.main.spend": [
    "union_update_hash",
    "terminal_hash",
    "field_certificate_policy_id",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_mint.main.else": [
    "union_update_hash",
    "terminal_hash",
    "field_certificate_policy_id",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_output_scan.main.spend": [
    "union_assets_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_output_scan.main.else": [
    "union_assets_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_outputs.main.spend": [
    "output_scan_hash",
    "field_certificate_policy_id",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_outputs.main.else": [
    "output_scan_hash",
    "field_certificate_policy_id",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_pre_state.main.spend": [
    "inputs_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_pre_state.main.else": [
    "inputs_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_terminal.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_terminal.main.else": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_update.main.spend": [
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/value_not_preserved/union_update.main.else": [
    "computation_thread_token_policy_id",
  ],

  "fraud_proofs/mint_authorization/evaluate.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/mint_authorization/evaluate.main.else": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/mint_authorization/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/mint_authorization/step_01.main.else": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/mint_authorization/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/mint_authorization/step_02.main.else": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/mint_authorization/step_03.main.spend": [
    "step_04_validator_script_hash",
    "step_05_validator_script_hash",
    "evaluate_validator_script_hash",
    "witness_scan_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/mint_authorization/step_03.main.else": [
    "step_04_validator_script_hash",
    "step_05_validator_script_hash",
    "evaluate_validator_script_hash",
    "witness_scan_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/mint_authorization/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/mint_authorization/step_04.main.else": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/mint_authorization/step_05.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/mint_authorization/step_05.main.else": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/mint_authorization/witness_scan.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/mint_authorization/witness_scan.main.else": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/withdrawal_mistag/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/withdrawal_mistag/step_01.main.else": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/withdrawal_mistag/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/withdrawal_mistag/step_02.main.else": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/withdrawal_mistag/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/withdrawal_mistag/step_03.main.else": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/withdrawal_mistag/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/withdrawal_mistag/step_04.main.else": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/withdrawal_mistag/step_05.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/withdrawal_mistag/step_05.main.else": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/min_ada/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/min_ada/step_01.main.else": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/min_ada/step_02.main.spend": [
    "step_03_validator_script_hash",
    "_step_05_validator_script_hash",
    "computation_thread_token_policy_id",
    "reference_script_auth_policy_id",
  ],
  "fraud_proofs/min_ada/step_02.main.else": [
    "step_03_validator_script_hash",
    "_step_05_validator_script_hash",
    "computation_thread_token_policy_id",
    "reference_script_auth_policy_id",
  ],
  "fraud_proofs/min_ada/step_02_yields.tx.withdraw": [
    "dispatcher_script_hash",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/min_ada/step_02_yields.tx.else": [
    "dispatcher_script_hash",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/min_ada/step_02_yields.utxo.withdraw": [
    "dispatcher_script_hash",
  ],
  "fraud_proofs/min_ada/step_02_yields.utxo.else": ["dispatcher_script_hash"],
  "fraud_proofs/min_ada/step_03.main.spend": [
    "step_04_validator_script_hash",
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/min_ada/step_03.main.else": [
    "step_04_validator_script_hash",
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/min_ada/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/min_ada/step_04.main.else": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/min_ada/step_05.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/min_ada/step_05.main.else": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/network_id/forced_scan.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/network_id/forced_scan.main.else": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/network_id/forced_step.main.spend": [
    "forced_scan_validator_script_hash",
    "computation_thread_token_policy_id",
    "expected_network_id",
  ],
  "fraud_proofs/network_id/forced_step.main.else": [
    "forced_scan_validator_script_hash",
    "computation_thread_token_policy_id",
    "expected_network_id",
  ],
  "fraud_proofs/transaction_output_non_canonical/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/transaction_output_non_canonical/step_01.main.else": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/transaction_output_non_canonical/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/transaction_output_non_canonical/step_02.main.else": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/transaction_output_non_canonical/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/transaction_output_non_canonical/step_03.main.else": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/transaction_output_non_canonical/step_04.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/transaction_output_non_canonical/step_04.main.else": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/network_id/step_01.main.spend": [
    "step_02_validator_script_hash",
    "forced_step_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
    "expected_network_id",
  ],
  "fraud_proofs/network_id/step_01.main.else": [
    "step_02_validator_script_hash",
    "forced_step_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
    "expected_network_id",
  ],
  "fraud_proofs/network_id/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/network_id/step_02.main.else": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],

  "fraud_proofs/native_script_invalid/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/native_script_invalid/step_01.main.else": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/native_script_invalid/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/native_script_invalid/step_02.main.else": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/native_script_invalid/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/native_script_invalid/step_03.main.else": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/native_script_invalid/step_04.main.spend": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/native_script_invalid/step_04.main.else": [
    "step_05_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/native_script_invalid/step_05.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],
  "fraud_proofs/native_script_invalid/step_05.main.else": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
  ],

  "fraud_proofs/cross_block_duplicate_event/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/cross_block_duplicate_event/step_01.main.else": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/cross_block_duplicate_event/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/cross_block_duplicate_event/step_02.main.else": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],

  "fraud_proofs/missing_signature/forced_signer.main.spend": [
    "forced_witness_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_signature/forced_step.main.spend": [
    "forced_signer_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/missing_signature/forced_witness.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_signature/step_01.main.spend": [
    "step_02_validator_script_hash",
    "forced_step_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/missing_signature/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/missing_signature/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/missing_signature/step_04.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],

  "fraud_proofs/input_set_uniqueness/step_01.main.spend": [
    "step_02_validator_script_hash",
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/input_set_uniqueness/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/input_set_uniqueness/step_03.main.spend": [
    "step_04_hash",
    "thread_policy",
    "field_certificate_policy",
  ],
  "fraud_proofs/input_set_uniqueness/step_04.main.spend": [
    "fraud_policy",
    "fraud_address",
    "thread_policy",
    "field_certificate_policy",
  ],

  "fraud_proofs/no_input/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/no_input/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/no_input/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/no_input/step_04.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/no_reference_input/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/no_reference_input/step_02.main.spend": [
    "step_03_validator_script_hash",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/no_reference_input/step_03.main.spend": [
    "step_04_validator_script_hash",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/no_reference_input/step_04.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/invalid_signature/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/invalid_signature/step_02.main.spend": [
    "computation_thread_token_policy_id",
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/min_fee/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/min_fee/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "fraud_proofs/invalid_range/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/invalid_range/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
  ],
  "fraud_proofs/zero_input/step_01.main.spend": [
    "step_02_validator_script_hash",
    "computation_thread_token_policy_id",
    "hub_oracle",
  ],
  "fraud_proofs/zero_input/step_02.main.spend": [
    "fraud_proof_token_policy_id",
    "fraud_proof_token_address",
    "computation_thread_token_policy_id",
    "field_preimage_certificate_policy_id",
  ],
  "state_queue.mint.mint": [
    "hub_oracle_script_hash",
    "correction_lock_script_hash",
    "active_operators_script_hash",
    "active_operators_addr",
    "retired_operators_script_hash",
    "scheduler_script_hash",
    "fraud_proof_script_hash",
    "settlement_script_hash",
    "da_attestation_policy_id",
    "availability_policy_id",
    "reference_script_auth_policy_id",
  ],
  "state_queue.spend.spend": [
    "state_queue_mint_script_hash",
    "da_attestation_policy_id",
    "availability_policy_id",
  ],
  "availability_challenge.availability_challenge.mint": [
    "hub_oracle_policy_id",
    "reference_script_auth_policy_id",
    "parameters",
  ],
  "availability_challenge.availability_challenge.spend": [
    "hub_oracle_policy_id",
    "reference_script_auth_policy_id",
    "parameters",
  ],
  "state_queue_yields.commit.withdraw": [
    "state_queue_policy_id",
    "hub_oracle_script_hash",
    "correction_lock_script_hash",
    "active_operators_script_hash",
    "active_operators_addr",
    "scheduler_script_hash",
    "da_attestation_policy_id",
  ],
  "state_queue_yields.remove_unattested.withdraw": [
    "state_queue_policy_id",
    "hub_oracle_script_hash",
    "correction_lock_script_hash",
  ],
  "state_queue_yields.remove_unavailable.withdraw": [
    "state_queue_policy_id",
    "hub_oracle_script_hash",
    "correction_lock_script_hash",
    "availability_policy_id",
  ],
  "state_queue_yields.remove_fraudulent.withdraw": [
    "state_queue_policy_id",
    "hub_oracle_script_hash",
    "correction_lock_script_hash",
    "active_operators_script_hash",
    "retired_operators_script_hash",
    "fraud_proof_script_hash",
  ],
  "state_queue_yields.merge.withdraw": [
    "state_queue_policy_id",
    "hub_oracle_script_hash",
    "correction_lock_script_hash",
    "settlement_script_hash",
    "da_attestation_policy_id",
  ],
  "availability_challenge_yields.bond.withdraw": [
    "availability_policy_id",
    "hub_oracle_policy_id",
    "parameters",
  ],
  "availability_challenge_yields.open.withdraw": [
    "availability_policy_id",
    "hub_oracle_policy_id",
    "parameters",
  ],
  "availability_challenge_yields.settle.withdraw": [
    "availability_policy_id",
    "hub_oracle_policy_id",
    "parameters",
  ],
  "availability_challenge_yields.close.withdraw": [
    "availability_policy_id",
    "hub_oracle_policy_id",
    "parameters",
  ],
  "availability_challenge_yields.timeout.withdraw": [
    "availability_policy_id",
    "hub_oracle_policy_id",
    "parameters",
  ],
};

const underscores = (value) => value.replaceAll("-", "_");

export const offchainTitleForGeneratedFile = (fileName) => {
  const direct = DIRECT_TITLES.get(fileName);
  if (direct !== undefined) return direct;

  const step = fileName.match(
    /^fraud-proof-(.+)-step-(\d{2})\.unapplied\.plutus\.json$/u,
  );
  if (step !== null) {
    return `fraud_proofs/${underscores(step[1])}/step_${step[2]}.main.spend`;
  }

  const transitionTrace = fileName.match(
    /^fraud-proof-transition-trace-(.+)-v1\.unapplied\.plutus\.json$/u,
  );
  if (transitionTrace !== null) {
    return `fraud_proofs/transition_trace/${underscores(transitionTrace[1])}_v1.main.spend`;
  }

  const validationTrace = fileName.match(
    /^fraud-proof-validation-trace-(.+)\.unapplied\.plutus\.json$/u,
  );
  if (validationTrace !== null) {
    return `fraud_proofs/validation_trace/${underscores(validationTrace[1])}.main.spend`;
  }

  if (fileName === "fraud-proof-validation-trace-proof-item-v1.plutus.json") {
    return "fraud_proofs/validation_trace/proof_item_v1.main.else";
  }

  throw new Error(`No off-chain blueprint title mapping for ${fileName}`);
};

const parseGeneratedScript = (raw, sourcePath) => {
  const parsed = JSON.parse(raw);
  if (
    typeof parsed !== "object" ||
    parsed === null ||
    parsed.type !== "PlutusScriptV3" ||
    typeof parsed.description !== "string" ||
    !/^midgard\./u.test(parsed.description) ||
    typeof parsed.cborHex !== "string" ||
    !/^[0-9a-f]+$/u.test(parsed.cborHex) ||
    parsed.cborHex.length % 2 !== 0
  ) {
    throw new Error(`Invalid generated Plutarch V3 script: ${sourcePath}`);
  }
  return parsed;
};

export const unwrapTextEnvelopeCborHex = (cborHex, source = "cborHex") => {
  const bytes = Buffer.from(cborHex, "hex");
  if (bytes.length === 0 || (bytes[0] & 0xe0) !== 0x40) {
    throw new Error(`${source} must be a definite CBOR byte string`);
  }

  const additionalInfo = bytes[0] & 0x1f;
  let headerLength;
  let payloadLength;
  if (additionalInfo < 24) {
    headerLength = 1;
    payloadLength = BigInt(additionalInfo);
  } else {
    const lengthBytes =
      additionalInfo === 24
        ? 1
        : additionalInfo === 25
          ? 2
          : additionalInfo === 26
            ? 4
            : additionalInfo === 27
              ? 8
              : 0;
    if (lengthBytes === 0 || bytes.length < 1 + lengthBytes) {
      throw new Error(`${source} has an invalid CBOR byte-string header`);
    }
    headerLength = 1 + lengthBytes;
    payloadLength = 0n;
    for (let index = 1; index < headerLength; index += 1) {
      payloadLength = (payloadLength << 8n) | BigInt(bytes[index]);
    }
  }

  if (payloadLength !== BigInt(bytes.length - headerLength)) {
    throw new Error(
      `${source} CBOR byte-string length does not match its payload`,
    );
  }
  const payload = bytes.subarray(headerLength);
  if (payload.length === 0 || (payload[0] & 0xe0) !== 0x40) {
    throw new Error(
      `${source} must wrap the single-CBOR script encoding expected by an Aiken blueprint`,
    );
  }
  return payload.toString("hex");
};

export const buildOffchainBlueprint = async ({
  generatedDir = path.join(plutarchDir, "generated"),
  outputPath = path.join(plutarchDir, "plutus.json"),
} = {}) => {
  const fileNames = (await readdir(generatedDir))
    .filter((fileName) => fileName.endsWith(".plutus.json"))
    .sort();
  if (fileNames.length === 0) {
    throw new Error(`No generated Plutarch scripts found in ${generatedDir}`);
  }

  const validators = await Promise.all(
    fileNames.map(async (fileName) => {
      const sourcePath = path.join(generatedDir, fileName);
      const generated = parseGeneratedScript(
        await readFile(sourcePath, "utf8"),
        sourcePath,
      );
      const title = offchainTitleForGeneratedFile(fileName);
      const parameters = REVIEWED_PARAMETERS[title];
      return {
        title,
        ...(parameters === undefined
          ? {}
          : { parameters: parameters.map((title) => ({ title })) }),
        compiledCode: unwrapTextEnvelopeCborHex(
          generated.cborHex,
          `${sourcePath}.cborHex`,
        ),
      };
    }),
  );

  const titles = validators.map(({ title }) => title);
  if (new Set(titles).size !== titles.length) {
    throw new Error(
      "Generated Plutarch scripts map to duplicate blueprint titles",
    );
  }

  const blueprint = {
    preamble: {
      title: "midgard/plutarch-offchain",
      description: "Plutarch contracts adapted for Midgard off-chain consumers",
      version: "1.0.0",
      plutusVersion: "v3",
      compiler: {
        name: "Plutarch",
        version: "v1.14.0+eac4bf1c",
      },
    },
    validators,
  };
  await writeFile(outputPath, `${JSON.stringify(blueprint, null, 2)}\n`);
  return { outputPath, validatorCount: validators.length, titles };
};

const invokedPath = process.argv[1]
  ? pathToFileURL(path.resolve(process.argv[1])).href
  : undefined;
if (invokedPath === import.meta.url) {
  const result = await buildOffchainBlueprint();
  process.stdout.write(
    `Wrote ${result.validatorCount.toString()} Plutarch validators to ${result.outputPath}\n`,
  );
}
