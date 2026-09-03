# Non-interactive proof-thread program V1 baseline

Captured before Wave 0 implementation on 2026-09-01. This is the reproducible
baseline required by
[`non-interactive-proof-thread-program-v1.md`](non-interactive-proof-thread-program-v1.md),
not a release-readiness claim.

## Identity

- Git branch: `colll78/canonical-v1-watcher-l1-source-checkpoint`
- Git commit: `815b703a99c26161cb735ab2f298bf0cbce4524d`
- Aiken: `v1.1.23+5adf783`
- Build: `cd onchain/aiken && aiken build --env testnet`
- Blueprint validator entries: 567
- Working-tree blueprint SHA-256:
  `2f7361162be0f1c4fc456cea5ad0706a5473612d732f416cb73433e36ab30103`
- Catalogue entries: 32 (`00000000` through `0000001f`)
- Catalogue root:
  `85ecf82f70e409621d5324c54ae8e2deedbb7c37698e28ba7d76481c17bb6e90`

The working-tree blueprint digest intentionally differs from the previously
documented clean-tree digest because the preserved withdrawal-mistag work
changes an applied validator.

## Preserved pre-program work

These paths were dirty before Wave 0 and are not cleanup targets:

- `demo/midgard-fault-proofs/src/withdrawal-mistag/prepare-withdrawal-mistag.ts`
- `demo/midgard-fault-proofs/src/withdrawal-mistag/submit-withdrawal-mistag-steps.ts`
- `demo/midgard-fault-proofs/tests/submit-init-emulator-withdrawal-mistag-cancel-resume.test.ts`
- `demo/midgard-fault-proofs/tests/submit-init-emulator-withdrawal-mistag-invalid-marked-valid.test.ts`
- `demo/midgard-fault-proofs/tests/submit-init-emulator-withdrawal-mistag-valid-marked-invalid.test.ts`
- `demo/midgard-fault-proofs/tests/support/withdrawal-mistag-emulator-v1.ts`
- `demo/midgard-fault-proofs/tests/withdrawal-mistag.test.ts`
- `demo/midgard-sdk/src/fraud-proof/withdrawal-mistag-v1.ts`
- `docs/fault-proofs/architecture.md`
- `docs/fault-proofs/catalogue-status.md`
- `docs/fault-proofs/coverage-matrix.md`
- `docs/fault-proofs/execution-plan.md`
- `docs/fault-proofs/testing-status.md`
- `onchain/aiken/lib/midgard/fraud-proofs/withdrawal-mistag/step-03.ak`
- `docs/public_testnet_readiness.md`
- untracked `docs/fault-proofs/non-interactive-proof-thread-program-v1.md`
- untracked `docs/fault-proofs/size-plans/`
- untracked empty file `typescript`

## Raw scripts at or above 16,384 bytes

The following 50 distinct compiled bodies are over the hard transaction-size
boundary before applied parameters or transaction wrapping. Sizes are bytes of
`compiledCode` in the freshly rebuilt blueprint.

| Blueprint body | Bytes |
| --- | ---: |
| `availability_challenge.availability_challenge` | 19,927 |
| `fraud_proofs/transition_trace/accepted_transaction_v1` | 40,869 |
| `fraud_proofs/transition_trace/deposit_v1` | 26,172 |
| `fraud_proofs/validation_trace/cek_context_step_semantic_v1` | 94,268 |
| `fraud_proofs/validation_trace/cek_core_step_semantic_v1` | 68,689 |
| `fraud_proofs/validation_trace/cek_execution_selection_semantic_v1` | 45,486 |
| `fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1` | 16,796 |
| `fraud_proofs/validation_trace/phase_a_native_scripts_at_least_container_frame_payload_semantic_v1` | 16,795 |
| `fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1` | 19,501 |
| `fraud_proofs/validation_trace/phase_a_native_scripts_signature_above_last_payload_semantic_v1` | 16,963 |
| `fraud_proofs/validation_trace/phase_a_native_scripts_signature_below_first_payload_semantic_v1` | 16,923 |
| `fraud_proofs/validation_trace/phase_a_native_scripts_signature_between_payload_semantic_v1` | 17,098 |
| `fraud_proofs/validation_trace/phase_a_native_scripts_signature_empty_payload_semantic_v1` | 16,762 |
| `fraud_proofs/validation_trace/phase_a_native_scripts_signature_membership_payload_semantic_v1` | 16,850 |
| `fraud_proofs/validation_trace/phase_a_script_preconditions_item_semantic_v1` | 28,066 |
| `fraud_proofs/validation_trace/phase_a_script_preconditions_semantic_v1` | 27,841 |
| `fraud_proofs/validation_trace/resolve_inputs_finish_semantic_v1` | 28,023 |
| `fraud_proofs/validation_trace/resolve_inputs_initial_semantic_v1` | 29,163 |
| `fraud_proofs/validation_trace/resolve_inputs_membership_begin_semantic_v1` | 31,141 |
| `fraud_proofs/validation_trace/resolve_inputs_membership_finalize_semantic_v1` | 34,586 |
| `fraud_proofs/validation_trace/resolve_inputs_membership_step_semantic_v1` | 72,039 |
| `fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1` | 30,319 |
| `fraud_proofs/validation_trace/script_sources_non_output_semantic_v1` | 115,590 |
| `fraud_proofs/validation_trace/script_sources_output_proof_begin_semantic_v1` | 37,945 |
| `fraud_proofs/validation_trace/script_sources_output_proof_finalize_semantic_v1` | 47,187 |
| `fraud_proofs/validation_trace/script_sources_output_proof_finish_semantic_v1` | 36,546 |
| `fraud_proofs/validation_trace/script_sources_output_proof_step_semantic_v1` | 82,309 |
| `fraud_proofs/validation_trace/script_sources_stage_eight_finish_semantic_v1` | 27,113 |
| `fraud_proofs/validation_trace/script_sources_stage_eight_purpose_semantic_v1` | 28,166 |
| `fraud_proofs/validation_trace/script_sources_stage_eleven_finish_semantic_v1` | 27,044 |
| `fraud_proofs/validation_trace/script_sources_stage_eleven_source_semantic_v1` | 28,894 |
| `fraud_proofs/validation_trace/script_sources_stage_one_finish_semantic_v1` | 31,826 |
| `fraud_proofs/validation_trace/script_sources_stage_one_redeemer_semantic_v1` | 87,545 |
| `fraud_proofs/validation_trace/script_sources_stage_seven_finish_semantic_v1` | 29,390 |
| `fraud_proofs/validation_trace/script_sources_stage_seven_observer_semantic_v1` | 33,961 |
| `fraud_proofs/validation_trace/script_sources_stage_seven_receive_semantic_v1` | 29,566 |
| `fraud_proofs/validation_trace/script_sources_stage_ten_match_semantic_v1` | 82,956 |
| `fraud_proofs/validation_trace/script_sources_stage_ten_mismatch_semantic_v1` | 83,005 |
| `fraud_proofs/validation_trace/script_sources_stage_ten_missing_semantic_v1` | 26,865 |
| `fraud_proofs/validation_trace/script_sources_stage_twelve_finish_semantic_v1` | 27,042 |
| `fraud_proofs/validation_trace/script_sources_stage_twelve_redeemer_semantic_v1` | 81,736 |
| `fraud_proofs/validation_trace/script_sources_stage_zero_begin_semantic_v1` | 20,004 |
| `fraud_proofs/validation_trace/value_and_mint_mint_asset_semantic_v1` | 18,550 |
| `fraud_proofs/validation_trace/value_and_mint_mint_finish_semantic_v1` | 17,859 |
| `fraud_proofs/validation_trace/value_and_mint_output_asset_semantic_v1` | 21,778 |
| `fraud_proofs/validation_trace/value_and_mint_output_descriptor_semantic_v1` | 21,161 |
| `fraud_proofs/validation_trace/value_and_mint_output_finish_semantic_v1` | 20,941 |
| `fraud_proofs/validation_trace/value_and_mint_replay_asset_semantic_v1` | 22,000 |
| `fraud_proofs/validation_trace/value_and_mint_replay_finish_semantic_v1` | 21,091 |
| `fraud_proofs/validation_trace/value_and_mint_replay_input_semantic_v1` | 21,320 |

Regenerate the table from `onchain/aiken/plutus.json` by selecting validators
whose `compiledCode` hex length divided by two is at least 16,384, collapsing
multi-purpose entrypoint suffixes, and sorting by title.

## Limit escape-hatch inventory

The baseline contains one raised positive emulator configuration:
`demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute.test.ts`
sets `maxTxSize: 262_144` and publishes oversized resolvers. Explicit
`oversized: true` call sites also exist in:

- `demo/midgard-fault-proofs/tests/resolver-proof-fit-sweep-generate-v1.test.ts`
- `demo/midgard-fault-proofs/tests/support/canonical-decodability-emulator-v1.ts`
- `demo/midgard-fault-proofs/tests/support/committed-field-shape-emulator-v1.ts`
- `demo/midgard-fault-proofs/tests/support/emulator/dispute-scenario.ts`
- `demo/midgard-fault-proofs/tests/support/input-set-uniqueness-emulator-v1.ts`
- `demo/midgard-fault-proofs/tests/support/mint-authorization-emulator-v1.ts`
- `demo/midgard-fault-proofs/tests/support/missing-native-script-tx-emulator-v1.ts`
- `demo/midgard-fault-proofs/tests/support/native-script-decoding-emulator-v1.ts`
- `demo/midgard-fault-proofs/tests/support/network-id-emulator-v1.ts`
- `demo/midgard-fault-proofs/tests/support/no-reference-input-emulator-v1.ts`
- `demo/midgard-fault-proofs/tests/support/value-not-preserved-emulator-v1.ts`
- `demo/midgard-fault-proofs/tests/support/withdrawn-reference-input-emulator-v1.ts`

No `localUPLCEval: false` call site exists in the fault-proof package. Wave 0
does not claim these paths are acceptable; the closure scan must remove them
from every positive, production-readiness, and merge-gating route or retain
them only as explicitly unpublishable negative diagnostics.
