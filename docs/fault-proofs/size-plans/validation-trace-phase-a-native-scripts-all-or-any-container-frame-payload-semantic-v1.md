# Size-fit plan: `phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1`

Reads with [00-primer.md](00-primer.md). Shared fix defined in
[validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md](validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md)
§2 and §4 (PA-CARRY, PA-UNDECODED). This plan also covers two **borderline
siblings** (§12): `phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1`
(16,325) and `phase_a_native_scripts_timelock_payload_semantic_v1` (16,229).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/phase-a-native-scripts-all-or-any-container-frame-payload-semantic-v1.ak` |
| Raw size | **16,796 bytes** |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` (2) |
| Phase / indices | `PhaseANativeScripts` (resolver 5), semantic index 3, global 13 |
| Machine stage | `stage ∈ {4, 5}` (`all` / `any` payload after a token head with `tag ∈ {1, 2}`), `child_count > 0` |
| Library entry point | `verify_phase_a_native_all_or_any_container_frame_payload_semantics_v1(pre, transition, chunk_proof, next_chunk_proof)` → `authenticated_phase_a_native_all_or_any_payload_v1` → `native_script_scan_v1.all_or_any_payload_at_v1(bytes, offset, cursor, stage - 3)` → `phase_a_native_container_frame_payload_successor_is_exact_v1` (`frame_for_token_v1`, `hash_frame_v1`, depth limit) |
| Redeemer / auxiliary | `VerifyToken { input_index, output_index, transition, chunk_proof, next_chunk_proof }`; auxiliary `NativeScriptTokenWitness { …, signer_proof: NoSignerSetProof }` |
| Rejection reasons | `WitnessNativeScriptMalformed` (`reject_invalid_field_type`), `WitnessNativeScriptDepthLimit` (`reject_native_script_depth`) |
| Role / deployment entry today | none / none |

## 2. Why it is this size

Anchor §2 floor plus `all_or_any_payload_at_v1` (probe `p10` 1,154),
`frame_for_token_v1` + `hash_frame_v1` and a second `rejected_successor_is_exact`
arm (depth). No signer helpers are reached.

| Build | container-frame | empty-container (borderline) | timelock (borderline) |
| --- | ---: | ---: | ---: |
| baseline | 16,796 | 16,325 | 16,229 |
| E1 PA-CARRY | 14,704 | 14,253 | 14,157 |
| E1c PA-CARRY + PA-UNDECODED | **11,954** | **11,452** | 14,157 (keeps the decoded window) |

## 3. Options considered

Prune chosen (2.5% over; borderline siblings 0.4% under the limit and over
once applied and wrapped). Yield split / chaining / redesign rejected (anchor
§3).

## 4. Chosen design

Anchor §4: `authenticated_phase_a_native_all_or_any_payload_v1` binds through
`authenticated_phase_a_native_payload_window_v1(…, 4, 5)` and returns the
token; both the container-frame and empty-container callers keep their exact
`child_count` guards. No ABI delta. Security: anchor §4.3; the successor
(`stage: 1`, new `stack_root`, `stack_depth + 1`) is a pure function of the
token and the bound control.

## 5. Size and budget projection

| Script | Today | Projected | Method |
| --- | ---: | ---: | --- |
| `…all_or_any_container_frame_payload_semantic_v1.main.spend` | 16,796 | **11,954** (applied ≈12,027) | measured, build E1c |

Fee band / ExUnits: anchor §5.

## 6. Off-chain work

Anchor §6; entry
`validationTraceDisputePhaseANativeScriptsAllOrAnyContainerFramePayloadSemantic`
(semantic index 3). Nothing exists today.

## 7. Emulator scenario tests

New file `submit-init-emulator-validation-dispute-phase-a-container.test.ts`
with fixture `buildPhaseANativeAllOrAnyContainerFixture({ kind: "all" | "any"
})`: witness script `all [sig a, sig b]` / `any [...]` whose frontier step is
the container payload; publication fit without `oversized`; lifecycle to
award; valid-block negative (honest frame successor claimed → challenger
refused); cancel/resume; maximum shape: nesting at
`native_script_scan_v1.max_native_script_depth - 1` so the depth-limit
rejection arm is exercised at exactly the boundary, and a payload straddling
a 4,095-byte chunk.

## 8. Aiken tests

`all_or_any_container_frame_wire_layout_is_pinned`,
`all_or_any_container_frame_validator_refuses_an_empty_container_step`,
`…_refuses_an_at_least_step`, `prepare_routes_all_or_any_container_frame_to_slot_three`
in `phase-a-split-v1.test.ak`; library property
`phase_a_payload_binding_agrees_with_full_binding` instantiated for stages 4
and 5.

## 9. Verification commands

Anchor §9 with pattern `phase_a_native_scripts_all_or_any`; expected both
`all_or_any_*` bodies ≤ 15,000 (measured 11,954 / 11,452).

## 10. Ordering and dependencies

Same regeneration as the anchor plan.

## 11. Risks

Anchor §11. PA-CARRY-only fallbacks: 14,704 / 14,253.

## 12. Borderline siblings covered by this plan

### 12.1 `phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1`

| Field | Value |
| --- | --- |
| Title / file | `…all_or_any_empty_container_payload_semantic_v1.main.spend` / `phase-a-native-scripts-all-or-any-empty-container-payload-semantic-v1.ak` |
| Raw size | **16,325** (59 bytes under the limit; fails once applied +73 and wrapped +276) |
| Semantic index / stage | 4 (global 14); `stage ∈ {4, 5}`, `child_count == 0` (`empty_container_result_v1`: `all [] = True`, `any [] = False`) |
| Entry point | `verify_phase_a_native_all_or_any_empty_container_payload_semantics_v1` → same window and decoder as §1, successor `stage: 2`, `result` set |
| Rejections | `WitnessNativeScriptMalformed` |
| Measured | 16,325 → 14,253 (PA-CARRY) → **11,452** (PA-UNDECODED) |
| Design | identical to §4 (shares `authenticated_phase_a_native_all_or_any_payload_v1`) |
| Off-chain | entry `validationTraceDisputePhaseANativeScriptsAllOrAnyEmptyContainerPayloadSemantic` |
| Tests | fixture `buildPhaseANativeEmptyContainerFixture({ kind })` with `all []` (true) and `any []` (false); Aiken `all_or_any_empty_container_wire_layout_is_pinned`, refuses the container-frame step |

### 12.2 `phase_a_native_scripts_timelock_payload_semantic_v1`

| Field | Value |
| --- | --- |
| Title / file | `…timelock_payload_semantic_v1.main.spend` / `phase-a-native-scripts-timelock-payload-semantic-v1.ak` |
| Raw size | **16,229** (155 under the limit; fails applied + wrapped) |
| Semantic index / stage | 7 (global 17); `stage ∈ {7, 8}` (`after` / `before`) |
| Entry point | `verify_phase_a_native_timelock_payload_semantics_v1` → `authenticated_phase_a_native_window_v1` → `timelock_payload_at_v1(bytes, offset, cursor, stage - 3)`; verdict reads `authenticated.verified.tx_compact.body.validity_interval_start/end` |
| Rejections | `WitnessNativeScriptMalformed` |
| Measured | 16,229 → **14,157** (PA-CARRY). PA-UNDECODED does not apply: the verdict needs the decoded validity interval, so `authenticated_phase_a_native_window_v1` keeps `verify_native_tx_proof_source_v1` and binds with `phase_a_native_control_is_bound_carried` |
| Design | PA-CARRY only (anchor §4.1); 843 bytes under the target. If regeneration erodes that margin, the next prune is a `verify_native_tx_proof_source_v1` variant that skips `decode_native_tx_field_preimage_lengths_v1` (unused by this step), estimated ≈0.5 KB |
| Off-chain | entry `validationTraceDisputePhaseANativeScriptsTimelockPayloadSemantic` |
| Tests | fixture `buildPhaseANativeTimelockFixture({ kind: "after" | "before", satisfied })`: validity interval boundary exactly at the slot (`>=` for after, `<=` for before); valid-block negative uses an unset interval (`-1`) which must evaluate `False`; Aiken `timelock_wire_layout_is_pinned`, `timelock_validator_refuses_a_container_step`, plus `phase_a_carried_binding_agrees_with_full_binding` for stages 7/8 |
