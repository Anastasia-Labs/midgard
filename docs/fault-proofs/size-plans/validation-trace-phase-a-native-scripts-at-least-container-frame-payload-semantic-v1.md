# Size-fit plan: `phase_a_native_scripts_at_least_container_frame_payload_semantic_v1`

Reads with [00-primer.md](00-primer.md). Shared fix defined in
[validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md](validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md)
§2 and §4 (PA-CARRY, PA-UNDECODED). This plan also covers two **borderline
siblings** (§12): `phase_a_native_scripts_at_least_empty_container_payload_semantic_v1`
(16,332) and `phase_a_native_scripts_token_head_semantic_v1` (16,193).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/phase_a_native_scripts_at_least_container_frame_payload_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/phase-a-native-scripts-at-least-container-frame-payload-semantic-v1.ak` |
| Raw size | **16,795 bytes** |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` (2) |
| Phase / indices | `PhaseANativeScripts` (resolver 5), semantic index 5, global 15 |
| Machine stage | `stage == 6` (`at_least` payload: threshold + child count), `child_count > 0` |
| Library entry point | `verify_phase_a_native_at_least_container_frame_payload_semantics_v1(pre, transition, chunk_proof, next_chunk_proof)` → `authenticated_phase_a_native_at_least_payload_v1` → `native_script_scan_v1.at_least_payload_at_v1(bytes, offset, cursor)` → `phase_a_native_container_frame_payload_successor_is_exact_v1` |
| Redeemer / auxiliary | `VerifyPayload { input_index, output_index, transition, chunk_proof, next_chunk_proof }` (constructor name differs from the all-or-any file's `VerifyToken`; same wire shape); auxiliary `NativeScriptTokenWitness { …, NoSignerSetProof }` |
| Rejection reasons | `WitnessNativeScriptMalformed`, `WitnessNativeScriptDepthLimit` |
| Role / deployment entry today | none / none |

## 2. Why it is this size

Anchor §2 floor plus `at_least_payload_at_v1` (probe `p11` 1,183) and the
frame successor. One byte from the all-or-any container-frame resolver.

| Build | at-least frame | at-least empty (borderline) | token-head (borderline) |
| --- | ---: | ---: | ---: |
| baseline | 16,795 | 16,332 | 16,193 |
| E1 PA-CARRY | 14,709 | 14,259 | 14,124 |
| E1c PA-CARRY + PA-UNDECODED | **11,930** | **11,468** | **11,243** |

## 3. Options considered

Prune chosen (2.5% over). Yield split / chaining / redesign rejected (anchor
§3).

## 4. Chosen design

Anchor §4: `authenticated_phase_a_native_at_least_payload_v1` binds through
`authenticated_phase_a_native_payload_window_v1(…, 6, 6)`. No ABI delta.
Security: anchor §4.3.

## 5. Size and budget projection

| Script | Today | Projected | Method |
| --- | ---: | ---: | --- |
| `…at_least_container_frame_payload_semantic_v1.main.spend` | 16,795 | **11,930** (applied ≈12,003) | measured, build E1c |

Fee band / ExUnits: anchor §5.

## 6. Off-chain work

Anchor §6; entry
`validationTraceDisputePhaseANativeScriptsAtLeastContainerFramePayloadSemantic`
(semantic index 5). Nothing exists today.

## 7. Emulator scenario tests

In `submit-init-emulator-validation-dispute-phase-a-container.test.ts`:
fixture `buildPhaseANativeAtLeastContainerFixture({ threshold, children })`
(`at_least 2 [sig a, sig b, sig c]`); publication fit without `oversized`;
lifecycle; valid-block negative; cancel/resume; maximum shape: threshold and
child count at their canonical maxima, nesting one below
`max_native_script_depth`, two-chunk window.

## 8. Aiken tests

`at_least_container_frame_wire_layout_is_pinned`,
`at_least_container_frame_validator_refuses_an_all_or_any_step`,
`prepare_routes_at_least_container_frame_to_slot_five`; anchor §8 property at
stage 6.

## 9. Verification commands

Anchor §9 with pattern `phase_a_native_scripts_at_least`; expected both
bodies ≤ 15,000 (measured 11,930 / 11,468).

## 10. Ordering and dependencies

Same regeneration as the anchor plan.

## 11. Risks

Anchor §11. PA-CARRY-only fallbacks: 14,709 / 14,259.

## 12. Borderline siblings covered by this plan

### 12.1 `phase_a_native_scripts_at_least_empty_container_payload_semantic_v1`

| Field | Value |
| --- | --- |
| Title / file | `…at_least_empty_container_payload_semantic_v1.main.spend` / `phase-a-native-scripts-at-least-empty-container-payload-semantic-v1.ak` |
| Raw size | **16,332** (52 under the limit; fails applied + wrapped) |
| Semantic index / stage | 6 (global 16); `stage == 6`, `child_count == 0` (`empty_container_result_v1`: `at_least n [] = (n <= 0)`) |
| Entry point | `verify_phase_a_native_at_least_empty_container_payload_semantics_v1` → same window/decoder as §1, successor `stage: 2`, `result` set |
| Measured | 16,332 → 14,259 → **11,468** |
| Design / off-chain | identical to §4; entry `validationTraceDisputePhaseANativeScriptsAtLeastEmptyContainerPayloadSemantic` |
| Tests | fixture `buildPhaseANativeEmptyContainerFixture({ kind: "at_least", threshold })` for thresholds 0 (true) and 1 (false); Aiken `at_least_empty_container_wire_layout_is_pinned`, refuses the container-frame step |

### 12.2 `phase_a_native_scripts_token_head_semantic_v1`

| Field | Value |
| --- | --- |
| Title / file | `…token_head_semantic_v1.main.spend` / `phase-a-native-scripts-token-head-semantic-v1.ak` |
| Raw size | **16,193** (191 under the limit; fails applied + wrapped) |
| Semantic index / stage | 2 (global 12); `stage == 1`; decodes the token head (`token_head_at_v1`, `token_head_is_well_formed_v1`) and moves to `stage: head.tag + 3` |
| Entry point | `verify_phase_a_native_token_head_semantics_v1(pre, transition, chunk_proof, next_chunk_proof)` → `verify_phase_a_native_token_head_scan_v1` → `authenticated_phase_a_native_token_head_v1` → `authenticated_phase_a_native_window_v1` |
| Rejections | `WitnessNativeScriptMalformed`, `WitnessNativeScriptNodeLimit` (`reject_native_script_node_count`) |
| Measured | 16,193 → 14,124 (PA-CARRY) → **11,243** (PA-UNDECODED with window `1..1`) |
| Design | `verify_phase_a_native_token_head_scan_v1` re-pointed to `authenticated_phase_a_native_payload_window_v1(…, 1, 1)`; `AuthenticatedNativeScriptTokenHeadV1` (carrying `verified`) is then reached only by the unused generic token scans and may be deleted. The stage-1 binding admits `result == -1`, `item_length > 0`, `cursor <= item_length` exactly as the full binding does for stage 1 |
| Off-chain | entry `validationTraceDisputePhaseANativeScriptsTokenHeadSemantic` |
| Tests | fixture `buildPhaseANativeTokenHeadFixture({ tag })` for all six tags plus a malformed head (rejection) and `node_count == max_native_script_nodes` (node-limit rejection); Aiken `token_head_wire_layout_is_pinned`, `token_head_validator_refuses_a_payload_step`, `phase_a_payload_binding_agrees_with_full_binding` at stage 1 |
