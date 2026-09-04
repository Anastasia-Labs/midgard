# Size-fit plan: `phase_a_native_scripts_signature_membership_payload_semantic_v1`

Reads with [00-primer.md](00-primer.md). Shared fix defined in
[validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md](validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md)
§2 and §4 (PA-CARRY, PA-UNDECODED).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/phase_a_native_scripts_signature_membership_payload_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/phase-a-native-scripts-signature-membership-payload-semantic-v1.ak` |
| Raw size | **16,850 bytes** |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` (2) |
| Phase / indices | `PhaseANativeScripts` (resolver 5), semantic index 8, global 18 |
| Machine stage | `stage == 3`, `SignerSetProofV1.SignerMembershipProof { peaks, signer_index, siblings }` — the only payload resolver whose verdict is `Some(True)` |
| Library entry point | `verify_phase_a_native_signature_membership_payload_semantics_v1(pre, transition, chunk_proof, next_chunk_proof, peaks, signer_index, siblings)`; closure: `signer_membership_is_valid(key_hash, signer_count, frontier_commitment, peaks, signer_index, siblings)` → `Some(True)` |
| Redeemer / auxiliary | `VerifyToken { …, signer_proof }`; `NativeScriptTokenWitness` `[3, 3]` |
| Rejection reasons | `WitnessNativeScriptMalformed` |
| Role / deployment entry today | none / none |

## 2. Why it is this size

Anchor §2 reachable set with a single membership opening and no
`bytearray.compare`. Dominators: full binding ≈5.0 KB, proof-source decode
≈2.2 KB, chunk window ≈2.6 KB, successor ≈2.2 KB, shell ≈3.3 KB.

| Build | Raw bytes |
| --- | ---: |
| baseline | 16,850 |
| E1 PA-CARRY | 14,792 |
| E1c PA-CARRY + PA-UNDECODED | **12,079** |

## 3. Options considered

Prune chosen (2.8% over). Yield split / chaining / redesign rejected as in the
anchor §3.

## 4. Chosen design

Anchor §4 (`authenticated_phase_a_native_payload_window_v1(…, 3, 3)`).
Closure unchanged. No ABI delta. Security: anchor §4.3; the positive verdict
depends only on `control.signer_count`/`signer_peaks` (bound by the
re-encode equality) and the redeemer's `peaks`/`siblings`, which
`signer_frontier_matches` ties to the control's frontier commitment.

## 5. Size and budget projection

| Script | Today | Projected | Method |
| --- | ---: | ---: | --- |
| `…signature_membership_payload_semantic_v1.main.spend` | 16,850 | **12,079** (applied ≈12,152) | measured, build E1c |

Fee band / ExUnits: anchor §5.

## 6. Off-chain work

Anchor §6; entry
`validationTraceDisputePhaseANativeScriptsSignatureMembershipPayloadSemantic`
(semantic index 8). Nothing exists today.

## 7. Emulator scenario tests

Case `buildPhaseANativeSignatureMembershipFixture`: `sig <key>` with `key` a
member of the required-signer set; the forced dispute claims `result == 0`
(operator says unsigned) and the honest step proves membership. Publication
fit without `oversized`; lifecycle; valid-block negative (honest claim of
`result == 1` cannot be disputed — assert the resolver refuses the challenger's
mismatching successor); cancel/resume; maximum shape with `signer_index ==
signer_count - 1` at maximal frontier height and a two-chunk window.

## 8. Aiken tests

`signature_membership_wire_layout_is_pinned`,
`signature_membership_validator_refuses_an_empty_set_step` (and the other
kinds), `prepare_routes_signature_membership_to_slot_eight`; anchor §8
library property. Extend the existing
`phase_a_native_scripts_resolves_a_nested_signature_one_step_at_a_time` in
`validation-machine-v1.test.ak` to drive the membership entry point through
the pruned window.

## 9. Verification commands

Anchor §9 with pattern `phase_a_native_scripts_signature_membership`;
expected ≤ 15,000 (measured 12,079).

## 10. Ordering and dependencies

Same regeneration as the anchor plan.

## 11. Risks

Anchor §11. PA-CARRY-only fallback 14,792.
