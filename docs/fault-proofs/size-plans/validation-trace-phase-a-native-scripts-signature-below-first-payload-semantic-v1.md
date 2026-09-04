# Size-fit plan: `phase_a_native_scripts_signature_below_first_payload_semantic_v1`

Reads with [00-primer.md](00-primer.md). Shared fix defined in
[validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md](validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md)
§2 and §4 (PA-CARRY, PA-UNDECODED).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/phase_a_native_scripts_signature_below_first_payload_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/phase-a-native-scripts-signature-below-first-payload-semantic-v1.ak` |
| Raw size | **16,923 bytes** |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` (2) |
| Phase / indices | `PhaseANativeScripts` (resolver 5), semantic index 10, global 20 |
| Machine stage | `stage == 3`, `SignerSetProofV1.SignerBelowFirstProof { peaks, first_signer_hash, siblings }` |
| Library entry point | `verify_phase_a_native_signature_below_first_payload_semantics_v1(pre, transition, chunk_proof, next_chunk_proof, peaks, first_signer_hash, siblings)`; closure: `compare(key_hash, first_signer_hash) == Less && signer_membership_is_valid(first_signer_hash, …, 0, siblings)` → `Some(False)` |
| Redeemer / auxiliary | `VerifyToken { …, signer_proof }`; `NativeScriptTokenWitness` `[3, 3]` |
| Rejection reasons | `WitnessNativeScriptMalformed` |
| Role / deployment entry today | none / none |

## 2. Why it is this size

Identical reachable set to above-last (one membership opening at index 0
instead of `signer_count - 1`; 40 bytes smaller). Dominators as anchor §2.

| Build | Raw bytes |
| --- | ---: |
| baseline | 16,923 |
| E1 PA-CARRY | 14,864 |
| E1c PA-CARRY + PA-UNDECODED | **12,149** |

## 3. Options considered

Prune chosen; yield split, chaining and redesign rejected for the anchor §3
reasons (3.3% over).

## 4. Chosen design

Anchor §4: `verify_phase_a_native_signature_payload_with_v1` →
`authenticated_phase_a_native_payload_window_v1(…, 3, 3)`. Closure unchanged.
No ABI delta. Security: anchor §4.3.

## 5. Size and budget projection

| Script | Today | Projected | Method |
| --- | ---: | ---: | --- |
| `…signature_below_first_payload_semantic_v1.main.spend` | 16,923 | **12,149** (applied ≈12,222) | measured, build E1c |

Fee band / referenced bytes / ExUnits: anchor §5.

## 6. Off-chain work

Anchor §6; entry
`validationTraceDisputePhaseANativeScriptsSignatureBelowFirstPayloadSemantic`
(semantic index 10). Nothing exists today.

## 7. Emulator scenario tests

Case `buildPhaseANativeSignatureBelowFirstFixture` in the phase-A signature
test file: `sig <key>` with `key` below the smallest required signer;
publication fit without `oversized`; lifecycle; valid-block negative;
cancel/resume; maximum shape with `first_signer_hash` opened at index 0 of a
maximal frontier.

## 8. Aiken tests

`signature_below_first_wire_layout_is_pinned`,
`signature_below_first_validator_refuses_an_above_last_step` (and the other
kinds), `prepare_routes_signature_below_first_to_slot_ten` in
`phase-a-split-v1.test.ak`; anchor §8 library property.

## 9. Verification commands

Anchor §9 with pattern `phase_a_native_scripts_signature_below_first`;
expected ≤ 15,000 (measured 12,149).

## 10. Ordering and dependencies

Same regeneration as the anchor plan.

## 11. Risks

Anchor §11. PA-CARRY-only fallback 14,864.
