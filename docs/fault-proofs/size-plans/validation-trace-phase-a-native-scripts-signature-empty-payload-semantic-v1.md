# Size-fit plan: `phase_a_native_scripts_signature_empty_payload_semantic_v1`

Reads with [00-primer.md](00-primer.md). Shared fix defined in
[validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md](validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md)
§2 and §4 (PA-CARRY, PA-UNDECODED).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/phase_a_native_scripts_signature_empty_payload_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/phase-a-native-scripts-signature-empty-payload-semantic-v1.ak` |
| Raw size | **16,762 bytes** |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` (2) |
| Phase / indices | `PhaseANativeScripts` (resolver 5), semantic index 9, global 19 |
| Machine stage | `stage == 3`, `SignerSetProofV1.EmptySignerSetProof { peaks }` |
| Library entry point | `verify_phase_a_native_signature_empty_payload_semantics_v1(pre, transition, chunk_proof, next_chunk_proof, peaks)`; closure: `control.signer_count == 0 && signer_frontier_matches(0, frontier_commitment, peaks)` → `Some(False)` |
| Redeemer / auxiliary | `VerifyToken { …, signer_proof }`; `NativeScriptTokenWitness` `[3, 3]` |
| Rejection reasons | `WitnessNativeScriptMalformed` |
| Role / deployment entry today | none / none |

## 2. Why it is this size

Smallest of the five signature resolvers: no `verify_membership`, only
`signer_frontier_matches` (probe `p15` 922 bytes vs `p14` 1,543). The
remaining 16.7 KB is the shared floor described in the anchor §2.

| Build | Raw bytes |
| --- | ---: |
| baseline | 16,762 |
| E1 PA-CARRY | 14,699 |
| E1c PA-CARRY + PA-UNDECODED | **11,986** |

## 3. Options considered

Prune chosen (2.3% over). Yield split / chaining / redesign rejected (anchor
§3).

## 4. Chosen design

Anchor §4 (`authenticated_phase_a_native_payload_window_v1(…, 3, 3)`).
Closure unchanged. No ABI delta. Security: anchor §4.3; the empty-set verdict
depends on `control.signer_count == 0` and the frontier match, both bound by
the canonical re-encode of the control.

## 5. Size and budget projection

| Script | Today | Projected | Method |
| --- | ---: | ---: | --- |
| `…signature_empty_payload_semantic_v1.main.spend` | 16,762 | **11,986** (applied ≈12,059) | measured, build E1c |

Fee band / ExUnits: anchor §5.

## 6. Off-chain work

Anchor §6; entry
`validationTraceDisputePhaseANativeScriptsSignatureEmptyPayloadSemantic`
(semantic index 9). Nothing exists today.

## 7. Emulator scenario tests

Case `buildPhaseANativeSignatureEmptySetFixture`: a transaction with no
required signers (`signer_count == 0`) whose witness script is `sig <key>`;
operator claims `result == 1`. Publication fit without `oversized`;
lifecycle; valid-block negative (honest `result == 0` claim → resolver
refuses the challenger); cancel/resume; maximum shape is the two-chunk window
(the signer proof is minimal by construction).

## 8. Aiken tests

`signature_empty_wire_layout_is_pinned`,
`signature_empty_validator_refuses_a_membership_step` (and the other kinds),
`prepare_routes_signature_empty_to_slot_nine`; anchor §8 library property,
plus `phase_a_payload_binding_refuses_nonzero_signer_count_for_empty_proof`.

## 9. Verification commands

Anchor §9 with pattern `phase_a_native_scripts_signature_empty`; expected
≤ 15,000 (measured 11,986).

## 10. Ordering and dependencies

Same regeneration as the anchor plan.

## 11. Risks

Anchor §11. PA-CARRY-only fallback 14,699.
