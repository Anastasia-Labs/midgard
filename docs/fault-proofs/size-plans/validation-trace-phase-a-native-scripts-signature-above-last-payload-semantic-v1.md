# Size-fit plan: `phase_a_native_scripts_signature_above_last_payload_semantic_v1`

Reads with [00-primer.md](00-primer.md). Shared fix defined in
[validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md](validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md)
§2 (component probes) and §4 (PA-CARRY, PA-UNDECODED). This plan records what
is specific to the above-last resolver.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/phase_a_native_scripts_signature_above_last_payload_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/phase-a-native-scripts-signature-above-last-payload-semantic-v1.ak` |
| Raw size | **16,963 bytes** |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` (2) |
| Phase / indices | `PhaseANativeScripts` (resolver 5), semantic index 11, global 21 |
| Machine stage | `stage == 3`, `SignerSetProofV1.SignerAboveLastProof { peaks, last_signer_hash, siblings }` |
| Library entry point | `verify_phase_a_native_signature_above_last_payload_semantics_v1(pre, transition, chunk_proof, next_chunk_proof, peaks, last_signer_hash, siblings)`; closure: `signer_count > 0 && compare(last_signer_hash, key_hash) == Less && signer_membership_is_valid(last_signer_hash, …, signer_count - 1, siblings)` → `Some(False)` |
| Redeemer / auxiliary | `VerifyToken { …, signer_proof }`; `NativeScriptTokenWitness` shape `[3, 3]` |
| Rejection reasons | `WitnessNativeScriptMalformed` (`reject_invalid_field_type`) |
| Role / deployment entry today | none / none (inline attach in `submit.ts`; no phase-A roster) |

## 2. Why it is this size

Same reachable set as the between resolver minus one `signer_membership_is_valid`
call site and the second sibling list (135 bytes smaller). Dominators:
`phase_a_native_control_is_bound` ≈5.0 KB (≈1.9 KB of it the NativeScripts
continuation trio), `verify_native_tx_proof_source_v1` ≈2.2 KB,
`phase_a_native_chunk_window` ≈2.6 KB, successor ≈2.2 KB, shell ≈3.3 KB.

| Build | Raw bytes |
| --- | ---: |
| baseline | 16,963 |
| E1 PA-CARRY | 14,898 |
| E1c PA-CARRY + PA-UNDECODED | **12,185** |

## 3. Options considered

Prune (chosen; measured above). Yield split rejected (3.5% over; a second
witness parse costs more than the excess). Chaining rejected (already a
one-token step). Redesign not warranted. Same reasoning as the anchor plan §3.

## 4. Chosen design

Anchor plan §4: `verify_phase_a_native_signature_payload_with_v1` binds through
`authenticated_phase_a_native_payload_window_v1(…, 3, 3)`; the closure body is
unchanged. No new validators, no ABI delta; script hash changes, so
`phase_a_native_scripts_v1` re-applies. Security argument: anchor §4.3 — the
above-last closure reads `control.signer_count`, `control.signer_peaks` and
the token's `key_hash` only, none of which come from the decoded transaction.

## 5. Size and budget projection

| Script | Today | Projected | Method |
| --- | ---: | ---: | --- |
| `…signature_above_last_payload_semantic_v1.main.spend` | 16,963 | **12,185** (applied ≈12,258) | measured, build E1c |

Referenced bytes and fee band as anchor §5 (single 12.2 KB body, first
`minFeeRefScriptCostPerByte` tier). ExUnits bounded by today's (subset of
checks); not re-measured.

## 6. Off-chain work

Shared with the anchor plan §6: deployment entry
`validationTraceDisputePhaseANativeScriptsSignatureAboveLastPayloadSemantic`
(semantic index 11 in
`VALIDATION_PHASE_A_NATIVE_SCRIPTS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`),
the `resolverIndex === 5` submit branch, one `spendDescriptor` row in
`contract-deployment-info.ts`, no role, no codec change. Nothing exists today.

## 7. Emulator scenario tests

Add to `submit-init-emulator-validation-dispute-phase-a-signature.test.ts`
(anchor §7) a case `buildPhaseANativeSignatureAboveLastFixture`: witness
script `sig <key>` with `key` above the largest required signer of a
non-empty set; fit publication without `oversized`, lifecycle to award,
valid-block negative (`result == 0` claimed → refused), cancel/resume, maximum
shape (two-chunk window, maximal signer frontier, `siblings` at max height,
`last_signer_hash` at index `signer_count - 1`).

## 8. Aiken tests

`phase-a-split-v1.test.ak`: `signature_above_last_wire_layout_is_pinned`,
`signature_above_last_validator_refuses_a_between_step` (and the other kinds),
`prepare_routes_signature_above_last_to_slot_eleven`. Library property in the
anchor plan §8 covers the binding equivalence.

## 9. Verification commands

As anchor §9 with pattern `phase_a_native_scripts_signature_above_last`;
expected raw ≤ 15,000 (measured 12,185).

## 10. Ordering and dependencies

Lands in the same `validation-machine-v1.ak` edit and blueprint regeneration
as the anchor plan and the other payload plans; catalogue root re-pinned once.

## 11. Risks

As anchor §11. Fallback under PA-CARRY alone: 14,898 (fits the target with
102 bytes to spare — thin; PA-UNDECODED is preferred).
