# Size-fit plan: `resolve_inputs_initial_semantic_v1`

Cites [00-primer.md](00-primer.md); shares `resolve-inputs-control-v1.ak`
(see [membership-step](validation-trace-resolve-inputs-membership-step-semantic-v1.md) §4.1).

## 1. Identity

| Field                                      | Value                                                                                                                                                                                                                                                                    |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Blueprint title                            | `fraud_proofs/validation_trace/resolve_inputs_initial_semantic_v1.main.spend`                                                                                                                                                                                            |
| File                                       | `onchain/aiken/validators/fraud-proofs/validation-trace/resolve-inputs-initial-semantic-v1.ak`                                                                                                                                                                           |
| Raw size                                   | 29,163 bytes (1.78× limit; 1.94× target)                                                                                                                                                                                                                                 |
| Applied parameters                         | `award_script_hash`, `computation_thread_policy_id`                                                                                                                                                                                                                      |
| Phase / resolver / semantic / global index | `ResolveInputs` / 7 / 0 of 6 / 26                                                                                                                                                                                                                                        |
| Auxiliary                                  | `NoAuxiliaryWitness`                                                                                                                                                                                                                                                     |
| Library entry                              | `verify_resolve_inputs_initial_semantics_v1` → `verify_native_tx_proof_source_v1`, `decode_validation_context`, `resolve_inputs_control_is_bound`, `cursor == 0`, `validity_interval_contains_slot` → successor with `cursor = 1` or `reject_validity_interval_mismatch` |
| Role / deployment entry today              | none / none                                                                                                                                                                                                                                                              |

## 2. Why it is this size

| Probe                                 |           Bytes | Isolates                                                                                                                                 |
| ------------------------------------- | --------------: | ---------------------------------------------------------------------------------------------------------------------------------------- |
| `d_initial`                           |           3,268 | dispatcher shell                                                                                                                         |
| `p_pred_initial`                      |          26,454 | predicate                                                                                                                                |
| `p_source_verify`                     |           3,720 | `verify_native_tx_proof_source_v1` + `decode_validation_context` + `validity_interval_contains_slot`                                     |
| `p_control_parse` / `p_control_bound` | 18,598 / 22,741 | generic parse / binding (unreachable `pending: Some` decoder + `encode_control_v1` through `encode_optional_resolve_input_output_proof`) |
| `p_control_bound_narrow`              |           2,990 | narrowed                                                                                                                                 |
| `p_rejected_exact`                    |           1,567 | rejection successor                                                                                                                      |
| `p_narrow_initial`                    |           6,103 | narrowed predicate                                                                                                                       |
| `v_initial`                           |           8,289 | full narrowed validator, measured                                                                                                        |

`aiken check`: `resolve_inputs_checks_validity_before_ledger_lookups`
4,141,254 mem / 1,617,140,255 cpu; `..._rejects_an_invalid_validity_interval_exactly`
3,470,822 / 1,310,381,630 (fixture-inclusive).

## 3. Options considered

Prune (chosen): `cursor == 0 && pending == None` is required, so the whole
output-proof codec is unreachable in honest and dishonest executions alike;
narrowing the control codec removes ~21 KB exactly. Yield split unnecessary
(8,289). Chaining/redesign rejected.

## 4. Chosen design

Same validator, title, parameters, redeemer (`VerifyInitial { input_index, output_index, transition }`), datum.
Predicate: `control = control_no_pending_from_witness(w)`;
`Pair(verified, _) = verify_native_tx_proof_source_v1(pre.transaction_id, control.compact_cbor, control.witness_set_compact_cbor, control.field_preimage_lengths_cbor)`;
`context = decode_validation_context(control.context_cbor)`;
`verified.version == 1`, `control_no_pending_is_bound(pre, transition, control)`, `control.cursor == 0`;
if `validity_interval_contains_slot(verified.tx_compact.body, context.block_slot)` then
`claimed_successor.phase == ResolveInputs && work_root == hash_work_witness(ResolveInputs, pc + 1, encode_control_no_pending({..control, cursor: 1}))`
else `rejected_successor_is_exact(pre, claimed_successor, reject_validity_interval_mismatch)`.
Every clause is the monolith's (the `pending == None` clause moves to the
parser); `encode_control_no_pending` is byte-identical to
`encode_resolve_inputs_witness(..., 1, ..., None, ...)`.

**Handshake and security argument.** No yield, no role NFT, no withdrawal,
no new parameter; the primer's handshake items are vacuous: _dispatch
uniqueness / role authentication / omission_ do not apply (one spend
validator, semantic index 0 of `resolve_inputs_v1`); _cross-arm
substitution_ is unchanged (`cursor == 0` and the fail-closed no-pending
parser exclude every other arm's witness); _output-state re-derivation_ is
`continue_winning` plus the recomputed `work_root` / exact rejection
successor as today; _what an attacker gains:_ nothing — clause-for-clause
equality with the monolith, codec byte-identity proven in §8.

## 5. Size and budget projection

| Script                                                        | Basis                                         | Projected raw bytes |
| ------------------------------------------------------------- | --------------------------------------------- | ------------------: |
| `resolve_inputs_initial_semantic_v1.main.spend` (only script) | `v_initial` measured, full narrowed validator |           **8,289** |

Referenced bytes per transaction: 8,289 (no yields) → tier 0, 8,289 × 15 ≈
124,000 lovelace (≈ 0.12 ADA), against 29,163 today (tier 1, ≈ 448,000
lovelace, ≈ 0.45 ADA). Aggregate ExUnits: one execution, strictly below
today's (no output-proof codec runs; `verify_native_tx_proof_source_v1` and
`decode_validation_context` are unchanged); recorded on the first emulator
lifecycle (§7) against the 13,200,000 basis.

## 6. Off-chain work

Deployment entry `validationTraceDisputeResolveInputsInitialSemantic`
(`…ENTRIES_V1[0]`), resolver-7 submit route (semantic 0 carries no
auxiliary fields: `semanticActionFieldsV1` already returns `base` for
resolver 7 indices 0 and 1), funding row
`validation-dispute.semantic.resolve-inputs.initial` (8,289 bytes). No
contracts.ts, role, manifest, or codec changes. **Nothing exists today for
this contract beyond the `contracts.ts` title: no deployment entry, no
submit route, no funding row, no role.** The deployment-entry table,
resolver-index constant and
`requireValidationResolveInputsSemanticReferenceScriptUtxo` are created
once by the step plan §6; this plan contributes row 0, its funding row and
its inspection-fixture entry.

## 7. Emulator scenario tests

`demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-resolve-inputs.test.ts`
(shared resolve-inputs journey file; fixture and harness per the step plan
§7): `publishes the resolve-inputs initial resolver inside the L1 envelope`
— `publishPlainReferenceScriptUtxo` **without `oversized`**, under
`withRealL1MaxTxSize`, asserting `l1ByteMargin > 0` and
`assertReferenceScriptRawBodiesFitL1EnvelopeV1`; positive lifecycle through
`submitValidationDisputeAward` with `disputedStep: "initial"` (the first `resolveInputs` state of the honest
trace, `disputedLowIndex = states.findIndex(phase === "resolveInputs")`);
rejection lifecycle: transaction whose `validity_interval_start` exceeds the
block slot → `reject_validity_interval_mismatch` terminal (the
`validation-machine.ts` branch `RejectCodes.ValidityIntervalMismatch`);
valid-block negative: challenger claims the rejection on an in-range
transaction → refused at the same frontier; cancel path. Maximum shape: the
largest compact transaction the proof source admits (both validity bounds
set).

## 8. Aiken tests

Shared codec properties (begin plan §8); `validation-machine-v1.test.ak`:
narrowed predicate agrees with `verify_one_step_evidence` on
`resolve_inputs_checks_validity_before_ledger_lookups` and
`resolve_inputs_rejects_an_invalid_validity_interval_exactly`; negatives:
`cursor != 0`, pending-bearing witness, wrong `transaction_id`, successor
with `cursor = 2`.

## 9. Verification commands

As the step plan §9; expect `resolve_inputs_initial_semantic_v1.main.spend` ≤ 15,000 (≈ 8,300).

## 10. Ordering and dependencies

Shares the narrowed codec with the five siblings; single regeneration; no
yield dependencies.

## 11. Risks

None material: 6.7 KB headroom, no ABI change.
