# Size-fit plan: `resolve_inputs_finish_semantic_v1`

Cites [00-primer.md](00-primer.md); shares `resolve-inputs-control-v1.ak`
(see [membership-step](validation-trace-resolve-inputs-membership-step-semantic-v1.md) §4.1).

## 1. Identity

| Field                                      | Value                                                                                                                                                                                                                                               |
| ------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title                            | `fraud_proofs/validation_trace/resolve_inputs_finish_semantic_v1.main.spend`                                                                                                                                                                        |
| File                                       | `onchain/aiken/validators/fraud-proofs/validation-trace/resolve-inputs-finish-semantic-v1.ak`                                                                                                                                                       |
| Raw size                                   | 28,023 bytes (1.71× limit; 1.87× target)                                                                                                                                                                                                            |
| Applied parameters                         | `award_script_hash`, `computation_thread_policy_id`                                                                                                                                                                                                 |
| Phase / resolver / semantic / global index | `ResolveInputs` / 7 / 1 of 6 / 27                                                                                                                                                                                                                   |
| Auxiliary                                  | `NoAuxiliaryWitness`                                                                                                                                                                                                                                |
| Library entry                              | `verify_resolve_inputs_finish_semantics_v1` → `resolve_inputs_control_is_bound`, `cursor > 0`, `pending == None`, `remaining_schedule_hash == empty_resolution_schedule_hash()`, successor `ScriptSources` via `encode_script_sources_witness(...)` |
| Role / deployment entry today              | none / none                                                                                                                                                                                                                                         |

## 2. Why it is this size

| Probe                                 |           Bytes | Isolates                                                                                |
| ------------------------------------- | --------------: | --------------------------------------------------------------------------------------- |
| `d_finish`                            |           3,268 | dispatcher shell                                                                        |
| `p_pred_finish`                       |          25,127 | predicate                                                                               |
| `p_control_parse` / `p_control_bound` | 18,598 / 22,741 | generic parse / binding (unreachable `pending: Some` decoder and encoder)               |
| `p_control_bound_narrow`              |           2,990 | narrowed                                                                                |
| `p_encode_script_sources`             |           2,889 | `encode_script_sources_witness` with the empty frontiers/controls + `hash_work_witness` |
| `p_narrow_finish`                     |           5,468 | narrowed predicate                                                                      |
| `v_finish`                            |           7,695 | full narrowed validator, measured                                                       |

`aiken check`: `resolve_inputs_finalizes_into_script_source_resolution`
4,139,151 mem / 1,696,938,546 cpu (fixture-inclusive).

## 3. Options considered

Prune (chosen): `pending == None` is a predicate clause, so the output-proof
codec is unreachable; narrowing removes ~20 KB exactly. Yield split
unnecessary (7,695). Chaining/redesign rejected.

## 4. Chosen design

Same validator, title, parameters, redeemer (`VerifyFinish`), datum.
Predicate: `control = control_no_pending_from_witness(w)`;
`control_no_pending_is_bound(pre, transition, control)`; `control.cursor > 0`;
`control.remaining_schedule_hash == empty_resolution_schedule_hash()`;
`claimed_successor.phase == ScriptSources`;
`work_root == hash_work_witness(ScriptSources, pc + 1, encode_script_sources_witness(control.compact_cbor, control.witness_set_compact_cbor, control.field_preimage_lengths_cbor, control.context_cbor, control.cursor - 1, control.accumulator, control.signer_count, control.signer_frontier_commitment, empty_frontier(), 0, 0, empty_frontier(), 0, empty_frontier(), 0, initial_resolution_accumulator(), empty_resolution_schedule_hash(), 0, 0, empty_frontier(), 0, 0, empty_frontier(), 0, empty_receive_purpose_scan_control(), 0, 0, empty_observer_purpose_scan_control(), empty_mint_fold_control(), control.resolution_schedule_hash))` — the monolith's call verbatim.

**Handshake and security argument.** No yield, no role NFT, no withdrawal,
no new parameter; the primer's handshake items are vacuous: _dispatch
uniqueness / role authentication / omission_ do not apply (one spend
validator, semantic index 1 of `resolve_inputs_v1`); _cross-arm
substitution_ is unchanged (the fail-closed no-pending parser and
`remaining_schedule_hash == empty_resolution_schedule_hash()` exclude every
other arm's witness); _output-state re-derivation_ is `continue_winning`
plus the recomputed `ScriptSources` `work_root` with the unchanged
`encode_script_sources_witness`; _what an attacker gains:_ nothing —
exactness by clause equality with the monolith and the codec property tests
of §8.

## 5. Size and budget projection

| Script                                                       | Basis                                        | Projected raw bytes |
| ------------------------------------------------------------ | -------------------------------------------- | ------------------: |
| `resolve_inputs_finish_semantic_v1.main.spend` (only script) | `v_finish` measured, full narrowed validator |           **7,695** |

Referenced bytes per transaction: 7,695 (no yields) → tier 0, 7,695 × 15 ≈
115,000 lovelace (≈ 0.12 ADA), against 28,023 today (tier 1, ≈ 428,000
lovelace, ≈ 0.43 ADA). Aggregate ExUnits: one execution, strictly below
today's (no output-proof codec runs; the `ScriptSources` witness encoder is
unchanged); recorded on the first emulator lifecycle (§7) against the
13,200,000 basis.

## 6. Off-chain work

Deployment entry `validationTraceDisputeResolveInputsFinishSemantic`
(`…ENTRIES_V1[1]`), resolver-7 submit route (no auxiliary fields), funding
row `validation-dispute.semantic.resolve-inputs.finish` (7,695 bytes). No
contracts.ts, role, manifest, or codec changes. **Nothing exists today for
this contract beyond the `contracts.ts` title: no deployment entry, no
submit route, no funding row, no role.** The deployment-entry table,
resolver-index constant and
`requireValidationResolveInputsSemanticReferenceScriptUtxo` are created
once by the step plan §6; this plan contributes row 1, its funding row and
its inspection-fixture entry.

## 7. Emulator scenario tests

`demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-resolve-inputs.test.ts`
(shared resolve-inputs journey file; fixture and harness per the step plan
§7): `publishes the resolve-inputs finish resolver inside the L1 envelope` —
`publishPlainReferenceScriptUtxo` **without `oversized`**, under
`withRealL1MaxTxSize`, asserting `l1ByteMargin > 0` and
`assertReferenceScriptRawBodiesFitL1EnvelopeV1`; positive lifecycle through
`submitValidationDisputeAward` with `disputedStep: "finish"`
(the last `resolveInputs` state, successor phase `scriptSources`); valid-block
negative: challenger claims finish while `remaining_schedule_hash` is
non-empty (one input unresolved) → refused at the same frontier; wrong
successor phase refused; cancel path. Maximum shape: a transaction with the
maximum admissible inputs so `cursor - 1` equals the resolved-input count
carried into `ScriptSources`.

## 8. Aiken tests

Shared codec properties (begin plan §8); `validation-machine-v1.test.ak`:
narrowed predicate agrees with `verify_one_step_evidence` on
`resolve_inputs_finalizes_into_script_source_resolution`; negatives: pending
present, non-empty remaining schedule, `cursor == 0`, successor phase
`ResolveInputs`.

## 9. Verification commands

As the step plan §9; expect `resolve_inputs_finish_semantic_v1.main.spend` ≤ 15,000 (≈ 7,700).

## 10. Ordering and dependencies

Shares the narrowed codec with the five siblings; the `ScriptSources`
witness encoder is shared with the script-sources plans but unchanged here;
single regeneration.

## 11. Risks

None material: 7.3 KB headroom, no ABI change.
