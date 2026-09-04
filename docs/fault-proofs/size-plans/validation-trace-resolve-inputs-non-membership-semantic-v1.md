# Size-fit plan: `resolve_inputs_non_membership_semantic_v1`

Cites [00-primer.md](00-primer.md); shares `resolve-inputs-control-v1.ak`
(see [membership-step](validation-trace-resolve-inputs-membership-step-semantic-v1.md) §4.1).

## 1. Identity

| Field                                      | Value                                                                                                                                                                                                              |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Blueprint title                            | `fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1.main.spend`                                                                                                                               |
| File                                       | `onchain/aiken/validators/fraud-proofs/validation-trace/resolve-inputs-non-membership-semantic-v1.ak`                                                                                                              |
| Raw size                                   | 30,319 bytes (1.85× limit; 2.02× target)                                                                                                                                                                           |
| Applied parameters                         | `award_script_hash`, `computation_thread_policy_id`                                                                                                                                                                |
| Phase / resolver / semantic / global index | `ResolveInputs` / 7 / 5 of 6 / 31                                                                                                                                                                                  |
| Auxiliary                                  | `ScheduledLedgerNonMembershipWitness { source_kind, key, next_schedule_hash, proof }`                                                                                                                              |
| Library entry                              | `verify_resolve_inputs_non_membership_semantics_v1` → `resolve_inputs_control_is_bound` + `resolve_non_membership_step` (`mpf_proof_v1.does_not_have`, `rejected_successor_is_exact(..., reject_input_not_found)`) |
| Role / deployment entry today              | none / none; `validation-dispute-submit.test.ts` already pins this arm's redeemer ABI (`encodes resolver-7 non-membership evidence into the exact semantic ABI`)                                                   |

## 2. Why it is this size

| Probe                                 |           Bytes | Isolates                                                            |
| ------------------------------------- | --------------: | ------------------------------------------------------------------- |
| `d_non_membership`                    |           3,594 | dispatcher shell                                                    |
| `p_pred_non_membership`               |          27,799 | predicate                                                           |
| `p_core_non_membership`               |          24,383 | parse + `resolve_non_membership_step`                               |
| `p_control_parse` / `p_control_bound` | 18,598 / 22,741 | generic parse / binding through the unreachable `pending: Some` arm |
| `p_control_bound_narrow`              |           2,990 | narrowed                                                            |
| `p_mpf_does_not_have`                 |           3,598 | `mpf_proof_v1.does_not_have`                                        |
| `p_rejected_exact`                    |           1,567 | `rejected_successor_is_exact`                                       |
| `p_narrow_non_membership`             |           7,212 | narrowed predicate                                                  |
| `v_non_membership`                    |           9,442 | full narrowed validator, measured                                   |

The body is 3.6 KB shell + 3.6 KB MPF + 1.6 KB rejection + ~1.5 KB
schedule hash, plus ~20 KB of `decode_control_v1` reached only through the
`pending: Some` decoder arm and `encode_optional_resolve_input_output_proof`
in `resolve_inputs_control_is_bound`. `aiken check`:
`resolve_inputs_proves_non_membership_as_an_exact_no_op` 3,342,886 mem /
1,281,777,732 cpu (fixture-inclusive; fits the 13,200,000 basis).

## 3. Options considered

Prune (chosen): the only oversized reachable code is a decoder for a shape
the arm rejects (`control.pending == None` is a predicate clause). Yield
split unnecessary (9,442). Chaining/redesign rejected (single MPF proof, one
rejection successor).

## 4. Chosen design

Same validator, title, parameters, redeemer, datum. Predicate
re-implemented with `control_no_pending_from_witness` and
`control_no_pending_is_bound` (exact 11-array parse with `pending == #"00"`
enforced at parse; binding clauses unchanged; witness bytes checked with
`encode_control_no_pending`), then the monolith's clauses verbatim:
`cursor > 0`, `remaining_schedule_hash != empty_resolution_schedule_hash()`,
`resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.remaining_schedule_hash`,
`mpf_proof_v1.does_not_have(pre.prior_ledger_root, key, proof)`,
`rejected_successor_is_exact(pre, claimed_successor, reject_input_not_found)`.
**Handshake and security argument.** No yield, no role NFT, no withdrawal,
no new parameter; the primer's handshake items are vacuous: _dispatch
uniqueness / role authentication / omission_ do not apply (one spend
validator, routed by `prepare_selected` to semantic index 5);
_cross-arm substitution_ is unchanged (the hash is one of the six in the
prepare validator's parameter list, and the narrowed parser fails closed on
any witness whose pending byte is not `#"00"`, so a membership witness cannot
be presented here); _output-state re-derivation_ is `continue_winning` plus
`rejected_successor_is_exact(pre, claimed_successor, reject_input_not_found)`
exactly as today; _what an attacker gains:_ nothing — security is
clause-for-clause equality with the monolith plus the byte-identity property
tests of the narrowed codec (§8).

## 5. Size and budget projection

| Script                                                               | Basis                                                | Projected raw bytes |
| -------------------------------------------------------------------- | ---------------------------------------------------- | ------------------: |
| `resolve_inputs_non_membership_semantic_v1.main.spend` (only script) | `v_non_membership` measured, full narrowed validator |           **9,442** |

Referenced bytes per transaction: 9,442 (no yields) → tier 0, 9,442 × 15 ≈
142,000 lovelace (≈ 0.14 ADA), against 30,319 today (tier 1, ≈ 469,000
lovelace, ≈ 0.47 ADA). Aggregate ExUnits: one execution, strictly below
today's (no output-proof codec runs); the `aiken check` figure in §2 already
fits the 13,200,000 basis fixture-inclusive; recorded on the first emulator
lifecycle (§7).

## 6. Off-chain work

Deployment entry `validationTraceDisputeResolveInputsNonMembershipSemantic`
(`…ENTRIES_V1[5]`), resolver-7 submit route, funding row
`validation-dispute.semantic.resolve-inputs.non-membership`
(`referenceScriptBytes` 9,442). No contracts.ts, role, manifest, or codec
changes. **Nothing exists today for this contract beyond the `contracts.ts`
title and the redeemer ABI unit test: no deployment entry, no submit route,
no funding row, no role.** The deployment-entry table, resolver-index
constant and `requireValidationResolveInputsSemanticReferenceScriptUtxo`
are created once by the step plan §6; this plan contributes row 5, its
funding row and its inspection-fixture entry.

## 7. Emulator scenario tests

`demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-resolve-inputs.test.ts`
(shared resolve-inputs journey file; fixture and harness per the step plan
§7): `publishes the non-membership resolver inside the L1 envelope` —
`publishPlainReferenceScriptUtxo` **without `oversized`**, under
`withRealL1MaxTxSize`, asserting `l1ByteMargin > 0` and
`assertReferenceScriptRawBodiesFitL1EnvelopeV1`; positive lifecycle through
`submitValidationDisputeAward` on a rejecting fixture
(operator accepts a transaction spending an input absent from the prior
ledger; honest terminal `reject_input_not_found`, built like
`buildAcceptedClaimOverRejectingTransactionFixture` but stopping at
`resolveInputs`/`InputNotFound`, which `demo/midgard-validation`
`validation-machine.ts` already emits as `scheduledLedgerLookup` with
`value: null`); negative: a valid block (input present) with a challenger
claiming `InputNotFound` is refused at the same frontier (MPF
non-membership proof cannot verify); non-empty MPF proof from
`validation-dispute-submit.test.ts` reused; cancel path. Maximum shape: the
longest admissible resolution schedule so the missing key sits last.

## 8. Aiken tests

Shared `resolve-inputs-control-v1.test.ak` properties (begin plan §8);
`validation-machine-v1.test.ak`: narrowed predicate agrees with
`verify_one_step_evidence` on `resolve_inputs_proves_non_membership_as_an_exact_no_op`
and rejects a pending-bearing witness, a wrong `next_schedule_hash`, a
membership proof, and an accepting successor.

## 9. Verification commands

As the step plan §9; expect `resolve_inputs_non_membership_semantic_v1.main.spend` ≤ 15,000 (≈ 9,450).

## 10. Ordering and dependencies

Shares the narrowed codec with the five siblings; lands in the single
regeneration; no dependency on the shared LOP stage yields or descriptor
yields (this arm never carries a pending output proof).

## 11. Risks

None material: 5.5 KB headroom, no ABI change, no spec conflict.
