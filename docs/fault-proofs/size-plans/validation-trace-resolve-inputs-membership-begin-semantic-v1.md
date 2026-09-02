# Size-fit plan: `resolve_inputs_membership_begin_semantic_v1`

Cites [00-primer.md](00-primer.md); shares `resolve-inputs-control-v1.ak`
with the other five plans (defined in
[membership-step](validation-trace-resolve-inputs-membership-step-semantic-v1.md) §4.1).

## 1. Identity

| Field                                      | Value                                                                                                                                                                                                                                                                                                                                       |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title                            | `fraud_proofs/validation_trace/resolve_inputs_membership_begin_semantic_v1.main.spend`                                                                                                                                                                                                                                                      |
| File                                       | `onchain/aiken/validators/fraud-proofs/validation-trace/resolve-inputs-membership-begin-semantic-v1.ak`                                                                                                                                                                                                                                     |
| Raw size                                   | 31,141 bytes (1.90× limit; 2.08× target)                                                                                                                                                                                                                                                                                                    |
| Applied parameters                         | `award_script_hash`, `computation_thread_policy_id`                                                                                                                                                                                                                                                                                         |
| Phase / resolver / semantic / global index | `ResolveInputs` / 7 / 2 of 6 / 28                                                                                                                                                                                                                                                                                                           |
| Auxiliary                                  | `ScheduledLedgerMembershipWitness { source_kind, key, next_schedule_hash, value: descriptor_cbor, proof: Proof, signer_proof }`                                                                                                                                                                                                             |
| Library entry                              | `verify_resolve_inputs_membership_begin_semantics_v1` → `resolve_inputs_control_is_bound` + `resolve_membership_begin_step` (`mpf_proof_v1.has`, `ledger_output_commitment_v1.decode`, `decode_midgard_tx_input_cbor`, `resolution_schedule_node_hash`, `resolve_inputs_successor_is_exact` with `pending = Some(initial_control_v1(...))`) |
| Role / deployment entry today              | none / none                                                                                                                                                                                                                                                                                                                                 |

## 2. Why it is this size

| Probe                                                                                      |           Bytes | Isolates                                                                                                                                         |
| ------------------------------------------------------------------------------------------ | --------------: | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `d_membership_begin`                                                                       |           4,233 | dispatcher shell (incl. `Proof`, `SignerSetProofV1` decoders)                                                                                    |
| `p_pred_membership_begin`                                                                  |          28,476 | predicate                                                                                                                                        |
| `p_control_parse` / `p_control_bound`                                                      | 18,598 / 22,741 | generic control parse / binding (reach `decode_control_v1` through the `pending` `Some` arm this arm never takes: `pending == None` is required) |
| `p_control_parse_narrow` / `p_control_bound_narrow`                                        |   1,458 / 2,990 | narrowed no-pending parse / binding                                                                                                              |
| `p_lop_encode_initial` = `encode_control_v1(initial_control_v1(...))`                      |          13,699 | the successor's pending encoding                                                                                                                 |
| `p_mpf_has`                                                                                |           3,174 | `mpf_proof_v1.has`                                                                                                                               |
| `p_narrow_begin`                                                                           |          23,553 | narrowed predicate still calling `encode_control_v1(initial_control_v1)`                                                                         |
| `v_membership_begin` (full validator, narrowed, scan encoder for the initial scan control) |          14,610 | measured                                                                                                                                         |
| `v_membership_begin_const` (initial scan control as a constant)                            |          12,547 | measured                                                                                                                                         |

Two reachable generic paths dominate: the control decoder through the
`pending: Some` arm (≈ 17 KB) that this arm forbids by construction, and
`encode_control_v1(initial_control_v1(...))` (13.7 KB) used only to encode a
control that is a constant except for three scalars.
`aiken check`: `resolve_inputs_begins_authenticated_membership_output`
26,234,474 mem / 11,132,442,773 cpu (fixture-inclusive).

## 3. Options considered

1. **Prune (chosen).** Both dominators are generic decoders reached through
   a shared entry point for a shape this arm can never take. Narrowing them
   is ABI-neutral and exact.
2. **Yield split.** Not needed after the prune (12,547 < 15,000); kept as
   the fallback: dispatcher (4,233 + 664) + one yield carrying the predicate.
3. **Chaining / redesign.** Rejected: one MPF proof and one hash, no budget
   pressure.

## 4. Chosen design

Same validator, same title, same two parameters, same redeemer and datum.
Body changes:

```aiken
verify_resolve_inputs_membership_begin_semantics_v1(pre, transition, source_kind, key,
  next_schedule_hash, descriptor_cbor, proof, signer_proof)
```

is re-implemented in `validation_machine_v1` (or moved next to the codec in
`resolve-inputs-control-v1.ak`) as:

1. `control = control_no_pending_from_witness(transition.work_witness_cbor)` — exact 11-array parse, `expect pending_bytes == #"00"` (the monolith's `control.pending == None` clause, now fail-closed at parse time).
2. `control_no_pending_is_bound(pre, transition, control)` — every clause of `resolve_inputs_control_is_bound` (the pending clause is vacuous for `None`); successor/witness bytes via `encode_control_no_pending`, byte-identical to `encode_resolve_inputs_witness(..., None, ...)`.
3. Unchanged clauses: `cursor > 0`, `remaining_schedule_hash != empty_resolution_schedule_hash()`, `signer_proof == NoSignerSetProof`, `source_kind ∈ {0,1}`, `resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.remaining_schedule_hash`, `decode_midgard_tx_input_cbor(key).output_index == descriptor.output_index`, `descriptor.total_length > 0`, `mpf_proof_v1.has(pre.prior_ledger_root, key, descriptor_cbor, proof)`.
4. Successor: `claimed_successor.phase == ResolveInputs` and `work_root == hash_work_witness(ResolveInputs, pc + 1, encode_control_with_pending(control, encode_pending_raw({source_kind, key, next_schedule_hash, descriptor_cbor, output_proof_cbor: ledger_output_proof_v1.encode_initial_control_v1(descriptor.output_index, descriptor.total_length, descriptor.item_commitment)})))`.

New `ledger_output_proof_v1.encode_initial_control_v1(output_index, total_length, item_commitment)`:
`expect output_index >= 0`, `expect total_length > 0`, `expect length(item_commitment) == 32` (exactly the clauses `control_is_well_formed` enforces on a stage-0 control), then
`encode_definite_array_header(12) ++ serialise(version) ++ serialise(stage_structure) ++ serialise(output_index) ++ serialise(total_length) ++ serialise(b_data(item_commitment)) ++ ledger_output_scan_v1.initial_control_cbor_v1 ++ #"d87a80" ++ #"d87a80" ++ serialise(0) ++ #"80" ++ #"d87a80" ++ #"d87a80"`,
where `ledger_output_scan_v1.initial_control_cbor_v1` is a `pub const` pinned by the test `encode_control_v1(initial_control_v1()) == initial_control_cbor_v1`.

**Handshake and security argument.** There is no yield, no role NFT, no
withdrawal and no new parameter, so the primer's handshake items are
vacuous — stated explicitly so the reviewer does not look for them:

- _Dispatch uniqueness / role authentication / omission:_ not applicable;
  the predicate runs inside the one spend validator that `prepare_selected`
  already routed to (semantic index 2 of `resolve_inputs_v1`).
- _Cross-arm substitution:_ unchanged from today — the resolver hash is one
  of the six in the prepare validator's parameter list, and the narrowed
  parser fails closed on any witness whose pending byte is not `#"00"`, so a
  step/finalize witness cannot be presented to this arm.
- _Output-state re-derivation:_ `continue_winning` still pins the award
  output; the successor `work_root` is recomputed with
  `encode_control_with_pending`, whose output is proven byte-identical to
  `encode_resolve_inputs_witness(..., Some(initial), ...)` (§8).
- _What an attacker gains:_ nothing — every predicate clause is the
  monolith's, and the two narrowed encoders are proven byte-identical to the
  generic ones (§8). Nothing is inherited or omitted. The pinned fork aborts
  on `Option<record> == None`; the parser compares the raw pending byte, not
  the option.

## 5. Size and budget projection

| Script                                                                 | Basis                                                                                                       | Projected raw bytes |
| ---------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------- | ------------------: |
| `resolve_inputs_membership_begin_semantic_v1.main.spend` (only script) | `v_membership_begin_const` measured, production shape, both narrowed codecs, constant initial scan encoding |          **12,547** |

Referenced bytes per transaction: 12,547 (the dispatcher itself, published
by reference; no yields) → tier 0 of the Conway reference-script fee,
12,547 × 15 ≈ 188,000 lovelace (≈ 0.19 ADA), against 31,141 today (tier 1:
384,000 + 5,541 × 18 ≈ 484,000 lovelace, ≈ 0.48 ADA). Aggregate ExUnits:
one execution; the output-proof codec (decode + encode round-trip) no longer
runs, so the cost is strictly below today's; unmeasured for the validator
alone (the `aiken check` figure in §2 is fixture-inclusive) and recorded on
the first emulator lifecycle (§7) against the 13,200,000 memory basis.

## 6. Off-chain work

- `contracts.ts`: no change (same title, same two parameters).
- Deployment entry `validationTraceDisputeResolveInputsMembershipBeginSemantic` in `VALIDATION_RESOLVE_INPUTS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1[2]`; submit route `resolverIndex === 7` consumes it by reference when present (ValueAndMint pattern) and refuses inline when the body would not fit the proof envelope; funding row `validation-dispute.semantic.resolve-inputs.membership-begin` (`referenceScriptBytes` 12,547).
- No roles, manifest, or codec changes. `demo/midgard-validation` already produces the `scheduledLedgerLookup` witness with `value` set.
- **Nothing exists today for this contract beyond the `contracts.ts` title:
  no deployment entry, no submit route, no funding row, no role** (only
  `VALIDATION_CEK_…` and `VALIDATION_VALUE_AND_MINT_…_DEPLOYMENT_ENTRIES_V1`
  exist in `submit.ts`). The deployment-entry table
  `VALIDATION_RESOLVE_INPUTS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`,
  `VALIDATION_RESOLVE_INPUTS_RESOLVER_INDEX_V1 = 7` and
  `requireValidationResolveInputsSemanticReferenceScriptUtxo` are created
  once by the step plan §6; this plan contributes its row (index 2), its
  funding row and its inspection-fixture entry.

## 7. Emulator scenario tests

In `demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-resolve-inputs.test.ts`
(the shared resolve-inputs journey file; fixture and harness per the step
plan §7 — `buildForgedOperatorSuccessorValidationDisputeFixture` with
`disputedPhase: "resolveInputs"`, `runForcedValidationDisputeScenario`
generalised to the resolver-7 roster):
`publishes the membership-begin resolver inside the L1 envelope` —
`publishPlainReferenceScriptUtxo` **without `oversized`**, under
`withRealL1MaxTxSize`, asserting `l1ByteMargin > 0` and
`assertReferenceScriptRawBodiesFitL1EnvelopeV1`; positive lifecycle through
`submitValidationDisputeAward` with `disputedStep: "membershipBegin"`
(honest fixture, first membership lookup), asserting `completeSignedBytes ≤ 16,384`
and recording `exUnits.mem`;
valid-block negative: forged successor (wrong `next_schedule_hash`) refused
at the same frontier; MPF proof for a different key refused; cancel path.
Maximum shape: schedule with the maximum admissible inputs so
`remaining_schedule_hash` chains through several nodes (the fixture's
`resolutionScheduleNodes`). Nothing exists today.

## 8. Aiken tests

`resolve-inputs-control-v1.test.ak`: property tests
`encode_control_no_pending(control_no_pending_from_witness(w)) == w`,
`encode_control_no_pending == encode_resolve_inputs_witness(..., None, ...)`,
`control_no_pending_from_witness` fails on any non-`#"00"` pending byte.
`ledger-output-proof-v1.test.ak`: property
`encode_initial_control_v1(i, l, c) == encode_control_v1(initial_control_v1(i, l, c))`
over `i ≥ 0`, `l > 0`, 32-byte `c`, and equal failure on each violated bound;
`initial_control_cbor_v1` pin. `validation-machine-v1.test.ak`: the narrowed
`verify_resolve_inputs_membership_begin_semantics_v1` agrees with
`verify_one_step_evidence` on `resolve_inputs_authenticated_membership_step(0)`
and on negatives (wrong `source_kind`, non-`NoSignerSetProof`, zero
`total_length`, stale schedule hash).

## 9. Verification commands

As the step plan §9; expect the `node -e` listing to show
`resolve_inputs_membership_begin_semantic_v1.main.spend` ≤ 15,000 (target
≈ 12,600) and `aiken check -m encode_initial_control` to pass.

## 10. Ordering and dependencies

Shares `resolve-inputs-control-v1.ak` with the five siblings and
`encode_initial_control_v1` with `script_sources_output_proof_begin_semantic_v1`
(37,945 bytes, same `initial_control_v1` encoding at `validation_machine_v1`
line 9307); land in the single blueprint regeneration.

## 11. Risks

Regeneration drift on a 12.5 KB body is comfortable (2.4 KB). If the
constant is mis-pinned the property test fails at `aiken check`, not on L1.
No ABI churn, no spec conflict.
