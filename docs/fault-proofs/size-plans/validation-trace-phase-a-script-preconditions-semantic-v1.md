# Size-fit plan: `phase_a_script_preconditions_semantic_v1`

Reads with [00-primer.md](00-primer.md). Sibling plan:
[validation-trace-phase-a-script-preconditions-item-semantic-v1.md](validation-trace-phase-a-script-preconditions-item-semantic-v1.md).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/phase_a_script_preconditions_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/phase-a-script-preconditions-semantic-v1.ak` |
| Raw size | **27,841 bytes** (1.70× the limit) |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id`, `field_preimage_certificate_policy_id` (3; the third is unused by this step's arms) |
| Phase / indices | `PhaseAScriptPreconditions` (resolver 6), semantic index 0 of 2, global 24 |
| Machine step | the **finalize** step: either the observer field is empty or every observer has been seen; applies `phase_a_script_preconditions_rejection` (integrity hash missing for a scripted transaction; observers on an untagged network) → terminal rejection, else hands off to `ResolveInputs` with `encode_resolve_inputs_witness(…, cursor 0, initial_resolution_accumulator(), …, pending None, …)` |
| Library entry point | `verify_phase_a_script_preconditions_semantics_v1(pre, evidence, door)` with `auxiliary = NoAuxiliaryWitness` |
| Redeemer / auxiliary | `Verify { input_index, output_index, transition }`; auxiliary shape `[0, 0]` |
| Rejection reasons | `ScriptIntegrityHashMissing`, `ObserversForbiddenOnUntaggedNetwork` (both `reject_invalid_field_type` today) |
| Role / deployment entry today | none / none |

## 2. Why it is this size

| Probe | Reachable code | Raw bytes | Delta |
| --- | --- | ---: | ---: |
| `p22_sp_finalize` | `p01` + `p20` + `phase_a_script_preconditions_finalize` | 19,106 | ≈15,400 |
| `p40_encode_ri_generic` | `encode_resolve_inputs_witness(a, b, c, d, 0, initial_resolution_accumulator(), rsh, sc, sfc, None, rsh)` | **14,570** | the `Option<ResolveInputOutputProofV1>` parameter makes `encode_optional_resolve_input_output_proof`'s `Some` arm reachable, which inlines `encode_resolve_input_output_proof` → `ledger_output_proof_v1.encode_control_v1` (the ledger-output-proof control codec, ~3,700 lines of `ledger-output-*`) |
| `p41_encode_ri_initial` | the same encoder specialised to `None` (`encode_definite_bytes(#"00")` inlined) | **640** | ≈13.9 KB saved |
| `p42_sp_finalize_initial` | `p01` + `p20` + finalize with the specialised encoder | 5,616 | finalize proper ≈1.9 KB |
| `p17_door_open` | §8 door (the item arm this resolver can never take) | 6,816 | ≈4,600 |

So 27,841 ≈ shell 3.3 + decode 1.5 + proof-source 2.2 + binding 2.0 +
finalize (≈1.9 + **13.9 of dead `Some`-arm encoder**) + item arm (door 4.6 +
successor + rejected) with sharing.

| Build | Raw bytes |
| --- | ---: |
| baseline | 27,841 |
| E2: finalize-only entry point (item arm removed) | 23,582 |
| E2 + E2b: finalize-only + `encode_resolve_inputs_initial_witness` | **8,824** |

## 3. Options considered

- **Prune (chosen), two cuts.** (a) A finalize-only entry point: with
  `auxiliary` fixed to `NoAuxiliaryWitness`, the item arm's `expect
  TransactionFieldChunkWitness … = auxiliary` can never succeed, so removing
  it changes nothing proven. (b) A hand-off encoder specialised to `pending =
  None`, byte-identical to `encode_resolve_inputs_witness(…, None, …)`:
  `encode_definite_bytes(#"00")` is literally the `None` arm of
  `encode_optional_resolve_input_output_proof`. Measured 27,841 → 8,824.
- **Yield split / chaining / redesign.** Rejected: the pruned body is 6.2 KB
  under the target.

## 4. Chosen design

New library functions in `validation-machine-v1.ak`:

```aiken
pub fn encode_resolve_inputs_initial_witness(
  compact_cbor, witness_set_compact_cbor, field_preimage_lengths_cbor, context_cbor,
  cursor: Int, accumulator: ByteArray, remaining_schedule_hash: ByteArray,
  signer_count: Int, signer_frontier_commitment: ByteArray, resolution_schedule_hash: ByteArray,
) -> ByteArray
```

Same `expect` guards and the same `#"8b"` array of eleven items as
`encode_resolve_inputs_witness`, with the tenth item fixed to
`encode_definite_bytes(#"00")`. A golden/property test pins byte equality
with the generic encoder at `pending = None` (§8).

```aiken
pub fn verify_phase_a_script_preconditions_finalize_semantics_v1(
  pre: ValidationMachineStateV1, witness: ValidationOneStepWitnessV1,
) -> Bool
```

Body: decode control; `verify_native_tx_proof_source_v1`; `observer_commitment`,
`has_redeemers`; `and { version == 1, control_is_bound, if observer_commitment
== empty_field_commitment { finalize(…, 0) } else { and { observer_count > 0,
observer_seen == observer_count, finalize(…, observer_count) } } }`, where
`phase_a_script_preconditions_finalize` is re-pointed to the specialised
encoder (its only call site passes `None`). No `door` parameter: the validator
drops `field_preimage_certificate_policy_id` (3 → 2 declared parameters; the
name-keyed loop in `contracts.ts` follows the blueprint, and `zz605` verifies
full application). Redeemer, auxiliary, work-witness bytes and rejection
codes are unchanged.

Security: the finalize step proves exactly what it proves today — the
successor `work_root` is `hash_work_witness(ResolveInputs, pc + 1, <same
bytes>)`; the item arm removed was unreachable. No yield; no dispatch, role,
substitution or omission surface is introduced. The 2-slot roster
(`phase_a_script_preconditions_semantic_resolver_count = 2`) is unchanged.

## 5. Size and budget projection

| Script | Today | Projected | Method |
| --- | ---: | ---: | --- |
| `…script_preconditions_semantic_v1.main.spend` | 27,841 | **8,824** (applied ≈8,897) | measured, build E2 + E2b |

Small enough to attach inline if desired, but published by reference like its
sibling for uniform routing (first fee tier, ≈0.13 ADA). ExUnits: a subset of
today's evaluation.

## 6. Off-chain work (none exists today)

- `contracts.ts`: parameter count follows the blueprint automatically; the
  `zz605` gate's "none of its under-applied prefixes" leg covers the new
  arity. Title unchanged.
- Deployment roster entry
  `validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic` (index 0)
  and the `resolverIndex === 6` submit branch (sibling plan §6).
- One `spendDescriptor` row; `inspect-contracts.test.ts` oversized list
  shrinks by one.
- No codec change; `validationSemanticResolverIndexV1` unchanged.

## 7. Emulator scenario tests (none exist today)

In `submit-init-emulator-validation-dispute-phase-a-preconditions.test.ts`:

- Publication fit for `semanticResolvers[24]` without `oversized`.
- Positive lifecycle `buildPhaseAPreconditionsFinalizeFixture({ observers: 0
  })` (empty observer field, native-only transaction; operator claims a wrong
  `ResolveInputs` work root) through award, and `{ observers: 2 }` after two
  item steps.
- Rejection routes: PlutusV3 witness with zero `script_integrity_hash` →
  `ScriptIntegrityHashMissing`; observers on `network_id == 255` →
  `ObserversForbiddenOnUntaggedNetwork`.
- Valid-block negative at the same frontier; cancel/resume; maximum shape
  (`observer_count` at the collection maximum, `signer_count` at maximum).

## 8. Aiken tests

- `validation-machine-v1.test.ak`:
  `encode_resolve_inputs_initial_witness_equals_generic_encoder_at_none`
  (property over all ten arguments), and
  `phase_a_script_preconditions_finalize_entry_equals_shared_function`
  (property with `NoAuxiliaryWitness`), `…_refuses_a_pending_observer_scan`
  (`observer_seen < observer_count` → `False`). The existing
  `phase_a_script_preconditions_advances_an_empty_bundle_to_resolution`,
  `…_proves_duplicate_observers_are_a_no_op`,
  `…_require_integrity_for_plutus_bytes` are re-pointed at the new entry.
- `phase-a-split-v1.test.ak`: `script_preconditions_finalize_wire_layout_is_pinned`,
  `script_preconditions_finalize_validator_refuses_an_item_step` (fail),
  `prepare_routes_script_preconditions_finalize_to_slot_zero`.

## 9. Verification commands

As the sibling plan §9; expect `phase_a_script_preconditions_semantic_v1.main.spend`
≈ 8,824 and two declared parameters in `plutus.json`; `aiken check -m
encode_resolve_inputs_initial_witness` passes the byte-equality property.

## 10. Ordering and dependencies

- Lands with the item sibling in the single regeneration.
- `encode_resolve_inputs_initial_witness` is reusable by any other hand-off
  that passes `pending = None` — check `resolve_inputs_initial_semantic_v1`
  and the resolve-inputs group's plans before they choose a design; the
  ≈13.9 KB saving is likely to apply there too.

## 11. Risks

- Low. The encoder specialisation is a pure code-size fix pinned by a byte
  equality test; the entry-point narrowing is unreachable-arm removal.
- The declared-parameter change (3 → 2) is a blueprint ABI change that the
  #605/#609 gates are designed to catch; deployment tooling that hand-writes
  parameter lists would break loudly, which is the intended behaviour.


### Canonical absence correction — 2026-09-05

Execution against the real host replay exposed an error in the original
plan's all-zero integrity-hash sentinel. The canonical codec and
`script_language_views_v1` both encode absence as Blake2b-256 of CBOR null
(`f6`), namely
`01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53`.
The Phase-A predicate, both dedicated family predicates and their host
classifiers now use that same commitment. An all-zero hash is a present,
incorrect integrity hash; it must not be reclassified as absence. This
replaces the zero-sentinel assumption above without a compatibility branch.


### Implemented preconditions and publication closure — 2026-09-05

Finalize and item now use separate predicates; finalize constructs only the
initial no-pending ResolveInputs encoding. The unchanged generic predicate
remains the differential reference. Public submission resolves the two
precondition contracts and all fourteen native Phase-A semantic contracts
through canonical deployment entries. Those publication entries have roles
in the current strict manifest registry; this updates the older plan's
assumption that published semantic entries needed no registered role.

Normal testnet blueprint
`314b1134813eb745b7f8df611d5643257df64774e2a199461271fb7f479062f8`:
12 lifecycle scenarios pass, including maximum 1,092-entry/32,763-byte
observer carriage and finalization, both honest-successor refusals, three
real rejection paths, cancellation and fresh out-ref recovery for both
resolvers. Every success mints permanent proof and removes the bad block.
The complete 725-row ledger records 371 publications, maximum 15,872 signed
bytes, 3,094,426 memory and 1,055,289,369 CPU. Both 20% execution reserves
and the publication reserve pass. The maximum observer fixture replays a
real interval rejection in the following ResolveInputs phase, avoiding
irrelevant quadratic ScriptSources replay while retaining every observer
transition under dispute.

Host Phase A additionally rejects descending observer hashes, matching its
on-chain strictly-increasing scan; 46 host tests pass. The two canonical
absence family lifecycle suites and ledger verifiers pass after the fix.
This worktree evidence does not replace the final shared-branch rerun or
close installed validation-dispute replay.

Parameter application was also updated in all four validation emulator
fixtures. The three complete-item suites pass 12 scenarios together; the
65-node native scan deployment probe passes its one scenario. The latter
probe reports memory above the 20% reserve and is parameter wiring evidence
only, not closure evidence for native scan execution fit.

### Shared integration verification

The combined normal testnet blueprint
`f9e3afeb905b5de2f6299ed091d41d77f5ad12d20cb1a3e9b834214908d0e24c`
passes the 12 preconditions, seven ResolveInputs boundary and 14 canonical
integrity-absence lifecycle scenarios together (33/33). A separate complete
12-case preconditions run regenerates its 725-row ledger with 371 publications,
maximum 15,872 signed bytes, 3,106,492 memory and 1,057,191,985 CPU.
Both execution reserves pass. The deployment identity test passes 12 cases;
node manifest/reference publication tests pass 28. Fault-proof typecheck passes.
The combined roster contains 382 contracts, 375 roles and 376 auth token names.
Commands use the named files
`submit-init-emulator-validation-dispute-phase-a-preconditions.test.ts`,
`submit-init-emulator-validation-dispute-resolve-inputs.test.ts`,
`script-integrity-hash-missing-lifecycle.test.ts`, and
`observers-forbidden-on-untagged-network-lifecycle.test.ts`; the ledger run sets
`MIDGARD_WRITE_FIT_LEDGER=1`. These results do not close the still-open maximum
ResolveInputs continuation or installed validation-dispute replay surfaces.
