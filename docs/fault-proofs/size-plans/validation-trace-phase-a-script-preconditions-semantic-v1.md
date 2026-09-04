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
