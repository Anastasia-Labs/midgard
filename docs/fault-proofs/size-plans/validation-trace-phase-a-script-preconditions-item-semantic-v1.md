# Size-fit plan: `phase_a_script_preconditions_item_semantic_v1`

Reads with [00-primer.md](00-primer.md). Sibling plan:
[validation-trace-phase-a-script-preconditions-semantic-v1.md](validation-trace-phase-a-script-preconditions-semantic-v1.md).
Both resolvers call the same library function today; the fix is to give each
its own narrowed entry point.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/phase_a_script_preconditions_item_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/phase-a-script-preconditions-item-semantic-v1.ak` |
| Raw size | **28,066 bytes** (1.71× the limit) |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id`, `field_preimage_certificate_policy_id` (3) |
| Phase / indices | `PhaseAScriptPreconditions` (resolver 6), semantic index 1 of `phase_a_script_preconditions_semantic_resolver_count = 2` (`validation-resolver-v1.ak`), global 25 |
| Machine step | one field-3 (required observers) item: opens item `control.observer_seen` through the §8 door, requires a 28-byte hash, strictly increasing order against `control.previous_observer`, successor `observer_seen + 1` |
| Library entry point | `verify_phase_a_script_preconditions_semantics_v1(pre, evidence, door)` → `verify_phase_a_script_preconditions(pre, transition, auxiliary, door)` with `auxiliary = TransactionFieldChunkWitness { field_index, item_index, carriage }` |
| Redeemer / auxiliary | `VerifyItem { input_index, output_index, transition, field_index, item_index, carriage }`; auxiliary shape `[1, 3]` |
| Rejection reasons | `ObserverOrderInvalid` (`reject_invalid_field_type`) |
| Role / deployment entry today | none / none (inline attach) |

## 2. Why it is this size

`verify_phase_a_script_preconditions` branches on `auxiliary` **at run time**,
so this resolver compiles both arms:

| Probe | Reachable code | Raw bytes | Delta |
| --- | --- | ---: | ---: |
| `p20_sp_decode` | `phase_a_script_preconditions_control_from_witness` | 1,490 | — |
| `p21_sp_bound` | p20 + `phase_a_script_preconditions_control_is_bound` (`native_tx_proof_commitment_v1`, `encode_phase_a_script_preconditions_witness`) | 3,450 | ≈1,960 |
| `p17_door_open` | `p01` + §8 door | 6,816 | ≈4,600 |
| `p22_sp_finalize` | `p01` + `p20` + `phase_a_script_preconditions_finalize` | 19,106 | **≈15,400** — the finalize arm this resolver can never take |
| `p40_encode_ri_generic` | `encode_resolve_inputs_witness(…, None, …)` | 14,570 | the `Option<ResolveInputOutputProofV1>` `Some` arm drags `ledger_output_proof_v1.encode_control_v1` in |

So 28,066 ≈ shell 3.3 + decode 1.5 + proof-source 2.2 + binding 2.0 + door
4.6 + item arm (`machine_field_item_bytes`, compare, successor ≈2.0, rejected
1.6) + **finalize arm ≈15.4** (unreachable here).

| Build | Raw bytes |
| --- | ---: |
| baseline | 28,066 |
| E2: validator calls `verify_phase_a_script_preconditions_item_semantics_v1` (item arm only) | **12,556** |

## 3. Options considered

- **Prune (chosen).** A resolver-specific entry point with the item arm only.
  With `auxiliary` fixed to `TransactionFieldChunkWitness` by the validator,
  the two `NoAuxiliaryWitness` arms of the shared function are unreachable
  (`auxiliary == NoAuxiliaryWitness` is false), so dropping them changes
  nothing the resolver proves. Measured 28,066 → 12,556.
- **Yield split.** Not needed after the prune; would add a role and a second
  parse for a body that fits with 2.4 KB to spare.
- **Chaining / redesign.** Not warranted; the step is already one observer.

## 4. Chosen design

New library function in `validation-machine-v1.ak`:

```aiken
pub fn verify_phase_a_script_preconditions_item_semantics_v1(
  pre: ValidationMachineStateV1, witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1, field_index: Int, item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool
```

Body: decode control; `verify_native_tx_proof_source_v1`; `observer_commitment
= verified_source.tx_compact.body.required_observers_hash`; `and { version ==
1, phase_a_script_preconditions_control_is_bound(pre, witness, control,
observer_commitment), observer_commitment != empty_field_commitment,
or { control.observer_count == 0, control.observer_seen <
control.observer_count } }` — the negation of the two finalize guards — then
the existing item arm verbatim (door open at field 3 / `control.observer_seen`,
`active_count`, `field_index == 3`, `item_index == control.observer_seen`,
`item_count == active_count`, `machine_field_item_length(item) == 28`,
ordering check → `rejected_successor_is_exact` or
`phase_a_script_preconditions_successor_is_exact`). The validator calls it with
`(datum |> semantic_pre_state, transition, door, field_index, item_index,
carriage)` instead of building `ValidationOneStepEvidenceV1`. The generic
`verify_phase_a_script_preconditions_semantics_v1` remains for
`verify_phase_a_script_preconditions_one_step_v1` (dispute-level use) and
tests.

No datum/redeemer/auxiliary/work-witness/rejection-code change; parameters
unchanged (3). Security: `continue_winning` still binds `transition` and the
`TransactionFieldChunkWitness` auxiliary to `state.evidence_hash`; the item
arm is unchanged; the added guards are exactly the conditions under which the
shared function reached the item arm. No yield, so dispatch uniqueness / role
authentication / cross-arm substitution / omission do not arise; the two
script-preconditions resolvers remain distinct hashes in the 2-slot roster.

## 5. Size and budget projection

| Script | Today | Projected | Method |
| --- | ---: | ---: | --- |
| `…script_preconditions_item_semantic_v1.main.spend` | 28,066 | **12,556** (applied ≈12,629) | measured, build E2 |

One 12.6 KB body referenced per observer-item transaction (first fee tier,
≈0.19 ADA). ExUnits: strictly a subset of today's work (no finalize arm is
evaluated today either, so runtime cost is unchanged apart from the removed
guard evaluations).

## 6. Off-chain work (none exists today)

- `contracts.ts`: no change (title and parameters unchanged).
- Deployment roster: `VALIDATION_PHASE_A_SCRIPT_PRECONDITIONS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1
  = { 0: "validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic", 1:
  "validationTraceDisputePhaseAScriptPreconditionsItemSemantic" }` with the
  `require…ReferenceScriptUtxo` helper, modelled on the ValueAndMint pair in
  `submit.ts` (~907–1040).
- Submit route: `resolverIndex === 6` branch in the semantic-resolution
  builder (published → by reference; absent and over the envelope → fail
  fast; absent and small → inline). The item transaction carries a field-3
  carriage (tier 1 inline up to 30 × 28-byte observers, tier 2/3 otherwise),
  so by-reference consumption is the normal route.
- Funding / descriptors: one `spendDescriptor` row per entry in
  `contract-deployment-info.ts`; `inspect-contracts.test.ts` oversized list
  shrinks by one.
- No `midgard-core` / `midgard-validation` codec change.

## 7. Emulator scenario tests (none exist today)

`demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-phase-a-preconditions.test.ts`
(shared with the finalize plan):

- Publication fit for `semanticResolvers[25]` without `oversized` under
  `withRealL1MaxTxSize`.
- Positive lifecycle `buildPhaseAObserverItemFixture({ observerCount: 3,
  index: 1 })` (operator claims a wrong `previous_observer` successor) through
  award.
- Valid-block negative: honest successor claimed → resolver refuses.
- `ObserverOrderInvalid` route: unsorted observers → terminal rejection
  successor proven.
- Cancel/resume.
- Maximum shape: `observer_count = max_tx_size_derived_collection_item_count`
  via tier-3 certificate carriage, item at the last index.

## 8. Aiken tests

- `validation-machine-v1.test.ak`: `phase_a_script_preconditions_item_entry_equals_shared_function`
  (property: for generated controls and a `TransactionFieldChunkWitness`
  auxiliary, the new entry point equals `verify_phase_a_script_preconditions`),
  `phase_a_script_preconditions_item_entry_refuses_a_complete_observer_scan`
  (`observer_seen == observer_count` → `False`), and
  `…_refuses_an_empty_observer_field`.
- `phase-a-split-v1.test.ak`: `script_preconditions_item_wire_layout_is_pinned`,
  `script_preconditions_item_validator_refuses_the_finalize_step` (fail),
  `prepare_routes_script_preconditions_item_to_slot_one`,
  `prepare_refuses_a_third_script_preconditions_resolver` (fail).

## 9. Verification commands

```bash
cd onchain/aiken && /home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/phase_a_script_preconditions/.test(v.title))console.log(v.title,Buffer.from(v.compiledCode,"hex").length)'
# expect item ≈12,556, finalize ≈8,824, prepare 5,302
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m phase_a_script_preconditions   # existing 4 + §8 tests
cd demo/midgard-fault-proofs && pnpm test -- tests/submit-init-emulator-validation-dispute-phase-a-preconditions.test.ts tests/zz605-semantic-resolver-arity.test.ts tests/inspect-contracts.test.ts
```

## 10. Ordering and dependencies

Lands with the finalize plan (same validator pair, same 2-hash list in
`phase_a_script_preconditions_v1`) in the single regeneration; independent of
the native-scripts plans except for the shared `submit.ts` roster pattern.

## 11. Risks

- Low: pure branch narrowing with a property test tying it to the shared
  function.
- Regeneration drift is absorbed by a 2.4 KB margin.
- Spec: C50 (observer semantics) unaffected — same checks, same rejection
  code.
