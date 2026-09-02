# `script_sources_stage_seven_observer_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
§4a (library prunes) and §4c (the `validation-script-sources-yield-v1.ak`
module). This plan defines the **two stage-seven observer yields**.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_seven_observer_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-seven-observer-semantic-v1.ak` |
| Raw size | 33,961 bytes |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId`, `field_preimage_certificate_policy_id: PolicyId` (3) |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 25 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 25) = 57` |
| Library entry point | `verify_script_sources_stage_seven_observer_semantics_v1` → `script_sources_stage_seven_control_from_witness`, `verify_native_tx_proof_source_v1`, `field_door.open_machine_field_item(door, verified, witness_set, 3, seen, carriage)`, `script_sources_stage_seven_control_is_bound` (**`exact_script_sources_control`**), `script_sources_stage_seven_observer_successor_is_exact` (**`script_sources_control_successor_is_exact` → `exact_script_sources_control`**), `rejected_successor_is_exact(reject_invalid_field_type)` |
| Redeemer action | `VerifyObserver { input_index, output_index, transition, field_index, item_index, carriage: FieldCarriageV1 }`; auxiliary rebuilt as `TransactionFieldChunkWitness { field_index, item_index, carriage }` (constructor 1, 3 fields) |
| Rejection reached | `reject_invalid_field_type` (`E_INVALID_FIELD_TYPE`, `ObserverOrderInvalid` in `rejection-reason-v1.ts:298`) |
| Role / deployment entry today | none (anchor §1) |

What the step proves (C45/C50): with a non-empty `required_observers_hash`
(field 3) and the receive scan not started, item `observer_scan.seen` of the
required-observers field, opened through the §8 door, is a 28-byte hash; the
field's item count fixes `total_count` on the first item; strictly ascending
order is enforced (`previous_hash < observer_hash`, else the exact
`E_INVALID_FIELD_TYPE` rejection); the successor appends
`purpose_leaf_hash(2, seen, hash, hash)` to `purpose_peaks` and advances
`observer_scan`.

## 2. Why it is this size

| Probe | Reachable code | Raw bytes | Δ |
| --- | --- | ---: | ---: |
| p00 | floor | 1,005 | — |
| p27 | `script_sources_stage_seven_control_from_witness` + `decode_native_tx_compact_v1(compact).body.required_observers_hash` | 4,309 | +3,304 |
| p26 | control decode + `verify_native_tx_proof_source_v1` (pair) | 5,030 | +4,025 |
| p28 | p26 + `field_door.open_machine_field_item` + `machine_field_item_{count,length,bytes}` (`authenticated_whole_field_view`, `field_item_extent`, `field_read_range`) | 8,666 | +3,636 over p26 |
| p21 | control decode + `script_sources_stage_seven_control_is_bound` via **`exact_script_sources_control`** | 23,124 | +19,505 |
| p22 | same with the **narrow stage-seven encoder** (anchor §4a.2) | 7,846 | +4,227 |
| p25 | whole observer predicate, narrow encoder for binding **and** successor (no shell) | 15,781 | — |
| **q24** | **resolver-shaped monolith after the prune** (`cancel` + `continue_winning` + p25, 3 params) | **18,097** | **over** |
| q40 | p25 as one yield (`withdraw`, `unique_dispatch`) | 16,447 | over |
| q15 | **door half** only: control decode, `verify_native_tx_proof_source_v1`, door open, item facts vs. claimed `(observer_hash, active_count)` | 9,016 | — |
| q16 | **binding half** only: narrow stage-seven binding + receive-scan pins + order check/rejection + narrow successor given the claimed facts | 9,725 | — |
| **q41 / q42** | **the two halves as yields** (`withdraw`, `unique_dispatch`; item yield has the certificate-policy parameter) | **9,665 / 10,203** | fits |
| q57 | **pure dispatcher**: typed redeemer, two `require_authenticated_zero_yield`, `continue_winning(True)` (3 params) | 4,116 | fits |

Reading: `exact_script_sources_control` is 60 % of the body (23,124 → 7,846
when replaced by the narrow encoder that only knows stage 7 and its stage-8
successor). The remainder — §8 door (3.6 KB), native-tx proof source (1.4 KB),
the 40-clause stage-seven binding (4.2 KB), control decode (2.6 KB), successor
(≈ 1.5 KB) — is all genuinely reachable and sums to ≈ 15.8 KB before the
resolver shell, so the observer cannot be a monolith or a single yield.

## 3. Options considered

| Option | Verdict | Reason |
| --- | --- | --- |
| 1. Prune (narrow encoder for binding and successor) | rejected as sufficient, **kept as prerequisite** | 18,097 measured (q24) |
| 2a. One yield with the whole predicate | rejected | 16,447 (q40) |
| 2b. **Two yields by fact — door half (item facts) and binding half (control binding + successor) — pure dispatcher** | **chosen** | 9,665 + 10,203 + 4,116; both halves read the same dispatcher redeemer, exchanging `(observer_hash, active_count)` as claimed facts |
| 2c. One yield for the door half only, binding in the dispatcher | rejected | dispatcher would be ≈ 4.1 + 9.7 − 1.6 ≈ 12.2 KB — fits, but the binding half is *exactly* what `seven-receive`/`seven-finish` also compute, and keeping it in a yield leaves the door free to grow (tier-3 carriage changes) without touching the resolver |
| 3. Chain (door step then binding step) | rejected | two transactions per observer item against C52 for a size problem two yields solve in one transaction |
| 4. Redesign | rejected | the observer/receive/finish partition is right (R5) |

## 4. Chosen design

### 4a. Library

Anchor §4a.2 (`script_sources_stage_seven_exact_control`,
`script_sources_stage_seven_successor_is_exact`, narrow
`script_sources_stage_seven_control_is_bound`). New in `validation-machine-v1.ak`:

```
pub fn verify_script_sources_stage_seven_observer_item_facts_v1(pre, witness, door, field_index, item_index, carriage, claimed_observer_hash, claimed_active_count) -> Bool   // yield A (q15)
pub fn verify_script_sources_stage_seven_observer_bound_semantics_v1(pre, witness, observer_hash, active_count) -> Bool                                              // yield B (q16)
```

Both must agree with `verify_script_sources_stage_seven_observer_semantics_v1`:
`A(…, h, n) && B(…, h, n)` ⇔ the monolith on the same `(door, carriage)` with
`h = machine_field_item_bytes(item)`, `n = active_count` (§8 property).

### 4b. New validator list

| Validator | Purpose | File | Params |
| --- | --- | --- | --- |
| `script_sources_stage_seven_observer_semantic_v1.main.spend` (**dispatcher**, same title) | narrow redeemer, two yield handshakes, `continue_winning(…, True, …)` | existing file | `award_script_hash`, `computation_thread_policy_id`, **`reference_script_auth_policy_id`** (the certificate policy **moves to yield A**; count stays 3) |
| `script_sources_stage_seven_observer_item_yield_v1.main.withdraw` (**yield A**, new) | §8 door: field-3 item facts | `validators/fraud-proofs/validation-trace/script-sources-stage-seven-observer-item-yield-v1.ak` | `observer_dispatcher_script_hash: ScriptHash`, `field_preimage_certificate_policy_id: PolicyId` |
| `script_sources_stage_seven_observer_bound_yield_v1.main.withdraw` (**yield B**, new) | stage-seven binding, order check, successor | `…/script-sources-stage-seven-observer-bound-yield-v1.ak` | `observer_dispatcher_script_hash: ScriptHash` |

Roles (`validation-script-sources-yield-v1.ak`, anchor §4c):
`stage_seven_observer_item_role = "V1VtSsS07ObserverItemYield"`,
`stage_seven_observer_bound_role = "V1VtSsS07ObserverBoundYield"`; helper
`unique_stage_seven_observer_dispatch(dispatcher_hash, inputs, redeemers) -> (Datum, ObserverActionV1)`.

### 4c. Redeemer ABI delta

```
VerifyObserver {
  input_index: Int, output_index: Int, transition: ValidationOneStepWitnessV1,
  field_index: Int, item_index: Int, carriage: FieldCarriageV1,     // unchanged (auxiliary)
  claimed_observer_hash: ByteArray,   // NEW — the 28-byte item yield A opens
  claimed_active_count: Int,          // NEW — the field's item count (or the carried total)
  yield_to_ref_input_index: Int,      // NEW — yield A at this index, yield B at index + 1
}
```

Auxiliary `TransactionFieldChunkWitness { field_index, item_index, carriage }`
unchanged → evidence hash, prepare and `VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk = [1, 3]`
unchanged. Datum and `ct.Cancel` unchanged.

### 4d. Exact handshake

Dispatcher (`ct.Continue(VerifyObserver {...})`, measured as q57):

1. `let item_yield = require_authenticated_zero_yield(tx.reference_inputs, tx.withdrawals, tx.redeemers, reference_script_auth_policy_id, stage_seven_observer_item_role, yield_to_ref_input_index)`;
   `let bound_yield = require_authenticated_zero_yield(…, stage_seven_observer_bound_role, yield_to_ref_input_index + 1)`;
   both used (`bytearray.length(x) == 28`) in the final `and`.
2. `continue_winning(ScriptSources, award_script_hash, computation_thread_policy_id, datum, input_index, output_index, transition, TransactionFieldChunkWitness{…} as Data, True, own_out_ref, tx)`.

Yield A (`withdraw`, params dispatcher hash + certificate policy; q41):
`unique_stage_seven_observer_dispatch`; `door = MachineFieldDoorV1 { reference_inputs: tx.reference_inputs, certificate_policy_id }`;
`control = script_sources_stage_seven_control_from_witness(transition.work_witness_cbor)`;
`Pair(verified, witness_set) = verify_native_tx_proof_source_v1(pre.transaction_id, control.compact_cbor, control.witness_set_compact_cbor, control.field_preimage_lengths_cbor)`;
`observer_commitment = verified.tx_compact.body.required_observers_hash`;
`item = open_machine_field_item(door, verified, witness_set, 3, control.observer_scan.seen, carriage)`;
`active_count = if observer_scan.total_count == 0 { machine_field_item_count(item) } else { observer_scan.total_count }`;
`and { observer_commitment != empty_field_commitment, !script_sources_stage_seven_observer_scan_is_complete(control, observer_commitment), active_count > 0, active_count <= max_tx_size_derived_collection_item_count, field_index == 3, item_index == observer_scan.seen, machine_field_item_count(item) == active_count, machine_field_item_length(item) == 28, claimed_active_count == active_count, claimed_observer_hash == machine_field_item_bytes(item) }`.

Yield B (`withdraw`, param dispatcher hash; q42):
`unique_stage_seven_observer_dispatch`; `control = script_sources_stage_seven_control_from_witness(...)`;
`common = and { script_sources_stage_seven_control_is_bound(pre, transition, control) /* narrow */, control.output_cursor == 0, receive_scan.receive_count == 0, receive_scan.previous_hash == #"", receive_scan.candidate_hash == #"", bytearray.length(claimed_observer_hash) == 28, claimed_active_count > 0, observer_scan.seen < claimed_active_count }`;
if `common && observer_scan.seen > 0 && compare(observer_scan.previous_hash, claimed_observer_hash) != Less` →
`rejected_successor_is_exact(pre, transition.claimed_successor, reject_invalid_field_type)`; else
`common && script_sources_stage_seven_successor_is_exact(pre, transition, ScriptSourcesControlV1 { ..control, purpose_count + 1, purpose_peaks: append_leaf(purpose_count, purpose_peaks, purpose_leaf_hash(2, observer_scan.seen, h, h)), observer_scan: { total_count: claimed_active_count, seen: seen + 1, previous_hash: h } })`.

### 4e. Security argument

- **Dispatch uniqueness.** Both yields require a singleton input at the
  observer dispatcher credential and read its `Spend` redeemer; one withdrawal
  cannot discharge two observer threads; A and B see the same
  `(claimed_observer_hash, claimed_active_count, transition, carriage)` bytes.
- **Role authentication.** Two role names, two reference inputs at
  `yield_to_ref_input_index` / `+ 1`, each with exactly one role token under
  `reference_script_auth_policy_id`, an exact zero withdrawal from its script
  hash and a unique withdraw redeemer. Substituting A for B fails on the role
  name; a foreign script fails on the withdrawal credential.
- **Cross-arm substitution.** Neither role is referenced by any other family;
  the redeemer-scan yield uses a three-hash `unique_dispatch` that excludes the
  observer dispatcher's credential.
- **Output-state re-derivation.** The dispatcher checks the continuation
  (`continue_winning`); yield B re-derives the machine successor from
  `control` (bound to `pre.work_root` by the narrow binding of the *same*
  `transition`) and the claimed facts; yield A binds the claimed facts to the
  authenticated field preimage (`verified` is bound to
  `pre.transaction_id`/`transaction_commitment` through
  `verify_native_tx_proof_source_v1` and `native_tx_proof_commitment_v1` in B's
  binding of the same control).
- **If yield A is omitted:** the dispatcher's first role check fails; if it
  were somehow skipped, B would accept any 28-byte `claimed_observer_hash` — this
  is why the dispatcher requires **both** roles and §8 has an omission vector
  for each. **If yield B is omitted:** no successor is checked; the second role
  check fails. **If `claimed_active_count` is forged:** A refuses (item count
  or carried total). **If the carriage is forged:** it is in the evidence hash
  and the door authenticates the preimage against the committed field hash.
  **If the order is violated:** B derives the exact rejection; a challenger
  cannot claim acceptance instead because the successor branch requires
  `previous_hash < h`.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +110 / +73 / +37) | Signed publication (≈ +276; yields mint one role NFT each) |
| --- | ---: | ---: | ---: |
| observer dispatcher (q57, 3 params) | 4,116 | ≈ 4,226 | ≈ 4,500 |
| observer item yield A (q41, 2 params) | 9,665 | ≈ 9,740 | ≈ 10,020 — fits |
| observer bound yield B (q42, 1 param) | 10,203 | ≈ 10,240 | ≈ 10,520 — fits |

Referenced bytes per semantic-resolution transaction ≈ 24,200 (plus the tier-2/3
carriage reference inputs the door already reads, which are data UTxOs, not
scripts) — first 25 KiB tier, ≈ 1.4 KB from the boundary (fee only).
ExUnits unmeasured: the door's whole-field hash (`authenticated_whole_field_view`,
≤ `max_transaction_aggregate_field_bytes`) runs once in A exactly as today; B
adds one control re-encoding it already paid inside `exact_script_sources_control`;
two extra redeemer parses. Measure with `MIDGARD_PRINT_PROOF_FIT=1` (§7).

## 6. Off-chain work

Does not exist today: everything in anchor §6 for resolver 8, plus the two
yields, their roles, entries and stake registrations, and the observer
dispatcher's parameter swap.

- **SDK contracts**: `semanticResolverParameterValues` already has
  `field_preimage_certificate_policy_id`; add `reference_script_auth_policy_id`
  (once). Titles `yields.scriptSourcesStageSevenObserverItem` /
  `…ObserverBound`; build A with `[semanticResolvers[57].spendingScriptHash, fieldPreimageCertificatePolicyId]`
  and B with `[semanticResolvers[57].spendingScriptHash]`.
- **Roles**: `"V1 validation-trace script-sources stage-seven observer item yield": "V1VtSsS07ObserverItemYield"`,
  `"… observer bound yield": "V1VtSsS07ObserverBoundYield"` in both vocabularies (anchor §6).
- **Manifest / deployment info**: entries
  `validationTraceDisputeScriptSourcesStageSevenObserverItemWithdraw`,
  `…ObserverBoundWithdraw` (`withdrawalDescriptor`, `manifestReferenceScriptTarget`,
  step-name map) and the dispatcher's plain entry
  `25: "validationTraceDisputeScriptSourcesStageSevenObserverSemantic"`.
- **Stake registration**: two `register.Stake` lines (`initialization.ts:294–320` pattern); emulator setup analogue.
- **Submit route**: `semanticActionFieldsV1` for semantic 25 emits
  `[input_index, output_index, transition, field_index, item_index, carriage, claimed_observer_hash, claimed_active_count, yield_to_ref_input_index]`;
  the two claimed facts come from the honest trace's door read (the TypeScript
  producer, anchor §7); `readFrom([itemYieldUtxo, boundYieldUtxo])` at
  consecutive positions and two zero `withdraw`s. Tier-2/3 carriages already
  add their own reference inputs (`carriageMaterial`, #600); the yield index
  must be computed after them via `requireReferenceInputIndex`.
- **Funding**: two authenticated yield publications (≈ 10.0 and 10.5 KB) and
  one plain dispatcher publication (≈ 4.5 KB).
- **Codec**: none beyond the action encoder.

## 7. Emulator scenario tests

Exists today: nothing reaching stage seven (anchor §7). Add
`tests/submit-init-emulator-script-sources-stage-seven-observer-v1.test.ts`:
fixture from the anchor's TypeScript producer with an honest transaction
carrying **two required observers** (ascending) so `seen == 0` (count fixed
from the field) and `seen == 1` (carried total) are both reachable, plus a
descending pair for the rejection journey; `disputedStep` selects
`stage == 7 && observer_scan.seen < total_count`. Publication fit for the
dispatcher (plain) and both yields (`publishAuthenticatedValidationDisputeControl`
targets added to the roster) without `oversized`, `l1ByteMargin > 0`;
positive lifecycle to award for a tier-1 carriage and for the tier-2
`RawUtxo` carriage (`carriageMaterial`); valid-block negatives: forged
`claimed_observer_hash` (A refuses), yield A omitted, yield B omitted, roles
swapped (A's token on B's index), forged successor with a wrong `total_count`;
cancel; maximum shape: `max_tx_size_derived_collection_item_count` observers
with the field-3 preimage at the tier-2 boundary.

## 8. Aiken tests

Keep `script_sources_stage_seven_observer_binds_canonical_successor_encoding`
(`validation-machine-v1.test.ak:7190`). Add, in a new
`validators/fraud-proofs/validation-trace/script-sources-stage-seven-split-v1.test.ak`:
`observer_wire_layout_is_pinned`, `observer_dispatcher_wins_with_both_yields`,
`observer_item_yield_accepts_first_item_and_fixes_total`,
`observer_item_yield_accepts_carried_total`,
`observer_bound_yield_appends_purpose_leaf_exactly`,
`observer_bound_yield_rejects_descending_order_exactly`; negatives (`fail`):
`_refuses_missing_item_yield`, `_refuses_missing_bound_yield`,
`_refuses_swapped_roles`, `_refuses_forged_observer_hash`,
`_refuses_forged_active_count`, `_refuses_receive_scan_started`,
`_refuses_two_dispatcher_inputs`, `_refuses_withdrawal_script_substitution`.
Library property: `script_sources_stage_seven_observer_split_agrees_with_the_aggregate`
(A ∧ B ⇔ monolith over fuzzed carriages/orders) and
`script_sources_stage_seven_exact_control_agrees_with_exact_script_sources_control`
for stage-7 and stage-8 controls.

## 9. Verification commands

Anchor §9 (dispatcher ≈ 4,116, yields ≈ 9,665 / 10,203 in the 15-line sweep;
`aiken check -m stage_seven`) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-seven-observer-v1.test.ts`.

## 10. Ordering and dependencies

- Both yields land before the dispatcher (their parameter is its hash; the
  dispatcher depends only on the two role constants).
- Shares anchor §4a.2 with `seven-receive` and `seven-finish` (the narrow
  binding is yield B's; the same function serves those two monoliths).
- `field_preimage_certificate_policy_id` leaves the semantic resolver's
  parameter list and enters yield A's — `zz605` must see the new arity;
  `inspect-contracts.test.ts` pins the catalogue root once for the regeneration.

## 11. Risks

- **Two-yield fact exchange:** the only new trust surface is the claimed
  `(hash, count)` pair; both yields are mandatory and both read the same
  redeemer — the omission vectors are load-bearing.
- **Door growth:** tier-3 carriage changes (#600/#617) land in yield A only;
  its 4.9 KB margin absorbs them.
- **Fee tier:** ≈ 24.2 KB referenced vs. the 25 KiB tier boundary.
- **ABI churn:** parameter swap on the resolver, three new redeemer fields, two
  new withdraw validators, two roles, three deployment entries.
- **Discarded-binding hazard** (anchor §11) applies twice here.
