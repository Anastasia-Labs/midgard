# Size-fit plan: `script_sources_output_proof_step_semantic_v1`

Cites [00-primer.md](00-primer.md) and the shared raw stage-frame library and
semantic-yield handshake defined in
[validation-trace-script-sources-non-output-semantic-v1.md](validation-trace-script-sources-non-output-semantic-v1.md)
§4.1–4.2. This plan defines the **shared ledger-output-proof (LOP) yield
family** that the output-proof begin/finalize/finish plans and the
resolve-inputs membership plans reference.

## 1. Identity

| Field                  | Value                                                                                                                                                                                                                                                                                                                                                                                                                             |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title        | `fraud_proofs/validation_trace/script_sources_output_proof_step_semantic_v1.main.spend`                                                                                                                                                                                                                                                                                                                                           |
| File                   | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-output-proof-step-semantic-v1.ak` (83 lines)                                                                                                                                                                                                                                                                                                               |
| Raw size               | **82,309 bytes**                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Applied parameters     | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId`                                                                                                                                                                                                                                                                                                                                                         |
| Phase / index          | `ScriptSources` (8), semantic slot **2** of 29, global index 34                                                                                                                                                                                                                                                                                                                                                                   |
| Library entry point    | `verify_script_sources_output_proof_step_semantics_v1(pre, transition, proof_witness)` → `script_sources_control_from_witness` + `script_sources_control_is_bound` + `!terminal_is_exact_v1(output_proof)` + `script_sources_output_proof_step` (→ `ledger_output_proof_v1.step_v1`, then `script_sources_control_successor_is_exact` or `rejected_successor_is_exact` for the four `LedgerOutputProofInvalid*`/`*Limit` results) |
| Redeemer               | `VerifyOutputProofStep { input_index, output_index, transition, proof_witness: LedgerOutputProofWitnessV1 }`; auxiliary hashed as `LedgerOutputProofStepWitness { witness }` (constructor 32)                                                                                                                                                                                                                                     |
| Role name today        | none                                                                                                                                                                                                                                                                                                                                                                                                                              |
| Deployment entry today | none (title in `contracts.ts` only; `submit.ts` `auxiliaryShapeV1` pins `ledgerOutputProofStep` for `semanticResolverIndex === 2`, `semanticActionFieldsV1` flattens `[…base, witness]`)                                                                                                                                                                                                                                          |

## 2. Why it is this size

Same probe copy and shell as the non-output plan (shell 3,349).

| Probe                             | Adds                                                                                                                                                                             |                                           Raw bytes |                                                     Delta |
| --------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------: | --------------------------------------------------------: |
| p03                               | `script_sources_control_from_witness` (includes the LOP control codec)                                                                                                           |                                              25,595 |                                                   +22,246 |
| p09                               | `ledger_output_proof_v1.decode_control_v1` alone (= `control_from_data` + `control_is_well_formed` + `encode_control_v1` round-trip)                                             |                                              21,801 |                                               **+18,452** |
| c06                               | raw 12-item LOP control parse (`un_list_data`, no sub-control)                                                                                                                   |                                               4,551 |                                                    +1,202 |
| c01 / c02 / c03 / c04 / c05       | sub-control codecs: `ledger_output_scan_v1` / `ledger_output_value_v1` / `cek_data_traverse_v1` / `blake2b_224_trace_v1` / `native_script_scan_v1` structure `decode_control_v1` |              7,277 / 5,982 / 11,644 / 5,659 / 5,461 |            +3,928 / +2,633 / **+8,295** / +2,310 / +2,112 |
| p10                               | p09 + `step_v1` (all six stages)                                                                                                                                                 |                                              67,409 |                                               **+45,608** |
| p11 / p12 / p13 / p14 / p15 / p16 | p09 + `structure_step` / `value_fold_step` / `datum_traversal_step` / `reference_script_commitment_step` / `script_hash_step` / `native_script_step`                             | 32,394 / 29,160 / 48,383 / 26,113 / 28,923 / 30,274 | +10,593 / +7,359 / **+26,582** / +4,312 / +7,122 / +8,473 |
| p24b                              | `cek_data_traverse_v1.step_v1` (generic, 9 actions)                                                                                                                              |                                              36,223 |                                                   +32,874 |
| p22                               | the deployed predicate (`script_sources_output_proof_step` on the typed control)                                                                                                 |                                              76,580 |                                                         — |
| p41                               | raw 31-item frame, extension splice to a redeemer-supplied `next_control_cbor`, no control decode                                                                                |                                               5,865 |                                                    +2,516 |
| p42                               | p41 + `require_authenticated_zero_yield`                                                                                                                                         |                                           **6,326** |                                                    +2,977 |
| y00                               | yield shell                                                                                                                                                                      |                                               1,663 |                                                         — |
| y10                               | yield: LOP codec + `step_v1` + `encode_control_v1` + rejected-successor arms                                                                                                     |                                              66,630 |                                                         — |
| y11 / y12 / y13 / y14 / y15 / y16 | yield per stage on the typed LOP control                                                                                                                                         | 31,454 / 28,283 / 47,605 / 25,252 / 27,996 / 29,333 |                                                         — |
| y30                               | yield prototype: raw LOP items + scan codec + `ledger_output_scan_v1.step_v1` over one verified chunk + splice of item 5                                                         |                                          **12,914** |                                                         — |

Dominators: `ledger_output_proof_v1.step_v1` (45.6 KB) of which the datum
traversal stage alone is 26.6 KB (generic `cek_data_traverse_v1.step_v1`);
the LOP control codec (18.5 KB, reachable through every typed path, including
each per-stage yield prototype y11–y16, which is why they all exceed 25 KB);
the ScriptSources generic parser/encoder (~20 KB each).

## 3. Options considered

- **Prune (1):** removing the generic ScriptSources parser/encoder (raw frame)
  saves ~40 KB but leaves `step_v1` + LOP codec at ≥64 KB. Applied inside the
  design, not sufficient alone.
- **Yield split (2) — chosen**, with two refinements forced by the numbers:
  (a) one yield per LOP stage (six stages), because any yield carrying
  `step_v1` is ≥64 KB; (b) every yield decodes **only its own sub-control**
  from the raw 12-item LOP control and splices the successor control bytes,
  because the full LOP codec alone (18.5 KB) exceeds the target; (c) the
  datum-traversal stage is split by traversal action family exactly as RF-021
  split the stage-one redeemer traversal (generic traverse step is 32.9 KB).
- **Multi-transaction chaining (3):** not needed for size — each step is one
  machine step and one yield fits a transaction. Considered for the datum
  stage (RF-021-style envelope → normalizer → executor) and rejected: the
  yield handshake gives the same isolation in one transaction without adding
  four hops per datum node to the C52 count.
- **Redesign (4):** not applicable; the LOP stage machine is already the
  right decomposition.

## 4. Chosen design

### 4.1 Dispatcher (rewritten `script-sources-output-proof-step-semantic-v1.ak`)

```aiken
pub type ActionV1 {
  VerifyOutputProofStep {
    input_index: Int, output_index: Int,
    transition: ValidationOneStepWitnessV1,
    proof_witness: Data,          // LedgerOutputProofWitnessV1, decoded only by the selected yield
    next_control_cbor: ByteArray, // successor LOP control, attested by the yield
    yield_role_index: Int,        // 0..10 into the LOP role table
    yield_ref_input_index: Int,
  }
}
validator main(award_script_hash, computation_thread_policy_id, reference_script_auth_policy_id: PolicyId)
```

Semantic predicate (measured shape p42): `frame = open_frame_v1(pre,
transition, 31, 5)`; `control_cbor = item_bytes_v1(frame, 30)`; require
`slice(len − len(enc(control_cbor)), …) == encode_definite_bytes(control_cbor)`;
`next = replace_extension_v1(witness, enc(control_cbor), enc(next_control_cbor))`;
`successor_is_exact_v1(pre, transition, next)` **or**, when the yield attests a
rejection, `claimed_successor.phase == Terminal` (the yield checks the exact
rejected successor); `require_semantic_yield_v1(tx, policy, lop_role(yield_role_index),
yield_ref_input_index)`. Auxiliary hashed into evidence:
`builtin.constr_data(32, [proof_witness])`, byte-identical to today's
`LedgerOutputProofStepWitness { witness }`. The dispatcher never decodes the
LOP control, the witness, or the ScriptSources control record.

### 4.2 Shared LOP yield family (new files `validators/fraud-proofs/validation-trace/ledger-output-proof-*-yield-v1.ak`)

Parameter for every yield: `dispatcher_script_hashes: List<ScriptHash>` —
this ScriptSources step dispatcher **and** the ResolveInputs
`resolve_inputs_membership_step_semantic_v1` dispatcher (its plan reuses these
yields unchanged; same `ledger_output_proof_v1.step_v1` semantics, different
carrier control). Each yield: `unique_semantic_dispatch_v1` → `(state,
transition, extra)`; reads `control_cbor` (= the carrier's extension item,
passed in `extra` by the dispatcher redeemer), `next_control_cbor` and the
raw `proof_witness`; parses the 12 LOP items (1,202 B); decodes only its
sub-control; runs the stage step; re-encodes the changed items and splices
`control_cbor` → requires equality with `next_control_cbor`; for a rejecting
result requires `rejected_successor_is_exact(pre, claimed_successor, code)`.

| idx | Role                             | Yield                                | LOP stage / action                                                                                                                        | Sub-control decoded                  | Basis                                                                                   |                                                         Projected |
| --: | -------------------------------- | ------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------ | --------------------------------------------------------------------------------------- | ----------------------------------------------------------------: |
|   0 | `V1VtLopStructureYield`          | `…-structure-yield-v1.ak`            | `stage_structure` (scan step, `finish_v1`, terminal → `stage_value_fold` with template `ledger_output_value_v1.initial_control_v1` bytes) | scan                                 | y30 measured 12,914 (+ two-chunk window, `finish_v1`, value-initial template ≈ +1.5 KB) |                                         **≈ 14,400 (borderline)** |
|   1 | `V1VtLopValueYield`              | `…-value-yield-v1.ak`                | `stage_value_fold` (`ledger_output_value_v1.step_v1`, terminal → datum initial template)                                                  | value                                | y00 + items + c02 + p12 delta 7,359 − traverse-initial encode replaced by template      |                                                          ≈ 10,500 |
|   2 | `V1VtLopDatumFoldMapYield`       | `…-datum-fold-map-yield-v1.ak`       | datum `FoldMap` via `prevalidated_fold_map_next_frame_root_v1` + frame-root template (RF-021 fold-map executor: 7,519)                    | traverse (template splice, no codec) | RF-021 executor + yield shell + items                                                   |                                                           ≈ 9,000 |
|   3 | `V1VtLopDatumFinalizeFrameYield` | `…-datum-finalize-frame-yield-v1.ak` | datum `FinalizeFrame` via `prevalidated_finalize_frame_transition_v1` (RF-021: 9,290)                                                     | traverse (template)                  | as above                                                                                |                                                          ≈ 10,500 |
|   4 | `V1VtLopDatumHeadYield`          | `…-datum-head-yield-v1.ak`           | `HeadScalar`, `HeadSequence`, `HeadMap`, `HeadLargeConstructor` (source bytes from the output chunk window)                               | traverse codec (c03 8,295)           | y00 + items + c03 + chunk window ~2 KB + head steps ~2–3 KB                             | **≈ 14,500 (borderline; split into scalar/sequence-map if over)** |
|   5 | `V1VtLopDatumAttachScalarYield`  | `…-datum-attach-scalar-yield-v1.ak`  | `AttachScalar` (+ integer/bytes sub-steps of `cek_data_integer_v1`/`cek_data_bytes_v1`)                                                   | traverse codec                       | y00 + items + c03 + ~3 KB                                                               |                                                          ≈ 14,000 |
|   6 | `V1VtLopDatumFoldListYield`      | `…-datum-fold-list-yield-v1.ak`      | `FoldList`                                                                                                                                | traverse (template like FoldMap)     | RF-021 analogue                                                                         |                                                           ≈ 9,000 |
|   7 | `V1VtLopDatumAdvanceYield`       | `…-datum-advance-yield-v1.ak`        | `NoAction` transitions and datum terminal → `stage_reference_script_commitment`                                                           | traverse codec                       | y00 + items + c03 + ~1 KB                                                               |                                                          ≈ 12,000 |
|   8 | `V1VtLopRefScriptYield`          | `…-reference-script-yield-v1.ak`     | `stage_reference_script_commitment` (chunk-hash frontier append; terminal → `script_hash`/`native_script` initial templates)              | none beyond items                    | y00 + items + p14 delta 4,312 + templates                                               |                                                           ≈ 8,000 |
|   9 | `V1VtLopScriptHashYield`         | `…-script-hash-yield-v1.ak`          | `stage_script_hash` (`blake2b_224_trace_v1.step_v1` over chunk window)                                                                    | blake (c04 2,310)                    | y00 + items + c04 + p15 delta 7,122                                                     |                                                          ≈ 12,300 |
|  10 | `V1VtLopNativeScriptYield`       | `…-native-script-yield-v1.ak`        | `stage_native_script` (`structure_token_step_v1` / `structure_frame_step_v1`, node/depth limits → rejections)                             | native (c05 2,112)                   | y00 + items + c05 + p16 delta 8,473                                                     |              **≈ 13,500 (borderline; split token/frame if over)** |

Library changes in `lib/midgard/ledger-output-proof-v1.ak`: expose the six
stage steps as `pub fn` taking `(raw_items, sub_control, witness)` variants
that return the changed sub-control bytes instead of a full
`LedgerOutputProofControlV1` (`structure_step_raw_v1`, `value_fold_step_raw_v1`,
…), plus `initial_value_control_cbor_v1(asset_count)`,
`initial_datum_control_cbor_v1(offset, length)`, `initial_script_hash_control_cbor_v1(len)`,
`initial_native_structure_control_cbor_v1(start, end)` template encoders pinned
against `encode_control_v1(initial_control_v1(…))` by golden tests. The
existing typed `step_v1` stays for the machine tests and the monolith-era
callers until they are removed.

**Security argument.** _Dispatch uniqueness / role authentication:_ as in the
non-output plan (`unique_semantic_dispatch_v1`, `require_authenticated_zero_yield`).
_Cross-arm substitution:_ every yield requires `un_i_data(items[1]) ==
its_stage` (and, for datum yields, the traversal action tag it owns), so a
structure yield presented for a value-stage control fails inside the yield,
and the dispatcher's role table forbids presenting a role it does not list.
_Output-state re-derivation:_ the yield recomputes `next_control_cbor` from
the pre control and the witness; the dispatcher recomputes
`claimed_successor.work_root` from `next_control_cbor` spliced into the
canonical witness; `continue_winning` pins the thread output. Neither side
trusts `next_control_cbor` — it is a communication channel checked on both
ends. _Omission:_ no yield → `require_authenticated_zero_yield` fails; a yield
whose `dispatcher_script_hashes` does not list the spending script cannot find
its dispatcher input. _Two dispatchers in one transaction_ (this one and the
resolve-inputs one) are refused by the singleton filter, so one withdrawal
never attests two threads. _Rejection arm:_ a yield attesting
`LedgerOutputProofInvalidOutput` checks `rejected_successor_is_exact(pre,
claimed_successor, reject_invalid_output)` itself; the dispatcher only
requires `claimed_successor.phase == Terminal` in that mode, so the yield —
not the prover — chooses the rejection code.

## 5. Size and budget projection

Dispatcher **≈ 6,400** (p42 6,326 + role table). Yields as tabled: 8,000–14,500
each; three are borderline and get a pre-merge measurement gate (§9). Per
transaction: dispatcher + exactly one yield = **≤ 21 KB referenced** (today
82,309), tier 0 of the Conway reference-script fee (≤ 0.32 ADA vs ≈ 1.54 ADA
today: tiers 0–2 full = 1,397,760 lovelace + 5,509 B × 25.92). Aggregate
ExUnits: two executions parse the dispatcher redeemer (transition + witness);
the yield additionally decodes one sub-control and the LOP witness. No
measured ExUnits exist for resolver 8 (fit sweep `unmeasured`); §7 measures.
Method: probe deltas; y30 is the one measured yield prototype.

## 6. Off-chain work

None of the following exists today for this contract.

- `contracts.ts`: `ledgerOutputProofYields` record (11 withdraw validators)
  applied with `[[scriptSourcesOutputProofStep.spendingScriptHash, resolveInputsMembershipStep.spendingScriptHash]]`;
  the step resolver gains `reference_script_auth_policy_id` (name-keyed
  parameter map, `zz605` gate).
- Roles: eleven `V1VtLop…Yield` names in `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES`
  and `midgard-core`'s token-name vocabulary; Aiken constants in
  `lib/midgard/validation-semantic-yield-v1.ak`.
- Deployment entries: `validationTraceDisputeScriptSourcesOutputProofStepSemantic`,
  `validationTraceDisputeLop{Structure,Value,DatumFoldMap,DatumFinalizeFrame,DatumHead,DatumAttachScalar,DatumFoldList,DatumAdvance,RefScript,ScriptHash,NativeScript}Yield`;
  reward-account registration for eleven stake credentials.
- Submit route: `semanticActionFieldsV1` (resolver 8, index 2) emits
  `[input, output, transition, proof_witness, next_control_cbor, yield_role_index, yield_ref_input_index]`;
  the builder adds `.readFrom([yieldUtxo])` + `.withdraw(yieldRewardAddress, 0n, …)`.
  New pure helper `ledgerOutputProofYieldRoleIndexV1(controlCbor, witness)`
  (decode the LOP control off-chain, pick the stage/action role) and
  `nextLedgerOutputProofControlCborV1` from the evidence builder (it already
  computes the successor control to build the successor work witness).
- Inspection fixtures: add the eleven yield step names.
- Funding rows: twelve publications + eleven reward deposits.
- Codec: one-step argument gains `resolverHints.nextControlCbor` and
  `resolverHints.yieldRoleIndex`; `VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1` unchanged.

## 7. Emulator scenario tests

Exists today: nothing reaches resolver 8 (see the non-output plan §7).

Add `tests/submit-init-emulator-script-sources-output-proof-v1.test.ts`
(covers this plan and the begin/finalize/finish plans in one journey file;
the stage-5 chain is one contiguous run of machine steps):

1. Publication fit for the four stage-5 resolvers and the fifteen LOP/desc
   yields without `oversized`, under `withRealL1MaxTxSize`.
2. Positive lifecycle: `buildForgedOperatorSuccessorValidationDisputeFixture({ disputedPhase: "scriptSources", disputedStep: { stage: 5, lopStage } })`
   for each LOP stage (structure, value, datum FoldMap/FinalizeFrame/Head/AttachScalar/FoldList/Advance, refscript, script-hash, native) over a fixture output built with
   `largeFittingOutputCbor` extended to carry two assets, an inline datum with
   a nested map and a native reference script (`encode_midgard_versioned_script`
   fixtures from `native-binding-fixture-v1`); through award and removal;
   assert `completeSignedBytes ≤ 16,384` and `exUnits.mem ≤ 13,200,000` per transaction.
3. Valid-block negative: honest operator successor at the same frontier →
   `expectOnchainRefusalV1` (yield rejects the forged `next_control_cbor`).
4. Rejection arms: a malformed output (`LedgerOutputProofInvalidOutput`) and a
   native reference script over the node limit
   (`LedgerOutputProofNativeScriptNodeLimit`) prove the rejecting terminal.
5. Cancel/resume at the prepared step (pattern of
   `submit-init-emulator-canonical-decodability-cancel-resume.test.ts`).
6. Maximum shape: a 16,384-byte output preimage (two-chunk windows on every
   chunk-reading stage) and a datum at the maximum nesting the traversal
   supports (`cek-data-traverse.max-cardano.test.ak` vectors).

## 8. Aiken tests

- `lib/midgard/ledger-output-proof-v1.test.ak`: for every stage, property
  test `raw_step_equals_typed_step_<stage>`: `encode_control_v1(step_v1(c, w))`
  equals the raw-yield splice for random well-formed controls (fuzz), including
  the rejecting results; golden pins for the four initial-control templates.
- `validators/fraud-proofs/validation-trace/ledger-output-proof-yields-v1.test.ak`
  (modelled on `cek-split-v1.test.ak`): `output_proof_step_wire_layout_is_pinned`,
  `prepare_routes_output_proof_step_to_slot_two`, one `<stage>_yield_wins_…`
  per role through `main.spend` + `withdraw`, negatives
  `dispatcher_refuses_a_missing_yield`, `dispatcher_refuses_cross_stage_role_substitution`
  (value role for a structure control), `dispatcher_refuses_withdrawal_script_substitution`,
  `yield_refuses_a_forged_next_control`, `yield_refuses_a_foreign_dispatcher`,
  `yield_refuses_two_dispatcher_inputs`, `native_yield_emits_node_limit_terminal`,
  `structure_yield_emits_invalid_output_terminal`, and
  `resolve_inputs_dispatcher_reuses_the_lop_yields` (the second listed dispatcher hash).

## 9. Verification commands

```bash
cd onchain/aiken && aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/(script_sources_output_proof_step_semantic_v1\.main\.spend|ledger_output_proof_.*_yield_v1\.main\.withdraw)$/.test(v.title)){const n=Buffer.from(v.compiledCode,"hex").length;console.log(v.title,n,n<=15000?"OK":"OVER")}'
# expected: 12 titles, all OK (gate: structure, datum-head, native-script yields must print OK before merge)
aiken check -m ledger_output_proof            # expected: existing 447-line suite + ≥ 12 new tests, 0 failures
aiken check -m ledger_output_proof_yields     # expected: ≥ 22 tests, 0 failures
cd ../../demo
pnpm --filter @al-ft/midgard-fault-proofs test -- tests/zz605-semantic-resolver-arity.test.ts tests/validation-dispute-submit.test.ts tests/inspect-contracts.test.ts
pnpm --filter @al-ft/midgard-fault-proofs test -- tests/submit-init-emulator-script-sources-output-proof-v1.test.ts
```

## 10. Ordering and dependencies

- Shares the LOP yield family with the **resolve-inputs membership step** plan
  (`resolve_inputs_membership_step_semantic_v1`, 72 KB group) — the yield
  parameter lists both dispatcher hashes, so both plans must land in the same
  regeneration; and with the output-proof finalize plan (descriptor yields,
  same library changes).
- Depends on the raw stage-frame library (non-output plan §4.1) and the
  yield handshake (§4.2).
- Reuses RF-021's `prevalidated_fold_map_next_frame_root_v1` /
  `prevalidated_finalize_frame_transition_v1` and the template technique of
  `script_sources_redeemer_normalization_v1.traversal_serialization_template_v1`
  for the datum yields; the stage-one redeemer plan extends the same library.

### 10.x Reconciliation notes from the resolve-inputs membership-step plan (2026-09-02)

- **Terminal-claim gap.** The dispatcher rule "`successor_is_exact_v1` or
  `claimed_successor.phase == Terminal`" lets a prover pair an honest
  `Advanced` yield verdict with a `Terminal` claimed successor, because the
  yield only checks `next_control_cbor` in advance mode. Adopt the
  resolve-inputs rule: the mode is encoded in the channel
  (`next_control_cbor == #""` ⇔ rejection) and checked on both ends.
- **Explicit `control_cbor`.** The shared yields cannot extract the LOP
  control from a carrier witness without that carrier's frame codec, so both
  dispatcher redeemers (this plan's and resolve-inputs membership step's)
  must carry `control_cbor` explicitly; add it to the redeemer listing above.
- Rejection-code mapping is identical in both carriers
  (`validation-machine-v1.ak` 6395–6448 vs 9318–9366), so no per-dispatcher
  mapping is needed in the yields.

## 11. Risks

- **Three yields projected within 1 KB of the target** (structure, datum
  head, native script). Fallback splits are named per row; the §9 size gate
  blocks merge otherwise.
- **Eleven new roles and reward accounts** for one resolver; operational
  surface grows. Mitigation: they are shared with resolve-inputs (no second
  set).
- **Datum-yield template correctness** depends on the RF-021 serialization
  template technique; the property tests in §8 compare against the typed
  `step_v1` on fuzzed controls and must stay property-based.
- **ExUnits**: unmeasured; the yield decodes the LOP witness (chunks up to
  the bounded-item chunk size) a second time. Measured in §7.
- **ABI churn**: redeemer gains three fields; `proof_witness` becomes `Data`
  on the wire (same bytes). Wire-layout pins regenerate once.
