# Size-fit plan: `script_sources_stage_one_redeemer_semantic_v1`

Cites [00-primer.md](00-primer.md), the shared raw stage-frame library
([non-output plan](validation-trace-script-sources-non-output-semantic-v1.md) §4.1)
and the RF-021 split chain that already exists for this stage.

## 1. Identity

| Field                         | Value                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| ----------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title               | `fraud_proofs/validation_trace/script_sources_stage_one_redeemer_semantic_v1.main.spend`                                                                                                                                                                                                                                                                                                                                                                                                                              |
| File                          | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-semantic-v1.ak` (88 lines)                                                                                                                                                                                                                                                                                                                                                                                                  |
| Raw size                      | **87,545 bytes**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Applied parameters            | `award_script_hash`, `computation_thread_policy_id`, `field_preimage_certificate_policy_id`                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Phase / index                 | `ScriptSources` (8), semantic slot **15** of 29 ("local 15"), global index 47                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Library entry point           | `verify_script_sources_stage_one_redeemer_semantics_v1(pre, transition, auxiliary, door)` = `script_sources_stage_one_redeemer_auxiliary_is_family(auxiliary)` (accepts `TransactionRedeemerItemBeginWitness` or `RedeemerItemStepWitness { redeemer_control: None }`) then the full `script_sources_stage_one` arm set (empty-redeemers → stage 2; complete → stage 2; begin via the door on field 8; item step via `redeemer_item_proof_v1.step_v1` incl. `RedeemerItemProofInvalid` → `reject_invalid_field_type`) |
| Redeemer                      | `VerifyRedeemer { input_index, output_index, transition, auxiliary: ValidationAuxiliaryWitnessV1 }`                                                                                                                                                                                                                                                                                                                                                                                                                   |
| Role / deployment entry today | none / none. `submit.ts` (`auxiliaryShapeV1`, resolver 8) routes `semanticResolverIndex === 15` for `transactionRedeemerItemBegin` **or** `redeemerItemStep`, and `=== 28` (the RF-021 envelope, global 90) only for `redeemerItemStep`; `encodeScriptSourcesStageOneSpendRedeemerV1` encodes the five split stages.                                                                                                                                                                                                  |

### RF-021 prior art (read in full)

`GOAL_REVIEW_REMEDIATION.md` row RF-021: "additive ScriptSources local 28 /
global 75 split route … The oversized local-15 monolith remains available for
its full action family. Heavy FoldMap/FinalizeFrame item steps dispatch
additively through envelope, traversal normalizer, outer normalizer, the
selected executor, settlement, and the existing award finalizer." Chain
(measured this build): `script-sources-stage-one-redeemer-envelope-v1.ak`
8,516 (params: `deployment_id`, four stage hashes, `settlement_script_hash`,
ct policy), `-traversal-normalizer-v1.ak` 11,871, `-outer-normalizer-v1.ak`
4,150, `-fold-map-executor-v1.ak` 7,519, `-finalize-frame-executor-v1.ak`
9,290, `-execution-settlement-v1.ak` 5,837 (params include
`expected_award_script_hash`), library
`lib/midgard/script-sources-redeemer-normalization-v1.ak` (879 lines, 39 tests).

What the split covers today — exactly: stage-1 `RedeemerItemStepWitness` with
`redeemer_control: None`, `RedeemerItemProofWitnessV1 { action:
RedeemerItemTraverseData { action }, chunk_proof: None, next_chunk_proof: None }`
where `action ∈ { FoldMap (tag 7, 6 fields), FinalizeFrame (tag 8, 2 fields) }`
(`canonical_action_hash_v1` pins auxiliary tag 18, witness tag 0, item-action
tag 2, both chunk proofs `None`, `action_family ∈ {0, 1}`), with the item
control in stage `stage_fold` and a `Some(traversal)`.

What still has **no split route** and is served only by the monolith:
(a) `TransactionRedeemerItemBeginWitness` (open the next redeemer item through
the field-8 door, initial `RedeemerItemProofControlV1`); (b) item actions
`RedeemerItemOpenHeader`, `RedeemerItemOpenTail`, `RedeemerItemFinishData`
(no traversal control yet / finishing); (c) `RedeemerItemTraverseData` with
`NoAction`, `HeadScalar`, `HeadSequence`, `HeadMap`, `HeadLargeConstructor`,
`AttachScalar`, `FoldList` (these read source bytes and therefore carry
`chunk_proof`/`next_chunk_proof`); (d) the `RedeemerItemProofInvalid` →
`reject_invalid_field_type` terminal; (e) the two "finish" arms
(empty-redeemer-set and complete-scan → stage 2) — already covered by slot
14 (`stage_one_finish`), so not needed here.

## 2. Why it is this size

| Probe     | Adds                                                                                                                                                                                                             |     Raw bytes |                                 Delta |
| --------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------: | ------------------------------------: |
| p01       | `ValidationAuxiliaryWitnessV1` decoder                                                                                                                                                                           |        16,702 |                           **+13,353** |
| p03       | generic ScriptSources parse                                                                                                                                                                                      |        25,595 |                               +22,246 |
| p06 / p07 | `verify_native_tx_proof_source_v1` / + door                                                                                                                                                                      | 5,351 / 8,514 |                       +2,002 / +3,163 |
| p24       | `redeemer_item_proof_v1.decode_control_v1` + `step_v1` (all actions)                                                                                                                                             |        43,435 |                           **+40,086** |
| p24b      | `cek_data_traverse_v1.decode_control_v1` + `step_v1` (9 actions)                                                                                                                                                 |        36,223 |                               +32,874 |
| c03       | `cek_data_traverse_v1` control codec alone                                                                                                                                                                       |        11,644 |                                +8,295 |
| p25       | p03 + aux decoder + `script_sources_stage_one` (the deployed arm set)                                                                                                                                            |        83,150 | +39,862 over the 43,288 dispatch base |
| p50       | **raw stage-one begin**: 30-item frame, native source, door on field 8, `redeemer_item_proof_v1.initial_control_v1` + `hash_control_v1`, spliced 31-item successor                                               |        21,151 |                                     — |
| p50b      | same with a hand-assembled initial item-control preimage (the `RedeemerItemProofControlV1` codec — `control_is_well_formed` + `encode_control_prefix_v1` + `encode_optional_traversal` — replaced by a template) |    **12,414** |                                     — |

Dominators: the generic redeemer-item step (40 KB, of which the generic data
traversal is 33 KB), the generic ScriptSources parse/encode (~20 KB each),
the auxiliary decoder (13 KB), the redeemer-item control codec (~9 KB).

## 3. Options considered

- **Shrink the monolith (prune) — rejected.** Even with the raw frame and no
  auxiliary decoder the arm set still contains `redeemer_item_proof_v1.step_v1`
  (40 KB).
- **Yield split of the monolith — rejected.** Per-action yields would
  duplicate the RF-021 chain's executors and its authenticated
  serialization-template machinery in a second shape; two routes for the same
  step is exactly the state RF-021 left us in.
- **Retire the monolith in favour of the split chain (chaining, pattern 3) —
  chosen.** Slot 15 becomes a narrow **stage-one begin** resolver (measured
  12,414 as a template variant) and every `RedeemerItemStepWitness` step
  routes through the RF-021 chain, whose action-family grammar is extended
  from `{FoldMap, FinalizeFrame}` to the whole `RedeemerItemProofActionV1`
  space. The primer already lists RF-021 as the precedent for this pattern.
- **Redesign (4):** not warranted; the RF-021 decomposition is the design.

## 4. Chosen design

### 4.1 Slot 15 → `script-sources-stage-one-begin-semantic-v1.ak` (rename of the file; same slot, same title stem)

```aiken
pub type ActionV1 { VerifyRedeemerBegin { input_index, output_index, transition, carriage: FieldCarriageV1 } }
validator main(award_script_hash, computation_thread_policy_id, field_preimage_certificate_policy_id)
```

Auxiliary hashed as `TransactionRedeemerItemBeginWitness { carriage }`
(constructor 29) built raw. Predicate (p50b shape): `frame = open_frame_v1(pre,
transition, 30, 1)`; `redeemer_count = item_int(12)`, `redeemer_total_count =
item_int(26)`; `verify_native_tx_proof_source_v1(pre.transaction_id, frame.compact_cbor, …)`;
`item = open_machine_field_item(door, verified, witness_set, 8, redeemer_count, carriage)`;
`active_total = if redeemer_total_count == 0 { item_count } else { redeemer_total_count }`;
`active_total > 0 && active_total <= max_tx_size_derived_collection_item_count && item_count == active_total`;
`pending = redeemer_item_proof_v1.initial_control_hash_v1(mode_data, redeemer_count, active_total, item_length, item_commitment, -1, -1)`
(new template hash: `blake2b_256(control_domain ++ header(16) ++ 01 01 00 ++ cbor(index) ++ cbor(count) ++ cbor(length) ++ bytes(commitment) ++ 20 20 20 20 00 00 20 20 ++ d87a80)`, pinned equal to `hash_control_v1(initial_control_v1(…))`);
successor = `header(31) ++ witness[2..suffix) ++ cbor(active_total) ++ empty_observer_scan_cbor ++ empty_mint_fold_cbor ++ bytes(resolution_schedule_hash) ++ bytes(pending)`
where `suffix` is located by re-encoding the old tail (`cbor(redeemer_total_count) ++ literals ++ bytes(schedule)`) — the stage-1 bind already requires empty observer/mint-fold controls, so the literals are exact. The monolith's `common_control_is_initial` conjuncts become raw item checks (`items[25] == items[10]`, `items[8] == []`, `items[14] == 0`, `items[17] == 0`, `items[18] == 0`, `items[20] == 0`, `items[21] == 0`, `items[24] == empty receive scan`).

### 4.2 Extend the RF-021 chain to the full action grammar

`lib/midgard/script-sources-redeemer-normalization-v1.ak`:

- Action families: `fold_map_family = 0`, `finalize_frame_family = 1` (unchanged),
  new `open_header_family = 2`, `open_tail_family = 3`, `head_family = 4`
  (`HeadScalar`/`HeadSequence`/`HeadMap`/`HeadLargeConstructor`),
  `attach_scalar_family = 5`, `fold_list_family = 6`, `advance_family = 7`
  (`NoAction`), `finish_data_family = 8`, `invalid_family = 9`.
- `canonical_action_hash_v1(auxiliary, family)` generalised: for families
  2–9 the witness may carry `chunk_proof`/`next_chunk_proof` (`Some`) and the
  item-action tag is `0`/`1`/`2`/`3` per family; the narrow preimage becomes
  `[current_item_control, item_action, chunk_proof, next_chunk_proof]`.
- `verify_raw_envelope_v1` unchanged (stage-1 pending witness shape).
- The traversal normalizer requires `Some(traversal)` in `stage_fold`;
  families 2, 3, 8 (no traversal yet / finished) and 9 bypass it: the envelope
  routes them **envelope → outer-only normalizer → executor → settlement**
  (a new `-outer-only-normalizer-v1.ak` that recomputes
  `stage_data_outer_fields_are_well_formed_v1` and the pending-hash prefix
  without a traversal template, ≈ 4–5 KB like the outer normalizer).
- New executors, one file each under `validators/fraud-proofs/validation-trace/`:
  `script-sources-stage-one-redeemer-open-header-executor-v1.ak`,
  `…-open-tail-executor-v1.ak`, `…-head-executor-v1.ak`,
  `…-attach-scalar-executor-v1.ak`, `…-fold-list-executor-v1.ak`,
  `…-advance-executor-v1.ak`, `…-finish-data-executor-v1.ak`,
  `…-invalid-executor-v1.ak`. Each takes `(deployment_id, computation_thread_policy_id)`,
  verifies the chunk window with `bounded_item_v1.verify_chunk` against
  `item_commitment` (field 8, `item_index`, `total_length`), runs the narrow
  step (`redeemer_item_proof_v1` header/tail/finish logic, or the
  `cek_data_traverse_v1` step for its action on the authenticated window),
  and produces `actual_next_item_control_hash` via the authenticated prefix +
  template (`hash_stage_data_from_authenticated_prefix_v1`), exactly as the
  fold-map executor does. The invalid executor instead requires
  `rejected_successor_is_exact(pre, claimed_successor, reject_invalid_field_type)`
  and settles to the award with the rejecting terminal.
- Envelope parameters grow by eight executor hashes (+ the outer-only
  normalizer); settlement parameters grow likewise (`expected_*_executor_script_hash`).
  `contracts.ts` builds executors first, then normalizers, settlement, envelope
  (present order).
- **Second carrier for the CEK context chain.** The envelope binder today
  reads the stage-1 pending witness (`verify_raw_envelope_v1`). The CEK
  context plan's V0b hop ([§4.2, §10 there](validation-trace-cek-context-step-semantic-v1.md))
  hands the same executors a `CekContextItemStepPendingV1 { staged,
item_control, item_witness_hash, claimed_next }` datum instead. The
  envelope commitment therefore binds a carrier-tagged claim
  `(carrier ∈ {script_sources_stage_one, cek_context}, current_item_control_hash,
canonical_action_hash, expected_next_item_control_hash)`, an
  `-envelope-cek-v1.ak` binder accepts the CEK datum, and the settlement's
  expected hand-off hash becomes a per-carrier parameter (award script for
  ScriptSources, the CEK return hop for CEK). Normalizers and executors are
  unchanged by the carrier.
- **Retire** `script_sources_stage_one_redeemer_semantic_v1` and the library
  entry `verify_script_sources_stage_one_redeemer_semantics_v1`
  (`validation-machine-v1.ak:12888-12913`); slot 15's title becomes the begin
  resolver. `submit.ts`: `semanticResolverIndex === 15` accepts only
  `transactionRedeemerItemBegin`; `=== 28` accepts every `redeemerItemStep`.

**Security argument (chain).** Every hop is a computation-thread `continue`
with an exact expected output state that carries the envelope commitment
(`envelope_commitment_v1` binds deployment id, evidence hash, resolution
identity, family, canonical auxiliary/action hashes, current and expected
next item-control hashes, counts, and all five stage hashes); the settlement
checks `execution_attestation_settlement_is_exact_v1` against the original
envelope and hands off to `award_script_hash`, i.e. reaches the same terminal
`continue_winning` reached. Routing is exact per hop
(`*_route_is_exact_v1`), so a light-family executor cannot settle a heavy
family (family is in the commitment and in the executor's own
`outer_normalized_state_is_bound_v1(state, family, …)`). Omitting a hop is
impossible: each validator only accepts the predecessor's output state
domain. The begin resolver (slot 15) has no yields: the door authenticates the
field-8 preimage and the successor is re-derived.

## 5. Size and budget projection

| Script                                                 | Basis                                                                                                                      |                                                                       Projected |
| ------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------: |
| slot 15 begin resolver                                 | p50b measured                                                                                                              |             **12,414** (≤ 12,600 with `initial_control_hash_v1` in the library) |
| envelope                                               | 8,516 + 9 params                                                                                                           |                                                                         ≈ 9,200 |
| traversal / outer / outer-only normalizers             | 11,871 / 4,150 / new                                                                                                       |                                                        11,871 / 4,150 / ≈ 4,800 |
| fold-map / finalize-frame executors                    | measured                                                                                                                   |                                                                   7,519 / 9,290 |
| open-header, open-tail, finish-data, advance executors | RF-021 executor shell (~4 KB) + chunk window (~2 KB) + header/tail logic (~1–3 KB)                                         |                                                              ≈ 7,000–9,000 each |
| head executor                                          | shell + chunk window + `cek_data_traverse_v1` head steps (part of the 32.9 KB generic step; c03 codec avoided by template) | **≈ 11,000–14,000 (measure; split scalar vs sequence/map/constructor if over)** |
| attach-scalar executor                                 | shell + chunk window + `cek_data_integer_v1`/`cek_data_bytes_v1` sub-steps                                                 |                                                                 ≈ 10,000–13,000 |
| fold-list executor                                     | fold-map analogue                                                                                                          |                                                                         ≈ 7,500 |
| invalid executor                                       | shell + `rejected_successor_is_exact`                                                                                      |                                                                         ≈ 5,500 |
| settlement                                             | 5,837 + 9 params                                                                                                           |                                                                         ≈ 6,300 |

Per transaction the chain references one script (≤ 14 KB): tier 0, ≤ 0.21 ADA
per hop; today one 87,545-byte reference costs ≈ 1.68 ADA. Per machine step
the chain is **5 transactions** (envelope, normalizer(s), executor, settlement
— 4 for the outer-only route) instead of 1. C52 accounting: a redeemer item of
`n` data nodes needs O(n) machine steps, so the proof-transaction count for a
disputed item step rises from 1 to 4–5 per node visited. Whether the worst
admissible redeemer (32,768-byte aggregate field, nesting bounded only by the
traversal's own limits) already exceeds the 5,000-transaction cap in machine
steps is **not established here** — the fit sweep row for resolver 8 is
unmeasured — so the C52 evidence must re-derive the governing bound with the
5× multiplier applied to every `RedeemerItemStepWitness` step, and the
§3.3(3) maturity computation must use it (for reference, 5,000 tx × ~40 s ≈
56 h against the 84 h half-maturity budget). If the multiplied count breaks
the cap, the mitigation is to merge the outer normalizer into each executor
(4 → 3 hops) and to collapse the head/attach families into one executor
where §5's size gate allows.
ExUnits per hop are those of the existing chain (envelope/normalizers
measured by RF-021's tests) plus one executor; the begin resolver is a single
execution.

## 6. Off-chain work

Exists today: `encodeScriptSourcesStageOneSpendRedeemerV1` (five stages),
`contracts.ts` `scriptSourcesStageOneRedeemerStages` (six validators),
`validation-dispute-submit.test.ts` "maps and encodes the split ScriptSources
stage-one route without replacing the legacy route". Missing: deployment
entries, submit routes for the chain hops, funding rows.

- `contracts.ts`: add the outer-only normalizer and eight executors to
  `scriptSourcesStageOneRedeemerStages`; envelope/settlement parameter lists
  extended; slot-15 title → `script_sources_stage_one_begin_semantic_v1`.
- Deployment entries: `validationTraceDisputeScriptSourcesStageOneBeginSemantic`
  and `validationTraceDisputeScriptSourcesStageOneRedeemer{Envelope,TraversalNormalizer,OuterNormalizer,OuterOnlyNormalizer,FoldMapExecutor,FinalizeFrameExecutor,OpenHeaderExecutor,OpenTailExecutor,HeadExecutor,AttachScalarExecutor,FoldListExecutor,AdvanceExecutor,FinishDataExecutor,InvalidExecutor,Settlement}`;
  no roles/withdrawals.
- Submit route: a `submitScriptSourcesStageOneRedeemerChain` driver in
  `submit.ts` that issues the 4–5 hops from one one-step argument (family
  derived from the auxiliary's item action tag and traversal action tag by a
  pure `scriptSourcesRedeemerActionFamilyV1`), each hop attaching its
  reference script; `encodeScriptSourcesStageOneSpendRedeemerV1` gains stage
  `"outer-only"` and the executor redeemer shapes for the light families
  (`{ input, output, traversal_action?, chunk_proof?, next_chunk_proof? }`);
  `auxiliaryShapeV1` narrows index 15 to `transactionRedeemerItemBegin`.
- Funding rows: sixteen publications.
- Codec: `ValidationOneStepArgument.resolverHints.actionFamily` and
  `expectedNextItemControlHash` (the evidence builder already computes the
  successor item control).
- Watcher: the multi-hop driver must be resumable from any hop (thread state
  identifies the hop); no operator-local inputs.

## 7. Emulator scenario tests

Exists: nothing reaches resolver 8. Add
`tests/submit-init-emulator-script-sources-stage-one-v1.test.ts`:
publication fit for all sixteen scripts without `oversized`; positive
lifecycle for the begin step (slot 15) and for one item step per family
through the 4–5-hop chain to award and removal (fixture redeemer: a nested
map containing a list, a large constructor and an integer/bytes scalar, so
every traversal family fires; `spendInputsOfCardinality` with a redeemer
field of two items so begin fires at `redeemer_count = 1`); valid-block
negative at the executor hop (forged `expected_next_item_control_hash`) and
at the begin frontier; cancel/resume at every hop; maximum shape: a redeemer
item at the 32,768-byte aggregate bound with the deepest supported nesting;
the invalid family proving `reject_invalid_field_type`.

## 8. Aiken tests

- `script-sources-redeemer-normalization-v1.test.ak`: extend the existing 39
  with `canonical_action_hash_binds_each_light_family_and_chunk_proofs`,
  `light_family_executors_equal_generic_step_v1` (property per family against
  `redeemer_item_proof_v1.step_v1`), `outer_only_normalizer_hash_prefix_equals_generic`,
  `settlement_accepts_every_bound_family`, `route_rejects_cross_family_executor`.
- New `validators/fraud-proofs/validation-trace/script-sources-stage-one-begin-v1.test.ak`:
  `stage_one_begin_wire_layout_is_pinned`, `prepare_routes_stage_one_begin_to_slot_fifteen`,
  `begin_wins_the_first_redeemer_item`, `begin_refuses_an_item_step_auxiliary`,
  `begin_refuses_a_pending_item` (31-item witness), `initial_control_hash_template_equals_typed`
  (fuzz), `begin_splice_equals_exact_encoder`.
- Retire `script_sources_stage_one_redeemer_family_guard`
  (`validation-machine-v1.test.ak:4891`) with the monolith; keep
  `script_sources_stage_one_accepts_more_than_sixteen_sources`.

## 9. Verification commands

```bash
cd onchain/aiken && aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/script_sources_stage_one_(begin_semantic|redeemer_[a-z_]+)_v1\.main\.spend$/.test(v.title)){const n=Buffer.from(v.compiledCode,"hex").length;console.log(v.title,n,n<=15000?"OK":"OVER")}'
# expected: 16 titles, all OK; script_sources_stage_one_redeemer_semantic_v1 absent
aiken check -m script_sources_redeemer_normalization   # expected: 39 + ≥ 5 tests, 0 failures
aiken check -m script_sources_stage_one_begin          # expected: ≥ 7 tests
cd ../../demo && pnpm --filter @al-ft/midgard-fault-proofs test -- tests/validation-dispute-submit.test.ts tests/zz605-semantic-resolver-arity.test.ts tests/submit-init-emulator-script-sources-stage-one-v1.test.ts
```

## 10. Ordering and dependencies

Lands with the stage-one finish plan (slot 14, same raw frame) and the
non-output plan (library); changes envelope/settlement parameters, so the
whole RF-021 chain re-applies in the same regeneration. Independent of the
stage-5 LOP yields, but the head/attach executors and the LOP datum yields
should share the `cek_data_traverse_v1` narrow step helpers added for both.

**Ownership of the redeemer-item machine (reconciled with the stage-ten-match
and CEK plans).** This plan owns the `mode_data` decomposition of
`redeemer_item_proof_v1.step_v1` (all action families, §4.2). Its one external
consumer is the CEK context plan's V0b hop
([validation-trace-cek-context-step-semantic-v1.md](validation-trace-cek-context-step-semantic-v1.md)
§4.2, §10) for stage 0/9 `RedeemerItemStepWitness` steps, served by the
second-carrier envelope in §4.2; that plan must not fork a copy. The
descriptor-mode header/tail steps of stages 10/12 are **not** this plan's:
they are the stage-ten-match plan's `V1VtSsRedeemerItemStepYield`
([§4b–4e there](validation-trace-script-sources-stage-ten-match-semantic-v1.md)),
a single-transaction yield that shares `header_step` / `tail_step` with this
chain's open-header / open-tail executors and is otherwise independent.
`initial_control_hash_v1` (§4.1) is pinned equal to
`redeemer_item_proof_v1.hash_descriptor_control_v1(initial_control_v1(…))`,
the traversal-`None` hash the stage-ten-match plan defines, rather than to a
separate encoder.

## 11. Risks

- **Executor sizes for the head and attach families are unmeasured**; the
  generic traversal step is 32.9 KB and the per-action narrow steps do not
  yet exist as functions. Gate: §9 size check before merge; fallback: split
  head into scalar / aggregate executors.
- **5× transactions per item step** raises maturity-window pressure
  (§3.3(3)); the current per-item step bound already exceeds C52 in the worst
  case, so this plan does not change the cap's governing term but must be
  recorded in the C52 evidence.
- **ABI churn** is the largest in the group: slot-15 redeemer changes shape,
  envelope/settlement parameters grow, `submit.ts` route table changes;
  `validation-dispute-submit.test.ts` (line 1638 test) must be rewritten to
  assert the legacy route is _gone_.
- **Spec conflict:** GOAL_SPEC §8.3 C45 ("Every … retained redeemer …") is
  unaffected; RF-021's "monolith remains available" statement in
  `GOAL_REVIEW_REMEDIATION.md` must be amended when the monolith is retired.
