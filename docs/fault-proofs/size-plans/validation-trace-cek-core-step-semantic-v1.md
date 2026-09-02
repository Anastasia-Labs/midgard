# Size-fit plan: `cek_core_step_semantic_v1` (validation-trace CEK core step)

Companion to [00-primer.md](00-primer.md). Chain conventions, the shared
library module `cek-semantic-chain-v1.ak`, the aux-projection helpers and the
family-wide probe table are defined in
[validation-trace-cek-context-step-semantic-v1.md](validation-trace-cek-context-step-semantic-v1.md)
§2.2 and §4.1 and are not repeated here.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/cek_core_step_semantic_v1.main.spend` (and `.else`) |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/cek-core-step-semantic-v1.ak` (78 lines) |
| Raw size | 68,689 bytes (measured 2026-09-01, pinned fork, fresh copy build) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` (2) |
| Phase / index | phase `Cek`, resolver index 11 (`cek_v1.main`), semantic resolver index 3 |
| Library entry point | `validation_machine_v1.verify_cek_core_step_semantics_v1(pre, transition, step: CoreStepEvidenceV1)` — rebuilds `CekCoreStepWitness { step }` for the evidence hash, then `cek_witness_is_well_formed_v1` + `cek_core_v1` → `verify_cek_core_step` → `cek_machine_v1.verify_core_step_v1(step.pre, step.post, step.witness)` |
| Redeemer today | `VerifyCoreStep { input_index, output_index, transition, step: CoreStepEvidenceV1 }` (the aux is flattened; `core_step_wire_layout_is_pinned` pins it) |
| Role / deployment entry | none / `validationTraceDisputeCekCoreStepSemantic` (`VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1[3]`) |
| SDK title key | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.cekCoreStep` |
| Emulator today | published `oversized: true` only; never journeyed |

One core step is one CEK machine transition (`MachineStateV1` → `MachineStateV1`)
witnessed by one of the 40 `CoreStepWitnessV1` constructors, grouped by the
pre-state `mode` (compute, lookup, return, case-select, case-apply, builtin,
semantic-builtin).

## 2. Why it is this size

Measured in the same probe copy (see the sibling plan for method and the
shared rows; baselines `d_step` 5,330 = `(MachineStateV1, MachineStateV1,
CoreStepWitnessV1)` decoder, `d_core` 14,755, `d2_args` 446, `d2_sem` 1,305,
`d2_fail` 431, `d2_type` 615).

| Probe | Function | Raw | Cost |
| --- | --- | ---: | ---: |
| `p_core_lib` | `verify_cek_core_step_semantics_v1` (whole entry point) | 66,559 | 60,462 over `d_ev` |
| `p_core` | `verify_cek_core_step` (framing + successor + machine) | 71,586 | **56,831** |
| `p_vcs` | `cek_machine_v1.verify_core_step_v1` | 58,120 | **52,790** |
| `p_compute` | `verify_compute_step` | 13,749 | 8,419 |
| `p_lookup` | `verify_lookup_step` | 6,204 | 874 |
| `p_return` | `verify_return_step` | 10,503 | 5,173 |
| `p_case_select` | `verify_case_select_step` | 6,262 | 932 |
| `p_case_apply` | `verify_case_apply_step` | 6,161 | 831 |
| `p_sem_builtin` | `verify_semantic_builtin_control_step_v1` (map-conversion steps) | 11,555 | 6,225 |
| `p_builtin` | `verify_builtin_step` (all seven builtin arms) | 47,639 | **42,309** |
| `p_b_direct` | `verify_builtin_direct_step` | 26,501 | 21,171 |
| `p_b_semantic` | `verify_builtin_semantic_step` | 29,792 | 24,462 |
| `p_b_mapstart` | `verify_builtin_map_conversion_start_v1` | 23,132 | 17,802 |
| `p_b_semfail` | `verify_builtin_semantic_failure_step` | 16,122 | 10,792 |
| `p_b_bls` | `verify_builtin_bls_final_step` | 19,068 | 13,738 |
| `p_b_fail` | `verify_builtin_failure_step` | 22,828 | 17,498 |
| `p_b_typefail` | `verify_builtin_type_failure_step` | 16,110 | 10,780 |
| `q_args_root` | `cek_builtin_v1.arguments_root_v1` | 7,070 | 6,624 |
| `q_budget` | `direct_builtin_budget_v1` (cost sizes + `cek_cost_v1` tables) | 9,206 | 8,760 |
| `q_direct` | `verify_direct_builtin_v1` (all families) | 14,261 | 13,815 |
| `q_int` … `q_g2` | one direct family each (`verify_integer_binary_v1`, `verify_bytes_v1`, `verify_strings_v1`, `verify_signature_v1`, `verify_control_v1`, `verify_pair_and_list_v1`, `verify_choose_data_v1`, `verify_data_v1`, `verify_v3_bytes_v1`, `verify_bls_g1_v1`, `verify_bls_g2_v1`) | 6,744–7,856 | 6,300–7,410 each, of which ≈ 6,000 is the shared constant infrastructure (`result_is_constant_v1`, `cek_constant_v1` decode/root) |
| `q_semantic` | `verify_semantic_builtin_v1` | 18,444 | 17,139 |
| `q_sem_data` / `q_sem_pair` / `q_sem_list` | `verify_semantic_data_v1` / `_pair_v1` / `_list_v1` | 13,925 / 10,533 / 12,308 | 12,620 / 9,228 / 11,003 (share ≈ 8,400 of `cek_data_v1` node inspectors) |
| `q_semfail` | `verify_semantic_builtin_failure_v1` | 11,331 | 10,026 |
| `q_known_fail` | `known_builtin_failure_v1` | 6,173 | 5,742 |
| `q_fail_direct` | `verify_direct_builtin_failure_v1` | 11,495 | 11,064 |
| `q_typefail` | `verify_builtin_type_failure_v1` | 10,872 | 10,257 |
| `m_datanode` / `m_listnode` / `m_pairnode` | `cek_data_v1.inspect_*_preimage_v1` + hash | 3,709 / 2,397 / 2,641 | 3,584 / 2,272 / 2,516 |

Reading: 68.7 KB = 3.2 (handshake) + 6.4 (shared CEK prefix) + 5.2
(`CoreStepEvidenceV1` decoder) + 4.0 (core framing and successor encoding) +
52.8 (`verify_core_step_v1`), with the builtin arms (42.3 KB) dominating.
The arms share three infrastructures that recur in every builtin hop:
constant decode/root (≈ 4–6 KB), `cek_cost_v1` tables (≈ 4.8 KB) and the
`cek_data_v1` node inspectors (≈ 8.4 KB). Family splitting inside an arm
therefore yields little; splitting *across* those infrastructures does.

ExUnits (aiken check, includes fixture construction): validator-level
`core_step_validator_wins_the_application_step` 6.04 M / 2.60 B;
lib `cek_active_core_step_replays_exact_machine_transition` 7.74 M / 3.07 B;
`direct_builtin_executes_with_exact_semantics_and_budget` 5.24 M / 1.92 B;
`paid_secp_failure_halts_with_the_exact_builtin_budget` 8.27 M / 2.97 B;
`ten_leaf_bls_final_transition_fits_the_l1_execution_reserve` **12.30 M /
8.54 B**; `ten_leaf_miller_loop_proof_fits_the_l1_execution_reserve`
11.44 M / 8.25 B; `semantic_map_conversion_scans_one_pair_then_finishes`
2.32 M / 1.06 B. Fixture overhead is roughly 3–4 M (finish vector 4.54 M for
a trivial predicate), so the honest ten-leaf BLS step is ≈ 8–9 M / ≈ 6–7 B
in one script — already at the 8 B cpu reserve of §3.3.

## 3. Options considered

| Option | Verdict | Reason |
| --- | --- | --- |
| Prune | Keep, insufficient alone | Read `step` as `Data` in the binder (saves the 5.2 KB `CoreStepWitnessV1` decoder there; wire unchanged, `aux = constr(12, [step])`). The arms are all reachable (any UPLC program can take any step); nothing is dead. |
| Yields in one tx | Rejected | Arm = yield gives one yield per transaction for machine arms, but every builtin arm needs 2–3 pieces (roots / budget / semantics) that would all run in one transaction with the dispatcher, each re-decoding a witness whose arguments carry up to `max_direct_builtin_revealed_payload_bytes`; the BLS arm would add ≈ 2 M / 1.5 B of dispatcher-plus-reparse to a step already at 8–9 M / 6–7 B, past the §3.3 basis (13.2 M / 8 B). Twelve-plus role NFTs and stake registrations. |
| Chain (pattern 3) | **Chosen** | Per-hop budgets; the BLS final step's two Miller-loop expression evaluations go into two hops (halving the cpu per transaction); no roles; same conventions and off-chain driver as the context-step plan. Cost: +2..+4 transactions per core-step dispute. |
| Redesign | Rejected | The machine's step grammar is spec (C48); only its verification is being sliced. Lowering `max_direct_bls_miller_loop_leaves` was considered as a budget escape hatch and is not needed with the two-hop BLS split. |

## 4. Chosen design

Chain: **B (binder) → A₁ [→ A₂ [→ A₃]] (arm hops) → S (settle) → award**,
using `CekChainBaseV1`, `base_is_bound_v1`, `continue_exact_v1`,
`award_continuation_is_exact_v1` from the sibling plan.

### 4.1 Datum types (in `cek-semantic-chain-v1.ak`)

```aiken
pub type CekCoreBoundV1 {
  chain: CekChainBaseV1,
  execution_cursor: Int, completed_cpu: Int, completed_memory: Int,
  execution_cpu_limit: Int, execution_memory_limit: Int,
  program_envelope_hash: ByteArray,
  pre: cek_machine_v1.MachineStateV1,
  post: cek_machine_v1.MachineStateV1,
  witness_hash: ByteArray,          // blake2b_256(cbor.serialise(step.witness as Data))
  arm: Int,                         // CoreStepWitnessV1 constructor index
  progress: Int,                    // hops completed inside the arm chain (0 at B)
  facts: CekCoreArmFactsV1,         // arm-local commitments, see 4.3
}
pub type CekCoreArmFactsV1 { NoFacts | BuiltinRoots { arguments_root, arguments_count, result_root, builtin_root } | BlsSide { left_root_ok: Bool, right_root_ok: Bool, left_metrics: Int, right_metrics: Int } }
```

Hops carry only the witness in their redeemer
(`VerifyArm { input_index, output_index, witness: Data }`); binding is
`blake2b_256(serialise(witness)) == bound.witness_hash` and `pre`/`post` come
from the datum, so arm hops never touch the transition and never re-run the
6.4 KB prefix.

### 4.2 Hop list

| # | Validator (title) | File | Responsibility | Parameters | Projected |
| --- | --- | --- | --- | --- | ---: |
| B | `cek_core_step_semantic_v1.main` (unchanged title, `cek_v1` slot 3) | `cek-core-step-semantic-v1.ak` | `base_is_bound_v1` with `aux = constr(12,[step])`; `cek_witness_control_v1` + `cek_witness_is_well_formed_v1`; `cek_control_is_core_step_v1`; `context_control_cbor == ""`; shallow decode `step` → `pre`, `post: MachineStateV1` (witness stays `Data`); framing from `verify_cek_core_step`: hash lengths, cursor bounds, limits `> 0`, `pre.execution_index == cursor`, `cek_machine_state_hash(pre) == active_state_hash`, `pre_state.execution_cpu == completed_cpu + pre.cpu` (and memory), `state_is_well_formed(pre/post)`, `pre.execution_index == post.execution_index`; arm routing: `un_constr_data(witness).1st` must be an arm admissible for `pre.mode` (table below); pays `arm_hop_script_hashes[group]` with `CekCoreBoundV1 { progress: 0, facts: NoFacts }` | `arm_hop_script_hashes: List<ScriptHash>` (13 groups), `computation_thread_policy_id` | 3.2 + 6.4 + 0.5 + 0.6 + 1.5 + 1.0 ≈ **13.2 KB** |
| A-compute | `cek_core_arm_compute_v1.main` | `cek-core-arm-compute-v1.ak` | `pre.mode == mode_compute`; `verify_compute_step(pre, post, witness)` (arms `ComputeVariable`…`ComputeContextConstant`, tags 0–11 and 39) | `settle_script_hash`, ct policy | 3.2 + 1.0 + 0.2 + 1.0 + 8.4 ≈ **13.8 KB** (marginal; fallback: split `ComputeConstant`'s `verify_constant_witness_v1` into its own hop) |
| A-machine | `cek_core_arm_machine_v1.main` | `cek-core-arm-machine-v1.ak` | modes lookup / return / case-select / case-apply: `verify_lookup_step`, `verify_return_step`, `verify_case_select_step`, `verify_case_apply_step` (0.9 + 5.2 + 0.9 + 0.8) | same | 5.4 + 7.8 ≈ **13.2 KB** |
| A-mapconv | `cek_core_arm_map_conversion_v1.main` | `cek-core-arm-map-conversion-v1.ak` | `mode_semantic_builtin`: `verify_semantic_builtin_control_step_v1` (`StepBuiltinListToMap`, `StepBuiltinMapToList`, `FinishBuiltinMapConversion`) | same | 5.4 + 6.2 ≈ **11.6 KB** |
| A-direct-1 | `cek_core_arm_direct_roots_v1.main` | `cek-core-arm-direct-roots-v1.ak` | `ExecuteBuiltinDirect`: `arguments_root_v1` (6.6), `hash_builtin_value_v1`, `arguments_count == builtin_argument_count(tag)`, `pre.focus_root == builtin_root`, `result_root_v1(result)` → commits `BuiltinRoots` | `direct_budget_script_hash`, ct policy | 5.4 + 0.6 + 6.6 + 0.5 + 1.0 ≈ **14.1 KB** (marginal) |
| A-direct-2 | `cek_core_arm_direct_budget_v1.main` | `cek-core-arm-direct-budget-v1.ak` | `direct_builtin_budget_v1(tag, arguments)` (8.8) and `post == exact_state(pre, mode_return, facts.result_root, empty_environment_root_v1, pre.continuation_root, 0, budget.cpu, budget.memory)` | `direct_semantics_script_hash`, ct policy | 5.4 + 0.6 + 8.8 + 0.3 ≈ **15.1 KB** (marginal; fallback: move `builtin_cost_sizes_v1` into A-direct-1 and commit the size list) |
| A-direct-3 | `cek_core_arm_direct_semantics_v1.main` | `cek-core-arm-direct-semantics-v1.ak` | `verify_direct_builtin_v1(tag, facts.builtin_root, arguments, result)` with the budget call removed from the family dispatch (13.8 − 4.8 tables − 2.6 hashing ≈ 6.4 own + 4.0 constant infra) | `settle_script_hash`, ct policy | 5.4 + 0.6 + 10.4 ≈ **16.4 KB** → two validators by family: `direct_semantics_scalar` (integer, bytes, strings, control, signature, v3 bytes) and `direct_semantics_structured` (pair/list, choose-data, data, bls g1/g2), each ≈ 5.4 + 0.6 + 4.0 + 2.5 ≈ **12.5 KB**; B routes by `tag` |
| A-sem-1 | `cek_core_arm_semantic_roots_v1.main` | `cek-core-arm-semantic-roots-v1.ak` | `ExecuteBuiltinSemantic`: same as A-direct-1 plus `semantic_constant_*` roots for the result | `semantic_budget_script_hash`, ct policy | ≈ **14.1 KB** (marginal) |
| A-sem-2 | `cek_core_arm_semantic_budget_v1.main` | `…-semantic-budget-v1.ak` | semantic budget (`cek_cost_v1.builtin_budget_v1` over semantic sizes) and `post == exact_state(...)` | `semantic_material_script_hash`, ct policy | ≈ **15.1 KB** (marginal, same fallback as A-direct-2) |
| A-sem-3 | `cek_core_arm_semantic_data_v1.main` / `_pair_v1` / `_list_head_v1` / `_list_cons_v1` | four files | `verify_semantic_data_v1` (12.6; split `mkconstr/unconstr` vs `mklist/unlist/idata/bdata` if > 15,000 after the probe), `verify_semantic_pair_v1` (9.2), `verify_semantic_list_v1` split by tag (head/tail/null vs mk_cons/choose_list); each embeds the 8.4 KB `cek_data_v1` inspectors | `settle_script_hash`, ct policy | 5.4 + 1.5 + {12.6 → **19.5 ✗ split**, 9.2 → **16.1 ✗** (reduce: drop `MachineStateV1` decode by committing `pre.focus_root`/`post.focus_root` only → ≈ 14.9), 8.4 + 1.3 → **≈ 14.5**} |
| A-mapstart | `cek_core_arm_map_start_roots_v1.main` → `cek_core_arm_map_start_nodes_v1.main` | two files | `StartBuiltinMapConversion` (17.8): roots/budget hop (≈ 13.5) then node-material hop (`MapConversionStartWitnessV1` nodes through the inspectors, ≈ 14) | chained | **13.5 / 14.0 KB** |
| A-semfail | `cek_core_arm_semantic_failure_roots_v1.main` → `…_material_v1.main` | two files | `ExecuteBuiltinSemanticFailure` (10.8): roots + halt successor (≈ 12) then `verify_semantic_builtin_failure_v1` material check with inspectors (≈ 15.0, marginal) | chained | **12 / 15.0 KB** |
| A-bls | `cek_core_arm_bls_left_v1.main` → `cek_core_arm_bls_right_v1.main` → `cek_core_arm_bls_final_v1.main` | three files | `ExecuteBuiltinBlsFinal` (13.7): left expression (`verify_bls_expression_v1` + `bls_expression_metrics_v1`, commits into `BlsSide`), right expression likewise, then `verify_direct_bls_final_with_expected_roots_v1` + `direct_builtin_budget_v1(70, …)` + `exact_state` | chained | **≈ 11.7 / 11.7 / 9.7 KB**; cpu per hop ≈ half of the ten-leaf 8.54 B |
| A-fail | `cek_core_arm_failure_known_v1.main` → `cek_core_arm_failure_budget_v1.main` | two files | `ExecuteBuiltinFailure` (17.5): `known_builtin_failure_v1` (5.7) + `arguments_root_v1` + focus (≈ 13.7); `direct_builtin_failure_budget_v1` (8.8 tables) + `error_successor` (≈ 14.4) | chained | **13.7 / 14.4 KB** |
| A-typefail | `cek_core_arm_type_failure_v1.main` | one file | `ExecuteBuiltinTypeFailure` (10.8): `runtime_arguments_root_v1` + `builtin_arguments_are_well_typed_v1` + uncharged halt | `settle_script_hash`, ct policy | 5.4 + 0.8 + 10.3 ≈ **16.5 KB** → split `runtime_arguments_root_v1` (roots, ≈ 11) from the kinds table check (≈ 9) |
| S | `cek_core_step_settle_v1.main` | `cek-core-step-settle-v1.ak` | `base_is_bound_v1` (transition + `constr(12,[step])` in redeemer), `cek_witness_control_v1`; `bound.chain.binder_script_hash == binder_script_hash`; `input_script_hash == arm_terminal_script_hashes[group]` and `bound.progress == arm_hop_counts[group]`; the successor branch of `verify_cek_core_step` (budget-exceeded / `mode_halt_error` → `rejected_successor_is_exact(reject_plutus_script_invalid)`; `mode_halt_success` → ValueAndMint hand-off or next-cursor selection via `encode_value_and_mint_witness_v1` / `encode_cek_witness_v1`; otherwise continue with `cek_machine_state_hash(post)`); `award_continuation_is_exact_v1` | `award_script_hash`, `binder_script_hash`, `arm_terminal_script_hashes: List<ScriptHash>`, `arm_hop_counts: List<Int>`, ct policy | 3.2 + 1.0 + 0.5 + 1.7 + 4.0 + 0.5 ≈ **10.9 KB** |

Arm groups routed by B (13 slots): compute, machine, mapconv,
direct-scalar, direct-structured, semantic (roots entry), mapstart, semfail,
bls, fail, typefail — 11 entry hashes plus two spare `#""` slots so the list
length can be pinned like `select_semantic_resolver` pins 4.

### 4.3 Handshake and security argument

- Dispatch uniqueness: one thread NFT, one hop per transaction
  (`fraud_proofs/common.continue`).
- Provenance: hop k pins hop k+1's hash; S pins the binder hash recorded at
  B and the arm's terminal hash + `progress` count, so no arm hop can be
  skipped or repeated (each hop increments `progress` and the datum type is
  identical across arm hops on purpose — the count and the terminal
  `input_script_hash` are what S checks).
- Cross-arm substitution: B routes on `(pre.mode, constructor tag)`; every
  arm hop re-asserts `bound.arm == its tag(s)` and `bound.pre.mode == its
  mode`; the wrong hop fails on those pins before any semantics.
- Evidence binding: B checks `hash_one_step_evidence(transition,
  constr(12,[step])) == evidence_hash`; arm hops bind their redeemer's
  `witness` to `bound.witness_hash`; `pre`/`post` are the values B decoded
  from the committed `step`. S re-binds the transition the same way for the
  successor encodings.
- Output-state re-derivation: S pins `winning_resolution()` and the award
  hash; every intermediate hop pins the next datum exactly, including the
  `facts` it adds.
- Omitted hop / omitted check: `progress` and the terminal hash; if the
  deployment list is short, B routes to `#""` and fails closed.
- Cancel at every hop via `cancel`.

## 5. Size and budget projection

- Sizes: §4.2. All hops ≤ 15,000 after the named splits; four marginal
  (A-compute 13.8, A-direct-1 14.1, A-direct-2 15.1, A-sem-2 15.1) with a
  stated fallback each. ≈ 27 validators.
- Referenced bytes per transaction: one hop (≤ 15 KB) → tier 1, ≤ 0.23 ADA
  reference-script fee per hop; today's 68,689-byte body costs ≈ 1.22 ADA
  (384,000 + 460,800 + 377,762 lovelace) in one transaction.
- ExUnits: per hop. B ≈ finish vector (≤ 5 M / 2 B). Machine arms ≤ the
  monolith vectors (6–8 M with fixture). BLS: left/right hops each carry one
  `verify_bls_expression_v1` evaluation, so the ten-leaf shape drops from
  8.54 B (with fixture) in one script to roughly half per hop, under the 8 B
  reserve with margin; `max_direct_bls_miller_loop_leaves = 10` stays.
- Transactions per dispute: 3 (B, one arm hop, S) to 5 (B, three arm hops,
  S); journey ≈ 40 → ≤ 44; C52 and §3.3 maturity untouched.

## 6. Off-chain work

Exists: SDK title `semantics.cekCoreStep`, deployment entry
`validationTraceDisputeCekCoreStepSemantic`, submit branch in
`semanticActionFieldsV1` (`[...base, ...auxiliary.fields]` for semantic 3),
`VALIDATION_AUXILIARY_SHAPES_V1.cekCoreStep = [12, 1]`. Missing: multi-hop
driver, journeys.

1. `contracts.ts`: `cekCoreStepStages` record (binder, arm hops, settle);
   by-name parameter values for `arm_hop_script_hashes`,
   `settle_script_hash`, `direct_budget_script_hash`,
   `direct_semantics_script_hash`, `semantic_budget_script_hash`,
   `semantic_material_script_hash`, `binder_script_hash`,
   `arm_terminal_script_hashes`, `arm_hop_counts` (an `Int` list parameter
   — first non-hash semantic parameter; the arity gate handles it by name).
2. Deployment entries per hop under semantic index 3; hash-checked, no
   role; node descriptors and publication targets per hop.
3. Submit: `isSplitCekCoreStep` branch in
   `submitValidationDisputeSemanticResolution`; `deriveCekCoreStepRouteDataV1`
   picks the arm group from the witness constructor (the TS twin of the
   routing table, in `demo/midgard-validation/src/cek-machine.ts` which
   already names every `CoreStepWitnessV1` constructor) and emits the hop
   datums including `BuiltinRoots` (recomputed off-chain with
   `cek-builtin.ts` roots) and `BlsSide`; encoder
   `encodeCekCoreStepSpendRedeemerV1({ hop, inputIndex, outputIndex,
   transition?, step?, witness? })`.
4. Codecs: `CekCoreBoundV1`/`CekCoreArmFactsV1` schemas in
   `demo/midgard-sdk/src/fraud-proof/validation-dispute.ts`; no
   `midgard-validation` machine change.
5. Funding row: +2..+4 transactions for a core-step dispute.

## 7. Emulator scenario tests

Exists: publication test with `oversized: true`; no journey. Add:

1. Publication fit for every hop without `oversized` (shared change in
   `submit-init-emulator-validation-dispute.test.ts`, see sibling plan §7.1).
2. `submit-init-emulator-cek-core-step-v1.test.ts`: fixture
   `cekStep: { kind: "core-step", arm }` over an L2 transaction running a
   small PlutusV3 program with at least one direct builtin (`addInteger`),
   one lambda application and the terminal `error`/`halt`; journeys for the
   application step (A-compute → S), a direct builtin step (A-direct-1..3 →
   S) and the halt step (A-machine → S with the ValueAndMint hand-off
   successor); valid-block negative at the same frontier; cancel at B and at
   an arm hop mid-chain; maximum shape: the ten-leaf BLS final step (fixture
   from `ten_leaf_bls_final_transition_fits_the_l1_execution_reserve`)
   asserting per-hop mem/cpu ≤ 13.2 M / 8 B and every transaction ≤ 16,384
   bytes.

## 8. Aiken tests

- `cek-split-v1.test.ak`: keep `core_step_wire_layout_is_pinned` (binder wire
  unchanged); add hop goldens; drive B and each arm hop on the lib fixtures
  (`cek_active_core_step_fixture`, the direct/semantic/failure/BLS fixtures
  in `cek-machine-v1.test.ak`); negatives: `witness_hash` mismatch, wrong arm
  hop for the mode, `progress` short at S, terminal hash mismatch at S,
  tampered `BuiltinRoots`, binder hash mismatch, cancel at each hop.
- `validation-machine-v1.test.ak`: `cek_core_chain_agrees_with_the_aggregate`
  — framing + arm hops' predicates + S's successor equal
  `verify_cek_core_step` on every existing core fixture;
  `cek_core_step_route_agrees_with_the_aggregate` and the partition test
  unchanged.
- `cek-machine-v1.test.ak`: the arm-split functions (`verify_direct_builtin_v1`
  without budget, per-family semantics entry points, BLS side evaluation)
  proven equal to the current arms on `direct_builtin_executes_with_exact_semantics_and_budget`,
  `bls_final_builtin_executes_from_authenticated_expressions`,
  `semantic_map_conversion_scans_one_pair_then_finishes` and the failure
  vectors; property: for random `(tag, arguments)` the split budget hop plus
  semantics hop accept iff `verify_builtin_direct_step` accepts.

## 9. Verification commands

```bash
cd /home/gumbo/midgard-hub/midgard/onchain/aiken
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");let n=0,bad=0;for(const v of b.validators){if(!/validation_trace\/cek_core_/.test(v.title)||!/\.spend$/.test(v.title))continue;n++;const s=Buffer.from(v.compiledCode,"hex").length;if(s>15000)bad++;console.log(v.title,s)}console.log("scripts",n,"over 15000:",bad)'
# expected: 27 spend validators, over 15000: 0
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m fraud_proofs/validation_trace/cek_split_v1
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m midgard/cek_machine_v1          # today 23 checks
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m midgard/cek_builtin_v1          # today 34 checks
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m "midgard/validation_machine_v1.{cek_core_chain_agrees_with_the_aggregate, cek_core_step_route_agrees_with_the_aggregate}"
cd /home/gumbo/midgard-hub/midgard
pnpm --filter @al-ft/midgard-fault-proofs test -- tests/zz605-semantic-resolver-arity.test.ts tests/validation-dispute-submit.test.ts
pnpm --filter @al-ft/midgard-fault-proofs test -- tests/submit-init-emulator-cek-core-step-v1.test.ts
```

## 10. Ordering and dependencies

- Lands with the two sibling CEK plans (shared chain module, shared aux
  projection, shared deployment-entry restructuring, one blueprint
  regeneration and catalogue-root re-pin). Independent of the script-sources
  plan (no redeemer-item machine here).
- `cek_builtin_v1` / `cek_cost_v1` / `cek_data_v1` are used by no other
  oversized family; the arm-split entry points are additive.
- The `demo/midgard-validation` cost/semantics twins (`cek-builtin.ts`,
  `cek-cost.ts`) are unchanged; only routing metadata is added.

## 11. Risks

- **Marginal hops.** Four hops project at 13.8–15.1 KB with the constant
  infrastructure counted once; a regeneration that inlines differently could
  push them over. Each has a named fallback split; measure before cutting
  over.
- **Count.** ≈ 27 validators is the price of three infrastructures (constant
  decode, cost tables, data-node inspectors) that cannot share a script with
  each other under 15 KB. An alternative that reduces count — a lookup-table
  encoding of `cek_cost_v1` (cost model as one ByteArray constant indexed by
  tag) — is a genuine prune of ≈ 4 KB per budget hop and could merge
  budget into roots hops; recommended as a follow-up probe, not assumed here.
- **BLS budget.** The two-hop split is what keeps the ten-leaf step under
  the 8 B reserve; if a re-measurement shows a single Miller-loop side above
  it, lowering `max_direct_bls_miller_loop_leaves` (mirrored in
  `demo/midgard-validation/src/cek-builtin.ts`) is the fallback and is a
  protocol-parameter change that must be recorded against C48.
- **ABI churn.** New datum types and per-hop redeemers; binder wire kept.
  The `facts` commitments (`BuiltinRoots`) must be recomputed off-chain
  bit-exactly; cross-language goldens are mandatory.
- **Spec.** C48 "bounded microsteps": the chain does not change the step
  grammar or budgets; the equality tests in §8 are the proof obligation.
