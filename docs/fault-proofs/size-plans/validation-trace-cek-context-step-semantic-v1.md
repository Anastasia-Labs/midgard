# Size-fit plan: `cek_context_step_semantic_v1` (validation-trace CEK context step)

Companion to [00-primer.md](00-primer.md). This plan also holds the
family-wide probe table and the chain conventions shared with
[validation-trace-cek-core-step-semantic-v1.md](validation-trace-cek-core-step-semantic-v1.md)
and
[validation-trace-cek-execution-selection-semantic-v1.md](validation-trace-cek-execution-selection-semantic-v1.md).

## 1. Identity

| Field                  | Value                                                                                                                                                                                                                                                                                                                                                           |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title        | `fraud_proofs/validation_trace/cek_context_step_semantic_v1.main.spend` (and `.else`)                                                                                                                                                                                                                                                                           |
| File                   | `onchain/aiken/validators/fraud-proofs/validation-trace/cek-context-step-semantic-v1.ak` (88 lines)                                                                                                                                                                                                                                                             |
| Raw size               | 94,268 bytes (measured 2026-09-01, pinned fork `v1.1.23+5adf783`, fresh copy build)                                                                                                                                                                                                                                                                             |
| Applied parameters     | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId`, `field_preimage_certificate_policy_id: PolicyId` (3)                                                                                                                                                                                                                                 |
| Phase / index          | phase `Cek`, resolver index 11 (`cek_v1.main`, a `prepare_selected` over 4 semantic hashes), semantic resolver index 2                                                                                                                                                                                                                                          |
| Library entry point    | `validation_machine_v1.verify_cek_context_step_semantics_v1(pre, transition, auxiliary, door)` wrapped by `validation_semantic_v1.continue_winning`                                                                                                                                                                                                             |
| Role name today        | none (published hash-checked, no auth-role token; the only CEK role in the roster is the retired `V1ValidationTraceCekResolver0`)                                                                                                                                                                                                                               |
| Deployment entry today | `validationTraceDisputeCekContextStepSemantic` (`VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1[2]`, `demo/midgard-fault-proofs/src/validation-dispute/submit.ts:815`)                                                                                                                                                                          |
| SDK title key          | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.cekContextStep` (`demo/midgard-sdk/src/fraud-proof/contracts.ts:458`)                                                                                                                                                                                                                                    |
| Emulator today         | published only in `submit-init-emulator-validation-dispute.test.ts` ("publishes and verifies the generated-blueprint CEK semantic-resolver reference scripts") with `oversized: true` under `maxTxSize: 262_144`; never driven through a dispute journey (the only CEK journey, `submit-init-emulator-cek-value-and-mint-v1.test.ts`, disputes the finish kind) |

The step this resolver proves is one of fourteen context-build stages
(`CekContextControlV1.stage` 0..13, stage 7 unused) that turn the native
transaction into the Plutus script context before execution starts.

## 2. Why it is this size

### 2.1 Method

Copy built at `/tmp/size-probe-cek` per the primer; private functions made
`pub` in the copy only; throwaway validators under `validators/probe/` take an
opaque `Data` redeemer and `expect` the argument tuple, so every probe pays the
decoder for its argument types. Decode-only baselines (`d_*`) measure that
decoder cost; a function's cost is `probe - baseline`. Full raw list:
`/tmp/cekprobe-sizes*.txt` while the copy existed; the copy has been deleted.

### 2.2 Family-wide table (bytes)

| Probe               | What it measures                                                                                          |    Raw | Baseline |                                                     Cost |
| ------------------- | --------------------------------------------------------------------------------------------------------- | -----: | -------: | -------------------------------------------------------: |
| `d_none`            | empty spend validator                                                                                     |     94 |        – |                                                        – |
| `d_wit`             | decode `(ValidationMachineStateV1, ValidationOneStepWitnessV1)`                                           |    860 |       94 |                                                      766 |
| `d_aux`             | decode `ValidationAuxiliaryWitnessV1` (the full 40-arm sum type)                                          | 13,356 |       94 |                                               **13,262** |
| `d_control`         | decode `NativeScriptsControlV1` record                                                                    |    663 |       94 |                                                      569 |
| `d_ctxctl`          | decode `CekContextControlV1` record                                                                       |    550 |       94 |                                                      456 |
| `d_step`            | decode `(MachineStateV1, MachineStateV1, CoreStepWitnessV1)`                                              |  5,330 |       94 |                                                    5,236 |
| `p_ctl_decode`      | `cek_witness_control_v1` (work-witness cbor → control)                                                    |  2,592 |      860 |                                                    1,732 |
| `p_wf`              | `cek_witness_control_v1` + `cek_witness_is_well_formed_v1`                                                |  7,228 |      860 |                                                **6,368** |
| `p_finish_lib`      | `verify_cek_finish_semantics_v1`                                                                          |  8,289 |      860 |                                                    7,429 |
| `p_dispatch`        | cek-finish validator with the semantic predicate replaced by `True`, full `ActionV1` redeemer (aux typed) | 16,520 |        – |                                                        – |
| `p_dispatch_narrow` | same with `auxiliary: Data`                                                                               |  3,278 |        – | **3,184** (cancel + `continue_winning` + `Datum` decode) |
| `y_skel`            | withdraw validator: unique-dispatch lookup + typed context `SpendRedeemer` decode                         | 14,936 |        – |                                                        – |
| `y_skel_narrow`     | same with `auxiliary: Data`                                                                               |  1,645 |        – |                                                **1,551** |
| `p_ctxctl_codec`    | `cek_context_control_from_cbor` + `cek_context_control_is_well_formed` + `encode_cek_context_control_v1`  |  8,400 |      663 |                                                **7,737** |
| `p_ctx_succ`        | `cek_context_successor_is_exact` (re-encode witness + control, hash work witness)                         | 17,680 |   15,078 |                                                    2,602 |
| `p_ctx_all`         | `verify_cek_context_step` (all stages)                                                                    | 90,145 |   15,078 |                                               **75,067** |
| `p_ctx_lib`         | `verify_cek_context_step_semantics_v1` (entry point)                                                      | 92,312 |   14,141 |                                                   78,171 |

Whole-body reconciliation: 3.2 (dispatcher) + 13.3 (aux decoder) + 6.4
(shared prefix) + 7.7 (context-control codec) + 75.1 (stages, which already
include the codec and successor once) − overlaps ≈ 94.3 KB. Measured 94,268.

### 2.3 Per-stage table (bytes, baseline `d_ctx` = 15,078, includes `cek_context_successor_is_exact`)

| Stage | Function                            |    Raw |       Cost | Dominant callee (round-2/3 probes)                                                                                               |
| ----: | ----------------------------------- | -----: | ---------: | -------------------------------------------------------------------------------------------------------------------------------- |
|     0 | `verify_cek_redeemer_selection`     | 56,038 | **40,960** | `redeemer_item_proof_v1.step_v1` 34,164 (of which `cek_data_traverse_v1.step_v1` 27,649), `hash_control_v1` 7,641                |
|     1 | `verify_cek_reference_context_step` | 26,777 |     11,699 | `script_context_v1.prepend_resolved_descriptor_tx_in_info_v1` 7,960                                                              |
|     2 | `verify_cek_spend_context_step`     | 26,777 |     11,699 | same as stage 1                                                                                                                  |
|     3 | `verify_cek_output_context_step`    | 22,434 |      7,356 | `prepend_output_descriptor_v1` 4,584                                                                                             |
|     4 | `verify_cek_signer_context_step`    | 21,761 |      6,683 | `prepend_signer_v1` 3,944                                                                                                        |
|     5 | `verify_cek_observer_context_step`  | 28,830 |     13,752 | `verify_native_tx_proof_source_v1` 1,969, `field_door.open_machine_field_item` (+proof source) 5,089                             |
|     6 | `verify_cek_mint_context_init`      | 19,288 |      4,210 | –                                                                                                                                |
|     8 | `verify_cek_mint_context_item`      | 23,334 |      8,256 | merkle membership + `finalize_current_mint_policy`                                                                               |
|     9 | `verify_cek_redeemer_data_step`     | 59,622 | **44,544** | `redeemer_item_proof_v1.step_v1` 34,164, `script_purpose_summary_v1` 3,812, `hash_control_v1` 7,641                              |
|    10 | `verify_cek_context_finalize`       | 36,706 | **21,628** | `cardano_script_info_summary_v1` 13,004, `cardano_spend_script_info_from_descriptor_v1` 6,880, `script_purpose_summary_v1` 3,812 |
|    11 | `verify_cek_context_assemble`       | 21,987 |      6,909 | `tx_info_tail_fields_summary_v1` 3,729                                                                                           |
|    12 | `verify_cek_tx_info_finalize`       | 23,851 |      8,773 | `tx_info_from_tail_summary_v1` 3,625, `decode_native_tx_compact_v1`                                                              |
|    13 | `verify_cek_context_seed`           | 21,842 |      6,764 | `script_context_summary_v1` 1,910, `hash_state_v1`                                                                               |

Redeemer-item machine, round 2/3 (baseline `d3_rip` 2,720 / `d3_trav` 2,146):
`step_v1` 34,164; `control_is_well_formed` 6,156; `hash_control_v1` 7,641;
`header_step` 7,221; `tail_step` 7,372; `cek_data_traverse_v1.step_v1` 27,649
with per-stage `step_head` 9,910, `step_integer` 14,311, `step_bytes` 15,718,
`step_large_constructor` 10,668, `step_large_fields` 8,650, `step_close` 6,396,
`step_fold` 11,728, `control_is_well_formed` 4,548, `hash_control_v1` 5,796.

### 2.4 Conclusions

1. The single largest avoidable cost is the redeemer decoder for
   `ValidationAuxiliaryWitnessV1` (13.3 KB). It is paid because `ActionV1`
   types the field; the evidence hash is over the aux as `Data`, so typing
   it is not load-bearing. Reading it as `Data` and projecting the one arm a
   stage needs (`builtin.un_constr_data`) is wire-identical.
2. The shared CEK prefix (`cek_witness_control_v1` + well-formedness) is
   6.4 KB and the context-control codec is 7.7 KB; together with the 3.2 KB
   thread handshake they exceed 15 KB by themselves once any stage logic is
   added. No single script can hold "prefix + codec + one stage".
3. Stages 0 and 9 embed the redeemer-item proof machine (34 KB), whose data
   traversal (27.6 KB) is itself above 15 KB even per traversal stage
   (`step_bytes` 15.7 KB). That machine is shared with ScriptSources stage
   one (`redeemer_item_proof_v1.step_v1` has six call sites in
   `validation-machine-v1.ak`, lines 8823, 11409, 11960, 12928, 15520,
   16354); its `mode_data` decomposition belongs to the script-sources
   stage-one-redeemer plan (the RF-021 chain extension) and this plan
   consumes it (§10).
4. ExUnits (aiken check, includes fixture construction): stage-13
   `cek_context_seed_fits_one_step` 11.95 M mem / 5.15 B cpu; stage-11
   `cek_context_assemble_fits_one_step` 11.35 M / 4.97 B; stage-12
   `cek_context_tx_info_finalize_fits_one_step` 14.26 M / 6.35 B; stage-10
   `cek_spend_finalize_authenticates_descriptor_datum_summary` **19.09 M /
   8.38 B**; observer max-224 (two steps) 17.44 M / 7.83 B; validator-level
   `context_step_validator_wins_the_mint_context_initialisation` 8.60 M /
   3.74 B. The finish vector (`finish_validator_wins_the_hand_off`, 4.54 M)
   bounds the fixture overhead at roughly 3–4 M, so the honest stage-10 step
   is already near the 13.2 M basis of GOAL_SPEC §3.3 in one script. Any
   split that adds scripts to that transaction breaches the basis.

## 3. Options considered

| Option                                     | Verdict                                | Reason                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| ------------------------------------------ | -------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Prune only                                 | Keep as first step, insufficient alone | Aux-as-`Data` saves 13.3 KB (94.3 → 81 KB); `hash_control_v1` specialised for a `None` traversal (stage 0 begin, stage 9 select) saves ~5 KB in those arms. Nothing else is reachable-but-unused: every stage is a real arm of `context_control.stage`.                                                                                                                                                                                                                                                              |
| Withdraw-zero yields, one tx               | Rejected                               | (a) 2.4 says the prefix+codec+handshake already exceed 15 KB, so the dispatcher itself would need yields, and each yield re-parses a redeemer whose aux can carry a 4 KB chunk (`max_blob_chunk_bytes = 4095`). (b) Aggregate budget: stage 10 measures 19.09 M with fixture, i.e. ≈ 15 M honest; adding a dispatcher plus one or two yields (each re-decoding the witness) lands above the 16.5 M cap, not just the 13.2 M basis. (c) 13 stages × 1–3 yields ≈ 25 role NFTs, manifest rows and stake registrations. |
| Multi-transaction chain (primer pattern 3) | **Chosen**                             | Each hop has its own 16.5 M / 10 B budget and its own 15 KB body; hops need no role NFT (published hash-checked like today's CEK entries); the family already has the RF-021 chain precedent with an off-chain driver (`submitSplitStage` in `submitValidationDisputeSemanticResolution`); bisection isolates one step per dispute, so extra hops cost +4..+7 transactions per dispute, not per machine step.                                                                                                        |
| Redesign                                   | Rejected                               | The arm boundaries (stages) are the machine's own; nothing is wrong with them.                                                                                                                                                                                                                                                                                                                                                                                                                                       |

## 4. Chosen design

### 4.1 Shared chain conventions (used by the core-step plan too)

New library module `onchain/aiken/lib/midgard/cek-semantic-chain-v1.ak`:

```aiken
pub const version: Int = 1

/// Every hop's datum starts with this. `binder_script_hash` is the
/// `input_script_hash` the binder saw in `continue`, recorded so the
/// settle hop can pin provenance without a parameter cycle.
pub type CekChainBaseV1 {
  base: validation_resolution_v1.PreparedValidationResolutionStateV1,
  binder_script_hash: ScriptHash,
}

/// Re-bind a hop's redeemer to the prepared evidence: the transition and
/// the auxiliary (as Data) must hash to `base.evidence_hash`, the base must
/// be well formed and in phase Cek. Cheap (~0.5 KB); every hop that reads
/// the transition or auxiliary from its own redeemer runs it.
pub fn base_is_bound_v1(base, transition, auxiliary: Data) -> Bool

/// `fraud_proofs/common.continue` with the output script hash and the exact
/// output datum pinned (the RF-021 shape).
pub fn continue_exact_v1(ct_policy, step_datum, input_index, output_index,
  own_out_ref, inputs, outputs, expected_output_script_hash,
  expected_output_state: Data) -> Bool

/// Last hop: `output_script_hash == award_script_hash` and
/// `output_state == validation_resolution_v1.winning_resolution()`.
pub fn award_continuation_is_exact_v1(award_script_hash,
  output_script_hash, output_state: Data) -> Bool
```

and, in `validation-machine-v1.ak`, Data-projection helpers that replace the
typed aux decoder:

```aiken
pub fn auxiliary_constr_v1(aux: Data) -> Pair<Int, List<Data>>   // builtin.un_constr_data
pub fn expect_aux_arm_v1(aux: Data, tag: Int) -> List<Data>      // fails unless constr tag matches
```

Rules every hop follows: `ct.Cancel` handled by `fraud_proofs/common.cancel`
unchanged; `ct.Continue` pins the next hop's script hash from a parameter
and the next datum exactly; hop k+1's hash is a parameter of hop k (never
the reverse), the award hash is a parameter of the last hop only, so the
applied-parameter graph is acyclic: settle → verifiers → control binder →
binder, and `cek_v1` keeps the binder in slot 2. Binder redeemers keep the
present field order and carry the auxiliary as `Data` (wire-identical), so
`context_step_wire_layout_is_pinned` still passes.

### 4.2 Hop list

All new files live in `onchain/aiken/validators/fraud-proofs/validation-trace/`.
Sizes are projections from §2 components (component sums; shared
infrastructure counted once per script); "marginal" means 14–15.5 KB and
must be re-probed at implementation.

| #    | Validator (title)                                        | File                                       | Responsibility                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | Parameters                                                                                                           | Datum in → out                                                                                                                                            |                                                                                                                                                                                                                                                                                 Projected |
| ---- | -------------------------------------------------------- | ------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: |
| B0   | `cek_context_step_semantic_v1.main` (unchanged title)    | `cek-context-step-semantic-v1.ak`          | witness binder: `base_is_bound_v1`; `cek_witness_control_v1`; `cek_witness_is_well_formed_v1`; `cek_control_is_context_step_v1`; `execution_cursor < execution_count`; both limits `== 0`                                                                                                                                                                                                                                                                                                                   | `control_binder_script_hash`, `computation_thread_policy_id`                                                         | `Datum` (prepared) → `CekContextBoundV1 { chain: CekChainBaseV1, execution_cursor, completed_cpu, completed_memory, program_envelope_hash }`              |                                                                                                                                                                                                                                                       3.2 + 6.4 + 0.5 + 1.0 ≈ **11.1 KB** |
| B1   | `cek_context_step_control_v1.main`                       | `cek-context-step-control-v1.ak`           | control binder: `base_is_bound_v1`; `cek_witness_control_v1`; `cek_context_control_from_cbor`; `encode_cek_context_control_v1(ctx) == cbor`; `cek_context_control_is_well_formed`; `cek.program_envelope_hash == ctx.program_envelope_hash`; stage ∈ {0..6, 8..13}; routes to `stage_verifier_script_hashes[stage]`                                                                                                                                                                                         | `stage_verifier_script_hashes: List<ScriptHash>` (14, index = stage, slot 7 = `#""`), `computation_thread_policy_id` | `CekContextBoundV1` → `CekContextStagedV1 { bound, context_control: CekContextControlV1, native_facts: CekContextNativeFactsV1 }`                         |                                                                                                                                                                                                                                            4.2 + 1.7 + 7.7 + 0.8 ≈ **14.4 KB** (marginal) |
| V0a  | `cek_context_stage_redeemer_begin_v1.main`               | `cek-context-stage-redeemer-begin-v1.ak`   | stage 0, `RedeemerScanBeginWitness` (aux tag 10): item leaf, redeemer membership, `initial_control_v1`, `redeemer_item_proof_v1.hash_descriptor_control_v1` (stage-ten-match plan §4b: traversal `None`, no traverse encoder)                                                                                                                                                                                                                                                                               | `settle_script_hash`, ct policy                                                                                      | `CekContextStagedV1` → `CekContextVerifiedV1 { staged, successor: ContinueContext(next) }`                                                                |                                                                                                                                                                                                                                                 5.5 + 1.0 + 2.0 + 1.0 + 2.0 ≈ **11.5 KB** |
| V0b  | `cek_context_stage_redeemer_item_v1.main`                | `cek-context-stage-redeemer-item-v1.ak`    | stage 0 and stage 9 `RedeemerItemStepWitness` glue (aux tag 18): `current_matches` (control hash via the item-proof chain's committed hash), stage-9 `finalize_v1`/`map_items` prepend/`current_redeemer` selection, `cek_redeemer_context_control_is_well_formed`; delegates `redeemer_item_proof_v1.step_v1(item_control, item_witness) == Some(Advanced(next))` to the shared item-proof hop chain (§10) through `CekContextItemStepPendingV1 { staged, item_control, item_witness_hash, claimed_next }` | `item_proof_entry_script_hash`, `settle_script_hash`, ct policy                                                      | `CekContextStagedV1` → pending → (item-proof hops) → `CekContextVerifiedV1`                                                                               |                                                                                                                                                                                                                                           5.5 + 2.0 + 3.0 + 2.0 ≈ **12.5 KB** (glue only) |
| V9a  | `cek_context_stage_redeemer_select_authenticate_v1.main` | `…-redeemer-select-authenticate-v1.ak`     | stage 9 `CekRedeemerContextSelectWitness` (tag 17) part 1: redeemer-item membership, purpose membership, `current_matches` on the redeemer-context control                                                                                                                                                                                                                                                                                                                                                  | `select_open_script_hash`, ct policy                                                                                 | staged → `CekContextSelectPendingV1 { staged, item_index, item_count, total_length, item_commitment, purpose_kind, purpose_index, script_hash, subject }` |                                                                                                                                                                                                                                                             5.5 + 2.0 + 4.0 ≈ **11.5 KB** |
| V9b  | `cek_context_stage_redeemer_select_open_v1.main`         | `…-redeemer-select-open-v1.ak`             | part 2: `script_purpose_summary_v1` (3.8), `initial_control_v1` (descriptor or data mode), `hash_descriptor_control_v1`, `cek_redeemer_context_main_successor` control update                                                                                                                                                                                                                                                                                                                               | `settle_script_hash`, ct policy                                                                                      | select-pending → `CekContextVerifiedV1`                                                                                                                   |                                                                                                                                                                                                                                                 5.5 + 3.8 + 1.0 + 2.0 + 1.0 ≈ **13.3 KB** |
| V12  | `cek_context_stage_resolved_item_v1.main`                | `cek-context-stage-resolved-item-v1.ak`    | stages 1 and 2 (`CekResolvedContextItemWitness`, tag 13; `source_kind` 1 / 0 fixed by stage): `prepend_resolved_descriptor_tx_in_info_v1` + terminal `NoAuxiliaryWitness` → stage+1                                                                                                                                                                                                                                                                                                                         | `settle_script_hash`, ct policy                                                                                      | staged → verified                                                                                                                                         | 5.5 + 1.0 + 9.1 ≈ **15.6 KB** — split into `resolved-item-authenticate` (membership of `resolved_context_item_leaf_hash`, ≈ 8 KB) and `resolved-item-summarize` (descriptor decode + `tx_in_info_data_v1` + `prepend_authenticated_list_item_v1`, ≈ 12 KB) if the probe confirms > 15,000 |
| V3   | `cek_context_stage_output_v1.main`                       | `cek-context-stage-output-v1.ak`           | stage 3 (`CekOutputContextItemWitness`, tag 14): `prepend_output_descriptor_v1` (4.6)                                                                                                                                                                                                                                                                                                                                                                                                                       | `settle_script_hash`, ct policy                                                                                      | staged → verified                                                                                                                                         |                                                                                                                                                                                                                                                             5.5 + 1.0 + 4.8 ≈ **11.3 KB** |
| V4   | `cek_context_stage_signer_v1.main`                       | `cek-context-stage-signer-v1.ak`           | stage 4 (`CekSignerContextItemWitness`, tag 15): `prepend_signer_v1` (3.9)                                                                                                                                                                                                                                                                                                                                                                                                                                  | same                                                                                                                 | staged → verified                                                                                                                                         |                                                                                                                                                                                                                                                             5.5 + 1.0 + 4.1 ≈ **10.6 KB** |
| V5a  | `cek_context_stage_observer_open_v1.main`                | `cek-context-stage-observer-open-v1.ak`    | stage 5 part 1 (`TransactionFieldChunkWitness`, tag 1): `verify_native_tx_proof_source_v1`, `field_door.machine_field_count`/`open_machine_field_item` on field 3 at the derived index, item length 28, `active_count` bounds                                                                                                                                                                                                                                                                               | `field_preimage_certificate_policy_id` (moves here from B0), `observer_fold_script_hash`, ct policy                  | staged → `CekContextObserverPendingV1 { staged, observer_hash, active_count }`                                                                            |                                                                                                                                                                                                                                           4.2 + 1.7 + 1.0 + 2.0 + 3.3 + 1.0 ≈ **13.2 KB** |
| V5b  | `cek_context_stage_observer_fold_v1.main`                | `cek-context-stage-observer-fold-v1.ak`    | stage 5 part 2 and the two terminal branches: strict descending order vs `previous_observer`, `prepend_cek_observer_item_v1`, `finalize_cek_observer_items_v1`, empty-commitment path                                                                                                                                                                                                                                                                                                                       | `settle_script_hash`, ct policy                                                                                      | observer-pending or staged → verified                                                                                                                     |                                                                                                                                                                                                                                                                    5.5 + 3.0 ≈ **8.5 KB** |
| V68  | `cek_context_stage_mint_v1.main`                         | `cek-context-stage-mint-v1.ak`             | stages 6 and 8 (`CekMintContextItemWitness`, tag 16, or none): `mint_asset_leaf_hash` membership, policy grouping, `finalize_current_mint_policy`                                                                                                                                                                                                                                                                                                                                                           | same                                                                                                                 | staged → verified                                                                                                                                         |                                                                                                                                                                                                                                                             5.5 + 1.0 + 7.3 ≈ **13.8 KB** |
| V10a | `cek_context_stage_finalize_spend_v1.main`               | `cek-context-stage-finalize-spend-v1.ak`   | stage 10, Cardano spend (`CekContextFinalizeSpendWitness`, tag 20): descriptor membership (`resolved_context_item_leaf_hash`), `cardano_spend_script_info_from_descriptor_v1` (6.9), `completed_redeemer_context_matches`, parts hash                                                                                                                                                                                                                                                                       | same                                                                                                                 | staged → verified                                                                                                                                         |                                                                                                         5.5 + 1.5 + 1.5 + 6.9 + 3.0 ≈ **18.4 KB** → two hops: `finalize-spend-authenticate` (membership, commits `descriptor_cbor` hash; ≈ 9 KB) and `finalize-spend-summarize` (≈ 13 KB) |
| V10b | `cek_context_stage_finalize_midgard_v1.main`             | `cek-context-stage-finalize-midgard-v1.ak` | stage 10, `language_tag == 128` (`CekContextFinalizeWitness`, tag 19): `script_purpose_summary_v1(…, True)`                                                                                                                                                                                                                                                                                                                                                                                                 | same                                                                                                                 | staged → verified                                                                                                                                         |                                                                                                                                                                                                                                                       5.5 + 1.0 + 3.8 + 3.0 ≈ **13.3 KB** |
| V10c | `cek_context_stage_finalize_cardano_v1.main`             | `cek-context-stage-finalize-cardano-v1.ak` | stage 10, Cardano non-spend (`cardano_script_info_summary_v1(kind, hash, subject, None)`, 13.0 KB)                                                                                                                                                                                                                                                                                                                                                                                                          | same                                                                                                                 | staged → verified                                                                                                                                         |                                                                                   5.5 + 1.0 + 13.0 + 3.0 ≈ **22.5 KB** → split `cardano_script_info_summary_v1` by `purpose_kind` (mint 1 / withdraw 2 / observe 3) into per-kind entry points and one validator each; probe recipe in §5 |
| V11  | `cek_context_stage_assemble_v1.main`                     | `cek-context-stage-assemble-v1.ak`         | stage 11 (`CekContextAssembleWitness`, tag 21): `hash_cek_context_parts_control_v1`, `tx_info_tail_fields_summary_v1`, `hash_cek_tx_info_assembly_control_v1`                                                                                                                                                                                                                                                                                                                                               | same                                                                                                                 | staged → verified                                                                                                                                         |                                                                                                                                                                                                                                                             5.5 + 1.0 + 4.3 ≈ **10.8 KB** |
| V12b | `cek_context_stage_tx_info_v1.main`                      | `cek-context-stage-tx-info-v1.ak`          | stage 12 (`CekTxInfoFinalizeWitness`, tag 22): `decode_native_tx_compact_v1` (fee, validity), `tx_info_from_tail_summary_v1`, `hash_cek_final_context_control_v1`                                                                                                                                                                                                                                                                                                                                           | same                                                                                                                 | staged → verified                                                                                                                                         |                                                                                                                                                                                                                                      4.2 + 1.7 + 1.0 + 1.0 + 6.2 ≈ **14.1 KB** (marginal) |
| V13  | `cek_context_stage_seed_v1.main`                         | `cek-context-stage-seed-v1.ak`             | stage 13 (`CekContextSeedWitness`, tag 23): `script_context_summary_v1`, `semantic_data_constant_root_v1`, `hash_context_constant_term_v1`, `hash_application_term_v1`, initial `MachineStateV1` + `hash_state_v1`                                                                                                                                                                                                                                                                                          | same                                                                                                                 | staged → `CekContextVerifiedV1 { staged, successor: StartExecution { initial_state_hash, execution_cpu_limit, execution_memory_limit } }`                 |                                                                                                                                                                                                                                                             5.5 + 1.0 + 4.2 ≈ **10.7 KB** |
| S    | `cek_context_step_settle_v1.main`                        | `cek-context-step-settle-v1.ak`            | successor settlement: `base_is_bound_v1`; `cek_witness_control_v1`; `staged.bound.chain.binder_script_hash == binder_script_hash`; `input_script_hash == stage_verifier_terminal_hashes[stage]`; `ContinueContext(next)` → `encode_cek_context_control_v1(next)` and `cek_context_successor_is_exact`; `StartExecution` → `cek_context_execution_successor_is_exact` shape with the committed hash and limits; `award_continuation_is_exact_v1`                                                             | `award_script_hash`, `binder_script_hash`, `stage_verifier_terminal_hashes: List<ScriptHash>` (14), ct policy        | `CekContextVerifiedV1` → `WinningValidationResolutionStateV1`                                                                                             |                                                                                                                                                                                                                                     4.2 + 0.5 + 1.7 + 2.5 + 2.6 + 0.3 + 0.5 ≈ **12.3 KB** |

`CekContextNativeFactsV1` (written by B1, read by verifiers so they avoid the
1.7 KB control decode) carries `resolved_input_count`, `spend_input_count`,
`resolved_item_peaks`, `signer_count`, `signer_frontier_commitment`,
`output_count`, `output_descriptor_peaks`, `mint_count`, `mint_peaks`,
`redeemer_count`, `redeemer_peaks`, `purpose_count`, `purpose_peaks`,
`execution_count`, `transaction_id`. V5a and V12b need the compact
transaction bytes and decode the control themselves.

Library refactor (ABI-neutral, keeps the aggregate verifier and the
partition tests valid): each `verify_cek_<stage>` in `validation-machine-v1.ak`
is split into `cek_<stage>_successor_v1(...) -> Option<CekContextSuccessorV1>`
(pure stage logic) and the shared
`cek_context_successor_matches_v1(pre, witness, native_control, successor,
cursor, cpu, mem, envelope_hash)`; `verify_cek_context_step` becomes the
composition, so `cek_context_step_route_agrees_with_the_aggregate` and
`cek_kinds_partition_the_cek_step_space` keep proving the monolith and the
chain agree.

```aiken
pub type CekContextSuccessorV1 {
  ContinueContext { next_control: CekContextControlV1 }
  StartExecution { initial_state_hash: ByteArray, execution_cpu_limit: Int, execution_memory_limit: Int }
}
```

### 4.3 Redeemer and datum ABI deltas

- B0 redeemer: `VerifyContextStep { input_index, output_index, transition, auxiliary: Data }` — Aiken type changes, wire unchanged.
- B1/V\*/S redeemers: `ct.StepRedeemer<{ input_index, output_index, transition: ValidationOneStepWitnessV1, auxiliary: Data }>` (same shape at every hop so the SDK reuses one encoder; V0b adds `claimed_next: RedeemerItemProofControlV1`, V5a adds nothing).
- New datum types: `CekContextBoundV1`, `CekContextStagedV1`, `CekContextObserverPendingV1`, `CekContextSelectPendingV1`, `CekContextItemStepPendingV1`, `CekContextVerifiedV1` (all `ct.StepDatum<…>` payloads), in `cek-semantic-chain-v1.ak`. Golden wire tests required (§8).
- B0 parameters change from 3 to 2 (`field_preimage_certificate_policy_id` moves to V5a).

### 4.4 Handshake and security argument

- **Dispatch uniqueness.** Every hop is a spend of the one thread UTxO
  carrying the computation-thread NFT; `fraud_proofs/common.continue`
  requires exactly that NFT in and out and no other tokens, so one
  transaction advances one thread by one hop. No withdrawal is involved.
- **Provenance / role authentication.** Hop k+1 only accepts a thread
  whose datum is the exact record hop k produces; hop k only pays to hop
  k+1's parameterised hash. Thread NFTs exist only inside deployed chains
  (every deployed `continue` pins its output script; `Init` mints at a
  catalogue-checked first step), so a `CekContextStagedV1` at a verifier
  address can only have come from B1. S additionally pins
  `binder_script_hash` (recorded by B0 from `input_script_hash`) and
  `input_script_hash == stage_verifier_terminal_hashes[stage]`, the same
  belt-and-braces RF-021 uses (`envelope_binder_script_hash`).
- **Cross-arm substitution.** B1 routes on the decoded `stage`; each
  verifier re-asserts `staged.context_control.stage == its stage` and refuses
  any other; S checks the verifier that ran matches the stage recorded. A
  prover cannot run the mint verifier on a stage-9 control.
- **Evidence binding.** Every hop that reads `transition`/`auxiliary` runs
  `base_is_bound_v1` (hash equals `base.evidence_hash`), so all hops verify
  the same evidence the prepare committed; B1's `context_control` record is
  a decode of bytes committed by the transition's work root, re-checked by S
  through `cek_context_successor_is_exact` (which re-encodes and re-hashes).
- **Output-state re-derivation.** Each hop pins the next datum exactly; S
  pins `winning_resolution()` and the award hash, exactly as
  `continue_winning` does today.
- **Omitted hop.** A thread cannot skip a hop: the datum type at each
  address is different and the next hop's `expect` on it fails. If a
  verifier were omitted from the deployment list, B1 would route to `#""`
  and fail closed (`select_semantic_resolver`-style length pin on the
  14-list).
- **Cancel.** `ct.Cancel` works at every hop through the generic `cancel`.

## 5. Size and budget projection

- Raw sizes: table in §4.2; every hop ≤ 15,000 with three marginal hops (B1
  14.4, V12b 14.1, V68 13.8) and three named second-level splits (V12
  resolved-item, V10a finalize-spend, V10c finalize-cardano). Method: sums
  of §2 probe deltas; shared infrastructure counted once per script;
  the 3.2 / 4.2 KB skeleton figures are `p_dispatch_narrow` and
  `p_dispatch_narrow` + binding decode + `base_is_bound_v1`.
- Probe recipe for the open splits (same copy procedure): expose
  `cardano_script_info_summary_v1`'s per-kind branches as
  `cardano_mint_script_info_summary_v1` etc. and probe each against
  `d5_kind` (138 B); probe `prepend_resolved_descriptor_tx_in_info_v1`'s
  membership half versus its summary half against `d8_resolved` (425 B).
- Referenced bytes per transaction: exactly one hop script (≤ 15 KB) plus,
  for V5a, the field-preimage certificate reference inputs (datums, not
  scripts). Fee band: tier 1 (≤ 25,600 bytes at `minFeeRefScriptCostPerByte`
  15 lovelace/byte) → ≤ 0.23 ADA of reference-script fee per hop, versus
  ≈ 1.85 ADA today (94,268 bytes spans four tiers: 384,000 + 460,800 +
  552,960 + 452,770 lovelace).
- ExUnits: per hop, in isolation. Binder B0 ≈ the finish vector (4.54 M /
  1.93 B including fixture); B1 adds the control codec (well under 5 M);
  verifiers inherit the monolith's stage cost minus the prefix; the heaviest,
  stage-10 spend finalize (19.09 M / 8.38 B with fixture), is split into two
  hops so each stays under the 13.2 M / 8 B basis. Stage 12 (14.26 M with
  fixture) is a single hop and must be re-measured at the validator level;
  if it exceeds the basis, `tx_info_from_tail_summary_v1` and the compact
  decode split into two hops the same way.
- Transactions per dispute: today 1 semantic transaction; after: B0, B1, one
  to three verifier hops, plus the shared item-proof hops for stage 0/9 item
  steps (count owned by the script-sources plan), plus S — 4 to ~8. Against
  GOAL_SPEC C52 (5,000 cap) the journey grows from ≈ 40 to ≤ 48
  transactions; against §3.3 maturity (302,400,000 ms half window) the
  added confirmations are minutes.

## 6. Off-chain work

Exists today for this contract: SDK title `semantics.cekContextStep`,
deployment entry `validationTraceDisputeCekContextStepSemantic`, submit route
(`requireValidationCekSemanticReferenceScriptUtxo`, `readFrom` in
`submitValidationDisputeSemanticResolution`), redeemer encoder
`semanticActionFieldsV1` (resolver 11 / semantic 2 branch), shape roster
`VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES_V1`. Does not exist: any
multi-hop driver for CEK, any role, any emulator journey.

1. `demo/midgard-sdk/src/fraud-proof/contracts.ts`: add
   `cekContextStepStages: { binder, control, redeemerBegin, redeemerItem,
redeemerSelectAuthenticate, redeemerSelectOpen, resolvedItem, output,
signer, observerOpen, observerFold, mint, finalizeSpend, finalizeMidgard,
finalizeCardano, assemble, txInfo, seed, settle }` mirroring
   `scriptSourcesStageOneRedeemerStages` (titles at :283); apply parameters
   by declared name through the existing `semanticResolverParameterValues`
   map, adding `control_binder_script_hash`, `stage_verifier_script_hashes`,
   `stage_verifier_terminal_hashes`, `settle_script_hash`,
   `observer_fold_script_hash`, `select_open_script_hash`,
   `item_proof_entry_script_hash`, `binder_script_hash` (build order:
   settle → verifiers → control → binder). `zz605-semantic-resolver-arity`
   sees them automatically (it discovers declared parameters); the
   `validation-resolver-applied-hashes.test.ts` assertion
   `contextStepValidator.parameters).toHaveLength(3)` becomes 2.
2. Deployment entries: one `validationTraceDisputeCekContextStep<Hop>`
   entry per hop in `VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`
   (restructure it to a per-hop map keyed by semantic index 2), consumed by
   `requireValidationCekSemanticReferenceScriptUtxo` per hop. No role token
   (same hash-checked consumption as today), so no
   `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` / `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES`
   change and no stake registration. Node: `contract-deployment-info.ts`
   spend descriptors and `transactions/reference-scripts.ts` publication
   targets for every hop; the inspection fixtures under
   `demo/midgard-node` that enumerate deployment entries gain the rows.
3. Submit route: in `submitValidationDisputeSemanticResolution` add an
   `isSplitCekContextStep` branch (resolver 11, semantic 2) driving hops
   with `submitSplitStage` exactly like `isSplitScriptSourcesStageOne`;
   derive the hop datums in a new `deriveCekContextStepRouteDataV1` (mirror
   of `deriveScriptSourcesStageOneRouteDataV1`) from the staged one-step
   argument: it needs the decoded context control and per-stage successor,
   which `demo/midgard-validation/src/cek-context.ts` already computes for
   the trace. One redeemer encoder `encodeCekContextStepSpendRedeemerV1({ stage, inputIndex, outputIndex, transition, auxiliary, claimedNext? })`.
4. Codecs: `demo/midgard-sdk/src/fraud-proof/validation-dispute.ts` gains
   the six binding datum schemas; `demo/midgard-validation` needs no machine
   change (the trace is unchanged).
5. Funding requirements: each hop is a normal thread continuation paid by
   the challenger's fee input; add the hop count to the challenger-runbook
   funding row for a CEK dispute.

## 7. Emulator scenario tests

Exists today: publication of the three oversized CEK bodies with
`oversized: true` under a 262,144 `maxTxSize` (`submit-init-emulator-validation-dispute.test.ts:156`);
the finish-kind journey (`submit-init-emulator-cek-value-and-mint-v1.test.ts`).
`dispute-scenario.ts` publishes the selected semantic under
`withRealL1MaxTxSize` unless `semanticIsOversized`.

Add / change:

1. `submit-init-emulator-validation-dispute.test.ts`: replace the CEK
   publication test with one that publishes every hop of all three CEK
   families under `withRealL1MaxTxSize` **without** `oversized`, asserts
   `publicationMeasurement.l1ByteMargin > 0`, runs
   `assertReferenceScriptRawBodiesFitL1EnvelopeV1` on all of them, and drops
   the `maxTxSize: 262_144` emulator override. Remove the
   `expect(appliedResolverBytes).toBeGreaterThan(maxTxSize)` assertions.
2. New `submit-init-emulator-cek-context-step-v1.test.ts` (one journey per
   file; see the wasm-heap note in the finish journey): fixture
   `buildForgedOperatorSuccessorValidationDisputeFixture({ disputedPhase:
"cek", cekStep: { kind: "context-step", stage } })` in
   `tests/support/submit-init-emulator-shared.ts`, built over an L2
   transaction whose one script execution is the one-node `error`
   PlutusV3 program used by `cek-split-v1.test.ak` (so the trace has
   selection, all context stages and a core step). Positive lifecycle
   through award and removal for stage 6 (smallest) and stage 13 (seed);
   valid-block negative (operator honest at the same frontier → challenger
   loses / `Cancel` path); cancel/resume at B1 and at S; maximum shape:
   stage 5 with 224 observers and stage 8 with the maximum mint set (the
   lib fixtures behind `cek_context_observer_cardano_maximum_224_first_item_and_terminal_agree`
   and `cek_context_maximum_mint_authenticates_last_asset_membership`),
   each asserting `completeSignedBytes ≤ 16,384` and mem/cpu ≤ the §3.3
   basis per hop via the existing `measureCompleteSignedTransaction` /
   proof-fit printing.
3. `dispute-scenario.ts`: the `semanticIsOversized` branch stays for
   ValueAndMint only; for resolver 11 the stage publications use the real
   envelope. `dispute-staging.ts` unchanged.

## 8. Aiken tests

- `validators/fraud-proofs/validation-trace/cek-split-v1.test.ak`: keep all
  44 tests (wire goldens unchanged for the binder); add goldens for each new
  redeemer and datum type; drive every hop through `main.spend` on the
  smallest honest step of its stage (reuse `context_step_fixture` for stage
  6 and add fixtures for 0-begin, 3, 4, 11, 12, 13 from the lib fixtures);
  negatives per hop: wrong `stage` in datum, foreign `base` (evidence hash
  mismatch), skipped hop (staged datum paid straight to S → S refuses on
  `input_script_hash`), wrong next-hop hash, tampered `next_control`
  (S refuses on work root), `binder_script_hash` mismatch at S, cancel at
  each hop.
- `lib/midgard/validation-machine-v1.test.ak`: `cek_context_chain_agrees_with_the_aggregate`
  — for every existing context fixture, `cek_<stage>_successor_v1` followed
  by `cek_context_successor_matches_v1` equals `verify_cek_context_step`;
  `cek_kinds_partition_the_cek_step_space` unchanged.
- `lib/midgard/cek-semantic-chain-v1.test.ak`: `base_is_bound_v1`
  positives/negatives, datum encode goldens (cross-language vectors mirrored
  in `demo/midgard-sdk` tests).
- Property: `hash_descriptor_control_v1(c) == hash_control_v1(c)` for every
  control with `traversal == None` (fuzz over the other fields).

## 9. Verification commands

```bash
cd /home/gumbo/midgard-hub/midgard/onchain/aiken
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");let n=0,bad=0;for(const v of b.validators){if(!/validation_trace\/cek_context_/.test(v.title)||!/\.spend$/.test(v.title))continue;n++;const s=Buffer.from(v.compiledCode,"hex").length;if(s>15000)bad++;console.log(v.title,s)}console.log("scripts",n,"over 15000:",bad)'
# expected: 21 spend validators (B0, B1, 17 verifier hops after the named splits, S), over 15000: 0
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m fraud_proofs/validation_trace/cek_split_v1      # today 44 checks, 0 errors; after: 44 + new hop vectors
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m midgard/cek_semantic_chain_v1
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m "midgard/validation_machine_v1.{cek_context_chain_agrees_with_the_aggregate, cek_context_step_route_agrees_with_the_aggregate, cek_kinds_partition_the_cek_step_space}"
cd /home/gumbo/midgard-hub/midgard
pnpm --filter @al-ft/midgard-sdk test -- tests/validation-resolver-applied-hashes.test.ts
pnpm --filter @al-ft/midgard-fault-proofs test -- tests/zz605-semantic-resolver-arity.test.ts tests/zz610-compiled-script-arity.test.ts tests/validation-dispute-submit.test.ts
pnpm --filter @al-ft/midgard-fault-proofs test -- tests/submit-init-emulator-validation-dispute.test.ts        # CEK publication test now without oversized
pnpm --filter @al-ft/midgard-fault-proofs test -- tests/submit-init-emulator-cek-context-step-v1.test.ts
```

Done criterion (primer): every `cek_context_*` body ≤ 15,000 bytes, published
without `oversized`, admitted by `assertReferenceScriptRawBodiesFitL1EnvelopeV1`,
journey green under `VAN_ROSSEM_TRANSACTION_LIMITS`, catalogue root re-pinned
once with the other plans.

## 10. Ordering and dependencies

- Lands with the other two CEK plans (shared `cek-semantic-chain-v1.ak`,
  shared aux-projection helpers, shared restructuring of
  `VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`) and with
  the single blueprint regeneration / catalogue-root re-pin of all 50 plans
  (the binder hash changes, so `cek_v1` and `dispute_v1` re-apply).
- **Hard dependency on the script-sources stage-one-redeemer plan**
  ([validation-trace-script-sources-stage-one-redeemer-semantic-v1.md](validation-trace-script-sources-stage-one-redeemer-semantic-v1.md)
  §4.2, §10), the owner of the `mode_data` decomposition of
  `redeemer_item_proof_v1.step_v1` (and inside it
  `cek_data_traverse_v1.step_v1`, 27.6 KB, whose `step_bytes` alone is
  15.7 KB) as the RF-021 chain extension. V0b consumes that chain through
  its second carrier: the `-envelope-cek-v1.ak` binder accepts
  `CekContextItemStepPendingV1 { staged, item_control, item_witness_hash,
claimed_next }`, the executors and normalizers are the same, and the
  settlement's expected hand-off hash for the `cek_context` carrier is V0b's
  return hop (`item_proof_entry_script_hash` is the CEK envelope binder's
  hash). This plan must not fork a second copy. The stage-ten-match plan's
  `V1VtSsRedeemerItemStepYield` is **not** an alternative: it covers
  `mode_descriptor` header/tail steps only and dispatches on
  ScriptSources credentials. The traversal-`None` control hash used by V0a
  and V9b is that plan's `redeemer_item_proof_v1.hash_descriptor_control_v1`.
- Shares `script_context_v1` summaries with the value-and-mint and
  resolve-inputs plans (read-only, no ordering constraint) and
  `verify_native_tx_proof_source_v1` with every native-tx family.
- Off-chain: the `submitSplitStage` driver is shared with RF-021; extend,
  do not copy.

## 11. Risks

- **Budget of single-hop stages.** Stage 12 (14.26 M with fixture) and the
  observer open hop are the closest to the 13.2 M basis; both have a named
  further split. Must be measured at the validator level before the plan is
  marked done.
- **Datum size.** `CekContextStagedV1` carries the full 25-field control
  record plus native facts (peak lists); ≈ 0.6–1 KB inline. Fine under
  16,384 but counted in every hop transaction; `compact_cbor` must never be
  copied into a datum.
- **ABI churn.** Six new datum types and one redeemer type per hop; every
  hop needs cross-language goldens or the "9f191e9a lesson" repeats. Wire of
  the prepare/binder redeemer is preserved on purpose.
- **Spec conflict.** GOAL*SPEC C47 requires exact context semantics; the
  refactor into `cek*<stage>\_successor_v1` must be proven equal to the
aggregate on every existing fixture (`cek_context_chain_agrees_with_the_aggregate`)
  before any validator is cut over.
- **Dependency risk.** The item-step interface is now fixed in the
  stage-one-redeemer plan §4.2 (second carrier); if that chain's hop count
  or commitment layout changes, V0b's `CekContextItemStepPendingV1` and the
  transaction count in §5 change with it. Land the two plans together.
- **Count.** ≈ 21 validators plus the shared item-proof hops; deployment
  info, node targets and inspection fixtures grow accordingly. The
  alternative (yields) was rejected for budget, not for count.
