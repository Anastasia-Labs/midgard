# HANDOFF — CEK semantic resolver size-fit plans

Written 2026-09-01 at session close. Docs-only work; no source, test or
blueprint file was edited; `aiken` was never run inside the checkout.

## 1. Plan file status

| File | Status |
| --- | --- |
| `validation-trace-cek-context-step-semantic-v1.md` | **Complete**, all 11 primer sections. Holds the family-wide probe table (§2.2) and the shared chain conventions (§4.1) the other two reference. |
| `validation-trace-cek-core-step-semantic-v1.md` | **Complete**, all 11 sections. |
| `validation-trace-cek-execution-selection-semantic-v1.md` | **Complete**, all 11 sections. |

Nothing is partially written; no "INCOMPLETE" markers were needed. What
remains is review, not authoring (see §5).

## 2. Probe measurements

Method: `cp -r onchain/aiken /tmp/size-probe-cek`, private functions made
`pub` in the copy only, throwaway validators under `validators/probe/`
taking an opaque `Data` redeemer and `expect`ing the argument tuple.
Decode-only baselines (`d_*`) isolate the argument decoder; a function's
cost is `probe − baseline`. Pinned fork `v1.1.23+5adf783`, `aiken build
--env testnet`, sizes = `Buffer.from(compiledCode,"hex").length`.

The two copies (`/tmp/size-probe-cek`, `/tmp/size-probe-cek2`) were
**deleted**. Still on disk under `/tmp` (WSL ubuntu):
`/tmp/probes/{d,shared,sel,core,ctx}.ak` (round 1),
`/tmp/probes2/q.ak` (round 2), `/tmp/probes3/m.ak` (round 3),
`/tmp/cekprobe-setup.sh`, `/tmp/cekprobe-run2.sh` (round-1 sed list of
functions made pub + build), `/tmp/cekprobe-run3.sh` (round 2),
`/tmp/cekprobe-run4.sh` (round 3), `/tmp/cekprobe-sizes.txt`,
`/tmp/cekprobe-sizes2.txt`, `/tmp/cekprobe-sizes3.txt`,
`/tmp/cekcheck.sh`, `/tmp/cekcheck2.sh`, `/tmp/cekcheck*.typescript`
(aiken check output with ExUnits). Note: aiken diagnostics only appear when
run under `script -q -c "..."` (a pty); a bare redirect loses them. The
round-1 build failed once because `type CekWitnessControlV1` had to be made
`pub` too.

Baseline blueprint (copy build, identical to the primer's figures):
`cek_context_step_semantic_v1.main.spend` 94,268;
`cek_core_step_semantic_v1.main.spend` 68,689;
`cek_execution_selection_semantic_v1.main.spend` 45,486;
`cek_finish_semantic_v1.main.spend` 10,897; `cek_v1.main.spend` 5,307;
RF-021 hops: envelope 8,516, traversal normalizer 11,871, outer normalizer
4,150, fold-map executor 7,519, finalize-frame executor 9,290, settlement
5,837; `script_sources_stage_one_redeemer_semantic_v1` 87,545;
`award_v1` 1,450; `user_events/cek_program_material_v1.spend` 94.

### Round 1 (`/tmp/probes/*.ak`)

| Probe | Isolates | Raw bytes |
| --- | --- | ---: |
| d_none | empty spend validator | 94 |
| d_wit | decode (ValidationMachineStateV1, ValidationOneStepWitnessV1) | 860 |
| d_aux | decode ValidationAuxiliaryWitnessV1 (full sum type) | 13,356 |
| d_control | decode NativeScriptsControlV1 | 663 |
| d_ctxctl | decode CekContextControlV1 | 550 |
| d_sel | decode (pre, witness, aux, control, Int, Int, Int) | 14,694 |
| d_ctx | d_sel + CekContextControlV1 | 15,078 |
| d_core | (pre, witness, aux, control, Int×3, ByteArray, Int, Int, ByteArray) | 14,755 |
| d_step | (MachineStateV1, MachineStateV1, CoreStepWitnessV1) | 5,330 |
| d_ev | (pre, witness, CoreStepEvidenceV1) | 6,097 |
| d_wit_aux | (pre, witness, aux) | 14,141 |
| d_route (sel.ak) | (ValidationOneStepEvidenceV1, CekMaterialRouteV1) | 14,379 |
| p_ctl_decode | cek_witness_control_v1 | 2,592 |
| p_wf | cek_witness_control_v1 + cek_witness_is_well_formed_v1 | 7,228 |
| p_finish_lib | verify_cek_finish_semantics_v1 | 8,289 |
| p_sel_lib | verify_cek_execution_selection_semantics_v1 | 28,603 |
| p_ctx_lib | verify_cek_context_step_semantics_v1 (+door) | 92,312 |
| p_core_lib | verify_cek_core_step_semantics_v1 | 66,559 |
| p_dispatch | cek-finish validator with predicate `True`, typed aux redeemer | 16,520 |
| p_dispatch_narrow | same, `auxiliary: Data` | 3,278 |
| y_skel | withdraw yield: unique-dispatch + typed context SpendRedeemer | 14,936 |
| y_skel_narrow | same with aux as Data | 1,645 |
| p_sel | verify_cek_execution_selection | 25,638 |
| p_sel_succ | cek_selection_successor_is_exact | 19,248 |
| p_route | verify_cek_route_v1 | 30,504 |
| p_core | verify_cek_core_step | 71,586 |
| p_vcs | cek_machine_v1.verify_core_step_v1 | 58,120 |
| p_compute / p_lookup / p_return / p_case_select / p_case_apply | verify_*_step | 13,749 / 6,204 / 10,503 / 6,262 / 6,161 |
| p_builtin | verify_builtin_step | 47,639 |
| p_sem_builtin | verify_semantic_builtin_control_step_v1 | 11,555 |
| p_b_direct / p_b_semantic / p_b_mapstart / p_b_semfail / p_b_bls / p_b_fail / p_b_typefail | builtin arms | 26,501 / 29,792 / 23,132 / 16,122 / 19,068 / 22,828 / 16,110 |
| p_ctx_all | verify_cek_context_step | 90,145 |
| p_ctx_succ | cek_context_successor_is_exact | 17,680 |
| p_ctxctl_codec | ctx-control decode + wf + encode (baseline d_control) | 8,400 |
| p_ctx_s0 / s1 / s2 / s3 / s4 / s5 / s6 / s8 / s9 / s10 / s11 / s12 / s13 | per-stage verify_cek_* (baseline d_ctx) | 56,038 / 26,777 / 26,777 / 22,434 / 21,761 / 28,830 / 19,288 / 23,334 / 59,622 / 36,706 / 21,987 / 23,851 / 21,842 |

### Round 2 (`/tmp/probes2/q.ak`)

| Probe | Isolates | Raw bytes |
| --- | --- | ---: |
| d2_args / d2_sem / d2_fail / d2_type | (Int, List<ValueWitnessV1>, ValueWitnessV1) / +SemanticBuiltinWitnessV1 / (Int, List<ValueWitnessV1>) / (Int, List<RuntimeValueWitnessV1>) | 446 / 1,305 / 431 / 615 |
| q_args_root | arguments_root_v1 | 7,070 |
| q_budget | direct_builtin_budget_v1 | 9,206 |
| q_direct | verify_direct_builtin_v1 | 14,261 |
| q_int / q_bytes / q_strings / q_sig / q_control / q_pairlist / q_choose / q_data / q_v3bytes / q_g1 / q_g2 | direct families | 6,839 / 7,153 / 6,770 / 6,744 / 7,312 / 7,801 / 7,048 / 7,782 / 7,856 / 7,012 / 7,017 |
| q_semantic / q_sem_data / q_sem_pair / q_sem_list / q_semfail | semantic builtin fns | 18,444 / 13,925 / 10,533 / 12,308 / 11,331 |
| q_known_fail / q_fail_direct / q_typefail | failure fns | 6,173 / 11,495 / 10,872 |
| d3_rip / d3_bytes / d3_trav | (RedeemerItemProofControlV1, Witness) / (control, ByteArray) / (DataTraverseControlV1, Option<ByteArray>, Action) | 2,720 / 1,472 / 2,146 |
| q_rip_step / q_rip_wf / q_rip_hash / q_rip_header / q_rip_tail | redeemer_item_proof_v1 step_v1 / control_is_well_formed / hash_control_v1 / header_step / tail_step | 36,884 / 8,876 / 10,361 / 8,693 / 8,844 |
| q_trav_step | cek_data_traverse_v1.step_v1 | 29,795 |
| d4_bytes2 / d4_entries | (ByteArray, ByteArray) / (ByteArray, List<CekProgramMaterialDatumV1>) | 125 / 211 |
| q_material_direct / q_material_entries / q_envelope | verify_complete_program_material_v1 / _entries_v1 / inspect+hash envelope | 13,566 / 12,829 / 2,296 |
| q_spend_info / d5_kind / q_script_info / q_purpose_summary | cardano_spend_script_info_from_descriptor_v1 / (Int, ByteArray, ByteArray) / cardano_script_info_summary_v1 / script_purpose_summary_v1 | 7,005 / 138 / 13,142 / 3,950 |
| d7_bytes4 / q_proof_source | (ByteArray×4) / verify_native_tx_proof_source_v1 | 148 / 2,117 |

### Round 3 (`/tmp/probes3/m.ak`)

| Probe | Isolates | Raw bytes |
| --- | --- | ---: |
| m_term / m_value / m_seq / m_blob / m_sidecar | program-material inspectors, sidecar parse | 3,000 / 2,111 / 1,945 / 2,078 / 2,649 |
| m_datanode / m_listnode / m_pairnode | cek_data_v1 inspect_*_preimage + hash | 3,709 / 2,397 / 2,641 |
| m_sorted / m_rootmatch / m_children / m_walk | strictly_sorted / list.all root_matches / source_program_material_children_v1 / walk_complete_program_material_v1 | 400 / 9,087 / 10,707 / 11,057 |
| d8_resolved / sc_prepend_resolved | baseline / prepend_resolved_descriptor_tx_in_info_v1 | 425 / 8,385 |
| d9_output / sc_prepend_output | baseline / prepend_output_descriptor_v1 | 393 / 4,977 |
| d10_signer / sc_prepend_signer | baseline / prepend_signer_v1 | 362 / 4,306 |
| d11_tail / sc_tail_fields | baseline / tx_info_tail_fields_summary_v1 | 350 / 4,079 |
| d12_txinfo / sc_tx_info | baseline / tx_info_from_tail_summary_v1 | 397 / 4,022 |
| d13_ctx / sc_context | baseline / script_context_summary_v1 | 206 / 2,116 |
| t_head / t_integer / t_bytes / t_largec / t_largef / t_close / t_fold | cek_data_traverse step_* (baseline d3_trav) | 12,056 / 16,457 / 17,864 / 12,814 / 10,796 / 8,542 / 13,874 |
| t_wf / t_hash | traverse control_is_well_formed / hash_control_v1 | 6,694 / 7,942 |
| d14_door / door_open | baseline / proof source + field_door.open_machine_field_item | 319 / 5,408 |

### ExUnits (aiken check in the copy; include fixture construction)

cek_split_v1 (44 checks): finish wins 4.54 M/1.93 B; selection wins
(direct material) 8.40 M/3.74 B; selection native-only 5.97 M/2.58 B;
context mint-init wins 8.60 M/3.74 B; core application wins 6.04 M/2.60 B.
Lib: `cek_execution_selection_authenticates_program_and_context_subject`
12.37 M/5.53 B; `cek_context_seed_fits_one_step` 11.95 M/5.15 B;
`cek_context_assemble_fits_one_step` 11.35 M/4.97 B;
`cek_context_tx_info_finalize_fits_one_step` 14.26 M/6.35 B;
`cek_spend_finalize_authenticates_descriptor_datum_summary` 19.09 M/8.38 B;
observer max-224 (two steps) 17.44 M/7.83 B; max mint 65–69 M (many steps);
`cek_active_core_step_replays_exact_machine_transition` 7.74 M/3.07 B;
`direct_builtin_executes_with_exact_semantics_and_budget` 5.24 M/1.92 B;
`paid_secp_failure…` 8.27 M/2.97 B;
`ten_leaf_bls_final_transition_fits_the_l1_execution_reserve` 12.30 M/8.54 B;
`ten_leaf_miller_loop_proof…` 11.44 M/8.25 B. Fixture overhead ≈ 3–4 M
(finish vector is 4.54 M for a trivial predicate). Limits: 16.5 M/10 B;
GOAL_SPEC §3.3 basis 13.2 M mem (20 % reserve) → 8 B cpu by the same rule.

## 3. Strategy per contract (as written in the plans)

- **Context step**: multi-transaction chain (pattern 3). Drivers: handshake
  3.2 + prefix 6.4 + ctx codec 7.7 KB already exceed 15 KB before any
  stage; stage 0/9 = 41/44.5 KB (redeemer-item machine 34 KB, traversal
  27.6 KB, `step_bytes` alone 15.7 KB); stage-10 spend finalize 19.09 M with
  fixture ≈ 15 M honest, so any same-tx split breaches 16.5 M. Shape:
  B0 (witness binder, title kept) → B1 (control binder, routes by stage) →
  per-stage verifier hop(s) → settle → award; ≈ 21 validators. Open
  questions: three marginal hops (B1 14.4, tx-info 14.1, mint 13.8 KB) and
  three named second-level splits (resolved-item, finalize-spend,
  finalize-cardano by purpose kind) need probes; hard dependency on the
  script-sources stage-one plan for the `redeemer_item_proof_v1.step_v1`
  decomposition (interface `CekContextItemStepPendingV1` proposed).
- **Core step**: chain (pattern 3). Drivers: builtin arms 42.3 KB share
  three infrastructures (constant decode ≈ 4–6 KB, cost tables 8.8 KB,
  data-node inspectors 8.4 KB) so family splitting yields little; ten-leaf
  BLS 12.30 M/8.54 B with fixture is at the 8 B cpu reserve alone, so a
  dispatcher + yields in one tx breaches it; the chain puts left/right
  Miller-loop sides in separate hops. Shape: B (binder, title kept) → 1–3
  arm hops → settle; ≈ 27 validators. Open: four marginal hops (compute
  13.8, direct-roots 14.1, direct-budget 15.1, semantic-budget 15.1 KB);
  a cost-table ByteArray-lookup prune (≈ −4 KB/budget hop) is suggested as
  a follow-up probe.
- **Execution selection**: withdraw-zero yields (pattern 2), single tx:
  dispatcher (title kept, +`reference_script_auth_policy_id`) + 4 yields
  (authenticate 11.1, successor 12.1, material-program 14.4,
  material-data 14.0 KB) with 4 new roles. Driver: the material walk
  (10.85 KB, eight inspectors 10.5 KB irreducible) fits only a 1.6 KB
  yield skeleton, not a 4.2 KB chain-hop skeleton; honest budget 5–9 M
  leaves room for ≈ +2–3 M of yield re-parses. Requires splitting
  `walk_complete_program_material_v1` into program (kinds 0–4) and data
  (kinds 5–7) walks with a composition proof; ABI: 3 appended redeemer
  fields, `DirectCekMaterial` carries `entries` instead of sidecar cbor,
  single-publication datum v2 with `entries`. Open: material yields are
  marginal (sourcing cost ≈ 2 KB estimated, not probed); aggregate worst
  case unmeasured; chain fallback documented.

## 4. Files read and key facts

- `onchain/aiken/validators/fraud-proofs/validation-trace/cek-{context-step,core-step,execution-selection,finish}-semantic-v1.ak`, `cek-v1.ak`: thin wrappers; each `main.spend` = `ct.Cancel → common.cancel` | `ct.Continue → validation_semantic_v1.continue_winning(Cek, award, ct_policy, datum, in, out, transition, aux_data, <predicate>, own_out_ref, tx)`. Core flattens `step: CoreStepEvidenceV1` and rebuilds `CekCoreStepWitness { step }` for the hash; selection also runs `verify_cek_route_v1(evidence, material_route, tx.reference_inputs, cek_program_material_script_hash)`; `cek_v1` = `prepare_selected(Cek, 4 hashes, 4, …)` (slots: 0 finish, 1 selection, 2 context, 3 core).
- `lib/midgard/validation-semantic-v1.ak`: `continue_winning` checks prepared_resolution_is_well_formed, phase, `hash_one_step_evidence(transition_data, auxiliary) == evidence_hash`, the predicate, `output_script_hash == award`, `output_state == winning_resolution()`.
- `lib/midgard/validation-machine-v1.ak` (19,142 lines): CEK section ≈ 13,600–17,200. `CekWitnessControlV1` (private type) via `cek_witness_control_v1` (16,883); `cek_witness_is_well_formed_v1` (16,910: proof source, commitment, context hash, control wf, cursor bounds, exact re-encode); discriminators `cek_control_is_{selecting,finish,execution_selection,context_step,core_step}_v1`; `verify_cek` (17,092); entry points `verify_cek_{finish,execution_selection,context_step,core_step}_semantics_v1` (17,117–17,159); `verify_cek_core_step` (15,006) = framing + `cek_machine_v1.verify_core_step_v1` + successor branches (budget exceeded/halt error → `rejected_successor_is_exact`, halt success → ValueAndMint hand-off or next cursor, else continue with `cek_machine_state_hash(post)`); `verify_cek_execution_selection` (15,276) and `cek_selection_successor_is_exact` (15,148); `verify_cek_context_step` (16,701) dispatches stage 0..13 (7 unused) to `verify_cek_redeemer_selection`, `_reference_`, `_spend_`, `_output_`, `_signer_`, `_observer_` (takes door), `_mint_context_init`, `_mint_context_item`, `_redeemer_data_step`, `_context_finalize`, `_context_assemble`, `_tx_info_finalize`, `_context_seed`; every stage ends in `cek_context_successor_is_exact` (re-encode witness + control, `hash_work_witness`) except stage 13 (`cek_context_execution_successor_is_exact`). `CekContextControlV1` 25 fields, codec `encode_cek_context_control_v1` / `cek_context_control_from_cbor` / `cek_context_control_is_well_formed` (14,089). `ValidationAuxiliaryWitnessV1` constructor indices: NoAuxiliaryWitness 0, TransactionFieldChunkWitness 1, … RedeemerScanBeginWitness 10, NativeExecutionScanWitness 11, CekCoreStepWitness 12, CekResolvedContextItemWitness 13, CekOutputContextItemWitness 14, CekSignerContextItemWitness 15, CekMintContextItemWitness 16, CekRedeemerContextSelectWitness 17, RedeemerItemStepWitness 18, CekContextFinalizeWitness 19, CekContextFinalizeSpendWitness 20, CekContextAssembleWitness 21, CekTxInfoFinalizeWitness 22, CekContextSeedWitness 23 (matches `VALIDATION_AUXILIARY_SHAPES_V1` in submit.ts). `redeemer_item_proof_v1.step_v1` has six call sites (8823, 11409, 11960, 12928, 15520, 16354) — shared with ScriptSources.
- `lib/midgard/cek-machine-v1.ak`: `verify_core_step_v1` dispatches on `pre.mode` (compute, lookup, return, case_select, case_apply, builtin, semantic_builtin); `verify_builtin_step` arms ExecuteBuiltinDirect/Semantic/StartBuiltinMapConversion/SemanticFailure/BlsFinal/Failure/TypeFailure; `CoreStepWitnessV1` has 40 constructors. `cek-builtin-v1.ak`: `verify_direct_builtin_v1` → per-family fns; `direct_builtin_budget_v1` → `cek_cost_v1.builtin_budget_v1` (87-tag cpu/mem tables); `max_direct_bls_miller_loop_leaves = 10`. `cek-proof-v1.ak`: `walk_complete_program_material_v1` over 8 material kinds (term/value/sequence/blob chunk+branch/data node/list/pair), `max_program_envelope_cbor_bytes = 50`.
- `lib/midgard/validation-resolver-v1.ak`: `CekMaterialRouteV1 { NoCekMaterial | DirectCekMaterial {envelope_cbor, sidecar_cbor} | SinglePublicationCekMaterial {envelope_cbor, reference_input_index} | MinimumMultiOutputCekMaterial {envelope_cbor, reference_input_indices} | IncrementalCekMaterial → False (fail closed, lease #520) }`; `CekSinglePublicationDatumV1 { version, program_envelope_hash, sidecar_cbor }`; `verify_cek_route_v1` slices the envelope from the aux first chunk via `versioned_script_header_v1`; `select_semantic_resolver` pins list length.
- `cek-split-v1.test.ak` (1,743 lines, 44 tests): validator-level vectors for all five CEK validators on the smallest honest step of each kind; wire-layout goldens (`*_wire_layout_is_pinned`), `every_kind_commits_to_the_prepared_evidence_hash`, prepare routing/cardinality negatives, 12 cross-kind refusal tests, #629 native-only selection pair, shared-guard negatives (foreign transaction commitment, cursor past execution count with witness-tail byte layout pinned), `every_fixture_is_an_honest_machine_step`. Lib partition proof `cek_kinds_partition_the_cek_step_space` in validation-machine-v1.test.ak. Fixtures: `empty_v1_transaction`, `empty_native_control`, `cek_state`, one-node `error` program with direct material.
- `demo/midgard-sdk/src/fraud-proof/contracts.ts`: titles at :315 (cek prepare) and :454–461 (four semantics); semantic parameters applied **by declared name** through `semanticResolverParameterValues` (:3883: award_script_hash, computation_thread_policy_id, field_preimage_certificate_policy_id, source_binder_script_hash, proof_item_script_hash, cek_program_material_script_hash); `referenceScriptAuthPolicyId` available (:1152, :3448); RF-021 stage record `scriptSourcesStageOneRedeemerStages` (:283) is the template for per-hop records; min-ADA yields at :249 (`fraud_proofs/min_ada/step_02_yields.tx.withdraw`).
- `demo/midgard-sdk/src/reference-scripts.ts`: `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` (:71 `V1ValidationTraceCekResolver0`, retired direct resolver; min-ADA yields :215), mirrored in `demo/midgard-core/src/deployment-manifest-identity-v1.ts` (:536 roster, :583 CEK row); `assertReferenceScriptRawBodiesFitL1EnvelopeV1` (:291) rejects ≥ 16,384.
- `demo/midgard-fault-proofs/src/validation-dispute/submit.ts`: `VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1` (:815, indices 1–3, hash-checked, no role); `requireValidationCekSemanticReferenceScriptUtxo` (:871); `semanticActionFieldsV1` (:3698) builds resolver-11 redeemers (finish: base; selection: +aux +materialRoute; context: +aux; core: +auxiliary.fields); `submitValidationDisputeSemanticResolution` (:5956) resolves the CEK reference UTxO (:6069) and, for ScriptSources split stage one (:6389), drives multi-hop with `submitSplitStage` — the driver to extend for CEK chains. `validateCekSubmissionEvidenceV1` (:1234) admits route material only for resolver 11 / semantic 1.
- Emulator: `tests/submit-init-emulator-validation-dispute.test.ts:156` publishes the three CEK bodies with `oversized: true` under `maxTxSize: 262_144` and asserts `l1ByteMargin < 0`; `tests/submit-init-emulator-cek-value-and-mint-v1.test.ts` journeys only the finish kind (key-witness spend, `execution_count == 0`); `tests/support/emulator/dispute-scenario.ts:352–430` publishes the selected semantic under `withRealL1MaxTxSize` unless `semanticIsOversized` (then functional params + `oversized: true`); `reference-scripts.ts:303` `publishPlainReferenceScriptUtxo({oversized})` asserts positive margin unless oversized; `dispute-staging.ts:96` `withRealL1MaxTxSize`; `setup-tx.ts:172/357` registers yield reward accounts (`register.Stake(scriptRewardAddress(...))`); `protocol-parameters.ts` pins 16,384 / 16.5 M / 10 B.
- Yield precedent: `validators/fraud-proofs/min-ada/step-02.ak` + `step-02-yields.ak`, `lib/midgard/fraud-proofs/min-ada/yield.ak` (`unique_dispatch`, roles `V1FpMinAdaS02TxYield`/`UtxoYield`), `lib/midgard/state-queue-yield.ak` (`require_authenticated_zero_yield(reference_inputs, withdrawals, redeemers, auth_policy, role, index)`); node rows in `demo/midgard-node/src/commands/contract-deployment-info.ts:833` and `transactions/reference-scripts.ts:1566`.
- Tests to keep green: `demo/midgard-fault-proofs/tests/zz605-semantic-resolver-arity.test.ts` (no hashes pinned; discovers declared params), `zz610-compiled-script-arity.test.ts`, `demo/midgard-sdk/tests/validation-resolver-applied-hashes.test.ts` (asserts selection and context validators declare 3 params — both change).
- GOAL_SPEC: §3.3 (byte fit; 13.2 M mem basis; 302,400,000 ms half maturity), §8.3 rows C47 (context semantics), C48 (CEK execution), C52 (5,000-tx cap), C53 (resolver proof-fit sweep).

## 5. Next steps, in order

1. Review the three plans against the primer's 11-section checklist and
   the sibling script-sources stage-one plan
   (`validation-trace-script-sources-stage-one-redeemer-semantic-v1.md`);
   reconcile the item-proof decomposition interface the context plan
   assumes (`CekContextItemStepPendingV1`) with what that plan defines.
2. Update `README.md` in this directory (index) if it does not yet list the
   three CEK plans and their chosen patterns.
3. Optional probes the plans name (recipe: same copy procedure, probe files
   still under `/tmp/probes*`): per-purpose-kind split of
   `cardano_script_info_summary_v1`; membership vs summary halves of
   `prepend_resolved_descriptor_tx_in_info_v1`; uniform material sourcing
   (`material_entries_v1`) cost; cost-table ByteArray-lookup prune of
   `cek_cost_v1`.
4. At implementation time, measure the marginal hops/yields listed in each
   plan's §5 before cutting over, and the ExUnits worst cases named in §7.
