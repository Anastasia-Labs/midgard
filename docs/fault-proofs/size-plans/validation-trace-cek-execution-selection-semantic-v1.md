# Size-fit plan: `cek_execution_selection_semantic_v1` (validation-trace CEK execution selection)

Companion to [00-primer.md](00-primer.md). Family-wide probe rows, the
aux-as-`Data` pruning and the shared CEK prefix numbers are in
[validation-trace-cek-context-step-semantic-v1.md](validation-trace-cek-context-step-semantic-v1.md)
§2.2; this plan is the one CEK member that chooses the withdraw-zero yield
pattern, and §3 says why.

## 1. Identity

| Field                   | Value                                                                                                                                                                                                                                                             |
| ----------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title         | `fraud_proofs/validation_trace/cek_execution_selection_semantic_v1.main.spend` (and `.else`)                                                                                                                                                                      |
| File                    | `onchain/aiken/validators/fraud-proofs/validation-trace/cek-execution-selection-semantic-v1.ak` (94 lines)                                                                                                                                                        |
| Raw size                | 45,486 bytes (measured 2026-09-01, pinned fork, fresh copy build)                                                                                                                                                                                                 |
| Applied parameters      | `award_script_hash`, `computation_thread_policy_id`, `cek_program_material_script_hash` (3)                                                                                                                                                                       |
| Phase / index           | phase `Cek`, resolver index 11 (`cek_v1.main`), semantic resolver index 1                                                                                                                                                                                         |
| Library entry points    | `validation_machine_v1.verify_cek_execution_selection_semantics_v1(pre, transition, auxiliary)` and `validation_resolver_v1.verify_cek_route_v1(evidence, material_route, tx.reference_inputs, cek_program_material_script_hash)`, both inside `continue_winning` |
| Redeemer today          | `VerifyExecutionSelection { input_index, output_index, transition, auxiliary: ValidationAuxiliaryWitnessV1, material_route: CekMaterialRouteV1 }`                                                                                                                 |
| Role / deployment entry | none / `validationTraceDisputeCekExecutionSelectionSemantic` (`VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1[1]`)                                                                                                                                |
| SDK title key           | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.cekExecutionSelection`                                                                                                                                                                                     |
| Emulator today          | published `oversized: true` only (`submit-init-emulator-validation-dispute.test.ts:156`); never journeyed                                                                                                                                                         |

The step selects execution `execution_cursor` of the native transaction:
it authenticates the purpose/source/execution leaves against the three
frontiers, parses the versioned script header from the first source chunk,
and either hands a native execution straight on (`language_tag == 0`) or
opens a Plutus/Midgard program envelope (`language_tag` 3 / 128), in which
case the complete program material must be verified through one of the
routes in `CekMaterialRouteV1`.

## 2. Why it is this size

Same probe copy and method as the sibling plan. Baselines: `d_sel` 14,694
(`(pre, witness, aux, control, Int×3)`), `d_wit_aux` 14,141, `d_route`
14,379 (`(ValidationOneStepEvidenceV1, CekMaterialRouteV1)`), `d4_bytes2`
125, `d4_entries` 211.

| Probe                                      | Function                                                                                            |                           Raw |                          Cost |
| ------------------------------------------ | --------------------------------------------------------------------------------------------------- | ----------------------------: | ----------------------------: |
| `d_aux`                                    | `ValidationAuxiliaryWitnessV1` decoder (paid by the typed redeemer field)                           |                        13,356 |                    **13,262** |
| `p_wf`                                     | `cek_witness_control_v1` + `cek_witness_is_well_formed_v1`                                          |                         7,228 |                         6,368 |
| `p_sel_lib`                                | `verify_cek_execution_selection_semantics_v1`                                                       |                        28,603 |                        14,462 |
| `p_sel`                                    | `verify_cek_execution_selection`                                                                    |                        25,638 |                    **10,944** |
| `p_sel_succ`                               | `cek_selection_successor_is_exact` (witness + empty context-control encoding, both hand-off shapes) |                        19,248 |                         4,554 |
| `p_route`                                  | `verify_cek_route_v1` (envelope slice from the aux + all routes)                                    |                        30,504 |                    **16,125** |
| `q_material_direct`                        | `verify_complete_program_material_v1` (sidecar + walk)                                              |                        13,566 |                        13,441 |
| `q_material_entries`                       | `verify_complete_program_material_entries_v1` (walk only)                                           |                        12,829 |                        12,618 |
| `q_envelope`                               | `inspect_program_envelope_v1` + `hash_program_envelope_v1`                                          |                         2,296 |                         2,171 |
| `m_walk`                                   | `walk_complete_program_material_v1`                                                                 |                        11,057 |                        10,846 |
| `m_children`                               | `source_program_material_children_v1` (all eight material kinds)                                    |                        10,707 |                        10,496 |
| `m_rootmatch`                              | `list.all(entries, program_material_root_matches_v1)`                                               |                         9,087 |                         8,876 |
| `m_term` / `m_value` / `m_seq` / `m_blob`  | program-material inspectors (kinds 0–4)                                                             | 3,000 / 2,111 / 1,945 / 2,078 | 2,875 / 1,986 / 1,820 / 1,953 |
| `m_datanode` / `m_listnode` / `m_pairnode` | data-material inspectors + hashes (kinds 5–7)                                                       |         3,709 / 2,397 / 2,641 |         3,584 / 2,272 / 2,516 |
| `m_sidecar`                                | `inspect_complete_program_material_sidecar_v1`                                                      |                         2,649 |                         2,524 |
| `m_sorted`                                 | `strictly_sorted_material_roots_v1`                                                                 |                           400 |                           189 |
| `p_dispatch_narrow` / `y_skel_narrow`      | thread handshake / yield skeleton with aux as `Data`                                                |                 3,278 / 1,645 |                 3,184 / 1,551 |

Reading: 45.5 KB = 3.2 (handshake) + 13.3 (aux decoder) + 6.4 (prefix) +
10.9 (selection: leaves, three memberships, chunk proof, header, envelope,
successor 4.6) + 16.1 (route: envelope slice 2.2, route dispatch and
reference-input datums ≈ 1.5, material walk 10.8–12.6) − overlaps. The
material walk is a single traversal over eight material kinds whose
inspectors are 10.5 KB together; it cannot be pruned (§3) and is the
piece that decides the pattern.

ExUnits (aiken check, includes fixture): validator-level
`execution_selection_validator_wins_with_direct_material` 8.40 M / 3.74 B;
`execution_selection_validator_wins_a_native_only_selection` 5.97 M /
2.58 B; lib `cek_execution_selection_authenticates_program_and_context_subject`
12.37 M / 5.53 B; the finish vector (4.54 M) bounds fixture overhead at
≈ 3–4 M, so the honest Plutus selection is ≈ 5–9 M / ≈ 2–4 B.

## 3. Options considered

| Option                       | Verdict                                      | Reason                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| ---------------------------- | -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Prune                        | Keep, insufficient alone                     | Aux as `Data` (−13.3 KB → ≈ 32 KB). Envelope facts (`term_root`, `node_count`, `material_byte_length`) are prover-claimable and checked once, removing `inspect_program_envelope_v1` from the material scripts. The walk itself has no dead code: every material kind is reachable from any program. Replacing the walk by a per-entry closure check was rejected because it would accept envelopes with orphan entries (node_count over-count), a semantic change against C47/C48. |
| Withdraw-zero yields, one tx | **Chosen**                                   | After pruning, the body decomposes into four independent predicates over the same evidence that each fit a 1.6 KB yield skeleton; the material walk fits _only_ in a yield (10.8 + 2.0 sourcing + 1.6 = 14.4 KB) — under the 4.2 KB chain-hop skeleton it does not (≥ 16.8 KB). Honest cost 5–9 M in one script plus ≈ 2–3 M for four yield re-parses stays under the 13.2 M basis.                                                                                                 |
| Chain (pattern 3)            | Rejected for this contract, kept as fallback | Hop skeleton (`continue` + binding + evidence re-bind, 4.2 KB) pushes the walk hop to ≈ 17 KB; a chain would still need the two material _yields_. Budget isolation is not needed here (see ExUnits). Documented fallback in §11 if the emulator worst case exceeds the basis.                                                                                                                                                                                                      |
| Redesign                     | Rejected (scope)                             | Moving material verification into the publication policy (mint a "verified material" token per envelope hash) would shrink the resolver to ≈ 20 KB and cut per-dispute cost, but changes the permissionless publication trust model — the accumulator territory the `IncrementalCekMaterial` comment assigns to its own lease (#520).                                                                                                                                               |

## 4. Chosen design

Dispatcher (existing `main.spend`, keeps title and `cek_v1` slot 1) plus
four rewarding validators in `cek-execution-selection-yields-v1.ak`, all
run in the single semantic-resolution transaction. Handshake exactly as
`min_ada/step-02` (`state_queue_yield.require_authenticated_zero_yield` on the
dispatcher side, `unique_dispatch` on the yield side).

### 4.1 Validators

| #          | Validator (title)                                         | Responsibility                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | Parameters                                                                                                                                                                                     | Role (Aiken const in `midgard/fraud_proofs/validation_trace/cek_selection_yield.ak` / roster label) |                                                  Projected |
| ---------- | --------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------- | ---------------------------------------------------------: |
| D          | `cek_execution_selection_semantic_v1.main`                | `continue_winning(Cek, …, semantic_transition_is_valid)` with `semantic_transition_is_valid = and { wf, discriminators, yields }`: `cek_witness_control_v1` + `cek_witness_is_well_formed_v1`; `cek_control_is_execution_selection_v1`; both limits `== 0`; `program_envelope_hash == ""`; `expect_aux_arm_v1(aux, 11)` and read `language_tag` (field 1) and `redeemer_leaf` (field 13) by index; `require_authenticated_zero_yield` for `authenticate_role` and `successor_role`; if `language_tag == 0`: `material_route == NoCekMaterial`, `redeemer_leaf == ""`, no material yields; else `language_tag ∈ {3,128}`, route ∈ {Direct, SinglePublication, MinimumMultiOutput} and `require_authenticated_zero_yield` for `material_program_role` and `material_data_role`                                                                                                                                                            | `award_script_hash`, `computation_thread_policy_id`, `cek_program_material_script_hash` (kept, for the yields' parameter parity check only — see §11), `reference_script_auth_policy_id` (new) | –                                                                                                   |                  3.2 + 6.4 + 0.5 + 0.5 + 2.0 ≈ **12.6 KB** |
| Y-auth     | `cek_execution_selection_yields_v1.authenticate.withdraw` | `unique_dispatch(dispatcher_script_hash, inputs, redeemers)`; decode `aux` arm 11 into the sixteen `NativeExecutionScanWitness` fields; `cek_witness_control_v1(transition)`; `execution_index == execution_cursor < execution_count == purpose_count`; `script_total_length` bounds; `first_chunk_proof` identity (`first_source_chunk_identity_matches`), `bounded_item_v1.verify_chunk`; `versioned_script_header_v1` and `header.language_tag == language_tag`; `purpose_leaf_hash`, `source_descriptor_leaf_hash`, `execution_leaf_hash`; the three `validation_merkle_v1.verify_membership` calls; for tags 3/128: `redeemer_leaf` length 32, `header.payload_length ≤ max_program_envelope_cbor_bytes` and offset bounds, and `route_envelope == slice(header.payload_offset, header.payload_length, chunk)` where `route_envelope` is `material_route`'s `envelope_cbor` (Y-succ and the material yields rely on this equality) | `dispatcher_script_hash`, `award_script_hash`                                                                                                                                                  | `V1VtCekSelAuthYield` / "V1 validation-trace CEK selection authenticate yield"                      |                        1.6 + 1.5 + 1.7 + 6.3 ≈ **11.1 KB** |
| Y-succ     | `…yields_v1.successor.withdraw`                           | `unique_dispatch`; aux arm 11 fields; `cek_witness_control_v1`; for tags 3/128: `inspect_program_envelope_v1(route_envelope)` = `Some(envelope)`, `hash_program_envelope_v1(1,1,0,…)` and the redeemer's claimed `envelope_facts { term_root, node_count, material_byte_length }` equal the envelope's; `cek_selection_successor_is_exact(pre, witness, control, cursor, cpu, mem, language_tag, purpose_kind, purpose_index, script_hash, subject, redeemer_leaf, term_root, envelope_hash)`; for tag 0: same call with empty roots; re-derives the continuation: output at `output_index` is at `award_script_hash` with inline datum `winning_resolution()`                                                                                                                                                                                                                                                                          | `dispatcher_script_hash`, `award_script_hash`                                                                                                                                                  | `V1VtCekSelSuccYield` / "… selection successor yield"                                               |            1.6 + 1.5 + 1.7 + 2.2 + 4.6 + 0.5 ≈ **12.1 KB** |
| Y-mat-prog | `…yields_v1.material_program.withdraw`                    | `unique_dispatch`; `material_entries_v1(route, reference_inputs, cek_program_material_script_hash)` (uniform sourcing: Direct → `route.entries`; SinglePublication → datum v2 `entries`; MinimumMultiOutput → `material_entries_from_references_v1`, indices unique and non-negative); `strictly_sorted_material_roots_v1`; **program walk**: `walk_program_material_v1([task(0, term_root, -1)], entries)` over kinds 0–4 only, treating kind-5 children of value material as leaves collected into `data_roots`; checks `program_node_count`, `program_byte_length` and the sorted `data_roots` list claimed in the redeemer, and `program_seen == count of entries with kind ≤ 4`                                                                                                                                                                                                                                                    | `dispatcher_script_hash`, `cek_program_material_script_hash`                                                                                                                                   | `V1VtCekSelMatProgYield` / "… selection material program yield"                                     | 1.6 + 0.5 + 2.0 + 0.2 + 8.6 + 1.5 ≈ **14.4 KB** (marginal) |
| Y-mat-data | `…yields_v1.material_data.withdraw`                       | `unique_dispatch`; same `material_entries_v1`; **data walk**: `walk_data_material_v1(data_roots as tasks, entries)` over kinds 5–7 (`cek_data_v1` inspectors), checks `data_node_count`, `data_byte_length`, `data_seen == count of entries with kind ≥ 5`; and the totals: `program_node_count + data_node_count == node_count`, `program_byte_length + data_byte_length == material_byte_length`, `program_seen + data_seen == list.length(entries)`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | `dispatcher_script_hash`, `cek_program_material_script_hash`                                                                                                                                   | `V1VtCekSelMatDataYield` / "… selection material data yield"                                        |       1.6 + 0.5 + 2.0 + 8.4 + 1.5 ≈ **14.0 KB** (marginal) |

Library changes (`cek-proof-v1.ak`): split `walk_complete_program_material_v1`
into `walk_program_material_v1` (kinds 0–4, emits data roots) and
`walk_data_material_v1` (kinds 5–7); keep
`verify_complete_program_material_entries_v1` as their composition so the
existing route tests and `verify_cek_material_route_for_selected_envelope_v1`
keep passing; property test that composition equals the old walk on every
material fixture. Domain separation of node hashes (term/value/sequence/blob
vs data node/list/pair) guarantees a root is of exactly one kind, so the
partition of `seen` is exact and orphans in either partition are still
rejected (`seen` counts must equal the per-kind entry counts).

### 4.2 ABI deltas

- Dispatcher redeemer: `VerifyExecutionSelection { input_index, output_index, transition, auxiliary: Data, material_route: CekMaterialRouteV1, yield_ref_input_indices: List<Int>, envelope_facts: CekEnvelopeFactsV1, material_facts: CekMaterialFactsV1 }` — three appended fields (wire change; `execution_selection_wire_layout_is_pinned` golden updated). `yield_ref_input_indices` has length 2 (tag 0) or 4; `CekEnvelopeFactsV1 { term_root, node_count, material_byte_length }` is all-empty for tag 0; `CekMaterialFactsV1 { program_node_count, program_byte_length, data_node_count, data_byte_length, data_roots: List<ByteArray> }`.
- `CekMaterialRouteV1.DirectCekMaterial` becomes `{ envelope_cbor, entries: List<CekProgramMaterialDatumV1> }` (the sidecar CBOR is an off-chain wire format only; `validationCekMaterialRouteDataV1` encodes the entries `deriveCekProgramMaterialPublicationsV1` already produces).
- `CekSinglePublicationDatumV1` v2: `{ version: 2, program_envelope_hash, entries: List<CekProgramMaterialDatumV1> }` (`deriveCekSinglePublicationV1`); v1 publications with `sidecar_cbor` are refused by `material_entries_v1` (fail closed, republish). `user_events/cek_program_material_v1.spend` is unchanged (it does not read the datum).
- Yield redeemers: `Data` (ignored), like `min_ada/step_02_yields`.

### 4.3 Handshake and security argument

- **Dispatch uniqueness.** Each yield's `unique_dispatch` requires exactly
  one input at `dispatcher_script_hash` and exactly one `Spend` redeemer
  for it; one zero-withdrawal cannot discharge two threads.
- **Role authentication.** D indexes each yield's reference input, requires
  exactly one token under `reference_script_auth_policy_id` with the arm's
  role name, and an exact zero withdrawal from the referenced script hash
  with a unique withdraw redeemer (`require_authenticated_zero_yield`).
  Script substitution fails on the withdrawal credential, role substitution
  on the asset name.
- **Cross-arm substitution.** Four distinct role constants; each yield is a
  different validator whose predicate is fixed, so presenting Y-succ under
  `authenticate_role` fails at D's asset-name check.
- **Evidence binding.** D's `continue_winning` pins
  `hash_one_step_evidence(transition, auxiliary) == evidence_hash` and the
  phase; the yields read the same datum and redeemer from the transaction,
  so every predicate is over the committed evidence. Facts the prover claims
  in the redeemer (`envelope_facts`, `material_facts`, `data_roots`) are each
  checked by exactly one yield against evidence-derived values (Y-succ:
  envelope; Y-mat-prog: program totals and `data_roots`; Y-mat-data: data
  totals and grand totals), so no claim is trusted anywhere.
- **Output-state re-derivation.** Y-succ re-derives the award continuation
  (`award_script_hash`, `winning_resolution()`) in addition to D.
- **Omitted yield.** D fails on the missing withdrawal; omitting a material
  yield for a Plutus selection is impossible because D requires four roles
  when `language_tag != 0` and Y-auth pins `header.language_tag ==
language_tag`, so the prover cannot relabel a Plutus execution as native.
- **Parameter flow.** Yields carry `dispatcher_script_hash`; D carries no
  yield hash (roles authenticate); acyclic, as in the min-ADA precedent.

## 5. Size and budget projection

- Sizes (§4.1): D 12.6, Y-auth 11.1, Y-succ 12.1, Y-mat-prog 14.4,
  Y-mat-data 14.0 KB; all ≤ 15,000 with two marginal. Method: probe deltas
  summed; `y_skel_narrow` 1.6 KB is the yield skeleton; sourcing 2.0 KB is
  an estimate for the uniform `material_entries_v1` (Direct list decode 0.2
  - publication datum decode ≈ 0.8 + `material_entries_from_references_v1`
    ≈ 1.5, sharing the reference-input lookup) — probe it first at
    implementation; fallback if a material yield exceeds 15,000: split
    sourcing per route into two yield variants selected by D from the route
    constructor (one extra role).
- Referenced bytes per resolution transaction: D + 2 yields (tag 0) ≈ 36 KB
  → tier 2, ≈ 0.57 ADA; D + 4 yields (Plutus) ≈ 64 KB → tier 3, ≈ 1.14 ADA
  (384,000 + 460,800 + 12,800 × 21.6 = 1,121,280 lovelace) versus ≈ 0.74 ADA
  for today's 45,486-byte body (384,000 + 19,886 × 18). Material
  publications are reference _datums_, not scripts, and do not count; total
  stays far under `maxRefScriptSizePerTx` (200 KiB).
- ExUnits (one transaction): honest monolith ≈ 5–9 M / 2–4 B (validator
  vectors 8.40 M / 3.74 B and 5.97 M / 2.58 B with fixture) plus four
  yields each re-parsing the datum and redeemer (≈ 0.5–0.8 M each; the aux
  carries one chunk ≤ 4,095 bytes and the route carries the entries) ≈ +2–3 M
  / +1 B → projected ≤ 12 M / 5 B, under the 13.2 M / 8 B basis. Worst
  case to measure: the maximum direct-material program that fits the
  16,384-byte redeemer, and a 50-byte envelope with `MinimumMultiOutputCekMaterial`
  over several publications.
- Transactions per dispute: unchanged (1 semantic transaction); C52 and
  §3.3 maturity unaffected. Deployment adds four stake registrations.

## 6. Off-chain work

Exists: SDK title, deployment entry, `requireValidationCekSemanticReferenceScriptUtxo`
route, `semanticActionFieldsV1` semantic-1 branch with `materialRoute`,
`validateCekSubmissionEvidenceV1`, `validationCekMaterialRouteDataV1`,
`deriveCekProgramMaterialPublicationsV1`, `deriveCekSinglePublicationV1`.
Missing: roles, yields, stake registration, journey.

1. `contracts.ts`: `cekExecutionSelectionYields: { authenticate, successor, materialProgram, materialData }` (withdrawal validators, built like `minAda.yields`); D gains `reference_script_auth_policy_id` in `semanticResolverParameterValues` (value already available as `referenceScriptAuthPolicyId`, `contracts.ts:3448`); yields' `dispatcher_script_hash` = D's applied hash, `award_script_hash`, `cek_program_material_script_hash`. `validation-resolver-applied-hashes.test.ts` (`selectionValidator.parameters).toHaveLength(3)`) becomes 4.
2. Roles: four entries in `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES`
   (`demo/midgard-sdk/src/reference-scripts.ts`) and the mirror in
   `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES`
   (`demo/midgard-core/src/deployment-manifest-identity.ts`, moving the
   pinned manifest id with an audit note as #477 did); Aiken constants in the
   new `cek_selection_yield.ak`; node `contract-deployment-info.ts`
   `withdrawalDescriptor` rows and `transactions/reference-scripts.ts`
   `manifestReferenceScriptTarget` rows (pattern: `fraudProofMinAdaStep02TxWithdraw`);
   inspection fixtures that enumerate the roster; stake registration of the
   four reward accounts wherever the min-ADA yields are registered
   (`tests/support/emulator/setup-tx.ts:357` in the harness; the node
   deployment flow equivalently).
3. Deployment entries: yields published as authenticated reference scripts
   with their role token (like the state-queue and min-ADA yields); D stays
   under `validationTraceDisputeCekExecutionSelectionSemantic`, now inside the
   envelope.
4. Submit (`submitValidationDisputeSemanticResolution`): for resolver 11 /
   semantic 1 add `.readFrom([yieldUtxos…])` and one `.withdraw(scriptRewardAddress(network, yield.withdrawalScript), 0n, Data.void())` per required yield (2 or 4), compute `yield_ref_input_indices` from the resolved reference-input order (the `onLayout` hook already resolves indices), compute `envelope_facts`/`material_facts`/`data_roots` in a new `deriveCekSelectionMaterialFactsV1` from `CekRouteMaterialV1` (the TS walk in `demo/midgard-sdk` material derivation already visits every node), and extend `semanticActionFieldsV1` for the three new fields; `validateCekSubmissionEvidenceV1` unchanged.
5. Codecs: `ValidationCekMaterialRouteV1Schema` (Direct → entries), the
   single-publication datum v2 encoder/decoder, the two facts schemas; the
   `midgard-validation` machine is unchanged.
6. Funding: the resolution transaction now carries up to four zero
   withdrawals and four reference inputs; min-Ada and fee rows in the
   challenger runbook.

## 7. Emulator scenario tests

Exists: publication with `oversized: true`; no journey. Add:

1. Publication fit for D and the four yields without `oversized` under
   `withRealL1MaxTxSize` (shared change, sibling plan §7.1), plus yield
   registration in the harness setup.
2. `submit-init-emulator-cek-execution-selection-v1.test.ts`: fixture
   `cekStep: { kind: "execution-selection" }` over an L2 transaction whose
   only execution is the one-node `error` PlutusV3 program with a Direct
   route (the `cek-split-v1.test.ak` material), and a second case with a
   native-only execution (`language_bitmap == 0`, the #629 shape, two
   yields); positive lifecycle through award and removal; valid-block
   negative; cancel/resume at the prepare-selected boundary (the dispatcher
   has no intermediate state, so cancel is the existing `ct.Cancel` on D);
   negatives: missing material yield for a Plutus selection (tx fails at D),
   swapped roles (fails at D), publication route with a v1 sidecar datum
   (fails at Y-mat-prog); maximum shape: the largest direct material that
   fits 16,384 bytes and a `MinimumMultiOutputCekMaterial` over ≥ 3
   publications, asserting aggregate mem/cpu ≤ 13.2 M / 8 B via the
   proof-fit measurement.

## 8. Aiken tests

- `cek-split-v1.test.ak`: update `execution_selection_wire_layout_is_pinned`
  for the three appended fields; keep
  `execution_selection_validator_wins_with_direct_material`,
  `…_wins_a_native_only_selection`, `…_refuses_a_missing_material_route`,
  `…_refuses_a_truncated_sidecar` (now "refuses a truncated entries list")
  driving D **and** the yields in one transaction fixture (extend
  `native_binding_fixture_v1` with withdrawals and role reference inputs as
  `min-ada/step-02` tests do); new negatives: yield omitted, role swapped,
  withdrawal non-zero, two dispatcher inputs, claimed `envelope_facts`
  tampered (Y-succ refuses), `data_roots` tampered (Y-mat-prog refuses),
  totals tampered (Y-mat-data refuses), v1 single-publication datum.
- `cek-proof-v1.test.ak`: `program_and_data_walks_compose_to_the_complete_walk`
  over every material fixture (including the 9000-byte data child of
  `semantic_head_list_extracts_a_9000_byte_data_child_by_root`); property:
  random kind-5 orphan appended to `entries` → rejected by the data walk's
  `seen` count.
- `validation-resolver-v1.test.ak`: route tests re-pointed at the entries
  form; `IncrementalCekMaterial → False` unchanged (18 checks today).

## 9. Verification commands

```bash
cd /home/gumbo/midgard-hub/midgard/onchain/aiken
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");let n=0,bad=0;for(const v of b.validators){if(!/validation_trace\/cek_execution_selection/.test(v.title)||!/\.(spend|withdraw)$/.test(v.title))continue;n++;const s=Buffer.from(v.compiledCode,"hex").length;if(s>15000)bad++;console.log(v.title,s)}console.log("scripts",n,"over 15000:",bad)'
# expected: 5 scripts (1 spend + 4 withdraw), over 15000: 0
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m fraud_proofs/validation_trace/cek_split_v1
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m midgard/cek_proof_v1
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m midgard/validation_resolver_v1
cd /home/gumbo/midgard-hub/midgard
pnpm --filter @al-ft/midgard-sdk test -- tests/validation-resolver-applied-hashes.test.ts
pnpm --filter @al-ft/midgard-core test -- src/deployment-manifest-identity.test.ts   # manifest id re-pin
pnpm --filter @al-ft/midgard-fault-proofs test -- tests/semantic-resolver-arity-gate.test.ts tests/validation-dispute-submit.test.ts tests/submit-init-emulator-validation-dispute.test.ts tests/submit-init-emulator-cek-execution-selection-v1.test.ts
```

## 10. Ordering and dependencies

- Lands with the two sibling CEK plans (shared aux projection helpers in
  `validation-machine-v1.ak`, shared deployment-entry restructuring, one
  regeneration / catalogue-root re-pin; D's hash changes, so `cek_v1` and
  `dispute_v1` re-apply).
- The role additions move the deployment-manifest identity; coordinate with
  every other plan that adds roles (value-and-mint, phase-A, availability
  challenge) so the manifest id is re-pinned once.
- `cek-proof-v1.ak` walk split is used by no other family; the
  single-publication datum v2 touches `deriveCekSinglePublicationV1` and
  the necessity evidence `docs/exec-plans/evidence/necessity/cek-program-material-v1.md`
  (re-pin its receipts).
- Independent of the script-sources plan.

## 11. Risks

- **Material yields are marginal (14.0–14.4 KB).** Sourcing cost is
  estimated; if over, the per-route variant fallback adds one role. The
  eight inspectors are the floor and cannot be pruned.
- **Aggregate budget worst case is unmeasured.** The fallback is the chain
  shape (binder → authenticate → material hop with the same two yields →
  settle), which keeps all validators and adds datum types; decide after the
  first emulator maximum-shape measurement.
- **ABI churn.** Redeemer gains three fields; `DirectCekMaterial` and the
  single-publication datum change shape (v2); every SDK material path and
  golden moves. The route enum keeps its constructor order so
  `IncrementalCekMaterial`/`NoCekMaterial` refusal vectors are unaffected.
- **Roles and stake.** Four new roles in two rosters, four reward accounts
  to register at deployment; a missing registration fails the withdrawal at
  submission with a ledger error, not a validator error — the submit path
  should check `rewardAccountAt(...).registered` first as `setup-tx.ts`
  does.
- **Spec.** C47/C48 material completeness must remain exact; the
  composition property in §8 is the proof obligation. The
  `cek_program_material_script_hash` parameter stays on D even though only
  the yields read it, so the necessity artifact's "third parameter" claim
  and the arity test keep a stable anchor; drop it only together with an
  update to `validation-resolver-applied-hashes.test.ts`.
