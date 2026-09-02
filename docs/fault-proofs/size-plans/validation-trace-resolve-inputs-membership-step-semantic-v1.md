# Size-fit plan: `resolve_inputs_membership_step_semantic_v1`

Cites [00-primer.md](00-primer.md). This is the anchor plan for the six
resolve-inputs resolvers: it defines the shared narrow control codec
(`resolve-inputs-control-v1.ak`, §4.1) used by all six. It does **not** define
its own output-proof yields: the stage yields it consumes are the shared
ledger-output-proof (LOP) yield family owned by
[validation-trace-script-sources-output-proof-step-semantic-v1.md](validation-trace-script-sources-output-proof-step-semantic-v1.md)
§4.2 (eleven `V1VtLop…Yield` roles, parameter `dispatcher_script_hashes`
listing both dispatchers) over the semantic-yield handshake of
[validation-trace-script-sources-non-output-semantic-v1.md](validation-trace-script-sources-non-output-semantic-v1.md)
§4.2. The reconciliation with that design is recorded in §10.

## 1. Identity

| Field                         | Value                                                                                                                                                                                                                                                                                                                                                                                                                        |
| ----------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title               | `fraud_proofs/validation_trace/resolve_inputs_membership_step_semantic_v1.main.spend`                                                                                                                                                                                                                                                                                                                                        |
| File                          | `onchain/aiken/validators/fraud-proofs/validation-trace/resolve-inputs-membership-step-semantic-v1.ak`                                                                                                                                                                                                                                                                                                                       |
| Raw size                      | 72,039 bytes (4.40× the 16,384 limit; 4.80× the 15,000 target)                                                                                                                                                                                                                                                                                                                                                               |
| Applied parameters            | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` (resolved by name in `buildValidationTraceDisputeChain`, `demo/midgard-sdk/src/fraud-proof/contracts.ts`)                                                                                                                                                                                                                                          |
| Phase / resolver index        | `ResolveInputs`, resolver 7 (`validation_resolution_v1.resolver_index`)                                                                                                                                                                                                                                                                                                                                                      |
| Semantic index / global index | 3 of 6 in `resolve_inputs_v1` (`prepare_selected(..., 6, ...)`); global 29 (`VALIDATION_SEMANTIC_RESOLVER_OFFSETS_V1[7] = 26`)                                                                                                                                                                                                                                                                                               |
| Auxiliary                     | `LedgerOutputProofStepWitness { witness: LedgerOutputProofWitnessV1 }` (constructor 32; submit.ts resolver-7 arm, `VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofStep`)                                                                                                                                                                                                                                                    |
| Library entry                 | `validation_machine_v1.verify_resolve_inputs_membership_step_semantics_v1(pre, transition, proof_witness)` → `resolve_inputs_control_is_bound` + `resolve_membership_proof_step` → `ledger_output_proof_v1.step_v1`, then `resolve_inputs_successor_is_exact` or `rejected_successor_is_exact` with `reject_invalid_output` / `reject_invalid_field_type` / `reject_native_script_node_count` / `reject_native_script_depth` |
| Work witness                  | `encode_resolve_inputs_witness`: definite 11-array; item 9 is `encode_definite_bytes(#"00")` (no pending) or `encode_definite_bytes(#"85" ++ source_kind ++ key ++ next_schedule_hash ++ descriptor_cbor ++ encode_definite_bytes(encode_control_v1(output_proof)))`, so the LOP control is a byte string nested two levels deep (`validation-machine-v1.ak` lines 905–969)                                                  |
| Role name today               | none (semantic resolvers carry no `reference_script_auth` role NFT)                                                                                                                                                                                                                                                                                                                                                          |
| Deployment entry today        | none; wired in `contracts.ts` `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.resolveInputsMembershipStep` only; no `submit.ts` deployment entry, no submit route, no funding row (only `VALIDATION_CEK_…` and `VALIDATION_VALUE_AND_MINT_…_DEPLOYMENT_ENTRIES_V1` exist at `submit.ts` lines 815 and 938)                                                                                                            |
| Emulator today                | no resolver-7 fixture; `resolver-proof-fit-sweep-generate-v1.test.ts` records resolver 7 in `unfit[]`                                                                                                                                                                                                                                                                                                                        |

## 2. Why it is this size

Measured on a copy at `/tmp/size-probe-ri` (pinned fork `v1.1.23+5adf783`,
`aiken build --env testnet`, three probe rounds; raw compiled-body bytes;
empty spend validator `p_floor` = 94). Probes are throwaway validators under
`validators/probe/` calling one function each; private library functions were
made `pub` in the copy only. The two `ri2_*` rows were measured on a second
copy (`/tmp/size-probe-ri2`, `git archive HEAD` at `815b703a9`) during the
review pass that reconciled this plan with the shared yield family; that
build reproduced all six baseline sizes byte-for-byte.

| Probe                                                                                                                                       |                                                  Bytes | What it isolates                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| ------------------------------------------------------------------------------------------------------------------------------------------- | -----------------------------------------------------: | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `d_membership_step` (validator with the predicate replaced by `True`)                                                                       |                                                  4,760 | dispatcher: `continue_winning`, `cancel`, action decoder                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `p_pred_membership_step` (predicate alone)                                                                                                  |                                                 69,687 | the whole semantic predicate                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| `p_control_parse` = `resolve_inputs_control_from_witness`                                                                                   |                                                 18,598 | control parse; almost all of it is `ledger_output_proof_v1.decode_control_v1`                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `p_lop_decode` = `ledger_output_proof_v1.decode_control_v1`                                                                                 |                                                 18,279 | sub-control decoders + outer well-formedness + encode round-trip                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| `p_lop_from_data` (decode only, no outer wf, no encode)                                                                                     |                                                 12,546 | the five sub-control `control_from_data_v1` decoders                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| `p_lop_from_data_wf`                                                                                                                        |                                                 15,297 | + `control_is_well_formed` (2,751)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| `p_lop_from_data_encode`                                                                                                                    |                                                 17,780 | + `encode_control_v1` (5,234)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `p_scan_from_data` / `p_scan_codec`                                                                                                         |                                          3,592 / 4,253 | `ledger_output_scan_v1` decode / + encode                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| `p_value_from_data` / `p_value_codec`                                                                                                       |                                          2,344 / 2,824 | `ledger_output_value_v1`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `p_trav_from_data` / `p_datum_codec`                                                                                                        |                                          7,200 / 8,397 | `cek_data_traverse_v1` (nests `cek_data_integer_v1` 5,041 and `cek_data_bytes_v1` 5,256 codecs)                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| `p_hash_codec` / `p_native_codec`                                                                                                           |                                          2,539 / 2,320 | `blake2b_224_trace_v1` / `native_script_scan_v1`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| `p_lop_step` = decode + `step_v1`                                                                                                           |                                                 64,264 | all six stage functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| `p_lop_structure` − `p_lop_decode`                                                                                                          |                                                 10,398 | `structure_step` (scan `step_v1` + `finish_v1` + `authenticated_chunk_window` + value initial control)                                                                                                                                                                                                                                                                                                                                                                                                                           |
| `p_lop_value` − decode                                                                                                                      |                                                  6,670 | `value_fold_step`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| `p_lop_datum` − decode                                                                                                                      |                                                 26,315 | `datum_traversal_step` (`cek_data_traverse_v1.step_v1`)                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| `p_lop_refscript` − decode                                                                                                                  |                                                  3,751 | `reference_script_commitment_step`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| `p_lop_hash` − decode                                                                                                                       |                                                  6,368 | `script_hash_step`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| `p_lop_native` − decode                                                                                                                     |                                                  7,661 | `native_script_step`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| scan sub-steps above `p_scan_from_data`                                                                                                     |      1,296 / 1,331 / 1,602 / 2,234 / 2,267 / 468 / 251 | `step_required_fields` / `step_value_header` / `step_policy_header` / `step_asset` / `step_optional_field` / `step_payload` / `finish_v1`                                                                                                                                                                                                                                                                                                                                                                                        |
| traverse stage steps above traverse decode + action decoder (9,426)                                                                         | 5,146 / 9,672 / 11,208 / 6,011 / 3,886 / 1,820 / 7,009 | `step_head` / `step_integer` / `step_bytes` / `step_large_constructor` / `step_large_fields` / `step_close` / `step_fold`                                                                                                                                                                                                                                                                                                                                                                                                        |
| `p_chunk_verify` = `bounded_item_v1.verify_chunk`                                                                                           |                                                  2,179 | chunk authentication incl. `ChunkProofV1` decoder                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| `p_successor_exact` − `p_control_parse`                                                                                                     |                                                  1,217 | `resolve_inputs_successor_is_exact` beyond the parse                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| `p_rejected_exact`                                                                                                                          |                                                  1,567 | `rejected_successor_is_exact`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `p_yield_auth`                                                                                                                              |                                                    664 | `state_queue_yield.require_authenticated_zero_yield`                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| `p_decode_step`                                                                                                                             |                                                  2,453 | decoders for `ValidationMachineStateV1` + `ValidationOneStepWitnessV1` + `LedgerOutputProofWitnessV1`                                                                                                                                                                                                                                                                                                                                                                                                                            |
| `p_control_parse_narrow` / `p_control_bound_narrow`                                                                                         |                                          1,458 / 2,990 | narrowed control parse / parse + binding with `pending` kept as raw bytes                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| `v_membership_step_dispatch` (earlier pure-oracle dispatcher prototype, superseded)                                                         |                                                 12,016 | measured, production shape, LOP frame parse + claim binding                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| `y_value`, `y_refscript`, `y_hash`, `y_datum_close`, `y_datum_bytes` (earlier pure stage-yield prototypes, superseded by the shared family) |               11,708 / 4,223 / 7,056 / 13,395 / 23,948 | measured; kept as corroboration of the shared plan's per-stage projections                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| **`ri2_step` (§4 dispatcher, production shape)**                                                                                            |                                              **8,402** | three parameters; raw 11-item parse + nested 5-item pending parse; every clause of `resolve_inputs_control_is_bound` except LOP well-formedness (incl. `ledger_output_commitment_v1.decode`, `decode_midgard_tx_input_cbor`, `resolution_schedule_node_hash`, `native_tx_proof_commitment_v1`, canonical re-encode of the witness); one `require_authenticated_zero_yield` over the 11-role table; `control_cbor` pin; successor by splice of `next_control_cbor` into the pending record, or `Terminal` phase in rejection mode |
| `ri2_fin`                                                                                                                                   |                                                 11,153 | the finalize dispatcher; see the finalize plan §2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |

Reading: the validator shell is 4.8 KB. The 67 KB predicate is
`ledger_output_proof_v1.step_v1` plus its control codec. The codec alone
(18.3 KB) exceeds the 15,000-byte target, so **no script that decodes the
whole `LedgerOutputProofControlV1` can fit**, whatever else it does. Inside
`step_v1`, the datum-traversal stage (26.3 KB, the `cek_data_traverse_v1`
machine) and the structure stage (10.4 KB) dominate. The carrier-specific
part of the resolver (resolve-inputs binding, pending record, successor) is
under 4 KB once the LOP codec is kept as raw bytes (`ri2_step` 8,402 minus
the 4,760 shell).

`aiken check -m resolve_inputs` on the copy:
`resolve_inputs_streams_authenticated_membership_output` 30,556,647 mem /
12,874,230,739 cpu. This test builds the whole output-proof trace inside the
test body, so it is an upper bound, not the validator's cost; validator-only
ExUnits are not measured today (§11).

## 3. Options considered

1. **Prune.** `resolve_inputs_control_is_bound` and
   `resolve_inputs_control_from_witness` can be narrowed (the `pending`
   output-proof control kept as raw bytes: 2,990 instead of 22,741) and that
   is kept as part of the design. Pruning cannot remove `step_v1` or its
   codec because the resolver's whole job is one `step_v1` transition.
   Rejected as sufficient on its own.
2. **Withdraw-zero yield split, single transaction.** Chosen. Only one stage
   function runs per transition (`control.stage` selects it), so routing each
   stage to its own yield keeps one script per transaction near the size of
   that stage. Two refinements are forced by the numbers and are the same
   two the script-sources output-proof-step plan arrived at independently:
   (a) each yield decodes only the sub-control its stage advances and splices
   the re-encoded sub-control into the raw LOP control bytes, because the
   full codec (18.3 KB) does not fit any yield; (b) the datum-traversal stage
   is split by traversal action family (six `V1VtLopDatum*` yields), because
   `step_integer`/`step_bytes` are machines of their own (9.7 / 11.2 KB).
   Because `script_sources_output_proof_step_semantic_v1` (82,309 bytes,
   `validation_machine_v1` line 9318) runs the _same_ `step_v1` with the
   _same_ four rejection codes (lines 9318–9366 versus 6395–6448), the yields
   are carrier-agnostic and are shared, not duplicated.
3. **Multi-transaction chaining.** Rejected for this contract. Chaining
   splits _execution_, not _reachable code_: every hop would still contain
   `decode_control_v1` (18,279 > 15,000) unless it also narrowed the codec,
   at which point it is the yield design with extra transactions. It would
   add ≥ 1 transaction per output-proof stage hop against GOAL_SPEC C52 and
   the §3.3 maturity margin, and the intermediate state would have to be
   hashed into the thread datum for a step that the trace already treats as
   atomic (one `program_counter` increment). Single-transaction yields keep
   the budget of today's monolith (one stage executes) plus one witness
   re-parse.
4. **Redesign.** The arm boundaries (`begin`/`step`/`finalize`) are right;
   what is wrong is the codec granularity, which the splice technique fixes
   without changing a trace byte.

## 4. Chosen design

### 4.1 Validator list

| Validator (blueprint title)                                                                                                                                                                   | Kind     | Responsibility                                                                                                                                                                                                                                                                                                                                             | Params                                                                                                              | Role NFT |                                                                            Size |
| --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- | -------- | ------------------------------------------------------------------------------: |
| `fraud_proofs/validation_trace/resolve_inputs_membership_step_semantic_v1.main.spend` (dispatcher, same title)                                                                                | spend    | `continue_winning` with `semantic_transition_is_valid := membership_step_dispatch(...)`: narrow resolve-inputs binding, one authenticated stage yield, successor by splice                                                                                                                                                                                 | `award_script_hash`, `computation_thread_policy_id`, **`reference_script_auth_policy_id`** (new)                    | none     | **8,402 measured** (`ri2_step`); ≈ 9,700 with the optional §4.3 scalar re-check |
| `…/ledger_output_proof_structure_yield_v1.main.withdraw` … `…/ledger_output_proof_native_script_yield_v1.main.withdraw` (eleven, **owned by the script-sources output-proof-step plan §4.2**) | withdraw | one LOP stage / traversal-action family each: `V1VtLopStructureYield`, `V1VtLopValueYield`, `V1VtLopDatumFoldMapYield`, `V1VtLopDatumFinalizeFrameYield`, `V1VtLopDatumHeadYield`, `V1VtLopDatumAttachScalarYield`, `V1VtLopDatumFoldListYield`, `V1VtLopDatumAdvanceYield`, `V1VtLopRefScriptYield`, `V1VtLopScriptHashYield`, `V1VtLopNativeScriptYield` | `dispatcher_script_hashes: List<ScriptHash>` = `[script_sources_output_proof_step, resolve_inputs_membership_step]` | as named |                8,000–14,500 projected there (y30 12,914 measured for structure) |

Shared library modules (new, this plan's own):

- `lib/midgard/resolve-inputs-control-v1.ak` — `PendingRawV1 { source_kind: Int, key, next_schedule_hash, descriptor_cbor, output_proof_cbor: ByteArray }`, `ResolveInputsControlRawV1` (the ten scalar fields of `ResolveInputsControlV1` plus `pending: Option<PendingRawV1>`), `control_raw_from_witness(witness_cbor)` (`cbor.deserialise` + `un_list_data` of the 11 items, then of the 5 pending items when item 9 is not `#"00"`), `encode_control_raw(control)` (byte-identical to `encode_resolve_inputs_witness` — the pending record is re-encoded as `#"85" ++ … ++ encode_definite_bytes(output_proof_cbor)` without touching the LOP bytes), `encode_pending_raw(pending)`, `control_raw_is_bound(pre, witness, control)` (every clause of `resolve_inputs_control_is_bound` except `ledger_output_proof_v1.control_is_well_formed` and the three LOP-scalar equalities, see §4.3), and the no-pending variants used by the four prune plans (`control_no_pending_from_witness`, `encode_control_no_pending`, `control_no_pending_is_bound`). The pinned fork aborts on `Option<record> == None`; the codec must test `pending_cbor == #"00"` on the raw bytes, never compare the option.
- Role constants live in the shared `lib/midgard/validation-semantic-yield-v1.ak` (non-output plan §4.2); this plan adds nothing there.

### 4.2 Redeemer / datum ABI deltas

```aiken
pub type ActionV1 {
  VerifyMembershipStep {
    input_index: Int, output_index: Int,
    transition: ValidationOneStepWitnessV1,
    proof_witness: Data,          // LedgerOutputProofWitnessV1 on the wire, decoded only by the yield
    control_cbor: ByteArray,      // the pre LOP control; pinned to pending.output_proof_cbor
    next_control_cbor: ByteArray, // successor LOP control (Advanced) or #"" (rejection)
    yield_role_index: Int,        // 0..10 into the shared V1VtLop… role table
    yield_ref_input_index: Int,
  }
}
validator main(award_script_hash, computation_thread_policy_id, reference_script_auth_policy_id: PolicyId)
```

The fields after `transition` are, in order, exactly those the
`script_sources_output_proof_step` dispatcher carries (§10 D1), so the yields
read one redeemer layout through `unique_semantic_dispatch_v1`'s `extra`.
Datum unchanged (`ct.StepDatum<PreparedValidationResolutionStateV1>`).
Auxiliary hashed into evidence as `builtin.constr_data(32, [proof_witness])`,
byte-identical to today's `LedgerOutputProofStepWitness { witness }`, so
`hash_one_step_evidence` and `prepare_selected` are untouched. **No trace ABI
change**: `encode_resolve_inputs_witness`, `ledger_output_proof_v1.encode_control_v1`
and every work-witness byte stay identical; `midgard-core/src/ledger-output-proof-v1.ts`
and the `demo/midgard-validation` codecs are untouched.

### 4.3 Handshake

1. **Dispatcher predicate** `membership_step_dispatch(pre, transition, control_cbor, next_control_cbor, yield_role_index, yield_ref_input_index, policy, tx)` (the `ri2_step` probe is this function verbatim):
   - `control = control_raw_from_witness(transition.work_witness_cbor)`; `expect Some(pending) = control.pending` (raw bytes `!= #"00"`).
   - `control_raw_is_bound(pre, transition, control)`: `native_tx_proof_commitment_v1(compact, ws, lengths) == pre.transaction_commitment`; `hash_validation_context(context_cbor) == pre.validation_context_hash`; the 32-byte length gates on `accumulator`, `remaining_schedule_hash`, `resolution_schedule_hash`, `signer_frontier_commitment`; `cursor <= 1 ⇒ remaining == resolution_schedule_hash`; `signer_count >= 0`; `cursor > 0`; `source_kind ∈ {0, 1}`; `resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == remaining_schedule_hash`; `decode_midgard_tx_input_cbor(key).output_index == ledger_output_commitment_v1.decode(descriptor_cbor).output_index`; `work_witness_cbor == encode_control_raw(control)` (canonicity of the agreed bytes, as the monolith checks).
   - **Optional scalar re-check (recommended, ≈ +1.3 KB):** `un_list_data` the LOP control into its 12 items (1,202 B, probe c06 of the shared plan) and require `items[0] == 1`, `0 ≤ items[1] ≤ 6`, `items[2] == descriptor.output_index`, `items[3] == descriptor.total_length`, `items[4] == descriptor.item_commitment` — the three equalities the monolith's `resolve_inputs_control_is_bound` states explicitly. Without it these facts are inherited from membership-begin (which constructs items 2–4 from the descriptor) and preserved because no stage step rewrites items 0–4; with it the dispatcher is clause-for-clause the monolith minus `control_is_well_formed`, which the yields discharge for the sub-control they advance. Size allows it (8,402 + ≈ 1,300 < 15,000); take it.
   - `control_cbor == pending.output_proof_cbor` — the channel value the yield attests is pinned to the agreed pre-state bytes.
   - `require_semantic_yield_v1(tx, reference_script_auth_policy_id, lop_role(yield_role_index), yield_ref_input_index)` (= `state_queue_yield.require_authenticated_zero_yield`; primer handshake item 1).
   - Successor (primer item 3): if `next_control_cbor == #""` (rejection mode) require `claimed_successor.phase == Terminal` — the yield has already required `rejected_successor_is_exact(pre, claimed_successor, code)` for the code _it_ computed; else require `claimed_successor.phase == ResolveInputs` and `claimed_successor.work_root == hash_work_witness(ResolveInputs, pre.program_counter + 1, encode_control_raw({..control, pending: Some({..pending, output_proof_cbor: next_control_cbor})}))`. An encoded LOP control is never empty, so the mode is unambiguous (§10 D2).
2. **Yield** (shared; script-sources output-proof-step plan §4.2): `unique_semantic_dispatch_v1(dispatcher_script_hashes, tx)` → exactly one input at either listed dispatcher credential, its inline datum as `validation_semantic_v1.Datum`, its `Spend` redeemer's `extra` = `(proof_witness, control_cbor, next_control_cbor, yield_role_index, yield_ref_input_index)`; `expect yield_role_index == own_index`; `un_list_data(control_cbor)` into 12 items; `expect un_i_data(items[1]) == own_stage` (and the traversal action tag for datum yields); decode only its sub-control with that module's `control_from_data_v1` (which asserts well-formedness); decode `proof_witness` as `LedgerOutputProofWitnessV1` and authenticate the chunk window (`bounded_item_v1.verify_chunk` against `items[4]`); run the stage step; on `Advanced` re-encode the changed sub-control(s) with the module encoder, splice into `control_cbor` (offset verified by `slice(offset, len(old)) == old` where `old` is the module encoder's output for the decoded pre sub-control) and require equality with `next_control_cbor`; on a rejecting result require `next_control_cbor == #""` and `rejected_successor_is_exact(state.resolution.pre_state, transition.claimed_successor, code)` with the code the yield computed.
3. Output-state re-derivation stays in the dispatcher: `continue_winning` requires the award script hash and the `winning_resolution()` datum on the continuation output.
4. Parameters (primer item 4): the dispatcher carries the auth policy id and a compiled-in role table; the yields carry both dispatcher hashes; nothing is trusted from a redeemer except as a channel checked on both ends.

### 4.4 Successor bytes without the LOP codec (the codec cut)

The resolve-inputs witness nests the LOP control as
`encode_definite_bytes(encode_control_v1(output_proof))` inside the pending
byte string (§1). The dispatcher therefore never needs the LOP codec: it
re-encodes the eleven items with `encode_control_raw`, substituting
`next_control_cbor` for the nested byte string. Canonicity of the substituted
bytes is the yield's responsibility (it produced them by splicing a
module-encoder output into the agreed canonical pre control); canonicity of
every other byte follows by induction from `prepare_selected`, which required
`hash_work_witness(pre.phase, pre.program_counter, work_witness_cbor) ==
pre.work_root`, and from membership-begin, which constructed the first pending
record with the canonical encoders. This is the same canonical-by-induction
argument the RF-021 envelope binder and the script-sources narrow resolvers
rely on (non-output plan §4.1).

### 4.5 Datum stage

Handled inside the shared family: `V1VtLopDatumFoldMapYield`,
`V1VtLopDatumFinalizeFrameYield` (RF-021 template executors, ≈ 9,000 / 10,500),
`V1VtLopDatumHeadYield`, `V1VtLopDatumAttachScalarYield`, `V1VtLopDatumFoldListYield`,
`V1VtLopDatumAdvanceYield` (traverse codec, ≈ 9,000–14,500). The earlier draft
of this plan left the datum stage blocked on a separate cek-data traversal
oracle; that gap is closed by adopting the shared family (§10 D5). The
per-stage traverse costs measured here (`step_head` 5,146, `step_integer`
9,672, `step_bytes` 11,208, `step_large_constructor` 6,011, `step_large_fields`
3,886, `step_close` 1,820, `step_fold` 7,009 above the 9,426 traverse
decode/action floor) corroborate the shared plan's projection that
`V1VtLopDatumHeadYield` and `V1VtLopDatumAttachScalarYield` are the borderline
ones and must pass the §9 size gate before merge.

### 4.6 Security argument

- **Dispatch uniqueness.** `unique_semantic_dispatch_v1` requires exactly one input at a listed dispatcher credential and exactly one `Spend` redeemer for it; `get_unique_withdraw_redeemer` admits exactly one redeemer per yield script. One zero-withdrawal therefore discharges exactly one thread, even when a script-sources and a resolve-inputs dispatcher sit in the same transaction (the singleton filter refuses that transaction).
- **Role authentication.** `require_authenticated_zero_yield` requires the indexed reference input to carry exactly one `reference_script_auth_policy_id` token whose name equals `lop_role(yield_role_index)`, an exact zero-lovelace withdrawal from that reference script's hash, and its unique redeemer. A look-alike script fails on the role NFT; a withdrawal from another script fails on the credential.
- **Cross-arm substitution.** `yield_role_index` is prover-chosen (like `semantic_resolver_index` in `prepare_selected`); the dispatcher maps it to a fixed role, and every yield checks `yield_role_index == own_index` and `items[1] == own_stage` (plus the traversal action tag) on the pinned `control_cbor`, so a value yield presented for a structure-stage control fails inside the yield. A descriptor yield (`V1VtLopDesc…`) presented here fails on the role name, and its predicate never produces a `next_control_cbor`.
- **Output-state re-derivation.** `continue_winning` re-derives the award output; the successor `work_root` is recomputed from the authenticated pre bytes plus `next_control_cbor`, which the yield has independently recomputed from the same pre bytes and the witness. `next_control_cbor` is a channel checked on both ends, never trusted.
- **Rejection mode.** The mode is encoded in the channel (`#""`); the yield chooses the rejection code and checks `rejected_successor_is_exact` itself, so the prover cannot present an honest `Advanced` step as a rejecting terminal (the yield would require a non-empty `next_control_cbor`) nor a rejection as an advance.
- **Omitted yield.** `require_authenticated_zero_yield` is an `expect`; omission fails the dispatcher, so the transition is unprovable rather than accepted. A yield whose `dispatcher_script_hashes` does not list this dispatcher cannot find its input. If a role were omitted from deployment, membership steps in that stage would be unprovable (liveness), never provable-wrong.
- **Well-formedness inheritance.** The monolith checks `control_is_well_formed` on the whole pre control; the split checks the LOP scalars in the dispatcher (§4.3 re-check) and, in the yield, the sub-control it advances (the module `control_from_data_v1` asserts it) plus the cross-field clauses the transition touches. Untouched sub-controls are copied verbatim from the agreed pre-state, which lies on the honest party's trace in every dispute where one party is honest (bisection invariant). The §8 property tests pin yield-splice equality with `step_v1` on every stage vector.

## 5. Size and budget projection

| Script                                                                          | Basis                                                                                                              |                      Projected raw bytes |
| ------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ | ---------------------------------------: |
| dispatcher                                                                      | `ri2_step` measured 8,402; + ≈ 1,300 for the §4.3 scalar re-check (shared-plan probe c06 1,202 + five comparisons) | **≈ 9,700** (8,402 without the re-check) |
| `V1VtLopStructureYield`                                                         | shared plan y30 12,914 + two-chunk window / `finish_v1` / value template                                           |                    ≈ 14,400 (borderline) |
| `V1VtLopValueYield`                                                             | shared plan                                                                                                        |                                 ≈ 10,500 |
| `V1VtLopDatumFoldMapYield` / `…FinalizeFrameYield` / `…FoldListYield`           | RF-021 executors + yield shell                                                                                     |                 ≈ 9,000 / 10,500 / 9,000 |
| `V1VtLopDatumHeadYield` / `…AttachScalarYield` / `…AdvanceYield`                | traverse codec 8,295 + steps                                                                                       |  ≈ 14,500 (borderline) / 14,000 / 12,000 |
| `V1VtLopRefScriptYield` / `V1VtLopScriptHashYield` / `V1VtLopNativeScriptYield` | shared plan                                                                                                        |   ≈ 8,000 / 12,300 / 13,500 (borderline) |

Referenced bytes per semantic transaction: dispatcher + exactly one stage
yield = **≤ 9,700 + 14,500 = 24,200 bytes** (today 72,039), which stays in
tier 0 of the Conway reference-script fee (`minFeeRefScriptCostPerByte` 15
lovelace, stride 25,600 bytes, multiplier 1.2): ≤ 363,000 lovelace
(≈ 0.36 ADA) against ≈ 1,295,000 lovelace (≈ 1.29 ADA: 384,000 + 460,800 +
20,839 × 21.6) for the monolith, and far under `maxRefScriptSizePerTx`
(200 KiB). The thread input itself is the dispatcher, published by reference,
so no inline script bytes count toward `maxTxSize`. Aggregate ExUnits: two
executions parse the dispatcher redeemer (`transition` + raw `proof_witness`);
the dispatcher additionally decodes the descriptor (≤ ~400 B) and re-encodes
the eleven items; the yield decodes one sub-control and the LOP witness (one
or two chunks). Today's monolith decodes every sub-control and runs the
encode round-trip, so the split is expected below today's cost; not measured
(no resolver-7 emulator fixture exists, §7) — the first lifecycle in §7
records `exUnits.mem` against the 13,200,000 production basis and the
16,500,000 harness limit.

## 6. Off-chain work

**Nothing exists today for this contract beyond the `contracts.ts` title:
no deployment entry, no submit route, no funding row, no role.** To create:

- **SDK contracts** (`demo/midgard-sdk/src/fraud-proof/contracts.ts`): add `reference_script_auth_policy_id` to the name-keyed parameter map of `resolveInputsMembershipStep` (value: the deployment's `referenceScriptAuthPolicy.policyId`; `buildMinimalFaultProofContracts` already threads `referenceScriptAuthPolicyId`); the `ledgerOutputProofYields` record (eleven withdraw validators, owned by the script-sources step plan) is applied with `[[scriptSourcesOutputProofStep.spendingScriptHash, resolveInputsMembershipStep.spendingScriptHash]]` — this plan contributes the second hash; `zz605-semantic-resolver-arity.test.ts` must see the three-parameter dispatcher and reject its two-parameter prefix; `zz610-compiled-script-arity.test.ts` gains the eleven yields.
- **Roles / manifest**: the eleven `V1VtLop…Yield` names in `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` (`demo/midgard-sdk/src/reference-scripts.ts`) and `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES` (`demo/midgard-core/src/deployment-manifest-identity-v1.ts`), the node manifest fixture (`demo/midgard-node/tests/deployment-manifest-v1.test.ts` `tokenName` rows), and a re-measure of `docs/exec-plans/evidence/canonical-v1-cg1-control-publication-fit-v1.json` — added once by the script-sources step plan; this plan adds no role of its own.
- **Deployment entries** (`demo/midgard-fault-proofs/src/validation-dispute/submit.ts`): new `VALIDATION_RESOLVE_INPUTS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1 = {0: validationTraceDisputeResolveInputsInitialSemantic, 1: …FinishSemantic, 2: …MembershipBeginSemantic, 3: …MembershipStepSemantic, 4: …MembershipFinalizeSemantic, 5: …NonMembershipSemantic}`, `VALIDATION_RESOLVE_INPUTS_RESOLVER_INDEX_V1 = 7`, and `requireValidationResolveInputsSemanticReferenceScriptUtxo` mirroring `requireValidationValueAndMintSemanticReferenceScriptUtxo` (line 1007); the yield entries `validationTraceDisputeLop{Structure,Value,DatumFoldMap,DatumFinalizeFrame,DatumHead,DatumAttachScalar,DatumFoldList,DatumAdvance,RefScript,ScriptHash,NativeScript}Yield` come from the script-sources step plan.
- **Submit route** (`submitValidationDisputeSemanticResolution`): for `resolverIndex === 7 && semanticResolverIndex === 3`, resolve the dispatcher by reference; `semanticActionFieldsV1` (line 1509 arm) emits `[input_index, output_index, transition, proof_witness, control_cbor, next_control_cbor, yield_role_index, yield_ref_input_index]`; select the yield with the shared pure helper `ledgerOutputProofYieldRoleIndexV1(controlCbor, witness)` and take `nextLedgerOutputProofControlCborV1` from the evidence builder (`demo/midgard-validation` `buildMidgardLedgerOutputProofTraceV1` already computes the successor control to build the successor work witness; for a rejecting step emit `#""`); `.readFrom([yieldUtxo])`, `.withdraw(scriptRewardAddress(network, yield.withdrawalScript), 0n, Data.void())`, `yield_ref_input_index` via `requireReferenceInputIndex` (min-ADA step-02 pattern, `demo/midgard-fault-proofs/src/min-ada/submit-step-02-v1.ts`). Reward accounts are registered once at deployment (as `registerPhasMembershipRewardAccount` does).
- **Funding rows**: `production-funding-requirements-v1.ts` measurement row for action kind `validation-dispute.semantic.resolve-inputs.membership-step` with `referenceScriptBytes` = dispatcher + largest yield (≈ 24,200); the eleven reward-account deposits (2 ADA each) are budgeted by the script-sources step plan.
- **Inspection fixtures**: `inspect-contracts.test.ts` and the contract-deployment-info fixtures gain the dispatcher's third parameter and the six resolve-inputs deployment entries.
- **midgard-core / validation codecs**: none (no trace ABI change). The one-step argument gains `resolverHints.controlCbor`, `resolverHints.nextControlCbor`, `resolverHints.yieldRoleIndex` (same hint names as the script-sources step plan).

## 7. Emulator scenario tests

New file `demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-resolve-inputs.test.ts`
(covers all six resolve-inputs plans), reusing `runForcedValidationDisputeScenario`
(`tests/support/emulator/dispute-scenario.ts`) after generalising its
ValueAndMint-only reference branch to the resolver-7 roster and
`buildRemovalDeploymentInfo` gaining `validationResolveInputsSemanticReferences`
and `ledgerOutputProofYieldReferences`.

- **Fixture**: extend `buildForgedOperatorSuccessorValidationDisputeFixture` (`validation-dispute-fixtures.ts`) with `disputedPhase: "resolveInputs"` and a `disputedStep` selector choosing the low index of the first `ledgerOutputProofStep` of each LOP stage (`structure`, `value`, datum `FoldMap`/`FinalizeFrame`/`Head`/`AttachScalar`/`FoldList`/`Advance`, `refscript`, `script_hash`, `native_script`); the honest transaction spends an output carrying two assets, an inline datum with a nested map and a native reference script (`largeFittingOutputCbor` extended as in the script-sources journey file) so every stage occurs — the maximum supported shape.
- `publishes the resolve-inputs step dispatcher and every LOP yield inside the L1 envelope`: `publishPlainReferenceScriptUtxo` (dispatcher) and `publishAuthenticatedValidationDisputeControl` (yields with role NFTs) **without `oversized`**, asserting `l1ByteMargin > 0` and `assertReferenceScriptRawBodiesFitL1EnvelopeV1` on every body, under `withRealL1MaxTxSize`.
- `resolves an honest output-proof <stage> step and awards` for each stage: full lifecycle through `submitValidationDisputeAward` and removal; assert `completeSignedBytes ≤ 16,384` and record `exUnits.mem` (≤ 13,200,000).
- `refuses a forged successor at the same frontier` (valid-block negative): the honest operator successor is presented as the challenger's claim at the same evidence frontier; expect on-chain refusal (`expect-onchain-refusal-v1.ts`) — the yield rejects the forged `next_control_cbor`.
- `refuses the wrong stage yield, a missing withdrawal, a stale control_cbor, and a Terminal claim on an advancing step`: four negatives on the same fixture.
- Rejection arm: a malformed output (`LedgerOutputProofInvalidOutput`) proves the `reject_invalid_output` terminal with `next_control_cbor = #""`.
- Cancel/resume at the prepared step (pattern of `submit-init-emulator-canonical-decodability-cancel-resume.test.ts`).
- Remove `oversized: true` for resolver 7 from `dispute-scenario.ts` / `tests/support/emulator/reference-scripts.ts`; the C53 fit sweep (`resolver-proof-fit-sweep-generate-v1.test.ts`) moves resolver 7 out of `unfit[]`.

Today: only `submit-init-emulator-validation-dispute.test.ts` (CEK resolver publication, canonical-decode lifecycle) and the InputSets fixture exist; nothing reaches resolver 7.

## 8. Aiken tests

- `lib/midgard/resolve-inputs-control-v1.test.ak`: property `encode_control_raw(control_raw_from_witness(w)) == w` and `encode_control_raw == encode_resolve_inputs_witness` over generated controls with and without pending; `control_raw_is_bound` agrees with `resolve_inputs_control_is_bound` on every existing resolve-inputs fixture (with the §4.3 re-check enabled, clause-for-clause except `control_is_well_formed`); `control_raw_from_witness` fails on a 10- or 12-item witness and on a pending byte string that is not a 5-array.
- `validators/fraud-proofs/validation-trace/ledger-output-proof-yields-v1.test.ak` (owned by the script-sources step plan): this plan adds `resolve_inputs_dispatcher_reuses_the_lop_yields` (the second listed dispatcher hash, honest vector from `resolve_inputs_authenticated_membership_step`), `yield_refuses_two_dispatcher_inputs` with one script-sources and one resolve-inputs thread, and `resolve_inputs_dispatcher_refuses_terminal_claim_with_nonempty_next_control`.
- `validators/fraud-proofs/validation-trace/resolve-inputs-split-v1.test.ak` (pattern: `value-and-mint-split-v1.test.ak`): for each stage, the honest vector passes the yield and the dispatcher; yield-splice ↔ `step_v1` equality on every stage transition including the four rejection outcomes; negatives — wrong `control_cbor`, wrong `proof_witness`, wrong `next_control_cbor`, wrong stage yield (role substitution), yield omitted, non-zero withdrawal, two withdraw redeemers for one yield, `yield_role_index` out of range, wrong `yield_ref_input_index`, pending absent (`#"00"`), descriptor `output_index` mismatch, and the §4.3 scalar re-check failing on a control whose `item_commitment` differs from the descriptor.

## 9. Verification commands

```bash
cp -r onchain/aiken /tmp/size-probe-ri && cd /tmp/size-probe-ri
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");const s=new Set();for(const v of b.validators){if(!/(resolve_inputs_.*semantic_v1\.main\.spend|ledger_output_proof_.*_yield_v1\.main\.withdraw|ledger_output_descriptor_.*_yield_v1\.main\.withdraw)$/.test(v.title)||s.has(v.title))continue;s.add(v.title);const n=Buffer.from(v.compiledCode,"hex").length;console.log(v.title,n,n<=15000?"OK":"OVER")}'
# expect 6 resolve_inputs semantic spend titles + 11 LOP yields + 4 descriptor yields = 21 titles, every body OK
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m resolve_inputs      # 8 existing + the split tests, 0 failures
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m ledger_output_proof_yields
cd /home/gumbo/midgard-hub/midgard/demo/midgard-fault-proofs
pnpm test -- tests/zz605-semantic-resolver-arity.test.ts tests/zz610-compiled-script-arity.test.ts tests/validation-dispute-submit.test.ts tests/inspect-contracts.test.ts
pnpm test -- tests/submit-init-emulator-validation-dispute-resolve-inputs.test.ts
cd ../midgard-sdk && pnpm test -- tests/reference-scripts.test.ts
cd ../midgard-node && pnpm test -- tests/deployment-manifest-v1.test.ts
```

## 10. Ordering and dependencies

- Lands with the other five resolve-inputs plans (shared `resolve-inputs-control-v1.ak`; `resolve_inputs_v1` re-applies all six hashes → catalogue root re-pin once) and with the script-sources output-proof-step and output-proof-finalize plans (the yield parameter lists both dispatcher hashes, so neither side can land alone).
- Depends on the semantic-yield handshake library (`validation-semantic-yield-v1.ak`, non-output plan §4.2) and the LOP library changes (`*_step_raw_v1`, initial-control templates) in the script-sources step plan.
- Requires `reference_script_auth_policy_id` in the semantic parameter set before the blueprint regeneration.

**Reconciliation record (review pass, 2026-09-02).** The earlier draft of this
plan defined its own nine parameterless "pure oracle" stage yields
(`V1VtOutputProof…Yield`, redeemer `OutputProofStepClaimV1` with byte spans)
and left the datum stage blocked on an external cek-data oracle. That design
was incompatible with the family the script-sources output-proof-step plan
owns. Decisions, preferring the shared design (the numbers allow it):

- **D1 Roles and files.** Adopt the eleven `V1VtLop…Yield` roles, the
  `ledger-output-proof-*-yield-v1.ak` files, `dispatcher_script_hashes`
  parameters and `unique_semantic_dispatch_v1`. The resolve-inputs step
  redeemer carries the same trailing fields as `VerifyOutputProofStep`
  **plus `control_cbor`**: the yields cannot extract the pre LOP control from
  a carrier witness without that carrier's frame codec (31-item script-sources
  frame vs 11-item resolve-inputs control), so both dispatchers must pass it
  explicitly and pin it to their own bytes. The script-sources step plan's
  wording "passed in `extra`" is read this way; its redeemer listing should
  gain the field.
- **D2 Rejection mode.** The shared dispatcher accepts "`successor_is_exact_v1`
  **or** `claimed_successor.phase == Terminal`". As written, an honest
  `Advanced` step could be presented with a `Terminal` claimed successor:
  the yield checks only `next_control_cbor` in advance mode and the
  dispatcher accepts the phase. This plan encodes the mode in the channel
  (`next_control_cbor == #""` ⇔ rejection; yields require the empty channel
  in rejection mode and a non-empty spliced control in advance mode). The
  script-sources step dispatcher should adopt the same one-line rule; until
  it does, the shared yields must at least require
  `claimed_successor.phase != Terminal` in advance mode.
- **D3 Uniqueness.** The earlier "two dispatchers may share one withdrawal
  for an identical fact" relaxation is withdrawn; the singleton filter of
  `unique_semantic_dispatch_v1` is the rule (min-ADA precedent).
- **D4 Spans → splice.** The byte-span factorisation technique is replaced by
  the shared splice-with-verified-offset technique and the
  canonical-by-induction argument (§4.4).
- **D5 Datum stage.** No external oracle; the six `V1VtLopDatum*` yields cover
  it, and the per-stage costs measured here corroborate their projections.
- **D6 Rejection codes.** Verified identical in both carriers
  (`script_sources_output_proof_step` vs `resolve_membership_proof_step`),
  so the yields' rejection arms need no per-dispatcher mapping.
- **D7 Measured.** The reconciled dispatcher was prototyped and measured
  (`ri2_step` 8,402) rather than projected; the earlier 12,016 prototype is
  superseded.

## 11. Risks

- **Borderline yields** (`structure`, `datum head`, `native script`, within
  1 KB of the target) are owned by the script-sources step plan; its §9 size
  gate blocks merge, and its named fallback splits apply. This dispatcher has
  ≥ 5 KB of headroom.
- **ExUnits unmeasured**: dispatcher + yield vs the 13,200,000 memory basis;
  measured on the first emulator lifecycle (§7). If the datum yields miss, the
  shared plan's fallback is a finer action split, not chaining.
- **ABI churn**: dispatcher redeemer gains four fields and a third parameter
  (`zz605` arity gate, `validation-dispute-submit.test.ts` redeemer ABI
  test); `proof_witness` becomes `Data` on the wire (same bytes); no trace
  bytes change.
- **Cross-plan coupling**: the yield parameter binds both dispatcher hashes,
  so a later change to either dispatcher re-applies all eleven yields and
  the catalogue root. Acceptable (one regeneration is the rule anyway) but
  worth stating in the deployment manifest.
- **D2 not adopted upstream**: if the script-sources step dispatcher keeps
  the unguarded `Terminal` acceptance, the shared yields must carry the
  `phase != Terminal` guard for both carriers; the §8 negative
  `resolve_inputs_dispatcher_refuses_terminal_claim_with_nonempty_next_control`
  is the regression test.
- **Spec**: GOAL_SPEC C41/C53 unchanged in meaning; the shared handshake is
  the min-ADA precedent generalised, which the primer should record.
