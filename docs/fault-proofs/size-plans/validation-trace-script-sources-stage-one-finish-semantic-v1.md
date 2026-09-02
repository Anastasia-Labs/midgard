# Size-fit plan: `script_sources_stage_one_finish_semantic_v1`

Cites [00-primer.md](00-primer.md) and the shared raw stage-frame library
([non-output plan](validation-trace-script-sources-non-output-semantic-v1.md) §4.1).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_one_finish_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-finish-semantic-v1.ak` (74 lines) |
| Raw size | **31,826 bytes** |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / index | `ScriptSources` (8), semantic slot **14** of 29, global index 46 |
| Library entry point | `verify_script_sources_stage_one_finish_raw_semantics_v1(pre, transition)` (`validation-machine-v1.ak:8613`) — already half raw: destructures the 30 `Data` items, decodes the witness set (`decode_native_tx_witness_set_compact` → `redeemer_tx_wits_hash`), checks the two arms (empty redeemer commitment ⇒ counts 0 and empty peaks; otherwise `redeemer_count == redeemer_total_count > 0` and `frontier_is_well_formed`), rebuilds `stage_prefix` with `script_sources_stage_zero_prefix_before_stage(control, 30)` and splices stage `01` → `02`; **but** it also calls `script_sources_control_from_witness` and requires `exact_script_sources_control(control) == witness.work_witness_cbor` |
| Redeemer | `VerifyFinish { input_index, output_index, transition }`; auxiliary `NoAuxiliaryWitness` |
| Role / deployment entry today | none / none (`submit.ts` index 14: `none` shape) |

## 2. Why it is this size

| Probe | Adds | Raw bytes | Delta |
| --- | --- | ---: | ---: |
| p03 | `script_sources_control_from_witness` | 25,595 | +22,246 |
| p20 | p03 + `exact_script_sources_control(c) == witness` | 29,935 | +4,340 |
| p06b | `decode_native_tx_compact_v1` + `decode_native_tx_witness_set_compact` | 4,768 | +1,419 |
| p39 | raw finish splice (30 items, stage byte) | 5,741 | +2,392 |

Dominator: the two generic calls (`script_sources_control_from_witness`,
`exact_script_sources_control`) — ≈ 26.5 KB of the 31.8 KB — used only to
prove the witness is canonical, which the canonical-by-induction argument
(non-output plan §4.1) and the head check already establish.

## 3. Options considered

- **Prune (1) — chosen:** drop the two generic calls; keep everything else
  the raw function already does, expressed over `open_frame_v1`.
- Yield / chain / redesign: not applicable.

## 4. Chosen design

Single validator, same file/slot/parameters/redeemer. New library entry
`verify_script_sources_stage_one_finish_semantics_v2` replaces the `_raw_v1`
body:

```aiken
let frame = open_frame_v1(pre, transition, 30, 1)          // canonical head, commitments, stage byte
let witness_set = decode_native_tx_witness_set_compact(frame.witness_set_compact_cbor)
let redeemer_commitment = witness_set.redeemer_tx_wits_hash
let redeemer_count = item_int_v1(frame, 12); let redeemer_peaks = item_frontier_v1(frame, 13)
let redeemer_total_count = item_int_v1(frame, 26)
and {
  script_sources_stage_one_raw_common_control_is_initial(items[8], items[10], items[14], items[17], items[18], items[20], items[21], items[24], items[25]),  // existing fn, unchanged
  bytearray.length(redeemer_commitment) == 32, bytearray.length(item_bytes_v1(frame, 29)) == 32,
  redeemer_total_count <= max_tx_size_derived_collection_item_count,
  if redeemer_commitment == empty_field_commitment { redeemer_count == 0 && redeemer_total_count == 0 && redeemer_peaks == [] }
  else { redeemer_total_count > 0 && redeemer_count == redeemer_total_count && frontier_is_well_formed(redeemer_count, redeemer_peaks) },
  successor_is_exact_v1(pre, transition, replace_stage_v1(frame, transition.work_witness_cbor, 2)),
}
```

The 30-item shape enforces `pending_source_cbor == ""`. Security: no yields;
successor re-derived; the dropped `exact_script_sources_control` equality is
replaced by the head check plus induction (the RF-021 envelope binder makes
the identical argument for the same stage-1 witness).

## 5. Size and budget projection

**≈ 7,600 bytes**: p39 5,741 + witness-set decode ≈ 1,000 + frontier checks
and `script_sources_stage_one_raw_common_control_is_initial` ≈ 900. Tier 0
fee ≤ 0.12 ADA (today ≈ 0.50 ADA). One execution; ExUnits below today's.

## 6. Off-chain work

Deployment entry `validationTraceDisputeScriptSourcesStageOneFinishSemantic`,
`require…ReferenceScriptUtxo`, funding row, inspection name. `contracts.ts`
and `submit.ts` unchanged; drop the `oversized` path.

## 7. Emulator scenario tests

In `tests/submit-init-emulator-script-sources-stage-one-v1.test.ts` (stage-one
redeemer plan §7): publication fit; positive finish after the last redeemer
item and for the empty-redeemer transaction; valid-block negative;
cancel/resume; maximum shape: `redeemer_total_count = max_tx_size_derived_collection_item_count`.

## 8. Aiken tests

`script-sources-stage-one-begin-v1.test.ak` (shared file with the begin
resolver): `stage_one_finish_wire_layout_is_pinned`,
`prepare_routes_stage_one_finish_to_slot_fourteen`, `finish_wins_the_complete_scan`,
`finish_wins_the_empty_redeemer_set`, `finish_refuses_an_incomplete_scan`,
`finish_refuses_a_pending_item`, `finish_splice_equals_exact_encoder`
(property against `exact_script_sources_control({..c, stage: 2})`); keep
`script_sources_stage_one_finish_raw_binds_canonical_successor_encoding`
(`validation-machine-v1.test.ak:7422`) pointed at the v2 entry.

## 9. Verification commands

```bash
cd onchain/aiken && aiken build --env testnet
node -e 'const b=require("./plutus.json");const v=b.validators.find(v=>v.title==="fraud_proofs/validation_trace/script_sources_stage_one_finish_semantic_v1.main.spend");const n=Buffer.from(v.compiledCode,"hex").length;console.log(n,n<=15000?"OK":"OVER")'   # expected ≈ 7600 OK
aiken check -m stage_one_finish   # expected: ≥ 8 tests, 0 failures
```

## 10. Ordering and dependencies

Raw stage-frame library; lands with the stage-one redeemer plan (same test
file and journey).

## 11. Risks

Low; the projection is a sum of measured parts, not a measured whole
(re-measure at implementation).
