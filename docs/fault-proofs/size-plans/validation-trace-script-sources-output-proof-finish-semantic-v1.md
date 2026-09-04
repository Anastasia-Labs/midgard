# Size-fit plan: `script_sources_output_proof_finish_semantic_v1`

Cites [00-primer.md](00-primer.md) and the shared raw stage-frame library
([non-output plan](validation-trace-script-sources-non-output-semantic-v1.md) §4.1).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_output_proof_finish_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-output-proof-finish-semantic-v1.ak` (79 lines) |
| Raw size | **36,546 bytes** |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / index | `ScriptSources` (8), semantic slot **4** of 29, global index 36 |
| Library entry point | `verify_script_sources_stage_five_finish_semantics_v1(pre, transition)` → generic parse/bind, `stage == 5`, `output_cursor == output_count`, successor = `encode_script_sources_witness(… stage 6 …)` |
| Redeemer | `VerifyOutputProofFinish { input_index, output_index, transition }`; auxiliary `NoAuxiliaryWitness` |
| Role / deployment entry today | none / none (`submit.ts` index 4: `none` shape, `base` fields) |

## 2. Why it is this size

| Probe | Adds | Raw bytes | Delta |
| --- | --- | ---: | ---: |
| p03 / p04 | generic parse / bind (LOP + discovery codecs reachable) | 25,595 / 35,851 | +22,246 / +10,256 |
| p44 | base 30-item parse + base encoder | 8,001 | +4,652 |
| p39 | **raw finish**: 30-item frame, stage byte 5 → 6, everything else copied, `hash_work_witness` successor | **5,741** | +2,392 |

Dominator: generic parse/bind (32.5 KB) for a step whose only output is a
one-byte change.

## 3. Options considered

- **Prune (1) — chosen:** raw frame + `replace_stage_v1`. Measured 5,741.
- Yield / chain / redesign: not applicable.

## 4. Chosen design

Single validator, same file/slot/parameters/redeemer. Predicate:

```aiken
let frame = open_frame_v1(pre, transition, 30, 5)      // 30 items ⇒ output_proof == None
and {
  item_int_v1(frame, 20) == item_int_v1(frame, 21),   // output_cursor == output_count
  successor_is_exact_v1(pre, transition, replace_stage_v1(frame, transition.work_witness_cbor, 6)),
}
```

The monolith additionally re-validated the whole control
(`script_sources_control_is_bound`: frontier well-formedness, receive/observer/
mint-fold well-formedness, `output_total_count == output_count`); those facts
were established when the predecessor step produced these bytes
(canonical-by-induction, non-output plan §4.1) and the stage-6 consumer
re-checks what it needs. Security: no yields; successor re-derived.

## 5. Size and budget projection

**≈ 5,800 bytes** (p39 measured 5,741). Tier 0 fee ≤ 0.09 ADA (today
≈ 0.58 ADA). One execution; ExUnits far below today's (no typed decode).

## 6. Off-chain work

Deployment entry `validationTraceDisputeScriptSourcesOutputProofFinishSemantic`,
`require…ReferenceScriptUtxo`, funding row, inspection name; `contracts.ts`
and `submit.ts` unchanged; drop the `oversized` path.

## 7. Emulator scenario tests

In the shared stage-5 journey: publication fit; positive finish after the
last output; valid-block negative; cancel/resume; the zero-output shape
(`output_count == 0`, finish immediately after the stage-4 finish).

## 8. Aiken tests

`script-sources-output-proof-v1.test.ak`: `output_proof_finish_wire_layout_is_pinned`,
`prepare_routes_output_proof_finish_to_slot_four`, `finish_wins_after_the_last_output`,
`finish_refuses_a_pending_output_proof` (31-item witness),
`finish_refuses_an_incomplete_cursor`, `finish_splice_equals_exact_encoder`
(property: `replace_stage_v1(…, 6)` == `exact_script_sources_control({..control, stage: 6})` on fuzzed stage-5 controls).

## 9. Verification commands

```bash
cd onchain/aiken && aiken build --env testnet
node -e 'const b=require("./plutus.json");const v=b.validators.find(v=>v.title==="fraud_proofs/validation_trace/script_sources_output_proof_finish_semantic_v1.main.spend");const n=Buffer.from(v.compiledCode,"hex").length;console.log(n,n<=15000?"OK":"OVER")'   # expected ≈ 5800 OK
aiken check -m script_sources_output_proof
```

## 10. Ordering and dependencies

Raw stage-frame library; lands with stage 5.

## 11. Risks

Minimal. The dropped re-validation is covered by the induction argument and
the equivalence property test.
