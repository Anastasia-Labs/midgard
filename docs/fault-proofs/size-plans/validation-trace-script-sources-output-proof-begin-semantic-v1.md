# Size-fit plan: `script_sources_output_proof_begin_semantic_v1`

Cites [00-primer.md](00-primer.md) and the shared raw stage-frame library
([non-output plan](validation-trace-script-sources-non-output-semantic-v1.md) §4.1).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_output_proof_begin_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-output-proof-begin-semantic-v1.ak` (96 lines) |
| Raw size | **37,945 bytes** |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / index | `ScriptSources` (8), semantic slot **1** of 29, global index 33 |
| Library entry point | `verify_script_sources_output_proof_begin_semantics_v1(pre, transition, output_index, total_length, item_commitment, siblings)` → generic parse/bind, `stage == 5`, `output_cursor < output_count`, `script_sources_output_proof_begin` (`output_proof == None`, `output_index == output_cursor`, `validation_merkle_v1.verify_membership(output_count, output_peaks, output_index, output_item_leaf_hash, siblings)`, successor with `output_proof = Some(ledger_output_proof_v1.initial_control_v1(…))`) |
| Redeemer | `VerifyOutputProofBegin { input_index, output_index, transition, ledger_output_index, total_length, item_commitment, siblings }`; auxiliary hashed as `LedgerOutputProofBeginWitness` (constructor 31) |
| Role / deployment entry today | none / none (`submit.ts` index 1: `ledgerOutputProofBegin` shape, flattened fields) |

## 2. Why it is this size

| Probe | Adds | Raw bytes | Delta |
| --- | --- | ---: | ---: |
| p03 / p04 | generic parse / bind | 25,595 / 35,851 | +22,246 / +10,256 |
| p47 | `validation_merkle_v1.verify_membership` | 4,199 | +850 |
| p21 | deployed predicate on the typed control | 32,209 | — |
| p40 | raw 30→31 frame + membership + successor built with `encode_control_v1(initial_control_v1(…))` | 21,427 | +18,078 (the LOP encoder reaches every sub-control encoder and `control_is_well_formed`) |
| p40b | same, initial control bytes hand-assembled except `ledger_output_scan_v1.encode_control_v1(initial_control_v1())` | 8,974 | +5,625 |
| p40c | same with the scan bytes as a **pinned literal** | **6,883** | +3,534 |

Dominators: generic parse/bind (32.5 KB) and, if kept, the LOP control
encoder (~14 KB) used only to encode a constant-shaped initial control.

## 3. Options considered

- **Prune (1) — chosen.** The raw frame removes parse/bind; a template
  encoder for the initial LOP control (12-item array: version 1, stage 0,
  index, length, commitment, pinned initial-scan literal, `d87a80`, `d87a80`,
  `00`, empty frontier, `d87a80`, `d87a80`) removes the encoder. Measured
  6,883 bytes end to end (p40c).
- **Yield split (2):** unnecessary at 6.9 KB.
- **Chaining (3) / redesign (4):** not applicable.

## 4. Chosen design

Single validator, same file, same slot, same parameters. Redeemer unchanged
on the wire (`VerifyOutputProofBegin` fields as today). Predicate:

```aiken
let frame = open_frame_v1(pre, transition, 30, 5)
let output_cursor = item_int_v1(frame, 20)
let output_count = item_int_v1(frame, 21)
let output_peaks = item_frontier_v1(frame, 22)
and {
  ledger_output_index == output_cursor, output_cursor < output_count,
  total_length > 0, bytearray.length(item_commitment) == 32,
  validation_merkle_v1.verify_membership(output_count, output_peaks, ledger_output_index,
    script_proof_v1.output_item_leaf_hash(ledger_output_index, item_commitment), siblings),
  successor_is_exact_v1(pre, transition,
    append_extension_v1(transition.work_witness_cbor,
      ledger_output_proof_v1.initial_control_cbor_v1(ledger_output_index, total_length, item_commitment))),
}
```

`ledger_output_proof_v1.initial_control_cbor_v1` is the new template encoder
(uses `script_sources_raw_frame_v1.initial_output_scan_cbor`), pinned equal
to `encode_control_v1(initial_control_v1(…))` by a golden test. The
`output_proof == None` precondition is the frame's 30-item shape. Security:
no yields, no new trust — the membership proof is checked as today and the
successor is re-derived; canonical-by-induction (non-output plan §4.1)
covers the copied 30 items.

## 5. Size and budget projection

**≈ 6,900 bytes** (p40c measured 6,883; `script_proof_v1.output_item_leaf_hash`
replaces the probe's stand-in hash, ±100 B). Referenced per transaction:
6.9 KB (today 37,945) — tier 0, fee ≤ 0.10 ADA (today ≈ 0.61 ADA). ExUnits:
one execution, fewer than today (no typed decode); unmeasured for resolver 8,
measured in the shared journey (§7).

## 6. Off-chain work

Nothing exists today. Add deployment entry
`validationTraceDisputeScriptSourcesOutputProofBeginSemantic` and its
`require…ReferenceScriptUtxo`, a funding row, and the inspection-fixture name;
`contracts.ts` title and parameters unchanged; `submit.ts` redeemer encoding
unchanged; no role, no withdrawal, no codec change. Remove the `oversized`
publication path for it in `dispute-scenario.ts`.

## 7. Emulator scenario tests

In `tests/submit-init-emulator-script-sources-output-proof-v1.test.ts` (step
plan §7): publication fit without `oversized`; positive begin step at
`output_cursor = 0` and at the last output; valid-block negative (forged
successor); cancel/resume; maximum shape: `output_count =
max_tx_size_derived_collection_item_count` with the deepest membership path.

## 8. Aiken tests

- `lib/midgard/ledger-output-proof-v1.test.ak`: golden
  `initial_control_cbor_template_equals_typed_encoder` (fuzz over index /
  length / commitment).
- `validators/fraud-proofs/validation-trace/script-sources-output-proof-v1.test.ak`:
  `output_proof_begin_wire_layout_is_pinned`, `prepare_routes_output_proof_begin_to_slot_one`,
  `begin_wins_the_first_output`, `begin_refuses_a_wrong_cursor`,
  `begin_refuses_a_foreign_membership_proof`, `begin_refuses_a_31_item_witness`,
  `begin_refuses_a_non_canonical_head` — reuse the vectors of
  `script_sources_begins_an_authenticated_output_proof` (`validation-machine-v1.test.ak:6190`).

## 9. Verification commands

```bash
cd onchain/aiken && aiken build --env testnet
node -e 'const b=require("./plutus.json");const v=b.validators.find(v=>v.title==="fraud_proofs/validation_trace/script_sources_output_proof_begin_semantic_v1.main.spend");const n=Buffer.from(v.compiledCode,"hex").length;console.log(n,n<=15000?"OK":"OVER")'   # expected ≈ 6900 OK
aiken check -m script_sources_output_proof        # expected: ≥ 7 new tests, 0 failures
```

## 10. Ordering and dependencies

Depends on the raw stage-frame library and the `initial_control_cbor_v1`
template added by the step plan; lands in the same regeneration as the rest
of stage 5.

## 11. Risks

Low. The only new construct is the pinned initial-scan literal; its golden
test is the guard. ABI unchanged.
