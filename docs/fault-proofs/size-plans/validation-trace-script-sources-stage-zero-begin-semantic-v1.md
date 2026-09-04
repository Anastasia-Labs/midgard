# Size-fit plan: `script_sources_stage_zero_begin_semantic_v1` (and the borderline sibling `script_sources_stage_zero_hash_block_semantic_v1`)

Cites [00-primer.md](00-primer.md) and the shared raw stage-frame library
([non-output plan](validation-trace-script-sources-non-output-semantic-v1.md) §4.1).

## 1. Identity

| Field                         | Begin                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | Hash block (sibling)                                                                                                                                                                                                                                                                                                              |
| ----------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title               | `fraud_proofs/validation_trace/script_sources_stage_zero_begin_semantic_v1.main.spend`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | `…/script_sources_stage_zero_hash_block_semantic_v1.main.spend`                                                                                                                                                                                                                                                                   |
| File                          | `validators/fraud-proofs/validation-trace/script-sources-stage-zero-begin-semantic-v1.ak` (100 lines)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | `…/script-sources-stage-zero-hash-block-semantic-v1.ak` (89 lines)                                                                                                                                                                                                                                                                |
| Raw size                      | **20,004**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | **16,256** (fails once applied: +72 params, +276 wrapper)                                                                                                                                                                                                                                                                         |
| Applied parameters            | `award_script_hash`, `computation_thread_policy_id`, `field_preimage_certificate_policy_id`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | `award_script_hash`, `computation_thread_policy_id`                                                                                                                                                                                                                                                                               |
| Phase / slot / global         | `ScriptSources` (8), slot **5**, global 37                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | slot **7**, global 39                                                                                                                                                                                                                                                                                                             |
| Library entry point           | `verify_script_sources_stage_zero_begin_semantics_v1(pre, transition, door, field_index, item_index, carriage)` (`:8180`): `script_sources_stage_zero_control_from_witness` (typed 30/31-item parse, no extension codecs), `verify_native_tx_proof_source_v1`, `open_machine_field_item(door, …, 6, source_count, carriage)`, `versioned_script_header_v1` (None → `reject_invalid_field_type`), `script_sources_stage_zero_control_is_bound`, `script_sources_stage_zero_begin_successor_is_exact` (re-encodes observer scan + mint fold to locate the suffix; appends `encode_inline_source_hash_control_v1` with `blake2b_224_trace_v1.initial_control_v1`) | `verify_script_sources_stage_zero_hash_block_semantics_v1(pre, transition, chunk_proof, next_chunk_proof)` (`:8266`): typed parse, `inline_source_hash_control_from_cbor`, `inline_source_hash_block_v1` (`bounded_item_v1.verify_chunk`), `blake2b_224_trace_v1.step_v1`, `script_sources_stage_zero_pending_successor_is_exact` |
| Redeemer                      | `VerifyBegin { input_index, output_index, transition, field_index, item_index, carriage: FieldCarriageV1 }`; auxiliary `TransactionFieldChunkWitness` (1)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      | `VerifyHashBlock { …, chunk_proof, next_chunk_proof }`; auxiliary `ScriptSourceHashBlockWitness` (36)                                                                                                                                                                                                                             |
| Role / deployment entry today | none / none (`submit.ts` index 5: `transactionFieldChunk` flattened)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | none / none (index 7: `scriptSourceHashBlock` flattened)                                                                                                                                                                                                                                                                          |

## 2. Why it is this size

| Probe      | Adds                                                                                                                                                                                                                                                              |     Raw bytes |           Delta |
| ---------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------: | --------------: |
| p05        | `script_sources_stage_zero_control_from_witness` + `script_sources_stage_zero_control_is_bound` (typed stage-zero bind: 30-item typed parse 2,024 + record equality against `empty_*_control()` + `inline_source_hash_control_from_cbor` + canonical-shape check) |        10,165 |      **+6,816** |
| p06 / p06b | `verify_native_tx_proof_source_v1` / decode only                                                                                                                                                                                                                  | 5,351 / 4,768 | +2,002 / +1,419 |
| p07 / p08  | + door on field 6 / + `versioned_script_header_v1`                                                                                                                                                                                                                | 8,514 / 9,614 | +3,163 / +1,100 |
| p38        | `inline_source_hash_control_from_cbor` + `inline_source_hash_block_v1` + `blake2b_224_trace_v1.step_v1`                                                                                                                                                           |        10,970 |          +7,621 |
| p51        | **raw stage-zero begin**: 30-item frame, source, door, header, `InlineSourceHashControlV1` encode + blake initial control, suffix located by re-encoding `encode_observer_purpose_scan_control(empty)` / `encode_mint_fold_control(empty)`, 31-item splice        |        15,661 |               — |
| p51b       | p51 with the two empty-control encodings as **pinned literals**                                                                                                                                                                                                   |    **15,104** |            −557 |
| p52        | **raw hash block**: 31-item frame, pending item from `items[30]`, hash block, blake step, `replace_extension_v1`                                                                                                                                                  |    **12,463** |               — |

Dominators (begin): typed stage-zero bind (6.8 KB) versus raw frame (2.4 KB);
native-source verify (2.0 KB, of which ~0.6 KB is the tx-id verification the
door needs only for tier-3 carriage); door (3.2 KB); the two empty-control
encoders (0.6 KB). Hash block: typed bind (6.8 KB) + hash block (7.6 KB).

## 3. Options considered

- **Prune (1) — chosen for both.** Raw frame replaces the typed bind; pinned
  literals replace the empty-control encoders (measured 15,104 for begin,
  12,463 for hash block). Begin still misses the 15,000 target by ~100 bytes,
  so a second prune is required: a door entry that takes the field
  commitment and transaction id directly instead of a `VerifiedMidgardNativeTxCompact`.
- **Yield split (2) — fallback for begin only.** If the door-entry change is
  refused, move `open_machine_field_item` + `versioned_script_header_v1` +
  pending-control construction into one `V1VtSsStage0BeginDoorYield` (≈ 1,663
  - 2,392 + 2,002 + 3,163 + 1,100 + 1,500 ≈ 11.8 KB) and keep a ~6.5 KB
    dispatcher. Costs a role, a reward account and a second execution for a
    100-byte problem; hence fallback.
- Chaining / redesign: not applicable.

## 4. Chosen design

### 4.1 Begin (single validator, same file/slot/parameters/redeemer)

Predicate (p51b shape) with two library changes:

1. `native_tx_field_access_v1.authenticated_whole_field_view_by_commitment(transaction_id, field_index, field_commitment, carriage, reference_inputs, certificate_policy_id)`
   and `field_door.open_machine_field_item_by_commitment(door, transaction_id, field_commitment, field_index, item_index, carriage)`:
   the same tier-1/2/3 authentication, but the field commitment is supplied
   by the caller (`witness_set.script_tx_wits_hash` from
   `decode_native_tx_witness_set_compact`, whose bytes `open_frame_v1` has
   already bound to `pre.transaction_commitment`) and the transaction id is
   `pre.transaction_id` (bound through `structural_transition_is_valid` /
   `machine_state_is_well_formed`). This drops `verify_native_tx_compact_cbor_v1`
   (body decode + id check) from every field-6/8/2/5 narrow resolver.
2. Literals `empty_observer_scan_cbor`, `empty_mint_fold_cbor` from the
   raw-frame library (golden-pinned).

```aiken
let frame = open_frame_v1(pre, transition, 30, 0)
let source_count = item_int_v1(frame, 10); let source_total_count = item_int_v1(frame, 25)
let witness_set = decode_native_tx_witness_set_compact(frame.witness_set_compact_cbor)
let item = open_machine_field_item_by_commitment(door, pre.transaction_id, witness_set.script_tx_wits_hash, 6, source_count, carriage)
… active_total_count, item_length, header (None → rejected_successor_is_exact(pre, claimed_successor, reject_invalid_field_type)) …
let pending = encode_inline_source_hash_control_v1(InlineSourceHashControlV1 { version: 1, source_index: source_count, source_total_count: active_total_count, language_tag, payload_offset, payload_length, item_length, item_commitment, hash_control: blake2b_224_trace_v1.initial_control_v1(payload_length + 1) })
let old_suffix = cbor(source_total_count) ++ cbor(item_int(26)) ++ empty_observer_scan_cbor ++ empty_mint_fold_cbor ++ bytes(item_bytes(29))
successor = header(31) ++ witness[2..suffix_offset) ++ cbor(active_total_count) ++ 00 ++ empty_observer_scan_cbor ++ empty_mint_fold_cbor ++ bytes(schedule) ++ bytes(pending)
```

plus the raw equivalents of `script_sources_stage_zero_control_is_bound`'s
stage-0 conjuncts (`items[8] == []`, `items[12] == 0`, `items[13] == []`,
`items[26] == 0`, `items[18] == 0`, `items[19] == []`, `items[20..23] == 0/[]/0`,
`items[24] == empty receive scan`, `field_index == 6`, `item_index == source_count`,
`item_count == active_total_count`, bounds). The suffix literal check makes
the empty observer/mint-fold requirement exact.

### 4.2 Hash block (single validator, same file/slot/parameters/redeemer)

p52 shape: `frame = open_frame_v1(pre, transition, 31, 0)`; `pending_cbor =
item_bytes_v1(frame, 30)`; `pending = inline_source_hash_control_from_cbor(pending_cbor)`;
`pending.source_index == item_int(10)`, `pending.source_total_count == item_int(25)`;
`block = inline_source_hash_block_v1(pending, chunk_proof, next_chunk_proof)`;
`next = blake2b_224_trace_v1.step_v1(pending.hash_control, Some(block))`;
successor = `replace_extension_v1(witness, bytes(pending_cbor), bytes(encode_inline_source_hash_control_v1({..pending, hash_control: next})))`.
The two remaining stage-zero siblings (`hash_advance` 13,610, `hash_terminal`
12,051) are under the limit today; migrating them to the same raw frame is
optional and recommended for uniformity (each would drop ≈ 4 KB).

Security (both): no yields; the field preimage is authenticated by the door
(tiers 1–3 unchanged), the chunk by `bounded_item_v1.verify_chunk` against
the pending item commitment, the successor re-derived; the induction argument
covers copied items. The commitment-based door entry is not a new trust
assumption: `open_frame_v1`'s `native_tx_proof_commitment_v1` check binds the
witness-set bytes that carry the field commitment, and `pre.transaction_id`
is the id the machine state already carries.

## 5. Size and budget projection

- Begin: p51b 15,104 − (`verify_native_tx_compact_cbor_v1` share ≈ 1,300–1,500) + rejected-successor arm (~400) ≈ **13,900–14,200**. If the door change is refused: fallback dispatcher ≈ 6,500 + yield ≈ 11,800.
- Hash block: **≈ 12,500** (p52 12,463).
- Fees: single reference ≤ 14.2 KB, tier 0 (≤ 0.21 ADA; today 0.30 / 0.24 ADA). One execution each; ExUnits below today's (no typed record equality on `empty_*_control()`).

## 6. Off-chain work

Deployment entries `validationTraceDisputeScriptSourcesStageZeroBeginSemantic`,
`…StageZeroHashBlockSemantic` (and, for uniformity, `…HashAdvance`,
`…HashTerminal`, `…StageZeroFinish`), `require…ReferenceScriptUtxo`, funding
rows, inspection names; `contracts.ts`/`submit.ts` unchanged (redeemer wire
shapes kept); fallback variant would add one role, one reward account and a
withdrawal in the submit route.

## 7. Emulator scenario tests

Add `tests/submit-init-emulator-script-sources-stage-zero-v1.test.ts`:
publication fit for all five stage-zero resolvers without `oversized`;
positive lifecycle through award for begin (tier-1 inline carriage and tier-2
`RawUtxo`), hash block (single-chunk and two-chunk windows), hash advance,
hash terminal and finish on a fixture with two Plutus scripts in field 6;
valid-block negative at the begin frontier; cancel/resume; maximum shape:
a 32,768-byte script item under tier-3 `Certified` carriage
(`max_aggregate_field_preimage_bytes`), and the `reject_invalid_field_type`
terminal for a malformed versioned-script header.

## 8. Aiken tests

- `lib/midgard/native-tx-field-access-v1.test.ak`: `by_commitment_view_equals_verified_view`
  (property over the three carriage tiers) and `by_commitment_view_rejects_a_foreign_commitment`.
- `validators/fraud-proofs/validation-trace/script-sources-stage-zero-v1.test.ak`:
  wire-layout pins for begin and hash block, `prepare_routes_stage_zero_begin_to_slot_five`,
  `prepare_routes_hash_block_to_slot_seven`, `begin_wins_the_first_source`,
  `begin_emits_invalid_field_type_terminal`, `begin_refuses_a_pending_source`,
  `begin_splice_equals_exact_encoder` (property vs `exact_script_sources_control`),
  `hash_block_wins_a_two_chunk_window`, `hash_block_refuses_a_mismatched_source_index`,
  `hash_block_splice_equals_exact_encoder`; keep the machine vectors
  `script_sources_stage_zero_begin_initializes_an_authenticated_identity_trace`
  and `…_accepts_an_authenticated_hash_block` (`validation-machine-v1.test.ak:15578, :15678`) as oracles.

## 9. Verification commands

```bash
cd onchain/aiken && aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/script_sources_stage_zero_[a-z_]+_semantic_v1\.main\.spend$/.test(v.title)){const n=Buffer.from(v.compiledCode,"hex").length;console.log(v.title,n,n<=15000?"OK":"OVER")}'
# expected: 5 titles, all OK (begin is the gate)
aiken check -m script_sources_stage_zero   # expected: existing 5 machine vectors + ≥ 10 new, 0 failures
aiken check -m native_tx_field_access      # expected: existing suite + 2, 0 failures
cd ../../demo && pnpm --filter @al-ft/midgard-fault-proofs test -- tests/submit-init-emulator-script-sources-stage-zero-v1.test.ts tests/semantic-resolver-arity-gate.test.ts
```

## 10. Ordering and dependencies

The commitment-based door entry benefits every door-using narrow resolver in
the validation-trace family (phase-A, resolve-inputs, script-sources stages
1/4/6/7, canonical decode) and should land once, in this regeneration, with
the raw stage-frame library.

## 11. Risks

- **Begin is the tightest script in the group** (15,104 measured before the
  door prune; ≈ 14 KB after). The §9 size gate decides between the prune and
  the fallback yield.
- The door API change touches `native-tx-field-access-v1.ak` (1,294 lines,
  golden tests `native-tx-field-access-v1-golden.test.ak`); the by-commitment
  view must be proven equal to the verified view for all three tiers.
- ABI unchanged for both scripts.
