# Size-fit plan: `script_sources_output_proof_finalize_semantic_v1`

Cites [00-primer.md](00-primer.md), the shared raw stage-frame library and
yield handshake ([non-output plan](validation-trace-script-sources-non-output-semantic-v1.md)
§4.1–4.2) and the LOP yield family ([output-proof-step plan](validation-trace-script-sources-output-proof-step-semantic-v1.md) §4.2).

## 1. Identity

| Field                         | Value                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| ----------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title               | `fraud_proofs/validation_trace/script_sources_output_proof_finalize_semantic_v1.main.spend`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| File                          | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-output-proof-finalize-semantic-v1.ak` (88 lines)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Raw size                      | **47,187 bytes**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| Applied parameters            | `award_script_hash`, `computation_thread_policy_id`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Phase / index                 | `ScriptSources` (8), semantic slot **3** of 29, global index 35                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Library entry point           | `verify_script_sources_output_proof_finalize_semantics_v1(pre, transition, descriptor_cbor, signer_proof)` → generic parse/bind + `terminal_is_exact_v1(output_proof)` + `script_sources_output_proof_finalize` (`ledger_output_commitment_v1.decode`, `descriptor_is_exact_v1`, `decode_canonical_address_bytes`, `decode_validation_context` network check → `reject_network_id_mismatch`, `protected_output_authorization` → `reject_missing_required_witness`, successor with `output_cursor + 1`, `receive_source_successor` on `receive_scan` with appended `output_descriptor_leaf_hash`, `output_proof: None`) |
| Redeemer                      | `VerifyOutputProofFinalize { input_index, output_index, transition, descriptor_cbor: ByteArray, signer_proof: SignerSetProofV1 }`; auxiliary hashed as `LedgerOutputProofFinalizeWitness { descriptor_cbor, signer_proof }` (constructor 33)                                                                                                                                                                                                                                                                                                                                                                           |
| Role / deployment entry today | none / none (title only; `submit.ts` index 3 flattens `[…base, descriptor_cbor, signer_proof]`)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |

## 2. Why it is this size

| Probe     | Adds                                                                                                                                                                                                                     |       Raw bytes |             Delta |
| --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | --------------: | ----------------: |
| p03 / p04 | generic parse / + `script_sources_control_is_bound`                                                                                                                                                                      | 25,595 / 35,851 | +22,246 / +10,256 |
| p09       | LOP control codec                                                                                                                                                                                                        |          21,801 |           +18,452 |
| p17       | p09 + `descriptor_is_exact_v1` + `ledger_output_commitment_v1.decode`                                                                                                                                                    |          29,144 |            +7,343 |
| c07       | `ledger_output_commitment_v1.decode` alone                                                                                                                                                                               |           6,440 |            +3,091 |
| c08       | `ledger_output_v1.decode_canonical_address_bytes`                                                                                                                                                                        |           3,798 |              +449 |
| p18       | `protected_output_authorization` (signer-set membership)                                                                                                                                                                 |           6,221 |            +2,872 |
| p19       | `decode_validation_context`                                                                                                                                                                                              |           4,736 |            +1,387 |
| p23       | the deployed predicate on the typed control                                                                                                                                                                              |          41,344 |                 — |
| p43       | **raw finalize dispatcher**: 31-item frame, descriptor decode, address, network check, protected auth, `receive_source_successor`, three-region splice (`output_cursor`, `receive_scan`, drop extension), successor hash |      **11,749** |                 — |
| y20       | yield: raw LOP items + scan codec + descriptor decode; checks version/index/length/commitment/address/lovelace/asset count/asset frontier/value size                                                                     |       **7,033** |                 — |
| y21       | yield: scan + blake codecs; reference-script language/digest                                                                                                                                                             |       **7,766** |                 — |
| y22       | yield: traverse codec; terminal datum `result` summary equals claimed                                                                                                                                                    |       **8,838** |                 — |
| y23       | yield: value codec; terminal value control equals claimed                                                                                                                                                                |       **3,996** |                 — |
| y17       | yield with the full typed `descriptor_is_exact_v1`                                                                                                                                                                       |          26,852 |                 — |

Dominators: generic ScriptSources parse/bind (32.5 KB, of which the LOP codec
is 18.5 KB) and `descriptor_is_exact_v1` (7.3 KB, needing every sub-control).

## 3. Options considered

- **Prune (1):** the raw dispatcher (p43) removes the generic parse/bind and
  the successor re-encode, but descriptor exactness still needs the terminal
  LOP control decoded: 11,749 + 18,452 + 7,343 > target. Applied, not sufficient.
- **Yield split (2) — chosen:** raw dispatcher keeps everything that does not
  need the LOP control (network, protected-output authorization, receive-scan
  successor, splice); descriptor exactness moves into four small yields, each
  decoding one sub-control (y20–y23 measured 4.0–8.8 KB). The composite
  summaries (`cardano_tx_out`, `midgard_tx_out`, `cardano_spend_datum`) are
  rebuilt in the scan-facts yield from leaf summaries that the datum and
  value yields attest against prover-claimed copies in the dispatcher redeemer.
- **Multi-transaction chaining (3) — fallback only:** hop A "terminal facts"
  (the four descriptor yields) → hop B (auth + successor). Chosen only if the
  five-execution single transaction misses the 13.2 M memory basis in §7.
- **Redesign (4):** not warranted.

## 4. Chosen design

### 4.1 Dispatcher (rewritten `…-output-proof-finalize-semantic-v1.ak`)

```aiken
pub type ActionV1 {
  VerifyOutputProofFinalize {
    input_index: Int, output_index: Int,
    transition: ValidationOneStepWitnessV1,
    descriptor_cbor: ByteArray,
    signer_proof: SignerSetProofV1,
    claimed_value_summary: Data,          // attested by V1VtLopDescValueSummaryYield
    claimed_datum_summary: Data,          // attested by V1VtLopDescDatumSummaryYield
    output_cursor_offset: Int, receive_scan_offset: Int,   // splice hints, verified
    yield_ref_input_indices: List<Int>,   // exactly four, in role-table order
  }
}
validator main(award_script_hash, computation_thread_policy_id, reference_script_auth_policy_id: PolicyId)
```

Predicate (measured p43 shape): `frame = open_frame_v1(pre, transition, 31, 5)`;
`control_cbor = item_bytes_v1(frame, 30)`; `descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)`;
`descriptor.output_index == item_int_v1(frame, 20)`; `address =
decode_canonical_address_bytes(descriptor.address)`; `context = decode_validation_context(frame.context_cbor)`;
if `address.network_id != context.expected_network_id` → require
`rejected_successor_is_exact(pre, claimed_successor, reject_network_id_mismatch)`;
else `protected_output_authorization(address, item_int(6), item_bytes(7), signer_proof)`:
`InputSignerMissing` → `reject_missing_required_witness` terminal;
`InputSignerProofMalformed` → False; `InputSignerAuthorized` → successor =
`header(30) ++ witness[2..off_cursor) ++ cbor(cursor+1) ++ witness[..off_receive) ++
encode_receive_purpose_scan_control(receive_source_successor(receive' , address)) ++
witness[off_receive_end..off_control)` where `receive'` appends
`output_descriptor_leaf_hash(cursor, descriptor_cbor)` to `descriptor_peaks`
(decoded from `items[24]` with a typed `expect`), each old region verified by
re-encoding, then `successor_is_exact_v1`. In every branch the dispatcher
also requires all four yields:
`require_semantic_yield_v1(tx, policy, role_k, yield_ref_input_indices[k])`
for `k in 0..3`, so descriptor exactness is proven even on rejecting terminals
(the monolith checked `descriptor_is_exact_v1` before either rejection).

### 4.2 Descriptor yields (new `validators/fraud-proofs/validation-trace/ledger-output-descriptor-*-yield-v1.ak`)

Parameter `dispatcher_script_hashes: List<ScriptHash>` (this dispatcher and
`resolve_inputs_membership_finalize_semantic_v1`, whose plan reuses them).
Each reads `(control_cbor, descriptor_cbor, claimed_*)` from the dispatcher
redeemer through `unique_semantic_dispatch_v1`, checks `un_i_data(items[1]) == stage_terminal (6)`, then:

| idx | Role                           | Checks                                                                                                                                                                                                                                                                                          | Basis                                                                        |    Projected |
| --: | ------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- | -----------: |
|   0 | `V1VtLopDescScanFactsYield`    | y20 set **plus** the three composite summaries: `cardano_tx_out_summary_v1`, `midgard_tx_out_summary_v1`, `cardano_spend_datum_summary_v1` rebuilt from `scan` fields + `claimed_value_summary` + `claimed_datum_summary` (library variants taking leaf summaries instead of the whole control) | y20 7,033 + summary builders (`cek_data_v1` constr/list summaries, ≈ 3–4 KB) | **≈ 11,000** |
|   1 | `V1VtLopDescRefScriptYield`    | y21 set plus `reference_script_total_length == total_length − reference_script_item_offset` and `reference_script_item_commitment == reference_script_item_commitment_v1` (narrow variant over scan + `reference_script_peaks`)                                                                 | y21 7,766 + ≈ 1 KB                                                           |      ≈ 8,800 |
|   2 | `V1VtLopDescDatumSummaryYield` | y22: `datum` control terminal, `result == claimed_datum_summary` (or `None` when the output has no datum)                                                                                                                                                                                       | y22 measured                                                                 |    **8,838** |
|   3 | `V1VtLopDescValueSummaryYield` | y23 plus `value_summary_v1` narrow variant equals `claimed_value_summary`                                                                                                                                                                                                                       | y23 3,996 + ≈ 1.5 KB                                                         |      ≈ 5,500 |

**Security argument.** Uniqueness/role authentication as in the non-output
plan; the dispatcher requires **all four** roles (no role index in the
redeemer, only reference-input positions), so omission of any one fails
`require_authenticated_zero_yield`. _Cross-arm substitution:_ role names are
fixed per position; a value yield at the datum position fails the role check.
_Leaf-summary channel:_ `claimed_value_summary`/`claimed_datum_summary` are
prover-supplied but each is pinned by its yield against the terminal
sub-control, and the composite yield consumes only pinned leaves; a forged
leaf fails at its yield, a forged composite fails at the scan-facts yield, so
`descriptor_is_exact_v1`'s conjunction is reconstituted exactly. _Successor
re-derivation:_ dispatcher recomputes `work_root` from the splice; the
address used for `receive_source_successor` is the descriptor's, which the
scan-facts yield pins to `scan.address`. _What an attacker gains from an
omitted yield at deployment:_ no finalize is provable (liveness only).

## 5. Size and budget projection

Dispatcher **≈ 12,200** (p43 11,749 + four-role loop + rejection arms ~400).
Yields ≈ 11,000 / 8,800 / 8,838 / 5,500. Per transaction: **≈ 46 KB
referenced** (today 47,187) — tier 1: fee ≈ 384,000 + 20,400 × 18 ≈ 0.75 ADA
(today ≈ 0.77 ADA); unchanged fee, but now publishable. ExUnits: five
executions parse the dispatcher redeemer (descriptor ≤ ~400 B, terminal LOP
control ≤ ~2 KB, signer proof) — the largest aggregate in this group; §7
measures against 13,200,000 memory and falls back to the two-hop chain (§3)
if it misses. Method: p43/y20–y23 measured; composite-summary and narrow
variants projected from `cek_data_v1` summary sizes.

## 6. Off-chain work

Nothing exists today for this contract.

- `contracts.ts`: `ledgerOutputDescriptorYields` (4) applied with both
  dispatcher hashes; finalize resolver gains `reference_script_auth_policy_id`.
- Roles: four `V1VtLopDesc…Yield` names in both token-name vocabularies; Aiken constants.
- Deployment entries `validationTraceDisputeScriptSourcesOutputProofFinalizeSemantic`,
  `validationTraceDisputeLopDesc{ScanFacts,RefScript,DatumSummary,ValueSummary}Yield`;
  four reward-account registrations.
- Submit route: `semanticActionFieldsV1` (resolver 8, index 3) emits
  `[…base, descriptor_cbor, signer_proof, claimed_value_summary, claimed_datum_summary, output_cursor_offset, receive_scan_offset, yield_ref_input_indices]`;
  builder reads four yield UTxOs and adds four zero withdrawals. New pure
  helpers `ledgerOutputDescriptorLeafSummariesV1(controlCbor)` and
  `scriptSourcesFinalizeSpliceOffsetsV1(workWitnessCbor)` in
  `demo/midgard-validation` (it already decodes the control to build descriptors).
- Inspection fixtures, funding rows (five publications, four deposits), codec
  (`resolverHints.claimedSummaries`, `resolverHints.itemOffsets`).

## 7. Emulator scenario tests

Covered by `tests/submit-init-emulator-script-sources-output-proof-v1.test.ts`
(step plan §7) with the finalize-specific cases: positive finalize of a
protected script-address output with `SignerMembershipProof`; unprotected
output with `NoSignerSetProof`; `reject_network_id_mismatch` terminal;
`reject_missing_required_witness` terminal; valid-block negative at the
finalize frontier; cancel/resume; maximum shape (output with maximum assets
and a nested datum, descriptor at its largest encoding). Assert the finalize
transaction's `exUnits.mem ≤ 13,200,000` — this is the gate for §3's fallback.

## 8. Aiken tests

- `lib/midgard/ledger-output-proof-v1.test.ak`: property
  `descriptor_is_exact_is_the_conjunction_of_the_four_yield_predicates` on
  fuzzed terminal controls and descriptors (both accepting and each
  single-field mutation), and `composite_summaries_from_pinned_leaves_equal_typed_summaries`.
- `validators/fraud-proofs/validation-trace/ledger-output-descriptor-yields-v1.test.ak`:
  `output_proof_finalize_wire_layout_is_pinned`, `prepare_routes_output_proof_finalize_to_slot_three`,
  `finalize_wins_with_four_yields`, `finalize_refuses_three_yields`,
  `finalize_refuses_role_permutation`, `finalize_refuses_forged_leaf_summary`,
  `finalize_refuses_forged_receive_splice_offset`, `finalize_emits_network_id_terminal`,
  `finalize_emits_missing_witness_terminal`, `yield_refuses_non_terminal_control`,
  `yield_refuses_two_dispatcher_inputs`.

## 9. Verification commands

```bash
cd onchain/aiken && aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/(script_sources_output_proof_finalize_semantic_v1\.main\.spend|ledger_output_descriptor_.*_yield_v1\.main\.withdraw)$/.test(v.title)){const n=Buffer.from(v.compiledCode,"hex").length;console.log(v.title,n,n<=15000?"OK":"OVER")}'
# expected: 5 titles, all OK
aiken check -m ledger_output_descriptor_yields   # expected: ≥ 11 tests, 0 failures
cd ../../demo && pnpm --filter @al-ft/midgard-fault-proofs test -- tests/submit-init-emulator-script-sources-output-proof-v1.test.ts tests/validation-dispute-submit.test.ts tests/semantic-resolver-arity-gate.test.ts
```

## 10. Ordering and dependencies

Lands with the output-proof-step plan (shared library changes in
`ledger-output-proof-v1.ak`) and the resolve-inputs membership-finalize plan
(shares the four descriptor yields; parameter lists both dispatchers); depends
on the raw stage-frame library. One regeneration, one catalogue re-pin.

### 10.x Reconciliation notes from the resolve-inputs membership-finalize plan (2026-09-02)

- Both dispatcher redeemers (this plan's and resolve-inputs membership
  finalize's) must carry `control_cbor` explicitly; the descriptor yields
  cannot recover it from a carrier witness without that carrier's frame
  codec.
- The resolve-inputs finalize dispatcher measured 11,153 bytes with all four
  descriptor-yield role checks, so the four-yield design is confirmed on
  that side; the two-hop chain remains its ExUnits fallback.

## 11. Risks

- **Aggregate ExUnits** of five executions is the highest in the group and
  unmeasured; fallback is the two-hop chain (adds one transaction per output
  finalize, bounded by `output_total_count ≤ max_tx_size_derived_collection_item_count`,
  well inside C52).
- **Composite-summary rebuild** duplicates `cardano_tx_out_summary_v1` logic
  in a leaf-taking variant; the §8 property test is the guard against drift.
- **Fee unchanged** (~0.75 ADA): this plan fixes publishability, not fee; a
  future prune of the dispatcher (moving protected-output auth into the
  scan-facts yield) would drop it to tier 0.
- ABI churn: redeemer gains five fields; wire-layout pins regenerate once.
