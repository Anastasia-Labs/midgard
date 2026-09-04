# Size-fit plan: `resolve_inputs_membership_finalize_semantic_v1`

Cites [00-primer.md](00-primer.md), the shared narrow control codec of
[membership-step](validation-trace-resolve-inputs-membership-step-semantic-v1.md)
§4.1, and the **shared ledger-output descriptor yield family** owned by
[validation-trace-script-sources-output-proof-finalize-semantic-v1.md](validation-trace-script-sources-output-proof-finalize-semantic-v1.md)
§4.2 (four `V1VtLopDesc…Yield` roles, parameter `dispatcher_script_hashes`
listing both finalize dispatchers) over the semantic-yield handshake of
[validation-trace-script-sources-non-output-semantic-v1.md](validation-trace-script-sources-non-output-semantic-v1.md)
§4.2. The reconciliation with that design is recorded in §10.

## 1. Identity

| Field                                      | Value                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| ------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title                            | `fraud_proofs/validation_trace/resolve_inputs_membership_finalize_semantic_v1.main.spend`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| File                                       | `onchain/aiken/validators/fraud-proofs/validation-trace/resolve-inputs-membership-finalize-semantic-v1.ak`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Raw size                                   | 34,586 bytes (2.11× limit; 2.31× target)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| Applied parameters                         | `award_script_hash`, `computation_thread_policy_id`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| Phase / resolver / semantic / global index | `ResolveInputs` / 7 / 4 of 6 / 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| Auxiliary                                  | `LedgerOutputProofFinalizeWitness { descriptor_cbor, signer_proof: SignerSetProofV1 }` (constructor 33)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Library entry                              | `verify_resolve_inputs_membership_finalize_semantics_v1` → `resolve_inputs_control_is_bound` + `resolve_membership_proof_finalize` (`validation-machine-v1.ak` 6450–6510: `descriptor_cbor == pending.descriptor_cbor`, `descriptor_is_exact_v1(pending.output_proof, descriptor)`, `decode_canonical_address_bytes`, `input_signer_authorization(source_kind, address, signer_count, signer_frontier_commitment, signer_proof)` → `reject_missing_required_witness` / `False` / successor with `cursor + 1`, `resolved_input_accumulator_successor`, `remaining_schedule_hash := pending.next_schedule_hash`, `pending := None`) |
| Role / deployment entry today              | none / none (`contracts.ts` `semantics.resolveInputsMembershipFinalize` only; no `submit.ts` deployment entry, submit route, or funding row)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |

## 2. Why it is this size

Probe copy `/tmp/size-probe-ri`, same procedure as the step plan; the
`ri2_fin` row was measured on `/tmp/size-probe-ri2` (`git archive HEAD` at
`815b703a9`) during the review pass.

| Probe                                                                                                   |                                 Bytes | Isolates                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| ------------------------------------------------------------------------------------------------------- | ------------------------------------: | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `d_membership_finalize`                                                                                 |                                 3,918 | dispatcher shell                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| `p_pred_membership_finalize`                                                                            |                                31,951 | predicate                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| `p_core_finalize` (parse + `resolve_membership_proof_finalize`)                                         |                                29,734 | core                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| `p_lop_decode`                                                                                          |                                18,279 | `decode_control_v1` of the pending output proof (12,546 decode-only, +2,751 wf, +5,234 encode)                                                                                                                                                                                                                                                                                                                                                                     |
| `p_lop_descriptor_exact` − `p_lop_decode`                                                               |                                 7,137 | `descriptor_is_exact_v1` (incl. `ledger_output_commitment_v1.decode`)                                                                                                                                                                                                                                                                                                                                                                                              |
| `p_sum_addr` / `p_sum_datum` / `p_sum_refscript` / `p_sum_txout` / `p_sum_spend_datum` − decode         | 3,198 / 1,800 / 3,008 / 3,816 / 1,487 | summary constructors (`cek_data_v1`)                                                                                                                                                                                                                                                                                                                                                                                                                               |
| `p_refscript_exact` − decode                                                                            |                                 1,796 | `descriptor_reference_script_is_exact_v1` + descriptor decode                                                                                                                                                                                                                                                                                                                                                                                                      |
| `p_signer_auth`                                                                                         |                                 3,107 | `input_signer_authorization` + `decode_canonical_address_bytes` + `SignerSetProofV1` decoder                                                                                                                                                                                                                                                                                                                                                                       |
| `p_encode_resolve_witness`                                                                              |                                15,083 | `encode_resolve_inputs_witness` (pulls `encode_control_v1` 13,699 through the `Some` pending arm even though finalize writes `None`)                                                                                                                                                                                                                                                                                                                               |
| `p_control_bound_narrow`                                                                                |                                 2,990 | narrowed parse + binding                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| `y_facts` / `y_summaries` (earlier two-yield prototype, superseded by the shared four)                  |                        7,231 / 11,163 | measured; corroborate the shared plan's y20–y23 (7,033 / 7,766 / 8,838 / 3,996)                                                                                                                                                                                                                                                                                                                                                                                    |
| `v_membership_finalize_dispatch` (earlier prototype with two role checks and claim binding, superseded) |                                12,869 | measured                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| **`ri2_fin` (§4 dispatcher, production shape)**                                                         |                            **11,153** | three parameters; raw 11-item + nested 5-item parse; every `resolve_inputs_control_is_bound` clause except LOP well-formedness; `descriptor_cbor == pending.descriptor_cbor`; `control_cbor` pin; **four** `require_authenticated_zero_yield` calls over `yield_ref_input_indices`; `decode_canonical_address_bytes`; `input_signer_authorization` with the three-way mapping; `resolved_input_accumulator_successor`; successor re-encoded with `pending = #"00"` |

Dominators: the pending output-proof codec (18.3 KB, needed once to read the
terminal control) and the successor encoder reaching `encode_control_v1`
(13.7 KB) although finalize always writes `pending = None`; the descriptor
exactness and summaries add ~7 KB; signer authorization 3.1 KB. Everything
carrier-specific (binding, signer authorization, accumulator successor,
four role checks) fits in 11,153 bytes once the LOP codec is kept as bytes.
`aiken check`: `resolve_inputs_finalizes_authenticated_membership_output`
40,060,879 mem / 16,715,409,206 cpu (fixture-inclusive upper bound).

## 3. Options considered

1. **Prune.** Two exact prunes are kept: the narrow raw control codec
   (`encode_control_raw` with `pending = None` never reaches
   `encode_control_v1`: −13.7 KB) and reading `descriptor_cbor ==
pending.descriptor_cbor` from raw bytes. Insufficient alone: the terminal
   control must still be decoded (18.3 KB) for `descriptor_is_exact_v1`.
2. **Yield split (chosen).** `descriptor_is_exact_v1` compares descriptor
   facts against the terminal control and partitions by sub-control; the
   script-sources output-proof-finalize plan measured four one-sub-control
   yields at 4.0–8.8 KB (y20–y23) for the _same_ `descriptor_is_exact_v1`
   call (`validation_machine_v1` line 9377 vs 6459), so the four are shared
   and this dispatcher keeps only what needs `control.signer_*`: signer
   authorization and the successor. The earlier two-yield draft of this plan
   (`facts`/`summaries`) is withdrawn in favour of the shared four (§10).
3. **Chaining.** Fallback only, mirrored from the shared plan: hop A "terminal
   facts" (the four descriptor yields) → hop B (signer authorization +
   successor), chosen only if the five-execution transaction misses the
   13.2 M memory basis in §7. Rejected as the primary route: the 18.3 KB
   codec would sit in every hop without the yields anyway, and it adds a
   transaction to an atomic trace step.
4. **Redesign.** Not needed.

## 4. Chosen design

### 4.1 Validators

| Validator                                                                                                    | Kind     | Responsibility                                                                                                                                                                                                                                                                                                                                                                                      | Params                                                                                                    | Role     |                                                                                              Size |
| ------------------------------------------------------------------------------------------------------------ | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------- | -------- | ------------------------------------------------------------------------------------------------: |
| `resolve_inputs_membership_finalize_semantic_v1.main.spend` (same title)                                     | spend    | `continue_winning` with `membership_finalize_dispatch`; narrow binding; pins `descriptor_cbor` and `control_cbor`; requires all four descriptor yields; signer authorization; accumulator successor or `reject_missing_required_witness`                                                                                                                                                            | `award_script_hash`, `computation_thread_policy_id`, **`reference_script_auth_policy_id`**                | none     | **11,153 measured** (`ri2_fin`); ≈ 12,450 with the optional scalar re-check of the step plan §4.3 |
| `ledger_output_descriptor_scan_facts_yield_v1.main.withdraw` (**owned by the script-sources finalize plan**) | withdraw | `V1VtLopDescScanFactsYield`: terminal scan control vs descriptor `version`, `output_index`, `total_length`, `item_commitment`, `address`, `lovelace`, `asset_count`, `asset_frontier_commitment`, `cardano_value_size`, plus the three composite summaries (`cardano_tx_out`, `midgard_tx_out`, `cardano_spend_datum`) rebuilt from scan fields + `claimed_value_summary` + `claimed_datum_summary` | `dispatcher_script_hashes` = `[script_sources_output_proof_finalize, resolve_inputs_membership_finalize]` | as named |                                                           ≈ 11,000 (y20 7,033 + summary builders) |
| `ledger_output_descriptor_ref_script_yield_v1.main.withdraw`                                                 | withdraw | `V1VtLopDescRefScriptYield`: reference-script language / digest / total length / item commitment                                                                                                                                                                                                                                                                                                    | same                                                                                                      | as named |                                                                               ≈ 8,800 (y21 7,766) |
| `ledger_output_descriptor_datum_summary_yield_v1.main.withdraw`                                              | withdraw | `V1VtLopDescDatumSummaryYield`: traverse control terminal, `result == claimed_datum_summary` (or `None` for no datum)                                                                                                                                                                                                                                                                               | same                                                                                                      | as named |                                                                                       8,838 (y22) |
| `ledger_output_descriptor_value_summary_yield_v1.main.withdraw`                                              | withdraw | `V1VtLopDescValueSummaryYield`: value control terminal, `value_summary_v1 == claimed_value_summary`                                                                                                                                                                                                                                                                                                 | same                                                                                                      | as named |                                                                               ≈ 5,500 (y23 3,996) |

Together the four yields are exactly the conjunction `descriptor_is_exact_v1`
(shared plan §8 property `descriptor_is_exact_is_the_conjunction_of_the_four_yield_predicates`);
`terminal_is_exact_v1` is covered by the per-sub-control `stage == 6`
checks. No successor control is produced, so no splice or spans are needed.

### 4.2 ABI deltas

```aiken
pub type ActionV1 {
  VerifyMembershipFinalize {
    input_index: Int, output_index: Int,
    transition: ValidationOneStepWitnessV1,
    descriptor_cbor: ByteArray,
    signer_proof: SignerSetProofV1,
    control_cbor: ByteArray,              // terminal LOP control; pinned to pending.output_proof_cbor
    claimed_value_summary: Data,          // attested by V1VtLopDescValueSummaryYield
    claimed_datum_summary: Data,          // attested by V1VtLopDescDatumSummaryYield
    yield_ref_input_indices: List<Int>,   // exactly four, in role-table order
  }
}
validator main(award_script_hash, computation_thread_policy_id, reference_script_auth_policy_id: PolicyId)
```

The fields after `signer_proof` are those of the script-sources finalize
dispatcher (`claimed_value_summary`, `claimed_datum_summary`,
`yield_ref_input_indices`) plus `control_cbor` (§10 D1); the script-sources
splice hints (`output_cursor_offset`, `receive_scan_offset`) are not needed
here because the resolve-inputs successor is re-encoded, not spliced. Datum,
auxiliary (`LedgerOutputProofFinalizeWitness { descriptor_cbor, signer_proof }`,
still typed), evidence hash and every trace byte are unchanged.

### 4.3 Handshake

1. **Dispatcher** `membership_finalize_dispatch(pre, transition, descriptor_cbor, signer_proof, control_cbor, yield_ref_input_indices, policy, tx)` (the `ri2_fin` probe is this function verbatim):
   - `control = control_raw_from_witness(...)`, `expect Some(pending) = control.pending`; `control_raw_is_bound(pre, transition, control)` (the clause list of the step plan §4.3, including the descriptor/input `output_index` equality and the canonical re-encode of the witness); optionally the LOP scalar re-check (step plan §4.3).
   - `descriptor_cbor == pending.descriptor_cbor` (the auxiliary, bound to the evidence hash by `continue_winning`, equals the agreed pending descriptor) and `control_cbor == pending.output_proof_cbor`.
   - `require_semantic_yield_v1(tx, policy, role_k, yield_ref_input_indices[k])` for `k in 0..3` in role-table order (`V1VtLopDescScanFactsYield`, `V1VtLopDescRefScriptYield`, `V1VtLopDescDatumSummaryYield`, `V1VtLopDescValueSummaryYield`), and `yield_ref_input_indices` has exactly four entries. All four are required **before** either rejection branch, because the monolith checked `descriptor_is_exact_v1` before `input_signer_authorization`.
   - `expect Some(address) = decode_canonical_address_bytes(descriptor.address)`; `input_signer_authorization(pending.source_kind, address, control.signer_count, control.signer_frontier_commitment, signer_proof)` mapped exactly as today: `InputSignerMissing → rejected_successor_is_exact(pre, claimed_successor, reject_missing_required_witness)`; `InputSignerProofMalformed → False`; `InputSignerAuthorized → claimed_successor.phase == ResolveInputs && work_root == hash_work_witness(ResolveInputs, pc + 1, encode_control_raw({..control, cursor: cursor + 1, accumulator: resolved_input_accumulator_successor(accumulator, source_kind, key, descriptor_cbor), remaining_schedule_hash: pending.next_schedule_hash, pending: None}))`.
2. **Yields** (shared; script-sources finalize plan §4.2): `unique_semantic_dispatch_v1(dispatcher_script_hashes, tx)` → the one dispatcher input, its datum, and `extra` = `(descriptor_cbor, signer_proof, control_cbor, claimed_value_summary, claimed_datum_summary, yield_ref_input_indices)` read raw; `un_list_data(control_cbor)` into 12 items; `expect un_i_data(items[1]) == 6` (`stage_terminal`); `descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)`; decode only its sub-control with the module `control_from_data_v1`; check its share of `descriptor_is_exact_v1`; the value/datum yields pin their leaf summary to `claimed_*`, the scan-facts yield consumes the pinned leaves to rebuild the composites. A successful withdrawal _is_ the fact; there is no yield output.
3. Output-state re-derivation stays in the dispatcher (`continue_winning`).
4. Parameters: auth policy id in the dispatcher; both dispatcher hashes in every yield; the role table is compiled in.

### 4.4 Security argument

- **Dispatch uniqueness / role authentication / omission** as in the step plan §4.6: one dispatcher input per transaction, exact role NFT, zero withdrawal, unique redeemer; the dispatcher requires **all four** roles at fixed positions, so omitting any one fails `require_authenticated_zero_yield`, and a yield whose parameter does not list this dispatcher cannot find its input.
- **Cross-arm substitution.** Role names are fixed per position, so a value yield at the datum position fails the role check; a LOP _stage_ yield (`V1VtLop…Yield`) presented at any position fails on the role name and could not parse the finalize `extra` anyway. A finalize claim presented to the step dispatcher fails on the role name.
- **Descriptor substitution.** The dispatcher pins `descriptor_cbor` both to `pending.descriptor_cbor` (agreed work witness) and to the evidence hash (auxiliary); the yields read the same `descriptor_cbor` from the dispatcher redeemer through the singleton dispatch, so they judge the same descriptor.
- **Leaf-summary channel.** `claimed_value_summary` / `claimed_datum_summary` are prover-supplied but each is pinned by its yield against the terminal sub-control, and the scan-facts yield consumes only pinned leaves; a forged leaf fails at its yield, a forged composite at the scan-facts yield, so the conjunction `descriptor_is_exact_v1` is reconstituted exactly (shared plan §8 property).
- **Signer facts** stay in the dispatcher because they depend on `control.signer_count` / `signer_frontier_commitment`, which the yields never see; the address they use is the descriptor's, which the scan-facts yield pins to `scan.address`.
- **Output-state re-derivation.** The successor is recomputed from the agreed control bytes, the pinned descriptor and the accumulator successor; nothing in the redeemer except the checked channel values influences it. `InputSignerProofMalformed → False` is preserved, so a malformed proof can never reach either successor.
- **What an attacker gains from an omitted yield at deployment:** nothing — finalize becomes unprovable (liveness), never provable-wrong.

## 5. Size and budget projection

| Script                         | Basis                                              |                                     Projected raw bytes |
| ------------------------------ | -------------------------------------------------- | ------------------------------------------------------: |
| dispatcher                     | `ri2_fin` measured                                 | **11,153** (≈ 12,450 with the optional scalar re-check) |
| `V1VtLopDescScanFactsYield`    | shared plan y20 7,033 + composite summary builders |                                                ≈ 11,000 |
| `V1VtLopDescRefScriptYield`    | y21 7,766 + ≈ 1 KB                                 |                                                 ≈ 8,800 |
| `V1VtLopDescDatumSummaryYield` | y22 measured                                       |                                                   8,838 |
| `V1VtLopDescValueSummaryYield` | y23 3,996 + ≈ 1.5 KB                               |                                                 ≈ 5,500 |

Referenced bytes per transaction ≈ 11,153 + 11,000 + 8,800 + 8,838 + 5,500 =
**≈ 45,300** (≈ 46,600 with the re-check; today 34,586): tier 1 of the
Conway reference-script fee, ≈ 384,000 + 19,700 × 18 ≈ 739,000 lovelace
(≈ 0.74 ADA) against ≈ 546,000 lovelace (≈ 0.55 ADA) today. The fee rises
by ≈ 0.19 ADA per finalize because four yields are referenced; the plan
fixes publishability, not fee (same trade the shared plan records). Well
under `maxRefScriptSizePerTx`. Aggregate ExUnits: five executions parse the
dispatcher redeemer (descriptor ≤ ~400 B, terminal LOP control ≤ ~2 KB,
signer proof), the dispatcher re-encodes the eleven items once, and the
scan control is decoded twice (scan-facts and ref-script yields); no encode
round-trip anywhere. This is the largest aggregate in the resolve-inputs
group and is unmeasured; §7 measures it against the 13,200,000 memory basis
(16,500,000 harness) and falls back to the two-hop chain (§3) if it misses.
Method: `ri2_fin` measured; yields from the shared plan's measured y20–y23
plus its projected additions.

## 6. Off-chain work

**Nothing exists today for this contract beyond the `contracts.ts` title:
no deployment entry, no submit route, no funding row, no role.** To create,
in addition to the step plan §6 items (parameter map, deployment-entry table,
`requireValidationResolveInputsSemanticReferenceScriptUtxo`, inspection
fixtures, funding rows):

- `contracts.ts`: `resolveInputsMembershipFinalize` gains `reference_script_auth_policy_id`; the `ledgerOutputDescriptorYields` record (four, owned by the script-sources finalize plan) is applied with `[[scriptSourcesOutputProofFinalize.spendingScriptHash, resolveInputsMembershipFinalize.spendingScriptHash]]` — this plan contributes the second hash.
- Roles, manifest rows, node fixture, evidence JSON: the four `V1VtLopDesc…Yield` names are added once by the script-sources finalize plan; this plan adds no role.
- Deployment entry `validationTraceDisputeResolveInputsMembershipFinalizeSemantic` (`…ENTRIES_V1[4]`); the yield entries `validationTraceDisputeLopDesc{ScanFacts,RefScript,DatumSummary,ValueSummary}Yield` and their four reward-account registrations come from the script-sources finalize plan.
- Submit route for `resolverIndex === 7 && semanticResolverIndex === 4`: `semanticActionFieldsV1` emits `[input_index, output_index, transition, descriptor_cbor, signer_proof, control_cbor, claimed_value_summary, claimed_datum_summary, yield_ref_input_indices]`; `control_cbor = encodeMidgardLedgerOutputProofControlV1(terminal)` from the staged pending record; the leaf summaries from the shared pure helper `ledgerOutputDescriptorLeafSummariesV1(controlCbor)`; the builder reads four yield UTxOs and adds four zero withdrawals; `yield_ref_input_indices` via `requireReferenceInputIndex`.
- Funding row `validation-dispute.semantic.resolve-inputs.membership-finalize` with `referenceScriptBytes` ≈ 45,300.
- Codec: `resolverHints.controlCbor` and `resolverHints.claimedSummaries` (same hint names as the script-sources finalize plan); no `midgard-core` change.

## 7. Emulator scenario tests

In `demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-resolve-inputs.test.ts` (step plan §7 fixture and harness):

- `publishes the resolve-inputs finalize dispatcher and the four descriptor yields inside the L1 envelope`: dispatcher via `publishPlainReferenceScriptUtxo`, yields via `publishAuthenticatedValidationDisputeControl` with role NFTs, **without `oversized`**, asserting `l1ByteMargin > 0` and `assertReferenceScriptRawBodiesFitL1EnvelopeV1`, under `withRealL1MaxTxSize`.
- `resolves an honest membership finalize and awards`: fixture `disputedStep: "membershipFinalize"` (the state after the last `ledgerOutputProofStep` of the first membership lookup), spend input at a key address with `SignerMembershipProof`; through award and removal; assert `completeSignedBytes ≤ 16,384` and record `exUnits.mem` (≤ 13,200,000 — the gate for the §3 fallback).
- `proves missing required witness exactly`: spend input whose signer is absent → `reject_missing_required_witness` terminal (rejecting fixture in the style of `buildAcceptedClaimOverRejectingTransactionFixture`, stopping at `resolveInputs`/`MissingRequiredWitness`).
- Valid-block negative at the same frontier: the honest operator successor is claimed by the challenger → `expectOnchainRefusalV1` (accumulator successor mismatch).
- Negatives: one yield omitted; `yield_ref_input_indices` permuted; `descriptor_cbor` swapped for another output's descriptor (scan-facts yield fails); forged `claimed_value_summary` (value yield fails); script-address input with a non-`NoSignerSetProof` (`InputSignerProofMalformed` → refusal).
- Cancel/resume at the prepared step.
- Maximum shape: output with the maximum assets, a nested inline datum and a native reference script, descriptor at its largest encoding.

Nothing exists today.

## 8. Aiken tests

- `validators/fraud-proofs/validation-trace/ledger-output-descriptor-yields-v1.test.ak` (owned by the script-sources finalize plan): this plan adds `resolve_inputs_finalize_wins_with_four_yields` (honest vector from `resolve_inputs_authenticated_membership_step(2)` through `main.spend` + four `withdraw`s), `resolve_inputs_finalize_refuses_three_yields`, `resolve_inputs_finalize_emits_missing_witness_terminal`, and `yield_refuses_two_dispatcher_inputs` with one script-sources and one resolve-inputs finalize thread.
- `resolve-inputs-split-v1.test.ak`: dispatcher negatives — wrong `control_cbor`, wrong `descriptor_cbor`, role permutation, non-zero withdrawal, duplicate withdraw redeemer, `InputSignerProofMalformed` rejected, `yield_ref_input_indices` of length three or five, pending absent, successor with `pending` still set.
- `resolve-inputs-control-v1.test.ak`: `encode_control_raw` with `pending = None` equals `encode_resolve_inputs_witness(..., None, ...)`; the successor of the honest finalize vector equals the monolith's `resolve_inputs_successor_is_exact` target byte-for-byte.
- The shared property `descriptor_is_exact_is_the_conjunction_of_the_four_yield_predicates` (script-sources finalize plan §8) is the equivalence guard; this plan adds the resolve-inputs terminal controls from `ledger-output-proof-v1.test.ak` to its vector set.

## 9. Verification commands

As the step plan §9 (the `node -e` listing covers the four
`ledger_output_descriptor_*_yield_v1.main.withdraw` titles); additionally
`aiken check -m ledger_output_descriptor_yields` must list the three
`resolve_inputs_finalize_*` tests and `aiken check -m descriptor_is_exact`
the conjunction property, all passing.

## 10. Ordering and dependencies

Lands with the five sibling plans (shared codec, one regeneration), the
script-sources output-proof-finalize plan (the yield parameter lists both
dispatcher hashes; shared `ledger-output-proof-v1.ak` narrow variants) and
the semantic-yield handshake library. Does not depend on the LOP _stage_
yields (the terminal control is read, not stepped). Requires
`reference_script_auth_policy_id` in the semantic parameter set.

**Reconciliation record (review pass, 2026-09-02).** The earlier draft
defined two parameterless pure yields (`V1VtOutputProofFactsYield`,
`V1VtOutputProofSummariesYield`, redeemer `OutputProofFinalizeClaimV1`)
that the dispatcher bound by comparing claim bytes. Decisions, preferring the
shared design:

- **D1** Adopt the four `V1VtLopDesc…Yield` roles, files, `dispatcher_script_hashes`
  parameter and `unique_semantic_dispatch_v1`; the redeemer carries
  `control_cbor` explicitly (the yields cannot extract the terminal control
  from an 11-item resolve-inputs witness without its codec) — the same
  field the step plan adds, and one the script-sources finalize redeemer
  should carry too.
- **D2** The composite summaries move into the scan-facts yield via the
  `claimed_*` leaf channel, as the shared plan specifies, instead of a
  dedicated `summaries` yield.
- **D3** Signer authorization stays in the dispatcher (as
  `protected_output_authorization` stays in the script-sources finalize
  dispatcher); the fee increase this implies (≈ 0.19 ADA) is accepted for
  publishability, with the shared plan's dispatcher prune as future work.
- **D4** Measured rather than projected: `ri2_fin` 11,153 replaces the
  12,869 prototype.

## 11. Risks

- **Aggregate ExUnits** of five executions is the highest in the group and
  unmeasured; fallback is the two-hop chain (adds one transaction per
  membership finalize, bounded by the resolution-schedule length, inside
  C52).
- **Fee up, not down** (≈ 0.55 → ≈ 0.74 ADA per finalize) because four
  reference scripts are read; acceptable because the transaction is
  publishable at all, which it is not today.
- Dispatcher headroom ≈ 3.8 KB (2.5 KB with the scalar re-check); if a
  regeneration pushes it, move `decode_canonical_address_bytes` into the
  scan-facts yield and pass the decoded address through the redeemer as a
  pinned leaf.
- ABI: redeemer gains four fields and a third parameter; `zz605`, the
  redeemer ABI test in `validation-dispute-submit.test.ts` and the wire
  layout pins regenerate once. Spec: C41 semantics unchanged.
