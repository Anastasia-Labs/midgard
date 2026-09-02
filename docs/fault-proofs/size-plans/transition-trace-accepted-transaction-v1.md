# Size-fit plan: `fraud_proofs/transition_trace/accepted_transaction_v1.main`

Cites [00-primer.md](00-primer.md). Companion plan:
[transition-trace-deposit-v1.md](transition-trace-deposit-v1.md) (shares the
`output_summaries` yield and the `final_yield_v1` redeemer introduced here).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/transition_trace/accepted_transaction_v1.main.spend` (+ `.else`) |
| File | `onchain/aiken/validators/fraud-proofs/transition-trace/accepted-transaction-v1.ak` (62 lines) |
| Raw size | **40,869 bytes** (reproduced 2026-09-01 on a `/tmp/size-probe-tt` copy, 567 validators) |
| Applied parameters (3) | `computation_thread_token_policy_id: PolicyId`, `fraud_proof_token_policy_id: PolicyId`, `fraud_proof_token_address: Address` (`contracts.ts` `buildTransitionTraceChain`, `finalSpecs` row `["accepted", false]`) |
| Family position | Transition-trace **final 4**; `route_v1.route_index` sends `InvalidOneStepTransition { witness: L2TransactionTransition }` and `AcceptedTransactionTransitionMismatch` here. Terminal step: `common.finalize` burns the thread and mints the permanent fraud-proof token |
| Library entry | `proof.validate_accepted_transaction_fault_proof(proof, asset_name)` → `validate_transition_fault_proof_envelope` + `validate_l2_transaction_transition` (7 witness fields) or `validate_accepted_transaction_transition_mismatch` |
| Role name today | `V1FpTransitionTraceFinal4` (`DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES["V1 fraud-proof transition-trace final-4"]`) |
| Deployment entry today | `fraudProofTransitionTraceAcceptedTransaction` (`TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES[4]` in `demo/midgard-fault-proofs/src/transition-trace/submit.ts`; same name in `midgard-core` `deployment-manifest-identity-v1.ts`, node `deployment-manifest-v1.ts`, `runtime.ts`, `inspect-contracts.ts`, watcher `proof-thread-indexer.ts`) |
| Emulator today | Published only with `oversized: true` via `TRANSITION_TRACE_OVERSIZED_REFERENCE_SCRIPT_ENTRIES` (`tests/support/emulator/reference-scripts.ts:644`); no scenario exercises final 4 |

## 2. Why it is this size

All probes were built on a copy (`cp -r onchain/aiken /tmp/size-probe-tt`,
pinned `v1.1.23-org-5adf7837`, `aiken build --env testnet`, sizes read from
`plutus.json` with `Buffer.from(compiledCode,"hex").length`). Probe validators
lived under `validators/probe/`; private `proof.ak` functions were made `pub`
in the copy only. The copy has been deleted. Raw bytes; the empty-validator
floor is 129 bytes.

### 2.1 Wrapper and decoders

| Probe | What it compiles | Bytes | Reading |
| --- | ---: | ---: | --- |
| p00 | empty spend validator | 129 | floor |
| p01 | `cancel` + `finalize`, datum `StepDatum<Data>` | 1,444 | the terminal wrapper is ~1.3 KB |
| p02 | same, datum typed `final_v1.Datum` | 1,497 | typed **datum** arg adds 53 B: no decoder is generated |
| p03 | p02 + `validate_transition_fault_proof_envelope` | 1,547 | envelope check ≈ 50 B (hashing is shared) |
| q05 | typed datum arg, never touched | 142 | confirms: validator datum types are not checked |
| q06 | opaque datum + explicit `expect s: Datum = d` | 9,293 | the full `TransitionFaultProof` decoder is **~9.1 KB** |
| q07 | opaque datum, `HeaderV1` decoded only | 553 | narrow decoding is cheap |
| q08 | typed **redeemer** arg (`Datum` type), never touched | 9,149 | validator redeemer types **are** fully checked |

Consequences used below: the route validator's typed redeemer
(`Args { proof: TransitionFaultProof }`) is what validates the proof shape;
finals read the datum lazily. A yield that re-parses the datum with a plain
`expect` pays 9.1 KB; a yield with a large typed redeemer pays its decoder.
Yields must therefore narrow-decode only the fields they use.

### 2.2 The two arms inside the real wrapper

| Probe | Arm | Bytes | Arm ≈ |
| --- | --- | ---: | ---: |
| p10 | wrapper + `AcceptedTransactionTransitionMismatch` only | 15,798 | 14.3 KB |
| p11 | wrapper + `L2TransactionTransition` only | 32,059 | 30.5 KB |
| shipped | both arms | 40,869 | shared ≈ 5.5 KB |

### 2.3 What the L2 arm reaches (opaque redeemer, no wrapper)

| Probe | Function | Bytes | Net of floor/decoders |
| --- | --- | ---: | ---: |
| p30 | `anchored_native_tx` + `unanchored_validity_code_of` + version | 1,828 | 1.7 KB |
| p31 | p30 + `door_body_field_items(spend_inputs_field_index, decode_midgard_tx_input_cbor)` | 5,945 | door + input decoder ≈ 4.1 KB |
| p32 | p30 + door over `outputs_field_index` with `decode_midgard_tx_output_cbor` | 7,551 | + output decoder ≈ 1.6 KB |
| p38 | `validate_l2_one_step_binding` | 6,012 | 5.9 KB (trace + event-to-step + source membership) |
| p33 | `apply_l2_spends` | 3,742 | 3.6 KB (MPF membership + delete) |
| p43 | `apply_insert_witness` | 3,348 | 3.2 KB (MPF non-membership + insert) |
| p37 | `encode_midgard_tx_output` | 2,486 | 2.4 KB |
| p34 | decode `List<MidgardTxOutput>` from Data | 1,220 | 1.1 KB |
| **p36** | **`ledger_output_descriptor_v1.ledger_value_v1`** | **16,751** | **16.6 KB — dominates** |
| p35 | `apply_l2_outputs` (= p36 + p37 + p43 + key check) | 21,263 | 21.1 KB |

Inside `ledger_value_v1` (`build_v1` + `ledger_output_commitment_v1.encode`):

| Probe | Function | Bytes |
| --- | --- | ---: |
| q60 | `ledger_output_v1.decode_canonical_output` | 8,662 |
| q61 | `script_context_v1.ledger_output_summaries_v1` (decode + `cek_data_v1.semantic_data_summary_v1` ×2 + spend-datum summary) | 12,718 |
| r04 | `canonical_plutus_data_v1.is_canonical_plutus_data_v1` | 3,548 |
| q63 | `output_item_commitment` | 1,403 |
| q62 | `descriptor_is_well_formed` + `encode` | 2,298 |
| r03 | the rest of `build_v1` (asset fold, reference-script facts, well-formedness, encode) given the three summaries | 5,095 (≈ 3.6 KB net) |
| r01 | a decode-free `ledger_value_from_decoded_v1(index, MidgardTxOutput)` | 14,736 | only 2 KB smaller: the CBOR/Data parsers and the canonical scan are shared |

### 2.4 What the claim arm reaches

| Probe | Function | Bytes |
| --- | --- | ---: |
| p20 | decode `HeaderV1` + `ValidationClaimWitnessV1` from Data | 5,407 (≈ 5.3 KB decoder) |
| p21 | p20 + `validation_claim_v1.committed_claim_is_valid` | 19,586 (≈ 14.2 KB predicate) |
| p22 | p20 + `terminal_acceptance_post_root` + `hash_work_witness` | 7,083 (≈ 1.7 KB) |
| q81 | narrow-decode + `committed_claim_structure_is_valid` (as a yield) | 11,462 |
| s20 | narrow-decode + `committed_claim_source_is_authenticated` (yield) | 11,413 |
| s21 | narrow-decode + `committed_claim_endpoints_and_source_are_valid` (yield) | 12,125 |
| s22 | narrow-decode + terminal checks only (yield) | 7,199 |
| q82 | source + endpoints + terminal in one yield | 17,930 (over) |

## 3. Options considered

1. **Prune reachable code.** The arm-specific entry points already exist
   (`validate_accepted_transaction_fault_proof` only reaches its two arms). The
   only large candidate is `ledger_value_v1`'s second decode of bytes the arm
   itself just produced with `encode_midgard_tx_output`. Measured: a
   decode-free variant is 14,736 vs 16,751 (r01 vs p36) — 2 KB, and it would
   drop `decode_canonical_output`'s canonicity round-trip, i.e. change what the
   arm proves. **Rejected**: too small to matter and not predicate-preserving.
2. **Withdraw-zero yield split (single transaction).** The terminal mint must
   stay in a spend validator that calls `finalize`; the arm predicates move to
   rewarding validators bound by `require_authenticated_zero_yield`. A naive
   one-yield-per-arm split does not fit: the L2 arm alone is 30.5 KB (q55:
   33,145 as a narrow-decoding yield) and the claim arm as a yield is 20,303
   (q56). It fits only when the L2 arm is staged as *open → summaries →
   assemble → replay* and the claim arm as *structure / source / endpoints*
   with the terminal checks in the dispatcher (§4). **Chosen.**
3. **Multi-transaction chaining.** Inserting a `continue` step before the final
   only moves `finalize` (~1.3 KB) out of the arm; the arm itself would still be
   30.5 KB in one script, so chaining alone solves nothing and costs one more
   transaction per proof inside the §3.3 maturity margin. **Rejected** (kept
   as a fallback only if the aggregate ExUnits of §5 fail).
4. **Redesign.** A ninth final for the claim arm measures 15,798 (p10):
   under 16,384 but over the primer's 15,000 target (≈ 240 B signed margin).
   Not adopted; recorded as the fallback if the three claim yields are judged
   too heavy operationally.

## 4. Chosen design

### 4.1 New validator list

All sizes are measured prototypes (probe id in brackets). Roles are asset names
≤ 32 bytes, matching `min_ada/yield.tx_role` style constants in a new
`lib/midgard/fraud-proofs/transition-trace/yield.ak`.

| Script (blueprint title) | Purpose | Kind / params | Role | Measured |
| --- | --- | --- | --- | ---: |
| `fraud_proofs/transition_trace/accepted_transaction_v1.main` (rewritten) | dispatcher: `cancel`; `finalize`; envelope; per-arm binding; yield handshakes; L2 descriptor assembly; claim terminal checks | spend; `(computation_thread_token_policy_id, fraud_proof_token_policy_id, fraud_proof_token_address, reference_script_auth_policy_id)` | `V1FpTransitionTraceFinal4` (unchanged) | 12,994 [s11] (+~100 B for the third claim handshake) |
| `fraud_proofs/transition_trace/accepted_transaction_yields_v1.l2_open` | anchor the committed tx, open fields 0 and 2, commit decoded items | withdraw; `(dispatcher_script_hash)` | `V1FpTtF4L2OpenYield` | 12,629 [r11] |
| `fraud_proofs/transition_trace/output_summaries_v1.yield` (shared with final 5) | `ledger_output_summaries_v1` over every committed output byte string, committed in its redeemer | withdraw; `(commit_yield_script_hash)` | `V1FpTtF4L2SummariesYield` | 13,171 [r12b] |
| `fraud_proofs/transition_trace/accepted_transaction_yields_v1.l2_replay` | MPF deletes over committed keys, inserts over produced witnesses, post-root mismatch | withdraw; `(dispatcher_script_hash, l2_open_yield_script_hash)` | `V1FpTtF4L2ReplayYield` | 5,023 [r13] |
| `...accepted_transaction_yields_v1.claim_structure` | `committed_claim_structure_is_valid` | withdraw; `(dispatcher_script_hash)` | `V1FpTtF4ClaimStructYield` | 11,462 [q81] |
| `...accepted_transaction_yields_v1.claim_source` | `committed_claim_source_is_authenticated` | withdraw; `(dispatcher_script_hash)` | `V1FpTtF4ClaimSourceYield` | 11,413 [s20] |
| `...accepted_transaction_yields_v1.claim_endpoints` | `committed_claim_endpoints_and_source_are_valid` | withdraw; `(dispatcher_script_hash)` | `V1FpTtF4ClaimEndsYield` | 12,125 [s21] |

Library changes (all ABI-neutral for `TransitionFaultProof`):

- `ledger_output_descriptor_v1`: extract `pub fn assemble_v1(output_index,
  output, output_cbor, cardano_tx_out, midgard_tx_out, cardano_spend_datum)`
  from `build_v1` and make `build_v1` call it, so the staged path and the
  monolith cannot drift (measured 5,095 standalone).
- `proof.ak`: keep `validate_l2_transaction_transition` and
  `validate_accepted_transaction_transition_mismatch` as the reference
  predicates for equivalence tests; expose `validate_l2_one_step_binding`,
  `apply_delete_witness`, `apply_insert_witness`, `door_body_field_items`,
  `terminal_acceptance_post_root` as `pub` for the dispatcher and yields.
- New `lib/midgard/fraud-proofs/transition-trace/final_yield_v1.ak`:
  `Args { input_index, output_index, hub_ref_input_index,
  fraud_proof_mint_redeemer_index, yield_ref_input_indices: List<Int> }`,
  `SpendRedeemer = ct.StepRedeemer<Args>`, `OpenedOutputsV1 { spend_input_keys:
  List<ByteArray>, outputs: List<MidgardTxOutput>, output_cbors: List<ByteArray> }`,
  `OutputSummariesV1 { summaries: List<(DataSummaryV1, DataSummaryV1,
  DataSummaryV1)> }`, `YieldTransitionTraceV1` (unit redeemer for the other
  yields), and `dispatched(dispatcher_script_hash, inputs, redeemers) -> (Data,
  Data, Data)` (header, fault, spend redeemer as raw Data) plus
  `one_step_witness(fault_data, tag)`. Finals 0–3, 6, 7 keep `final_v1.Args`.

### 4.2 Datum / redeemer ABI deltas

- Datum: unchanged (`ct.StepDatum<TransitionFaultProof>`); `TransitionFaultProof`
  and every witness type unchanged, so `midgard-sdk` schemas, the proof CBOR
  the CLI accepts, `reconstruct.ts`, `detect.ts`, `witnesses.ts` are untouched.
- Final-4 spend redeemer: `final_v1.Args` → `final_yield_v1.Args` (adds
  `yield_ref_input_indices`). Off-chain `makeTransitionTraceFinalSpendRedeemer`
  must branch on `finalIndex ∈ {4, 5}`.
- New withdraw redeemers: `OpenedOutputsV1` (l2_open), `OutputSummariesV1`
  (output_summaries), `YieldTransitionTraceV1` (replay, claim_*). Their decoder
  cost is inside the measured sizes (q08 shows typed redeemers are checked).

### 4.3 Exact handshake

Dispatcher `Continue`:

1. `finalize(...)` exactly as today (thread input, fraud-proof output at
   `fraud_proof_token_address` with the same asset name, mint redeemer at
   `fraud_proof_mint_redeemer_index`).
2. `validate_transition_fault_proof_envelope(proof, asset_name)`.
3. `when proof.fault is`:
   - `InvalidOneStepTransition { witness: L2TransactionTransition { trace_proof,
     event_to_step, source_membership, produced_utxos, .. } }`:
     `expect validate_l2_one_step_binding(header, trace_proof, event_to_step,
     source_membership)`; `expect [i_open, i_sum, i_replay] =
     yield_ref_input_indices`; `open_hash = require_authenticated_zero_yield(…,
     l2_open_role, i_open)`, `sum_hash = require_…(l2_summaries_role, i_sum)`,
     `require_…(l2_replay_role, i_replay)`; read
     `OpenedOutputsV1` at `Withdraw(Script(open_hash))` and `OutputSummariesV1`
     at `Withdraw(Script(sum_hash))` with `utils.get_unique_withdraw_redeemer`;
     `assembled_match(source_membership.key, 0, outputs, output_cbors,
     summaries, produced_utxos)`: for every index `i`, `produced_utxos[i].key ==
     encode_midgard_tx_input({tx_id, i})` and `produced_utxos[i].value ==
     encode(assemble_v1(i, outputs[i], output_cbors[i], s1, s2, s3))`, all
     four lists exhausted together.
   - `AcceptedTransactionTransitionMismatch { witness }`: `expect [i_str, i_src,
     i_end] = yield_ref_input_indices`; require the three claim roles; then
     `descriptor.verdict == Accepted`, `terminal.work_root ==
     hash_work_witness(Terminal, terminal.program_counter, cbor)`,
     `terminal_acceptance_post_root(cbor) !=
     claim.transition_step_membership.value.post_utxos_root` (the terminal
     checks measured at s22 − decoders ≈ 1.7 KB, s11 − s10 = 1,701).
   - `_ -> fail`.

`l2_open` (redeemer `OpenedOutputsV1`): `dispatched(dispatcher_hash, inputs,
redeemers)` (exactly one input at the dispatcher credential, exactly one spend
redeemer for it, constructor `Continue`); `one_step_witness(fault, 4)` → decode
`source_membership: RootMembershipProof<ByteArray, ByteArray>`, the two
preimages; `expect source: L2TransactionSourceV1 = cbor.deserialise(value)`;
`anchored_native_tx(BodyTxOpening{compact_cbor}, BodyAnchor{tx_id: key})`;
`unanchored_validity_code_of == 0`; both doors (`spend_inputs_field_index`,
`outputs_field_index`); then `cbor.serialise(source) == value`, `source.tx_id
== key`, `anchored_native_tx_version == 1`, `redeemer.spend_input_keys ==
map(encode_midgard_tx_input)`, `redeemer.outputs == outputs`,
`redeemer.output_cbors == map(encode_midgard_tx_output)`.

`output_summaries` (redeemer `OutputSummariesV1`): read the commit yield's
redeemer at `Withdraw(Script(commit_yield_script_hash))` (parameter), take
field 2 (`output_cbors`) narrowly, and require `redeemer.summaries ==
map(output_cbors, fn(c) { expect Some(s) = ledger_output_summaries_v1(c); s })`.
This runs `decode_canonical_output` on the same re-encoded bytes `build_v1`
runs it on today, so canonicity is checked exactly as before.

`l2_replay`: `dispatched(...)`; decode `trace_proof`, `spent_utxos`,
`produced_utxos`; read `spend_input_keys` (field 0) of the open yield's
redeemer; fold `apply_delete_witness` requiring `witness.key == key` pairwise
and both lists exhausted; fold `apply_insert_witness` over `produced_utxos`;
`result != trace_proof.value.post_utxos_root`.

`claim_structure` / `claim_source` / `claim_endpoints`: `dispatched(...)`;
`expect unconstr_index(fault) == 9`; decode `header: HeaderV1` and `claim:
ValidationClaimWitnessV1`; run the named `validation_claim_v1` predicate.

### 4.4 Security argument

- **Dispatch uniqueness.** Every yield that reads the thread does so through
  `dispatched`, which requires exactly one input at the dispatcher credential
  and exactly one spend redeemer for it; one withdrawal cannot discharge two
  threads. `output_summaries` does not read the thread; it is bound to the one
  `l2_open` withdrawal in the transaction, which is itself bound to the one
  thread.
- **Role authentication.** Each yield is admitted only through
  `require_authenticated_zero_yield` on a reference input carrying exactly one
  `reference_script_auth_policy_id` token with that arm's role name, whose
  `reference_script` hash has an exact zero withdrawal and a unique withdraw
  redeemer. The dispatcher uses the returned hash to locate the yield's
  redeemer, so nothing about the yield identity comes from the spend redeemer.
- **Cross-arm substitution.** Roles are distinct per yield; the L2 yields also
  `expect` witness constructor tag 4 and the claim yields fault tag 9, so a
  claim thread cannot be discharged by L2 yields even if roles were misapplied.
  `output_summaries` is bound to a specific commit yield hash by parameter, so
  the final-5 instance cannot serve final 4.
- **Re-derivation instead of trust.** No yield accepts a prover-supplied
  value. `l2_open` re-derives keys/outputs/bytes from the door and compares them
  with its redeemer; `output_summaries` re-derives summaries from those bytes;
  the dispatcher re-derives every descriptor with `assemble_v1` and compares it
  with the witness values `l2_replay` inserts. Since the thread has no
  continuation datum (terminal step), the "output-state re-derivation" of the
  primer is exactly this chain of equalities plus `finalize`'s fraud-proof
  output check.
- **Omission.** Omit `l2_open`: dispatcher fails at the handshake (and could not
  read a redeemer). Omit `output_summaries`: same. Omit `l2_replay`: the post
  root is never compared and an honest block could be slashed on a well-formed
  witness; hence it is mandatory and role-authenticated. Omit any claim yield:
  a fabricated claim (unverified memberships / unauthenticated source / wrong
  endpoints) could mint; all three are mandatory.
- **Malformed data.** The route validator's typed redeemer decodes the whole
  `TransitionFaultProof` before it is written into the datum (q08); every yield
  still narrow-decodes what it reads, so a datum forged outside the route path
  fails in the yield rather than being read lazily.

## 5. Size and budget projection

| Script | Raw bytes (measured prototype) | ≤ 15,000 |
| --- | ---: | :-: |
| dispatcher | 12,994 + ~100 | yes (margin ≈ 1.9 KB) |
| l2_open | 12,629 | yes |
| output_summaries | 13,171 | yes (margin 1.8 KB) |
| l2_replay | 5,023 | yes |
| claim_structure | 11,462 | yes |
| claim_source | 11,413 | yes |
| claim_endpoints | 12,125 | yes |

Referenced bytes per final transaction (plus `computation_thread.mint` 3,979
and `fraud_proof.mint` 845, which the final already reads as witness reference
scripts; hub oracle and event UTxOs are datum-only reference inputs):

- L2 transaction: 13.1 + 12.6 + 13.2 + 5.0 + 4.8 ≈ **48.7 KB** → second
  `minFeeRefScriptCostPerByte` tier (25–50 KiB), roughly 0.80 ADA of
  reference-script fee at 15 lovelace/byte ×1.2 per tier. Today: 40.9 + 4.8 =
  45.7 KB, same tier.
- Claim transaction: 13.1 + 11.5 + 11.4 + 12.1 + 4.8 ≈ **52.9 KB** → crosses
  into the third tier by ~3 KB (≈ 0.91 ADA). Acceptable; if it must stay in
  tier two, the two mint policies can be attached inline instead of referenced
  for this transaction only (they are 4.8 KB and fit).

Execution budget: the split re-executes only cheap plumbing. Extra work over
today's arm: three `dispatched` scans of inputs/redeemers, narrow decodes of
the witness pieces (q07-scale), one extra `encode_midgard_tx_output` pass
(`l2_open` commits bytes the dispatcher then hashes), Data equality of the
committed lists, and three `require_authenticated_zero_yield` scans. The
descriptor computation itself is not duplicated (summaries in one yield,
assembly in the dispatcher).

Today's monolithic arm, measured on the copy with `aiken check --env testnet
-m 'proof_exunits_frontier.{..}'` (pinned fork; the `_fixture_only` row is the
fixture construction the test itself pays, so arm ≈ row − fixture):

| Test | mem | cpu | Arm ≈ |
| --- | ---: | ---: | --- |
| `l2_no_op_arm` (1 spend, 1 output) | 15.27 M | 6.87 B | 5.76 M mem, 2.50 B cpu |
| `l2_no_op_arm_fixture_only` | 9.51 M | 4.37 B | — |
| `l2_arm_refuses_full_output_replay` | 15.26 M | 6.87 B | (neutralisation selector) |

Projection: the split adds to the ~5.8 M arm roughly one narrow decode per
yield (≤ 0.2 M each at q07 scale), three `dispatched` scans, one extra output
encode and the Data equalities — on the order of 1–2 M memory in total, well
inside the GOAL_SPEC §3.3 basis of 13,200,000 for the whole transaction. The
claim arm has no frontier row today; `accepted_forced_transition_case` in
`proof.test.ak` should be promoted to one. Per-yield ExUnits were **not**
measured here (no transaction-level probe); §7 makes the emulator assertion
under the shared Van Rossem limits the authority.

## 6. Off-chain work

Exists today: `contracts.ts` `buildTransitionTraceChain` (8 finals, no yields);
`TRANSITION_TRACE_FAULT_PROOF_TITLES`; role table rows for route and finals
0–7; deployment entries `fraudProofTransitionTrace*`; `submit.ts`
`submitTransitionTraceFinalV1` (single spend, `readFrom` hub + final reference
script + mint witnesses); `transitionTraceFinalIndex`. Does not exist: any
yield, any withdraw route, any reference-script role beyond the finals.

1. **SDK contracts** (`demo/midgard-sdk/src/fraud-proof/contracts.ts`):
   `finalSpecs` row `accepted` gets `referenceScriptAuthPolicyId` as fourth
   parameter (already in `BuildFaultProofContractsParams`, as min-ADA uses);
   add titles for the six withdraw validators; build them with
   `makeWithdrawalValidator(applyBlueprintParams(...))` — `l2_open`,
   `claim_*` with `[dispatcher.spendingScriptHash]`, `output_summaries` with
   `[l2Open.withdrawalScriptHash]`, `l2_replay` with `[dispatcher, l2Open]`;
   extend `TransitionTraceFaultProofContracts["transitionTrace"]` with
   `yields: { l2Open, l2Summaries, l2Replay, claimStructure, claimSource,
   claimEndpoints }`. Dispatcher hash depends only on the four parameters, so
   there is no cycle. Arity: `zz605-semantic-resolver-arity.test.ts` /
   `zz610-compiled-script-arity.test.ts` must see the new titles.
2. **Reference-script roles**: add six rows to
   `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES`
   (`"V1 fraud-proof transition-trace final-4 L2 open yield": "V1FpTtF4L2OpenYield"`,
   …), mirrored in `midgard-core/src/deployment-manifest-identity-v1.ts`
   (both the contract-name list and the role map), `midgard-node/src/deployment-manifest-v1.ts`,
   `midgard-node/src/transactions/reference-scripts.ts`
   (`manifestReferenceScriptTarget(name, yields.x.withdrawalScript)` as for
   `fraudProofMinAdaStep02TxWithdraw`), and
   `midgard-node/src/commands/contract-deployment-info.ts`
   (`withdrawalDescriptor`). Aiken constants must match byte-for-byte
   (ABI-04 fails closed).
3. **Deployment entries**: `fraudProofTransitionTraceAcceptedTransactionL2OpenWithdraw`,
   `...L2SummariesWithdraw`, `...L2ReplayWithdraw`, `...ClaimStructureWithdraw`,
   `...ClaimSourceWithdraw`, `...ClaimEndpointsWithdraw` added to
   `DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES`, `runtime.ts`
   `FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY` (keep the 9 step entries as
   the step list; add a separate yield list, since
   `publishFraudProofChainReferenceScripts` requires `steps.length ===
   entryNames.length`), `inspect-contracts.ts`, watcher
   `proof-thread-indexer.ts` (`stepCount` stays 9), and the inspection fixture
   `demo/da-committee-node/tests/fixtures/da-contract-deployment-info.json`.
4. **Submit route** (`transition-trace/submit.ts`): in
   `submitTransitionTraceFinalV1`, when `finalIndex === 4`: resolve the yield
   reference scripts by entry name, `readFrom` them, `.withdraw(scriptRewardAddress(network, yield.withdrawalScript), 0n, redeemer)`
   for each, and build `final_yield_v1.Args` with `yield_ref_input_indices`
   from `requireReferenceInputIndex` (pattern: `min-ada/submit-step-02-v1.ts`).
   Redeemers: `OpenedOutputsV1` from the reconstruction's decoded spend inputs
   (`midgardOutRefToCbor` keys) and outputs (canonical codec in
   `midgard-core`), `OutputSummariesV1` from
   `buildCanonicalMidgardLedgerOutputMaterialV1`'s descriptor (decode the
   three `DataSummaryV1` fields from `descriptorCbor`). Reward accounts for the
   six yield scripts must be registered at deployment as the min-ADA yields
   are (verify how `fraudProofMinAdaStep02TxWithdraw` is registered in
   `midgard-node` before copying).
5. **Funding requirements**: six more reference-script publications in the
   deployment funding table (each ≤ 13.2 KB raw, ~20 ADA min-Ada at the
   emulator's `publishPlainReferenceScriptUtxo` default); the
   `assertReferenceScriptRawBodiesFitL1EnvelopeV1` admission covers them.
6. **Codecs**: no `midgard-core`/`validation` codec change; `TransitionFaultProof`
   is unchanged. Add `OpenedOutputsV1`/`OutputSummariesV1` Data schemas to
   `midgard-sdk` (transition-trace schema file) with a golden test against the
   Aiken types.

## 7. Emulator scenario tests

Exists today (`demo/midgard-fault-proofs/tests`):

- `submit-init-emulator-transition-trace.test.ts` — "submits and removes a
  tail transition-trace fraud proof end to end": fixture
  `buildInvalidForcedTransitionTraceFixture` → **final 3** (forced). Publishes
  all nine transition-trace scripts with
  `oversizedEntryNames: TRANSITION_TRACE_OVERSIZED_REFERENCE_SCRIPT_ENTRIES`.
- `submit-init-emulator-transition-trace-subvariants.test.ts` — omitted due
  withdrawal → final 6; out-of-window withdrawal → final 6; transition-step
  count mismatch → final 0; negative "rejects an honest late withdrawal accused
  as omitted at final 6". Same publication call.
- No emulator scenario reaches final 4. Unit coverage of the arm exists only in
  Aiken (`proof.test.ak`: `accepts_valid_l2_transaction_no_op_transition_fault`,
  `rejects_l2_transaction_when_trace_matches_expected_post_root`, the
  `rejects_l2_transaction_with_*` set, `accepts_valid_forced_transaction_wrong_accepted_post_root_fault`
  and its three rejections) and in `transition-trace-challenger.test.ts`
  (`buildL2TransactionTransitionWitness`, tag-4 detection).

When the `oversized` marker is removed: `publishPlainReferenceScriptUtxo`
asserts `l1ByteMargin > 0` for `fraudProofTransitionTraceAcceptedTransaction`
and `fraudProofTransitionTraceDeposit`; both existing suites therefore prove
the dispatchers publish (they still publish all nine step entries). Delete
`TRANSITION_TRACE_OVERSIZED_REFERENCE_SCRIPT_ENTRIES` and the two
`oversizedEntryNames:` arguments; add
`publishTransitionTraceYieldReferenceScriptsV1` (modelled on
`publishMinAdaYieldReferenceScriptsV1`) publishing the six final-4 yields and
the two final-5 yields without `oversized`.

Add `submit-init-emulator-transition-trace-final4.test.ts`:

- *Publication fit*: every final-4 script publishes; assert each
  `publicationMeasurement.completeSignedBytes <= 16,384` and record it.
- *Positive L2 lifecycle*: fixture from `buildL2ReplayFixture({
  matchingCommittedRoot: false })` (already in
  `transition-trace-challenger.test.ts`; move to
  `tests/support/submit-init-emulator-fixtures.ts`) with one spend and one
  output; init → route → final 4 with `l2_open`, `output_summaries`,
  `l2_replay` withdrawals → fraud-proof token minted → `submitRemoveFraudulentBlock`.
- *Valid-block negative at the same frontier*: `matchingCommittedRoot: true`
  — expect `l2_replay` to fail the transaction; also a forged
  `produced_utxos[0].value` — expect the dispatcher's `assembled_match` to
  fail (proves the summaries/assembly binding, not just the replay).
- *Positive claim lifecycle*: fixture from `accepted_forced_transition_case`
  (Aiken) ported to TypeScript: a committed validation trace with
  `Accepted` verdict whose terminal post root differs from the trace step;
  final 4 with the three claim withdrawals.
- *Claim negative*: matching post root; and one claim yield omitted
  (expect the handshake failure).
- *Cancel*: route to final 4, then `ct.Cancel` by the prover (the family
  supports cancel at every step).
- *Maximum supported shape*: the C22 boundary output (5,000-byte Cardano
  Value) as the single produced output, and a wide transaction (e.g. 16
  outputs) to exercise the list folds; assert memory ≤ 13,200,000 and CPU ≤
  10,000,000,000 aggregate from `localUPLCEval`.

## 8. Aiken tests

New `onchain/aiken/validators/fraud-proofs/transition-trace/yields-v1.test.ak`
(pattern: `validators/fraud-proofs/min-ada/family-v1.test.ak`):

- Positive: dispatcher + `l2_open` + `output_summaries` + `l2_replay` accept
  the `l2_no_op_parts` fixture; dispatcher + three claim yields accept
  `accepted_forced_transition_case`.
- Equivalence: for the existing `proof.test.ak` L2 and claim fixtures,
  `validate_l2_transaction_transition` /
  `validate_accepted_transaction_transition_mismatch` accept iff the split
  accepts (both directions, including the `fail` cases).
- `assemble_v1` factoring: property test (`aiken/fuzz` over `MidgardTxOutput`)
  that `encode(assemble_v1(i, o, encode(o), summaries(encode(o)))) ==
  ledger_value_v1(i, encode(o))` whenever the latter is `Some`, and both are
  `None` together.
- Substitution: swap `V1FpTtF4L2OpenYield` and `V1FpTtF4L2ReplayYield`
  reference-input roles → fail; `output_summaries` applied to the final-5
  commit hash presented to final 4 → fail; claim yields presented to an L2
  thread → fail on tag.
- Omission: each of the six yields omitted in turn → fail.
- Forgery: `OpenedOutputsV1` with one altered key / output / byte string →
  `l2_open` fails; `OutputSummariesV1` with one altered summary → fails;
  `produced_utxos` value altered → dispatcher fails.
- Multiplicity: two dispatcher inputs in one transaction → `dispatched` fails.
- Route unchanged: `route_index` still maps both faults to index 4.

## 9. Verification commands

```bash
cd onchain/aiken
aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/transition_trace/.test(v.title)&&!/\.else$/.test(v.title))console.log(v.title,Buffer.from(v.compiledCode,"hex").length)'
# expect: accepted_transaction_v1.main.spend ≤ 13,200; six accepted_transaction_yields_v1.* ≤ 13,200;
#         output_summaries_v1.yield ≤ 13,300; all other transition_trace entries unchanged
#         (route 10,704; control 7,217; source 6,845; withdrawal 6,465; forced 5,441; l1_event 10,984; duplicate 4,528)
node -e 'const b=require("./plutus.json");console.log(b.validators.length)'   # 567 + 2 per new validator (7 → 581), before the other plans land
aiken check --env testnet -m transition_trace          # all existing proof.test.ak selectors still pass
aiken check --env testnet -m yields_v1                 # new file, expect every test listed in §8
aiken check --env testnet -m proof-exunits-frontier    # l2_no_op_arm / deposit_arm_* still within basis
cd ../../demo
pnpm --filter @al-ft/midgard-sdk test -- transition-trace                       # schema goldens incl. OpenedOutputsV1
pnpm --filter @al-ft/midgard-core test -- deployment-manifest-identity-v1        # role/entry tables
pnpm --filter @al-ft/midgard-fault-proofs test -- zz605 zz610                    # arity
pnpm --filter @al-ft/midgard-fault-proofs test -- submit-init-emulator-transition-trace  # existing 1 + 4 + new final4 file
pnpm --filter @al-ft/midgard-node test -- contract-deployment-info               # manifest fixtures
```

Baseline ExUnits from the frontier tests on the pinned fork are in §5 (run in
the copy with `aiken check --env testnet -m 'proof_exunits_frontier.{..}'`,
25 checks, 0 errors); after the split the same selector must still report
`l2_no_op_arm` at 15.27 M / 6.87 B because the reference predicate is kept.

## 10. Ordering and dependencies

- Lands together with [transition-trace-deposit-v1.md](transition-trace-deposit-v1.md):
  both use `final_yield_v1.ak`, `output_summaries_v1.yield`, and the
  `assemble_v1` factoring.
- `ledger_output_descriptor_v1.build_v1` is also reached by
  `network-id/step-01`, `network-id/step-02`, `missing-native-script-utxo/step-03`,
  `step-04` and the out-of-scope `withdrawal-mistag/step-03`; the factoring is
  size-neutral for them (build_v1 keeps its signature) but they must be
  rebuilt in the same blueprint.
- The dispatcher hash changes (new parameter), so
  `route_v1.main`'s `final_validator_script_hashes` changes → route hash →
  catalogue category first step → one catalogue-root re-pin shared with all
  50 plans.
- Shares `state_queue_yield.require_authenticated_zero_yield` and the
  `reference_script_auth_policy_id` plumbing with min-ADA step 02 and the state
  queue; no change to those.

## 11. Risks

- **Budget**: four scripts in the L2 transaction and four in the claim
  transaction; the summaries yield runs `semantic_data_summary_v1` twice per
  output, as today. Aggregate ExUnits are unmeasured until the emulator test
  exists; fallback is to move `validate_l2_one_step_binding` from the
  dispatcher into `l2_replay` (5.0 → ~11 KB, still fits) or to chain (§3.3).
- **Thin margins**: `output_summaries` (13,171) and the dispatcher (~13.1 KB)
  leave ≈ 1.8 KB under the 15,000 target; any growth in
  `script_context_v1`/`cek_data_v1` lands here first. Re-measure on every
  regeneration.
- **ABI churn**: final-4/5 spend redeemer type changes; off-chain must branch
  per final. Proof ABI is unchanged.
- **Cross-yield redeemer reading** is a new pattern in this family (RF-021
  used chained steps); reviewers should check that every read goes through the
  hash returned by `require_authenticated_zero_yield` or a parameter, never a
  redeemer-supplied hash.
- **Compiler behaviour dependency**: sizes rely on the fork not decoding typed
  datum arguments (q05) while decoding typed redeemers (q08). If a future
  compiler decodes datums, every final grows by ~9 KB and the route's redeemer
  check becomes redundant — re-measure before any Aiken pin change.
- **Spec**: GOAL_SPEC §9.1 outputs 4, 6, 9 require the yields to have tests,
  catalogue/deployment records and emulator lifecycle; C52 is unaffected (still
  three transactions per proof).
