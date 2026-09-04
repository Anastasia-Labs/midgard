# Size-fit plan: `fraud_proofs/transition_trace/deposit_v1.main`

Cites [00-primer.md](00-primer.md). Companion plan:
[transition-trace-accepted-transaction-v1.md](transition-trace-accepted-transaction-v1.md)
(final 4), which defines the shared `final_yield_v1` redeemer, the shared
`output_summaries_v1.yield`, and the `assemble_v1` factoring this plan reuses.
The probe method, floor, and decoder findings (§2.1 there) apply verbatim and
are not repeated.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/transition_trace/deposit_v1.main.spend` (+ `.else`) |
| File | `onchain/aiken/validators/fraud-proofs/transition-trace/deposit-v1.ak` (67 lines) |
| Raw size | **26,172 bytes** (reproduced 2026-09-01 on the `/tmp/size-probe-tt` copy; a byte-identical re-implementation as probe p12 measured 26,144) |
| Applied parameters (4) | `computation_thread_token_policy_id: PolicyId`, `fraud_proof_token_policy_id: PolicyId`, `fraud_proof_token_address: Address`, `hub_oracle: PolicyId` (`contracts.ts` `finalSpecs` row `["deposit", true]`) |
| Family position | Transition-trace **final 5**; `route_v1.route_index` sends `InvalidOneStepTransition { witness: ValidDepositTransition }` here. Terminal step (`common.finalize`) |
| Library entry | `proof.validate_deposit_fault_proof(proof, asset_name, hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index), reference_inputs)` → envelope + `validate_valid_deposit_transition` |
| Witness fields | `trace_proof`, `event_to_step`, `source_membership: RootMembershipProof<DepositId, DepositInfo>`, `event_ref_input_index`, `event_asset_name`, `projected_utxo: LedgerInsertWitness` |
| Role name today | `V1FpTransitionTraceFinal5` |
| Deployment entry today | `fraudProofTransitionTraceDeposit` (`TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES[5]`) |
| Emulator today | Published only with `oversized: true` (`TRANSITION_TRACE_OVERSIZED_REFERENCE_SCRIPT_ENTRIES`); no scenario exercises final 5 |

## 2. Why it is this size

Same copy, toolchain and method as the companion plan; raw bytes, 129-byte
floor. `validate_valid_deposit_transition` does, in order:
`get_authenticated_deposit_reference` (L1 deposit UTxO by NFT), `ledger_outref_key`,
`ledger_value_v1(key.output_index, projected_deposit_output_cbor(...))`,
`apply_insert_witness`, `validate_deposit_one_step_binding`, and five
equalities.

| Probe | What it compiles | Bytes | Net |
| --- | --- | ---: | ---: |
| p04 | wrapper + envelope + `hub.get_datum` (this final's fixed cost) | 2,507 | hub read ≈ 1.0 KB |
| p12 | p04 + the whole `ValidDepositTransition` arm | 26,144 | arm ≈ 23.6 KB |
| p40 | `get_authenticated_deposit_reference` | 1,056 | 0.9 KB |
| p41 | `projected_deposit_output_cbor` (address/value/datum conversion + `encode_midgard_tx_output`) | 2,959 | 2.8 KB |
| p36 | `ledger_output_descriptor_v1.ledger_value_v1` | 16,751 | **16.6 KB — dominates** |
| p42 | p41 + p36 composed as the arm composes them | 18,354 | 18.2 KB |
| p43 | `apply_insert_witness` (MPF non-membership + insert) | 3,348 | 3.2 KB |
| p39 | `validate_deposit_one_step_binding` | 6,370 | 6.2 KB |
| q61 | of p36: `script_context_v1.ledger_output_summaries_v1` | 12,718 | 12.6 KB |
| q60 | of q61: `ledger_output_v1.decode_canonical_output` | 8,662 | 8.5 KB |
| r03 | remainder of `build_v1` given the summaries (`assemble_v1`) + `encode` | 5,095 | 3.6 KB |
| r01 | decode-free `ledger_value_from_decoded_v1` | 14,736 | saves only 2 KB |

Sum of parts ≈ 30 KB against a 23.6 KB arm: the MPF trie, CBOR and hashing
helpers are shared between binding, insert and descriptor code. The descriptor
derivation is the one piece that does not fit any single 15 KB script together
with anything else.

## 3. Options considered

1. **Prune.** The projected output is built on-chain from authenticated L1
   data and encoded canonically, then `ledger_value_v1` decodes it again. The
   decode-free variant measured 14,736 (r01) — still over target on its own and
   only 2 KB smaller, because the canonical-Data scan and the CBOR parsers are
   shared with the summaries. It would also weaken the canonicity round-trip
   the descriptor path performs today. **Rejected.**
2. **Yield split.** One yield holding the arm does not fit even after narrow
   decoding (q57: 27,563) or with the prune (r21: 18,525); a "projection +
   `ledger_value_v1`" yield is 21,193 (r22). It fits when the descriptor
   pipeline is staged: a small **projection** yield commits the projected
   output and its bytes, the shared **summaries** yield derives the three
   `DataSummaryV1` values, and the dispatcher assembles and encodes the
   descriptor, replays the insert, and compares the post root. **Chosen.**
3. **Chaining.** A `continue` step before the final would still hold the
   23.6 KB arm; only `finalize` (~1.3 KB) moves. **Rejected**; fallback only if
   §5's aggregate budget fails.
4. **Redesign.** Not warranted: the arm boundary (one deposit → one insert) is
   right; only the descriptor derivation is heavy.

## 4. Chosen design

### 4.1 New validator list

| Script (blueprint title) | Purpose | Kind / params | Role | Measured |
| --- | --- | --- | --- | ---: |
| `fraud_proofs/transition_trace/deposit_v1.main` (rewritten) | dispatcher: `cancel`; `finalize`; envelope; `validate_deposit_one_step_binding`; two yield handshakes; `assemble_v1` + `encode` of the committed projection; `projected_utxo.key/value` checks; `apply_insert_witness`; post-root mismatch | spend; `(computation_thread_token_policy_id, fraud_proof_token_policy_id, fraud_proof_token_address, reference_script_auth_policy_id)` — **`hub_oracle` moves to the projection yield** | `V1FpTransitionTraceFinal5` (unchanged) | 12,226 [s30] |
| `fraud_proofs/transition_trace/deposit_yields_v1.projection` | authenticate the L1 deposit event, build the projected `MidgardTxOutput`, commit it and its canonical bytes | withdraw; `(dispatcher_script_hash, hub_oracle: PolicyId)` | `V1FpTtF5ProjectionYield` | 5,975 [r22a] |
| `fraud_proofs/transition_trace/output_summaries_v1.yield` (shared with final 4, second application) | `ledger_output_summaries_v1` over the committed bytes | withdraw; `(commit_yield_script_hash = projection yield hash)` | `V1FpTtF5SummariesYield` | 13,171 [r12b] |

Measured intermediate shapes that informed the split: dispatcher with binding
and handshakes only 5,913 (r20); plus the insert replay 6,587 (r20b); plus
assembly 12,226 (s30). The projection yield's redeemer is `OpenedOutputsV1`
with `spend_input_keys = []`, `outputs = [output]`, `output_cbors = [bytes]`,
so the summaries yield needs no deposit-specific variant.

Library changes: those listed in the companion plan (`assemble_v1` extraction,
`final_yield_v1.ak`), plus in `proof.ak` a `pub fn projected_deposit_output(
deposit_info, deposit_value, deposit_policy_id, event_asset_name) ->
MidgardTxOutput` factored out of `projected_deposit_output_cbor` (which becomes
`encode_midgard_tx_output(projected_deposit_output(...))`), and `pub` on
`get_authenticated_deposit_reference`, `validate_deposit_one_step_binding`,
`apply_insert_witness`. `validate_valid_deposit_transition` stays as the
reference predicate for equivalence tests.

### 4.2 Datum / redeemer ABI deltas

- Datum and `TransitionFaultProof`/`ValidDepositTransition` unchanged.
- Final-5 spend redeemer: `final_v1.Args` → `final_yield_v1.Args`
  (`yield_ref_input_indices: List<Int>`, two entries: projection, summaries).
  `hub_ref_input_index` stays in `Args` because the projection yield reads it
  from the dispatcher's spend redeemer (narrowly, field 2 of the `Continue`
  payload).
- New withdraw redeemers: `OpenedOutputsV1` (projection), `OutputSummariesV1`
  (summaries).
- Parameter change: `deposit_v1.main` loses `hub_oracle` and gains
  `reference_script_auth_policy_id`; `contracts.ts` `finalSpecs` row
  `["deposit", true]` becomes a dispatcher row with the auth policy, and the
  projection yield is applied with `[dispatcher.spendingScriptHash,
  hubOraclePolicyId]`.

### 4.3 Exact handshake

Dispatcher `Continue`:

1. `finalize(...)` as today.
2. `validate_transition_fault_proof_envelope(proof, asset_name)`.
3. `expect InvalidOneStepTransition { witness: ValidDepositTransition {
   trace_proof, event_to_step, source_membership, projected_utxo, .. } } =
   proof.fault`.
4. `expect validate_deposit_one_step_binding(header, trace_proof,
   event_to_step, source_membership)` (trace membership, event-to-step
   membership, deposits-root membership, event-key and phase equalities).
5. `expect [i_proj, i_sum] = yield_ref_input_indices`;
   `proj_hash = require_authenticated_zero_yield(…, projection_role, i_proj)`;
   `sum_hash = require_authenticated_zero_yield(…, summaries_role, i_sum)`.
6. Read `OpenedOutputsV1` at `Withdraw(Script(proj_hash))` and
   `OutputSummariesV1` at `Withdraw(Script(sum_hash))` via
   `utils.get_unique_withdraw_redeemer`; `expect [output] = outputs`,
   `expect [output_cbor] = output_cbors`, `expect [(s1, s2, s3)] = summaries`.
7. `expect Some(descriptor) = assemble_v1(source_membership.key.output_index,
   output, output_cbor, s1, s2, s3)`.
8. `projected_utxo.key == ledger_outref_key(source_membership.key)` and
   `projected_utxo.value == ledger_output_commitment_v1.encode(descriptor)` and
   `apply_insert_witness(trace_proof.value.pre_utxos_root, projected_utxo) !=
   trace_proof.value.post_utxos_root`.

`projection` yield (redeemer `OpenedOutputsV1`): `dispatched(dispatcher_hash,
inputs, redeemers)` (unique dispatcher input, unique `Continue` spend
redeemer); `one_step_witness(fault, 3)` → decode `source_membership:
RootMembershipProof<DepositId, DepositInfo>`, `event_ref_input_index`,
`event_asset_name`, `projected_utxo`; `hub_datum = hub.get_datum(reference_inputs,
hub_oracle, hub_ref_input_index)` (index read narrowly from the spend
redeemer); `reference = get_authenticated_deposit_reference(reference_inputs,
hub_datum.deposit, event_asset_name, event_ref_input_index)` (requires the
deposit NFT and the `OptimisticDatum` shape); `output =
projected_deposit_output(source_membership.value, reference.value,
hub_datum.deposit, event_asset_name)`; then `reference.event.id ==
source_membership.key`, `reference.event.info == source_membership.value`,
`projected_utxo.key == ledger_outref_key(source_membership.key)`,
`redeemer.spend_input_keys == []`, `redeemer.outputs == [output]`,
`redeemer.output_cbors == [encode_midgard_tx_output(output)]`.

`output_summaries` yield: as in the companion plan, parameterised with the
projection yield's hash; runs `ledger_output_summaries_v1` (including
`decode_canonical_output`'s canonicity round-trip and the
`max_output_canonical_cbor_bytes` cap) over the committed bytes.

### 4.4 Security argument

- **Dispatch uniqueness.** `dispatched` in the projection yield requires
  exactly one input at the dispatcher credential and exactly one spend
  redeemer for it; the summaries yield binds to the unique projection
  withdrawal (`get_unique_withdraw_redeemer`), which is itself bound to the one
  thread.
- **Role authentication.** Both yields enter only through
  `require_authenticated_zero_yield` with `V1FpTtF5ProjectionYield` /
  `V1FpTtF5SummariesYield`; the dispatcher reads their redeemers at the
  returned script hashes, never at a redeemer-supplied hash.
- **Cross-arm substitution.** The projection yield `expect`s witness tag 3
  (`ValidDepositTransition`) and the dispatcher `expect`s the same
  constructor; final-4 yields carry different roles and tags. The final-4
  summaries instance is bound to the L2 open yield hash by parameter and cannot
  be presented here.
- **Re-derivation.** Nothing the prover supplies is trusted: the projected
  output is rebuilt from the NFT-authenticated L1 deposit UTxO and the
  MPF-authenticated `DepositInfo`; the bytes are re-encoded and compared; the
  summaries are re-derived from those bytes; the descriptor is re-assembled and
  re-encoded in the dispatcher and compared with `projected_utxo.value`; the
  key is re-derived with `ledger_outref_key`. The insert then uses only the
  verified `(key, value)`, exactly as `validate_valid_deposit_transition` does
  today with `checked_projected_utxo`.
- **Omission.** Omit the projection yield: the handshake fails and there is no
  committed output to assemble. Omit the summaries yield: same. Neither can be
  skipped by the redeemer because both roles are hard-coded in the dispatcher.
  What an attacker would gain if the dispatcher did not compare
  `projected_utxo.value` with the assembled descriptor: inserting an arbitrary
  value under the right key yields an arbitrary post root, so any honest
  deposit step could be "proved" wrong — this equality is the load-bearing
  check and it is in the dispatcher, not in a yield.
- **Malformed data.** The route validator's typed redeemer decodes the whole
  proof; the yields still narrow-decode what they read.

## 5. Size and budget projection

| Script | Raw bytes (measured prototype) | ≤ 15,000 |
| --- | ---: | :-: |
| `deposit_v1.main` (dispatcher) | 12,226 | yes (margin 2.8 KB) |
| `deposit_yields_v1.projection` | 5,975 | yes |
| `output_summaries_v1.yield` (final-5 application) | 13,171 | yes (margin 1.8 KB) |

Referenced bytes in the final-5 transaction: 12.2 + 6.0 + 13.2 + mint witnesses
4.8 (`computation_thread.mint` 3,979, `fraud_proof.mint` 845) ≈ **36.2 KB** →
second `minFeeRefScriptCostPerByte` tier (≈ 0.59 ADA of reference-script fee
at 15 lovelace/byte, ×1.2 above 25 KiB). Today: 26.2 + 4.8 = 31.0 KB, same
tier. The hub oracle and the deposit event UTxO are datum-only reference
inputs and do not count.

Execution: the projected `MidgardTxOutput` is built once (projection yield)
and re-read as Data by the dispatcher (decode ≈ p34 scale, 1.1 KB of code).
Extra work over today's arm: two `dispatched`/redeemer scans, one
`encode_midgard_tx_output` (already performed today), Data equality on a single
output, one narrow decode of the deposit witness, and two
`require_authenticated_zero_yield` scans.

Today's arm at the datum-width frontier, measured on the copy with
`aiken check --env testnet -m 'proof_exunits_frontier.{..}'` (pinned fork;
`_fixture_only` rows are the fixture construction the test pays):

| Test | mem | cpu | Arm ≈ (row − fixture) |
| --- | ---: | ---: | --- |
| `deposit_arm_datum_absent` / `_fixture_only` | 12.77 M / 6.21 M | 5.77 B / 2.83 B | 6.56 M mem, 2.94 B cpu |
| `deposit_arm_datum_32_bytes` / `_fixture_only` | 14.58 M / 7.15 M | 6.48 B / 3.20 B | 7.43 M mem, 3.28 B cpu |
| `deposit_arm_datum_256_bytes` / `_fixture_only` | 15.95 M / 7.86 M | 7.00 B / 3.46 B | 8.09 M mem, 3.54 B cpu |
| `deposit_arm_rejects_arbitrary_projected_value` | 12.73 M | 5.75 B | (neutralisation selector) |

Projection: ~8.1 M memory at the 256-byte datum rung plus on the order of 1 M
for the split's plumbing stays inside the 13,200,000 basis, but the margin at
wider datums is the thinnest in this family; the emulator maximum-shape test
(§7) is the gate. Per-yield ExUnits were not measured (no transaction-level
probe).

## 6. Off-chain work

Exists today: dispatcher-only wiring (`finalSpecs` `["deposit", true]`, role
`V1FpTransitionTraceFinal5`, entry `fraudProofTransitionTraceDeposit`,
`submitTransitionTraceFinalV1` with the hub oracle reference input and
`additionalReferenceInputs` for the deposit event UTxO). Does not exist: any
yield, role, or withdraw route for this final.

1. **SDK contracts**: `finalSpecs` row `deposit` → parameters
   `[computationThread.policyId, fraudProof.policyId, fraudProofTokenAddressData,
   referenceScriptAuthPolicyId]`; build `projection` with
   `[dispatcher.spendingScriptHash, hubOraclePolicyId]` and a second
   `output_summaries_v1.yield` application with
   `[projection.withdrawalScriptHash]`; `transitionTrace.yields` gains
   `depositProjection`, `depositSummaries`. Arity tests (`zz605`, `zz610`).
2. **Roles**: `"V1 fraud-proof transition-trace final-5 projection yield":
   "V1FpTtF5ProjectionYield"`, `"V1 fraud-proof transition-trace final-5
   summaries yield": "V1FpTtF5SummariesYield"` in
   `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES`, `midgard-core`
   `deployment-manifest-identity.ts`, node `deployment-manifest.ts`,
   node `transactions/reference-scripts.ts` (`manifestReferenceScriptTarget`),
   node `commands/contract-deployment-info.ts` (`withdrawalDescriptor`); Aiken
   constants identical.
3. **Deployment entries**: `fraudProofTransitionTraceDepositProjectionWithdraw`,
   `fraudProofTransitionTraceDepositSummariesWithdraw` in
   `DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES`, `runtime.ts` (separate yield list;
   the nine-entry step list is unchanged), `inspect-contracts.ts`, watcher
   `proof-thread-indexer.ts` (`stepCount` 9 unchanged), and
   `da-committee-node/tests/fixtures/da-contract-deployment-info.json`.
4. **Submit route** (`transition-trace/submit.ts`, `finalIndex === 5`): resolve
   the two yield reference scripts, `readFrom` them, two zero `withdraw`s with
   `OpenedOutputsV1` / `OutputSummariesV1` redeemers, `final_yield_v1.Args`
   with `yield_ref_input_indices`. The redeemer content is computed from the
   authenticated deposit event already fetched for `additionalReferenceInputs`
   (`fetch.ts`) and the `DepositInfo` in the proof: project the output with the
   `midgard-core` canonical codec (the same projection `detect.ts` uses to
   decide the fault), and take the three `DataSummaryV1` fields from
   `buildCanonicalMidgardLedgerOutputMaterialV1(...).descriptorCbor`. Reward
   accounts for both yield scripts registered at deployment, as for min-ADA.
5. **Funding**: two more reference-script publications (~6 KB and ~13 KB raw).
6. **Codecs**: none for the proof; `OpenedOutputsV1`/`OutputSummariesV1`
   schemas shared with final 4.

## 7. Emulator scenario tests

Exists today: the two suites named in the companion plan publish
`fraudProofTransitionTraceDeposit` with `oversized: true` and never route to
final 5. The only executable coverage of the arm is Aiken:
`proof.test.ak` `accepts_valid_deposit_transition_fault`,
`rejects_valid_deposit_transition_with_arbitrary_projected_output`,
`rejects_valid_deposit_transition_with_wrong_network_id`,
`rejects_deposit_transition_replaying_full_output_bytes`, and the
`proof-exunits-frontier.test.ak` `deposit_arm_datum_*` sweep and
`deposit_arm_rejects_arbitrary_projected_value`.

Removing the `oversized` marker: both existing suites then assert the final-5
dispatcher publishes with a positive `l1ByteMargin` (they publish all nine step
entries); the new yields are published by
`publishTransitionTraceYieldReferenceScriptsV1` without `oversized`.

Add `submit-init-emulator-transition-trace-final5.test.ts`:

- *Publication fit*: dispatcher, projection, final-5 summaries all publish;
  record `completeSignedBytes`.
- *Positive lifecycle*: fixture `buildValidDepositTransitionFixture` in
  `tests/support/submit-init-emulator-fixtures.ts`: a deposit event UTxO
  minted under the harness deposit policy with an `OptimisticDatum` whose
  `DepositInfo` matches the block's `deposits_root` leaf (port of the Aiken
  `deposit_reference_input`/`sample_deposit_info` fixtures), a trace step whose
  `post_utxos_root` differs from the honest insert; init → route → final 5 with
  the two withdrawals → fraud-proof token → `submitRemoveFraudulentBlock`.
- *Valid-block negative at the same frontier*: honest `post_utxos_root` →
  dispatcher fails at step 8; forged `projected_utxo.value` → dispatcher fails
  the descriptor equality; wrong `event_asset_name` → projection yield fails.
- *Cancel*: route to final 5 then `ct.Cancel`.
- *Maximum supported shape*: the datum-width frontier used by
  `deposit_arm_datum_256_bytes` and a deposit value at the C22 5,000-byte
  Cardano Value boundary (the descriptor's `max_cardano_value_cbor_bytes`);
  assert aggregate memory ≤ 13,200,000 and CPU ≤ 10,000,000,000 from
  `localUPLCEval`, and the transaction ≤ 16,384 bytes.

## 8. Aiken tests

In `validators/fraud-proofs/transition-trace/yields-v1.test.ak` (shared file
with final 4):

- Positive: dispatcher + projection + summaries accept the
  `accepts_valid_deposit_transition_fault` fixture and every
  `deposit_arm_datum_*` rung.
- Equivalence: `validate_valid_deposit_transition` accepts iff the split
  accepts, over the four existing deposit fixtures (including the
  wrong-network-id and full-output-replay rejections).
- Substitution: projection yield role swapped with the final-4 open yield →
  fail; final-4 summaries instance presented → fail (parameter binds the
  commit hash); an L2 thread presented to the projection yield → tag 3 check
  fails.
- Omission: each yield omitted → handshake fails.
- Forgery: `OpenedOutputsV1` with a different output / bytes / non-empty
  `spend_input_keys` → projection fails; altered summary → summaries yield
  fails; altered `projected_utxo.value` or `.key` → dispatcher fails.
- Deposit-specific: reference input without the deposit NFT, mismatched
  `event.id`/`event.info`, `l2_network_id` outside {0,1} (abort in
  `deposit_address_to_midgard`), bignum `l2_datum` (descriptor `None` →
  `assemble_v1`/summaries fail closed, matching today's `expect Some`).
- Multiplicity: two dispatcher inputs → `dispatched` fails.

## 9. Verification commands

```bash
cd onchain/aiken
aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/transition_trace\/(deposit|output_summaries)/.test(v.title)&&!/\.else$/.test(v.title))console.log(v.title,Buffer.from(v.compiledCode,"hex").length)'
# expect: deposit_v1.main.spend ≤ 12,500; deposit_yields_v1.projection.withdraw ≤ 6,200; output_summaries_v1.yield.withdraw ≤ 13,300
aiken check --env testnet -m transition_trace
aiken check --env testnet -m yields_v1
aiken check --env testnet -m proof_exunits_frontier      # deposit_arm_datum_* rows unchanged (baseline below)
cd ../../demo
pnpm --filter @al-ft/midgard-core test -- deployment-manifest-identity-v1
pnpm --filter @al-ft/midgard-fault-proofs test -- zz605 zz610
pnpm --filter @al-ft/midgard-fault-proofs test -- submit-init-emulator-transition-trace   # existing 5 scenarios + new final5 file
pnpm --filter @al-ft/midgard-node test -- contract-deployment-info
```

Baseline ExUnits of today's arm are in §5 (`aiken check --env testnet -m
'proof_exunits_frontier.{..}'`, 25 checks, 0 errors on the copy). After the
split the same selector must report the `deposit_arm_datum_*` rows unchanged,
because `validate_valid_deposit_transition` is kept as the reference predicate.

## 10. Ordering and dependencies

- Must land with the final-4 plan: shared `final_yield_v1.ak`,
  `output_summaries_v1.yield`, `assemble_v1` factoring, the yield test file,
  and the same off-chain tables.
- `ledger_output_descriptor_v1.build_v1` callers (`network-id/step-01`,
  `step-02`, `missing-native-script-utxo/step-03`, `step-04`,
  `withdrawal-mistag/step-03`) rebuild unchanged in the same blueprint.
- Dispatcher hash changes (parameters) → `route_v1` hash → catalogue
  first-step → the single shared catalogue-root re-pin.
- The projection yield reads the hub oracle; if the hub-oracle datum layout
  changes (`hub.get_datum` field order), this yield and `l1_event_v1` change
  together.

## 11. Risks

- **Budget**: three scripts in one transaction; the summaries yield runs the
  semantic summaries once (as today). Unmeasured until the emulator test; the
  fallback is to move `apply_insert_witness` + the post-root compare into the
  projection yield (6.0 → ~9.3 KB) if the dispatcher's CPU is the constraint.
- **Margins**: summaries 13,171 and dispatcher 12,226 leave 1.8–2.8 KB;
  growth in `script_context_v1`/`cek_data_v1`/`ledger_output_commitment_v1`
  lands here first.
- **ABI churn**: final-5 spend redeemer and parameter list change; the
  off-chain final builder must branch on `finalIndex`.
- **Hub parameter relocation**: `hub_oracle` moving from the dispatcher to the
  projection yield changes which script authenticates the hub datum; the
  dispatcher no longer sees `hub_datum.deposit` — acceptable because the
  dispatcher only consumes the yield's re-derived output, but reviewers should
  confirm no other check in the arm depended on the hub datum (today: only
  `hub_datum.deposit` for the NFT policy and the value subtraction, both inside
  the projection).
- **Compiler behaviour dependency**: as in the companion plan (typed datum not
  decoded, typed redeemer decoded).
- **Spec**: GOAL_SPEC §9.1 outputs 4, 6, 9 for the new yields; Q47/Q39
  deposit-fidelity rows unaffected; C52 unchanged (three transactions).
