# Size-fit plan: `phase_a_native_scripts_item_semantic_v1`

Reads with [00-primer.md](00-primer.md). Component probes are tabulated in
[validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md](validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.md)
§2; this plan adds the item-specific probes and chooses a **withdraw-zero
yield split** (primer pattern 2) because pruning cannot reach the target.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/phase-a-native-scripts-item-semantic-v1.ak` |
| Raw size | **19,501 bytes** (19% over the limit) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId`, `field_preimage_certificate_policy_id: PolicyId` (3) |
| Phase / indices | `PhaseANativeScripts` (resolver 5), semantic index 1, global 11 |
| Machine stage | `stage == 0`, `script_count != 0`: opens field-6 item `control.script_seen` through the §8 door, reads the versioned script header, and either enters the token walk (`language_tag == 0` → `stage: 1`, `item_commitment` derived) or completes a non-native script (`phase_a_native_complete_script_is_exact`: next item, hand-off to `PhaseAScriptPreconditions`, or return to `NativeScripts` for a late continuation) |
| Library entry point | `verify_phase_a_native_item_semantics_v1(pre, transition, door, field_index, item_index, carriage)` → `verify_phase_a_native_item_scan` |
| Redeemer / auxiliary | `VerifyItem { input_index, output_index, transition, field_index, item_index, carriage }`; auxiliary `TransactionFieldChunkWitness { field_index, item_index, carriage }` (shape `[1, 3]`) |
| Rejection reasons | `WitnessScriptHeaderMalformed` (`reject_invalid_field_type`) |
| Role / deployment entry today | none / none |

## 2. Why it is this size

| Probe | Reachable code | Raw bytes | Delta |
| --- | --- | ---: | ---: |
| `p17_door_open` | `p01` + `field_door.open_machine_field_item` + `machine_field_item_count/length/chunk/commitment` | 6,816 | **≈4,600** for the §8 door (`authenticated_whole_field_view`, tiers 1–3, `field_item_extent`, `bounded_item_v1.from_bytes`) |
| `p18_versioned_header` | `native_script_scan_v1.versioned_script_header_v1` | 1,227 | — |
| `p19_complete_script` | `p02` + `phase_a_native_complete_script_is_exact` (NativeScripts trio, `encode_native_scripts_control_v1`, `phase_a_native_to_preconditions_is_exact` → `encode_phase_a_script_preconditions_witness`, `phase_a_native_successor_is_exact`) | 6,128 | **≈4,400** |
| `p03_bound_full` | full binding incl. the continuation trio | 8,941 | ≈5,000 |

Sum of dominators: shell 3.3 + control decode 1.7 + proof-source 2.2 + full
binding 5.0 + door 4.6 + header 1.2 + complete-script 4.4 + successor 2.2 +
rejected 1.6 ≈ 26 KB before sharing → 19,501 after. Unlike the payload steps
the item step **needs** both the decoded transaction (the door) and the
continuation trio (a non-native item under a late continuation returns to
`NativeScripts`), so PA-CARRY/PA-UNDECODED do not apply to its binding.

Whole-validator experiments:

| Build | Script | Raw bytes |
| --- | --- | ---: |
| E3 | `item_native` resolver (only `language_tag == 0` branch, carried binding) | 16,297 |
| E3 | `item_foreign` resolver (only `language_tag != 0` branch, full binding) | 18,400 |
| yield | `p30_item_dispatcher`: shell + control decode + proof-source + full binding + stage/index guards + `require_authenticated_zero_yield` | **11,973** |
| yield | `p31_item_yield` (single role): dispatch + output re-derivation + proof-source + door + header + both successors | 14,700 |
| yield | `p32_item_yield_native` (tag == 0 branch + malformed-header rejection) | **12,548** |
| yield | `p33_item_yield_foreign` (tag != 0 branch, `complete_script_is_exact`) | **13,638** |

## 3. Options considered

- **Prune.** Rejected: the only ABI-neutral prune (E3, split by language tag
  into two resolvers) leaves 16,297 / 18,400 and would grow the
  `prepare_selected` roster from 14 to 15 (`phase_a_native_scripts_v1` literal
  `14`, SDK title list, `validationSemanticResolverIndexV1`, submit shape
  table).
- **Withdraw-zero yield split (chosen).** Dispatcher keeps the roster slot,
  the evidence hash and `continue_winning`; yields carry the door and the
  successor derivation. Measured 11,973 + 12,548 / 13,638.
- **Chaining.** Rejected: a two-hop item step would double the per-item
  transaction count of every native-script walk (C52 cap, §3.3 maturity).
- **Redesign.** Not warranted.

## 4. Chosen design

### 4.1 Validators

| Script | File | Purpose | Parameters |
| --- | --- | --- | --- |
| `phase_a_native_scripts_item_semantic_v1.main.spend` (dispatcher, same title, same roster slot 1) | existing file | `cancel` / `continue_winning` with `semantic_transition_is_valid = and { full binding, stage == 0, script_count != 0, field_index == 6, item_index == control.script_seen, require_authenticated_zero_yield(...) }` | `award_script_hash`, `computation_thread_policy_id`, `reference_script_auth_policy_id` (replaces `field_preimage_certificate_policy_id`, which moves to the yields) |
| `phase_a_native_scripts_item_yields_v1.native.withdraw` | new `validators/fraud-proofs/validation-trace/phase-a-native-scripts-item-yields-v1.ak` | header decodes, `language_tag == 0` → `phase_a_native_successor_is_exact(stage: 1, item_commitment, cursor: payload_offset)`; header `None` → `rejected_successor_is_exact(reject_invalid_field_type)` | `dispatcher_script_hash`, `award_script_hash`, `field_preimage_certificate_policy_id` |
| `phase_a_native_scripts_item_yields_v1.foreign.withdraw` | same file | header `Some` with `language_tag != 0` → `phase_a_native_complete_script_is_exact(pre, post, control, active_count, script_seen + 1, 1)` | same |

Shared library module `lib/midgard/fraud-proofs/validation-trace/phase-a-native-item-yield-v1.ak`
(`midgard/fraud_proofs/validation_trace/phase_a_native_item_yield_v1`):
`pub const native_role: AssetName = "V1VtPhaseANativeItemNativeYield"`,
`pub const foreign_role: AssetName = "V1VtPhaseANativeItemForeignYield"`,
`pub fn unique_dispatch(dispatcher_script_hash, inputs, redeemers) -> (validation_semantic_v1.Datum, SpendRedeemer, OutputReference)`
(the `min_ada/yield.unique_dispatch` shape: exactly one input at the
dispatcher credential, its `Spend` redeemer), and
`pub fn open_item(door, verified, witness_set, control, carriage)` returning
`(item_count, item_length, active_count, header, item)`.

### 4.2 Redeemer ABI delta

`ActionV1.VerifyItem` gains `yield_to_ref_input_index: Int` and `yield_kind:
Int` (0 native, 1 foreign). The **auxiliary** stays
`TransactionFieldChunkWitness { field_index, item_index, carriage }`, so the
evidence hash, `prepare_selected`, the work-witness encodings and the
rejection codes are unchanged. Yields take `_yield_redeemer: Data` (unused,
as `min_ada/step_02.YieldRedeemer`).

### 4.3 Handshake (primer pattern 2, all four points)

1. Dispatcher: `require_authenticated_zero_yield(tx.reference_inputs,
   tx.withdrawals, tx.redeemers, reference_script_auth_policy_id, role,
   yield_to_ref_input_index)` with `role = if yield_kind == 0 { native_role }
   else { foreign_role }`: the indexed reference input carries exactly one
   role NFT under the auth policy with that name, an exact zero withdrawal
   from its script hash exists, and its withdraw redeemer is unique.
2. Yield: `unique_dispatch(dispatcher_script_hash, …)` finds the single
   dispatcher input, reads its inline `Datum`
   (`ct.StepDatum<PreparedValidationResolutionStateV1>`) and spend redeemer;
   `pre = state.resolution.pre_state`, `transition` and `carriage` come from
   that redeemer — the same values the dispatcher hashed against
   `state.evidence_hash`.
3. Yield re-derives the continuation: output `output_index` is at
   `Script(award_script_hash)` with inline datum
   `ct.StepDatum { data: Some(winning_resolution()) }`; then it proves the
   step (door, header, successor) against `transition.claimed_successor`.
4. Parameters: yields carry `dispatcher_script_hash`; the dispatcher carries
   the role names (library constants) and `reference_script_auth_policy_id`.
   Nothing is trusted from the redeemer except indices that are checked.

### 4.4 Security argument

- **Dispatch uniqueness**: `unique_dispatch` requires exactly one input at the
  dispatcher credential, so one zero-withdrawal cannot discharge two threads.
- **Role authentication**: cross-arm substitution (a foreign yield for a
  native item) fails twice — the yield itself refuses (`language_tag != 0`
  guard) and the dispatcher's role name is fixed by `yield_kind`; a foreign
  script pretending to be a yield fails `require_authenticated_zero_yield`'s
  withdrawal-credential match (the reference input's `reference_script` hash
  must equal the withdrawing script hash, which must carry the role NFT).
- **Output re-derivation**: the yield checks the award output and the winning
  datum (point 3), so a yield satisfied for a different dispatcher output is
  impossible; the dispatcher's own `continue_winning` checks the same.
- **Omission**: without the yield the dispatcher's
  `require_authenticated_zero_yield` fails (`expect` on the reference input,
  withdrawal, redeemer); without the dispatcher the yield fails
  `unique_dispatch`. An attacker gains nothing from omitting either: the
  dispatcher proves binding + zero-yield, the yield proves the step; only
  both together satisfy `continue_winning`.
- **What moved**: the door, header and successor derivation moved from the
  resolver body to the yield verbatim; the yield is evaluated in the same
  transaction under the same `pre`/`transition`, so the predicate proven is
  unchanged.

## 5. Size and budget projection

| Script | Today | Projected raw | Method |
| --- | --- | ---: | --- |
| `…item_semantic_v1.main.spend` (dispatcher) | 19,501 | **≈12,000** (p30 11,973 + `yield_kind` field) | measured probe |
| `…item_yields_v1.native.withdraw` | — | **12,548** | measured probe p32 |
| `…item_yields_v1.foreign.withdraw` | — | **13,638** | measured probe p33 |

Per item transaction: dispatcher + one yield referenced ≈ 24.5–25.7 KB, plus
the two minting witnesses if referenced — straddles the first 25,600-byte
`minFeeRefScriptCostPerByte` tier (15 lovelace/byte, ×1.2 above): ≈0.39 ADA
of reference-script fee. Both yields also carry an auth-role NFT (min-Ada as
for `V1FpMinAdaS02TxYield`). ExUnits: two scripts share the 16.5 M / 10 G
budget; the yield adds one `Datum` + `SpendRedeemer` parse (the transition
carries two `ValidationMachineStateV1`s) and a second
`verify_native_tx_proof_source_v1`; the door's `blake2b_256` over the field-6
preimage (≤ 32,768 bytes) is paid once as today. Unmeasured; §9 prints it.
Fallback: the single-role yield (14,700) if two roles are judged too many.

## 6. Off-chain work (none exists today)

- `contracts.ts`: add `reference_script_auth_policy_id` to
  `semanticResolverParameterValues`; this makes `referenceScriptAuthPolicyId`
  mandatory for `BuildValidationTraceDisputeFaultProofContractsParams`
  (optional in `BuildFaultProofContractsParams` today; callers such as
  `submit-init-emulator-validation-dispute.test.ts` line ~172 pass none).
  Build the two yields after the semantic loop with
  `makeWithdrawalValidator(applyBlueprintParams(blueprint, title,
  [itemDispatcher.spendingScriptHash, award.spendingScriptHash,
  fieldPreimageCertificatePolicyId]))` and expose
  `validationTraceDispute.phaseANativeItemYields: { native, foreign }`; add
  the two titles under a new `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.phaseANativeItemYields`.
- Roles: `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` (`demo/midgard-sdk/src/reference-scripts.ts`)
  `"V1 validation-trace phase-A native item native yield": "V1VtPhaseANativeItemNativeYield"`,
  `"… foreign yield": "V1VtPhaseANativeItemForeignYield"`; mirrored in
  `demo/midgard-core/src/deployment-manifest-identity-v1.ts` (entry-name map
  ~line 524 and token-name map ~line 740, with
  `deployment-manifest-identity-v1.test.ts`), `demo/midgard-node/src/deployment-manifest-v1.ts`
  (~line 479: `validationTraceDisputePhaseANativeItemNativeWithdraw` /
  `…ForeignWithdraw`), and `contract-deployment-info.ts`
  `withdrawalDescriptor(...)` rows beside `fraudProofMinAdaStep02TxWithdraw`.
- Deployment entries: dispatcher entry
  `validationTraceDisputePhaseANativeScriptsItemSemantic` (index 1 of the
  phase-A roster, anchor §6) plus the two yield entries above.
- Submit route (`submit.ts` semantic-resolution builder): for `resolverIndex
  === 5 && semanticResolverIndex === 1` add `readFrom([yieldUtxo])`,
  `.withdraw(scriptRewardAddress(network, yield.withdrawalScript), 0n,
  Data.void())`, compute `yield_to_ref_input_index` with
  `requireReferenceInputIndex` and `yield_kind` from the item header (the
  off-chain trace knows the language tag), following
  `demo/midgard-fault-proofs/src/min-ada/submit-step-02-v1.ts`
  (`requireLinearFaultReferenceScriptV1`, `scriptRewardAddress`). The yield
  reward accounts must be registered before first use (emulator pattern
  `registerStateQueueYieldRewardAccountsV1` in `tests/support/emulator/setup-tx.ts`).
- Funding: two extra reference-script publications (each with a role NFT
  mint) in the deployment funding computation
  (`referenceScriptPublicationFundingTarget`).
- Inspection fixtures: `inspect-contracts.test.ts` gains the two withdraw
  validators in its parameterized-validator roster; `zz605` extends its
  full-application check to the yield titles; `zz610` sees two new compiled
  scripts.
- `midgard-validation`: the item step's redeemer builder emits the two new
  fields; `validationSemanticResolverIndexV1` unchanged.

## 7. Emulator scenario tests (none exist today)

`demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-phase-a-item.test.ts`:

- Publication fit for the dispatcher and **both** yields with
  `publishPlainReferenceScriptUtxo` under `withRealL1MaxTxSize`, no
  `oversized`; yields published with their role NFT via the
  `publishMinAdaYieldReferenceScriptsV1` pattern
  (`tests/support/emulator/reference-scripts.ts`) generalised to
  `publishPhaseANativeItemYieldReferenceScriptsV1`.
- Positive lifecycle `buildPhaseANativeItemFixture({ kind: "native" })`
  (frontier on the item step of a native witness script) and `{ kind:
  "foreign" }` (a PlutusV3 witness script, `contains_non_native_script` set)
  through award; both with the two-script transaction ≤ 16,384 bytes.
- Late continuation: fixture where `NativeScripts` re-enters phase A with
  `continuation_cbor != ""` and the item is native (`native` yield) — the
  full binding's continuation trio is exercised in the dispatcher.
- Valid-block negatives at the same frontier for both kinds; substitution
  negatives: foreign yield presented for a native item (refused by the
  yield), reference input at the wrong index, a second dispatcher input in the
  same transaction (`unique_dispatch` refuses), missing withdrawal.
- Cancel/resume on the prepared thread.
- Maximum shape: field 6 at `max_aggregate_field_preimage_bytes` via tier-3
  certificate carriage, item at the last index, malformed header
  (`WitnessScriptHeaderMalformed`).

## 8. Aiken tests

- `phase-a-split-v1.test.ak`: `item_dispatcher_wire_layout_is_pinned`,
  `item_dispatcher_requires_native_role_for_kind_zero`,
  `item_dispatcher_refuses_a_missing_zero_withdrawal` (fail),
  `item_dispatcher_refuses_a_nonzero_withdrawal` (fail),
  `item_dispatcher_refuses_a_foreign_role_nft_on_the_native_slot` (fail),
  `native_yield_enters_the_token_walk`, `foreign_yield_completes_the_script`,
  `foreign_yield_returns_to_native_scripts_under_late_continuation`,
  `native_yield_refuses_a_foreign_header` (fail),
  `foreign_yield_refuses_a_native_header` (fail),
  `yield_refuses_two_dispatcher_inputs` (fail),
  `yield_refuses_a_non_winning_output_datum` (fail),
  `yield_refuses_a_wrong_dispatcher_hash` (fail).
- Library: property `item_yield_predicate_equals_item_scan` — for generated
  stage-0 controls, `dispatcher_predicate && yield_predicate ==
  verify_phase_a_native_item_scan(...)`.

## 9. Verification commands

```bash
cd onchain/aiken && /home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/phase_a_native_scripts_item/.test(v.title))console.log(v.title,Buffer.from(v.compiledCode,"hex").length)'
# expect three scripts: main.spend ≈12,000; native.withdraw ≈12,548; foreign.withdraw ≈13,638; all ≤ 15,000
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m phase_a_split   # §8 tests (13 validator-level + 1 property)
cd demo/midgard-fault-proofs && pnpm test -- tests/submit-init-emulator-validation-dispute-phase-a-item.test.ts tests/zz605-semantic-resolver-arity.test.ts tests/zz610-compiled-script-arity.test.ts tests/inspect-contracts.test.ts
cd ../midgard-core && pnpm test -- tests/deployment-manifest-identity-v1.test.ts
```

## 10. Ordering and dependencies

- Same blueprint regeneration and catalogue-root re-pin as the payload plans
  (all change `phase_a_native_scripts_v1`'s hash list).
- Shares `state_queue_yield.require_authenticated_zero_yield` and the
  role-name registries with the min-Ada yields and any other plan adding
  yields (state-queue, availability challenge, transition-trace finals):
  coordinate the `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` / manifest-identity
  edits in one change.
- Adds the phase-A deployment roster and `resolverIndex === 5` submit branch
  (shared with the anchor plan §6).

## 11. Risks

- **ABI churn**: the item redeemer gains two fields; every off-chain encoder
  for semantic index 1 (`submit.ts`, `midgard-validation`) and the arity
  gates change together. The dispatcher's parameter set changes
  (`field_preimage_certificate_policy_id` → `reference_script_auth_policy_id`),
  which the name-keyed loop handles but which makes
  `referenceScriptAuthPolicyId` mandatory for validation-trace builds.
- **Budget**: two scripts per item transaction; ExUnits unmeasured. If the
  aggregate approaches the §3.3 reserve, the fallback is to drop the yield's
  award-output re-derivation (already enforced by the dispatcher) — not
  recommended.
- **Reward-account registration**: a forgotten registration makes every item
  step unbuildable on a fresh deployment; add it to the deployment manifest
  checks.
- **Margin**: the foreign yield sits 1,362 bytes under the target; the
  single-role fallback would sit 300 under.
