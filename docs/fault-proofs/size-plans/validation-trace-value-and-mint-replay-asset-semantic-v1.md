# `value_and_mint_replay_asset_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md). This plan also defines the **shared
asset-fold yield** used by the `output-asset` and `mint-asset` plans, and the
**library arm split** used by every plan in the value-and-mint group; those
plans reference this file rather than repeating it.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/value_and_mint_replay_asset_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/value-and-mint-replay-asset-semantic-v1.ak` |
| Raw size (2026-09-01 build) | 22,000 bytes (applied 22,046 per the #634 note in `demo/midgard-fault-proofs/src/validation-dispute/submit.ts`) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` |
| Phase / resolver index | `ValueAndMint`, resolver 12 (`validation_resolution_v1.resolver_index`) |
| Semantic index (arm) | 3 of 11 in the `value_and_mint_v1` prepare group (`prepare_selected(ValueAndMint, hashes, 11, …)`); global slot `validationSemanticResolverGlobalIndexV1(12, 3)` |
| Library entry point | `validation_machine_v1.verify_value_and_mint_replay_asset_semantics_v1` → `value_and_mint_stage_two` with `ValueInputAssetWitness` |
| Redeemer action | `VerifyReplayAsset { input_index, output_index, transition, source_kind, key, next_schedule_hash, descriptor_cbor, asset_index, policy_id, asset_name, quantity, asset_peaks, asset_siblings, mutation }` |
| Role name today | none — semantic resolvers are hash-checked plain reference scripts, no auth-role NFT |
| Deployment entry today | `validationTraceDisputeValueAndMintReplayAssetSemantic` (`VALIDATION_VALUE_AND_MINT_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1[3]`, submit.ts ~line 943) |
| SDK title key | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.valueAndMintReplayAsset` (`demo/midgard-sdk/src/fraud-proof/contracts.ts:468`) |

What the step proves: stage 2 of the value-and-mint control with
`replay_asset_cursor > 0` — fold asset `asset_index` of the replayed input's
authenticated descriptor into the value accumulator (`+quantity`), or take the
exact `E_ASSET_COUNT` rejection when the accumulator's distinct-asset cap
(`ledger_output_v1.max_distinct_asset_count = 16,384`) is exceeded.

## 2. Why it is this size

All measurements: copy of `onchain/aiken` at `/tmp/size-probe-vam`, pinned
fork `v1.1.23-org-5adf7837`, `aiken build --env testnet`, raw unapplied
`compiledCode` bytes. Probe validators were exact copies of the real
resolver shape (same `cancel` arm, same `continue_winning` call) with the
semantic predicate swapped; "rich" probes carry one extra
`{blob, n, peaks, sibs, mutation}` redeemer so deltas isolate one function.

### 2a. Building blocks (rich-redeemer scaffold)

| Probe | Reachable addition | Raw bytes | Δ |
| --- | --- | ---: | ---: |
| p00 | scaffold: `cancel`, `continue_winning`, redeemer/datum parse, predicate `True` | 3,810 | — |
| p01 | + `value_and_mint_control_from_witness` (incl. `native_scripts_control_from_witness`, `value_accumulator_from_cbor`) | 5,908 | +2,098 |
| p02 | + `value_and_mint_verified_body_v1` (`verify_native_tx_proof_source_v1`, `native_tx_proof_commitment_v1`, `hash_validation_context`, `native_scripts_control_is_well_formed`, `encode_value_and_mint_control_v1`) | 11,147 | +5,239 |
| p03 | real `verify_value_and_mint_begin_semantics_v1` (sanity) | 12,070 | stage-zero +923 |
| p04 | p02 + `ledger_output_commitment_v1.decode` | 12,797 | +1,650 |
| **p05** | **p02 + `apply_value_asset_mutation`** (`mpf_proof_v1.update_root`, `insert_root`, `proof_has_at_most_steps`) | **16,535** | **+5,388** |
| p06 | p02 + `decode` + `verify_asset_membership` | 13,599 | +2,452 (membership beyond decode +802) |
| p07 | p02 + `validation_merkle_v1.verify_membership` | 11,655 | +508 |
| p08 | p02 + `rejected_successor_is_exact` | 11,618 | +471 |
| p09 | p02 + `resolved_input_accumulator_successor` + `resolution_schedule_node_hash` | 11,305 | +158 |
| p10 | p02 + `output_meets_min_ada_v1` + `script_proof_v1.output_descriptor_leaf_hash` | 11,249 | +102 |
| p11 | p02 + `encode_ledger_delta_witness_v1` | 11,502 | +355 |
| p12 | p02 + `value_and_mint_successor_is_exact` | 11,473 | +326 |
| p13 | p02 + `mint_asset_leaf_hash` | 11,260 | +113 |
| p14 | real `verify_value_and_mint_replay_finish_semantics_v1` on the rich redeemer | 21,690 | — |

### 2b. Whole-resolver prototypes (exact validator shape)

| Prototype | Raw bytes | Fits ≤ 15,000 |
| --- | ---: | :-: |
| `pr_replay_asset` — arm-only predicate (stage-2 asset arm, nothing else reachable) | 20,995 | no |
| `ds_replay_asset` — arm-only predicate with **only** `apply_value_asset_mutation` delegated to a yield + `require_authenticated_zero_yield` | 16,273 | no |
| `ds2_replay_asset` — arm-only predicate with descriptor decode + asset membership + mutation delegated to the asset-fold yield | **13,674** | yes |
| `ys_asset_mutation_yield` — mutation-only yield (3 params) | 6,179 | yes |
| `ys2_asset_fold_yield` — decode + `verify_asset_membership` + mutation yield (3 params) | **10,949** | yes |

Reading: the resolver today reaches the whole of `value_and_mint_stage_two`
(all three arms) plus its own arm's `decode` (1,650), `verify_asset_membership`
(802), `apply_value_asset_mutation` (5,388) and `rejected_successor_is_exact`
(471) on top of the 11.1k verified-body base. Pruning the sibling arms saves
only ~1,000 bytes because this arm *is* the heavy one; the mpf mutation alone
is 36 % of the target budget.

## 3. Options considered

| Option | Verdict | Reason |
| --- | --- | --- |
| 1. Prune (arm split only) | rejected as sufficient, **kept as a prerequisite** | 20,995 measured; the arm itself carries 8.3k of decode/membership/mutation |
| 2a. Yield: mutation only | rejected | dispatcher measured 16,273 (> 15,000, and > 16,193 borderline once applied) |
| 2b. **Yield: asset fold (decode + membership + mutation)** | **chosen** | dispatcher 13,674, yield 10,949; one reference input + one zero withdrawal; shared with output-asset and mint-asset |
| 2c. Yield: move `value_and_mint_verified_body_v1` out instead | rejected | dispatcher would stay ≈15,750 (20,995 − 5,239) and the yield would need control decode + pre-state binding (≈10k) — no better split, worse trust surface |
| 3. Chain (two transactions per asset step) | rejected | doubles proof-transaction count for the asset ladder (up to 16,384 asset steps per output, C52 cap) with no budget need — the single-transaction predicate is already in budget today |
| 4. Redesign the arm boundaries | rejected | the eleven-kind partition is already the right granularity (R5 item 1); the only heavy sub-function is a pure function that yields cleanly |

## 4. Chosen design

### 4a. Library arm split (shared by all eight plans; ABI-neutral)

In `onchain/aiken/lib/midgard/validation-machine-v1.ak` split each stage body
into one function per arm and make the aggregate stage dispatch to them:

```
fn value_and_mint_stage_two_finish_arm(pre, witness, control) -> Bool
fn value_and_mint_stage_two_input_arm(pre, witness, control, source_kind, key, next_schedule_hash, value) -> Bool
fn value_and_mint_stage_two_asset_arm(pre, witness, control, source_kind, key, next_schedule_hash, descriptor_cbor, asset_index, policy_id, asset_name, quantity, asset_peaks, asset_siblings, mutation) -> Bool
fn value_and_mint_stage_three_finish_arm / _descriptor_arm / _asset_arm
fn value_and_mint_stage_four_finish_arm / _asset_arm
```

`value_and_mint_stage_two/three/four` keep their signatures: they test the
cursor discriminators, `expect` the auxiliary constructor (or
`auxiliary == NoAuxiliaryWitness` for the finish arm) and call the arm, so
`verify_value_and_mint` (the aggregate route behind
`verify_value_and_mint_one_step_v1`) is unchanged. Each per-kind
`verify_value_and_mint_*_semantics_v1` keeps its control decode,
`value_and_mint_verified_body_v1`, stage and cursor pins, and calls **its arm
directly** instead of the stage. Nothing about what a resolver proves changes;
the existing `value_and_mint_*_route_agrees_with_the_aggregate` and
`value_and_mint_kinds_partition_the_value_and_mint_step_space` tests in
`lib/midgard/validation-machine-v1.test.ak` guard the refactor. The probe
functions `probe_vam_*` were verbatim copies of these arm bodies, so the
`pr_*` sizes in §2b are the arm-split sizes.

### 4b. New validator list

| Validator | Purpose | File | Params |
| --- | --- | --- | --- |
| `value_and_mint_replay_asset_semantic_v1.main.spend` (**dispatcher**, same title) | control decode, verified body, stage-2 asset-arm pins, yield handshake, `continue_winning` | existing file | `award_script_hash`, `computation_thread_policy_id`, **`reference_script_auth_policy_id: PolicyId`** |
| `value_and_mint_asset_fold_yield_v1.main.withdraw` (**shared yield**, new) | descriptor decode + asset membership + mpf mutation for the three asset dispatchers | `validators/fraud-proofs/validation-trace/value-and-mint-asset-fold-yield-v1.ak` | `replay_asset_dispatcher_script_hash`, `output_asset_dispatcher_script_hash`, `mint_asset_dispatcher_script_hash: ScriptHash` |

New library module `lib/midgard/validation-value-and-mint-yield-v1.ak`
(mirrors `lib/midgard/fraud-proofs/min-ada/yield.ak`):

```
pub const asset_fold_role: AssetName = "V1VtVamAssetFoldYield"
pub fn unique_asset_fold_dispatch(replay_hash, output_hash, mint_hash, inputs, redeemers) -> (ScriptHash, AssetFoldClaimV1)
```

Types and the yield predicate live in `validation-machine-v1.ak` because
`apply_value_asset_mutation` is private there:

```
pub type AssetDescriptorClaimV1 { descriptor_cbor: ByteArray, asset_index: Int, asset_peaks: List<FrontierPeak>, asset_siblings: List<ByteArray>, asset_count: Int }
pub type AssetFoldClaimV1 {
  policy_id: ByteArray, asset_name: ByteArray, quantity: Int,          // signed as the auxiliary carries it
  mutation: ValueAssetMutationWitnessV1,
  pre_value_accumulator: ValueAccumulatorV1,                            // must equal control.value_accumulator
  outcome: ValueAccumulatorUpdateV1,                                    // Updated(next) | AssetLimitExceeded; MutationInvalid refused
  descriptor: Option<AssetDescriptorClaimV1>,                           // Some for replay/output asset, None for mint asset
}
pub fn verify_asset_fold_claim_v1(claim: AssetFoldClaimV1, negate_quantity: Bool) -> Bool
pub fn verify_value_and_mint_replay_asset_dispatch_semantics_v1(pre, witness, source_kind, key, next_schedule_hash, claim) -> Bool
```

### 4c. Redeemer ABI delta (this contract)

```
VerifyReplayAsset {
  claim: AssetFoldClaimV1,          // NEW, must be field 0 (the yield reads it positionally)
  input_index: Int,
  output_index: Int,
  transition: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  yield_to_ref_input_index: Int,    // NEW
}
```

`descriptor_cbor, asset_index, policy_id, asset_name, quantity, asset_peaks,
asset_siblings, mutation` move into `claim` (no duplication on the wire). The
auxiliary `ValueInputAssetWitness` is rebuilt from `claim` + action fields, so
the one-step **evidence hash, the prepare validator and
`prepare_semantic_resolution` are unchanged**. The datum (`validation_semantic_v1.Datum`)
is unchanged. `ct.Cancel` is unchanged.

### 4d. Exact handshake

Dispatcher `main.spend`, `ct.Continue(VerifyReplayAsset {...})`:

1. `let yield_hash = require_authenticated_zero_yield(reference_inputs, withdrawals, redeemers, reference_script_auth_policy_id, asset_fold_role, yield_to_ref_input_index)` — bind and **use** the result (`yield_hash != ""` inside the final `and`); never `let _ =` (see the discarded-binding hazard in §11).
2. `expect Some(d) = claim.descriptor`; rebuild `ValueInputAssetWitness { source_kind, key, next_schedule_hash, descriptor_cbor: d.descriptor_cbor, asset_index: d.asset_index, policy_id: claim.policy_id, asset_name: claim.asset_name, quantity: claim.quantity, asset_peaks: d.asset_peaks, asset_siblings: d.asset_siblings, mutation: claim.mutation }` as `auxiliary_data: Data`.
3. `continue_winning(ValueAndMint, award_script_hash, computation_thread_policy_id, datum, input_index, output_index, transition, auxiliary_data, verify_value_and_mint_replay_asset_dispatch_semantics_v1(pre, transition, source_kind, key, next_schedule_hash, claim), own_out_ref, tx)`.

`verify_value_and_mint_replay_asset_dispatch_semantics_v1` (measured as
`probe_vam_replay_asset_dispatch2_v1`):

- `control = value_and_mint_control_from_witness(witness.work_witness_cbor)`; `value_and_mint_verified_body_v1(pre, witness, control)` is `True`;
- `control.stage == 2`, `replay_remaining_schedule_hash != empty`, `replay_asset_cursor > 0`;
- **`control.value_accumulator == claim.pre_value_accumulator`**;
- `source_kind == 0`, `resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash`, `control.replay_cursor < native_control.resolved_input_count`;
- **`blake2b_256(d.descriptor_cbor) == control.replay_value_hash`**, `d.asset_index == control.replay_asset_cursor - 1`;
- `when claim.outcome`: `ValueAccumulatorAssetLimitExceeded -> rejected_successor_is_exact(pre, witness.claimed_successor, reject_asset_count)`; `ValueAccumulatorMutationInvalid -> False`; `ValueAccumulatorUpdated(next) ->` successor exact with `complete_value_input_replay(...)` when `control.replay_asset_cursor == d.asset_count`, else `replay_asset_cursor + 1`.

Yield `main.withdraw(_redeemer: Data, _credential, tx)`:

1. `unique_asset_fold_dispatch`: exactly one input whose payment credential is `Script(h)` with `h ∈ {replay, output, mint dispatcher hash}`; the `Spend(out_ref)` redeemer of that input; decode `Constr 1 [Constr _ [claim_data, ..]]` (`ct.Continue(action)`, claim at field 0); `expect claim: AssetFoldClaimV1 = claim_data`. Returns `(h, claim)`.
2. `is_mint = h == mint_hash`; `has_descriptor = claim.descriptor is Some`; require `is_mint != has_descriptor`.
3. `verify_asset_fold_claim_v1(claim, negate_quantity: h == output_hash)`: when `Some(d)`: `descriptor = ledger_output_commitment_v1.decode(d.descriptor_cbor)` (this is where `descriptor_is_well_formed`, including the `cardano_value_size ≤ 5,000` C22 boundary, is enforced), `descriptor.asset_count == d.asset_count`, `verify_asset_membership(descriptor, d.asset_index, claim.policy_id, claim.asset_name, claim.quantity, d.asset_peaks, d.asset_siblings)`; always: `claim.outcome != ValueAccumulatorMutationInvalid` and `apply_value_asset_mutation(claim.pre_value_accumulator, policy_id ++ asset_name, ±quantity, claim.mutation) == claim.outcome`.

### 4e. Security argument

- **Dispatch uniqueness.** The yield requires a singleton input at any of the three dispatcher credentials and reads that input's own `Spend` redeemer, so one withdrawal cannot discharge two asset threads, and the claim it verifies is byte-identical to the one the dispatcher acts on (same redeemer `Data`).
- **Role authentication.** `require_authenticated_zero_yield` needs the indexed reference input to carry exactly one `V1VtVamAssetFoldYield` token under `reference_script_auth_policy_id` and `reference_script: Some(h)`, an exact zero-lovelace withdrawal from `Script(h)`, and a unique `Withdraw` redeemer for `h`. A different script fails on the role token (only the deployment auth policy mints it, onto the published yield UTxO); a different UTxO fails on the withdrawal credential.
- **Cross-arm substitution.** The sign of the fold is derived from the *matched dispatcher credential*, never from the redeemer; a mint step must carry `descriptor: None` and a replay/output step `Some` on both sides (`is_mint != has_descriptor` in the yield, `expect Some(d)` / `descriptor == None` in the dispatchers). No other family references `asset_fold_role`.
- **Output-state re-derivation.** The dispatcher, not the yield, derives and checks the continuation (`continue_winning`: award script hash, `winning_resolution()`, evidence hash, phase). This deviates from the min-ADA yields, which re-derive the output state themselves, and deliberately so: the yield here attests a *pure function of the claim* (decode, membership, mutation), and every claim input is bound by the dispatcher to authenticated state — `pre_value_accumulator` to `control.value_accumulator` (control bound to `pre.work_root` by `structural_transition_is_valid` at prepare and re-encoded exactly by `value_and_mint_verified_body_v1`), `descriptor_cbor` to `control.replay_value_hash`, `asset_index` to the cursor, `outcome` and `asset_count` to the successor derivation. Re-deriving the output state in the yield would cost control decode + verified body (≈7.3k) and push it past 18k for no additional binding.
- **If the yield is omitted:** `require_authenticated_zero_yield` fails on `list.at(reference_inputs, i)` / the token filter, the dispatcher fails, no award. **If `outcome` is forged:** the yield recomputes the mutation and refuses. **If `asset_count` is forged:** the yield's decode check refuses. **If `pre_value_accumulator` is forged:** the dispatcher refuses. **If `mutation` is forged:** it is part of the evidence-hashed auxiliary, so `continue_winning` refuses before semantics run. **If the yield accepts a claim for a different step:** impossible in the same transaction (singleton dispatcher input) and irrelevant across transactions (withdrawals are per transaction).

## 5. Size and budget projection

| Script | Raw (measured) | Applied (+73 / +110 for 3 params) | Signed publication (≈+276, yield also mints one role NFT) |
| --- | ---: | ---: | ---: |
| replay-asset dispatcher (`ds2_replay_asset`) | 13,674 | ≈13,747 | ≈14,030 — fits, margin ≈2,350 |
| asset-fold yield (`ys2_asset_fold_yield`) | 10,949 | ≈11,060 | ≈11,500 — fits |

Total referenced script bytes in the semantic-resolution transaction:
dispatcher + yield ≈ 24,800 bytes — inside the first 25 KiB
`minFeeRefScriptCostPerByte` tier (base 15 lovelace/byte ≈ 0.37 ADA); ~800
bytes from the tier boundary, which only changes the fee multiplier on the
excess, not fit. Assumes the transaction references no other script UTxO
(today it reads only the resolver's reference script via
`scriptCarriage.referenceInputs`).

ExUnits: **not measured in this plan** (no emulator run). Expected shape: the
predicate is executed once (decode/membership/mutation in the yield, verified
body in the dispatcher) plus one extra redeemer `Data` parse and four
`list.filter`s for the handshake; budget the yield's parse of the largest
witness (transition with a 15-field state, a 16-step mpf proof, up to 14 asset
siblings) once, per the primer's cost model. Measure with
`MIDGARD_PRINT_PROOF_FIT=1` (`semanticMeasurement` from
`dispute-scenario.ts`) in the emulator test of §7 against 13,200,000 memory
units (§3.3 basis).

## 6. Off-chain work

Does not exist today for this contract: the yield, its role, its deployment
entry, its stake registration, the `reference_script_auth_policy_id`
parameter on a semantic resolver, and any funding row for a validation-trace
yield.

- **SDK contracts** (`demo/midgard-sdk/src/fraud-proof/contracts.ts`): add `["reference_script_auth_policy_id", referenceScriptAuthPolicyId]` to `semanticResolverParameterValues` (name-keyed, ~line 3883 — the three asset dispatchers are then served automatically and the `builtSemanticResolvers.length !== 90` check is unchanged; make `referenceScriptAuthPolicyId` a required builder input for the validation-trace chain). Add `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.yields.valueAndMintAssetFold = "fraud_proofs/validation_trace/value_and_mint_asset_fold_yield_v1.main.withdraw"` and `validationTraceDispute.yields.valueAndMintAssetFold = makeWithdrawalValidator(applyBlueprintParams(blueprint, title, [replayAsset.spendingScriptHash, outputAsset.spendingScriptHash, mintAsset.spendingScriptHash]))` (no cycle: dispatchers depend only on the role constant). Extend `ValidationTraceDisputeFaultProofContracts` with `yields`.
- **Arity gate** `demo/midgard-fault-proofs/tests/zz605-semantic-resolver-arity.test.ts`: derives parameters from the blueprint; passes once the name is in the map, fails closed (#609 message) otherwise.
- **Reference-script roles**: `"V1 validation-trace value-and-mint asset-fold yield": "V1VtVamAssetFoldYield"` in `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` (`demo/midgard-sdk/src/reference-scripts.ts`) and `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES` (`demo/midgard-core/src/deployment-manifest-identity-v1.ts`, with its `deployment-manifest-identity-v1.test.ts`); Aiken `asset_fold_role` must equal the string.
- **Manifest / deployment info**: `demo/midgard-node/src/deployment-manifest-v1.ts` step-name map entry → `validationTraceDisputeValueAndMintAssetFoldWithdraw`; `demo/midgard-node/src/commands/contract-deployment-info.ts` `withdrawalDescriptor("validationTraceDisputeValueAndMintAssetFoldWithdraw", contracts.validationTraceDispute.yields.valueAndMintAssetFold, "V1 validation-trace value-and-mint asset-fold yield")`; `demo/midgard-node/src/transactions/reference-scripts.ts` `manifestReferenceScriptTarget(...)` next to the min-ADA yield (line ~1565). Inspection fixtures: `inspect-contracts.test.ts` derives `oversizedAppliedSpendingScripts` from the applied scripts (no hardcoded list) but pins `Q13_CATALOGUE_ROOT` — re-pin once for the whole regeneration.
- **Stake registration**: `demo/midgard-sdk/src/initialization.ts` (`.register.Stake(scriptRewardAddress(network, …yields.valueAndMintAssetFold.withdrawalScript))`, alongside the min-ADA yields at lines ~326–333); emulator `tests/support/emulator/setup-tx.ts` analogue of `registerStateQueueYieldRewardAccountsV1`.
- **Submit route** (`demo/midgard-fault-proofs/src/validation-dispute/submit.ts`): `semanticActionFieldsV1` (line ~3779, `resolverIndex === 12`) must build `[claim, input_index, output_index, transition, source_kind, key, next_schedule_hash, yield_to_ref_input_index]` for semantic 3 instead of `[...base, ...auxiliary.fields]` — `claim.pre_value_accumulator` is the pre-state control's accumulator (the trace builder already decodes it), `claim.outcome` is the off-chain replay of the mutation (the honest successor's accumulator, or `AssetLimitExceeded`), `yield_to_ref_input_index` via `requireReferenceInputIndex` inside the `makeIndexedValidationStageRedeemer` layout callback (precedent `src/min-ada/submit-step-02-v1.ts:264`, `remove-fraudulent-block.ts:2308`). `submitValidationDisputeSemanticResolution` (line ~5956) adds `.readFrom([yieldReferenceUtxo])` and `.withdraw(scriptRewardAddress(network, yield.withdrawalScript), 0n, Data.void())` for semantic 3/6/8 (precedent `submit-step-02-v1.ts:297,548–568`), sourcing the yield UTxO from the new deployment entry with a `requireValidationValueAndMintAssetFoldYieldUtxo` helper shaped like `requireValidationValueAndMintSemanticReferenceScriptUtxo` (line ~1007). Keep the `VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1` table (the auxiliary is unchanged) and drop the "publish it" oversized refusal branch (lines ~6091–6104) once all eleven fit.
- **Funding requirements**: the validation-dispute production funding roster gains one authenticated yield publication (~11.5 KB script, role NFT mint) — same row shape as the min-ADA `fraudProofMinAdaStep02TxWithdraw` reference in `src/min-ada/production-workflow-v1.ts:527`.
- **Codec**: TypeBox schema for `AssetFoldClaimV1`/`AssetDescriptorClaimV1` next to the ValueAndMint action encoders in `demo/midgard-sdk/src/fraud-proof/validation-dispute.ts`; no `midgard-core` rejection-reason change.
- **Watcher**: `validationTraceDispute` is not installed in `demo/midgard-watcher`; nothing required, and the yield uses no operator-local input.

## 7. Emulator scenario tests

Exists today: `tests/submit-init-emulator-value-and-mint-v1.test.ts` (one journey, semantic 0 `begin`, via `runForcedValidationDisputeScenario` + `buildForgedOperatorSuccessorValidationDisputeFixture({disputedPhase: "valueAndMint"})`), `tests/submit-init-emulator-cek-value-and-mint-v1.test.ts` (cek half), and `tests/submit-init-emulator-validation-dispute.test.ts` (publishes CEK semantics with `oversized: true`). `tests/support/emulator/dispute-scenario.ts:352–425` publishes an oversized ValueAndMint semantic under raised parameters; `tests/support/emulator/reference-scripts.ts:303` is `publishPlainReferenceScriptUtxo` with the `oversized` flag.

Add `tests/submit-init-emulator-value-and-mint-replay-asset-v1.test.ts` (one journey per file — see the wasm-heap note at the top of the existing value-and-mint test):

- Fixture: extend `buildForgedOperatorSuccessorValidationDisputeFixture` (`tests/support/emulator/validation-dispute-fixtures.ts:930`) with a `disputedStep` selector that picks the first honest state whose ValueAndMint control has `stage == 2 && replay_asset_cursor > 0`; needs an honest native transaction (`buildHonestAcceptedNativeTransactionTraceV1` variant) that spends an input carrying at least one native asset.
- Publication fit: publish the dispatcher plainly and the yield through `publishAuthenticatedValidationDisputeControl` with target `{ control: "value-and-mint asset-fold yield", name: "V1 validation-trace value-and-mint asset-fold yield" }` added to the roster at `tests/support/emulator/reference-scripts.ts:46–75`; both **without `oversized`**, asserting `publicationMeasurement.l1ByteMargin > 0`; `buildRemovalDeploymentInfo` (`removal-deployment.ts`) gains the yield entry.
- Positive lifecycle: prepare-selected → semantic resolution (dispatcher + yield, `requireL1ProofEnvelope` passes) → award → removal, all under `withRealL1MaxTxSize`.
- Valid-block negative at the same frontier: operator honest, challenger forges the claim's `outcome` (yield refuses), omits the withdrawal (dispatcher refuses), or references the award's role UTxO instead of the yield's (role refusal); each surfaces as a local-evaluation failure, no award.
- Cancel: `ct.Cancel` at the dispatcher (the family supports cancel; resume is not a validation-dispute concept).
- Maximum supported shape: an input descriptor at the C22 boundary (`cardano_value_size = 5,000`, `max_cardano_value_cbor_bytes`) with 16,384 assets (14 asset siblings) and an accumulator whose mpf proof for the folded unit is the deepest constructible (≤ `value_map_maximum_proof_steps = 16`); assert the signed semantic-resolution bytes ≤ 16,384 and print `semanticMeasurement`.
- Drop `oversized: true` for ValueAndMint in `dispute-scenario.ts` and delete the `semanticIsOversized` branch once every value-and-mint script fits.

## 8. Aiken tests

Extend `validators/fraud-proofs/validation-trace/value-and-mint-split-v1.test.ak` (62 tests today):

- Update `replay_asset_wire_layout_is_pinned` to the claim-first layout; add a golden for `AssetFoldClaimV1`.
- `replay_asset_dispatcher_wins_with_authenticated_yield` (fixture transaction with the yield reference input carrying `V1VtVamAssetFoldYield`, a zero withdrawal and a unique withdraw redeemer — extend `native_binding_fixture_v1.continue_mid_step_l1_tx_v1`).
- Negatives (`fail`): `_refuses_missing_yield_reference_input`, `_refuses_cross_arm_role_token` (e.g. `V1FpMinAdaS02TxYield`), `_refuses_withdrawal_script_substitution`, `_refuses_nonzero_withdrawal`, `_refuses_pre_accumulator_mismatch`, `_refuses_foreign_descriptor_hash`, `_refuses_descriptor_none`, `_refuses_mutation_invalid_outcome`.
- Yield vectors in a new `value-and-mint-asset-fold-yield-v1.test.ak`: `asset_fold_yield_accepts_honest_replay_asset_claim`, `_accepts_asset_limit_exceeded_claim`, `_refuses_forged_outcome`, `_refuses_forged_asset_count`, `_refuses_two_dispatcher_inputs`, `_refuses_descriptor_on_mint_dispatcher`, `_refuses_missing_descriptor_on_replay_dispatcher`, `_negates_quantity_only_for_output_dispatcher`, `_refuses_claim_not_at_field_zero`.
- Library (`lib/midgard/validation-machine-v1.test.ak`): keep `value_and_mint_replay_asset_route_agrees_with_the_aggregate` and the partition test; add a property `value_and_mint_replay_asset_split_agrees_with_the_aggregate` fuzzing `mutation`/`quantity`: `dispatch_semantics(claim) && verify_asset_fold_claim_v1(claim, False)` ⇔ `verify_value_and_mint(pre, witness, ValueInputAssetWitness{...})` for honest and forged claims.

## 9. Verification commands

```bash
# 1. Sizes (run in a copy, never in the checkout)
cp -r onchain/aiken /tmp/size-check-vam && cd /tmp/size-check-vam
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/value_and_mint.*(spend|withdraw)$/.test(v.title)){const n=Buffer.from(v.compiledCode,"hex").length;console.log(n<=15000?"ok ":"BIG",n,v.title)}'
# expect 12 lines, all "ok": eleven semantics (replay_asset ≈13,674) + asset_fold_yield ≈10,949
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m value_and_mint   # all pass; ≥ 62 + new vectors in split-v1, 22 in machine tests
cd - && rm -rf /tmp/size-check-vam

# 2. TypeScript (from demo/midgard-fault-proofs, pinned Node 22.22.2)
pnpm exec vitest run tests/zz605-semantic-resolver-arity.test.ts tests/validation-dispute-submit.test.ts tests/inspect-contracts.test.ts
pnpm exec vitest run tests/submit-init-emulator-value-and-mint-replay-asset-v1.test.ts   # publication margins > 0, award + removal
# from demo/midgard-core
pnpm exec vitest run tests/deployment-manifest-identity-v1.test.ts
```

## 10. Ordering and dependencies

- Lands with the other seven value-and-mint plans in the one blueprint regeneration: all eleven hashes feed `value_and_mint_v1`'s `semantic_resolver_script_hashes` (count 11 unchanged), which re-applies up the family to the catalogue root.
- The **library arm split (§4a)** is shared by all eight; the **asset-fold yield (§4b–4d)** is shared with `output-asset` and `mint-asset` and must land first (its three parameters are those dispatchers' hashes; the dispatchers depend only on `asset_fold_role`).
- The `reference_script_auth_policy_id` semantic-parameter name should be reused by any other group's plan that adds a yield, so the `semanticResolverParameterValues` map grows once.
- The stage bodies are private to the value-and-mint machine; no other group's plan touches them.

## 11. Risks

- **Budget:** ExUnits unmeasured; the extra redeemer parse in the yield is the only new work. Mitigation: the §7 max-shape assertion against 13,200,000 memory units.
- **Fee tier edge:** ≈24.8 KB referenced vs the 25 KiB first tier; a regeneration could tip the excess into tier 2 (fee only).
- **ABI churn:** claim-first action layout, third parameter, new withdraw validator, new role and deployment entry; every ValueAndMint wire-layout golden and the SDK action encoder change. Evidence hash and prepare are unchanged, which bounds the churn to this arm's redeemer.
- **Compiler hazard (observed):** the pinned fork aborted (SIGABRT, no diagnostic) on the first round-2 probe build; rewriting `claim.descriptor == None` as a `when`, dropping a redundant `..` spread on a fully named `Input` pattern, and binding the auxiliary to a typed `let` before the `Data` upcast fixed it. Avoid those constructs in the implementation.
- **Discarded-binding hazard:** `let _x = require_authenticated_zero_yield(...)` deletes the call and its `expect`s; bind the hash and use it (min-ADA step-02 uses `expect _yield_hash = …`; prefer a used binding).
- **Spec:** C49 semantics unchanged (same predicate, split across two scripts in one transaction); C22's 5,000/5,001 boundary now enforced in the yield's decode — covered by the omission argument in §4e and by the `_refuses_missing_yield_reference_input` vector; C52 unaffected (no added transactions); §3.3 byte fit is the done criterion.
