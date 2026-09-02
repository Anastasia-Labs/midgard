# `value_and_mint_output_asset_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md). Uses the **library arm split** and the
**shared asset-fold yield** defined in
[`validation-trace-value-and-mint-replay-asset-semantic-v1.md`](validation-trace-value-and-mint-replay-asset-semantic-v1.md)
§4a–4d; only what differs for this arm is spelled out here.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/value_and_mint_output_asset_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/value-and-mint-output-asset-semantic-v1.ak` |
| Raw size (2026-09-01 build) | 21,778 bytes (applied 21,823, #634 note in `submit.ts`) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` |
| Phase / resolver index | `ValueAndMint`, resolver 12 |
| Semantic index (arm) | 6 of 11; global slot `validationSemanticResolverGlobalIndexV1(12, 6)` |
| Library entry point | `verify_value_and_mint_output_asset_semantics_v1` → `value_and_mint_stage_three` with `ValueOutputAssetWitness` |
| Redeemer action | `VerifyOutputAsset { input_index, output_index, transition, ledger_output_index, descriptor_cbor, asset_index, policy_id, asset_name, quantity, asset_peaks, asset_siblings, mutation }` (`ledger_output_index` is the witness's `output_index`, renamed on a wire-identical position) |
| Role name today | none |
| Deployment entry today | `validationTraceDisputeValueAndMintOutputAssetSemantic` (`…ENTRIES_V1[6]`) |
| SDK title key | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.valueAndMintOutputAsset` (`contracts.ts:474`) |

What the step proves: stage 3 with `output_asset_cursor > 0` — fold asset
`asset_index` of the current output's authenticated descriptor into the value
accumulator with **negative** sign (`0 - quantity`), or take the exact
`E_ASSET_COUNT` rejection at the distinct-asset cap. The output descriptor was
authenticated against `output_descriptor_peaks` by the preceding
`output_descriptor` step; this step binds `descriptor_cbor` to the control's
`replay_value_hash` instead.

## 2. Why it is this size

Full building-block table: replay-asset plan §2a (same copy, same build).
Rows that matter here:

| Reachable code | Raw Δ |
| --- | ---: |
| scaffold (`cancel`, `continue_winning`, parse) | 3,810 |
| `value_and_mint_control_from_witness` | +2,098 |
| `value_and_mint_verified_body_v1` | +5,239 |
| `ledger_output_commitment_v1.decode` | +1,650 |
| `verify_asset_membership` (beyond decode) | +802 |
| **`apply_value_asset_mutation`** | **+5,388** |
| `rejected_successor_is_exact` | +471 |
| `validation_merkle_v1.verify_membership` + `output_meets_min_ada_v1` + `output_descriptor_leaf_hash` (descriptor arm, unreachable after the split) | +610 |

Whole-resolver prototypes (exact validator shape):

| Prototype | Raw bytes | ≤ 15,000 |
| --- | ---: | :-: |
| today (`value_and_mint_output_asset_semantic_v1`) | 21,778 | no |
| `pr_output_asset` — stage-3 asset arm only | 20,705 | no |
| `ds_output_asset` — arm only, mutation delegated to a yield | 15,976 | no |
| `ds2_output_asset` — arm only, decode + membership + mutation delegated to the asset-fold yield | **13,369** | yes |
| `ys2_asset_fold_yield` (shared) | 10,949 | yes |

The other two arms of `value_and_mint_stage_three` (descriptor arm with
min-Ada, finish arm) cost only ~1,070 bytes; the resolver's own arm carries
the 8.3k of decode/membership/mutation.

## 3. Options considered

Same table as the replay-asset plan §3: prune alone measured 20,705
(prerequisite, not sufficient); mutation-only yield 15,976 (over target);
**asset-fold yield chosen** (13,369 / 10,949); moving the verified body out
leaves ≈15.5k; chaining rejected (asset ladder length, C52); redesign rejected.

## 4. Chosen design

### 4a. Library arm split
As replay-asset §4a: `value_and_mint_stage_three_asset_arm(pre, witness,
control, output_index, descriptor_cbor, asset_index, policy_id, asset_name,
quantity, asset_peaks, asset_siblings, mutation)`; the aggregate
`value_and_mint_stage_three` dispatches to it after `expect
ValueOutputAssetWitness {…} = auxiliary`.

### 4b. Validators

| Validator | Change |
| --- | --- |
| `value_and_mint_output_asset_semantic_v1.main.spend` (dispatcher, same title) | adds `reference_script_auth_policy_id: PolicyId` as third parameter; calls `verify_value_and_mint_output_asset_dispatch_semantics_v1` |
| `value_and_mint_asset_fold_yield_v1.main.withdraw` (shared) | second parameter `output_asset_dispatcher_script_hash` is this dispatcher's applied hash; a match on it sets `negate_quantity = True` |

### 4c. Redeemer ABI delta

```
VerifyOutputAsset {
  claim: AssetFoldClaimV1,        // NEW, field 0; descriptor: Some(AssetDescriptorClaimV1)
  input_index: Int,
  output_index: Int,
  transition: ValidationOneStepWitnessV1,
  ledger_output_index: Int,
  yield_to_ref_input_index: Int,  // NEW
}
```

Moved into `claim`: `descriptor_cbor, asset_index, asset_peaks, asset_siblings`
(as `descriptor`), `policy_id, asset_name, quantity, mutation`. `quantity` is
carried **positive as in the auxiliary**; the negation happens in both halves
(`0 - quantity` in the dispatcher's successor derivation, `negate_quantity`
from the matched credential in the yield). Auxiliary `ValueOutputAssetWitness
{ output_index: ledger_output_index, descriptor_cbor: d.descriptor_cbor, … ,
mutation: claim.mutation }` is rebuilt from `claim` + action fields, so the
evidence hash, `prepare_semantic_resolution` and the datum are unchanged.

### 4d. Handshake (dispatcher side; yield side is shared)

`verify_value_and_mint_output_asset_dispatch_semantics_v1(pre, witness,
output_index, claim)` (measured as `probe_vam_output_asset_dispatch2_v1`):

- control decode; `value_and_mint_verified_body_v1` true;
- `control.stage == 3`, `control.output_cursor < native_control.output_count`, `control.output_asset_cursor > 0`;
- `control.value_accumulator == claim.pre_value_accumulator`;
- `expect Some(d) = claim.descriptor`; `output_index == control.output_cursor`; `d.asset_index == control.output_asset_cursor - 1`; `blake2b_256(d.descriptor_cbor) == control.replay_value_hash`;
- `when claim.outcome`: `AssetLimitExceeded → rejected_successor_is_exact(pre, post, reject_asset_count)`; `MutationInvalid → False`; `Updated(next) →` successor exact with `{ output_cursor + 1, output_asset_cursor: 0, replay_value_hash: no_rejection_code_hash }` when `control.output_asset_cursor == d.asset_count`, else `{ output_asset_cursor + 1 }`.

Preceded in `main.spend` by `require_authenticated_zero_yield(…,
asset_fold_role, yield_to_ref_input_index)` with the result bound and used.

### 4e. Security argument
As replay-asset §4e, with the arm-specific bindings above. The sign is the one
cross-arm hazard particular to this dispatcher: a replay-asset claim replayed
against the output-asset dispatcher would fold `+quantity` where `-quantity`
is required; the yield derives negation from the *credential it matched*, so
the claim's `outcome` is checked with the output sign whenever the
output-asset dispatcher is the spent input, and the dispatcher's successor
derivation uses the same `outcome`. Omitting the yield fails
`require_authenticated_zero_yield`; forging `outcome`/`asset_count` fails in
the yield; forging `pre_value_accumulator` or `descriptor_cbor` fails in the
dispatcher; forging `mutation` fails the evidence hash.

## 5. Size and budget projection

| Script | Raw (measured) | Applied | Signed publication |
| --- | ---: | ---: | ---: |
| output-asset dispatcher (`ds2_output_asset`) | 13,369 | ≈13,442 | ≈13,720 — fits, margin ≈2,660 |
| asset-fold yield (shared) | 10,949 | ≈11,060 | ≈11,500 — fits |

Referenced bytes per semantic-resolution transaction ≈ 24,500 (first
`minFeeRefScriptCostPerByte` tier, ≈0.37 ADA). ExUnits unmeasured here; same
expectation and measurement route as replay-asset §5.

## 6. Off-chain work

Identical list to replay-asset §6 (the SDK parameter-map entry, yield builder,
roles, manifest/deployment entries, stake registration and funding row are
shared and land once). Specific to this arm:

- `submit.ts` `semanticActionFieldsV1` (`resolverIndex === 12`, semantic 6): emit `[claim, input_index, output_index, transition, ledger_output_index, yield_to_ref_input_index]`; `claim.quantity` positive, `claim.outcome` computed off-chain with the **negated** delta.
- `submitValidationDisputeSemanticResolution`: `.readFrom([yieldUtxo])` + zero `.withdraw` for semantic 6.
- `VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1[6]` (`valueOutputAsset`) stays as the auxiliary shape check; only the flattening rule changes.
- `validation-dispute-submit.test.ts` "gives every ValueAndMint semantic resolver a reference-script deployment entry" is unchanged (the yield is not a semantic resolver).

## 7. Emulator scenario tests

Exists today: only the `begin` journey (`tests/submit-init-emulator-value-and-mint-v1.test.ts`) and the oversized publication path in `dispute-scenario.ts:352–425`.

Add `tests/submit-init-emulator-value-and-mint-output-asset-v1.test.ts`
(one journey per file): fixture selects the first honest state with `stage ==
3 && output_asset_cursor > 0` (an honest native transaction producing a
multi-asset output — the `E_MIN_ADA` fixture in
`validation-dispute-fixtures.ts:516` already reaches the output ladder);
publication of dispatcher and yield without `oversized` (margins > 0);
positive lifecycle through award and removal under `withRealL1MaxTxSize`;
valid-block negatives (forged `outcome`, missing withdrawal, wrong role token,
and specifically a claim built with the *replay* sign); `ct.Cancel`; maximum
shape: output descriptor at `cardano_value_size = 5,000` (C22 boundary) with
16,384 assets and the deepest constructible mpf proof; assert signed bytes
≤ 16,384 and print `semanticMeasurement`.

## 8. Aiken tests

- `value-and-mint-split-v1.test.ak`: update `output_asset_wire_layout_is_pinned`; add `output_asset_dispatcher_wins_with_authenticated_yield`, `_refuses_missing_yield_reference_input`, `_refuses_cross_arm_role_token`, `_refuses_withdrawal_script_substitution`, `_refuses_nonzero_withdrawal`, `_refuses_pre_accumulator_mismatch`, `_refuses_foreign_descriptor_hash`, `_refuses_descriptor_none`, `_refuses_positive_sign_outcome` (outcome computed with `+quantity`).
- `value-and-mint-asset-fold-yield-v1.test.ak` (shared file): `asset_fold_yield_negates_quantity_only_for_output_dispatcher` covers this arm's sign.
- `validation-machine-v1.test.ak`: keep `value_and_mint_output_asset_route_agrees_with_the_aggregate` and the partition test; add `value_and_mint_output_asset_split_agrees_with_the_aggregate`.
- Existing `value_and_mint_replays_output_descriptor_asset_on_l1` (line ~14123) keeps exercising the aggregate route.

## 9. Verification commands

As replay-asset §9; expected line for this script `ok 13369
fraud_proofs/validation_trace/value_and_mint_output_asset_semantic_v1.main.spend`
(±regeneration drift), and the journey file
`tests/submit-init-emulator-value-and-mint-output-asset-v1.test.ts`.

## 10. Ordering and dependencies

Lands with the group's single regeneration; depends on the arm split and on
the asset-fold yield (whose second parameter is this dispatcher's hash);
shares the `reference_script_auth_policy_id` parameter-map entry with the
other two asset dispatchers.

## 11. Risks

As replay-asset §11 (unmeasured ExUnits, fee-tier edge, ABI churn, the
observed compiler abort on `Option == None`/block upcasts, the discarded
`let _` hazard). Arm-specific: the sign convention is a correctness trap —
covered by `_refuses_positive_sign_outcome` and the yield's
credential-derived negation, and by the aggregate-agreement property.
