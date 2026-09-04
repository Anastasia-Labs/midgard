# `value_and_mint_mint_asset_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md). Uses the **library arm split** and the
**shared asset-fold yield** defined in
[`validation-trace-value-and-mint-replay-asset-semantic-v1.md`](validation-trace-value-and-mint-replay-asset-semantic-v1.md)
§4a–4d; this arm carries **no descriptor** (`claim.descriptor = None`).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/value_and_mint_mint_asset_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/value-and-mint-mint-asset-semantic-v1.ak` |
| Raw size (2026-09-01 build) | 18,550 bytes (applied 18,622, #634 note in `submit.ts`) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` |
| Phase / resolver index | `ValueAndMint`, resolver 12 |
| Semantic index (arm) | 8 of 11; global slot `validationSemanticResolverGlobalIndexV1(12, 8)` |
| Library entry point | `verify_value_and_mint_mint_asset_semantics_v1` → `value_and_mint_stage_four` with `ValueMintAssetWitness` |
| Redeemer action | `VerifyMintAsset { input_index, output_index, transition, mint_index, policy_id, asset_name, quantity, siblings, mutation }` |
| Role name today | none |
| Deployment entry today | `validationTraceDisputeValueAndMintMintAssetSemantic` (`…ENTRIES_V1[8]`) |
| SDK title key | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.valueAndMintMintAsset` (`contracts.ts:478`) |

What the step proves: stage 4 with `mint_cursor < native_control.mint_count` —
the mint leaf `mint_asset_leaf_hash(policy_id, asset_name, quantity)` at
`mint_index` is a member of the native control's authenticated `mint_peaks`
frontier (`validation_merkle_v1.verify_membership`), `policy_id` is 28 bytes,
`asset_name` ≤ 32 bytes, `quantity != 0`, and the signed quantity is folded
into the value accumulator (`+quantity`; burns are negative), or the exact
`E_ASSET_COUNT` rejection is taken.

## 2. Why it is this size

Full building-block table: replay-asset plan §2a. Rows that matter here:

| Reachable code | Raw Δ |
| --- | ---: |
| scaffold | 3,810 |
| `value_and_mint_control_from_witness` | +2,098 |
| `value_and_mint_verified_body_v1` | +5,239 |
| `validation_merkle_v1.verify_membership` | +508 |
| `mint_asset_leaf_hash` | +113 |
| **`apply_value_asset_mutation`** | **+5,388** |
| `rejected_successor_is_exact` | +471 |

| Prototype | Raw bytes | ≤ 15,000 |
| --- | ---: | :-: |
| today | 18,550 | no |
| `pr_mint_asset` — stage-4 asset arm only | 18,296 | no |
| `ds_mint_asset` — arm only, mutation delegated (mutation-only claim) | 13,605 | yes |
| `ds2_mint_asset` — arm only, shared asset-fold claim (`descriptor: None`) | **13,816** | yes |
| `ys2_asset_fold_yield` (shared) | 10,949 | yes |

Unlike the two descriptor arms, this arm has no decode (the mint frontier is
in the control), so the *only* oversized component is the mpf mutation: the
stage-4 finish arm is worth just 254 bytes of pruning (18,550 → 18,296).

## 3. Options considered

| Option | Verdict | Reason |
| --- | --- | --- |
| 1. Prune | rejected as sufficient, kept as prerequisite | 18,296 |
| 2a. Dedicated mutation-only yield for mint | rejected | 13,605 fits, but a second yield validator, role, entry and stake registration for a 211-byte saving over the shared one |
| **2b. Shared asset-fold yield with `descriptor: None`** | **chosen** | 13,816; one yield for all three asset dispatchers (primer: one shared yield per dominating function) |
| 3. Chain | rejected | up to 16,384 mint steps (`max_distinct_asset_count`) would double, C52 |
| 4. Redesign | rejected | arm boundary is right |

## 4. Chosen design

### 4a. Library arm split
`value_and_mint_stage_four_asset_arm(pre, witness, control, mint_index,
policy_id, asset_name, quantity, siblings, mutation)` and
`value_and_mint_stage_four_finish_arm`; the aggregate `value_and_mint_stage_four`
dispatches on `mint_cursor == mint_count`.

### 4b. Validators

| Validator | Change |
| --- | --- |
| `value_and_mint_mint_asset_semantic_v1.main.spend` (dispatcher) | third parameter `reference_script_auth_policy_id`; calls `verify_value_and_mint_mint_asset_dispatch_semantics_v1` |
| `value_and_mint_asset_fold_yield_v1.main.withdraw` (shared) | third parameter `mint_asset_dispatcher_script_hash`; a match on it requires `claim.descriptor == None` (written as a `when`, see §11) and uses `negate_quantity = False` |

### 4c. Redeemer ABI delta

```
VerifyMintAsset {
  claim: AssetFoldClaimV1,        // NEW, field 0; descriptor: None
  input_index: Int,
  output_index: Int,
  transition: ValidationOneStepWitnessV1,
  mint_index: Int,
  siblings: List<ByteArray>,      // frontier siblings stay in the action (dispatcher-side merkle check)
  yield_to_ref_input_index: Int,  // NEW
}
```

Moved into `claim`: `policy_id, asset_name, quantity, mutation`. Auxiliary
`ValueMintAssetWitness { mint_index, policy_id: claim.policy_id, asset_name:
claim.asset_name, quantity: claim.quantity, siblings, mutation: claim.mutation }`
is rebuilt, so the evidence hash, prepare and datum are unchanged.

### 4d. Handshake (dispatcher side)

`verify_value_and_mint_mint_asset_dispatch_semantics_v1(pre, witness,
mint_index, siblings, claim)` (measured as `probe_vam_mint_asset_dispatch2_v1`):

- control decode; verified body true; `control.stage == 4`; `control.mint_cursor < native_control.mint_count`;
- `control.value_accumulator == claim.pre_value_accumulator`; `claim.descriptor` is `None`;
- `mint_index == control.mint_cursor`; `bytearray.length(claim.policy_id) == 28`; `bytearray.length(claim.asset_name) <= 32`; `claim.quantity != 0`;
- `validation_merkle_v1.verify_membership(native_control.mint_count, native_control.mint_peaks, mint_index, mint_asset_leaf_hash(claim.policy_id, claim.asset_name, claim.quantity), siblings)` — the **mint-frontier membership stays in the dispatcher** (508 + 113 bytes; the yield's `descriptor` half is unused here);
- `when claim.outcome`: `AssetLimitExceeded → rejected_successor_is_exact(…, reject_asset_count)`; `MutationInvalid → False`; `Updated(next) → value_and_mint_successor_is_exact(pre, witness, { ..control, mint_cursor + 1, value_accumulator: next })`.

Preceded by `require_authenticated_zero_yield(…, asset_fold_role,
yield_to_ref_input_index)` with the result bound and used.

### 4e. Security argument
As replay-asset §4e. Arm-specific: the mint leaf is authenticated in the
dispatcher against `native_control.mint_peaks` (itself bound by
`value_and_mint_verified_body_v1` to `pre.transaction_commitment`), so the
yield's job is purely the mutation; a claim with `Some(descriptor)` against
this dispatcher is refused on both sides (`is_mint != has_descriptor` in the
yield, the `None` pin here), and a mint claim replayed against a descriptor
dispatcher fails their `expect Some(d)`. Omission, forged `outcome`, forged
`pre_value_accumulator` and forged `mutation` fail exactly as in the
replay-asset argument.

## 5. Size and budget projection

| Script | Raw (measured) | Applied | Signed publication |
| --- | ---: | ---: | ---: |
| mint-asset dispatcher (`ds2_mint_asset`) | 13,816 | ≈13,889 | ≈14,170 — fits, margin ≈2,200 |
| asset-fold yield (shared) | 10,949 | ≈11,060 | ≈11,500 — fits |

Referenced bytes per semantic-resolution transaction ≈ 24,950 — inside the
first 25 KiB (25,600) fee tier by ≈650 bytes (fee-only if exceeded). ExUnits
unmeasured; same expectation and measurement route as replay-asset §5, with
the yield doing strictly less work for this arm (no decode).

## 6. Off-chain work

Shared items as replay-asset §6. Specific to this arm:

- `submit.ts` `semanticActionFieldsV1` (semantic 8): `[claim, input_index, output_index, transition, mint_index, siblings, yield_to_ref_input_index]` with `claim.descriptor = None`; `claim.outcome` is the off-chain mutation replay with the signed quantity.
- `submitValidationDisputeSemanticResolution`: `.readFrom([yieldUtxo])` + zero `.withdraw` for semantic 8.
- `VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1[8]` (`valueMintAsset`) unchanged.
- `rejection-reason.ts` unchanged (`MintDeclaredAssetLimit`/`MintAssetAccumulationLimit` → `E_ASSET_COUNT` already exist).

## 7. Emulator scenario tests

Exists today: `begin` journey only. Add
`tests/submit-init-emulator-value-and-mint-mint-asset-v1.test.ts`: fixture
selects the first honest state with `stage == 4 && mint_cursor < mint_count`
(an honest native transaction with a non-empty `mint` field — a signed burn
settling an accumulator entry to zero, as `mint_asset_fixture` does in the
Aiken split test); publication of dispatcher and yield without `oversized`;
positive lifecycle through award and removal; valid-block negatives (forged
`outcome`, missing withdrawal, wrong role token, `Some(descriptor)` on the
mint dispatcher); `ct.Cancel`; maximum shape: `mint_count = 16,384`
(`native_scripts_control_is_well_formed` caps `mint_count` at
`max_distinct_asset_count`) with 14 siblings, a 60-byte unit and the deepest
constructible mpf proof; assert signed bytes ≤ 16,384, print
`semanticMeasurement`.

## 8. Aiken tests

- `value-and-mint-split-v1.test.ak`: update `mint_asset_wire_layout_is_pinned` and `mint_asset_validator_refuses_a_retargeted_quantity` (quantity now lives in `claim`); add `mint_asset_dispatcher_wins_with_authenticated_yield`, `_refuses_missing_yield_reference_input`, `_refuses_cross_arm_role_token`, `_refuses_withdrawal_script_substitution`, `_refuses_nonzero_withdrawal`, `_refuses_pre_accumulator_mismatch`, `_refuses_descriptor_some`, `_refuses_zero_quantity`, `_refuses_foreign_mint_leaf`.
- Shared yield file: `asset_fold_yield_refuses_descriptor_on_mint_dispatcher`, `asset_fold_yield_accepts_signed_burn_claim`.
- `validation-machine-v1.test.ak`: keep `value_and_mint_mint_asset_route_agrees_with_the_aggregate`, `value_and_mint_replays_signed_burn_leaf_on_l1`, `value_and_mint_uses_authenticated_native_mint_frontier`; add `value_and_mint_mint_asset_split_agrees_with_the_aggregate`.

## 9. Verification commands

As replay-asset §9; expected `ok 13816
fraud_proofs/validation_trace/value_and_mint_mint_asset_semantic_v1.main.spend`
and the journey file `tests/submit-init-emulator-value-and-mint-mint-asset-v1.test.ts`.

## 10. Ordering and dependencies

Lands with the group's regeneration; depends on the arm split and the shared
yield (third parameter = this dispatcher's hash); shares the
`reference_script_auth_policy_id` parameter-map entry.

## 11. Risks

As replay-asset §11. Arm-specific: this dispatcher is the largest of the three
(13,816) because it keeps the merkle membership locally; if regeneration drift
ever threatens 15,000, the fallback is to move the mint-frontier membership
into the yield's `descriptor`-less branch (−621 bytes) via a
`MintLeafClaimV1` option — not needed today. The referenced-bytes total is
the closest of the three to the 25 KiB fee tier (fee-only consequence). The
`claim.descriptor == None` comparison must be written as a `when` (the pinned
compiler aborted on the `==` form during probing).
