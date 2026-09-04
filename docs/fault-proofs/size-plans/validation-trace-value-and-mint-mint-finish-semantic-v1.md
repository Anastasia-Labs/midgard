# `value_and_mint_mint_finish_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md). Strategy: **prune only** via the
library arm split defined in
[`validation-trace-value-and-mint-replay-asset-semantic-v1.md`](validation-trace-value-and-mint-replay-asset-semantic-v1.md)
§4a. No ABI change, no new validator.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/value_and_mint_mint_finish_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/value-and-mint-mint-finish-semantic-v1.ak` |
| Raw size (2026-09-01 build) | 17,859 bytes (applied 17,931, #634 note in `submit.ts`) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` |
| Phase / resolver index | `ValueAndMint`, resolver 12 |
| Semantic index (arm) | 9 of 11; global slot `validationSemanticResolverGlobalIndexV1(12, 9)` |
| Library entry point | `verify_value_and_mint_mint_finish_semantics_v1` → `value_and_mint_stage_four` with `NoAuxiliaryWitness` |
| Redeemer action | `VerifyMintFinish { input_index, output_index, transition }` (transition-only layout) |
| Role name today | none |
| Deployment entry today | `validationTraceDisputeValueAndMintMintFinishSemantic` (`…ENTRIES_V1[9]`) |
| SDK title key | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.valueAndMintMintFinish` (`contracts.ts:480`) |

What the step proves: stage 4 with `mint_cursor == native_control.mint_count`
— the successor is the same control at stage 5 (hand-off to `finalize`, which
checks the value equation against `body.fee`).

## 2. Why it is this size

A transition-only resolver reaching the whole `value_and_mint_stage_four`;
the smallest of the eight because stage 4 has no descriptor decode. Full
table: replay-asset plan §2a.

| Reachable code | Raw Δ | After split |
| --- | ---: | :-: |
| scaffold | 3,810 | yes |
| `value_and_mint_control_from_witness` | +2,098 | yes |
| `value_and_mint_verified_body_v1` | +5,239 | yes |
| `value_and_mint_successor_is_exact` | +326 | yes |
| `validation_merkle_v1.verify_membership` + `mint_asset_leaf_hash` (asset arm) | +621 | no |
| `apply_value_asset_mutation` (asset arm) | +5,388 | no |
| `rejected_successor_is_exact` (asset arm) | +471 | no |

| Prototype | Raw bytes | ≤ 15,000 |
| --- | ---: | :-: |
| today | 17,859 | no |
| `pr_mint_asset` (sibling arm, for reference) | 18,296 | no |
| `pr_mint_finish` — stage-4 finish arm only (verbatim) | **11,042** | yes (margin 3,958) |

The 691-byte gap between `mint_finish` (17,859) and `mint_asset` (18,550)
today is just the `ValueMintAssetWitness` reconstruction and the richer
redeemer parse; the 6.8k of mutation/membership code is shared by both
because both call the whole stage.

## 3. Options considered

**1. Prune (arm split) — chosen** (11,042 measured). Yield, chain, redesign
rejected: no size or budget need.

## 4. Chosen design

No new validators, roles, parameters or redeemer changes. In
`lib/midgard/validation-machine-v1.ak`:

```
fn value_and_mint_stage_four_finish_arm(pre, witness, control) -> Bool {
  value_and_mint_successor_is_exact(pre, witness, ValueAndMintControlV1 { ..control, stage: 5 })
}
```

The aggregate `value_and_mint_stage_four` keeps `auxiliary ==
NoAuxiliaryWitness` and the `mint_cursor == mint_count` test and calls the
arm; `verify_value_and_mint_mint_finish_semantics_v1` keeps `control.stage ==
4` and `control.mint_cursor == control.native_control.mint_count` and calls
the arm directly.

Handshake and security argument: unchanged `continue_winning`; identical
predicate; nothing to omit or substitute.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (+73) | Signed publication (≈+276) |
| --- | ---: | ---: | ---: |
| `value_and_mint_mint_finish_semantic_v1` | 11,042 | ≈11,115 | ≈11,390 — fits, margin ≈5,000 |

Referenced bytes ≈ 11.1 KB. ExUnits: not more than today.

## 6. Off-chain work

Nothing new. Stays: title `valueAndMintMintFinish`, entry
`validationTraceDisputeValueAndMintMintFinishSemantic`, submit base fields
for semantic 9 (`VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1[9] = none`).
Group-level hash change and catalogue re-pin only.

## 7. Emulator scenario tests

Exists today: `begin` journey only. Add
`tests/submit-init-emulator-value-and-mint-mint-finish-v1.test.ts`: fixture
selects the honest state with `stage == 4 && mint_cursor == mint_count`
(every honest trace has one; the pure key-witness fixture has `mint_count =
0`, so it is the state right after `output_finish`); publish without
`oversized` (margin > 0); positive lifecycle through award and removal;
valid-block negative (forged successor stage at the same frontier);
`ct.Cancel`; maximum shape: none beyond the transition; assert signed bytes ≤
16,384.

## 8. Aiken tests

Must keep passing: `mint_finish_validator_wins_the_exhausted_mints`,
`mint_finish_validator_refuses_mint_asset`,
`mint_finish_validator_refuses_finalize`,
`transition_only_wire_layouts_are_pinned`,
`prepare_routes_mint_finish_to_slot_nine` (split file);
`value_and_mint_mint_finish_route_agrees_with_the_aggregate` and the partition
test (machine file). Add `mint_finish_validator_refuses_pending_mints`
(`mint_cursor < mint_count` — refused on the discriminator without touching
the asset arm).

## 9. Verification commands

As replay-asset §9; expected `ok 11042
fraud_proofs/validation_trace/value_and_mint_mint_finish_semantic_v1.main.spend`
(±drift).

## 10. Ordering and dependencies

Lands with the group's regeneration; depends only on the arm split of
`value_and_mint_stage_four` (shared with mint-asset).

## 11. Risks

None specific: 4k margin, no ABI change, C49 predicate unchanged.
