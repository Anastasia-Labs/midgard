# `value_and_mint_output_finish_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md). Strategy: **prune only** via the
library arm split defined in
[`validation-trace-value-and-mint-replay-asset-semantic-v1.md`](validation-trace-value-and-mint-replay-asset-semantic-v1.md)
§4a. No ABI change, no new validator.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/value_and_mint_output_finish_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/value-and-mint-output-finish-semantic-v1.ak` |
| Raw size (2026-09-01 build) | 20,941 bytes (applied 20,987, #634 note in `submit.ts`) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` |
| Phase / resolver index | `ValueAndMint`, resolver 12 |
| Semantic index (arm) | 7 of 11; global slot `validationSemanticResolverGlobalIndexV1(12, 7)` |
| Library entry point | `verify_value_and_mint_output_finish_semantics_v1` → `value_and_mint_stage_three` with `NoAuxiliaryWitness` |
| Redeemer action | `VerifyOutputFinish { input_index, output_index, transition }` (transition-only layout) |
| Role name today | none |
| Deployment entry today | `validationTraceDisputeValueAndMintOutputFinishSemantic` (`…ENTRIES_V1[7]`) |
| SDK title key | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.valueAndMintOutputFinish` (`contracts.ts:476`) |

What the step proves: stage 3 with `output_cursor ==
native_control.output_count` — `output_asset_cursor == 0`,
`replay_value_hash == no_rejection_code_hash`, and the successor is the same
control at stage 4.

## 2. Why it is this size

Same cause as replay-finish: a transition-only resolver reaching the whole
`value_and_mint_stage_three`. Full table: replay-asset plan §2a.

| Reachable code | Raw Δ | After split |
| --- | ---: | :-: |
| scaffold | 3,810 | yes |
| `value_and_mint_control_from_witness` | +2,098 | yes |
| `value_and_mint_verified_body_v1` | +5,239 | yes |
| `value_and_mint_successor_is_exact` | +326 | yes |
| `decode` (descriptor + asset arms) | +1,650 | no |
| `validation_merkle_v1.verify_membership` + `output_descriptor_leaf_hash` + `output_meets_min_ada_v1` (descriptor arm) | +610 | no |
| `verify_asset_membership` (asset arm) | +802 | no |
| `apply_value_asset_mutation` (asset arm) | +5,388 | no |
| `rejected_successor_is_exact` (both arms) | +471 | no |

| Prototype | Raw bytes | ≤ 15,000 |
| --- | ---: | :-: |
| today | 20,941 | no |
| `pr_output_finish` — stage-3 finish arm only (verbatim) | **11,082** | yes (margin 3,918) |

## 3. Options considered

**1. Prune (arm split) — chosen** (11,082 measured; 9,859 bytes unreachable
from this arm). Yield, chain and redesign rejected: no size or budget need.

## 4. Chosen design

No new validators, roles, parameters or redeemer changes. In
`lib/midgard/validation-machine-v1.ak`:

```
fn value_and_mint_stage_three_finish_arm(pre, witness, control) -> Bool {
  and {
    control.output_asset_cursor == 0,
    control.replay_value_hash == no_rejection_code_hash,
    value_and_mint_successor_is_exact(pre, witness, ValueAndMintControlV1 { ..control, stage: 4 }),
  }
}
```

The aggregate `value_and_mint_stage_three` keeps `auxiliary ==
NoAuxiliaryWitness` and the `output_cursor == output_count` test and calls the
arm; `verify_value_and_mint_output_finish_semantics_v1` keeps `control.stage
== 3` and `control.output_cursor == control.native_control.output_count` and
calls the arm directly.

Handshake and security argument: unchanged `continue_winning`; identical
predicate; nothing to omit or substitute.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (+73) | Signed publication (≈+276) |
| --- | ---: | ---: | ---: |
| `value_and_mint_output_finish_semantic_v1` | 11,082 | ≈11,155 | ≈11,430 — fits, margin ≈4,950 |

Referenced bytes ≈ 11.2 KB. ExUnits: not more than today.

## 6. Off-chain work

Nothing new. Stays: title `valueAndMintOutputFinish`, entry
`validationTraceDisputeValueAndMintOutputFinishSemantic`, submit base fields
for semantic 7 (`VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1[7] = none`).
Group-level hash change and catalogue re-pin only.

## 7. Emulator scenario tests

Exists today: `begin` journey only. Add
`tests/submit-init-emulator-value-and-mint-output-finish-v1.test.ts`: fixture
selects the honest state with `stage == 3 && output_cursor == output_count`;
publish without `oversized` (margin > 0); positive lifecycle through award and
removal; valid-block negative (forged claimed successor, e.g. stage 5 instead
of 4, at the same frontier); `ct.Cancel`; maximum shape: none beyond the
transition; assert signed bytes ≤ 16,384.

## 8. Aiken tests

Must keep passing: `output_finish_validator_wins_the_exhausted_outputs`,
`output_finish_validator_refuses_output_descriptor`,
`output_finish_validator_refuses_output_asset`,
`output_finish_validator_refuses_mint_finish`,
`transition_only_wire_layouts_are_pinned`,
`prepare_routes_output_finish_to_slot_seven` (split file);
`value_and_mint_output_finish_route_agrees_with_the_aggregate` and the
partition test (machine file). Add
`output_finish_validator_refuses_an_open_asset_cursor` (cursor exhausted but
`output_asset_cursor = 1` — refused by the arm's own conjunct).

## 9. Verification commands

As replay-asset §9; expected `ok 11082
fraud_proofs/validation_trace/value_and_mint_output_finish_semantic_v1.main.spend`
(±drift).

## 10. Ordering and dependencies

Lands with the group's regeneration; depends only on the arm split of
`value_and_mint_stage_three` (shared with output-descriptor and output-asset).

## 11. Risks

None specific: 3.9k margin, no ABI change, C49 predicate unchanged.
