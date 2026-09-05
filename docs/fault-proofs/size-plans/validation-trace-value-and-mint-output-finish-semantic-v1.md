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

## Implementation evidence (2026-09-05)

The library branch extraction is implemented in the isolated
`codex/value-semantics-fit` worktree. It preserves the existing datum,
parameters, evidence hash, and semantic guards. Integration remains grouped
with the three asset-fold yielding resolvers required by the replay-asset plan.

The pinned testnet build `b7803f9a2ddec61f9de2a0c98c5b6c7509b95cbfb0e210473c41f193fb74f74e` measures 11,092 raw bytes for this
resolver. Its fully applied signed publication is 11,412 bytes,
leaving 4,972 bytes of ledger margin and
4,460 bytes beyond the 512-byte publication reserve.

Validation: 16 exact Aiken scenarios passed, including all eleven
specialized-versus-aggregate routes, the cross-kind partition, and four
min-Ada boundary/rejection cases. The five-script publication test passed,
as did the min-Ada semantic-resolution and ValueAndMint begin/removal
emulator journeys. These checks do not close the remaining asset-fold
yielding, maximum-evidence, or final-tree gates.


## Group integration evidence (2026-09-05)

The eight-plan ValueAndMint group is implemented together: exact library arm
extraction, three authenticated asset dispatchers, and one shared rewarding
validator. The three dispatchers receive the reference-script authentication
policy; the SDK applies their hashes to the yield afterward. The deployment
manifest, role identity, reference publication, reward registration, node
consumers, and semantic submitter use the same applied identities. The new
source names omit version suffixes; the wire role is `V1VtVamAssetFoldYield`.
Evidence hashes, prepare routing, and cancellation semantics are unchanged.

The normal pinned testnet blueprint
`c9179b11f5cc12b5d2fb75fba96cacef4a58086063af1ba0e5dda94d93fa84ef`
has all eleven semantic bodies and the shared yield below 15,000 raw bytes.
The eight signed semantic publications are at most 14,746 bytes;
all publish under the real protocol limits. The 581-row
[complete lifecycle ledger](validation-trace-value-and-mint-fit-ledger.json)
records at most 15,108 signed bytes, 7,361,187 memory units, and
2,718,905,240 CPU units. Every recorded transaction passed local evaluation.

Checks passed in the family worktree:

- `MIDGARD_AIKEN_ENV=testnet node scripts/guard-focused-selector.mjs fraud_proofs/validation_trace/value_and_mint_split_v1`:
  74/74, including authenticated yielding, substitution refusal, all three
  cancellation paths, and a property comparing all three split asset rules to
  the aggregate rule across positive quantities and signed mint quantities.
- `pnpm exec vitest run tests/value-and-mint-asset-yield-lifecycle.test.ts`:
  12/12, with three honest-block refusals and nine publication-to-removal
  journeys. These cover all asset arms, a real 1,304-asset output whose Cardano
  Value is exactly 5,000 bytes, and a committed one-step fixture combining
  14 asset siblings, 16 widest MPF branches, a 16,384-asset frontier, and a
  5,000-byte Value descriptor. The latter tests the selected one-step relation;
  it does not claim a valid earlier history for the fabricated accumulator.
- `tests/value-and-mint-publication-fit.test.ts`: all eight publications.
- Semantic submit encoding: 21/21; deployment identity: 12/12; node deployment
  descriptors: 19/19; SDK contract application: 30/30.

The shared-branch blueprint and ledgers must be regenerated after integration.
This group does not close the remaining validation-dispute workflow,
funding-roster, other semantic-group size plans, or final program review gates.
