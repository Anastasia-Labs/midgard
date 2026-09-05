# `value_and_mint_replay_finish_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md). Strategy: **prune only** via the
library arm split defined in
[`validation-trace-value-and-mint-replay-asset-semantic-v1.md`](validation-trace-value-and-mint-replay-asset-semantic-v1.md)
§4a. No ABI change, no new validator.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/value_and_mint_replay_finish_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/value-and-mint-replay-finish-semantic-v1.ak` |
| Raw size (2026-09-01 build) | 21,091 bytes (applied 21,138, #634 note in `submit.ts`) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` |
| Phase / resolver index | `ValueAndMint`, resolver 12 |
| Semantic index (arm) | 4 of 11; global slot `validationSemanticResolverGlobalIndexV1(12, 4)` |
| Library entry point | `verify_value_and_mint_replay_finish_semantics_v1` → `value_and_mint_stage_two` with `NoAuxiliaryWitness` |
| Redeemer action | `VerifyReplayFinish { input_index, output_index, transition }` (transition-only wire layout shared with begin/replay_begin/output_finish/mint_finish/finalize) |
| Role name today | none |
| Deployment entry today | `validationTraceDisputeValueAndMintReplayFinishSemantic` (`…ENTRIES_V1[4]`) |
| SDK title key | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.valueAndMintReplayFinish` (`contracts.ts:470`) |

What the step proves: stage 2 with the replay schedule exhausted
(`replay_remaining_schedule_hash == empty_resolution_schedule_hash()`) — the
replay cursor equals `native_control.resolved_input_count`, the asset cursor
is 0, `replay_value_hash` is cleared, the replay accumulator equals
`native_control.resolved_inputs_accumulator`, and the successor is the same
control at stage 3.

## 2. Why it is this size

A transition-only resolver that weighs 21,091 bytes because it calls the
**whole** `value_and_mint_stage_two`: the compiler reaches both sibling arms.
Full table: replay-asset plan §2a.

| Reachable code | Raw Δ | Reachable after the split |
| --- | ---: | :-: |
| scaffold | 3,810 | yes |
| `value_and_mint_control_from_witness` | +2,098 | yes |
| `value_and_mint_verified_body_v1` | +5,239 | yes |
| `value_and_mint_successor_is_exact` | +326 | yes |
| `ledger_output_commitment_v1.decode` (input + asset arms) | +1,650 | no |
| `verify_asset_membership` (asset arm) | +802 | no |
| `apply_value_asset_mutation` (asset arm) | +5,388 | no |
| `rejected_successor_is_exact` (asset arm) | +471 | no |
| `resolved_input_accumulator_successor`, schedule hash, `complete_value_input_replay` | +158 | no |

| Prototype | Raw bytes | ≤ 15,000 |
| --- | ---: | :-: |
| today | 21,091 | no |
| `p14_replay_finish_real` (same call on a rich redeemer, sanity) | 21,690 | no |
| `pr_replay_finish` — stage-2 finish arm only (verbatim) | **11,179** | yes (margin 3,821) |

`replay_begin` (11,013) and `begin` (11,473) are the natural comparison: a
transition-only value-and-mint step costs ≈11k, all of it the shared control
decode + verified body.

## 3. Options considered

| Option | Verdict | Reason |
| --- | --- | --- |
| **1. Prune (arm split)** | **chosen** | 11,179 measured; 9,912 bytes were code this resolver can never execute |
| 2–4 | rejected | no size or budget need |

## 4. Chosen design

No new validators, roles, parameters or redeemer changes. In
`lib/midgard/validation-machine-v1.ak`:

```
fn value_and_mint_stage_two_finish_arm(pre, witness, control) -> Bool {
  and {
    control.replay_cursor == control.native_control.resolved_input_count,
    control.replay_asset_cursor == 0,
    control.replay_value_hash == no_rejection_code_hash,
    control.replay_accumulator == control.native_control.resolved_inputs_accumulator,
    value_and_mint_successor_is_exact(pre, witness, ValueAndMintControlV1 { ..control, stage: 3 }),
  }
}
```

The aggregate `value_and_mint_stage_two` keeps `auxiliary ==
NoAuxiliaryWitness` and the `replay_remaining_schedule_hash == empty` test
and calls the arm; `verify_value_and_mint_replay_finish_semantics_v1` keeps
`control.stage == 2` and `replay_remaining_schedule_hash ==
empty_resolution_schedule_hash()` and calls the arm directly (its auxiliary is
the literal `NoAuxiliaryWitness`, so the equality it drops is a tautology).

Handshake and security argument: unchanged `continue_winning`; the finish
predicate is identical; no dispatch/role/yield exists to omit or substitute.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (+73) | Signed publication (≈+276) |
| --- | ---: | ---: | ---: |
| `value_and_mint_replay_finish_semantic_v1` | 11,179 | ≈11,252 | ≈11,530 — fits, margin ≈4,850 |

Referenced bytes ≈ 11.3 KB per semantic-resolution transaction. ExUnits: not
more than today.

## 6. Off-chain work

Nothing new. Stays: `contracts.ts` title `valueAndMintReplayFinish`, entry
`validationTraceDisputeValueAndMintReplayFinishSemantic`, submit base fields
`[input_index, output_index, transition]` for semantic 4
(`VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1[4] = none`). Group-level hash
change and catalogue re-pin only.

## 7. Emulator scenario tests

Exists today: `begin` journey only. Add
`tests/submit-init-emulator-value-and-mint-replay-finish-v1.test.ts`: fixture
selects the first honest state with `stage == 2` and an exhausted schedule
(every honest trace has exactly one such state); publish without `oversized`
(margin > 0); positive lifecycle through award and removal; valid-block
negative (forged claimed successor at the same frontier — refused by the
evidence hash / successor exactness); `ct.Cancel`. Maximum shape: none beyond
the transition (no auxiliary); assert signed bytes ≤ 16,384 with the largest
`NativeScriptsControlV1` the honest fixture produces.

## 8. Aiken tests

Must keep passing: `replay_finish_validator_wins_the_exhausted_schedule`,
`replay_finish_validator_refuses_replay_input`,
`replay_finish_validator_refuses_replay_asset`,
`replay_finish_validator_refuses_output_finish`,
`transition_only_wire_layouts_are_pinned`,
`prepare_routes_replay_finish_to_slot_four` (split file);
`value_and_mint_replay_finish_route_agrees_with_the_aggregate` and the
partition test (machine file). Add `replay_finish_validator_refuses_a_pending_schedule_with_zero_cursor`
(control with a pending schedule but cursor 0 — refused on the discriminator,
not by decoding the missing auxiliary).

## 9. Verification commands

As replay-asset §9; expected `ok 11179
fraud_proofs/validation_trace/value_and_mint_replay_finish_semantic_v1.main.spend`
(±drift).

## 10. Ordering and dependencies

Lands with the group's regeneration; depends only on the arm split of
`value_and_mint_stage_two` (shared with replay-input and replay-asset).

## 11. Risks

None specific: 3.8k margin, no ABI change, no spec interaction beyond the
unchanged C49 predicate.

## Implementation evidence (2026-09-05)

The library branch extraction is implemented in the isolated
`codex/value-semantics-fit` worktree. It preserves the existing datum,
parameters, evidence hash, and semantic guards. Integration remains grouped
with the three asset-fold yielding resolvers required by the replay-asset plan.

The pinned testnet build `b7803f9a2ddec61f9de2a0c98c5b6c7509b95cbfb0e210473c41f193fb74f74e` measures 11,189 raw bytes for this
resolver. Its fully applied signed publication is 11,509 bytes,
leaving 4,875 bytes of ledger margin and
4,363 bytes beyond the 512-byte publication reserve.

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
