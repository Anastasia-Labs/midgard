# `value_and_mint_replay_input_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md). Strategy: **prune only** (primer
pattern 1) via the library arm split defined in
[`validation-trace-value-and-mint-replay-asset-semantic-v1.md`](validation-trace-value-and-mint-replay-asset-semantic-v1.md)
§4a. No ABI change, no new validator.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/value_and_mint_replay_input_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/value-and-mint-replay-input-semantic-v1.ak` |
| Raw size (2026-09-01 build) | 21,320 bytes (applied 21,367, #634 note in `submit.ts`) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` |
| Phase / resolver index | `ValueAndMint`, resolver 12 |
| Semantic index (arm) | 2 of 11; global slot `validationSemanticResolverGlobalIndexV1(12, 2)` |
| Library entry point | `verify_value_and_mint_replay_input_semantics_v1` → `value_and_mint_stage_two` with `ResolvedInputReplayWitness` |
| Redeemer action | `VerifyReplayInput { input_index, output_index, transition, source_kind, key, next_schedule_hash, value }` |
| Role name today | none |
| Deployment entry today | `validationTraceDisputeValueAndMintReplayInputSemantic` (`…ENTRIES_V1[2]`) |
| SDK title key | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.valueAndMintReplayInput` (`contracts.ts:466`) |

What the step proves: stage 2 with the replay schedule pending and
`replay_asset_cursor == 0` — the next scheduled input `(source_kind, key)`
hashes to `replay_remaining_schedule_hash`, its descriptor `value` decodes,
its lovelace is added for spends (`source_kind == 0`), and either the input
completes (`complete_value_input_replay`: cursor +1, accumulator successor,
schedule advanced) when it is a reference input or has no assets, or the
asset cursor opens at 1 with `replay_value_hash = blake2b_256(value)`.

## 2. Why it is this size

Full table: replay-asset plan §2a. Rows reachable from this arm:

| Reachable code | Raw Δ |
| --- | ---: |
| scaffold | 3,810 |
| `value_and_mint_control_from_witness` | +2,098 |
| `value_and_mint_verified_body_v1` | +5,239 |
| `ledger_output_commitment_v1.decode` | +1,650 |
| `resolved_input_accumulator_successor` + `resolution_schedule_node_hash` | +158 |
| `value_and_mint_successor_is_exact` | +326 |

Unreachable after the split (reached today through the whole
`value_and_mint_stage_two`): `apply_value_asset_mutation` (+5,388),
`verify_asset_membership` (+802), `rejected_successor_is_exact` (+471) — the
asset arm — and the finish arm.

| Prototype | Raw bytes | ≤ 15,000 |
| --- | ---: | :-: |
| today | 21,320 | no |
| `pr_replay_input` — stage-2 input arm only (verbatim arm body) | **13,725** | yes (margin 1,275) |

## 3. Options considered

| Option | Verdict | Reason |
| --- | --- | --- |
| **1. Prune (arm split)** | **chosen** | 13,725 measured; ABI-neutral; proves exactly what it proves today |
| 2. Yield | rejected | not needed; would add a role, a reference input and a parse for no size need |
| 3. Chain | rejected | no budget need |
| 4. Redesign | rejected | arm boundary is right |

## 4. Chosen design

No new validators, roles, parameters or redeemer changes. In
`lib/midgard/validation-machine-v1.ak`:

```
fn value_and_mint_stage_two_input_arm(pre, witness, control, source_kind, key, next_schedule_hash, value) -> Bool
```

holds today's `else if control.replay_asset_cursor == 0` branch of
`value_and_mint_stage_two` verbatim (after the `expect
ResolvedInputReplayWitness {…} = auxiliary`, which stays in the aggregate).
`verify_value_and_mint_replay_input_semantics_v1` keeps its control decode,
`value_and_mint_verified_body_v1`, `control.stage == 2`,
`replay_remaining_schedule_hash != empty_resolution_schedule_hash()`,
`replay_asset_cursor == 0` pins and calls the arm with its four action fields
instead of `value_and_mint_stage_two(…, ResolvedInputReplayWitness {…}, …)`.

Handshake: unchanged (`continue_winning` in `main.spend`; `ct.Cancel` arm
unchanged). Security argument: the predicate is the same conjunction the
aggregate evaluates on this branch; the discriminators the resolver pins are
exactly the ones the aggregate uses to select the branch, so
`value_and_mint_replay_input_route_agrees_with_the_aggregate` and the
partition test continue to hold. There is no dispatch, role or yield to
substitute or omit.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (+73) | Signed publication (≈+276) |
| --- | ---: | ---: | ---: |
| `value_and_mint_replay_input_semantic_v1` | 13,725 | ≈13,798 | ≈14,075 — fits, margin ≈2,300 |

Referenced bytes per semantic-resolution transaction ≈ 13.8 KB (first fee
tier). ExUnits: strictly not more than today (same code path executed, less
code loaded); no measurement needed beyond the §7 journey's
`semanticMeasurement`.

## 6. Off-chain work

Nothing new for this contract. Exists today and stays: `contracts.ts` title
`valueAndMintReplayInput`, deployment entry
`validationTraceDisputeValueAndMintReplayInputSemantic`, submit flattening
`[...base, ...auxiliary.fields]` for semantic 2
(`VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1[2] = resolvedInputReplay`).
Group-level: the hash changes (re-applied `value_and_mint_v1`, catalogue root
re-pin) and the removal of the ValueAndMint `oversized` path in
`dispute-scenario.ts` and of the "publish it" refusal in `submit.ts:6091–6104`
once all eleven fit. Does not exist today and is **not** needed: a role, a
yield, a funding row.

## 7. Emulator scenario tests

Exists today: `begin` journey only. Add
`tests/submit-init-emulator-value-and-mint-replay-input-v1.test.ts` (one
journey per file): fixture selects the first honest state with `stage == 2`,
schedule pending and `replay_asset_cursor == 0` (the honest native
transaction already has a resolved spend input); publish the resolver without
`oversized` (margin > 0); positive lifecycle through award and removal;
valid-block negative at the same frontier (forged `next_schedule_hash`, or a
`value` whose hash differs — `continue_winning`'s evidence hash refuses first,
so also a forged claimed successor); `ct.Cancel`; maximum shape: descriptor at
the C22 boundary (`cardano_value_size = 5,000`) for a reference-input replay
(`source_kind == 1`, completes in one step) and for a spend with
`asset_count > 0` (opens the asset cursor); assert signed bytes ≤ 16,384.

## 8. Aiken tests

No new validators, so no new substitution/omission vectors. Must keep passing:
`replay_input_validator_wins_an_asset_free_input_replay`,
`replay_input_validator_refuses_replay_asset`,
`replay_input_validator_refuses_replay_finish`,
`replay_input_wire_layout_is_pinned`, `prepare_routes_replay_input_to_slot_two`
(`value-and-mint-split-v1.test.ak`);
`value_and_mint_replay_input_route_agrees_with_the_aggregate`,
`value_and_mint_kinds_partition_the_value_and_mint_step_space`
(`validation-machine-v1.test.ak`). Add one vector for the arm boundary the
split introduces: `replay_input_validator_wins_a_spend_that_opens_the_asset_cursor`
(descriptor with `asset_count = 1`, successor `replay_asset_cursor = 1`,
`replay_value_hash = blake2b_256(value)`).

## 9. Verification commands

As replay-asset §9; expected `ok 13725
fraud_proofs/validation_trace/value_and_mint_replay_input_semantic_v1.main.spend`
(±drift) and `aiken check -m value_and_mint` all green (62 + 1 in the split
file, 22 in the machine file).

## 10. Ordering and dependencies

Lands with the group's regeneration. Depends only on the shared arm split of
`value_and_mint_stage_two` (also used by replay-finish and replay-asset). No
yield dependency.

## 11. Risks

- Regeneration drift: margin 1,275 to target. Fallback if ever exceeded: the
  descriptor decode could move behind the asset-fold yield's `descriptor`
  claim (−1,650), at the cost of joining the yield ABI — not planned.
- ABI: none. Spec: none (C49 predicate unchanged; C22 boundary still enforced
  locally by `decode`).
