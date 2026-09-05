# `value_and_mint_output_descriptor_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md). Strategy: **prune only** via the
library arm split defined in
[`validation-trace-value-and-mint-replay-asset-semantic-v1.md`](validation-trace-value-and-mint-replay-asset-semantic-v1.md)
§4a. No ABI change, no new validator. This is the tightest prune-only fit in
the group (14,336).

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/value_and_mint_output_descriptor_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/value-and-mint-output-descriptor-semantic-v1.ak` |
| Raw size (2026-09-01 build) | 21,161 bytes (applied 21,207, #634 note; the #627 min-Ada journey measured this resolution at 21,576 complete signed bytes) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` |
| Phase / resolver index | `ValueAndMint`, resolver 12 |
| Semantic index (arm) | 5 of 11; global slot `validationSemanticResolverGlobalIndexV1(12, 5)` |
| Library entry point | `verify_value_and_mint_output_descriptor_semantics_v1` → `value_and_mint_stage_three` with `ValueOutputDescriptorWitness` |
| Redeemer action | `VerifyOutputDescriptor { input_index, output_index, transition, ledger_output_index, descriptor_cbor, siblings }` |
| Role name today | none |
| Deployment entry today | `validationTraceDisputeValueAndMintOutputDescriptorSemantic` (`…ENTRIES_V1[5]`) |
| SDK title key | `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.valueAndMintOutputDescriptor` (`contracts.ts:472`) |

What the step proves: stage 3 with outputs pending and
`output_asset_cursor == 0` — the descriptor at `output_index ==
control.output_cursor` is a member of `native_control.output_descriptor_peaks`
(`validation_merkle_v1.verify_membership` over
`script_proof_v1.output_descriptor_leaf_hash`), decodes, and either fails the
parameterized minimum-Ada floor (`output_meets_min_ada_v1(env.coins_per_utxo_byte,
total_length, lovelace)` → exact `E_MIN_ADA` rejection; #618 ruling 1, C49
min-Ada) or its lovelace is subtracted and the output either completes
(`asset_count == 0`) or opens its asset cursor.

## 2. Why it is this size

Full table: replay-asset plan §2a. Rows reachable from this arm:

| Reachable code | Raw Δ |
| --- | ---: |
| scaffold | 3,810 |
| `value_and_mint_control_from_witness` | +2,098 |
| `value_and_mint_verified_body_v1` | +5,239 |
| `ledger_output_commitment_v1.decode` | +1,650 |
| `validation_merkle_v1.verify_membership` | +508 |
| `rejected_successor_is_exact` (min-Ada rejection) | +471 |
| `output_meets_min_ada_v1` + `output_descriptor_leaf_hash` | +102 |
| `value_and_mint_successor_is_exact` | +326 |

Unreachable after the split: the stage-3 asset arm (`verify_asset_membership`
+802, `apply_value_asset_mutation` +5,388) and the finish arm.

| Prototype | Raw bytes | ≤ 15,000 |
| --- | ---: | :-: |
| today | 21,161 | no |
| `pr_output_descriptor` — stage-3 descriptor arm only (verbatim arm body) | **14,336** | yes (margin 664) |

## 3. Options considered

| Option | Verdict | Reason |
| --- | --- | --- |
| **1. Prune (arm split)** | **chosen** | 14,336 measured, ABI-neutral; margin is small but real (applied ≈14,409, signed ≈14,690 vs 16,384) |
| 2. Yield (descriptor decode behind the asset-fold yield) | rejected for now, **documented fallback** | would drop to ≈12,700 but joins the claim ABI and adds a reference input to the most common output step; only justified if drift erodes the 664-byte margin |
| 3. Chain | rejected | no budget need |
| 4. Redesign | rejected | arm boundary is right |

## 4. Chosen design

No new validators, roles, parameters or redeemer changes. In
`lib/midgard/validation-machine-v1.ak`:

```
fn value_and_mint_stage_three_descriptor_arm(pre, witness, control, output_index, descriptor_cbor, siblings) -> Bool
```

holds today's `else if control.output_asset_cursor == 0` branch of
`value_and_mint_stage_three` verbatim, including the E_MIN_ADA comment block
and the `rejected_successor_is_exact(pre, witness.claimed_successor,
reject_min_ada)` branch. `verify_value_and_mint_output_descriptor_semantics_v1`
keeps its pins (`stage == 3`, `output_cursor < output_count`,
`output_asset_cursor == 0`) and calls the arm directly.

Handshake and security argument: unchanged `continue_winning`; same
conjunction as the aggregate's branch; no dispatch/role/yield to substitute or
omit. The min-Ada rejection path stays reachable (it is inside the arm), so
`output_descriptor_validator_wins_the_min_ada_rejection` is the guard that the
prune did not drop the conviction path this resolver exists for.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (+73) | Signed publication (≈+276) |
| --- | ---: | ---: | ---: |
| `value_and_mint_output_descriptor_semantic_v1` | 14,336 | ≈14,409 | ≈14,690 — fits, margin ≈1,700 to `maxTxSize` |

Referenced bytes per semantic-resolution transaction ≈ 14.4 KB (first fee
tier). ExUnits: not more than today.

## 6. Off-chain work

Nothing new for this contract. Stays: `contracts.ts` title
`valueAndMintOutputDescriptor`, deployment entry
`validationTraceDisputeValueAndMintOutputDescriptorSemantic`, submit
flattening for semantic 5 (`VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1[5]
= valueOutputDescriptor`, with the `ledger_output_index` rename noted at
`submit.ts:3781`). Group-level: hash change, catalogue re-pin, removal of the
`oversized` ValueAndMint path. Not needed: role, yield, funding row.

## 7. Emulator scenario tests

Exists today: the #627 min-Ada journey fixture in
`tests/support/emulator/validation-dispute-fixtures.ts:516` ("E_MIN_ADA wiring
in the ValueAndMint output ladder") already disputes this resolver's
rejection step, currently via the oversized publication path
(`dispute-scenario.ts:352–425`). Add
`tests/submit-init-emulator-value-and-mint-output-descriptor-v1.test.ts`:
publish the resolver without `oversized` (margin > 0); positive lifecycle for
both branches — the E_MIN_ADA conviction (existing fixture) and the honest
fold step (first honest state with `stage == 3 && output_asset_cursor == 0`) —
through award and removal; valid-block negative at the same frontier (operator
funds the floor exactly, challenger claims E_MIN_ADA: refused); `ct.Cancel`;
maximum shape: descriptor with `total_length = 16,384`
(`max_serialized_output_preimage_bytes`), `cardano_value_size = 5,000` (C22)
and `asset_count = 16,384` opening the asset cursor, with 14 descriptor
siblings; assert signed bytes ≤ 16,384 and print `semanticMeasurement`.

## 8. Aiken tests

Must keep passing: `output_descriptor_validator_wins_the_descriptor_step`,
`output_descriptor_validator_wins_the_min_ada_rejection`,
`output_descriptor_validator_refuses_output_asset`,
`output_descriptor_validator_refuses_output_finish`,
`output_descriptor_wire_layout_is_pinned`,
`prepare_routes_output_descriptor_to_slot_five` (split file);
`value_and_mint_output_descriptor_route_agrees_with_the_aggregate`,
`value_and_mint_authenticates_output_descriptor_on_l1`,
`value_and_mint_output_descriptor_admits_exact_min_ada_floor`,
`value_and_mint_output_descriptor_rejects_min_ada_short_by_one_lovelace`,
`value_and_mint_output_descriptor_rejects_zero_lovelace_output`,
`value_and_mint_output_descriptor_min_ada_code_is_not_interchangeable`, and
the partition test (machine file). Add
`output_descriptor_validator_wins_an_asset_free_output` (`asset_count = 0`,
successor `output_cursor + 1`).

## 9. Verification commands

As replay-asset §9; expected `ok 14336
fraud_proofs/validation_trace/value_and_mint_output_descriptor_semantic_v1.main.spend`
(±drift; **any regeneration that prints > 15,000 here triggers the §3 fallback**).

## 10. Ordering and dependencies

Lands with the group's regeneration. Depends only on the arm split of
`value_and_mint_stage_three` (shared with output-finish and output-asset).

## 11. Risks

- **Margin:** 664 bytes to the 15,000 target is the smallest in the group.
  `env.coins_per_utxo_byte` is a compiled constant, so a rate change does not
  move the size, but any growth in `ledger_output_commitment_v1.decode`
  (e.g. a new descriptor field) lands here first. Fallback documented in §3.
- ABI: none. Spec: none — the E_MIN_ADA conviction path (C49 min-Ada, #618
  ruling 1, R8 of decision 0005) is unchanged and guarded by the existing
  rejection vectors.

## Implementation evidence (2026-09-05)

The library branch extraction is implemented in the isolated
`codex/value-semantics-fit` worktree. It preserves the existing datum,
parameters, evidence hash, and semantic guards. Integration remains grouped
with the three asset-fold yielding resolvers required by the replay-asset plan.

The pinned testnet build `b7803f9a2ddec61f9de2a0c98c5b6c7509b95cbfb0e210473c41f193fb74f74e` measures 14,425 raw bytes for this
resolver. Its fully applied signed publication is 14,746 bytes,
leaving 1,638 bytes of ledger margin and
1,126 bytes beyond the 512-byte publication reserve.

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
