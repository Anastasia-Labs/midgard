# `script_sources_stage_eight_finish_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
§4a.1 (sliced discovery-stage binding). **Strategy: prune only — no new validator.**

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_eight_finish_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-eight-finish-semantic-v1.ak` |
| Raw size | 27,113 bytes |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 23 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 23) = 55` |
| Library entry point | `verify_script_sources_stage_eight_finish_semantics_v1` → `script_sources_stage_eight_control_from_witness`, `script_sources_stage_eight_control_is_bound` (**`exact_script_sources_control`**), `script_discovery_successor_is_exact(…, 11, reset_script_discovery_current(discovery))` |
| Redeemer action | `VerifyFinish { input_index, output_index, transition }`; auxiliary `NoAuxiliaryWitness` |
| Role / deployment entry today | none (anchor §1) |

What the step proves (C45): every purpose has been discovered
(`purpose_cursor == purpose_count`) and the machine enters the stage-11
inline-source audit with the current fields reset.

## 2. Why it is this size

| Probe | Reachable code | Raw bytes |
| --- | --- | ---: |
| p03 / p04 | discovery-stage binding via `exact_script_sources_control` / sliced | 23,522 / 6,067 |
| q13 | whole eight-finish predicate with the sliced binding (no shell) | 6,506 |
| baseline | today's resolver | 27,113 |
| **q29** | **resolver-shaped prototype with the sliced binding** | **8,875** |

## 3. Options considered

Prune chosen (8,875); yield/chain/redesign rejected (one comparison and one successor).

## 4. Chosen design

Library only: sliced `script_sources_stage_eight_control_is_bound` (as
`eight-purpose` §4, shared function). Validator file, `ActionV1`, parameters,
datum, evidence hash **unchanged**; handshake/security unchanged; soundness per
anchor §4a.1.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +73) | Signed publication (≈ +276) |
| --- | ---: | ---: | ---: |
| eight-finish resolver (q29) | 8,875 (+ ≤ 300 predicates) | ≈ 9,250 | ≈ 9,520 — fits, margin ≈ 5,700 |

Referenced bytes ≈ 9,250 (tier 1). ExUnits unmeasured; less than today.

## 6. Off-chain work

Anchor §6 group items only: roster entry
`23: "validationTraceDisputeScriptSourcesStageEightFinishSemantic"`,
descriptor/manifest entries, funding row (≈ 9.5 KB). No redeemer/codec change
(shape `none`).

## 7. Emulator scenario tests

Add `tests/submit-init-emulator-script-sources-stage-eight-finish-v1.test.ts`:
reachable from the existing script-free honest transaction once the
TypeScript stage 7–12 producer exists (zero purposes → stage 8 finish
immediately after `seven-finish`), and from the effectful fixture after its
purpose completes. Publication fit without `oversized`; positive lifecycle to
award; valid-block negative (`purpose_cursor < purpose_count`); cancel;
maximum shape: `execution_count = purpose_count = max_tx_size_derived_collection_item_count`
(32-peak execution frontier).

## 8. Aiken tests

Keep `script_sources_stage_eight_finishes_empty_discovery_exactly` and
`stage_eight_finish_pending_redeemer_hash_divergence_is_unreachable`; the
shared sliced-binding agreement property covers stage 8.

## 9. Verification commands

Anchor §9 (≈ 8,875 in the sweep) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-eight-finish-v1.test.ts`.

## 10. Ordering and dependencies

Anchor §10; shares the sliced stage-eight binding with `eight-purpose`.

## 11. Risks

Anchor §11. No ABI churn beyond the hash.
