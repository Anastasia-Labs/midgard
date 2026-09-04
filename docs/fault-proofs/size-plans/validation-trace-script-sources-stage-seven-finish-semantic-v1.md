# `script_sources_stage_seven_finish_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
§4a.2 (stage-seven narrow encoder). **Strategy: prune only — no new validator.**

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_seven_finish_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-seven-finish-semantic-v1.ak` |
| Raw size | 29,390 bytes |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 27 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 27) = 59` |
| Library entry point | `verify_script_sources_stage_seven_finish_semantics_v1` → `script_sources_stage_seven_control_from_witness`, `decode_native_tx_compact_v1`, `script_sources_stage_seven_control_is_bound` (**`exact_script_sources_control`**), `script_sources_stage_seven_observer_scan_is_complete`, `script_sources_stage_seven_finish_successor_is_exact` (**`exact_script_sources_control`**, two branches) |
| Redeemer action | `VerifyFinish { input_index, output_index, transition }`; auxiliary `NoAuxiliaryWitness` |
| Role / deployment entry today | none (anchor §1) |

What the step proves (C45/C50): the observer scan is complete and the receive
scan cursor reached `receive_scan.source_count`. If no candidate remains
(`candidate_hash == ""`) the successor enters **stage 8** with `output_cursor = output_count`,
an empty receive scan keeping `descriptor_peaks`, empty observer scan and
empty discovery; otherwise it appends `purpose_leaf_hash(3, receive_count, h, h)`
for the candidate, advances `receive_count`, sets `previous_hash = h`, resets
`output_cursor` and stays in stage 7 for the next pass.

## 2. Why it is this size

| Probe | Reachable code | Raw bytes |
| --- | --- | ---: |
| p27 | control decode + `decode_native_tx_compact_v1` | 4,309 |
| p21 / p22 | stage-seven binding with `exact_script_sources_control` / with the narrow encoder | 23,124 / 7,846 |
| p23 | whole finish predicate with the narrow encoder for binding and both successor branches (no shell) | 11,014 |
| baseline | today's resolver | 29,390 |
| **q25** | **resolver-shaped prototype after the prune** | **13,614** |

The stage-8 successor branch is what makes the narrow encoder reach
`encode_script_sources_discovery_witness` (31-item form) in addition to the
30-item `encode_script_sources_witness`; both are already needed by the
observer/receive successors, so the finish resolver costs ≈ 200 bytes less
than receive.

## 3. Options considered

As `seven-receive` §3: prune chosen (13,614); yield-B split recorded as
fallback (margin 1,386); chain/redesign rejected.

## 4. Chosen design

Library only (anchor §4a.2): narrow binding; both branches of
`script_sources_stage_seven_finish_successor_is_exact` go through
`script_sources_stage_seven_successor_is_exact`. Validator file, `ActionV1`,
parameters, datum, evidence hash **unchanged**. This is the **induction base**
for the sliced discovery-stage binding (anchor §4a.1): the stage-8 successor is
produced here by the canonical encoder, so every stage ≥ 8 work witness is
canonical.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +73) | Signed publication (≈ +276) |
| --- | ---: | ---: | ---: |
| seven-finish resolver (q25) | 13,614 | ≈ 13,690 | ≈ 13,960 — fits, margin ≈ 1,390 |

Referenced bytes ≈ 13,690 (tier 1). ExUnits unmeasured; less than today.

## 6. Off-chain work

Anchor §6 group items only: roster entry
`27: "validationTraceDisputeScriptSourcesStageSevenFinishSemantic"`,
descriptor/manifest entries, funding row (≈ 14 KB). No redeemer/codec change
(`semanticActionFieldsV1` for 27 stays `[input_index, output_index, transition]`, shape `none`).

## 7. Emulator scenario tests

Add `tests/submit-init-emulator-script-sources-stage-seven-finish-v1.test.ts`
on the anchor fixture: **this is the one stage 7–12 step the existing
script-free honest transaction (`buildHonestAcceptedNativeTransactionTraceV1`)
already reaches** (no observers, no receive outputs → `finish` with
`candidate_hash == ""` straight into stage 8), so the fixture needs only the
TypeScript stage-7 producer and `disputedPhase: "scriptSources"`. Journeys:
the empty-scan stage-8 handoff, and (with the receive fixture) the
candidate-append branch. Publication fit without `oversized`; positive
lifecycle to award; valid-block negative (challenger claims finish while
`output_cursor < source_count`); cancel; maximum shape:
`receive_count = source_count - 1` with a 32-peak `purpose_peaks` frontier.

## 8. Aiken tests

Keep `script_sources_stage_seven_finish_binds_canonical_successor_encoding`
(`validation-machine-v1.test.ak:7094`). Add
`seven_finish_stage_eight_handoff_is_canonical_discovery_witness` (the
successor bytes equal `encode_script_sources_discovery_witness(control', 8, empty_script_discovery_control())`
and pass `script_sources_discovery_control_is_bound(…, 8, #"08")` — the
induction base) and the shared encoder-agreement property.

## 9. Verification commands

Anchor §9 (≈ 13,614 in the sweep) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-seven-finish-v1.test.ts`.

## 10. Ordering and dependencies

Shares anchor §4a.2 with `seven-observer` / `seven-receive`; the induction
base for `eight-*`, `ten-*`, `eleven-*`, `twelve-*` sliced bindings — land together.

## 11. Risks

As `seven-receive` §11 (margin 1,386; yield-B fallback). If the stage-8
handoff encoding ever changed, every discovery-stage binding's soundness
argument would need re-proving — the §8 handoff test is the guard.
