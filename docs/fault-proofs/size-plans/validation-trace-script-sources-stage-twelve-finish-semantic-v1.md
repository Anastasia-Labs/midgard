# `script_sources_stage_twelve_finish_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
§4a.1 (sliced discovery-stage binding). **Strategy: prune only — no new validator.**

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_twelve_finish_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-twelve-finish-semantic-v1.ak` |
| Raw size | 27,042 bytes |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 18 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 18) = 50` |
| Library entry point | `verify_script_sources_stage_twelve_finish_semantics_v1` → `script_sources_stage_twelve_control_from_witness`, `script_sources_stage_twelve_control_is_bound` (**`exact_script_sources_control`**), `encode_native_scripts_witness_v1(control, 0, 0)`, `hash_work_witness(NativeScripts, pc + 1, …)` |
| Redeemer action | `VerifyFinish { input_index, output_index, transition }`; auxiliary `NoAuxiliaryWitness` |
| Role / deployment entry today | none (anchor §1) |

What the step proves (C45 → C44 handoff): the redeemer audit is exhausted
(`redeemer_cursor == redeemer_count`, no open item scan) and the successor is
the **NativeScripts** phase with the 26-item native-scripts control
(`execution_cursor = 0`, `language_bitmap = 0`) — the only stage 7–12 step
whose successor leaves the ScriptSources phase.

## 2. Why it is this size

| Probe | Reachable code | Raw bytes |
| --- | --- | ---: |
| p03 / p04 | discovery-stage binding via `exact_script_sources_control` / sliced | 23,522 / 6,067 |
| p19 | whole twelve-finish predicate with the sliced binding (`encode_native_scripts_witness_v1` + work-root hash; no shell) | 7,195 |
| baseline | today's resolver | 27,042 |
| **q31** | **resolver-shaped prototype with the sliced binding** | **9,560** |

`encode_native_scripts_witness_v1` (26 concatenations, frontier encoders) is
≈ 1.1 KB — the reason this finish is ≈ 700 bytes larger than the stage-8/11 ones.

## 3. Options considered

Prune chosen (9,560); yield/chain/redesign rejected.

## 4. Chosen design

Library only: sliced `script_sources_stage_twelve_control_is_bound` (shared
with `twelve-redeemer`, stage byte `#"0c"`, predicates `purpose_cursor == purpose_count`,
`source_cursor == source_count`, current/matched reset). Validator file,
`ActionV1`, parameters, datum, evidence hash **unchanged**; the NativeScripts
handoff encoding is untouched (the `native_scripts_v1` prepare group is another
plan's concern and does not change). Soundness per anchor §4a.1.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +73) | Signed publication (≈ +276) |
| --- | ---: | ---: | ---: |
| twelve-finish resolver (q31) | 9,560 (+ ≤ 300 predicates) | ≈ 9,930 | ≈ 10,210 — fits, margin ≈ 5,000 |

Referenced bytes ≈ 9,930 (tier 1). ExUnits unmeasured; less than today.

## 6. Off-chain work

Anchor §6 group items only: roster entry
`18: "validationTraceDisputeScriptSourcesStageTwelveFinishSemantic"`,
descriptor/manifest entries, funding row (≈ 10.2 KB). No redeemer/codec change.

## 7. Emulator scenario tests

Add `tests/submit-init-emulator-script-sources-stage-twelve-finish-v1.test.ts`:
reachable from the script-free honest transaction (zero redeemers) once the
TypeScript producer exists — this is the phase boundary, so the fixture also
cross-checks the TypeScript `encode_native_scripts_witness_v1` port against
the honest trace's NativeScripts work root. Publication fit without
`oversized`; positive lifecycle to award; valid-block negative (open item scan:
`redeemer_item_control_hash != ""`); cancel; maximum shape: every frontier at
32 peaks (largest 26-item native-scripts control).

## 8. Aiken tests

Keep `stage_twelve_finish_pending_redeemer_hash_divergence_is_unreachable` and
`script_sources_stage_twelve_audits_redeemers_exactly`; the shared
sliced-binding agreement property covers stage 12; add
`twelve_finish_native_scripts_handoff_is_exact` (golden work root).

## 9. Verification commands

Anchor §9 (≈ 9,560 in the sweep) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-twelve-finish-v1.test.ts`.

## 10. Ordering and dependencies

Anchor §10; shares the sliced stage-twelve binding with `twelve-redeemer`.
The NativeScripts control ABI it emits is consumed by the native-scripts
resolvers (not in this program's oversized set) — unchanged.

## 11. Risks

Anchor §11. No ABI churn beyond the hash.
