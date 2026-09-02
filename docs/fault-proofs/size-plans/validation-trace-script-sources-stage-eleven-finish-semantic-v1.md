# `script_sources_stage_eleven_finish_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
§4a.1 (sliced discovery-stage binding). **Strategy: prune only — no new validator.**

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_eleven_finish_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-eleven-finish-semantic-v1.ak` |
| Raw size | 27,044 bytes |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 16 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 16) = 48` |
| Library entry point | `verify_script_sources_stage_eleven_finish_semantics_v1` → `script_sources_stage_eleven_control_from_witness`, `script_sources_stage_eleven_control_is_bound` (**`exact_script_sources_control`**), `script_discovery_successor_is_exact(…, 12, { ..discovery, redeemer_cursor: 0 })` |
| Redeemer action | `VerifyFinish { input_index, output_index, transition }`; auxiliary `NoAuxiliaryWitness` |
| Role / deployment entry today | none (anchor §1) |

What the step proves (C45): the inline-source audit is exhausted
(`source_cursor == source_count`) and the machine enters the stage-12 redeemer
audit with `redeemer_cursor = 0`.

## 2. Why it is this size

| Probe | Reachable code | Raw bytes |
| --- | --- | ---: |
| p03 / p04 | discovery-stage binding via `exact_script_sources_control` / sliced | 23,522 / 6,067 |
| q14 | whole eleven-finish predicate with the sliced binding (no shell) | 6,407 |
| baseline | today's resolver | 27,044 |
| **q30** | **resolver-shaped prototype with the sliced binding** | **8,776** |

## 3. Options considered

Prune chosen (8,776); yield/chain/redesign rejected.

## 4. Chosen design

Library only: sliced `script_sources_stage_eleven_control_is_bound` (shared with
`eleven-source`, stage byte `#"0b"`). Validator file, `ActionV1`, parameters,
datum, evidence hash **unchanged**; handshake/security unchanged; soundness per
anchor §4a.1.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +73) | Signed publication (≈ +276) |
| --- | ---: | ---: | ---: |
| eleven-finish resolver (q30) | 8,776 (+ ≤ 300 predicates) | ≈ 9,150 | ≈ 9,420 — fits, margin ≈ 5,800 |

Referenced bytes ≈ 9,150 (tier 1). ExUnits unmeasured; less than today.

## 6. Off-chain work

Anchor §6 group items only: roster entry
`16: "validationTraceDisputeScriptSourcesStageElevenFinishSemantic"`,
descriptor/manifest entries, funding row (≈ 9.4 KB). No redeemer/codec change.

## 7. Emulator scenario tests

Add `tests/submit-init-emulator-script-sources-stage-eleven-finish-v1.test.ts`:
reachable from the script-free honest transaction (zero sources) once the
TypeScript producer exists, and from the effectful fixture. Publication fit
without `oversized`; positive lifecycle to award; valid-block negative
(`source_cursor < source_count`); cancel; maximum shape:
`source_count = max_tx_size_derived_collection_item_count`.

## 8. Aiken tests

Keep `script_sources_stage_eleven_audits_inline_sources_exactly`; the shared
sliced-binding agreement property covers stage 11; add
`eleven_finish_enters_stage_twelve_with_zero_redeemer_cursor` (golden successor).

## 9. Verification commands

Anchor §9 (≈ 8,776 in the sweep) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-eleven-finish-v1.test.ts`.

## 10. Ordering and dependencies

Anchor §10; shares the sliced stage-eleven binding with `eleven-source`.

## 11. Risks

Anchor §11. No ABI churn beyond the hash.
