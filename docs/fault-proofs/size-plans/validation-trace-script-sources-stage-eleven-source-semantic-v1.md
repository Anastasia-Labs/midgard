# `script_sources_stage_eleven_source_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
§4a.1 (sliced discovery-stage binding). **Strategy: prune only — no new validator.**

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_eleven_source_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-eleven-source-semantic-v1.ak` |
| Raw size | 28,894 bytes |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 17 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 17) = 49` |
| Library entry point | `verify_script_sources_stage_eleven_source_semantics_v1` → `script_sources_stage_eleven_control_from_witness`, `script_sources_stage_eleven_control_is_bound` (**`exact_script_sources_control`**), `script_sources_stage_nine_source_is_authenticated` (`source_descriptor_leaf_hash` + `verify_membership`), `script_discovery_bitmap_has`, `script_discovery_successor_is_exact` / `rejected_successor_is_exact(reject_invalid_field_type)` |
| Redeemer action | `VerifySource { input_index, output_index, transition, source_index, origin_kind, source_key, script_language_tag, script_hash, script_total_length, script_item_commitment, siblings }`; auxiliary rebuilt as `ScriptSourceScanWitness` (constructor 9, 8 fields) |
| Rejection reached | `reject_invalid_field_type` (`E_INVALID_FIELD_TYPE`) for an unused inline source |
| Role / deployment entry today | none (anchor §1) |

What the step proves (C45 "unused/extraneous rejection"): after discovery,
source `source_cursor` is authenticated against the source frontier; an inline
source (`origin_kind == 0`) that no purpose used (`!bitmap_has(used_inline_bitmap, i)`)
yields the exact rejecting terminal; otherwise `source_cursor + 1` in stage 11.

## 2. Why it is this size

| Probe | Reachable code | Raw bytes |
| --- | --- | ---: |
| p02-shaped | `script_sources_stage_eleven_control_from_witness` (same 31-item decoder as stage ten) | ≈ 3,619 |
| p03 / p04 | stage binding via `exact_script_sources_control` / sliced | 23,522 / 6,067 |
| p20 | whole eleven-source predicate with the sliced binding (`source_is_authenticated`, bitmap check, both successors; no shell) | 8,462 |
| nine-mismatch / nine-effectful-match | today's stage-nine siblings with the same auxiliary and sliced binding | 10,668 / 10,931 |
| baseline | today's resolver | 28,894 |
| **q27** | **resolver-shaped prototype with the sliced binding** | **11,156** |

The 17.7 KB gap is `exact_script_sources_control`; the stage-nine resolvers
prove the rest fits.

## 3. Options considered

Prune chosen (11,156); yield/chain/redesign rejected (anchor §3 reasoning —
nothing heavy remains; the membership proof is ≈ 0.8 KB).

## 4. Chosen design

Library only: `script_sources_stage_eleven_control_is_bound` →
`script_sources_discovery_control_is_bound(pre, witness, control, 11, #"0b")`
plus its stage-eleven discovery predicates (`purpose_count == purpose_cursor`,
current/matched fields reset, cursors in range). Validator file, `ActionV1`,
parameters, datum, evidence hash **unchanged**; handshake and security
unchanged. Soundness: anchor §4a.1 induction (stage 11 is entered from stage 8's
finish via `script_discovery_successor_is_exact`).

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +73) | Signed publication (≈ +276) |
| --- | ---: | ---: | ---: |
| eleven-source resolver (q27) | 11,156 | ≈ 11,230 | ≈ 11,500 — fits, margin ≈ 3,840 |

Referenced bytes ≈ 11,230 (tier 1). ExUnits unmeasured; less than today.

## 6. Off-chain work

Anchor §6 group items only: roster entry
`17: "validationTraceDisputeScriptSourcesStageElevenSourceSemantic"`,
descriptor/manifest entries, funding row (≈ 11.5 KB). No redeemer/codec change
(`semanticActionFieldsV1` for 17 stays `[...base, ...scriptSourceScan fields]`; shape `[9, 8]`).

## 7. Emulator scenario tests

Add `tests/submit-init-emulator-script-sources-stage-eleven-source-v1.test.ts`
on the anchor fixture: (1) the effectful transaction's used inline source
(advance), (2) a variant carrying an **extra unused inline script** in field 6
(rejection → award), (3) a reference-origin source (`origin_kind != 0`,
advance regardless of bitmap). Publication fit without `oversized`; positive
lifecycle to award; valid-block negative (challenger claims rejection for a
used source); cancel; maximum shape: `source_count = max_tx_size_derived_collection_item_count`
with `used_inline_bitmap = 2^source_count - 1`.

## 8. Aiken tests

Keep `script_sources_stage_eleven_audits_inline_sources_exactly` and
`script_sources_stage_eleven_rejects_an_unused_inline_source_exactly`
(`validation-machine-v1.test.ak:8442/8540`). Add the shared
`script_sources_discovery_control_is_bound_agrees_with_exact_encoding`
property with a stage-11 case.

## 9. Verification commands

Anchor §9 (≈ 11,156 in the sweep) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-eleven-source-v1.test.ts`.

## 10. Ordering and dependencies

Anchor §10; shares the sliced stage-eleven binding with `eleven-finish`.

## 11. Risks

Anchor §11 (induction assumption). No ABI churn beyond the hash.
