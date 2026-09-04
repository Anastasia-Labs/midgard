# `script_sources_stage_eight_purpose_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
§4a.1 (sliced discovery-stage binding). **Strategy: prune only — no new validator.**

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_eight_purpose_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-eight-purpose-semantic-v1.ak` |
| Raw size | 28,166 bytes |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 24 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 24) = 56` |
| Library entry point | `verify_script_sources_stage_eight_purpose_semantics_v1` → `script_sources_stage_eight_control_from_witness`, `script_sources_stage_eight_control_is_bound` (**`exact_script_sources_control`**), `script_proof_v1.purpose_leaf_hash`, `validation_merkle_v1.verify_membership`, `script_discovery_successor_is_exact(…, 9, …)` |
| Redeemer action | `VerifyPurpose { input_index, output_index, transition, purpose_kind, purpose_index, script_hash, subject, siblings }`; auxiliary rebuilt as `ScriptPurposeScanWitness` (constructor 8, 5 fields) |
| Role / deployment entry today | none (anchor §1) |

What the step proves (C45): purpose `purpose_cursor` of the purpose frontier is
authenticated (`purpose_kind ∈ 0..3`, 28-byte hash, non-empty subject,
membership) and the discovery enters stage 9 with the current purpose loaded
and cursors reset.

## 2. Why it is this size

| Probe | Reachable code | Raw bytes |
| --- | --- | ---: |
| p03 / p04 | discovery-stage binding via `exact_script_sources_control` / sliced (stage-ten predicates; stage eight's are the same shape) | 23,522 / 6,067 |
| q12 | whole eight-purpose predicate with the sliced binding (no shell) | 7,512 |
| baseline | today's resolver | 28,166 |
| **q28** | **resolver-shaped prototype with the sliced binding** | **10,086** |

## 3. Options considered

Prune chosen (10,086); yield/chain/redesign rejected (nothing heavy remains).

## 4. Chosen design

Library only: `script_sources_stage_eight_control_is_bound` →
`script_sources_discovery_control_is_bound(pre, witness, control, 8, #"08")`
plus its stage-eight predicates (`purpose_cursor <= purpose_count`,
`source_cursor == 0`, `redeemer_cursor == 0`, current/matched reset). The
probe used the predicate-light variant; the real function adds a few integer
comparisons (≤ 300 bytes). Validator file, `ActionV1`, parameters, datum,
evidence hash **unchanged**; handshake/security unchanged. Soundness: anchor
§4a.1 induction — stage 8 is entered from `seven-finish`'s canonical encoder
(the base) or from stage 9/10 completions via `script_discovery_successor_is_exact`.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +73) | Signed publication (≈ +276) |
| --- | ---: | ---: | ---: |
| eight-purpose resolver (q28) | 10,086 (+ ≤ 300 predicates) | ≈ 10,460 | ≈ 10,740 — fits, margin ≈ 4,500 |

Referenced bytes ≈ 10,460 (tier 1). ExUnits unmeasured; less than today.

## 6. Off-chain work

Anchor §6 group items only: roster entry
`24: "validationTraceDisputeScriptSourcesStageEightPurposeSemantic"`,
descriptor/manifest entries, funding row (≈ 10.7 KB). No redeemer/codec change
(shape `[8, 5]` at `semanticResolverIndex === 24` in submit.ts is already right).

## 7. Emulator scenario tests

Add `tests/submit-init-emulator-script-sources-stage-eight-purpose-v1.test.ts`
on the anchor fixture (effectful transaction: one spend purpose, kind 0):
publication fit without `oversized`; positive lifecycle to award; valid-block
negative (challenger presents purpose `purpose_cursor + 1`); cancel; maximum
shape: `purpose_count = max_tx_size_derived_collection_item_count` (observer +
receive + spend + mint purposes) — deepest `siblings`.

## 8. Aiken tests

Keep `script_sources_stage_eight_finishes_empty_discovery_exactly` and
`stage_eight_finish_pending_redeemer_hash_divergence_is_unreachable`; add a
stage-8 case to `script_sources_discovery_control_is_bound_agrees_with_exact_encoding`
and `eight_purpose_loads_current_purpose_exactly` (golden successor for each
`purpose_kind`).

## 9. Verification commands

Anchor §9 (≈ 10,086 in the sweep) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-eight-purpose-v1.test.ts`.

## 10. Ordering and dependencies

Anchor §10; shares the sliced stage-eight binding with `eight-finish`; depends
on `seven-finish`'s canonical handoff as induction base.

## 11. Risks

Anchor §11. No ABI churn beyond the hash.
