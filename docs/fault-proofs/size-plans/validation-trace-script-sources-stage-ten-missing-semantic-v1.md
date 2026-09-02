# `script_sources_stage_ten_missing_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
§4a (shared library prunes). **Strategy: prune only — no new validator.**

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_ten_missing_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-ten-missing-semantic-v1.ak` |
| Raw size | 26,865 bytes |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 20 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 20) = 52` |
| Library entry point | `verify_script_sources_stage_ten_missing_semantics_v1` → `script_sources_stage_ten_control_from_witness`, `script_sources_stage_ten_control_is_bound` (**`exact_script_sources_control`**), `rejected_successor_is_exact(…, reject_missing_required_witness)` |
| Redeemer action | `VerifyMissing { input_index, output_index, transition }`; auxiliary `NoAuxiliaryWitness` (constructor 0) |
| Rejection reached | `reject_missing_required_witness` (`E_MISSING_REQUIRED_WITNESS`, `rejection-reason-v1.ts:258`) |
| Role / deployment entry today | none (anchor §1) |

What the step proves (C45): the redeemer scan for the current effectful purpose
exhausted every redeemer item (`redeemer_cursor == redeemer_count`) without a
match — the transaction lacks the required redeemer — and the successor is the
exact rejecting terminal.

## 2. Why it is this size

| Probe | Reachable code | Raw bytes |
| --- | --- | ---: |
| p00 | floor | 1,005 |
| p02 | `script_sources_stage_ten_control_from_witness` | 3,619 |
| p03 | + `script_sources_stage_ten_control_is_bound` via **`exact_script_sources_control`** | 23,522 |
| p06 / p08 | `exact_script_sources_control` alone / its `ledger_output_proof_v1` encode+decode share | 22,489 / 19,460 |
| p04 | + sliced binding instead | 6,067 |
| p18 | p04 + cursor pin + `rejected_successor_is_exact` (the whole pruned predicate) | 6,648 |
| baseline | today's resolver (`cancel` + `continue_winning` + p03-shaped predicate) | 26,865 |
| nine-missing | today's `script_sources_stage_nine_missing_semantic_v1` — same predicate shape with the sliced binding | 8,789 |
| **q23** | **resolver-shaped prototype with the sliced binding** (exact validator shape) | **9,012** |

The 18,076-byte gap to stage-nine-missing is `exact_script_sources_control`,
which re-encodes every stage's control shape (stage-0 pending source, stage-5
`ledger_output_proof_v1` control, discovery) to compare one stage-10 control.
Nothing else in this resolver is heavy.

## 3. Options considered

| Option | Verdict | Reason |
| --- | --- | --- |
| 1. **Prune: sliced discovery-stage binding (anchor §4a.1)** | **chosen** | 9,012 measured in the exact validator shape; no ABI change |
| 2. Yield split | rejected | nothing left to move; a dispatcher alone is ≈ 4.8 KB |
| 3. Chain | rejected | one comparison and one terminal hash |
| 4. Redesign | rejected | the arm boundary (missing vs. match vs. mismatch) is right |

## 4. Chosen design

Library only: `script_sources_stage_ten_control_is_bound` switches to
`script_sources_discovery_control_is_bound(pre, witness, control, 10, #"0a")`
plus its existing stage-ten discovery predicates (anchor §4a.1). The validator
file, `ActionV1`, parameters, datum and evidence hash are **unchanged**; the
compiled hash changes, so the prepare validator is re-applied with the group.

Handshake / security: unchanged — `continue_winning` with
`verify_script_sources_stage_ten_missing_semantics_v1`; no dispatch, no role.
The soundness of the sliced binding is anchor §4a.1's induction (same as stage
nine's `script_sources_stage_nine_missing_semantic_v1` today).

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +73) | Signed publication (≈ +276) |
| --- | ---: | ---: | ---: |
| ten-missing resolver (q23) | 9,012 | ≈ 9,085 | ≈ 9,360 — fits, margin ≈ 7,000 |

Referenced bytes per transaction ≈ 9,085 (tier 1). ExUnits: strictly less
work than today (one fewer full re-encoding of the control); unmeasured.

## 6. Off-chain work

Does not exist today: any deployment entry or funded publication for this
resolver (attaches inline today, which cannot fit beside a 31-item control
witness — the emulator publishes it `oversized`). Add roster entry
`20: "validationTraceDisputeScriptSourcesStageTenMissingSemantic"` in the new
`VALIDATION_SCRIPT_SOURCES_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`
(anchor §6), the matching `spendDescriptor` / `manifestReferenceScriptTarget`
entries and a funding row (≈ 9.4 KB plain publication). No SDK contract,
role, stake, submit-redeemer or codec change: `semanticActionFieldsV1` for
semantic 20 stays `[input_index, output_index, transition]` and the auxiliary
shape stays `none`.

## 7. Emulator scenario tests

Exists today: nothing reaching this step (anchor §7). Add
`tests/submit-init-emulator-script-sources-stage-ten-missing-v1.test.ts` on the
anchor fixture with an honest effectful transaction that **omits** the redeemer
for its spend (the operator claims acceptance; the honest trace rejects at
stage 10 after the scan exhausts): publication fit without `oversized`;
positive lifecycle to award; valid-block negative at the same frontier (the
operator's honest transaction *has* the redeemer, so `redeemer_cursor < redeemer_count`
and the challenger's rejection claim fails); cancel; maximum shape:
`redeemer_count = max_tx_size_derived_collection_item_count` with a full
`used_redeemer_bitmap`.

## 8. Aiken tests

Keep `stage_ten_missing_pending_redeemer_hash_divergence_is_unreachable` and
`script_sources_stage_ten_proves_mismatch_and_missing_redeemer_exactly`. Add
`script_sources_discovery_control_is_bound_agrees_with_exact_encoding`
(anchor §8) and `ten_missing_refuses_non_canonical_middle_after_canonical_entry`
(a stage-10 witness whose middle bytes are re-encoded indefinitely cannot be
produced by any stage-9 successor — documents the induction).

## 9. Verification commands

Anchor §9 (≈ 9,012 in the sweep) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-ten-missing-v1.test.ts`.

## 10. Ordering and dependencies

Anchor §10; shares the sliced stage-ten binding with ten-match / ten-mismatch.

## 11. Risks

Anchor §11 (induction assumption). No ABI churn beyond the hash.
