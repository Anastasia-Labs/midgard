# `script_sources_stage_twelve_redeemer_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
(§4a prunes, §4b descriptor-mode surface, §4c–4f shared
`V1VtSsRedeemerItemStepYield`). Only what differs for this contract is here.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_twelve_redeemer_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-twelve-redeemer-semantic-v1.ak` |
| Raw size | 81,736 bytes |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 19 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 19) = 51` |
| Library entry point | `verify_script_sources_stage_twelve_redeemer_semantics_v1` → `script_sources_stage_twelve_redeemer_auxiliary_is_family` → `verify_script_sources_stage_twelve_semantics_v1` (generic decoder/binder + whole `script_sources_stage_twelve`, incl. `redeemer_item_proof_v1.step_v1`, `descriptor_v1`, `hash_control_v1`) |
| Redeemer action | `VerifyRedeemer { input_index, output_index, transition, auxiliary: ValidationAuxiliaryWitnessV1 }` |
| Auxiliaries accepted | `RedeemerScanBeginWitness` (constructor 10) when `redeemer_item_control_hash == ""`; `RedeemerItemStepWitness` (constructor 18, `redeemer_control: None`) otherwise |
| Rejection reached | `reject_invalid_field_type` (`E_INVALID_FIELD_TYPE`, `demo/midgard-sdk/src/rejection-reason.ts:247`) for an extraneous redeemer |
| Role / deployment entry today | none (anchor §1) |

What the step proves (C45 "unused/extraneous rejection"): after every purpose
is discovered (stage 12: `purpose_cursor == purpose_count`,
`source_cursor == source_count`), the redeemer audit walks every redeemer item
in descriptor mode. Begin arm as in ten-mismatch (stage 12 successor). Step arm:
non-terminal → re-commit `hash_control_v1(next)`; terminal → if
`used_redeemer_bitmap` has `item_index` advance `redeemer_cursor` (stage 12),
else the exact `E_INVALID_FIELD_TYPE` rejecting terminal.

## 2. Why it is this size

Anchor §2a decomposition applies (81,736 = same generic decoder/binder 32 KB,
sum-type auxiliary 13 KB, `step_v1` 37 KB; the stage-twelve body is ~1.2 KB
smaller than stage ten's because it has no execution-leaf append). Probes:

| Probe | Reachable code | Raw bytes |
| --- | --- | ---: |
| p19 | stage-twelve **finish** with sliced bound (for scale) | 7,195 |
| q10 | step arm with descriptor surface, light sliced bound, both outcomes + rejection | 13,401 |
| q11 | begin + step arms, no shell | 13,566 |
| q22 | **resolver-shaped monolith** (two-constructor typed redeemer) | 16,236 — over |
| q56 | q11 as one per-resolver yield | 14,086 — margin 914 |
| **q53** | **dispatcher: begin arm inline + step arm given the yield-verified claim** | **13,156** |
| q51 | shared redeemer-item-step yield | 10,942 |

## 3. Options considered

Anchor §3. Per-resolver yield (q56) has the widest margin of the three
fallbacks (914) but the shared-yield design still wins on script count and on
the ≥ 1.8 KB dispatcher margin.

## 4. Chosen design

### 4a–4b. Library

Anchor §4a (sliced `script_sources_stage_twelve_control_is_bound`, stage byte
`#"0c"`, stage-twelve discovery predicates: `purpose_count == purpose_cursor`,
`source_cursor == source_count`, current/matched fields reset) and §4b. New:

```
pub fn verify_script_sources_stage_twelve_begin_semantics_v1(pre, witness, item_index, item_count, total_length, item_commitment, siblings) -> Bool
pub fn verify_script_sources_stage_twelve_redeemer_dispatch_semantics_v1(pre, witness, claim) -> Bool
```

(`verify_script_sources_stage_{ten,twelve}_begin_semantics_v1` share one
private `script_sources_redeemer_scan_begin(pre, witness, control, stage, …)`.)

### 4c. Validators

| Validator | Purpose | Params |
| --- | --- | --- |
| `script_sources_stage_twelve_redeemer_semantic_v1.main.spend` (**dispatcher**, same title, existing file) | begin arm; step arm with yield handshake, sliced stage-12 binding, bitmap audit, successor or rejection | `award_script_hash`, `computation_thread_policy_id`, **`reference_script_auth_policy_id`** |
| `script_sources_redeemer_item_step_yield_v1.main.withdraw` | shared (anchor §4c); this dispatcher's hash is its third parameter | — |

### 4d. Redeemer ABI delta

Identical to ten-mismatch §4d (constructor 0 `VerifyRedeemerItemStep { input_index, output_index, transition, claim, yield_to_ref_input_index }`, constructor 1 `VerifyRedeemerScanBegin {…}`). Auxiliary rebuilt → evidence hash unchanged.

### 4e. Exact handshake

Begin arm: as ten-mismatch §4e with stage 12 (`script_sources_stage_twelve_control_from_witness`,
successor `script_discovery_successor_is_exact(…, 12, { ..discovery, redeemer_item_control_hash: hash_descriptor_control_v1(initial…) })`).

Step arm: anchor §4e steps 1–3 with
`verify_script_sources_stage_twelve_redeemer_dispatch_semantics_v1` (measured as q53):

- sliced stage-twelve binding; `next = claim.claimed_next`;
- if `next.stage == stage_terminal`:
  if `!script_discovery_bitmap_has(discovery.used_redeemer_bitmap, next.item_index)` →
  `rejected_successor_is_exact(pre, witness.claimed_successor, reject_invalid_field_type)`
  else `script_discovery_successor_is_exact(pre, witness, control, 12, { ..discovery, redeemer_cursor: next.item_index + 1, redeemer_item_control_hash: #"" })`;
- else `script_discovery_successor_is_exact(pre, witness, control, 12, { ..discovery, redeemer_item_control_hash: hash_descriptor_control_v1(descriptor_control_v1(next)) })`.

The shared yield (anchor §4e) accepts stage 10 **or** 12 controls
(`script_sources_redeemer_scan_control_from_witness`); the stage pin lives in the dispatcher.

### 4f. Security argument

Anchor §4f. Additionally: **the rejection outcome is dispatcher-derived** from
the yield-verified `next.item_index` and the discovery bitmap bound to
`pre.work_root`; a prover cannot turn a used redeemer into a rejection because
`claimed_next.item_index` is pinned to `item_control.item_index` by the yield's
recomputation (`header_step`/`tail_step` never change `item_index`) and
`item_control` to the committed hash. **If the yield is omitted:** role check
fails. **If the bitmap is forged:** the sliced binding refuses (the discovery
suffix is part of the bound witness bytes).

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +110) | Signed publication (≈ +276) |
| --- | ---: | ---: | ---: |
| twelve-redeemer dispatcher (q53) | 13,156 | ≈ 13,270 | ≈ 13,550 — fits, margin ≈ 2,830 |
| redeemer-item-step yield (q51) | 10,942 | ≈ 11,060 | ≈ 11,340 |

Referenced bytes per transaction: begin ≈ 13,270; step ≈ 24,330 (tier 1).
ExUnits unmeasured (anchor §5).

## 6. Off-chain work

Anchor §6 with roster entry `19: "validationTraceDisputeScriptSourcesStageTwelveRedeemerSemantic"`;
`semanticActionFieldsV1` for semantic 19 mirrors ten-mismatch (constructor by
staged auxiliary shape); yield `readFrom` + zero `withdraw` for the step arm.
The stage-12 rejection successor's `E_INVALID_FIELD_TYPE` is already in
`rejection-reason.ts`; no codec change beyond the shared claim schema.

## 7. Emulator scenario tests

Anchor §7 fixture plus a second honest effectful transaction carrying one
**extraneous** redeemer (pointer at an index with no purpose) so the rejecting
terminal is reachable as a challenger win, i.e. the operator's forged
acceptance is refuted at stage 12. `tests/submit-init-emulator-script-sources-stage-twelve-redeemer-v1.test.ts`
journeys: begin step, header step, terminal step on a used redeemer (advance),
terminal step on an unused redeemer (rejection → award). Publication fit
without `oversized`; negatives: forged `claimed_next.item_index`, forged
bitmap (binding refuses), yield omitted; cancel; maximum shape as ten-mismatch.

## 8. Aiken tests

Anchor §8 plus: `twelve_redeemer_wire_layout_is_pinned`,
`twelve_redeemer_begin_arm_commits_initial_descriptor_hash`,
`twelve_redeemer_step_arm_advances_used_item_with_authenticated_yield`,
`twelve_redeemer_step_arm_rejects_unused_item_exactly` (golden from
`script_sources_stage_twelve_audits_redeemers_exactly`), negatives
`_refuses_missing_yield_reference_input`, `_refuses_cross_arm_role_token`,
`_refuses_stage_ten_control` (stage pin), `_refuses_forged_claimed_next`;
library `script_sources_stage_twelve_redeemer_split_agrees_with_the_aggregate`.
Keep `script_sources_stage_twelve_redeemer_family_guard` and
`stage_twelve_finish_pending_redeemer_hash_divergence_is_unreachable`.

## 9. Verification commands

Anchor §9 (≈ 13,156 in the sweep) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-twelve-redeemer-v1.test.ts`.

## 10. Ordering and dependencies

Anchor §10: after the shared yield; the begin helper is shared with
ten-mismatch; `stage-twelve-finish` shares the sliced stage-twelve binding.

## 11. Risks

Anchor §11. Specific: the yield accepts controls of two stages; the stage pin
is the dispatcher's — a dispatcher that forgot it would accept a stage-10 claim,
which the `_refuses_stage_ten_control` vector guards.
