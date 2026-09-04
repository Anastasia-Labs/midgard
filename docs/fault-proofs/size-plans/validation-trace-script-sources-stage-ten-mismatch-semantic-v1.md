# `script_sources_stage_ten_mismatch_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
(§4a library prunes, §4b descriptor-mode surface, §4c–4f shared
`V1VtSsRedeemerItemStepYield`). Only what differs for this contract is here.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_ten_mismatch_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-ten-mismatch-semantic-v1.ak` |
| Raw size | 83,005 bytes (largest of the group) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 21 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 21) = 53` |
| Library entry point | `verify_script_sources_stage_ten_mismatch_semantics_v1` → `script_sources_stage_ten_mismatch_auxiliary_is_family` (runs `step_v1` via `script_sources_stage_ten_item_matches_current_purpose` to prove the descriptor does **not** match) → `verify_script_sources_stage_ten_semantics_v1` (generic decoder/binder + whole `script_sources_stage_ten`) |
| Redeemer action | `VerifyMismatch { input_index, output_index, transition, auxiliary: ValidationAuxiliaryWitnessV1 }` |
| Auxiliaries accepted | (a) `RedeemerScanBeginWitness { item_index, item_count, total_length, item_commitment, siblings }` (constructor 10, 5 fields) when `redeemer_item_control_hash == ""`; (b) `RedeemerItemStepWitness` (constructor 18) whose step advances **without** producing a terminal descriptor matching the current purpose (non-terminal header step, or terminal tail step with `(purpose_tag, pointer_index)` ≠ current) |
| Role / deployment entry today | none (see anchor §1) |

What the step proves (C45): the redeemer scan for the current effectful
purpose advances. Begin arm: item `redeemer_cursor` of the redeemer frontier is
authenticated (`verify_membership(redeemer_count, redeemer_peaks, item_index, redeemer_item_leaf_hash(item_index, item_commitment), siblings)`,
`item_count == redeemer_count`) and the discovery commits to
`hash_control_v1(initial_control_v1(mode_descriptor, item_index, item_count, total_length, item_commitment, -1, -1))`.
Step arm: the committed item control takes one descriptor-mode step; a
non-terminal result re-commits `hash_control_v1(next)`; a terminal result whose
descriptor does not point at the current purpose moves
`redeemer_cursor` to `item_index + 1` and clears the item hash. Both successors
stay in stage 10.

## 2. Why it is this size

Same decomposition as anchor §2a (the two resolvers differ by 49 bytes: the
family guard). Additional probes specific to the two arms:

| Probe | Reachable code | Raw bytes |
| --- | --- | ---: |
| p12 | begin arm today: sliced bound + `verify_membership` + `initial_control_v1` + **`hash_control_v1` (10.2 KB, drags `cek_data_traverse_v1.encode_control_v1`)** + successor | 16,156 |
| q08 | begin arm with `hash_descriptor_control_v1` (§4b) instead | 9,288 |
| p14 | advance-only step arm today (`step_v1`, `hash_control_v1(next)`) | 45,541 |
| q07 | non-match step arm with the descriptor surface (advance + terminal-mismatch, sliced bound) | 13,727 |
| q09 | both arms, descriptor surface, sliced bound, no shell | 13,838 |
| q21 | **resolver-shaped monolith** (`cancel` + `continue_winning` + q09, two-constructor typed redeemer) | 16,487 — over |
| q55 | q09 as one per-resolver yield | 14,355 — margin 645 |
| **q52** | **dispatcher: begin arm inline + step arm given the yield-verified claim** | **13,389** |
| q51 | shared redeemer-item-step yield | 10,942 |

## 3. Options considered

Same table as anchor §3. Specific to this contract: the begin arm needs no
item-step machinery, so it stays entirely in the dispatcher (9,288 predicate,
no yield); moving it to its own yield would add a script for ~2.5 KB of
membership + hash code that already fits.

## 4. Chosen design

### 4a–4b. Library

Anchor §4a (sliced binding for `script_sources_stage_ten_control_is_bound`)
and §4b (`descriptor_control_is_well_formed_v1`, `hash_descriptor_control_v1`,
`descriptor_step_v1`). New in `validation-machine-v1.ak`:

```
pub fn verify_script_sources_stage_ten_begin_semantics_v1(pre, witness, item_index, item_count, total_length, item_commitment, siblings) -> Bool
pub fn verify_script_sources_stage_ten_mismatch_dispatch_semantics_v1(pre, witness, claim: RedeemerItemDescriptorStepClaimV1) -> Bool
```

### 4c. Validators

| Validator | Purpose | File | Params |
| --- | --- | --- | --- |
| `script_sources_stage_ten_mismatch_semantic_v1.main.spend` (**dispatcher**, same title) | begin arm in full; step arm: yield handshake + sliced binding + non-match successor | existing file | `award_script_hash`, `computation_thread_policy_id`, **`reference_script_auth_policy_id`** |
| `script_sources_redeemer_item_step_yield_v1.main.withdraw` | shared (anchor §4c) — this dispatcher's hash is its second parameter | anchor | — |

### 4d. Redeemer ABI delta

```
pub type ActionV1 {
  VerifyRedeemerItemStep {     // constructor 0 — same layout as ten-match / twelve-redeemer constructor 0
    input_index: Int, output_index: Int, transition: ValidationOneStepWitnessV1,
    claim: RedeemerItemDescriptorStepClaimV1, yield_to_ref_input_index: Int,
  }
  VerifyRedeemerScanBegin {    // constructor 1 — no yield
    input_index: Int, output_index: Int, transition: ValidationOneStepWitnessV1,
    item_index: Int, item_count: Int, total_length: Int, item_commitment: ByteArray, siblings: List<ByteArray>,
  }
}
```

The typed `auxiliary` field disappears; each arm rebuilds its auxiliary
(`RedeemerScanBeginWitness {…}` / `RedeemerItemStepWitness {…}` as anchor §4d)
so the evidence hash is unchanged. Datum and `ct.Cancel` unchanged.

### 4e. Exact handshake

Begin arm (`VerifyRedeemerScanBegin`): no yield. `continue_winning(…, RedeemerScanBeginWitness{…} as Data, verify_script_sources_stage_ten_begin_semantics_v1(pre, transition, …), …)` where the predicate is:
`control = script_sources_stage_ten_control_from_witness`, sliced `script_sources_stage_ten_control_is_bound`,
`discovery.redeemer_cursor < control.redeemer_count`, `discovery.redeemer_item_control_hash == ""`,
`item_index == discovery.redeemer_cursor`, `item_count == control.redeemer_count`,
`verify_membership(control.redeemer_count, control.redeemer_peaks, item_index, redeemer_item_leaf_hash(item_index, item_commitment), siblings)`,
`script_discovery_successor_is_exact(pre, witness, control, 10, ScriptDiscoveryControlV1 { ..discovery, redeemer_item_control_hash: hash_descriptor_control_v1(initial_control_v1(mode_descriptor, item_index, item_count, total_length, item_commitment, -1, -1)) })`.

Step arm (`VerifyRedeemerItemStep`): steps 1–3 of anchor §4e with
`verify_script_sources_stage_ten_mismatch_dispatch_semantics_v1` (measured as q52):

- sliced stage-ten binding; `next = claim.claimed_next`;
- if `next.stage == stage_terminal`: **`!redeemer_pointer_matches_purpose_v1(discovery.current_purpose_kind, discovery.current_purpose_index, next.purpose_tag, next.pointer_index)`** and
  `script_discovery_successor_is_exact(pre, witness, control, 10, ScriptDiscoveryControlV1 { ..discovery, redeemer_cursor: next.item_index + 1, redeemer_item_control_hash: #"" })`;
- else `script_discovery_successor_is_exact(pre, witness, control, 10, ScriptDiscoveryControlV1 { ..discovery, redeemer_item_control_hash: hash_descriptor_control_v1(descriptor_control_v1(next)) })`.

The yield is the shared one (anchor §4e): it binds `claim.control` to
`discovery.redeemer_item_control_hash` and recomputes `next`.

### 4f. Security argument

Anchor §4f applies verbatim to the step arm. Additionally:

- **Arm partition with ten-match:** a terminal step whose descriptor *matches*
  fails this dispatcher's negated match check and passes ten-match's; a
  non-terminal step fails ten-match's `stage_terminal` pin and passes here. The
  two dispatchers share one yield but have disjoint accepting sets, mirroring
  `script_sources_stage_ten_mismatch_auxiliary_is_family` /
  `script_sources_stage_ten_item_matches_current_purpose` today.
- **Begin arm needs no yield:** it consumes only the redeemer frontier
  membership proof and hashes a freshly built initial control; nothing a yield
  could attest is left unbound. Its `siblings` are in the evidence hash.
- **If the yield is omitted on the step arm:** `require_authenticated_zero_yield`
  fails. **If a begin-arm redeemer is submitted while an item scan is open:**
  `redeemer_item_control_hash == ""` fails. **If the begin arm's `total_length`
  or `item_commitment` are forged:** `verify_membership` against
  `redeemer_peaks` (bound to `pre.work_root`) fails.

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +110) | Signed publication (≈ +276) |
| --- | ---: | ---: | ---: |
| ten-mismatch dispatcher (q52) | 13,389 | ≈ 13,500 | ≈ 13,780 — fits, margin ≈ 2,600 |
| redeemer-item-step yield (q51) | 10,942 | ≈ 11,060 | ≈ 11,340 |

Referenced bytes per semantic-resolution transaction: begin arm ≈ 13,500
(dispatcher only, tier 1); step arm ≈ 24,560 (tier 1, ≈ 1 KB from the 25 KiB
boundary — fee only). ExUnits unmeasured (anchor §5); the begin arm's
`verify_membership` over a `max_tx_size_derived_collection_item_count` frontier
is the same work as today.

## 6. Off-chain work

Anchor §6, with: roster entry `21: "validationTraceDisputeScriptSourcesStageTenMismatchSemantic"`;
`semanticActionFieldsV1` for semantic 21 emits constructor 1
`[input_index, output_index, transition, item_index, item_count, total_length, item_commitment, siblings]`
when the staged auxiliary is `redeemerScanBegin` (10, 5) and constructor 0 with
the claim when it is `redeemerItemStep` (18, 3); the submit route adds the yield
`readFrom` + zero `withdraw` **only** for the step arm. The yield's second
parameter is this dispatcher's applied hash.

## 7. Emulator scenario tests

Anchor §7 fixture (effectful transaction, TypeScript stage 7–12 producer).
Add `tests/submit-init-emulator-script-sources-stage-ten-mismatch-v1.test.ts`
with three journeys selected by `disputedStep`: (1) the begin step
(`stage == 10 && redeemer_item_control_hash == ""`), (2) the header step
(non-terminal advance), (3) a terminal tail step of a redeemer whose pointer
does not match — which needs the honest transaction to carry **two** redeemers
so the scan passes over a non-matching item. Publication fit for the dispatcher
without `oversized`; positive lifecycle through award for each; valid-block
negatives: forged `claimed_next` (yield refuses), matching descriptor submitted
to this dispatcher (negated match refuses), yield omitted on the step arm;
begin arm submitted with an open item scan; cancel; maximum shape: two-chunk
header span and a redeemer frontier at the collection cap.

## 8. Aiken tests

Anchor §8 plus, in `script-sources-redeemer-scan-split-v1.test.ak`:
`ten_mismatch_wire_layout_is_pinned` (constructor 0 identical to ten-match,
constructor 1 begin), `ten_mismatch_begin_arm_commits_initial_descriptor_hash`
(equals today's `hash_control_v1(initial_control_v1(…))` golden from
`script_sources_stage_ten_proves_mismatch_and_missing_redeemer_exactly`),
`ten_mismatch_step_arm_advances_with_authenticated_yield`,
`ten_mismatch_step_arm_skips_non_matching_terminal`; negatives:
`_refuses_matching_terminal` (belongs to ten-match), `_refuses_begin_with_open_scan`,
`_refuses_missing_yield_reference_input`, `_refuses_cross_arm_role_token`,
`_refuses_forged_membership_siblings`. Library:
`script_sources_stage_ten_mismatch_split_agrees_with_the_aggregate` (begin and
both step outcomes ⇔ `verify_script_sources`).

## 9. Verification commands

Anchor §9 (this dispatcher ≈ 13,389 in the 15-line size sweep) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-ten-mismatch-v1.test.ts`.

## 10. Ordering and dependencies

Anchor §10: lands after the shared yield; shares `verify_script_sources_stage_ten_begin_semantics_v1`'s
shape with `stage-twelve-redeemer`'s begin arm (stage-parameterised helper).

## 11. Risks

Anchor §11. Specific: the two-redeemer effectful fixture is the only way to
exercise the terminal-mismatch outcome end to end; until it exists the arm is
covered by Aiken vectors only.
