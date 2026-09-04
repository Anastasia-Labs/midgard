# `script_sources_stage_seven_receive_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md) and the anchor plan
[`validation-trace-script-sources-stage-ten-match-semantic-v1.md`](validation-trace-script-sources-stage-ten-match-semantic-v1.md)
§4a.2 (stage-seven narrow encoder). **Strategy: prune only — no new validator.**

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/script_sources_stage_seven_receive_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-seven-receive-semantic-v1.ak` |
| Raw size | 29,566 bytes |
| Applied parameters | `award_script_hash`, `computation_thread_policy_id` |
| Phase / resolver index | `ScriptSources`, resolver 8 |
| Semantic index (arm) | 26 of 29; global slot `validationSemanticResolverGlobalIndexV1(8, 26) = 58` |
| Library entry point | `verify_script_sources_stage_seven_receive_semantics_v1` → `script_sources_stage_seven_control_from_witness`, `decode_native_tx_compact_v1(control.compact_cbor).body.required_observers_hash`, `script_sources_stage_seven_control_is_bound` (**`exact_script_sources_control`**), `script_sources_stage_seven_observer_scan_is_complete`, `validation_merkle_v1.verify_membership`, `script_sources_stage_seven_receive_successor_is_exact` (**`exact_script_sources_control`**) |
| Redeemer action | `VerifyReceive { input_index, output_index, transition, purpose_kind, purpose_index, script_hash, subject, siblings }`; auxiliary rebuilt as `ScriptPurposeScanWitness` (constructor 8, 5 fields) |
| Role / deployment entry today | none (anchor §1) |

What the step proves (C45/C50): the observer scan is complete; output
`output_cursor` of the receive-scan frontier (`receive_scan.source_count/peaks`)
is a receive purpose (`purpose_kind == 3`, `subject == script_hash`, 28 bytes,
membership proof); the successor advances `output_cursor` and updates
`candidate_hash` via `next_receive_candidate` (smallest hash above
`previous_hash`), staying in stage 7.

## 2. Why it is this size

| Probe | Reachable code | Raw bytes |
| --- | --- | ---: |
| p27 | control decode + `decode_native_tx_compact_v1` (observer commitment) | 4,309 |
| p21 | control decode + stage-seven binding via **`exact_script_sources_control`** | 23,124 |
| p22 | same with the narrow stage-seven encoder | 7,846 |
| p24 | whole receive predicate with the narrow encoder for binding and successor (no shell) | 11,479 |
| baseline | today's resolver | 29,566 |
| **q26** | **resolver-shaped prototype after the prune** (exact validator shape) | **13,852** |

The 17.7 KB gap between baseline and q26 is `exact_script_sources_control`
(reached twice — binding and successor — compiled once); `verify_membership`
and `next_receive_candidate` are ≈ 1 KB.

## 3. Options considered

| Option | Verdict | Reason |
| --- | --- | --- |
| 1. **Prune: narrow stage-seven encoder for binding and successor (anchor §4a.2)** | **chosen** | 13,852 measured in the exact validator shape; no ABI change |
| 2. Yield (binding half as in the observer plan's yield B) | rejected, **recorded as fallback** | would give dispatcher ≈ 4.8 KB + yield ≈ 10.5 KB; only needed if regeneration pushes the monolith past 15,000 (margin today 1,148) |
| 3. Chain / 4. Redesign | rejected | one membership proof and one successor |

## 4. Chosen design

Library only (anchor §4a.2): `script_sources_stage_seven_control_is_bound`
compares `script_sources_stage_seven_exact_control(control) == witness.work_witness_cbor`;
`script_sources_stage_seven_receive_successor_is_exact` calls
`script_sources_stage_seven_successor_is_exact`. The validator file, `ActionV1`,
parameters, datum and evidence hash are **unchanged**. Handshake and security
unchanged (`continue_winning`, no dispatch, no role). The narrow encoder is
byte-identical to `exact_script_sources_control` for every control with
`stage ∈ {7, 8}`, `output_proof == None`, `pending_source_cbor == ""` — the
only controls this stage binds or produces (§8 property).

## 5. Size and budget projection

| Script | Raw (measured) | Applied (≈ +73) | Signed publication (≈ +276) |
| --- | ---: | ---: | ---: |
| seven-receive resolver (q26) | 13,852 | ≈ 13,925 | ≈ 14,200 — fits, margin ≈ 1,150 to the 15,000 target, ≈ 2,180 to the envelope |

Referenced bytes per transaction ≈ 13,925 (tier 1). ExUnits: strictly less
than today; unmeasured.

## 6. Off-chain work

Anchor §6 group items only: roster entry
`26: "validationTraceDisputeScriptSourcesStageSevenReceiveSemantic"`,
`spendDescriptor` / `manifestReferenceScriptTarget`, funding row (≈ 14.2 KB
plain publication). No SDK contract, role, stake, redeemer or codec change
(`semanticActionFieldsV1` for semantic 26 stays `[...base, ...scriptPurposeScan fields]`).

## 7. Emulator scenario tests

Exists today: nothing reaching stage seven (anchor §7). Add
`tests/submit-init-emulator-script-sources-stage-seven-receive-v1.test.ts` on
the anchor fixture with an honest transaction whose output set includes a
receive-purpose output (script-credential output the machine's stage-5/6
scans registered in `receive_scan`): publication fit without `oversized`;
positive lifecycle to award; valid-block negative (challenger claims a
`purpose_index` ahead of `output_cursor`); cancel; maximum shape:
`receive_scan.source_count = max_tx_size_derived_collection_item_count`
(deepest siblings list) and a candidate ordering that flips `candidate_hash`.

## 8. Aiken tests

Keep `script_sources_stage_seven_receive_binds_canonical_successor_encoding`
(`validation-machine-v1.test.ak:7310`). Add
`script_sources_stage_seven_exact_control_agrees_with_exact_script_sources_control`
(observer plan §8) and `seven_receive_refuses_stage_five_output_proof_control`
(a control with `output_proof: Some` cannot reach stage 7 and must be refused
by the narrow encoder's `expect`).

## 9. Verification commands

Anchor §9 (≈ 13,852 in the sweep) plus
`pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-seven-receive-v1.test.ts`.

## 10. Ordering and dependencies

Shares anchor §4a.2 with `seven-observer` (yield B) and `seven-finish`; lands
with the group's regeneration.

## 11. Risks

Thinnest prune-only margin of the group (1,148 bytes to the target). If a
library change (e.g. `validation_merkle_v1` or `native_tx_carriage_v1`
growth) pushes it over, apply the observer plan's yield-B split: dispatcher
≈ 4.8 KB + a "stage-seven bound + receive successor" yield ≈ 10.5 KB.
