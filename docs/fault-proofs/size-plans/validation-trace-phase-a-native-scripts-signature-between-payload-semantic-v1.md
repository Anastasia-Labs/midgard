# Size-fit plan: `phase_a_native_scripts_signature_between_payload_semantic_v1`

Reads with [00-primer.md](00-primer.md). This is the **anchor plan for the
phase-A native-script payload family**: the seven over-limit payload resolvers
and the four borderline siblings are structurally one program with a different
payload decoder and signer closure, so §2 and §4 here define the shared fix
(PA-CARRY and PA-UNDECODED) that the six sibling plans reference. Each sibling
plan records its own measured numbers.

## 1. Identity

| Field | Value |
| --- | --- |
| Blueprint title | `fraud_proofs/validation_trace/phase_a_native_scripts_signature_between_payload_semantic_v1.main.spend` |
| File | `onchain/aiken/validators/fraud-proofs/validation-trace/phase-a-native-scripts-signature-between-payload-semantic-v1.ak` |
| Raw size (2026-09-01 blueprint, re-measured in the probe copy) | **17,098 bytes** (limit 16,384; target 15,000) |
| Applied parameters | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId` (2 params, +72–73 bytes applied) |
| Phase / resolver index | `PhaseANativeScripts`, resolver index 5 (`validationResolverIndexV1`) |
| Semantic index | 12 of 14 in `phase_a_native_scripts_v1.main(semantic_resolver_script_hashes, …)` (`prepare_selected(PhaseANativeScripts, hashes, 14, …)`); global semantic index 22 (`validationSemanticResolverGlobalIndexV1(5, 12)`) |
| Machine stage | control `stage == 3` (signature payload after a `token_head` step), `SignerSetProofV1.SignerBetweenProof` |
| Library entry point | `validation_machine_v1.verify_phase_a_native_signature_between_payload_semantics_v1(pre, transition, chunk_proof, next_chunk_proof, peaks, lower_index, lower_signer_hash, lower_siblings, upper_signer_hash, upper_siblings)` |
| Redeemer | `ct.StepRedeemer<ActionV1>` with `VerifyToken { input_index, output_index, transition, chunk_proof, next_chunk_proof, signer_proof }`; auxiliary hashed as `NativeScriptTokenWitness { chunk_proof, next_chunk_proof, signer_proof }` (shape `[3, 3]` in `VALIDATION_AUXILIARY_SHAPES_V1.nativeScriptToken`) |
| Rejection reasons it can emit | `WitnessNativeScriptMalformed` (`reject_invalid_field_type`, `demo/midgard-sdk/src/rejection-reason.ts` PhaseANativeScripts section) |
| Role name today | none — semantic resolvers carry no auth-role NFT; they are hash-checked against the applied contract |
| Deployment entry today | none. `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.phaseANativeScriptsSignatureBetweenPayload` in `demo/midgard-sdk/src/fraud-proof/contracts.ts` only; `submit.ts` attaches the body inline, which cannot fit at any size above ~12 KB once the two-chunk redeemer is present |

## 2. Why it is this size

Procedure: primer §"Measuring what dominates". Copy at `/tmp/size-probe-pa`
(deleted after measurement); pinned `aiken v1.1.23-org-5adf7837`, `--env
testnet`. Private helpers were made `pub` in the copy only. Probe sources kept
at `/tmp/pa-probe/` (`pa-probes.ak`, `pa-probes-2.ak`, `append-e1*.ak`).
Every probe is one `spend` validator whose redeemer is decoded into the exact
argument types, so absolute numbers include ~0.6–1.2 KB of probe wrapper and
type decoders; deltas between probes are the load-bearing figures.

### Component probes (shared by every phase-A resolver)

| Probe | Reachable code | Raw bytes | Delta vs. its inputs |
| --- | --- | ---: | ---: |
| `p00_shell` | `cancel` + `continue_winning` + `Datum`/`StepRedeemer` decoders, semantic = `True` | 3,267 | shell floor |
| `p01_proof_source` | `compact.verify_native_tx_proof_source_v1` (compact tx, witness set, field lengths decode + `native_tx_id` check) | 2,220 | — |
| `p02_control_decode` | `phase_a_native_control_from_witness` (19-field CBOR list + `decode_frontier_peaks`) | 1,721 | — |
| `p03_bound_full` | p01 + p02 + `phase_a_native_control_is_bound` | 8,941 | **≈5,000** for the full binding |
| `p04_bound_carried` | p01 + p02 + carried binding (PA-CARRY, §4) | 7,050 | ≈3,109; the NativeScripts continuation trio costs **≈1,900** |
| `p08_ns_trio` | `native_scripts_control_from_witness` + `native_scripts_control_is_well_formed` + `encode_native_scripts_control_v1` | 3,790 | reached only through `is_late_continuation` |
| `p05_successor` | p02 + `phase_a_native_successor_is_exact` (`encode_phase_a_native_scripts_scan_witness` + `hash_work_witness`) | 3,904 | ≈2,183 |
| `p25_encode_pa_witness` | p02 + `encode_phase_a_native_scripts_scan_witness` | 2,794 | ≈1,073 |
| `p06_rejected` | `rejected_successor_is_exact` (`encode_terminal_rejection_witness`, `hash_rejection_code`) | 1,573 | — |
| `p07_chunk_window` | p02 + `phase_a_native_chunk_window` (two `bounded_item_v1.verify_chunk`, `validation_merkle_v1.verify_membership`) | 4,285 | ≈2,564 |
| `p09_signature_payload_at` | `native_script_scan_v1.signature_payload_at_v1` | 1,169 | ≈0.4 KB net of wrapper |
| `p14_signer_membership` | `signer_membership_is_valid` (`signer_frontier_matches`, `verify_membership`, `signer_leaf_hash`) | 1,543 | — |
| `p15` / `p16` | `signer_frontier_matches` / `frontier_is_well_formed` + `frontier_commitment` | 922 / 927 | — |

### What the 17,098 bytes are

`verify_phase_a_native_signature_between_payload_semantics_v1` reaches, through
`verify_phase_a_native_signature_payload_with_v1` →
`authenticated_phase_a_native_window_v1`:

1. `verify_native_tx_proof_source_v1` (≈2.2 KB) — decodes the whole compact
   transaction although this step reads none of it;
2. `phase_a_native_control_is_bound` (≈5.0 KB) — of which ≈1.9 KB is the
   `is_late_continuation` branch that decodes, re-encodes and range-checks a
   *NativeScripts-phase* control (`NativeScriptsControlV1`, 26 fields) that a
   payload step never interprets;
3. `phase_a_native_chunk_window` (≈2.6 KB), `phase_a_native_successor_is_exact`
   (≈2.2 KB), `rejected_successor_is_exact` (≈1.6 KB), the shell (≈3.3 KB),
   control decode (≈1.7 KB), `signature_payload_at_v1` and the two
   `signer_membership_is_valid` calls of the between closure (≈1.5 KB shared).

Frame (13,698) and advance (13,856) are the floor of this family; every payload
resolver is floor + chunk window + its payload decoder + signer helper.

### Experiment builds (whole validators, same copy)

| Build | Change | between | above-last | below-first | membership | empty | all/any frame | at-least frame | all/any empty | at-least empty | timelock | token-head |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| baseline | — | 17,098 | 16,963 | 16,923 | 16,850 | 16,762 | 16,796 | 16,795 | 16,325 | 16,332 | 16,229 | 16,193 |
| E1 = PA-CARRY | `authenticated_phase_a_native_window_v1` binds with `phase_a_native_control_is_bound_carried` | **15,064** | 14,898 | 14,864 | 14,792 | 14,699 | 14,704 | 14,709 | 14,253 | 14,259 | 14,157 | 14,124 |
| E1c = PA-CARRY + PA-UNDECODED | stages 1, 3, 4–6 bind without decoding the compact transaction | **12,324** | 12,185 | 12,149 | 12,079 | 11,986 | 11,954 | 11,930 | 11,452 | 11,468 | 14,157 | 11,243 |

PA-CARRY alone leaves this resolver 64 bytes above the 15,000 target (1,320
under the hard limit). PA-UNDECODED is therefore required for the signature
family and is adopted for all nine stage-1/3/4/5/6 resolvers; timelock keeps
the decoded window (it reads `validity_interval_start/end`).

## 3. Options considered

- **Prune (chosen).** Two narrowings of the library entry point, both
  ABI-neutral and both leaving what the step proves unchanged (argument in §4):
  PA-CARRY removes the NativeScripts continuation interpretation from steps
  that only carry `continuation_cbor`; PA-UNDECODED removes the compact
  transaction decode from steps that read nothing from the decoded
  transaction. Measured 17,098 → 12,324.
- **Withdraw-zero yield split.** Rejected: the resolver is 4% over; a
  dispatcher/yield pair would cost a second datum+redeemer parse
  (`ValidationOneStepWitnessV1` carries a full `ValidationMachineStateV1`),
  a new role NFT, a manifest role and a submit route for ≈700 bytes of
  headroom that pruning delivers for free.
- **Multi-transaction chaining.** Rejected: one payload step is already the
  minimal unit of work; chaining would add a hop per native-script token
  against the §3.3 maturity margin and C52's 5,000-transaction cap.
- **Redesign.** Not warranted; arm boundaries (one resolver per token kind and
  signer-proof kind) are right and match `prepare_selected`'s 14-slot roster.

## 4. Chosen design (shared fix for the payload family)

No new validators. Two new library functions in
`onchain/aiken/lib/midgard/validation-machine-v1.ak`, and one existing
function re-pointed.

### 4.1 PA-CARRY: `phase_a_native_control_is_bound_carried`

Same signature and body as `phase_a_native_control_is_bound(pre, witness,
control, verified, witness_set)` except that the `is_late_continuation`
branch becomes

```aiken
if is_late_continuation {
  and { control.stage > 0, control.script_count == 1, control.script_seen == 0,
        control.contains_non_native_script == 0 }
}
```

i.e. it no longer calls `native_scripts_control_from_witness`,
`native_scripts_control_is_well_formed`, `encode_native_scripts_control_v1`,
`blake2b_256(continuation_cbor) == resolution_schedule_hash`, or the eight
field equalities against the NativeScripts control. Used by
`verify_phase_a_native_timelock_payload_scan_v1` (stages 7/8) through
`authenticated_phase_a_native_window_v1`, and by
`verify_phase_a_native_frame_scan` if desired (frame already fits; optional).

### 4.2 PA-UNDECODED: `phase_a_native_payload_control_is_bound` and `authenticated_phase_a_native_payload_window_v1`

```aiken
fn phase_a_native_payload_control_is_bound(
  pre: ValidationMachineStateV1, witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1, min_stage: Int, max_stage: Int,
) -> Bool
```

Checks, in order: `native_tx_proof_commitment_v1(compact, witness_set,
lengths) == pre.transaction_commitment`; `hash_validation_context(context_cbor)
== pre.validation_context_hash`; `resolution_schedule_hash` is 32 bytes;
`min_stage <= stage <= max_stage`; `0 < script_count <=
max_tx_size_derived_collection_item_count`; `0 <= script_seen <
script_count`; the carried continuation clause of §4.1 when
`continuation_cbor != ""`; `contains_non_native_script ∈ {0,1}`; signer count
bounds and `frontier_is_well_formed(signer_count, signer_peaks)`; `0 <
item_length <= max_aggregate_field_preimage_bytes`; `item_commitment` is 32
bytes; `0 <= cursor <= item_length`; `stack_root`/`stack_depth` consistency;
`node_count` bounds; `result == -1`; and the canonical re-encode equality
`witness.work_witness_cbor == encode_phase_a_native_scripts_scan_witness(…)`.
It is the stage ≥ 1, `result == -1` slice of `phase_a_native_control_is_bound`
with the `verified.version == 1` and `scripts_are_empty` clauses removed.

```aiken
fn authenticated_phase_a_native_payload_window_v1(
  pre, witness, control, chunk_proof, next_chunk_proof, min_stage, max_stage,
) -> Option<NativeChunkWindowV1>
```

returns `phase_a_native_chunk_window(control, chunk_proof, next_chunk_proof)`
when the binding holds, else `None`. Callers and their stage windows:

| Caller (existing function, body re-pointed) | `min_stage..max_stage` | Resolvers |
| --- | --- | --- |
| `verify_phase_a_native_signature_payload_with_v1` | 3..3 | signature membership / empty / below-first / above-last / **between** |
| `authenticated_phase_a_native_all_or_any_payload_v1` | 4..5 | all-or-any container-frame, all-or-any empty-container |
| `authenticated_phase_a_native_at_least_payload_v1` | 6..6 | at-least container-frame, at-least empty-container |
| `verify_phase_a_native_token_head_scan_v1` | 1..1 | token-head |

Each caller keeps its own exact stage check (`control.stage != 3 → False`,
etc.) so the window bounds are belt-and-braces. `AuthenticatedNativeScriptWindowV1`
(carrying `verified`) stays for timelock only. The generic
`verify_phase_a_native_token_scan` / `*_token_semantics_v1` functions (unused
by any validator) are left untouched.

### 4.3 Security argument

The step proves exactly what it proves today. Facts used:

1. **Pre-state anchoring.** `continue_winning` requires
   `prepared_resolution_is_well_formed(state)` and
   `hash_one_step_evidence(transition, auxiliary) == state.evidence_hash`;
   `prepare_semantic_resolution` (`validation-resolution-v1.ak`) ran
   `structural_transition_is_valid(pre, transition)`, which binds
   `hash_work_witness(pre.phase, pre.program_counter, work_witness_cbor) ==
   pre.work_root`. So the control the step decodes is *the* control committed
   in `pre`, and the re-encode equality makes the decode canonical.
2. **Transaction bytes.** `pre.transaction_commitment` is
   `native_tx_proof_commitment_v1(compact, witness_set, lengths)`, pinned
   `pre == post` by `immutable_context_matches` in every phase since
   `CompactBinding`, where `verify_native_tx_compact_cbor_v1` ran against
   `pre.transaction_id`. Re-decoding the same bytes in a payload step yields
   the same `verified` value and no new fact; the payload predicates
   (`signature_payload_at_v1`, `signer_membership_is_valid`,
   `successor_is_exact`) read no field of `verified` or `witness_set`.
3. **`scripts_are_empty` is implied.** For `stage >= 1` the full binding
   already requires `script_count > 0 && item_length > 0`. A stage-1+ control
   is produced only by `verify_phase_a_native_item_scan`, which opens field 6
   through the §8 door against `witness_set.script_tx_wits_hash`; an empty
   field commitment cannot authenticate a non-empty preimage, so no
   stage ≥ 1 control with `script_count > 0` exists for an empty field.
4. **Continuation interpretation belongs to the steps that consume it.**
   `continuation_cbor` is copied unchanged (`..control`) by every payload
   step; it is interpreted only by `phase_a_native_complete_script_is_exact`
   (item, finalize), which still runs under the full binding and re-derives
   the NativeScripts successor from it. Its well-formedness was checked by
   the NativeScripts step that entered phase A (encoded it into the
   PhaseANativeScripts work witness) and is re-checked at consumption.

Dispatch uniqueness / role authentication / cross-arm substitution / output
re-derivation: unchanged — no yield is introduced; `prepare_selected` still
routes by the exact 14-hash list and the resolver still proves
`semantic_transition_is_valid` inside `continue_winning`. Omitting nothing is
possible: there is one script.

### 4.4 ABI deltas

None to datum, redeemer, auxiliary, work-witness encodings or rejection
codes. Only the 14 phase-A native semantic script hashes change (13 bodies
change; the advance resolver is untouched but is re-applied because the
prepare validator's hash list changes only in content, not shape), hence
`phase_a_native_scripts_v1` re-applies, then the family's first step and the
catalogue root (primer "Applied-parameter graph").

## 5. Size and budget projection

| Script | Today | Projected raw | Method |
| --- | ---: | ---: | --- |
| `phase_a_native_scripts_signature_between_payload_semantic_v1.main.spend` | 17,098 | **12,324** (applied ≈12,397; signed publication ≈12,675) | measured, build E1c |

Referenced bytes per semantic-resolution transaction: the resolver body
(12.3 KB) plus the two shared minting witnesses when they are sourced by
reference (`computationThreadMint`, `fraudProofMint`,
`publishFaultProofWitnessReferenceScriptsV1`); well inside the first 25,600-byte
`minFeeRefScriptCostPerByte` tier (base 15 lovelace/byte on mainnet: ≈0.19 ADA
for the resolver alone). ExUnits: strictly fewer operations than today (the
retained checks are a subset; the CBOR decode of the compact transaction and
the NativeScripts control are removed), so the step's memory/CPU is bounded by
today's measured figure. Not re-measured here — see §9 for the command that
prints it (`MIDGARD_PRINT_PROOF_FIT=1`).

## 6. Off-chain work

None of this exists today for phase-A semantics; all of it is shared by the
fourteen phase-A native resolvers and lands once:

- **SDK contracts** (`demo/midgard-sdk/src/fraud-proof/contracts.ts`): no
  change to titles or parameters; `semanticResolverParameterValues` already
  serves `award_script_hash` and `computation_thread_policy_id`.
- **Deployment entries** (`demo/midgard-fault-proofs/src/validation-dispute/submit.ts`):
  add `VALIDATION_PHASE_A_NATIVE_SCRIPTS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`
  keyed by semantic index 0..13 (`validationTraceDisputePhaseANativeScriptsAdvanceSemantic`,
  `…ItemSemantic`, `…TokenHeadSemantic`, `…AllOrAnyContainerFramePayloadSemantic`,
  `…AllOrAnyEmptyContainerPayloadSemantic`, `…AtLeastContainerFramePayloadSemantic`,
  `…AtLeastEmptyContainerPayloadSemantic`, `…TimelockPayloadSemantic`,
  `…SignatureMembershipPayloadSemantic`, `…SignatureEmptyPayloadSemantic`,
  `…SignatureBelowFirstPayloadSemantic`, `…SignatureAboveLastPayloadSemantic`,
  `…SignatureBetweenPayloadSemantic`, `…FrameSemantic`), with
  `validationPhaseANativeScriptsSemanticReferenceScriptDeploymentEntryV1(index)`
  and `requireValidationPhaseANativeScriptsSemanticReferenceScriptUtxo({lucid,
  deploymentInfo, semanticResolverIndex, expectedScriptHash})` modelled on the
  ValueAndMint pair (`submit.ts` lines ~907–1040). Hash-checked, no auth role.
- **Submit route**: in the semantic-resolution builder (the block at
  `submit.ts` ~6040–6110 that computes `cekSemanticReferenceScriptUtxo` and
  `valueAndMintSemanticReferenceScriptUtxo`), add the `resolverIndex === 5`
  branch with the ValueAndMint policy: published entry → consume by reference
  (`spendingScriptReferenceUtxo`); absent entry and body >
  `MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES` → fail fast with "publish it as
  `<entry>`"; absent and small → inline. Even at 12.3 KB the two-chunk redeemer
  (up to 2 × 4,095 payload bytes plus sibling lists) makes inline attachment
  infeasible, so the emulator scenario publishes by reference.
- **Reference-script roles / manifest**: none (no yield in this plan).
- **Funding**: the reference-script publication funding computation
  (`referenceScriptPublicationFundingTarget`, `selectReferenceScriptFundingUtxos`
  in `demo/midgard-sdk/src/reference-scripts.ts`) and the node's
  contract-deployment-info descriptors (`spendDescriptor(...)` list in
  `demo/midgard-node/src/commands/contract-deployment-info.ts`) gain one
  spend-descriptor row per phase-A entry (14).
- **Inspection fixtures**: `demo/midgard-fault-proofs/tests/inspect-contracts.test.ts`
  `oversizedAppliedSpendingScripts` loses these entries (derived, not pinned).
- **`midgard-core` / `midgard-validation` codecs**: none (no wire change).
  `validationSemanticResolverIndexV1(witness)` in
  `demo/midgard-validation/src/validation-machine-data.ts` is unchanged.
- **Watcher**: not installed for `validationTraceDispute`; nothing here adds
  an operator-local input.

## 7. Emulator scenario tests

Exists today: `demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute.test.ts`
covers canonical-decode complete-item routes and the CEK by-reference
publication (with `oversized: true` and `maxTxSize: 262_144`); the harness
`runForcedValidationDisputeScenario(buildFixture, { stopAfter })` in
`tests/support/emulator/dispute-scenario.ts` is fixture-driven
(`fixture.evidence.oneStepArgument.resolverIndex`) and publishes an over-limit
semantic resolver with `oversized: true` under `functionalProtocolParameters`.
No phase-A fixture exists.

Add `demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-phase-a-signature.test.ts`
with fixtures in `tests/support/emulator/validation-dispute-fixtures.ts`:

- `buildPhaseANativeSignatureBetweenFixture({ operatorVkey, now })`: one
  Midgard transaction whose field-6 witness script is `sig <key>` where `key`
  sorts strictly between two adjacent required signers of a ≥ 3-signer set;
  the operator claims a successor with `result == 1`; the honest trace's
  frontier lands on the stage-3 step with `SignerBetweenProof { lower_index,
  … }`.
- **Publication fit**: publish `semanticResolvers[22]` with
  `publishPlainReferenceScriptUtxo` under `withRealL1MaxTxSize` **without**
  `oversized`; assert `publicationMeasurement.l1ByteMargin > 0`.
- **Positive lifecycle** through `prepare-selected` (semantic index 12) and
  award, transaction sizes ≤ `PROTOCOL_PARAMETERS_DEFAULT.maxTxSize`.
- **Valid-block negative** at the same frontier: an honest operator successor
  (`result == 0`) makes the resolver refuse (expect
  `expectOnchainRefusalV1`).
- **Cancel/resume**: `ct.Cancel` on the prepared thread, then a fresh open.
- **Maximum shape**: the sig token straddles a `bounded_item_v1.chunk_bytes`
  (4,095) boundary so both `chunk_proof` and `next_chunk_proof` are present,
  signer set at `max_tx_size_derived_collection_item_count`, sibling lists at
  their maximum height.
- Remove `oversized: true` for phase-A publications in `dispute-scenario.ts`
  once all fourteen fit (`semanticIsOversized` becomes false).

## 8. Aiken tests

- `lib/midgard/validation-machine-v1.test.ak`:
  `phase_a_payload_binding_agrees_with_full_binding` (property over generated
  stage-3 controls: `phase_a_native_payload_control_is_bound(…, 3, 3) ==
  phase_a_native_control_is_bound(…)` whenever the full binding holds and the
  transaction decodes; and refuses every control the full binding refuses);
  `phase_a_payload_binding_refuses_stage_zero`;
  `phase_a_carried_binding_refuses_late_continuation_with_wrong_counts`;
  `phase_a_signature_between_payload_resolves_one_step_at_a_time` extending
  the existing `phase_a_native_scripts_resolves_a_nested_signature_one_step_at_a_time`.
- New `validators/fraud-proofs/validation-trace/phase-a-split-v1.test.ak`
  mirroring `cek-split-v1.test.ak`: `signature_between_wire_layout_is_pinned`,
  `every_phase_a_kind_commits_to_the_prepared_evidence_hash`,
  `prepare_routes_signature_between_to_slot_twelve`,
  `signature_between_validator_refuses_a_membership_step` and the other 12
  kinds' honest steps, `prepare_refuses_a_fifteenth_semantic_resolver_index`.

## 9. Verification commands

```bash
cd onchain/aiken && /home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/phase_a_native_scripts_signature_between/.test(v.title))console.log(v.title,Buffer.from(v.compiledCode,"hex").length)'
# expect: main.spend 12,324 ± regeneration drift, and ≤ 15,000
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m phase_a   # all phase_a tests pass; new count = existing 7 + those in §8
cd demo/midgard-fault-proofs && pnpm test -- tests/submit-init-emulator-validation-dispute-phase-a-signature.test.ts   # 1 publication-fit + 1 lifecycle + 1 negative + 1 cancel + 1 max-shape
pnpm test -- tests/semantic-resolver-arity-gate.test.ts tests/compiled-script-arity-gate.test.ts tests/inspect-contracts.test.ts
MIDGARD_PRINT_PROOF_FIT=1 pnpm test -- tests/submit-init-emulator-validation-dispute-phase-a-signature.test.ts   # prints ExUnits per stage
```

## 10. Ordering and dependencies

- Lands with the six sibling payload plans and the token-head/timelock/empty
  borderline subsections: one edit to `validation-machine-v1.ak`, one
  blueprint regeneration, one `phase_a_native_scripts_v1` re-application, one
  catalogue-root re-pin (`Q13_CATALOGUE_ROOT` in `inspect-contracts.test.ts`).
- The item plan (`validation-trace-phase-a-native-scripts-item-semantic-v1.md`)
  changes the same prepare list and adds the phase-A deployment roster and
  submit route; both plans share §6 and must land in the same regeneration.
- Independent of the script-preconditions plans except for the shared
  `submit.ts` roster pattern.
- Other groups touching `validation-machine-v1.ak` (resolve-inputs,
  script-sources, value-and-mint) must rebase onto the same file; no shared
  function is modified in place here (new functions + re-pointed callers).

## 11. Risks

- **Soundness review of PA-UNDECODED.** §4.3 argues inductively from the
  item step. Reviewer must confirm no other producer of a stage ≥ 1
  PhaseANativeScripts control exists (`phase_a_native_successor_is_exact`
  callers: item, token-head, payload, frame steps only).
- **Fallback.** If PA-UNDECODED is refused, PA-CARRY alone leaves this
  resolver at 15,064 (over the 15,000 target, under the 16,384 limit by
  1,320); a further prune would be needed (e.g. dropping the redundant outer
  `rejected_successor_is_exact` arm is not possible; the next candidate is a
  narrower `verify_native_tx_proof_source_v1` that skips
  `decode_native_tx_field_preimage_lengths_v1`).
- **Regeneration drift**: the signature files changed on 2026-09-01; the 2.7 KB
  margin absorbs ordinary drift.
- **ExUnits unmeasured** for the pruned body; bounded by today's figure.
- **Spec**: GOAL_SPEC §8.3 C44 (every node kind, thresholds, empty/invalid
  cases) is unaffected; C53 (resolver proof-fit sweep) is what this plan
  serves.
