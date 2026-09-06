# Shared ledger output proof implementation

The source plans are the ScriptSources output-proof step/finalize and
ResolveInputs membership step/finalize plans. The four dispatchers preserve
all existing auxiliary hashes and semantic successor/rejection predicates.
Step dispatch carries the authenticated input control, the proposed next
control, and a role selector. An empty next-control channel means rejection
on both sides; every advance requires the ordinary carrier successor.
Finalize requires the conjunction of four descriptor facts before either
signer/protected-output authorization branch.

Each physical yield authenticates one dispatcher input, its reference role
NFT and its zero withdrawal. The stage yields read the same 12-item LOP
control but decode only the active sub-control. Initial sub-control encodings
are pinned against the typed encoders. Datum actions split by action family;
additional physical splits are allowed only when measured publication or
aggregate transaction limits require them. The four descriptor yields pin
value and datum leaves independently before composing the output summaries.

First measurement seam: a raw control frame with canonical primitive/list/
Option encoding, without a typed sub-control decoder. The exact LOP control
wire uses definite lists and bytes and indefinite nonempty Option fields.
The Blake sub-control alone uses Plutus Data byte encoding for its active
block and working words; the raw encoder preserves that field-specific form.
The helper is specific to this control grammar and rejects map/foreign
constructor forms. Parity fixtures must cover every stage and optional
sub-control, including the existing long terminal golden. If whole-frame
encoding exceeds reserve, replace unchanged fields by authenticated byte
spans; never remove semantic checks or enlarge limits to fit.

Integration owns six ResolveInputs publication roles plus the new LOP roles.
The existing 29 ScriptSources and all other deployment rosters stay intact.
Verification includes positive and honest-refusal registered-chain journeys,
all physical publications, cancel/fresh recovery, maximum output/descriptor/
datum/reference-script/proof carriage, signed bytes and aggregate execution
reserve. The parent owns maxima for the other four ResolveInputs prunes.

## Ruling record (2026-09-06)

Two escalations shaped the shipped structure; both rulings are binding and
recorded here for the independent §9 reviewer.

**Escalation 1 — the 19 stage yields were unreachable (no dispatcher half).**
Ruling: implement per the source plans — the §4.1 step-dispatcher redeemer
with `require_semantic_yield_v1` and the V1VtLop role table, yields
parameterized by both step dispatcher hashes; finalize via four new
descriptor yields per the finalize plans; ABI/identity regeneration
sanctioned.

**Escalation 2 — two committed stage yields exceeded the 16,384-byte hard
limit** (advance-bytes 19,425, advance-integer 18,809 raw compiledCode bytes
after the `scalar_prefix` codec split; 20,152 / 19,786 before it). Ruling:
adopt remedies (A) and (B) together — the finalize plan's decomposition
pattern applied to the step path. Remedy (C), a sub-stage split of the
CEK-data step machine, is **rejected permanently** on the measured evidence
in the ablation table below.

The ruling's structure, as shipped:

1. Attestation role **authenticated-output-span** (`V1VtLopSpanYield`): one
   yield performs the chunk-merkle output-span verification exactly once per
   step transaction. Span-consuming stage yields drop their inline span
   verification; the dispatcher cross-binds the span claim.
2. Attestation roles **scalar-control decode**
   (`V1VtLopScalarIntegerYield` / `V1VtLopScalarBytesYield`): each attests
   that the dispatcher-pinned control's active scalar sub-control decodes
   exactly to the claimed tuple. Datum-family stage yields consume the
   decoded scalars instead of linking `control_from_data_v1`.
3. **The dispatcher conjoins.** The step dispatchers' redeemer carries
   `yield_ref_input_indices: List<Int>` in role-table order with exact
   length fixed by the role; the dispatcher calls
   `require_semantic_yields_v1` once per required role and checks the
   cross-claim equalities. Every yield reads the SAME dispatcher action via
   the shared dispatch — claims bind action fields directly, never a
   redeemer-supplied hash.
4. Soundness invariant: the conjunction of stage yield, attestation yields
   and dispatcher cross-binding verifies the monolithic predicate
   clause-for-clause; every claim binds all free identifying inputs
   (control identity, transaction/output identity, span offsets) so nothing
   substitutes across steps, disputes, or roles.

## Ablation evidence (remedy C is dead)

Complete byte attribution of the pre-ruling 19,425-byte advance-bytes yield
(probe validators, never committed; aiken v1.1.23+5adf783, raw compiledCode
bytes from fresh `aiken build --env testnet`):

| component                                                  | bytes  | probe              |
| ---------------------------------------------------------- | ------ | ------------------ |
| yield floor: shared dispatch + attest                      | 4,349  | p_floor_dispatch   |
| + scalar decode (`scalar_prefix` + `control_from_data_v1`) | +3,773 | p_floor_scalar     |
| + authenticated output span (chunk merkle `source(...)`)   | +3,546 | p_no_span=15,879   |
| + `cek_data_bytes_v1.step_v1`                              | +6,482 | p_no_step=12,943   |
| + `encode_control_v1` + `replace_scalar`                   | +1,179 | p_no_encode=18,246 |

Sub-stage partitions of `step_v1` (remedy C candidates), each as a complete
probe validator:

| partition     | bytes  |
| ------------- | ------ |
| {syntax} only | 14,086 |
| {blob, break} | 18,549 |
| {blob} alone  | 18,412 |
| {break} alone | 13,177 |

The non-splittable prerequisites every advance yield must link (floor 4,349
plus scalar decode 3,773, span 3,546 and successor re-encode 1,179, total
12,847) leave 3,537 bytes of headroom against the 16,384 hard limit, while
the blob sub-stage alone needs 5,469 — no partition of the step machine
fits. Hence (A)+(B): move the span (3,546) and the scalar decode (3,773)
into their own attested roles.

## Roles-table choice

`onchain/aiken/lib/midgard/ledger-output-proof-roles.ak` keeps **three
tables** rather than one appended list, because the three families have
different consumers and different conjunction rules:

- `stage_role` — 23 entries (0 Structure, 1 Value, 2 DatumFoldMap,
  3 DatumFinalizeFrame, 4 DatumHeadScalar, 5 DatumAttachInteger,
  6 DatumFoldList, 7 DatumAdvanceInteger, 8 ReferenceScript, 9 ScriptHash,
  10 NativeScript, 11 StructureAssets, 12 StructureOptional,
  13 StructureFinish, 14 DatumHeadSequence, 15 DatumHeadMap,
  16 DatumHeadLargeConstructor, 17 DatumAttachBytes, 18 DatumAdvanceBytes,
  19 DatumFinish, 20 DatumLargeConstructor, 21 DatumLargeFields,
  22 DatumClose). Exactly one stage role per step transaction, selected by
  the planner from the pinned control's stage.
- attestation roles — span, scalar-integer, scalar-bytes, joined to stage
  roles by the `stage_attestation_roles` map ({4,8,9,14,15,16,20,21,22} →
  [span]; 5 → [scalarInteger]; 17 → [scalarBytes]; 7 → [span,
  scalarInteger]; 18 → [span, scalarBytes]; all others → []). Zero or more
  per step transaction, in role-table order in
  `yield_ref_input_indices`.
- `descriptor_roles` — 4 entries (0 ScanFacts, 1 ReferenceScript,
  2 DatumSummary, 3 ValueSummary), always all four, finalize path only.

Roles 19–22 (DatumFinish, DatumLargeConstructor, DatumLargeFields,
DatumClose) are physical splits of what the source plans sketched as fewer
datum actions, taken under the sketch's own rule — "additional physical
splits are allowed only when measured publication or aggregate transaction
limits require them": the combined validators measured over the repo's
15,000-byte target, and the split members all land under it (table below).
Role renumbering/extension was sanctioned by the ruling; every consumer
(planner `ledger-output-proof-plan.ts`, naming table, deployment roster,
submit roster) uses the tables above.

**Claim mode.** Claims are content-committing: each attestation carries the
attested content (span commitment, decoded scalar tuple) plus every binding
field of the shared dispatch action, and the dispatcher checks cross-claim
equalities against the values it pins itself from the frame. No claim is a
bare hash supplied by the redeemer, so no attestation can be replayed against
a different action, output, or control.

## Receipt (2026-09-06)

Blueprint: fresh `aiken build --env testnet`, sha256
`f066a2e60b434fdc4e35599181daf6548048c402bd986fb2ed51c858145b8574`, compiler
v1.1.23+5adf783 (the patched fork).

### Validator byte table (raw compiledCode bytes; target ≤ 15,000)

| validator                                              | bytes  |
| ------------------------------------------------------ | ------ |
| ledger_output_proof_datum_large_constructor_yield      | 14,332 |
| ledger_output_proof_datum_advance_bytes_yield          | 14,302 |
| ledger_output_proof_datum_attach_bytes_yield           | 13,888 |
| ledger_output_proof_datum_attach_integer_yield         | 13,700 |
| ledger_output_proof_value_yield                        | 13,375 |
| ledger_output_proof_datum_advance_integer_yield        | 13,094 |
| ledger_output_descriptor_datum_summary_yield           | 12,509 |
| ledger_output_proof_structure_optional_yield           | 11,889 |
| ledger_output_descriptor_scan_facts_yield              | 11,788 |
| ledger_output_proof_structure_assets_yield             | 11,577 |
| ledger_output_proof_datum_head_sequence_yield          | 11,428 |
| ledger_output_proof_datum_head_map_yield               | 11,356 |
| ledger_output_proof_script_hash_yield                  | 11,281 |
| ledger_output_proof_native_script_yield                | 11,015 |
| ledger_output_proof_structure_yield                    | 10,894 |
| ledger_output_proof_datum_close_yield                  | 10,875 |
| ledger_output_proof_datum_head_scalar_yield            | 10,669 |
| ledger_output_proof_datum_head_large_constructor_yield | 10,499 |
| ledger_output_descriptor_reference_script_yield        | 10,394 |
| ledger_output_proof_datum_large_fields_yield           | 10,213 |
| ledger_output_proof_datum_finalize_frame_yield         | 9,549  |
| ledger_output_proof_reference_script_yield             | 9,431  |
| ledger_output_proof_datum_fold_map_yield               | 7,975  |
| ledger_output_proof_structure_finish_yield             | 7,920  |
| ledger_output_proof_datum_fold_list_yield              | 7,728  |
| ledger_output_descriptor_value_summary_yield           | 7,648  |
| ledger_output_proof_scalar_bytes_yield                 | 6,551  |
| ledger_output_proof_scalar_integer_yield               | 6,337  |
| ledger_output_proof_span_yield                         | 5,605  |
| ledger_output_proof_datum_finish_yield                 | 3,867  |
| resolve_inputs_membership_finalize_semantic_v1         | 13,045 |
| resolve_inputs_membership_step_semantic_v1             | 11,330 |
| script_sources_output_proof_finalize_semantic_v1       | 12,916 |
| script_sources_output_proof_step_semantic_v1           | 7,759  |

All 34 are under the 15,000-byte target. The pre-ruling advance yields
(19,425 / 18,809) landed at 14,302 / 13,094 after moving span + scalar into
attestation roles; the pre-ruling finalize monoliths (47,310 / 34,559)
landed at 13,045 / 12,916 on the four-descriptor-yield decomposition.

### Publication fit (real 16,384-byte envelope, 512-byte reserve)

`demo/midgard-fault-proofs/tests/ledger-output-proof-publication.test.ts`
publishes all 30 yields + 4 dispatchers as reference scripts; ledger pinned
at `validation-trace-ledger-output-proof-publication-fit-ledger.json`
(34 entries). Worst signed publication:
`validationTraceDisputeLedgerOutputProofDatumLargeConstructorWithdraw` at
14,678 bytes — minimum margin 1,706, all margins ≥ 512.

### Measured step/finalize execution (emulator, Van Rossem parameters)

Positive-path basis: mem ≤ 13,200,000, CPU ≤ 8,000,000,000 (80% of the
16,500,000 / 10,000,000,000 caps). Semantic-resolution transaction totals:

| lifecycle                                                                                                     | mem        | verdict                                 |
| ------------------------------------------------------------------------------------------------------------- | ---------- | --------------------------------------- |
| ResolveInputs step, single-yield (spend 4,475,758 + stage 3,771,861)                                          | 8,247,619  | within basis                            |
| ScriptSources step, single-yield (spend 1,885,382 + stage 3,768,731)                                          | 5,654,113  | within basis                            |
| ScriptSources advance-bytes step, 3 yields (2,158,062 + 4,136,586 + 2,792,411 + 2,608,597)                    | 11,695,656 | within basis                            |
| ResolveInputs advance-integer step, 3 yields (5,961,500 + 4,126,300 + 2,605,777 + 2,790,924)                  | 15,481,633 | OVER basis, under 16.5M cap — escalated |
| ResolveInputs finalize, 4 descriptor yields (spend 6,689,330 + 2,826,486 + 3,720,207 + 5,554,225 + 2,569,268) | 21,359,516 | OVER cap — escalated                    |
| ScriptSources finalize, 4 descriptor yields (spend 5,391,872 + same four yields)                              | 20,046,146 | OVER cap — escalated                    |

The finalize misses are exactly the case for which
`validation-trace-script-sources-output-proof-finalize-semantic-v1.md` §3
reserves the two-hop chain fallback and §7 sets the 13,200,000 gate. The
two-hop chain's cross-transaction binding is not designed in any shipped
plan, so it is escalated rather than improvised; the finalize emulator
lifecycles are skipped in the suite with the measured numbers, and the
finalize negative is skipped with them (budget exhaustion would mask the
semantic refusal it exists to prove). The ResolveInputs advance-integer
positive is likewise skipped on the basis miss; its negative stays active
because the honest transaction fits the evaluator cap, so its refusal is
semantic. Per-yield cost is dominated by
`unique_semantic_dispatch_v1` re-decoding the dispatcher input/transition in
every yield (~2.5–5.5M mem each) — any remedy that shares that decode (or
the two-hop chain) is an owner decision.

### Verification

- `aiken check` — 6,350 checks, 0 errors (baseline 6,339 at stage 1 + the
  11-test descriptor-yield transaction battery
  `ledger-output-descriptor-yields-v1.test.ak`: honest four-yield finalize
  wins; three-yield, role-permutation, forged-leaf, spliced-receive-scan,
  non-terminal-control, and double-dispatcher-input shapes refused; wire
  layout and prepare routing pinned; both rejecting terminals emitted).
- Emulator lifecycles
  (`submit-init-emulator-validation-dispute-ledger-output-proof.test.ts`):
  both step positives with basis assertions, both step negatives pinned to
  the on-chain EvaluatorError, multi-yield advance coverage in both
  polarities via `disputedMatchOrdinal` + inline-datum fixtures; finalize
  pair + RI advance-integer positive skipped per the escalations above.
- Publication fit test green; fit ledger regenerated with
  `MIDGARD_WRITE_FIT_LEDGER=1`.
