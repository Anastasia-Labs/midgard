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
NFT and its zero withdrawal. The stage yields read the same 17-item LOP
control (12 machine items, one span-window commitment, four descriptor-fact
commitments) but decode only the active sub-control. Initial sub-control encodings
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

## Ruling record — execution-budget restructure (2026-09-06, second ruling)

The first-ruling structure left three positive-path execution misses (memory
units against the 13,200,000 basis / 16,500,000 evaluator cap):
ResolveInputs finalize 21,359,516 and ScriptSources finalize 20,046,146
(both over the cap — unpublishable), and the ResolveInputs advance-integer
three-yield step 15,481,633. Ruling, binding:

1. **Finalize → descriptor fact-attach steps + thin terminal.** Each attach
   is an ordinary checkpointed machine step: dispatcher spend plus one or
   two descriptor yields, grouped `[[2, 3], [0], [1]]` (datum+value
   summaries together, then scan facts, then reference script) so each
   positive path measures under the basis. The successor control records
   the attested fact(s) in commitment form
   (`fact_commitment_v1(role, descriptor, value_summary, datum_summary)` =
   blake2b-256 of the serialized role payload — every fact commits to all
   its identifying inputs). The terminal step requires ALL FOUR recorded
   facts to recompute exactly from the redeemer's descriptor
   (`facts_are_exact_v1`), performs authorization + successor, and carries
   NO descriptor yields.
2. **Steps → span verified once.** A span-attach step (new stage role 23,
   `LedgerOutputProofSpanAttach` witness, allowed at the datum-traversal /
   reference-script / script-hash stages) runs `authenticated_output_span`
   once and records `Some { start, length, digest }` in the control (item
   12); subsequent stage steps drop the span yield entirely and bind their
   redeemer window bytes to the commitment inline
   (`bound_window_bytes_v1`: containment + length + blake2b-256 equality +
   slice). Scalar attestation yields stay per-step.
3. Rejected: raising any cap or the 20% basis; redesigning
   `unique_semantic_dispatch_v1`.
4. Any positive path still over basis after this is escalated with
   measurements, never papered over (see the advance-integer entry in the
   measured table below).

**Conjunction equivalence.** The chained conjunction equals the monolithic
finalize predicate clause-for-clause: each descriptor yield performs exactly
the same leaf verification it performed in the five-execution transaction,
and its verified output now enters the control only as that step's
`fact_attach_v1` successor — the dispatcher recomputes the successor control
itself and requires bytewise equality, so a fact can be recorded only in a
transaction that carries that fact's descriptor yield attestation
(machine-attach steps themselves carry no descriptor check; the yield
conjunction in the same transaction is the check). The thin terminal then
recomputes all four commitments from its own redeemer descriptor and demands
equality with the recorded facts, so the terminal is impossible with any
fact missing, forged, stale (different descriptor/dispute), or duplicated —
`fact_attach_v1` only fills the first all-`None` group, and a well-formed
control admits facts only at the terminal stage. The span consumers'
`bound_window_bytes_v1` equality against the recorded digest is exactly the
old per-step authenticated-span check with the merkle walk hoisted into the
span-attach step.

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

- `stage_role` — 24 entries (0 Structure, 1 Value, 2 DatumFoldMap,
  3 DatumFinalizeFrame, 4 DatumHeadScalar, 5 DatumAttachInteger,
  6 DatumFoldList, 7 DatumAdvanceInteger, 8 ReferenceScript, 9 ScriptHash,
  10 NativeScript, 11 StructureAssets, 12 StructureOptional,
  13 StructureFinish, 14 DatumHeadSequence, 15 DatumHeadMap,
  16 DatumHeadLargeConstructor, 17 DatumAttachBytes, 18 DatumAdvanceBytes,
  19 DatumFinish, 20 DatumLargeConstructor, 21 DatumLargeFields,
  22 DatumClose, 23 SpanAttach). Exactly one stage role per step
  transaction, selected by the planner from the pinned control's stage (and
  the `SpanAttach` witness at the content stages).
- attestation roles — scalar-integer and scalar-bytes only, joined to stage
  roles by the `stage_attestation_roles` map ({5, 7} → [scalarInteger];
  {17, 18} → [scalarBytes]; all others → []). Zero or one per step
  transaction, after the stage yield in `yield_ref_input_indices`. The span
  attestation of the first ruling was retired by the second ruling below:
  the span is verified once by the span-attach STEP (stage role 23), its
  window commitment is recorded in the control, and every later consumer
  binds its redeemer window bytes to that commitment inline (one blake2b-256
  plus equality).
- `descriptor_roles` — 4 entries (0 ScanFacts, 1 ReferenceScript,
  2 DatumSummary, 3 ValueSummary), finalize path only; each fact-attach
  step references exactly its attach group's yields (`[[2, 3], [0], [1]]`
  order) and the thin terminal references none.

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
`618dda7547bf0ad8e23b91062d47ae181ba37ac2617d5acebc7ccda3e04a4f5d`, compiler
v1.1.23+5adf783 (the patched fork). (The byte and execution tables below
supersede the first-ruling receipt measured at blueprint `f066a2e6…`.)

### Validator byte table (raw compiledCode bytes; target ≤ 15,000)

| validator                                              | bytes  |
| ------------------------------------------------------ | ------ |
| ledger_output_proof_datum_large_constructor_yield      | 14,608 |
| ledger_output_proof_datum_advance_bytes_yield          | 14,565 |
| resolve_inputs_membership_finalize_semantic_v1         | 14,402 |
| script_sources_output_proof_finalize_semantic_v1       | 14,390 |
| ledger_output_proof_datum_attach_bytes_yield           | 13,920 |
| ledger_output_proof_datum_attach_integer_yield         | 13,734 |
| ledger_output_proof_value_yield                        | 13,445 |
| ledger_output_proof_datum_advance_integer_yield        | 13,376 |
| ledger_output_descriptor_datum_summary_yield           | 12,514 |
| ledger_output_proof_structure_optional_yield           | 11,932 |
| ledger_output_descriptor_scan_facts_yield              | 11,790 |
| ledger_output_proof_datum_head_sequence_yield          | 11,713 |
| ledger_output_proof_structure_assets_yield             | 11,642 |
| ledger_output_proof_datum_head_map_yield               | 11,640 |
| ledger_output_proof_datum_close_yield                  | 11,161 |
| resolve_inputs_membership_step_semantic_v1             | 11,137 |
| ledger_output_proof_native_script_yield                | 11,068 |
| ledger_output_proof_structure_yield                    | 10,959 |
| ledger_output_proof_datum_head_scalar_yield            | 10,954 |
| ledger_output_proof_script_hash_yield                  | 10,901 |
| ledger_output_proof_datum_head_large_constructor_yield | 10,784 |
| ledger_output_proof_datum_large_fields_yield           | 10,475 |
| ledger_output_descriptor_reference_script_yield        | 10,379 |
| ledger_output_proof_datum_finalize_frame_yield         | 9,583  |
| ledger_output_proof_reference_script_yield             | 9,127  |
| ledger_output_proof_datum_fold_map_yield               | 8,009  |
| ledger_output_proof_structure_finish_yield             | 7,985  |
| script_sources_output_proof_step_semantic_v1           | 7,973  |
| ledger_output_proof_datum_fold_list_yield              | 7,761  |
| ledger_output_descriptor_value_summary_yield           | 7,650  |
| ledger_output_proof_scalar_bytes_yield                 | 6,601  |
| ledger_output_proof_scalar_integer_yield               | 6,387  |
| ledger_output_proof_span_yield                         | 6,249  |
| ledger_output_proof_datum_finish_yield                 | 3,927  |

All 34 are under the 15,000-byte target (maximum 14,608). The finalize
dispatchers grew (13,045 → 14,402 / 12,916 → 14,390) with the fact-attach /
thin-terminal branch, and most stage yields grew modestly with the 17-item
control and the inline window binding; the pre-ruling finalize monoliths
(47,310 / 34,559) and advance yields (19,425 / 18,809) remain retired.

### Publication fit (real 16,384-byte envelope, 512-byte reserve)

`demo/midgard-fault-proofs/tests/ledger-output-proof-publication.test.ts`
publishes all 30 yields + 4 dispatchers as reference scripts; ledger pinned
at `validation-trace-ledger-output-proof-publication-fit-ledger.json`
(34 entries, blueprint sha `618dda75…`). Worst signed publication:
`validationTraceDisputeLedgerOutputProofDatumLargeConstructorWithdraw` at
14,954 bytes — minimum margin 1,430, all margins ≥ 512.

### Measured step/finalize execution (emulator, Van Rossem parameters)

Positive-path basis: mem ≤ 13,200,000, CPU ≤ 8,000,000,000 (80% of the
16,500,000 / 10,000,000,000 caps). Semantic-resolution transaction totals,
before (first-ruling structure) → after (fact-attach + span-attach
restructure); every "after" row is asserted in the emulator suite:

| lifecycle                                      | before     | after      | verdict                           |
| ---------------------------------------------- | ---------- | ---------- | --------------------------------- |
| ResolveInputs step, single-yield               | 8,247,619  | 9,041,949  | within basis                      |
| ScriptSources step, single-yield               | 5,654,113  | 7,555,644  | within basis                      |
| ResolveInputs finalize (was one tx, now four): | 21,359,516 |            | was OVER 16.5M cap                |
| — attach [2,3] datum+value summaries           |            | 12,238,119 | within basis                      |
| — attach [0] scan facts                        |            | 12,166,670 | within basis                      |
| — attach [1] reference script                  |            | 10,477,090 | within basis                      |
| — thin terminal []                             |            | 6,850,145  | within basis                      |
| ScriptSources finalize (was one tx, now four): | 20,046,146 |            | was OVER 16.5M cap                |
| — attach [2,3] datum+value summaries           |            | 10,744,841 | within basis                      |
| — attach [0] scan facts                        |            | 10,683,465 | within basis                      |
| — attach [1] reference script                  |            | 8,999,796  | within basis                      |
| — thin terminal []                             |            | 7,260,182  | within basis                      |
| ScriptSources span-attach step (new, role 23)  | —          | 8,009,216  | within basis                      |
| ScriptSources advance-bytes step (2 yields)    | 11,695,656 | 12,750,094 | within basis                      |
| ResolveInputs advance-integer step (2 yields)  | 15,481,633 | 14,226,253 | OVER basis, under cap — ESCALATED |

CPU maxima are far under basis (worst 4,772,365,922 of 8,000,000,000). The
step-path growth against the first-ruling numbers is the 17-item control
(span window + four fact slots) in every control encode/decode plus the
inline window binding; the finalize decomposition trades one over-cap
transaction for four ordinary checkpointed steps.

The single remaining miss — the ResolveInputs advance-integer positive at
14,226,253 (down from 15,481,633 after retiring the per-step span yield) —
is dominated by the ResolveInputs dispatcher's pending-carrier decode inside
`unique_semantic_dispatch_v1`, whose redesign the ruling rejects. It is
escalated with these measurements; its lifecycle stays skipped in the suite
(the honest transaction fits the evaluator cap, so the matching negative
stays active — its refusal is semantic, not budget exhaustion). Every other
previously-skipped lifecycle is active and green.

### Verification

- `aiken check` — 6,359 checks, 0 errors (fresh `aiken build --env testnet`
  first; the second-ruling battery adds fact-commitment/attach/exactness
  selector tests in both polarities in `ledger-output-proof-v1.test.ak` /
  `ledger-output-proof-stages.test.ak` and the machine-level fact-attach
  chain in `validation-machine-v1.test.ak` on top of the first-ruling
  descriptor-yield battery).
- Emulator lifecycles
  (`submit-init-emulator-validation-dispute-ledger-output-proof.test.ts`):
  19 passed / 1 deliberately skipped (the escalated ResolveInputs
  advance-integer positive). Both step positives, all EIGHT finalize-shaped
  positives (both families × three attach groups + thin terminal), the
  span-attach positive and the ScriptSources advance-bytes positive, all
  with basis assertions; refusals pinned to the on-chain EvaluatorError for
  forged successors at membershipStep, ScriptSources step, fact-attach
  (ordinal 0), thin terminal (ordinal 3), span-attach, advance-integer and
  advance-bytes.
- Publication fit test green; fit ledger regenerated with
  `MIDGARD_WRITE_FIT_LEDGER=1` against the fresh blueprint
  (`618dda75…`, compiler v1.1.23+5adf783).
