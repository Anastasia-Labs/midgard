# Native-script decoding fault: offchain implementation plan (v1)

> **Registration update (2026-08-26):** this family is now registered as
> `nativeScriptDecoding` at `0000000d`. Generic Init,
> catalogue/inspection, node/core deployment identity, watcher proof-thread
> topology, and all four mandatory authenticated reference scripts are wired.
> Family-specific CLI and autonomous watcher detector/prover mounting are not
> implied by that topology and remain open, as do preprod/live evidence. The
> identity change requires fresh genesis/redeployment; there is no migration or
> compatibility path.

Plan date: 2026-08-25. Branch: `plan/decoding-offchain-635` (off
`wave/decoding-thread-635` at `db83dd31`). Issues: #635 (family), #633
(originating divergence). This began as a planning document and now also
records the implemented registration surface that
`docs/fault-proofs/native-script-decoding-fault-thread-design-v1.md` §3.4
explicitly deferred ("Catalogue registration (MPF insert of the new category
id → step-01 script hash), `catalogue.ts` update, and watcher detection logic
are follow-up work items"), against the family as actually built on
`wave/decoding-thread-635` (commits `53b87ff9` family + tests, `db83dd31`
exec ledger + verifier; base `96a5e16a` = `wave/format-640`).

Ruled decisions this plan implements and never re-opens:

- **"Store the pair" (owner ruling 2026-08-25, amends design §4):** the
  thread state carries `outpoint_source_kind` (0 = spend / 1 = reference)
  and `outpoint_cursor` (item ordinal within that field), copied verbatim
  from the forced leaf's rejection arm (direction B) or prover-chosen
  (direction A). The offchain prover supplies exactly this pair; no
  spend-count authority exists anywhere.
- Byte authentication is design §5 option (a): the
  `reference_script_item_commitment` anchor, chunk 0 pinned at bind; the
  #545 published-chunk carriage is an optional transport at step-01, never
  the authentication root.
- `tx_order_id` in the thread state is a ByteArray — the serialised CBOR of
  the forced leaf's trie key (`TxOrderId = OutputReference`), `#""` for
  Normal sources (the type repair flagged in the wave report).
- The family is gated on #640's format-wave integration and cannot land
  ahead of it (design §8; the branch base is the #640 wave itself).
- The rejection commitment is #633's forced-leaf `OperatorVerdictV1` with
  the 47-arm `RejectionReasonV1`
  (`docs/fault-proofs/rejection-reason-catalogue-v1.md`, normative).
- All economics use the PINNED realized rates in
  `onchain/aiken/scripts/native-script-decoding-engine-exec-ledger-v1.json`
  (fork `aiken v1.1.23+6801f62`). Design §6's derived throughput figures
  (≈61–69 nodes/tx, ≈100 transactions) are superseded — the realized fold
  rate is ≈3.5× the one-shot rates; §6's conclusions survive at ≈330–350
  transactions (see §6 below).
- **Dual-consumer proving (owner ruling 2026-08-25):** the offchain code is
  designed so the same proving core is consumable by the watcher for
  autonomous proving AND by the CLI for manual proving. The core is
  consumer-agnostic (§4.3); the CLI and the watcher are thin adapters over
  it. Whether/when autonomous proving is *enabled* in a deployment remains
  an owner act; the shipped policy defaults are decided in §10 Q5.

Where this plan uncovered a genuine decision, it originally recorded an
owner question in §10. On 2026-08-25 the owner delegated the open register
to be decided under the AGENTS.md north star (tradeoff order: correctness,
safety, liveness, performance, convenience); §10 is now a **decision
register** — each entry records the decision, the evidence behind it, and
what would reopen it. The delegation does not extend to the ruled
decisions above, which stand untouched.

All `file:line` anchors are against the worktree state at `db83dd31`.

---

## 1. The contract the builders must satisfy

The as-built onchain family is the byte-for-byte target; nothing in this
section is new design, it is the checklist the builders are written against.

**Step chain** `01 → 02 → 03 → (03 self-loop)* → 04`, `ct.Cancel` on every
step. Parameterization (acyclic, applied backwards like
`buildDoubleSpendChain`, `demo/midgard-sdk/src/fraud-proof/contracts.ts:1078-1160`):

| Validator | Parameters (in order) |
|---|---|
| `fraud_proofs/native_script_decoding/step_01` | `step_02_hash`, `computation_thread_policy_id`, `hub_oracle` |
| `…/step_02` | `step_03_hash`, `computation_thread_policy_id` |
| `…/step_03` | `step_04_hash`, `computation_thread_policy_id`, `field_preimage_certificate_policy_id` |
| `…/step_04` | `computation_thread_policy_id`, `fraud_proof_token_policy_id`, `fraud_proof_token_address` |

**Init** (generic computation-thread mint): one unit of asset name
`category_id(4B) ‖ header_hash(28B)` minted to step-01's address with
`StepDatum { fraud_prover, data: None }`, category membership proven against
the catalogue MPF (`onchain/aiken/validators/computation-thread.ak`).

**Step-01** (`validators/fraud-proofs/native-script-decoding/step-01.ak`):

- `Continue(BindNormalTransaction { carriage })` — direction A over a
  Normal source. `carriage: NativeTxInclusionCarriage` is the shared
  inclusion machinery (`RedeemerCarriedInclusion` or the #545
  `PublishedChunkInclusion`); the bound leaf's embedded scalar must claim
  acceptance (`validity_code == 0`, the §2.4.3(d) predicate,
  `step-01.ak:79`). Output state
  `BindStateV1 { direction: 0, source_kind: 0, verified_tx_id }` at
  step-02's address.
- `Continue(RecordForcedSource { direction, input_index, output_index })` —
  either direction over a Forced source; records the direction only and
  forwards `BindStateV1 { direction, source_kind: 1, verified_tx_id: #"" }`.

**Step-02** (`…/step-02.ak`): the header rides the redeemer and must hash to
the thread NFT's asset-name tail (`step-02.ak:93-98`). Both source kinds
open `event_to_step_root` and `transition_trace_root`
(`engine.verify_committed_pre_state_v1`, `engine.ak:482-525`) to freeze
`prior_ledger_root := transition_step.pre_utxos_root`. Forced threads open
the forced leaf (`engine.verify_forced_leaf_v1`, `engine.ak:533-563`,
including the §2.4.3(e) verdict↔scalar bit equality); direction A requires
`ForcedTxValid` and takes the prover-chosen pair from the redeemer's
`chosen_outpoint_source_kind` / `chosen_outpoint_cursor`; direction B
requires `ForcedTxInvalid` with one of the three decoding arms and copies
the pair and class verbatim from the leaf's reason
(`engine.scan_accusation_of_v1`, `engine.ak:576-609`) — the `chosen_*`
fields are ignored. Output: the 15-field `ScanThreadStateV1`
(`engine.ak:120-152`) with descriptor/machine fields at their sentinels.

**Step-03** (`…/step-03.ak`), three Continue arms:

- `BindOutpoint` (once, gated on the pre-bind sentinels): opens the accused
  field (0 or 1 by `outpoint_source_kind`) through the §8.8 door
  (`opened_field_view` with `BodyAnchor { tx_id: verified_tx_id }`), reads
  the outpoint at exactly `outpoint_cursor` via the 38-byte stride read
  (`spend_input_at` — an out-of-domain ordinal ABORTS, never clamps),
  authenticates the descriptor as the ledger-trie value at
  `encode_midgard_tx_input(outpoint)` under `prior_ledger_root`
  (`mpf_proof_v1.has`), and branches on
  `descriptor.reference_script_language`:
  - language 0: chunk-0 proof mandatory
    (`verify_reference_script_chunk`), `bind_machine_v1` on the first
    chunk — `MachineBoundV1` self-loops with
    `machine_state_hash = hash_machine_control_v1(control_cbor)`;
    `MachineBindMalformedV1` (undecodable wrapper or empty tag-0 payload)
    closes straight to step-04 with `refusal_class = 0`, direction A only;
    `MachineBindNonNativeV1` (wrapper contradicts the descriptor's tag)
    HARD-FAILS for both directions — see §7.3.
  - language ≠ 0: direction B closes to step-04 with the class-0
    contradiction marker (descriptor contradiction), no chunk proof;
    direction A fails the arm.
- `Scan` (self-loop): carried `control_cbor` must hash to the committed
  `machine_state_hash`; an optional chunk window (cursor's chunk plus,
  mandatorily, the adjacent following chunk whenever the item has one) is
  authenticated once per L1 transaction
  (`engine.authenticated_scan_window_v1`, `engine.ak:271-310`); the fold
  (`engine.budgeted_scan_v1`, `engine.ak:392-457`) runs up to `step_budget`
  primitive steps and MUST stop without a refusal (`ScanAdvancedV1`) — a
  refusal mid-fold fails the transaction, so the builder must budget to
  stop just before it. Frame witnesses are supplied in consumption order
  and must hash-chain to the control's `stack_root`. The window-less form
  (`chunk_proof: None`) is legal for frame/finalize-only stretches. The
  safe-read margin (`max_token_byte_width = 33`) stops a token read short
  of the window edge rather than refusing (`engine.ak:371-384`).
- `Verdict` (once): direction B exhibits the exact canonical terminal
  (`structure_terminal_is_exact_v1`, no chunk proofs); direction A runs
  `budgeted_scan_v1(control, window, [], 1)` and requires `ScanRefusedV1`
  — exactly one refusing primitive step, whose class is recorded. Output
  advances to step-04 with `refusal_class` set.

**Step-04** (`…/step-04.ak`): `common.finalize` burns the thread NFT and
mints the permanent `fraud_proof` token at the fraud-proof address
(`fraud_proof_mint_redeemer_index` in the Args); direction B requires
`source_kind == forced ∧ refusal_class == 0 ∧ scan_reason_class ∈ {0,1,2}`,
direction A requires `refusal_class ∈ {0,1,2}`.

**Wire shapes** are pinned by the lib twins
(`lib/midgard/fraud-proofs/native-script-decoding/step-0{1..4}.ak`) and the
thread fixture (`thread-fixture-v1.ak`), which is effectively an executable
spec for the builders: subject/forced-leaf assembly
(`subject_tx_v1`, `forced_leaf_v1`, `forced_leaf_membership_v1`), the
committed-claim fixture (`thread_claim_v1` — the exact
`RootMembershipProof` record shapes the redeemers carry), the asset-name
derivation (`header_asset_name_v1`), descriptors and chunk proofs
(`scan_descriptor_v1`, `single_chunk_proof_v1`), and the self-loop L1
transaction shape (`self_loop_l1_tx_v1`).

---

## 2. Registration

### 2.1 Category id and the append ledger

Canonical category id: **`0000000d`** (append index 13). The SDK's 25-entry
catalogue, generic Init, deployment manifests/inspection, and watcher
proof-thread topology bind `nativeScriptDecoding` to the applied step-01 hash.

### 2.2 Registered positional/pinned surfaces

Registration appended the family to every positional list and re-pinned the
derived roots. The complete checklist remains the deployment audit surface:

| Surface | Where |
|---|---|
| Catalogue order (SDK) | `demo/midgard-sdk/src/fraud-proof/catalogue.ts:26-37` (append `nativeScriptDecoding`) |
| `FraudProofs` contracts record | `demo/midgard-sdk/src/common.ts:214-245` |
| `FaultProofContracts` + `buildFaultProofContracts` | `demo/midgard-sdk/src/fraud-proof/contracts.ts:718-732`, `:2765-2830` |
| Deployment-manifest identity (positional = ABI) | `demo/midgard-core/src/deployment-manifest-identity-v1.ts:28-112` (contract names), `:114-127` (category order) |
| Node manifest-driven `fraudProofs` record | `demo/midgard-node/src/services/midgard-contracts.ts:701-780` |
| Node script descriptors | `demo/midgard-node/src/commands/contract-deployment-info.ts:499-535` |
| Catalogue MPF build (id → step-01 hash insert) | `demo/midgard-node/src/transactions/initialization.ts:82-129` (`fraudProofsToIndexedValidators` → `createFraudProofCatalogueMpf`), deployment info `:131-160`, atomic init `:912-978` |
| CLI category parse | `demo/midgard-fault-proofs/src/bin.ts:195-219` |
| Inspect-contracts unions and readiness gate | `demo/midgard-fault-proofs/src/inspect-contracts.ts:222-252`, `:254-265`, `:285-296`, `:298-308` |
| Watcher thread policy | `demo/midgard-watcher/src/proof-thread-indexer.ts:146-168` (`families[]` entry) |
| Reference-script targets (new family-steps class, §2.3 Q3) | `demo/midgard-node/src/transactions/reference-scripts.ts:1124-1285` |
| Test pins | §8.4 below |

The catalogue is init-time-immutable (spending validator always fails;
D-S13, `docs/fault-proofs/catalogue-status.md:211-214`): registration is a
**fresh genesis-level deployment**, never an upgrade of a live one. The MPF
insert itself is mechanical once the id exists —
`encodeFraudProofCatalogueKey/Value` (`initialization.ts:92-108`) maps the
4-byte BE index to the step-01 script hash, and
`buildFraudProofCatalogueDeploymentInfo` derives the root and per-category
membership proofs the Init builder consumes.

### 2.3 Script deployment: reference scripts (decided by measurement)

The established pattern does **not** deploy per-family step validators as
reference scripts. `nodeRuntimeReferenceScriptTargets`
(`demo/midgard-node/src/transactions/reference-scripts.ts:1124-1285`)
publishes the shared machinery (hub-oracle, catalogue mint, membership-proof
withdrawal, the six validation-trace scripts, …); family submitters attach
their spending validator inline (`attach.SpendingValidator`), which is why
`inspect-contracts.test.ts` asserts `standaloneScriptBytes`,
`withinL1TransactionByteEnvelopeNecessaryCondition` and
`l1SpendingScriptEnvelopeNecessaryCondition` per step.

**Measured (2026-08-25):** blueprint built into scratch from the wave
branch at `db83dd31` with the pinned fork (452 validators); unapplied
compiled sizes: step-01 **6,783 B**, step-02 **11,507 B**, step-03
**24,862 B**, step-04 1,673 B. Step-03 alone exceeds the entire
16,384-byte `maxTxSize` envelope — inline attach is impossible for it
under any redeemer diet, and step-02 inline plus a worst-case forced-leaf
redeemer cannot fit either.

**Decided (Q3, by measurement):** the family's four step validators
deploy as **reference scripts** — one new `referenceScriptTargets` class
for fault-proof family steps, published by the same deployment machinery
as the shared scripts. All four, not just step-03: uniformity keeps the
submitters, the deployment manifest, and the emulator harness one shape,
and the per-step inline-envelope assertions in `inspect-contracts.test.ts`
gain a reference-script-deployment variant for this family instead of
being force-fitted. The §8.2(1) suite remains, recast: it charts the
per-step redeemer frontier (subject-size coverage) under this shape
rather than deciding it.

**Decided (Q4):** v1 implements **both** step-01 carriages —
redeemer-carried (fast path) and the #545 published-chunk transport
(completeness path). Both onchain arms already exist on the wave branch
and are measured (exec-ledger rows: normal 1.74M / published-chunk 2.89M
mem), so the offchain cost is one more evidence path; and completeness
demands it — the subject's size is adversary-controlled, the §8.11
publication frontier admits items to ~13.4 KB
(`docs/spec/midgard-tx.md:528`), and the redeemer-carried frontier is
marginal against that even with reference-scripted validators.
Scan-window publication stays out of scope: design §5(c) keeps it a
transport optimization "the security argument never rests on". Step-02
has no chunked arm on-chain: the §8.2(1) frontier chart must show every
door-admissible forced leaf fits the step-02 redeemer, and a demonstrated
gap is a **wave-branch completeness finding to escalate**, never to
absorb offchain.

---

## 3. Detection

### 3.1 What exists today

There is no autonomous detection→proving loop anywhere
(`docs/fault-proofs/offchain-reference.md` §6, still accurate): the watcher
observes and indexes, proving is manual/CLI-driven. What the watcher
already computes is exactly the raw material this family needs:

- `demo/midgard-watcher/src/block-replay.ts:183,582-583,771-778,2017` —
  recomputes `canonicalOperatorValidity` against the committed
  `authenticatedOperatorValidity` per event.
- `demo/midgard-watcher/src/user-event-indexer.ts:238-618` — projects the
  forced leaf's `OperatorVerdictV1` onto the accepting literal or the
  `RejectionReasonV1` constructor tag
  (`watcherForcedOperatorVerdictV1`, `:592-618`).
- `demo/midgard-watcher/src/event-classification-verifier.ts:138-139,635-636`
  — the classification cross-check.
- `demo/midgard-core/src/native-script-scan-v1.ts` — the TS twin of the
  frozen scan engine (see §5.2), so "run the frozen scan on the committed
  bytes" is already an offchain capability.

### 3.2 Direction B recognition (wrongful rejection)

A block-replay divergence of this family's shape is: a forced leaf whose
verdict is `ForcedTxInvalid { reason }` with `reason` one of
`ResolvedReferenceScriptMalformed` / `ResolvedReferenceScriptNodeLimit` /
`ResolvedReferenceScriptDepthLimit`, where the watcher's own recomputation
of that transaction's resolution disagrees. The detector:

1. Filter forced leaves by the three constructor tags (the
   `watcherForcedOperatorVerdictV1` projection already yields the tag).
2. Read the accused pair `{ source_kind, input_index }` **verbatim from the
   reason's payload** (the 2026-08-25 ruling: this pair IS the accusation;
   the watcher never derives it).
3. **Domain pre-check:** decode the leaf's own `source.compact_cbor` and
   check `input_index < |field(source_kind)|`. An out-of-domain pair is
   classified `OutOfDomainAccusation` and — per the Q1 decision (§7.2) —
   routed to proving through the cardinality close once the closing-arm
   amendment lands on the wave branch. Until that amendment lands, the
   classification refuses Init loudly (on-chain the stride read would
   abort); it is never silently dropped.
4. Resolve the accused outpoint against the pre-state ledger at the
   event's transition step (the same state block-replay reconstructs) and
   fetch the resolved output's descriptor.
5. Decide provability:
   - descriptor's `reference_script_language ≠ 0` → provable at bind
     (descriptor contradiction);
   - language 0 and the TS scan of the item bytes reaches the exact
     canonical terminal → provable by full scan;
   - language 0 and the TS scan itself refuses → the rejection was
     substantively right (possibly with the wrong class — a class
     misattribution among the three arms is NOT provable by this family
     and falls to the residual analysis of design §7.6);
   - descriptor tag 0 but the item's wrapper decodes non-native → the
     wrapper-contradiction corner, unprovable in this family (§7.3).

### 3.3 Direction A recognition (wrongful acceptance)

An acceptance claim is a Normal `transactions_root` leaf whose embedded
scalar is 0, or a forced leaf whose verdict is `ForcedTxValid`. The
detector design scans every accepted transaction: for
every resolved outpoint (spend inputs then reference inputs), if the
resolved descriptor carries `reference_script_language == 0`, run the TS
scan twin over the committed item bytes; any refusal (malformed /
node-limit / depth-limit) is a direction-A fault, and the prover-chosen
pair is that outpoint's `(source_kind, ordinal)`. When several outpoints
refuse, any one suffices; choose the cheapest (fewest nodes before the
refusal) to minimize the thread length.

This sweep adds scan work proportional to the number of resolved tag-0
reference scripts per block. The scan twin is linear in payload bytes with
trivial constants. The finding/proving core exists, but no watcher replay path
currently mounts this sweep; catalogue/topology registration did not make it
default-on. Mounting policy and any incident-response kill switch remain
operational watcher work.

### 3.4 Detection output and routing

The detector emits a **typed finding record**,
`NativeScriptDecodingFindingV1` — direction, source kind, event key
(`tx_id` or serialised `TxOrderId`), header hash, the accused/chosen pair,
the reason class (direction B), the descriptor fields, and the provability
class from §3.2/3.3. The finding record is the CONTRACT between detection
and proving (ruled 2026-08-25): it is the sole input the proving core
(§4.3) accepts, and it is deliberately self-contained — everything needed
to start (or resume) a thread is derivable from it plus chain state, so
the same record drives either consumer:

- **Manual:** the watcher journals the finding alongside a ready-to-run
  CLI invocation; an operator drives the thread through the
  `midgard-fault-proofs` CLI verbs, which call the proving core.
- **Autonomous:** the watcher hands the finding directly to the proving
  core through the prover API (§4.3), gated by its autonomy policy
  (settlement depth, budget caps, dedup — §4.3; defaults decided in
  §10 Q5). Turning the adapter on in a deployment remains an explicit
  owner configuration act.

Findings whose provability class is the wrapper-contradiction corner
(§7.3) are journaled but never routed to proving by either path — the
classification, not the consumer, is the gate. (The §7.2 corner leaves
this refused set once the cardinality close lands; §10 Q1.)

The watcher's `proof-thread-indexer` has the canonical family/topology entry
(category id, step script hashes) so third-party threads of this family are
indexed like every other family's; the indexer also feeds
the autonomy path's dedup check (do not Init a thread whose asset name an
indexed live thread already carries — duplicates are sound but waste fees).

---

## 4. New offchain modules

### 4.1 SDK (`demo/midgard-sdk`)

One new module `src/fraud-proof/native-script-decoding-v1.ts`, following the
fabricated-family template:

- `NATIVE_SCRIPT_DECODING_VIOLATION_ID_V1`, canonical category id
  `0000000d`, and an asset-name helper parameterized on the id:
  `nativeScriptDecodingThreadTokenAssetNameV1(categoryId, headerHash)`.
- Per-step `State` / `Datum` / `Args` / `SpendRedeemer` schemas built from
  the shared generics (`faultProofStepDatumSchema` /
  `faultProofStepRedeemerSchema`, `src/fraud-proof/native.ts:228-248`),
  matching the lib wire twins field-for-field:
  - `BindStateV1Schema` (3 fields) and `ScanThreadStateV1Schema` (15
    fields, declaration order exactly as `engine.ak:120-152`; `tx_order_id`
    as `Data.Bytes()`).
  - Step-01 `Args` enum: `BindNormalTransaction { carriage:
    NativeTxInclusionCarriageSchema }` (already in `native.ts:81`) |
    `RecordForcedSource { direction, input_index, output_index }`.
  - Step-02 `Args` object: indices, `header` (the SDK's `HeaderV1` schema),
    the two `rootMembershipProofSchema` instantiations
    (`src/transition-trace.ts:143` — `EventKey → EventToStepValue` and
    `Int → TransitionStep`), `forced_membership:
    Data.Nullable(rootMembershipProofSchema(TxOrderId,
    ForcedInclusionTxV1))`, and the two `chosen_*` integers. The forced
    leaf's value schema must be the post-#640 `ForcedInclusionTxV1` with
    `verdict: OperatorVerdictV1Schema`
    (`src/rejection-reason-v1.ts:228-233`) — already wire-normative in the
    SDK.
  - Step-03 `Args` enum: `BindOutpoint { input_index, output_index,
    subject_field_opening: FieldOpeningV1Schema, descriptor_cbor,
    ledger_membership_proof, first_chunk_proof }` | `Scan { …,
    control_cbor, chunk_proof, next_chunk_proof, frames, step_budget }` |
    `Verdict { … }`.
  - Step-04 `Args`: indices plus `fraud_proof_mint_redeemer_index`.
- **Schemas that do not exist yet in the SDK and must be added** (each
  verified against its Aiken twin before use):
  - `BoundedItemChunkProofV1Schema` (`bounded_item_v1.ChunkProofV1`:
    version, field_index, item_index, total_length, chunk_index, chunk,
    frontier, siblings). The builder half
    (`demo/midgard-core/src/bounded-item-v1.ts`) exists; the `Data` wire
    schema is new.
  - `NativeScriptFrameV1Schema` (tail, kind, child_count, remaining,
    valid_count, required — `native-script-scan-v1.ak:59-66`).
  - An `mpf.Proof` wire schema for `ledger_membership_proof`, if none is
    already exported (the MPF library is consumed in four fault-proofs
    modules; `prepare-double-spend.ts` is the precedent for converting
    library proofs to on-chain `Proof` data).
- `src/fraud-proof/contracts.ts` additions are registered (title
  map `NATIVE_SCRIPT_DECODING_FAULT_PROOF_TITLES` over the four blueprint
  titles, `buildNativeScriptDecodingChain` applying parameters backwards
  per §1's table through the arity-checked `applyBlueprintParams`
  (`contracts.ts:916-933`), and the `FaultProofContracts` extension). Until
  registration the emulator harness resolves the four validators directly
  from the blueprint, exactly as fabricated-deposit's submitters take an
  already-resolved contracts record instead of going through
  `resolveFaultProofDeploymentContracts`
  (`demo/midgard-fault-proofs/src/submit-fabricated-deposit-step-01.ts`
  header).

### 4.2 Evidence assembly (`demo/midgard-fault-proofs`)

New module family `src/native-script-decoding/`:

- **`evidence-v1.ts`** — assembles, per thread, everything the redeemers
  carry:
  - the committed-claim openings via the existing witness builders
    (`src/transition-trace/witnesses.ts`: `buildEventToStepMembershipProof`
    `:118`, `buildIndexedTraceProof` `:83`; counted-root plumbing in
    `src/transition-trace/phas.ts`);
  - the forced-leaf membership proof under
    `ForcedTransactionsV1RootDomain` (same phas machinery; the leaf value
    encodes with the post-#640 verdict);
  - the subject's field opening via the door builder
    (`src/field-opening-v1.ts`: `planFaultProofFieldOpeningV1` `:208`,
    `faultProofFieldOpeningV1` `:421`) for field 0 or 1 per the pair;
  - the accused outpoint's trie key
    (`encodeMidgardTxInput` — TS twin of `encode_midgard_tx_input`) and
    the ledger-trie membership proof of `descriptor_cbor` under
    `prior_ledger_root`. **Data source:** the pre-state ledger trie at the
    event's transition step, reconstructed the same way the watcher's
    block replay reconstructs it (the node's ledger MPF machinery,
    `demo/midgard-node/src/workers/utils/mpf.ts`); the evidence module
    takes the trie handle as an input rather than owning reconstruction.
  - the reference-script item bytes and chunk proofs via
    `demo/midgard-core/src/bounded-item-v1.ts`
    (`buildMidgardBoundedItemChunkProofV1` `:190`) in the
    reference-script commitment domain the descriptor pins;
  - the field-cardinality opening for the out-of-domain close (Q1, §7.2):
    the same §8.8 door exposes the accused field's item count, and the
    evidence module proves `outpoint_cursor ≥ count` for the closing arm
    — direction B only.
- **`scan-plan-v1.ts`** — the scan-loop planner (§5).
- **Submitters** `submit-native-script-decoding-init.ts` and
  `…-step-01.ts` … `…-step-04.ts`, following the per-step submitter
  pattern: each an independent Effect that builds one L1 transaction,
  sources the step validator by reference (§2.3, Q3), and returns
  `nextThreadOutRef = "txHash#index"`. Init mirrors `submit-init.ts`
  (asset name `categoryId ‖ headerHash` `:537`, first-step datum
  `{ fraud_prover: signer.paymentKeyHash, data: null }` `:551-557`, Init
  mint redeemer with the catalogue membership proof and the four reference
  indices `:576-609`, plus the PHAS zero-withdrawal `:613-637`).
  Generic `submit-init` accepts the category. Family-specific step CLI verbs
  remain open even though the category exists.

### 4.3 The proving core and its two consumers (ruled 2026-08-25)

No existing family loops, so nothing in the runtime assumes a self-loop;
the step-03 loop needs a driver above the per-step submitters. Per the
2026-08-25 ruling, that driver is designed as a **consumer-agnostic
proving core** consumable by the watcher (autonomous) and the CLI
(manual) alike. The dependency direction already exists —
`demo/midgard-watcher/package.json` depends on
`@al-ft/midgard-fault-proofs` (workspace) — so the core lives in
`demo/midgard-fault-proofs/src/native-script-decoding/prover-v1.ts` and
no new workspace edges are introduced.

**The core:**

```ts
proveNativeScriptDecodingFaultV1(
  finding: NativeScriptDecodingFindingV1,   // §3.4 — the sole input
  deps: NativeScriptDecodingProverDepsV1,
): Effect<NativeScriptDecodingProofOutcomeV1>
```

driving Init → step-01 → step-02 → BindOutpoint → (Scan)* → Verdict →
step-04: build each transaction from the evidence module and planner,
submit, await confirmation, feed `nextThreadOutRef` forward. Properties
the ruling requires of it:

- **Capability-injected, zero consumer coupling.** `deps` carries
  everything environmental: signer/wallet, chain provider, the pre-state
  ledger-trie source (§4.2), a confirmation awaiter, a journal sink for
  progress events, and the policy record below. The core imports nothing
  from the CLI and nothing from the watcher; both adapters import it.
- **Resumable and idempotent-by-reconstruction** (§7.1): invoked against
  a header whose thread already exists, it locates the thread UTxO by
  asset name, reads the on-chain `StepDatum`, recovers the position
  (including mid-loop via the `machine_state_hash` boundary search), and
  continues. Crash, retry, and double-invocation all converge — which is
  precisely what makes unattended operation safe.
- **Policy as data, not code.** `NativeScriptDecodingProverPolicyV1`:
  settlement-depth gate (min L1 depth of the faulted header before
  spending; default = the watcher's existing finality-policy depth,
  `finality-engine.ts:31` `confirmationDepth` 2,160), per-thread fee
  budget cap (default 650 ADA — worst case ≈510 plus margin — checked
  against §6's plan-time estimate before Init and re-checked as the loop
  progresses), a single-flight cap (default one autonomous thread at a
  time), a maturity guard (refuse Init when the remaining maturity window
  is under twice the predicted serial duration), and a dedup predicate
  (skip when the proof-thread indexer already sees a live thread with
  this asset name, §3.4). Defaults decided in §10 Q5; every value is
  deployment-overridable. The core enforces whatever policy
  it is handed; it hard-codes none. Only the §3.2/3.3 provability
  classification is non-negotiable — unprovable corners are refused at
  the API boundary regardless of policy.
- **Outcome as data:** proven (fraud-proof token minted), refused
  (classification/policy, with the reason), stalled (unexpected on-chain
  abort — surfaced loudly, never silently cancelled; cancellation is its
  own explicit call).

**The two adapters (thin by construction):**

- **CLI (manual):** `bin.ts` verbs — a one-shot
  `prove-native-script-decoding` wrapping the core end-to-end, plus the
  per-step submitter verbs every family exposes for surgical use. The
  operator IS the policy: the CLI adapter passes a permissive policy and
  the operator's wallet.
- **Watcher (autonomous):** a prover entry the watcher can mount as a
  fiber — consume finding records (§3.4), apply the configured
  `ProverPolicyV1`, invoke the core, journal outcomes. The adapter ships
  with the family; whether the watcher process *enables* it is owner-set
  configuration. It ships **default OFF**, and enabling requires an
  explicit config block naming a dedicated prover wallet distinct from
  the watcher's operational identity — the adapter refuses to run
  autonomously on the operational wallet (§10 Q5).

Per-step submitters remain independently exported (the CLI's surgical
verbs and the emulator tests use them directly); the core composes them
rather than replacing them.

---

## 5. The step-03 scan loop, in full

This is the genuinely new offchain surface. The invariant: the builder
maintains the exact machine the validator will re-derive, so every
`Scan`/`Verdict` transaction is constructed from a locally simulated run
that the chain then confirms.

### 5.1 Lockstep simulation

`demo/midgard-core/src/native-script-scan-v1.ts` already twins the frozen
primitives (`advanceMidgardNativeScriptStructureTokenV1` `:428`,
`…FrameV1` `:498`, `finalizeMidgardNativeScriptStructureV1` `:542`,
`encode/decodeMidgardNativeScriptStructureControlV1` `:233/:251`,
`hashMidgardNativeScriptScanFrameV1` `:313`, and the whole-trace builder
`buildMidgardNativeScriptStructureTraceV1` `:565`). The plan treats this
twin as the single source of offchain scan truth and adds only:

- a TS twin of `engine.bind_machine_v1` (versioned wrapper parse →
  initial control) and of `hash_machine_control_v1` (domain
  `"midgard/fraud-proofs/native-script-decoding/control-v1"` ‖ canonical
  control CBOR, blake2b-256 — `engine.ak:101-103`, `:233-237`);
- a TS twin of the fold's stop conditions (`budgeted_scan_v1` semantics:
  budget, terminal, frame exhaustion, and the 33-byte safe-read margin
  `engine.ak:371-384`) so the planner predicts exactly where the on-chain
  fold stops for a given (control, window, frames, budget).

**Byte-fidelity gate:** before any emulator run, a differential test drives
both engines — the TS twin and the Aiken engine via its fixture vectors
(`engine.test.ak`, `midgard/native_script_payload_vectors_v1`) — over the
same items and asserts control-CBOR and `machine_state_hash` equality at
every step boundary (§8.3).

### 5.2 The segment planner (`scan-plan-v1.ts`)

Input: the item bytes, the direction, and the budget policy. Output: an
ordered list of transaction plans, each
`{ control_before, control_after, window: {chunk_index, need_next},
frames_consumed, step_budget }`, with the final entry a `Verdict` plan.

Planning algorithm:

1. Run the full trace offchain (`buildMidgardNativeScriptStructureTraceV1`
   plus the frame stack), recording every primitive step, its kind
   (token/frame/finalize), the cursor, and the frames popped.
2. Cut the trace into segments of at most `N` primitive steps, where `N`
   is the per-transaction node budget from the **pinned** engine ledger:
   at the §3.3 basis (13.2M mem) minus the ≈1.0M step envelope, the
   realized rates (≈746K mem/node deep slope, ≈655K wide — ledger note)
   give **≈16 nodes/tx deep, ≈18 wide**; the planner defaults to 16 and
   accepts a policy override, and additionally respects window geometry: a
   segment never spans a window change (the window is authenticated once
   per transaction, so a segment's token steps must all read inside one
   `(chunk, chunk+1)` window). The safe-read margin makes the on-chain
   fold stop early at a window edge; the planner cuts segments at exactly
   the same boundary so `step_budget` is always reached or the stop is
   window-exact — either way the on-chain fold ends `ScanAdvancedV1` at
   the planned `control_after`.
3. Direction A: the trace ends at a refusing step; the planner cuts the
   last segment to stop **one primitive step before** the refusal (the
   fold aborts a transaction that hits a refusal mid-budget), and emits a
   `Verdict` plan whose single-step fold exhibits the refusal with its
   window.
4. Direction B: the trace ends at the exact terminal; the last `Scan`
   segment folds through `finalize` to the terminal stage, and the
   `Verdict` plan carries no window (`structure_terminal_is_exact_v1` on
   the carried control).
5. Frames: each segment carries the frames popped within it, in
   consumption order (they hash-chain against `stack_root`; a wrong or
   reordered frame aborts on-chain, and can never mint — witness error,
   not verdict).

Bind-time short circuits need no plan: `MachineBindMalformedV1`
(direction A), the non-tag-0 descriptor contradiction (direction B), and
— once the Q1 closing arm lands — the out-of-domain cardinality close
(direction B, §7.2) all close at `BindOutpoint`, giving the minimal
thread Init → 01 → 02 → bind → 04 (five transactions).

### 5.3 Budget and ExUnits discipline

Every transaction plan is checked against the pinned ledger before
submission: predicted ExUnits = fold share (nodes × pinned slope) + the
pinned step-row envelope for the arm
(`native-script-decoding-engine-exec-ledger-v1.json`: bind 4.57M mem, scan
row 4.30M at fixture size, verdict 3.20–3.39M, plus the fold slope for the
budgeted nodes), and the builder refuses to submit a plan predicted over
the 13.2M/8B basis rather than discovering it on-chain. The evaluator's
actual numbers (emulator or node preview) are cross-checked against the
prediction in tests; a divergence beyond the ledger's fixture share is a
finding (possible rate drift), never something to absorb by raising
budgets.

---

## 6. Economics and pacing

All figures from the pinned engine ledger (fork `+6801f62`); design §6's
derived figures are superseded and appear nowhere below.

- **Per-transaction throughput:** ≈16–18 nodes per scan transaction (deep /
  wide), mem-bound; CPU non-binding (≈302M cpu/node deep).
- **Worst case** (maximal 5,447-node payload, either direction): ≈320–340
  scan transactions, ≈330–350 L1 transactions total including the
  envelope steps. At the design's stated fee assumption (≈1.45 ADA per
  full step transaction — an operational assumption, not a pin) that is
  **≈480–510 ADA**, under 0.7% of the
  75,000-ADA `fraud_prover_reward` production profile.
- **Typical case:** committed native scripts are overwhelmingly small; a
  signature-node or few-node script closes in a handful of primitive steps,
  so the common thread is 6–8 transactions (Init, 01, 02, bind, ≤1–2
  Scan, Verdict, 04) ≈ 10 ADA. The bind-time short circuits are 5.
- **Pacing:** the thread is a single linear UTxO — strictly one
  transaction per block per thread (~20s), so the worst case runs ≈2 hours
  serial, comfortably inside the half-of-seven-days maturity fit
  (`architecture.md:283-286`). The proving core (§4.3) submits, awaits
  confirmation, then builds the next transaction from the confirmed
  `nextThreadOutRef`; same-block chaining of unconfirmed steps is a
  possible optimization deliberately out of scope for v1 (rollback of a
  chained prefix would strand the suffix).
- **Funding and retry:** the prover wallet funds fees, the thread UTxO's
  min-ADA, and collateral. The plan-time cost estimate rides the finding
  record, and the core's budget-cap policy (§4.3) checks it against the
  wallet before Init and as the loop progresses — the same check serves
  the operator reading a CLI prompt and the autonomous path refusing an
  over-budget thread. Every submitter is idempotent-by-reconstruction: on
  timeout/rollback the core re-queries the thread UTxO by asset name and
  rebuilds the next transaction from the on-chain state — no local state
  is authoritative (§7.1).

---

## 7. Cancel, recovery, and the unprovable corners

### 7.1 Crash-resume

The on-chain thread state is the recovery root. Resume procedure, given a
header hash and category id:

1. Locate the thread UTxO by asset name `category_id ‖ header_hash` at any
   of the four step addresses (or conclude the thread is finished /
   cancelled by the mint history).
2. Decode `StepDatum`; the holding address identifies the step, the state
   identifies the position: `BindStateV1` → run step-02 next;
   `ScanThreadStateV1` with pre-bind sentinels → BindOutpoint; with
   `machine_state_hash ≠ #""` and `refusal_class == -1` → mid-loop; with
   `refusal_class ≠ -1` → step-04.
3. Mid-loop position recovery: re-run the offchain trace (§5.1) and find
   the unique step boundary whose control hashes to the committed
   `machine_state_hash` (the control preimage is not on-chain by design;
   determinism makes re-derivation exact). Re-plan the remaining segments
   from there. A hash that matches no boundary of the re-derived trace
   means the local bytes differ from the committed item — an evidence
   error to surface loudly, never to paper over.
- Cancel: `ct.Cancel` at every step (prover-signed;
  `BurnForCancellation` on the thread mint) reclaims the min-ADA and burns
  the NFT. A cancel submitter/CLI verb ships with the family like every
  other step.

### 7.2 The B-completeness corner (DECIDED 2026-08-25 — closing arm)

If a forced leaf's decoding-arm reason names a pair whose `input_index` is
outside the named field's cardinality, the on-chain bind can never succeed
(`spend_input_at` aborts past the committed collection; §7.3
abort-never-clamp), so direction B was **unprovable for exactly the leaves
whose accusation is most absurd**.

**Decision (delegated): the closing arm.** Step-03 gains a third
bind-time close, direction B only: prove
`outpoint_cursor ≥ |field(outpoint_source_kind)|` from the committed
bytes (the §8.8 field-opening door already exposes the item count) and
close to step-04 with the class-0 contradiction marker — the accusation
names a subject the machine could never have resolved, which is exactly
this family's fault statement ("the committed reason is not the machine's
verdict on the named subject", design §7.6). Step-04 needs **no change**:
its direction-B gate (`source_kind == forced ∧ refusal_class == 0 ∧
scan_reason_class ∈ {0,1,2}`) already admits the close, and
`scan_accusation_of_v1` guarantees the class domain.

Why this branch:

- **Safety asymmetry.** The stall-and-cancel alternative was sound —
  design §7.6's misattribution analysis lands the residual on the
  interactive family (a machine trace can never validly emit a reason
  naming a nonexistent input, and `verify_forced_leaf_v1` range-checks
  nothing, so the contradiction surfaces in the trace) — but it made the
  *cheapest* wrongful rejection to mount (write a garbage index) the only
  one requiring the *most expensive* remedy (the interactive protocol).
  The catalogue's whole thrust (#633) is maximal single-party coverage;
  correctness-before-convenience closes it non-interactively.
- **Timing.** The wave branch is unmerged and registration is a fresh
  genesis-level deployment regardless (D-S13): adding the arm now costs
  zero deployment churn; after registration it would cost a whole new
  deployment.
- **Long-term rule.** AGENTS.md: no stopgaps meant to be replaced later —
  shipping the family with a known coverage notch and a backstop is
  exactly that shape.

**Onchain amendment scope** (rides the owner's integration of
`wave/decoding-thread-635`, executed there — not on this plan branch): the
step-03 arm (an out-of-domain proof path beside `BindOutpoint`), its lib
wire twin, fixture + selector tests, and an exec-ledger row for the new
arm. The offchain consequences are now planned, not hypothetical: the
evidence module's cardinality-proof path (§4.2), the planner's fourth
short circuit (§5.2), and the detector routing the corner to proving
(§3.2). The decision reopens only if the door turns out not to expose the
field's item count — the §4.2 builder would surface that immediately.

### 7.3 The wrapper-contradiction corner (recorded, not open)

A descriptor claiming `reference_script_language == 0` over an item whose
versioned wrapper decodes non-native hard-fails `BindOutpoint` in both
directions (`step-03.ak:198-203` — `MachineBindNonNativeV1` ⇒ fail): the
frozen machine never scans such an item, so neither direction closes
through this family. The detector must classify this case (§3.2) and not
start a thread on it; a thread accidentally started (evidence raced a
better view of the bytes) stalls and cancels per §7.1. This mirrors the
as-built onchain behavior and is not a new decision; it is listed here so
the detector's classification table is total.

---

## 8. Testing

**Standing requirement (owner directive 2026-08-25):** every plan for new
offchain code that interacts with the contracts — this one and future ones
— includes **lucid-evolution emulator tests of realistic scenarios, in
both polarities**: the full fault-proof flow succeeds when the fault
really exists in the state commitment, and fails when it should not — an
adversarial actor attempting the fault proof against an honest state
commitment must be refused by the validators. Suites 4–5 below carry the
positive polarity; suite 7 carries the adversarial one.

### 8.1 Where and how

- Emulator end-to-end tests (the lucid-evolution `Emulator` harness the
  fault-proof suites already run on) live in
  `demo/midgard-fault-proofs/tests/`, in
  their **own file** (`submit-init-emulator-native-script-decoding.test.ts`)
  — the wasm32 UPLC heap ceiling is why per-family files exist
  (`submit-init-emulator-registered-families.test.ts` header). All vitest
  runs go through the package scripts (`--pool=forks
  --no-file-parallelism` live in the script, not the config —
  `demo/midgard-fault-proofs/vitest.config.ts` only wires the heap guard),
  under the standard memory-capped invocation.
- Pre-registration, the harness registers the family as an extra category:
  `buildCatalogueDeploymentInfo(fraudProofs, extraCategories)`
  (`tests/support/submit-init-emulator-shared.ts:1426-1490`) inserts it
  into the same MPF trie and returns its membership proof — the exact
  fabricated-deposit pattern
  (`submit-init-emulator-fabricated-deposit.test.ts:177`). A
  `realNativeScriptDecoding` opt-in flag joins
  `buildMinimalFaultProofContracts` (`:828`) so other suites keep loading
  always-succeeds stubs.

### 8.2 Suites, in order of construction

1. **Envelope/frontier measurement (before builders):** blueprint built
   into scratch with the pinned fork, parameters applied, and the
   per-step redeemer frontier charted against the 16,384-byte envelope
   under the reference-script deployment shape (§2.3, decided): the
   worst-case Scan window arithmetic, the step-01 carriage frontier per
   transport (both carriages, Q4), and the step-02 forced-leaf frontier
   against the §8.11 publication bound — a demonstrated step-02 coverage
   gap escalates as a wave-branch completeness finding (§2.3). The
   2026-08-25 compiled-size measurement (step-03 24,862 B > the whole
   envelope) is this suite's first pinned datum.
2. **Twin differential suite** (`midgard-core` or `midgard-fault-proofs`
   unit tests): TS scan twin + the new bind/hash twins vs the Aiken
   engine's fixture vectors — control CBOR, frame hashes, and
   `machine_state_hash` equal at every step boundary; the four bind
   dispatch cases; the safe-read stop; refusal classes 0/1/2 (limit
   classes via crafted controls, as `engine.test.ak` does, since capped
   items cannot reach them authentically).
3. **Planner unit tests:** segment cuts respect budget and window
   geometry; direction A stops one step short of the refusal and the
   Verdict plan refuses; direction B ends terminal-exact; resume from any
   committed boundary re-derives the identical remaining plan; ExUnits
   prediction per plan is within basis.
4. **Emulator end-to-end, direction A (Normal source):** Init → 01 → 02
   → bind → Scan* → Verdict → 04 on a malformed multi-chunk payload, in
   both step-01 carriages (redeemer-carried and published-chunk — Q4);
   assert the fraud-proof token mints and the thread NFT burns.
5. **Emulator end-to-end, direction B (forced source):** a forced leaf
   with `ResolvedReferenceScriptMalformed` over a canonical payload —
   full scan to terminal; plus the descriptor-contradiction short circuit
   (non-tag-0) and the direction-A forced-acceptance variant
   (`ForcedTxValid` leaf, resolved malformed payload).
6. **Negative/abort coverage at the emulator level:** cancel at each
   step; a Scan with a stale control (replay) refused; a substituted
   chunk refused; the out-of-domain cardinality close convicts on a
   genuinely out-of-domain accusation and is refused on an in-domain one
   (the §7.2 arm, both polarities, once the amendment lands); resume
   mid-loop after a simulated crash.
7. **Adversarial-prover suite (honest commitment, realistic scenarios;
   the owner-directed negative polarity):** a fully honest header —
   canonical payloads, correct verdicts — against which an adversarial
   prover attempts every road to a wrongful conviction, each refused
   on-chain at the exact step named: direction A against a well-formed
   payload (the scan folds to the canonical terminal, no Verdict window
   can exhibit a refusal, and a forged mid-fold refusal fails the Scan
   transaction itself); direction B against an honest, correctly-classed
   rejection of a genuinely malformed payload (terminal-exact can never
   pass, the descriptor contradiction cannot fire on tag-0, the
   cardinality close is refused in-domain); forged evidence — substituted
   chunk bytes, a foreign descriptor, a mismatched leaf — refused by the
   chunk/MPF/membership proofs; and a third party attempting to drive or
   cancel an honest prover's thread. The suite asserts not merely that
   these fail but that each fails at the intended validator check, so a
   refactor that accidentally widens an arm turns it red.

### 8.3 Registration re-pins

Registration moved the pinned surfaces, and every re-pin follows the
provenance convention (append an entry recording
blueprint md5, validator count, and the derivation route; never transcribe
from a failing assertion):

| Pin | Where |
|---|---|
| `Q13_CATALOGUE_ROOT`, `Q13_APPLIED_STEP_HASHES` | `demo/midgard-fault-proofs/tests/inspect-contracts.test.ts:46-51,87-88` |
| Watcher catalogue roots (`FIXED_SCRIPT_CATALOGUE`, `POSITIONAL_SCRIPT_CATALOGUE`) | `demo/midgard-watcher/tests/canonical-fraud-proof-catalogue.ts` (derived by running `buildFraudProofCatalogueDeploymentInfo`, per its own comment) |
| Blueprint `validatorCount` pins | `demo/scripts/verify-canonical-v1-abi-freeze.mjs:731,1075`; `verify-canonical-v1-cg1-control-publication-fit.mjs:406-408` |
| Node catalogue tail assertions | `demo/midgard-node/tests/fraud-proof-catalogue.test.ts:41-45` (append-safe; insertion is not) |
| Inspect-contracts unions / category order / manifest identity | §2.2 table |

Note the standing drift the first re-pin will surface: the
inspect-contracts provenance records 398 validators under fork
`+2a78108`, while the family's own commit records a 444 → 452 blueprint
delta — the pins are several waves behind the onchain tree, so the first
re-derivation moves by a large amount and must go through the recorded
derivation route (`MIDGARD_PRINT_PROOF_FIT=1`), not a hand edit.
`onchain/aiken/plutus.json` is gitignored and stale-or-absent in
checkouts; every blueprint-dependent step builds into scratch with the
pinned fork.

---

## 9. Sequencing and dependencies

1. **#640 format wave integration** — hard gate (ruled). The family's
   branch is based on the wave; nothing offchain lands ahead of it.
2. **Onchain family integration** — the owner integrates
   `wave/decoding-thread-635` (`53b87ff9`, `db83dd31`), now including the
   §7.2 closing-arm amendment (Q1, decided) executed on that branch.
   Blueprint regeneration and the ABI/validator-count evidence ride that
   integration, not this plan.
3. **Builder wave:** SDK schemas, twins and
   planner (§5), evidence module and submitters (§4.2) including both
   step-01 carriages (Q4) and the cardinality-proof path (Q1), the
   proving core and both adapters (§4.3), suites §8.2(1–7) under the
   extra-category harness. Deliverable is green emulator end-to-ends in
   both directions and both polarities with zero pin movement. Item
   §8.2(1) (envelope/frontier) runs first; its step-02 frontier check is
   the one place that can still escalate (§2.3).
4. **Registration/deployment:** canonical `0000000d`,
   catalogue/manifest/generic-Init appends (§2.2), all four mandatory
   reference-script targets (§2.3, Q3), watcher proof-thread topology, re-pins
   (§8.3), and fresh genesis-level deployment (D-S13) are complete.
   Family-specific step CLI and autonomous watcher actuation remain open.
5. **No outstanding rulings block the sequence** (register decided
   2026-08-25, §10). The remaining owner acts are operational: the wave
   integration itself (step 2) and any deployment's decision to switch the autonomous
   adapter on (Q5).

---

## 10. Decision register (owner-delegated 2026-08-25)

The owner delegated the open questions to be decided under the AGENTS.md
north star (tradeoff order: correctness, safety, liveness, performance,
convenience). Each entry records the decision, the evidence, and what
would reopen it.

- **Q1 — B-completeness corner: DECIDED, closing arm (§7.2).** Step-03
  gains a direction-B cardinality close; step-04 is untouched (its gate
  already admits the close). Grounds: single-party coverage of the
  cheapest censorship move; zero deployment cost while the wave is
  unmerged; the interactive-family backstop (design §7.6) made
  stall-and-cancel sound but stopgap-shaped. Reopens only if the
  field-opening door turns out not to expose the field's item count —
  the §4.2 builder would surface that immediately.
- **Q2 — Category id: DECIDED and registered (§2.1).**
  `nativeScriptDecoding` is canonically `0000000d`.
- **Q3 — Deployment shape: DECIDED BY MEASUREMENT, reference scripts
  (§2.3).** Step-03's compiled validator is 24,862 bytes — larger than
  the whole 16,384-byte envelope — so inline attach is impossible, not
  merely tight. All four steps deploy as reference scripts (one new
  `referenceScriptTargets` class); the per-step envelope assertions gain
  a reference-script variant. Not reopenable: the measurement is
  arithmetic.
- **Q4 — Step-01 carriage scope: DECIDED, both carriages in v1 (§2.3).**
  Subject size is adversary-controlled and the redeemer-carried frontier
  is marginal against the §8.11 publication bound, so the
  published-chunk path is completeness, not optimization — and both
  onchain arms already exist and are measured. Scan-window publication
  stays out (design §5(c)). Residual guard: the §8.2(1) frontier chart
  must show step-02's redeemer covers every door-admissible forced leaf;
  a gap escalates as a wave-branch finding.
- **Q5 — Autonomy enablement defaults: DECIDED (§4.3).** The watcher
  adapter ships default OFF; enabling is an explicit per-deployment
  config block that must name a dedicated prover wallet distinct from
  the watcher's operational identity (the adapter refuses the
  operational wallet). Policy defaults: settlement-depth gate = the
  watcher's existing finality depth (`finality-engine.ts:31`,
  `confirmationDepth` 2,160); budget cap 650 ADA/thread (worst case
  ≈510 + margin); one autonomous thread in flight; maturity guard =
  refuse Init when under 2× the predicted serial duration remains. All
  deployment-overridable; the act of enabling in any real deployment
  remains the owner's.
- **Q6 — Direction-A sweep: core defined, watcher mounting open (§3.3).**
  The cost is bounded per block (linear in resolved tag-0 bytes), but no
  default replay-path mount or kill-switch is claimed by this registration.

Not questions (already ruled or repaired): the pair ruling (2026-08-25),
the `tx_order_id` ByteArray type repair (flagged in the wave report), the
pinned-rates supersession of design §6, completed catalogue-id allocation,
and the fee assumptions recorded in the registered deployment review.

---

## 11. Out of scope

- Any onchain change: validators, engine, formats, pins, exec ledgers,
  and the #640 wave itself. (The Q1 closing-arm amendment is *scoped* in
  §7.2 but executed on the wave branch, not on this plan branch.)
- Family-specific CLI and autonomous watcher detector/prover mounting;
  catalogue, manifests, re-pins, and proof-thread topology are registered.
- The interactive validation-trace family; the witness-set twin family
  (design §9 Q4); the D-S10 output-well-formedness overlap accounting
  (recorded in the registered identity per design §9 Q3).
- Enabling autonomous proving in any deployed watcher, and its wallet
  custody / policy defaults (the architecture ships per §4.3; enablement
  is owner configuration, §10 Q5).
- Published-chunk publication of scan *windows* (§2.3 — a transport
  optimization only; the step-01 published-chunk carriage IS in scope,
  Q4).
- Future fee-price re-pinning after a consensus/deployment identity change.
- GOAL_PROGRESS ledger rows (owner may want one for this plan's landing).
