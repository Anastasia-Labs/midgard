# Native-script decoding fault: offchain implementation plan (v1)

Plan date: 2026-08-25. Branch: `plan/decoding-offchain-635` (off
`wave/decoding-thread-635` at `db83dd31`). Issues: #635 (family), #633
(originating divergence). This is a PLANNING document only: it implements
nothing, registers nothing, and deploys nothing. It elaborates the offchain /
registration surface that
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

Where this plan uncovers a genuine decision, it records the decision as an
owner question in §10 and takes no position beyond presenting the branches.

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

The design deliberately names no id (design §8, §9 Q7 — ruled: allocated in
the registration wave only, after the drift is reconciled). The current
facts the registration wave will find:

- On-chain catalogue: eleven categories `00000000`–`0000000a`; append
  index 11 = `0000000b` is RESERVED for the fabricated-deposit family
  (#617): `FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1 = "0000000b"`
  (`demo/midgard-sdk/src/fraud-proof/fabricated-deposit-v1.ts:59`) — a
  hard-coded SDK constant for a family that is itself not yet registered.
- `demo/midgard-sdk/src/fraud-proof/catalogue.ts:26-37` holds **11**
  positional categories. `docs/fault-proofs/catalogue-status.md:13-14,106`
  still says "exactly 8" — the doc is stale, not the code; the "8 vs 11"
  drift cited in design §8 has since closed. The registration wave should
  correct `catalogue-status.md` in passing.
- 20 family directories exist under
  `onchain/aiken/validators/fraud-proofs/` against the 11 registered
  categories; this family is the newest of the unregistered set.

Under the append discipline this family takes the next free index after
every standing reservation — `0000000c` if fabricated-deposit's
`0000000b` reservation stands and nothing else is reserved ahead of it —
but the id is **allocated by the registration wave, not by this plan**
(§10 Q2 asks the owner to confirm the reservation ordering, since two
unregistered families claiming "next free" is a live collision hazard).

### 2.2 What registration touches (the positional/pinned surfaces)

Registering the family is an append to every positional list plus a re-pin
of every derived root. The complete checklist:

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
| Test pins | §8.4 below |

The catalogue is init-time-immutable (spending validator always fails;
D-S13, `docs/fault-proofs/catalogue-status.md:211-214`): registration is a
**fresh genesis-level deployment**, never an upgrade of a live one. The MPF
insert itself is mechanical once the id exists —
`encodeFraudProofCatalogueKey/Value` (`initialization.ts:92-108`) maps the
4-byte BE index to the step-01 script hash, and
`buildFraudProofCatalogueDeploymentInfo` derives the root and per-category
membership proofs the Init builder consumes.

### 2.3 Script deployment: inline attach, not reference scripts

The established pattern does **not** deploy per-family step validators as
reference scripts. `nodeRuntimeReferenceScriptTargets`
(`demo/midgard-node/src/transactions/reference-scripts.ts:1124-1285`)
publishes the shared machinery (hub-oracle, catalogue mint, membership-proof
withdrawal, the six validation-trace scripts, …); family submitters attach
their spending validator inline (`attach.SpendingValidator`), which is why
`inspect-contracts.test.ts` asserts `standaloneScriptBytes`,
`withinL1TransactionByteEnvelopeNecessaryCondition` and
`l1SpendingScriptEnvelopeNecessaryCondition` per step.

This family's step-03 is the largest working step the fault-proof surface
has ever shipped, and its `Scan` transactions additionally carry up to two
4,095-byte chunks, the control bytes, and frame witnesses. **Whether the
inline-attach pattern fits inside the 16,384-byte L1 envelope for step-03's
worst-case Scan transaction is a measurement this plan requires before the
builders are written** (§8.2 item 1): build the blueprint into scratch,
apply parameters, and run the same envelope-fit arithmetic the
inspect-contracts assertions encode. Two outcomes:

- Fits: follow the pattern, inline attach, nothing new.
- Does not fit: the family needs a deviation — either reference-scripting
  the step-03 validator (a new `referenceScriptTargets` class for
  fault-proof steps) or shrinking the window transport (single-chunk
  windows where the margin allows). That deviation is not this plan's to
  choose: §10 Q3.

Note the published-chunk transport (#545) exists only for the step-01
membership proof (`NativeTxInclusionCarriage`), not for step-03 scan
windows; the design keeps scan-window publication as a permitted transport
optimization that "the security argument never rests on" (design §5(c)).
V1 of the builders implements the redeemer-carried forms only, with the
published-chunk option recorded as follow-up (§10 Q4).

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
   check `input_index < |field(source_kind)|`. An out-of-domain pair makes
   direction B unprovable on-chain (the stride read aborts, §7.2) — the
   detector must classify such a leaf as the B-completeness corner and NOT
   start a thread (§7.2, §10 Q1).
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
detector runs during block replay: for every accepted transaction, for
every resolved outpoint (spend inputs then reference inputs), if the
resolved descriptor carries `reference_script_language == 0`, run the TS
scan twin over the committed item bytes; any refusal (malformed /
node-limit / depth-limit) is a direction-A fault, and the prover-chosen
pair is that outpoint's `(source_kind, ordinal)`. When several outpoints
refuse, any one suffices; choose the cheapest (fewest nodes before the
refusal) to minimize the thread length.

This sweep adds scan work proportional to the number of resolved tag-0
reference scripts per block. The scan twin is linear in payload bytes with
trivial constants, so the expected cost is small, but it is standing
watcher work that did not exist before; §10 Q6 asks the owner to confirm
it belongs in the watcher's default replay path (recommended) rather than
behind a flag.

### 3.4 Detection output and routing

Following the established shape (observer watcher, manual prover): the
detector emits a **finding record** — direction, source kind, event key
(`tx_id` or serialised `TxOrderId`), header hash, the accused/chosen pair,
the reason class (direction B), the descriptor fields, and the provability
class from §3.2/3.3 — into the watcher's journal/log surface, alongside a
ready-to-run CLI invocation. Driving the thread remains operator-initiated
through the `midgard-fault-proofs` CLI (§4.3). No autonomous prover is
introduced by this plan (that would be new policy, not pattern-following;
flagged as future work, not an owner question).

The watcher's `proof-thread-indexer` gains a `families[]` policy entry
(category id, step script hashes) at registration so third-party threads of
this family are indexed like every other family's.

---

## 4. New offchain modules

### 4.1 SDK (`demo/midgard-sdk`)

One new module `src/fraud-proof/native-script-decoding-v1.ts`, mirroring
the unregistered-family template (`fabricated-deposit-v1.ts`):

- `NATIVE_SCRIPT_DECODING_VIOLATION_ID_V1` and — at registration time only
  — the category-id constant; until then the module exports the asset-name
  helper parameterized on the id:
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
- `src/fraud-proof/contracts.ts` additions land **at registration** (title
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
    reference-script commitment domain the descriptor pins.
- **`scan-plan-v1.ts`** — the scan-loop planner (§5).
- **Submitters** `submit-native-script-decoding-init.ts` and
  `…-step-01.ts` … `…-step-04.ts`, following the per-step submitter
  pattern: each an independent Effect that builds one L1 transaction,
  attaches the step validator, and returns
  `nextThreadOutRef = "txHash#index"`. Init mirrors `submit-init.ts`
  (asset name `categoryId ‖ headerHash` `:537`, first-step datum
  `{ fraud_prover: signer.paymentKeyHash, data: null }` `:551-557`, Init
  mint redeemer with the catalogue membership proof and the four reference
  indices `:576-609`, plus the PHAS zero-withdrawal `:613-637`).
  CLI verbs are added to `bin.ts` **at registration**, matching the
  fabricated-deposit precedent (modules land first, verbs when the
  category exists).

### 4.3 Thread driver

No existing family loops, so nothing in the runtime assumes a self-loop;
the step-03 loop needs a driver above the per-step submitters:

`drive-native-script-decoding-thread.ts` — given a finding record (§3.4)
and a wallet: Init → step-01 → step-02 → BindOutpoint → (Scan)* →
Verdict → step-04, submitting each transaction, awaiting confirmation,
and feeding `nextThreadOutRef` forward. The driver is resumable (§7.1):
started against an existing thread UTxO it reads the on-chain
`StepDatum`, identifies the step and (for step-03) the machine position,
and continues. The driver is a convenience over the submitters, not a
replacement — every step remains individually drivable by CLI, like every
other family.

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
(direction A) and the non-tag-0 descriptor contradiction (direction B)
close at `BindOutpoint`, giving the minimal thread
Init → 01 → 02 → bind → 04 (five transactions).

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
  full step transaction — an assumption to restate at registration, per
  design §9 Q8, not a pin) that is **≈480–510 ADA**, under 0.7% of the
  75,000-ADA `fraud_prover_reward` production profile.
- **Typical case:** committed native scripts are overwhelmingly small; a
  signature-node or few-node script closes in a handful of primitive steps,
  so the common thread is 6–8 transactions (Init, 01, 02, bind, ≤1–2
  Scan, Verdict, 04) ≈ 10 ADA. The bind-time short circuits are 5.
- **Pacing:** the thread is a single linear UTxO — strictly one
  transaction per block per thread (~20s), so the worst case runs ≈2 hours
  serial, comfortably inside the half-of-seven-days maturity fit
  (`architecture.md:283-286`). The driver (§4.3) submits, awaits
  confirmation, then builds the next transaction from the confirmed
  `nextThreadOutRef`; same-block chaining of unconfirmed steps is a
  possible optimization deliberately out of scope for v1 (rollback of a
  chained prefix would strand the suffix).
- **Funding and retry:** the prover wallet funds fees, the thread UTxO's
  min-ADA, and collateral; the worst-case budget (~510 ADA + margin)
  should be checked by the driver up front against the wallet balance,
  with the finding record carrying the estimate so the operator sees the
  cost before starting. Every submitter is idempotent-by-reconstruction:
  on timeout/rollback the driver re-queries the thread UTxO by asset name
  and rebuilds the next transaction from the on-chain state — no local
  state is authoritative (§7.1).

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

### 7.2 The B-completeness corner (OPEN — owner question Q1)

If a forced leaf's decoding-arm reason names a pair whose `input_index` is
outside the named field's cardinality, the on-chain bind can never succeed
(`spend_input_at` aborts past the committed collection; §7.3
abort-never-clamp), so direction B is **unprovable for exactly the leaves
whose accusation is most absurd**. Whether this stays stall-and-cancel or
gains a closing arm is an open owner decision; the plan presents both
branches' offchain consequences without deciding:

- **Branch (a) — stays stall-and-cancel (no onchain change):** the
  detector's domain pre-check (§3.2 step 3) becomes load-bearing: it must
  run before Init so no thread is ever started on an out-of-domain pair
  (a started one is min-ADA + fees lost to a guaranteed cancel). The
  finding record carries the corner classification; the wrongful
  rejection itself stays unremedied by this family and the residual falls
  where design §7.6 places it. Offchain cost: one pre-check, no new
  builder surface.
- **Branch (b) — a closing arm is added (onchain change, new deployment
  class):** step-03 (or -02) would gain an arm proving
  `input_index ≥ |field(source_kind)|` from the committed bytes (the
  field-opening door exposes the item count), closing the thread as a
  direction-B conviction — the accusation names a subject the machine
  could never have resolved. Offchain consequences: the evidence module
  gains a cardinality-proof path, the planner gains a fourth short
  circuit, the detector routes the corner to proving instead of stalling,
  and the family's validators change (new hashes, new blueprint, new
  pins) — which is why the decision is format/deployment-scoped and not
  this plan's to make.

The builders are structured so branch (b), if ruled, adds a module rather
than reshaping one: detection classification (§3.2) already separates the
corner; only its disposition changes.

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

### 8.1 Where and how

- Emulator end-to-end tests live in `demo/midgard-fault-proofs/tests/`, in
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

1. **Envelope-fit measurement (before builders):** blueprint built into
   scratch with the pinned fork, parameters applied, serialized script
   sizes + worst-case Scan transaction arithmetic against the 16,384-byte
   envelope (§2.3). Outcome gates the deployment-shape decision (Q3).
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
4. **Emulator end-to-end, direction A (Normal source):** Init → 01
   (redeemer-carried inclusion) → 02 → bind → Scan* → Verdict → 04 on a
   malformed multi-chunk payload; assert the fraud-proof token mints and
   the thread NFT burns.
5. **Emulator end-to-end, direction B (forced source):** a forced leaf
   with `ResolvedReferenceScriptMalformed` over a canonical payload —
   full scan to terminal; plus the descriptor-contradiction short circuit
   (non-tag-0) and the direction-A forced-acceptance variant
   (`ForcedTxValid` leaf, resolved malformed payload).
6. **Negative/abort coverage at the emulator level:** cancel at each
   step; a Scan with a stale control (replay) refused; a substituted
   chunk refused; out-of-domain pair aborts bind (the §7.2 corner's
   on-chain half); resume mid-loop after a simulated crash.

### 8.3 What lands at registration (re-pins)

Registration — not the builder wave — moves the pinned surfaces, and every
re-pin follows the provenance convention (append an entry recording
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
   `wave/decoding-thread-635` (`53b87ff9`, `db83dd31`). Blueprint
   regeneration and the ABI/validator-count evidence ride that
   integration, not this plan.
3. **Builder wave (this plan's implementation, pre-registration):** SDK
   schemas (§4.1 minus the contracts.ts registration parts), twins and
   planner (§5), evidence module and submitters (§4.2), driver (§4.3),
   suites §8.2(1–6) under the extra-category harness. Deliverable is
   green emulator end-to-ends in both directions with zero pin movement.
   Item §8.2(1) (envelope fit) runs first and, if it fails, blocks the
   submitter shape on Q3.
4. **Registration wave (separate, owner-scheduled):** category-id
   allocation (after the reservation ordering is confirmed — Q2),
   catalogue/manifest/union appends (§2.2), CLI verbs, watcher
   `families[]` policy entry, re-pins (§8.3), `catalogue-status.md`
   correction, and the fresh genesis-level deployment (D-S13).
5. **Blocked on outstanding owner rulings:** the B-completeness corner
   disposition (Q1) blocks only the corner's routing (branch (a) is the
   default behavior of the planned code; branch (b) adds onchain surface
   first); Q2–Q6 block the specific items that name them, none blocks
   starting step 3.

---

## 10. Owner questions register

- **Q1 — B-completeness corner (carried from the wave report; OPEN).** A
  direction-B accusation whose pair is out of the subject's field domain
  is on-chain unprovable: stall-and-cancel (status quo) or a closing arm
  (onchain change)? §7.2 presents both branches' offchain consequences;
  the builders are shaped so either ruling is additive.
- **Q2 — Category-id reservation ordering.** Fabricated-deposit hard-codes
  the reserved `0000000b` while itself unregistered
  (`fabricated-deposit-v1.ts:59`); this family would take `0000000c` if
  that reservation stands. Confirm the append order (and whether any
  other unregistered family holds a reservation) before any id constant
  is written, and note `catalogue-status.md`'s category count is stale
  (11 registered, not 8).
- **Q3 — Step-validator deployment shape, contingent on measurement.** The
  pattern is inline attach (no per-family reference scripts). If §8.2(1)
  shows step-03's worst-case Scan transaction cannot fit the 16,384-byte
  envelope with the script inline, the family needs a deviation
  (reference-scripting the step validators, or constraining window
  transport). Only reached if the measurement fails; the measurement
  itself is unconditional.
- **Q4 — #545 published-chunk carriage scope for v1.** The design keeps
  published-chunk transport optional. Plan v1 implements redeemer-carried
  forms only (step-01 inclusion and step-03 windows inline). Confirm
  deferring the published-chunk transport is acceptable for the family's
  first registration, or name it in-scope.
- **Q5 — Detection disposition.** The plan follows the established
  observer-watcher/manual-prover split: the watcher emits finding records
  and the operator drives the CLI. Confirm this family does not warrant
  departing from that (an autonomous prover would be new policy with
  wallet custody implications).
- **Q6 — Direction-A sweep placement.** Scanning every accepted
  transaction's resolved tag-0 reference scripts during block replay is
  new standing watcher work (small per-block, unbounded in aggregate).
  Confirm it runs in the default replay path rather than behind a flag.

Not questions (already ruled or repaired): the pair ruling (2026-08-25),
the `tx_order_id` ByteArray type repair (flagged in the wave report), the
pinned-rates supersession of design §6, catalogue-id allocation deferral
to the registration wave (design §9 Q7), and the fee restatement at
registration (design §9 Q8).

---

## 11. Out of scope

- Any onchain change: validators, engine, formats, pins, exec ledgers,
  and the #640 wave itself.
- The registration wave's execution (id allocation, appends, re-pins,
  genesis deployment) — planned here (§2, §8.3), executed separately.
- The interactive validation-trace family; the witness-set twin family
  (design §9 Q4); the D-S10 output-well-formedness overlap accounting
  (recorded at registration per design §9 Q3).
- Autonomous prover orchestration beyond the resumable driver (§4.3).
- Published-chunk transport for scan windows (§2.3, Q4).
- Fee-price re-pinning (registration-time, design §9 Q8).
- GOAL_PROGRESS ledger rows (owner may want one for this plan's landing).
