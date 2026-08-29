# Native-script decoding fault: standalone computation-thread family (design v1)

> **Registration update (2026-08-26):** the designed family is registered as
> `nativeScriptDecoding` at `0000000d`. Its six-validator topology and all six
> mandatory authenticated reference scripts are part of canonical deployment
> identity. Watcher topology registration does not mount the autonomous
> detector/prover. Adoption is a fresh genesis/redeployment with no migration
> or compatibility path.

Audit date: 2026-08-24. Branch: `wave/lane-o`. Issue: #633, direction (d).
Revised same day to incorporate the owner rulings of 2026-08-24: the
**Rejected ⇒ Forced** invariant governs the rejection commitment (the thread
binds wrongful rejections to the `ForcedInclusionTxV1` leaf under
`header.forced_transactions_root`, not to the validation-machine descriptor),
the which-outpoint amendment moves onto that leaf in place (pending a future
format-revision wave), and both directions shed the machine-state preimages.
Decisions below cited "ruled 2026-08-24" are settled; this document implements
them. Revised again the same day to adopt the **47-arm rejection-reason
catalogue** (`docs/fault-proofs/rejection-reason-catalogue-v1.md`, committed
on this branch) as the normative arm inventory for `RejectionReasonV1`:
§2.4.1 now defers to it instead of restating a reason type of its own.

This document is the architecture and specification for a standalone
single-party fault-proof computation-thread family covering the native-script
decoding/canonicity fault. The family is implemented on-chain and off-chain;
nothing in this document re-opens the decisions
recorded in `docs/fault-proofs/architecture.md` §2 ("Application: native-script
structural canonicity", recorded 2026-08-24). Where this document depends on a
commitment that does not exist yet, the dependency is marked **OPEN** rather
than assumed. Where it needs a measurement that requires running `aiken`, the
need is recorded (this design was produced under a no-build constraint) and the
number is marked **derived-from-pinned-rates** against the pinned execution
ledger `onchain/aiken/scripts/native-script-scan-exec-ledger-v1.json`.

All `file:line` anchors are against the worktree state at the audit date
(content commit lineage of `f1c8f321`).

---

## 1. Architecture overview

### 1.1 Placement in the catalogue

The family is a **single-party computation thread that faults the header
directly**, in the sense of `docs/fault-proofs/architecture.md` §2: the prover
walks a deterministic recomputation alone, across as many L1 transactions as
the budget requires, and the terminal step mints the permanent `fraud_proof`
token that authorizes `RemoveFaultyBlockHeader` on the state queue. The
interactive validation-trace machine is **never involved** — that is the
settled decision this design implements, driven by the measured 15.3–16.2M
mem/tx cost of the embedded interactive step (above both the 13.2M GOAL_SPEC
§3.3 basis and the 14M L1 protocol cap), of which ~13–14M is the interactive
control open/close envelope and only ~1.9M is the scan itself
(`architecture.md:89-109`). Extracting the scan into its own thread removes
the envelope, not the work.

Working name of the family: **`native-script-decoding-fault`**. Its catalogue
category id is the next free append index; see §8 for why the id cannot be
allocated by this document.

### 1.2 One-sentence fault statements

The governing invariant (ruled 2026-08-24) is **Rejected ⇒ Forced**: a
Normal-source transaction's validation claim must be `Accepted`
(`source_binding_is_exact`'s Normal arm,
`onchain/aiken/lib/midgard/validation-claim-v1.ak:311-317`, the `Accepted`
requirement at `:315`; the forced twin's verdict coupling
`forced_verdict_matches` at `:204-212`), so every legitimate rejection lives
as a `ForcedInclusionTxV1` leaf under `header.forced_transactions_root`
(root field `onchain/aiken/lib/midgard/ledger-state.ak:62`; dict
`ForcedTransactions` at `:442-446`; leaf type at `:540-544`). That leaf —
not the validation-machine descriptor — is the rejection commitment this
thread binds to.

The family covers **both** fault directions ruled in scope for #633:

- **Direction A — wrongful acceptance.** The header commits an acceptance
  claim for a transaction — for a **Normal** source, a `transactions_root`
  leaf whose own validity field claims acceptance
  (`MidgardTxCompact.validity == TxIsValid`, `ledger-state.ak:473-477`; the
  scalar `validity_code` embedded in the leaf's compact CBOR,
  `onchain/aiken/lib/midgard/fraud-proofs/native-tx/compact.ak:291-301`);
  for a **Forced** source, a `forced_transactions_root` leaf whose verdict
  is `ForcedTxValid` (§2.4 target format; today
  `operator_validity == TxIsValid`, `ledger-state.ak:540-544`) — yet at
  least one output that transaction resolves (spend or reference input)
  carries a tag-0 reference-script payload whose bytes are not a canonical
  native script under the frozen scan semantics — bytes the same header's
  `prev_utxos_root` itself commits. The source leaf's validity is the
  binding (follow-up ruling 2026-08-24): the format wave makes it the
  authoritative per-tx verdict (§2.4.3), and direction A is gated on that
  wave exactly as direction B is. **No descriptor is opened in either
  direction, and each proof opens exactly one source root** (§2.1).
- **Direction B — wrongful rejection.** The header commits, under its
  `forced_transactions_root`, a forced leaf whose operator verdict is
  `ForcedTxInvalid` with one of the three scan-borne resolved-outpoint
  reasons — `ResolvedReferenceScriptMalformed`,
  `ResolvedReferenceScriptNodeLimit`, `ResolvedReferenceScriptDepthLimit`
  (§2.4's target leaf format, **pending-format-wave**; arm names per the
  normative catalogue) — charged to a named resolved outpoint, yet the
  payload at exactly that outpoint is either not a tag-0 native reference
  script at all or scans to a canonical terminal.

**Explicit carve-out (ruled 2026-08-24; narrowed by the catalogue):** exactly
**one** reason arm is not refutable by any single-party family —
`PlutusExecutionFailed { execution_index }`, the genuine CEK verdict, which
no committed-content recomputation thread can adjudicate (§7.6, §9 Q1
residuals). The legacy `E_PLUTUS_SCRIPT_INVALID` code's other half,
`ReceivePurposePlutusV3Forbidden { execution_index }` (the static
V3-for-receive selection rejection,
`onchain/aiken/lib/midgard/validation-machine-v1.ak:15193-15201`), is a
static language/purpose fact and **is** single-party refutable — the
catalogue's split of that code widens single-party coverage to 46 of 47
arms (catalogue §2 item 7, §3).

The scan semantics are frozen: 16,384-node and 16,384-depth bounds, and the
same accept/reject verdicts as `native_script_scan_v1`
(`onchain/aiken/lib/midgard/native-script-scan-v1.ak:24-26`), per the #633
ruling and `docs/spec/midgard-tx.md` §5.5 (`docs/spec/midgard-tx.md:377-404`,
the tag-0 rule under which `decode_canonical_output` refuses a non-canonical
tag-0 payload).

### 1.3 Actors

- **Fraud prover** (single party): initializes the thread with the
  `computation-thread` mint policy's Init (which requires their signature and
  catalogue membership,
  `onchain/aiken/validators/computation-thread.ak:23-130`), drives every
  Continue step, and collects the `fraud_prover_reward` at removal.
  Challengers are self-selecting and permissionless
  (`architecture.md:264-289`).
- **Operator being faulted**: passive; their bond funds the reward.
- **Anyone else**: may advance a Continue step (steps are signature-free and
  transition-validated, see §7.1) but cannot cancel (Cancel is prover-signed,
  `onchain/aiken/lib/midgard/fraud-proofs/common.ak:437-481`) and gains
  nothing by advancing honestly.

### 1.4 End-to-end lifecycle

Mirrors the double-spend family
(`onchain/aiken/validators/fraud-proofs/double-spend/step-01.ak` …
`step-04.ak`) and the generic lifecycle in `architecture.md` §3
(`architecture.md:123-177`):

1. **Detect** (off-chain): the watcher recomputes verdicts for a committed
   header and finds a transaction whose committed verdict contradicts the
   frozen scan on the committed pre-state bytes.
2. **Evidence assembly** (off-chain): collect the counted-root membership
   witness for the faulted event's **one** source leaf (Normal transaction
   leaf or forced leaf, by source kind — §2.1's one-root-per-proof
   property), the event-to-step and transition-step leaves that source the
   pre-state root, the ledger-trie membership proof for the accused
   resolved outpoint, and the reference-script payload bytes with their
   chunk proofs. No machine-state preimages and no validation-trace
   descriptors are needed (rulings 2026-08-24; §2.1).
3. **Init**: mint one computation-thread unit named
   `category_id(4B) ‖ header_hash(28B)`
   (`onchain/aiken/validators/computation-thread.ak:109-115`) at the step-01
   address with `StepDatum { fraud_prover, data: None }`.
4. **Steps**: step-01 binds a Normal-source transaction and its leaf's
   acceptance claim to the header (direction A, Normal) or records
   direction and source kind (forced-source threads); step-02 binds the
   forced leaf's verdict where one is involved — `ForcedTxInvalid` with a
   scan-borne reason for direction B, `ForcedTxValid` for direction A's
   forced arm — and sources the pre-state root from the transition trace;
   OpenSubject authenticates and commits the accused outpoint,
   BindDescriptor authenticates its descriptor, and AdvanceOrClose scans its
   payload bytes under the pinned per-node budget for as many L1 transactions
   as needed; step-04 concludes via `common.finalize`
   (`onchain/aiken/lib/midgard/fraud-proofs/common.ak:579-673`), minting the
   permanent `fraud_proof` token at the fraud-proof address.
5. **Removal**: the `fraud_proof` token authorizes state-queue
   `RemoveFaultyBlockHeader` within the seven-day maturity window; §3.3's fit
   rule binds the whole proof against **half** of that window
   (`architecture.md:283-286`).

A duplicate Init by a second prover mints a second unit of the _same_ asset
name — benign under the recorded analysis (`architecture.md:159-164`); see
§7.3.

---

## 2. Fault statement bindings

Every claim the thread proves is anchored to a commitment the faulted header
itself carries. `HeaderV1` is at
`onchain/aiken/lib/midgard/ledger-state.ak:58-84`. This section was rewritten
for the 2026-08-24 rulings: the rejection commitment is the forced-inclusion
leaf, the pre-state root comes from the transition trace, and no machine-state
preimage is ever opened.

### 2.1 Commitments used, per direction

Both directions:

- **`transition_trace_root` / `transition_step_count` and `event_to_step_root`
  / `total_event_count` — the pre-state root source.** The per-event pre-state
  ledger root is committed by the transition trace, not (only) by the
  validation machine: `TransitionStep` carries `pre_utxos_root` and
  `post_utxos_root` (`onchain/aiken/lib/midgard/ledger-state.ak:571-578`),
  and the claim machinery pins the machine's `prior_ledger_root` to it
  (`onchain/aiken/lib/midgard/validation-claim-v1.ak:407`). The thread
  performs the same two counted-root openings the claim machinery already
  defines, with no descriptor and no state preimage in the chain:
  1. `event_to_step_root` at the event key (by source kind: Normal:
     `L2TransactionEventKey { tx_id }`; Forced:
     `ForcedTransactionEventKey { tx_order_id }` —
     `ledger-state.ak:553-558`) yields `EventToStepValue { step_index,
phase }` (`ledger-state.ak:560-563`), opened exactly as
     `verify_event_to_step_membership` (`validation-claim-v1.ak:187-201`;
     domain `EventToStepRootDomain`, count `header.total_event_count`).
  2. `transition_trace_root` at that `step_index` yields the
     `TransitionStep`, opened exactly as `verify_transition_step_membership`
     (`validation-claim-v1.ak:164-185`; domain `TransitionTraceRootDomain`,
     count `header.transition_step_count`, with `key == value.step_index`
     and the schema-version check).
     Cross-checks mirrored from `committed_claim_structure_is_valid`
     (`validation-claim-v1.ak:357-361`): the step's `event_key` equals the
     opened event key, and `phase == phase_for_event_key(event_key)`
     (`validation-claim-v1.ak:320-330`). The thread's `prior_ledger_root` is
     the opened step's `pre_utxos_root`. (For a Rejected forced event the claim
     machinery additionally forces `pre_utxos_root == post_utxos_root`,
     `validation-claim-v1.ak:429-430` — consistent with a rejection applying no
     delta.)
- **Ledger trie (pre-state)**: for a resolved outpoint K, membership of the
  leaf `key = cbor(K)`, `value = cbor(LedgerOutputCommitmentV1)` under
  `prior_ledger_root` — the exact check the validation machine itself performs
  (`onchain/aiken/lib/midgard/validation-machine-v1.ak:6398-6407`). The
  descriptor value (`onchain/aiken/lib/midgard/ledger-output-commitment-v1.ak:31-48`)
  carries the fields the scan needs: `reference_script_language` (-1 / 0 / 3 /
  128), `reference_script_total_length` (the byte offset lives in the
  output-scan control, `output_scan.reference_script_offset`,
  `ledger-output-proof-v1.ak:109-110`, not in this descriptor), and the
  32-byte `item_commitment` chunk commitment over the full output bytes
  (4095-byte chunks, `onchain/aiken/lib/midgard/bounded-item-v1.ak:12`).
- **The resolved-outpoint coordinate system.** Ordinals over resolved
  outpoints are in **field order**: spend inputs (field 0) in committed
  order, then reference inputs (field 1) in committed order — ordinal ∈
  `[0, spend_count + reference_count)` (ruled 2026-08-24; field indices at
  `onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.ak:102-106`, fixed
  38-byte stride reads via `native_tx_machine_walk_v1.spend_input_at`,
  `onchain/aiken/lib/midgard/native-tx-machine-walk-v1.ak:532`). This is
  deliberately the positionally-openable coordinate: the §8.8 stride read _is_
  the ordinal's semantics. The §2.4 leaf arms present the **same coordinate
  as a pair** `{ source_kind, input_index }` — catalogue convention:
  `source_kind` 0 = spend (field 0), 1 = reference (field 1), `input_index`
  the item ordinal within that field
  (`rejection-reason-catalogue-v1.md` §5, coordinate conventions) — which
  flattens injectively onto the thread's cursor as `input_index` (spend) /
  `spend_count + input_index` (reference). Note honestly that the machine's
  internal resolution-schedule _hash_ sorts scheduled inputs by encoded key
  (`transaction_resolution_schedule_hash`,
  `validation-machine-v1.ak:859-885`; comparator at `:852-857`) — that sorted
  position is a different, non-positionally-openable ordering and is **not**
  the leaf coordinate.

**One root per proof.** The two validation sources open **disjoint** roots:
the claim machinery's Normal arm opens `header.transactions_root` keyed by
`tx_id` (`validation-claim-v1.ak:254`), its Forced arm opens
`header.forced_transactions_root` keyed by `tx_order_id` (`:233`); forced
transactions do **not** appear in `transactions_root`, and a forced leaf
carries its transaction's full compact bytes — inline `validity_code`
included — in its own `source.compact_cbor`. Every proof of this family
therefore opens exactly one source root, chosen by the faulted event's
source kind. Direction A covers **both** kinds (a forced transaction can be
wrongfully accepted too); direction B is forced-only by the Rejected ⇒
Forced invariant.

Direction A additionally, for a **Normal**-source transaction:

- **`transactions_root` / `l2_transaction_count` — the acceptance
  commitment** (counted, domain-tagged; `commit_counted_root` at
  `onchain/aiken/lib/midgard/transition-trace.ak:67-81`): step-01 proves the
  faulted L2 transaction T is committed by the header, via
  `verify_native_tx_in_state_queue_node_with`
  (`onchain/aiken/lib/midgard/fraud-proofs/common.ak:792-847`; counted-root
  authentication at `common.ak:831-836`), whose returned view exposes the
  decoded compact — including T's own scalar `validity_code`
  (`onchain/aiken/lib/midgard/fraud-proofs/native-tx/compact.ak:291-301`,
  `:381-390`; type-level `MidgardTxCompact.validity` at
  `ledger-state.ak:473-477`; the V1 leaf wrapper is `L2TransactionSourceV1`,
  `ledger-state.ak:532-536`). Direction A requires `validity_code ==
TxIsValid` (code 0, `codec.ak:31-46`) **on this leaf, full stop** — that
  field is the acceptance claim the fault contradicts (follow-up ruling
  2026-08-24). Its authority is established by the §2.4.3 authoritativeness
  predicates, which ride the same format wave direction B is gated on; the
  thread family ships whole when the wave lands. **No
  `validation_traces_root` opening exists anywhere in this design**; the
  descriptor survives only in historical notes and the frozen-format
  `rejection_code_of` bridge (§2.4.2).
- T's resolved-outpoint sets, opened from T's own committed bytes through the
  §8.8 field-opening door (fields 0 and 1, coordinate system above).

Direction A additionally, for a **Forced**-source transaction:

- **`forced_transactions_root` / `forced_transaction_count` — the acceptance
  commitment.** The same forced-leaf opening direction B uses (below), but
  the verdict match is `ForcedTxValid` (§2.4 target format; today
  `operator_validity == TxIsValid`). The leaf-internal §2.4.3(e) predicate
  makes that verdict agree with the `validity_code` inside the leaf's own
  `source.compact_cbor`, so the acceptance claim, the transaction bytes,
  and the field-opening door all come from the **same single leaf** — no
  second root is ever opened.

Direction B additionally:

- **`forced_transactions_root` / `forced_transaction_count` — the rejection
  commitment.** Step-02 opens the `ForcedInclusionTxV1` leaf at key
  `ForcedTransactionEventKey { tx_order_id }`, exactly the opening
  `verify_source_authentication` already performs
  (`validation-claim-v1.ak:215-241`: domain `ForcedTransactionsV1RootDomain`
  at `:232`, count `header.forced_transaction_count`, key check
  `membership.key == tx_order_id` at `:238`, and source-triple
  authentication via `verify_native_tx_proof_source_v1`,
  `compact.ak:493-514`). Against the §2.4 target format the thread
  pattern-matches `ForcedTxInvalid { reason }` and requires the reason to be
  one of the three scan-borne resolved-outpoint arms —
  `ResolvedReferenceScriptMalformed` / `ResolvedReferenceScriptNodeLimit` /
  `ResolvedReferenceScriptDepthLimit`, each carrying
  `{ source_kind, input_index }` (§2.4.1).
  **No descriptor opening, no terminal-state preimage, no
  `phase == ResolveInputs` check**: the constructor arm replaces the phase
  attribution entirely (ruled 2026-08-24). The superseded phase-attribution
  machinery — and why the code hash alone could not attribute (the same
  codes are emitted from CanonicalDecode, `validation-machine-v1.ak:1627`,
  `:1941`, and PhaseANativeScripts, `:4041-4069`, among ~50 sites) — is
  retained as the design _rationale_ for the per-arm subject coordinates,
  now developed in full by the catalogue's §2 ambiguity map (§2.4.1).
- The forced leaf's `source.compact_cbor` supplies the committed transaction
  bytes for the field-opening door (same §8.8 coordinate as direction A);
  `tx_id` is authenticated against them by
  `verify_native_tx_proof_source_v1`.
- Direction B scans **exactly the accused outpoint** and therefore inherits
  direction A's ~100-transaction bound (§6).

### 2.2 OPEN markers (revised 2026-08-24)

- **FOLDED (A-1 → format wave, follow-up ruling 2026-08-24): leaf-validity
  authoritativeness.** Nothing on-chain today ties the compact leaf's
  embedded `validity_code` to anything: `expect_validity_code` bounds it to
  `0..5` (`onchain/aiken/lib/midgard/fraud-proofs/native-tx/codec.ak:25-29`)
  and the claim machinery never reads it (`validation-claim-v1.ak` contains
  no `validity_code` consumer; its Normal arm constrains only the
  descriptor, `:311-317`). The ruling's disposition is **not** an interim
  descriptor binding but a specified format-wave work item: the §2.4.3
  authoritativeness predicates make the tx-leaf validity the authoritative
  per-tx verdict, and direction A binds to it, gated on the wave exactly as
  direction B is. No descriptor path exists in the thread design.
- **DISSOLVED (B-1, ruled 2026-08-24): which-outpoint commitment.** The
  amendment moved off the machine/descriptor and onto the forced leaf, edited
  in place (§2.4). The old marker — no commitment named the accused outpoint,
  forcing universal quantification over all resolved outpoints — is retained
  in §6 only as a historical note on the superseded V1-format binding.
- **DISSOLVED (B-3, ruled 2026-08-24): schedule of resolution.** The leaf
  reason names the accused outpoint's ordinal in the field-order coordinate
  system (§2.1); no schedule inference remains.
- **FOLDED (B-2 → format wave): `total_length` cap.** The descriptor
  well-formedness predicate only requires `total_length >= 0`
  (`ledger-output-commitment-v1.ak:113-133`); nothing caps a reference
  script's `total_length` at 16,384. The 16,384 cap rides the **same
  format-revision wave** as the forced-leaf amendment (ruled 2026-08-24;
  §8). **Exact conjunct (readiness-review resolution):**
  `descriptor.reference_script_total_length <= 16_384` added inside
  `reference_script_is_well_formed`
  (`ledger-output-commitment-v1.ak:90-109`), applying **only when
  `reference_script_language == 0`** (the native tag — the cap exists for
  the §2.3 scan lemma; it deliberately imposes no new size rule on Plutus
  reference scripts, languages 3/128). The field is
  `reference_script_total_length`, not the whole-output `total_length`
  (`:118`). Until it lands, the §2.3 shortcut stays conditional and the
  thread scans rather than assumes.

### 2.3 Bounds-unreachability lemma (context, not a shortcut)

The maximum reference-script payload reachable through committed L2 outputs is
16,341 bytes = 5,447 nodes (pinned ledger,
`native-script-scan-exec-ledger-v1.json`), and 5,447 < 16,384. So _within the
byte caps_ the node-limit and depth-limit codes are wrongful per se. But
until the `total_length` cap lands (folded B-2, §2.2) the thread cannot
conclude from the codes alone; it proves canonicity by scanning, and this
lemma only explains why honest blocks never charge those codes to a resolved
outpoint.

### 2.4 Target commitment format (NEW, pending-format-wave; ruled 2026-08-24)

The which-outpoint amendment is an **in-place revision of the forced leaf**
— no V2 side-by-side type: nothing is deployed, and the house rule is no
compat shims. It rides a **future format-revision wave, not the current
zero-blueprint-movement wave**; this section is the specified target format.
Today's leaf (`ledger-state.ak:540-544`) carries
`operator_validity: MidgardTxValidity`; the revision replaces that field with
a verdict sum whose collapse deliberately makes "valid with a reason" and
"invalid without one" unrepresentable:

```aiken
// NEW / pending-format-wave — in-place revision of ForcedInclusionTxV1
// (today: ledger-state.ak:540-544, field `operator_validity`)
pub type ForcedInclusionTxV1 {
  tx_id: ByteArray,
  source: NativeTxProofSourceV1,
  verdict: OperatorVerdictV1,
}

pub type OperatorVerdictV1 {
  ForcedTxValid
  ForcedTxInvalid { reason: RejectionReasonV1 }
}
```

#### 2.4.1 `RejectionReasonV1` — the fully enumerated reason type

**Normative arm inventory:**
`docs/fault-proofs/rejection-reason-catalogue-v1.md` (committed on this
branch; audited 2026-08-24 against the same machine revision, phase by
phase over every emission site). This document **defers** to the catalogue
and does not restate its 47 arms: the catalogue is the single source of
truth for the arm list, payloads, emission-site anchors, refutability
classes, and design notes. What follows is the type's _shape_ and the arms
this thread consumes.

**Shape (catalogue §5).** **47 constructors**, derived from the machine's
19 raw rejection-code constants (the `const reject_*` inventory, all in
`validation-machine-v1.ak`: `:1186-1205`, `:2128-2134`, `:2277-2278`,
`:2805-2808`, `:3450-3453`, `:6164-6169`, `:14338`) by splitting every code
emitted for more than one (reason, subject-space) pair — the catalogue's §2
ambiguity map: `E_INVALID_FIELD_TYPE` → 13 constructors,
`E_MISSING_REQUIRED_WITNESS` → 5, `E_ASSET_COUNT` /
`E_NATIVE_SCRIPT_NODE_COUNT` / `E_NATIVE_SCRIPT_DEPTH` → 4 each,
`E_NATIVE_SCRIPT_INVALID` / `E_INVALID_OUTPUT` / `E_PLUTUS_SCRIPT_INVALID`
→ 2 each; the eleven unambiguous codes map 1:1. The constructor tag **is**
the code, and each constructor carries **its own** subject coordinates
directly (`{ source_kind, input_index }`, `{ script_index }`, …).

This supersedes the earlier draft's `RejectionSubjectV1` companion sum:
with one constructor per (reason, subject-space) pair, a shared subject sum
is unnecessary, and the admissibility table it required ("which subject
arms may this code carry") collapses into the type itself — well-formedness
reduces to non-negative ordinal bounds. Payload discipline is unchanged:
name the subject, never carry the argument (catalogue design note 4 — even
knowable scan positions are arguments, not subjects). The earlier draft's
two ruled coarse choices are also superseded by finer arms: the tx-global
`E_ASSET_COUNT` arm becomes four per-crossing accumulator arms, and
`ValueNotPreserved` loses its asset-id payload and becomes tx-global (the
whole-fold terminal is the subject; catalogue §1.13, §5).

**The arms this thread consumes** (direction B, §2.1) are the three
scan-borne resolved-outpoint arms, named here exactly as the catalogue
names them (catalogue §5, ResolveInputs group; emission sites
`validation-machine-v1.ak:6464-6481`):

```aiken
// NEW / pending-format-wave — the three arms direction B refutes
// (excerpt; the full 47-constructor type is normative in
// rejection-reason-catalogue-v1.md §5)
  /// Resolved output's tag-0 reference script structurally invalid.
  ResolvedReferenceScriptMalformed { source_kind: Int, input_index: Int }
  /// That scan exceeds the 16,384-node bound.
  ResolvedReferenceScriptNodeLimit { source_kind: Int, input_index: Int }
  /// That scan exceeds the 16,384-depth bound.
  ResolvedReferenceScriptDepthLimit { source_kind: Int, input_index: Int }
```

`source_kind` is 0 = spend (field 0) / 1 = reference (field 1) and
`input_index` the item ordinal within that field — the §2.1 field-order
coordinate presented as a pair. Two neighbouring resolved-outpoint arms
are deliberately **not** in this thread's domain:
`InputSpentOutputNonCanonical { source_kind, input_index }` (the _output_
canonicity scan, `:6458-6463` — the D-S10 output-well-formedness overlap,
§9 Q3) and the own-output twins (`OutputReferenceScript*`, ScriptSources
stage 5 — the witness/own-bytes sibling concern of §9 Q4).

**Refutability totals (catalogue §3):** 46 of 47 arms are single-party
refutable — 13 in a single L1 transaction, 33 as bounded computation
threads; exactly one arm, `PlutusExecutionFailed { execution_index }`, is
interactive-domain (§1.2 carve-out). That census is the quantified form of
the Rejected ⇒ Forced invariant's enforceability. The catalogue's design
note 2 records that the subject payloads are precisely what dissolved this
document's old OPEN (B-1) and re-scoped direction B to one named outpoint.

A reason that **misattributes** the subject space — e.g. a genuinely
resolved-outpoint scan fault recorded as `WitnessNativeScriptMalformed` —
is a trace-detail fault for the interactive family, exactly as the
misattributed-phase residual was before (§7.6). The 47-way split _shrinks_
that residual: each arm carries its own single-party refutation procedure
(catalogue §3), so a wrongly-chosen arm whose named subject does not
exhibit its named fault is refutable under that arm's own procedure without
this thread's involvement.

#### 2.4.2 Consistency predicates (NEW, specified with the format)

All three ruled 2026-08-24; the descriptor and machine-state formats stay
**untouched** (frozen) — the bridge runs through the already-committed code
hash:

- **(a) `rejection_code_of(reason: RejectionReasonV1) -> ByteArray`** — a
  **total, non-injective 47 → 19 map** from constructor to the frozen `E_*`
  label bytes (the `const reject_*` values,
  `validation-machine-v1.ak:1186-1205` etc.): every arm returns exactly the
  legacy code it was split from. The normative arm-by-arm table is the
  catalogue's §5.1 — this document does not duplicate it (mechanically, a
  single `when`-expression). The bridge to the FROZEN descriptor format is
  unchanged: `hash_rejection_code(rejection_code_of(reason)) ==
descriptor.rejection_code_hash` (`hash_rejection_code` at
  `onchain/aiken/lib/midgard/validation-trace-v1.ak:239-243`, domain-tagged
  blake2b-256).
- **(b) `coarse_bucket_of(reason: RejectionReasonV1) -> MidgardTxValidity`**
  — a **total 47-arm map** into the compact-tx leaf's validity vocabulary
  (`ledger-state.ak:485-492`). The normative table is the catalogue's §5.2;
  its `FailedScript` row is honest about being a **documented lossy
  convention**, not a semantics — `MidgardTxValidity` is literally
  `// TODO`-marked (`ledger-state.ak:484`) and has no malformed-structure
  arm, so the structural family routes there by convention.
  `TxIsValid` is unreachable from this map by construction.
  **Reconciliation:** this document's earlier recommendation was to widen
  the enum with a `MalformedTx` arm; the catalogue's finding C-3
  (§4.5, design note 3) rules the other way — **retire** the five
  rejection arms of `MidgardTxValidity` from the forced leaf in the same
  wave (the full `RejectionReasonV1` subsumes them, and the arm choice is
  unadjudicated by today's `forced_verdict_matches` anyway). This design
  **adopts the catalogue's recommendation and withdraws the widening**
  (§8, §9 Q1 residuals). **Disposition of `coarse_bucket_of`
  (readiness-review resolution, 2026-08-24):** with the rejection arms
  retired in the same wave, the map's codomain degenerates to
  `TxIsInvalid`, so `coarse_bucket_of` is **not shipped as onchain code**.
  It survives only as the catalogue §5.2 documented table — the migration
  reference for off-chain consumers leaving the old six-code vocabulary,
  which are updated in the same wave. The only shipped onchain bridge is
  `rejection_code_of` (a).
- **(c) `forced_verdict_matches` extended.** Today it couples the leaf's
  coarse validity to the descriptor verdict
  (`validation-claim-v1.ak:204-212`). The extension requires, on the
  Rejected arm, that the leaf **reason** agrees with the descriptor's
  committed code hash: `verdict is ForcedTxInvalid { reason }` ⇒
  `descriptor.verdict == Rejected` and
  `hash_rejection_code(rejection_code_of(reason)) ==
descriptor.rejection_code_hash`; `ForcedTxValid` ⇒
  `descriptor.verdict == Accepted`.
- **Module hosting (readiness-review resolution, 2026-08-24).** A new
  module `onchain/aiken/lib/midgard/rejection-reason-v1.ak` hosts
  `RejectionReasonV1`, `OperatorVerdictV1`, `rejection_code_of`, and the
  **canonical `pub` definitions of the 19 `E_*` byte constants**;
  `validation-machine-v1.ak` replaces its private `const reject_*` copies
  with imports from the new module (identical byte values — the machine's
  encodings are unchanged; the wave's ABI gate verifies descriptor/machine
  byte-identity). This direction avoids an import cycle:
  `ledger-state.ak` imports the types from the new module,
  `validation-claim-v1.ak` imports `rejection_code_of`, and the new module
  imports nothing from either. If the implementer finds a cycle regardless,
  the fallback is `pub`-ing the constants in place in
  `validation-machine-v1.ak` and importing them from the new module —
  never duplicating the byte literals.

#### 2.4.3 Leaf-validity authoritativeness (NEW, same wave; follow-up ruling 2026-08-24)

The follow-up ruling replaces the earlier "interim descriptor binding"
disposition: the format wave carries two additional predicates that make the
tx-leaf validity the **authoritative per-tx verdict**, and direction A binds
to it — full stop, no descriptor path, gated on the wave like direction B.

- **(d) Normal-source authoritativeness (NEW).** The claim machinery's
  Normal arm additionally requires the committed tx leaf's embedded scalar
  to claim acceptance:
  `verified.tx_compact.validity_code == 0` (`TxIsValid`; the code map is
  `validity_from_code`, `codec.ak:31-46`). This extends the existing
  Normal ⇒ Accepted enforcement (`source_binding_is_exact`'s Normal arm,
  `validation-claim-v1.ak:311-317`): today the arm constrains only the
  descriptor; after the wave the leaf itself carries the same obligation,
  so a Normal leaf under `transactions_root` **is** an acceptance verdict.
- **(e) Forced-source authoritativeness (NEW, leaf-internal).** The claim
  machinery requires the forced leaf's verdict to agree with the embedded
  scalar of the **same leaf's own** `source.compact_cbor`, extending
  `forced_verdict_matches` (`validation-claim-v1.ak:204-212`) alongside the
  §2.4.2(c) extension: `ForcedTxValid` ⇒
  `verified.tx_compact.validity_code == 0`; `ForcedTxInvalid { reason }` ⇒
  `verified.tx_compact.validity_code ==
validity_to_code(coarse_bucket_of(reason))` (`validity_to_code`,
  `codec.ak:48`; `coarse_bucket_of`, §2.4.2(b)). This is **never a
  cross-root opening**: the two validation sources open disjoint roots
  (§2.1, `validation-claim-v1.ak:233` vs `:254`), forced transactions do
  not appear in `transactions_root`, and the forced leaf is self-contained
  — verdict and transaction bytes travel together. **Post-wave final form
  (readiness-review resolution, 2026-08-24).** The same wave retires
  `MidgardTxValidity`'s five rejection arms (catalogue C-3; §2.4.2(b), §8),
  so the normative post-wave state is: `MidgardTxValidity = TxIsValid |
TxIsInvalid`; the compact wire scalar takes exactly two values (0 =
  valid, 1 = invalid); `expect_validity_code` bounds shrink to 0..1 and
  `validity_from_code` / `validity_to_code` shrink with them
  (`codec.ak:25-56` — this changes the canonical compact bytes, in-wave
  like everything else). Predicate (e)'s final right-hand side is therefore
  the bit equality — `ForcedTxValid` ⇒ `validity_code == 0`;
  `ForcedTxInvalid { _ }` ⇒ `validity_code == 1` — with **no onchain
  composition of `coarse_bucket_of`** (whose codomain is degenerate once
  the arms retire; see §2.4.2(b)). Predicate (d) is unchanged
  (`validity_code == 0`).
- **Placement.** Both predicates run in `verify_source_authentication`
  (`validation-claim-v1.ak:215-265`), which executes on every claim via
  `committed_claim_source_is_authenticated` (`:383-392`) and **already
  decodes the compact bytes in both arms** — the Forced arm's
  `verify_native_tx_proof_source_v1` at `:219-224`, the Normal arm's at
  `:244-249` — but today consumes only `verified.version == 1` (`:239`,
  `:261`). The additions are constant-cost field equalities on
  already-decoded values: no new root opening, no new hashing, no format
  change to any leaf beyond the §2.4 revision itself. This is also the
  _soundest_ placement examined: `source_binding_is_exact` receives the
  raw membership, not the decoded view, so putting the checks there would
  re-pay the decode; no cheaper site exists because every claim already
  passes through this function.
- **Outcome.** After the wave, the per-tx verdict authority is the one leaf
  the transaction lives under — `MidgardTxCompact.validity` for Normal
  sources, `OperatorVerdictV1` (coupled to its own embedded scalar by (e))
  for Forced sources — and direction A's binding (Normal:
  `validity_code == TxIsValid`; Forced: `ForcedTxValid`) is sound, one root
  per proof. The descriptor's verdict becomes a derived commitment,
  consistency-checked by (c) through the frozen `rejection_code_hash`
  bridge, and appears nowhere in this thread's bindings.

---

## 3. Contract set

Six spending validators plus reuse of the existing generic machinery. Names
follow the double-spend family's convention.

| #   | Validator (new)                                                              | Role                                                                                                                        |
| --- | ---------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| 1   | `validators/fraud-proofs/native-script-decoding/step-01.ak`                  | Bind the faulted transaction T to the header (direction A); record the direction and pass through (direction B)             |
| 2   | `validators/fraud-proofs/native-script-decoding/step-02.ak`                  | Direction B: bind the forced leaf's verdict; both: source the pre-state root from the transition trace; branch on direction |
| 3   | `validators/fraud-proofs/native-script-decoding/step-03-open-subject.ak`     | Authenticate the accused field and commit its exact outpoint, or close an out-of-domain direction-B claim                   |
| 4   | `validators/fraud-proofs/native-script-decoding/step-03-bind-descriptor.ak`  | Authenticate the committed outpoint's ledger descriptor and initialize or close the scan                                    |
| 5   | `validators/fraud-proofs/native-script-decoding/step-03-advance-or-close.ak` | Self-looping budgeted scan; close a direction-A refusal or direction-B exact terminal                                       |
| 6   | `validators/fraud-proofs/native-script-decoding/step-04.ak`                  | Conclude: `common.finalize`, mint `fraud_proof`                                                                             |

### 3.1 Parameterization (acyclic chain)

- step-01(`step_02_hash`, `computation_thread_policy_id`, `hub_oracle`)
- step-02(`step_03_open_subject_hash`, `computation_thread_policy_id`)
- step-03-open-subject(`step_03_bind_descriptor_hash`, `step_04_hash`,
  `computation_thread_policy_id`, `field_preimage_certificate_policy_id`)
- step-03-bind-descriptor(`step_03_advance_or_close_hash`, `step_04_hash`,
  `computation_thread_policy_id`)
- step-03-advance-or-close(`step_04_hash`,
  `computation_thread_policy_id`)
- step-04(`computation_thread_policy_id`, `fraud_proof_token_policy_id`,
  `fraud_proof_token_address`)

The chain is acyclic in parameters:
01→02→OpenSubject→BindDescriptor→(AdvanceOrClose)\*→04. OpenSubject and
BindDescriptor may also close directly to step-04. AdvanceOrClose knows its
own hash from `own_out_ref` resolution, so its scan self-loop needs no
self-parameter.

### 3.2 Redeemer arms

Every step carries `ct.Cancel` via `common.cancel`
(`common.ak:437-481`), exactly as
`resolve-inputs-membership-step-semantic-v1.ak:37-45` and the double-spend
steps do. The Continue arms:

- **step-01** `Continue(BindTransaction { direction, source_kind,
carriage })` — one source root per thread (§2.1):
  - Direction A, Normal source: verbatim reuse of
    `pass_native_tx_to_next_step_carried` (`common.ak:149-252`) including
    the published-chunk carriage duality (#545), as in
    `double-spend/step-01.ak:57-94` — binds T under `transactions_root` —
    **plus** the verdict binding itself (follow-up ruling 2026-08-24): the
    returned view's decoded compact must claim acceptance,
    `native_tx_view.tx_compact.validity_code == 0` (`TxIsValid`; view type
    field at
    `onchain/aiken/lib/midgard/fraud-proofs/native-tx/types.ak:201`, code
    map `codec.ak:31-46`). The tx leaf is the verdict authority (§2.4.3);
    no descriptor is opened at any step.
  - Direction A, Forced source, and direction B: no `transactions_root`
    work — the faulted transaction lives under `forced_transactions_root`
    only and is bound at step-02 from the forced leaf itself. This arm
    records direction and source kind and passes through.
  - Output state: `{ direction, source_kind, verified_tx_id }`
    (forced-source threads: a sentinel until step-02 authenticates `tx_id`
    from the forced leaf) at step-02's address.
- **step-02** `Continue(BindVerdict { … })` — redesigned (ruled 2026-08-24):
  no machine-state preimage is opened in either direction.
  1. `common.continue` (`common.ak:501-577`) for thread-token conservation.
     All root openings below run against the header the thread NFT names —
     the header body rides the redeemer and is checked against the asset
     name's `header_hash`, the same binding Init used.
  2. **Pre-state root** (both directions): open `event_to_step_root` at the
     event key, then `transition_trace_root` at the yielded `step_index`,
     with the §2.1 cross-checks (the openings of
     `validation-claim-v1.ak:164-201`); freeze `prior_ledger_root :=
transition_step.pre_utxos_root` into the state.
  3. Direction A, Normal source (`BindVerdict { event_step_openings }`):
     nothing beyond item 2 — the acceptance verdict was already bound at
     step-01 from the tx leaf's own validity field (follow-up ruling
     2026-08-24), so this arm only sources the pre-state root and freezes
     the prover-chosen ordinal. No descriptor membership exists in the
     redeemer.
  4. Forced-source threads (`BindVerdict { forced_membership,
event_step_openings }`): open the `ForcedInclusionTxV1` leaf at
     `ForcedTransactionEventKey { tx_order_id }` (the
     `verify_source_authentication` opening,
     `validation-claim-v1.ak:215-241`), authenticate the source triple
     (`verify_native_tx_proof_source_v1`, `compact.ak:493-514`) and set
     `verified_tx_id := leaf.tx_id`; then match by direction (§2.4 target
     format, pending-format-wave):
     - Direction A, Forced source: require `verdict == ForcedTxValid` —
       the acceptance claim, from the same single leaf that carries the
       transaction bytes.
     - Direction B: pattern-match `ForcedTxInvalid { reason }` and require
       the reason to be one of `ResolvedReferenceScriptMalformed` /
       `ResolvedReferenceScriptNodeLimit` /
       `ResolvedReferenceScriptDepthLimit`, each carrying
       `{ source_kind, input_index }` (§2.4.1, arm names per the normative
       catalogue); freeze the reason's constructor tag and the flattened
       field-order cursor — `input_index` for spend (arm `source_kind` 0),
       `spend_count + input_index` for reference (arm `source_kind` 1),
       per §2.1 — into the state.
       **No descriptor opening, no terminal-state preimage, no phase check**
       — the constructor arm is the attribution.
  5. Output state: the §4 schema, cursor frozen at the accused ordinal
     (direction B) or at the prover-chosen ordinal (direction A, either
     source kind).
- **step-03 OpenSubject** `Continue { subject_field_opening, … }`: runs once
  on step-02's sentinel state. If the source kind names spend/reference and
  the ordinal is non-negative, open T's corresponding field through the
  §8.8 door and use its authenticated item count. An in-domain ordinal commits
  `blake2b_256(cbor(K))` and K's output index, then pays BindDescriptor. An
  out-of-domain subject (unknown field, negative ordinal, or ordinal at/past
  the authenticated count) closes only in direction B, paying step-04 with
  the class-0 contradiction marker.
- **step-03 BindDescriptor** `Continue { outpoint_key_cbor,
descriptor_cbor, ledger_membership_proof, first_chunk_proof, … }`: require
  the supplied canonical K bytes to hash to OpenSubject's commitment, then
  prove `mpf.has(prior_ledger_root, cbor(K), descriptor_cbor, proof)` and
  require the descriptor's output index to equal the opened one. A non-tag-0
  descriptor closes only direction B. A tag-0 descriptor authenticates chunk
  zero, freezes the item anchor, and either initializes the machine at
  AdvanceOrClose or closes a malformed wrapper only in direction A.
- **step-03 AdvanceOrClose** `Continue { control_cbor, chunk_proof,
next_chunk_proof, frames, step_budget, … }`: require the control hash and
  frozen descriptor anchor, authenticate the 1–2 adjacent chunks once, and
  run the bounded fold. A non-terminal advance self-loops. A direction-B
  advance reaching the exact canonical terminal closes to step-04 in that
  same transaction. A refusal closes only direction A with its exact class.
  A separately committed direction-B terminal may also close canonically
  with a zero-budget, windowless transition.
- **step-04** `Continue(Finalize { … })`: `common.finalize`
  (`common.ak:579-673`) exactly as `double-spend/step-04.ak:53-72`; re-check
  the carried terminal marker (direction A: refusal recorded; direction B:
  canonical terminal — or non-tag-0 descriptor contradiction — recorded on
  the accused outpoint) and mint the permanent token.

The one-shot `payload_structure_is_canonical_v1`
(`native-script-scan-v1.ak:1148-1154`) remains deliberately dead code and is
**not wired** (its own module says DO NOT WIRE); the thread composes only the
staged primitives.

### 3.3 New library code

One new library module,
`lib/midgard/fraud_proofs/native_script_decoding/engine.ak` (name
illustrative), containing:

- the §4 state type and its canonical encoder;
- `budgeted_scan_v1(control, window_bytes, max_nodes) -> control`: the thin
  fold described above. This is NEW code, deliberately _not_ a reuse of
  `ledger_output_proof_v1.step_v1` (`ledger-output-proof-v1.ak:1028-1063`)
  in a loop: `step_v1` re-authenticates its chunk window per invocation,
  which at one call per node would multiply the chunk-hash cost ~60-fold per
  transaction. The engine authenticates the window once per L1 transaction
  and steps the frozen primitives directly, matching the pushdown template's
  budgeted-run shape (`native-tx-script-pushdown-v1.ak:264-273`,
  resume-from-commitment `:643-654`).

**Measurement required (cannot be run here):** the engine's real per-node
throughput must be re-measured with `aiken` and recorded in a new pinned
execution-ledger JSON before the budget in §6 is treated as anything but
derived-from-pinned-rates.

### 3.4 Off-chain / registration surface (not in scope of this doc's commit)

Catalogue registration (MPF insert of the new category id → step-01 script
hash), `catalogue.ts` update, and watcher detection logic are follow-up work
items; see §8 and §9 Q7.

---

## 4. Thread state

The thread state is the `data: Option<Data>` of the generic
`ct.StepDatum { fraud_prover, data }`
(`onchain/aiken/lib/midgard/computation-thread.ak:4`), evolving along the
chain. Constant-size discipline follows `docs/spec/midgard-tx.md` §7 item 6
("positions, not bytes", `docs/spec/midgard-tx.md:491-493`): no unbounded
bytes ever ride the datum — payload bytes live in redeemers, authenticated
per-transaction against carried commitments.

```aiken
// step-02 output onward (the full schema; earlier steps carry prefixes)
pub type ScanThreadStateV1 {
  // -- frozen at step-02 --
  direction: Int,                    // 0 = wrongful acceptance, 1 = wrongful rejection
  source_kind: Int,                  // 0 = Normal, 1 = Forced (direction B always 1;
                                     // one source root per thread, §2.1)
  verified_tx_id: ByteArray,         // 32B; Normal: step-01's counted-root binding;
                                     // Forced: the forced leaf's authenticated tx_id
  tx_order_id: Int,                  // Forced: the forced leaf's key; -1 for Normal
  scan_reason_class: Int,            // direction B: 0 = ResolvedReferenceScriptMalformed,
                                     // 1 = ResolvedReferenceScriptNodeLimit,
                                     // 2 = ResolvedReferenceScriptDepthLimit
                                     // (the leaf reason's constructor, §2.4.1);
                                     // -1 for direction A
  prior_ledger_root: ByteArray,      // 32B, transition_step.pre_utxos_root (§2.1 opening)
  // -- the accused/chosen outpoint, frozen at step-02 --
  outpoint_cursor: Int,              // FROZEN ordinal in [0, spend_count + reference_count):
                                     // direction B: flattened from the leaf reason's
                                     // { source_kind, input_index } pair (spend/reference
                                     // kind — distinct from this state's Normal/Forced
                                     // source_kind field), per §2.1;
                                     // direction A: prover-chosen
  outpoint_key_hash: ByteArray,      // 32B blake2b_256(cbor(K)); binds OpenSubject → BindDescriptor
  reference_script_language: Int,    // from the bound descriptor: -1 | 0 | 3 | 128
  output_index: Int,
  total_length: Int,                 // descriptor.reference_script_total_length
  item_commitment: ByteArray,        // 32B; the byte-authentication anchor (§5)
  // -- inner machine --
  machine_state_hash: ByteArray,     // 32B blake2b_256(domain ‖ encode_control_v1(control)),
                                     // or the pre-bind sentinel
  // -- terminal marker --
  refusal_class: Int,                // direction A: -1 until a refusal is proven, then the class;
                                     // direction B: -1 until the contradiction is proven
                                     // (canonical terminal, or non-tag-0 descriptor)
}
```

Every field is fixed-width or a bounded `Int`; the encoded datum is
constant-size (< 300 bytes) regardless of payload size.

**Single-outpoint discipline (replaces the old cursor iteration; ruled
2026-08-24).** Both directions bind **exactly one** outpoint per thread, so
`outpoint_cursor` is frozen at step-02 and never advances: direction B's
comes from the forced leaf's reason, direction A's is prover-chosen (one
refusal contradicts `Accepted`). `OpenSubject` may only fire when
`machine_state_hash` is the pre-open sentinel, and must open the outpoint at
exactly `outpoint_cursor` (the 38-byte stride read is positional, so the
ordinal _is_ the identity — `double-spend/step-03.ak:89-92` precedent). A
prover cannot substitute an outpoint: the frozen ordinal is the key,
`outpoint_key_hash` pins K across the OpenSubject→BindDescriptor boundary, and
`item_commitment` pins the bytes (§5). `machine_state_hash` carries the inner
cursor between L1 transactions exactly as the pushdown template's
`script_digest`-protected cursor does
(`native-tx-script-pushdown-v1.ak:643-654`), and the frozen control is already
constant-size (`NativeScriptStructureControlV1`: version, stage, start_offset,
cursor, end_offset, stack_root hash-chain, stack_depth, node_count). Replays
of an old redeemer fail the hash chain; replaying an old _state_ is impossible
because the thread is a single linear UTxO (§7.4).

---

## 5. Byte authentication strategy

The scan consumes up to 16,341 bytes of reference-script payload; an L1
transaction is capped at 16,384 bytes total. The bytes therefore cannot be
"in the datum", and mostly cannot even ride a single redeemer alongside
proofs. Three options were evaluated.

**(a) The block's own chunk commitment — CHOSEN.** The ledger-trie descriptor
already commits the full output bytes as `item_commitment`, a 32-byte
commitment over 4095-byte chunks
(`ledger-output-commitment-v1.ak:31-48`; chunk size
`bounded-item-v1.ak:12`; verification `bounded-item-v1.ak:145`). The
BindDescriptor inherits it via `initial_control_v1(output_index,
total_length, item_commitment)` (`ledger-output-proof-v1.ak:279-300`), and
each `Scan` transaction authenticates a 1–2-chunk adjacent window once via
`authenticated_chunk_window` (`ledger-output-proof-v1.ak:490-534`) and steps
within it. Why chosen: **the commitment already exists in the faulted header's
own pre-state** — no init-time re-commitment ceremony, no trusted setup, no
new hashing pass; substitution is impossible because the window check chains
to `item_commitment`, which the ledger-trie membership proof chains to
`prior_ledger_root`, which the counted transition-trace openings (§2.1:
`event_to_step_root` → `transition_trace_root` → `pre_utxos_root`) chain to
the header the thread NFT names.
It is also the exact mechanism the interactive machine's own resolve-inputs
step uses (`validation-machine-v1.ak:6419-6423`), so the thread proves against
the same bytes the machine would have seen.

**(b) Full bytes + digest table at init — REJECTED.** Committing
`blake2b_256` digests of the payloads (or the payloads themselves) into the
thread datum at Init would require the Init transaction to carry the payload
bytes for hashing: a 16,341-byte payload plus the Init envelope, thread
outputs, and proofs does not fit the 16,384-byte L1 cap, so worst-case
payloads would need a multi-transaction commitment ceremony — reinventing (a)
with extra steps and a redundant commitment. It also bloats Init for every
thread even when payloads are small. Nothing (a) lacks is gained.

**(c) Reference-input parking — KEPT AS OPTIONAL CARRIAGE, NOT THE ANCHOR.**
Publishing chunk bytes as reference-input datums (the #545 published-chunk
duality, `double-spend/step-01.ak:153-243`,
`common.ak` `NativeTxInclusionCarriage`) lets a step transaction pay two
small integers and reference inputs instead of inlining bytes. This is a
_transport_ optimization, not an authentication root: the parked chunks still
verify against `item_commitment`. The design permits a published-chunk
transport for `Scan` windows mirroring #545, but the security argument never
rests on it.

---

## 6. Budget math

All numbers **derived-from-pinned-rates** from
`onchain/aiken/scripts/native-script-scan-exec-ledger-v1.json` (PINNED, never
modified). Rates: one-shot scan ≈ 197,726 mem/node (deep worst case),
≈ 174,338 mem/node (wide); ≈ 66.1M cpu/node (deep). Basis: GOAL_SPEC §3.3 —
13.2M mem / 8B cpu per L1 transaction. Max reachable payload: 16,341 bytes =
5,447 nodes.

**Per-step node throughput.** Reserve ~1.0M mem per step for the thread
envelope (token conservation via `common.continue`, datum decode/encode,
one chunk-window authentication — envelope share consistent with the pinned
ledger's 67-node basis exhaustion, which includes envelope). Usable ≈ 12.2M:

- deep-nested worst case: ⌊12,200,000 / 197,726⌋ = **61 nodes/tx**
- wide worst case: ⌊12,200,000 / 174,338⌋ = **69 nodes/tx** (~70)

CPU is non-binding: 61 × 66.1M ≈ 4.03B < 8B.

**Worst-case step count** (single maximal 5,447-node payload):

- scan steps, deep: ⌈5,447 / 61⌉ = **90 transactions**
- plus binding/structure overhead: step-01, step-02, OpenSubject,
  BindDescriptor, the closing transition, and step-04 — call it ~10 more. Both directions bind
  exactly one outpoint (ruled 2026-08-24). **Worst case ≈ 100 L1
  transactions, in either direction.**

**Fee per transaction** (mainnet price assumption, stated as an assumption,
not pinned): mem price 0.0577 lovelace/unit ⇒ 13.2M mem ≈ 761,640 lovelace;
cpu 0.0000721 ⇒ ~4.4B cpu ≈ 317,240 lovelace; size ~161.4 lovelace/byte at a
full 16KB ≈ 375,381 lovelace incl. the 155,381 base. **≈ 1.45 ADA per step
transaction.** Worst case (either direction) ≈ 100 × 1.45 ≈ **145 ADA**
(~130–150 ADA band across deep/wide mixes) — under 0.2% of the 75,000-ADA
`fraud_prover_reward` production profile (`architecture.md:264-289`).

**Direction B now inherits the same ~100-transaction bound** (ruled
2026-08-24): the forced leaf's reason names the accused outpoint, so the
thread scans exactly one payload. At 1 tx/block (~20s) that is ~33 minutes —
comfortably inside the half-of-seven-days maturity fit rule
(`architecture.md:283-286`) in both directions.

**Historical note (superseded V1-format binding).** Against the pre-ruling
leaf format — no which-outpoint commitment (old OPEN B-1) — direction B had
to universally quantify over all ≤ 296 spend + ≤ 819 reference = ≤ 1,115
resolved outpoints (`docs/spec/midgard-tx.md:363-375`), each up to 5,447
nodes ⇒ up to ~100 × 1,115 ≈ ~110,000 L1 transactions ≈ 160,000 ADA ≈ ~5.8
days of 100% chain script capacity — failing the half-maturity fit at the
adversarial extreme. That extreme is what the 2026-08-24 forced-leaf
amendment (§2.4) was ruled to eliminate; it survives here only as the
motivation record.

**Sensitivity.** Mem is the binding axis everywhere; an X% reduction in
mem/node reduces scan step count by ~X% (and fees nearly proportionally).
The engine of §3.3 avoids per-node window re-authentication precisely to keep
the realized rate at or below the pinned one-shot rate; if re-measurement
comes in materially better than 197,726 mem/node, every figure above shrinks
proportionally, but no conclusion in this document depends on it doing so.

---

## 7. Adversarial analysis

### 7.1 Griefing and third-party interference

Continue arms are signature-free but fully transition-validated: any arm
either performs the unique correct transition (and reproduces the thread
token and datum at the mandated address, enforced by `common.continue`'s
conservation checks, `common.ak:501-577`) or fails. A third party "advancing"
the thread does the prover's work for free; they cannot steer it anywhere
wrong. Cancellation is prover-signed only (`common.cancel`,
`common.ak:437-481`, checked against `StepDatum.fraud_prover`), so an
adversary cannot burn a live thread.

### 7.2 Abandonment

An abandoned thread simply sits; it holds only the prover's own min-ADA and
thread token. It proves nothing until step-04, mints nothing, and blocks
nothing — the state queue is untouched until a `fraud_proof` token exists.
The maturity window is the only clock, and it penalizes only the abandoning
prover.

### 7.3 Duplicate threads

A second Init for the same (category, header) mints a second unit of the same
asset name `category_id ‖ header_hash`
(`computation-thread.ak:109-115`). Threads are independent UTxOs; both can run
to completion; the second `fraud_proof` mint is redundant but harmless, and
removal is idempotent. This matches the recorded analysis for the existing
families (`architecture.md:159-164`). No first-past-the-post race affects
soundness; reward assignment at removal is outside this family (generic
machinery).

### 7.4 Cursor replay / rewind

Covered in §4: the frozen single-outpoint `outpoint_cursor`, arm-gating on
the pre-bind sentinel, positional binding of K to the frozen ordinal, and
the inner `machine_state_hash` chain. A prover replaying an old redeemer
against a new state fails the hash chain; replaying an old _state_ is
impossible because the thread is a single linear UTxO (the token conservation
check admits exactly one continuation).

### 7.5 Payload substitution

The only bytes the scan ever consumes are chunk windows authenticated against
`item_commitment` (§5), which is bound through descriptor → ledger-trie leaf →
`prior_ledger_root` → `transition_step.pre_utxos_root` →
`transition_trace_root` (with the `event_to_step_root` key binding) → header
→ thread NFT asset name. Substituting bytes, descriptors, outpoints,
transactions, or headers each breaks a distinct link in that chain. The control-bytes canonicity re-encode
(`decode_control_v1` re-encoding, `ledger-output-proof-v1.ak:445`) blocks
non-canonical control encodings from aliasing a different machine state under
the same hash.

### 7.6 Wrongful-rejection edge cases

- **Attribution** (revised 2026-08-24; catalogue-adopted): the leaf reason's
  constructor replaces the old `terminal.phase == ResolveInputs` check
  entirely. A rejection recorded under any non-resolved-outpoint arm
  (`FieldItemWidthIllegal`, `WitnessNativeScriptMalformed`, …) cannot be
  attacked by this family — correctly: under the 47-arm split each such arm
  carries its own single-party refutation procedure (catalogue §3), so a
  wrongful rejection under it is another family's business, not an escape
  hatch. The residual — a genuinely scan-borne rejection recorded under a
  **misattributed arm** to dodge this thread — **shrinks** with the split:
  whenever the wrongly-chosen arm's named subject does not exhibit its
  named fault, that arm's own procedure refutes it single-party; what
  remains is a trace-detail fault for the interactive family, exactly as
  the misattributed-phase residual was before (state assumptions only, per
  the no-trap residual ruling, `architecture.md:111-121`). Conversely, a
  rejection whose reason names an outpoint against canonical payloads **is**
  covered regardless of what a hypothetical honest run would have rejected
  later: the fault proven is "this committed reason is not the machine's
  verdict on the named subject", which is exactly a header fault.
- **Maturity-window fit**: both directions fit — ~100 transactions ≈ 33
  minutes at 1 tx/block (§6). The old direction-B adversarial extreme is a
  historical note on the superseded V1-format binding (§6); its dissolution
  is what the forced-leaf amendment was ruled to buy.
- **CEK carve-out** (ruled 2026-08-24; narrowed by the catalogue): exactly
  one arm is interactive-only — `PlutusExecutionFailed { execution_index }`,
  the genuine CEK verdict. Its legacy-code sibling
  `ReceivePurposePlutusV3Forbidden { execution_index }` (static
  V3-for-receive selection, `validation-machine-v1.ak:15193-15201`) is
  single-party refutable as a bounded thread (catalogue §3 #41), so the
  split keeps the carve-out from leaking onto a static fact that merely
  shared the legacy `E_PLUTUS_SCRIPT_INVALID` code (catalogue §2 item 7,
  design note 10). The leaf still _represents_ the interactive arm so the
  format stays total; its refutation belongs to the interactive machinery,
  which should consume the arm's `execution_index` subject to select the
  single execution under dispute (catalogue design note 10).
- **Non-native reference scripts**: a scan-borne reason charged to an
  outpoint whose trie-authenticated descriptor carries language 3 / 128 / -1
  is contradicted by the descriptor alone — direction B finishes at
  `BindDescriptor` without running the scan (§3.2). No Plutus-script bytes can
  be dressed up as a scan target, and a prover cannot skip a tag-0 payload by
  lying about the language (the descriptor is trie-authenticated, not
  prover-supplied).
- **Out-of-domain accusation subjects** (decided 2026-08-25): a scan-borne
  reason whose verbatim pair names a subject outside the committed
  transaction's domain — `source_kind ∉ {0, 1}`, a negative `input_index`,
  or an ordinal at/past the named field's item count — is contradicted
  without any descriptor: the machine resolves only subjects that exist, so
  such a pair in a committed reason is not the machine's verdict. Direction
  B closes at OpenSubject (§3.2); the count face is
  proven against the §8.8 door's authenticated item count, the other two
  faces on the state alone. Soundness runs one way only: an honest
  operator's reasons always name in-domain subjects, so the arm can never
  close against one, and the in-domain refusal is a pinned neutralisation
  selector in the engine exec ledger.

### 7.7 Wrongful-acceptance edge cases

- Direction A accepts **any** refusal class of the frozen machine (not only
  the three scan refusal classes that mirror §2.4.1's direction-B arms): if
  the committed verdict is `Accepted`, _any_
  divergence the staged machine exhibits on the committed bytes (including
  `InvalidOutput`-class refusals from earlier stages of the output machine)
  contradicts it. Whether to keep this breadth or narrow to the
  reference-script stages is §9 Q3; the design recommends keeping it, with
  the coordination caveat noted there.
- A prover cannot manufacture a refusal on honest bytes: the machine is
  deterministic on (control, authenticated window), and both are pinned.
- **Forced-source wrongful acceptance is covered** (correction ruling
  2026-08-24): a forced transaction recorded `ForcedTxValid` whose resolved
  payloads include a non-canonical tag-0 script is attacked by direction A's
  forced-source arm — the acceptance claim, the transaction bytes, and the
  field-opening door all come from the one forced leaf (§2.1, §3.2), so the
  proof shape is identical to the Normal case from OpenSubject onward.

---

## 8. Compatibility and migration

**The embedded interactive scan stage stays frozen and becomes
unreachable-by-policy, not removed.** The existing interactive placement
(`onchain/aiken/validators/fraud-proofs/validation-trace/resolve-inputs-membership-step-semantic-v1.ak`,
whole file, 83 lines) and its semantics
(`verify_resolve_inputs_membership_step_semantics_v1`,
`validation-machine-v1.ak:6387-6499`) are not modified by this family — this
document's constraints forbid touching any `.ak` file, and the settled
decision is a _standalone_ family, not a surgery on the interactive machine.
Off-chain policy (watcher/prover routing) directs scan-borne faults to the new
thread; the interactive step remains deployed, measured-over-cap
(`architecture.md:89-109`), and simply never chosen. No datum or redeemer
format of any existing validator changes.

**Format-wave scope (ruled 2026-08-24).** The migration surface this design
needs is **the forced-leaf revision plus the consistency predicates — not a
machine-state revision**. Concretely, one future format-revision wave (not
the current zero-blueprint-movement wave) carries: the in-place
`ForcedInclusionTxV1` verdict-sum revision with the fully enumerated
**47-arm** `RejectionReasonV1` (normative inventory:
`rejection-reason-catalogue-v1.md` §5; shape in §2.4.1; in place because
nothing is deployed and the house rule is no compat shims — no V2
side-by-side type), the consistency machinery
(`rejection_code_of` and the `forced_verdict_matches`
extension, §2.4.2; `coarse_bucket_of` is a documented table only,
§2.4.2(b)), the reference-script `total_length` cap folded in from old
OPEN B-2 — the exact conjunct is `reference_script_total_length <= 16_384`
**when `reference_script_language == 0`** (native only; no new size rule
for Plutus reference scripts), added in `reference_script_is_well_formed`
(`ledger-output-commitment-v1.ak:90-109`) — and the two **leaf-validity
authoritativeness predicates**
(§2.4.3, follow-up ruling 2026-08-24: Normal-source `validity_code == 0`
and Forced-source verdict-bit equality per (e)'s post-wave final form,
both in `verify_source_authentication`). The descriptor format
(`ValidationTraceDescriptorV1`) and the machine-state format
(`ValidationMachineStateV1`) stay **frozen**; the only bridge to them is
`hash_rejection_code(rejection_code_of(reason)) ==
descriptor.rejection_code_hash` (`validation-trace-v1.ak:239-243`). Both
directions of this family are specified against the target formats and gated
on that wave landing — the thread family ships whole when the wave lands
(§9 Q1 residuals).

**Catalogue-adoption consequences for the wave (2026-08-24).** Four findings
of the rejection-reason catalogue bind the same wave:

- **Extension policy (catalogue design note 1).** With constructor-as-code,
  adding a rejection reason changes the wire format of the forced leaf and
  therefore the meaning of `forced_transactions_root` — a protocol format
  revision of the same class as the verdict restructure itself.
  **Deliberately no in-band extension point**: an "other/unknown" arm would
  be a sentinel by another name and would break constructor-as-code. A
  future `RejectionReasonV2` is a new leaf schema version;
  `rejection_code_of` / `coarse_bucket_of` are the compatibility surface for
  frozen consumers.
- **`MidgardTxValidity` retirement (catalogue §4.5 / OPEN C-3, design
  note 3).** The same wave should retire the five rejection arms of
  `MidgardTxValidity` from the forced leaf — the full reason subsumes them,
  and today's `forced_verdict_matches` never adjudicated the arm choice
  anyway. The enum shrinks to `TxIsValid | TxIsInvalid` (wire scalar 0/1;
  §2.4.3(e) post-wave final form); `coarse_bucket_of` is a documented table
  only (§2.4.2(b)); off-chain consumers of the six-code vocabulary are
  updated in the same wave. This supersedes this document's earlier
  `MalformedTx`-widening recommendation (§2.4.2(b)).
- **The stall concern is RESOLVED by owner ruling (2026-08-24): the
  forced-order door already excludes stall-class preimages.** The L1
  forced-order publication path (`docs/spec/midgard-tx.md` §8.11) refuses
  to finalize an order whose preimages violate the bare-conjunct guardrails
  of catalogue §4.3, so a forced transaction that would stall the machine
  never becomes an order and the operator always has an honest verdict.
  The leaf-format freeze is **no longer gated** on an audit; the
  `GuardrailExceeded` family stays **reserved, not populated**, and is
  expected to remain so. Residual (evidence, non-gating): document the
  per-conjunct coverage mapping — each §4.3 conjunct to the §8.11 door
  check that excludes it — tracked as #641 (§9 Q11).
- **Possibly-dead structural arms (catalogue §4.4, design note 5).** The
  three `ExecutionNativeScript{Malformed,NodeLimit,DepthLimit}` arms are
  plausibly unreachable (every Phase-B native source is pre-scanned by an
  earlier phase); if the reachability proof closes, the wave drops them and
  the type shrinks to 44 (§9 Q12). None of the three is an arm this thread
  consumes.

**Catalogue immutability consequence — checked and reported honestly.** The
fraud-proof catalogue is an MPF root in a datum
(`onchain/aiken/lib/midgard/fraud-proof-catalogue.ak:7-29`,
`Datum = MerkleRoot<Int, ByteArray>`, `id_byte_count = 4`) whose spending
validator **always fails** (`docs/fault-proofs/onchain-reference.md:43`), so
the catalogue is init-time-immutable. This family is registered only through
a **fresh genesis-level deployment**, per
D-S13 ("upgrades = new deployments",
`docs/fault-proofs/catalogue-status.md`). The canonical catalogue holds 25
categories `00000000`–`00000018`; this family is
`nativeScriptDecoding` at `0000000d`. There is no live migration or
compatibility path. See §9 Q7.

**Pinned artifacts.** `native-script-scan-exec-ledger-v1.json` is read-only
input to §6 and is not superseded; the new engine gets its **own** ledger
file when measured (§3.3), leaving the pinned one-shot ledger as the
historical basis it is.

---

## 9. Open questions

Numbered; each with a recommendation.

1. **Direction-B outpoint attribution and maturity fit — RESOLVED by the
   2026-08-24 ruling.** The which-outpoint amendment moved off the
   machine/descriptor and onto the forced leaf, edited in place: the
   Rejected ⇒ Forced invariant makes the `ForcedInclusionTxV1` leaf the
   rejection commitment, its `ForcedTxInvalid { reason }` names the accused
   outpoint's ordinal, and direction B binds one outpoint and inherits
   direction A's ~100-transaction bound (§2.4, §6). OPEN B-1 and B-3
   dissolved; B-2's 16,384 `total_length` cap folded into the same format
   wave, as was A-1's leaf-validity authoritativeness (follow-up ruling
   2026-08-24, §2.4.3). Residual opens:
   - **format-wave scheduling** — the whole family, both directions, is
     specified against the target formats (forced-leaf revision, §2.4;
     authoritativeness predicates, §2.4.3) and gated on the wave landing
     (§8); when it is scheduled is owned outside this document;
   - **the CEK carve-out** — narrowed by the catalogue to exactly one arm:
     `PlutusExecutionFailed { execution_index }` stays interactive-only,
     permanently outside this family; its former code-sharing sibling
     `ReceivePurposePlutusV3Forbidden` is single-party refutable (§1.2,
     §7.6);
   - **the leaf-format freeze** is no longer gated on the catalogue's C-2
     stall concern — resolved by owner ruling 2026-08-24, the forced-order
     door excludes stall-class preimages (§8, §9 Q11); the per-conjunct
     coverage mapping remains as a non-gating evidence task (#641).
2. **Engine ExUnits ledger — RULED by owner (2026-08-24): measurement
   authorized.** §6 rests on the pinned one-shot rates; the
   batched engine (§3.3) must be measured with `aiken` and pinned in a new
   `native-script-decoding-engine-exec-ledger-v1.json` before the family is
   scheduled. The owner authorized building the v2 scan primitives and the
   batched engine and running the measurement; registration stays gated on
   that ledger existing.
3. **Breadth of direction A's refusal acceptance — RULED by owner
   (2026-08-24): recommendation adopted.** Accepting any
   staged-machine refusal (not just the three scan refusal classes)
   maximizes coverage
   but overlaps the planned output-well-formedness family (D-S10).
   The breadth is kept as designed; the overlap is recorded in
   `catalogue-status.md`; D-S10's scoping now subtracts the registered family.
4. **PhaseA witness-script twin.** `reject_invalid_field_type` and the limit
   codes are also emitted for _witness-set_ native scripts in
   PhaseANativeScripts (`validation-machine-v1.ak:3882-4069`, `:4528-4900`);
   those bytes are committed differently (witness-set compact CBOR, not
   ledger descriptors). The §2.4 leaf already _represents_ those rejections
   (the catalogue's `WitnessScriptHeaderMalformed` /
   `WitnessNativeScript{Malformed,NodeLimit,DepthLimit,False}` arms), so a
   twin family would slot into the same reason format. _Recommendation:_ a twin family sharing the engine but
   with a witness-set byte-authentication front end; out of scope here.
5. **Refusal-class fidelity.** AdvanceOrClose preserves the
   staged machine's three-way distinction (invalid / node-limit / depth-limit,
   `validation-machine-v1.ak:6458-6484`) in `refusal_class`.
   _Recommendation:_ keep it — it costs one `Int` and makes the minted proof
   auditable against the descriptor's code without re-running anything.
6. **Direction-A leaf-validity authority — RESOLVED by the follow-up
   ruling (2026-08-24).** The interim descriptor binding is removed; the
   tx-leaf validity is made authoritative by the §2.4.3 format-wave
   predicates — (d) Normal arm: `verified.tx_compact.validity_code == 0`,
   (e) Forced arm: leaf verdict ↔ embedded scalar as the post-wave bit
   equality (§2.4.3(e) final form) — both placed in
   `verify_source_authentication` (`validation-claim-v1.ak:215-265`), where
   the compact bytes are already decoded on every claim, so the cost is two
   field equalities. Direction A binds the acceptance claim on the one leaf
   the transaction lives under — `validity_code == TxIsValid` on the
   `transactions_root` leaf for Normal sources, `ForcedTxValid` on the
   forced leaf for Forced sources — gated on the wave like direction B; no
   descriptor is opened anywhere in the thread. After the wave, a header
   whose leaves and descriptors disagree cannot assemble a valid claim
   witness at all — the disagreement is a claim-machinery format fault, and
   this thread need not (and does not) cover it.
7. **Catalogue id allocation — COMPLETE.** `nativeScriptDecoding` is
   canonically registered at `0000000d`. The fresh genesis-level deployment
   requirement (§8) is accepted; no migration/compatibility path exists.
8. **Fee-price assumptions.** §6's lovelace figures assume current mainnet
   execution prices; the pinned ledger pins ExUnits, not prices.
   Treat only the step-count arithmetic as durable; fee assumptions are an
   operational deployment review item.
9. **`prior_ledger_root` source — RESOLVED by the 2026-08-24 rework.** The
   thread now takes `prior_ledger_root` directly from the transition trace
   (`event_to_step_root` → `transition_trace_root` →
   `transition_step.pre_utxos_root`, the openings of
   `validation-claim-v1.ak:164-201` with the `:357-361` cross-checks; the
   claim machinery's own pin is `:407`), and opens no machine-state preimage
   at all. The old question — whether to cross-check the initial-state
   preimage's root against the trace — dissolved with the preimage itself: a
   header whose _machine states_ lie about the pre-root is a
   validation-trace fault for the interactive family, and this thread's
   proof is a genuine contradiction within the header's own transition-trace
   commitments either way.
10. **Dispute-side code register (catalogue OPEN C-1) — RULED by owner
    (2026-08-24): in the format wave.**
    `docs/spec/midgard-tx.md` carries no dispute-side rejection-code table
    (the earlier "no §12" phrasing here and in catalogue §4.1 is stale —
    the spec gained §12 "Fault statements" before this audit), so nothing
    normative outside the catalogue names the reason space.
    **Execution form (readiness-review resolution):** the format wave
    (#640) adds a **new §13** to the tx spec, "Dispute-side rejection-code
    register", that lists the 19 `E_*` wire labels inline and
    **normatively incorporates by reference** the catalogue's §5 arm
    inventory and §6 type (one source of truth for the 47 arms — no
    duplicated arm table, so the leaf schema, the machine codes, and the
    spec cannot drift apart).
11. **Stall conditions vs forced verdicts (catalogue OPEN C-2) —
    RESOLVED by owner ruling (2026-08-24).** Machine guardrails written as
    bare step-relation conjuncts (oversized field-6 script items, the
    derived collection-count caps, malformed out-ref and address-witness
    items, deep field-5 mint shape — the catalogue's §4.3 verified
    anchors) make a violating transaction **stall**: no accepting _or_
    rejecting successor exists. The owner ruled that the L1 forced-order
    publication path (`docs/spec/midgard-tx.md` §8.11) **already excludes
    such preimages** — a stall-class transaction never becomes a forced
    order, so the operator always has an honest verdict and the reserved
    `GuardrailExceeded` family stays unpopulated (catalogue design
    note 6's second branch). The leaf-format freeze is not gated on this.
    _Residual (evidence, non-gating):_ record the exclusion as the
    invariant that keeps the family unreserved by documenting the
    per-conjunct coverage mapping — each §4.3 conjunct to the §8.11 door
    check that excludes it — tracked as #641.
12. **Possibly-dead `ExecutionNativeScript*` structural arms (catalogue
    §4.4, design note 5) — RULED by owner (2026-08-24): keep all 47.**
    Kept for totality with the machine as written; every Phase-B native
    source is pre-scanned by an earlier phase, so the three structural arms
    are plausibly unreachable. The format wave ships the 47-arm type as-is;
    the reachability proof stays bundled in #641 as non-gating evidence.
    If it later closes, dropping the three arms (47 → 44, asserting the
    invariant in the machine) is a subsequent format revision, not part of
    #640. `ExecutionNativeScriptFalse` is genuinely reachable and stays
    regardless. None of the four is an arm this thread consumes, so the
    outcome does not touch this design's bindings.
