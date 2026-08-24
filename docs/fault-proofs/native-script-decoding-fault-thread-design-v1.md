# Native-script decoding fault: standalone computation-thread family (design v1)

Audit date: 2026-08-24. Branch: `wave/lane-o`. Issue: #633, direction (d).
Revised same day to incorporate the owner rulings of 2026-08-24: the
**Rejected ⇒ Forced** invariant governs the rejection commitment (the thread
binds wrongful rejections to the `ForcedInclusionTxV1` leaf under
`header.forced_transactions_root`, not to the validation-machine descriptor),
the which-outpoint amendment moves onto that leaf in place (pending a future
format-revision wave), and both directions shed the machine-state preimages.
Decisions below cited "ruled 2026-08-24" are settled; this document implements
them.

This document is the architecture and specification for a NEW standalone
single-party fault-proof computation-thread family covering the native-script
decoding/canonicity fault. It is a design document only: no validator, library,
or off-chain code is changed by it, and nothing in it re-opens the decisions
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

- **Direction A — wrongful acceptance.** The header commits, under its
  `transactions_root`, a Normal-source transaction — a leaf whose own
  validity field claims acceptance (`MidgardTxCompact.validity`,
  `ledger-state.ak:473-477`; the scalar `validity_code` embedded in the
  leaf's compact CBOR,
  `onchain/aiken/lib/midgard/fraud-proofs/native-tx/compact.ak:291-301`) and
  which the Rejected ⇒ Forced invariant obliges to be `Accepted` — yet at
  least one output that transaction resolves (spend or reference input)
  carries a tag-0 reference-script payload whose bytes are not a canonical
  native script under the frozen scan semantics — bytes the same header's
  `prev_utxos_root` itself commits.
- **Direction B — wrongful rejection.** The header commits, under its
  `forced_transactions_root`, a forced leaf whose operator verdict is
  `ForcedTxInvalid` with a scan-borne reason (`InvalidFieldType`,
  `NativeScriptDepth`, `NativeScriptNodeCount` — §2.4's target leaf format,
  **pending-format-wave**) charged to a named resolved outpoint, yet the
  payload at exactly that outpoint is either not a tag-0 native reference
  script at all or scans to a canonical terminal.

**Explicit carve-out (ruled 2026-08-24):** rejections whose reason is
`E_PLUTUS_SCRIPT_INVALID` are **not refutable by this family** — CEK verdicts
are interactive-only, and no committed-content recomputation thread can
adjudicate them (§7.6, §9 Q1 residuals).

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
   witnesses (direction A: the Normal transaction leaf and — interim, §2.2
   OPEN A-1 — its validation-trace descriptor; direction B: the forced leaf;
   both: the event-to-step and transition-step leaves that source the
   pre-state root), the ledger-trie membership proof for the accused resolved
   outpoint, and the reference-script payload bytes with their chunk proofs.
   No machine-state preimages are needed (ruled 2026-08-24; §2.1).
3. **Init**: mint one computation-thread unit named
   `category_id(4B) ‖ header_hash(28B)`
   (`onchain/aiken/validators/computation-thread.ak:109-115`) at the step-01
   address with `StepDatum { fraud_prover, data: None }`.
4. **Steps**: step-01 binds the faulted transaction to the header (direction
   A) or records the direction (direction B); step-02 binds the committed
   verdict — the forced leaf's `ForcedTxInvalid` reason for direction B, the
   Normal leaf plus (interim) descriptor for direction A — and sources the
   pre-state root from the transition trace; step-03 binds the accused
   outpoint's descriptor and scans its payload bytes under the pinned
   per-node budget, as many L1 transactions as needed; step-04
   concludes via `common.finalize`
   (`onchain/aiken/lib/midgard/fraud-proofs/common.ak:579-673`), minting the
   permanent `fraud_proof` token at the fraud-proof address.
5. **Removal**: the `fraud_proof` token authorizes state-queue
   `RemoveFaultyBlockHeader` within the seven-day maturity window; §3.3's fit
   rule binds the whole proof against **half** of that window
   (`architecture.md:283-286`).

A duplicate Init by a second prover mints a second unit of the *same* asset
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
  1. `event_to_step_root` at the event key (direction A:
     `L2TransactionEventKey { tx_id }`; direction B:
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
  128), `reference_script_offset` / `total_length`, and the 32-byte
  `item_commitment` chunk commitment over the full output bytes (4095-byte
  chunks, `onchain/aiken/lib/midgard/bounded-item-v1.ak:12`).
- **The resolved-outpoint coordinate system.** Ordinals over resolved
  outpoints are in **field order**: spend inputs (field 0) in committed
  order, then reference inputs (field 1) in committed order — ordinal ∈
  `[0, spend_count + reference_count)` (ruled 2026-08-24; field indices at
  `onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.ak:102-106`, fixed
  38-byte stride reads via `native_tx_machine_walk_v1.spend_input_at`,
  `onchain/aiken/lib/midgard/native-tx-machine-walk-v1.ak:532`). This is
  deliberately the positionally-openable coordinate: the §8.8 stride read *is*
  the ordinal's semantics. Note honestly that the machine's internal
  resolution-schedule *hash* sorts scheduled inputs by encoded key
  (`transaction_resolution_schedule_hash`,
  `validation-machine-v1.ak:859-885`; comparator at `:852-857`) — that sorted
  position is a different, non-positionally-openable ordering and is **not**
  the leaf coordinate.

Direction A additionally:

- **`transactions_root` / `l2_transaction_count`** (counted, domain-tagged;
  `commit_counted_root` at
  `onchain/aiken/lib/midgard/transition-trace.ak:67-81`): step-01 proves the
  faulted L2 transaction T is committed by the header, via
  `verify_native_tx_in_state_queue_node_with`
  (`onchain/aiken/lib/midgard/fraud-proofs/common.ak:792-847`; counted-root
  authentication at `common.ak:831-836`). The committed compact CBOR carries
  T's own scalar `validity_code`
  (`onchain/aiken/lib/midgard/fraud-proofs/native-tx/compact.ak:291-301`,
  `:381-390`; type-level `MidgardTxCompact.validity` at
  `ledger-state.ak:473-477`; the V1 leaf wrapper is `L2TransactionSourceV1`,
  `ledger-state.ak:532-536`). Under the Rejected ⇒ Forced ruling this leaf
  validity **is** the acceptance claim the fault contradicts; the descriptor
  opening below is the interim authority only because leaf-validity
  consistency is not yet independently enforced (OPEN A-1).
- **Interim: `validation_traces_root` / `validation_trace_count`.** Step-02
  opens the `ValidationTraceDescriptorV1`
  (`onchain/aiken/lib/midgard/validation-trace-v1.ak:80-89`) for T, keyed by
  `L2TransactionEventKey { tx_id }`, exactly as `verify_descriptor_membership`
  (`validation-claim-v1.ak:147-162`), and requires `verdict == Accepted` —
  reading the verdict from the **descriptor leaf itself**; no state preimage
  is opened. This binding is kept until the format wave makes the tx-leaf
  validity authoritative (OPEN A-1, §9 Q6).
- T's resolved-outpoint sets, opened from T's own committed bytes through the
  §8.8 field-opening door (fields 0 and 1, coordinate system above).

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
  a scan-borne arm carrying a `ResolvedOutpointSubject { outpoint_ordinal }`.
  **No descriptor opening, no terminal-state preimage, no
  `phase == ResolveInputs` check**: the constructor arm replaces the phase
  attribution entirely (ruled 2026-08-24). The superseded phase-attribution
  machinery — and why the code hash alone could not attribute (the same
  codes are emitted from CanonicalDecode, `validation-machine-v1.ak:1627`,
  `:1941`, and PhaseANativeScripts, `:4041-4069`, among ~50 sites) — is
  retained as the design *rationale* for the subject coordinate in §2.4.
- The forced leaf's `source.compact_cbor` supplies the committed transaction
  bytes for the field-opening door (same §8.8 coordinate as direction A);
  `tx_id` is authenticated against them by
  `verify_native_tx_proof_source_v1`.
- Direction B scans **exactly the accused outpoint** and therefore inherits
  direction A's ~100-transaction bound (§6).

### 2.2 OPEN markers (revised 2026-08-24)

- **OPEN (A-1): Normal-tx leaf-validity consistency is not independently
  enforced.** Nothing on-chain today ties the compact leaf's embedded
  `validity_code` to the descriptor verdict or to the applied delta for
  Normal-source transactions: `expect_validity_code` bounds it to `0..5`
  (`onchain/aiken/lib/midgard/fraud-proofs/native-tx/codec.ak:25-29`) and the
  claim machinery never reads it (`validation-claim-v1.ak` contains no
  `validity_code` consumer; its Normal arm constrains only the descriptor,
  `:311-317`). Until the format wave either makes the tx-leaf validity
  authoritative or adds the Normal-tx consistency predicate (recommendation,
  §9 Q6), direction A keeps the descriptor binding as interim design.
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
  §8). Until it lands, the §2.3 shortcut stays conditional and the thread
  scans rather than assumes.

### 2.3 Bounds-unreachability lemma (context, not a shortcut)

The maximum reference-script payload reachable through committed L2 outputs is
16,341 bytes = 5,447 nodes (pinned ledger,
`native-script-scan-exec-ledger-v1.json`), and 5,447 < 16,384. So *within the
byte caps* the node-limit and depth-limit codes are wrongful per se. But
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

One constructor per distinct machine rejection code — the constructor tag
**is** the code: no code strings, no code hashes, no sentinels ride the leaf.
The machine defines exactly **19** codes today (verified by `const reject_`
inventory over `onchain/aiken/lib/midgard`, all in
`validation-machine-v1.ak`: `:1186-1205`, `:2128-2134`, `:2277-2278`,
`:2805-2808`, `:3450-3453`, `:6164-6169`, `:14338`). Each arm carries only
its **subject ordinal(s)** in machine-defined coordinate systems — name the
subject, never carry the argument (no byte payloads, no hashes-of-evidence;
the one exception is `ValueNotPreserved`, whose subject *is* an asset id).

```aiken
// NEW / pending-format-wave
pub type RejectionSubjectV1 {
  // resolution-schedule ordinal (§2.1): spend inputs (field 0) in field
  // order, then reference inputs (field 1) in field order
  ResolvedOutpointSubject { outpoint_ordinal: Int }
  // §8.8 field-door coordinate, 0..8 (field-opening-v1.ak:102-118)
  FieldSubject { field_index: Int }
  // ordinal into the witness set's script witnesses (field 6)
  WitnessScriptSubject { script_index: Int }
  // ordinal into required signers (field 4)
  RequiredSignerSubject { signer_index: Int }
  // ordinal into required observers (field 3)
  ObserverSubject { observer_index: Int }
  // ordinal into redeemers (field 8)
  RedeemerSubject { redeemer_index: Int }
}

pub type RejectionReasonV1 {
  // -- tx-global: NO fields (recomputable from header + committed tx bytes) --
  MinFee                          // E_MIN_FEE
  EmptyInputs                     // E_EMPTY_INPUTS
  NetworkIdMismatch               // E_NETWORK_ID_MISMATCH
  AssetCount                      // E_ASSET_COUNT
  ValidityIntervalMismatch        // E_VALIDITY_INTERVAL_MISMATCH
  InvalidValidityIntervalFormat   // E_INVALID_VALIDITY_INTERVAL_FORMAT
  // -- field-scoped --
  FieldPreimageSize { field_index: Int }         // E_FIELD_PREIMAGE_SIZE
  // -- input-scoped (resolved-outpoint ordinal) --
  InputNotFound { outpoint_ordinal: Int }        // E_INPUT_NOT_FOUND
  DuplicateInputInTx { outpoint_ordinal: Int }   // E_DUPLICATE_INPUT_IN_TX
  InvalidOutput { outpoint_ordinal: Int }        // E_INVALID_OUTPUT
  // -- scan-capable (multi-site): subject coordinate --
  InvalidFieldType { subject: RejectionSubjectV1 }      // E_INVALID_FIELD_TYPE
  NativeScriptDepth { subject: RejectionSubjectV1 }     // E_NATIVE_SCRIPT_DEPTH
  NativeScriptNodeCount { subject: RejectionSubjectV1 } // E_NATIVE_SCRIPT_NODE_COUNT
  // -- witness-scoped --
  InvalidSignature { vkey_witness_index: Int }   // E_INVALID_SIGNATURE
  MissingRequiredWitness { subject: RejectionSubjectV1 } // E_MISSING_REQUIRED_WITNESS
  NativeScriptInvalid { script_index: Int }      // E_NATIVE_SCRIPT_INVALID
  // -- output-scoped --
  MinAda { output_index: Int }                   // E_MIN_ADA
  // -- redeemer-scoped --
  PlutusScriptInvalid { redeemer_index: Int }    // E_PLUTUS_SCRIPT_INVALID
  // -- asset-scoped --
  ValueNotPreserved { policy_id: ByteArray, asset_name: ByteArray } // E_VALUE_NOT_PRESERVED
}
```

Arm-by-arm subject assignments, with the emission sites that justify them
(all anchors `validation-machine-v1.ak` unless noted):

| Code | Payload | Subject space and justification |
|---|---|---|
| `E_MIN_FEE` | none | Tx-global (ruled explicitly): the fee floor is recomputable from header `min_fee_a`/`min_fee_b` (`ledger-state.ak:79-80`) plus the committed tx bytes. Emitted `:2708`. |
| `E_EMPTY_INPUTS` | none | Tx-global: the subject is field 0 being empty; nothing finer to name. Emitted `:2404`. |
| `E_NETWORK_ID_MISMATCH` | none | Tx-global: one network id per tx vs `header.expected_network_id`. Emitted `:2706`. |
| `E_ASSET_COUNT` | none | Tx-global **by choice**: the cap is a whole-transaction aggregate fold (`fold.asset_count + asset_count > max_distinct_asset_count`, `:9644-9651`; also `:17396`, `:17561`, `:17644`), so the item at which the aggregate crosses is traversal-order-dependent — naming it would encode the machine's walk, not the subject. |
| `E_VALIDITY_INTERVAL_MISMATCH` | none | Tx-global: interval vs `header.block_slot`. Emitted `:6744`, `:6844`. |
| `E_INVALID_VALIDITY_INTERVAL_FORMAT` | none | Tx-global: one validity-interval field per tx. Emitted `:2452`. |
| `E_FIELD_PREIMAGE_SIZE` | `field_index` | The field whose preimage breaches the size discipline; §8.8 door coordinate 0..8. Emitted `:1501`, `:1649`, `:1961`. |
| `E_INPUT_NOT_FOUND` | `outpoint_ordinal` | The input whose outpoint has no leaf under the pre-state root. Emitted `:6559`. |
| `E_DUPLICATE_INPUT_IN_TX` | `outpoint_ordinal` | The **later** occurrence of the duplicated input (spend-input ordinal; the earlier occurrence is derivable from the bytes). Emitted `:2621`. |
| `E_INVALID_OUTPUT` | `outpoint_ordinal` | Both emission sites charge a **resolved** output (`LedgerOutputProofInvalidOutput` mapping, `:6462` in ResolveInputs, `:9380` in ScriptSources), so the coordinate is the resolution ordinal, not a field-2 output index, despite the name. |
| `E_INVALID_FIELD_TYPE` | `subject` | Multi-site (~50 emissions across CanonicalDecode `:1627`/`:1941`, PhaseANativeScripts `:3882`-`:5371`, ResolveInputs `:6468`, ScriptSources `:8272`-`:9386`, NativeScripts `:11737`+, tail `:14986`): no single ordinal space covers them, so the payload is the subject coordinate. Scan-borne (resolved-output) emissions MUST use `ResolvedOutpointSubject`. |
| `E_NATIVE_SCRIPT_DEPTH` | `subject` | Multi-site: PhaseA witness scripts (`:4069`, `:4585`, `:4866`) vs resolved outputs (`:6480`, `:9398`). Admissible arms: `WitnessScriptSubject`, `ResolvedOutpointSubject`. |
| `E_NATIVE_SCRIPT_NODE_COUNT` | `subject` | Multi-site: PhaseA witness scripts (`:4049`, `:4544`, `:4851`, `:4900`, `:5117`, `:5216`) vs resolved outputs (`:6474`, `:9392`). Admissible arms as above. |
| `E_INVALID_SIGNATURE` | `vkey_witness_index` | Per-witness signature check in the Signatures phase (`:3005`); ruled coordinate. |
| `E_MISSING_REQUIRED_WITNESS` | `subject` | Multi-site: required signers (`:3275` → `RequiredSignerSubject`), resolved-input signer authorization (`:6518` → `ResolvedOutpointSubject`), missing script source at ScriptSources stage nine (`:9431`, `:11308` → `RedeemerSubject` for the requiring purpose), native-script signers and observers (`:10905`, `:11373`, `:11614` → `WitnessScriptSubject` / `ObserverSubject`). A missing witness has no vkey-witness index, so the subject is the unmet **requirement**, named in its own field's coordinate. |
| `E_NATIVE_SCRIPT_INVALID` | `script_index` | Single site: a witness-set native script evaluating false in PhaseA (`:4250`); field-6 ordinal. |
| `E_MIN_ADA` | `output_index` | Per-output floor in the ValueAndMint output-descriptor step (`:17494`; doc comment at `:1196-1204`); field-2 ordinal. |
| `E_PLUTUS_SCRIPT_INVALID` | `redeemer_index` | Ruled coordinate; CEK sites `:15078`, `:15200`. Not refutable by this family (§1.2 carve-out). |
| `E_VALUE_NOT_PRESERVED` | `{ policy_id, asset_name }` | Ruled: names the allegedly unbalanced asset. Emitted `:17688`. |

A `rejection_reason_is_well_formed` predicate (NEW, format wave) restricts
each multi-site arm to its admissible subject constructors per the table and
bounds every ordinal non-negative. A reason whose subject arm misattributes
the site (e.g. a genuinely scan-borne rejection recorded with `FieldSubject`)
is a trace-detail fault for the interactive family — exactly the residual the
superseded phase-attribution check had (§7.6).

#### 2.4.2 Consistency predicates (NEW, specified with the format)

All three ruled 2026-08-24; the descriptor and machine-state formats stay
**untouched** (frozen) — the bridge runs through the already-committed code
hash:

- **(a) `rejection_code_of(reason: RejectionReasonV1) -> ByteArray`** — a
  total map from constructor to the frozen `E_*` label bytes (the
  `const reject_*` values, `validation-machine-v1.ak:1186-1205` etc.),
  bridged to the FROZEN descriptor format by
  `hash_rejection_code(rejection_code_of(reason)) ==
  descriptor.rejection_code_hash` (`hash_rejection_code` at
  `onchain/aiken/lib/midgard/validation-trace-v1.ak:239-243`, domain-tagged
  blake2b-256).
- **(b) `coarse_bucket_of(reason: RejectionReasonV1) -> MidgardTxValidity`**
  — a total map into the compact-tx leaf's validity vocabulary
  (`ledger-state.ak:485-492`), for consistency with the compact leaf's
  validity field: `InputNotFound → NonExistentInputUtxo`;
  `InvalidSignature | MissingRequiredWitness → InvalidSignature`;
  `NativeScriptInvalid | PlutusScriptInvalid | NativeScriptDepth |
  NativeScriptNodeCount → FailedScript`; `MinFee → FeeTooLow`;
  `ValueNotPreserved | MinAda | AssetCount → UnbalancedTx`; the structural /
  format codes (`EmptyInputs`, `DuplicateInputInTx`, `InvalidOutput`,
  `InvalidFieldType`, `FieldPreimageSize`, `NetworkIdMismatch`, both
  validity-interval codes) map to `FailedScript` as a **documented lossy
  catch-all** — `MidgardTxValidity` is literally `// TODO`-marked
  (`ledger-state.ak:484`) and has no malformed-structure arm; the format
  wave should widen it (recommendation: add a `MalformedTx` arm) rather than
  keep the lossy bucket. `TxIsValid` is unreachable from this map by
  construction.
- **(c) `forced_verdict_matches` extended.** Today it couples the leaf's
  coarse validity to the descriptor verdict
  (`validation-claim-v1.ak:204-212`). The extension requires, on the
  Rejected arm, that the leaf **reason** agrees with the descriptor's
  committed code hash: `verdict is ForcedTxInvalid { reason }` ⇒
  `descriptor.verdict == Rejected` and
  `hash_rejection_code(rejection_code_of(reason)) ==
  descriptor.rejection_code_hash`; `ForcedTxValid` ⇒
  `descriptor.verdict == Accepted`.

---

## 3. Contract set

Four spending validators plus reuse of the existing generic machinery. Names
follow the double-spend family's convention.

| # | Validator (new) | Role |
|---|---|---|
| 1 | `validators/fraud-proofs/native-script-decoding/step-01.ak` | Bind the faulted transaction T to the header (direction A); record the direction and pass through (direction B) |
| 2 | `validators/fraud-proofs/native-script-decoding/step-02.ak` | Bind the committed verdict (forced leaf / Normal leaf + interim descriptor); source the pre-state root from the transition trace; branch on direction |
| 3 | `validators/fraud-proofs/native-script-decoding/step-03.ak` | Self-looping resolve-and-scan engine (multi-arm redeemer) |
| 4 | `validators/fraud-proofs/native-script-decoding/step-04.ak` | Conclude: `common.finalize`, mint `fraud_proof` |

### 3.1 Parameterization (acyclic chain)

- step-01(`step_02_hash`, `computation_thread_policy_id`, `hub_oracle`)
- step-02(`step_03_hash`, `computation_thread_policy_id`)
- step-03(`step_04_hash`, `computation_thread_policy_id`,
  `field_preimage_certificate_policy_id`)
- step-04(`computation_thread_policy_id`, `fraud_proof_token_policy_id`,
  `fraud_proof_token_address`)

A naive design would put "bind outpoint" and "scan payload" in two validators
that hand off to each other; that is a parameterization **cycle** (each needs
the other's hash) and is impossible under hash parameterization. Step-03 is
therefore a **single self-looping validator**: its continuation output is
either its own script address (next arm / next scan window) or step-04's. A
validator knows its own hash from `own_out_ref` resolution, so the self-loop
needs no self-parameter; only the forward edge to step-04 is a parameter.
The chain 01→02→03→(03)*→04 is acyclic in parameters.

### 3.2 Redeemer arms

Every step carries `ct.Cancel` via `common.cancel`
(`common.ak:437-481`), exactly as
`resolve-inputs-membership-step-semantic-v1.ak:37-45` and the double-spend
steps do. The Continue arms:

- **step-01** `Continue(BindTransaction { direction, carriage })`:
  - Direction A: verbatim reuse of `pass_native_tx_to_next_step_carried`
    (`common.ak:149-252`) including the published-chunk carriage duality
    (#545), as in `double-spend/step-01.ak:57-94` — binds T under
    `transactions_root`. Under the 2026-08-24 ruling the compact leaf's own
    `validity_code` **is** the acceptance claim being contradicted; the
    descriptor opened at step-02 remains the interim verdict authority only
    because leaf-validity consistency is unenforced today (OPEN A-1, §9 Q6).
  - Direction B: no `transactions_root` work — the faulted transaction lives
    under `forced_transactions_root` and is bound at step-02 from the forced
    leaf itself. This arm only records the direction and passes through.
  - Output state: `{ direction, verified_tx_id }` (direction B: a sentinel
    until step-02 authenticates `tx_id` from the forced leaf) at step-02's
    address.
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
  3. Direction A (`BindVerdict { descriptor_membership,
     event_step_openings }`): descriptor membership as in §2.1 (counted,
     domain `ValidationTracesRootDomain`, `validation-claim-v1.ak:147-162`),
     `descriptor key == L2TransactionEventKey { verified_tx_id }`, and
     `descriptor.verdict == Accepted` — read from the descriptor leaf, no
     preimage.
  4. Direction B (`BindVerdict { forced_membership, event_step_openings }`):
     open the `ForcedInclusionTxV1` leaf at
     `ForcedTransactionEventKey { tx_order_id }` (the
     `verify_source_authentication` opening,
     `validation-claim-v1.ak:215-241`), authenticate the source triple
     (`verify_native_tx_proof_source_v1`, `compact.ak:493-514`) and set
     `verified_tx_id := leaf.tx_id`; pattern-match
     `ForcedTxInvalid { reason }` (§2.4 target format,
     pending-format-wave) and require the reason to be one of
     `InvalidFieldType` / `NativeScriptDepth` / `NativeScriptNodeCount`
     with `subject == ResolvedOutpointSubject { outpoint_ordinal }`; freeze
     the reason's constructor tag and `outpoint_ordinal` into the state.
     **No descriptor opening, no terminal-state preimage, no phase check**
     — the constructor arm is the attribution.
  5. Output state: the §4 schema, cursor frozen at the accused ordinal
     (direction B) or at the prover-chosen ordinal (direction A).
- **step-03**, three Continue arms:
  - `BindOutpoint { field_opening, outpoint_index, ledger_membership,
    descriptor_bytes }`: open T's field 0 or 1 through the §8.8 door
    (`opened_field_view` + the 38-byte stride read, as
    `double-spend/step-03.ak:78-92`), read outpoint K at the cursor index;
    prove `mpf.has(prior_ledger_root, cbor(K), descriptor_bytes, proof)`
    (mirroring `validation-machine-v1.ak:6398-6407`); decode the descriptor
    (`ledger-output-commitment-v1.ak:162`) and check
    `descriptor_is_well_formed` (`:113-133`). The bound ordinal must equal
    the frozen cursor — direction B binds **exactly the accused outpoint**,
    direction A the prover-chosen one. If
    `reference_script_language != 0` (not tag-0 native): direction B is
    **already done** — a scan-borne rejection charged to an outpoint whose
    trie-authenticated descriptor is not a tag-0 native script is
    contradicted by the descriptor alone; record the contradiction and hand
    off toward step-04. Direction A **fails** the arm (the prover picked the
    wrong K — direction A needs exactly one faulting K). If tag-0:
    initialize the inner machine with
    `ledger_output_proof_v1.initial_control_v1(output_index, total_length,
    item_commitment)` (`onchain/aiken/lib/midgard/ledger-output-proof-v1.ak:279-300`)
    advanced to the native-script stage bound to the descriptor's
    reference-script window
    (`ledger-output-proof-v1.ak:102-113` binds
    `native_control.start_offset == output_scan.reference_script_offset` etc.).
  - `Scan { control_bytes, chunk_window, budget_witness }`: the budgeted fold
    (§3.3). Decode-and-re-encode `control_bytes`
    (`decode_control_v1` re-encodes for canonicity,
    `ledger-output-proof-v1.ak:445`), check
    `blake2b_256(domain ‖ control_bytes)` equals the carried machine hash,
    authenticate the 1–2 adjacent 4095-byte chunks **once** via
    `authenticated_chunk_window` (`ledger-output-proof-v1.ak:490-534`,
    backed by `bounded_item_v1.verify_chunk`,
    `bounded-item-v1.ak:145`), then loop the frozen per-node primitives
    (`structure_token_step_v1` `native-script-scan-v1.ak:805-863`,
    `structure_frame_step_v1` `:868-911`) up to the per-transaction node
    budget, and carry the new control hash. Terminal within the window:
    `finalize_structure_v1` (`:913-938`) /
    `structure_terminal_is_exact_v1` (`:940-947`).
  - `Verdict { … }`: consume the inner machine's terminal.
    - Direction A: **any refusal** of the frozen machine on the committed
      bytes (invalid-field-type, node-limit, depth-limit — the same
      distinctions the result mapping draws,
      `validation-machine-v1.ak:6458-6484`) proves the contradiction with
      `Accepted`; hand off to step-04 with the refusal class recorded.
    - Direction B: the machine must reach the **exact canonical terminal**
      (`terminal_is_exact` discipline as
      `ledger-output-proof-v1.ak:1286` / descriptor exactness `:1336`) on the
      accused outpoint; that single terminal contradicts the leaf's
      scan-borne reason, and the thread hands off to step-04. No iteration
      over other outpoints exists any more (the universal quantification of
      the superseded V1-format binding is a §6 historical note).
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
  fold described above. This is NEW code, deliberately *not* a reuse of
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
  verified_tx_id: ByteArray,         // 32B; direction A: step-01's counted-root binding;
                                     // direction B: the forced leaf's authenticated tx_id
  tx_order_id: Int,                  // direction B: the forced leaf's key; -1 for direction A
  scan_reason_class: Int,            // direction B: 0 = InvalidFieldType, 1 = NativeScriptDepth,
                                     // 2 = NativeScriptNodeCount (the leaf reason's tag);
                                     // -1 for direction A
  prior_ledger_root: ByteArray,      // 32B, transition_step.pre_utxos_root (§2.1 opening)
  // -- the accused/chosen outpoint, frozen at step-02 --
  outpoint_cursor: Int,              // FROZEN ordinal in [0, spend_count + reference_count):
                                     // direction B: the leaf reason's outpoint_ordinal;
                                     // direction A: prover-chosen
  outpoint_key_hash: ByteArray,      // 32B blake2b_256(cbor(K)); binds BindOutpoint → Scan
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
refusal contradicts `Accepted`). `BindOutpoint` may only fire when
`machine_state_hash` is the pre-bind sentinel, and must bind the outpoint at
exactly `outpoint_cursor` (the 38-byte stride read is positional, so the
ordinal *is* the identity — `double-spend/step-03.ak:89-92` precedent). A
prover cannot substitute an outpoint: the frozen ordinal is the key,
`outpoint_key_hash` pins K across the Bind→Scan→Verdict arc, and
`item_commitment` pins the bytes (§5). `machine_state_hash` carries the inner
cursor between L1 transactions exactly as the pushdown template's
`script_digest`-protected cursor does
(`native-tx-script-pushdown-v1.ak:643-654`), and the frozen control is already
constant-size (`NativeScriptStructureControlV1`: version, stage, start_offset,
cursor, end_offset, stack_root hash-chain, stack_depth, node_count). Replays
of an old redeemer fail the hash chain; replaying an old *state* is impossible
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
`BindOutpoint` arm inherits it via `initial_control_v1(output_index,
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
*transport* optimization, not an authentication root: the parked chunks still
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
- plus binding/structure overhead: step-01, step-02, one `BindOutpoint`,
  the `Verdict` arm, step-04 — call it ~10 more. Both directions bind
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
against a new state fails the hash chain; replaying an old *state* is
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

- **Attribution** (revised 2026-08-24): the leaf reason's constructor arm and
  subject replace the old `terminal.phase == ResolveInputs` check entirely.
  A rejection recorded with a scan-capable code but a non-outpoint subject
  (`FieldSubject`, `WitnessScriptSubject`, …) cannot be attacked by this
  family — correctly, since those emissions (CanonicalDecode, PhaseA; §2.4.1
  inventory) may be legitimate for other reasons. The residual — a genuinely
  scan-borne rejection recorded with a **misattributed subject arm** to dodge
  refutability — is a trace-detail fault for the interactive family, exactly
  as the misattributed-phase residual was before (state assumptions only, per
  the no-trap residual ruling, `architecture.md:111-121`). Conversely, a
  rejection whose reason names an outpoint against canonical payloads **is**
  covered regardless of what a hypothetical honest run would have rejected
  later: the fault proven is "this committed reason is not the machine's
  verdict on the named subject", which is exactly a header fault.
- **Maturity-window fit**: both directions fit — ~100 transactions ≈ 33
  minutes at 1 tx/block (§6). The old direction-B adversarial extreme is a
  historical note on the superseded V1-format binding (§6); its dissolution
  is what the forced-leaf amendment was ruled to buy.
- **CEK carve-out** (ruled 2026-08-24): `E_PLUTUS_SCRIPT_INVALID` rejections
  are not refutable by this family — CEK verdicts are interactive-only. The
  leaf still *represents* them (`PlutusScriptInvalid { redeemer_index }`,
  §2.4.1) so the format stays total; refutation belongs to the interactive
  machinery.
- **Non-native reference scripts**: a scan-borne reason charged to an
  outpoint whose trie-authenticated descriptor carries language 3 / 128 / -1
  is contradicted by the descriptor alone — direction B finishes at
  `BindOutpoint` without running the scan (§3.2). No Plutus-script bytes can
  be dressed up as a scan target, and a prover cannot skip a tag-0 payload by
  lying about the language (the descriptor is trie-authenticated, not
  prover-supplied).

### 7.7 Wrongful-acceptance edge cases

- Direction A accepts **any** refusal class of the frozen machine (not only
  the three scan codes): if the committed verdict is `Accepted`, *any*
  divergence the staged machine exhibits on the committed bytes (including
  `InvalidOutput`-class refusals from earlier stages of the output machine)
  contradicts it. Whether to keep this breadth or narrow to the
  reference-script stages is §9 Q3; the design recommends keeping it, with
  the coordination caveat noted there.
- A prover cannot manufacture a refusal on honest bytes: the machine is
  deterministic on (control, authenticated window), and both are pinned.

---

## 8. Compatibility and migration

**The embedded interactive scan stage stays frozen and becomes
unreachable-by-policy, not removed.** The existing interactive placement
(`onchain/aiken/validators/fraud-proofs/validation-trace/resolve-inputs-membership-step-semantic-v1.ak`,
whole file, 83 lines) and its semantics
(`verify_resolve_inputs_membership_step_semantics_v1`,
`validation-machine-v1.ak:6387-6499`) are not modified by this family — this
document's constraints forbid touching any `.ak` file, and the settled
decision is a *standalone* family, not a surgery on the interactive machine.
Off-chain policy (watcher/prover routing) directs scan-borne faults to the new
thread; the interactive step remains deployed, measured-over-cap
(`architecture.md:89-109`), and simply never chosen. No datum or redeemer
format of any existing validator changes.

**Format-wave scope (ruled 2026-08-24).** The migration surface this design
needs is **the forced-leaf revision plus the consistency predicates — not a
machine-state revision**. Concretely, one future format-revision wave (not
the current zero-blueprint-movement wave) carries: the in-place
`ForcedInclusionTxV1` verdict-sum revision with the fully enumerated
`RejectionReasonV1` and its subject coordinates (§2.4; in place because
nothing is deployed and the house rule is no compat shims — no V2
side-by-side type), the three consistency predicates
(`rejection_code_of`, `coarse_bucket_of`, and the `forced_verdict_matches`
extension, §2.4.2), the 16,384 `total_length` cap folded in from old OPEN
B-2 (§2.2), and — recommended, not yet ruled — the direction-A leaf-validity
authority or Normal-tx consistency predicate (OPEN A-1, §9 Q6). The
descriptor format (`ValidationTraceDescriptorV1`) and the machine-state
format (`ValidationMachineStateV1`) stay **frozen**; the only bridge to them
is `hash_rejection_code(rejection_code_of(reason)) ==
descriptor.rejection_code_hash` (`validation-trace-v1.ak:239-243`).
Direction B of this family is specified against the target leaf and is gated
on that wave landing (§9 Q1 residuals).

**Catalogue immutability consequence — checked and reported honestly.** The
fraud-proof catalogue is an MPF root in a datum
(`onchain/aiken/lib/midgard/fraud-proof-catalogue.ak:7-29`,
`Datum = MerkleRoot<Int, ByteArray>`, `id_byte_count = 4`) whose spending
validator **always fails** (`docs/fault-proofs/onchain-reference.md:43`), so
the catalogue is init-time-immutable: **adding this family to a live
deployment is impossible; it requires a new genesis-level deployment**, per
D-S13 ("upgrades = new deployments",
`docs/fault-proofs/catalogue-status.md:211-214`). The current catalogue holds
eleven categories `00000000`–`0000000a`
(`docs/fault-proofs/onchain-reference.md:108-113`) with append index 11 =
`0000000b` reserved for the fabricated-deposit family (#617,
`catalogue-status.md`). This family takes the next free index at registration
time — **the id is deliberately not fixed by this document** because the
reserved-index ledger is owned by the registration wave and `catalogue.ts` is
already drifted (8 registered vs 11 on-chain,
`catalogue-status.md`); pinning a number here would add a third source of
truth. See §9 Q7.

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
   wave. Residual opens:
   - **format-wave scheduling** — direction B is specified against the
     target leaf format and is gated on the wave landing (§8); when it is
     scheduled is owned outside this document;
   - **direction-A leaf-validity trust (OPEN A-1)** — leaf-validity ↔
     applied-delta consistency for Normal txs is not independently enforced
     today, so direction A keeps the interim descriptor binding (§2.2, Q6);
   - **the CEK carve-out** — `E_PLUTUS_SCRIPT_INVALID` rejections stay
     interactive-only, permanently outside this family (§1.2, §7.6).
2. **Engine ExUnits ledger.** §6 rests on the pinned one-shot rates; the
   batched engine (§3.3) must be measured with `aiken` (not runnable under
   this document's constraints) and pinned in a new
   `native-script-decoding-engine-exec-ledger-v1.json` before the family is
   scheduled. *Recommendation:* gate registration on that ledger existing.
3. **Breadth of direction A's refusal acceptance.** Accepting any
   staged-machine refusal (not just the three scan codes) maximizes coverage
   but overlaps the planned output-well-formedness family (D-S10).
   *Recommendation:* keep the breadth; record the overlap in
   `catalogue-status.md` at registration so D-S10's scoping subtracts it.
4. **PhaseA witness-script twin.** `reject_invalid_field_type` and the limit
   codes are also emitted for *witness-set* native scripts in
   PhaseANativeScripts (`validation-machine-v1.ak:3882-4069`, `:4528-4900`);
   those bytes are committed differently (witness-set compact CBOR, not
   ledger descriptors). The §2.4 leaf already *represents* those rejections
   (`WitnessScriptSubject`), so a twin family would slot into the same
   reason format. *Recommendation:* a twin family sharing the engine but
   with a witness-set byte-authentication front end; out of scope here.
5. **Refusal-class fidelity.** The thread's `Verdict` arm preserves the
   staged machine's three-way distinction (invalid / node-limit / depth-limit,
   `validation-machine-v1.ak:6458-6484`) in `refusal_class`.
   *Recommendation:* keep it — it costs one `Int` and makes the minted proof
   auditable against the descriptor's code without re-running anything.
6. **Direction-A leaf-validity authority (OPEN A-1).** Under the
   2026-08-24 ruling the compact leaf's own validity field is the acceptance
   claim direction A contradicts, but leaf-validity ↔ applied-delta
   consistency for Normal txs is not independently enforced today
   (§2.2: `expect_validity_code` bounds `0..5`, `codec.ak:25-29`; the claim
   machinery constrains only the descriptor, `validation-claim-v1.ak:315`;
   the forced twin is coupled by `forced_verdict_matches`, `:204-212`, with
   the §2.4.2(c) extension pending). *Recommendation:* the format wave makes
   the tx-leaf validity authoritative — or adds the Normal-tx consistency
   predicate (`coarse_bucket_of`-style, §2.4.2(b)) — at which point direction
   A drops the interim descriptor opening and binds the machine-free leaf
   directly. A header where leaf and descriptor *disagree* remains a
   distinct, cheap, single-transaction consistency fault; do not widen this
   thread to cover it.
7. **Catalogue id allocation and inventory drift.** Next free index after
   `0000000b`, but `catalogue.ts` is drifted (8 vs 11). *Recommendation:*
   allocate in the registration wave only, after the drift is reconciled;
   this document intentionally names no number.
8. **Fee-price assumptions.** §6's lovelace figures assume current mainnet
   execution prices; the pinned ledger pins ExUnits, not prices.
   *Recommendation:* restate fees at registration time; treat only the
   step-count arithmetic as durable.
9. **`prior_ledger_root` source — RESOLVED by the 2026-08-24 rework.** The
   thread now takes `prior_ledger_root` directly from the transition trace
   (`event_to_step_root` → `transition_trace_root` →
   `transition_step.pre_utxos_root`, the openings of
   `validation-claim-v1.ak:164-201` with the `:357-361` cross-checks; the
   claim machinery's own pin is `:407`), and opens no machine-state preimage
   at all. The old question — whether to cross-check the initial-state
   preimage's root against the trace — dissolved with the preimage itself: a
   header whose *machine states* lie about the pre-root is a
   validation-trace fault for the interactive family, and this thread's
   proof is a genuine contradiction within the header's own transition-trace
   commitments either way.
