# Native-script decoding fault: standalone computation-thread family (design v1)

Audit date: 2026-08-24. Branch: `wave/lane-o`. Issue: #633, direction (d).

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

The family covers **both** fault directions ruled in scope for #633:

- **Direction A — wrongful acceptance.** The header commits, under its
  `validation_traces_root`, an `Accepted` verdict for an L2 transaction, yet at
  least one output that transaction resolves (spend or reference input) carries
  a tag-0 reference-script payload whose bytes are not a canonical native
  script under the frozen scan semantics — bytes the same header's
  `prev_utxos_root` itself commits.
- **Direction B — wrongful rejection.** The header commits a `Rejected`
  verdict whose rejection code is one of the three scan-borne codes
  (`E_INVALID_FIELD_TYPE`, node-limit, depth-limit) attributed to the
  resolve-inputs phase, yet every output the transaction resolves is either
  not a tag-0 native reference script at all or scans to a canonical terminal.

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
   witnesses (transaction, validation-trace descriptor), the ledger-trie
   membership proof(s) for the resolved outpoint(s), the machine-state
   preimages, and the reference-script payload bytes with their chunk proofs.
3. **Init**: mint one computation-thread unit named
   `category_id(4B) ‖ header_hash(28B)`
   (`onchain/aiken/validators/computation-thread.ak:109-115`) at the step-01
   address with `StepDatum { fraud_prover, data: None }`.
4. **Steps**: step-01 binds the L2 transaction to the header; step-02 binds
   the committed verdict and opens the machine-state preimages; step-03 loops
   — binding an outpoint's descriptor and scanning its payload bytes under the
   pinned per-node budget, as many L1 transactions as needed; step-04
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
`onchain/aiken/lib/midgard/ledger-state.ak:58-84`.

### 2.1 Commitments used, per direction

Both directions:

- **`transactions_root` / `transaction_count`** (counted, domain-tagged;
  `commit_counted_root` at
  `onchain/aiken/lib/midgard/transition-trace.ak:67-81`): step-01 proves the
  faulted L2 transaction T is committed by the header, via
  `verify_native_tx_in_state_queue_node_with`
  (`onchain/aiken/lib/midgard/fraud-proofs/common.ak:792-847`; counted-root
  authentication at `common.ak:831-836`). The leaf value is T's
  `MidgardTxCompactV1` CBOR, which carries T's `validity_code`
  (`onchain/aiken/lib/midgard/fraud-proofs/native-tx/compact.ak:291-301`,
  `:381-390`).
- **`validation_traces_root` / `validation_trace_count`**: step-02 proves the
  header commits a `ValidationTraceDescriptorV1`
  (`onchain/aiken/lib/midgard/validation-trace-v1.ak:80-89`) for T, keyed by
  `EventKey` = `L2TransactionEventKey { tx_id }`
  (`onchain/aiken/lib/midgard/ledger-state.ak:553-558`), opened with
  `transition_trace.verify_root_membership_with_bytes(witness,
  ValidationTracesRootDomain, header.validation_traces_root,
  header.validation_trace_count, cbor(key), cbor(value))` — exactly the
  opening the claim machinery already performs
  (`onchain/aiken/lib/midgard/validation-claim-v1.ak:154-161`;
  domain at `onchain/aiken/lib/midgard/transition-trace.ak:16`).
- **Descriptor state hashes**: the descriptor's `initial_state_hash` and
  `terminal_state_hash` bind full `ValidationMachineStateV1` preimages
  (`validation-trace-v1.ak:62-78`) under the canonical encoding
  `encode_machine_state` (`validation-trace-v1.ak:191-216`) and domain-tagged
  hash (`:218-222`). Opening the initial state yields
  `prior_ledger_root` — the pre-state ledger root the validation machine ran
  against — which the claim machinery pins to the transition trace's
  `pre_utxos_root` (`validation-claim-v1.ak:407`) and which
  `immutable_context_matches` holds constant to the terminal state
  (`validation-claim-v1.ak:135-145`). The thread uses the initial-state
  preimage as its source of `prior_ledger_root` (see §9 Q9 for why it does
  not independently re-derive it from the transition trace).
- **Ledger trie (pre-state)**: for a resolved outpoint K, membership of the
  leaf `key = cbor(K)`, `value = cbor(LedgerOutputCommitmentV1)` under
  `prior_ledger_root` — the exact check the validation machine itself performs
  (`onchain/aiken/lib/midgard/validation-machine-v1.ak:6398-6407`). The
  descriptor value (`onchain/aiken/lib/midgard/ledger-output-commitment-v1.ak:31-48`)
  carries the fields the scan needs: `reference_script_language` (-1 / 0 / 3 /
  128), `reference_script_offset` / `total_length`, and the 32-byte
  `item_commitment` chunk commitment over the full output bytes (4095-byte
  chunks, `onchain/aiken/lib/midgard/bounded-item-v1.ak:12`).

Direction A additionally:

- descriptor `verdict == Accepted`.
- T's resolved-outpoint sets, opened from T's own committed bytes through the
  §8.8 field-opening door: `spend_inputs_field_index = 0`,
  `reference_inputs_field_index = 1`
  (`onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.ak:102-118`),
  fixed 38-byte stride reads via `native_tx_machine_walk_v1.spend_input_at`
  (`onchain/aiken/lib/midgard/native-tx-machine-walk-v1.ak:532`).

Direction B additionally:

- descriptor `verdict == Rejected` and `rejection_code_hash` equal to
  `hash_rejection_code` (`validation-trace-v1.ak:239-243`) of one of the three
  scan-borne codes (`validation-machine-v1.ak:1190-1195`).
- the **terminal**-state preimage, to read `phase`. This is load-bearing:
  `reject_invalid_field_type` is also emitted from CanonicalDecode
  (`validation-machine-v1.ak:~1620-1627`, `~1935-1943`) and
  PhaseANativeScripts (`~3878-3886`, `~4041`), and the limit codes also from
  PhaseANativeScripts (`~4049`). The code hash alone therefore **cannot**
  attribute a rejection to the resolve-inputs scan; the thread requires
  `terminal.phase == ResolveInputs` (phase code 7,
  `validation-trace-v1.ak:33-49`) and
  `terminal.rejection_code_hash == descriptor.rejection_code_hash`.

### 2.2 Honest OPEN markers

- **OPEN (B-1): no which-outpoint commitment.** Neither the descriptor nor the
  terminal machine state commits *which* resolved outpoint the rejection was
  charged to. `ValidationMachineStateV1` has no "pending outpoint" field
  surviving to the terminal hash in a form this thread can open without
  re-paying the interactive control binding (~13–14M mem,
  `architecture.md:89-93`) on the penultimate state. Direction B must
  therefore universally quantify over **all** resolved outpoints (§7.6, §9
  Q1).
- **OPEN (B-2): descriptor `total_length` is uncapped.** The descriptor
  well-formedness predicate only requires `total_length >= 0`
  (`ledger-output-commitment-v1.ak:113-133`); nothing in the leaf format caps
  a reference script's `total_length` at 16,384. The bounds-unreachability
  shortcut in §2.3 is therefore conditional, and the thread must scan rather
  than assume (§9 Q1 recommendation folds a cap into the same amendment).
- **OPEN (B-3): schedule of resolution.** The order in which the machine
  resolves outpoints (spend inputs field 0, then reference inputs field 1)
  determines *which* outpoint a genuine limit rejection would be charged to;
  direction B sidesteps schedule dependence entirely by quantifying over all
  of them, at the cost measured in §6.

### 2.3 Bounds-unreachability lemma (context, not a shortcut)

The maximum reference-script payload reachable through committed L2 outputs is
16,341 bytes = 5,447 nodes (pinned ledger,
`native-script-scan-exec-ledger-v1.json`), and 5,447 < 16,384. So *within the
byte caps* the node-limit and depth-limit codes are wrongful per se. But
because of OPEN (B-2) the thread cannot conclude from the codes alone; it
proves canonicity by scanning, and this lemma only explains why honest blocks
never produce those codes from ResolveInputs.

---

## 3. Contract set

Four spending validators plus reuse of the existing generic machinery. Names
follow the double-spend family's convention.

| # | Validator (new) | Role |
|---|---|---|
| 1 | `validators/fraud-proofs/native-script-decoding/step-01.ak` | Bind the faulted transaction T to the header |
| 2 | `validators/fraud-proofs/native-script-decoding/step-02.ak` | Bind the committed verdict; open machine-state preimages; branch on direction |
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
either its own script address (next arm / next outpoint) or step-04's. A
validator knows its own hash from `own_out_ref` resolution, so the self-loop
needs no self-parameter; only the forward edge to step-04 is a parameter.
The chain 01→02→03→(03)*→04 is acyclic in parameters.

### 3.2 Redeemer arms

Every step carries `ct.Cancel` via `common.cancel`
(`common.ak:437-481`), exactly as
`resolve-inputs-membership-step-semantic-v1.ak:37-45` and the double-spend
steps do. The Continue arms:

- **step-01** `Continue(NativeTxInclusionCarriage)`: verbatim reuse of
  `pass_native_tx_to_next_step_carried` (`common.ak:149-252`) including the
  published-chunk carriage duality (#545), as in `double-spend/step-01.ak:57-94`.
  Output state: `{ verified_tx_id }` at step-02's address. The compact leaf's
  `validity_code` is *not* trusted for the verdict — the descriptor is the
  verdict authority (§9 Q6).
- **step-02** `Continue(BindVerdict { descriptor_membership,
  initial_state_preimage, terminal_state_preimage, direction })`:
  1. `common.continue` (`common.ak:501-577`) for thread-token conservation.
  2. Descriptor membership as in §2.1 (counted, domain
     `ValidationTracesRootDomain`, against the header the thread NFT names —
     the header is re-derivable because the asset name carries
     `header_hash`; the header body rides the redeemer and is checked against
     that hash, the same binding Init used).
  3. `hash_machine_state(initial_preimage) == descriptor.initial_state_hash`,
     same for terminal; `immutable_context_matches`-style equality of the
     immutable fields (`validation-claim-v1.ak:135-145`).
  4. `initial.transaction_id == verified_tx_id` and `descriptor key ==
     L2TransactionEventKey { verified_tx_id }`.
  5. Direction A: `descriptor.verdict == Accepted`. Direction B:
     `descriptor.verdict == Rejected`, `terminal.phase == ResolveInputs`,
     `terminal.rejection_code_hash == descriptor.rejection_code_hash`, and
     the code hash ∈ the three scan-code hashes.
  6. Output state: the §4 schema with `prior_ledger_root :=
     initial.prior_ledger_root`, cursor at the first outpoint.
- **step-03**, three Continue arms:
  - `BindOutpoint { field_opening, outpoint_index, ledger_membership,
    descriptor_bytes }`: open T's field 0 or 1 through the §8.8 door
    (`opened_field_view` + the 38-byte stride read, as
    `double-spend/step-03.ak:78-92`), read outpoint K at the cursor index;
    prove `mpf.has(prior_ledger_root, cbor(K), descriptor_bytes, proof)`
    (mirroring `validation-machine-v1.ak:6398-6407`); decode the descriptor
    (`ledger-output-commitment-v1.ak:162`) and check
    `descriptor_is_well_formed` (`:113-133`). If
    `reference_script_language != 0` (not tag-0 native): direction B advances
    the cursor (this outpoint is vacuously fine); direction A **fails** the
    arm (the prover picked the wrong K — direction A needs exactly one
    faulting K). If tag-0: initialize the inner machine with
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
      `ledger-output-proof-v1.ak:1286` / descriptor exactness `:1336`); then
      advance the cursor to the next outpoint (back to `BindOutpoint`), or,
      after the last outpoint (cursor == spend_count + reference_count, both
      counts read once from the opened field views and frozen into state),
      hand off to step-04.
- **step-04** `Continue(Finalize { … })`: `common.finalize`
  (`common.ak:579-673`) exactly as `double-spend/step-04.ak:53-72`; re-check
  the carried terminal marker (direction A: refusal recorded; direction B:
  cursor exhausted) and mint the permanent token.

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
  verified_tx_id: ByteArray,         // 32B, from step-01's counted-root binding
  verdict_code_hash: ByteArray,      // 32B; direction B: the committed rejection code hash
                                     // direction A: no_rejection_code_hash
                                     // (validation-trace-v1.ak:132-133)
  prior_ledger_root: ByteArray,      // 32B, from the opened initial-state preimage
  spend_count: Int,                  // frozen when field 0 is first opened (≤ 296)
  reference_count: Int,              // frozen when field 1 is first opened (≤ 819)
  // -- cursor (replay protection) --
  outpoint_cursor: Int,              // next outpoint ordinal in [0, spend_count + reference_count)
  // -- current outpoint (meaningful only mid-scan) --
  outpoint_key_hash: ByteArray,      // 32B blake2b_256(cbor(K)); binds BindOutpoint → Scan
  reference_script_language: Int,    // from the bound descriptor: -1 | 0 | 3 | 128
  output_index: Int,
  total_length: Int,                 // descriptor.reference_script_total_length
  item_commitment: ByteArray,        // 32B; the byte-authentication anchor (§5)
  // -- inner machine --
  machine_state_hash: ByteArray,     // 32B blake2b_256(domain ‖ encode_control_v1(control)),
                                     // or a sentinel between outpoints
  // -- direction-A terminal marker --
  refusal_class: Int,                // -1 until a refusal is proven; then the class
}
```

Every field is fixed-width or a bounded `Int`; the encoded datum is
constant-size (< 300 bytes) regardless of payload size or outpoint count.

**Cursor replay protection.** `outpoint_cursor` is strictly monotone: the
`Verdict` arm is the only arm that may increment it, and only by exactly 1,
and only from a state whose `machine_state_hash` is at an exact terminal.
`BindOutpoint` may only fire when `machine_state_hash` is the between-outpoints
sentinel, and must bind the outpoint at exactly `outpoint_cursor` (the 38-byte
stride read is positional, so the index *is* the identity —
`double-spend/step-03.ak:89-92` precedent). A prover cannot re-scan an
already-passed outpoint, skip one, or substitute one: the ordinal is the key,
`outpoint_key_hash` pins K across the Bind→Scan→Verdict arc, and
`item_commitment` pins the bytes (§5). `machine_state_hash` carries the inner
cursor between L1 transactions exactly as the pushdown template's
`script_digest`-protected cursor does
(`native-tx-script-pushdown-v1.ak:643-654`), and the frozen control is already
constant-size (`NativeScriptStructureControlV1`: version, stage, start_offset,
cursor, end_offset, stack_root hash-chain, stack_depth, node_count).

**Direction A** freezes `outpoint_cursor` at the prover-chosen single K (the
cursor still names it positionally; no iteration over the rest is needed —
one refusal contradicts `Accepted`).

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
`prior_ledger_root`, which the opened initial-state preimage chains to the
descriptor, which the counted root chains to the header the thread NFT names.
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
- plus binding/structure overhead: step-01, step-02, one `BindOpen` per
  outpoint, `Verdict` arms, step-04 — call it ~10 more for direction A
  (one outpoint). **Direction A worst case ≈ 100 L1 transactions.**

**Fee per transaction** (mainnet price assumption, stated as an assumption,
not pinned): mem price 0.0577 lovelace/unit ⇒ 13.2M mem ≈ 761,640 lovelace;
cpu 0.0000721 ⇒ ~4.4B cpu ≈ 317,240 lovelace; size ~161.4 lovelace/byte at a
full 16KB ≈ 375,381 lovelace incl. the 155,381 base. **≈ 1.45 ADA per step
transaction.** Direction A worst case ≈ 100 × 1.45 ≈ **145 ADA** (~130–150
ADA band across deep/wide mixes) — under 0.2% of the 75,000-ADA
`fraud_prover_reward` production profile (`architecture.md:264-289`).

**Direction B worst case** multiplies by the outpoint count: ≤ 296 spend +
≤ 819 reference = ≤ 1,115 resolved outpoints
(`docs/spec/midgard-tx.md:363-375`), each up to 5,447 nodes ⇒ up to ~100 ×
1,115 ≈ **~110,000 L1 transactions ≈ 160,000 ADA**, and at one
script-saturated transaction per block (~20s) ≈ **~5.8 days of 100% chain
script capacity** — which **fails** the half-of-seven-days maturity fit rule
(`architecture.md:283-286`) at the adversarial extreme. This is the design's
headline open problem (§9 Q1). The *typical* direction-B case (few resolved
outpoints, small scripts) fits trivially: a 10-outpoint transaction with
1KB payloads is ~40 transactions ≈ 1 hour ≈ 60 ADA. Direction A always fits:
~100 transactions ≈ 33 minutes at 1 tx/block.

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

Covered in §4: strict monotone `outpoint_cursor`, arm-gating on the
between-outpoints sentinel, positional binding of K to the cursor ordinal, and
the inner `machine_state_hash` chain. A prover replaying an old redeemer
against a new state fails the hash chain; replaying an old *state* is
impossible because the thread is a single linear UTxO (the token conservation
check admits exactly one continuation).

### 7.5 Payload substitution

The only bytes the scan ever consumes are chunk windows authenticated against
`item_commitment` (§5), which is bound through descriptor → ledger-trie leaf →
`prior_ledger_root` → initial-state preimage → descriptor hash →
`validation_traces_root` → header → thread NFT asset name. Substituting bytes,
descriptors, outpoints, transactions, or headers each breaks a distinct link
in that chain. The control-bytes canonicity re-encode
(`decode_control_v1` re-encoding, `ledger-output-proof-v1.ak:445`) blocks
non-canonical control encodings from aliasing a different machine state under
the same hash.

### 7.6 Wrongful-rejection edge cases

- **Attribution ambiguity** (§2.1): required `terminal.phase ==
  ResolveInputs`; a CanonicalDecode or PhaseA rejection with the same code
  cannot be attacked by this family — correctly, since those rejections may
  be legitimate for other reasons. The residual (a transaction genuinely
  invalid *elsewhere* whose trace nonetheless records a wrong scan code from
  ResolveInputs against canonical payloads) **is** covered: direction B
  proves every resolved payload canonical, contradicting the recorded
  terminal regardless of what a hypothetical honest run would have rejected
  later — the fault proven is "this committed terminal is not the machine's
  terminal", which is exactly a header fault. What direction B does *not*
  cover is a rejection recorded with a scan code but a **non**-ResolveInputs
  phase; that is a trace-detail fault for the interactive family, out of
  scope here (state assumptions only, per the no-trap residual ruling,
  `architecture.md:111-121`).
- **Maturity-window fit**: direction A always fits (~33 min worst case).
  Direction B fits in all but adversarially-constructed extremes (§6); the
  extreme fails the half-maturity rule and is OPEN (§9 Q1). Until resolved,
  the family is *sound but not complete* in direction B: no false proof can
  ever finalize, but a sufficiently expensive genuine fault might not be
  provable in time by this thread alone.
- **Non-native reference scripts**: language codes 3 / 128 / -1 descriptors
  are skipped by descriptor field alone — the scan never runs, so no
  Plutus-script bytes can be dressed up as a scan target, and conversely a
  prover cannot skip a tag-0 payload by lying about the language (the
  descriptor is trie-authenticated, not prover-supplied).

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

1. **Direction-B outpoint attribution and maturity fit (headline).** Without
   a which-outpoint commitment (OPEN B-1) direction B quantifies over all
   ≤ 1,115 resolved outpoints and fails the half-maturity fit at the
   adversarial extreme (§6). *Recommendation:* amend the commitment format —
   fold the offending outpoint ordinal (or resolution-schedule index) and a
   16,384 cap on reference-script `total_length` (OPEN B-2) into the
   rejection commitment (descriptor or terminal state) at the next format
   version. With that, direction B binds one outpoint and inherits direction
   A's ~100-transaction bound. Until then, ship direction B as sound-but-
   incomplete with the extreme documented.
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
   PhaseANativeScripts (`validation-machine-v1.ak:~3878-4049`); those bytes
   are committed differently (witness-set compact CBOR, not ledger
   descriptors). *Recommendation:* a twin family sharing the engine but with
   a witness-set byte-authentication front end; out of scope here.
5. **Refusal-class fidelity.** The thread's `Verdict` arm preserves the
   staged machine's three-way distinction (invalid / node-limit / depth-limit,
   `validation-machine-v1.ak:6458-6484`) in `refusal_class`.
   *Recommendation:* keep it — it costs one `Int` and makes the minted proof
   auditable against the descriptor's code without re-running anything.
6. **`validity_code` ↔ descriptor-verdict consistency.** The compact leaf's
   `validity_code` and the descriptor's verdict are two commitments to "the
   same" judgment; the claim machinery ties them (`forced_verdict_matches`,
   `validation-claim-v1.ak:204-213`) but this thread trusts only the
   descriptor. A header where they *disagree* is a distinct fault.
   *Recommendation:* leave to a (cheap, single-transaction) consistency
   family; do not widen this thread.
7. **Catalogue id allocation and inventory drift.** Next free index after
   `0000000b`, but `catalogue.ts` is drifted (8 vs 11). *Recommendation:*
   allocate in the registration wave only, after the drift is reconciled;
   this document intentionally names no number.
8. **Fee-price assumptions.** §6's lovelace figures assume current mainnet
   execution prices; the pinned ledger pins ExUnits, not prices.
   *Recommendation:* restate fees at registration time; treat only the
   step-count arithmetic as durable.
9. **Independent `prior_ledger_root` cross-check.** The thread takes
   `prior_ledger_root` from the opened initial-state preimage; it does not
   independently open the transition trace to confirm
   `initial.prior_ledger_root == transition_step.pre_utxos_root`
   (`validation-claim-v1.ak:407`). *Recommendation:* do not add the
   cross-check — a header whose initial state lies about its pre-root is a
   transition-trace fault with its own families, and the thread's proof is
   already a genuine contradiction *within* the header's own commitments
   either way.
