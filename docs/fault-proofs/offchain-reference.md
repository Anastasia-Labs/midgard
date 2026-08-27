# Off-Chain Reference (TypeScript)

> Audited 2026-07-10 against branch `tx-validation` (HEAD `269bf6b3`) plus its
> contemporaneous working tree; reconstructed on clean base `55afdc54`. Code map for the
> TypeScript side: evidence construction, submission CLI, state correction, DA retrieval,
> and local validation. Historical `fraud-proof` source paths are preserved literally.
> Transition-trace CLI status was reconciled on 2026-08-26 against the current
> working tree; the broader audit provenance is unchanged.

## 1. Package roles

| Package                     | Role                                                                                                                                                                                                                                                         |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `demo/midgard-fault-proofs` | Evidence preparation + proof submission CLI + faulty-block removal. The challenger toolbox.                                                                                                                                                                  |
| `demo/midgard-sdk`          | Plutus-data codecs, contract builders (`fraud-proof/{catalogue,computation-threads,contracts,tokens,double-spend,invalid-range,non-existent-input,native}.ts`, `fraud-proofs/transition-trace.ts`), state-queue tx programs. Never touches MPF tries itself. |
| `demo/midgard-core`         | Frozen DA wire contract (`da-transport.ts`), payload envelope/compression/sizing, canonical plutus-data CBOR.                                                                                                                                                |
| `demo/da-committee-node`    | DA payload verification, attestation signing, retention store, libp2p payload + proof-artifact serving.                                                                                                                                                      |
| `demo/midgard-validation`   | Phase A/B local tx validation (mempool admission) — _not_ part of the L1 dispute path.                                                                                                                                                                       |
| `demo/midgard-node`         | Operator node; consumes validation; deploys contracts; hosts `/stateQueueMutationLease`; persists evidence-relevant data. Never imports `@al-ft/midgard-fault-proofs`.                                                                                       |
| `demo/midgard-watcher`      | Implemented ingestion, indexing, finality, rollback, and durable-state foundations; autonomous detection/proof/removal remains an acceptance gap.                                                                                                            |

## 2. CLI surface (`demo/midgard-fault-proofs/src/bin.ts`)

`prepare-double-spend` · `prepare-invalid-range` · `prepare-non-existent-input` · `prepare-zero-input` ·
`prepare-transition-trace` ·
`inspect-contracts` · `submit-init` · `submit-step-01..04` ·
`submit-invalid-range-step-01..02` · `submit-non-existent-input-step-01..04` ·
`submit-zero-input-step-01..02` · `submit-da-hash-preimage-step-01..02` ·
`submit-input-no-idx-step-01..04` (and `submit-input-no-idx-fold`) ·
`submit-transition-trace-proof` ·
`remove-fraudulent-block`.

`submit-init --fraud-category` accepts exactly eleven values — `doubleSpend |
nonExistentInput | nonExistentInputNoIndex | invalidRange | transitionTrace |
zeroInput | validationTraceDispute | daHashPreimage | noReferenceInput |
referenceInputNoIdx | invalidSignature` — matching the catalogue and
inspector. Transition-trace preparation accepts only a retained-DA envelope
pinned to the committed header hash. Submission strictly decodes canonical
`TransitionFaultProof` Data CBOR and resolves each repeatable
`--reference-input` from the live provider.

All `submit-*` commands hit a real L1 (Blockfrost/Kupmios via
`runtime.ts:makeLucidForSubmit`, `src/runtime.ts:99-159`) and sign/submit; all `prepare-*`
commands are offline.

## 3. Workflows per family

### double-spend (5 manual steps)

`prepare-double-spend` (`src/prepare-double-spend.ts` — 3 modes: live node
`GET /block`+`/tx` (`:180-247`), local JSON file (`:236,759`), hardcoded sample
(`:280-318`); builds MPF trie over block txs; emits `tx1/tx2-inclusion.json`,
`tx1/tx2-inputs.json`, `plan.json`) → `submit-init` → `submit-step-01` → `-02` →
`-03` (publishes tx1 spend-input reference witness via `spend-input-witness.ts`) →
`-04` (publishes tx2 witness, burns thread, **mints fault-proof token**).

### invalid-range (4 manual steps)

`prepare-invalid-range` (re-derives violation via `normalizeNativeTxValidityRange` /
`invalidRangeViolationReason`) → `submit-init --fault-category invalidRange` →
`submit-invalid-range-step-01` (re-derives the violation from on-chain header data) →
`-02` (mints token).

### non-existent-input (6 manual steps)

`prepare-non-existent-input` (node/file modes plus optional
`--prev-block-payload-file` — a **local** `DaPayloadV1` CBOR file, plain `readFile` at
`src/prepare-non-existent-input.ts:406`, not a live DA fetch; non-membership via empty
genesis trie or `reconstructDaPayloadV1` + `keyValuePhasNonMembershipProof`) →
`submit-init` → `ne-submit-step-01..04` (step-03 uses the `pexcludes` withdrawal carrier;
step-04 mints token).

### zero-input (4 manual steps)

`prepare-zero-input` reconstructs the raw native transaction trie from node/file input,
selects a transaction whose native spend-input list is empty, and emits the step-01
membership witness. `--expected-transactions-root` is mandatory: the preparer derives
the counted/domain-tagged root using the block transaction count and fails before
writing submit-ready artifacts if it differs from the authoritative header root.
`submit-init --fraud-category zeroInput` → `submit-zero-input-step-01` →
`submit-zero-input-step-02` concludes the thread and mints the fault-proof token.

### input-no-idx (Q13 lifecycle)

`prepare-input-no-idx` consumes canonical block evidence and emits the exact
inclusion/output artifacts → `submit-init --fraud-category
nonExistentInputNoIndex` → `submit-input-no-idx-step-01` → `-02` or resumable
`submit-input-no-idx-fold` → `-03` → `-04`, which concludes the thread and
mints the fault-proof token. The emulator lifecycle reaches faulty-block
removal. Family closure status is tracked individually in
[`catalogue-status.md`](catalogue-status.md).

### missing-signature (Q16 library lifecycle; pre-registration)

`src/missing-signature/` provides strict finding/evidence codecs, the resumable
Init → step-01 → step-02 → step-03 → step-04 proving core, cancellation, and
CLI/watcher adapters. Every validator spend uses a hash-checked reference-script
UTxO; the terminal step burns the computation thread and mints the fraud-proof
token. Emulator coverage drives both the core and direct submitters through
faulty-block removal, proves refusal for an honest commitment, and covers local
negative, cancel, and resume paths. Watcher detection distinguishes an absent
witness from an unknown verification-key preimage and a present-but-invalid
witness before emitting a finding. The family is intentionally absent from the
production CLI, catalogue, deployment manifest, and watcher `families[]` until
the registration wave; `0000000e` is used only by the isolated emulator harness.
Step-04 authenticates field 7 once per spend and advances a canonical,
thread-committed absence-walk checkpoint in 32-witness batches. The final batch
burns the thread and mints the permanent proof; cancellation and crash-resume
work from interior checkpoints. The envelope suite proves both the first
automatic tier-2 case (140 witnesses) and the maximum admissible field-7 vector
(318 witnesses) within the 13.2M-memory/8B-CPU transaction limits. It also
submits the worst-depth step-01 binding transaction under those limits. No
smaller off-chain evidence claim is used.

### transition-trace (3 manual commands; route→final is internal)

`transition-trace/detect.ts` (fault detection over reconstructed payloads),
`reconstruct.ts` (rebuild ledger/roots from raw DA payload CBOR and centralize direct
`SDK.decodeDaPayloadV1` use), `witnesses.ts` (PHAS membership/non-membership
builders), `phas.ts` (MPF root/proof library shared across families), `fetch.ts`
(`DaLibp2pRetainedDaSource` — the only real libp2p DA retrieval in the package,
hash-verifying every response), `submit.ts` (authenticated route→selected
final, thread burn, token mint). `prepare-transition-trace` reconstructs a
caller-pinned retained-DA envelope, writes each header-derivable proof plus an
auditable `plan.json`, and records explicit guidance for variants needing
external L1 or ledger evidence. `submit-transition-trace-proof` strictly reads
that proof and accepts repeatable live reference-input outrefs for L1-event
witnesses.

### Faulty-block removal (all families)

`src/remove-fraudulent-block.ts` (2483 lines; replaces the deleted
`remove-fraudulent-block.ts`): loops `RemoveFaultyBlocksLink` successor removals until the
faulty block is the queue tail, then `RemoveLastFaultyBlock` (`:2373-2422`), one L1 tx per
link; resolves the slashing plan (`resolveOperatorSlashingPlan`/`buildSlashingInputs`,
`:1397-1749`; active/retired/already-slashed) and updates operator lists + scheduler
datum in the same txs; non-tail removals require the node's admin-gated
`/stateQueueMutationLease` HTTP coordinator (`:2200-2223`). Underlying tx programs:
`demo/midgard-sdk/src/state-queue.ts:899-1009`.

## 4. Evidence construction internals

- **MPF proofs**: `@aiken-lang/merkle-patricia-forestry` `Store`/`Trie`, imported in
  exactly `src/ne-proofs.ts:1`, `src/inspect-contracts.ts:1`,
  `src/transition-trace/phas.ts:1`, `src/prepare-double-spend.ts:4`. Non-membership is
  simulated by inserting a throwaway empty-value entry and proving against the resulting
  trie (`ne-proofs.ts:8-57`) — the on-chain `pexcludes` reconstructs the original root.
- **Plutus-data CBOR**: `src/plutus-data-cbor.ts` is a dependency-free canonical CBOR
  parser/encoder (map keys sorted by encoded bytes, `:200-208`); `tx-layout.ts`,
  `spend-input-witness.ts`, `inspect-contracts.ts` use
  `@al-ft/midgard-core/plutus-data-cbor`'s `canonicalPlutusDataCbor`.
- **Reference witnesses**: `spend-input-witness.ts` publishes a UTxO carrying a faulty
  tx's full spend-input list to dodge tx-size limits (emulator-tested at 180 inputs,
  `tests/spend-input-witness.test.ts:32-68`).
- **On-chain position lookups** (`requireReferenceInputIndex`,
  `requireUniqueOutputIndex`, …) live in `demo/midgard-sdk/src/state-queue.ts`.
- **Blueprint consistency**: `inspect-contracts.ts` rebuilds all contract families from
  `onchain/aiken/plutus.json` via `buildFaultProofContracts` and cross-checks hashes +
  catalogue root/membership proofs against deployment-info JSON.
- **Data sources**: the three CLI families use live node REST or local files — none fetch
  from the DA layer at evidence-construction time; only `transition-trace/fetch.ts` does
  real libp2p retrieval.

## 5. DA retrieval & retention (evidence availability)

- **Wire contract** (`demo/midgard-core/src/da-transport.ts:62-72`): eight payload/proof
  libp2p protocols (plus a `capabilities` handshake) incl. `proof-bundle-by-header`,
  `trace-step-by-index`, `event-to-step-by-event`; limits `:27-35` (64 MiB payload, 1 MiB chunk, 15 s timeout,
  `minimumRetentionDays = 15` at `:34`); zstd envelope with pre-decompression size guard +
  sha256 (`da-payload-envelope.ts:237-313`).
- **Server**: chunked payload + metadata handlers
  (`demo/da-committee-node/src/da/libp2p/payload-protocols.ts:207-368,781-797`) on a real
  libp2p node (`DaLibp2pNode.ts:120-171,238-283`); proof artifacts derived and re-verified
  at request time (`src/da/proof-artifacts.ts:136-457`).
- **Client**: `DaLibp2pPayloadSource.fetchPayloadCandidates()` dials
  retrieval/bootstrap/committee peers, verifies every chunk hash and payload hash
  (`payload-source.ts:64-93,176-220,442-449`).
- **Retention (Q54 PASS)**: the derived 15-day deployment window is bound through the
  manifest, node/committee configuration, pruning predicate, readiness, and
  `retention-check`. The committee-store pruner remains deliberately inert and is a
  Q58/W-O7 residual, not an absence of retention enforcement.
- **Node DB**: raw tx CBOR in `immutable` (never pruned,
  `demo/midgard-node/src/database/immutable.ts:120-131`); payload bytes + roots in
  `da_payloads` are pruned only through the Q54-derived policy;
  `blocks` (header→tx_ids) deleted at merge and unordered
  (`TX_VALIDATION_TABLE_ROLES.md:91-95,150-151,168-175`) — after merge, DA payloads are
  the only ordered per-block evidence source.

## 6. Detection & classification (and their limits)

- **Phase A** (`demo/midgard-validation/src/phase-a.ts:338-460`): stateless per-tx —
  CBOR/tx-id/`TxIsValid`/aux-data/network-id/min-fee/input-set/interval-format/required-
  signers/ed25519/native-script/observer checks.
- **Phase B** (`phase-b.ts:1072-1265`): dependency DAG + conflict components; input
  existence, double-spend, reference resolution, script-material checks, full local
  Plutus/MidgardV1 execution with ex-units budgets (`local-script-eval.ts`),
  script-integrity-hash recomputation, value preservation
  (`value-accounting.ts:111-133`), slot-interval enforcement; cascade rejection; UTxO
  state patch output.
- **Classification**: 25-code `RejectCodes` (`src/types.ts:17-42`); four codes defined but
  never raised (`UnsupportedFieldNonEmpty`, `PlutusEvaluationUnavailable`,
  `CertificatesForbidden`, `NonZeroWithdrawal`). Rejections persist verbatim to
  `tx_admissions`/`tx_rejections`
  (`demo/midgard-node/src/database/txAdmissions.ts:1054-1153`).
- **Deliberately unmapped to fault categories**: admission rejects concern pre-block
  transactions; fault proofs concern committed blocks. Zero references to
  `RejectCode` in `demo/midgard-fault-proofs`. Consequence: nothing today classifies a
  _committed_ block's violation into a proof family — that selection is fully manual.
- **Watcher**: no autonomous detection loop anywhere
  (`demo/midgard-watcher/midgard-watcher-architecture.md:11-25` admits it; roadmap item 5
  `:414-431`; adversarial review verdict "No-go as a production-ready plan in its first
  draft", `watcher-plan-adversarial-review.md:22`).

## 7. Manual-effort summary

| Family             | Commands to conclude a proof                                                    | Then removal                    |
| ------------------ | ------------------------------------------------------------------------------- | ------------------------------- |
| double-spend       | 6 (prepare + init + 4 steps)                                                    | +1 per descendant link +1 final |
| invalid-range      | 4                                                                               | same                            |
| non-existent-input | 6                                                                               | same                            |
| zero-input         | 4 (prepare + init + 2 steps)                                                    | same                            |
| min-fee            | library-only pre-registration prepare + init + 2 steps; no registered CLI verbs | explicit-category removal       |
| transition-trace   | not possible via CLI (library calls only)                                       | same                            |
| Remaining families | atomic closure remains task-specific                                            | —                               |

Each command needs env/config (Blockfrost or Kupmios keys, deployment-info JSON, out-ref
plumbing between steps via JSON files). There is no single-command orchestration.
