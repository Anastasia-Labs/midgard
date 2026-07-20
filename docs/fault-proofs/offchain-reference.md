# Off-Chain Reference (TypeScript)

> Audited 2026-07-10 against branch `tx-validation` (HEAD `269bf6b3`) plus its
> contemporaneous working tree; reconstructed on clean base `55afdc54`. Code map for the
> TypeScript side: evidence construction, submission CLI, state correction, DA retrieval,
> and local validation. Historical `fraud-proof` source paths are preserved literally.

## 1. Package roles

| Package                     | Role                                                                                                                                                                                                                                                         |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `demo/midgard-fault-proofs` | Evidence preparation + proof submission CLI + faulty-block removal. The challenger toolbox.                                                                                                                                                                  |
| `demo/midgard-sdk`          | Plutus-data codecs, contract builders (`fraud-proof/{catalogue,computation-threads,contracts,tokens,double-spend,invalid-range,non-existent-input,native}.ts`, `fraud-proofs/transition-trace.ts`), state-queue tx programs. Never touches MPF tries itself. |
| `demo/midgard-core`         | Frozen DA wire contract (`da-transport.ts`), payload envelope/compression/sizing, canonical plutus-data CBOR.                                                                                                                                                |
| `demo/da-committee-node`    | DA payload verification, attestation signing, retention store, libp2p payload + proof-artifact serving.                                                                                                                                                      |
| `demo/midgard-validation`   | Phase A/B local tx validation (mempool admission) — _not_ part of the L1 dispute path.                                                                                                                                                                       |
| `demo/midgard-node`         | Operator node; consumes validation; deploys contracts; hosts `/stateQueueMutationLease`; persists evidence-relevant data. Never imports `@al-ft/midgard-fault-proofs`.                                                                                       |
| `demo/midgard-watcher`      | **Docs only** (`midgard-watcher-architecture.md`, `watcher-plan-adversarial-review.md`). Zero code.                                                                                                                                                          |

## 2. CLI surface (`demo/midgard-fault-proofs/src/bin.ts:441-462`)

`prepare-double-spend` · `prepare-invalid-range` · `prepare-non-existent-input` ·
`inspect-contracts` · `submit-init` · `submit-step-01..04` ·
`submit-invalid-range-step-01..02` · `submit-non-existent-input-step-01..04` ·
`remove-fraudulent-block`.

`submit-init --fault-category` accepts only four values — `doubleSpend | nonExistentInput
| invalidRange | transitionTrace` (`bin.ts:96,107,117-126`; `nonExistentInputNoIndex` is
registered in the catalogue but rejected by the CLI parser) — and
**no transition-trace proof-submission command exists** — `bin.ts` never imports
`./transition-trace/`; `submitTransitionTraceProofFromFiles`
(`src/transition-trace/submit.ts:421-439`) is library-only.

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
`--prev-block-payload-file` — a **local** `DaPayloadV2` CBOR file, plain `readFile` at
`src/prepare-non-existent-input.ts:406`, not a live DA fetch; non-membership via empty
genesis trie or `reconstructDaPayloadV2` + `keyValuePhasNonMembershipProof`) →
`submit-init` → `ne-submit-step-01..04` (step-03 uses the `pexcludes` withdrawal carrier;
step-04 mints token).

### transition-trace (library-only)

`transition-trace/detect.ts` (fault detection over reconstructed payloads),
`reconstruct.ts` (rebuild ledger/roots from raw DA payload CBOR and centralize direct
`SDK.decodeDaPayloadV2` use), `witnesses.ts` (PHAS membership/non-membership
builders), `phas.ts` (MPF root/proof library shared across families), `fetch.ts`
(`DaLibp2pRetainedDaSource` — the only real libp2p DA retrieval in the package,
hash-verifying every response), `submit.ts` (terminal step: burns thread, mints token).
No `prepare-transition-trace` and no submit CLI.

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
- **Committee store**: single JSON file, atomic tmp+rename
  (`src/store.ts:87-371`); Postgres variant exists; **no delete/expiry capability**
  (`WatcherStore` interface `store.ts:43-85`). The 14-day retention promise is
  documentation, not code
  (`demo/da-committee-node/docs/da-committee-node-architecture.md:72,166-167,199`).
- **Node DB**: raw tx CBOR in `immutable` (never pruned,
  `demo/midgard-node/src/database/immutable.ts:120-131`); payload bytes + roots in
  `da_payloads` (prunable — hourly sweeper, off unless `RETENTION_DAYS > 0`, floor 8 days:
  `src/fibers/retention-sweeper.ts:24-61`, `src/database/retention-policy.ts:3-27`);
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

| Family             | Commands to conclude a proof                   | Then removal                    |
| ------------------ | ---------------------------------------------- | ------------------------------- |
| double-spend       | 5 (prepare + init + 4 steps)                   | +1 per descendant link +1 final |
| invalid-range      | 4                                              | same                            |
| non-existent-input | 6                                              | same                            |
| transition-trace   | not possible via CLI (library calls only)      | same                            |
| other 8 types      | not possible (no tooling; 7 also unregistered) | —                               |

Each command needs env/config (Blockfrost or Kupmios keys, deployment-info JSON, out-ref
plumbing between steps via JSON files). There is no single-command orchestration.
