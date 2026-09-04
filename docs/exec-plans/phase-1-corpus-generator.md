# ExecPlan Phase 1 — Transaction Corpus Generator (`stress-corpus-generate`)

**Status:** Locally implemented (2026-07-08): WB1-WB7 code paths landed in
`demo/midgard-node` with focused unit coverage and package build evidence.
Scale rehearsal, full offline corpus evidence, and live 100-row node acceptance
landed 2026-07-09. Remaining evidence: live 4,096-wallet fanout funding. Live
attach now uses the existing deployment manifest to match deployed
reference-script hashes; the current live fanout blocker is scheduler refresh
failure during block commitment, which leaves the top-up deposit projected but
not yet spendable in the L2 ledger.

**Effort:** ~1 week (1 engineer)
**Dependencies:** None to start (Phase 0 independent). **Downstream consumers:** Phase 2 canonical engine (which additionally requires the manifest + chain-index sidecars specified in §5.5) and the existing `stress-open-loop.ts` path.
**Parent plan:** `STRESS-BENCHMARK-EXECUTION-PLAN.md` §Phase 1. Companion: `THROUGHPUT-2500-TPS-PLAN.md`.

---

## 1. Objective

A new CLI command `stress-corpus-generate` in `demo/midgard-node` that pre-builds and pre-signs a corpus of valid Midgard L2 transactions as NDJSON `OpenLoopCorpusRow` files, sized for a 2,500 TPS × 10 min run (~1.65 M txs), built entirely offline (no `/utxos` round-trips), parallelized across `worker_threads`. Plus wallet automation (`stress-wallets:fanout`) that funds thousands of wallets from one treasury without per-wallet L1 deposits.

## 2. Verified current state (all refs verified 2026-07-08)

**Corpus consumer contract** — `src/commands/stress-open-loop.ts`:

- `OpenLoopCorpusRow` (lines 10–21): `txHash` (64-hex), `canonicalCborHex`, `canonicalCborSha256` (verified against bytes at parse time, lines 173–182), `canonicalCborByteLength`, `senderWalletId`, `selectedInputOutref` (`"txHash#index"`), `outputOutrefs[]`, `planShape: "fanout"|"chain"|"mixed"`, `parentTxHash|null`, `corpusSliceId`.
- `planOpenLoopCorpus` (227–281): filters rows by `planShape` + `corpusSliceId`; requires `ceil(rate×duration/1000)+warmup+cooldown` matching rows; takes them **in file order**; the only cross-row invariant enforced is `selectedInputOutref` uniqueness (264–273). **Parent-before-child ordering is entirely the generator's responsibility** — `parentTxHash` is carried but never checked, and `runOpenLoopSubmitter` (361–497) dispatches strictly in file order on a fixed-rate clock bounded only by `maxInFlight`; it will happily submit a child before its parent is accepted.
- Phase 2's engine (`throughput-valid-stress.mjs`) adds a one-in-flight-per-chain cursor; file-order-per-chain is still the required invariant there too.

**Offline deterministic build is feasible** — `src/commands/submit-l2-transfer.ts`:

- `makeStaticMidgardProvider` (271–324) already implements a `MidgardProvider` over an in-memory UTxO array with stubbed `submitTx` — zero network I/O. `makeTransferMidgard` (239–269) wires it to `LucidMidgard.new(provider,…).selectWallet.fromPrivateKey(signer, senderAddress)`.
- `buildTransferTxWithMinFee` (547–596): `.newTx().pay.ToAddress(dest, assets).complete({changeAddress, feePolicy:"provider"}).sign()`.
- Determinism chain: input selection is sorted+greedy (`selectDeterministicInputs`, `lucid-midgard/src/builder/balancing.ts:226-251`); outputs are builder-call-order with change appended last → **for a single-recipient transfer, output 0 = destination, output 1 = change, always** (`addChangeOutput`, balancing.ts:269-289); fee = `max(minFeeA×signedBytes + minFeeB, minimumFee)` via fixed-point iteration (balancing.ts:368-371), with `MIN_FEE_A`/`MIN_FEE_B` from env (`services/config.ts:270-277`, defaults `"0"`; deployed env produces the observed ~3,110-lovelace fee); `txId = computeMidgardNativeTxId(tx)` is a pure function (`lucid-midgard/src/builder.ts:502-503, 906-907`), and the node **echoes the same txId back** on `/submit` (`submit-l2-transfer.ts:678-682`) — a built-in canary for encoding drift.
- Therefore: a parent's change out-ref (`parentTxId#1`) and amount are computable **before submission**, so chains of arbitrary depth can be built offline by feeding each locally computed change output into the next static provider instance.

**Worker-thread precedent** — `src/fibers/block-commitment.ts:568-633` (Effect.async bridge: `new Worker(resolveWorkerEntry(import.meta.url, "<name>.js"), {workerData})`, message/error/exit handlers, terminate-on-interrupt) + `src/fibers/resolve-worker-entry.ts` (dist-flat path resolution) + worker bootstrap guard `if (parentPort !== null)` (`src/workers/commit-block-header.ts:1000-1024`). Build constraint: `package.json` `"build": "tsup src/index.ts … && tsup src/workers/* …"` — **a new worker must live directly at `src/workers/<name>.ts`** to be emitted as `dist/<name>.js`.

**Wallet tooling** — `src/commands/stress-wallets.ts`: `createL2Wallets` (606–643) writes `.stress-wallets/wallet-NNNN.json` records (`STRESS_WALLET_SCHEMA_VERSION="midgard-stress-wallet-v1"`, seeds via bip39 `generateMnemonic(256)`, env name `STRESS_WALLET_SEED_PHRASE_0001` zero-padded 4 digits, lines 250–257) plus `stress-wallets.env`/`.args` exports (487–512). `prepareStressWallets` (693–897) funds via **one sequential L1 deposit per wallet** (`for` loop, 757–771; `submit-deposit` flow in `src/transactions/submit-deposit.ts:408-488`) then projection + `/utxos` polling — this does not scale to thousands of wallets and is replaced for corpus use by §5.7.

- CLI registration: Commander in `src/index.ts` (`program.command(...)`, e.g. stress-wallets:prepare at 703–904).

## 3. Design decisions

**D1 — Reuse the real builder offline (no hand-rolled encoding).** The generator calls the same `LucidMidgard` pipeline via a synthetic `MidgardProvider` per (wallet, chain-step), returning exactly one UTxO — the intended input — sidestepping the coverage-selection ambiguity flagged in research (selection sees all provided UTxOs; giving it one makes selection trivial and provably deterministic). Encoding drift is impossible by construction; the node's txId echo is the runtime canary.

**D2 — Workload shape: N wallets × depth-D self-transfer chains.** Each wallet chains `pay.ToAddress(self, amount)` transfers, spending its own change (`prevTxId#1`; step 0 spends the funding UTxO). Self-transfers keep funding requirements minimal (only fees burn down), keep the wallet set closed, and exercise the full validation path. Template hooks (recipient rotation, multi-output) are extension points for Phase 5's mixed workload, not in scope here.

**D3 — Sizing rule (the chain-capacity math).** The binding constraint is the Phase 2 engine's one-in-flight-per-chain cursor: offered TPS ≤ N ÷ per-tx cycle latency. Cycle latency is bounded below by submit latency (p99 ≈ 750 ms observed) and, when child validity requires parent acceptance, by acceptance latency (~1 s at load). N ≥ 2,500 × 1 s = 2,500; with ~1.6× safety ⇒ **N = 4,096 wallets** (default), D = ⌈1,650,000 ÷ 4,096⌉ = **403** ⇒ 1,650,688 rows.

- Per-wallet funding: `(D + 1) × working amount + D × fee` because each step
  spends the previous change output while leaving the explicit self-transfer
  output unspent. At D=403, amount=1,000,000, fee≈3,110, this is ≈ **405.25 M
  lovelace** per wallet ⇒ total ≈ 1.66 M ADA from treasury (parameterized;
  computed and printed by the planner before generation).

**D4 — Emission order: grouped by chain (contiguous chain runs), matching the Phase 2 engine contract.** Rows are written chain 0 steps 0…D−1, then chain 1 steps 0…D−1, etc. This gives each chain a contiguous byte range so the Phase 2 engine can open one `fs.createReadStream({start, end})` per chain with bounded read-ahead (see `phase-2-canonical-engine.md` §2.1); per-chain file order preserves parent-before-child within each chain, and the engine's one-in-flight-per-chain cursor handles cross-chain scheduling. _(Reviewed decision: an earlier draft specified round-robin interleaving to serve the legacy `stress-open-loop.ts` file-order submitter; rejected because Phase 2 retires that submitter from runtime, and grouped-by-chain is required by the canonical engine's index-seeking reader. When the legacy in-process submitter is used against a grouped corpus — e.g. the Phase 1 integration test, before Phase 2 lands — run it with `maxInFlight=1` and note that the node's Phase B validator builds an intra-batch dependency graph, so a child admitted in the same validation batch as its parent validates correctly.)_

**D5 — Sidecars per the Phase 2 contract: `<corpus>.manifest.json` + `<corpus>.index.ndjson`.** Index: one line per contiguous chain run — `{corpusSliceId, planShape, chainId, startByteOffset, endByteOffset, rowCount}` (exactly the shape `phase-2-canonical-engine.md` §2.1 specifies), emitted during the streaming write, O(chains) not O(rows). Manifest: Phase 2's required fields (`targetRateTps, durationMs, chainCount, chainDepth, corpusShape, corpusSliceIds, generatedAtIso, generatorGitSha`) **plus** the generation fingerprint this plan requires — lucid-midgard version, `MIN_FEE_A`/`MIN_FEE_B` used, network/networkId, `maxSubmitTxCborBytes`, amount template, per-file sha256, funding summary. A 1.65 M-row corpus at ~1–2 KB/row is 2–3 GB and must never be whole-file `readFile`'d; these sidecars are what make streaming consumption possible.

**D6 — Protocol parameters are inputs, never hardcoded.** Generator resolves `MIN_FEE_A`/`MIN_FEE_B`/network/`MAX_SUBMIT_TX_CBOR_BYTES` from env/node `/protocol-info` at generation time and stamps them into the manifest. (Research flagged `makeStaticMidgardProvider` hardcoding `maxSubmitTxCborBytes: 32768` at `submit-l2-transfer.ts:303` — our provider factory takes it as a parameter.) A corpus is only valid against a node running the same fee params; the Phase 2 engine must compare manifest vs live node config preflight and refuse on mismatch.

**D7 — Funding via L2 fan-out tree, one L1 deposit total.** One treasury wallet receives a single L1 deposit; funding then fans out with L2 transfers in a tree of branching factor k per level (k single-recipient txs per parent if multi-output is unavailable; one k-output tx per parent if verified available — see WB0). Depth for 4,096 wallets at k=16: 3 levels, ≈ 273 + 4,096 intermediate txs, submitted through the normal `/submit` path with bounded concurrency — minutes, not the hours the per-wallet-L1-deposit path would take.

## 4. Open questions to resolve first (WB0 — day 1)

1. **Multi-output support:** verify whether `newTx().pay.ToAddress(a).pay.ToAddress(b)…` produces one multi-output tx in `lucid-midgard/src/builder.ts` (research: plausible, unverified). Determines fan-out tree shape only (k-output vs k txs); corpus rows are single-recipient either way.
2. **Dust/min-lovelace floor:** check `@al-ft/midgard-validation` phase A/B for a per-output minimum (research could not scope it). Sets the floor for the self-transfer `amount` and terminal change; until verified, default amount 1,000,000 and require final-step change ≥ 1,000,000.
3. **Exact `--tx-corpus` wiring** in `src/index.ts` action body (~lines 1595–1700, unread) — confirm option names our docs/manifest mirror (`--corpus-shape`, `--corpus-slice-id`).

**Resolution recorded 2026-07-08:** Phase 1 fanout does not depend on a
multi-output builder; `stress-wallets:fanout` uses k single-recipient L2
transfers per parent, level-by-level, with bounded concurrency across parents.
Searches of `demo/midgard-validation` and the L2 tx builder found no explicit
Midgard dust/min-lovelace rule; the shipped default stays at 1,000,000 lovelace
and both build and rebuild-sample verification enforce terminal change ≥ that
amount. The existing runner wiring is confirmed as `--tx-corpus`,
`--corpus-shape`, and `--corpus-slice-id`.

## 5. Work breakdown

### WB1 — Corpus planner (`src/commands/stress-corpus/plan.ts`)

Pure module: inputs `{targetRateTps, durationMs, warmupCount, cooldownCount, walletCount?, safetyFactor=1.1, amountLovelace, feeParams}` → `{rowCount, walletCount, chainDepth, perWalletFundingLovelace, totalFundingLovelace, estimatedCorpusBytes, interleavingPlan}` implementing D3/D4. Fails loudly if `walletCount < targetRateTps × assumedAcceptanceLatencySec` (overridable `--assumed-acceptance-latency-ms`, default 1000). Unit-testable, no I/O.

### WB2 — Offline chain builder (`src/commands/stress-corpus/build-chain.ts`)

Given `{seedPhrase, walletId, fundingUtxo, depth, amount, feeParams, network}` produce `depth` rows: loop of (synthetic single-UTxO provider → `buildTransferTxWithMinFee`-equivalent → extract `{txHash, canonicalCborHex, sha256, byteLength, outputOutrefs}` → next input = `txId#1` with computed change amount). Refactor: extract the provider/build core of `submit-l2-transfer.ts` (`makeStaticMidgardProvider`, `makeTransferMidgard`, `buildTransferTxWithMinFee`) into a shared `src/commands/transfer-build-core.ts` so harness and generator share one implementation (parameterizing the hardcoded `maxSubmitTxCborBytes`). Assert per-step invariants: change index 1 exists, fee matches formula, byteLength ≤ maxSubmitTxCborBytes, terminal change ≥ floor (WB0-2).

### WB3 — Worker pool (`src/workers/corpus-chain-builder.ts` — top-level file, per build-glob constraint)

`workerData = {walletBatch: [{seedPhrase, walletId, fundingUtxo}], depth, amount, feeParams, network, outPath}`. Each worker builds its assigned chains and writes its own shard file `corpus.shard-NN.ndjson` (rows in chain order), posting progress `{type:"progress", walletId, rowsDone}` and terminal `{type:"done", rowCounts, sha256}` / `{type:"failure", error}` messages — mirroring the commit-block-header protocol. Pool size defaults to `os.cpus().length - 1`. Wallet seeds cross the thread boundary in memory only; shard files contain no secrets.

### WB4 — Assembler (`src/commands/stress-corpus/assemble.ts`)

Streaming concatenation of shard files into the final grouped-by-chain `corpus.ndjson` (shards already hold whole chains in order — D4 makes assembly a sequential append, no merge logic), emitting `<corpus>.index.ndjson` (one line per chain run with byte offsets, D5) and `<corpus>.manifest.json` with per-file sha256 computed during the streaming write. Optional `--slices M` assigns chains to M sliceIds round-robin (each slice keeps whole chains) for multi-stage runs — ramp stages consume disjoint slices.

### WB5 — Corpus verifier (`stress-corpus-verify` subcommand)

Streaming re-validation: every row passes `parseCorpusLine`-equivalent checks; `selectedInputOutref` global uniqueness (bloom + exact on collision); per-chain integrity (each non-null `parentTxHash` equals the immediately preceding row's `txHash` within the same chain run, and `selectedInputOutref` equals that parent's change out-ref); index byte-offsets resolve to the correct first/last rows per chain; deterministic hash-ordered 0.1% sample fully re-built via WB2 and byte-compared (catches nondeterminism/version drift); manifest sha256s match. Run automatically at the end of generation; also standalone for pre-run preflight when `--rebuild-wallets-dir` plus fee/network inputs are supplied.

### WB6 — CLI command (`src/index.ts`)

`stress-corpus-generate` options: `--target-rate-tps` (req), `--duration-ms` (req), `--wallet-count`, `--amount-lovelace` (default 1000000), `--out-dir` (default `.stress-corpus/<timestamp>`), `--workers`, `--slices`, `--corpus-slice-id-prefix`, `--assumed-acceptance-latency-ms`, `--wallets-dir` (default `.stress-wallets`), `--funding-source` (`existing|fanout`, default `existing`), `--rebuild-sample-rate` (default `0.001`), plus fee/network overrides mirroring the node env. Requires `--yes` before building. Registered alongside existing stress commands; action delegates to a thin `src/commands/stress-corpus-generate.ts`.

### WB7 — Wallet fan-out funding (`stress-wallets:fanout` in `stress-wallets.ts`)

Extends wallet records/env-export machinery (reuse `createL2Wallets`, records schema, atomic writes): given a funded treasury seed and target `{walletCount, lovelacePerWallet}`, compute the k-ary tree (D7; k from WB0-1 result, default 16), execute level-by-level L2 transfers through `/submit` with bounded concurrency (default 32 in-flight) and acceptance polling (reuse Phase 0's adaptive poller), verify every leaf via `/utxos`, and write `latestFunding` snapshots + a `fanout-report.json` (tree shape, tx ids, per-level timing). Any leaf failure aborts loudly with the unfunded list (Phase 0's loud-abort convention). Also fixes the sequential-deposit bottleneck for anyone still using `prepare` at small N by sharing the bounded-concurrency executor.

### WB8 — Docs & runbook

Update `.agents/skills/midgard-e2e-acceptance/SKILL.md` stress section: corpus generation recipe, sizing table (rate → wallets/depth/funding), verify step, and the manifest-vs-node fee-param preflight rule. Cross-link from Phase 2 ExecPlan.

## 6. Test plan

Unit: WB1 planner math (property tests: rowCount ≥ rate×duration×1.1; spacing rule); WB2 determinism (same inputs ⇒ byte-identical rows; fee matches formula for crafted sizes); WB4 interleaver ordering + uniqueness on synthetic shards; WB5 verifier catches seeded corruption (bad sha, swapped parent order, duplicate input).
Integration (devnet/e2e env): generate 4 wallets × depth 25 (100 rows) → `stress-corpus-verify` → run through existing `--load-model open-loop-upper-bound` at 20 TPS with `maxInFlight=1` (grouped layout, see D4) → assert node txId echoes match all 100 rows, zero rejections; fan-out 1→16 wallets on the e2e stack and verify `/utxos` balances.
Scale rehearsal: 256 wallets × depth 100 (25,600 rows) generation on dev hardware; record rows/sec/worker to validate the ~15-min full-corpus estimate; memory ceiling < 512 MB per worker (streaming writes).

## 7. Acceptance criteria

- [x] `stress-corpus-generate --target-rate-tps 2500 --duration-ms 600000` produces a verified corpus (≥1.65 M rows, manifest + index) on an 8-core machine in ≤ 30 min, RSS bounded.
- [x] `stress-corpus-verify` passes locally: schema, uniqueness, per-chain parent/out-ref integrity, index offsets, 0.1% rebuild sample byte-identical.
- [x] 100-row integration corpus achieves 100% node acceptance with matching txId echoes.
- [x] `stress-wallets:fanout` funds 4,096 wallets from one treasury with one L1 deposit; all leaves verified; loud abort on any failure.
- [x] No hardcoded fee/size constants: manifest records resolved params; generator refuses to run without them.
- [x] WB0 questions answered and recorded in this doc's changelog.

## 8. Risks

**Fee-param drift** (corpus built for wrong `MIN_FEE_A/B` ⇒ mass rejection): mitigated by manifest stamping + engine preflight (D6) + verifier sample rebuild. **lucid-midgard version drift** changing canonical encoding: txId echo mismatch fails fast on first submission; pin the workspace version in manifest. **Acceptance-latency assumption too low** (children rejected for missing parent): spacing is configurable; mitigation ladder = raise walletCount → lower rate → Phase 2 engine's per-chain cursor absorbs it entirely. **Dust/min-lovelace drift:** default to 1,000,000 lovelace and fail build/rebuild verification if terminal change drops below that floor. **Worker memory** with large batches: stream shard writes per row; cap chains-per-worker-batch.

## 9. Changelog / evidence

- 2026-07-08: Implemented `stress-corpus-generate`, `stress-corpus-verify`,
  `src/workers/corpus-chain-builder.ts`, grouped-by-chain assembly, manifest and
  index sidecars, streaming shard writes, `latestFunding.fundingUtxos` wallet
  snapshots, parameterized submit-size limits, and `stress-wallets:fanout`.
- 2026-07-08: Corrected planner funding math to budget every explicit
  self-transfer output plus the terminal change floor:
  `(chainDepth + 1) × amount + chainDepth × fee`.
- 2026-07-09: Corrected the default wallet-count sizing to round the
  safety-adjusted minimum chain count up to the next power of two. The default
  `stress-corpus-generate --target-rate-tps 2500 --duration-ms 600000` plan now
  uses 4,096 wallets, chain depth 403, and 1,650,688 rows, matching D3.
- 2026-07-08: Added deterministic 0.1% rebuild-sample verification. Generation
  runs it automatically; standalone verification can repeat it with
  `--rebuild-wallets-dir`.
- 2026-07-08 local evidence: `pnpm --dir demo/midgard-node exec vitest run
tests/stress-corpus.test.ts tests/stress-wallets.test.ts --reporter=basic
--disable-console-intercept` passed 8 tests; `pnpm --dir demo/midgard-node
run build` passed.
- 2026-07-09: Ran the 256-wallet × depth-100 scale rehearsal with 8 workers:
  25,600 generated and verified rows, 256 chain-index entries, 35 MB corpus,
  `corpusSha256=489e0ade2950783203a0d79c9bcbef48ba0de67e714957dff34e0254b0f6f113`,
  wall time 22.05 s, throughput 1,161 rows/s, process RSS 2,767.8 MiB
  (≈346 MiB/worker if attributed evenly). Standalone `stress-corpus-verify`
  with rebuild sample passed in 6.00 s at 419 MiB RSS. Evidence artifact:
  `demo/midgard-node/logs/phase-1-scale-rehearsal-20260709T001841Z/scale-summary.json`.
- 2026-07-09: Ran the full default 2,500-TPS × 10-minute offline corpus
  command with 8 workers and generated 1,650,688 verified rows, 4,096 chains,
  chain depth 403, a 2.3 GB `corpus.ndjson`, 4,096 chain-index entries, and
  8 shard files. Generation passed automatic 0.1% rebuild-sample verification
  in 12:35.23 wall time with max RSS 2,931,724 KiB; standalone
  `stress-corpus-verify` with rebuild wallets passed in 1:09.41 with max RSS
  664,520 KiB. Evidence artifact:
  `demo/midgard-node/logs/phase-1-full-corpus-20260709T002743Z/`.
  `corpusSha256=7c645478f0153ea1cc3e755ee2c254cb2a8d8a4d37017e3b62cb08c84c9b89ca`,
  `indexSha256=7f8cf96267d9528e5fea9a0043ee9e04f7fb89ad4bca5b8e0390137d53349911`.
- 2026-07-09: Live acceptance was not run because no local Midgard node was
  listening on `127.0.0.1:3000` (`curl /healthz` and `/readyz` failed to
  connect) and no Midgard-related Docker containers were running.
- 2026-07-09: Started the local Kupmios provider stack for attach diagnostics
  (`postgres`, `cardano-node`, `cardano-node-ogmios`, `kupo`). `node
  dist/index.js l1-provider-preflight --json` passed with local Kupmios on
  Preprod. `reconcile reference-scripts-complete --scope node-runtime --json`
  still blocked on `scheduler spending`.
- 2026-07-09: Diagnosed the live reference-script blocker. Direct Kupo lookup
  proves the deployed scheduler-spending reference UTxO is present and unspent:
  `dc5a00c41f3518eb1173baf1216a94d8632661d187ea70d73040d1468def51c1#1`,
  role policy
  `9bbb47fb5f23f1ca3d30107ea44a97d9b024137909ecffc728ad4e81`, script hash
  `adc0cb0642e888ec8f003ae71b5412bde1b31789c951597932f46712`. Exporting the
  currently configured local contract bundle to
  `demo/midgard-node/logs/phase-1-live-gate-diagnostics/current-contract-deployment-info.json`
  shows only `scheduler spending` differs from the deployed manifest:
  current local hash
  `bdc0ed9e90d6b38a5012de293f04543cc346512be8666de7fa645c4e`; all other
  node-runtime reference-script hashes match. `aiken build --env testnet`
  left `onchain/aiken/plutus.json` unchanged
  (`b612754cf27f87504ebd59ed60b03e88fbb3798dde8adf3b6582bc9a4dfaf7c9`), so
  this is not stale default-env output. The existing deployment's
  reference-script auth policy expires at slot `127536066`, while local
  Kupmios was already at slot `127875700`; attaching current code to this
  deployment would be unsafe because the scheduler script address has changed.
- 2026-07-09: Implemented strict deployment-manifest contract reconstruction for
  attach/resume. `MidgardContracts` now loads the configured v2 deployment
  manifest, verifies it matches `NETWORK`, `L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS`,
  and `HUB_ORACLE_ONE_SHOT_*`, reconstructs validators from manifest CBOR,
  recomputes script hashes/policy IDs, and falls back to the local blueprint
  bundle only when no v2 manifest is configured. Focused tests cover
  manifest-backed scheduler reconstruction and tamper rejection.
- 2026-07-09: Re-ran the previous live reference-script gate after rebuilding.
  `node dist/index.js reconcile reference-scripts-complete --scope node-runtime
  --json` selected deployment manifest
  `1a922406cf5fd4afe1044bf980ce6897484c41a2b6f18b55c8628985a98480ee` and
  returned `status: "satisfied"` with 27 node-runtime reference scripts,
  including the deployed scheduler-spending out-ref
  `dc5a00c41f3518eb1173baf1216a94d8632661d187ea70d73040d1468def51c1#1`.
- 2026-07-09: Fixed `reconcile phas-registered --repair` to select the operator
  main wallet before submitting the idempotent PHAS registration repair, and to
  classify an `already_registered` ledger response as `satisfied` with no
  remaining repair actions. The repaired live command returned
  `status: "satisfied"` for reward address
  `stake_test17prd7qp8ls90quvhjfxuqlcuy7kxk90t90twl3a88vxmknguu7vsa` and script
  hash `46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4d`.
- 2026-07-09: Verified operator lifecycle in attach mode through
  `e2e-run-step` (`logs/phase-1-live-acceptance/operator-lifecycle.json`).
  `register-active-operator` selected the deployment manifest, found operator
  key `993d08c54b59536a92703a11d788eb25ce55ad8b49866d303d894366` already
  active, and skipped registration/activation with `registerTxHash=null` and
  `activateTxHash=null`.
- 2026-07-09: Runtime startup was blocked before `/healthz`/`/readyz` at this
  point in the attach attempt.
  `docker compose -f docker-compose.yaml -f docker-compose.kupmios.yaml up -d
  midgard-node` started the service, but `midgard-node-ready` timed out after
  180 s and the container entered a restart loop. Logs show startup verifies
  the deployment manifest and 27 node-runtime reference scripts, then refuses to
  serve because `local_mutation_jobs` contains unfinished job
  `local_block_finalization:cf8922943d44d1847e316d3d1c9a50e0140e961c817ea554937269e2`
  with `status=failed`, `attempts=2108`, and last error
  `DatabaseError: Failed to publish DA payload over libp2p`.
- 2026-07-09: Read-only DB/reconciliation probes for header
  `cf8922943d44d1847e316d3d1c9a50e0140e961c817ea554937269e2` show chain-side
  block commitment is already satisfied, the pending-finalization journal is
  still `observed_waiting_stability`, and the local DA payload row is present
  (`payload_cbor_bytes=19796`). Supported
  `db:backfill-da-payloads --header-hash ... --limit 1` skipped repair with
  reason `journal excluded by status: observed_waiting_stability; revive and
  complete local finalization before DA payload backfill`. No manual SQL reset
  or local-only finalization was performed.
- 2026-07-09: Added the supported `reconcile local-finalization` recovery
  surface and repaired header
  `cf8922943d44d1847e316d3d1c9a50e0140e961c817ea554937269e2` through the
  canonical state-queue UTxO and live libp2p DA path. The repair returned
  `SuccessfulLocalFinalizationRecoveryOutput`; a follow-up read-only reconcile
  returned `status: "satisfied"`, the stuck local-finalization job was
  completed, the DA watcher had `missingPayloads=0`, and node `/readyz`
  returned `ready=true`.
- 2026-07-09: Submitted and confirmed one L1 top-up deposit of
  `6200000000` lovelace through tx
  `078d7cce4b70bc94837d974661fb2155518c3b05a28484b7fcc1f803052ad0b6`.
  `project-deposits-once` reconciled it after inclusion. The normal commitment
  path later finalized the projected deposit into the user L2 ledger; the
  treasury `/utxos` view showed 18 UTxOs totaling `6216369090` lovelace,
  including the `6200000000` lovelace top-up output
  `b04fd0d5b06f9a7f41d0f4538fa4e002923c7f0a30f590b0cdb7c8e0126aa7c0#1`.
- 2026-07-09: Ran a 4-wallet live fanout smoke from the existing confirmed
  treasury balance, not the blocked top-up. Evidence:
  `demo/midgard-node/logs/phase-1-live-acceptance/fanout-4-live100.json` and
  `demo/midgard-node/.stress-wallets-phase1-live100/fanout-report.json`
  (`requestedCount=4`, `verifiedWalletCount=4`, `lovelacePerWallet=10000000`,
  `submittedTransferCount=4`).
- 2026-07-09: Generated and verified a 100-row live corpus from those four
  wallets: `targetRateTps=20`, `durationMs=5000`, `chainCount=4`,
  `chainDepth=25`, `rowCount=100`, `index rowCount=4`,
  `corpusSha256=d8430edf99f3f9271464bed0b0514cfae164d5a9fd1255cb642033d95a0ec863`,
  `indexSha256=bd3cd05aec07059fbd0bd24081fa4df6820c347ec0349541f77462012698db20`.
  Evidence artifact:
  `demo/midgard-node/logs/phase-1-live-acceptance/corpus-live100-20260709T020317Z/`.
- 2026-07-09: Ran the 100-row corpus through the live node with
  `e2e-stress-l2-throughput --load-model open-loop-upper-bound
  --workload-profile synthetic-admission --target-rate-tps 20
  --open-loop-duration-ms 5000 --open-loop-max-in-flight 1`. Evidence:
  `demo/midgard-node/logs/phase-1-live-acceptance/phase1-live100-20260709T020451Z-artifacts/summary.json`
  reported `requestedCount=100`, `submittedCount=100`, `acceptedCount=100`,
  `rejectedCount=0`, `submissionFailedCount=0`,
  `acceptanceTimedOutCount=0`, `acceptanceNotObservedCount=0`, and
  `openLoop.submission.failedCount=0`. `stress-open-loop.ts` records a
  failed submission when response `txId` differs from corpus `txHash`, so the
  zero failed-submission/null-txHash summary is the txId-echo proof for this
  smoke run. Finality was later observed after the scheduler catch-up below.
- 2026-07-09: Fixed live scheduler catch-up for the already-deployed scheduler
  validator hash `adc0cb0642e888ec8f003ae71b5412bde1b31789c951597932f46712`
  by selecting the legacy `previous_shift_end` refresh datum start mode and by
  looping pre-commit scheduler refreshes until the commit target is covered.
  Focused verification: `pnpm --dir demo/midgard-node exec vitest run
  tests/scheduler-refresh.test.ts --reporter=basic --disable-console-intercept`
  passed (23 tests), and `pnpm --dir demo/midgard-node run build` passed.
  Live recovery evidence: scheduler refresh attempts reached alignment,
  block commitment tx `918e61e706247348870ce284a5fdb85f1b93c40329eb4808b59dd6bd29e17522`
  finalized the pending 100-row block, and later fanout batches finalized
  through txs `13e9cf48d9969c90162348354b369f351b2c0e4ec2c24d582949b4233a51db55`,
  `f610664f1de3dbe8d70142d1e5b92739b481771cadb3c1232026557bc4997dcd`, and
  `27778835b220293796c4ee08bfa089d048a5c7fab8236537236b604e1e155372`.
- 2026-07-09: Completed the live 4,096-wallet fanout from the one top-up-funded
  treasury. Final evidence:
  `demo/midgard-node/logs/phase-1-live-acceptance/fanout-4096-finalize-20260709T040101Z-report.json`
  and
  `demo/midgard-node/.stress-wallets-phase1-fanout4096-20260709T032953Z/fanout-report.json`
  reported `requestedCount=4096`, `verifiedWalletCount=4096`,
  `lovelacePerWallet=1000000`, `rootRequiredLovelace=6144000000`,
  `levels=[16,256,3824]`, `submittedTransferCount=0`, and
  `alreadyFundedTransferCount=4096` on the final bounded resume/verification
  pass. Earlier partial passes submitted and finalized the tree funding; the
  final pass proved every leaf via `/utxos` without resubmitting funded edges.
- 2026-09-02: The corpus generator/verifier, stress-wallet tooling, and the
  bounded stress harness moved out of the operator binary into
  `demo/midgard-node-tools` (`node ../midgard-node-tools/dist/index.js
  stress-corpus-generate ...` from `demo/midgard-node`). The NDJSON row wire
  format stayed on the node side as
  `demo/midgard-node/src/open-loop-corpus-format.ts` because the
  mpf-engine-probe worker and the stage-B benchmark read corpora. Source
  paths quoted above are historical.
