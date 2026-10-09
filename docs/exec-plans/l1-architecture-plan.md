# Midgard L1 architecture plan: node, watcher, DA committee

Status: ACCEPTED, core scope only (§1.4), revision 2, 2026-10-07. Nothing is
implemented yet. The program ships as one pull request; the parent issue is
#784. Written at `368263e5d` and re-verified at `17fdffd9b`, the program
base. Re-verify paths, line numbers and counts at the branch tip before
editing code.

This plan replaces `node-l1-rollback-redesign.md` (rev 4, 2026-09-26) and the
six audits in `node-l1-rollback-audits/`. Everything in them that is still
valid is restated here, and line counts and line numbers have been rechecked
at `368263e5d` (the 2026-09 file splits moved most of them) and again at
`17fdffd9b`. The superseded
documents are deleted in the program pull request (§17). Forced-order carriage
and by-hash content fetch without Kupo are designed in §12 (no on-chain
change, owner 2026-10-07). Stores: Postgres for the node and the committee,
SQLite for the watcher (Q1, owner 2026-10-07).

Paths are relative to the repository root. Line numbers are at `17fdffd9b`.

Terms:

- **k** is 2,160 blocks, the Cardano security parameter and the manifest's
  `automaticRecoveryMaxDepth` (`demo/midgard-core/src/deployment-manifest-identity/types.ts:35`).
- **cd** is the manifest `confirmationDepth`: 3 on the emulator, 10 on testing,
  30 on public.
- **Point** means (slot, block hash).
- **Facts** are canonical L1 observations.

Identifier scheme:

| Ids | Meaning | Where |
|---|---|---|
| K1–K8 | Liveness bricks in the current code | §3.2 |
| CC, WC, NC | State-size-dependent paths in the committee, watcher and node | §3.4 |
| D-C, D-W, D-N | Destructive actions gated on cd | §3.5 |
| S0–S6 | Pipeline stages | §4.1 |
| INV1–INV6 | Fact-store invariants | §5.2 |
| P0–P10, PW, WP, CP | Projections | §5.5 |
| R1–R10 | Readiness reasons for intervention cases | §7.5 |
| L, F, C, W, N, I, M, U + number | Tickets | §15 |
| B1–B8 | Benchmarks | §16.2 |
| Q1 | Owner question, ruled 2026-10-07 | §18.1 |
| In scope / deferred | The tickets this program delivers, and those it defers | §1.4 |
| DR- prefix | Items in the public-testnet decision register | §18.2 |

## 1. Goals and binding rulings

### 1.1 Goals

- **G1. Liveness first.**
  - No role bricks, and no role needs a manual fix, after any L1 rollback
    shallower than k.
  - Each case that needs intervention is enumerated (§7.5). It fails `/readyz`
    with an actionable reason and never causes a restart loop.
  - Prevention outranks recovery.
  - The test is comparative: if a Cardano, OP Stack or Nitro node would
    recover from the analogous failure, Midgard must too.
- **G2. One authority.** In each process, one ChainSync follower on the
  operator's own cardano-node is the only source of canonical L1 history.
- **G3. Rollback means rewind the facts, then recompute.** No feature writes
  its own inverse.
- **G4. Per-block work is proportional to the block, not to the state.**
  Anything that depends on the size of the state runs off the per-block path,
  within a budget.
- **G5. Delete the bespoke rollback machinery** in all three roles (§13).

Non-goals:

- carriage and by-hash content (§12);
- on-chain changes, other than those already queued for the single redeploy;
- outputs created before the deployment origin point.

### 1.2 Owner rulings that bind this plan

| Date | Ruling | Effect here |
|---|---|---|
| 2026-10-09 | The commit-event depth d is bound to the deployment profile (`l1_finality.commit_event_depth`, `NodeConfig.COMMIT_EVENT_DEPTH`); the operator setting `HISTORY_COMMIT_HORIZON_LAG_BLOCKS` is deleted, and the profile build refuses a d that the event wait cannot cover: no inactivity strike, and an anchor cap that reaches the commit TTL floor (now + 30 s), W − 30 s − slot ≥ 3(d + 1)·slot/f, which sets d = 3 on the five-minute testing profiles. Mainnet: d = k = 2160 and `event_wait_ms` = 130,080,000. A forced order whose L1 order left the chain while an own landed block includes it is followed and logged (readiness degradation `l1_own_block_forced_order_orphaned`) until a fault proof for a fabricated forced transaction exists. Scope: the commit anchor and the narrow own-block hold only; deriving the commit from the follower view alone (no journal anchor) is a follow-up. | §8.1 (the commit anchor and its residual), §7.5 (`l1_own_block_event_orphaned`), ticket U3. |
| 2026-10-06 | One follower design: shared code, one instance per process, never a shared service. Transport is direct node-to-client (N2C) to the operator's own cardano-node through a thin Go gouroboros sidecar, generalised from `demo/midgard-watcher/native-chain-sync/`. The sidecar carries only ChainSync, LocalStateQuery (LSQ), LocalTxSubmission and LocalTxMonitor; all Midgard semantics live in one shared TS module. **Kupo and Ogmios are both removed.** Bootstrap uses LSQ, submit uses LocalTxSubmission, and eval stays local (`localUPLCEval: true`). The follower starts at a deployment origin point in the manifest; pre-origin outputs at shared credentials are out of scope. No per-deployment script parameterisation. | §4, §5, §12. Rev-4 D3 ("node only") now covers all three roles. Moots open decision-register item DR-B7 (Kupo `--match` narrowing; `public-testnet-decisions-2026-10-01/source-context.md:145`). Superseded: rev-4 "Kupo stays as content service" (tickets 3, 5, 12) and the #599 Ogmios+Kupo boundary binding (`demo/midgard-node/src/fibers/fetch-and-insert-tx-order-utxos.reconstruct-tx-order-material.ts:176-179`). |
| 2026-10-01 | Liveness first. Rollbacks shallower than k recover automatically. Intervention cases are enumerated, fail `/readyz` and never restart-loop. cd is a liveness setting only, never durability; durability is k. | Rev-4 O1 (halt) is replaced by recompute (§7.4). Rev-4 D2 levels are redefined (§9). A sweep for confirmationDepth-gated destructive actions is required (§3.5). |
| 2026-09-26 | Whichever lands wins. A missed commit is replaced without a coverage proof or a retention hold. The tail node makes the old and new commits mutually exclusive. Never build on state that differs from the landed block. Revive your own abandoned block if it lands. Keep abandoned signed content, bounded by the rollback horizon. | §8 (intent outcomes). |
| 2026-10-03 | Speculative commit mode is deleted (`SPECULATIVE_COMMIT_BUILD` and the whole builder). Commits wait for one L1 block, never for cd. Foreign-tip reconciliation is deleted with speculative mode. Every landed block is processed in order (#744/#695): adopt when the node holds the post-state, otherwise fetch, replay and compare; own blocks are never replayed. The journal status `finalized` is renamed. LV-ND1 keeps the narrow commit skip. RN1 removes the uncapped `signedHeaderRecoveryHoldSlot` hold now; the coverage path is deleted only when ticket I3 handles a landed or abandoned own block with orphaned deposit funding in any shape. | Rev-4 ticket 15 ("speculative build preserved") is dropped. §15 Phase 0. |
| 2026-10-04 | Watcher completed journals: a verified durable marker past k is skipped on restart, and only non-completed objectives count toward the cap. Collateral: an unretired attempt never gates later actions. Collateral is released at confirmation and re-signed on rollback, and the same funding input makes the attempts mutually exclusive. | §8.4, ticket L2. |
| 2026-10-05 | Pursue the watcher hot paths: (a) the whole-store re-encode on each persist; (b) one process spawn per native query. | §3.4 WC1 and WC3; tickets W2 and F1. |
| 2026-10-06 | Committee lease race: `renew()` does a check-then-rename that clobbers a successor's lock. Fixed in PR #769 (merged) by holding the lease under a SQLite process mutex. | As merged, the SQLite mutex is the only lock authority, and the 444-line lease became a 212-line instance lock (`demo/da-committee-node/src/store.json-file-instance-lock.ts`) (updated at 17fdffd9b). The JSON store, instance lock and mutex are deleted (Q1, ticket C2). |
| 2026-10-07 | Q1: the store backend is chosen per role. Node: Postgres. Committee: Postgres, and one member may run active/passive across hosts against one store. Watcher: SQLite (`node:sqlite`). | §4.4, §13.3, tickets C2 and W2, §18.1. |
| 2026-10-07 | Forced-order carriage needs no on-chain change. The stake-credential custody rule is rejected. | §12, ticket N10. |
| 2026-10-07 | Scope: the full plan is too expensive. Deliver the core (one follower as the authority for canonical L1 history; rollback = rewind the facts, then recompute; replace as much bespoke rollback machinery as possible) plus the committee JSON-store deletion, in one pull request. | §1.4. Everything else is deferred, with a trigger for each item. |
| 2026-09-26 | D1: raise `event_wait` with a horizon lag d. The commit window cap becomes `(tip − d) + W − 1`, with d ≈ W / 20 s. Needs a redeploy. | Ticket U3, which rides the single redeploy. Refined 2026-10-09: d is profile-bound and the cap is the commit anchor's (§8.1). |
| 2026-09-26 | D2: every status below the durable level is reversible, and the API exposes the head level. | §9 (levels redefined per 2026-10-01). |
| standing | R1b: never prune anything live. A script body lives while a live output's script_ref points at it. "Spent" means spent beyond rollback. | §11. |
| standing | One redeploy: all on-chain changes ride a single redeploy, which waits for T405. | U3, F3. |
| standing | Replace superseded variants rather than keeping parallel ones. No compat shims; prefer dependency bumps. No redundant on-chain checks. Non-interactive fault proofs first. Every offchain contract plan includes emulator tests in both polarities. | §13, §16. |
| rev 4, audits | Orphaned-origin ids are readmitted, because the same nonce outref gives the same id. Never-reuse applies only to canonical (live or retired) origins. | §5.4. |

### 1.3 Rev-4 defaults: kept, changed or dropped

| Rev-4 item | Status here |
|---|---|
| O1: halt on post-admission correction rollback | **Replaced.** Below k this recomputes; beyond k it is an intervention case (§7.4). |
| O2: unhealthy topology stops proposals and fails `/readyz` | Kept. Reads keep serving. |
| O3: tier-1 roots for architecture_g only; tier 2 engine-uniform | **Replaced** by §10. A single engine is already the case since `6794e37cd`. |
| O4: spent fact rows pruned past k; never-reuse key set append-only | Kept (§11). |
| O5: legacy-engine wedge hotfix (ticket 0) | **Closed by `6794e37cd`.** Architecture G is the only engine, and expired-intent release is not engine-gated (`demo/midgard-node/src/services/event-history-runtime.ts:165-173`). Ticket L4 now carries the correction-rewind halt (K5). |
| Ticket 15: speculative build preserved | Dropped (2026-10-03). |
| Tickets 3, 5, 12: Kupo as content service and bootstrap index | Dropped (2026-10-06). |
| Ticket 27: event_wait raise | Kept as U3. |

### 1.4 Scope of this program (owner, 2026-10-07)

The owner ruled the full plan too expensive (about 90 agent-days) and chose
its core: **one ChainSync follower as the only authority for canonical L1
history in every role, and rollback as "rewind the facts, then recompute"**,
plus the committee JSON-store deletion (C2). Everything ships as **one pull
request** against `colll78/canonical-v1-watcher-l1-source-checkpoint`.

**In scope.** Each Phase 0 ticket is absorbed by the ticket that replaces the
code it would have patched, so no interim fix is written for code this PR
deletes.

| Ticket | Absorbs |
|---|---|
| L8 | (itself; issue #752) |
| F1, F2, F3, F4, F5, F7 | F7 takes L7's `depth()`, `slotNow` and the §3.6 wall-clock sites that no other ticket deletes |
| F8 (no F6) | — |
| C1 | L1 |
| C2 | — |
| C3 | — |
| C4, as a gate on C1 | — |
| W1, W2, W3 | W2 takes L2; W3 takes L3 |
| N1, N2, N3, N5, N10 | — |
| N4 | L4 |
| N6 | L7's D-N4 and D-N7 |
| I1 | L7's D-N8 |
| I2, I5 | — |
| I3 | L7's D-N2 and D-N3. L6 already landed on the base branch (#759, `2b31ed070`); I3 keeps its check. |
| U3, off-chain part only | — |
| §17 deletions (part of U5) | — |

**Deferred.** Each item below has its trigger for picking it up.

| Ticket | Why it can wait | Revisit when |
|---|---|---|
| F6 (pipelined ingestion), B1 | The F2 sequential writer is correct; only catch-up speed suffers. | Catch-up from origin is too slow for a restart or a new operator. |
| F9 (devnet fork drill) | The F8 simulator covers fork shapes in CI. | Before the public testnet. |
| M1–M4 (MPF versioning), B4 | Interim: the MPF follows a rewind through `restoreRetainedRoot`. That restarts the MPF child process, not the node, once per rewind that crosses an applied block. | That restart shows up in liveness or latency. |
| N7, N8, N9 | Performance only. | B2 or a soak misses its target. |
| I4 (watcher and committee intents, collateral rule) | The current submit paths keep working on the F5 provider. | The next watcher or committee submission bug. |
| U1 (head levels in the API; D-N5) | Reporting only. | Before the public testnet. |
| U2 (readiness sweep R1–R9), B7 | Each in-scope ticket adds the unready reasons it introduces. | Before the public testnet. |
| U4 (Kupo and Ogmios out of the compose, drills and docs) | No role reads them after this PR. The compose may keep running them. | Right after this PR. |
| U5 (everything except §17) | — | With U4. |
| L5 (watcher archive retention) | Not a rollback item. | Tracked as its own issue. |
| M5 (compaction on an MPF cap breach) | Not a rollback item. | Tracked as its own issue. |

Rules for the deferral:

- **MPF interim.** N3, N4 and N5 run on today's MPF owner. Acceptance that
  says "root switch only" or "no restart" applies to the node process. One
  MPF child restart per rewind is allowed until M1–M3.
- **U3 off-chain.** The node caps the commit end time at the commit
  anchor's `time(A) + W − 1`, A the follower block d below the view the
  commit is planned at (§8.1), with d from the deployment profile
  (`l1_finality.commit_event_depth`).
  - The profile field and the mainnet W ride the single redeploy.
  - The profile build refuses a d whose anchor the event wait cannot cover:
    the event must not be struck, and the anchor cap must reach the commit
    TTL floor, W − 30 s − slot ≥ 3(d + 1)·slot/f
    (`config/deployments/README.md`).
- **No F9.** The devnet does not fork; the fork cases come from the F8
  simulator corpus.
- **Benchmarks in scope:** B2 (N1 only), B3, B5, B6, and B8 for the follower
  tables (synthetic chain). B1, B4 and B7 are deferred with their tickets.
- **C4 gate (resolved 2026-10-07).** No on-chain rule can slash a sibling
  signature. The off-chain conflict builder did treat cross-header pairs as
  equivocation; the owner ruled they are not, and C4 changed the rule to one
  signer, one header, two commitments
  (`docs/midgard/decisions/da-sibling-signatures-not-slashable.md`). C1 may
  sign siblings, and must retain the payload of every header it signed until
  that header is final (k) or provably cannot land.

**Estimate.** About 52 agent-days in total: about 50 for the core, plus about
1.5 for C2 and about 0.5 for the C4 gate. That is about 4 weeks with 5 lanes,
or about 5 weeks with 3. The serial chain is F1 → F2 → F8 → N1 → N2 → I1 → I3.

**What it removes.** Every §13 row except the 87-line M2 row: about 49,300
lines deleted and about 11,100 rewritten. The new code is estimated at
10–14k lines.

**Issues.** The parent issue is #784, and the map from ticket to issue is in
§15. Deferred work is tracked in #811.

## 2. Evaluation of the eleven proposals

Verdict key:

- **ADOPT**: a clear improvement, taken as proposed.
- **ADOPT, AMENDED**: taken, with the stated changes.
- **ALREADY IN REV 4**: restated here. The "New here" column says what this
  plan adds.

| # | Proposal | Verdict | Evidence at `17fdffd9b` | New here vs rev 4 |
|---|---|---|---|---|
| 1 | One ChainSync follower is the only authority for canonical L1 history | **ADOPT** | Today the three roles run 3 ChainSync clients over 3 transports, fed by 4 store designs. **Node:** an Ogmios ChainSync journal covers only the deposit and withdrawal lists; everything else polls Kupo. **Watcher:** a native N2C ChainSync, plus a per-block Kupo and Ogmios re-read and cross-check (`demo/midgard-watcher/src/l1/local-kupmios-native-observation.ts:254-437`). **Committee:** an Ogmios ChainSync cursor (`demo/da-committee-node/src/l1/provider.local-node-chain-authority.ts:380-381`), plus a Kupo replay provider (`demo/da-committee-node/src/l1/state-queue-replay-provider.create-local-kupmios-state-queue-replay-provider.ts:142-253`). | Applies to all three roles. N2C replaces Ogmios ChainSync. Kupo and Ogmios are deleted, not demoted. |
| 2 | A point-bound fact store: blocks, txs and outputs tied to hash, slot, height and revision, with full tx structure | **ADOPT, AMENDED** | Rev 4 already specifies the decoded DDL. Raw tx CBOR was deferred because Ogmios needs `--include-transaction-cbor` (`demo/midgard-node/src/l1-tx-order-carriage*`, rev-4 schema G.1). An N2C roll-forward already carries the raw block (`demo/midgard-watcher/native-chain-sync/transport.go:118-145`). | (a) Store the raw CBOR of every qualifying tx; G.1 is closed. (b) No per-row "revision" column: a row is bound to its block by `block_hash`, and rows above a fork point are deleted, so a revision on each row adds nothing. The follower's **generation** lives in the cursor and in a rollback log instead (§8). (c) Bootstrap replays from the manifest origin point, with one LSQ seed for the wallets (§5.3), instead of an LSQ ledger snapshot. (d) One store adapter interface with two backends: Postgres for the node and the committee, SQLite for the watcher (Q1). |
| 3 | Rollback means rewind the canonical facts, then recompute | **ALREADY IN REV 4**, extended | Today's bespoke inverses: about 28k lines in the node (§13); the watcher rollback engine (`demo/midgard-watcher/src/l1/rollback-engine/`, 6,375 lines); the committee's terminal quarantine plus a replay of the rollback feed (`demo/da-committee-node/src/committee-service.check-l1-rollback-feed.ts`, 286 lines). | A generic mechanism: every incremental projection is a **temporal table**, registered with the one rewind procedure, and one property test covers every registered table (§7.2). Extended to the watcher and the committee. |
| 4 | Four data classes: canonical observations (rewound), own signed intents (never erased), immutable content, derived projections | **ALREADY IN REV 4** (kinds A to D) | Rev 4 §4. | Every table declares its class and its retention bound, enforced by lint (§11). Projections split into four sub-classes (§5.1). The watcher and committee stores are mapped onto the classes. Committee signatures are class B. |
| 5 | Keep PostgreSQL; fix schema, batching, indexes, transaction boundaries and deltas first | **ADOPT for the node and the committee.** Watcher: SQLite (Q1, ruled 2026-10-07) | The node's Postgres is not the bottleneck; its access patterns are (§3.4). The committee's JSON store parses and rewrites the whole file on every mutation (`demo/da-committee-node/src/store.json-file-committee-store.ts:877-916`). It already needs a SQLite process mutex (`demo/da-committee-node/src/store.json-file-process-mutex.ts:1`) and a 212-line instance lock (`demo/da-committee-node/src/store.json-file-instance-lock.ts`), which replaced the 444-line lease in #769 (updated at 17fdffd9b). The committee's separate public retained-DA reader requires Postgres with a read-only role (`demo/da-committee-node/src/public-retained-da-config.ts:85-89`). The watcher already uses `node:sqlite` (`demo/midgard-watcher/src/storage/sqlite-record-store.ts:57-70`). | Postgres for the node and the committee; the committee's JSON-file backend is deleted (C2). SQLite (`node:sqlite`) behind the same adapter for the watcher. |
| 6 | Remove state-size-dependent work from per-block processing; benchmark it | **ADOPT** (new) | Inventory in §3.4. Worst case: each 15 s committee tick re-walks about k blocks of queue history, with one Kupo read per queue node and one new Ogmios WebSocket per tx. | Inventory with complexity per path (§3.4). Benchmark harness and targets (§16.2). |
| 7 | A multi-versioned native MPF: rollback is a root-pointer switch plus invalidation, with no index rebuild and no native restart; old roots kept under explicit pins | **ADOPT, AMENDED** | The native resident index is already append-only across promotions (`demo/midgard-node/native/mpf-event-flat-wasm/src/owner/runtime_owner.rs:160-202`), so every root promoted in a child's lifetime is still resident. Missing pieces: a switch RPC, a multi-root load contract (`demo/midgard-node/native/mpf-event-flat-wasm/src/owner/compact_index.rs:359-373` rejects any record unreachable from the one root), a durable GC (none exists: `demo/midgard-node/src/mpf/root-view-store.ts:556-580`), and pins. Today's restore is a full index read plus a child restart (`demo/midgard-node/src/services/mpf-native-owner/service.production-native-mpf-owner-service.ts:460-560`). | The restart leaves the rollback path entirely. Within the resident window, a rollback is `SwitchRoot` in O(1). Deeper than that, `ImportClosure` appends the target's missing records from Level, costing O(delta), with no restart. A restart remains only as **planned compaction**, outside the per-block path. Keeping all k roots resident does not fit the resident cap at high throughput (§10.3). |
| 8 | Pipelined ingestion: fetch ahead, decode concurrently, one ordered canonical writer | **ADOPT** (new) | The sidecar runs with `PipelineLimit: 1, RecvQueueSize: 4` (`demo/midgard-watcher/native-chain-sync/transport.go:116-117`), sends hex inside JSON lines, and has no acknowledgement flow control. The committee opens one WebSocket per tx (`demo/da-committee-node/src/l1/state-queue-replay-provider.parse-transaction.ts:113-119`). The watcher re-reads every block from Kupo and Ogmios. | §6: credit-based pipelining, a decode worker pool, and one ordered writer that batches blocks during catch-up. |
| 9 | Every action is tied to an L1 view (generation G at point P); work from a stale G cannot submit without reconciliation | **ADOPT, AMENDED** | Partly present: the committee cursor carries `rollbackGeneration` (`demo/da-committee-node/src/committee-service.check-l1-rollback-feed.ts:129,147`) and promise bindings check it (`demo/da-committee-node/src/availability/promise-causal-policy.ts:113`). | A view is the pair (generation counter, point). The counter alone over-invalidates; the fork point alone costs a lookup on every check. Work is stale iff its build point is no longer canonical. The check runs under the cursor lock in the same transaction as the intent write (§8.1). |
| 10 | Signed L1 txs are durable intents; after a rollback, reconcile to still-valid, landed elsewhere, expired, needs resubmission, or abandon | **ALREADY IN REV 4** (kind B and the §4.E loop), extended | Today: own commits only, through the pending-finalization machine. Other own txs go through `demo/midgard-node/src/transactions/utils.ts` with no can-it-land rule. The watcher has file journals for fault proofs. The committee has an outbox. | (a) Covers every own tx in every role (§8.4). (b) Status is **derived** from facts, never stored, so a rollback needs no write to the journal (§8.2). (c) Outcomes are mapped onto whichever-lands-wins (§8.3). (d) LocalTxMonitor and LocalTxSubmission replace Ogmios submit. |
| 11 | One deterministic pipeline: ChainSync → facts → derivation → {projections, MPF} → planner → intent journal → submission | **ADOPT** as the organising principle | Restates rev 4 §4. | Stage contracts (inputs, outputs, idempotence key) for each role (§4.1). |

## 3. Current state (verified at `368263e5d`, re-verified at `17fdffd9b`)

### 3.1 L1 sources and stores per role

| Role | Canonical source today | Other L1 reads | Store |
|---|---|---|---|
| Node | Ogmios ChainSync journal, for the deposit and withdrawal lists only (`demo/midgard-node/src/l1-event-history-chain.ts`, `demo/midgard-node/src/l1-event-history-source.ts`) | Kupo/Ogmios through Lucid `Kupmios`, 51 non-test files reference Kupo; reward accounts through the Go binary over LSQ (`demo/midgard-node/src/services/native-ledger.ts:72-85`, one `execFile` per query at `demo/midgard-core/src/native-reward-account.ts:293`) | Postgres; LevelDB MPF; native MPF owner child |
| Watcher | N2C ChainSync through the Go sidecar, one intersection point per process (`demo/midgard-watcher/native-chain-sync/main.go:180-186`), so a restart walks the candidates with one process each (`demo/midgard-watcher/src/l1/native-chain-sync.start-watcher-native-chain-sync-with-retry.ts:127-173`) | Kupo `/checkpoints` and an Ogmios block read **for every streamed block**, cross-checked (`demo/midgard-watcher/src/l1/local-kupmios-native-observation.ts:254-437`, Kupo-lag poll every 2 s); one Ogmios session per `readBlock` (`demo/midgard-fault-proofs/src/workflow/local-kupmios-http-ogmios-source.*`, 4,014 lines); exact-point queries run as sessions of one persistent helper process, each session with its own node connection (`demo/midgard-watcher/src/l1/native-chain-sync.exact-point-service.ts`; updated at 17fdffd9b) | `node:sqlite` singleton snapshot with CAS (`demo/midgard-watcher/src/storage/sqlite-record-store.ts:57-79`); HMAC'd one-file-per-record journals |
| Committee | Ogmios ChainSync cursor (`demo/da-committee-node/src/l1/provider.local-node-chain-authority.ts`), JSONL event journal (`demo/da-committee-node/src/l1/provider.file-chain-sync-cursor-store.ts`, 398 lines) | Kupo `fetchSpend` per queue node and one Ogmios WebSocket per tx during replay (§3.4 CC1) | JSON file plus SQLite mutex plus instance lock (`demo/da-committee-node/src/store.json-file-*.ts`, 1,178 lines; updated at 17fdffd9b), or Postgres (`demo/da-committee-node/src/store/postgres.postgres-committee-store.ts`, 1,067 lines) |

### 3.2 Liveness bricks (verified)

| # | Role | Brick | Evidence | Ticket |
|---|---|---|---|---|
| K1 | Committee | **Quarantine is terminal.** Any replayed ChainSync rollback below a persisted decision's slot quarantines the source, and so does a decision that disappears, forks or loses finality. Once quarantined, every tick returns early, and the state can never go back to healthy. Recovery is a manual CLI that exits 78 on failure. | Rollback trigger: `demo/da-committee-node/src/committee-service.check-l1-rollback-feed.ts:178-190`. Disappear/fork/lost-finality triggers: `:46-70`. Early return each tick: `demo/da-committee-node/src/committee-service.committee-service.ts:600-604`. Persisted at `:994-1015`. Terminal merge rule: `demo/da-committee-node/src/store.persisted-decision-transition.ts:26-31` (`throw ... "terminal"`). Manual CLI: `demo/da-committee-node/src/recover-l1-source.ts`, `demo/da-committee-node/src/l1/recovery-command.ts`. The trigger fires for **any** depth that crosses a signed decision, so a 1-block rollback at the tip is enough. Since #645 (`8bbbeb2d1`), a status disagreement at an unchanged state-queue output also throws `L1SourceIntegrityError` into the same quarantine path (`demo/da-committee-node/src/l1/state-queue-scanner.ts:186-210`) (updated at 17fdffd9b). | L1 |
| K2 | Watcher | **Journal caps.** The fault-proof queue journal appends a record for every enqueue, start, requeue and finish. A retried objective therefore adds 2 records per cycle, and nothing is ever compacted. At 65,536 records both append and startup throw, so a restart cannot recover. The decision journal has the same cap, but it grows only per detected fault. | Cap: `demo/midgard-watcher/src/fault-proofs/fault-proof-queue-journal.ts:21`. Appends: `:338` (start/finish), `:369` (requeue/reopen), `:385` (enqueue). Append throws: `:301-302`. Startup throws: `:193-195`. Decision journal: `demo/midgard-watcher/src/fault-proofs/fault-decision-journal.exact-record.ts:30`, startup `fault-decision-journal.create-journal.ts:107-110`, appends only `fault_detected` (`fault-decision-bridge.create-bridge.ts:388-389`). | L2 |
| K3 | Watcher | **cd treated as finality.** A rollback that removes a block that was final at cd moves the watcher to `quarantined`. Post-finality recovery (bounded by k) runs only on the startup path. | Finality at cd: `demo/midgard-watcher/src/l1/finality-engine.evaluate-watcher-finality.ts:298`. Incident: `demo/midgard-watcher/src/l1/rollback-engine/state.evaluate-watcher-rollback-step.ts:262-280`. Startup-only recovery: `demo/midgard-watcher/src/runtime/watcher-runtime.create-watcher-runtime.ts:177-190` and `watcher-runtime.restart-quarantine.ts`. Bound: `demo/midgard-watcher/src/l1/finality-engine.watcher-finality-reason-codes.ts:20`. | L3 |
| K4 | Watcher | **Unbounded archive.** `watcher_user_event_archive_v1` is insert-only; no statement anywhere in `demo/midgard-watcher/src/` deletes from it (corrected at 17fdffd9b: other watcher tables do have `DELETE`s). | Table at `demo/midgard-watcher/src/storage/sqlite-record-store.ts:64`, insert-only statement at `:84` | L5 |
| K5 | Node | **Correction-rewind halt (rev-4 O1).** The node admits a state-queue correction once it is cd deep and rewinds the native root off the removed blocks. If a rollback then removes the correction, the rewind has no inverse: the node raises `state_queue_correction_rewind`, which holds commit, merge and settlement. The halt clears only if the same correction lands again. If it never does, the node is held until an operator intervenes. Any rollback deeper than cd and shallower than k can trigger it. | Admission at depth ≥ cd: `demo/midgard-node/src/services/state-queue-correction-observer.reconcile-state-queue-correction-observer.ts:275-296`. No inverse; refusal: `demo/midgard-node/src/services/state-queue-correction-recovery.ts:416-446`. Halt raised: `demo/midgard-node/src/fibers/attestation-timeout-correction.attestation-timeout-correction-action.ts:337-346`; source documented at `demo/midgard-node/src/services/liveness-halt.ts:21-23`. The rev-4 ticket-0 wedge is closed: the legacy MPF engine was deleted in `6794e37cd` (no `MPF_ENGINE` setting remains), and expired-intent release runs whenever the manifest is present (`demo/midgard-node/src/services/event-history-runtime.ts:165-173`). | L4 |
| K6 | Node | The uncapped `signedHeaderRecoveryHoldSlot` retention hold (RN1 ruled to remove it now). **Fixed:** removed by #759 (`2b31ed070`); the grep finds zero under `demo/` (updated at 17fdffd9b). | Was in `demo/midgard-node/src/services/history-signed-header-recovery.ts` and `demo/midgard-node/src/services/event-history-owner.retention.ts` | L6 (done) |
| K7 | Node | Native MPF resident cap. A breach in `PreparePromotion` returns an error, and nothing compacts, so every later promotion fails the same way. | `demo/midgard-node/native/mpf-event-flat-wasm/src/owner/compact_index.rs:256-258`, `demo/midgard-node/native/mpf-event-flat-wasm/src/owner/runtime_owner.rs:146-152`; caps at `demo/midgard-node/native/mpf-event-flat-wasm/src/owner.rs:16-18` (2M records, 2 GiB) and `demo/midgard-node/src/services/mpf-native-owner/protocol.ts:10,16` | M5 |
| K8 | Node | **Replacement halt (cd treated as final).** Expired-intent release unwinds a displaced own block once the winner is cd deep. If the displaced block later lands, the node raises `SignedIntentReplacementIntegrityError`, which holds every commit fiber. That is exactly the case the 2026-09-26 ruling says to revive. The remaining sites in §3.5 have the same shape. | Gate and throw: `demo/midgard-node/src/services/history-expired-intent-release.displacement.ts:70-80`. Halt source: `demo/midgard-node/src/services/liveness-halt.ts:24-27`. Other cd-durability sites: §3.5. | L7 |

### 3.3 What the follower must replace in each role

- **Node.**
  - The Ogmios ChainSync history journal goes: `demo/midgard-node/src/l1-event-history-*`
    (17 files, 3,624 lines). It currently covers only the deposit and withdrawal lists,
    so it also takes with it the event-history owner, journal, census,
    provenance and coverage machinery that it feeds (§13).
  - Every Kupo poll goes. 51 non-test files reference Kupo. Among the timer
    fibers there are 19 whole-queue fetch sites, and 12 of them run on every
    busy commit tick (§3.4 NC6). Each becomes a read of the state-queue
    projection at the view point.
  - The Ogmios tip and slot reads go (the interim tip source in `demo/midgard-node/src/l1-heads.ts`, through
    `demo/midgard-core/src/ogmios-slot.ts:155-206`), and so does Ogmios submit. Evaluation is
    already local.
  - Reward-account queries move from one `execFile` per query
    (`demo/midgard-core/src/native-reward-account.ts:293`) to the long-lived sidecar's LSQ.
- **Watcher.**
  - The sidecar is already N2C and already streams raw blocks. What it needs
    is multi-point intersection, pipelining, one long-lived process, and the
    LSQ, submit and monitor protocols.
  - Everything that re-authenticates a block through Kupo and Ogmios goes:
    `demo/midgard-watcher/src/l1/multi-provider-consistency*` (1,534 lines),
    `demo/midgard-watcher/src/l1/local-kupmios*` (666 lines) and `demo/midgard-fault-proofs/src/workflow/local-kupmios*`
    (4,751 lines).
  - With Kupo and Ogmios removed (2026-10-06), the operator's own cardano-node
    becomes the only trust root, as it is for any Cardano wallet or indexer.
- **Committee.**
  - Ogmios ChainSync, its JSONL cursor journal, the rollback-feed replay and
    the Kupo replay provider are all replaced by the follower and a
    state-queue projection.

### 3.4 State-size-dependent per-block work (proposal 6 inventory)

Q = state-queue length; S = size of the role's durable store; k = 2,160.

| # | Role | Path | Per-block / per-tick cost | Evidence | Fix |
|---|---|---|---|---|---|
| CC1 | Committee | State-queue replay on every tick | **O(k · 16) checkpoints**, each with Kupo `fetchSpend` per queue node, a new Ogmios WebSocket per tx, ancestor and correction-lock reads. The walk limit is `2·(k+1)·16 ≈ 69k`. The replay anchor advances only past retirement depth (k+2), so every 15 s tick re-walks about k blocks of history. | `demo/da-committee-node/src/l1/state-queue-scanner.ts:60-61,165-331` (called with `automaticRecoveryMaxDepth` at `:228-230`); anchor at `demo/da-committee-node/src/l1/terminal-retention-observation.ts:139-160`; provider at `demo/da-committee-node/src/l1/state-queue-replay-provider.create-local-kupmios-state-queue-replay-provider.ts:142-253`; one WS per tx at `state-queue-replay-provider.parse-transaction.ts:113-119`; tick period at `demo/da-committee-node/src/config.load-committee-config.ts:208` | Replace with a temporal state-queue projection: O(changed nodes) per block, no replay (C2). |
| CC2 | Committee | Full state-queue snapshot every tick | O(Q) | `demo/da-committee-node/src/l1/state-queue-scanner.ts:165-331` | Same projection. |
| CC3 | Committee | JSON store mutation | O(S): read, parse, stringify the whole store, fsync, rename | `demo/da-committee-node/src/store.json-file-committee-store.ts:877-916` | The JSON store is deleted; the committee keeps only its Postgres store, which writes rows (Q1, C2). |
| CC4 | Committee | Postgres store reads on each tick and each readiness probe | O(S): every tick lists every stored state-queue header; every readiness probe lists every header, fetches each one's DA payload, and lists every signature and L1 submission | `demo/da-committee-node/src/committee-service.committee-service.ts:323-328,620`; `demo/da-committee-node/src/store/postgres.postgres-committee-store.ts:604` | Indexed queries over headers that are not yet final; readiness reads counts, not rows (C2). |
| WC1 | Watcher | Durable observation persist | O(S): builds a Map over all stored observations, then re-encodes, re-hashes and MACs the whole snapshot for each revision. Quiet blocks are batched, but every non-quiet block pays it. | `demo/midgard-watcher/src/l1/rollback-engine/durable-authority.persist-watcher-rollback-durable-observation.ts:139-144,218-233`; `durable-authority.commit-rollback-durable-authority.ts:146-166` | Per-row tables in SQLite. If tamper evidence is kept, one MAC per row plus a chained revision digest, which is O(delta) (W2). |
| WC2 | Watcher | Per-block Kupo/Ogmios cross-check | O(1) per block, but bound by network latency: Kupo-lag polling every 2 s for up to 3 head moves, one Ogmios session per read. Ingestion is serialised behind it. | `demo/midgard-watcher/src/l1/local-kupmios-native-observation.ts:254-437`; `...capture-exact-block-with-kupo-lag.ts` | Deleted (2026-10-06). |
| WC3 | Watcher | One process spawn per intersection candidate. Exact-point queries no longer spawn a process each: they run as sessions of one persistent helper (at most 256 live), and each session opens its own node connection (updated at 17fdffd9b). | O(candidates) spawns at each restart; one node connection per query | `demo/midgard-watcher/src/l1/native-chain-sync.start-watcher-native-chain-sync-with-retry.ts:127-173`; `native-chain-sync.open-watcher-native-exact-point-query.ts`; `native-chain-sync.exact-point-service.ts`, `native-chain-sync.exact-point-session.ts`; Go side `demo/midgard-watcher/native-chain-sync/service.go` | One persistent sidecar per role (F1). |
| WC4 | Watcher | Journal startup | O(records): reads and verifies every record file | `demo/midgard-watcher/src/fault-proofs/fault-decision-journal.create-journal.ts:96-124`; `fault-proof-queue-journal.ts:190-230` | Compaction (L2), then SQLite tables (W2). |
| NC1 | Node | History ingestion, per L1 block | **O(b·L) Plutus datum decodes.** Every live list node is authenticated for every tx, before the relevance check. O(b) is the txs in the block; L is the live list outputs. | `demo/midgard-node/src/l1-event-history-block-stage.ts:84-94`; `demo/midgard-node/src/l1-event-history-transition.decode-event-history-transition.ts:76-79` | Qualify first (§5.2), then project only touched outrefs: O(r). |
| NC2 | Node | Journal append, per L1 block | **O(I + L log L) CPU**, where I is every incarnation ever. It re-hashes all incarnations, re-serialises every live event, digests the whole list snapshot and serialises the whole block into a receipt. The SQL is O(r), but the CPU is not. | `demo/midgard-node/src/database/eventHistoryJournal.prepare-append.ts:203-253`; `demo/midgard-node/src/database/eventHistoryJournal.validate-live-coverage.ts:167-261`; `demo/midgard-node/src/l1-event-history-source.ts:271-302` | Deleted with the journal (§13). Facts are bound to their block by hash. |
| NC3 | Node | Tables that are never pruned | `event_history_census_blocks` gets one row per L1 block; `event_history_incarnations` and `foreign_verified_segments` also grow forever. Every block runs an O(F) UPDATE and `min()` over the segments. | `demo/midgard-node/src/database/eventHistoryForeignCensus.ts:223-263`; `demo/midgard-node/src/database/foreignNativeAdoptions.ts:451-490` | Deleted (§13). The retention lint (§11) rejects any table without a bound. |
| NC4 | Node | Deposit projection, per L1 block | O(P): loads every `projected` deposit, plus a mempool-ledger lookup for each | `demo/midgard-node/src/fibers/project-deposits-to-mempool-ledger.ts:31-105`; `demo/midgard-node/src/database/deposits.ts:284-297` | Temporal deposit projection; the mempool-ledger delta is computed per block. |
| NC5 | Node | Any L1 rollback, including 1-block rollbacks and heartbeat misses | **O(I + L + D + N), then O(S) wall time.** The node loads the full journal, runs an unscoped materialisation, fully reloads the mempool-ledger cache, then re-verifies every completed settlement job, one per 5 s tick. | `demo/midgard-node/src/services/event-history-owner.make-event-history-owner.ts:214-223`; `demo/midgard-node/src/database/eventHistoryJournal.load-locked.ts:117-139`; `demo/midgard-node/src/database/eventHistoryMaterialization.ts:315-340`; `demo/midgard-node/src/services/mempool-ledger-cache.make-mempool-ledger-cache-service.ts:95-127`; `demo/midgard-node/src/database/settlement.ts:116-121` | Rewind in O(depth × rows per block) (§7.1). Caches replay the rollback log. Settlement status is derived (§8.2). |
| NC6 | Node | Timer fibers | **19 whole-queue fetch sites**: 10 single requests plus 9 walks, each walk being Q sequential `utxosAtWithUnit` calls. 12 of them run on every busy commit tick. 17 timer fibers are on by default. Address-wide scans also return whatever third parties pay to the queue address. | `demo/midgard-node/src/commands/listen.node-fibers.ts:79-146`; walk at `demo/midgard-node/src/services/state-queue-topology.ts:174`; only the policy path filters: `demo/midgard-sdk/src/common.utxos-at-by-nftpolicy-id.ts:86-97` | Reads of the projection at the view point. Fibers become stage-S4 planner runs triggered on head change (§4.1). |
| NC7 | Node | Block confirmation, every 2 s | A new worker thread on each non-idle tick, an address-wide queue scan, then Q serial journal lookups | `demo/midgard-node/src/workers/utils/confirm-block-commitments.ts:144`; `demo/midgard-node/src/services/canonical-journal-recovery.ts:355-367` | Confirmation becomes a derived status, a join on `l1_txs` (§8.2). The worker is deleted. |
| NC8 | Node | Commit build | **O((k+1)·N log N)** (here k is the number of unmerged own journals). The default `MPF_PAYLOAD_ROOT_CHECK=every_block`, or any candidate tx, forced tx or withdrawal, forces a full `confirmed_ledger` load, one from-scratch root per journal, and one more from-scratch payload root. | `demo/midgard-node/src/services/config.make-config.ts:794-798`; `demo/midgard-node/src/workers/commit-block-header.pending-user-event-counts-up-to.ts:116-133`; `demo/midgard-node/src/transactions/state-queue/confirmed-ledger-snapshot.ts:167-300`; `demo/midgard-node/src/mpf/process.process-mpfs.ts:1169-1218` | The commit base is the native root at the parent (§10). The from-scratch cross-check runs off the commit path (ticket N8). |
| NC9 | Node | Foreign-base verify, on every commit-worker run | Two queue walks, a full ledger read plus a root, then an O(N log N) filter and root for each unmerged queue node: **O(Q·N log N)** | `demo/midgard-node/src/workers/commit-block-header.database-operations-program.ts:55`; `demo/midgard-node/src/workers/commit-block-header.verify-foreign-base.ts` (495 lines) | In-order landed-block processing (#744) adopts or replays each landed block once. The commit base is then the projection tip, so no per-commit verify is needed. |
| NC10 | Node | Local finalisation, per block | Every DA payload carries the full post-state UTxO set: O(N) bytes, an O(N log N) sort and root, and script-ref pins over all N outputs | `demo/midgard-node/src/workers/utils/commit-submission.finalize-committed-block-locally.ts:128-142`; `demo/midgard-node/src/workers/commit-block-header/da-payload.compute-da-payload-roots.ts:126-160` | Payload format, out of scope here: existing open item DR-F5 (§18.2). |
| NC11 | Node | Merge finalisation | Three full `confirmed_ledger` reads and at least three from-scratch roots per merged block, two of them under `LOCK TABLE confirmed_ledger IN EXCLUSIVE MODE` | `demo/midgard-node/src/transactions/state-queue/confirmed-ledger-snapshot.ts:417-456` | A temporal `confirmed_ledger` with delta apply, and the native root at the merged height (§10.5): O(delta). |
| NC12 | Node | Hot-path SQL | `COUNT(*) FROM mempool` on every commit and merge tick; pending counts done as `SELECT *` and counted in JS; `ProcessedMempoolDB.retrieve` with no LIMIT; a full unindexed `retrieveSpendable` on every cache reload; the DA reconciler re-seeds 15 days of payloads per peer every 30 s | `demo/midgard-node/src/database/utils/common.ts:15-25`; `demo/midgard-node/src/workers/commit-block-header.pending-user-event-counts-up-to.ts:59-99`; `demo/midgard-node/src/database/utils/tx.ts:179-189`; `demo/midgard-node/src/database/mempoolLedger.ts:178-198`; `demo/midgard-node/src/database/daPayloadPublications.ts:75-108` | Ticket N9: bounded queries, counters, indexes. |
| NC13 | Node | Settlement, per job | Scans the whole deposit or withdrawal list to find one event | `demo/midgard-node/src/commands/reserve-payout.retry-after-retirement-protection.ts:157,185` | A lookup by event id in the list projection. |
| NC14 | Node | Membership (60 s) and watchdog (1 s) | Reads the retired-operator list, which only grows, plus two full journal loads every 60 s | `demo/midgard-node/src/fibers/operator-membership.ts:261,436,466`; `demo/midgard-sdk/src/operator-lifecycle/directory.ts:143,224` | An operator-set projection, updated per block. |
| NC15 | Node | Tx-order fiber, every 10 s | Re-reads every visible order and the CEK credential, with one carriage RPC per order | `demo/midgard-node/src/fibers/fetch-and-insert-tx-order-utxos.reconstruct-tx-order-material.ts:315` | §12, ticket N10. |

### 3.5 cd used as a durability gate (sweep owed by the 2026-10-01 ruling)

Every site below treats cd as final. Each one must either become reversible
below k, or gate on k.

| # | Role | Site | Effect today | Required |
|---|---|---|---|---|
| D-C1 | Committee | `finalityDepth` = cd (`demo/da-committee-node/src/config.load-committee-config.ts:91,204,240`). A decision that loses finality at cd quarantines the source (`demo/da-committee-node/src/committee-service.check-l1-rollback-feed.ts:66`). | Terminal (K1) | Recompute; durable actions such as retention release gate on k (C3). |
| D-W1 | Watcher | Finality at cd (`demo/midgard-watcher/src/l1/finality-engine.evaluate-watcher-finality.ts:298`); a rollback of a cd-final block is an incident (K3) | Quarantine until restart | Facts below k are reversible; incidents only beyond k (W3). |
| D-W2 | Watcher | Completion marker requires recovery depth k (`demo/midgard-watcher/src/fault-proofs/fault-proof-completion-marker.ts:46-49,91-97`) | Correct already | Keep. |
| D-N1 | Node | Correction admission and rewind at cd (`demo/midgard-node/src/services/state-queue-correction-observer.reconcile-state-queue-correction-observer.ts:275-296`; `demo/midgard-node/src/services/state-queue-correction-recovery.ts:66-74`) | Halt with no automatic exit (K5) | Reversible below k (§7.4, L4). |
| D-N2 | Node | Displacement of a replaced own block once the winner is cd deep (`demo/midgard-node/src/services/history-expired-intent-release.displacement.ts:70-80`; `compensate-displacement.ts:195`) | Halt (K8) | Whichever lands wins (§8.3, L7). |
| D-N3 | Node | Signed intent `covered_absent` at TTL + cd, which abandons the header and rewinds the root (`demo/midgard-node/src/services/signed-intent-canonical-coverage.ts:232-246`; `demo/midgard-node/src/services/history-signed-header-recovery.ts:347-389`) | Whether revival covers this abandonment is not established | Deleted with the coverage path (RN1, I3). |
| D-N4 | Node | Settlement attempt marked `confirmed` at cd (`demo/midgard-node/src/services/settlement.reconcile-attempt.ts:178-193`; `demo/midgard-node/src/database/settlement.ts:93-104`) | No re-check after a deeper rollback was found | Derived status (§8.2); terminal only at k. |
| D-N5 | Node | Tx status `merged` reported to clients at cd (`demo/midgard-node/src/commands/tx-status-merge-evidence.ts:59`) | The claim flips silently | Report the head level (§9). |
| D-N6 | Node | Terminal DA outcome persisted at cd (`demo/midgard-node/src/database/daPayloadTerminalOutcomes.ts:85,106`) | Revoked on rollback, so it recovers | Keep. Make it a temporal row (§7.2). |
| D-N7 | Node | Operator removal at head − cd makes the process exit (`demo/midgard-node/src/fibers/operator-membership.ts:221-227,264-266,473-482`) | The state is in memory only, so a supervisor restart re-derives it. A removal that is still canonical **exits again: a restart loop** | Fail `/readyz` with `operator_removed` and stay up (§7.5). |
| D-N8 | Node (SDK CLI) | Availability-challenge intent retired or expired at cd (`demo/midgard-sdk/src/availability-challenge-operation.reconcile.ts:131,202-206,241`) | A retired intent is no longer observed | Derived status (§8.2). |

Depth is also counted three different ways today:

- inclusive: `demo/midgard-node/src/database/settlement.ts:100`;
- descendants only: `demo/midgard-node/src/services/signed-intent-canonical-coverage.ts:230-231`
  (that file was since deleted by I3, #810); <!-- doc-links:historical -->
- head − d: `demo/midgard-node/src/fibers/operator-membership.ts:266` (that file was
  since deleted by N6, #804). <!-- doc-links:historical -->

The heads module (§9) defines depth once. Retention points already use k:
`demo/midgard-node/src/services/event-history-owner.retention.ts:31-50` (since deleted) and
`demo/midgard-node/src/fibers/retention-sweeper.ts:206-207`. <!-- doc-links:historical -->

### 3.6 Wall-clock reads where an L1 slot is required

Lucid 0.6.5's `currentSlot()` is `unixTimeToEnclosingSlot(Date.now())` off
the emulator. All 14 `.currentSlot(` call sites in `demo/midgard-node/src/` are therefore wall
clock; 13 of them make decisions. Another 29 `Date.now()` / `new Date()` sites
feed L1 slot or cutoff decisions. The ones that matter for liveness:

| Site | Decision | Problem |
|---|---|---|
| `demo/midgard-node/src/workers/utils/confirm-block-commitments.ts:95`; `demo/midgard-node/src/workers/confirm-block-commitments.ts:235,290,321` | Abandons an unsubmitted or submitted commit once `Date.now()` passes the end time plus grace | Settlement expiry and journaled-intent expiry use the chain tip (`demo/midgard-node/src/services/settlement.reconcile-attempt.ts:132-145`; `demo/midgard-l1-follower/src/intents/status.ts:309-312`). A node whose clock runs fast abandons a commit that can still land. |
| `demo/midgard-node/src/fibers/tx-queue-processor.tx-queue-processor-action.ts:221` | L2 admission validity interval | Block replay uses the header's `blockSlot` (`demo/midgard-node/src/mpf/process.evaluate-normal-block-candidates.ts:22`), so admission and replay disagree near the bounds. |
| `demo/midgard-node/src/fibers/retention-sweeper.ts:264,408` → `demo/midgard-node/src/database/daPayloads.ts:396` | Irreversible DA payload delete when `block_end_time` is older than the cutoff | Mitigated by the finality and recovery holds (`demo/midgard-node/src/database/daPayloads.ts:309-340`), but the cutoff itself is wall clock. |
| `demo/midgard-node/src/transactions/register-active-operator/clock.ts:27`; `demo/midgard-node/src/fibers/operator-watchdog.ts:207` | Operator lifecycle "now" (the error message calls it chain time) | Wall clock |
| `demo/midgard-node/src/transactions/state-queue/merge-readiness.ts:420`; `demo/midgard-node/src/workers/commit-block-header/state-queue.ts:124`; `demo/midgard-node/src/services/attestation-timeout-observation.ts:38-45` | Merge maturity, DA-timeout commit pause, correction submit gate | Wall clock |

**Rule (ticket L7).** Any decision that compares against an L1 validity bound
uses the heads module's `slotNow`:

- below the tip, it is `max(tip slot, last tip slot + elapsed / slotLength)`,
  which is the hybrid that `demo/midgard-core/src/ogmios-slot.ts:155-206` already computes;
- the follower supplies the tip slot.

Validity intervals of txs that the node builds may still use the wall clock as
an upper estimate. A dead or abandoned verdict needs one of two things: a tip
slot past `valid_to`, or a conflicting spend in the facts (§8.3).

## 4. Target architecture

### 4.1 The pipeline (proposal 11)

```
cardano-node (operator's own, N2C socket)
   │  ChainSync · LSQ · LocalTxSubmission · LocalTxMonitor
   ▼
[S0] Go transport sidecar ── transport only, no Midgard semantics
   │  framed events: roll_forward(raw block, seq) / roll_backward(point, seq)
   ▼
[S1] Follower (shared TS module): decode pool → ordered writer
   │  one DB transaction per block (per batch during catch-up)
   ▼
[S2] Fact store (class A) + cursor + generation        ◄── rewind(point)
   │
   ▼
[S3] Deterministic derivation, role-specific, same transaction or keyed by point
   ├─► temporal projections (class D-t): heads, queue, events, decisions …
   └─► versioned external store: native MPF roots (class D-x, §10)
   │
   ▼
[S4] Planner: pure function of (view V, projections, intents) → proposals
   ▼
[S5] Intent journal (class B): signed bytes + inputs + validity + deps + view
   ▼
[S6] Submitter: LocalTxSubmission of exact bytes; LocalTxMonitor for presence
```

| Stage | Input | Output | Idempotence key | On rollback |
|---|---|---|---|---|
| S0 sidecar | N2C socket | Ordered events with a sequence number | Sequence number | Emits `roll_backward` in order |
| S1 follower | Events | Fact rows, cursor | Block hash | Applies the rewind in stream order |
| S2 facts | Writer batches | Facts at the tip | (tx_hash, ix), block hash | One rewind transaction (§7.1) |
| S3 derivation | Facts delta for one block | Projection rows stamped with the point | (projection, block hash) | The same rewind truncates the temporal rows. External stores switch root (§10). |
| S4 planner | View V = (g, P), projections, intents | Proposals | Proposal key (for example "commit on tail T") | Re-plans from the new V |
| S5 journal | Signed tx and its view | Intent rows | tx hash | Never touched; status is derived (§8.2) |
| S6 submitter | Live intents | Submissions | tx hash | Resubmits only what still can land (§8.3) |

Only S1 writes class A. Only the stage that owns a projection writes it. Only
S5 writes class B. Each role runs this pipeline in its own process against
its own store. Nothing is shared between roles except code.

### 4.2 Shared transport sidecar (S0)

<!-- doc-links:future -->
Move `demo/midgard-watcher/native-chain-sync/` to `demo/l1-node-transport/` and build it once. The node
already points at the same binary for reward-account reads through
`L1_NATIVE_CHAIN_SYNC_BINARY_PATH` (`demo/midgard-node/src/services/native-ledger.ts:20-23`).

Changes from today:

| Area | Today | Target |
|---|---|---|
| Lifetime | One process per intersection candidate and per reward-account query. Exact-point queries share one persistent helper process, but each opens its own node connection (`demo/midgard-watcher/native-chain-sync/service.go`) (updated at 17fdffd9b). | One long-lived process per role, multiplexing ChainSync, LSQ, LocalTxSubmission and LocalTxMonitor on one N2C connection. gouroboros provides all four; F1 pins v0.211.0 built with Go 1.26.5 (`demo/l1-node-transport/native/go.mod:3,6`). Interim: each concurrently open chain-sync stream beyond the first gets a bounded auxiliary N2C connection carrying chain-sync only (`demo/l1-node-transport/README.md`); the role cutovers delete them if no consumer still needs them. |
| Intersection | One point (`demo/midgard-watcher/native-chain-sync/main.go:180-186`) | The full point list in one `FindIntersect` (the last 64 blocks plus exponentially spaced older ones down to the origin) |
| Flow control | `PipelineLimit: 1`, `RecvQueueSize: 4`, OS pipe backpressure only (`demo/midgard-watcher/native-chain-sync/transport.go:116-117`) | Credit window: the follower grants N credits and acknowledges each persisted sequence number; the sidecar pipelines up to the credit (50 during catch-up, 1 at tip) |
| Framing | JSON lines with the raw block in hex (`demo/midgard-watcher/native-chain-sync/transport.go:134-145`) | Length-prefixed frames: a small CBOR header plus raw block bytes. Raw bytes are never hex-encoded. |
| LSQ | `reward_account` only (`demo/midgard-watcher/native-chain-sync/reward_account.go`) | `Acquire(point)`, `GetUTxOByAddress`, `GetUTxOByTxIn`, `GetCurrentProtocolParams`, `GetEraHistory`, `GetSystemStart`, `GetChainPoint`, filtered reward accounts |
| Submit | none | `submit(tx_cbor) → accepted | rejected(reason bytes)` |
| Monitor | none | `has_tx(tx_hash)`, snapshot sizes |

The sidecar validates framing and era decoding only. It never decides
qualification, depth, finality or validity. That leaves one place where the
gouroboros version matters, and a hard fork means bumping gouroboros, not
patching around it.

### 4.3 Shared follower module (S1, S2)

<!-- doc-links:future -->
A new workspace package, `demo/midgard-l1-follower/`, which every role
depends on. It contains:

- the sidecar supervisor: start, credit window, restart with backoff, and
  readiness reasons;
- the decode pool (§6);
- the tracked-set function `trackedSetFor(role, finalizedManifest)` (§5.2);
- the qualification rules (§5.2) and the ordered writer;
- the rewind procedure and the temporal-table registry (§7);
- the generation and view API (§8.1);
- the store adapter interface (`FactStore`), with a Postgres adapter and a
  SQLite adapter used only by the watcher (Q1);
- the read API: live UTxOs by address, credential, unit or outref, at the tip
  or at a point; spender of an outref; tx by hash; `isCanonical(blockHash)`;
  depth of a point;
- a Lucid `Provider` over the read API and the sidecar (UTxO queries, datum,
  protocol parameters, slot config from era history, submit through
  LocalTxSubmission). Evaluation stays local (`localUPLCEval: true`);
- slot↔time from LSQ era history and system start. This replaces
  `demo/midgard-core/src/ogmios-slot.ts` and the interim Ogmios tip source in
  `demo/midgard-node/src/l1-heads.ts` (F7 moved it there from the deleted local ledger-slot module).

### 4.4 Per-role instantiation

| | Node | Watcher | Committee |
|---|---|---|---|
| Store | Postgres | SQLite (Q1) | Postgres; one member may run active/passive across hosts (Q1) |
| Tracked set (§5.2) | Items 1–13 | State queue and auth validator, hub oracle, deposit, withdrawal and tx-order lists, correction lock, fraud-proof addresses and policies, scheduler and operator lists, DA params and attestation, reference scripts, own prover wallet, CEK credential | State queue and auth validator, hub oracle, DA params, attestation and bond pool, availability-challenge address, correction lock, reference scripts, own submitter wallet |
| Derivations (S3) | Heads; landed queue and topology; event set and key set; event status; deposit spendability; correction admission; in-order landed-block processing (#744/#695); MPF roots; `confirmed_ledger`; wallet view | Heads; landed queue; per-header verification inputs; fault classification; proof objectives; user-event history | Heads; landed queue; headers awaiting attestation; availability verification; promise and retention obligations |
| Intents (S5) | Commit, merge, DA attestation, timeout correction, operator register/exit/takeover, reserve payout, reference publication and sweep, script-reward registration, PHAS membership, availability-challenge registration, list inserts | Fault-proof step txs, collateral, prover funding | DA attestation/availability txs, bond pool top-up and withdraw, retirement and promise txs; signed availability decisions (non-tx class B) |
| Deleted (§13) | Kupo/Ogmios reads, coverage path, finalization machine, receipts, repair, plans, rewind/restore, speculative mode, foreign-tip reconciliation | Multi-provider consistency, Kupo/Ogmios capture, rollback engine state machine, whole-snapshot CAS, file journals | Rollback-feed quarantine, recovery CLI, Kupo replay provider, Ogmios sessions, JSONL cursor journal, JSON store, mutex and instance lock (Q1) |

## 5. Data classes and schema

### 5.1 Classes

| Class | What | Write rule | On L1 rollback | Retention (§11) |
|---|---|---|---|---|
| A, facts | Blocks, qualifying txs, tracked outputs, cursor, rollback log | S1 only, one transaction per block or batch | One rewind (§7.1) | Live rows are kept forever (R1b). Spent rows are kept until the spend is k deep. |
| B, own signed material | Signed L1 txs, own block contents, committee availability signatures, watcher fault decisions | Append-only | Never touched | Terminal and k deep (§8.5) |
| C, immutable content | DA payloads by header hash, tx CBOR by hash, script bodies by hash | Write-once, verified by hash | Never touched | While referenced by a retained A, B or D row, or by a pin |
| D-t, temporal projections | Rows stamped `(from_slot, to_slot)` or `(created_slot, removed_slot)` | Owning S3 stage | Truncated by the same rewind | Closed rows pruned once k deep |
| D-v, read views | SQL views and functions over A, B, C | n/a | Nothing to do | n/a |
| D-x, versioned external stores | Native MPF roots and Level nodes | Owning S3 stage plus the MPF owner | Root switch (§10) | Pins plus sweep (§10.4) |
| D-c, caches | In-memory tracked-outref set, resident MPF index, heads cache | Rebuilt from A/B/D | Dropped and rebuilt | n/a |

Every persistent table declares its class and its retention rule in its
migration. A lint fails any table that declares neither (ticket F2).

### 5.2 Fact schema (Postgres; the SQLite adapter mirrors it logically)

This is the rev-4 schema draft, restated here in full so that this plan stands
alone. Changes from the draft are marked **Δ**.

```sql
CREATE TABLE l1_blocks (
  slot         bigint NOT NULL PRIMARY KEY,   -- one chain stored: at most one block per slot
  hash         bytea  NOT NULL UNIQUE,        -- canonicality test for views (§8.1)
  height       bigint NOT NULL UNIQUE,
  parent_hash  bytea,                         -- NULL only for the origin row
  qualifying_tx_count integer NOT NULL        -- Δ: retention of empty blocks (§11)
);

CREATE TABLE l1_txs (                         -- one row per qualifying tx, valid or phase-2-failed
  tx_hash          bytea   PRIMARY KEY,
  block_slot       bigint  NOT NULL REFERENCES l1_blocks(slot) ON DELETE CASCADE,
  block_tx_index   integer NOT NULL,
  is_valid         boolean NOT NULL,
  inputs           bytea[] NOT NULL,          -- 34-byte outrefs (hash || u16 index), ledger-sorted
  reference_inputs bytea[] NOT NULL,
  collaterals      bytea[] NOT NULL,
  output_count     integer NOT NULL,          -- the collateral-return index equals output_count
  has_collateral_return boolean NOT NULL,
  mint             jsonb   NOT NULL,          -- {policy: {name: qty}}, sorted
  withdrawals      jsonb   NOT NULL,
  redeemers        jsonb   NOT NULL,          -- [{purpose, index, cbor}], sorted
  invalid_before   bigint, invalid_after bigint,
  body_cbor        bytea   NOT NULL,          -- Δ: exact bytes from the block; blake2b-256(body_cbor) = tx_hash
  witness_cbor     bytea   NOT NULL,          -- Δ: exact bytes
  aux_cbor         bytea,                     -- Δ: exact bytes, when present
  UNIQUE (block_slot, block_tx_index)
);

CREATE TABLE l1_tx_mint_policies (
  tx_hash   bytea NOT NULL REFERENCES l1_txs(tx_hash) ON DELETE CASCADE,
  policy_id bytea NOT NULL,
  PRIMARY KEY (tx_hash, policy_id)
);

CREATE TABLE l1_scripts (                     -- Δ: class C, one row per script body
  script_hash bytea PRIMARY KEY, script_type text NOT NULL, bytes bytea NOT NULL
);

CREATE TABLE l1_outputs (                     -- one row per tracked output, live or spent
  tx_hash bytea NOT NULL, output_index integer NOT NULL,
  address bytea NOT NULL,
  payment_cred bytea, payment_cred_is_script boolean,
  stake_cred bytea,
  lovelace numeric(20,0) NOT NULL,
  assets jsonb NOT NULL,                      -- exact-equality source of truth
  datum_hash bytea, datum bytea,              -- inline datum CBOR
  script_ref_hash bytea REFERENCES l1_scripts, -- Δ: replaces script_ref and script_ref_type
  created_slot bigint REFERENCES l1_blocks(slot) ON DELETE CASCADE,  -- NULL only for seed rows
  created_tx_index integer,
  spent_slot bigint REFERENCES l1_blocks(slot) ON DELETE SET NULL,
  spent_tx bytea REFERENCES l1_txs(tx_hash) ON DELETE SET NULL,
  seed_slot bigint,                           -- Δ: renamed from bootstrap_slot; LSQ wallet seed point (§5.3)
  PRIMARY KEY (tx_hash, output_index),
  CHECK ((created_slot IS NULL) = (created_tx_index IS NULL)),
  CHECK ((created_slot IS NULL) = (seed_slot IS NOT NULL)),
  CHECK ((spent_slot IS NULL) = (spent_tx IS NULL)),
  CHECK (datum_hash IS NULL OR datum IS NULL)
);

CREATE TABLE l1_output_assets (               -- index for unit and policy lookups
  tx_hash bytea NOT NULL, output_index integer NOT NULL,
  policy_id bytea NOT NULL, asset_name bytea NOT NULL, quantity numeric(40,0) NOT NULL,
  PRIMARY KEY (tx_hash, output_index, policy_id, asset_name),
  FOREIGN KEY (tx_hash, output_index) REFERENCES l1_outputs ON DELETE CASCADE
);

CREATE TABLE l1_follower_cursor (
  id boolean PRIMARY KEY DEFAULT true CHECK (id),
  slot bigint NOT NULL, hash bytea NOT NULL, height bigint NOT NULL,
  generation bigint NOT NULL,                 -- Δ: +1 per applied rewind, never decremented (§8.1)
  origin_slot bigint NOT NULL, origin_hash bytea NOT NULL   -- Δ: manifest origin (§5.3)
);

CREATE TABLE l1_rollbacks (                   -- Δ: append-only log, last 1,000 rows kept
  generation bigint PRIMARY KEY,
  from_slot bigint NOT NULL, from_hash bytea NOT NULL,
  to_slot bigint NOT NULL, to_hash bytea NOT NULL,
  depth_blocks integer NOT NULL
);

CREATE TABLE l1_event_keys (                  -- never-reuse key set, append-only (rev-4 O4)
  kind text NOT NULL, key bytea NOT NULL, origin_outref bytea NOT NULL,
  first_canonical_slot bigint NOT NULL,
  PRIMARY KEY (kind, key)
);

CREATE INDEX l1_outputs_address_live ON l1_outputs (address)      WHERE spent_slot IS NULL;
CREATE INDEX l1_outputs_payment_live ON l1_outputs (payment_cred) WHERE spent_slot IS NULL;
CREATE INDEX l1_outputs_stake_live   ON l1_outputs (stake_cred)   WHERE spent_slot IS NULL;  -- Δ (§12)
CREATE INDEX l1_outputs_spent_tx     ON l1_outputs (spent_tx)     WHERE spent_tx IS NOT NULL;
CREATE INDEX l1_outputs_created_slot ON l1_outputs (created_slot);
CREATE INDEX l1_outputs_spent_slot   ON l1_outputs (spent_slot)   WHERE spent_slot IS NOT NULL;
CREATE INDEX l1_output_assets_unit   ON l1_output_assets (policy_id, asset_name);
CREATE INDEX l1_tx_mint_policy       ON l1_tx_mint_policies (policy_id);
```

**Live at slot s.** This is the predicate behind every view at a point:
`(created_slot IS NULL OR created_slot <= s) AND (spent_slot IS NULL OR spent_slot > s)`.
Ordering within a block uses `created_tx_index` and the spender's
`block_tx_index` in the writer's block stage.

**Invariants.** Each query must return zero rows. They are checked at
startup, after every rewind and in the property test.

- **INV1.** No output is spent before it is created. Within one block, the
  spender's index is greater than the creator's.
- **INV2.** Every `spent_tx` exists and sits at `spent_slot`.
- **INV3.** `spent_tx` consumes the outref in the phase it ran: inputs if
  valid, collaterals if failed.
- **INV4.** Every non-seed output has its creating tx. Valid txs create indexes
  below `output_count`. Failed txs create only the collateral return.
- **INV5.** Nothing lies above the cursor, and blocks above the origin form a
  parent-linked chain with consecutive heights.
- **INV6.** No seed row lies above the cursor, and no stored tx contradicts a
  seed row: its creator, if stored, sits at or below `seed_slot` and creates
  that index.

```sql
-- INV1
SELECT * FROM l1_outputs WHERE spent_slot < created_slot;
SELECT o.* FROM l1_outputs o JOIN l1_txs t ON t.tx_hash = o.spent_tx
 WHERE o.spent_slot = o.created_slot AND t.block_tx_index <= o.created_tx_index;
-- INV2
SELECT o.* FROM l1_outputs o LEFT JOIN l1_txs t ON t.tx_hash = o.spent_tx
 WHERE o.spent_tx IS NOT NULL AND (t.tx_hash IS NULL OR t.block_slot <> o.spent_slot);
-- INV3
SELECT o.* FROM l1_outputs o JOIN l1_txs t ON t.tx_hash = o.spent_tx
 WHERE NOT ((t.is_valid AND (o.tx_hash || int2send(o.output_index::int2)) = ANY (t.inputs))
         OR (NOT t.is_valid AND (o.tx_hash || int2send(o.output_index::int2)) = ANY (t.collaterals)));
-- INV4
SELECT o.* FROM l1_outputs o LEFT JOIN l1_txs t ON t.tx_hash = o.tx_hash
 WHERE o.created_slot IS NOT NULL AND (t.tx_hash IS NULL OR t.block_slot <> o.created_slot
   OR (t.is_valid AND o.output_index >= t.output_count)
   OR (NOT t.is_valid AND NOT (t.has_collateral_return AND o.output_index = t.output_count)));
-- INV5
SELECT * FROM l1_blocks WHERE slot > (SELECT slot FROM l1_follower_cursor);
SELECT b.* FROM l1_blocks b LEFT JOIN l1_blocks pb ON pb.hash = b.parent_hash
 WHERE b.slot > (SELECT origin_slot FROM l1_follower_cursor)
   AND (pb.hash IS NULL OR pb.height + 1 <> b.height);
-- INV6
SELECT * FROM l1_outputs WHERE seed_slot > (SELECT slot FROM l1_follower_cursor);
SELECT o.* FROM l1_outputs o JOIN l1_txs t ON t.tx_hash = o.tx_hash
 WHERE o.seed_slot IS NOT NULL AND (t.block_slot > o.seed_slot
   OR (t.is_valid AND o.output_index >= t.output_count)
   OR (NOT t.is_valid AND NOT (t.has_collateral_return AND o.output_index = t.output_count)));
```

The `int2send` concatenation relies on the 34-byte outref encoding. A
composite-type array works equally well.

**Why raw tx CBOR is stored now (Δ).**

- It is free over N2C: the block arrives raw.
- It removes every by-hash fetch for post-origin qualifying txs: list-replay
  creating bodies, the carriage mint redeemer, and the observer's spender
  outputs.
- The bytes are kept exactly as in the block, so the hash can always be
  re-derived.
- Size: at most 16,384 B per tx (maxTxSize, `demo/midgard-core/src/consensus-profile.ts:95`),
  and only for qualifying txs.

The rev-4 reason for not storing it was Ogmios's
`--include-transaction-cbor`. That reason is gone with Ogmios.

**Tracked set.** Derived per role from the finalized manifest (§4.4):

1. the hub oracle;
2. the deposit and withdrawal lists, plus their retention addresses;
3. the tx-order address and policy;
4. the state queue;
5. the scheduler, and the registered, active and retired operator lists;
6. the correction lock;
7. settlement, reserve and payout;
8. fraud-proof addresses;
9. the DA params governor, attestation and bond pool;
10. `stateQueueAuthValidator`, where it is distinct;
11. reference-script publication;
12. own wallets;
13. the CEK-material **payment credential**, which matches any stake part;
14. always-succeeds test addresses (testing profiles only).

Carriage adds no rule: the order tx already qualifies, and §12 resolves its
reference inputs.

The tracked policies for rule (c) are the hub, list, state-queue,
correction-lock, fraud-proof, tx-order and DA-attestation policies.

**Qualification.** An output gets a row iff it matches the tracked set by
address or by credential. A tx, valid or phase-2-failed, gets a row iff any
of the following holds:

- **(a)** it creates a qualifying output (for a failed tx, only the
  collateral return counts);
- **(b)** it spends an outref in `l1_outputs` (for a failed tx, through its
  collaterals);
- **(c)** it mints or burns under a tracked policy;
- **(d)** it references an outref in `l1_outputs`.

The writer decides (b) and (d) against the tracked-outref cache and the
current block stage (§6). Rule (d) is stricter than Kupo, which never
recorded reference-only txs. It is needed because commits reference the hub
and the scheduler without spending them.

Third-party outputs paid to a tracked script address do qualify. Their
storage is bounded by the ADA they lock, the same as any other address
index. Projections filter them by NFT.

### 5.3 Origin and bootstrap

There are no ledger snapshots and no Kupo index.

1. **Origin point O.**
   - Add `l1Origin: {slot, blockHash}` to the finalized manifest. It is the
     point immediately before the block that holds the first deployment step,
     `prepareHubOracleNonce` (`demo/midgard-core/src/deployment-manifest-identity/types.ts:22-30`).
   - The field rides the single redeploy. Until then, a running deployment
     supplies O through operator config.
2. **Fresh start.**
   - `FindIntersect([O])`, then replay forward.
   - Every Midgard output is created after O, so the tracked scope is
     complete from O.
   - This closes every pre-origin gap that rev 4 left open or put to the
     owner:
     - the activation tx location;
     - CEK material by payment credential, which LSQ cannot enumerate;
     - list replay from activation into a snapshot;
     - the correction observer's complete-scope question.
   - On that last point: with a complete scope, "absent from `l1_outputs`"
     soundly means "not a lock and not a proof", and failing closed would
     give the same answers.
   - The rev-4 raw-CBOR question goes with Ogmios (§5.2). The carriage
     question goes to §12. Spent-row retention was
     already ruled (O4).
3. **Completeness assertion.**
   - The follower must see the tx that spends the manifest's
     `hubOracleOneShot` outref
     (`demo/midgard-core/src/deployment-manifest-identity/finalized.verify-finalized-deployment-manifest.ts:91-98`).
   - If the cursor reaches the tip without seeing it, `/readyz` fails with
     `origin_after_protocol_init`. This is an intervention case: the fix is a
     config value.
4. **Wallet seed.** Own wallets can hold UTxOs the follower has no row for:
   pre-origin outputs, and outputs paid to a wallet before it was tracked.
   - Once the cursor is within k of the tip, acquire LSQ at the cursor point
     P. LSQ can only acquire volatile points.
   - Call `GetUTxOByAddress(own wallets)` and insert, as seed rows
     (`created_slot NULL, seed_slot = P`), the outrefs not already stored as
     an output row.
   - The write is refused (`cursor_moved`) unless the cursor is still P; the
     seed then reads again at the new cursor.
   - A seed is a fact observed at P. A rewind to T < P deletes the seed rows
     with `seed_slot > T` in the rewind's transaction, and the wallet is owed
     a seed again. Every wallet takes this one path; none waits for k.
   - Repeat the seed whenever a wallet is added. Until a wallet's seed lands,
     the role reports the transient reason `wallet_seed_pending`.
5. **Rollback below O, or below the oldest retained block.** This cannot
   happen on a correct node once O is k deep. If it does happen, it is an
   intervention case (§7.5).

### 5.4 Event identity

These rules carry over from rev 4:

- An event id is the consumed nonce outref.
- `inclusion_time` is the admitting tx's `invalid_after` plus `event_wait`.
- The retirement reason is redeemer-only.
- An **orphaned-origin id is readmitted**, because the same nonce outref gives
  the same id.
- Never-reuse applies only to canonical (live or retired) origins, which is
  `l1_event_keys`.

### 5.5 Projection catalogue

These are the rev-4 projection acceptance criteria, restated and updated for
this plan.

- Every projection here is D-t unless marked otherwise.
- Each is a pure function of facts (A), own content (B) and verified content
  (C) at a point.
- The F2 property test and the F8 simulator cover each one. Its ticket adds
  the cases listed.

| # | Projection | Output | Invariants | Simulator cases (both polarities) | Ticket |
|---|---|---|---|---|---|
| P0 | Heads (§9) | Level and depth of every own and landed header | local ≥ landed ≥ safe ≥ final, by chain position. A pure function of facts plus intents. Never cached across a rewind. | A cached head injected after a rewind is detected | F7 |
| P1 | Landed queue and topology health | Linked list from root to tail, and `{healthy, reason}` | Healthy means one root, one tail, and a walk that visits every valid policy UTxO once. Over the node cap, or a malformed NFT or datum, means unhealthy with a named reason. Unhealthy fails `/readyz` and stops proposals, while reads keep serving (rev-4 O2). Third-party outputs at the address are ignored. | An orphan node with a valid datum is unhealthy. A rewind that removes the tail recomputes a healthy queue. | N2, C1, W1 |
| P2 | Event set and key set | `{event_id, kind, inclusion_time, outref, retired_at}`, and the append-only `l1_event_keys` | `inclusion_time` comes from the slot, never the wall clock. At most one canonical origin per key. A retired or live key is never readmitted. An orphaned-origin id is readmitted (§5.4). | A fork removes an origin, then the same id is admitted. A resubmitted retired key is refused. The same key under another kind is admitted. | N1 |
| P3 | Event status | awaiting, included (own or foreign header), or carried forward, plus its level | Included(h) means h is landed, or is the one live own block. A foreign block with a non-empty event root and no verified DA defers: it never declares an omission. A due event omitted by a verified foreign window is carried forward, never dropped and never included twice. A foreign landed block is applied through in-order processing (#744). | A root mismatch is refused with nothing projected. A fork removes the foreign block and the event returns to awaiting. | N1, N3 |
| P4 | Deposit spendability | Spendable deposit UTxOs in the working ledger | Spendable iff included(h) with h landed or live-own. The cutoff is `slotNow`. A rollback removes spendability, and dependent L2 txs are rejected transitively and atomically. | A fake clock ahead of the tip is still unspendable. | N1, N7 |
| P5 | Landed-block local effects | Per header: immutable rows, mempool removal, withdrawal effects, the DA payload and CEK release | Idempotent by header hash. Present iff the header is landed or live-own, and removed when it leaves. The DA payload equals the ledger at the header's root. | A crash mid-effects converges. A fork removes h, so its rows go and its members return to the mempool. | N3, I1 |
| P6 | Withdrawal classification and forced verdicts | Class B block content | Never recomputed for a signed header. A replacement re-derives against its own base and stores under its own header. No row mutation changes signed content. | Mutating a row after signing changes neither the payload nor the roots. | I1 |
| P7 | Build-base admissibility | ok, or defer with a reason | Never build on a header removed by an admitted correction. A replaced own block that landed is followed or revived, not refused. A refusal is a defer, never a halt. | A correction-removed tail is deferred. A revived tail is accepted. | N4, I3 |
| P8 | Correction admission | `queue_corrections`: removal tx, removed headers | Admitted when the removal tx is landed. Acting may wait for `safe` (liveness). A rollback recomputes (§7.4): no halt. | Admit, then roll back at cd + 1: nothing halts, and the root equals a fresh replay. | L4, N4 |
| P9 | MPF roots (D-x) | `mpf_roots`, pins, the working root | `mpf_root(h)` equals h's signed root for every landed h within k. A switch is O(1) within the resident window; deeper, it is an import. A sweep never removes a pinned closure. | Depth-3 switch with no restart. Depth-2,160 import. Sweep, then every pinned root reopens. | M1–M4 |
| P10 | `confirmed_ledger` | The ledger at the landed confirmed header | Its root equals the on-chain confirmed-state root at the view point. Merge effects apply when the merge tx lands, and are rewound if it rolls back. Status reports the level. Applying checks the base root before the delta. | A merge, then its rollback, restores the prior ledger. A wrong base root is refused. | N5 |
| PW | Wallet view (§8.5) | Available own UTxOs | Recomputed per head change. A dead intent's inputs reappear. No two live intents share a wallet input. | No "Missing vkey witness" after a fork | I2 |
| WP | Watcher: verification inputs, fault classification, proof objectives, user-event history | Per header | Objectives exist iff the target header is canonical and the fault is still provable. Signed decisions are class B. | A fork removes the faulty header and the objective drops. A re-land restores it. | W1, W3 |
| CP | Committee: headers awaiting attestation, availability obligations (promise, retention) | Per header | Signing is truthful across forks (§7.3, C4). Release and retention gate on `final`. | Sibling headers on two forks are both signed. A release waits for k. | C1, C3 |

## 6. Pipelined ingestion (proposal 8)

```
sidecar ──frames(seq)──► decode pool (W workers) ──summaries(seq)──► reorder buffer ──► ordered writer ──► DB
   ▲                                                                                        │
   └──────────────────────────── ack(seq committed) ◄───────────────────────────────────────┘
```

1. **Credit window.**
   - The follower grants the sidecar C frames: C = 50 while the cursor is more
     than k blocks behind the node tip, and C = 1 at the tip.
   - The sidecar pipelines ChainSync `RequestNext` up to the credit.
   - Each acknowledgement names the highest sequence number committed to the
     store. The sidecar never buffers more than C blocks, so a slow store
     slows the node, never the other way round.
2. **Decode pool.** A pool of `worker_threads`, W = min(4, cores − 1). Each
   worker turns one raw block into a block summary. The work is stateless:
   - parse the block;
   - take each tx's exact body, witness and aux byte slices, and hash the
     body;
   - extract inputs, reference inputs, collaterals, outputs (address, payment
     and stake credential, value, datum, script ref), mint, withdrawals,
     redeemers and validity bounds;
   - evaluate qualification rules (a) and (c) against the static part of the
     tracked set. That part is addresses, credentials and policies; it is
     fixed per manifest, except for wallets, which the writer passes in.

   Rules (b) and (d) need the live tracked-outref set, so they stay with the
   writer.
3. **Reorder buffer.** Summaries are released strictly in sequence order. A
   `roll_backward` frame is a barrier: the writer finishes everything before
   it, and the summaries after it are decoded only once it has been applied.
4. **Ordered writer.** This is the only writer of class A. For each summary:
   - resolve rules (b) and (d) against the in-memory **tracked-outref set**
     plus the outputs that the current batch has already created;
   - insert rows with multi-row `INSERT … SELECT unnest(…)`, or `COPY` during
     catch-up;
   - run the role's S3 derivations for D-t projections in the same
     transaction;
   - advance the cursor and commit.

   Batching:
   - during catch-up, one transaction per K blocks, where K is up to 100 or
     250 ms of work;
   - at the tip, one transaction per block.

   `synchronous_commit` stays on, and batching amortises the fsync.
5. **Tracked-outref set (class D-c).**
   - On start it is loaded from `l1_outputs WHERE spent_slot IS NULL`. Its
     size is the live tracked outputs, not history.
   - After a rewind it is patched from the rewind's own result (un-spent and
     deleted outrefs), so it never needs a full reload.
6. **External stores.**
   - The MPF owner and any other D-x consumer follow the cursor
     asynchronously. They are keyed by point and never block the writer.
   - A consumer that needs a root for point P waits until the owner reports
     P. It never reads a root that is ahead of or behind the facts.

**Result.** Per-block cost is O(txs in the block) to decode and O(qualifying
rows) to write. Neither term depends on the size of the stored state (G4).
B1 and B2 in §16.2 measure this.

## 7. Rollback (proposal 3)

### 7.1 The rewind procedure

`rewind(target)` runs as one database transaction, the same for every role
and every adapter:

1. `SELECT … FROM l1_follower_cursor FOR UPDATE`. In SQLite, the transaction
   is `BEGIN IMMEDIATE`.
2. Require that `target.hash` is in `l1_blocks`. If it is not, this is
   intervention case R1 or R2 (§7.5).
3. For each registered D-t table, in reverse dependency order:
   - `DELETE` the rows created after `target.slot`;
   - set `removed_slot = NULL` (or `to_slot = NULL`) where it is greater than
     `target.slot`.
4. Facts:
   - un-spend the outputs spent after the target;
   - delete the outputs created after the target;
   - delete txs and blocks after the target (cascade).

   ```sql
   UPDATE l1_outputs SET spent_slot = NULL, spent_tx = NULL WHERE spent_slot > :t_slot;
   DELETE FROM l1_outputs WHERE created_slot > :t_slot;   -- cascades l1_output_assets
   DELETE FROM l1_txs     WHERE block_slot   > :t_slot;   -- cascades l1_tx_mint_policies
   DELETE FROM l1_blocks  WHERE slot         > :t_slot;
   ```

   Seed rows (`created_slot IS NULL`) are never touched.
5. Set the cursor to the target and increment `generation`. Insert the
   `l1_rollbacks` row.
6. Commit.
7. After the commit, publish the new generation in process. For Postgres,
   also `NOTIFY l1_generation`.

Cost is O(rows created after the target), which is O(depth × qualifying rows
per block), and does not depend on the size of the state.

After the commit:

- the MPF owner switches to the root of the newest `mpf_roots` row at or
  before the target (§10);
- the planner re-plans from the new view (§8);
- nothing else runs.

### 7.2 Temporal tables

Every D-t table has one of two shapes:

- **Versioned state:** `(key…, value…, from_slot, to_slot NULL)`, with a
  partial unique index on `(key…) WHERE to_slot IS NULL`. An update closes
  the current row and inserts a new one.
  - Examples: queue node status, event status, operator set, correction
    admissions, `confirmed_ledger` entries, `mpf_roots`.
  - A read at the tip uses `to_slot IS NULL`.
  - A read at point P uses `from_slot ≤ P.slot AND (to_slot IS NULL OR
    to_slot > P.slot)`.
- **Append-only log:** `(…, created_slot)`.
  - Examples: landed own-block effects, forced-order fields (§12), committee
    observed headers.

Slot alone is enough to order rows: only one chain is stored, and a rewind
cuts at a slot.

Each table registers:

- its name;
- its slot columns;
- its parents, for ordering;
- its retention rule (§11).

The rewind SQL is generated from the registry.

**Determinism rule.** An S3 derivation is a pure function of class A rows,
class B and C content, and the manifest. It may not read the wall clock, use
randomness or call the network. A lint enforces this: S3 modules cannot
import `Date`, the sidecar client or an HTTP client.

**One property test for every table (ticket F2).**

- A generator produces a random chain from the origin, plus a random
  sequence of `apply(block)` and `rollback(depth ≤ min(k, height))`
  operations.
- After every step, every registered table and every fact table must equal a
  fresh replay of the current canonical chain from the origin.
- The test runs for each role's derivation set, against both adapters.
- Each new projection adds itself to the registry and is covered with no new
  test code.
- Schema lint: every table in every migration declares a class. Every D-t
  table is registered.

### 7.3 What "recompute" means per role

- **Node.**
  - Rewinding the projections reverts every L1-derived fact the node acts
    on: queue, events, key set, deposits, operator set, correction
    admissions, `confirmed_ledger` and roots.
  - Own L2 content is class B: own block bodies and signed commits. It stays.
    Its status is derived from the facts (§8.2).
  - The planner builds only on the landed queue tip
    (2026-09-26, 2026-10-03), so a rolled-back own commit is just a live
    intent that whichever-lands-wins settles (§8.3).
  - The mempool-ledger cache rebases on the new landed tip. It applies the
    delta between the old and new `confirmed_ledger` versions plus own blocks
    that are still live, so it never fully reloads (`demo/midgard-node/src/services/mempool-ledger-cache.make-mempool-ledger-cache-service.ts:95-127` today).
- **Watcher.**
  - Header verification inputs, fault classification and proof objectives
    are projections, so they rewind.
  - In-flight proof txs are intents.
  - Fault decisions already signed are class B, kept with derived status.
  - The completion marker (2026-10-04) stays and gates on k.
- **Committee.**
  - The landed-queue projection rewinds, and the set of headers awaiting
    attestation is recomputed.
  - Signed availability decisions are class B.
  - Signing a header on one fork and a sibling header on another is truthful,
    because the signature commits to content, not to a chain position.
    `deriveExpectedDaAvailabilityCommitment` binds the deployment identity,
    header hash, payload and response geometry (`demo/da-committee-node/src/peer/signatures.ts:84-101`, called at
    `demo/da-committee-node/src/committee-service.sign-verified-payload.ts:27`).
  - Ticket C4 confirmed this (owner ruling 2026-10-07): equivocation is one
    signer, one header hash, two commitments
    (`docs/midgard/decisions/da-sibling-signatures-not-slashable.md`).

### 7.4 Correction rollback (rev-4 O1, brick K5)

1. Correction admission becomes a versioned D-t projection
   (`queue_corrections`), with rows for removed headers stamped at the slot
   of the correction tx.
2. The desired ledger root is a function of the canonical queue projection:
   - when a correction is admitted, it is the root of the last surviving
     block;
   - when the correction rolls back, the same rewind restores the earlier
     rows, so the desired root is the restored tail's root.
3. That root is still pinned, because every canonical root within k is
   retained (§10.4).
4. The removed headers' content is still there: own blocks are class B, kept
   until k, and foreign payloads are class C, kept while referenced.

The rollback of a correction is therefore "rewind, then switch root". There
is no halt and no special path.

Admission may still wait for cd before acting. That avoids thrash and is a
liveness choice. It is not needed for durability.

Beyond k, this is intervention case R1.

### 7.5 Intervention cases (the complete list)

In each case:

- `/readyz` fails with the given reason;
- read APIs keep serving;
- duties that depend on the failed condition are held;
- the process **stays up**, so a supervisor restart can never loop.

A process may exit only before its `/readyz` listener is bound, for example
on an unparseable config or a port conflict.

| # | Reason | Trigger | Operator action |
|---|---|---|---|
| R1 | `rollback_beyond_k` | `roll_backward` to a point more than k blocks below the stored tip, or below the oldest retained block | Investigate the cardano-node or the chain. A Cardano node would not do this unless the chain broke its security assumption. Recovery: `follower reset --to-origin`, a full replay. |
| R2 | `intersection_outside_history` | `FindIntersect` matches none of the offered points, which include the origin | Wrong network or a wiped node DB. Fix the node, then reset. |
| R3 | `origin_after_protocol_init` | Caught up, but the `hubOracleOneShot` spend was never seen (§5.3) | Correct `l1Origin`. |
| R4 | `origin_not_on_chain` | The origin hash is not on the node's chain | Correct `l1Origin`, or the network. |
| R5 | `store_integrity` | Invariant INV1–INV6, or the registry check, fails at start or after a rewind | Restore from a backup, or reset. |
| R6 | `mpf_closure_missing` | `ImportClosure` cannot find a record reachable from a root that is within k and pinned. On the node: the landed-block rebase's native restore finds no root of the processed landed chain retained in full in the native MPF store | Stop the node, install at `LEDGER_MPF_DB_PATH` a native MPF store that retains the root in full, and restart; the next rebase completes. |
| R7 | `operator_removed` | The operator is no longer in the active set (D-N7) | Expected end state; re-register or retire. |
| R8 | `wallet_below_floor` | Own wallet funds fall below the fee floor for pending intents | Fund the wallet. An automatic refill loop is open decision-register item DR-B3 (`public-testnet-decisions-2026-10-01/source-context.md:139`), not decided here. |
| R9 | `manifest_mismatch` | The config does not match the finalised manifest identity | Fix the config. |
| R10 | `origin_mismatch` | The configured `l1Origin` differs from the origin the follower store was initialised at | Restore the previous `l1Origin`, or run `follower reset --to-origin` to replay from the new one. The reset never deletes class B or class C rows: signed material stays, and foreign payloads stay while referenced. |

The node's own holds that need more than waiting each belong to a class
above or are listed here with what clears them. Each keeps the process up,
names its reason on `/readyz`, and is re-evaluated on every driver run (a
startup step: on every run of the step):

| Reason | Class | Trigger | What clears it |
|---|---|---|---|
| `landed_block_own_journal_mismatch` | R5 | This node's own landed block disagrees with its journal: another base or root, or a delta that does not reach the header's root on the parent's ledger | Restore the node database from a backup that holds the block's journal; the next driver run adopts the block from it. |
| `mpf_closure_missing` | R6 | See R6 | See R6. |
| `l1_own_block_event_orphaned` | none: not an intervention | A deposit or withdrawal included by this node's own landed block left the chain (a rollback the commit anchor did not cover). Commits hold, and so does the merge of that block and of every block built on it; a spend of an output projected from the event is rejected. Admissions, ancestor merges, DA, follower ingestion and retention continue | The block leaves the landed queue: a landed correction removes it, or a rollback does. No action on this node. |
| `landed_block_invalid` | none: the block's, not this node's | A foreign landed block does not replay to its header's root or does not link to its parent. It is never adopted and nothing after it is processed | The block leaves the landed queue: a landed correction removes it (§7.4), or a rollback does. No action on this node. |
| `native_mpf_restore_index_cap_exceeded`, `native_mpf_promotion_index_cap_exceeded` | build limit | The root to restore or promote has a full index over `FULL_INDEX_MAX_RECORDS` or `FULL_INDEX_MAX_BYTES`, the caps fixed in the node build. Nothing is written | A promotion: one that fits (merges and withdrawals shrink the ledger), or a canonical restore of another root. Either: a node build whose caps cover the root. |
| `landed_block_follower_schema_missing` | install | The follower's admission tables (`l1_event_keys`, `node_l1_forced_order_fields`) are missing from the node database | Run `migrate`; the next driver run clears it. |
| `protocol_initialization_failed` | R9, or deployment | A run of the startup protocol check fails: the deployment manifest does not match the config (R9), an availability-challenge reward account is not registered, or a runtime reference script is missing or differs. The node has not started serving; the check runs again from the start on a backoff that grows to 30 s, re-reading the deployment status first | Fix what the log names (the config, the registration or the reference script); the next run passes and startup goes on. No restart. |

Everything else is transient and recovers automatically with backoff, while
`/readyz` reports a reason:

- the cardano-node is unreachable or still syncing;
- the sidecar crashed;
- Postgres is down;
- a peer is unreachable;
- DA is not yet published.

That matches the 2026-10-01 directive.

**Watcher journal integrity.** The watcher's journals
(`watcher-journals.sqlite`) authenticate each row with a MAC under the
rollback key over its journal, key, scope, state, revision and body. Each
journal's head carries a MAC over its revision, its chained revision digest,
its live row count and a keyed sum of its rows' MACs, and the latest
revisions keep their chained digest and delta. Startup verifies every row's
MAC, the row count and keyed sum against the head, and the retained
revisions' order, MACs and chain. A failed check refuses the journals for
the rest of the process: the watcher stays up and reports
`journal_integrity` on `/readyz` until an operator repairs the journals and
restarts it.

## 8. Generations, views and intents (proposals 9 and 10)

### 8.1 Generation and view binding

The **generation** g is `l1_follower_cursor.generation`:

- it increases by exactly one per applied rewind, in the rewind's own
  transaction;
- it never decreases;
- a forward block does not change it.

A **view** is V = (g, P_b), where P_b is the cursor point that the planner
read.

**Validity check.** `viewValid(V)` runs inside the same transaction as the
write it guards, under `SELECT … FROM l1_follower_cursor FOR SHARE` (in
SQLite, the writer's `BEGIN IMMEDIATE`):

```
viewValid(V) =
  cursor.generation == V.g                         -- fast path: no rewind since planning
  OR EXISTS (SELECT 1 FROM l1_blocks WHERE hash = V.P_b.hash)   -- P_b is still canonical
```

The second clause is sound for this reason. A rewind deletes only rows above
its target. So if P_b survived every rewind since g, then every fact and every
temporal row at or below P_b is unchanged, and those are exactly the rows the
planner read.

Why not one of the two alone:

- the generation alone invalidates work for rollbacks that never reached
  P_b;
- the point alone needs a lookup on every check, even though the fast path
  answers almost every check.

The `l1_rollbacks` log lets a consumer whose cache is keyed by generation
find out how deep each rollback went without reloading.

The check is used in three places:

| Guarded write | Stale view means |
|---|---|
| S5: insert an intent row for a newly signed tx | The row is still written, because class B never erases signed bytes, but with a `stale_at_write` event and no submission. Reconciliation (§8.3) decides whether the tx can still land and is still wanted. It was never submitted, so it is normally abandoned. |
| S6: submit or resubmit | Submit only if the view is still valid, or if reconciliation says "can land and wanted" under the current view. |
| Durable side effects (release, prune, signing a committee decision) | They gate on the final level (§9), not on the view. |

**The commit anchor.** A commit is planned at its write permit's view P. Its
anchor A is the follower block d below P (height h(P) − d), d the deployment
profile's commit-event depth, and its header end E is at most
time(A) + W − 1 (and the forced-order bound). An event is due at
I = valid_to + W, so every included event has valid_to ≤ time(A) − 1: its
L1 block is strictly below A, on A's chain. While A is canonical, so is every
included event. The journal stores A and rechecks E against it in the gated
journal transaction; the commit is signed (S5), kept by S6 (which also waits
until the tip is d blocks above A) and kept by the own-journal disposition
only while A is canonical. Once A is pruned (more than k below the tip) it
counts as canonical while no other block is stored at its height.

**Residual risk (top risk 1).** A commit submitted under a valid view can be
sitting in the cardano-node mempool when a rollback removes the deposit or
order tx behind an event it includes. If the commit's own inputs still exist,
it can still land. It would then commit an event that no longer exists on L1.

- What bounds it:
  - the commit anchor: such a rollback must remove A, so it is deeper than d
    blocks below P;
  - whichever lands wins: on such a rollback, the planner immediately builds
    a replacement on the same tail without the event, which races the
    original.
- The risk remains above zero only for rollbacks deeper than d that hit a
  commit still in flight. On mainnet d = k, so it remains only after a
  rollback beyond k (R1).
- An own block that lands without such an event holds commits and its own
  merge (`l1_own_block_event_orphaned`, §7.5) until it leaves the landed
  queue.

### 8.2 Intent journal (class B)

```sql
CREATE TABLE l1_intents (
  tx_hash         bytea  PRIMARY KEY,
  family          text   NOT NULL,     -- commit, merge, attest, correction, register, …
  workflow_key    text   NOT NULL,     -- e.g. 'commit:tail=<outref>', 'attest:<header>'
  tx_cbor         bytea  NOT NULL,     -- signed bytes, exactly as submitted
  inputs          bytea[] NOT NULL, reference_inputs bytea[] NOT NULL, collaterals bytea[] NOT NULL,
  own_outputs     jsonb  NOT NULL,     -- predicted own-wallet outputs (wallet view, §8.5)
  valid_from_slot bigint, valid_to_slot bigint,
  depends_on      bytea[] NOT NULL,    -- tx hashes whose outputs this spends or references
  built_generation bigint NOT NULL, built_slot bigint NOT NULL, built_hash bytea NOT NULL,
  content_ref     bytea                -- class B/C content this tx commits to (own block body hash)
);
CREATE TABLE l1_intent_events (
  tx_hash bytea REFERENCES l1_intents, seq integer, kind text NOT NULL,
  -- signed | stale_at_write | submit_attempt | submit_rejected | abandoned | superseded_by
  detail jsonb, tip_slot bigint, at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (tx_hash, seq)
);
```

**Invariant (checked at insert).** Every input, reference input and
collateral of an intent is in the role's tracked set. Own wallets and
protocol addresses always are. This means reconciliation can always decide
from the facts. A build that breaks the invariant is a defect and is refused.

**Status is a view, never a stored column.** A rollback that un-lands a tx
therefore makes it live again with no write:

| Derived status | Definition |
|---|---|
| `landed(depth)` | `tx_hash` is in `l1_txs` with `is_valid`. The depth comes from the heads module. |
| `failed_landed` | In `l1_txs` with `NOT is_valid`; the collateral was consumed. |
| `conflicted(spender)` | Some input has `spent_by ≠ tx_hash` in `l1_outputs`. |
| `expired` | Tip slot ≥ `valid_to_slot`. Only the tip slot decides; the wall clock may only mark it "likely expired" (§3.6). |
| `dependency_dead` | Some `depends_on` intent is dead. |
| `abandoned` | An `abandoned` event exists, and the tx has not landed. |
| `live` | None of the above. |

"Dead" means conflicted, expired, dependency_dead or abandoned. A dead intent
that later lands (abandoned, then landed) shows as landed: the facts win.

**Retention.** Rows are deleted only after the intent has been terminal for k
blocks: landed k deep, or dead with every input either spent k deep or past
its validity k deep.

### 8.3 Reconciliation outcomes and whichever-lands-wins

Reconciliation runs as stage S6 on every head change and every generation
change. It is a pure function of (facts, intents, mempool snapshot), and its
actions are idempotent:

| State | Action |
|---|---|
| `landed`, depth ≤ k | Follow. Derivations that depend on it proceed. If the intent is an own commit that was abandoned or replaced, **revive** its block: its content is class B and still there (2026-09-26). |
| `landed`, depth > k | Terminal. Prune after retention. |
| `live`, `HasTx` true (LocalTxMonitor) | Wait. |
| `live`, not in the mempool, family predicate true | Resubmit the exact bytes, at most once per tip change. |
| `live`, family predicate false | Abandon. Example: a merge whose queue head is gone. |
| `conflicted`, spender is an own intent | Superseded. Nothing to do. |
| `conflicted`, spender is foreign | Dead. The planner re-plans from the facts. Example: the tail was taken by another operator's commit. |
| `expired` | Dead. The planner proposes a replacement if the workflow still wants one. |
| `dependency_dead` | Dead, cascading to dependants. |
| `failed_landed` | Dead. Collateral rule from 2026-10-04 (below). |

**Commits.**

- The old commit and its replacement both spend the same tail node, so they
  are mutually exclusive on L1.
- Whichever lands is followed. If the old one lands, its block is revived and
  the replacement becomes `conflicted` by an own spender.
- No coverage proof and no retention hold are needed. That is the 2026-09-26
  ruling. It deletes `demo/midgard-node/src/services/history-expired-intent-release*`
  (24 files, 4,568 lines) and the coverage path once I3 lands (RN1).
- The planner builds only on the landed queue tip. A live, unlanded own block
  is never a base (2026-10-03: commits wait one L1 block). LV-ND1's narrow
  commit skip is kept.

### 8.4 Families per role

| Role | Family | Family predicate (can still land and still wanted) |
|---|---|---|
| Node | commit | The tail outref is unspent, the scheduler slot is still ours at `valid_from`, and the included events are still canonical and at least d behind the tip. |
| Node | merge | The queue head and the confirmed node are unchanged. |
| Node | DA attestation, attestation-timeout correction | The header is still in the landed queue. The correction condition still holds. |
| Node | register, activate, exit, takeover, retire, recover-bond | The operator-set projection still allows the transition. |
| Node | reserve payout, settlement | The event is still in the list projection, and not settled by another tx. |
| Node | reference publication or sweep, script-reward registration, PHAS membership | The target state has not been reached by the facts. |
| Watcher | fault-proof step tx | The objective projection still lists the step, and the target header is still canonical. |
| Watcher | collateral, prover funding | Collateral rule (below). |
| Committee | attestation or availability tx, bond-pool top-up or withdraw, promise or retention tx | The obligation projection still lists it. |
| Committee | signed availability decision (not a tx) | Never reconciled. A class B record, retained to k after its header is final or gone. |

**Collateral (2026-10-04).**

- An unretired attempt never gates later actions.
- Collateral is released when the attempt is confirmed, and re-signed if a
  rollback removes that confirmation.
- Attempts that share a funding input are mutually exclusive, through the
  same input-conflict rule as commits.

### 8.5 Wallet view

```
available(wallet) = live own outputs at the tip (facts)
                  − inputs and collaterals of live intents
                  + own_outputs of live intents (predicted change)
```

It is recomputed on each head change. It replaced
`demo/midgard-node/src/operator-wallet-view.ts` (169 lines, since deleted by I2) <!-- doc-links:historical -->
and the Lucid wallet override, which freezes the UTxO view (`overrideUTxOs`). So there is no stale pin, and
"Missing vkey witness" for your own key cannot happen. The same function
serves the watcher prover wallet and the committee submitter wallet.

## 9. Heads module

There is one module per process, and it is the only place that knows k, cd
and how depth is counted. A lint forbids any other module from comparing
against `confirmationDepth` or `automaticRecoveryMaxDepth`.

**Depth.** `depth(point) = tip.height − point.height + 1`, so the tip block
has depth 1. This one definition replaces the three variants listed in §3.5.

**Levels**, for any tx, block or L2 header:

| Level | Condition | Meaning | May gate |
|---|---|---|---|
| `local` | In an own intent or own block, not landed | Ours, not on L1 | Nothing durable |
| `landed` | depth ≥ 1 | On the current chain | Building on it (commits wait one block), derivations |
| `safe` | depth ≥ cd | Unlikely to roll back. **Liveness only.** | Waiting, API display, starting slow work. Never deletes, releases or retires. |
| `final` | depth > k | Durable: at least k blocks on top, so no legal rollback (at most k blocks) removes it | Pruning, releasing, retention deletes, intent retirement, completion markers, DA payload deletion, slashing-evidence release |
| `merged` | The L2 header's merge tx is at `landed` or deeper. Reported together with that tx's level. | Merged into confirmed state on L1 | The same as the merge tx's level |

The 2026-10-01 ruling redefines D2 this way: the durable level is `final` (k),
not cd. The API reports `{point, depth, level}` for each head. Tx status
reports the level, so `merged` is never claimed bare (D-N5).

**Time.**

- `slotNow` = max(tip slot, last tip slot + elapsed / slotLength). Era
  history comes from LSQ.
- It is the only "now" allowed for L1 validity decisions (§3.6).
- The wall clock can still pick `valid_to` for new txs. It can never declare
  an intent dead.

## 10. Native MPF versioning (proposal 7)

### 10.1 Today

| Fact | Evidence |
|---|---|
| The resident index is append-only across promotions in one child's lifetime. A commit appends `new_records` and never removes any. | `demo/midgard-node/native/mpf-event-flat-wasm/src/owner/runtime_owner.rs:160-202` |
| Forking requires the base to equal the root marker | `demo/midgard-node/native/mpf-event-flat-wasm/src/owner/runtime_owner.rs:44-48` |
| Caps: 2M records and 2 GiB resident. A breach fails `PreparePromotion` every time from then on (K7). | `demo/midgard-node/native/mpf-event-flat-wasm/src/owner.rs:16-18`; `demo/midgard-node/native/mpf-event-flat-wasm/src/owner/runtime_owner.rs:146-152`; `demo/midgard-node/native/mpf-event-flat-wasm/src/owner/compact_index.rs:119-120,256-258`; `demo/midgard-node/src/services/mpf-native-owner/protocol.ts:10,16` (`maxResidentNodes` 2M, `maxActiveGenerations` 2) |
| Load contract: a single root, and any record unreachable from it is rejected | `demo/midgard-node/native/mpf-event-flat-wasm/src/owner/compact_index.rs:359-373` (`authenticate_complete_closure`) |
| Restoring an older root reads the full index, closes the RPC, starts a new child, batch-writes `__root__`, clears the generation leases and counts a restart | `demo/midgard-node/src/services/mpf-native-owner/service.production-native-mpf-owner-service.ts:460-560` (sidecar disabled at `:503`, `__root__` at `:535`, `:548-549`) |
| No durable GC of Level. Deletes are skipped while the live arena is on. Only the per-block in-memory arena is pruned to the current root. | `demo/midgard-node/src/mpf/root-view-store.ts:556-580`, `:1384-1416` |
| Architecture G is the only engine | `6794e37cd` |

### 10.2 Target

**Tables (node Postgres).**

- `mpf_roots`: versioned D-t. Columns: `(queue_position, header_hash, root,
  from_slot, to_slot)`, the canonical ledger root at each landed queue
  position. It also has rows for own local roots, with `from_slot NULL` and
  linked to their intent.
- `mpf_root_pins`: `(root, reason, holder, until_point NULL)`. Reasons:
  - `canonical_within_k`: maintained by the derivation;
  - `live_intent`: own blocks not yet terminal;
  - `proof`: watcher objectives, and merge proofs while in flight;
  - `operator`: manual.

**Native owner RPCs.**

| RPC | Precondition | Effect | Cost |
|---|---|---|---|
| `SwitchRoot(root)` | No active generations, and the root is in the resident index | CAS `__root__` from the expected value to `root`; clear the generation leases; drain readers (`readsDrained`, `:636`); bump the epoch. No child restart. | O(1) |
| `ImportClosure(root)` | The root is in Level | Walk from the root and append to the resident index every record it is missing. The index stays append-only. | O(missing records); about one block's dirty set per block of depth |
| `Sweep(budget)` | No active generations (a quiescent window) | Mark from every pinned root in `mpf_root_pins`, then sweep unmarked Level records, resumably and within the budget | O(store) off the block path |
| `Compact()` | Planned | Restart the child, loading only the closure of the pinned roots resident inside k | O(resident window), off the block path |

**Load contract change.** `authenticate_complete_closure` accepts a record
iff it is reachable from **any** root in the pin set that the owner supplies
at load. It still rejects orphans.

**Rollback flow.**

1. The rewind (§7.1) truncates `mpf_roots`.
2. The owner reads the desired root.
3. If the desired root is resident, call `SwitchRoot`.
4. Otherwise call `ImportClosure`, then `SwitchRoot`.
5. If the closure is missing from Level, raise intervention R6.

There is no restart and no index rebuild on any path within k.

**Old roots stay readable.** Proofs (watcher) and merge witnesses can be
generated at any pinned root. This replaces rev-4 tiers 1 and 2. It needs
neither a confirmed-root restore nor a per-block replay log.

### 10.3 Why the resident window cannot be k

Estimated dirty-node rates are about 90, 700 and 5,400 nodes per block at
10, 100 and 1,000 L2 txs per block. The estimate assumes about 3 touched
UTxOs per tx at trie depth about 5 for N ≈ 10^6, with upper levels shared.
Bench B4 measures the real rate:

| L2 txs per block | Dirty nodes per block | × k = 2,160 | Versus the 2M resident cap |
|---|---|---|---|
| 10 | ~90 | ~194k | Fits |
| 100 | ~700 | ~1.5M | 75% of the cap, with no room for the live set |
| 1,000 | ~5,400 | ~11.7M | 5.8× the cap |

So:

- the resident window is bounded by the cap, and a compaction is triggered
  above 75% of it;
- Level keeps every pinned root's closure for k. At 100 txs per block that is
  about 1.5M nodes, or about 340 MB of disk at roughly 220 B per node;
- rollbacks deeper than the resident window use `ImportClosure`.

The cap becomes a compaction trigger, never a permanent error. That fixes K7.

### 10.4 Retention

- A root is pinned `canonical_within_k` while its `mpf_roots` row is current,
  or closed less than k blocks ago.
- `live_intent` pins drop when the intent becomes terminal.
- `Sweep` runs when the store has grown by 20% since the last sweep, or
  daily, within a budget (target in §16.2).

### 10.5 `confirmed_ledger` and merge

- `confirmed_ledger` becomes versioned D-t. It is keyed by outref, with
  `from_slot` and `to_slot` taken from the merge tx that added or removed the
  row.
- A merge applies the merged block's delta: O(delta).
- The merged root is the `mpf_roots` row at that queue position, so there is
  no from-scratch root and no `LOCK TABLE … EXCLUSIVE`. That fixes NC11.
- A rolled-back merge is rewound like anything else, which makes rev-4
  "`confirmed_ledger` at settled depth" (ticket 25) unnecessary.

### 10.6 The from-scratch cross-check

`MPF_PAYLOAD_ROOT_CHECK=every_block` is a correctness net against a native
trie defect. Correctness comes first, so this plan keeps it on by default,
but its cost changes:

- one from-scratch root per commit, over the post-state;
- not the k + 1 roots that hydration does today (NC8).

The rest goes:

- the hydration forced by any candidate tx;
- per-journal materialisation;
- the full `confirmed_ledger` read on every commit.

The commit base is the native root at the parent queue position.

Ticket N8 measures the remaining check against the per-block budget at
N = 10^6. If it does not fit, the numbers go to the owner then. That is a
future question, not one asked now.

## 11. Retention

Every table states its rule in its migration header as
`-- class: <A|B|C|D-t|D-x>; retention: <rule>`. The F2 schema lint fails any
table that lacks one. A soak test (B8) holds the live set fixed and requires
every table to plateau.

| Table | Class | Kept while | Pruned when |
|---|---|---|---|
| `l1_blocks` | A | depth < k, or referenced by a retained `l1_txs` row | Deeper than k and unreferenced. Every 1,000th block is kept as an intersection checkpoint. |
| `l1_txs` (+ mint policies) | A | Any output it created is live (R1b: the body lives with its UTxO), or a retained intent or D-t row references it, or depth < k | All of its outputs are spent at least k deep, and nothing references it (rev-4 O4) |
| `l1_outputs`, `l1_output_assets` | A | Unspent | The spend is at least k deep (O4) |
| `l1_scripts` | C | Any live output's `script_ref_hash` points at it (R1b) | Unreferenced and at least k deep |
| `l1_event_keys` | A | Always (the never-reuse set, O4) | Never |
| `l1_rollbacks` | A | The last 1,000 rows | Older rows |
| D-t current rows | D-t | `to_slot IS NULL` | Never, while current |
| D-t closed rows | D-t | `to_slot` < k deep | `to_slot` at least k deep |
| `l1_intents`, `l1_intent_events` | B | Not terminal, or terminal less than k deep | Terminal at least k deep (§8.2) |
| Own block bodies and headers | B | The intent is live, or the block is landed and not yet `final`-merged | The block is merged and final, or dead at least k deep. DA payload retention is separate (next row). |
| Committee decisions, watcher fault decisions | B | The header is not final or gone | Final or gone at least k deep. The watcher completion marker (2026-10-04) stays as specified. |
| DA payloads | C | `RETENTION_DAYS` from the manifest (devnet value: open decision-register item DR-B5, `public-testnet-decisions-2026-10-01/source-context.md:141`), and not yet retired | A retirement proof deeper than k (existing gate: `demo/midgard-node/src/database/daPayloads.ts:276-339`), and the cutoff measured by `slotNow`, not wall clock (§3.6) |
| `l1_forced_order_fields` (and `carriage_pending` rows, §12.4) | D-t | The order output is live, or the block that included the forced tx is not yet settled | The order is burned at least k deep and the including block is settled; DA retention then holds the preimage |
| `mpf_roots`, `mpf_root_pins` | D-t / D-x | §10.4 | §10.4 |
| Watcher user-event archive (K4) | D-t | Less than k deep, and within `RETENTION_DAYS` | Both bounds passed (L5) |

Two tables only ever grow, and both are bounded:

- `l1_event_keys`, by events ever created, at one small row per event;
- the intersection checkpoints, at one row per 1,000 blocks.

## 12. Carriage and by-hash content

**Decided (owner, 2026-10-07): no on-chain change.** Carriage stays where
#594 put it, in the order creator's own wallet, reclaimable at any time. The
operator node resolves it off-chain, from chain data alone, when the follower
applies the order block.

### 12.1 The gap

- The follower always has the order itself. An order tx mints under the
  tx-order policy, so it qualifies (§5.2), and its raw CBOR carries the mint
  redeemer with the carriage vector.
- The carriage bytes are not in the order tx. Each `RawUtxo` entry, and each
  `Certified` chunk, names a reference input of the order tx: an output that
  an earlier tx created, usually at the creator's wallet. The order tx lists
  only its outref.
- When that earlier tx was applied, nothing marked the output as carriage,
  so the follower kept nothing. The mint proved the output was live when the
  order landed, but the creator may spend it right after.

### 12.2 Who needs the bytes from L1

Only the operator node. Everyone else takes them from DA once a block
includes the forced tx, already bound to the order's commitments:

| Consumer | Source | Evidence |
|---|---|---|
| Operator node, forced-tx admission | L1, this section | `demo/midgard-node/src/fibers/fetch-and-insert-tx-order-utxos.tx-order-utx-oto-entry.ts:75-83` |
| Watcher, block replay | DA payload (`fullTransactionCbor`) | `demo/midgard-watcher/src/verification/block-replay.evaluate-watcher-block-replay.ts:166-173` |
| DA committee | DA payload (`forced_transaction_preimages`) | `demo/da-committee-node/src/da/payload.validate-da-payload-consensus.ts:89-97` |
| Omission fault proof | No bytes needed | `onchain/aiken/validators/fraud-proofs/transition-trace/l1-event-yield.ak:143-232` |
| Wrongful-verdict and dispute steps | The prover's own copy, digest-checked | `docs/spec/midgard-tx.md:644-652` |

### 12.3 Resolution, in order

The node resolves every referenced outref of an order when the follower
applies the order block. Resolution runs in the S2 decode stage (§6),
pinned to the order block's parent point, so the single writer never waits on
network I/O. The writer persists the result in the block's transaction.

1. **Same block.** If the outref was created by an earlier tx of the order's
   own block, read it from that block. The follower holds the block, and a
   reference input cannot come from a later tx.
2. **Ledger state at the parent point.** LocalStateQuery `GetUTxOByTxIn`,
   acquired at the order block's parent point. This is exact even if the
   output was spent afterwards. It works while the parent point is within the
   node's last k blocks, about 12 hours.
3. **Current UTxO set.** If the parent point is older than k, query
   `GetUTxOByTxIn` at the tip. An unspent output never changes, so if it is
   still there, its bytes are the bytes the mint checked.
4. **Content fetch by tx id.** Fetch the creating tx by the outref's tx id from
   a configured source: a peer operator, a public indexer, or the creator.
   Check that blake2b-256 of the body equals the tx id, then read the output.
   Peer operators may instead serve the materialised field preimages by order
   outref; those are checked against the order's field commitments. No source
   is trusted. This is the "fetch a body by hash" use that proposal 1 allows.
   It never decides what is live.

Rules for every step:

- `Certified` chunks resolve the same way, one chunk outref at a time. The
  certificate itself is never read: the certificate policy is shared and
  unparameterised (`demo/midgard-sdk/src/user-events/contracts.ts:126`), so a
  certificate can predate the origin, and the order's existence already
  proves the mint checked it.
- Reference inputs are taken in ledger set order, `(txHash, index)`, as
  `compareOutRefs` does today.
- Every resolved field is still checked against the order's field
  commitment (`verifyMidgardV1TxFieldPreimage`) before it is used.
- The result is one `l1_forced_order_fields` row (D-t). The one rewind
  truncates it with its block (§7.2).

### 12.4 When nothing resolves yet

This is the only case left: the node did not apply the order block within
about k blocks of it landing (it was down, or catching up), the creator has
already spent the carriage, and no configured source holds the creating tx.

- The order is recorded as `carriage_pending`, with its outrefs. The
  follower keeps applying blocks.
- The node retries step 4 with backoff. It reports unready
  `forced_order_carriage_pending` while a pending order is due for a block it
  must build. It never exits, and it never writes a rejection verdict.
- **Determinism.** The bytes are fixed by the tx id and the field
  commitments. A source changes only when a node gets them, never what any
  node decides.
- **Where it matters.** On mainnet the case is realistic. Forced orders fall
  due 36 h 8 min after landing (`config/deployments/mainnet.yaml:24`), so an
  operator that is down for longer than k comes back in time to include the
  order but past step 2's window. Every operator that was following captured
  the bytes at step 2, so a peer source normally closes it. The testing
  profiles use a 5-minute event wait
  (`config/deployments/preprod-testing.yaml:10`), so there the case needs the
  whole L2 to be down for longer than k.

### 12.5 Rejected: an on-chain custody rule

A design that required carriage to carry the stake credential
`Script(txOrderPolicyId)`, checked by the tx-order mint, was prototyped and
measured: +2.6k mem and +0.67M CPU per mint, +3.5% on the worst-case row. It
would make every follower capture carriage at creation, so step 4 would never
be needed. It was rejected (owner, 2026-10-07). It buys only independence from
an external copy in the §12.4 case, at the price of a protocol change, an SDK
custody helper and rewording #594.

The other options considered all lose to §12.3:

- **Inline-only carriage** caps forced material at roughly 12–14 KB
  (estimated) instead of 32,768 bytes.
- **Carriage as outputs of the order tx** has the same size cap and costs
  more.
- **A tracked token, address or pin for carriage** each needs an on-chain
  change.
- **Creation-point hints** are unverifiable on-chain.
- **A typed rejection for unreadable carriage** is not provable on-chain, so
  it would let an operator censor forced txs.

### 12.6 What this plan provides

1. **No new tracked-set rule.** The order tx already qualifies.
2. **Raw tx CBOR** for every qualifying tx (§5.2): the order's mint
   redeemer, list-replay bodies and creating bodies, with no by-hash fetch
   for anything in scope.
3. **LocalStateQuery** through the S0 sidecar (§4.2): `GetUTxOByTxIn` at an
   acquired point and at the tip.
4. **A content-source client**, node only: a configured list of endpoints,
   each answer verified as in step 4. An empty list leaves steps 1–3.
5. <!-- doc-links:future --> **Package.** The resolver lives in the shared
   follower package, `demo/midgard-l1-follower/`. Only the node calls it.
6. **No redeploy dependency.** Ticket N10 lands as soon as the follower does.
   The current Kupo and Ogmios reader, `demo/midgard-node/src/l1-tx-order-carriage*`
   (6 files, 1,398 lines), is deleted by N10 (§13).

## 13. Deletion list

Counts are `wc -l` at `17fdffd9b`, including split siblings
(`base.*.ts` and similar). "Rewrite" means the function survives on the new
substrate, and the old file goes. The program pull request removes every row
except the M2 row (§1.4).

### 13.1 Node (`demo/midgard-node/src/`)

| Group | Files | Lines | Removed by |
|---|---|---|---|
| `l1-event-history-*` (Ogmios ChainSync journal) | 17 | 3,624 | N1-close |
| `l1-ledger-snapshot.ts` | 1 | 289 | N1-close |
| `local-ledger-slot.ts`, `local-ogmios-slot.ts` | 2 | 114 | F7 |
| `services/event-history-owner*` | 11 | 1,761 | N1-close |
| `services/event-history-runtime.ts`, `event-history-producer.ts`, `event-history-recovery.ts` | 3 | 1,017 | N1-close |
| `database/eventHistoryJournal*`, `eventHistoryJournalCodec.ts` | 8 | 1,967 | N1-close |
| `database/eventHistoryLedgerReceipts.ts`, `eventHistoryLedgerRepair.ts`, `eventHistoryMaterialization.ts`, `eventHistoryReplayReceipts.ts` | 4 | 1,398 | N1-close |
| `database/eventHistoryAuthority*` | 3 | 581 | N1-close |
| `database/eventHistoryForeignCensus.ts`, `workers/commit-block-header.foreign-event-census*` | 2 | 454 | N3 |
| `database/foreignNativeAdoptions.ts` | 1 | 494 | N3 |
| `database/foreignTipReconciliations*`, `workers/t2-foreign-event-reconciliation*` | 12 | 2,728 | L8 |
| `fibers/speculative-commit-builder*`, `speculative-commit-state.ts`, `user-event-barrier-refresher.ts` | 8 | 1,857 | L8 |
| `services/history-expired-intent-release*` | 24 | 4,568 | I3 |
| `services/history-signed-header-recovery.ts`, `signed-intent-canonical-coverage.ts`, `database/eventHistoryCanonicalCoverage.ts` | 3 | 928 | I3 (the L6 hold already went in #759) |
| `services/history-recovery-state-queue.ts`, `history-pending-backoff.ts`, `canonical-journal-recovery.ts` | 3 | 722 | I1 |
| `database/mutationJobs.ts` | 1 | 212 | I1 |
| `fibers/block-confirmation*`, `workers/confirm-block-commitments.ts`, `workers/utils/confirm-block-commitments.ts` | 8 | 1,877 | I1 |
| `fibers/user-event-ingestion.ts`, `fetch-and-insert-deposit-utxos.ts`, `fetch-and-insert-withdrawal-utxos.ts`, `project-deposits-to-mempool-ledger.ts` | 4 | 617 | N1 |
| `services/state-queue-correction-recovery.ts`, `state-queue-correction-rewind*`, `state-queue-correction-ledger-restore*` | 9 | 2,174 | N4 |
| `services/native-mpf-local-finalization.ts` | 1 | 87 | M2 (deferred) |
| `services/state-queue-topology.ts` | 1 | 394 | N2 |
| `operator-wallet-view.ts` | 1 | 169 | I2 |
| **Node, deleted** | **127** | **28,032** | |
| Rewrite: `database/pendingBlockFinalizations*` (own-block store, class B; the status machine goes, and `finalized` is renamed) | 12 | 3,448 | I1 |
| Rewrite: `workers/utils/commit-submission*`, `workers/commit-block-header/pending-journal.ts` | 5 | 1,194 | I1 |
| Rewrite: `workers/commit-block-header.verify-foreign-base.ts`, `services/foreign-native-adoption*`, `foreign-confirmed-ledger.ts`, `history-landed-merge-ledger.ts` (in-order landed-block processing, #744) | 6 | 1,551 | N3 |
| Rewrite: `services/state-queue-correction-observer*` (as a temporal projection) | 10 | 2,025 | N4 |
| Rewrite: `fibers/fetch-and-insert-tx-order-utxos*` (§12) | 4 | 757 | N10 |
| **Node, rewritten** | **37** | **8,975** | |
| Kept, re-evaluated at N4: `database/eventHistoryRecoveryPlans*`, `services/history-dependent-recovery.ts` | 6 | 1,225 | — |
| Kept: `database/stateQueueMutationLeases*` (multi-process DB lease) | 4 | 714 | — |
| Carriage reader (Kupo + Ogmios): `l1-tx-order-carriage*` | 6 | 1,398 | N10 |

### 13.2 Watcher (`demo/midgard-watcher/src/`, `demo/midgard-fault-proofs/src/`)

| Group | Lines | Removed by |
|---|---|---|
| `demo/midgard-watcher/src/l1/multi-provider-consistency*` | 1,534 | W1 |
| `demo/midgard-watcher/src/l1/local-kupmios*` | 666 | W1 |
| Finality-engine external providers (`clone-external-providers`, `external-provider-bindings-match-policy`) | 568 | W1 |
| `native-chain-sync` start-with-retry, exact-point query, and the exact-point service and session added after `368263e5d` (`native-chain-sync.exact-point-service.ts`, `native-chain-sync.exact-point-session.ts`) | 1,082 | F1 |
| `demo/midgard-watcher/src/l1/rollback-engine/*` (state machine, durable authority, whole-snapshot CAS) | 6,375 | W3 |
| `demo/midgard-fault-proofs/src/workflow/local-kupmios-http-ogmios-source*` | 4,014 | W1 |
| `fp` local-kupmios raw L1 authority and read operation | 737 | W1 |
| **Watcher, deleted** | **14,976** | |
| Rewrite: fault-decision, queue and objective journals (as SQLite tables, after L2 compaction) | 1,166 | W2 |

### 13.3 Committee (`demo/da-committee-node/src/`)

| Group | Lines | Removed by |
|---|---|---|
| `committee-service.check-l1-rollback-feed.ts` | 286 | C1 (absorbs L1) |
| Recovery: `recovery-command`, incident, native proof, `recover-l1-source.ts`, `store/l1-recovery-*` | 620 | C1 (absorbs L1) |
| `l1/state-queue-replay-provider*` | 1,350 | C1 |
| Ogmios session files (`ogmios-rpc-session`, `run-ogmios-session`, `create-ogmios-chain-sync-request`, `request-ogmios-descendant-depth`) | 1,108 | C1 |
| Cursor stores (`file-chain-sync-cursor-store`, consumer cursor, `parse-persisted-chain-sync-state`, `same-persisted-cursor`) | 847 | C1 |
| JSON store, instance lock (replaced the lease in #769), mutex, JSON promise capacity | 1,233 | C2 |
| Lucid and multi state-queue providers, `provider-from-url` | 970 | C1 |
| **Committee, deleted** | **6,414** | |
| Kept: `store/postgres.*` (Postgres store and its advisory-lock instance lock) | — | — |
| Rewrite: `l1/state-queue-scanner.ts`, `l1/terminal-retention-observation.ts` (as projections) | 964 | C1 |

**Total deleted:** about 49,400 lines. About
11,100 more lines are rewritten. Kupo and Ogmios also leave the devnet
compose and tooling (ticket U4, deferred). After this program no role reads
them.

## 14. Migration and cutover

The program ships as one pull request (§1.4). Inside it, the work still runs
expand-then-contract, so every intermediate commit builds and its reached
tests pass.

1. **Straight cutover, per role (owner ruling 2026-10-07).**
   - There is no shadow phase and no per-role comparison with the current
     code. Matching the old code is the wrong measure, because part of its
     behaviour is what this program replaces.
   - Each role ticket (C1, W1, N1) switches that role's decisions to the
     follower and deletes its old code (§13) in the same change.
   - The gates are:
     - the ticket's acceptance tests, in both polarities;
     - simulator fork scenarios (F8) in which each of the role's
       projections equals a fresh replay of the facts after every
       operation;
     - the devnet journeys, with no Kupo or Ogmios configured;
     - the liveness rules: each intervention fails `/readyz` with a named
       reason, and the process never exits or restart-loops.
   - A difference from the old behaviour that turns up during
     implementation is reported when it suggests a missing requirement. It
     is never preserved.
2. **Order inside the pull request.**
   1. L8 (speculative mode) first, because it shrinks the node before N-work
      starts.
   2. The foundation: F1, F2, F8, then F3, F4, F5 and F7.
   3. The committee: C4 (gate), C1, C2, C3. The committee is the smallest
      role, it has the terminal brick, and its old path costs O(k) per tick.
   4. The watcher: W1, W2, W3.
   5. The node: N1, N2, then N3–N6 and N10, I1, I2, I5 and I3 with the U3
      off-chain cap.
   6. The §17 deletions and doc reconciliation.

   The lanes overlap wherever the dependency columns in §15 allow.
3. **The single redeploy** (after T405) carries:
   - the `l1Origin` manifest field (F3);
   - the U3 `event_wait` raise and the profile field for the commit-event
     depth d.

   It is not part of this program. Carriage needs no redeploy (§12.6).

   Until the redeploy, `l1Origin` and d come from operator config, and the
   startup assertions are identical. This PR changes no running deployment's
   identity.
4. **Existing deployment.**
   - `follower find-origin --tx <prepareHubOracleNonce txHash>` (F3)
     intersects at a known earlier point, streams forward to the block that
     contains the tx, and prints the preceding point for the config.
   - This is a one-time step. The origin is never guessed.

## 15. Tickets

Every ticket that changes offchain contract behaviour includes emulator tests
in both polarities: the honest path succeeds, and the adversarial or stale
path is refused at the exact check. "Fork simulator" means F8.

In this program (§1.4), each Phase 0 ticket is absorbed by the ticket that
replaces the code it patched, and its acceptance moves there. Rows marked
deferred are not part of the program pull request.

### Phase 0: liveness bricks, on the current code (no follower dependency)

| Ticket | Scope | Depends on | Acceptance |
|---|---|---|---|
| **L1** | **Absorbed into C1 (§1.4).** Committee: a rollback recomputes instead of quarantining (K1, D-C1). Delete the terminal merge rule, the quarantine early return and the recovery CLI. On any rollback below k, rewind the cursor and re-derive the queue view from the rollback point. Signed decisions stay as class B. | — | Rollbacks of depth 1, cd and cd + 1 across a signed decision: the committee keeps ticking with no CLI, and `/readyz` returns to ready within one tick after the node catches up. A rollback deeper than k: unready `rollback_beyond_k`, the process stays up, and it is not restarted. The decision for a header that disappeared is kept, never re-signed with different content, and never deleted before k. |
| **L2** | **Absorbed into W2 (§1.4).** Watcher journals: compaction per 2026-10-04. Completed objectives with a verified marker past k are skipped on restart, and only non-completed records count toward the cap. Compaction rewrites a journal to its live records under the same HMAC chain. | — | 100k retry cycles of one objective leave the journal at most 2× its live size, and a restart succeeds. A tampered compacted record is refused at startup. A cap reached by live records alone means unready `journal_capacity`, not a throw loop. |
| **L3** | **Absorbed into W3 (§1.4).** Watcher: in-process post-finality recovery. Losing a block that was final at cd recomputes in-process, bounded by k, and never sets `quarantined` (K3, D-W1). | — | Emulator: a rollback that removes a cd-final block resumes in-process with no restart. Rollback beyond k: unready. The startup path is unchanged. |
| **L4** | **Absorbed into N4 (§1.4).** Node: a correction rollback recomputes (K5, D-N1). Interim on the current code: restore the pre-rewind root through the existing `restoreRetainedRoot` path. That costs a restart, which is acceptable until M1–M3. Reinstate the removed own journals from class B, then let the observer re-derive. Delete `refuseRewoundStateQueueCorrectionRollback` and the `state_queue_correction_rewind` halt. | — | Emulator: a correction admitted at cd, then rolled back at depth cd + 1 (still below k): commit, merge and settlement resume with no intervention, and the ledger root equals a fresh replay. A correction that lands again rewinds exactly once. A rollback beyond k: unready, no exit. |
| **L5** | **Deferred; a separate issue (§1.4).** Watcher archive retention (K4): delete rows that are more than k deep and older than `RETENTION_DAYS` from the manifest. | — | A soak with constant event flow keeps the archive row count flat. A row still inside either bound is never deleted. |
| **L6** | **Done on the base branch (#759, `2b31ed070`); I3 keeps the check (§1.4).** Remove the `signedHeaderRecoveryHoldSlot` hold (RN1, K6). | — | The grep finds zero. A retention test with a long-lived signed header still prunes at k. |
| **L7** | **Split (§1.4):** `depth()`, `slotNow` and the §3.6 sites go to F7; D-N4 and D-N7 to N6; D-N8 to I1; D-N2 and D-N3 to I3; D-N5 is deferred with U1. cd sweep and `slotNow` (K8, §3.5, §3.6). The replacement halt becomes revival: a displaced own block that lands is followed. The operator-removed exit becomes unready R7. Settlement `confirmed`, tx-status `merged` and availability-challenge retirement use the level. Commit abandonment uses the chain-tip slot. One `depth()` function replaces the three variants. | — | For each D-N site, a test where a rollback deeper than cd crosses the decision: no halt, no exit, and the status reverts. A fake clock 10 minutes fast does not abandon a commit that can still land. A removed operator stays up and unready across a supervisor restart. |
| **L8** | **In scope (#752).** Delete speculative mode and foreign-tip reconciliation (the 2026-10-03 ruling is not yet on this branch). Remove `SPECULATIVE_COMMIT_BUILD` (`demo/midgard-node/src/services/config.make-config.ts:197-199,986`; 45 tracked files name it at `17fdffd9b`, 38 of them under `demo/`), its three fibers (`demo/midgard-node/src/commands/listen.node-fibers.ts:115,120,123`) and the `demo/midgard-node/src/` groups in §13.1. | — | The grep finds zero. Commit and confirmation tests reached by the change pass. |
| **M5** | **Deferred; a separate issue (§1.4).** MPF cap breach triggers compaction (K7). Above 75% of the resident cap, or on a `PreparePromotion` cap error, run a planned child restart that loads only the current root's closure, instead of returning the error forever. | — | A fixture that exceeds the cap: the next promotion succeeds after compaction, and the root is unchanged. Compaction takes at most 10 s at 1M resident records (B4). |

### Phase 1: follower foundation

| Ticket | Scope | Depends on | Acceptance |
|---|---|---|---|
| **F1** | Sidecar v2 (§4.2): move to `demo/l1-node-transport/`; one long-lived process; multi-point `FindIntersect`; credit window; length-prefixed frames; LSQ, LocalTxSubmission and LocalTxMonitor. Delete the per-candidate spawns and the exact-point helper service with its per-session node connections (WC3; updated at 17fdffd9b). | — | Conformance against a local devnet node: intersect on the point list, `roll_backward` ordering under a forced fork, credit never exceeded, raw bytes round-trip, `submit` returns the ledger's rejection bytes, `has_tx` agrees with the mempool. A sidecar crash and restart resume from the acknowledged sequence with no gap and no duplicate. |
| **F2** | The `demo/midgard-l1-follower/` package: the `FactStore` interface, Postgres and SQLite adapters, the §5.2 DDL, rewind (§7.1), the temporal registry (§7.2), schema lint (class and retention), and invariants INV1–INV6. | — | The property test from §7.2 passes on both adapters with 10^4 random operations. The lint fails a fixture migration that has no class. Rewind cost is linear in the rows above the target (B3). |
| **F3** | Origin: the `l1Origin` manifest field (rides the redeploy), an operator-config override until then, the `deployment:check` invariant (origin before the `prepareHubOracleNonce` block), the R3 and R4 assertions, and the `follower find-origin` tool. | F1, F2 | An origin after protocol init: unready R3. A wrong hash: R4. `find-origin` on devnet returns the point immediately before the block holding the tx. |
| **F4** | LSQ wallet seed (§5.3, step 4). | F1, F2 | Pre-origin wallet UTxOs appear as seed rows. A rewind at or above a seed's point never touches it; a rewind below it deletes the seed rows and the wallet re-seeds. Adding a wallet re-seeds. |
| **F5** | A Lucid `Provider` over the read API and the sidecar (UTxOs, datums, protocol parameters, slot config from era history, submit), with local evaluation. | F1, F2 | Wallet coin selection, reference-script publication and one tx of each node family build and submit on devnet with no Kupo or Ogmios configured. |
| **F6** | **Deferred (§1.4).** Decode pool, reorder buffer, ordered writer, batching and the tracked-outref cache (§6). | F2 | B1 and B2 targets met. Under random worker delays, writes stay in sequence order. After a rewind, the cache equals a fresh load. |
| **F7** | Heads module (§9): levels, `depth()`, `slotNow`, and a lint against direct k or cd comparisons. Deletes `local-ledger-slot` (§13.1). Takes L7's rule: the three depth variants (§3.5) become one `depth()`, and every §3.6 site that no other ticket deletes reads `slotNow`. | F2 | Level transitions are correct under the fork simulator. The lint flags a fixture that compares against `confirmationDepth`. From L7: a fake clock 10 minutes fast changes no L1 decision. |
| **F8** | Fork simulator (rev-4 ticket 4): fast-check over the transport, covering re-land, never re-land, a changed `valid_to`, new-fork-only and phase-2-failed. After every operation, each plugged-in projection must equal a fresh replay of the facts. It drives the F2 sequential writer, because F6 is deferred (§1.4). The shadow-diff harness and soak runner it first shipped were deleted after the owner dropped the shadow gate (2026-10-07, §14). | F1, F2 | Runs in CI, narrowed to changed files. Every later projection ticket adds cases to it. |
| **F9** | **Deferred (§1.4).** Devnet L1 fork drill. The devnet runs one cardano-node (`demo/midgard-node-tools/devnet/phase4-process/compose.yaml:12-13`), so it cannot fork, and the drill catalogue has no fork drill (`demo/midgard-node-tools/src/devnet-stack/chaos.drill-catalogue.ts:95-149`). Add a second block producer, a partition toggle between the two, and drills `l1-fork-shallow` (depth ≤ cd), `l1-fork-deep` (cd < depth < k) and `l1-fork-correction` (the fork removes an admitted correction). Replace `stop-kupo` and `stop-ogmios` with `kill-sidecar`. | F1 | Each drill produces a fork of the requested depth, checked against both nodes' tips. Each role's `/readyz` returns to ready with no restart and no CLI. Projections equal a fresh replay after the rejoin. |

### Phase 2: committee

| Ticket | Scope | Depends on | Acceptance |
|---|---|---|---|
| **C1** | Committee on the follower: a temporal landed-queue projection, plus headers awaiting attestation and obligations as projections. Delete the replay provider, the Ogmios sessions, the cursor stores and the Lucid providers (§13.3). Absorbs L1: delete the terminal merge rule, the quarantine early return, the rollback feed and the recovery CLI; signed decisions stay class B. Gated on C4 before it signs a sibling header (§1.4). | F1–F5, F7, F8 | Simulator fork scenarios: every committee projection equals a fresh replay of the facts after every operation. Devnet journeys pass with no Kupo or Ogmios configured. Tick p99 ≤ 50 ms at Q = 1,000 (B5). No Kupo or Ogmios configured. From L1: rollbacks of depth 1, cd and cd + 1 across a signed decision: the committee keeps ticking with no CLI, and `/readyz` returns to ready within one tick after the node catches up. A rollback deeper than k: unready `rollback_beyond_k`, and the process stays up and is not restarted. The decision for a header that disappeared is kept, never re-signed with different content, and never deleted before k. |
| **C2** | Committee store on Postgres only (Q1). Delete the JSON-file backend: the store, instance lock, process mutex and JSON promise capacity (§13.3), the `file` arm of `LocalStateConfig` and `openCommitteeStore` (`demo/da-committee-node/src/store/factory.ts:12-21`), and `DA_COMMITTEE_DB_PATH` with its call sites, tests, devnet env builder and docs (including the README's "JSON store ownership" section and the docs-site committee guide). The follower's committee tables live in the same database. The public retained-DA reader's read-only role is granted only the tables it reads today, never the follower's. | C1 | Mutation cost, tick store reads and readiness probe cost do not depend on store size (B5, CC4). Active/passive across hosts: the passive member is refused by the advisory instance lock (`demo/da-committee-node/src/store/postgres.instance-lock.ts`) while the active one holds it, and takes over within one reconnect after the active process dies, with no lost or duplicated decision effect. The public reader cannot `SELECT` any follower table. No `DA_COMMITTEE_DB_PATH` reference remains. |
| **C3** | Committee durable actions (retention release, promise expiry) gate on `final`. | C1, F7 | A rollback deeper than cd and shallower than k after a release decision: nothing is released early. |
| **C4** | **In scope as a gate on C1 (§1.4).** Check that signing sibling headers is truthful: confirm that no on-chain validator or off-chain slashing rule treats two content-bound commitments for siblings as equivocation. | — | A written finding with file:line references, plus an emulator test where a member signs siblings on two forks and neither is slashable. If either is slashable, stop: owner question. |

### Phase 3: watcher

| Ticket | Scope | Depends on | Acceptance |
|---|---|---|---|
| **W1** | Watcher on the follower. Delete multi-provider consistency, the Kupmios capture and the `fp` Kupmios sources (§13.2). | F1–F5, F7, F8 | Simulator fork scenarios: every watcher projection equals a fresh replay of the facts after every operation. Fault detection and proof journeys pass on devnet with no Kupo or Ogmios. Each intervention W1 introduces fails `/readyz` with a named reason; the process never exits or restart-loops. |
| **W2** | Watcher store as per-row SQLite tables. Each row carries a MAC, and each revision a chained digest, O(delta). The journals become tables. Delete the whole-snapshot CAS. Absorbs L2: a completed objective with a verified marker past k is skipped on restart and pruned, and only non-completed rows count toward the cap. | W1 | Persist p99 ≤ 20 ms at 10^5 stored observations (B6). A tampered row or a reordered revision is detected at startup. The watcher README and compose state that the SQLite file must sit on a local disk, never a network filesystem. From L2: 100k retry cycles of one objective keep its table at most 2× its live rows, and a restart succeeds. A cap reached by live rows alone means unready `journal_capacity`, not a throw loop. |
| **W3** | Watcher rollback = rewind + recompute. Delete `demo/midgard-watcher/src/l1/rollback-engine/*`. Incidents only beyond k. Absorbs L3: losing a block that was final at cd recomputes in-process and never sets `quarantined`. | W1, W2 | Simulator: the watcher's projections equal a fresh replay after every operation. From L3: a rollback that removes a cd-final block resumes in-process with no restart. A rollback beyond k: unready, and the process stays up. |

### Phase 4: node, intents and MPF

| Ticket | Scope | Depends on | Acceptance |
|---|---|---|---|
| **N1** | Lists, events, key set and deposit spendability as projections over facts, using `slotNow` and never `new Date()`. Ingestion moves to the follower through a typed follower-change driver, and member identity moves to the §5.4 event key plus an immutable admission point, with the commit horizon at min(journal coverage, follower covered tip). Delete the user-event ingestion fibers and whatever becomes reader-free; the event-history control plane goes at N1-close (§13.1). | F1–F5, F7, F8 | Simulator fork scenarios: every node projection equals a fresh replay of the facts after every operation. Devnet journeys pass with no Kupo or Ogmios configured. Each intervention N1 introduces fails `/readyz` with a named reason; the process never exits or restart-loops. An orphaned-origin id is readmitted, and a retired key is refused. Per-block apply is O(r) (B2, ratio ≤ 1.2). |
| **N1-close** | Inside this program's pull request, after the node tickets that read the journal have moved to follower identity. Delete the event-history control plane (owner, runtime, producer, recovery, authority, journal core, ledger receipts, `l1-event-history-*`, `l1-ledger-snapshot`; §13.1 rows marked N1-close). Re-source the U3 horizon from the follower. Remove the node's Kupo and Ogmios config keys. Run the node devnet journeys with no Kupo or Ogmios. The I5 gate stays with I5. | N1, I1, I3, I5, N3, N4, N6, U3 | Grep finds no reader of the deleted rows. Devnet journeys pass with no Kupo or Ogmios configured. Each N1 intervention fails `/readyz` with a named reason; the process never exits or restart-loops. |
| **N2** | State-queue projection. Every fiber reads it. Delete the topology walks and the address-wide scans (NC6). | N1 | Zero L1 reads in fiber ticks (checked by grep and a test spy). Third-party outputs paid to the queue address never enter the projection. |
| **N3** | In-order landed-block processing (#744/#695) on facts: adopt when the node holds the post-state, otherwise fetch, replay and compare. Own blocks are never replayed. Delete the census and foreign adoption; rewrite the foreign-base verify (NC9). Runs on today's MPF owner until M2 (§1.4). | N2 | A foreign block that lands is processed exactly once. A rollback that removes it reverts the projection. No per-commit verify remains. |
| **N4** | Correction admission as a temporal projection (§7.4). Delete correction rewind, restore and recovery. Absorbs L4: delete `refuseRewoundStateQueueCorrectionRollback` and the `state_queue_correction_rewind` halt, and reinstate removed own journals from class B. Until M1–M3, the MPF follows the rewind through `restoreRetainedRoot` (§1.4). | N2 | From L4: a correction admitted at cd, then rolled back at depth cd + 1 (below k): commit, merge and settlement resume with no intervention, and the ledger root equals a fresh replay. A correction that lands again rewinds exactly once. A rollback beyond k: unready, no exit. The node process never restarts; until M1–M3, at most one MPF child restart per rewind. |
| **N5** | Temporal `confirmed_ledger` and merge by delta (§10.5). Runs on today's MPF owner until M2 (§1.4). | N2 | Merge finalisation is O(delta), with no `LOCK TABLE`. A rolled-back merge reverts the ledger. |
| **N6** | Settlement, operator-set and watchdog projections (NC13, NC14). Takes L7's D-N4 (settlement `confirmed` becomes derived, terminal only at k) and D-N7 (a removed operator is unready `operator_removed` and stays up). | N1, N2 | Settlement finds an event by id. Membership reads only the changed rows. From L7: a rollback deeper than cd across a settlement confirmation reverts its status, and a removed operator stays up and unready across a supervisor restart. |
| **N7** | **Deferred (§1.4).** The mempool-ledger cache rebases by delta after a rewind (NC5). | N5 | No full reload on rollback, commit error or rejection. |
| **N8** | **Deferred (§1.4).** The commit base is the native root at the parent. No hydration and no per-journal materialisation. The cross-check computes one from-scratch root (§10.6). | M2 | Commit build at N = 10^6 meets the B2 target, or the numbers go to the owner. |
| **N9** | **Deferred (§1.4).** Hot-path SQL (NC12): bounded queries, counters, the `address_history` index, and a DA reconciler that runs on deltas. Can start at once. | — | Each query has a plan test or an `EXPLAIN` fixture with no sequential scan over a growing table. |
| **N10** | Carriage resolver per §12.3–12.4: same block, `GetUTxOByTxIn` at the parent point, then at the tip, then the verified content fetch; `carriage_pending` plus unready when nothing resolves. Delete `l1-tx-order-carriage*` and its Kupo/Ogmios wiring, and switch `reconstructTxOrderMaterial` to field preimages read from `l1_forced_order_fields`. | N1, F2 | Scripted chain: carriage spent in the block after the order resolves at step 2; carriage created earlier in the order's own block resolves at step 1; parent point older than k with the carriage unspent resolves at step 3; spent and older than k gives `carriage_pending` and unready with no exit or verdict, then resolves when a source supplies the creating tx; a source returning bytes whose hash is not the tx id is refused; rolling back the order block truncates its row. Emulator, both polarities: a real forced order is admitted from resolved carriage; tampered bytes are refused at the field-commitment check. |
| **I1** | Intent journal (§8.2), derived status and reconciliation (§8.3) for every node family. The own-block store becomes class B. Delete the finalization machine, block confirmation and commit-submission recovery. Takes L7's D-N8 (the SDK availability-challenge intent status is derived). | F2, F7, N2 | Simulator: dead intents are never resubmitted, live ones are resubmitted with identical bytes, and a landed own commit that rolls back is live again with no write. The devnet journeys pass with every node family on the intent journal; the F9 drills follow F9 (§1.4). From L7: a rollback deeper than cd across a retired availability-challenge intent reverts its status. |
| **I2** | Wallet view (§8.5). Delete `operator-wallet-view` and the Lucid override. | I1 | A dead intent's inputs reappear. No "Missing vkey witness" after a fork. |
| **I3** | Commits under whichever-lands-wins: replacement on the same tail, revival of the old block if it lands, and orphaned deposit funding handled in any shape. Delete the coverage path and expired-intent release (RN1). Mitigate the fabricated-deposit race (§8.1). Takes L7's D-N2 (the replacement halt becomes revival) and D-N3. L6 already landed (#759). | I1, U3 (off-chain part) | Emulator, both polarities: (a) the old commit lands, so its block is revived and the replacement is superseded; (b) the replacement lands, so the old one is conflicted; (c) a deposit rolls back while its commit is in the mempool, so the replacement without the deposit is submitted within one block. Adversarial: a commit that includes an event less than d deep is refused at build. From L6: the grep for `signedHeaderRecoveryHoldSlot` finds zero, and a long-lived signed header is still pruned at k. From L7: a rollback deeper than cd across a displacement causes no halt and no exit. |
| **I4** | **Deferred (§1.4).** Watcher and committee intent families, including the collateral rule (§8.4). | I1, W1, C1 | Collateral is released at confirmation and re-signed after a rollback. An unretired attempt does not block the next one. |
| **I5** | View binding: `viewValid` inside the S5 and S6 write transactions (§8.1). | I1 | A planner racing a rewind: a stale-view intent is recorded with `stale_at_write` and never submitted. A valid view at a surviving point is accepted after an unrelated rollback. |
| **M1** | **Deferred (§1.4).** The `SwitchRoot` RPC. | — | Switching to a resident root at depth 3 takes ≤ 10 ms with no restart (B4). It is refused while generations are active. |
| **M2** | **Deferred (§1.4).** The multi-root load contract, pins and `mpf_roots`. | M1 | Load accepts records reachable from any pinned root and rejects orphans. |
| **M3** | **Deferred (§1.4).** `ImportClosure`. | M2 | A switch at depth 2,160 meets the B4 target with no restart. A missing record raises R6. |
| **M4** | **Deferred (§1.4).** Budgeted `Sweep`. | M2 | After a sweep, every pinned root reopens and every unpinned root is gone. Meets the B4 target. |

### Phase 5: surface

| Ticket | Scope | Depends on | Acceptance |
|---|---|---|---|
| **U1** | **Deferred (§1.4).** API head levels; tx status reports the level (D2, D-N5). | F7 | A status below `final` reverts after a fork in the simulator. |
| **U2** | **Deferred (§1.4).** Readiness reasons R1–R9 and the no-exit rule, in every role (§7.5). | F2 | For each reason, a test that the process stays up and is unready with that reason. A supervisor test shows no restart loop. |
| **U3** | Raise `event_wait` with the commit-event depth d (D1; rev-4 ticket 27; refined 2026-10-09). The W change and the profile field for d ride the redeploy. **In this program:** the commit end time is capped at the commit anchor's `time(A) + W − 1` (§8.1), with d from the deployment profile, and the profile build refuses a d the event wait cannot cover (no strike; the anchor cap reaches the commit TTL floor). | — | A property test over the fork simulator: every included event is strictly below A, and while A is canonical every included event is. Postgres and emulator tests at the cap and at depths d and d + 1. |
| **U4** | **Deferred (§1.4).** Remove Kupo and Ogmios from the devnet compose, tooling, config and docs. | C1, W1, N1–N10 | No Kupo or Ogmios process runs in the devnet. Every journey passes. |
| **U5** | **Partly in scope (§1.4):** the §17 deletions ship in the program pull request; the rest waits for U4. Docs: the readiness entry, the role READMEs and a decision record. Delete the superseded docs (§17). | U4 | Doc checks pass. |

### Issue map

Parent: #784. In the program pull request, each issue has its blocking edges set as native GitHub dependencies:

| Ticket | Issue | Ticket | Issue | Ticket | Issue |
|---|---|---|---|---|---|
| L8 | #752 | F1 | #785 | F2 | #786 |
| F3 | #788 | F4 | #789 | F5 | #790 |
| F7 | #791 | F8 | #787 | C4 | #792 |
| C1 | #793 | C2 | #794 | C3 | #795 |
| W1 | #796 | W2 | #797 | W3 | #798 |
| N1 | #799 | N2 | #800 | N3 | #801 |
| N4 | #802 | N5 | #803 | N6 | #804 |
| N10 | #805 | I1 | #806 | I2 | #807 |
| I3 | #810 | I5 | #808 | U3 | #809 |

Deferred work is tracked in #811. It includes L5 (#812) and M5 (#813), which are separate issues outside the program.

### Rev-4 ticket mapping

| Rev 4 | Here |
|---|---|
| 0 | Closed by `6794e37cd` (§1.3) |
| 1 | F2 |
| 2 | F6 + F8 (fork simulator; the shadow mode was dropped on 2026-10-07) |
| 3 | F3 + F4 (Kupo bootstrap dropped) |
| 4 | F8 |
| 5 | Dropped: raw CBOR (§5.2) and §12 |
| 6 | F5 |
| 7 | F7 + N2 |
| 8 | N1 |
| 9 | N10 |
| 10 | N2 + N6 |
| 11 | N4 (halt replaced by recompute) |
| 12 | Dropped (Kupo removed) |
| 13, 14 | I1 |
| 15 | I3 (speculative mode dropped) |
| 16 | I1 + I4 |
| 17 | I2 |
| 18 | I1 deletions |
| 19, 24 | N1 + N3 |
| 20 | N1 |
| 21 | I1 (signed block content is class B) |
| 22 | M1 + M2 + M4 |
| 23 | M3 (replaces tier 2) |
| 25 | N5 |
| 26 | N4 + N1 deletions |
| 27 | U3 |
| 28 | U1 |
| 29 | U5 |

### Critical path

```
F1 ─► F2 ─► F8 ─► N1 ─► N2 ─► I1 ─► I3
L8 starts on day one. F3, F4, F5 and F7 run beside F8.
C1 ─► C2, C3 and W1 ─► W2 ─► W3 branch off F8; N3–N6 and N10 branch off N1/N2.
Later, outside this program: the redeploy (U3 fields, F3 field), M1 ─► M2 ─► M3, F6, F9.
```

## 16. Test strategy and benchmarks

### 16.1 Test layers

Standing rules apply. Run only the tests a change reaches; the full battery
runs only when the owner asks. Every offchain contract change gets emulator
tests in both polarities.

| Layer | What it proves | Owner ticket | Runs |
|---|---|---|---|
| Fact-store property test | Any random sequence of apply and rollback gives every registered temporal table the same state as a fresh replay of the surviving chain. Runs on both adapters. A new temporal table is covered by registering it, not by writing a test. | F2 | CI, narrowed to `l1-follower` and registry changes |
| Invariants INV1–INV6 (§5.2) | Store integrity after every simulator step. In production, a check at startup and after each rewind, where a failure is R5. | F2 | CI and runtime |
| Fork simulator | Projections and intent outcomes across re-land, never re-land, a changed `valid_to`, a fork that exists only on the new branch, and a phase-2 failure. After every operation, each projection equals a fresh replay of the facts. Each projection ticket adds its §5.5 cases. This is the cutover gate that replaced the shadow diff (§14). | F8 | CI, narrowed |
| Emulator, both polarities | The honest path succeeds, and the stale, adversarial or conflicting path is refused at the exact check. This covers the I3 cases, collateral (I4), view binding (I5) and correction recompute (L4, N4). | each ticket | CI, narrowed |
| Devnet journeys | Each role's journeys pass with no Kupo or Ogmios configured, on real CBOR and a real era. | C1, W1, N1, I3 | devnet |
| Devnet drills | Existing catalogue (`demo/midgard-node-tools/src/devnet-stack/chaos.drill-catalogue.ts:95-149`): `kill-node`, `kill-watcher`, `kill-public-retained-da`, `pause-postgres`, `restart-cardano-node`. F9 adds `kill-sidecar` and the three fork drills, and U4 deletes `stop-kupo` and `stop-ogmios` (both deferred, §1.4). Each drill asserts the role returns to ready with no restart and no CLI. | F9, U4 | devnet, on demand |
| Readiness and no-restart-loop | For each of R1–R9, the process stays up, `/healthz` stays live, and `/readyz` reports the reason. A supervisor test restarts the process and gets the same unready state, not a crash loop. In this program, each ticket tests the reasons it introduces; the full sweep is U2 (deferred). | each ticket; U2 | CI |
| Lints | Every table declares its class and retention (§11). Projection code reads no clock, randomness or network (§7.2). No direct comparison against `confirmationDepth` or k outside the heads module (F7). No `new Date()` in a projection or planner. | F2, F7 | CI, fast |

### 16.2 Benchmarks

No benchmark was run for this plan. None of the existing benchmarks measures
per-block L1 work against state size, rollback cost, or committee tick cost:

- the Phase 1 query/write bench measures mempool and admission SQL
  (`demo/midgard-node/tests/benchmarks/phase1-query-write.bench.ts:219`);
- the group-commit A/B bench measures Postgres commit grouping
  (`demo/midgard-node/tests/benchmarks/phase1-group-commit-ab.bench.ts:45`);
- the validation and codec benches measure L2 tx validation;
- the Architecture G soak measures MPF growth under L2 load, and needs 24 hours
  at 5,000 TPS (`docs/benchmark-scenarios/phase-3-architecture-g-soak.md:11-19`);
- the Phase 4 one-hour pipelined-commit gate was deleted with speculative
  building (#752), and no replacement gate was filed.

The harness is therefore part of the tickets. It has four parts:

1. A synthetic chain generator, seeded for reproducibility. It sets blocks per
   second, txs per block, the qualifying fraction, the fork depth and the fork
   rate. It feeds the F8 fake transport, so no cardano-node is needed.
2. A state-size dial N. N counts tracked live outputs, L2 ledger entries and
   event-history rows, preloaded through the ordinary apply path.
3. Vitest bench files under the existing bench config
   (`demo/midgard-node/vitest.bench.config.ts`), one per B-target. Each writes
   a JSON report.
4. Registration with the existing CI regression gate
   (`scripts/ci/check-benchmark-regression.mjs:3-4`: 10% over the trailing
   median of 5 runs).

| # | Measures | Workload | Target | Ticket |
|---|---|---|---|---|
| B1 | Catch-up throughput | Sparse blocks (≤ 1 qualifying tx) and dense blocks (50 qualifying txs), credit window 50 | ≥ 1,000 blocks/s sparse; ≥ 100 blocks/s dense | F6 (deferred) |
| B2 | Per-block apply at tip, including every projection; DA payload encoding excluded (DR-F5) | 10 qualifying txs per block, at N = 10^4, 10^5 and 10^6 | p99 ≤ 50 ms; p99(10^6) / p99(10^4) ≤ 1.2 | N1 (F6 and N8 deferred) |
| B3 | Rewind plus recompute, MPF excluded | Depth 30 and depth 2,160 at N = 10^6 | ≤ 200 ms at depth 30; ≤ 5 s at depth 2,160; linear in rows above the target | F2 |
| B4 | MPF versioning | `SwitchRoot` to a resident root; `ImportClosure` at depth 2,160; `Sweep` and compaction at 1M resident records; dirty nodes per block at 10, 100 and 1,000 txs per block | Switch ≤ 10 ms; import ≤ 30 s; sweep ≤ 60 s in budgeted slices; compaction ≤ 10 s. The dirty-node counts replace the §10.3 estimates, and if k × the measured count exceeds the cap, the resident window is resized from them. | M1–M5 (deferred) |
| B5 | Committee | Tick at Q = 1,000 queue nodes; one store mutation and one readiness probe at 10^3 and 10^5 records | Tick p99 ≤ 50 ms; mutation ratio and readiness ratio ≤ 1.2 | C1, C2 |
| B6 | Watcher persist | One observation at 10^5 stored | p99 ≤ 20 ms | W2 |
| B7 | Restart to ready | Cursor at tip, N = 10^6, including the MPF load of the current root | ≤ 30 s | U2 (deferred) |
| B8 | Retention soak | Constant event, deposit and withdrawal flow for 10 × (k + the retention window) blocks, with a short devnet retention window | Every class A, B and C table's row count plateaus: slope ≤ 1% over the last third. `l1_event_keys` is exempt: it is class A and never pruned (§11), so it grows with events ever created. | F2 (follower tables, synthetic chain; the default run scales k to 500 for speed, and a full k = 2,160 run passes on both adapters); L5 (deferred) |

If a target is missed, the ticket reports the numbers to the owner rather
than relaxing the target.

## 17. Superseded documents

Delete these in the program pull request (§1.4). No other tracked file links
to them; they reference only each other. Re-checked with a grep at
`17fdffd9b`, and removed with `git rm` in the program worktree.

| Document | Why |
|---|---|
| `docs/exec-plans/node-l1-rollback-redesign.md` | Rev 4. Replaced by this plan. |
| `docs/exec-plans/node-l1-rollback-audits/schema-draft.md` | Replaced by §5.2. |
| `docs/exec-plans/node-l1-rollback-audits/projection-acceptance.md` | Replaced by §5.5. |
| `docs/exec-plans/node-l1-rollback-audits/p0-deletion-audit.md` | Replaced by §13. |
| `docs/exec-plans/node-l1-rollback-audits/p0a-mpf-retained-roots.md` | Replaced by §10. |
| `docs/exec-plans/node-l1-rollback-audits/p0b-mempool-reinjection.md` | Replaced by §8.3 and ticket N7. |
| `docs/exec-plans/node-l1-rollback-audits/recovery-callers-and-chaining.md` | Replaced by §7.4, §7.5 and §13.1. |

Reconcile these in the program pull request; do not delete them:

- `docs/research/cardano-inclusion-and-rollback.md` is still valid research.
  Its Ogmios examples illustrate the protocol, so it needs only a note that
  Midgard uses N2C directly.
- `docs/public_testnet_readiness.md`: the L1 authority section (`:134-149`),
  the Kupo and Ogmios services list (`:89`), the provider-health item (`:126`)
  and the `stop-kupo`/`stop-ogmios` drills (`:357-358`).


## 18. Owner questions

### 18.1 Ruled

**Q1. The store backend per role, and committee HA.** Ruled by the owner on
2026-10-07.

| Role | Store | Why |
|---|---|---|
| Node | Postgres | Largest state; already on Postgres (§2, proposal 5). |
| Committee | Postgres | The separate public retained-DA reader (`public-retained-da.js`) reads the committee's database through a read-only Postgres role and refuses to start without one (`demo/da-committee-node/src/public-retained-da-config.ts:85-89`). One member may run active/passive across hosts against one store; the existing session advisory lock (`demo/da-committee-node/src/store/postgres.instance-lock.ts`) fences the passive process. The committee is a small, known, bonded set, so running Postgres is a small ask. |
| Watcher | SQLite (`node:sqlite`) | Watchers are permissionless and their security comes from their number, so the footprint stays at one file and no server. The watcher already ships it. Many watchers already cover the loss of one, so a single watcher needs no HA. |

Consequences:

- The committee's JSON-file backend is deleted: store, instance lock, process
  mutex and JSON promise capacity, 1,233 lines (§13.3, ticket C2). The Postgres
  store is kept.
- The follower keeps two `FactStore` adapters. Postgres serves the node and
  the committee; SQLite serves only the watcher. One property test covers
  both (F2).
- Shared follower SQL stays in the subset both backends support. Role code
  that runs only on Postgres (node, committee) may use Postgres features.
- SQLite must sit on a local disk. Network filesystems break its locking, so
  the watcher's docs and compose say so (W2).

### 18.2 Existing open items this plan touches (not re-asked)

The decision register is
`docs/exec-plans/public-testnet-decisions-2026-10-01/source-context.md`. Its
item ids carry a `DR-` prefix here so they do not collide with this plan's
ticket and benchmark ids.

| Item | Effect of this plan |
|---|---|
| DR-F5: the DA payload carries the full UTxO set (`:521-540`) | Unchanged and still open. It is the one O(N)-per-block path left after this plan (NC10). B2 excludes payload encoding, so a pass does not hide it. |
| DR-F6: is a cardano-node restart terminal (`:543-575`)? | Moot once Ogmios is gone (U4). A sidecar socket loss is transient by construction (F1). The interim classification fix is still needed until U4. |
| DR-B1: raise `confirmation_depth` above 3 | Still open. Under this plan cd gates liveness only, so the answer no longer affects durability. |
| DR-B4: convert terminal quarantines to recovering states | Answered for every quarantine in scope (K1, K3, K5 and K8 become recompute). Intervention cases fail `/readyz`, not `/healthz` (§7.5). |
| DR-B6: Kupmios single-attempt reads | Moot once Kupo and Ogmios are removed. |
| DR-B7: Kupo `--match` narrowing | Moot once Kupo is removed. |

### 18.3 May become questions after measurement

- N8: the per-commit from-scratch root cross-check, if B2 cannot be met at
  N = 10^6 with it on every block (§10.6).
- B4: the resident window, if the measured dirty-node rate is far from the
  §10.3 estimate.

## 19. Changes versus rev 4

1. **Scope.** All three roles, not the node only: rev-4 D3 is extended.
2. **Sources.** Kupo and Ogmios are removed. One N2C sidecar carries
   ChainSync, LSQ, LocalTxSubmission and LocalTxMonitor. Raw tx CBOR is
   stored, which closes G.1. Bootstrap is origin replay plus an LSQ wallet
   seed, with no Kupo index and no LSQ ledger snapshot.
3. **Binding.** There is no per-row revision. The generation lives in the
   cursor and `l1_rollbacks`. A view is (g, P_b), checked for canonicality in
   the same transaction as the write.
4. **Rewind.** A temporal-table registry with one generic rewind and one
   property test. Projections split into D-t, D-v, D-x and D-c. Every table
   carries a class and retention lint.
5. **Liveness.** Rev-4 O1 (halt) is replaced by recompute. The correction
   halt (K5) and the replacement halt (K8) are removed. The cd-gated
   destructive sites D-N1 to D-N8 are swept. One `depth()` and `slotNow`
   replace wall-clock cutoffs. Phase 0 fixes the bricks K1 to K8 on the
   current code first.
6. **MPF.** Rev-4 tier 1 (`SwitchRoot`, resident set, mark and sweep) is
   kept and generalised with pins and a multi-root load contract. Tier 2
   (restore the confirmed root and replay saved event logs) is replaced by
   `ImportClosure`, which costs O(delta) with no restart. The cap math shows
   the resident window cannot hold k at high throughput, so planned
   compaction is new, and it is the only remaining restart.
7. **New proposals adopted.** Pipelined ingestion (§6); an inventory of
   state-size-dependent work (§3.4) with targets B1 to B8; and a stage
   contract for each role (§4.1).
8. **Intents.** They cover every own tx in every role. Status is derived, and
   the 2026-10-04 collateral rule is included.
9. **Dropped.** Speculative mode (rev-4 ticket 15) and Kupo as a content
   service (tickets 3, 5 and 12). Rev-4 ticket 0 was closed by `6794e37cd`.
10. **Rev-4 audit decisions.** Orphaned-origin readmission is kept (§5.4).
    Foreign inclusion now goes through in-order processing (#744/#695), which
    replaces the `t2-foreign-event-reconciliation` route. Rev 4 wanted the tail
    refusal keyed on correction removals only. Today it also covers
    replacement-abandoned journals
    (`demo/midgard-node/src/workers/commit-block-header.resolve-commit-base-ledger-entries.ts:149-167`)
    and relies on revival running first
    (`demo/midgard-node/src/services/canonical-journal-recovery.ts:171`). Under this plan the
    own-block status is derived (I1), so P7 needs no revival step: a landed
    replaced block is followed directly. Topology health is kept as O2.
11. **New artefacts.** The deletion list with line counts (§13), the
    projection catalogue (§5.5), the F9 devnet fork drill, and carriage
    resolved off-chain from the order block's parent ledger state, with a
    hash-verified content fallback and no on-chain change (§12).
