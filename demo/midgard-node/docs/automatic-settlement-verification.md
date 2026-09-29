# Automatic settlement verification — 2026-09-29

Base: `c8b62ed6ce1be2424dba7bbd15d02d1e35391b11`. Checks ran against the
shared working checkout, which also contains unrelated changes. No deployment,
production database migration, service reset, commit or push was performed.

## Behavior and operating requirements

`listen` supervises a settlement worker thread with its own Lucid client,
dedicated fee/collateral wallet, serial transaction stream and two SQL connections.
Deposit consumption and valid withdrawal finalization enqueue durable jobs in the
same SQL transaction. Builders preserve the existing settlement proof,
retirement protection, reserve accounting and exact payout rules. Each signed
transaction is journaled before broadcast. Confirmation uses deployment-bound
block depth and exact historical outputs, including outputs already spent.

The worker retries deferred work, survives restarts by reconciling signed bytes,
and rechecks completed receipts after history recovery. Current work alternates
with historical auditing; restored fee coins are reconciled before reuse, with
an independent guard at the durable write. Replacement requires
expiry and synchronized evidence. Pending attempts retain their confirmation
history. A stalled worker is terminated and restarted; its journal survives.
Settlement health is exposed separately from L2 readiness.

Upgrading requires `db:migrate` and a funded `L1_SETTLEMENT_SEED_PHRASE`, distinct
from commitment, merge and reference-script wallets. Keep a separate ADA-only
collateral output. The phase 4 funding script now supplies this role and splits
fee funds from collateral. See the node README for operation and recovery.

## Evidence

All runs below are dated 2026-09-29 UTC (some started on 2026-09-28 local time).
The host was concurrently running other compilation/test jobs; timings are not
performance measurements. Shell commands used Node 24.13.1 via
`PATH=/home/gumbo/.nvm/versions/node/v24.13.1/bin:$PATH`.
Commands are from the repository root unless another directory is stated.
Postgres checks used distinct `MIDGARD_TEST_DATABASE_PREFIX` values.

| Command                                                                                                                                                                                                                                                                                                      | Observed result                                                                                                                              |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------- |
| `node scripts/doctor.mjs`                                                                                                                                                                                                                                                                                    | Prerequisites available; Node version warning (24 locally, 22 in CI); exit 0.                                                                |
| `MIDGARD_TEST_DATABASE_PREFIX=midgard_settlement_final MIDGARD_NODE_TEST_FORKS=2 pnpm --dir demo/midgard-node exec vitest run tests/settlement.test.ts tests/settlement-journal.test.ts tests/history-source-owner-retention.test.ts tests/l1-event-history-streaming-production-lifecycle-emulator.test.ts` | 20 tests passed in 4 files; exit 0.                                                                                                          |
| `MIDGARD_TEST_DATABASE_PREFIX=midgard_settlement_fees MIDGARD_NODE_TEST_FORKS=2 pnpm --dir demo/midgard-node exec vitest run tests/settlement.test.ts tests/settlement-journal.test.ts tests/history-source-owner-retention.test.ts tests/l1-event-history-streaming-production-lifecycle-emulator.test.ts`  | 22 tests passed after fairness and restored-fee guards; exit 0.                                                                              |
| `MIDGARD_TEST_DATABASE_PREFIX=midgard_settlement_receipts MIDGARD_NODE_TEST_FORKS=2 pnpm --dir demo/midgard-node exec vitest run tests/settlement-journal.test.ts tests/l1-event-history-streaming-production-lifecycle-emulator.test.ts`                                                                    | Final receipt-audit change: 10 passed (9 journal/recovery tests and real lifecycle); exit 0.                                                 |
| `MIDGARD_TEST_DATABASE_PREFIX=midgard_settlement_node MIDGARD_NODE_TEST_FORKS=2 pnpm --dir demo run test:tx-prep:node`                                                                                                                                                                                       | Node preparation: 130 passed; tools reliability: 52 passed; exit 0.                                                                          |
| `MIDGARD_TEST_DATABASE_PREFIX=midgard_settlement_sdk pnpm --dir demo run test:tx-prep:sdk`                                                                                                                                                                                                                   | Lucid package: 175 passed. SDK: 873 passed, 3 timed out at their 5-second limit; exit 1.                                                     |
| `pnpm --dir demo/midgard-sdk exec vitest run tests/lucid-inline-datum-preservation.test.ts --maxWorkers=1 --minWorkers=1`                                                                                                                                                                                    | All 6 tests passed with original timeouts; exit 0.                                                                                           |
| `pnpm --dir demo/midgard-sdk exec vitest run --maxWorkers=2 --minWorkers=1`                                                                                                                                                                                                                                  | Full SDK: 876 passed in 92 files; exit 0.                                                                                                    |
| `MIDGARD_TEST_DATABASE_PREFIX=midgard_settlement_emulator MIDGARD_NODE_TEST_FORKS=2 pnpm --dir demo run test:tx-prep:emulator`                                                                                                                                                                               | Node: 68 passed. Fault proofs: 617 passed, 2 timed out; 1 suite failed its pinned fit-ledger/blueprint identity check. Exit 1.               |
| `MIDGARD_TEST_DATABASE_PREFIX=midgard_settlement_fp_rerun pnpm --dir demo/midgard-fault-proofs exec vitest run --pool=forks --maxWorkers=1 --minWorkers=1 tests/submit-init-emulator-input-set-uniqueness-tier2.test.ts tests/submit-init-emulator-withdrawn-input-lifecycle.test.ts`                        | Both timeout files passed with original timeouts: 3 tests; exit 0.                                                                           |
| `pnpm --dir demo/midgard-node run typecheck`                                                                                                                                                                                                                                                                 | Exit 0.                                                                                                                                      |
| `pnpm --dir demo/midgard-sdk run typecheck`                                                                                                                                                                                                                                                                  | Exit 0.                                                                                                                                      |
| `pnpm --dir demo/midgard-sdk run build`                                                                                                                                                                                                                                                                      | SDK ESM, CJS and declaration builds succeeded; exit 0.                                                                                       |
| `pnpm --dir demo/midgard-node run build`                                                                                                                                                                                                                                                                     | Operator and worker ESM/declaration builds succeeded; exit 0.                                                                                |
| `node --test demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs`                                                                                                                                                                                                                            | 18 passed; exit 0.                                                                                                                           |
| `node /tmp/settlement-worker-smoke.mjs`                                                                                                                                                                                                                                                                      | 1 built-worker entry/error-report/shutdown assertion passed; intentionally empty configuration; exit 0. This is not live startup acceptance. |
| `git diff --check`                                                                                                                                                                                                                                                                                           | Exit 0.                                                                                                                                      |

Scoped ESLint and Prettier checks also exited 0. Their exact scope is the file
list below; from `demo`, the commands were `pnpm exec eslint --max-warnings=0`
with the `.ts` paths, and `pnpm exec prettier --check` with `.ts`/`.md` paths
(each path below without its initial `demo/`). No lint baseline was changed.

## Review and regression mutations

Four review passes used the user-event invariants and all twelve consensus
review lenses. The final pass found no remaining ranked finding.

- F1, consumed confirmation output blocking the queue: closed by historical
  exact-output evidence. Reintroducing `?unspent` in an isolated worktree failed
  the spent-output assertion.
- F2, confirmation block pruned during downtime: closed by a durable pending
  hold composed with the existing retention hold. Removing that wiring in the
  isolated worktree failed the owner retention assertion.
- Indexer recovery edges: closed by synchronizing before auditing any receipt
  batch. This covers missing receipts during catch-up and stale positive
  confirmations from a rolled-back fork. Removing synchronization failed the
  corresponding assertions.
- Historical backlog starvation: closed by alternating current work with
  completed-receipt auditing. Restoring oldest-due-only ordering failed the
  current-work selection assertion.
- Restored fee-input reuse: closed by an indexed pre-build receipt lookup and
  a durable-write guard. Removing the write guard failed at the expected
  journal refusal; bypassing pre-build recovery failed its recovery assertion.

Mutation commands ran in `/tmp/midgard-settlement-redcheck` with
`MIDGARD_TEST_DATABASE_PREFIX=midgard_settlement_red`,
`MIDGARD_NODE_TEST_FORKS=1`, and `MIDGARD_SKIP_NATIVE_BUILD=1` (the already-built
native binary was available):

| Command                                                                                                                                                                         | Observed result                                               |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- |
| `pnpm --dir demo/midgard-node exec vitest run tests/settlement.test.ts tests/history-source-owner-retention.test.ts` with F1/F2 restored                                        | Exactly the 2 targeted regressions failed, 11 passed; exit 1. |
| `pnpm --dir demo/midgard-node exec vitest run tests/settlement-journal.test.ts -t 'waits for a restored indexer'` with the barrier removed                                      | Targeted test failed, 5 unrelated tests not selected; exit 1. |
| `pnpm --dir demo/midgard-node exec vitest run tests/settlement.test.ts tests/settlement-journal.test.ts tests/history-source-owner-retention.test.ts` after restoring all fixes | 19 passed in 3 files; exit 0.                                 |

Additional mutations after fairness/recovery review:

| Command in the isolated worktree                                                                                                                                                                                            | Observed result                                                                             |
| --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------- |
| `pnpm --dir demo/midgard-node exec vitest run tests/settlement-journal.test.ts -t 'gives new work a turn'` with old ordering                                                                                                | Target failed, 6 unrelated tests not selected; exit 1.                                      |
| `pnpm --dir demo/midgard-node exec vitest run tests/settlement-journal.test.ts -t 'recovers a rolled-back receipt before fresh'` without the save-time guard                                                                | Target failed at expected Left versus actual Right, 7 unrelated tests not selected; exit 1. |
| `pnpm --dir demo/midgard-node exec vitest run tests/settlement-journal.test.ts -t 'recovers a rolled-back receipt before fresh\|does not trust an old-fork'` without pre-build recovery and positive-status synchronization | Both targets failed, 7 unrelated tests not selected; exit 1.                                |

Final restoration check: `pnpm --dir demo/midgard-node exec vitest run tests/settlement-journal.test.ts`
in the same isolated worktree with all fixes restored collected **9 passing
tests**, exit 0.

Lens conclusions: manifest parameter authority unchanged; no new validator
arms; decoding fails closed; reserve/payout accounting stays in existing
builders; confirmations bind canonical blocks and exact signed outputs;
authenticated reference scripts preserved; positive/negative recovery tests
present; mutation checks fail at the intended assertions; no added on-chain
execution path; no codec/Aiken twin changed; output indices come from completed
layouts; replacement/replay paths retain proof and expiry guards.

## Broad emulator gate failure

Both timeout files passed on the single-worker rerun with their original
timeouts. The remaining unresolved failure is the ledger identity check below.

The fault-proof gate's pinned ledger
`docs/fault-proofs/size-plans/transition-trace-forced-window-fit-ledger.json`
expects blueprint SHA-256
`3ddd74900b586e3b471e2c668a70dc46e1e864566e3fc5f620d98b274bf5f463`;
the current `onchain/aiken/plutus.json` hashes to
`636e8b9fe6cbc2cf247175c3303fa18ee61660891928e14620fad831be4fae1f`.
The ledger is unchanged from HEAD. This task changed neither the ledger nor
Aiken/blueprint sources; the shared checkout contains other contract work.
The mismatch remains unresolved, so the broad gate is **not passing**.
A base-branch execution was not performed; no baseline-pass claim is made.

## Limits and residual operational risks

- No live Cardano/Kupo/Ogmios node-and-watcher deployment or sustained crash/soak
  acceptance was run. The lifecycle uses real emulator script execution,
  production history reconciliation, native commitment, and SQL jobs, with
  synthetic network point labels/observation transport.
- L2 throughput and worst-case settlement backlog were not benchmarked. Worker
  isolation avoids blocking the main JS event loop and shared operational fee
  inputs; CPU, memory, provider and database capacity remain shared. Processing
  deliberately waits for manifest finality with one transaction in flight, so
  high settlement volume can build a backlog.
- Ambiguous submissions retain the journal and block further settlement until
  evidence resolves them. A depleted wallet still needs funding. A prolonged
  unresolved attempt holds history and can grow retained storage.
- Earlier manual settlements without a durable receipt cannot be declared
  complete merely because their inputs disappeared; those need chain evidence
  reconciliation. Invalid withdrawals retain the existing refund flow.
- Full-repository preflight, CI and live performance/release gates were not run;
  this task neither pushes nor claims release readiness. No validator source
  was changed by this task.

## Reviewed file scope

- `demo/midgard-node/src/commands/listen-router.ts`
- `demo/midgard-node/src/commands/listen.ts`
- `demo/midgard-node/src/commands/reserve-payout.ts`
- `demo/midgard-node/src/database/migrations/index.ts`
- `demo/midgard-node/src/services/config.ts`
- `demo/midgard-node/src/services/globals.ts`
- `demo/midgard-node/src/transactions/reserve-payout.ts`
- `demo/midgard-sdk/src/reserve-payout.ts`
- `demo/midgard-node/vitest.config.ts`
- `demo/midgard-node/.env.example`
- `demo/midgard-node/src/database/migrations/sql/0002_automatic_settlement.sql`
- `demo/midgard-node/src/database/settlement.ts`
- `demo/midgard-node/src/services/settlement.ts`
- `demo/midgard-node/src/fibers/settlement.ts`
- `demo/midgard-node/src/workers/settlement.ts`
- `demo/midgard-node/tests/settlement.test.ts`
- `demo/midgard-node/tests/settlement-journal.test.ts`
- `demo/midgard-node/src/services/settlement-output.ts`
- `demo/midgard-node/src/services/event-history-owner.ts`
- `demo/midgard-node/src/transactions/reference-publication-provider.ts`
- `demo/midgard-node/tests/history-source-owner-retention.test.ts`
- `demo/midgard-node/tests/helpers/automatic-settlement-lifecycle.ts`
- `demo/midgard-node/tests/l1-event-history-streaming-production-lifecycle-emulator.test.ts`
- `demo/midgard-node/README.md`
- `demo/midgard-node-tools/devnet/phase4-process/README.md`
- `demo/midgard-node-tools/devnet/phase4-process/scripts/fund-wallets.sh`
