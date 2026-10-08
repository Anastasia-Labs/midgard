/**
 * Read-only state reconciliation.
 *
 * Compares the four places a node's view of the rollup lives:
 *
 * - L1, read through the node's configured Lucid provider (state queue,
 *   deposit and withdrawal orders, payouts, settlements);
 * - SQL, read inside one `REPEATABLE READ, READ ONLY` transaction so every
 *   table is observed at the same database instant;
 * - the persisted native ledger root (the Architecture-G owner's durable root,
 *   or the MPF LevelDB root marker);
 * - the SQL ledger cache (`mempool_ledger`) that serves L2 UTxO queries.
 *
 * Nothing here writes: no leases, no audit records, no LevelDB opens on the
 * live store (the offline reader opens a private copy). The L1 and native-root
 * reads are repeated after the SQL snapshot and the whole collection retried
 * when either moved, so a comparison never mixes two different chain points.
 *
 * Every check reports PASS, FAIL or SKIPPED with a reason. Transient states the
 * reconciler can prove (a value equal to a recomputed later ledger point) PASS
 * with a note; transient states it cannot prove FAIL unless the operator passes
 * `allowInFlight`, in which case they are listed and accepted.
 */

import "node:fs/promises";
import "node:os";
import "node:path";
import "@al-ft/midgard-sdk";
import "@effect/sql";
import "@lucid-evolution/lucid";
import "effect";
import "level";
import "../database/index.js";
import "../database/utils/ledger.js";
import "../database/utils/tx.js";
import "../mpf/commit-rejection.js";
import "../mpf/index.js";
import "../mpf/store-primitives.js";
import "../services/index.js";
import "../transactions/state-queue/confirmed-ledger-snapshot.js";
import "./readiness.js";
import "./state-reconciliation.compares.js";
import "./state-reconciliation.walk-merged-chain.js";
import "./state-reconciliation.check-native-root.js";
import "./state-reconciliation.check-state-queue-journal.js";
import "./state-reconciliation.check-deposits.js";
import "./state-reconciliation.check-payouts.js";
import "./state-reconciliation.check-ledger-cache.js";
import "./state-reconciliation.collect-l1-state-view.js";
import "./state-reconciliation.materialize-point.js";
import "./state-reconciliation.collect-sql-state-snapshot.js";
import "./state-reconciliation.state-reconciliation-program.js";
export {
  evaluateStateReconciliation,
  formatStateReconciliationReport,
} from "./state-reconciliation.check-ledger-cache.js";
export {
  collectL1StateView,
  l1Fingerprint,
} from "./state-reconciliation.collect-l1-state-view.js";
export {
  collectSqlStateSnapshot,
  committedTipSelector,
} from "./state-reconciliation.collect-sql-state-snapshot.js";
export {
  type CheckId,
  type CheckStatus,
  type DepositPayload,
  type HeaderRoots,
  type JournalSummary,
  type L1EventOrder,
  type L1Observation,
  type L1Payout,
  type L1QueueHeader,
  type L1Settlement,
  type L1StateView,
  type LedgerPoint,
  type LedgerPointResult,
  type NativeRootObservation,
  type PendingTxDelta,
  type QueueRemoval,
  type ReconciliationCheck,
  type ReconciliationReport,
  type SqlDepositRow,
  type SqlStateSnapshot,
  type SqlWithdrawalRow,
  STATE_RECONCILIATION_CHECK_IDS,
  type WithdrawalPayload,
} from "./state-reconciliation.compares.js";
export {
  type NativeRootSourceOptions,
  readNativeRoot,
  readNativeRootFromLevelCopy,
  readNativeRootFromReadiness,
  type StateReconciliationOptions,
  stateReconciliationProgram,
} from "./state-reconciliation.state-reconciliation-program.js";
export {
  redactSensitive,
  type StateReconciliationInput,
} from "./state-reconciliation.walk-merged-chain.js";
