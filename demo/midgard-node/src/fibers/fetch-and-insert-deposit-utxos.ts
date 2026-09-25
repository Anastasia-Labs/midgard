import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution, type Network } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";

import { DepositsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { depositDataToEntry } from "../l1-event-history-entries.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  logReconciledVisibleUserEvents,
  persistVisibleUserEventUTxOs,
  runCommitTimeUserEventIngestionBarrier,
  type UserEventFetchBounds,
  type UserEventReconcileResult,
} from "./user-event-ingestion.js";

/**
 * Background ingestion for deposit UTxOs into the off-chain
 * deposit observation log.
 *
 * Projection into the mempool ledger is intentionally handled by a separate
 * step so ingestion remains idempotent and projection can enforce its own
 * timing and exactly-once rules.
 */

/**
 * Projects one deposit UTxO into the database row shape used by the off-chain
 * deposits ledger.
 */
export const depositUTxOToEntry = (
  depositUTxO: SDK.DepositUTxO,
  network: Network,
): Effect.Effect<DepositsDB.Entry, SDK.LucidError> =>
  depositDataToEntry({ ...depositUTxO, location: depositUTxO.utxo }, network);

/**
 * Fetches the currently visible deposit UTxO set.
 *
 * Production correctness matters more than incremental scan efficiency here:
 * repeatedly reconciling the full visible deposit set avoids missing deposits
 * when provider/indexer visibility lags behind the node's previous scan time.
 */
const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  config?: UserEventFetchBounds,
): Effect.Effect<SDK.DepositUTxO[], SDK.LucidError, MidgardContracts> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const fetchConfig: SDK.EventHistoryFetchConfig = {
      ...SDK.eventHistoryDeploymentFromContracts(
        SDK.requireEventHistoryContracts(contracts).deposit,
      ),
      ...config,
    };
    return yield* SDK.fetchDepositUTxOsProgram(lucid, fetchConfig);
  });

/** Persist the exact authenticated snapshot already read by the caller. */
export const persistDepositUTxOs = (
  depositUTxOs: readonly SDK.DepositUTxO[],
  network: Network,
) =>
  persistVisibleUserEventUTxOs({
    visibleUtxos: depositUTxOs,
    toEntry: (utxo) => depositUTxOToEntry(utxo, network),
    insertEntries: DepositsDB.insertEntries,
    emptyLogMessage: "🏦 No deposit UTxOs found.",
    foundLogMessage: (count) => `🏦 ${count} deposit UTxOs found.`,
  });

export const reconcileVisibleDepositUTxOs = (
  config?: UserEventFetchBounds,
): Effect.Effect<
  UserEventReconcileResult,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const nodeConfig = yield* NodeConfig;

    const depositUTxOs: SDK.DepositUTxO[] = yield* fetchDepositUTxOs(
      lucid,
      config,
    );
    return yield* persistDepositUTxOs(depositUTxOs, nodeConfig.NETWORK);
  });

/**
 * Runs one deposit-discovery pass and persists newly visible deposits into the
 * deposit observation log.
 */
export const fetchAndInsertDepositUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals | NodeConfig
> = Effect.gen(function* () {
  const globals = yield* Globals;

  yield* Effect.logDebug("🏦 fetching DepositUTxOs...");
  const { reconciledCount, completedAt } =
    yield* reconcileVisibleDepositUTxOs();
  yield* Ref.set(globals.LATEST_DEPOSIT_FETCH_TIME, completedAt.getTime());
  yield* logReconciledVisibleUserEvents({
    reconciledCount,
    message: (count) =>
      `🏦 Reconciled ${count} visible deposit UTxO(s) into deposits_utxos.`,
  });
});

export const fetchAndInsertDepositUTxOsForCommitBarrier = (
  inclusionTimeUpperBound: Date,
): Effect.Effect<
  Date,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | NodeConfig
> =>
  runCommitTimeUserEventIngestionBarrier({
    inclusionTimeUpperBound,
    inclusionTimeUpperBoundOffsetMs: 0,
    startLogMessage: (upperBound) =>
      `🏦 Running commit-time deposit ingestion barrier up to ${upperBound.toISOString()}.`,
    completedLogMessage: ({
      reconciledCount,
      completedAt,
      inclusionTimeUpperBound: upperBound,
    }) =>
      `🏦 Commit-time deposit barrier reconciled ${reconciledCount} deposit UTxO(s); fetch completed at ${completedAt.toISOString()} and locked the visibility barrier at ${upperBound.toISOString()}.`,
    reconcile: reconcileVisibleDepositUTxOs,
  });
