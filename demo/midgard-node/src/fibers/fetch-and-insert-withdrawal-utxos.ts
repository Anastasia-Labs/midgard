import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Ref, Schedule } from "effect";

import { WithdrawalsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { withdrawalDataToEntry } from "../l1-event-history-entries.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
} from "../services/index.js";
import {
  logReconciledVisibleUserEvents,
  persistVisibleUserEventUTxOs,
  repeatVisibleUserEventIngestionFiber,
  runCommitTimeUserEventIngestionBarrier,
  type UserEventFetchBounds,
  type UserEventReconcileResult,
} from "./user-event-ingestion.js";

/**
 * Fetches the currently visible withdrawal UTxO set.
 *
 * This intentionally mirrors deposit ingestion: repeated full-set
 * reconciliation is safer than cursor-only scanning when provider visibility
 * lags or an earlier scan races the chain indexer.
 */
const fetchWithdrawalUTxOs = (
  lucid: LucidEvolution,
  config?: UserEventFetchBounds,
): Effect.Effect<SDK.WithdrawalUTxO[], SDK.LucidError, MidgardContracts> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const fetchConfig: SDK.EventHistoryFetchConfig = {
      ...SDK.eventHistoryDeploymentFromContracts(
        SDK.requireEventHistoryContracts(contracts).withdrawal,
      ),
      ...config,
    };
    return yield* SDK.fetchWithdrawalUTxOsProgram(lucid, fetchConfig);
  });

export const withdrawalUTxOToEntry = (
  withdrawalUTxO: SDK.WithdrawalUTxO,
): Effect.Effect<WithdrawalsDB.Entry, SDK.LucidError> =>
  withdrawalDataToEntry({
    ...withdrawalUTxO,
    location: withdrawalUTxO.utxo,
    payloadCbor: withdrawalUTxO.history.payloadCbor,
  });

export const reconcileVisibleWithdrawalUTxOs = (
  config?: UserEventFetchBounds,
): Effect.Effect<
  UserEventReconcileResult,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const withdrawalUTxOs = yield* fetchWithdrawalUTxOs(lucid, config);
    return yield* persistVisibleUserEventUTxOs({
      visibleUtxos: withdrawalUTxOs,
      toEntry: withdrawalUTxOToEntry,
      insertEntries: WithdrawalsDB.insertEntries,
      emptyLogMessage: "No withdrawal UTxOs found.",
      foundLogMessage: (count) => `${count} withdrawal UTxO(s) found.`,
    });
  });

export const fetchAndInsertWithdrawalUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.HEARTBEAT_WITHDRAWAL_FETCH, Date.now());

  yield* Effect.logDebug("fetching WithdrawalUTxOs...");
  const { reconciledCount } = yield* reconcileVisibleWithdrawalUTxOs();
  yield* logReconciledVisibleUserEvents({
    reconciledCount,
    message: (count) =>
      `Reconciled ${count} visible withdrawal UTxO(s) into withdrawal_utxos.`,
  });
});

export const fetchAndInsertWithdrawalUTxOsForCommitBarrier = (
  inclusionTimeUpperBound: Date,
): Effect.Effect<
  Date,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> =>
  runCommitTimeUserEventIngestionBarrier({
    inclusionTimeUpperBound,
    inclusionTimeUpperBoundOffsetMs: 1,
    startLogMessage: (upperBound) =>
      `Running commit-time withdrawal ingestion barrier up to ${upperBound.toISOString()}.`,
    completedLogMessage: ({
      reconciledCount,
      completedAt,
      inclusionTimeUpperBound: upperBound,
    }) =>
      `Commit-time withdrawal barrier reconciled ${reconciledCount} withdrawal UTxO(s); fetch completed at ${completedAt.toISOString()} and locked the visibility barrier at ${upperBound.toISOString()}.`,
    reconcile: reconcileVisibleWithdrawalUTxOs,
  });

export const fetchAndInsertWithdrawalUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals
> =>
  repeatVisibleUserEventIngestionFiber({
    schedule,
    startLogMessage: "Fetch and insert WithdrawalUTxOs.",
    spanName: "fetch-and-insert-withdrawal-utxos-fiber",
    action: fetchAndInsertWithdrawalUTxOs,
  });
