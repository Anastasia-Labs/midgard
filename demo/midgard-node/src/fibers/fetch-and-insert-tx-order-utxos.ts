import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import { ForcedTransactionsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  logReconciledVisibleUserEvents,
  persistVisibleUserEventUTxOs,
  repeatVisibleUserEventIngestionFiber,
  runCommitTimeUserEventIngestionBarrier,
  type UserEventFetchBounds,
  type UserEventReconcileResult,
} from "@/fibers/user-event-ingestion.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
} from "@/services/index.js";

const rawDatum = (
  txOrderUTxO: SDK.TxOrderUTxO,
): Effect.Effect<Buffer, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const datum = txOrderUTxO.utxo.datum;
      if (datum === undefined || datum === null) {
        throw new Error(
          `Missing inline datum for tx-order UTxO ${txOrderUTxO.utxo.txHash}#${txOrderUTxO.utxo.outputIndex.toString()}`,
        );
      }
      return Buffer.from(datum, "hex");
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to read tx-order inline datum",
        cause,
      }),
  });

/**
 * Fetches the currently visible tx-order UTxO set.
 *
 * This mirrors deposit and withdrawal ingestion: reconciling the full visible
 * set is safer than cursor-only scans when provider visibility lags.
 */
const fetchTxOrderUTxOs = (
  lucid: LucidEvolution,
  config?: UserEventFetchBounds,
): Effect.Effect<SDK.TxOrderUTxO[], SDK.LucidError, MidgardContracts> =>
  Effect.gen(function* () {
    const { txOrder } = yield* MidgardContracts;
    const fetchConfig: SDK.UserEventFetchConfig = {
      eventAddress: txOrder.spendingScriptAddress,
      eventPolicyId: txOrder.policyId,
      ...config,
    };
    return yield* SDK.fetchTxOrderUTxOsProgram(lucid, fetchConfig);
  });

const txOrderUTxOToEntry = (
  txOrderUTxO: SDK.TxOrderUTxO,
): Effect.Effect<ForcedTransactionsDB.Entry, SDK.LucidError | DatabaseError> =>
  Effect.gen(function* () {
    const txCompact = txOrderUTxO.datum.event.tx;
    const inclusionTime = txOrderUTxO.inclusionTime;
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValue({
      txCompact,
      operatorValidity: txCompact.validity,
    });
    const datum = yield* rawDatum(txOrderUTxO);

    return {
      [ForcedTransactionsDB.Columns.TX_ORDER_ID]: Buffer.from(
        txOrderUTxO.idCbor,
      ),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]: Buffer.from(
        txOrderUTxO.utxo.txHash,
        "hex",
      ),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]:
        txOrderUTxO.utxo.outputIndex,
      [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.from(
        txOrderUTxO.assetName,
        "hex",
      ),
      [ForcedTransactionsDB.Columns.RAW_DATUM]: datum,
      [ForcedTransactionsDB.Columns.TX_ID]: encoded.txId,
      [ForcedTransactionsDB.Columns.TX_COMPACT]: Buffer.from(
        txOrderUTxO.infoCbor,
      ),
      [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
      [ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]: txCompact.validity,
      [ForcedTransactionsDB.Columns.INCLUSION_TIME]: inclusionTime,
      [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
      [ForcedTransactionsDB.Columns.STATUS]:
        ForcedTransactionsDB.Status.Awaiting,
    };
  });

export const reconcileVisibleTxOrderUTxOs = (
  config?: UserEventFetchBounds,
): Effect.Effect<
  UserEventReconcileResult,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const txOrderUTxOs = yield* fetchTxOrderUTxOs(lucid, config);
    return yield* persistVisibleUserEventUTxOs({
      visibleUtxos: txOrderUTxOs,
      toEntry: txOrderUTxOToEntry,
      insertEntries: ForcedTransactionsDB.insertEntries,
      emptyLogMessage: "No tx-order UTxOs found.",
      foundLogMessage: (count) => `${count} tx-order UTxO(s) found.`,
    });
  });

export const fetchAndInsertTxOrderUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> = Effect.gen(function* () {
  yield* Effect.logDebug("fetching TxOrderUTxOs...");
  const { reconciledCount } = yield* reconcileVisibleTxOrderUTxOs();
  yield* logReconciledVisibleUserEvents({
    reconciledCount,
    message: (count) =>
      `Reconciled ${count} visible tx-order UTxO(s) into forced_transaction_utxos.`,
  });
});

export const fetchAndInsertTxOrderUTxOsForCommitBarrier = (
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
      `Running commit-time tx-order ingestion barrier up to ${upperBound.toISOString()}.`,
    completedLogMessage: ({
      reconciledCount,
      completedAt,
      inclusionTimeUpperBound: upperBound,
    }) =>
      `Commit-time tx-order barrier reconciled ${reconciledCount} tx-order UTxO(s); fetch completed at ${completedAt.toISOString()} and locked the visibility barrier at ${upperBound.toISOString()}.`,
    reconcile: reconcileVisibleTxOrderUTxOs,
  });

export const fetchAndInsertTxOrderUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals
> =>
  repeatVisibleUserEventIngestionFiber({
    schedule,
    startLogMessage: "Fetch and insert TxOrderUTxOs.",
    spanName: "fetch-and-insert-tx-order-utxos-fiber",
    action: fetchAndInsertTxOrderUTxOs,
  });
