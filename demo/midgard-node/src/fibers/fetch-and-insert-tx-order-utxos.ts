import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import { ForcedTransactionsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Database, Lucid, MidgardContracts } from "@/services/index.js";

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
  config?: Pick<
    SDK.UserEventFetchConfig,
    "inclusionTimeLowerBound" | "inclusionTimeUpperBound"
  >,
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
  config?: Pick<
    SDK.UserEventFetchConfig,
    "inclusionTimeLowerBound" | "inclusionTimeUpperBound"
  >,
): Effect.Effect<
  { readonly reconciledCount: number; readonly completedAt: Date },
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const txOrderUTxOs = yield* fetchTxOrderUTxOs(lucid, config);

    if (txOrderUTxOs.length <= 0) {
      yield* Effect.logDebug("No tx-order UTxOs found.");
      return {
        reconciledCount: 0,
        completedAt: new Date(),
      } as const;
    }

    yield* Effect.logInfo(`${txOrderUTxOs.length} tx-order UTxO(s) found.`);

    const entries = yield* Effect.forEach(txOrderUTxOs, txOrderUTxOToEntry);
    yield* ForcedTransactionsDB.insertEntries(entries);
    return {
      reconciledCount: entries.length,
      completedAt: new Date(),
    } as const;
  });

export const fetchAndInsertTxOrderUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> = Effect.gen(function* () {
  yield* Effect.logDebug("fetching TxOrderUTxOs...");
  const { reconciledCount } = yield* reconcileVisibleTxOrderUTxOs();
  if (reconciledCount <= 0) {
    return;
  }
  yield* Effect.logInfo(
    `Reconciled ${reconciledCount} visible tx-order UTxO(s) into forced_transaction_utxos.`,
  );
});

export const fetchAndInsertTxOrderUTxOsForCommitBarrier = (
  inclusionTimeUpperBound: Date,
): Effect.Effect<
  Date,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `Running commit-time tx-order ingestion barrier up to ${inclusionTimeUpperBound.toISOString()}.`,
    );
    const inclusiveUpperBound = BigInt(inclusionTimeUpperBound.getTime() + 1);
    const { reconciledCount, completedAt } =
      yield* reconcileVisibleTxOrderUTxOs({
        inclusionTimeUpperBound: inclusiveUpperBound,
      });
    yield* Effect.logInfo(
      `Commit-time tx-order barrier reconciled ${reconciledCount} tx-order UTxO(s); fetch completed at ${completedAt.toISOString()} and locked the visibility barrier at ${inclusionTimeUpperBound.toISOString()}.`,
    );
    return inclusionTimeUpperBound;
  });

export const fetchAndInsertTxOrderUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("Fetch and insert TxOrderUTxOs.");
    const action = fetchAndInsertTxOrderUTxOs.pipe(
      Effect.withSpan("fetch-and-insert-tx-order-utxos-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
