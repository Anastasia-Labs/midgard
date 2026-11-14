import { SqlClient } from "@effect/sql";
import { Duration, Effect, Metric } from "effect";

import { Database } from "../services/database.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import { WriteBehind } from "../services/write-behind.js";
import { ProcessedTx } from "../utils.js";
import type * as AddressHistoryDB from "./addressHistory.js";
import * as DepositsDB from "./deposits.js";
import * as MempoolInclusionsDB from "./mempoolInclusions.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import * as MempoolTxDeltasDB from "./mempoolTxDeltas.js";
import { tableName as txAdmissionsTableName } from "./txAdmissions.verify-claimed-payload-rows.js";
import {
  clearTable,
  DatabaseError,
  logDatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";
import * as Ledger from "./utils/ledger.js";
import * as Tx from "./utils/tx.js";

export const tableName = "mempool";

/**
 * Restores already-validated journal payloads after their state-queue block is
 * removed. Ledger effects are deliberately not applied twice: they remained
 * in the speculative mempool ledger across local block finalization.
 */
export const restoreJournalEntries = (
  entries: readonly Tx.EntryWithTimeStamp[],
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.insertEntries(tableName, [...entries]);

const mempoolPersistTxRowsDurationTimer = Metric.timer(
  "mempool_persist_tx_rows_duration",
  "Duration of accepted transaction row inserts into mempool",
);

const mempoolPersistProducedDurationTimer = Metric.timer(
  "mempool_persist_produced_duration",
  "Duration of accepted produced UTxO inserts into mempool_ledger",
);

const mempoolPersistSpentDurationTimer = Metric.timer(
  "mempool_persist_spent_duration",
  "Duration of accepted spent-input deletion from mempool_ledger",
);

const mempoolRetrievePageDurationTimer = Metric.timer(
  "mempool_retrieve_page_duration",
  "Duration of oldest-first mempool page retrieval",
);

const mempoolRetrievePageRowsGauge = Metric.gauge(
  "mempool_retrieve_page_rows",
  {
    description: "Rows returned by the latest mempool page retrieval",
    bigint: true,
  },
);

export const toTxDelta = (
  processedTx: ProcessedTx,
): MempoolTxDeltasDB.TxDelta => ({
  txId: processedTx.txId,
  spent: processedTx.spent.map((outRef) => Buffer.from(outRef)),
  produced: processedTx.produced.map((entry) => ({
    [Ledger.Columns.OUTREF]: Buffer.from(entry[Ledger.Columns.OUTREF]),
    [Ledger.Columns.OUTPUT]: Buffer.from(entry[Ledger.Columns.OUTPUT]),
  })),
});

export const toAddressHistoryEntries = (
  processedTxs: readonly ProcessedTx[],
): readonly AddressHistoryDB.Entry[] => {
  const unique = new Map<string, AddressHistoryDB.Entry>();
  for (const processedTx of processedTxs) {
    for (const entry of processedTx.produced) {
      const address = entry[Ledger.Columns.ADDRESS];
      unique.set(`${processedTx.txId.toString("hex")}:${address}`, {
        [Ledger.Columns.TX_ID]: processedTx.txId,
        [Ledger.Columns.ADDRESS]: address,
      });
    }
  }
  return [...unique.values()];
};

export const compactLedgerEffects = (
  processedTxs: readonly ProcessedTx[],
): {
  readonly produced: readonly Ledger.Entry[];
  readonly spent: readonly Buffer[];
} => {
  const produced = processedTxs.flatMap((tx) => tx.produced);
  const producedOutRefs = new Set(
    produced.map((entry) => entry[Ledger.Columns.OUTREF].toString("hex")),
  );
  const spentByOutRef = new Map<string, Buffer>();
  for (const tx of processedTxs) {
    for (const spent of tx.spent) {
      const outRefHex = spent.toString("hex");
      if (!producedOutRefs.has(outRefHex)) {
        spentByOutRef.set(outRefHex, spent);
      }
    }
  }
  const spentOutRefs = new Set(
    processedTxs.flatMap((tx) =>
      tx.spent.map((spent) => spent.toString("hex")),
    ),
  );
  return {
    produced: produced.filter(
      (entry) =>
        !spentOutRefs.has(entry[Ledger.Columns.OUTREF].toString("hex")),
    ),
    spent: [...spentByOutRef.values()],
  };
};

export const enqueueAcceptedWriteBehind = (
  processedTxs: readonly ProcessedTx[],
): Effect.Effect<void, DatabaseError, WriteBehind> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const writeBehind = yield* WriteBehind;
    yield* writeBehind.enqueueTxDeltas(processedTxs.map(toTxDelta));
    yield* writeBehind.enqueueAddressHistory(
      toAddressHistoryEntries(processedTxs),
    );
  });

export const insertMultipleCore = (
  processedTxs: readonly ProcessedTx[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const txEntries = processedTxs.map((v) => ({
      tx_id: v.txId,
      tx: v.txCbor,
    }));
    const txRowsStartedAt = Date.now();
    yield* Tx.insertEntries(tableName, txEntries);
    yield* mempoolPersistTxRowsDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - txRowsStartedAt)),
    );

    yield* applyLedgerEffectsCore(processedTxs);
  });

export const applyLedgerEffectsCore = (
  processedTxs: readonly ProcessedTx[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }

    // Phase B may accept dependency chains in one batch. Persist only the net
    // ledger transition: intermediate outputs do not need to be inserted and
    // immediately deleted inside the same transaction.
    const { produced, spent } = compactLedgerEffects(processedTxs);

    const producedStartedAt = Date.now();
    yield* MempoolLedgerDB.insert(produced);
    yield* mempoolPersistProducedDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - producedStartedAt)),
    );

    const spentStartedAt = Date.now();
    const consumedDepositEventIds = yield* MempoolLedgerDB.clearUTxOs(spent);
    yield* DepositsDB.markConsumedByEventIds(consumedDepositEventIds);
    yield* mempoolPersistSpentDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - spentStartedAt)),
    );
  });

export const insertMultiple = (
  processedTxs: readonly ProcessedTx[],
): Effect.Effect<void, DatabaseError, Database | WriteBehind> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* withFollowerWrite(
      sql.withTransaction(insertMultipleCore(processedTxs)),
    );
    yield* enqueueAcceptedWriteBehind(processedTxs);
  }).pipe(
    Effect.withLogSpan(`insert ${tableName}`),
    Effect.tapError((e) => logDatabaseError(tableName, "insert", e)),
    sqlErrorToDatabaseError(tableName, "Failed to insert mempool transactions"),
  );

export const insert = (
  processedTx: ProcessedTx,
): Effect.Effect<void, DatabaseError, Database | WriteBehind> =>
  insertMultiple([processedTx]);

/**
 * Retrieves pending mempool transaction CBOR by transaction hash (a row a
 * block's inclusion mark holds is not pending).
 */
export const retrieveTxCborByHash = (txHash: Buffer) =>
  MempoolInclusionsDB.retrievePendingValue(tableName, txHash);

/**
 * Retrieves pending mempool transaction CBOR blobs for a batch of hashes.
 */
export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => MempoolInclusionsDB.retrievePendingValues(tableName, txHashes);

export type MempoolCursor = {
  readonly timeStampTz: Date;
  /** The row's admission order (`ADMISSION_ORDER_NONE` for a row with no
   * admission), as decimal text. */
  readonly arrivalSeq: string;
  readonly txId: Buffer;
};

export type MempoolPage = {
  readonly entries: readonly Tx.EntryWithTimeStamp[];
  readonly nextCursor: MempoolCursor | null;
};

/** The admission order of a mempool row with no admission row: after every
 * row that has one (the largest `bigint`). */
const ADMISSION_ORDER_NONE = "9223372036854775807";

/**
 * A page of pending mempool rows in admission order: the time stamp (each
 * accepted batch's insert time), then the admission sequence
 * (`tx_admissions.arrival_seq`; a row with none after those with one), then
 * the tx id. A transaction is accepted only once its parents' outputs are in
 * the working ledger, at an earlier insert time or earlier in the same
 * batch, whose admissions are validated in admission order: so a parent
 * comes before its child, and a page, or any prefix of the pages, never
 * holds a child without its pending parent.
 */
export const retrievePage = ({
  after,
  limit,
  upTo,
}: {
  readonly after?: MempoolCursor;
  readonly limit: number;
  readonly upTo?: Date;
}): Effect.Effect<MempoolPage, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve page`);
    const startedAt = Date.now();
    const sql = yield* SqlClient.SqlClient;
    const pageLimit = Math.max(1, Math.floor(limit));
    const afterTime = after?.timeStampTz ?? null;
    const afterSeq = after?.arrivalSeq ?? null;
    const afterTxId = after?.txId ?? null;
    const upperTime = upTo ?? null;
    const rows = yield* sql<Tx.EntryWithTimeStamp & { order_seq: string }>`
      SELECT * FROM (
        SELECT
          mempool.${sql(Tx.Columns.TX_ID)},
          mempool.${sql(Tx.Columns.TX)},
          mempool.${sql(Tx.Columns.TIMESTAMPTZ)},
          COALESCE(admission.arrival_seq, ${ADMISSION_ORDER_NONE}::bigint)
            AS order_seq
        FROM ${sql(tableName)} AS mempool
        LEFT JOIN ${sql(txAdmissionsTableName)} AS admission
          ON admission.tx_id = mempool.${sql(Tx.Columns.TX_ID)}
        WHERE mempool.${sql(MempoolInclusionsDB.INCLUDED_BY)} IS NULL
          AND (${upperTime}::timestamptz IS NULL
          OR mempool.${sql(Tx.Columns.TIMESTAMPTZ)} <= ${upperTime}::timestamptz)
      ) AS pending
      WHERE (${afterTime}::timestamptz IS NULL)
        OR (${sql(Tx.Columns.TIMESTAMPTZ)}, order_seq, ${sql(Tx.Columns.TX_ID)}) >
           (${afterTime}::timestamptz, ${afterSeq}::bigint, ${afterTxId}::bytea)
      ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} ASC, order_seq ASC,
        ${sql(Tx.Columns.TX_ID)} ASC
      LIMIT ${pageLimit}`;
    const entries: readonly Tx.EntryWithTimeStamp[] = rows.map((row) => ({
      [Tx.Columns.TX_ID]: row[Tx.Columns.TX_ID],
      [Tx.Columns.TX]: row[Tx.Columns.TX],
      [Tx.Columns.TIMESTAMPTZ]: row[Tx.Columns.TIMESTAMPTZ],
    }));
    yield* mempoolRetrievePageDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - startedAt)),
    );
    yield* mempoolRetrievePageRowsGauge(Effect.succeed(BigInt(entries.length)));
    const last = rows.at(-1);
    return {
      entries,
      nextCursor:
        rows.length === pageLimit && last !== undefined
          ? {
              timeStampTz: last[Tx.Columns.TIMESTAMPTZ],
              arrivalSeq: String(last.order_seq),
              txId: last[Tx.Columns.TX_ID],
            }
          : null,
    };
  }).pipe(
    Effect.withLogSpan(`retrievePage ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      logDatabaseError(tableName, "retrievePage", e),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve mempool page"),
  );

/** The number of pending (unmarked) mempool rows. */
export const retrieveTxCount: Effect.Effect<bigint, DatabaseError, Database> =
  MempoolInclusionsDB.countPending(tableName);

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Tx.delMultiple(tableName, txHashes);
    yield* MempoolTxDeltasDB.clearTxs(txHashes);
  });

export const clear: Effect.Effect<void, DatabaseError, Database> = Effect.gen(
  function* () {
    yield* clearTable(tableName);
    yield* MempoolTxDeltasDB.clear;
  },
);
