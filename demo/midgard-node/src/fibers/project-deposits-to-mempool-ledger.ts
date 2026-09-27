import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { DepositsDB, MempoolLedgerDB } from "../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import { withHistoryIngestion } from "../services/event-history-producer.js";

const sameProjectedDepositEntry = (
  expected: MempoolLedgerDB.DepositEntry,
  actual: MempoolLedgerDB.EntryWithTimeStamp,
): boolean =>
  expected[MempoolLedgerDB.Columns.TX_ID].equals(
    actual[MempoolLedgerDB.Columns.TX_ID],
  ) &&
  expected[MempoolLedgerDB.Columns.OUTREF].equals(
    actual[MempoolLedgerDB.Columns.OUTREF],
  ) &&
  expected[MempoolLedgerDB.Columns.OUTPUT].equals(
    actual[MempoolLedgerDB.Columns.OUTPUT],
  ) &&
  expected[MempoolLedgerDB.Columns.ADDRESS] ===
    actual[MempoolLedgerDB.Columns.ADDRESS] &&
  actual[MempoolLedgerDB.Columns.SOURCE_EVENT_ID] !== null &&
  expected[MempoolLedgerDB.Columns.SOURCE_EVENT_ID].equals(
    actual[MempoolLedgerDB.Columns.SOURCE_EVENT_ID],
  );

const reconcileAlreadyProjectedDeposits = Effect.gen(function* () {
  const projectedEntries = yield* DepositsDB.retrieveProjectedEntries();
  if (projectedEntries.length <= 0) {
    return {
      mutationCount: 0,
      spendableUpserts: [] as readonly MempoolLedgerDB.DepositEntry[],
    };
  }
  const mempoolEntries = yield* Effect.forEach(
    projectedEntries,
    DepositsDB.toMempoolLedgerEntry,
  );
  const existing = yield* MempoolLedgerDB.retrieveBySourceEventIds(
    projectedEntries.map((entry) => entry[DepositsDB.Columns.ID]),
  );
  const existingBySourceEventId = new Map(
    existing
      .filter(
        (
          entry,
        ): entry is MempoolLedgerDB.EntryWithTimeStamp & {
          readonly source_event_id: Buffer;
        } => entry[MempoolLedgerDB.Columns.SOURCE_EVENT_ID] !== null,
      )
      .map(
        (entry) =>
          [
            entry[MempoolLedgerDB.Columns.SOURCE_EVENT_ID].toString("hex"),
            entry,
          ] as const,
      ),
  );

  const missingEntries: MempoolLedgerDB.DepositEntry[] = [];
  for (const entry of mempoolEntries) {
    const sourceEventIdHex =
      entry[MempoolLedgerDB.Columns.SOURCE_EVENT_ID].toString("hex");
    const existingEntry = existingBySourceEventId.get(sourceEventIdHex);
    if (existingEntry === undefined) {
      missingEntries.push(entry);
      continue;
    }
    if (!sameProjectedDepositEntry(entry, existingEntry)) {
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolLedgerDB.tableName,
          message:
            "Projected deposit reconciliation found an existing mempool_ledger row with mismatched payload",
          cause: `source_event_id=${sourceEventIdHex}`,
        }),
      );
    }
  }

  yield* MempoolLedgerDB.insertDepositEntriesStrict(missingEntries);
  const projectedByEventId = new Map(
    projectedEntries.map((entry) => [
      entry[DepositsDB.Columns.ID].toString("hex"),
      entry,
    ]),
  );
  return {
    mutationCount: missingEntries.length,
    spendableUpserts: missingEntries.filter((entry) => {
      const projected = projectedByEventId.get(
        entry[MempoolLedgerDB.Columns.SOURCE_EVENT_ID].toString("hex"),
      );
      return (
        projected?.[DepositsDB.Columns.PROJECTED_HEADER_HASH] !== null &&
        projected?.[DepositsDB.Columns.PROJECTED_HEADER_HASH] !== undefined
      );
    }),
  };
});

const projectAwaitingDeposits = (upTo: Date) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const awaitingEntries =
          yield* DepositsDB.retrieveAwaitingEntriesDueBy(upTo);
        if (awaitingEntries.length <= 0) {
          return 0;
        }
        const mempoolEntries = yield* Effect.forEach(
          awaitingEntries,
          DepositsDB.toMempoolLedgerEntry,
        );
        yield* MempoolLedgerDB.reconcileDepositEntries(mempoolEntries);
        yield* DepositsDB.markAwaitingAsProjected(
          awaitingEntries.map((entry) => entry[DepositsDB.Columns.ID]),
        );
        return mempoolEntries.length;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      MempoolLedgerDB.tableName,
      "Failed to project awaiting deposits into mempool ledger",
    ),
  );

/** SQL only. The history owner calls this under its existing recovery
 * transaction and publishes the cache by completing that recovery generation.
 * The cutoff is the authenticated source frontier, never a polling timestamp.
 */
export const reconcileDepositProjection = (upTo: Date) =>
  Effect.gen(function* () {
    const reconciled = yield* reconcileAlreadyProjectedDeposits;
    const projectedCount = yield* projectAwaitingDeposits(upTo);
    return { reconciled, projectedCount };
  }).pipe(withHistoryIngestion);
