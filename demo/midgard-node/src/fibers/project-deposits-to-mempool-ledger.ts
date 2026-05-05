import { Database, Globals } from "@/services/index.js";
import { DepositsDB, MempoolLedgerDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { sqlErrorToDatabaseError } from "@/database/utils/common.js";
import { SqlClient } from "@effect/sql";
import { Effect, Ref, Schedule } from "effect";

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
    return 0;
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
        (entry): entry is MempoolLedgerDB.EntryWithTimeStamp & {
          readonly source_event_id: Buffer;
        } => entry[MempoolLedgerDB.Columns.SOURCE_EVENT_ID] !== null,
      )
      .map((entry) => [
        entry[MempoolLedgerDB.Columns.SOURCE_EVENT_ID].toString("hex"),
        entry,
      ] as const),
  );

  const missingEntries: MempoolLedgerDB.DepositEntry[] = [];
  for (const entry of mempoolEntries) {
    const sourceEventIdHex = entry[
      MempoolLedgerDB.Columns.SOURCE_EVENT_ID
    ].toString("hex");
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
  return missingEntries.length;
});

const projectAwaitingDeposits = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return yield* sql.withTransaction(
    Effect.gen(function* () {
      const awaitingEntries = yield* DepositsDB.retrieveAwaitingEntriesDueBy(
        new Date(),
      );
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
      return awaitingEntries.length;
    }),
  );
}).pipe(
  sqlErrorToDatabaseError(
    MempoolLedgerDB.tableName,
    "Failed to project awaiting deposits into mempool ledger",
  ),
);

export const projectDepositsToMempoolLedger: Effect.Effect<
  void,
  DatabaseError,
  Database | Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const [reconciledCount, projectedCount] = yield* Effect.all(
    [reconcileAlreadyProjectedDeposits, projectAwaitingDeposits],
    { concurrency: 1 },
  );
  const totalMutations = reconciledCount + projectedCount;
  if (totalMutations <= 0) {
    return;
  }
  yield* Ref.update(globals.MEMPOOL_LEDGER_VERSION, (version) => version + 1);
  yield* Effect.logInfo(
    `🏦 Reconciled ${reconciledCount} projected deposit UTxO(s) and projected ${projectedCount} awaiting deposit UTxO(s) into mempool_ledger.`,
  );
});

export const projectDepositsToMempoolLedgerFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, Database | Globals> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🏦 Deposit projector fiber started.");
    const action = projectDepositsToMempoolLedger.pipe(
      Effect.withSpan("project-deposits-to-mempool-ledger-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
