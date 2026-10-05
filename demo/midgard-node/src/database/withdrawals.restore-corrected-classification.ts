import { outRefToCbor } from "@al-ft/lucid-midgard";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { withHistoryWrite } from "../services/event-history-producer.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";
import {
  Columns,
  type Entry,
  type SettlementInfoAssignment,
  Status,
  tableName,
  Validity,
} from "./withdrawals.insert-entries.js";

export const reopenAfterStateQueueCorrectionByEventIds = (
  ids: readonly Buffer[],
  removedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.forEach(
        [...ids].sort(Buffer.compare),
        (id) =>
          Effect.gen(function* () {
            const [current] =
              yield* sql<Entry>`SELECT * FROM ${sql(tableName)} WHERE ${sql(Columns.ID)} = ${id} FOR UPDATE`;
            // A block that never reached confirmation (an unlanded signed
            // commit, or one awaiting its submission acknowledgement)
            // selected and classified the withdrawal without assigning it a
            // header; no other block holds it while its journal does.
            const selectedUnassigned =
              current !== undefined &&
              current[Columns.PROJECTED_HEADER_HASH] === null &&
              current[Columns.STATUS] === Status.Projected;
            if (
              !selectedUnassigned &&
              !current?.[Columns.PROJECTED_HEADER_HASH]?.equals(
                removedHeaderHash,
              )
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Cannot reopen withdrawal not assigned to the corrected header",
                  cause: id.toString("hex"),
                }),
              );
            }
            yield* sql`UPDATE ${sql(tableName)} SET
        ${sql(Columns.SETTLEMENT_EVENT_INFO)} = NULL,
        ${sql(Columns.VALIDITY)} = NULL,
        ${sql(Columns.VALIDITY_DETAIL)} = '{}'::jsonb,
        ${sql(Columns.STATUS)} = ${Status.Awaiting},
        ${sql(Columns.PROJECTED_HEADER_HASH)} = NULL,
        ${sql(Columns.REOPENED_FROM_HEADER_HASH)} = ${removedHeaderHash},
        ${sql(Columns.CLASSIFICATION_REVISION)} = ${sql(Columns.CLASSIFICATION_REVISION)} + 1,
        updated_at = NOW()
        WHERE ${sql(Columns.ID)} = ${id}`;
          }),
        { discard: true },
      ),
    );
  }).pipe(
    withHistoryWrite,
    sqlErrorToDatabaseError(
      tableName,
      "Failed to reopen corrected withdrawals",
    ),
  );

/** Only a validated, locked correction journal may supply these snapshots.
 * Each withdrawal must be unassigned and reopened from one of `reopenedFrom`
 * (by default `headerHash` itself): a replaced block that won its state-queue
 * slot takes back withdrawals a later replacement of it re-included and then
 * reopened again. */
export const restoreCorrectedClassification = (
  assignments: readonly Omit<
    SettlementInfoAssignment,
    "expectedClassificationRevision"
  >[],
  headerHash: Buffer,
  reopenedFrom: readonly Buffer[] = [headerHash],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.forEach(
        [...assignments].sort((a, b) => Buffer.compare(a.eventId, b.eventId)),
        (assignment) =>
          Effect.gen(function* () {
            const [current] =
              yield* sql<Entry>`SELECT * FROM ${sql(tableName)} WHERE ${sql(Columns.ID)} = ${assignment.eventId} FOR UPDATE`;
            if (
              !current ||
              current[Columns.PROJECTED_HEADER_HASH] !== null ||
              !reopenedFrom.some(
                (header) =>
                  current[Columns.REOPENED_FROM_HEADER_HASH]?.equals(header) ===
                  true,
              )
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Cannot restore corrected withdrawal after conflicting header assignment or correction",
                  cause: assignment.eventId.toString("hex"),
                }),
              );
            }
            yield* sql`UPDATE ${sql(tableName)} SET
        ${sql(Columns.SETTLEMENT_EVENT_INFO)} = ${assignment.settlementEventInfo},
        ${sql(Columns.VALIDITY)} = ${assignment.validity},
        ${sql(Columns.VALIDITY_DETAIL)} = CAST(${JSON.stringify(assignment.validityDetail ?? {})} AS TEXT)::JSONB,
        ${sql(Columns.STATUS)} = ${Status.Projected},
        ${sql(Columns.PROJECTED_HEADER_HASH)} = ${headerHash},
        ${sql(Columns.REOPENED_FROM_HEADER_HASH)} = NULL,
        ${sql(Columns.CLASSIFICATION_REVISION)} = ${sql(Columns.CLASSIFICATION_REVISION)} + 1,
        updated_at = NOW()
        WHERE ${sql(Columns.ID)} = ${assignment.eventId}`;
          }),
        { discard: true },
      ),
    );
  }).pipe(
    withHistoryWrite,
    sqlErrorToDatabaseError(
      tableName,
      "Failed to restore corrected withdrawal classification",
    ),
  );

export const markFinalizedByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ [Columns.ID]: Buffer }>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Finalized},
          ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash},
          updated_at = NOW()
      WHERE ${sql(Columns.ID)} IN ${sql.in(ids)}
        AND ${sql(Columns.STATUS)} IN (${Status.Projected}, ${Status.Finalized})
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash}
      RETURNING ${sql(Columns.ID)}`;
    if (rows.length !== ids.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to finalize withdrawals because at least one row is missing, unprojected, or assigned to a different header",
          cause: `requested=${ids.length},finalized=${rows.length},header_hash=${projectedHeaderHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    withHistoryWrite,
    Effect.withLogSpan(`markFinalizedByEventIds ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to mark withdrawals finalized"),
  );

export const toRootKeyValue = (
  entry: Entry,
): Effect.Effect<
  { readonly key: Buffer; readonly value: Buffer },
  DatabaseError,
  never
> =>
  Effect.gen(function* () {
    const settlementEventInfo = entry[Columns.SETTLEMENT_EVENT_INFO];
    if (settlementEventInfo === null) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to convert withdrawal to root key/value because it has not been classified",
          cause: `event_id=${entry[Columns.ID].toString("hex")}`,
        }),
      );
    }
    return {
      key: Buffer.from(entry[Columns.ID]),
      value: Buffer.from(settlementEventInfo),
    };
  });

export const toLedgerOutRef = (
  entry: Pick<Entry, Columns.L2_OUTREF>,
): Effect.Effect<Buffer, DatabaseError, never> =>
  Effect.try({
    try: () => {
      const outRef = LucidData.from(
        entry[Columns.L2_OUTREF].toString("hex"),
        SDK.OutputReference,
      );
      // The ledger key is the §5.3 field-0/1 item encoding, matching on-chain
      // `ledger_outref_key` — never CML's minimal-index TransactionInput CBOR.
      return outRefToCbor({
        txHash: outRef.transactionId,
        outputIndex: Number(outRef.outputIndex),
      });
    },
    catch: (cause) =>
      new DatabaseError({
        table: tableName,
        message: "Failed to convert withdrawal l2_outref into ledger outref",
        cause,
      }),
  });

/**
 * Ledger outrefs (hex) named by withdrawals that are not finalized and not
 * classified invalid. Each will be consumed by its withdrawal, or is still
 * unclassified, so an L2 spend of one is refused at admission instead of at
 * commit.
 */
export const retrievePendingLedgerOutRefHexes: Effect.Effect<
  ReadonlySet<string>,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<Pick<Entry, Columns.L2_OUTREF>>`SELECT ${sql(
    Columns.L2_OUTREF,
  )} FROM ${sql(tableName)}
    WHERE ${sql(Columns.STATUS)} IN (${Status.Awaiting}, ${Status.Projected})
      AND (${sql(Columns.VALIDITY)} IS NULL
        OR ${sql(Columns.VALIDITY)} = ${Validity.WithdrawalIsValid})`;
  const outRefs = new Set<string>();
  for (const row of rows) {
    // An l2_outref that does not decode names no L2 output, so no spend can
    // match it; one such row must not stop admission.
    const outRef = yield* Effect.either(toLedgerOutRef(row));
    if (outRef._tag === "Right") outRefs.add(outRef.right.toString("hex"));
    else
      yield* Effect.logWarning(
        `Skipping pending withdrawal l2_outref ${row[Columns.L2_OUTREF].toString("hex")}: it does not decode to an output reference`,
      );
  }
  return outRefs;
}).pipe(
  Effect.withLogSpan(`retrievePendingLedgerOutRefHexes ${tableName}`),
  sqlErrorToDatabaseError(
    tableName,
    "Failed to retrieve outrefs named by pending withdrawals",
  ),
);

export const clear = clearTable(tableName);
