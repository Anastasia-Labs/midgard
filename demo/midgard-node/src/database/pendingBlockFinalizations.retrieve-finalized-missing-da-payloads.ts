import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { withHistoryWrite } from "../services/event-history-producer.js";
import * as DaPayloadsDB from "./daPayloads.js";
import {
  Columns,
  MemberColumns,
  type RawRow,
  type Row,
  Status,
  tableName,
  WithdrawalMemberColumns,
  type WithdrawalMemberRecord,
} from "./pendingBlockFinalizations.columns.js";
import { decodePendingBlockFinalizationRow } from "./pendingBlockFinalizations.decode-pending-block-finalization-row.js";
import { type Record } from "./pendingBlockFinalizations.parse-ledger-delta.js";
import { retrieveRecord } from "./pendingBlockFinalizations.retrieve-record.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as WithdrawalsDB from "./withdrawals.js";

export const retrieveByStateQueueLeaseToken = (
  stateQueueLeaseToken: string,
): Effect.Effect<readonly Row[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATE_QUEUE_LEASE_TOKEN)} = ${stateQueueLeaseToken}
      ORDER BY ${sql(Columns.CREATED_AT)} ASC`;
    return yield* Effect.forEach(
      rows,
      (row) =>
        decodePendingBlockFinalizationRow(row).pipe(
          Effect.map(({ normalizedRow }) => normalizedRow),
        ),
      { concurrency: 1 },
    );
  }).pipe(
    Effect.withLogSpan(`retrieveByStateQueueLeaseToken ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve pending-finalization records by state-queue lease token",
    ),
  );

export const retrieveFinalizedMissingDaPayloads = ({
  headerHash,
  limit = 100,
}: {
  readonly headerHash?: Buffer;
  readonly limit?: number;
} = {}): Effect.Effect<readonly Record[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const safeLimit = Math.max(1, Math.floor(limit));
    const sql = yield* SqlClient.SqlClient;
    const rows =
      headerHash === undefined
        ? yield* sql<RawRow>`SELECT ${sql(tableName)}.* FROM ${sql(tableName)}
            LEFT JOIN ${sql(DaPayloadsDB.tableName)}
              ON ${sql(DaPayloadsDB.tableName)}.${sql(
                DaPayloadsDB.Columns.HEADER_HASH,
              )} = ${sql(tableName)}.${sql(Columns.HEADER_HASH)}
            WHERE ${sql(tableName)}.${sql(Columns.STATUS)} = ${Status.Finalized}
              AND ${sql(DaPayloadsDB.tableName)}.${sql(
                DaPayloadsDB.Columns.HEADER_HASH,
              )} IS NULL
            ORDER BY ${sql(tableName)}.${sql(Columns.CREATED_AT)} ASC
            LIMIT ${safeLimit}`
        : yield* sql<RawRow>`SELECT ${sql(tableName)}.* FROM ${sql(tableName)}
            LEFT JOIN ${sql(DaPayloadsDB.tableName)}
              ON ${sql(DaPayloadsDB.tableName)}.${sql(
                DaPayloadsDB.Columns.HEADER_HASH,
              )} = ${sql(tableName)}.${sql(Columns.HEADER_HASH)}
            WHERE ${sql(tableName)}.${sql(Columns.STATUS)} = ${Status.Finalized}
              AND ${sql(DaPayloadsDB.tableName)}.${sql(
                DaPayloadsDB.Columns.HEADER_HASH,
              )} IS NULL
              AND ${sql(tableName)}.${sql(Columns.HEADER_HASH)} = ${headerHash}
            ORDER BY ${sql(tableName)}.${sql(Columns.CREATED_AT)} ASC
            LIMIT ${safeLimit}`;
    return yield* Effect.forEach(rows, (row) => retrieveRecord(sql, row), {
      concurrency: 1,
    });
  }).pipe(
    Effect.withLogSpan(`retrieveFinalizedMissingDaPayloads ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve finalized journals missing DA payloads",
    ),
  );

/** A lost submit response requires reconciliation before another candidate build.
 * The transactional prepare guard remains authoritative against later races. */
export const assertNoUnreconciledSignedSubmission = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
  }>`SELECT header_hash FROM pending_block_finalizations
    WHERE status = ${Status.PendingSubmission} AND intended_tx_hash IS NOT NULL LIMIT 1`;
  if (rows.length !== 0)
    return yield* Effect.fail(
      new DatabaseError({
        table: tableName,
        message:
          "Refusing to prepare a new pending block while another active pending-finalization record exists",
        cause: `signed_header=${rows[0]!.header_hash.toString("hex")}; canonical reconciliation required`,
      }),
    );
}).pipe(
  withHistoryWrite,
  sqlErrorToDatabaseError(tableName, "Failed signed submission preflight"),
);

export const withdrawalMemberToAssignment = (
  member: WithdrawalMemberRecord,
): WithdrawalsDB.SettlementInfoAssignment => ({
  eventId: member[MemberColumns.MEMBER_ID],
  expectedClassificationRevision:
    member[WithdrawalMemberColumns.CLASSIFICATION_REVISION],
  settlementEventInfo: member[MemberColumns.PAYLOAD_CBOR],
  validity: member[WithdrawalMemberColumns.VALIDITY],
  validityDetail: member[WithdrawalMemberColumns.VALIDITY_DETAIL],
});
