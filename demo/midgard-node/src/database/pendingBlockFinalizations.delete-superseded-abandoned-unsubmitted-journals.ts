import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import * as MutationJobsDB from "./mutationJobs.js";
import {
  ACTIVE_STATUSES,
  Columns,
  MemberColumns,
  type MemberRecord,
  type RawRow,
  type Row,
  Status,
  tableName,
} from "./pendingBlockFinalizations.columns.js";
import { decodePendingBlockFinalizationRow } from "./pendingBlockFinalizations.decode-pending-block-finalization-row.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as TxTable from "./utils/tx.js";

export const reviveAbandonedCanonical = (
  headerHash: Buffer,
  observedConfirmedAtMs: bigint,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.ObservedWaitingStability},
          ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)} = COALESCE(
            ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)},
            ${observedConfirmedAtMs}
          ),
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} = ${Status.Abandoned}
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to revive abandoned canonical pending block journal",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    withFollowerWrite,
    Effect.withLogSpan(`reviveAbandonedCanonical ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to revive abandoned canonical pending block journal",
    ),
  );

export const markFinalized = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql.withTransaction(
      Effect.gen(function* () {
        const updated = yield* sql<Row>`UPDATE ${sql(tableName)}
          SET ${sql(Columns.STATUS)} = ${Status.LocallyApplied},
              ${sql(Columns.UPDATED_AT)} = NOW()
          WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
            AND ${sql(Columns.STATUS)} IN (
              ${Status.SubmittedUnconfirmed},
              ${Status.ObservedWaitingStability}
            )
          RETURNING *`;
        const finalized = updated[0];
        if (finalized !== undefined) {
          yield* deleteSupersededAbandonedUnsubmittedJournals(sql, finalized);
        }
        return updated;
      }),
    );
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to finalize pending block journal",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    withFollowerWrite,
    Effect.withLogSpan(`markFinalized ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to finalize pending block journal",
    ),
  );

const deleteSupersededAbandonedUnsubmittedJournals = (
  sql: SqlClient.SqlClient,
  finalized: Row,
) =>
  sql<{ readonly header_hash: Buffer }>`DELETE FROM ${sql(tableName)}
    WHERE ${sql(Columns.STATUS)} = ${Status.Abandoned}
      AND ${sql(Columns.SUBMITTED_TX_HASH)} IS NULL
      AND ${sql(Columns.INTENDED_TX_HASH)} IS NULL
      AND ${sql(Columns.HEADER_HASH)} <> ${finalized[Columns.HEADER_HASH]}
      AND ${sql(Columns.BASE_TAIL_HEADER_HASH)} = ${
        finalized[Columns.BASE_TAIL_HEADER_HASH]
      }
      AND ${sql(Columns.BASE_UTXOS_ROOT)} = ${
        finalized[Columns.BASE_UTXOS_ROOT]
      }
      AND ${sql(Columns.BASE_FORCED_TRANSACTIONS_ROOT)} = ${
        finalized[Columns.BASE_FORCED_TRANSACTIONS_ROOT]
      }
      AND ${sql(Columns.BASE_TRANSACTIONS_ROOT)} = ${
        finalized[Columns.BASE_TRANSACTIONS_ROOT]
      }
      AND ${sql(Columns.BASE_DEPOSITS_ROOT)} = ${
        finalized[Columns.BASE_DEPOSITS_ROOT]
      }
      AND ${sql(Columns.BASE_WITHDRAWALS_ROOT)} = ${
        finalized[Columns.BASE_WITHDRAWALS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_UTXOS_ROOT)} = ${
        finalized[Columns.EXPECTED_UTXOS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT)} = ${
        finalized[Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_TRANSACTIONS_ROOT)} = ${
        finalized[Columns.EXPECTED_TRANSACTIONS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_DEPOSITS_ROOT)} = ${
        finalized[Columns.EXPECTED_DEPOSITS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_WITHDRAWALS_ROOT)} = ${
        finalized[Columns.EXPECTED_WITHDRAWALS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_TRANSITION_TRACE_ROOT)} = ${
        finalized[Columns.EXPECTED_TRANSITION_TRACE_ROOT]
      }
      AND ${sql(Columns.EXPECTED_EVENT_TO_STEP_ROOT)} = ${
        finalized[Columns.EXPECTED_EVENT_TO_STEP_ROOT]
      }
      AND ${sql(Columns.EXPECTED_WITHDRAWAL_COUNT)} = ${
        finalized[Columns.EXPECTED_WITHDRAWAL_COUNT]
      }
      AND ${sql(Columns.EXPECTED_FORCED_TRANSACTION_COUNT)} = ${
        finalized[Columns.EXPECTED_FORCED_TRANSACTION_COUNT]
      }
      AND ${sql(Columns.EXPECTED_L2_TRANSACTION_COUNT)} = ${
        finalized[Columns.EXPECTED_L2_TRANSACTION_COUNT]
      }
      AND ${sql(Columns.EXPECTED_DEPOSIT_COUNT)} = ${
        finalized[Columns.EXPECTED_DEPOSIT_COUNT]
      }
      AND ${sql(Columns.EXPECTED_TOTAL_EVENT_COUNT)} = ${
        finalized[Columns.EXPECTED_TOTAL_EVENT_COUNT]
      }
      AND ${sql(Columns.EXPECTED_TRANSITION_STEP_COUNT)} = ${
        finalized[Columns.EXPECTED_TRANSITION_STEP_COUNT]
      }
    RETURNING ${sql(Columns.HEADER_HASH)}`;

export const deleteSupersededAbandonedUnsubmitted = (): Effect.Effect<
  number,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const deleted = yield* sql.withTransaction(
      Effect.gen(function* () {
        const finalizedRows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} = ${Status.LocallyApplied}
          ORDER BY ${sql(Columns.CREATED_AT)} ASC`;
        const deletedRows = yield* Effect.forEach(
          finalizedRows,
          (finalized) =>
            decodePendingBlockFinalizationRow(finalized).pipe(
              Effect.flatMap(({ normalizedRow }) =>
                deleteSupersededAbandonedUnsubmittedJournals(
                  sql,
                  normalizedRow,
                ),
              ),
            ),
          { concurrency: 1 },
        );
        return deletedRows.reduce((total, rows) => total + rows.length, 0);
      }),
    );
    return deleted;
  }).pipe(
    withFollowerWrite,
    Effect.withLogSpan(`deleteSupersededAbandonedUnsubmitted ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to delete superseded pre-submit pending block journals",
    ),
  );

export const markAbandoned = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Abandoned},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
        AND ${sql(Columns.INTENDED_TX_HASH)} IS NULL
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to abandon pending block journal",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
    yield* MutationJobsDB.abandonLocalBlockFinalization(
      headerHash,
      "pending block journal abandoned before its commit was signed",
    );
  }).pipe(
    withFollowerWrite,
    Effect.withLogSpan(`markAbandoned ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to abandon pending block journal",
    ),
  );

/**
 * Terminal transition for a locally known block removed by authenticated
 * state-queue correction. Idempotence is restricted to the resulting
 * Abandoned state; every other missing/conflicting status fails closed.
 */
export const markCorrectedAfterStateQueueRemoval = (
  headerHash: Buffer,
  transitionDigest: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Abandoned},
          ${sql(Columns.CORRECTION_TRANSITION_DIGEST)} = ${transitionDigest},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN (
          ${Status.SubmittedLocalFinalizationPending},
          ${Status.SubmittedUnconfirmed},
          ${Status.ObservedWaitingStability},
          ${Status.LocallyApplied}
        )
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to mark state-queue-corrected block abandoned",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
    yield* MutationJobsDB.abandonLocalBlockFinalization(
      headerHash,
      `block removed on L1 by admitted state-queue correction ${transitionDigest}`,
    );
  }).pipe(
    withFollowerWrite,
    Effect.withLogSpan(`markCorrectedAfterStateQueueRemoval ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark state-queue-corrected journal",
    ),
  );

export const markUnsubmittedAbandoned = (
  headerHash: Buffer,
): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Abandoned},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} = ${Status.PendingSubmission}
        AND ${sql(Columns.SUBMITTED_TX_HASH)} IS NULL
      AND ${sql(Columns.INTENDED_TX_HASH)} IS NULL
      RETURNING *`;
    if (rows.length !== 1) return false;
    yield* MutationJobsDB.abandonLocalBlockFinalization(
      headerHash,
      "pending block journal abandoned before submission",
    );
    return true;
  }).pipe(
    withFollowerWrite,
    Effect.withLogSpan(`markUnsubmittedAbandoned ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to abandon unsubmitted pending block journal",
    ),
  );

export const txMemberToEntry = (
  member: MemberRecord,
): TxTable.EntryWithTimeStamp => ({
  [TxTable.Columns.TX_ID]: Buffer.from(member[MemberColumns.MEMBER_ID]),
  [TxTable.Columns.TX]: Buffer.from(member[MemberColumns.PAYLOAD_CBOR]),
  [TxTable.Columns.TIMESTAMPTZ]: member[MemberColumns.SOURCE_TIMESTAMP],
});
