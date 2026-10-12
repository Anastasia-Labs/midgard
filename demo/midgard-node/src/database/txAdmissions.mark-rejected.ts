import "./txAdmissions.admit-reserved-batch.js";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { SqlClient } from "@effect/sql";
import { Duration, Effect } from "effect";

import { Database } from "../services/database.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import {
  Columns,
  type Entry,
  payloadTableName,
  type RawEntry,
  tableName,
  toBigInt,
  txAdmissionMarkRejectedDurationTimer,
} from "./txAdmissions.verify-claimed-payload-rows.js";
import * as TxRejectionsDB from "./txRejections.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** A terminal admission rejection: a validation rejection, or a refusal by
 * node admission policy under its own code. */
export type AdmissionRejection = Readonly<{
  txId: Buffer;
  code: string;
  detail: string | null;
}>;

export const markRejected = ({
  rows,
  leaseOwner,
  rejectedTxs,
}: {
  readonly rows: readonly Pick<Entry, Columns.TX_ID>[];
  readonly leaseOwner: string;
  readonly rejectedTxs: readonly AdmissionRejection[];
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (rejectedTxs.length === 0) {
      return;
    }
    const startedAt = Date.now();
    const sql = yield* SqlClient.SqlClient;
    const rejectionRows = rejectedTxs.map((rejectedTx) => ({
      tx_id: rejectedTx.txId,
      reject_code: rejectedTx.code,
      reject_detail: rejectedTx.detail,
    }));
    const txIds = rejectedTxs.map((tx) => tx.txId);
    const rejectionValues = rejectedTxs.map(
      (rejectedTx) =>
        sql`(${rejectedTx.txId}, ${rejectedTx.code}, ${rejectedTx.detail})`,
    );
    yield* withFollowerWrite(
      sql.withTransaction(
        Effect.gen(function* () {
          const persistedRejections = yield* sql<
            Pick<TxRejectionsDB.Entry, TxRejectionsDB.Columns.TX_ID>
          >`INSERT INTO ${sql(TxRejectionsDB.tableName)} ${sql.insert(
            rejectionRows,
          )}
          ON CONFLICT (${sql(TxRejectionsDB.Columns.TX_ID)}) DO UPDATE SET
            ${sql(TxRejectionsDB.Columns.TX_ID)} = ${sql(
              TxRejectionsDB.tableName,
            )}.${sql(TxRejectionsDB.Columns.TX_ID)}
          WHERE
            ${sql(TxRejectionsDB.tableName)}.${sql(
              TxRejectionsDB.Columns.REJECT_CODE,
            )} = EXCLUDED.${sql(TxRejectionsDB.Columns.REJECT_CODE)}
            AND ${sql(TxRejectionsDB.tableName)}.${sql(
              TxRejectionsDB.Columns.REJECT_DETAIL,
            )} IS NOT DISTINCT FROM EXCLUDED.${sql(
              TxRejectionsDB.Columns.REJECT_DETAIL,
            )}
          RETURNING ${sql(TxRejectionsDB.Columns.TX_ID)}`;
          if (persistedRejections.length !== rejectedTxs.length) {
            return yield* Effect.fail(
              new DatabaseError({
                table: TxRejectionsDB.tableName,
                message:
                  "Failed to persist rejected transactions exactly once with matching rejection metadata",
                cause: `expected=${rejectedTxs.length},persisted=${persistedRejections.length}`,
              }),
            );
          }
          const updated = yield* sql<Pick<RawEntry, Columns.TX_ID>>`
          UPDATE ${sql(tableName)} AS admissions
          SET
            ${sql(Columns.STATUS)} = 'rejected',
            ${sql(Columns.LEASE_OWNER)} = NULL,
            ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
            ${sql(Columns.TERMINAL_AT)} = GREATEST(
              NOW(),
              admissions.${sql(Columns.FIRST_SEEN_AT)},
              admissions.${sql(Columns.LAST_SEEN_AT)},
              admissions.${sql(Columns.UPDATED_AT)},
              COALESCE(
                admissions.${sql(Columns.VALIDATION_STARTED_AT)},
                admissions.${sql(Columns.FIRST_SEEN_AT)}
              )
            ),
            ${sql(Columns.REJECT_CODE)} = rejected.reject_code,
            ${sql(Columns.REJECT_DETAIL)} = rejected.reject_detail,
            ${sql(Columns.UPDATED_AT)} = GREATEST(
              NOW(),
              admissions.${sql(Columns.FIRST_SEEN_AT)},
              admissions.${sql(Columns.LAST_SEEN_AT)},
              admissions.${sql(Columns.UPDATED_AT)},
              COALESCE(
                admissions.${sql(Columns.VALIDATION_STARTED_AT)},
                admissions.${sql(Columns.FIRST_SEEN_AT)}
              )
            )
          FROM (VALUES ${sql.csv(rejectionValues)})
            AS rejected(tx_id, reject_code, reject_detail)
          WHERE admissions.${sql(Columns.TX_ID)} = rejected.tx_id
            AND admissions.${sql(Columns.STATUS)} = 'validating'
            AND admissions.${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
          RETURNING admissions.${sql(Columns.TX_ID)}`;
          if (updated.length !== txIds.length) {
            return yield* Effect.fail(
              new DatabaseError({
                table: tableName,
                message:
                  "Failed to mark rejected admissions exactly once under the active validation lease",
                cause: `expected=${txIds.length},updated=${updated.length},claimed=${rows.length}`,
              }),
            );
          }
          // Terminal rejections retain the original sidecar digest for exact
          // duplicate matching but do not retain attacker-sized sidecar bytes.
          // Rejected rows are never claimable, so the canonical empty sidecar is
          // a tombstone rather than validation input.
          const terminalSidecar = encodeMidgardCekProgramMaterialSidecar([]);
          yield* sql`UPDATE ${sql(payloadTableName)}
          SET ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)} =
            ${terminalSidecar}
          WHERE ${sql.in(Columns.TX_ID, txIds)}`;
        }),
      ),
    );
    yield* txAdmissionMarkRejectedDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - startedAt)),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to mark admissions rejected"),
  );

/**
 * Terminally rejects admissions a state-queue correction took back after
 * acceptance, with the same terminal shape as `markRejected`. Runs inside the
 * caller's reinclusion transaction, which has already cleared the pending
 * rows and recorded the rejections. A transaction with no admission row (not
 * submitted through admission) has nothing to update; one whose admission is
 * anything but accepted is refused, since its acceptance cannot be undone.
 */
export const markAcceptedRejectedAfterCorrection = (
  rejectedTxs: readonly Readonly<{
    txId: Buffer;
    code: string;
    detail: string;
  }>[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (rejectedTxs.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const txIds = rejectedTxs.map(({ txId }) => txId);
    const rejectionValues = rejectedTxs.map(
      ({ txId, code, detail }) => sql`(${txId}, ${code}, ${detail})`,
    );
    const updated = yield* sql<Pick<RawEntry, Columns.TX_ID>>`
      UPDATE ${sql(tableName)} AS admissions
      SET
        ${sql(Columns.STATUS)} = 'rejected',
        ${sql(Columns.LEASE_OWNER)} = NULL,
        ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
        ${sql(Columns.TERMINAL_AT)} = GREATEST(
          NOW(),
          admissions.${sql(Columns.FIRST_SEEN_AT)},
          admissions.${sql(Columns.LAST_SEEN_AT)},
          admissions.${sql(Columns.UPDATED_AT)},
          COALESCE(
            admissions.${sql(Columns.VALIDATION_STARTED_AT)},
            admissions.${sql(Columns.FIRST_SEEN_AT)}
          )
        ),
        ${sql(Columns.REJECT_CODE)} = rejected.reject_code,
        ${sql(Columns.REJECT_DETAIL)} = rejected.reject_detail,
        ${sql(Columns.UPDATED_AT)} = GREATEST(
          NOW(),
          admissions.${sql(Columns.FIRST_SEEN_AT)},
          admissions.${sql(Columns.LAST_SEEN_AT)},
          admissions.${sql(Columns.UPDATED_AT)},
          COALESCE(
            admissions.${sql(Columns.VALIDATION_STARTED_AT)},
            admissions.${sql(Columns.FIRST_SEEN_AT)}
          )
        )
      FROM (VALUES ${sql.csv(rejectionValues)})
        AS rejected(tx_id, reject_code, reject_detail)
      WHERE admissions.${sql(Columns.TX_ID)} = rejected.tx_id
        AND admissions.${sql(Columns.STATUS)} = 'accepted'
      RETURNING admissions.${sql(Columns.TX_ID)}`;
    const remaining = yield* sql<{
      readonly tx_id: Buffer;
      readonly status: string;
    }>`SELECT ${sql(Columns.TX_ID)} AS tx_id, ${sql(Columns.STATUS)}::text AS status
      FROM ${sql(tableName)}
      WHERE ${sql.in(Columns.TX_ID, txIds)}
        AND ${sql(Columns.STATUS)} <> 'rejected'
      LIMIT 1`;
    if (remaining.length !== 0)
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "A transaction rejected after a state-queue correction has an admission that is not accepted",
          cause: `tx=${remaining[0]?.tx_id.toString("hex")},status=${remaining[0]?.status},updated=${updated.length}`,
        }),
      );
    if (updated.length === 0) return;
    const terminalSidecar = encodeMidgardCekProgramMaterialSidecar([]);
    yield* sql`UPDATE ${sql(payloadTableName)}
      SET ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)} = ${terminalSidecar}
      WHERE ${sql.in(
        Columns.TX_ID,
        updated.map((row) => row[Columns.TX_ID]),
      )}`;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to reject admissions after a state-queue correction",
    ),
  );

export const countBacklog: Effect.Effect<bigint, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly count: bigint | number | string }>`
      SELECT (
        (SELECT COUNT(*) FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} = 'queued')
        +
        (SELECT COUNT(*) FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} = 'validating')
      )::bigint AS count`;
    return toBigInt(rows[0].count);
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to count backlog"));

export const oldestQueuedAgeMs: Effect.Effect<number, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly age_ms: number | string }>`
      SELECT (COALESCE(EXTRACT(EPOCH FROM (NOW() - MIN(${sql(
        Columns.FIRST_SEEN_AT,
      )}))), 0) * 1000)::double precision AS age_ms
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} = 'queued'`;
    return Number(rows[0].age_ms);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to compute oldest queued age"),
  );
