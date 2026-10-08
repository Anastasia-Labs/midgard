import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import {
  requireCandidateHistory,
  withHistoryWrite,
} from "../services/event-history-producer.js";
import * as DepositsDB from "./deposits.js";
import * as HistoryAuthority from "./eventHistoryAuthority.js";
import {
  ACTIVE_STATUSES,
  Columns,
  MemberColumns,
  type MemberRecord,
  type Row,
  Status,
  tableName,
} from "./pendingBlockFinalizations.columns.js";
import { validateSignedIntent } from "./pendingBlockFinalizations.decode-pending-block-finalization-row.js";
import { retrieveByHeaderHash } from "./pendingBlockFinalizations.retrieve-record.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as WithdrawalsDB from "./withdrawals.js";

type IdentifiedMember = Pick<
  MemberRecord,
  MemberColumns.MEMBER_ID | "l1_event_key" | "l1_origin_outref"
>;

/** Check retained admission identity before applying journal effects: the
 * member's follower admission identity is its event row's, and the follower's
 * key set still holds it. Retirement keeps the key, so a spent list node
 * remains a valid member. Public event IDs alone never authorize mutation of a
 * replacement row. */
export const assertCanonicalEventMembers = (record: {
  readonly depositMembers: readonly IdentifiedMember[];
  readonly withdrawalMembers: readonly IdentifiedMember[];
}): Effect.Effect<void, DatabaseError, Database> =>
  withHistoryWrite(
    Effect.gen(function* () {
      const owned = yield* HistoryAuthority.currentOwnedTransaction;
      const sql = yield* SqlClient.SqlClient;
      for (const [kind, eventTable, members] of [
        ["deposit", DepositsDB.tableName, record.depositMembers],
        ["withdrawal", WithdrawalsDB.tableName, record.withdrawalMembers],
      ] as const) {
        for (const member of members) {
          const key = member.l1_event_key;
          const origin = member.l1_origin_outref;
          // Only the explicit, unowned fixture transaction may contain old model
          // members. withHistoryWrite has already excluded any acquired owner.
          if (Option.isNone(owned) && key == null && origin == null) continue;
          if (key?.length !== 32 || origin?.length !== 34)
            return yield* Effect.fail(
              new DatabaseError({
                table: tableName,
                message:
                  "Journal member is missing its exact history incarnation",
                cause: member[MemberColumns.MEMBER_ID].toString("hex"),
              }),
            );
          const rows = yield* sql`
          SELECT e.event_id FROM ${sql(eventTable)} e
          JOIN l1_event_keys k ON k.kind = ${kind}
            AND k.key = e.l1_event_key AND k.origin_outref = e.l1_origin_outref
          WHERE e.event_id = ${member[MemberColumns.MEMBER_ID]}
            AND e.l1_event_key = ${key} AND e.l1_origin_outref = ${origin}
          FOR UPDATE OF e FOR SHARE OF k`;
          if (rows.length !== 1)
            return yield* Effect.fail(
              new DatabaseError({
                table: tableName,
                message:
                  "Journal member no longer identifies its canonical history row",
                cause: member[MemberColumns.MEMBER_ID].toString("hex"),
              }),
            );
        }
      }
    }),
  ).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to check journal event incarnations",
    ),
  );

/** SQL commit must complete before handing these exact signed bytes to L1. */
export const recordSignedIntent = (
  headerHash: Buffer,
  txHash: Buffer,
  signedCbor: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (
      Option.isSome(
        yield* Effect.serviceOption(SqlClient.TransactionConnection),
      )
    )
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Signed intent must commit before broadcast outside an inherited transaction",
          cause: headerHash.toString("hex"),
        }),
      );
    yield* Effect.try({
      try: () => validateSignedIntent(txHash, signedCbor),
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message: "Invalid signed transaction intent",
          cause,
        }),
    });
    yield* withHistoryWrite(
      Effect.gen(function* () {
        yield* requireCandidateHistory;
        const sql = yield* SqlClient.SqlClient;
        const record = yield* retrieveByHeaderHash(headerHash, true);
        if (Option.isNone(record))
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message: "Signed intent has no prepared journal",
              cause: headerHash.toString("hex"),
            }),
          );
        yield* assertCanonicalEventMembers(record.value);
        const rows = yield* sql`UPDATE ${sql(tableName)}
        SET intended_tx_hash = ${txHash}, signed_tx_cbor = ${signedCbor}, updated_at = clock_timestamp()
        WHERE header_hash = ${headerHash} AND status = ${Status.PendingSubmission}
          AND submitted_tx_hash IS NULL AND prepared_tx_hash = ${txHash}
          AND ((intended_tx_hash IS NULL AND signed_tx_cbor IS NULL)
            OR (intended_tx_hash = ${txHash} AND signed_tx_cbor = ${signedCbor}))
        RETURNING header_hash`;
        if (rows.length !== 1)
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message: "Signed intent conflicts with the pending journal",
              cause: headerHash.toString("hex"),
            }),
          );
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to persist signed intent before broadcast",
    ),
  );

export const markSubmitted = (
  headerHash: Buffer,
  submittedTxHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const owned = yield* HistoryAuthority.currentOwnedTransaction;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.SUBMITTED_TX_HASH)} = ${submittedTxHash},
          ${sql(Columns.STATUS)} = CASE WHEN ${sql(Columns.STATUS)} = ${Status.PendingSubmission}
            THEN ${Status.SubmittedLocalFinalizationPending} ELSE ${sql(Columns.STATUS)} END,
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN ${sql.in([...ACTIVE_STATUSES, Status.LocallyApplied])}
        AND (${sql(Columns.INTENDED_TX_HASH)} = ${submittedTxHash} OR (${sql(Columns.INTENDED_TX_HASH)} IS NULL AND ${Option.isNone(owned)}))
        AND (${sql(Columns.SUBMITTED_TX_HASH)} IS NULL OR ${sql(Columns.SUBMITTED_TX_HASH)} = ${submittedTxHash})
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to mark pending block as submitted",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    withHistoryWrite,
    Effect.withLogSpan(`markSubmitted ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark pending block as submitted",
    ),
  );

export const discardUnsubmittedPendingSubmission = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} = ${Status.PendingSubmission}
        AND ${sql(Columns.SUBMITTED_TX_HASH)} IS NULL
      AND ${sql(Columns.INTENDED_TX_HASH)} IS NULL`;
  }).pipe(
    withHistoryWrite,
    Effect.withLogSpan(`discardUnsubmittedPendingSubmission ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to discard unsubmitted pending block journal",
    ),
  );

export const markLocalFinalizationComplete = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.SubmittedUnconfirmed},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN (
          ${Status.SubmittedLocalFinalizationPending},
          ${Status.SubmittedUnconfirmed}
        )
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to mark pending block as locally finalized and awaiting confirmation",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    withHistoryWrite,
    Effect.withLogSpan(`markLocalFinalizationComplete ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark pending block local finalization complete",
    ),
  );

export const markObservedWaitingStability = (
  headerHash: Buffer,
  observedConfirmedAtMs: bigint,
  submittedTxHash?: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.SUBMITTED_TX_HASH)} = COALESCE(
            ${sql(Columns.SUBMITTED_TX_HASH)},
            ${submittedTxHash ?? null}
          ),
          ${sql(Columns.STATUS)} = ${Status.ObservedWaitingStability},
          ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)} = COALESCE(
            ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)},
            ${observedConfirmedAtMs}
          ),
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN (
          ${Status.PendingSubmission},
          ${Status.SubmittedLocalFinalizationPending},
          ${Status.SubmittedUnconfirmed},
          ${Status.ObservedWaitingStability}
        )
        AND (${submittedTxHash ?? null}::bytea IS NULL OR ${sql(Columns.SUBMITTED_TX_HASH)} IS NULL OR ${sql(Columns.SUBMITTED_TX_HASH)} = ${submittedTxHash ?? null})
        AND (${submittedTxHash ?? null}::bytea IS NULL OR ${sql(Columns.INTENDED_TX_HASH)} IS NULL OR ${sql(Columns.INTENDED_TX_HASH)} = ${submittedTxHash ?? null})
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to mark pending block as observed waiting stability",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    withHistoryWrite,
    Effect.withLogSpan(`markObservedWaitingStability ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark pending block as observed waiting stability",
    ),
  );
