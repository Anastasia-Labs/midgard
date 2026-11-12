import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import { Columns, tableName } from "./pendingBlockFinalizations.columns.js";
import { type DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/**
 * Deletes `headerHash`'s journal if its commit was never signed, with its
 * members (by cascade). An unsigned journal was never broadcast, so its
 * block can never land, and no event is projected to it (events are
 * projected to a block once it lands); deleting it leaves nothing for the
 * landed-block rebase to dispose of. A signed journal is never touched: its
 * commit may be on L1.
 *
 * Returns whether a journal was deleted.
 */
export const discardUnsignedJournal = (
  headerHash: Buffer,
): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const deleted = yield* sql`DELETE FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.INTENDED_TX_HASH)} IS NULL
      RETURNING ${sql(Columns.HEADER_HASH)}`;
    return deleted.length === 1;
  }).pipe(
    withFollowerWrite,
    Effect.withLogSpan(`discardUnsignedJournal ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to discard an unsigned journal"),
  );

/**
 * Deletes every journal whose commit was never signed (see
 * `discardUnsignedJournal`). The caller holds the state-queue mutation
 * lease, so each was left by an attempt that stopped before signing.
 *
 * Returns the deleted journals' headers (hex).
 */
export const discardUnsignedJournals: Effect.Effect<
  readonly string[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const deleted = yield* sql<{ header_hash: Buffer }>`DELETE FROM ${sql(
    tableName,
  )}
    WHERE ${sql(Columns.INTENDED_TX_HASH)} IS NULL
    RETURNING ${sql(Columns.HEADER_HASH)}`;
  return deleted.map((row) => row.header_hash.toString("hex"));
}).pipe(
  withFollowerWrite,
  Effect.withLogSpan(`discardUnsignedJournals ${tableName}`),
  sqlErrorToDatabaseError(tableName, "Failed to discard unsigned journals"),
);
