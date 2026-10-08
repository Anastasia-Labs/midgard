/**
 * What the node reads from the queue-terminal projection (N4), as SQL
 * fragments for the statements that act on it: retention, DA delivery and
 * the operator's reports. Each is re-read inside the statement that acts, so
 * a rollback between a caller's L1 view and its write cannot release a row
 * the rollback put back.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  type DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import { Database } from "../services/database.js";
import { QUEUE_TERMINALS_TABLE } from "./schema.js";

/** The verified manifest ID of the running deployment, if it has one. */
export const deploymentIdentityDigestOf = (identity: {
  readonly manifestId?: string;
}): Buffer | undefined =>
  identity.manifestId === undefined
    ? undefined
    : Buffer.from(identity.manifestId, "hex");

const NODE_PREFIX = Buffer.from(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX, "hex");

/**
 * True when `headerColumn` names a header the follower's facts hold a live
 * state-queue node for: a live output carrying the node token of that
 * header (prefix and 28-byte header). Read from the facts in the statement,
 * not from a caller's view. Any policy's token counts: an extra hold only
 * keeps a row longer.
 */
export const liveQueueNodeHeader = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
) => sql`${sql(headerColumn)} IN (
  SELECT substring(asset.asset_name from ${NODE_PREFIX.length + 1})
  FROM l1_output_assets AS asset
  JOIN l1_outputs AS output ON output.tx_hash = asset.tx_hash
    AND output.output_index = asset.output_index
  WHERE output.spent_slot IS NULL
    AND octet_length(asset.asset_name) = ${NODE_PREFIX.length + 28}
    AND substring(asset.asset_name from 1 for ${NODE_PREFIX.length}) = ${NODE_PREFIX})`;

/**
 * True when `headerColumn` names a header with a terminal row that is not
 * final: its tx is at a height above `finalThroughHeight` (the greatest
 * final height of the caller's view). With no height, every terminal row
 * counts as not final.
 */
export const nonFinalTerminalHeader = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
  finalThroughHeight: number | undefined,
) => sql`EXISTS (
  SELECT 1 FROM ${sql(QUEUE_TERMINALS_TABLE)} AS terminal
  WHERE terminal.header_hash = ${sql(headerColumn)}
    AND ${finalThroughHeight === undefined ? sql`TRUE` : sql`terminal.height > ${finalThroughHeight}`})`;

/**
 * True when `headerColumn` names the merge boundary a reader at finality
 * sees: the newest merged header whose merge is at or below
 * `finalThroughHeight` (the newest merged header overall without one). A
 * rollback of every later merge makes it the confirmed head again; the later
 * merges are held as not final (`nonFinalTerminalHeader`).
 */
export const latestMergedHeader = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
  finalThroughHeight?: number,
) => sql`${sql(headerColumn)} IN (
  SELECT latest.header_hash FROM (
    SELECT terminal.header_hash FROM ${sql(QUEUE_TERMINALS_TABLE)} AS terminal
    WHERE terminal.terminal_outcome = 'merged'
      AND ${finalThroughHeight === undefined ? sql`TRUE` : sql`terminal.height <= ${finalThroughHeight}`}
    ORDER BY terminal.height DESC, terminal.tx_index DESC
    LIMIT 1) AS latest)`;

/** The newest terminal outcome of `headerColumn`'s header, or NULL. */
export const newestTerminalOutcome = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
) => sql`(
  SELECT terminal.terminal_outcome FROM ${sql(QUEUE_TERMINALS_TABLE)} AS terminal
  WHERE terminal.header_hash = ${sql(headerColumn)}
  ORDER BY terminal.height DESC, terminal.tx_index DESC
  LIMIT 1)`;

/**
 * True when `headerColumn` names a header whose newest terminal row is a
 * removal.
 */
export const removedHeader = (sql: SqlClient.SqlClient, headerColumn: string) =>
  sql`COALESCE(${newestTerminalOutcome(sql, headerColumn)} = 'removed', FALSE)`;

/**
 * The SQL condition under which the DA payload aliased `payload` is still owed
 * to committee peers and announcements. It is not once a landed tx removed
 * its header from the state queue (the newest terminal row is a removal), or
 * while its journal is abandoned under a named cause (the replacement digest
 * the landed-block rebase records when it disposes of an own block). A
 * rollback of the removal deletes the terminal row, and a journal revived
 * after all clears its digest, so the payload is owed again.
 * `deploymentIdentityDigest` is the verified manifest ID; a node running a
 * derived contract bundle consults only the journal arm.
 */
export const owedPayload = (
  sql: SqlClient.SqlClient,
  deploymentIdentityDigest: Buffer | undefined,
) => {
  const removed =
    deploymentIdentityDigest === undefined
      ? sql`FALSE`
      : removedHeader(sql, "payload.header_hash");
  return sql`NOT ${removed} AND NOT EXISTS (
    SELECT 1 FROM pending_block_finalizations journal
    WHERE journal.header_hash = payload.header_hash
      AND journal.status = 'abandoned'
      AND journal.correction_transition_digest IS NOT NULL)`;
};

/**
 * Deletes the terminal rows the node no longer reads: final (at or below
 * `finalThroughHeight`, so no legal rollback can remove them), and naming a
 * header with neither a DA payload nor a block journal. Returns how many.
 */
export const pruneFinalQueueTerminals = (
  finalThroughHeight: number,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly header_hash: Buffer }>`
      DELETE FROM ${sql(QUEUE_TERMINALS_TABLE)} AS terminal
      WHERE terminal.height <= ${finalThroughHeight}
        AND NOT EXISTS (SELECT 1 FROM da_payloads AS payload
          WHERE payload.header_hash = terminal.header_hash)
        AND NOT EXISTS (SELECT 1 FROM pending_block_finalizations AS journal
          WHERE journal.header_hash = terminal.header_hash)
      RETURNING terminal.header_hash`;
    return rows.length;
  }).pipe(
    sqlErrorToDatabaseError(
      QUEUE_TERMINALS_TABLE,
      "Failed to prune final queue-terminal rows",
    ),
  );

/**
 * The height of the follower's covered tip (its cursor), which depths are
 * counted from, or null before the follower has a tip.
 */
export const followerCoveredTipHeight = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ readonly height: number | string }>`
    SELECT height FROM l1_follower_cursor LIMIT 1`;
  const row = rows[0];
  return row === undefined ? null : Number(row.height);
});
