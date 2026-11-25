/**
 * The snapshot anchor: the confirmed header H whose post-state root R a node
 * took its ledger from (a peer's ledger snapshot), and H's end time. A node
 * that started from the empty ledger has none.
 *
 * A block includes the events whose inclusion time is after its start time
 * (its parent's end time) and at or before its end time, so an event
 * included at or before H's end time belongs to H or an earlier block. The
 * ledger the node took already holds what those events did, and the node
 * gives them no row: a deposit is not projected a second time, and a
 * withdrawal or forced order never becomes a due event no block can take.
 * The nodes holding their rows settle them.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { sqlErrorToDatabaseError } from "./utils/common.js";

export const tableName = "node_ledger_snapshot_anchor";

export type SnapshotAnchor = Readonly<{
  /** H, hex. */
  headerHash: string;
  /** R, H's post-state UTxO root, hex. */
  utxosRoot: string;
  /** H's end time, POSIX ms: the confirmed state's `endTime`. */
  endTimeMs: bigint;
}>;

/**
 * Records the anchor, in the caller's transaction: the one that installs
 * the snapshot's ledger. A node records it once.
 */
export const record = (anchor: SnapshotAnchor) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} (id, header_hash, utxos_root, end_time_ms)
      VALUES (true, ${Buffer.from(anchor.headerHash, "hex")}, ${anchor.utxosRoot},
        ${anchor.endTimeMs.toString()}::bigint)`;
  }).pipe(sqlErrorToDatabaseError(tableName, `Failed to write ${tableName}`));

/** The anchor, or `undefined` on a node that started from the empty ledger. */
export const retrieve = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [row] = yield* sql<{
    header_hash: Buffer;
    utxos_root: string;
    end_time_ms: string;
  }>`SELECT header_hash, utxos_root, end_time_ms::text AS end_time_ms
    FROM ${sql(tableName)}`;
  return row === undefined
    ? undefined
    : ({
        headerHash: Buffer.from(row.header_hash).toString("hex"),
        utxosRoot: row.utxos_root,
        endTimeMs: BigInt(row.end_time_ms),
      } satisfies SnapshotAnchor);
}).pipe(sqlErrorToDatabaseError(tableName, `Failed to read ${tableName}`));

/** Whether H or an earlier block included an event of `inclusionTime` (ms). */
export const includedByAnchor = (
  anchor: SnapshotAnchor | undefined,
  inclusionTime: bigint,
): boolean => anchor !== undefined && inclusionTime <= anchor.endTimeMs;

/**
 * SQL condition, `includedByAnchor` negated: the follower projection row
 * aliased `alias` (its `inclusion_time` in POSIX ms) was included after the
 * anchor, or there is no anchor.
 */
export const includedAfterAnchor = (sql: SqlClient.SqlClient, alias: string) =>
  sql`NOT EXISTS (SELECT 1 FROM ${sql(tableName)} anchor
    WHERE ${sql(alias)}.inclusion_time <= anchor.end_time_ms)`;
