/**
 * The record that this operator was once active (D-N7). An operator that a
 * slash or a bond recovery takes out of every list leaves only its spent
 * active node behind, and the follower prunes spent rows past k. The record
 * keeps the evidence: a block on whose chain this operator's active node
 * existed, written the first time the hook sees an own active-node row, at
 * any landed depth (an operator slashed while its node is offline for more
 * than k must still read removed when the node returns).
 *
 * It is `operator_membership_observations` (migration 0003), keyed by
 * (manifest_id, operator_key). The record counts while the follower's facts
 * do not show its block orphaned (`pointStatusIn`): on the stored chain, or
 * below the retained window. A hook run that finds its block orphaned
 * replaces it with current evidence or clears it. Every orphaning happens
 * within k, inside the retained window, and the driver runs the hook on every
 * rewind it sees, so the orphaned point is gone before it can fall below the
 * window and count. The one exception is a node whose driver does not run
 * between the rewind and a prune past the point (it caught up more than k
 * blocks first); that record then counts, which only reads `removed` for an
 * operator that is in no list at all.
 */
import type { Point } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

export type RecordedActivity = Readonly<{ point: Point; height: number }>;

export type OperatorActivityRecord = Readonly<{
  /** The recorded activation, or null. */
  read: () => Promise<RecordedActivity | null>;
  /** Records (or replaces) the evidence of an activation. */
  write: (activity: RecordedActivity) => Promise<void>;
  /** Drops a record whose block a fork orphaned. */
  clear: () => Promise<void>;
}>;

/** A record kept in memory only (tests, and a deployment without a manifest). */
export const memoryActivityRecord = (): OperatorActivityRecord => {
  let recorded: RecordedActivity | null = null;
  return {
    read: () => Promise.resolve(recorded),
    write: (activity) => {
      recorded = activity;
      return Promise.resolve();
    },
    clear: () => {
      recorded = null;
      return Promise.resolve();
    },
  };
};

/** The node database's record of this operator under one deployment. */
export const databaseActivityRecord = (options: {
  readonly run: <A>(
    effect: Effect.Effect<A, unknown, SqlClient.SqlClient>,
  ) => Promise<A>;
  /** The deployment manifest id (64 hex). */
  readonly manifestId: string;
  /** The operator key hash (56 hex). */
  readonly ownKey: string;
}): OperatorActivityRecord => {
  const manifestId = Buffer.from(options.manifestId, "hex");
  const operatorKey = Buffer.from(options.ownKey, "hex");
  return {
    read: () =>
      options.run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const rows = yield* sql<{
            active_block_hash: Buffer;
            active_block_slot: string;
            active_block_height: string;
          }>`SELECT active_block_hash, active_block_slot::text, active_block_height::text
            FROM operator_membership_observations
            WHERE manifest_id = ${manifestId} AND operator_key = ${operatorKey}`;
          const row = rows[0];
          return row === undefined
            ? null
            : {
                point: {
                  slot: Number(row.active_block_slot),
                  hash: Buffer.from(row.active_block_hash),
                },
                height: Number(row.active_block_height),
              };
        }),
      ),
    write: ({ point, height }) =>
      options.run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`INSERT INTO operator_membership_observations
            (manifest_id, operator_key, active_block_hash, active_block_slot, active_block_height)
            VALUES (${manifestId}, ${operatorKey}, ${point.hash}, ${point.slot}, ${height})
            ON CONFLICT (manifest_id, operator_key) DO UPDATE SET
              active_block_hash = EXCLUDED.active_block_hash,
              active_block_slot = EXCLUDED.active_block_slot,
              active_block_height = EXCLUDED.active_block_height`;
        }),
      ),
    clear: () =>
      options.run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`DELETE FROM operator_membership_observations
            WHERE manifest_id = ${manifestId} AND operator_key = ${operatorKey}`;
        }),
      ),
  };
};
