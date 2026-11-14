/**
 * The activity record in the follower's prune step (N6-R5). The driver
 * hook drops a record whose block a fork orphaned, but it runs only while
 * the follower is caught up; a catch-up prune could otherwise pass the
 * orphaned point first, leaving it below the retained window, where
 * `pointStatusIn` reads `point_beyond_retention` and the record counts: a
 * false `operator_removed` for an operator in no list. So the prune step
 * itself drops it, in its transaction:
 *
 * - The floor runs first, before the step moves `pruned_through_slot`. A
 *   record whose point the facts show orphaned (`point_not_canonical`) at or
 *   above the boundary holds the boundary at its slot, so the point stays
 *   decidable (`point_not_canonical`) through the step.
 * - The retention hook then drops it (`clearIn`), in the same transaction.
 *   The floor holds for that one step.
 *
 * A retention hook is skipped while a store reset replays (`PruneHook`):
 * the facts are incomplete until the replay passes them. The floor is not,
 * so a replay prune never passes an orphaned point either: a point still
 * absent at its slot (orphaned, or not replayed yet) holds the boundary
 * there. A canonical point is replayed (the cursor passes its height) long
 * before the boundary, k blocks behind the cursor, reaches its slot, and
 * then holds nothing. An orphaned one holds the boundary until the replay
 * ends and the first prune step after it drops the record. So once the
 * replay ends the point still reads `point_not_canonical`, never
 * `point_beyond_retention`, and no false removal can follow from the skip.
 */
import {
  type Dialect,
  pointStatusIn,
  type PruneFloor,
  type PruneHook,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";

import type { OperatorActivityRecord, RecordedActivity } from "./activity.js";

/**
 * The activity record the prune step reads: undefined until the node knows
 * its operator key (resolved once the store is open), or when it has none.
 */
export type ActivityRecordBinding = () => OperatorActivityRecord | undefined;

export const OPERATOR_ACTIVITY_TABLE = "operator_membership_observations";

/** The record, when the facts show its block orphaned. */
const orphanedIn = async (
  tx: SqlTx,
  dialect: Dialect,
  record: OperatorActivityRecord,
): Promise<RecordedActivity | null> => {
  const recorded = await record.readIn(tx);
  if (recorded === null) return null;
  const status = await pointStatusIn(tx, dialect, recorded.point);
  return status.kind === "point_not_canonical" ? recorded : null;
};

/** Holds the boundary at an orphaned record's slot while it is at or above it. */
export const activityPruneFloor = (
  activity: ActivityRecordBinding,
): PruneFloor => ({
  name: "operator_activity_record",
  floor: async ({ tx, dialect }) => {
    const record = activity();
    if (record === undefined) return null;
    const orphaned = await orphanedIn(tx, dialect, record);
    if (orphaned === null) return null;
    const cursor = await tx.query(
      "SELECT pruned_through_slot FROM l1_follower_cursor",
    );
    const prunedThrough = Number(cursor[0]?.pruned_through_slot ?? 0);
    // Below the boundary already (a kept block holds its slot): it reads
    // orphaned whatever the step does, so nothing needs holding.
    return orphaned.point.slot >= prunedThrough ? orphaned.point.slot : null;
  },
});

/** Drops a record whose block the facts show orphaned. */
export const activityPruneHook = (
  activity: ActivityRecordBinding,
): PruneHook => ({
  kind: "retention",
  table: OPERATOR_ACTIVITY_TABLE,
  apply: async ({ tx, dialect }) => {
    const record = activity();
    if (record === undefined) return 0;
    return (await orphanedIn(tx, dialect, record)) === null
      ? 0
      : await record.clearIn(tx);
  },
});
