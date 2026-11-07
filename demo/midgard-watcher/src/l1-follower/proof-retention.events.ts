import type { SqlTx } from "@al-ft/midgard-l1-follower";
import { EVENTS_TABLE } from "@al-ft/midgard-l1-follower/events";

/**
 * How a deposit or withdrawal event's row stands under the follower's
 * cursor lock (proof-retention.ts, event holds):
 * - `present`: the event projection stores its row;
 * - `pruned`: no row, but its key is in the follower's never-reuse key set
 *   (`l1_event_keys`), which a rewind clears with the row and pruning never
 *   touches: the event was admitted and its retired row pruned since;
 * - `none`: never admitted on the stored chain (the read decides).
 */
export const eventRowStateIn = async (
  tx: SqlTx,
  kind: string,
  key: Buffer,
): Promise<"present" | "pruned" | "none"> => {
  const row = await tx.query(
    `SELECT 1 AS one FROM ${EVENTS_TABLE} WHERE kind = ? AND event_key = ? LIMIT 1`,
    [kind, key],
  );
  if (row.length > 0) return "present";
  const known = await tx.query(
    "SELECT 1 AS one FROM l1_event_keys WHERE kind = ? AND key = ? LIMIT 1",
    [kind, key],
  );
  return known.length > 0 ? "pruned" : "none";
};
