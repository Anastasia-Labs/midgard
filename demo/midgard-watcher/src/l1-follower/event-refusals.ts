import type { SqlTx } from "@al-ft/midgard-l1-follower";
import { REFUSALS_TABLE } from "@al-ft/midgard-l1-follower/events";

import type { WatcherL1Degradation } from "./tx-inputs.js";

/** A user order output the event projection refused to admit. */
export const L1_USER_EVENT_REFUSED = "l1_user_event_refused";

/**
 * The follower's event refusals still within k, as one degradation: their
 * count, the count per refusal reason and the newest refusal. A refusal is
 * a user's malformed or key-reusing order, so it reaches status and metrics
 * only and never fails readiness: anyone could otherwise hold the watcher
 * unready.
 */
export const eventRefusalDegradationsIn = async (
  tx: SqlTx,
): Promise<readonly WatcherL1Degradation[]> => {
  const newest = (
    await tx.query(
      `SELECT reason, kind, slot, tx_hash, output_index, detail FROM ${REFUSALS_TABLE} ORDER BY slot DESC, tx_hash DESC, output_index DESC LIMIT 1`,
      [],
    )
  )[0];
  if (newest === undefined) return [];
  const byReason = await tx.query(
    `SELECT reason, COUNT(*) AS refused FROM ${REFUSALS_TABLE} GROUP BY reason ORDER BY reason`,
    [],
  );
  let total = 0;
  const counts = byReason
    .map((row) => {
      const refused = Number(row.refused as number | string);
      total += refused;
      return `${String(row.reason)}=${refused.toString()}`;
    })
    .join(", ");
  const outRef = `${Buffer.from(newest.tx_hash as Uint8Array).toString("hex")}#${String(newest.output_index)}`;
  return [
    Object.freeze({
      reason: L1_USER_EVENT_REFUSED,
      count: total,
      detail: `${counts}; newest: ${String(newest.kind)} order ${outRef} at slot ${String(newest.slot)} refused as ${String(newest.reason)}: ${String(newest.detail)}`,
    }),
  ];
};
