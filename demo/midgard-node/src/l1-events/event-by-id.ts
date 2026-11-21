/**
 * One deposit or withdrawal event by its id, as the node's jobs read it
 * from the follower's facts (#869).
 *
 * The follower's own read (`eventOrderByIdIn`) answers `absent` both for an
 * event it has never admitted and for one whose retirement went more than k
 * deep, once the prune step removed its rows. A job keyed to the event must
 * tell them apart: the first may still come, the second is settled for
 * good. The follower keeps every admitted event key forever while it is
 * canonical (`l1_event_keys`, class A: prune never removes a key, and a
 * rewind removes only keys first seen above its target, together with
 * their admissions). The prune step removes an event's rows only once its
 * retirement is more than k deep. So, at the follower's tip, an event key
 * the follower knows with no event row is an event retired more than k
 * deep: `pruned`.
 *
 * Run it in one snapshot of the node database (`inFollowerSnapshot`), as
 * `eventOrderByIdIn` asks.
 */
import { postgresDialect, type SqlTx } from "@al-ft/midgard-l1-follower";
import {
  eventKeyOfId,
  eventOrderByIdIn,
  type EventOrderList,
  type EventOrderRead,
} from "@al-ft/midgard-l1-follower/events";

export type NodeEventOrderRead =
  | EventOrderRead
  /** Admitted on the canonical chain and retired more than k deep; the
   * follower pruned its rows and keeps only its key. */
  | Readonly<{ kind: "pruned" }>;

export const eventOrderOrPrunedByIdIn = async (
  tx: SqlTx,
  list: EventOrderList,
  eventId: Buffer,
): Promise<NodeEventOrderRead> => {
  const read = await eventOrderByIdIn(tx, postgresDialect, list, eventId);
  if (read.kind !== "absent") return read;
  const known = await tx.query(
    "SELECT 1 AS known FROM l1_event_keys WHERE kind = ? AND key = ?",
    [list.kind, eventKeyOfId(eventId)],
  );
  return known.length > 0 ? { kind: "pruned" } : read;
};
