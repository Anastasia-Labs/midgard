/**
 * S6 for a role's own send of journaled bytes (§8.1): the decision to send,
 * taken in one transaction with the view check. The network send follows
 * the commit; S6's reconciler (`reconcile.ts`) takes the same decision for
 * its resends under the pass's view.
 */
import type { Dialect, SqlTx } from "../sql/backend.js";
import { readCursor } from "../store/rows.js";
import { viewValidIn } from "../store/view.js";
import {
  appendIntentEventIn,
  type Intent,
  readIntentEventsIn,
  readIntentIn,
} from "./journal.js";

export type SubmitHoldReason =
  /** No such intent is journaled (or the store has no cursor). */
  | "not_journaled"
  /** It was recorded under a stale view: never sent on its own write. */
  | "stale_at_write"
  /** A rewind since it was recorded removed the point it was planned at. */
  | "view_stale"
  /** S6 abandoned it: its family no longer wants it. */
  | "abandoned";

export type SubmitDecision =
  | Readonly<{ kind: "send"; intent: Intent }>
  | Readonly<{ kind: "hold"; reason: SubmitHoldReason; detail: string }>;

/**
 * Whether the role may send `txHash`'s journaled bytes now, in the caller's
 * write transaction: only when it was not recorded stale (`stale_at_write`),
 * was not abandoned, and its view is still valid (`viewValid(V)` under the
 * cursor share lock; SQLite: the writer's `BEGIN IMMEDIATE`). A `send`
 * appends a `submit_attempt` event in that transaction. Anything held is
 * left to S6's reconciler, which resends it only if, under the current view,
 * it can still land and is still wanted.
 *
 * Residual window: the send happens after this transaction commits, so a
 * rewind that commits between the commit and the node's mempool admitting
 * the bytes is not seen here. Such a transaction can still land only if
 * its inputs survived the rewind; that is the plan's residual risk 1
 * (§8.1), bounded by the commit anchor d blocks below the planning view and
 * whichever-lands-wins.
 */
export const decideSubmitIn = async (
  tx: SqlTx,
  dialect: Dialect,
  txHash: Buffer,
): Promise<SubmitDecision> => {
  const cursor = await readCursor(tx, dialect, "share");
  const intent =
    cursor === null ? null : await readIntentIn(tx, dialect, txHash);
  if (cursor === null || intent === null)
    return {
      kind: "hold",
      reason: "not_journaled",
      detail: `tx ${txHash.toString("hex")} is not journaled`,
    };
  const events = await readIntentEventsIn(tx, txHash);
  if (events.some((event) => event.kind === "abandoned"))
    return {
      kind: "hold",
      reason: "abandoned",
      detail: "S6 abandoned it; its family no longer wants it",
    };
  if (events.some((event) => event.kind === "stale_at_write"))
    return {
      kind: "hold",
      reason: "stale_at_write",
      detail:
        "it was recorded under a stale view; S6 decides under the current one",
    };
  if (!(await viewValidIn(tx, dialect, intent.built)))
    return {
      kind: "hold",
      reason: "view_stale",
      detail: `a rewind removed its planned view (generation ${intent.built.generation}, slot ${intent.built.point.slot}; now generation ${cursor.generation}); S6 decides under the current view`,
    };
  await appendIntentEventIn(tx, dialect, txHash, "submit_attempt", {
    detail: { generation: cursor.generation, by: "role" },
    tipSlot: cursor.point.slot,
  });
  return { kind: "send", intent };
};
