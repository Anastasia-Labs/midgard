/**
 * Whether the scheduler successor already holds this node's pending L2
 * transactions, as the forwarder's last catch-up check with it showed.
 *
 * At a shift end a node that holds work but is not the successor pushes the
 * turn to the successor. When the successor holds all of that work it takes
 * the turn itself, so the push only races it: the pusher waits the deployment's
 * `new_shift_inactivity_grace_period` first, the window in which the
 * successor is expected to act (`scheduler-refresh.ensure-scheduler-aligned-for-commit.ts`).
 * Anything short of proof (no sharing configured, the successor's lane down,
 * unsigned or mixed answers, one pending tx it lacks, a failed read) means
 * the push goes at once, as without sharing.
 */
import { Effect, Ref } from "effect";

import { L2PeerShareDB } from "../database/index.js";
import type { Database } from "../services/database.js";
import { Globals } from "../services/globals.globals.js";

type HeldGlobals = Pick<Globals, "L2_PEER_HELD_TXS">;

/**
 * Sets (or with `held` undefined, drops) what operator `operator` showed it
 * holds. Does nothing for an unknown operator.
 */
export const recordPeerHeldTxs = (
  globals: HeldGlobals,
  operator: string | undefined,
  held: ReadonlySet<string> | undefined,
): Effect.Effect<void> =>
  operator === undefined
    ? Effect.void
    : Ref.update(globals.L2_PEER_HELD_TXS, (current) => {
        if (held === undefined && !current.has(operator)) return current;
        const next = new Map(current);
        if (held === undefined) next.delete(operator);
        else next.set(operator, held);
        return next;
      });

/**
 * True only when this node has pending L2 transactions and the last catch-up
 * check with `successor` showed it holds every one of them.
 */
export const successorHoldsAllPending = (
  globals: HeldGlobals,
  successor: string,
): Effect.Effect<boolean, never, Database> =>
  Effect.gen(function* () {
    const held = (yield* Ref.get(globals.L2_PEER_HELD_TXS)).get(successor);
    if (held === undefined) return false;
    const pending = yield* L2PeerShareDB.pendingIds;
    return (
      pending.length > 0 && pending.every((id) => held.has(id.toString("hex")))
    );
  }).pipe(Effect.orElseSucceed(() => false));

/** {@link successorHoldsAllPending} bound to this node's globals and database. */
export const successorHoldsAllPendingHere: Effect.Effect<
  (successor: string) => Effect.Effect<boolean>,
  never,
  Globals | Database
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const context = yield* Effect.context<Database>();
  return (successor: string) =>
    successorHoldsAllPending(globals, successor).pipe(Effect.provide(context));
});
