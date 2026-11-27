/**
 * This operator's membership, from the operator set.
 *
 * - `active`: its active node is live.
 * - `removed`: its retired node is live, or it is in no list after having
 *   been active (its active node in the follower's retained facts, or a
 *   recorded activation at a final block). Removal is the `/readyz` reason
 *   `operator_removed` under `HaltSource.operatorMembership`, which holds
 *   every operator duty (`FIBER_HALT_SOURCES`), and then shuts the node down
 *   (`untilOperatorRemoved`) with exit code 0.
 * - `awaiting_activation`: registered, not yet active (also after a
 *   re-registration that follows a bond recovery).
 * - `unknown`: none of these (never registered, or no evidence left).
 *
 * The state is a view of the facts at the set's view, and a restart reads
 * the same facts back to the same state: a node still removed shuts down
 * again. A removed operator recovers its bond and re-registers with
 * `--l1 kupmios|blockfrost`, since `--l1 node` reads this node's follower.
 */
import { Deferred, Effect, Ref } from "effect";

import { Globals } from "../services/globals.globals.js";
import {
  clearLivenessIncident,
  HaltSource,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import type { OperatorSet } from "./set.js";

export type OperatorMembershipState =
  | "unknown"
  | "active"
  | "awaiting_activation"
  | "removed";

/** The `/readyz` reason of a removed operator. */
export const OPERATOR_REMOVED = "operator_removed";

export type OperatorMembership = Readonly<{
  state: OperatorMembershipState;
  /** Why: what the set shows for this operator. */
  detail: string;
}>;

/** Where the evidence of a removal stands on the chain. */
export type RemovalPoint = Readonly<{
  slot: number;
  /** The heads module's depth of its block, and its level. */
  depth: number;
  level: string;
}>;

const atPoint = (point: RemovalPoint | null): string =>
  point === null
    ? ""
    : ` at slot ${point.slot.toString()} (depth ${point.depth.toString()}, ${point.level})`;

/**
 * This operator's membership in `set`. `recordedActivity`: an activation at
 * a final block recorded earlier (it outlives the facts' retention).
 * `removedAt`: where the removal evidence sits, for the detail.
 */
export const classifyOperatorMembership = (
  set: OperatorSet,
  recordedActivity: boolean,
  removedAt: RemovalPoint | null = null,
): OperatorMembership => {
  const key = set.ownKey;
  if (set.ownActivity.some((activity) => activity.spentSlot === null))
    return { state: "active", detail: `operator ${key} is active` };
  if (set.ownRetired !== null)
    return {
      state: "removed",
      detail: `operator ${key} is in the retired list${atPoint(removedAt)}; recover the bond and re-register with --l1 kupmios|blockfrost`,
    };
  // A registered node is keyed by its activation time and names its
  // operator in its datum.
  if (set.registered.some((node) => node.registered?.operator === key))
    return {
      state: "awaiting_activation",
      detail: `operator ${key} is registered, not yet active`,
    };
  if (set.ownActivity.length > 0 || recordedActivity)
    return {
      state: "removed",
      detail: `operator ${key} was active and is in no operator list${atPoint(removedAt)} (slashed or bond recovered); re-register with --l1 kupmios|blockfrost to return`,
    };
  return {
    state: "unknown",
    detail: `operator ${key} is in no operator list and was never seen active`,
  };
};

/**
 * Publishes a membership: removal raises `operator_removed` (duties held,
 * `/readyz` failing) and starts the shutdown; every other state clears it.
 */
export const publishOperatorMembership = (
  membership: OperatorMembership,
): Effect.Effect<void, never, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.set(globals.OPERATOR_MEMBERSHIP, membership.state);
    if (membership.state !== "removed")
      return yield* clearLivenessIncident(
        globals,
        HaltSource.operatorMembership,
      );
    yield* raiseLivenessIncident(
      globals,
      HaltSource.operatorMembership,
      OPERATOR_REMOVED,
      membership.detail,
    );
    if (yield* Deferred.succeed(globals.OPERATOR_REMOVED, undefined))
      yield* Effect.logError(
        `${OPERATOR_REMOVED}: ${membership.detail}; the node is shutting down`,
      );
  });

/**
 * Runs `node` until a published removal, then gives `/readyz` one second to
 * be read and ends it, so the node stops cleanly with exit code 0.
 */
export const untilOperatorRemoved = <A, E, R>(
  node: Effect.Effect<A, E, R>,
): Effect.Effect<void, E, R | Globals> =>
  Effect.flatMap(Globals, (globals) =>
    Effect.raceFirst(
      Effect.asVoid(node),
      Deferred.await(globals.OPERATOR_REMOVED).pipe(
        Effect.zipRight(Effect.sleep("1 second")),
      ),
    ),
  );
