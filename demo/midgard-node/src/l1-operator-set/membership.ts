/**
 * This operator's membership (D-N7, plan §7.5 R7), from the operator set.
 *
 * - `active`: its active node is live.
 * - `removed`: its retired node is live, or it is in no list after having
 *   been active (its active node in the follower's retained facts, or a
 *   recorded activation at a final block). Removal is the `/readyz` reason
 *   `operator_removed` under `HaltSource.operatorMembership`, which holds
 *   every operator duty (`FIBER_HALT_SOURCES`). The process stays up.
 * - `awaiting_activation`: registered, not yet active (also after a
 *   re-registration that follows a bond recovery).
 * - `unknown`: none of these (never registered, or no evidence left).
 *
 * The state is a view of the facts at the set's view: a rollback that undoes
 * the removal makes the next read `active` again and clears the reason, and
 * a restart reads the same facts back to the same state. Nothing is sticky
 * and nothing exits.
 */
import { Effect, Ref } from "effect";

import { Globals } from "../services/globals.js";
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

/** The `/readyz` reason of a removed operator (plan §7.5 R7). */
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
      detail: `operator ${key} is in the retired list${atPoint(removedAt)}; recover the bond and re-register, or shut the node down`,
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
      detail: `operator ${key} was active and is in no operator list${atPoint(removedAt)} (slashed or bond recovered); re-register to return`,
    };
  return {
    state: "unknown",
    detail: `operator ${key} is in no operator list and was never seen active`,
  };
};

/**
 * Publishes a membership: removal raises `operator_removed` (duties held,
 * `/readyz` failing, the process up); every other state clears it.
 */
export const publishOperatorMembership = (
  membership: OperatorMembership,
): Effect.Effect<void, never, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.set(globals.OPERATOR_MEMBERSHIP, membership.state);
    if (membership.state === "removed")
      yield* raiseLivenessIncident(
        globals,
        HaltSource.operatorMembership,
        OPERATOR_REMOVED,
        membership.detail,
      );
    else yield* clearLivenessIncident(globals, HaltSource.operatorMembership);
  });
