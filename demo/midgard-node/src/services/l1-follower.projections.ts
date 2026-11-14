/**
 * The node's follower projections, in the order the prune step runs their
 * hooks. Building them reads nothing: their tracked sets, tables and
 * derivations come from the plan alone, so a process with no node database
 * runtime (a reader, a command) builds the same list. The two runtime
 * inputs are bound by the store's writer once it has them:
 *
 * - the operator's activity record, once its key is resolved (after the
 *   store opens): the prune step keeps it too (N6-R5);
 * - the landed frontier's reads (the node database and the slot clock):
 *   until bound, its prune floor keeps every fact.
 */
import {
  type FollowerProjection,
  intentJournalProjection,
} from "@al-ft/midgard-l1-follower";
import { eventProjection } from "@al-ft/midgard-l1-follower/events";
import type { Effect } from "effect";

import { forcedOrderProjection } from "../forced-orders/index.js";
import {
  type OperatorActivityRecord,
  operatorSetProjection,
} from "../l1-operator-set/index.js";
import { queueTerminalProjection } from "../l1-queue-terminals/index.js";
import { landedStateQueueProjection } from "../landed-blocks/index.js";
import type { Database } from "./database.js";
import type { NodeIndexPlan } from "./l1-follower.plan.js";
import { settlementProjection } from "./settlement.final-hook.js";

/** The plan fields the projections read. */
export type NodeProjectionPlan = Pick<
  NodeIndexPlan,
  "projection" | "stateQueue" | "operatorSet" | "forcedOrders"
>;

/** What the landed frontier's prune floor reads while the store prunes. */
export type NodeProjectionRuntime = Readonly<{
  run: <A, E>(effect: Effect.Effect<A, E, Database>) => Promise<A>;
  /** The slot holding a POSIX time, as the landed-block replay maps it. */
  slotOf: (unixTimeMs: number) => number;
}>;

export type NodeFollowerProjections = Readonly<{
  projections: readonly FollowerProjection[];
  bindActivity: (record: OperatorActivityRecord | undefined) => void;
  bindRuntime: (runtime: NodeProjectionRuntime) => void;
}>;

const UNBOUND = "the landed frontier's runtime is not bound";

export const nodeFollowerProjections = (
  plan: NodeProjectionPlan,
): NodeFollowerProjections => {
  const activity: { record?: OperatorActivityRecord } = {};
  let runtime: NodeProjectionRuntime | undefined;
  const projections: FollowerProjection[] = [
    eventProjection(plan.projection),
    // A failed read or a throw holds the prune back (`landedFrontierPruneFloor`).
    landedStateQueueProjection(
      plan.stateQueue,
      (effect) =>
        runtime === undefined
          ? Promise.reject(new Error(UNBOUND))
          : runtime.run(effect),
      (ms) => {
        if (runtime === undefined) throw new Error(UNBOUND);
        return runtime.slotOf(ms);
      },
    ),
    queueTerminalProjection(plan.stateQueue),
    operatorSetProjection(plan.operatorSet, () => activity.record),
    forcedOrderProjection(plan.forcedOrders),
    // Stores a settlement attempt's `final` in the prune step that prunes
    // its journal entry (D-N4).
    settlementProjection,
    intentJournalProjection,
  ];
  return {
    projections,
    bindActivity: (record) => {
      activity.record = record;
    },
    bindRuntime: (bound) => {
      runtime = bound;
    },
  };
};
