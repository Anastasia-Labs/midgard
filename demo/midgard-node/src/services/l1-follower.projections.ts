/**
 * The node's follower projections, in the order the prune step runs their
 * hooks. The operator's activity record is bound once its key is resolved
 * (after the store opens): the prune step keeps it too (N6-R5).
 */
import {
  type FollowerProjection,
  intentJournalProjection,
} from "@al-ft/midgard-l1-follower";
import type { Effect } from "effect";

import { forcedOrderProjection } from "../forced-orders/index.js";
import { eventProjection } from "../l1-events/projection.js";
import {
  type OperatorActivityRecord,
  operatorSetProjection,
} from "../l1-operator-set/index.js";
import { landedStateQueueProjection } from "../landed-blocks/index.js";
import type { Database } from "./database.js";
import type { L1FollowerPlan } from "./l1-follower.plan.js";
import { settlementProjection } from "./settlement.final-hook.js";

export const nodeFollowerProjections = (
  plan: Extract<L1FollowerPlan, { kind: "run" }>,
  run: <A, E>(effect: Effect.Effect<A, E, Database>) => Promise<A>,
) => {
  const activity: { record?: OperatorActivityRecord } = {};
  const projections: FollowerProjection[] = [
    eventProjection(plan.projection),
    landedStateQueueProjection(plan.stateQueue, run),
    operatorSetProjection(plan.operatorSet, () => activity.record),
    forcedOrderProjection(plan.forcedOrders),
    // Stores a settlement attempt's `final` in the prune step that prunes
    // its journal entry (D-N4).
    settlementProjection,
    intentJournalProjection,
  ];
  return {
    projections,
    bindActivity: (record: OperatorActivityRecord | undefined) => {
      activity.record = record;
    },
  };
};
