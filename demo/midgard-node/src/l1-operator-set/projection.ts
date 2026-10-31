/**
 * The node's operator-set projection as the follower plugs it in (NC14):
 * its tracked set and, given this operator's activity record, the prune
 * step's part in keeping that record (N6-R5, `activity-prune.ts`). It keeps
 * no tables of its own: the operator-set hook reads the follower facts (the
 * live list nodes, then only the rows that changed since its last read), so
 * the set equals a fresh replay whenever the facts do.
 */
import type { FollowerProjection } from "@al-ft/midgard-l1-follower";

import {
  activityPruneFloor,
  activityPruneHook,
  type ActivityRecordBinding,
} from "./activity-prune.js";
import { type OperatorSetConfig, operatorSetTrackedSet } from "./config.js";

export const operatorSetProjection = (
  config: OperatorSetConfig,
  activity?: ActivityRecordBinding,
): FollowerProjection => ({
  name: "node_operator_set",
  trackedSet: operatorSetTrackedSet(config),
  ...(activity === undefined
    ? {}
    : {
        pruneHooks: [activityPruneHook(activity)],
        pruneFloor: activityPruneFloor(activity),
      }),
});
