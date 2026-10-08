/**
 * The node's operator-set projection as the follower plugs it in (NC14):
 * only its tracked set. It keeps no tables of its own: the operator-set
 * hook reads the follower facts (the live list nodes, then only the rows
 * that changed since its last read), so the set equals a fresh replay
 * whenever the facts do.
 */
import type { FollowerProjection } from "@al-ft/midgard-l1-follower";

import { type OperatorSetConfig, operatorSetTrackedSet } from "./config.js";

export const operatorSetProjection = (
  config: OperatorSetConfig,
): FollowerProjection => ({
  name: "node_operator_set",
  trackedSet: operatorSetTrackedSet(config),
});
