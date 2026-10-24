/**
 * The node's state-queue projection as the follower plugs it in (plan §5.5
 * P1, N2): only its tracked set. P1 keeps no tables: it is read from the
 * follower facts at a view (`landedStateQueueIn`), so it equals a fresh
 * replay whenever the facts do. The fork simulator adds traffic and checks
 * to this (tests only).
 */
import type { FollowerProjection } from "@al-ft/midgard-l1-follower";

import {
  type StateQueueProjectionConfig,
  stateQueueTrackedSet,
} from "./config.js";

export const stateQueueProjection = (
  config: StateQueueProjectionConfig,
): FollowerProjection => ({
  name: "node_state_queue",
  trackedSet: stateQueueTrackedSet(config),
});
