/**
 * The queue-terminal projection as the follower plugs it in (plan §5.5 P8,
 * N4): the state queue's tracked set, the D-t table, its migrations and the
 * derivation.
 */
import type { FollowerProjection } from "@al-ft/midgard-l1-follower";

import {
  type StateQueueProjectionConfig,
  stateQueueTrackedSet,
} from "../l1-state-queue/config.js";
import { queueTerminalDerivation } from "./derive.js";
import { QUEUE_TERMINAL_TABLES, queueTerminalMigrations } from "./schema.js";

export const queueTerminalProjection = (
  config: StateQueueProjectionConfig,
): FollowerProjection => ({
  name: "node_l1_queue_terminals",
  trackedSet: stateQueueTrackedSet(config),
  temporalTables: QUEUE_TERMINAL_TABLES,
  migrations: queueTerminalMigrations,
  derivations: [queueTerminalDerivation(config)],
});
