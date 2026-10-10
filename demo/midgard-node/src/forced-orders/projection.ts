/**
 * The forced-order projection as the follower plugs it in (plan §12, N10):
 * tracked set, D-t table, migrations and the derivation.
 */
import type { FollowerProjection } from "@al-ft/midgard-l1-follower";

import { type ForcedOrderConfig, forcedOrderTrackedSet } from "./config.js";
import { forcedOrderDerivation } from "./derive.js";
import { FORCED_ORDER_TABLES, forcedOrderMigrations } from "./schema.js";

export const forcedOrderProjection = (
  config: ForcedOrderConfig,
): FollowerProjection => ({
  name: "node_l1_forced_orders",
  trackedSet: forcedOrderTrackedSet(config),
  temporalTables: FORCED_ORDER_TABLES,
  migrations: forcedOrderMigrations,
  derivations: [forcedOrderDerivation(config)],
});
