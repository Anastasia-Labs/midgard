/**
 * The node event projection as the follower plugs it in (plan §4.4, §15
 * N1): tracked set, D-t tables, migrations and the derivation. The fork
 * simulator adds its own traffic and checks to this (tests only).
 */
import type { FollowerProjection } from "../projection.js";
import { type EventProjectionConfig, eventTrackedSet } from "./config.js";
import { eventDerivation } from "./derive.js";
import { EVENT_TABLES, eventMigrations } from "./schema.js";

export const eventProjection = (
  config: EventProjectionConfig,
): FollowerProjection => ({
  name: "node_l1_events",
  trackedSet: eventTrackedSet(config),
  temporalTables: EVENT_TABLES,
  migrations: eventMigrations,
  derivations: [eventDerivation(config)],
});
