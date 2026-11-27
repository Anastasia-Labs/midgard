/**
 * The node event projection as the follower plugs it in: tracked set, D-t
 * tables, schema and the derivation. The fork
 * simulator adds its own traffic and checks to this (tests only).
 */
import type { FollowerProjection } from "../projection.js";
import { type EventProjectionConfig, eventTrackedSet } from "./config.js";
import { eventDerivation } from "./derive.js";
import { EVENT_TABLES, eventSchema } from "./schema.js";

export const eventProjection = (
  config: EventProjectionConfig,
): FollowerProjection => ({
  name: "node_l1_events",
  trackedSet: eventTrackedSet(config),
  temporalTables: EVENT_TABLES,
  schema: eventSchema,
  derivations: [eventDerivation(config)],
});
