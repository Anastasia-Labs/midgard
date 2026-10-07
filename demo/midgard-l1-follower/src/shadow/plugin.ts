import type { L1NodeTransport } from "@al-ft/l1-node-transport";

import type { FactStore } from "../store/fact-store.js";
import type { ShadowComparator, ShadowRole } from "./comparator.js";
import type { FollowerProjection } from "./projection.js";

/** What a soak plugin gets to build its comparators. */
export type ShadowEnv = Readonly<{
  transport: L1NodeTransport;
  store: FactStore;
  /** The soak directory, for any state the plugin keeps. */
  dir: string;
  /** The plugin's own `options` from `soak.json`. */
  options: unknown;
}>;

/**
 * A soak plugin module's default export. A role ticket (C1, W1, N1) ships
 * one: its projections join the soak's store, and its comparators run after
 * every event. The module that reads the old code is deleted at cutover.
 */
export type ShadowPlugin = Readonly<{
  role: Exclude<ShadowRole, "follower">;
  projections?: readonly FollowerProjection[];
  comparators: (
    env: ShadowEnv,
  ) => readonly ShadowComparator[] | Promise<readonly ShadowComparator[]>;
}>;

export const isShadowPlugin = (value: unknown): value is ShadowPlugin =>
  typeof value === "object" &&
  value !== null &&
  "role" in value &&
  (value.role === "committee" ||
    value.role === "watcher" ||
    value.role === "node") &&
  "comparators" in value &&
  typeof value.comparators === "function";
