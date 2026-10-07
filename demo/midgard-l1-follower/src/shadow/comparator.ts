import type { ChainSyncEvent } from "@al-ft/l1-node-transport";

import type { FactStore } from "../store/fact-store.js";
import type { Point } from "../types.js";

/**
 * The roles that run the shadow diff (plan §14). `follower` is for
 * comparators of the follower's own facts (the ledger stub).
 */
export const SHADOW_ROLES = ["committee", "watcher", "node"] as const;
export type ShadowRole = (typeof SHADOW_ROLES)[number] | "follower";

/** Where the store stands when a comparison runs. */
export type ShadowContext = Readonly<{
  store: FactStore;
  /** The store's cursor: the block both sides are read at. */
  at: Readonly<{ point: Point; height: number; generation: number }>;
  /**
   * The event just applied; absent for a comparison at the cursor after a
   * restart (the soak's resume record).
   */
  event?: ChainSyncEvent;
}>;

/** One side's view: a JSON-like value, or why it cannot be read now. */
export type ShadowReading =
  | Readonly<{ kind: "value"; value: unknown }>
  | Readonly<{ kind: "unavailable"; reason: string }>;

export const reading = (value: unknown): ShadowReading => ({
  kind: "value",
  value,
});

export const unavailable = (reason: string): ShadowReading => ({
  kind: "unavailable",
  reason,
});

/**
 * One per-block comparison of a role's new projection (read from the
 * follower's store) with the current code's view of the same thing.
 *
 * A role ticket (C1, W1, N1) supplies its comparators. Each one that reads
 * the old code is development tooling: it lives in its own file and is
 * deleted with the old code at that role's cutover (§14).
 */
export type ShadowComparator = Readonly<{
  role: ShadowRole;
  /** Unique within its role; the journal keys results by `role/name`. */
  name: string;
  /** Called for every event before the comparison (feed the old code here). */
  observe?: (context: ShadowContext) => Promise<void>;
  /** The new projection's view at `context.at`. */
  projected: (context: ShadowContext) => Promise<ShadowReading>;
  /** The current code's view at the same block. */
  current: (context: ShadowContext) => Promise<ShadowReading>;
  close?: () => Promise<void>;
}>;
