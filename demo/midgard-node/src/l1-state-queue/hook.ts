/**
 * The follower-change driver's state-queue hook (plan §4.1, §5.5 P1, N2).
 * It runs first on every driver run, reads the landed queue at the view the
 * driver is applying, hands the read to the node (queue length and the head
 * signal that triggers the planner fibers) and holds `/readyz` on
 * `state_queue_unhealthy` while the queue is unhealthy. It never throws for
 * an unhealthy queue and never stops the process: reads keep serving.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";

import type {
  DriverHold,
  DriverHook,
  FollowerChange,
} from "../l1-events/driver.js";
import type { StateQueueProjectionConfig } from "./config.js";
import {
  formatLandedStateQueue,
  landedStateQueueIn,
  type LandedStateQueueRead,
} from "./landed.js";

/** The `/readyz` reason of an unhealthy landed queue (the detail names why). */
export const STATE_QUEUE_UNHEALTHY = "state_queue_unhealthy";

/** The landed queue at the store's view `at` (the current one by default). */
export const readLandedStateQueueFrom = (
  store: Pick<FactStore, "dialect" | "transaction">,
  config: StateQueueProjectionConfig,
  at?: FollowerChange["view"],
): Promise<LandedStateQueueRead> =>
  // The view check takes FOR SHARE on Postgres: a read-write transaction.
  store.transaction("write", (tx) =>
    landedStateQueueIn(tx, store.dialect, config, at),
  );

/** The hold a read leaves, if any. */
export const landedStateQueueHold = (
  read: LandedStateQueueRead,
): DriverHold | undefined =>
  read.kind === "ok" && !read.queue.healthy
    ? {
        reason: STATE_QUEUE_UNHEALTHY,
        detail: formatLandedStateQueue(read.queue),
      }
    : // A refused read means the follower moved off the view while the
      // driver ran: the move triggers the next run, which reads again.
      undefined;

export const landedStateQueueHook =
  (options: {
    readonly store: Pick<FactStore, "dialect" | "transaction">;
    readonly config: StateQueueProjectionConfig;
    /** Publishes the read (queue length, head signal); runs before the hold is returned. */
    readonly publish: (
      change: FollowerChange,
      read: LandedStateQueueRead,
    ) => Promise<void>;
  }): DriverHook =>
  async (change) => {
    const read = await readLandedStateQueueFrom(
      options.store,
      options.config,
      change.view,
    );
    await options.publish(change, read);
    return landedStateQueueHold(read);
  };
