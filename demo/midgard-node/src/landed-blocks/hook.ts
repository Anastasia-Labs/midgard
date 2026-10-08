/**
 * The driver's landed-block hook (plan §7.3, N3): at the view each driver
 * run applies, the landed state queue (P1) is processed in order, and what
 * it cannot do yet is the hold it returns. An unhealthy queue is P1's own
 * hold (`state_queue_unhealthy`), so this hook waits for it quietly.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import type { Effect } from "effect";

import type { DriverHold, DriverHook } from "../l1-events/driver.js";
import {
  readLandedStateQueueFrom,
  type StateQueueProjectionConfig,
} from "../l1-state-queue/index.js";
import type { Database } from "../services/database.js";
import type { LandedBlockPorts } from "./ports.js";
import { processLandedQueue } from "./process.js";

export const landedBlockHook =
  <R>(options: {
    readonly store: Pick<FactStore, "dialect" | "transaction">;
    readonly config: StateQueueProjectionConfig;
    readonly ports: LandedBlockPorts<R>;
    readonly run: (
      effect: Effect.Effect<DriverHold | undefined, never, R | Database>,
    ) => Promise<DriverHold | undefined>;
  }): DriverHook =>
  async (change) => {
    const read = await readLandedStateQueueFrom(
      options.store,
      options.config,
      change.view,
    );
    if (read.kind !== "ok" || !read.queue.healthy) return undefined;
    return options.run(processLandedQueue(options.ports, read.queue));
  };
