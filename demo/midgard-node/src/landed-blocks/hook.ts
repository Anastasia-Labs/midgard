/**
 * The driver's landed-block hook (plan §7.3, N3, N5): at the view each
 * driver run applies, the landed state queue (P1) is processed in order,
 * and what it cannot do yet is the hold it returns. An unhealthy queue is
 * P1's own hold (`state_queue_unhealthy`), so this hook waits for it
 * quietly. The run passes the follower's prune boundary to the fold prune,
 * then publishes where `confirmed_ledger` stands, with its level (P10).
 */
import type { DepthParameters, FactStore } from "@al-ft/midgard-l1-follower";
import { Effect } from "effect";

import type { DriverHook } from "../l1-events/driver.js";
import {
  readLandedStateQueueFrom,
  type StateQueueProjectionConfig,
} from "../l1-state-queue/index.js";
import type { Database } from "../services/database.js";
import type { LandedBlockPorts } from "./ports.js";
import {
  type ConfirmedLedgerPosition,
  frontierMerge,
  mergeLevel,
} from "./position.js";
import { processLandedQueue } from "./process.js";

export const landedBlockHook =
  <R>(options: {
    readonly store: Pick<FactStore, "dialect" | "transaction" | "cursor">;
    readonly config: StateQueueProjectionConfig;
    readonly ports: LandedBlockPorts<R>;
    readonly run: <A>(
      effect: Effect.Effect<A, never, R | Database>,
    ) => Promise<A>;
    /** Publishes where `confirmed_ledger` stands after each run. */
    readonly publish?: Readonly<{
      depth: DepthParameters;
      position: (position: ConfirmedLedgerPosition) => void;
    }>;
  }): DriverHook =>
  async (change) => {
    const read = await readLandedStateQueueFrom(
      options.store,
      options.config,
      change.view,
    );
    if (read.kind !== "ok" || !read.queue.healthy) return undefined;
    // A cursor read that fails only skips this run's fold prune.
    const cursor = await options.store.cursor().catch(() => null);
    const held = await options.run(
      processLandedQueue(
        options.ports,
        read.queue,
        cursor === null ? {} : { prunedThroughSlot: cursor.prunedThroughSlot },
      ),
    );
    const root = read.queue.root;
    if (options.publish !== undefined && root !== null) {
      const { depth, position } = options.publish;
      // Status only: a failed read keeps the last published position.
      const merge = await options.run(
        frontierMerge(root).pipe(Effect.orElseSucceed(() => undefined)),
      );
      if (merge !== undefined)
        position({
          ...merge,
          level: await mergeLevel(options.store, merge.mergeSlot, depth).catch(
            () => null,
          ),
        });
    }
    return held;
  };
