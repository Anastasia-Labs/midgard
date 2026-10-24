/**
 * The node's landed-block processing across a follower restart, against the
 * deployment's recorded origin T (the manifest's `l1Origin`). The node runs
 * the follower from its plan's origin and starts landed-block processing
 * from the follower's statuses (`landedBlockTrigger`). A follower whose
 * store origin is after T reports R3 (`origin_after_protocol_init`) from its
 * first status after the restart, so processing never starts; at T it
 * starts once the follower reaches the tip.
 */
import type { ChainSyncEvent, ChainSyncStream } from "@al-ft/l1-node-transport";
import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import {
  followChain,
  type FollowStatus,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  simProtocolInitChain,
  simStoreOptions,
} from "@al-ft/midgard-l1-follower/testing";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { landedBlockTrigger } from "../src/services/l1-follower.readiness.js";
import { nodeFollowerPlanFor } from "./helpers/node-follower-plan.js";

const R3 = "origin_after_protocol_init";

const pointOf = (
  point: Readonly<{ slot: number; hash: Buffer }>,
): L1Origin => ({
  slot: point.slot,
  blockHash: point.hash.toString("hex"),
});

/**
 * A node transport serving `events` below `served.limit`, every stream
 * resuming after the last acknowledged event.
 */
const transport = (
  events: readonly ChainSyncEvent[],
  served: { limit: number; acked: number },
) => ({
  readiness: { ready: true as const, nodeToClientVersion: 32784 },
  onReadiness: (): (() => void) => () => undefined,
  openChainSync: (): ChainSyncStream => {
    let position = served.acked;
    let closed = false;
    return {
      opened: Promise.resolve(),
      next: async () => {
        for (;;) {
          if (closed) return undefined;
          if (position < Math.min(served.limit, events.length)) {
            position += 1;
            return events[position - 1];
          }
          await new Promise((resolve) => setTimeout(resolve, 2));
        }
      },
      ack: (seq: bigint) => {
        const index = events.findIndex((event) => event.seq === seq);
        if (index >= 0) served.acked = Math.max(served.acked, index + 1);
      },
      close: () => {
        closed = true;
        return Promise.resolve();
      },
    } as unknown as ChainSyncStream;
  },
});

/**
 * Follows the chain one block short of the tip, then restarts the follower
 * on the same store with the node's landed-block trigger, until it reaches
 * the tip and has checked the protocol-init spend; returns the restarted
 * follower's statuses and how often landed-block processing started.
 */
const restartedNode = async (recordedOrigin: L1Origin) => {
  const { events, hubOracleOneShot } = simProtocolInitChain();
  const plan = await Effect.runPromise(
    nodeFollowerPlanFor({
      l1Origin: pointOf(SIM_ORIGIN.point),
      recordedOrigin,
      hubOracleOneShot,
    }),
  );
  if (plan.kind !== "run")
    throw new Error(`the node's follower does not run: ${plan.detail}`);
  const store = openSqliteFactStore({
    ...simStoreOptions([], 6, "sqlite"),
    path: ":memory:",
  });
  const served = { limit: events.length - 1, acked: 0 };
  const run = async (
    until: (status: FollowStatus) => boolean,
    onStatus: (status: FollowStatus) => void,
  ): Promise<readonly FollowStatus[]> => {
    const abort = new AbortController();
    const statuses: FollowStatus[] = [];
    const running = followChain({
      store,
      transport: transport(events, served),
      origin: plan.origin,
      signal: abort.signal,
      log: () => undefined,
      onStatus: (status) => {
        statuses.push(status);
        onStatus(status);
        if (until(status)) abort.abort();
      },
    });
    const deadline = Date.now() + 8_000;
    while (!abort.signal.aborted && Date.now() < deadline)
      await new Promise((resolve) => setTimeout(resolve, 2));
    abort.abort();
    await running;
    return statuses;
  };
  try {
    await run(
      (status) => status.events >= events.length - 1,
      () => undefined,
    );
    served.limit = events.length;
    let replays = 0;
    const statuses = await run(
      (status) =>
        status.atTip && status.events >= 1 && status.protocolInit !== "unknown",
      landedBlockTrigger(() => {
        replays += 1;
      }),
    );
    return { statuses, replays };
  } finally {
    await store.close();
  }
};

describe("the node's landed-block processing after a follower restart", () => {
  it("N-d: does not start while the follower's store origin is after the recorded origin, whose R3 the first status already reports", async () => {
    const { statuses, replays } = await restartedNode({
      slot: SIM_ORIGIN.point.slot - 1,
      blockHash: "9f".repeat(32),
    });
    expect(statuses[0]!.interventions.map((i) => i.reason)).toEqual([R3]);
    expect(statuses.some((status) => status.atTip)).toBe(true);
    expect(replays).toBe(0);
  });

  it("N-d: starts once the follower reaches the tip when its store origin is the recorded origin", async () => {
    const { statuses, replays } = await restartedNode(
      pointOf(SIM_ORIGIN.point),
    );
    expect(statuses.at(-1)).toMatchObject({ atTip: true, interventions: [] });
    expect(replays).toBeGreaterThanOrEqual(1);
  });
});
