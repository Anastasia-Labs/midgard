/**
 * Replay-transcript retirement is gated on an idle supervisor: the driver
 * retires at the release-final observation only while
 * `watcherRetirementReady` holds, and resets the retirement witnesses
 * otherwise.
 */
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { afterEach, describe, expect, it } from "vitest";

import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import {
  createWatcherDecisionDriver,
  type WatcherDecisionDriver,
} from "../../src/runtime/watcher-runtime.decision-driver.js";
import { watcherRetirementReady } from "../../src/runtime/watcher-runtime.retirement-ready.js";
import {
  applyAll,
  AUTHORITY,
  chainEvents,
  collaborators,
  openStore,
  RELEASE_DEPTH,
  SOURCE_ID,
  until,
} from "../support/l1-follower-store-reset.js";

const IDLE = {
  recovered: true,
  phase: "accepting",
  unfinishedObjectiveCount: 0,
  queuedJobCount: 0,
  activeJob: null,
  blockedJob: null,
} as const;

describe("watcherRetirementReady", () => {
  it("holds for a recovered, accepting supervisor with no work", () => {
    expect(watcherRetirementReady(IDLE)).toBe(true);
  });

  it.each([
    ["not recovered", { recovered: false }],
    ["not accepting", { phase: "closing" }],
    ["an unfinished objective", { unfinishedObjectiveCount: 1 }],
    ["a queued job", { queuedJobCount: 1 }],
    ["an active job", { activeJob: {} }],
    ["a blocked job", { blockedJob: {} }],
  ])("fails with %s", (_label, change) => {
    expect(
      watcherRetirementReady({ ...IDLE, ...change } as unknown as Parameters<
        typeof watcherRetirementReady
      >[0]),
    ).toBe(false);
  });
});

const closers: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});

describe("the decision driver's retirement gate", () => {
  it("retires at the release-final observation only while retirement is ready, and resets otherwise", async () => {
    const store = openStore(":memory:");
    closers.push(() => store.close());
    expect((await store.start()).kind).toBe("ready");
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(store, chainEvents(8));
    const c = collaborators();
    let ready = false;
    let resets = 0;
    const retired: WatcherAuthenticatedStateQueueObservation[] = [];
    const driver: WatcherDecisionDriver = createWatcherDecisionDriver(
      {
        store,
        onFollowerChange: () => () => undefined,
        authority: AUTHORITY,
        sourceId: SOURCE_ID,
        releaseDepth: RELEASE_DEPTH,
        bridge: c.bridge as never,
        availability: c.availability as never,
        retirement: {
          ready: () => ready,
          retire: (observation) => {
            retired.push(observation);
            return Promise.resolve();
          },
          reset: () => {
            resets += 1;
            return Promise.resolve();
          },
        },
        retryDelayMs: 10,
      },
      { atTip: () => true, started: () => true },
    );
    closers.push(() => driver.close());
    const pass = async () => {
      const dispatched = c.seen.dispatched.length;
      driver.wake();
      await until(
        "a decision",
        () =>
          c.seen.dispatched.length > dispatched &&
          driver.readiness().length === 0,
      );
      await driver.idle();
    };

    await pass();
    expect(retired).toEqual([]);
    expect(resets).toBeGreaterThan(0);

    ready = true;
    const resetsBefore = resets;
    await pass();
    expect(resets).toBe(resetsBefore);
    expect(retired).toHaveLength(1);
    // The release-final observation: RELEASE_DEPTH deep, the tip being 1.
    expect(BigInt(retired[0]!.nativePoint.blockNo)).toBe(
      BigInt(driver.current().nativePoint.blockNo) - BigInt(RELEASE_DEPTH - 1),
    );
  });
});
