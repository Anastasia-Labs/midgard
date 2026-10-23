/**
 * A pass that fails because a replay local fault outlived its bound
 * (`WatcherReplayLocalFaultExhaustedError`, from the bridge's classification
 * or the replayed ledger's advance) rejects the driver's `exhausted` with it,
 * which ends the watcher. Any other failed pass stays a readiness reason and
 * leaves `exhausted` pending.
 */
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { afterEach, describe, expect, it } from "vitest";

import { WatcherReplayLocalFaultExhaustedError } from "../../src/fault-proofs/replay-local-fault-bound.js";
import {
  createWatcherDecisionDriver,
  WATCHER_DECISION_PASS_FAILED,
} from "../../src/runtime/watcher-runtime.decision-driver.js";
import {
  applyAll,
  AUTHORITY,
  chainEvents,
  collaborators,
  driverOver,
  openStore,
  RELEASE_DEPTH,
  SOURCE_ID,
} from "../support/l1-follower-store-reset.js";

const PENDING = Symbol("pending");

/** The promise's outcome, or PENDING once queued work has drained. */
const settled = async (promise: Promise<unknown>) =>
  await Promise.race([
    promise.then(
      () => "resolved",
      () => "rejected",
    ),
    new Promise<typeof PENDING>((resolve) =>
      setTimeout(() => resolve(PENDING), 20),
    ),
  ]);

const exhaustion = () =>
  new WatcherReplayLocalFaultExhaustedError(
    "ab".repeat(28),
    "the script evaluator failed locally",
  );

const closers: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});

const store = async () => {
  const opened = openStore(":memory:");
  closers.push(() => opened.close());
  expect((await opened.start()).kind).toBe("ready");
  expect((await opened.initialize(SIM_ORIGIN)).kind).toBe("initialized");
  await applyAll(opened, chainEvents(8));
  return opened;
};

describe("the decision driver's local-fault exhaustion", () => {
  it("rejects exhausted with the bridge's exhaustion, and not on another failed pass", async () => {
    const c = collaborators();
    let failure: Error = new Error("the classifier refused the header");
    c.bridge.reconcileAndDispatch = () => Promise.reject(failure);
    const driver = driverOver(await store(), c, []);
    closers.push(() => driver.close());
    driver.wake();
    await driver.idle();
    expect(driver.readiness()).toMatchObject([
      { reason: WATCHER_DECISION_PASS_FAILED },
    ]);
    expect(await settled(driver.exhausted)).toBe(PENDING);

    failure = exhaustion();
    driver.wake();
    await driver.idle();
    expect(await settled(driver.exhausted)).toBe("rejected");
    await expect(driver.exhausted).rejects.toBe(failure);
  });

  it("rejects exhausted with the replayed ledger's exhaustion", async () => {
    const c = collaborators();
    const failure = exhaustion();
    const driver = createWatcherDecisionDriver(
      {
        store: await store(),
        onFollowerChange: () => () => undefined,
        authority: AUTHORITY,
        sourceId: SOURCE_ID,
        releaseDepth: RELEASE_DEPTH,
        bridge: c.bridge as never,
        availability: c.availability as never,
        replayedLedger: {
          depth: RELEASE_DEPTH,
          advance: () => Promise.reject(failure),
        },
        retryDelayMs: 10,
      },
      { atTip: () => true, started: () => true },
    );
    closers.push(() => driver.close());
    driver.wake();
    await driver.idle();
    expect(await settled(driver.exhausted)).toBe("rejected");
    await expect(driver.exhausted).rejects.toBe(failure);
    expect(c.seen.dispatched).toEqual([]);
  });
});
