/**
 * A recovery preparation that a later header's classification failed still
 * prepared the decided prefix of the tip it read: the same pass reconciles
 * and dispatches that tip, and the pass is named failed with the header's
 * failure. A preparation that prepared nothing is owed again, as before.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { WATCHER_DECISION_PASS_FAILED } from "../../src/runtime/watcher-runtime.decision-driver.js";
import {
  applyAll,
  chainEvents,
  collaborators,
  driverOver,
  openStore,
  until,
} from "../support/l1-follower-store-reset.js";

const scratch = mkdtempSync(join(tmpdir(), "watcher-partial-recovery-"));
const opened: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of opened.splice(0).reverse()) await close();
});
afterAll(() => {
  rmSync(scratch, { recursive: true, force: true });
});

/**
 * Stub collaborators whose bridge fails every classification pass on a later
 * header; `preparesPrefix` says whether its recovery preparation still
 * prepared the tip it was handed, as the bridge does for a decided prefix.
 */
const failingBridge = (preparesPrefix: boolean) => {
  const c = collaborators();
  const failure = new Error("replay of a later block threw");
  let prepared: string | null = null;
  const bridge = {
    ...c.bridge,
    prepareForRecovery: (observation: { observationDigest: string }) => {
      c.seen.recoveryPreparations += 1;
      if (preparesPrefix) prepared = observation.observationDigest;
      return Promise.reject(failure);
    },
    reconcileAndDispatch: (observation: { observationDigest: string }) => {
      c.seen.dispatched.push(observation.observationDigest);
      prepared = observation.observationDigest;
      return Promise.reject(failure);
    },
    status: () => ({ observationDigest: prepared, target: null }),
  };
  return { ...c, bridge, failure };
};

const run = async (preparesPrefix: boolean, name: string) => {
  const store = openStore(join(scratch, `${name}.db`));
  opened.push(() => store.close());
  expect((await store.start()).kind).toBe("ready");
  expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
  await applyAll(store, chainEvents(6));
  const c = failingBridge(preparesPrefix);
  const driver = driverOver(store, c, []);
  opened.push(() => driver.close());
  const pass = async () => {
    const before = driver.status().passes;
    driver.wake();
    await until("a pass", () => driver.status().passes > before);
    await driver.idle();
  };
  return { c, driver, pass };
};

describe("the decision driver after a partial recovery preparation", () => {
  it("dispatches the prepared prefix in the same pass and names the failure", async () => {
    const { c, driver, pass } = await run(true, "prefix");
    await pass();
    expect(c.seen.recoveryPreparations).toBe(1);
    expect(c.seen.dispatched).toHaveLength(1);
    expect(driver.readiness()).toEqual([
      {
        reason: WATCHER_DECISION_PASS_FAILED,
        detail: expect.stringContaining(c.failure.message),
      },
    ]);

    // Recovery is not owed again: the next pass reconciles and dispatches.
    await pass();
    expect(c.seen.recoveryPreparations).toBe(1);
    expect(c.seen.dispatched).toHaveLength(2);
  });

  it("owes recovery again when the preparation prepared nothing", async () => {
    const { c, driver, pass } = await run(false, "nothing");
    await pass();
    expect(c.seen.recoveryPreparations).toBe(1);
    expect(c.seen.dispatched).toEqual([]);
    expect(driver.readiness()).toEqual([
      {
        reason: WATCHER_DECISION_PASS_FAILED,
        detail: expect.stringContaining(c.failure.message),
      },
    ]);
    await pass();
    expect(c.seen.recoveryPreparations).toBe(2);
    expect(c.seen.dispatched).toEqual([]);
  });
});
