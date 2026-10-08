/**
 * The decision driver over a store reset (lane SI, ruling SI-R2; lane
 * SI-fix, ruling SIFIX-R1): the start that resets the store tells the
 * driver as a rewind to the origin; a driver that was not subscribed (a
 * reset at a start before it subscribed, a rewind its process stopped
 * before handling) pulls it from the rollback log by generation, once; and
 * the driver makes no pass until the follower in this process has finished
 * its store start. The tx-input sweep's side is in
 * `tracked-set-reset-inputs.test.ts`.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  openSqliteFactStore,
  projectionStoreOptions,
} from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { openWatcherFollowerRuntime } from "../../src/l1-follower/follower-runtime.js";
import { watcherProjection } from "../../src/l1-follower/projection.js";
import {
  createWatcherDecisionDriver,
  WATCHER_FOLLOWER_NOT_STARTED,
  watcherFollowerStarted,
} from "../../src/runtime/watcher-runtime.decision-driver.js";
import { SIM_HUB_ORACLE_ONE_SHOT } from "../support/l1-follower-state-queue-traffic.js";
import {
  applyAll,
  AUTHORITY,
  chainEvents,
  collaborators,
  D,
  decideOnce,
  driverOver,
  dropRecord,
  K,
  openStore,
  RECOVERY_DEPTH,
  RELEASE_DEPTH,
  scriptedTransport,
  SOURCE_ID,
  until,
} from "../support/l1-follower-store-reset.js";

const scratch = mkdtempSync(join(tmpdir(), "watcher-tracked-set-reset-"));
const opened: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of opened.splice(0).reverse()) await close();
});
afterAll(() => {
  rmSync(scratch, { recursive: true, force: true });
});

describe("the decision driver over a tracked-set reset", () => {
  it("hears the reset as a rewind to the origin: invalidates and re-arms recovery", async () => {
    const path = join(scratch, "driver-reset.db");
    const store = openStore(path);
    const c = collaborators();
    const rewound: number[] = [];
    const driver = driverOver(store, c, rewound);
    opened.push(async () => {
      await driver.close();
      await store.close();
    });
    const events = chainEvents(6);
    expect((await store.start()).kind).toBe("ready");
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(store, events);
    driver.wake();
    await until("the first decision", () => driver.readiness().length === 0);
    await driver.idle();
    const before = driver.current().observationDigest;
    expect(c.seen.recoveryPreparations).toBe(1);

    // A start that finds the store unrecorded resets it.
    await dropRecord(store);
    expect(await store.start()).toMatchObject({
      kind: "ready",
      cursor: null,
      trackedSet: { kind: "reset", cause: "unrecorded" },
      replaying: true,
    });
    expect(c.seen.bridgeInvalidations).toBe(1);
    expect(c.seen.availabilityInvalidations).toBe(1);
    expect(driver.status().rewinds).toBe(1);
    expect(rewound).toEqual([1]);
    expect(driver.inclusion()).toBeNull();

    // The replay from the origin: the next pass re-prepares recovery, and
    // the decision is the one before.
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(store, events);
    driver.wake();
    await until(
      "the decision after the replay",
      () => c.seen.recoveryPreparations === 2,
    );
    await driver.idle();
    expect(driver.readiness()).toEqual([]);
    expect(driver.current().observationDigest).toBe(before);
    expect(c.seen.dispatched.at(-1)).toBe(before);
    // Heard pushed, then found again by the pull: handled once.
    expect(driver.status().rewinds).toBe(1);
    expect(rewound).toEqual([1]);
    expect(c.seen.bridgeInvalidations).toBe(1);
  });

  it("pulls a reset made at a start before it subscribed: raises one rewind, and a later driver raises none", async () => {
    const path = join(scratch, "driver-reset-before-subscribe.db");
    const c = collaborators();
    const rewound: number[] = [];
    const events = chainEvents(6);
    // The process before: a driver decided at the tip, then the process stopped.
    const first = openStore(path);
    expect((await first.start()).kind).toBe("ready");
    expect((await first.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(first, events);
    expect((await decideOnce(first, c, rewound)).rewinds).toBe(0);
    await dropRecord(first);
    await first.close();

    // The next process: the follower's start resets the store before the
    // driver exists (the production order), then replays.
    const store = openStore(path);
    opened.push(() => store.close());
    expect(await store.start()).toMatchObject({
      kind: "ready",
      trackedSet: { kind: "reset", cause: "unrecorded" },
      replaying: true,
    });
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(store, events);
    const status = await decideOnce(store, c, rewound);
    expect(status.rewinds).toBe(1);
    expect(rewound).toEqual([1]);
    expect(c.seen.bridgeInvalidations).toBe(1);
    expect(c.seen.availabilityInvalidations).toBe(1);

    // Handled durably: the next process's driver raises none.
    expect((await decideOnce(store, c, rewound)).rewinds).toBe(0);
    expect(rewound).toEqual([1]);
    expect(c.seen.bridgeInvalidations).toBe(1);
  });

  it("pulls a rewind its process stopped before handling, once, and a later driver raises none", async () => {
    const path = join(scratch, "driver-rewind-unhandled.db");
    const c = collaborators();
    const rewound: number[] = [];
    const events = chainEvents(6);
    const store = openStore(path);
    opened.push(() => store.close());
    expect((await store.start()).kind).toBe("ready");
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(store, events.slice(0, 4));
    const target = (await store.cursor())!.point;
    await applyAll(store, events.slice(4));
    expect((await decideOnce(store, c, rewound)).rewinds).toBe(0);

    // A rewind with no driver subscribed: the process stopped before a
    // driver handled it.
    expect((await store.rewind(target)).kind).toBe("rewound");
    const generation = (await store.cursor())!.generation;

    const status = await decideOnce(store, c, rewound);
    expect(status.rewinds).toBe(1);
    expect(rewound).toEqual([generation]);
    expect(c.seen.bridgeInvalidations).toBe(1);
    expect((await decideOnce(store, c, rewound)).rewinds).toBe(0);
    expect(rewound).toEqual([generation]);
    expect(c.seen.bridgeInvalidations).toBe(1);
  });

  it("makes no pass while the follower's start is held on a store with a cursor, and passes once it completes", async () => {
    const path = join(scratch, "driver-locked.db");
    const events = chainEvents(6);
    const held = 4;
    // Another process holds the writer lease of a store that has a cursor.
    const holder = openSqliteFactStore({
      ...projectionStoreOptions(
        [watcherProjection(D)],
        {
          securityParameter: K,
          trackedSet: {
            addresses: new Set(),
            paymentCredentials: new Set(),
            policies: new Set(),
          },
        },
        "sqlite",
      ),
      path,
    });
    let holderOpen = true;
    opened.push(async () => {
      if (holderOpen) await holder.close();
    });
    expect((await holder.start()).kind).toBe("ready");
    expect((await holder.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await applyAll(holder, events.slice(0, held));

    const state = { acked: held };
    const transport = scriptedTransport(events, state);
    const follower = openWatcherFollowerRuntime({
      deployment: D,
      storePath: path,
      automaticRecoveryMaxDepth: RECOVERY_DEPTH,
      origin: {
        origin: SIM_ORIGIN.point,
        hubOracleOneShot: SIM_HUB_ORACLE_ONE_SHOT,
      },
      node: {
        binaryPath: "unused",
        socketPath: "unused",
        networkMagic: 42,
        requestTimeoutMs: 1_000,
      },
      walletAddresses: [],
      unsafeTransportForTest: transport,
    });
    const c = collaborators();
    const driver = createWatcherDecisionDriver(
      {
        store: follower.store,
        onFollowerChange: (listener) => follower.onChange(() => listener()),
        authority: AUTHORITY,
        sourceId: SOURCE_ID,
        releaseDepth: RELEASE_DEPTH,
        bridge: c.bridge as never,
        availability: c.availability as never,
        retryDelayMs: 10,
      },
      {
        atTip: () => follower.status()?.atTip === true,
        started: () => watcherFollowerStarted(follower.status()),
      },
    );
    opened.push(async () => {
      await driver.close();
      await follower.close();
    });

    await until("the follower to wait out the held lease", () =>
      JSON.stringify(follower.status() ?? {}).includes("store_locked"),
    );
    driver.wake();
    await driver.idle();
    // The store has a cursor, but this process's follower has not started.
    expect((await follower.store.cursor())?.point.slot).toBeGreaterThan(
      SIM_ORIGIN.point.slot,
    );
    expect(driver.readiness().map(({ reason }) => reason)).toEqual([
      WATCHER_FOLLOWER_NOT_STARTED,
    ]);
    expect(c.seen.dispatched).toEqual([]);
    expect(c.seen.recoveryPreparations).toBe(0);

    await holder.close();
    holderOpen = false;
    await until(
      "a pass once the follower started",
      () => c.seen.dispatched.length > 0 && driver.readiness().length === 0,
    );
    expect(watcherFollowerStarted(follower.status())).toBe(true);
    expect(c.seen.recoveryPreparations).toBe(1);
  });
});
