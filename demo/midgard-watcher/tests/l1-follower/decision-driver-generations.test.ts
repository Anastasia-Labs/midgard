/**
 * The decision driver's handled-generation marker (`follower-generation.ts`)
 * and its pull by generation (lane SI-fix2, ruling SIFIX-R2):
 *
 * - push and pull meet in one handler that deduplicates by generation;
 * - the marker moves only after the replay-transcript retirement reset held;
 * - a pull of a generation this process already handled handles nothing
 *   again;
 * - a failed or still pending retirement reset is a named readiness reason,
 *   and the pass waits for a pending one at most `retryDelayMs`;
 * - only a transient failure (the store busy, a connection refused) is
 *   retried on a timer; any other is named and waits for the next rewind;
 * - migration 0010 seeds the marker from the cursor of a store that has one.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  decodeBlock,
  type FactStore,
  type FactStoreOptions,
  openSqliteFactStore,
  type Point,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  type SimTx,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { readHandledFollowerGeneration } from "../../src/l1-follower/follower-generation.js";
import { watcherProjection } from "../../src/l1-follower/projection.js";
import {
  createWatcherDecisionDriver,
  WATCHER_DECISION_PASS_FAILED,
  WATCHER_RETIREMENT_RESET_FAILED,
  WATCHER_RETIREMENT_RESET_PENDING,
  type WatcherDecisionDriver,
} from "../../src/runtime/watcher-runtime.decision-driver.js";
import {
  commitTx,
  initTx,
  queueState,
} from "../support/l1-follower-state-queue-traffic.js";
import {
  AUTHORITY,
  type Collaborators,
  collaborators,
  D,
  K,
  openStore,
  RELEASE_DEPTH,
  SOURCE_ID,
  until,
} from "../support/l1-follower-store-reset.js";
import { postgresSchemas } from "../support/postgres-schemas.js";

const scratch = mkdtempSync(join(tmpdir(), "watcher-driver-generations-"));
const schemas = postgresSchemas();
const opened: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of opened.splice(0).reverse()) await close();
});
afterAll(async () => {
  await schemas.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

type Step = "reject" | "fail" | "resolve" | "defer";

/** node:sqlite's SQLITE_BUSY: the transient store failure. */
const storeBusy = (): Error =>
  Object.assign(new Error("the transcript store is locked"), {
    code: "ERR_SQLITE_ERROR",
    errcode: 5,
  });

/** A failure no classifier recognises: not transient. */
const storeBroken = (): Error => new Error("the transcript row is malformed");

/** Retirement whose resets follow `script` (then resolve); `settle` resolves the deferred ones. */
const scriptedRetirement = (script: readonly Step[]) => {
  const deferred: (() => void)[] = [];
  let calls = 0;
  return {
    calls: () => calls,
    settle: () => {
      for (const resolve of deferred.splice(0)) resolve();
    },
    retirement: {
      ready: () => true,
      retire: () => Promise.resolve(),
      reset: (): Promise<void> => {
        const step = script[calls] ?? "resolve";
        calls += 1;
        if (step === "reject") return Promise.reject(storeBusy());
        if (step === "fail") return Promise.reject(storeBroken());
        if (step === "defer")
          return new Promise<void>((resolve) => deferred.push(resolve));
        return Promise.resolve();
      },
    },
  };
};

const driverOn = (
  store: FactStore,
  c: Collaborators,
  options: Readonly<{
    retryDelayMs: number;
    retirement?: ReturnType<typeof scriptedRetirement>["retirement"];
    rewound?: number[];
  }>,
): WatcherDecisionDriver => {
  const driver = createWatcherDecisionDriver(
    {
      store,
      onFollowerChange: () => () => undefined,
      authority: AUTHORITY,
      sourceId: SOURCE_ID,
      releaseDepth: RELEASE_DEPTH,
      bridge: c.bridge as never,
      availability: c.availability as never,
      ...(options.retirement === undefined
        ? {}
        : { retirement: options.retirement }),
      onRewind: (generation) => options.rewound?.push(generation),
      retryDelayMs: options.retryDelayMs,
    },
    { atTip: () => true, started: () => true },
  );
  opened.push(() => driver.close());
  return driver;
};

/** Waits for `holds` (async) to be true. */
const eventually = async (
  what: string,
  holds: () => Promise<boolean>,
  ms = 20_000,
) => {
  const deadline = Date.now() + ms;
  while (!(await holds())) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
};

const decided = async (driver: WatcherDecisionDriver, c: Collaborators) => {
  const dispatched = c.seen.dispatched.length;
  driver.wake();
  await until(
    "a decision",
    () =>
      c.seen.dispatched.length > dispatched && driver.readiness().length === 0,
  );
  await driver.idle();
};

/**
 * A store at a protocol chain's tip: the init, a commit, then `blocks`
 * empty blocks. A first driver decided there (the marker is at generation
 * 0); the returned chain extends it.
 */
const decidedStore = async (name: string, blocks = 6) => {
  const store = openStore(join(scratch, `${name}.db`));
  opened.push(() => store.close());
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const forward = async (txs: readonly SimTx[] = []) =>
    expect(
      (await store.applyBlock(decodeBlock(chain.forward(txs).encoded.raw)))
        .kind,
    ).toBe("applied");
  expect((await store.start()).kind).toBe("ready");
  expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
  await forward([initTx(D)]);
  await forward([commitTx(queueState(chain, D)!, D)]);
  for (let i = 0; i < blocks; i += 1) await forward();
  const c = collaborators();
  const first = driverOn(store, c, { retryDelayMs: 10 });
  await decided(first, c);
  await first.close();
  const marker = () => readHandledFollowerGeneration(store);
  expect(await marker()).toBe(0);
  /** Rolls the chain and the store back `depth` blocks; returns the target. */
  const rewind = async (depth: number): Promise<Point> => {
    chain.backward(depth);
    const target = chain.tip.point;
    expect((await store.rewind(target)).kind).toBe("rewound");
    return target;
  };
  return { store, chain, c, forward, marker, rewind };
};

/** A rewind no driver heard, then the chain extended past its target. */
const unheardRewind = async (name: string) => {
  const s = await decidedStore(name);
  await s.rewind(3);
  for (let i = 0; i < 4; i += 1) await s.forward();
  return s;
};

describe("the decision driver's handled generation", () => {
  it("handles a pulled generation once: a push of the next generation that lands first covers it", async () => {
    const s = await decidedStore("merge-before-dedupe", 8);
    // Generation 1, unheard, to a low target; the chain grows again.
    const low = await s.rewind(5);
    for (let i = 0; i < 5; i += 1) await s.forward();
    const rewound: number[] = [];
    const driver = driverOn(s.store, s.c, {
      retryDelayMs: 10,
      rewound,
    });
    // Generation 2, pushed, to a higher target: its pass pulls generation 1.
    const high = await s.rewind(2);
    expect(high.slot).toBeGreaterThan(low.slot);
    await until("the pass", () => driver.readiness().length === 0);
    await driver.idle();
    expect(rewound).toEqual([2]);
    expect(await s.marker()).toBe(2);
  });

  it("keeps the marker while the retirement reset fails, names the failure, and moves the marker once the retried reset held, handling the rewind once", async () => {
    const s = await unheardRewind("reset-fails-once");
    const r = scriptedRetirement(["reject", "defer"]);
    const driver = driverOn(s.store, s.c, {
      retryDelayMs: 60_000,
      retirement: r.retirement,
    });
    driver.wake();
    await until("the failed reset named", () =>
      driver
        .readiness()
        .some(({ reason }) => reason === WATCHER_RETIREMENT_RESET_FAILED),
    );
    await driver.idle();
    expect(driver.readiness()).toEqual([
      {
        reason: WATCHER_RETIREMENT_RESET_FAILED,
        detail: "the transcript store is locked",
      },
    ]);
    expect(await s.marker()).toBe(0);
    // The rewind's reset, and the pass's retry of it.
    expect(r.calls()).toBe(2);
    expect(driver.status().rewinds).toBe(1);

    // The retry holds: its settling wakes the driver.
    r.settle();
    await eventually("the marker", async () => (await s.marker()) === 1);
    await until("readiness", () => driver.readiness().length === 0);
    await driver.idle();
    // The pull of the same generation handled nothing again.
    expect(driver.status().rewinds).toBe(1);
  });

  it("retries a retirement reset that keeps failing one retry delay apart, names it every pass, and handles the rewind once", async () => {
    const s = await unheardRewind("reset-keeps-failing");
    const r = scriptedRetirement(["reject", "reject", "reject"]);
    const driver = driverOn(s.store, s.c, {
      retryDelayMs: 10,
      retirement: r.retirement,
    });
    const seen = new Set<string>();
    driver.wake();
    await eventually("the marker", async () => {
      for (const { reason } of driver.readiness()) seen.add(reason);
      return (await s.marker()) === 1;
    });
    await until("readiness", () => driver.readiness().length === 0);
    await driver.idle();
    expect(seen).toContain(WATCHER_RETIREMENT_RESET_FAILED);
    expect(seen).not.toContain(WATCHER_RETIREMENT_RESET_PENDING);
    expect(r.calls()).toBe(4);
    expect(driver.status().rewinds).toBe(1);
  });

  it("waits for a pending retirement reset at most the retry delay: the pass completes, names the wait, and the marker moves once the reset settles", async () => {
    const s = await unheardRewind("reset-pending");
    const r = scriptedRetirement(["defer"]);
    const driver = driverOn(s.store, s.c, {
      retryDelayMs: 20,
      retirement: r.retirement,
    });
    const dispatched = s.c.seen.dispatched.length;
    driver.wake();
    await until(
      "the pass to decide with the reset pending",
      () => s.c.seen.dispatched.length > dispatched,
      5_000,
    );
    await driver.idle();
    expect(driver.readiness().map(({ reason }) => reason)).toEqual([
      WATCHER_RETIREMENT_RESET_PENDING,
    ]);
    expect(await s.marker()).toBe(0);

    r.settle();
    await eventually("the marker", async () => (await s.marker()) === 1);
    await until("readiness", () => driver.readiness().length === 0);
    expect(r.calls()).toBe(1);
    expect(driver.status().rewinds).toBe(1);
  });
});

describe("the decision driver's failure classes", () => {
  it("does not retry a retirement reset that failed with a non-transient failure: it names it and the marker stays until the next rewind", async () => {
    const s = await unheardRewind("reset-fails-not-transient");
    const r = scriptedRetirement(["fail", "fail", "fail", "fail"]);
    const driver = driverOn(s.store, s.c, {
      retryDelayMs: 10,
      retirement: r.retirement,
    });
    driver.wake();
    await until("the failed reset named", () =>
      driver
        .readiness()
        .some(({ reason }) => reason === WATCHER_RETIREMENT_RESET_FAILED),
    );
    await driver.idle();
    // Well past many retry delays: nothing retried it.
    await new Promise((resolve) => setTimeout(resolve, 200));
    await driver.idle();
    expect(r.calls()).toBe(1);
    expect(await s.marker()).toBe(0);
    expect(driver.readiness()).toEqual([
      {
        reason: WATCHER_RETIREMENT_RESET_FAILED,
        detail: expect.stringContaining(
          "the transcript row is malformed (not transient",
        ) as unknown as string,
      },
    ]);
    // The next rewind runs it again, and a reset that holds moves the marker.
    await s.forward();
    await s.rewind(1);
    await until("the second reset", () => r.calls() >= 2);
    await driver.idle();
    expect(r.calls()).toBe(2);
  });

  it("does not retry a decision pass that failed with a non-transient failure, and retries a transient one", async () => {
    const s = await decidedStore("pass-fails-by-class");
    let failure: (() => Error) | undefined = storeBroken;
    let passes = 0;
    const store = new Proxy(s.store, {
      get(target, key, receiver) {
        const value = Reflect.get(target, key, receiver) as unknown;
        if (typeof value !== "function") return value;
        if (key !== "rewindsSince")
          return (value as (...a: unknown[]) => unknown).bind(target);
        return (...args: unknown[]) => {
          passes += 1;
          if (passes > 50) throw new Error("retried without bound");
          const make = failure;
          if (make !== undefined) return Promise.reject(make());
          return (value as (...a: unknown[]) => unknown).apply(target, args);
        };
      },
    });
    const driver = driverOn(store, s.c, { retryDelayMs: 10 });
    driver.wake();
    await until("the failed pass named", () =>
      driver
        .readiness()
        .some(({ reason }) => reason === WATCHER_DECISION_PASS_FAILED),
    );
    await driver.idle();
    const afterFirst = passes;
    await new Promise((resolve) => setTimeout(resolve, 200));
    await driver.idle();
    expect(passes).toBe(afterFirst);
    expect(driver.readiness()[0]?.detail).toContain("(not transient");

    // A transient failure is retried one delay later, and recovers.
    failure = storeBusy;
    driver.wake();
    await until("the transient failure named", () =>
      driver.readiness().some(({ detail }) => detail.includes("is locked")),
    );
    const transientAt = passes;
    failure = undefined;
    await until("readiness", () => driver.readiness().length === 0);
    await driver.idle();
    expect(passes).toBeGreaterThan(transientAt);
  });
});

describe.each(["sqlite", "postgres"] as const)(
  "migration 0010 (%s)",
  (dialect) => {
    const without0010 = (options: FactStoreOptions): FactStoreOptions => ({
      ...options,
      migrations: (options.migrations ?? []).map((set) => ({
        ...set,
        migrations: set.migrations.filter(
          ({ id }) => id !== "0010_watcher_follower_generation",
        ),
      })),
    });
    const opener = async (): Promise<
      (options: FactStoreOptions) => FactStore
    > => {
      if (dialect === "postgres") return (await schemas.open()).store;
      const path = join(scratch, `${String(Math.random()).slice(2)}.db`);
      return (options) => openSqliteFactStore({ ...options, path });
    };
    const options = simStoreOptions([watcherProjection(D)], K, dialect);

    it("seeds the marker from the cursor's generation of a store that has one", async () => {
      const open = await opener();
      const before = open(without0010(options));
      try {
        expect((await before.start()).kind).toBe("ready");
        expect((await before.initialize(SIM_ORIGIN)).kind).toBe("initialized");
        const chain = new SimChain(simUniverse(), SIM_ORIGIN);
        for (let i = 0; i < 4; i += 1)
          expect(
            (
              await before.applyBlock(
                decodeBlock(chain.forward([]).encoded.raw),
              )
            ).kind,
          ).toBe("applied");
        chain.backward(2);
        expect((await before.rewind(chain.tip.point)).kind).toBe("rewound");
        expect((await before.cursor())?.generation).toBe(1);
      } finally {
        await before.close();
      }
      const store = open(options);
      try {
        const started = await store.start();
        expect("migrated" in started ? started.migrated : []).toEqual([
          expect.stringContaining("0010_watcher_follower_generation"),
        ]);
        expect(await readHandledFollowerGeneration(store)).toBe(1);
      } finally {
        await store.close();
      }
    });

    it("seeds no marker on a store with no cursor", async () => {
      const open = await opener();
      const before = open(without0010(options));
      try {
        expect((await before.start()).kind).toBe("ready");
      } finally {
        await before.close();
      }
      const store = open(options);
      try {
        expect((await store.start()).kind).toBe("ready");
        expect(await readHandledFollowerGeneration(store)).toBeNull();
      } finally {
        await store.close();
      }
    });
  },
);
