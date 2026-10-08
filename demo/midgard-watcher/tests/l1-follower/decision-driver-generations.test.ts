/**
 * The decision driver's handled-generation marker (`follower-generation.ts`)
 * and its pull by generation (lane SI-fix2, ruling SIFIX-R2):
 *
 * - push and pull meet in one handler that merges the history target before
 *   it deduplicates by generation;
 * - the marker moves only after the history is back at every target and the
 *   replay-transcript retirement reset held;
 * - a pull of a generation this process already rolled the history back
 *   through rolls nothing back again;
 * - a failed or still pending retirement reset is a named readiness reason,
 *   and the pass waits for a pending one at most `retryDelayMs`;
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

const native = (point: Point) => ({
  kind: "point",
  blockHash: point.hash.toString("hex"),
  slot: String(point.slot),
});

/**
 * A user-event history stand-in: it rolls back to a point, forgetting its
 * height (so the next release-final observation advances it again), and
 * can refuse its next rollbacks.
 */
const historyStub = () => {
  const rollbacks: unknown[] = [];
  let refusals = 0;
  let head = {
    blockHash: SIM_ORIGIN.point.hash.toString("hex"),
    slot: String(SIM_ORIGIN.point.slot),
    blockNo: String(SIM_ORIGIN.height),
    pointId: "",
  };
  return {
    rollbacks,
    refuseNext: (count: number) => {
      refusals = count;
    },
    head: () => head,
    history: {
      read: () => ({
        status: "ready" as const,
        currentPoint: head,
        headCursor: head,
        generation: 0,
      }),
      advanceThrough: (point: typeof head) => {
        head = point;
        return Promise.resolve();
      },
      handleRollback: (point: { slot: string; blockHash: string }) => {
        if (refusals > 0) {
          refusals -= 1;
          return Promise.reject(new Error("the history store is busy"));
        }
        rollbacks.push(point);
        head = {
          ...head,
          slot: point.slot,
          blockHash: point.blockHash,
          blockNo: "0",
        };
        return Promise.resolve();
      },
    },
  };
};

type Step = "reject" | "resolve" | "defer";

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
        if (step === "reject")
          return Promise.reject(new Error("the transcript store is locked"));
        if (step === "defer")
          return new Promise<void>((resolve) => deferred.push(resolve));
        return Promise.resolve();
      },
    },
  };
};

type History = ReturnType<typeof historyStub>;

const driverOn = (
  store: FactStore,
  c: Collaborators,
  history: History,
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
      history: history.history as never,
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
 * 0 and the history advanced); the returned chain extends it.
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
  const history = historyStub();
  const first = driverOn(store, c, history, { retryDelayMs: 10 });
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
  return { store, chain, c, history, forward, marker, rewind };
};

/** A rewind no driver heard, then the chain extended past its target. */
const unheardRewind = async (name: string) => {
  const s = await decidedStore(name);
  const target = await s.rewind(3);
  for (let i = 0; i < 4; i += 1) await s.forward();
  // The history is above the target: the rollback is owed.
  expect(BigInt(s.history.head().slot)).toBeGreaterThan(BigInt(target.slot));
  return { ...s, target };
};

describe("the decision driver's handled generation", () => {
  it("merges a pulled lower target before it deduplicates: a push of the next generation that lands first does not hide it", async () => {
    const s = await decidedStore("merge-before-dedupe", 8);
    // Generation 1, unheard, to a low target; the chain grows again.
    const low = await s.rewind(5);
    for (let i = 0; i < 5; i += 1) await s.forward();
    const rewound: number[] = [];
    const driver = driverOn(s.store, s.c, s.history, {
      retryDelayMs: 10,
      rewound,
    });
    // Generation 2, pushed, to a higher target: its pass pulls generation 1.
    const high = await s.rewind(2);
    expect(high.slot).toBeGreaterThan(low.slot);
    await until("the pass", () => driver.readiness().length === 0);
    await driver.idle();
    expect(s.history.rollbacks).toEqual([native(low)]);
    expect(rewound).toEqual([2]);
    expect(await s.marker()).toBe(2);
  });

  it("keeps the marker while the history rollback fails, and moves it once the rollback applied", async () => {
    const s = await unheardRewind("history-fails-once");
    s.history.refuseNext(1);
    const driver = driverOn(s.store, s.c, s.history, { retryDelayMs: 60_000 });
    driver.wake();
    await until("the failed pass", () => driver.status().lastError !== null);
    await driver.idle();
    expect(driver.readiness().map(({ reason }) => reason)).toEqual([
      WATCHER_DECISION_PASS_FAILED,
    ]);
    expect(await s.marker()).toBe(0);
    expect(s.history.rollbacks).toEqual([]);

    driver.wake();
    await until("the next pass", () => driver.readiness().length === 0);
    await driver.idle();
    expect(s.history.rollbacks).toEqual([native(s.target)]);
    expect(await s.marker()).toBe(1);
  });

  it("keeps the marker while the retirement reset fails, names the failure, and moves the marker once the retried reset held, rolling the history back once", async () => {
    const s = await unheardRewind("reset-fails-once");
    const r = scriptedRetirement(["reject", "defer"]);
    const driver = driverOn(s.store, s.c, s.history, {
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
    expect(s.history.rollbacks).toEqual([native(s.target)]);
    // The history advanced again past the target in that pass.
    expect(BigInt(s.history.head().slot)).toBeGreaterThan(
      BigInt(s.target.slot),
    );

    // The retry holds: its settling wakes the driver.
    r.settle();
    await eventually("the marker", async () => (await s.marker()) === 1);
    await until("readiness", () => driver.readiness().length === 0);
    await driver.idle();
    // The pull of the same generation rolled nothing back again.
    expect(s.history.rollbacks).toEqual([native(s.target)]);
    expect(driver.status().rewinds).toBe(1);
  });

  it("retries a retirement reset that keeps failing one retry delay apart, names it every pass, and rolls the history back once", async () => {
    const s = await unheardRewind("reset-keeps-failing");
    const r = scriptedRetirement(["reject", "reject", "reject"]);
    const driver = driverOn(s.store, s.c, s.history, {
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
    expect(s.history.rollbacks).toEqual([native(s.target)]);
  });

  it("waits for a pending retirement reset at most the retry delay: the pass completes, names the wait, and the marker moves once the reset settles", async () => {
    const s = await unheardRewind("reset-pending");
    const r = scriptedRetirement(["defer"]);
    const driver = driverOn(s.store, s.c, s.history, {
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
    expect(s.history.rollbacks).toEqual([native(s.target)]);
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
