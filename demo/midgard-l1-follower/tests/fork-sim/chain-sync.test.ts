import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { chainPoint, ORIGIN } from "@al-ft/l1-node-transport";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  applyChainSyncEvent,
  type FactStore,
  intersectionPoints,
  openSqliteBackend,
  openSqliteFactStore,
  stepLocked,
  stepSettled,
  transportPoint,
} from "../../src/index.js";
import {
  buildForkSteps,
  checkpointFailure,
  type ForkCheckpoint,
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  simUniverse,
} from "../../src/testing/index.js";
import { SIM_K } from "../support/fork-sim.js";

const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
const scratch = mkdtempSync(join(tmpdir(), "l1-chain-sync-"));
afterAll(() => {
  rmSync(scratch, { recursive: true, force: true });
});

const freshStore = async (
  trackedSet = simUniverse().tracked,
): Promise<FactStore> => {
  const store = openSqliteFactStore({
    ...simStoreOptions([], SIM_K, "sqlite"),
    trackedSet,
    path: ":memory:",
  });
  opened.push(store);
  expect(await store.start()).toMatchObject({ kind: "ready" });
  expect(await store.initialize(SIM_ORIGIN)).toMatchObject({
    kind: "initialized",
  });
  return store;
};

const tip = { point: transportPoint(SIM_ORIGIN.point), blockNo: 1000n };

describe("applyChainSyncEvent", () => {
  it("refuses a rollback to the genesis as rollback_beyond_k without touching the store", async () => {
    const store = await freshStore();
    const step = await applyChainSyncEvent(store, {
      kind: "roll_backward",
      seq: 1n,
      point: ORIGIN,
      tip,
    });
    expect(step.result).toMatchObject({
      kind: "intervention",
      reason: "rollback_beyond_k",
    });
    expect((await store.cursor())?.height).toBe(SIM_ORIGIN.height);
  });

  it("reports an undecodable block instead of applying it", async () => {
    const store = await freshStore();
    const step = await applyChainSyncEvent(store, {
      kind: "roll_forward",
      seq: 1n,
      point: chainPoint(50_001n, "ab".repeat(32)),
      blockNo: 1001n,
      blockType: 7,
      prevHash: SIM_ORIGIN.point.hash.toString("hex"),
      tip,
      block: Uint8Array.from([0x82, 0x01, 0x02]),
    });
    expect(step.result).toMatchObject({ kind: "block_undecodable" });
    expect((await store.cursor())?.height).toBe(SIM_ORIGIN.height);
  });

  it("applies the simulator's blocks and rewinds to its rollback points", async () => {
    const store = await freshStore();
    const chain = new SimChain(simUniverse(), SIM_ORIGIN);
    for (let i = 0; i < 4; i += 1)
      expect(
        (await applyChainSyncEvent(store, chain.forward([]).event)).result,
      ).toMatchObject({ kind: "applied" });
    const back = chain.backward(2);
    expect((await applyChainSyncEvent(store, back)).result).toMatchObject({
      kind: "rewound",
    });
    expect((await store.cursor())?.height).toBe(SIM_ORIGIN.height + 2);
  });

  it("reports store_locked from a fenced store, unsettled and without advancing, for both directions", async () => {
    const path = join(scratch, "fenced.sqlite");
    const store = openSqliteFactStore({
      ...simStoreOptions([], SIM_K, "sqlite"),
      path,
    });
    opened.push(store);
    expect(await store.start()).toMatchObject({ kind: "ready" });
    expect(await store.initialize(SIM_ORIGIN)).toMatchObject({
      kind: "initialized",
    });
    const chain = new SimChain(simUniverse(), SIM_ORIGIN);
    for (let i = 0; i < 3; i += 1)
      expect(
        (await applyChainSyncEvent(store, chain.forward([]).event)).result,
      ).toMatchObject({ kind: "applied" });
    const before = await store.cursor();
    // What a newer writer's start does to this store.
    const other = openSqliteBackend(path);
    try {
      await other.transaction("write", (tx) =>
        tx.query(
          "UPDATE l1_follower_writer SET writer_epoch = writer_epoch + 1",
        ),
      );
    } finally {
      await other.close();
    }
    for (const event of [chain.forward([]).event, chain.backward(1)]) {
      const step = await applyChainSyncEvent(store, event);
      expect(step.result).toMatchObject({ kind: "store_locked" });
      expect(stepLocked(step)).toBe(true);
      expect(stepSettled(step)).toBe(false);
      const after = await store.cursor();
      expect(after?.height).toBe(before?.height);
      expect(after?.point.hash.equals(before!.point.hash)).toBe(true);
      expect(after?.generation).toBe(before?.generation);
    }
    // The caller starts again, and the store follows from where it was.
    expect(await store.start()).toMatchObject({
      kind: "ready",
      cursor: { height: before?.height },
    });
  });
});

describe("intersectionPoints", () => {
  it("offers the 64 newest blocks, exponentially older ones, then the origin", async () => {
    const store = await freshStore();
    const chain = new SimChain(simUniverse(), SIM_ORIGIN);
    const points = [];
    for (let i = 0; i < 300; i += 1) {
      const { event } = chain.forward([]);
      points.push(event.point);
      await applyChainSyncEvent(store, event);
    }
    const offered = await intersectionPoints(store);
    expect(offered.slice(0, 64)).toEqual(points.slice(-64).reverse());
    // Heights 1172 and 1044 (128 and 256 below the tip 1300), then the origin.
    expect(offered.slice(64)).toEqual([
      points[171],
      points[43],
      transportPoint(SIM_ORIGIN.point),
    ]);
  });

  it("a fork between 2,049 and k blocks deep intersects at the retained boundary and rewinds without R1", async () => {
    // Mainnet k: the exponential gaps stop at 2,048, the next one (4,096)
    // lies below k, so only the boundary candidate keeps such a fork inside
    // the window.
    const k = 2_160;
    const store = openSqliteFactStore({
      ...simStoreOptions([], k, "sqlite"),
      path: ":memory:",
    });
    opened.push(store);
    expect(await store.start()).toMatchObject({ kind: "ready" });
    expect(await store.initialize(SIM_ORIGIN)).toMatchObject({
      kind: "initialized",
    });
    const chain = new SimChain(simUniverse(), SIM_ORIGIN);
    const points = [];
    const length = 2_400;
    for (let i = 0; i < length; i += 1) {
      const { event } = chain.forward([]);
      points.push(event.point);
      expect((await applyChainSyncEvent(store, event)).result.kind).toBe(
        "applied",
      );
    }
    for (let done = false; !done; ) {
      const pruned = await store.prune();
      if (!("done" in pruned)) throw new Error(JSON.stringify(pruned));
      done = pruned.done;
    }
    // The node switched to a fork whose last common block is 2,100 deep.
    const forkDepth = 2_100;
    const key = (point: ReturnType<typeof transportPoint>): string =>
      `${String(point.slot)}.${point.hash}`;
    const shared = new Set(
      [
        transportPoint(SIM_ORIGIN.point),
        ...points.slice(0, length - forkDepth),
      ].map(key),
    );
    const offered = await intersectionPoints(store);
    // Best first: strictly descending, the boundary right after the 2,048 gap.
    const boundary = points[length - 1 - k];
    const at = (point: ReturnType<typeof transportPoint> | undefined): number =>
      offered.findIndex((offer) => key(offer) === key(point!));
    expect(at(boundary)).toBeGreaterThan(0);
    expect(at(boundary)).toBe(at(points[length - 1 - 2_048]) + 1);
    // FindIntersect answers with the first offered point on the node's chain.
    const intersection = offered.find((point) => shared.has(key(point)));
    expect(intersection).toEqual(boundary);
    const step = await applyChainSyncEvent(store, {
      kind: "roll_backward",
      seq: 1n,
      point: intersection!,
      tip: {
        point: intersection!,
        blockNo: BigInt(SIM_ORIGIN.height + length - k),
      },
    });
    expect(step.result).toMatchObject({ kind: "rewound", depth: k });
  }, 120_000);

  it("offers only the origin for a store at its origin", async () => {
    const store = await freshStore();
    expect(await intersectionPoints(store)).toEqual([
      transportPoint(SIM_ORIGIN.point),
    ]);
  });
});

describe("checkpointFailure", () => {
  const scenario = {
    seed: 7,
    episodes: [
      {
        shape: "reland" as const,
        depth: 2,
        extra: 1,
        landAt: 1,
        variant: 0,
        lead: 1,
      },
    ],
  };

  const followed = async (store: FactStore): Promise<ForkCheckpoint> => {
    const { steps } = buildForkSteps(scenario);
    for (const step of steps) await applyChainSyncEvent(store, step.event);
    const last = steps[steps.length - 1]?.checkpoint;
    if (last === undefined) throw new Error("no final checkpoint");
    return last;
  };

  it("passes for an honest store and fails for each wrong expectation", async () => {
    const store = await freshStore();
    const checkpoint = await followed(store);
    expect(await checkpointFailure(store, checkpoint)).toBeNull();
    const [present, spender] = checkpoint.checks;
    expect(present?.kind).toBe("tx_present");
    expect(spender?.kind).toBe("spender");
    const wrongSpender = {
      ...checkpoint,
      checks: [{ ...spender!, spentBy: null } as const],
    };
    expect(await checkpointFailure(store, wrongSpender)).toMatch(
      /F: expected unspent, got spent by/u,
    );
    const wrongValidTo = {
      ...checkpoint,
      checks: [{ ...present!, invalidAfter: 1n } as const],
    };
    expect(await checkpointFailure(store, wrongValidTo)).toMatch(
      /re-landed tx: expected stored \(valid true, invalidAfter 1\)/u,
    );
    const missingOne = {
      ...checkpoint,
      checks: [],
      liveTracked: checkpoint.liveTracked.slice(1),
    };
    expect(await checkpointFailure(store, missingOne)).toMatch(
      /live tracked outputs differ from the model/u,
    );
  });

  // A qualification bug that the store and its fresh replay share (both
  // drop the tracked payment credential) is invisible to the replay
  // comparison; the model's live set catches it.
  it("catches a tracked-set bug a fresh replay would share", async () => {
    const universe = simUniverse();
    const store = await freshStore({
      ...universe.tracked,
      paymentCredentials: new Set(),
    });
    const checkpoint = await followed(store);
    expect(await checkpointFailure(store, checkpoint)).toMatch(
      /live tracked outputs differ from the model/u,
    );
  });
});
