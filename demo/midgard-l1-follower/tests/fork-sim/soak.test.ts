import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type ChainPoint,
  chainPoint,
  type RollForward,
  samePoint,
} from "@al-ft/l1-node-transport";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  applyChainSyncEvent,
  type FactStore,
  openSqliteBackend,
  openSqliteFactStore,
  transportPoint,
} from "../../src/index.js";
import {
  ledgerComparator,
  readJournal,
  runSoak,
  type ShadowComparator,
  type SoakStream,
  startWhenFree,
  summarise,
} from "../../src/shadow/index.js";
import {
  buildForkSteps,
  forkCorpus,
  type ForkStep,
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  simUniverse,
} from "../../src/testing/index.js";
import { SIM_K } from "../support/fork-sim.js";
import { fakeLedger, fakeNode } from "../support/soak-fakes.js";

let dir: string;
const opened: FactStore[] = [];
beforeEach(async () => {
  dir = await mkdtemp(join(tmpdir(), "l1-soak-"));
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
  await rm(dir, { recursive: true, force: true });
});

const scenario = forkCorpus(SIM_K).find(
  (entry) => entry.name === "every shape in sequence",
)!.scenario;
const { steps, chain } = buildForkSteps(scenario);
const universe = simUniverse();

const storeAt = (): FactStore =>
  openSqliteFactStore({
    ...simStoreOptions([], SIM_K, "sqlite"),
    path: join(dir, "follower.sqlite"),
  });

const openStore = async (initialize: boolean): Promise<FactStore> => {
  const store = storeAt();
  opened.push(store);
  expect(await store.start()).toMatchObject({ kind: "ready" });
  if (initialize)
    expect(await store.initialize(SIM_ORIGIN)).toMatchObject({
      kind: "initialized",
    });
  return store;
};

/** What a newer writer's start does to the soak's store. */
const fence = async (): Promise<void> => {
  const other = openSqliteBackend(join(dir, "follower.sqlite"));
  try {
    await other.transaction("write", (tx) =>
      tx.query("UPDATE l1_follower_writer SET writer_epoch = writer_epoch + 1"),
    );
  } finally {
    await other.close();
  }
};

/**
 * A node on one linear chain that, like a real one, serves from the best
 * intersection offered on every (re)opening, and records acknowledgements.
 */
const linearNode = (events: readonly RollForward[]) => {
  const acked: bigint[] = [];
  const origin = transportPoint(SIM_ORIGIN.point);
  return {
    acked,
    openChainSync: ({
      points,
    }: Readonly<{ points: readonly ChainPoint[] }>): SoakStream => {
      const offered = (point: ChainPoint): boolean =>
        points.some((p) => samePoint(p, point));
      let at = -1;
      events.forEach((event, index) => {
        if (offered(event.point)) at = index;
      });
      if (at === -1 && !offered(origin))
        throw new Error("no intersection offered");
      let closed = false;
      return {
        next: () => {
          const event = closed ? undefined : events[at + 1];
          if (event !== undefined) at += 1;
          return Promise.resolve(event);
        },
        ack: (seq) => void acked.push(seq),
        close: () => {
          closed = true;
          return Promise.resolve();
        },
      };
    },
  };
};

const comparatorsFor = (
  ledger: ReturnType<typeof fakeLedger>,
): ShadowComparator[] => [
  ledgerComparator({
    ledger,
    addresses: [universe.trackedAddress, universe.credentialAddress],
    dir,
  }),
  {
    role: "committee",
    name: "heights",
    projected: ({ at }) => Promise.resolve({ kind: "value", value: at.height }),
    current: ({ at }) => Promise.resolve({ kind: "value", value: at.height }),
  },
];

describe("devnet soak runner", () => {
  it("journals every event, resumes after a crash and agrees with the ledger across forks", async () => {
    const node = fakeNode(steps);
    const comparators = comparatorsFor(fakeLedger(steps));
    const first = await openStore(true);
    expect(
      await runSoak({
        dir,
        store: first,
        openChainSync: node.openChainSync,
        comparators,
        maxEvents: 10,
      }),
    ).toMatchObject({ reason: "limit", events: 10 });
    // A crash between apply and append: the store moved, the journal did not.
    expect(
      (await applyChainSyncEvent(first, node.take())).result,
    ).toMatchObject({ kind: "applied" });
    await first.close();
    opened.splice(0);
    const second = await openStore(false);
    const rest = steps.length - node.served;
    expect(
      await runSoak({
        dir,
        store: second,
        openChainSync: node.openChainSync,
        comparators,
        maxEvents: rest,
      }),
    ).toMatchObject({ reason: "limit", events: rest });
    const { records, corrupt } = await readJournal(dir);
    const summary = summarise(records, corrupt);
    const forwards = steps.filter(
      (s: ForkStep) => s.event.kind === "roll_forward",
    ).length;
    expect(summary).toMatchObject({
      blocks: forwards - 1,
      rollbacks: steps.length - forwards,
      // A fresh soak compares at its origin; the restart compares at the
      // block the crash applied but never journaled.
      resumes: 2,
      starts: 2,
      firstDiff: null,
      firstError: null,
      rolesWithout: ["watcher", "node"],
      lastBlock: { height: chain.tip.height, slot: chain.tip.point.slot },
      lastStop: { reason: "limit" },
      corruptLines: 0,
    });
    expect(summary.comparators["follower/ledger-utxos"]).toEqual({
      equal: steps.length + 1,
      differs: 0,
      skipped: 0,
      error: 0,
    });
    const cursor = await second.cursor();
    expect(cursor?.point.hash.equals(chain.tip.point.hash)).toBe(true);
  });

  it("reports the first non-empty diff and keeps following", async () => {
    const node = fakeNode(steps);
    const at = SIM_ORIGIN.height + 5;
    const ledger = fakeLedger(steps, (height, utxos) =>
      height >= at ? utxos.slice(1) : utxos,
    );
    const store = await openStore(true);
    expect(
      await runSoak({
        dir,
        store,
        openChainSync: node.openChainSync,
        comparators: comparatorsFor(ledger),
        maxEvents: steps.length,
      }),
    ).toMatchObject({ reason: "limit" });
    const { records } = await readJournal(dir);
    const summary = summarise(records);
    expect(summary.firstDiff).toMatchObject({
      comparator: "follower/ledger-utxos",
      height: at,
    });
    expect(summary.blocks + summary.rollbacks).toBe(steps.length);
  });

  it("stops at an intervention and says why", async () => {
    const unknown = chainPoint(
      BigInt(chain.tip.point.slot - 1),
      "ee".repeat(32),
    );
    const head = steps.slice(0, 6);
    const node = fakeNode([
      ...head,
      {
        event: {
          kind: "roll_backward",
          seq: 99n,
          point: unknown,
          tip: { point: unknown, blockNo: 0n },
        },
      },
    ]);
    const store = await openStore(true);
    const stopped = await runSoak({
      dir,
      store,
      openChainSync: node.openChainSync,
      comparators: [],
    });
    expect(stopped).toMatchObject({ reason: "intervention", events: 6 });
    expect(stopped.detail).toContain("intersection_outside_history");
    const { records } = await readJournal(dir);
    expect(records[records.length - 1]).toMatchObject({
      type: "stop",
      reason: "intervention",
    });
  });

  it("stops as store_locked on a fenced store, acknowledging, applying and journaling nothing for that event; started again, it resumes from the cursor", async () => {
    const chain = new SimChain(universe, SIM_ORIGIN);
    const events = Array.from({ length: 8 }, () => chain.forward([]).event);
    const node = linearNode(events);
    const store = await openStore(true);
    const run = (maxEvents?: number) =>
      runSoak({
        dir,
        store,
        openChainSync: node.openChainSync,
        comparators: [],
        backoffMs: { initial: 1, max: 4 },
        ...(maxEvents === undefined ? {} : { maxEvents }),
      });
    expect(await run(3)).toMatchObject({ reason: "limit", events: 3 });
    await fence();
    const before = await store.cursor();
    const locked = await run(5);
    expect(locked).toMatchObject({ reason: "store_locked", events: 0 });
    expect(locked.detail).toContain(`roll_forward #${events[3]!.seq}`);
    expect(node.acked).toEqual(events.slice(0, 3).map((e) => e.seq));
    const after = await store.cursor();
    expect(after?.height).toBe(before?.height);
    expect(after?.point.hash.equals(before!.point.hash)).toBe(true);
    const { records } = await readJournal(dir);
    expect(records.filter((r) => r.type === "block")).toHaveLength(4);
    expect(records[records.length - 1]).toMatchObject({
      type: "stop",
      reason: "store_locked",
    });
    // The caller starts the store again and runs the soak again.
    expect(await startWhenFree(store)).toMatchObject({ kind: "ready" });
    expect(await run(5)).toMatchObject({ reason: "limit", events: 5 });
    expect(node.acked).toEqual(events.map((e) => e.seq));
    expect((await store.cursor())?.height).toBe(SIM_ORIGIN.height + 8);
  });

  it("startWhenFree waits out a lease another store holds, and gives up on abort", async () => {
    const holder = await openStore(true);
    const waiter = storeAt();
    opened.push(waiter);
    const lines: string[] = [];
    const waiting = startWhenFree(waiter, {
      backoffMs: { initial: 2, max: 8 },
      log: (line) => lines.push(line),
    });
    await new Promise((resolve) => setTimeout(resolve, 40));
    await holder.close();
    opened.splice(opened.indexOf(holder), 1);
    expect(await waiting).toMatchObject({ kind: "ready" });
    expect(lines.some((line) => line.includes("store locked"))).toBe(true);
    const third = storeAt();
    opened.push(third);
    const abort = new AbortController();
    const gaveUp = startWhenFree(third, {
      signal: abort.signal,
      backoffMs: { initial: 2, max: 8 },
    });
    await new Promise((resolve) => setTimeout(resolve, 20));
    abort.abort();
    expect(await gaveUp).toBeUndefined();
  });

  it("retries a transient store error instead of stopping", async () => {
    const node = fakeNode(steps.slice(0, 5));
    const store = await openStore(true);
    let failures = 1;
    const flaky: FactStore = {
      ...store,
      applyBlock: async (block) => {
        if (failures > 0) {
          failures -= 1;
          return { kind: "error", error: new Error("connection reset") };
        }
        return await store.applyBlock(block);
      },
    };
    const lines: string[] = [];
    expect(
      await runSoak({
        dir,
        store: flaky,
        openChainSync: node.openChainSync,
        comparators: [],
        maxEvents: 5,
        backoffMs: { initial: 1, max: 4 },
        log: (line) => lines.push(line),
      }),
    ).toMatchObject({ reason: "limit", events: 5 });
    expect(lines.some((line) => line.includes("connection reset"))).toBe(true);
  });
});
