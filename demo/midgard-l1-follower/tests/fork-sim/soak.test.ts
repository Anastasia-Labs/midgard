import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { chainPoint } from "@al-ft/l1-node-transport";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  applyChainSyncEvent,
  type FactStore,
  openSqliteFactStore,
} from "../../src/index.js";
import {
  ledgerComparator,
  readJournal,
  runSoak,
  type ShadowComparator,
  summarise,
} from "../../src/shadow/index.js";
import {
  buildForkSteps,
  forkCorpus,
  type ForkStep,
  SIM_ORIGIN,
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

const openStore = async (initialize: boolean): Promise<FactStore> => {
  const store = openSqliteFactStore({
    ...simStoreOptions([], SIM_K, "sqlite"),
    path: join(dir, "follower.sqlite"),
  });
  opened.push(store);
  expect(await store.start()).toMatchObject({ kind: "ready" });
  if (initialize)
    expect(await store.initialize(SIM_ORIGIN)).toMatchObject({
      kind: "initialized",
    });
  return store;
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
