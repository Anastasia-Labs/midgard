import {
  applyChainSyncEvent,
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
  stepSettled,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  simStoreOptions,
} from "@al-ft/midgard-l1-follower/testing";
import { afterAll, describe, expect, it } from "vitest";

import { slotTimeMs } from "../../src/l1/follower/obligations.js";
import {
  committeeProjection,
  readCommitteeView,
} from "../../src/l1/follower/projection.js";
import { postgresTestDatabases } from "../helpers/postgres-database.js";
import { QueueChain } from "./queue-chain.js";
import { SIM_QUEUE, SIM_SLOT_TIME } from "./queue-sim.js";

const K = 3;
const PARAMETERS = { confirmationDepth: 1, securityParameter: K } as const;
const projection = committeeProjection(SIM_QUEUE);

const databases = postgresTestDatabases("midgard_test_c1_committee_view");
afterAll(async () => {
  await databases.dropAll();
});

type Opened = { store: FactStore; close: () => Promise<void> };

const openSqlite = async (): Promise<Opened> => {
  const store = openSqliteFactStore({
    ...simStoreOptions([projection], K, "sqlite"),
    path: ":memory:",
  });
  return { store, close: () => store.close() };
};

const openPostgres = async (): Promise<Opened> => {
  const database = await databases.create();
  const store = openPostgresFactStore({
    ...simStoreOptions([projection], K, "postgres"),
    connection: { connectionString: database.url },
  });
  return { store, close: () => store.close() };
};

/**
 * The view's reads over closed history, against the blocks the chain built:
 * the queue at the latest final block, each asked header's exit, and the
 * block of every live node.
 */
describe.each([
  ["SQLite", openSqlite],
  ["Postgres", openPostgres],
] as const)("the committee view over closed queue history (%s)", (_, open) => {
  it("reads the final queue, the exits and the node blocks from the facts", async () => {
    const { store, close } = await open();
    try {
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(
        await store.initialize({
          point: SIM_ORIGIN.point,
          height: SIM_ORIGIN.height,
        }),
      ).toMatchObject({ kind: "initialized" });
      const queue = new QueueChain();
      const apply = async (
        event: Parameters<typeof applyChainSyncEvent>[1],
      ) => {
        expect(stepSettled(await applyChainSyncEvent(store, event))).toBe(true);
        return queue.chain.tip;
      };
      await apply(queue.init());
      await apply(queue.append());
      const a = queue.nodes[0]!.hash;
      await apply(queue.append());
      const b = queue.nodes[1]!.hash;
      // A merges into the root: its node and the root are spent, and the
      // new root carries A's header hash.
      const merge = await apply(queue.merge());
      // Each append relinks the tail, so B, C and D are live as relinked
      // outputs created in the next append's block.
      const tips = [];
      for (let i = 0; i < 3; i += 1) tips.push(await apply(queue.append()));
      const tip = queue.chain.tip;
      // k = 3 below a tip at the merge's height + 3: the merge block is the
      // latest final block.
      expect(tip.height - K).toBe(merge.height);

      const view = await readCommitteeView(store, {
        parameters: PARAMETERS,
        slotTime: SIM_SLOT_TIME,
        exitsOf: [b, a, "ff".repeat(28)],
      });

      expect(view?.finalSlot).toBe(merge.point.slot);
      // Live at the merge block: the root it created (A) and B, whose output
      // was spent only by the next append. The root the merge spent is not.
      expect(view?.finalQueueHeaderHashes).toEqual([a, b].sort());
      expect(view?.exits).toEqual([
        {
          headerHash: a,
          status: "merged",
          slot: merge.point.slot,
          blockHash: merge.point.hash.toString("hex"),
          blockHeight: merge.height,
          depth: tip.height - merge.height + 1,
        },
      ]);
      expect(view?.nodeBlocks).toEqual(
        new Map(
          tips.map((block) => [
            block.point.slot,
            block.point.hash.toString("hex"),
          ]),
        ),
      );
    } finally {
      await close();
    }
  });

  it("reads the release clock as the latest final block's slot start, so an end time inside that slot is still pending", async () => {
    const { store, close } = await open();
    try {
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(
        await store.initialize({
          point: SIM_ORIGIN.point,
          height: SIM_ORIGIN.height,
        }),
      ).toMatchObject({ kind: "initialized" });
      const queue = new QueueChain();
      const apply = async (
        event: Parameters<typeof applyChainSyncEvent>[1],
      ): Promise<void> => {
        expect(stepSettled(await applyChainSyncEvent(store, event))).toBe(true);
      };
      await apply(queue.init());
      for (let i = 0; i < K + 2; i += 1) await apply(queue.empty());
      const read = (endTimesMs: readonly bigint[]) =>
        readCommitteeView(store, {
          parameters: PARAMETERS,
          slotTime: SIM_SLOT_TIME,
          // Headers never committed: only the release clock decides them.
          signed: endTimesMs.map((endTimeMs, i) => ({
            headerHash: (i + 1).toString(16).padStart(56, "0"),
            endTimeMs,
          })),
        });
      const first = await read([]);
      expect(first?.finalSlot).not.toBeNull();
      const slotStartMs = slotTimeMs(first!.finalSlot!, SIM_SLOT_TIME);
      expect(first?.finalBlockTimeMs).toBe(slotStartMs);
      const view = await read([
        BigInt(slotStartMs - 1),
        BigInt(slotStartMs),
        BigInt(slotStartMs + SIM_SLOT_TIME.slotLength / 2),
      ]);
      expect(view?.obligations.map(({ state }) => state)).toEqual([
        "cannot_land",
        "pending",
        "pending",
      ]);
    } finally {
      await close();
    }
  });
});
