import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import {
  type BlockSummary,
  type FactStore,
  type OriginConfig,
  protocolInitStatus,
  resetToOrigin,
} from "../src/index.js";
import { testDatabases } from "./support/postgres.js";
import { resetAdapters } from "./support/reset-stores.js";
import { fill, ORIGIN, output, tx } from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-protocol-init-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

/**
 * A node-shaped chain: the protocol-init tx spends the one-shot nonce and
 * mints under the hub policy; a merge then spends its state-queue output,
 * so once that spend is k deep (k = 2 in the reset fixtures) the init tx's
 * row is pruned (R1b keeps a tx only while an output it created is live).
 */
const HUB_POLICY = fill(0x44, 28);
const HUB_ADDRESS = Buffer.concat([Buffer.from([0x70]), HUB_POLICY]);
const QUEUE_ADDRESS = Buffer.concat([Buffer.from([0x70]), fill(0x55, 28)]);
const NONCE = { txHash: fill(0x01), index: 0 };
const INIT = fill(0xd1);
const MERGE = fill(0xd2);

const trackedSet = {
  addresses: new Set([QUEUE_ADDRESS.toString("hex")]),
  paymentCredentials: new Set<string>(),
  policies: new Set([HUB_POLICY.toString("hex")]),
};

/** origin <- init <- merge <- five empty blocks. */
const nodeChain = (): BlockSummary[] => {
  const blocks: BlockSummary[] = [];
  let parent = ORIGIN.point.hash;
  const push = (txs: BlockSummary["txs"]): void => {
    const height = ORIGIN.height + blocks.length + 1;
    const block: BlockSummary = {
      point: {
        slot: ORIGIN.point.slot + blocks.length + 1,
        hash: fill(0xe0 + blocks.length),
      },
      height,
      parentHash: parent,
      txs,
    };
    parent = block.point.hash;
    blocks.push(block);
  };
  push([
    tx(INIT, {
      inputs: [NONCE],
      outputs: [
        output(HUB_ADDRESS, 2_000_000n),
        output(QUEUE_ADDRESS, 2_000_000n),
      ],
      mint: new Map([[HUB_POLICY.toString("hex"), new Map([["aa", 1n]])]]),
    }),
  ]);
  push([
    tx(MERGE, {
      inputs: [{ txHash: INIT, index: 1 }],
      outputs: [output(QUEUE_ADDRESS, 2_000_000n)],
    }),
  ]);
  for (let i = 0; i < 5; i += 1) push([]);
  return blocks;
};

const blocks = nodeChain();
const initBlock = blocks[0] as BlockSummary;
const tip = (blocks.at(-1) as BlockSummary).point;
const config: OriginConfig = { origin: ORIGIN.point, hubOracleOneShot: NONCE };

const applyAll = async (
  store: FactStore,
  applied: readonly BlockSummary[],
): Promise<void> => {
  for (const block of applied)
    expect(await store.applyBlock(block)).toMatchObject({ kind: "applied" });
};

const pruneFully = async (store: FactStore): Promise<void> => {
  for (let i = 0; i < 20; i += 1) {
    const result = await store.prune();
    if ("done" in result && result.done) return;
  }
  throw new Error("prune did not finish");
};

describe.each(resetAdapters(databases, scratch))(
  "protocol-init fact ($name)",
  (adapter) => {
    const opened = async (): Promise<{
      store: FactStore;
      location: Awaited<ReturnType<typeof adapter.create>>;
    }> => {
      const location = await adapter.create();
      const store = location.store();
      store.setTrackedSet(trackedSet);
      store.watchProtocolInit(NONCE);
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(await store.initialize(ORIGIN)).toMatchObject({
        kind: "initialized",
      });
      return { store, location };
    };

    it("R3 does not fire after the init tx is pruned", async () => {
      const { store } = await opened();
      try {
        await applyAll(store, blocks);
        expect(await store.txByHash(INIT)).not.toBeNull();
        await pruneFully(store);
        // The init tx row is gone; the fact is not.
        expect(await store.txByHash(INIT)).toBeNull();
        expect(await store.txSpending(NONCE)).toBeNull();
        expect(await protocolInitStatus(store, config, tip)).toEqual({
          kind: "seen",
          txHash: INIT,
          slot: initBlock.point.slot,
        });
      } finally {
        await store.close();
      }
    });

    it("R3 still fires when the origin really is after protocol init", async () => {
      const location = await adapter.create();
      const store = location.store();
      try {
        store.setTrackedSet(trackedSet);
        store.watchProtocolInit(NONCE);
        expect(await store.start()).toMatchObject({ kind: "ready" });
        // O is the init block itself: the init tx lies at the origin, not after it.
        expect(
          await store.initialize({
            point: initBlock.point,
            height: initBlock.height,
          }),
        ).toMatchObject({ kind: "initialized" });
        await applyAll(store, blocks.slice(1));
        expect(
          await protocolInitStatus(
            store,
            { origin: initBlock.point, hubOracleOneShot: NONCE },
            tip,
          ),
        ).toMatchObject({
          kind: "intervention",
          reason: "origin_after_protocol_init",
        });
      } finally {
        await store.close();
      }
    });

    it("records the fact whether or not the init tx qualifies", async () => {
      const { store } = await opened();
      try {
        store.setTrackedSet({
          addresses: new Set(),
          paymentCredentials: new Set(),
          policies: new Set(),
        });
        await applyAll(store, blocks.slice(0, 1));
        expect(await store.txByHash(INIT)).toBeNull();
        expect(await store.protocolInit(NONCE)).toEqual({
          txHash: INIT,
          slot: initBlock.point.slot,
        });
      } finally {
        await store.close();
      }
    });

    it("a rewind across the init block removes the fact, and the block landing again records it", async () => {
      const { store } = await opened();
      try {
        await applyAll(store, blocks.slice(0, 3));
        // A rewind to the init block keeps it: the fact's block is still canonical.
        expect(await store.rewind(initBlock.point)).toMatchObject({
          kind: "rewound",
        });
        expect(await store.protocolInit(NONCE)).not.toBeNull();
        expect(await store.rewind(ORIGIN.point)).toMatchObject({
          kind: "rewound",
        });
        expect(await store.protocolInit(NONCE)).toBeNull();
        expect(
          await protocolInitStatus(store, config, ORIGIN.point),
        ).toMatchObject({
          kind: "intervention",
          reason: "origin_after_protocol_init",
        });
        await applyAll(store, blocks);
        expect(await protocolInitStatus(store, config, tip)).toEqual({
          kind: "seen",
          txHash: INIT,
          slot: initBlock.point.slot,
        });
      } finally {
        await store.close();
      }
    });

    it("a reset deletes the fact and the replay records it again", async () => {
      const { store, location } = await opened();
      await applyAll(store, blocks);
      await pruneFully(store);
      await store.close();
      const backend = location.backend();
      try {
        expect(await resetToOrigin(backend)).toMatchObject({
          tables: expect.arrayContaining(["l1_protocol_init"]) as unknown,
        });
      } finally {
        await backend.close();
      }
      const replay = location.store();
      try {
        replay.setTrackedSet(trackedSet);
        replay.watchProtocolInit(NONCE);
        expect(await replay.start()).toMatchObject({ kind: "ready" });
        expect(await replay.protocolInit(NONCE)).toBeNull();
        expect(await replay.initialize(ORIGIN)).toMatchObject({
          kind: "initialized",
        });
        await applyAll(replay, blocks);
        expect(await protocolInitStatus(replay, config, tip)).toMatchObject({
          kind: "seen",
          slot: initBlock.point.slot,
        });
      } finally {
        await replay.close();
      }
    });
  },
);
