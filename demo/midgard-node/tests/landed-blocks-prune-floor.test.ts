/**
 * The landed frontier's prune floor (N3) against the production foreign
 * replayer on a real follower store. Caught up or lagging, the floor keeps
 * every fact after `T` of a block starting at the frontier's end, less one
 * slot, so a forced order the next block names and the facts lack is the
 * block's fault (`invalid`), and an honest one is found. That holds when a
 * processed node after the frontier leaves the chain and the next block
 * starts at its predecessor's end. A store pruned past that bound before the
 * floor applied stays pruned: the block waits (`event_unknown`, bounded by
 * the shared local-fault run limit).
 *
 * The bench's slot configuration maps slot `s` to `s * 1000` ms.
 */
import type {
  FactStore,
  FollowerProjection,
  OutRef,
  PruneResult,
} from "@al-ft/midgard-l1-follower";
import { type SimOutput, simTxHash } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { stateQueueProjection } from "../src/l1-state-queue/index.js";
import { landedFrontierPruneFloor } from "../src/landed-blocks/index.js";
import { db } from "./helpers/forced-orders-node-store.js";
import type { ChainDriver } from "./helpers/l1-events-store.js";
import {
  NODE_PREFIX,
  nodeDatum,
  queueOutput,
  ROOT_ASSET,
  rootDatum,
  SIM_QUEUE_CONFIG,
  simHeader,
} from "./helpers/state-queue-sim.fixtures.js";
import {
  clock,
  FIXTURE_ORDER_ID,
  fixtureOrderTx,
  followChain,
  forcedBlock,
  idKey,
  K,
} from "./landed-blocks-replay-foreign.fixture.js";
import {
  benchSlotOf,
  originSlot,
  reach,
  replayAt,
  SLOT_MS,
  spendTx,
  startFor,
  strangerOrderBlock,
  windowed,
} from "./landed-blocks-replay-foreign.window.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];

beforeEach(async () => {
  clock.offsetMs = 0;
  await db(resetApplicationTables);
});
afterEach(async () => {
  vi.restoreAllMocks();
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

/** The header the node's confirmed ledger holds (the queue root's). */
const FRONTIER = "fa".repeat(28);

type Element = Readonly<{
  outRef: OutRef;
  output: (link: string | null) => SimOutput;
}>;

/** A node header over `(start, end]` after `prev`. */
const nodeHeader = (
  nonce: number,
  prev: string,
  start: bigint,
  end: bigint,
): SDK.Header => ({
  ...simHeader(nonce, prev),
  startTime: start,
  endTime: end,
});

/**
 * A follower store with the landed frontier's floor on its prune (the
 * node's frontier and processed headers given here), its chain past the
 * origin by one block, and the landed queue on it.
 */
const bench = async () => {
  const processed = new Set<string>();
  /** Whether the floor is in force yet. */
  const inForce = { on: true };
  const floor = landedFrontierPruneFloor({
    config: SIM_QUEUE_CONFIG,
    needs: () => Promise.resolve({ frontier: FRONTIER, processed }),
    slotOf: benchSlotOf,
  });
  const projection: FollowerProjection = {
    ...stateQueueProjection(SIM_QUEUE_CONFIG),
    pruneFloor: {
      name: floor.name,
      floor: (context) =>
        inForce.on ? floor.floor(context) : Promise.resolve(null),
    },
  };
  const { store, chain } = await followChain({}, [projection]);
  opened.push(store);
  const slots = new Map<number, number>();
  const forward = async (txs: Parameters<ChainDriver["forward"]>[0]) => {
    const hashes = await chain.forward(txs);
    slots.set(chain.tip.height, chain.tip.point.slot);
    return hashes;
  };
  await forward([]);
  const origin = await originSlot(store);
  const queue: Element[] = [];
  const send = async (inputs: OutRef[], outputs: SimOutput[]) => {
    const [hash] = await forward([
      { inputs, outputs, nonce: chain.chain.nonce() },
    ]);
    return hash!;
  };
  return {
    store,
    chain,
    origin,
    processed,
    inForce,
    forward,
    /** `blocks` empty blocks. */
    advance: async (blocks: number) => {
      for (let block = 0; block < blocks; block++) await forward([]);
    },
    /** The slot of the block the retained depth below the tip. */
    retainedSlot: () => slots.get(chain.tip.height - K)!,
    /** The queue root at the frontier, its confirmed state ending at `endTime`. */
    root: async (endTime: bigint) => {
      const output = (link: string | null) =>
        queueOutput(ROOT_ASSET, rootDatum(FRONTIER, link, endTime));
      const txHash = await send([chain.chain.outsideInput()], [output(null)]);
      queue.push({ outRef: { txHash, index: 0 }, output });
    },
    /** Appends `header` after the queue's tail; returns its hash. */
    append: async (header: SDK.Header) => {
      const hash = SDK.stateQueueHeaderHash(header);
      const tail = queue.pop()!;
      const output = (link: string | null) =>
        queueOutput(NODE_PREFIX + hash, nodeDatum(header, "Unattested", link));
      const txHash = await send(
        [tail.outRef],
        [tail.output(hash), output(null)],
      );
      queue.push(
        { outRef: { txHash, index: 0 }, output: tail.output },
        { outRef: { txHash, index: 1 }, output },
      );
      return hash;
    },
    /** Removes the queue's tail node, relinking its predecessor. */
    removeTail: async () => {
      const tail = queue.pop()!;
      const before = queue.pop()!;
      const txHash = await send(
        [before.outRef, tail.outRef],
        [before.output(null)],
      );
      queue.push({ outRef: { txHash, index: 0 }, output: before.output });
    },
    /** One prune step. */
    prune: async (): Promise<PruneResult> => {
      const result = await store.prune();
      if (!("floorLags" in result))
        throw new Error(`prune: ${JSON.stringify(result)}`);
      return result;
    },
  };
};

/**
 * Caught up: the root at the frontier (ending at `T = origin + 2`) and two
 * processed nodes after it, the last ending at `T = origin + 6`, where the
 * next block starts; then `K + 6` more blocks.
 */
const caughtUp = async (inForce = true) => {
  const at = await bench();
  at.inForce.on = inForce;
  const { origin } = at;
  await at.root(startFor(origin + 2));
  const first = await at.append(
    nodeHeader(1, FRONTIER, startFor(origin + 2), startFor(origin + 4)),
  );
  const second = await at.append(
    nodeHeader(2, first, startFor(origin + 4), startFor(origin + 6)),
  );
  at.processed.add(first).add(second);
  await at.advance(K + 6);
  expect(at.retainedSlot()).toBeGreaterThan(origin + 6);
  return { ...at, next: startFor(origin + 6) };
};

describe("the landed frontier's prune floor and an absent forced order", () => {
  it("is invalid in the next block while caught up, with processed nodes after the frontier", async () => {
    const at = await caughtUp();
    const { origin } = at;
    const retained = at.retainedSlot();
    const pruned = await at.prune();
    expect(pruned.prunedThroughSlot).toBe(origin + 1);
    expect(pruned.floorLags).toContainEqual({
      floor: "landed_frontier",
      lagSlots: retained - (origin + 1),
    });
    const block = await strangerOrderBlock(at.next);
    await reach(at.store, block);
    expect(await replayAt(at.store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringMatching(
        /^forced order .* is not admitted at the view/,
      ),
    });
  });

  it("is invalid in the next block while lagging, with nothing processed after the frontier", async () => {
    const at = await bench();
    const { origin } = at;
    await at.advance(4);
    await at.root(startFor(origin + 2));
    expect(at.chain.tip.point.slot - 1).toBeGreaterThan(origin + 2);
    const block = await strangerOrderBlock(startFor(origin + 2));
    await at.append(block.block_body.header);
    await at.advance(K + 3);
    expect((await at.prune()).prunedThroughSlot).toBe(origin + 1);
    await reach(at.store, block);
    expect(await replayAt(at.store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringMatching(
        /^forced order .* is not admitted at the view/,
      ),
    });
  });

  it("waits, not invalid, when the store was pruned past T(B) before the floor applied", async () => {
    const at = await caughtUp(false);
    const { origin } = at;
    const past = at.retainedSlot();
    expect((await at.prune()).prunedThroughSlot).toBe(past);
    at.inForce.on = true;
    expect((await at.prune()).prunedThroughSlot).toBe(past);
    const block = await strangerOrderBlock(at.next);
    await reach(at.store, block);
    expect(await replayAt(at.store, block)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        `the facts are pruned through slot ${past.toString()}, past slot ${(origin + 6).toString()}`,
      ),
    });
  });
});

describe.each([
  {
    route: "a processed node rolled back",
    leave: (at: Awaited<ReturnType<typeof bench>>) => at.chain.backward(1),
  },
  {
    route: "a processed node removed from the queue",
    leave: async (at: Awaited<ReturnType<typeof bench>>) => {
      await at.removeTail();
      await at.prune();
    },
  },
])("the next block after $route", ({ leave }) => {
  /**
   * The root at the frontier (ending at `T = origin`), the forced fixture's
   * order admitted at slot `s` and spent in the same block, a processed node
   * ending at `T = s - 1` (the next block starts there), `K + 3` more
   * blocks, then a processed node after it ending at `T = s + 1`, pruned
   * once and then gone.
   */
  const gone = async () => {
    const at = await bench();
    const { origin } = at;
    await at.root(startFor(origin));
    const admittedAt = at.chain.chain.nextSlot();
    const next = startFor(admittedAt - 1);
    const order = await fixtureOrderTx(
      at.chain,
      FIXTURE_ORDER_ID,
      next + SLOT_MS,
    );
    await at.forward([
      order,
      spendTx({ txHash: simTxHash(order), index: 0 }, at.chain.chain.nonce()),
    ]);
    expect(at.chain.tip.point.slot).toBe(admittedAt);
    const first = await at.append(
      nodeHeader(1, FRONTIER, startFor(origin), next),
    );
    await at.advance(K + 3);
    const second = await at.append(
      nodeHeader(2, first, next, startFor(admittedAt + 1)),
    );
    at.processed.add(first).add(second);
    expect(at.retainedSlot()).toBeGreaterThan(admittedAt);
    await at.prune();
    await leave(at);
    return { ...at, next };
  };

  it("finds an honest forced order admitted and spent inside its window", async () => {
    const { store, next } = await gone();
    const block = await windowed(await forcedBlock(), next, 3n * SLOT_MS);
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "replayed",
      forcedIds: [Buffer.from(idKey(FIXTURE_ORDER_ID), "hex")],
    });
  });

  it("is invalid for a forced order no one admitted", async () => {
    const { store, next } = await gone();
    const block = await strangerOrderBlock(next);
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringMatching(
        /^forced order .* is not admitted at the view/,
      ),
    });
  });
});
