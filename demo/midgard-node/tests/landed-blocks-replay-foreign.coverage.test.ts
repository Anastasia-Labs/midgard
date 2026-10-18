/**
 * The production foreign replayer on a real follower store: a deposit,
 * withdrawal or forced order a block names and the view does not know is the
 * block's fault (`invalid`) once the facts cover the block's range, and a
 * bounded wait (`event_unknown`) while they do not. The facts cover it when
 * the follower store is still in the view's generation, its origin is at or
 * before `T(B)` (the window's start less the event wait and the longest
 * event validity range, as a slot: the block's events are admitted after
 * it), and the prune is still below the slot the block's node output
 * entered the facts at (for a forced order, at or before `T(B)`). An origin
 * after `T(B)`, or a node output seeded at the origin, is a store whose
 * origin is too late for the blocks after the landed frontier
 * (`origin_after_frontier`), never the block's fault.
 *
 * The bench's slot configuration maps slot `s` to `s * 1000` ms; the view's
 * clock is moved forward so the view's horizon reaches each block's end.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import { simTxHash } from "@al-ft/midgard-l1-follower/testing";
import type * as SDK from "@al-ft/midgard-sdk";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { db } from "./helpers/forced-orders-node-store.js";
import { retirementTx } from "./helpers/l1-events-chain.js";
import {
  clock,
  depositBlock,
  emptyBlock,
  FIXTURE_ORDER_ID,
  fixtureOrderTx,
  followChain,
  forcedBlock,
  idKey,
  inNode,
  inputFor,
  K,
  naming,
  nonceRef,
  replayerFor,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import {
  admitDeposit,
  originSlot,
  reach,
  replayAt,
  SLOT_MS,
  spendTx,
  startFor,
  strangerDepositBlock,
  strangerOrderBlock,
  windowed,
} from "./landed-blocks-replay-foreign.window.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];
const follow = async (...args: Parameters<typeof followChain>) => {
  const followed = await followChain(...args);
  opened.push(followed.store);
  return followed;
};

beforeEach(async () => {
  clock.offsetMs = 0;
  await db(resetApplicationTables);
});
afterEach(async () => {
  vi.restoreAllMocks();
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

describe("an absent deposit or withdrawal where the facts cover the block's range", () => {
  it("is invalid for a deposit, the origin at T(B) and the prune below the block's entry", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await strangerDepositBlock(startFor(await originSlot(store)));
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("whose facts cover the block's range"),
    });
  });

  it("is invalid for a withdrawal", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await windowed(
      await naming(await emptyBlock(), {
        withdrawals: [[idKey(nonceRef()), "00"]],
      }),
      startFor(await originSlot(store)),
    );
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringMatching(
        /^withdrawal .* is not admitted at the view/,
      ),
    });
  });

  it("accepts an honest deposit included at the window's end, after a view one block short of it waits", async () => {
    const { store, chain } = await follow();
    const start = startFor(await originSlot(store));
    const { entry } = await admitDeposit(chain, start + SLOT_MS);
    const block = await windowed(await depositBlock(entry), start);
    await reach(store, block, -1);
    expect(await replayAt(store, block)).toEqual({
      kind: "missing",
      detail: "the block ends past what the follower view can know",
    });
    await chain.forward([]);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "replayed",
      depositIds: [Buffer.from(entry.idCbor, "hex")],
    });
  });
});

describe("a retired deposit and the prune at the block's entry", () => {
  /** A deposit retired after the block's entry at `entered`, then `K + 3` more blocks. */
  const retiredAfterEntry = async () => {
    const floor: { slot: number | null } = { slot: null };
    const { store, chain } = await follow({
      pruneFloors: [{ name: "test", floor: () => Promise.resolve(floor.slot) }],
    });
    const start = startFor(await originSlot(store));
    const { order, outRef, entry } = await admitDeposit(chain, start + SLOT_MS);
    const entered = chain.tip.point.slot;
    await chain.forward([
      retirementTx(order, outRef, "absorbed", chain.chain.nonce()),
    ]);
    const retiredAt = chain.tip.point.slot;
    for (let block = 0; block < K + 3; block++) await chain.forward([]);
    const block = await windowed(await depositBlock(entry), start);
    await reach(store, block);
    return { store, floor, block, entered, retiredAt };
  };

  it("finds a deposit retired after the block's entry with the prune one slot below it", async () => {
    const { store, floor, block, entered } = await retiredAfterEntry();
    floor.slot = entered - 1;
    await store.prune();
    expect((await store.cursor())!.prunedThroughSlot).toBe(entered - 1);
    expect(await replayAt(store, block, entered)).toMatchObject({
      kind: "replayed",
    });
  });

  it("waits, not invalid, once the prune reached the block's entry and the deposit is gone", async () => {
    const { store, floor, block, retiredAt } = await retiredAfterEntry();
    floor.slot = retiredAt;
    await store.prune();
    expect((await store.cursor())!.prunedThroughSlot).toBe(retiredAt);
    expect(await replayAt(store, block, retiredAt)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        `the facts are pruned through slot ${retiredAt.toString()}, not below the block's entry at slot ${retiredAt.toString()}`,
      ),
    });
  });
});

describe("the follower's origin against T(B)", () => {
  it("refuses to judge an absent deposit as a late-origin store when the origin is one slot after T(B)", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const origin = await originSlot(store);
    const block = await strangerDepositBlock(startFor(origin - 1));
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "origin_after_frontier",
      detail: expect.stringContaining(
        `the follower's origin at slot ${origin.toString()} is after slot ${(origin - 1).toString()}`,
      ),
    });
  });

  it("accepts a block whose named deposit is known, whatever the origin", async () => {
    const { store, chain } = await follow();
    const start = startFor((await originSlot(store)) - 1);
    const { entry } = await admitDeposit(chain, start + SLOT_MS);
    const block = await windowed(await depositBlock(entry), start);
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({ kind: "replayed" });
  });

  it("refuses to judge an absent deposit as a late-origin store when the block's node output was seeded", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await strangerDepositBlock(startFor(await originSlot(store)));
    await reach(store, block);
    expect(await replayAt(store, block, null)).toMatchObject({
      kind: "origin_after_frontier",
      detail: expect.stringContaining(
        "the block's node output predates the follower's origin",
      ),
    });
  });
});

describe("the follower store's generation against the view's", () => {
  it("waits on an absent deposit when the follower store changed generation since the view", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await strangerDepositBlock(startFor(await originSlot(store)));
    await reach(store, block);
    await retain(await retainedRow(block));
    const input = await inputFor(store, block);
    await chain.forward([]);
    await chain.backward(1);
    expect((await store.cursor())!.generation).not.toBe(input.view.generation);
    expect(await inNode(replayerFor(store)(input))).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        "the follower store changed generation since the view",
      ),
    });
  });
});

describe("an absent forced order against T(B)", () => {
  it("is invalid for a forced order, the origin and the prune at T(B)", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await strangerOrderBlock(startFor(await originSlot(store)));
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringMatching(
        /^forced order .* is not admitted at the view/,
      ),
    });
  });

  it("refuses to judge an absent forced order as a late-origin store when the origin is one slot after T(B)", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const origin = await originSlot(store);
    const block = await strangerOrderBlock(startFor(origin - 1));
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "origin_after_frontier",
      detail: expect.stringContaining(
        `the follower's origin at slot ${origin.toString()} is after slot ${(origin - 1).toString()}`,
      ),
    });
  });
});

describe("a spent forced order and the prune at T(B)", () => {
  /**
   * The forced fixture's order, included at `inclusionTime(admittedAt)`,
   * admitted at `admittedAt` and spent in the same block (`sameBlock`) or the
   * next, then `K + 3` more blocks.
   */
  const spentOrder = async (
    inclusionTime: (admittedAt: number) => bigint,
    sameBlock = false,
  ) => {
    const floor: { slot: number | null } = { slot: null };
    const { store, chain } = await follow({
      pruneFloors: [{ name: "test", floor: () => Promise.resolve(floor.slot) }],
    });
    await chain.forward([]);
    const origin = await originSlot(store);
    const admittedAt = chain.chain.nextSlot();
    const order = await fixtureOrderTx(
      chain,
      FIXTURE_ORDER_ID,
      inclusionTime(admittedAt),
    );
    const spend = () =>
      spendTx({ txHash: simTxHash(order), index: 0 }, chain.chain.nonce());
    if (sameBlock) await chain.forward([order, spend()]);
    else {
      await chain.forward([order]);
      await chain.forward([spend()]);
    }
    const spentAt = chain.tip.point.slot;
    for (let block = 0; block < K + 3; block++) await chain.forward([]);
    /** Prunes through `slot` and replays `block` at the view. */
    const prunedThrough = async (
      slot: number,
      block: SDK.DaPayload,
      enteredSlot?: number,
    ) => {
      floor.slot = slot;
      await store.prune();
      expect((await store.cursor())!.prunedThroughSlot).toBe(slot);
      await reach(store, block);
      return replayAt(store, block, enteredSlot);
    };
    return { store, origin, admittedAt, spentAt, prunedThrough };
  };

  /**
   * An order spent one slot after `T(B)`: the window starts at `T(B)`, the
   * spend's slot less one, and spans 3 s, since the spend lands one or two
   * slots after the admission; the order's inclusion time is in the window
   * either way.
   */
  const spentAfterTB = async () => {
    const spent = await spentOrder(
      (admittedAt) => startFor(admittedAt + 1) + SLOT_MS,
    );
    const tb = spent.spentAt - 1;
    expect(tb).toBeGreaterThanOrEqual(spent.admittedAt);
    const block = await windowed(
      await forcedBlock(),
      startFor(tb),
      3n * SLOT_MS,
    );
    return { ...spent, tb, block };
  };

  it("finds an order spent one slot after T(B) with the prune at T(B)", async () => {
    const { tb, block, prunedThrough } = await spentAfterTB();
    expect(await prunedThrough(tb, block)).toMatchObject({
      kind: "replayed",
      forcedIds: [Buffer.from(idKey(FIXTURE_ORDER_ID), "hex")],
    });
  });

  it("waits, not invalid, once the prune passed T(B) and the order is gone", async () => {
    const { tb, block, prunedThrough } = await spentAfterTB();
    expect(await prunedThrough(tb + 1, block)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        `the facts are pruned through slot ${(tb + 1).toString()}, past slot ${tb.toString()}`,
      ),
    });
  });

  it("waits, not invalid, on an order spent before the block's entry once the prune passed its spend", async () => {
    const { store, origin, spentAt, prunedThrough } = await spentOrder(
      (admittedAt) => startFor(admittedAt) + SLOT_MS,
    );
    const block = await windowed(
      await forcedBlock(),
      startFor(origin),
      3n * SLOT_MS,
    );
    const entered = (await store.currentView())!.point.slot;
    expect(spentAt).toBeLessThan(entered);
    expect(await prunedThrough(spentAt, block, entered)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        `the facts are pruned through slot ${spentAt.toString()}, past slot ${origin.toString()}`,
      ),
    });
  });

  it("finds an order admitted and spent at the first slot after T(B), the window starting inside a slot, while the prune is at T(B)", async () => {
    const offset = SLOT_MS / 2n;
    const { admittedAt, spentAt, prunedThrough } = await spentOrder(
      (admittedAt) => startFor(admittedAt - 1) + offset + SLOT_MS,
      true,
    );
    expect(spentAt).toBe(admittedAt);
    const tb = admittedAt - 1;
    const block = await windowed(await forcedBlock(), startFor(tb) + offset);
    expect(await prunedThrough(tb, block)).toMatchObject({
      kind: "replayed",
      forcedIds: [Buffer.from(idKey(FIXTURE_ORDER_ID), "hex")],
    });
    expect(await prunedThrough(tb + 1, block)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        `the facts are pruned through slot ${(tb + 1).toString()}, past slot ${tb.toString()}`,
      ),
    });
  });
});
