import {
  type BlockSummary,
  type FactStore,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import { afterEach, describe, expect, it } from "vitest";

import { l1BlockBelowCoveredTip } from "../src/l1-heads.js";

const fill = (byte: number): Buffer => Buffer.alloc(32, byte);
const ORIGIN = { point: { slot: 1_000, hash: fill(0xa0) }, height: 40 };

/** origin(40) <- 41 <- 42 <- ... <- 40 + count, one block every 2 slots. */
const blocks = (count: number): BlockSummary[] => {
  const out: BlockSummary[] = [];
  let parent = ORIGIN.point.hash;
  for (let i = 1; i <= count; i += 1) {
    const hash = fill(0xb0 + i);
    out.push({
      point: { slot: ORIGIN.point.slot + 2 * i, hash },
      height: ORIGIN.height + i,
      parentHash: parent,
      txs: [],
    });
    parent = hash;
  }
  return out;
};

const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

const openStore = async (): Promise<FactStore> => {
  const store = openSqliteFactStore({
    securityParameter: 4,
    trackedSet: {
      addresses: new Set(),
      paymentCredentials: new Set(),
      policies: new Set(),
    },
    path: ":memory:",
  });
  opened.push(store);
  expect(await store.start()).toMatchObject({ kind: "ready" });
  return store;
};

describe("l1BlockBelowCoveredTip: the block d below the covered tip", () => {
  it("is unavailable before the follower has a covered tip", async () => {
    const store = await openStore();
    expect(await l1BlockBelowCoveredTip(store, 2)).toMatchObject({
      kind: "unavailable",
      reason: "not_initialized",
    });
  });

  it("reads the block at depth d + 1 and follows the covered tip through a rewind", async () => {
    const store = await openStore();
    await store.initialize(ORIGIN);
    const chain = blocks(6);
    for (const block of chain)
      expect(await store.applyBlock(block)).toMatchObject({ kind: "applied" });
    // Covered tip = height 46 (depth 1). d = 0 is the tip itself.
    expect(await l1BlockBelowCoveredTip(store, 0)).toMatchObject({
      kind: "block",
      height: 46,
      depth: 1,
      point: { slot: chain[5]!.point.slot },
    });
    const below = await l1BlockBelowCoveredTip(store, 3);
    expect(below).toMatchObject({
      kind: "block",
      height: 43,
      depth: 4,
      tip: { height: 46 },
    });
    if (below.kind !== "block") throw new Error("expected a block");
    expect(below.point.hash.equals(chain[2]!.point.hash)).toBe(true);
    expect(below.point.slot).toBe(1_006);
    // d reaching the origin answers the origin; one more is outside history.
    expect(await l1BlockBelowCoveredTip(store, 6)).toMatchObject({
      kind: "block",
      height: 40,
      point: { slot: ORIGIN.point.slot },
    });
    expect(await l1BlockBelowCoveredTip(store, 7)).toMatchObject({
      kind: "unavailable",
      reason: "outside_history",
    });
    // A rollback moves the covered tip down, and the lagged block with it.
    expect(await store.rewind(chain[3]!.point)).toMatchObject({
      kind: "rewound",
    });
    expect(await l1BlockBelowCoveredTip(store, 3)).toMatchObject({
      kind: "block",
      height: 41,
      depth: 4,
      tip: { height: 44 },
    });
  });

  it("refuses a negative or fractional lag", async () => {
    const store = await openStore();
    await expect(l1BlockBelowCoveredTip(store, -1)).rejects.toThrow(RangeError);
    await expect(l1BlockBelowCoveredTip(store, 1.5)).rejects.toThrow(
      RangeError,
    );
  });
});
