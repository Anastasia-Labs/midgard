import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import {
  type BlockSummary,
  createHeads,
  createSlotClock,
  depth,
  type DepthParameters,
  type FactStore,
  HeadsParameterError,
  heightAtDepth,
  isFinal,
  isSafe,
  levelAtDepth,
  levelOf,
  mergedStatus,
  openSqliteFactStore,
} from "../src/index.js";
import { Rng } from "./support/chain.js";
import {
  fill,
  options,
  ORIGIN,
  output,
  TRACKED,
  tx,
} from "./support/small-chain.js";

const scratch = mkdtempSync(join(tmpdir(), "l1-follower-heads-"));
afterAll(() => rmSync(scratch, { recursive: true, force: true }));

const EMULATOR: DepthParameters = {
  confirmationDepth: 3,
  securityParameter: 6,
};

describe("depth()", () => {
  it("counts the tip as depth 1 and a height above the tip as off the chain", () => {
    expect(depth(53, 53)).toBe(1);
    expect(depth(53, 51)).toBe(3);
    expect(depth(53, 54)).toBe(0);
    expect(heightAtDepth(53, 1)).toBe(53);
    expect(heightAtDepth(53, depth(53, 40))).toBe(40);
  });

  it("is the one definition behind the three variants of plan §3.5", () => {
    const tip = 1_000;
    const block = 990;
    // Inclusive (settlement): head − block + 1.
    expect(depth(tip, block)).toBe(tip - block + 1);
    // Descendants only (signed-intent coverage): one less.
    expect(depth(tip, block) - 1).toBe(tip - block);
    // head − d (operator membership): the block at depth d + 1.
    const d = 10;
    expect(heightAtDepth(tip, d + 1)).toBe(tip - d);
  });
});

describe("levels", () => {
  it("puts each boundary at the right level", () => {
    const { confirmationDepth: cd, securityParameter: k } = EMULATOR;
    expect(levelAtDepth(0, EMULATOR)).toBeNull();
    expect(levelAtDepth(1, EMULATOR)).toBe("landed");
    expect(levelAtDepth(cd - 1, EMULATOR)).toBe("landed");
    expect(levelAtDepth(cd, EMULATOR)).toBe("safe");
    expect(levelAtDepth(k, EMULATOR)).toBe("safe");
    expect(levelAtDepth(k + 1, EMULATOR)).toBe("final");
    expect([isSafe(cd - 1, EMULATOR), isSafe(cd, EMULATOR)]).toEqual([
      false,
      true,
    ]);
    expect([isFinal(k, EMULATOR), isFinal(k + 1, EMULATOR)]).toEqual([
      false,
      true,
    ]);
  });

  it("calls an absent own item local and an absent foreign item nothing", () => {
    expect(levelOf(null, EMULATOR, true)).toBe("local");
    expect(levelOf(0, EMULATOR, true)).toBe("local");
    expect(levelOf(null, EMULATOR, false)).toBeNull();
    expect(levelOf(4, EMULATOR, true)).toBe("safe");
  });

  it("never claims merged bare: it carries the merge tx's level", () => {
    expect(mergedStatus("landed")).toEqual({ merged: true, level: "landed" });
    expect(mergedStatus("final")).toEqual({ merged: true, level: "final" });
    expect(mergedStatus("local")).toEqual({ merged: false, level: "local" });
    expect(mergedStatus(null)).toEqual({ merged: false, level: null });
  });

  it("refuses cd below 1 and k below cd", () => {
    expect(() =>
      createHeads({
        confirmationDepth: 0,
        securityParameter: 6,
        slotLengthMs: 1_000,
      }),
    ).toThrow(HeadsParameterError);
    expect(() =>
      createHeads({
        confirmationDepth: 10,
        securityParameter: 9,
        slotLengthMs: 1_000,
      }),
    ).toThrow(HeadsParameterError);
  });
});

describe("slotNow", () => {
  afterEach(() => vi.useRealTimers());

  it("is unknown until a tip is observed, then advances with elapsed time", () => {
    let nowMs = 5_000;
    const clock = createSlotClock({
      slotLengthMs: 1_000,
      monotonicNowMs: () => nowMs,
    });
    expect(clock.slotNow()).toBeNull();
    clock.observeTipSlot(200);
    expect(clock.slotNow()).toBe(200);
    nowMs += 2_999;
    expect(clock.slotNow()).toBe(202);
    nowMs += 1;
    expect(clock.slotNow()).toBe(203);
  });

  it("takes the tip when it is ahead, and never moves back for a stale or rolled-back tip", () => {
    let nowMs = 0;
    const clock = createSlotClock({
      slotLengthMs: 1_000,
      monotonicNowMs: () => nowMs,
    });
    clock.observeTipSlot(100);
    nowMs = 10_000;
    expect(clock.slotNow()).toBe(110);
    clock.observeTipSlot(130);
    expect(clock.slotNow()).toBe(130);
    clock.observeTipSlot(90);
    expect(clock.tipSlot()).toBe(90);
    expect(clock.slotNow()).toBe(130);
    nowMs += 5_000;
    expect(clock.slotNow()).toBe(135);
  });

  it("does not move when the wall clock is 10 minutes fast", () => {
    const clock = createSlotClock({ slotLengthMs: 1_000 });
    clock.observeTipSlot(5_000);
    const before = clock.slotNow();
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(Date.now() + 10 * 60_000);
    expect(clock.slotNow()).toBe(before);
    vi.useRealTimers();
  });
});

/**
 * A minimal fork driver until F8's simulator replaces it: a seeded walk that
 * extends the chain or forks it (rewind, then a new branch), with one watched
 * own tx that lands, rolls back, and re-lands or never re-lands. After every
 * step the level read from the store through the heads module must equal the
 * level computed from the model chain.
 */
describe("level transitions under forks (sqlite)", () => {
  const K = 6;
  const PARAMETERS: DepthParameters = {
    confirmationDepth: 2,
    securityParameter: K,
  };
  type ModelBlock = { block: BlockSummary; watched: Buffer | null };

  const makeBlock = (
    rng: Rng,
    parent: { slot: number; hash: Buffer; height: number },
    watched: Buffer | null,
  ): BlockSummary => ({
    point: { slot: parent.slot + 1 + rng.int(3), hash: rng.bytes(32) },
    height: parent.height + 1,
    parentHash: parent.hash,
    txs:
      watched === null
        ? []
        : [
            tx(watched, {
              inputs: [{ txHash: fill(0x01), index: 0 }],
              outputs: [output(TRACKED, 2_000_000n)],
            }),
          ],
  });

  /** The watched own tx's level as the store and the heads module see it. */
  const storeLevel = async (store: FactStore, watched: Buffer) => {
    const stored = await store.txByHash(watched);
    if (stored === null) return levelOf(null, PARAMETERS, true);
    const block = await store.blockAtOrBeforeSlot(stored.blockSlot);
    if (block === null) throw new Error("watched tx without its block");
    const status = await store.pointStatus({
      slot: block.slot,
      hash: block.hash,
    });
    if (status.kind !== "canonical") throw new Error(status.detail);
    return levelOf(status.depth, PARAMETERS, true);
  };

  /** The same level computed from the model chain alone. */
  const modelLevel = (model: readonly ModelBlock[], watched: Buffer) => {
    const tip = model.at(-1)?.block.height ?? ORIGIN.height;
    const holder = model.find((entry) => entry.watched?.equals(watched));
    return levelOf(
      holder === undefined ? null : depth(tip, holder.block.height),
      PARAMETERS,
      true,
    );
  };

  it("matches the model after every extend and fork, and sees every transition", async () => {
    const rng = new Rng(791);
    const store = openSqliteFactStore({
      ...options(K),
      path: join(scratch, "heads-forks.db"),
    });
    const heads = createHeads({ ...PARAMETERS, slotLengthMs: 1_000 });
    const transitions = new Set<string>();
    try {
      await store.start();
      await store.initialize(ORIGIN);
      let model: ModelBlock[] = [];
      let watched = rng.bytes(32);
      let previous = levelOf(null, PARAMETERS, true);
      for (let step = 0; step < 2_000; step += 1) {
        const forkable = Math.min(model.length, K);
        if (forkable > 0 && rng.chance(0.3)) {
          // A fork: roll back up to k blocks; the new branch arrives as
          // ordinary extends on the following steps.
          const rollback = 1 + rng.int(forkable);
          const target =
            model[model.length - 1 - rollback]?.block.point ?? ORIGIN.point;
          expect(await store.rewind(target)).toMatchObject({
            kind: "rewound",
          });
          model = model.slice(0, model.length - rollback);
        } else {
          const parent = model.at(-1)?.block;
          const landed = model.some((entry) => entry.watched?.equals(watched));
          // Re-land or never re-land: an absent watched tx lands again with
          // some probability on each new block.
          const include = !landed && rng.chance(0.3) ? watched : null;
          const block = makeBlock(
            rng,
            parent === undefined
              ? { ...ORIGIN.point, height: ORIGIN.height }
              : { ...parent.point, height: parent.height },
            include,
          );
          expect(await store.applyBlock(block)).toMatchObject({
            kind: "applied",
          });
          model.push({ block, watched: include });
        }
        const cursor = await store.cursor();
        heads.observeTip({ slot: cursor!.point.slot, height: cursor!.height });
        const expected = modelLevel(model, watched);
        expect(await storeLevel(store, watched)).toBe(expected);
        const holder = model.find((entry) => entry.watched?.equals(watched));
        expect(
          holder === undefined ? "local" : heads.levelOf(holder.block.height),
        ).toBe(expected);
        if (previous !== expected)
          transitions.add(`${String(previous)}->${String(expected)}`);
        previous = expected;
        // A final tx is beyond every legal rollback: watch a new one.
        if (expected === "final") {
          watched = rng.bytes(32);
          previous = levelOf(null, PARAMETERS, true);
        }
      }
    } finally {
      await store.close();
    }
    // Landing, deepening, a shallow rollback that keeps the tx but drops it
    // below cd, a rollback that removes it from each chain level below final,
    // and re-landing must all have happened; nothing ever leaves final.
    expect([...transitions].sort()).toEqual(
      [
        "landed->local",
        "landed->safe",
        "local->landed",
        "safe->final",
        "safe->landed",
        "safe->local",
      ].sort(),
    );
  });

  it("final is beyond every rollback the store accepts", async () => {
    const store = openSqliteFactStore({
      ...options(K),
      path: join(scratch, "heads-final.db"),
    });
    const rng = new Rng(7);
    const noWatched = null;
    try {
      await store.start();
      await store.initialize(ORIGIN);
      let parent = { ...ORIGIN.point, height: ORIGIN.height };
      const blocks: BlockSummary[] = [];
      for (let i = 0; i < K + 2; i += 1) {
        const block = makeBlock(rng, parent, noWatched);
        await store.applyBlock(block);
        blocks.push(block);
        parent = { ...block.point, height: block.height };
      }
      const tip = blocks.at(-1)!.height;
      // A rollback of k blocks is legal and removes the block at depth k.
      const atK = blocks.find((block) => depth(tip, block.height) === K)!;
      expect(isFinal(K, PARAMETERS)).toBe(false);
      const beyond = blocks.find(
        (block) => depth(tip, block.height) === K + 1,
      )!;
      expect(isFinal(K + 1, PARAMETERS)).toBe(true);
      expect(await store.rewind(beyond.point)).toMatchObject({
        kind: "rewound",
        depth: K,
      });
      expect(await store.isCanonical(atK.point.hash)).toBe(false);
      expect(await store.isCanonical(beyond.point.hash)).toBe(true);
    } finally {
      await store.close();
    }
  });
});
