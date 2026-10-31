import { describe, expect, it } from "vitest";

import {
  halveMidgardCekStagedProductHop,
  MIDGARD_CEK_STAGED_PRODUCT_HOP_BUDGET,
  MIDGARD_CEK_STAGED_PRODUCT_MAX_HOP_COLUMNS,
  midgardCekStagedProductHopCost,
  type MidgardCekStagedProductHopPlan,
  type MidgardCekStagedProductLoop,
  midgardCekStagedProductLoop,
  planMidgardCekStagedProductHop,
} from "../src/cek-staged-product.hops.js";

/**
 * The hops of the staged product loop: the planner splits a step's columns
 * into hops the calibrated model fits in a hop's budget with its margin,
 * halves a hop that went over, and resumes from the cursor the chain holds.
 */

/** 2^(8 * bytes - 1): an integer of `bytes` magnitude bytes. */
const wide = (bytes: number): bigint => 1n << BigInt(8 * bytes - 1);

const top = (1n << 262_143n) - 1n;
const bottom = -top - 1n;

/** The end of a planned hop. */
const endOf = (plan: MidgardCekStagedProductHopPlan): number => {
  if (plan.kind !== "hop") throw new Error("indeterminate");
  return plan.end;
};

/** Every hop of `loop`, planned from cursor 0 and then from each hop's end. */
const plan = (loop: MidgardCekStagedProductLoop): number[] => {
  const ends: number[] = [];
  let cursor = 0;
  while (cursor < loop.columns) {
    cursor = endOf(planMidgardCekStagedProductHop(loop, cursor));
    ends.push(cursor);
  }
  return ends;
};

const loopOf = (tag: bigint, a: bigint, b: bigint) => {
  const loop = midgardCekStagedProductLoop(tag, a, b);
  if (loop === null) throw new Error("no loop");
  return loop;
};

const kept = (units: bigint): bigint => (units * 80n) / 100n;

describe("the hops of the staged product loop", () => {
  it.each([
    [
      "multiplyInteger of 9,216 by 4,000 bytes",
      2n,
      wide(9_216),
      -wide(4_000),
      26,
    ],
    ["multiplyInteger of the largest arguments", 2n, top, top, 128],
    ["multiplyInteger of the smallest arguments", 2n, bottom, bottom, 128],
    ["multiplyInteger by zero", 2n, 0n, -5n, 1],
    ["multiplyInteger of zeros", 2n, 0n, 0n, 1],
    ["divideInteger of the smallest by -1", 3n, bottom, -1n, 65],
    [
      "quotientInteger of 32,768 by 17,000 bytes",
      4n,
      -wide(32_768),
      wide(17_000),
      65,
    ],
    [
      "remainderInteger of 17,000 by 32,768 bytes",
      5n,
      wide(17_000),
      wide(32_768),
      64,
    ],
    ["modInteger of a smaller dividend", 6n, -5n, wide(4_096), 9],
  ] as const)("counts the columns of %s", (_, tag, a, b, columns) => {
    expect(loopOf(tag, a, b).columns).toBe(columns);
  });

  it("has no loop for a division by zero or another builtin", () => {
    for (const tag of [3n, 4n, 5n, 6n]) {
      expect(midgardCekStagedProductLoop(tag, 5n, 0n)).toBeNull();
    }
    expect(midgardCekStagedProductLoop(1n, 5n, 7n)).toBeNull();
  });

  it.each([
    ["the largest product", loopOf(2n, top, top)],
    ["the smallest arguments' product", loopOf(2n, bottom, bottom)],
    [
      "a 32,768 by 17,000-byte division",
      loopOf(4n, -wide(32_768), wide(17_000)),
    ],
    [
      "a 32,768 by 17,000-byte remainder",
      loopOf(5n, -top, wide(17_000) + 12_345n),
    ],
    ["the smallest dividend by -1", loopOf(3n, bottom, -1n)],
    ["a 9,216 by 4,000-byte product", loopOf(2n, wide(9_216), -wide(4_000))],
    ["one column", loopOf(2n, 3n, 5n)],
  ])("covers every column of %s in hops within the budget", (_, loop) => {
    const ends = plan(loop);
    expect(ends.at(-1)).toBe(loop.columns);
    let cursor = 0;
    for (const end of ends) {
      expect(end).toBeGreaterThan(cursor);
      expect(end - cursor).toBeLessThanOrEqual(
        MIDGARD_CEK_STAGED_PRODUCT_MAX_HOP_COLUMNS,
      );
      const cost = midgardCekStagedProductHopCost(loop, cursor, end);
      expect(cost.memory).toBeLessThanOrEqual(
        kept(MIDGARD_CEK_STAGED_PRODUCT_HOP_BUDGET.memory),
      );
      expect(cost.cpu).toBeLessThanOrEqual(
        kept(MIDGARD_CEK_STAGED_PRODUCT_HOP_BUDGET.cpu),
      );
      cursor = end;
    }
  });

  it.each([
    ["the largest product", loopOf(2n, top, top), 31],
    ["the smallest arguments' product", loopOf(2n, bottom, bottom), 31],
    [
      "a 32,768 by 17,000-byte division",
      loopOf(4n, -wide(32_768), wide(17_000)),
      8,
    ],
    ["the smallest dividend by -1", loopOf(3n, bottom, -1n), 5],
    ["a 9,216 by 4,000-byte product", loopOf(2n, wide(9_216), -wide(4_000)), 1],
  ] as const)("plans %s in its calibrated number of hops", (_, loop, hops) => {
    expect(plan(loop)).toHaveLength(hops);
  });

  it("models the first hop's binding of every source", () => {
    const loop = loopOf(2n, top, top);
    const unbound = { ...loop, sourceBytes: 0 };
    expect(loop.sourceBytes).toBeGreaterThan(loop.decodedBytes);
    expect(midgardCekStagedProductHopCost(loop, 0, 1).memory).toBeGreaterThan(
      midgardCekStagedProductHopCost(unbound, 0, 1).memory,
    );
    expect(midgardCekStagedProductHopCost(loop, 1, 2)).toEqual(
      midgardCekStagedProductHopCost(unbound, 1, 2),
    );
  });

  it("plans the same hops each time and resumes from the chain's cursor", () => {
    const loop = loopOf(2n, top, top);
    const ends = plan(loop);
    expect(plan(loop)).toEqual(ends);
    for (const [at, cursor] of ends.slice(0, -1).entries()) {
      expect(planMidgardCekStagedProductHop(loop, cursor)).toEqual({
        kind: "hop",
        end: ends[at + 1],
      });
    }
  });

  it("plans one column when the budget fits only one", () => {
    const loop = loopOf(2n, top, top);
    const one = midgardCekStagedProductHopCost(loop, 10, 11);
    expect(
      planMidgardCekStagedProductHop(loop, 10, {
        memory: (one.memory * 100n) / 80n + 1n,
        cpu: (one.cpu * 100n) / 80n + 1n,
      }),
    ).toEqual({ kind: "hop", end: 11 });
  });

  it("is indeterminate when the budget fits not even one column", () => {
    const loop = loopOf(2n, top, top);
    expect(
      planMidgardCekStagedProductHop(loop, 10, { memory: 1n, cpu: 1n }),
    ).toEqual({ kind: "indeterminate" });
    const one = midgardCekStagedProductHopCost(loop, 10, 11);
    expect(
      planMidgardCekStagedProductHop(loop, 10, {
        ...MIDGARD_CEK_STAGED_PRODUCT_HOP_BUDGET,
        memory: (one.memory * 100n) / 80n - 1n,
      }),
    ).toEqual({ kind: "indeterminate" });
  });

  it("halves a hop that went over budget down to one column", () => {
    expect(halveMidgardCekStagedProductHop(10, 74)).toEqual({
      kind: "hop",
      end: 42,
    });
    expect(halveMidgardCekStagedProductHop(10, 13)).toEqual({
      kind: "hop",
      end: 11,
    });
    expect(halveMidgardCekStagedProductHop(10, 12)).toEqual({
      kind: "hop",
      end: 11,
    });
  });

  it("is indeterminate when a one-column hop went over budget", () => {
    expect(halveMidgardCekStagedProductHop(10, 11)).toEqual({
      kind: "indeterminate",
    });
  });

  it("refuses a cursor outside the loop", () => {
    const loop = loopOf(2n, 3n, 5n);
    for (const cursor of [-1, 2, 0.5]) {
      expect(() => planMidgardCekStagedProductHop(loop, cursor)).toThrow(
        RangeError,
      );
    }
  });
});
