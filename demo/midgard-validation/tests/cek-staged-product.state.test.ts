import { Constr } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { midgardCekStagedProductLoop } from "../src/cek-staged-product.hops.js";
import {
  midgardCekStagedProductStateData,
  midgardCekStagedProductStates,
} from "../src/cek-staged-product.state.js";

/**
 * The states an honest prover hands from hop to hop of the staged product
 * loop: one a column, and after the last column the state the loop's final
 * checks take (`advance_product_v1`): nothing carried into the claimed
 * result, and for a division no borrow out of the remainder, which is below
 * the divisor.
 */

/** 2^(8 * bytes - 1) + low: an integer of `bytes` magnitude bytes. */
const wide = (bytes: number, low = 0n): bigint =>
  (1n << BigInt(8 * bytes - 1)) + low;

const A = wide(3_000, 12_345n);
const B = wide(1_100, 77n);
const TOP = (1n << 262_143n) - 1n;

const NAMES = {
  2: "multiplyInteger",
  3: "divideInteger",
  4: "quotientInteger",
  5: "remainderInteger",
  6: "modInteger",
} as const;

const cases = ([2n, 3n, 4n, 5n, 6n] as const).flatMap((tag) =>
  (
    [
      [A, B],
      [-A, B],
      [A, -B],
      [-A, -B],
      [B, A],
      [-B, A],
      [TOP, wide(8_192)],
      [-TOP, -wide(8_192, 1n)],
      [A * B, B],
      [-(A * B), B],
      [A * B + 1n, -B],
      [1n, 1n],
      [0n, -B],
    ] as const
  ).map(
    ([a, b]) =>
      [
        `${NAMES[Number(tag) as keyof typeof NAMES]} of ${a < 0n ? "-" : ""}${(a < 0n ? -a : a).toString(16).length.toString()} hex digits by ${b < 0n ? "-" : ""}${(b < 0n ? -b : b).toString(16).length.toString()}`,
        tag,
        a,
        b,
      ] as const,
  ),
);

describe("the staged product loop's states", () => {
  it.each(cases)(
    "%s: a state a column, ending in the state the final checks take",
    (_, tag, a, b) => {
      const loop = midgardCekStagedProductLoop(tag, a, b)!;
      const states = midgardCekStagedProductStates(tag, a, b);
      expect(states).toHaveLength(loop.columns);
      const last = states.at(-1)!;
      expect(last.carry).toBe(0n);
      expect(last.increment).toBe(0n);
      if (tag !== 2n) {
        expect(last.borrow).toBe(0n);
        expect(last.order).toBe(-1n);
      }
      for (const state of states) {
        expect([0n, 1n]).toContain(state.borrow);
        expect([0n, 1n]).toContain(state.increment);
        expect([-1n, 0n, 1n]).toContain(state.order);
      }
    },
  );

  it("carries one into a negative result's limbs until a limb takes it", () => {
    // -2^4096 * 1: |n| - 1 spells 4,096 one bits, a whole limb of ones, so
    // the one is carried out of the first limb into the second, of the
    // three columns a two-limb by one-limb product takes.
    const states = midgardCekStagedProductStates(2n, -(1n << 4_096n), 1n);
    expect(states.map(({ increment }) => increment)).toEqual([1n, 0n, 0n]);
  });

  it("has no states for a division by zero or another builtin", () => {
    expect(() => midgardCekStagedProductStates(3n, A, 0n)).toThrow();
    expect(() => midgardCekStagedProductStates(0n, A, B)).toThrow();
  });

  it("spells a state as ProductStateV1", () => {
    expect(
      midgardCekStagedProductStateData({
        carry: 5n,
        borrow: 1n,
        order: -1n,
        increment: 0n,
      }),
    ).toEqual(new Constr(0, [5n, 1n, -1n, 0n]));
  });
});
