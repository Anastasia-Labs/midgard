import {
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import {
  midgardCekConstantMemorySize,
  midgardCekDataMemorySize,
  midgardCekIntegerMemorySize,
} from "../src/cek-constant.js";
import { computeMidgardCekBuiltinBudget } from "../src/cek-cost.js";

describe("V1 pinned Plutus V3 builtin costs", () => {
  it("matches integer, byte-string, and BLS cost-model vectors", () => {
    expect(computeMidgardCekBuiltinBudget(0, [1n, 2n])).toEqual({
      cpu: 101_628n,
      memory: 3n,
    });
    expect(computeMidgardCekBuiltinBudget(18, [32n])).toEqual({
      cpu: 993_468n,
      memory: 4n,
    });
    expect(computeMidgardCekBuiltinBudget(68, [48n, 96n])).toEqual({
      cpu: 254_006_273n,
      memory: 72n,
    });
    expect(computeMidgardCekBuiltinBudget(70, [192n, 192n])).toEqual({
      cpu: 333_849_714n,
      memory: 1n,
    });
  });

  it("costs multiplyInteger cpu over the product of the argument sizes", () => {
    expect(computeMidgardCekBuiltinBudget(2, [4n, 3n])).toEqual({
      cpu: 96_662n,
      memory: 7n,
    });
    expect(computeMidgardCekBuiltinBudget(2, [1n, 1n])).toEqual({
      cpu: 90_953n,
      memory: 2n,
    });
  });

  it("fails closed on unknown tags, wrong arity, or negative sizes", () => {
    expect(() => computeMidgardCekBuiltinBudget(87, [1n])).toThrow(
      /outside Plutus V3/u,
    );
    expect(() => computeMidgardCekBuiltinBudget(0, [1n])).toThrow(
      /requires 2 cost sizes/u,
    );
    expect(() => computeMidgardCekBuiltinBudget(18, [-1n])).toThrow(
      /must be non-negative/u,
    );
  });

  it("sizes integers and byte strings in 64-bit words, as cardano-node does", () => {
    const word = 1n << 64n;
    expect(
      [-word, 1n - word, -129n, -1n, 0n, 128n, word - 1n, word].map(
        midgardCekIntegerMemorySize,
      ),
    ).toEqual([2n, 1n, 1n, 1n, 1n, 1n, 1n, 2n]);
    expect(
      [0, 1, 8, 9].map((length) =>
        midgardCekConstantMemorySize(
          { kind: "bytes" },
          new DataB(new Uint8Array(length)),
        ),
      ),
    ).toEqual([1n, 1n, 1n, 2n]);
  });

  it("sums list/pair payloads and charges four words per Data node", () => {
    const list = new DataList([new DataI(1n), new DataI(256n)]);
    expect(
      midgardCekConstantMemorySize(
        { kind: "list", element: { kind: "integer" } },
        list,
      ),
    ).toBe(2n);

    const pair = new DataConstr(0, [
      new DataI(1n),
      new DataB(new Uint8Array(4)),
    ]);
    expect(
      midgardCekConstantMemorySize(
        {
          kind: "pair",
          first: { kind: "integer" },
          second: { kind: "bytes" },
        },
        pair,
      ),
    ).toBe(2n);

    const data = new DataMap([
      { fst: new DataI(1n), snd: new DataList([new DataB(new Uint8Array(2))]) },
    ]);
    expect(midgardCekDataMemorySize(data)).toBe(18n);
  });
});
