import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import { toCostModelV3 } from "@harmoniclabs/cardano-costmodels-ts";
import {
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { Machine } from "@harmoniclabs/plutus-machine";
import { Application, Builtin, UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  midgardCekConstantMemorySize,
  midgardCekDataMemorySize,
  midgardCekIntegerMemorySize,
} from "../src/cek-constant.js";
import {
  computeMidgardCekBuiltinBudget,
  normalizeMidgardCekBitwiseCostSizes,
} from "../src/cek-cost.js";

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

  it("matches the reference CEK's total budget for a concrete program", () => {
    const program = new Application(
      new Application(Builtin.addInteger, UPLCConst.int(1)),
      UPLCConst.int(128),
    );
    const evaluation = new Machine(
      toCostModelV3([
        ...PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
      ] as unknown as Parameters<typeof toCostModelV3>[0]),
    ).eval(program);
    const builtin = computeMidgardCekBuiltinBudget(0, [1n, 2n]);

    // startup + builtin + two application + two constant CEK nodes
    expect(evaluation.budgetSpent.cpu).toBe(100n + 5n * 16_000n + builtin.cpu);
    expect(evaluation.budgetSpent.mem).toBe(100n + 5n * 100n + builtin.memory);
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

  it("uses signed integer bytes and cardano-node's empty-byte rule", () => {
    expect(
      [-129n, -128n, -1n, 0n, 127n, 128n].map(midgardCekIntegerMemorySize),
    ).toEqual([2n, 1n, 1n, 1n, 1n, 2n]);
    expect(
      midgardCekConstantMemorySize(
        { kind: "bytes" },
        new DataB(new Uint8Array()),
      ),
    ).toBe(1n);
  });

  it("sums list/pair payloads and charges four words per Data node", () => {
    const list = new DataList([new DataI(1n), new DataI(256n)]);
    expect(
      midgardCekConstantMemorySize(
        { kind: "list", element: { kind: "integer" } },
        list,
      ),
    ).toBe(3n);

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
    ).toBe(5n);

    const data = new DataMap([
      new DataPair(new DataI(1n), new DataList([new DataB(new Uint8Array(2))])),
    ]);
    expect(midgardCekDataMemorySize(data)).toBe(19n);
  });

  it("normalizes bitwise arguments after extend/truncate", () => {
    expect(normalizeMidgardCekBitwiseCostSizes(true, 2n, 5n)).toEqual([
      1n,
      5n,
      5n,
    ]);
    expect(normalizeMidgardCekBitwiseCostSizes(false, 2n, 5n)).toEqual([
      1n,
      2n,
      2n,
    ]);
  });
});
