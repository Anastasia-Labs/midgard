import {
  DataB,
  DataConstr,
  DataI,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import { evaluateMidgardCekPv11Builtin } from "../src/cek-builtin.pv11.js";
import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import {
  integer,
  integers,
  PV11_CASES,
} from "./fixtures/cek-builtin-pv11.cases.js";

describe("Cardano PV 11 builtin semantics", () => {
  it("covers every PV 11 addition", () => {
    expect(
      [...new Set(PV11_CASES.map(({ tag }) => Number(tag)))].sort(
        (a, b) => a - b,
      ),
    ).toEqual(Array.from({ length: 14 }, (_, index) => index + 87));
  });

  it.each(PV11_CASES)("$name", ({ tag, arguments: arguments_, expected }) => {
    const evaluated = evaluateMidgardCekPv11Builtin(tag, arguments_);
    if (expected === "failure") {
      expect(evaluated.kind).toBe("failure");
      return;
    }
    expect(evaluated.kind).toBe("success");
    if (evaluated.kind !== "success")
      throw new Error("expected builtin result");
    const result =
      typeof expected === "boolean"
        ? {
            type: { kind: "boolean" },
            payload: new DataConstr(expected ? 1 : 0, []),
          }
        : expected;
    expect(evaluated.result.type).toEqual(result.type);
    expect(encodeMidgardCekPlutusData(evaluated.result.payload)).toEqual(
      encodeMidgardCekPlutusData(result.payload),
    );
  });

  it.each([
    ["expMod positive power", 891_994n, 1n],
    ["expMod base lower bound", 1_337_991n, 1n],
    ["dropList negative", 120_625n, 4n],
    ["array length", 231_883n, 10n],
    ["listToArray", 75_514n, 10n],
    ["indexArray", 232_010n, 32n],
    ["blsG1 MSM identity", 346_925_113n, 18n],
    ["blsG2 MSM identity", 685_190_255n, 36n],
    ["insertCoin replaces", 393_750n, 87n],
    ["lookupCoin found", 238_839n, 1n],
    ["unionValue combines", 539_428n, 87n],
    ["valueContains true", 648_657n, 1n],
    ["valueContains false", 213_283n, 1n],
    ["valueData", 39_159n, 24n],
    ["unValueData", 480_690n, 56n],
    ["scaleValue positive", 278_577n, 33n],
  ] as const)("pins the PV 11 budget for %s", (name, cpu, memory) => {
    const entry = PV11_CASES.find((candidate) => candidate.name === name)!;
    expect(
      evaluateMidgardCekPv11Builtin(entry.tag, entry.arguments).budget,
    ).toEqual({ cpu, memory });
  });

  it("charges every scalar in MSM even when the point list is shorter", () => {
    const entry = PV11_CASES.find(
      ({ name }) => name === "blsG1 MSM unused scalar upper outside",
    )!;
    expect(evaluateMidgardCekPv11Builtin(entry.tag, entry.arguments)).toEqual({
      kind: "failure",
      budget: { cpu: 372_012_782n, memory: 18n },
    });
  });

  it("charges indexArray denotation failures but not Int64 unlifting failures", () => {
    for (const index of [-1n, -(1n << 63n), (1n << 63n) - 1n]) {
      expect(
        evaluateMidgardCekPv11Builtin(91n, [
          integers([], "array"),
          integer(index),
        ]),
      ).toEqual({ kind: "failure", budget: { cpu: 232_010n, memory: 32n } });
    }
    for (const index of [-(1n << 63n) - 1n, 1n << 63n]) {
      expect(
        evaluateMidgardCekPv11Builtin(91n, [
          integers([], "array"),
          integer(index),
        ]),
      ).toEqual({ kind: "failure", budget: { cpu: 0n, memory: 0n } });
    }
  });

  it.each([40_000, 40_001])("valueData entry bound at %i", (count) => {
    const payload = new DataMap([
      new DataPair(
        new DataB(new Uint8Array()),
        new DataMap(
          Array.from({ length: count }, (_, index) => {
            const token = Buffer.alloc(4);
            token.writeUInt32BE(index);
            return new DataPair(new DataB(token), new DataI(1n));
          }),
        ),
      ),
    ]);
    const result = evaluateMidgardCekPv11Builtin(98n, [
      { type: { kind: "value" }, payload },
    ]);
    expect(result.kind).toBe(count === 40_000 ? "success" : "failure");
    expect(result.budget.cpu).toBeGreaterThan(0n);
  });
});
