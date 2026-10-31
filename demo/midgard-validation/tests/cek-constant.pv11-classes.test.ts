import {
  hashMidgardCekTermNode,
  MIDGARD_CEK_MAX_CONSTR_FIELDS,
  midgardCekTermExceedsConstrFields,
  midgardCekTermMinimumVersionMinor,
  type MidgardCekTermNode,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  MIDGARD_CEK_MAX_CONSTANT_TYPE_FLAT_SIZE,
  midgardCekConstantHoldsBlsValue,
  type MidgardCekConstantType,
  midgardCekConstantTypeFlatSize,
} from "../src/cek-constant.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "../src/uplc-data.js";

const G1: MidgardCekConstantType = { kind: "blsG1" };
const G2: MidgardCekConstantType = { kind: "blsG2" };
const INTEGER: MidgardCekConstantType = { kind: "integer" };
const list = (element: MidgardCekConstantType): MidgardCekConstantType => ({
  kind: "list",
  element,
});
const array = (element: MidgardCekConstantType): MidgardCekConstantType => ({
  kind: "array",
  element,
});
const pair = (
  first: MidgardCekConstantType,
  second: MidgardCekConstantType,
): MidgardCekConstantType => ({ kind: "pair", first, second });

const point = new DataB(Buffer.alloc(48, 0xc0));
const items = (...values: Data[]) => new DataList(values);
const both = (first: Data, second: Data) => new DataConstr(0n, [first, second]);

describe("PV 11 constant classes", () => {
  it("sizes constant types on both sides of the 32-tag bound", () => {
    expect(MIDGARD_CEK_MAX_CONSTANT_TYPE_FLAT_SIZE).toBe(32);
    // list^k integer has 2k + 1 tags: k = 15 sits at 31, k = 16 is 33.
    let type: MidgardCekConstantType = INTEGER;
    for (let depth = 0; depth < 15; depth += 1) type = list(type);
    expect(midgardCekConstantTypeFlatSize(type)).toBe(31);
    expect(midgardCekConstantTypeFlatSize(pair(type, INTEGER))).toBe(35);
    expect(midgardCekConstantTypeFlatSize(list(type))).toBe(33);
    expect(
      midgardCekConstantTypeFlatSize(
        pair(list(list(list(INTEGER))), list(type)),
      ),
    ).toBe(3 + 7 + 33);
  });

  it.each([
    ["a G1 constant", G1, point],
    ["a G2 constant", G2, new DataB(Buffer.alloc(96, 0xc0))],
    ["a BLS pair component", pair(INTEGER, G1), both(new DataI(1n), point)],
    ["a non-empty BLS list", list(G1), items(point)],
    ["a non-empty BLS array", array(G1), items(point)],
    [
      "a BLS element of a nested list",
      list(list(G1)),
      items(items(), items(point)),
    ],
    [
      "a BLS pair inside a list",
      list(pair(G1, INTEGER)),
      items(both(point, new DataI(1n))),
    ],
  ])("finds the BLS value in %s", (_name, type, payload) => {
    expect(midgardCekConstantHoldsBlsValue(type, payload)).toBe(true);
  });

  it.each([
    ["an empty BLS list", list(G1), items()],
    ["an empty BLS array", array(G2), items()],
    ["a list of empty BLS lists", list(list(G1)), items(items(), items())],
    [
      "a pair whose BLS side is an empty list",
      pair(list(G1), INTEGER),
      both(items(), new DataI(1n)),
    ],
    ["an integer list", list(INTEGER), items(new DataI(1n))],
  ])("finds no BLS value in %s", (_name, type, payload) => {
    expect(midgardCekConstantHoldsBlsValue(type, payload)).toBe(false);
  });

  it("bounds a constr at 1,024 fields and needs 1.1.0 for constr and case", () => {
    const fields = hashMidgardCekTermNode({ kind: "error" });
    const constr = (termsCount: bigint): MidgardCekTermNode => ({
      kind: "constr",
      tag: 0n,
      termsCount,
      termsRoot: fields,
    });
    expect(MIDGARD_CEK_MAX_CONSTR_FIELDS).toBe(1_024n);
    expect(midgardCekTermExceedsConstrFields(constr(1_024n))).toBe(false);
    expect(midgardCekTermExceedsConstrFields(constr(1_025n))).toBe(true);
    expect(midgardCekTermMinimumVersionMinor(constr(0n))).toBe(1n);
    expect(
      midgardCekTermMinimumVersionMinor({
        kind: "case",
        scrutinee: fields,
        branchesCount: 1n,
        branchesRoot: fields,
      }),
    ).toBe(1n);
    expect(midgardCekTermMinimumVersionMinor({ kind: "error" })).toBe(0n);
    expect(
      midgardCekTermMinimumVersionMinor({ kind: "builtin", tag: 100n }),
    ).toBe(0n);
  });
});
