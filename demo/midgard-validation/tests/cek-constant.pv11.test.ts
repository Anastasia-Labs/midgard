import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import {
  decodeMidgardCekConstantTypeCbor,
  decodeMidgardCekConstantWitness,
  encodeMidgardCekConstantTypeCbor,
  encodeMidgardCekPlutusData,
  hashMidgardCekConstantWitness,
  MIDGARD_CEK_MAX_VALUE_QUANTITY,
  MIDGARD_CEK_MIN_VALUE_QUANTITY,
  type MidgardCekConstantType,
  midgardCekConstantTypeFlatSize,
} from "../src/cek-constant.js";

const bytes = (hex: string): DataB => new DataB(Buffer.from(hex, "hex"));
const tokens = (...entries: [string, bigint][]): DataMap<Data, Data> =>
  new DataMap(
    entries.map(
      ([name, amount]) => new DataPair(bytes(name), new DataI(amount)),
    ),
  );
const value = (...entries: [string, Data][]): DataMap<Data, Data> =>
  new DataMap(
    entries.map(([policy, inner]) => new DataPair(bytes(policy), inner)),
  );

const policy = "aa".repeat(28);
const canonicalValue = value(
  ["", tokens(["", 1_000_000n])],
  [policy, tokens(["01", 5n], ["0102", -3n])],
);

const VALUE: MidgardCekConstantType = { kind: "value" };
const ARRAY_OF_INTEGERS: MidgardCekConstantType = {
  kind: "array",
  element: { kind: "integer" },
};

const witness = (type: MidgardCekConstantType, payload: Data) => ({
  typeCbor: encodeMidgardCekConstantTypeCbor(type),
  payloadCbor: encodeMidgardCekPlutusData(payload),
});

const hex = (input: Uint8Array): string => Buffer.from(input).toString("hex");

describe("PV 11 Value and array constants", () => {
  it("commits Value and array types under Cardano's tags 13 and 12", () => {
    expect(hex(encodeMidgardCekConstantTypeCbor(VALUE))).toBe("9f0dff");
    expect(hex(encodeMidgardCekConstantTypeCbor(ARRAY_OF_INTEGERS))).toBe(
      "9f0c00ff",
    );
    expect(
      hex(encodeMidgardCekConstantTypeCbor({ kind: "list", element: VALUE })),
    ).toBe("9f050dff");
    for (const type of [
      VALUE,
      ARRAY_OF_INTEGERS,
      { kind: "array", element: { kind: "array", element: VALUE } } as const,
    ]) {
      expect(
        decodeMidgardCekConstantTypeCbor(
          encodeMidgardCekConstantTypeCbor(type),
        ),
      ).toEqual(type);
    }
    expect(() =>
      decodeMidgardCekConstantTypeCbor(Buffer.from("9f0eff", "hex")),
    ).toThrow(/unknown tag/u);
    expect(() =>
      decodeMidgardCekConstantTypeCbor(Buffer.from("9f07ff", "hex")),
    ).toThrow(/unknown tag/u);
  });

  it("commits a canonical Value with its entry count as memory", () => {
    const constant = witness(VALUE, canonicalValue);
    expect(hex(constant.payloadCbor)).toBe(
      "a240a1401a000f4240581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa241010542010222",
    );
    expect(decodeMidgardCekConstantWitness(constant).type).toEqual(VALUE);
    expect(hex(hashMidgardCekConstantWitness(constant))).toBe(
      "927aa3834dec4fd50eeec6ae8276d3496c323129be9af04ba291809b0631d8bd",
    );
    const empty = witness(VALUE, value());
    expect(hex(empty.payloadCbor)).toBe("a0");
    expect(hex(hashMidgardCekConstantWitness(empty))).toBe(
      "4a0ee6f6379d9f5a8aa40f228cc79a5594238830dedfb6bfea54973ed52c1eb8",
    );
  });

  it("admits the Value bounds Cardano admits", () => {
    for (const payload of [
      value(["aa".repeat(32), tokens(["bb".repeat(32), 1n])]),
      value([policy, tokens(["", MIDGARD_CEK_MAX_VALUE_QUANTITY])]),
      value([policy, tokens(["", MIDGARD_CEK_MIN_VALUE_QUANTITY])]),
      // A proper prefix sorts first, as in Haskell's ByteString order.
      value(["01", tokens(["", 1n])], ["0100", tokens(["", 1n])]),
      value([policy, tokens(["ff", 1n], ["ff00", 2n])]),
    ]) {
      expect(() =>
        decodeMidgardCekConstantWitness(witness(VALUE, payload)),
      ).not.toThrow();
    }
  });

  it.each([
    ["a 33-byte currency symbol", value(["aa".repeat(33), tokens(["", 1n])])],
    ["a 33-byte token name", value([policy, tokens(["bb".repeat(33), 1n])])],
    [
      "a quantity above the signed 128-bit range",
      value([policy, tokens(["", MIDGARD_CEK_MAX_VALUE_QUANTITY + 1n])]),
    ],
    [
      "a quantity below the signed 128-bit range",
      value([policy, tokens(["", MIDGARD_CEK_MIN_VALUE_QUANTITY - 1n])]),
    ],
    ["a zero quantity", value([policy, tokens(["", 0n])])],
    ["an empty token map", value([policy, tokens()])],
    [
      "currency symbols out of order",
      value(["02", tokens(["", 1n])], ["01", tokens(["", 1n])]),
    ],
    [
      "a repeated currency symbol",
      value(["01", tokens(["", 1n])], ["01", tokens(["01", 1n])]),
    ],
    [
      "a prefix after its extension",
      value(["0100", tokens(["", 1n])], ["01", tokens(["", 1n])]),
    ],
    [
      "token names out of order",
      value([policy, tokens(["02", 1n], ["01", 1n])]),
    ],
    ["a repeated token name", value([policy, tokens(["01", 1n], ["01", 2n])])],
    ["a list payload", new DataList([])],
    [
      "an integer currency symbol",
      new DataMap([new DataPair(new DataI(1n), tokens(["", 1n]))]),
    ],
    [
      "a token map that is not a map",
      new DataMap([new DataPair(bytes(""), new DataList([]))]),
    ],
    [
      "a quantity that is not an integer",
      value([policy, new DataMap([new DataPair(bytes(""), bytes("01"))])]),
    ],
  ])("refuses a Value with %s", (_name, payload) => {
    expect(() =>
      decodeMidgardCekConstantWitness(witness(VALUE, payload)),
    ).toThrow(/does not match its type/u);
  });

  it("commits an array like a list, with its length as memory", () => {
    const items = new DataList([
      new DataI(1n),
      new DataI(1_000n),
      new DataI(-5n),
    ]);
    const array = witness(ARRAY_OF_INTEGERS, items);
    expect(hex(array.payloadCbor)).toBe("9f011903e824ff");
    expect(decodeMidgardCekConstantWitness(array).payload).toEqual(items);
    expect(hex(hashMidgardCekConstantWitness(array))).toBe(
      "b806d66b21c3df91b6272fd7b367f8d4c07612f6fe73fa08187557b2fbeddcbb",
    );
    expect(hashMidgardCekConstantWitness(array)).not.toEqual(
      hashMidgardCekConstantWitness(
        witness({ kind: "list", element: { kind: "integer" } }, items),
      ),
    );
    expect(() =>
      decodeMidgardCekConstantWitness(
        witness(ARRAY_OF_INTEGERS, new DataList([bytes("01")])),
      ),
    ).toThrow(/does not match its type/u);
    expect(() =>
      decodeMidgardCekConstantWitness(
        witness(ARRAY_OF_INTEGERS, new DataConstr(0n, [])),
      ),
    ).toThrow(/does not match its type/u);
  });

  it("sizes a type by Cardano's flat tag count", () => {
    expect(midgardCekConstantTypeFlatSize({ kind: "integer" })).toBe(1);
    expect(midgardCekConstantTypeFlatSize(VALUE)).toBe(1);
    expect(
      midgardCekConstantTypeFlatSize({
        kind: "list",
        element: { kind: "integer" },
      }),
    ).toBe(3);
    expect(midgardCekConstantTypeFlatSize(ARRAY_OF_INTEGERS)).toBe(3);
    expect(
      midgardCekConstantTypeFlatSize({
        kind: "pair",
        first: { kind: "integer" },
        second: { kind: "data" },
      }),
    ).toBe(5);
  });
});
