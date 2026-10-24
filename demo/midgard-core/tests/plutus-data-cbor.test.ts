import { describe, expect, it } from "vitest";

import {
  buildMidgardCekDataTraverseTrace,
  finalizeMidgardCekDataTraverse,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
  nextMidgardCekDataTraverseSpan,
} from "../src/cek-data-traverse.js";
import {
  aikenSerialisedPlutusDataCbor,
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  compactPlutusDataCarriageCbor,
  countPlutusDataCborNodes,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "../src/plutus-data-cbor.js";

const payload = Buffer.alloc(96, 0xab);
const aikenBytes = Buffer.concat([
  Buffer.from([0x5f, 0x58, 0x40]),
  payload.subarray(0, 64),
  Buffer.from([0x58, 0x20]),
  payload.subarray(64),
  Buffer.from([0xff]),
]).toString("hex");

describe("Aiken PlutusData serialization", () => {
  // Semantic encoding rules from PlutusCore/Data.hs; reference-runtime parity
  // is recorded separately from these regression vectors.
  it.each([
    ["c240", "00"],
    ["c24100", "00"],
    ["c24101", "01"],
    ["c2420001", "01"],
    ["c340", "20"],
    ["c34100", "20"],
    ["c34101", "21"],
    ["c3420001", "21"],
    ["c248ffffffffffffffff", "1bffffffffffffffff"],
    ["c348ffffffffffffffff", "3bffffffffffffffff"],
    ["c249010000000000000000", "c249010000000000000000"],
    ["c24a00010000000000000000", "c249010000000000000000"],
    ["c35f410049010000000000000000ff", "c349010000000000000000"],
    ["d866820080", "d87980"],
    ["d8669f0080ff", "d87980"],
    ["d866820680", "d87f80"],
    ["d866820780", "d9050080"],
    ["d86682187f80", "d9057880"],
    ["d8669f188080ff", "d86682188080"],
    ["a302c2410101d86682008002c2420001", "a3020101d879800201"],
    ["d866820081c24101", "d8799f01ff"],
  ])("normalizes semantic Data framing %s", (source, expected) => {
    expect(aikenSerialisedPlutusDataCborPreservingMapOrder(source)).toBe(
      expected,
    );
    expect(aikenSerialisedPlutusDataCborPreservingMapOrder(expected)).toBe(
      expected,
    );
  });

  it("counts every raw map pair and only logical constructor/integer nodes", () => {
    const repeated = "a302c2410101d86682008002c2420001";
    expect(countPlutusDataCborNodes(repeated, 7n)).toBe(7n);
    expect(() => countPlutusDataCborNodes(repeated, 6n)).toThrow(
      "Data-node bound",
    );
    expect(countPlutusDataCborNodes(`d866820081${repeated}`, 8n)).toBe(8n);
    const deep = `${"d8799f".repeat(4000)}00${"ff".repeat(4000)}`;
    expect(countPlutusDataCborNodes(deep, 4001n)).toBe(4001n);
  });

  it.each([
    "00zz",
    "0",
    "",
    "c200",
    "c400",
    "d8668100",
    "d866822080",
    "d866820000",
    "d87900",
  ])("rejects invalid Data shape %s", (source) => {
    expect(() =>
      aikenSerialisedPlutusDataCborPreservingMapOrder(source),
    ).toThrow();
    expect(() => countPlutusDataCborNodes(source, 100n)).toThrow();
  });

  it("replaces nested Data fields without changing map order or repeated keys", () => {
    const original = "d8799f01d87a9f0203ff04ff";
    const rawMap = "a3020001010202";
    const replaced = replacePlutusConstrFieldCbor(original, [1, 0], rawMap);
    expect(replaced).toBe("d8799f01d87a9fa302000101020203ff04ff");
    expect(plutusConstrFieldCbor(replaced, [1, 0])).toBe(rawMap);
    expect(plutusConstrFieldCbor(replaced, [1, 1])).toBe("03");
    expect(replacePlutusConstrFieldCbor(replaced, [1, 0], "02")).toBe(original);
  });

  it.each([
    ["d8799f00ff", [0], "00ff"],
    ["d8799f00ffff", [0], "00"],
    ["d8799f00ff", [-1], "00"],
    ["d8799f00ff", [0.5], "00"],
    ["d8799f00ff", [1], "00"],
    ["d8799f00ff", [0, 0], "00"],
    ["d8799f00ff", [0], "gg"],
  ] as const)(
    "refuses invalid raw replacement %s/%s/%s",
    (original, path, replacement) => {
      expect(() =>
        replacePlutusConstrFieldCbor(original, path, replacement),
      ).toThrow();
    },
  );

  it("accepts and canonically chunks bytestrings larger than 64 bytes", () => {
    expect(aikenSerialisedPlutusDataCbor(aikenBytes)).toBe(aikenBytes);
    expect(
      aikenSerialisedPlutusDataCbor(
        Buffer.concat([Buffer.from([0x58, 0x60]), payload]).toString("hex"),
      ),
    ).toBe(aikenBytes);
  });

  it("rejects malformed indefinite bytestring chunks", () => {
    expect(() => aikenSerialisedPlutusDataCbor("5f8101ff")).toThrow(
      /only definite byte chunks/u,
    );
  });

  it("distinguishes typed map sorting from raw Data map order", () => {
    const assetThenAda = "bf4111014002ff";
    expect(aikenSerialisedPlutusDataCbor(assetThenAda)).toBe("a24002411101");
    expect(aikenSerialisedPlutusDataCborPreservingMapOrder(assetThenAda)).toBe(
      "a24111014002",
    );
  });

  it("normalizes and traverses a unary depth beyond the former host stack ceiling", () => {
    const depth = 4_000;
    const unary = `${"9f".repeat(depth)}00${"ff".repeat(depth)}`;
    expect(aikenSerialisedPlutusDataCborPreservingMapOrder(unary)).toBe(unary);
    expect(aikenSerialisedPlutusDataCbor(unary)).toBe(unary);

    const trace = buildMidgardCekDataTraverseTrace({
      sourceStart: 0,
      source: Buffer.from(unary, "hex"),
    });
    const terminal = finalizeMidgardCekDataTraverse(trace.terminal);
    expect(terminal).not.toBeNull();
    expect(terminal!.cborLength).toBe(BigInt(unary.length / 2));
    expect(
      trace.steps.reduce(
        (maximum, { control }) =>
          Math.max(
            maximum,
            nextMidgardCekDataTraverseSpan(control)?.length ?? 0,
          ),
        0,
      ),
    ).toBeLessThanOrEqual(MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN);
  });

  it("still rejects trailing, broken, and truncated CBOR", () => {
    expect(() => aikenSerialisedPlutusDataCbor("00ff")).toThrow(
      /trailing bytes/u,
    );
    expect(() => aikenSerialisedPlutusDataCbor("ff")).toThrow(/break marker/u);
    expect(() => aikenSerialisedPlutusDataCbor("9f00")).toThrow(
      /Unexpected end/u,
    );
    expect(() => aikenSerialisedPlutusDataCbor("bf00ff")).toThrow(
      /missing a value/u,
    );
  });
});

describe("referenced Plutus Data transport", () => {
  it("uses one definite byte string without changing nested map order", () => {
    const bytes = Buffer.alloc(32768, 0xab).toString("hex");
    const source = `d8799fa241ff01420000025f${Array.from({ length: 512 }, () => "5840" + "ab".repeat(64)).join("")}ffff`;
    const compact = compactPlutusDataCarriageCbor(source);
    expect(compact).toBe(`d8799fa241ff0142000002598000${bytes}ff`);
    expect(aikenSerialisedPlutusDataCborPreservingMapOrder(compact)).toBe(
      source,
    );
    expect(compactPlutusDataCarriageCbor(compact)).toBe(compact);
  });
  it("rejects a trailing item instead of dropping it", () => {
    expect(() => compactPlutusDataCarriageCbor("0000")).toThrow(/Trailing/);
  });
});
