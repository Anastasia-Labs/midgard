import { describe, expect, it } from "vitest";

import {
  buildMidgardCekDataTraverseTrace,
  finalizeMidgardCekDataTraverse,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
  nextMidgardCekDataTraverseSpan,
} from "../src/cek-data-traverse-v1.js";
import {
  aikenSerialisedPlutusDataCbor,
  aikenSerialisedPlutusDataCborPreservingMapOrder,
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
