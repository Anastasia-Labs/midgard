import { describe, expect, it } from "vitest";

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
    expect(aikenSerialisedPlutusDataCbor(assetThenAda)).toBe(
      "a24002411101",
    );
    expect(
      aikenSerialisedPlutusDataCborPreservingMapOrder(assetThenAda),
    ).toBe("a24111014002");
  });
});
