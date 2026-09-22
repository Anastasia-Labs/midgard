import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import { describe, expect, it } from "vitest";

import {
  decodeScriptDiscoveryBitmap,
  encodeScriptDiscoveryBitmap,
  encodeScriptDiscoveryControlCbor,
} from "../src/validation-machine/control-encoding.js";

describe("script discovery bitmap wire", () => {
  it.each([
    0n,
    6n,
    (1n << 64n) - 1n,
    1n << 64n,
    (1n << 1092n) - 1n,
    (1n << 16384n) - 1n,
  ])("round trips the canonical bounded bitmap %s", (bitmap) => {
    const encoded = encodeScriptDiscoveryBitmap(bitmap);
    expect(decodeScriptDiscoveryBitmap(encoded)).toBe(bitmap);
    const wire = encodeScriptDiscoveryControlCbor({
      purposeCursor: 0,
      sourceCursor: 0,
      redeemerCursor: 0,
      currentPurposeKind: -1,
      currentPurposeIndex: -1n,
      currentScriptHash: Buffer.alloc(0),
      currentSubject: Buffer.alloc(0),
      matchedSourceIndex: -1,
      matchedLanguageTag: -1,
      matchedSourceLeaf: Buffer.alloc(0),
      usedInlineBitmap: bitmap,
      usedRedeemerBitmap: bitmap,
      redeemerItemControlHash: Buffer.alloc(0),
      executionFrontier: { count: 0, peaks: [] },
    });
    const fields = decodeSingleCbor(wire);
    expect(Array.isArray(fields)).toBe(true);
    if (!Array.isArray(fields)) throw new Error("expected control list");
    expect(decodeScriptDiscoveryBitmap(fields[10])).toBe(bitmap);
    expect(decodeScriptDiscoveryBitmap(fields[11])).toBe(bitmap);
    expect(encoded.length).toBeLessThanOrEqual(2048);
  });
  it("pins zero and byte order", () => {
    expect(encodeScriptDiscoveryBitmap(0n).toString("hex")).toBe("");
    expect(encodeScriptDiscoveryBitmap(256n).toString("hex")).toBe("0100");
  });
  it.each([
    0n,
    -1n,
    Buffer.from([0]),
    Buffer.from([0, 1]),
    Buffer.alloc(2049, 1),
  ])("refuses noncanonical or oversized decoded data", (value) => {
    expect(() => decodeScriptDiscoveryBitmap(value)).toThrow();
  });
  it.each([-1n, 1n << 16384n])("refuses out-of-domain integers", (value) => {
    expect(() => encodeScriptDiscoveryBitmap(value)).toThrow();
  });
});
