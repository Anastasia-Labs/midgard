import { randomBytes } from "node:crypto";

import { describe, expect, it } from "vitest";

import { compressDaPayloadZstd } from "../src/da-compression.js";
import { wrapDaPayloadV3 } from "../src/da-payload-envelope.js";
import {
  canonicalCborArgumentSize,
  canonicalCborByteStringSize,
  daPayloadEnvelopeV3EncodedSize,
  daPayloadSubmitV1EncodedSize,
  maxDaPayloadV1InnerBytes,
  projectDaPayloadV1Sizes,
  zstdCompressBound,
} from "../src/da-payload-sizing.js";
import {
  DA_TRANSPORT_LIMITS_V1,
  encodeDaPayloadSubmitRequestV1Cbor,
} from "../src/da-transport.js";

const submitRequestBytes = (
  payloadBytes: Buffer,
  payloadSchemaVersion: 2 | 3,
): number =>
  encodeDaPayloadSubmitRequestV1Cbor({
    deploymentFingerprint: Buffer.alloc(32),
    headerHash: Buffer.alloc(28),
    payloadHash: Buffer.alloc(32),
    payloadSchemaVersion,
    mode: "inline",
    payloadBytes,
    chunkManifest: null,
  }).length;

describe("DA V1 payload sizing", () => {
  it("matches canonical CBOR argument and byte-string boundaries", () => {
    const cases = [
      [0, 1],
      [23, 1],
      [24, 2],
      [0xff, 2],
      [0x100, 3],
      [0xffff, 3],
      [0x1_0000, 5],
      [0xffff_ffff, 5],
      [0x1_0000_0000, 9],
    ] as const;
    for (const [value, expected] of cases) {
      expect(canonicalCborArgumentSize(value)).toBe(expected);
      expect(canonicalCborByteStringSize(value)).toBe(value + expected);
    }
  });

  it("matches actual envelope and inline-request encoders", async () => {
    for (const innerLength of [1, 23, 24, 255, 256, 65_535, 65_536]) {
      const inner = Buffer.alloc(innerLength, 0x5a);
      const identity = await wrapDaPayloadV3(inner, { mode: "identity" });
      expect(identity.length).toBe(
        daPayloadEnvelopeV3EncodedSize({
          innerBytes: innerLength,
          bodyBytes: innerLength,
        }),
      );
      expect(submitRequestBytes(inner, 2)).toBe(
        daPayloadSubmitV1EncodedSize({
          payloadBytes: innerLength,
          payloadSchemaVersion: 2,
        }),
      );
      expect(submitRequestBytes(identity, 3)).toBe(
        daPayloadSubmitV1EncodedSize({
          payloadBytes: identity.length,
          payloadSchemaVersion: 3,
        }),
      );
    }
  });

  it("uses the standard zstd worst-case bound", async () => {
    for (const length of [1, 1024, 128 * 1024, 1024 * 1024]) {
      const input = randomBytes(length);
      const compressed = await compressDaPayloadZstd(input, 3);
      expect(compressed.length).toBeLessThanOrEqual(zstdCompressBound(length));
    }
  });

  it("computes exact mode ceilings beneath the unchanged V1 frame cap", () => {
    expect(maxDaPayloadV1InnerBytes("off")).toBe(67_108_757);
    expect(maxDaPayloadV1InnerBytes("identity")).toBe(67_108_710);
    expect(maxDaPayloadV1InnerBytes("zstd")).toBe(66_847_587);
    for (const mode of ["off", "identity", "zstd"] as const) {
      const maximum = maxDaPayloadV1InnerBytes(mode);
      const pass = projectDaPayloadV1Sizes(maximum, mode);
      const fail = projectDaPayloadV1Sizes(maximum + 1, mode);
      expect(pass.requestBytesUpperBound).toBe(
        DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      );
      expect(fail.requestBytesUpperBound).toBeGreaterThan(
        DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      );
    }
  });
});
