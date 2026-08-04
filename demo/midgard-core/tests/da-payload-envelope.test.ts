import { describe, expect, it, vi } from "vitest";

import { encodeCbor } from "../src/codec/cbor.js";
import { compressDaPayloadZstd } from "../src/da-compression.js";
import {
  DA_PAYLOAD_ENVELOPE_V3_VERSION,
  DaPayloadContentEncoding,
  DaPayloadEnvelopeError,
  decodeDaPayloadEnvelopeV3,
  encodeDaPayloadEnvelopeV3,
  unwrapDaPayload,
  wrapDaPayloadV3,
} from "../src/da-payload-envelope.js";

const MAX = 1024 * 1024;
const inner = Buffer.from("midgard-da-envelope-payload".repeat(512));

const expectReason = async (
  operation: Promise<unknown> | (() => unknown),
  reasonCode: DaPayloadEnvelopeError["reasonCode"],
) => {
  try {
    if (typeof operation === "function") {
      operation();
    } else {
      await operation;
    }
    throw new Error("expected operation to reject");
  } catch (error) {
    expect(error).toBeInstanceOf(DaPayloadEnvelopeError);
    expect((error as DaPayloadEnvelopeError).reasonCode).toBe(reasonCode);
  }
};

describe("DaPayloadEnvelopeV3", () => {
  it.each(["identity", "zstd"] as const)(
    "round-trips canonical %s envelopes and preserves stored-byte identity",
    async (mode) => {
      const storedBytes = await wrapDaPayloadV3(inner, { mode, zstdLevel: 3 });
      const decoded = decodeDaPayloadEnvelopeV3(storedBytes);
      expect(encodeDaPayloadEnvelopeV3(decoded)).toEqual(storedBytes);
      const unwrapped = await unwrapDaPayload(storedBytes, {
        maxPayloadBytes: MAX,
        schemaVersion: 3,
      });
      expect(unwrapped.schemaVersion).toBe(3);
      expect(unwrapped.innerBytes).toEqual(inner);
      expect(unwrapped.storedBytes).toEqual(storedBytes);
      expect(unwrapped.contentEncoding).toBe(
        mode === "identity"
          ? DaPayloadContentEncoding.identity
          : DaPayloadContentEncoding.zstd,
      );
    },
  );

  it("passes legacy schema-v2 bytes through unchanged", async () => {
    const raw = Buffer.from("legacy-v2-owned-by-sdk-decoder");
    const result = await unwrapDaPayload(raw, {
      maxPayloadBytes: MAX,
      schemaVersion: 2,
    });
    expect(result.schemaVersion).toBe(2);
    expect(result.innerBytes).toEqual(raw);
    expect(result.storedBytes).toEqual(raw);
  });

  it("rejects unknown content encodings before decompression", async () => {
    const decompress = vi.fn(async () => Buffer.alloc(1));
    const stored = encodeCbor([
      3n,
      99n,
      BigInt(inner.length),
      Buffer.alloc(32),
      Buffer.from("body"),
    ]);
    await expectReason(
      unwrapDaPayload(stored, {
        maxPayloadBytes: MAX,
        schemaVersion: 3,
        decompress,
      }),
      "unknown_content_encoding",
    );
    expect(decompress).not.toHaveBeenCalled();
  });

  it("normalizes malformed envelope fields to a structured envelope error", async () => {
    const stored = encodeCbor([3n, 0n, 1n, "not-bytes", Buffer.from("x")]);
    await expectReason(
      unwrapDaPayload(stored, {
        maxPayloadBytes: MAX,
        schemaVersion: 3,
      }),
      "malformed_envelope",
    );
  });

  it("rejects non-minimal envelope framing without a whole-envelope re-encode", () => {
    const canonical = encodeDaPayloadEnvelopeV3({
      version: DA_PAYLOAD_ENVELOPE_V3_VERSION,
      contentEncoding: DaPayloadContentEncoding.identity,
      innerBytes: inner.length,
      innerSha256: Buffer.alloc(32),
      body: inner,
    });
    const nonMinimalArray = Buffer.concat([
      Buffer.from([0x98, 0x05]),
      canonical.subarray(1),
    ]);

    expect(() => decodeDaPayloadEnvelopeV3(nonMinimalArray)).toThrow(
      "failed to decode DA payload envelope",
    );
  });

  it("reports envelope parse, decompress, and inner-hash stages", async () => {
    const stages: string[] = [];
    let now = 0;
    const stored = await wrapDaPayloadV3(inner, { mode: "zstd" });
    const result = await unwrapDaPayload(stored, {
      maxPayloadBytes: MAX,
      schemaVersion: 3,
      timing: {
        monotonicNow: () => {
          now += 2;
          return now;
        },
        onStageTiming: (stage, durationMs) => {
          expect(durationMs).toBe(2);
          stages.push(stage);
        },
      },
    });

    expect(result.innerBytes).toEqual(inner);
    expect(stages).toEqual([
      "envelope_parse",
      "envelope_decompress",
      "inner_hash",
    ]);
  });

  it("keeps validation semantics when the optional timing clock throws", async () => {
    const stored = await wrapDaPayloadV3(inner, { mode: "zstd" });
    await expect(
      unwrapDaPayload(stored, {
        maxPayloadBytes: MAX,
        schemaVersion: 3,
        timing: {
          monotonicNow: () => {
            throw new Error("clock unavailable");
          },
          onStageTiming: () => {
            throw new Error("sink unavailable");
          },
        },
      }),
    ).resolves.toMatchObject({ innerBytes: inner });
  });

  it("rejects an oversized declared inner payload before decompression", async () => {
    const decompress = vi.fn(async () => Buffer.alloc(1));
    const stored = encodeCbor([
      BigInt(DA_PAYLOAD_ENVELOPE_V3_VERSION),
      BigInt(DaPayloadContentEncoding.zstd),
      BigInt(MAX + 1),
      Buffer.alloc(32),
      Buffer.from("body"),
    ]);
    await expectReason(
      unwrapDaPayload(stored, {
        maxPayloadBytes: MAX,
        schemaVersion: 3,
        decompress,
      }),
      "declared_inner_too_large",
    );
    expect(decompress).not.toHaveBeenCalled();
  });

  it("enforces maxOutputLength against an over-expanding zstd frame", async () => {
    const expanded = Buffer.alloc(512 * 1024, 0x61);
    const compressed = await compressDaPayloadZstd(expanded, 3);
    const stored = encodeDaPayloadEnvelopeV3({
      version: DA_PAYLOAD_ENVELOPE_V3_VERSION,
      contentEncoding: DaPayloadContentEncoding.zstd,
      innerBytes: 64,
      innerSha256: Buffer.alloc(32),
      body: compressed,
    });
    await expectReason(
      unwrapDaPayload(stored, {
        maxPayloadBytes: MAX,
        schemaVersion: 3,
      }),
      "decompression_failed",
    );
  });

  it("rejects inner length and hash mismatches", async () => {
    const lengthMismatch = encodeDaPayloadEnvelopeV3({
      version: DA_PAYLOAD_ENVELOPE_V3_VERSION,
      contentEncoding: DaPayloadContentEncoding.identity,
      innerBytes: inner.length + 1,
      innerSha256: Buffer.alloc(32),
      body: inner,
    });
    await expectReason(
      unwrapDaPayload(lengthMismatch, {
        maxPayloadBytes: MAX,
        schemaVersion: 3,
      }),
      "inner_length_mismatch",
    );

    const hashMismatch = encodeDaPayloadEnvelopeV3({
      version: DA_PAYLOAD_ENVELOPE_V3_VERSION,
      contentEncoding: DaPayloadContentEncoding.identity,
      innerBytes: inner.length,
      innerSha256: Buffer.alloc(32),
      body: inner,
    });
    await expectReason(
      unwrapDaPayload(hashMismatch, {
        maxPayloadBytes: MAX,
        schemaVersion: 3,
      }),
      "inner_hash_mismatch",
    );
  });
});
