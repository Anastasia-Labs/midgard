import { describe, expect, it, vi } from "vitest";

import { encodeCbor } from "../src/codec/cbor.js";
import { compressDaPayloadZstd } from "../src/da-compression.js";
import {
  DA_PAYLOAD_ENVELOPE_V1_VERSION,
  DaPayloadContentEncoding,
  DaPayloadEnvelopeError,
  decodeDaPayloadEnvelopeV1,
  encodeDaPayloadEnvelopeV1,
  unwrapDaPayloadV1,
  wrapDaPayloadV1,
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

describe("DaPayloadEnvelopeV1", () => {
  it.each(["identity", "zstd"] as const)(
    "round-trips canonical %s envelopes and preserves stored-byte identity",
    async (mode) => {
      const storedBytes = await wrapDaPayloadV1(inner, { mode, zstdLevel: 3 });
      const decoded = decodeDaPayloadEnvelopeV1(storedBytes);
      expect(encodeDaPayloadEnvelopeV1(decoded)).toEqual(storedBytes);
      const unwrapped = await unwrapDaPayloadV1(storedBytes, {
        maxPayloadBytes: MAX,
      });
      expect(unwrapped.schemaVersion).toBe(1);
      expect(unwrapped.innerBytes).toEqual(inner);
      expect(unwrapped.storedBytes).toEqual(storedBytes);
      expect(unwrapped.contentEncoding).toBe(
        mode === "identity"
          ? DaPayloadContentEncoding.identity
          : DaPayloadContentEncoding.zstd,
      );
    },
  );

  it("rejects raw payload bytes without the mandatory V1 envelope", async () => {
    await expectReason(
      unwrapDaPayloadV1(Buffer.from("raw-payload"), {
        maxPayloadBytes: MAX,
      }),
      "malformed_envelope",
    );
  });

  it("rejects unknown content encodings before decompression", async () => {
    const decompress = vi.fn(async () => Buffer.alloc(1));
    const stored = encodeCbor([
      1n,
      99n,
      BigInt(inner.length),
      Buffer.alloc(32),
      Buffer.from("body"),
    ]);
    await expectReason(
      unwrapDaPayloadV1(stored, {
        maxPayloadBytes: MAX,
        decompress,
      }),
      "unknown_content_encoding",
    );
    expect(decompress).not.toHaveBeenCalled();
  });

  it("normalizes malformed envelope fields to a structured envelope error", async () => {
    const stored = encodeCbor([1n, 0n, 1n, "not-bytes", Buffer.from("x")]);
    await expectReason(
      unwrapDaPayloadV1(stored, {
        maxPayloadBytes: MAX,
      }),
      "malformed_envelope",
    );
  });

  it("rejects non-minimal envelope framing without a whole-envelope re-encode", () => {
    const canonical = encodeDaPayloadEnvelopeV1({
      version: DA_PAYLOAD_ENVELOPE_V1_VERSION,
      contentEncoding: DaPayloadContentEncoding.identity,
      innerBytes: inner.length,
      innerSha256: Buffer.alloc(32),
      body: inner,
    });
    const nonMinimalArray = Buffer.concat([
      Buffer.from([0x98, 0x05]),
      canonical.subarray(1),
    ]);

    expect(() => decodeDaPayloadEnvelopeV1(nonMinimalArray)).toThrow(
      "failed to decode DA payload envelope",
    );
  });

  it("reports envelope parse, decompress, and inner-hash stages", async () => {
    const stages: string[] = [];
    let now = 0;
    const stored = await wrapDaPayloadV1(inner, { mode: "zstd" });
    const result = await unwrapDaPayloadV1(stored, {
      maxPayloadBytes: MAX,
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
    const stored = await wrapDaPayloadV1(inner, { mode: "zstd" });
    await expect(
      unwrapDaPayloadV1(stored, {
        maxPayloadBytes: MAX,
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
      BigInt(DA_PAYLOAD_ENVELOPE_V1_VERSION),
      BigInt(DaPayloadContentEncoding.zstd),
      BigInt(MAX + 1),
      Buffer.alloc(32),
      Buffer.from("body"),
    ]);
    await expectReason(
      unwrapDaPayloadV1(stored, {
        maxPayloadBytes: MAX,
        decompress,
      }),
      "declared_inner_too_large",
    );
    expect(decompress).not.toHaveBeenCalled();
  });

  it("enforces maxOutputLength against an over-expanding zstd frame", async () => {
    const expanded = Buffer.alloc(512 * 1024, 0x61);
    const compressed = await compressDaPayloadZstd(expanded, 3);
    const stored = encodeDaPayloadEnvelopeV1({
      version: DA_PAYLOAD_ENVELOPE_V1_VERSION,
      contentEncoding: DaPayloadContentEncoding.zstd,
      innerBytes: 64,
      innerSha256: Buffer.alloc(32),
      body: compressed,
    });
    await expectReason(
      unwrapDaPayloadV1(stored, {
        maxPayloadBytes: MAX,
      }),
      "decompression_failed",
    );
  });

  it("rejects inner length and hash mismatches", async () => {
    const lengthMismatch = encodeDaPayloadEnvelopeV1({
      version: DA_PAYLOAD_ENVELOPE_V1_VERSION,
      contentEncoding: DaPayloadContentEncoding.identity,
      innerBytes: inner.length + 1,
      innerSha256: Buffer.alloc(32),
      body: inner,
    });
    await expectReason(
      unwrapDaPayloadV1(lengthMismatch, {
        maxPayloadBytes: MAX,
      }),
      "inner_length_mismatch",
    );

    const hashMismatch = encodeDaPayloadEnvelopeV1({
      version: DA_PAYLOAD_ENVELOPE_V1_VERSION,
      contentEncoding: DaPayloadContentEncoding.identity,
      innerBytes: inner.length,
      innerSha256: Buffer.alloc(32),
      body: inner,
    });
    await expectReason(
      unwrapDaPayloadV1(hashMismatch, {
        maxPayloadBytes: MAX,
      }),
      "inner_hash_mismatch",
    );
  });
});
