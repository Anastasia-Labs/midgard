import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardBlake2b256Trace,
  buildMidgardBlake2b256Trace,
  digestMidgardBlake2b256Trace,
  encodeCbor,
  encodeMidgardBlake2b256TraceControl,
  hashMidgardCekBlobChunk,
  initialMidgardBlake2b256TraceControl,
  MIDGARD_BLAKE2B_256_BLOCK_BYTES,
  MIDGARD_BLAKE2B_256_ROUNDS,
} from "../src/index.js";

const referenceDigest = (message: Uint8Array): Buffer =>
  Buffer.from(blake2b(message, { dkLen: 32 }));

describe("bounded BLAKE2b-256 trace V1", () => {
  it.each([1, 127, 128, 129, 4_095, 16_385])(
    "matches the standard digest for %i bytes",
    (length) => {
      const message = Buffer.alloc(length, 0x6b);
      const trace = buildMidgardBlake2b256Trace(message);
      const terminal = trace.at(-1)!.next;
      const blockCount = Math.ceil(length / MIDGARD_BLAKE2B_256_BLOCK_BYTES);

      expect(trace).toHaveLength(blockCount * (MIDGARD_BLAKE2B_256_ROUNDS + 2));
      expect(digestMidgardBlake2b256Trace(terminal)).toStrictEqual(
        referenceDigest(message),
      );
    },
  );

  it("reproduces the CEK blob-leaf hash from streamed framing", () => {
    const chunk = Buffer.alloc(4_095, 0x6a);
    const message = Buffer.concat([
      Buffer.from("MidgardCekBlobChunkV1", "ascii"),
      encodeCbor(chunk),
    ]);
    const terminal = buildMidgardBlake2b256Trace(message).at(-1)!.next;

    expect(digestMidgardBlake2b256Trace(terminal)).toStrictEqual(
      hashMidgardCekBlobChunk(chunk),
    );
  });

  it("encodes the multi-block terminal state canonically for Aiken", () => {
    const message = Buffer.alloc(6_001, 0x6b);
    const terminal = buildMidgardBlake2b256Trace(message).at(-1)!.next;
    expect(encodeMidgardBlake2b256TraceControl(terminal).toString("hex")).toBe(
      "8901031917711917715840f9ddc8a9845baa65c817312211978ef15f2755c6187cfb471438370299d41f38ea7d74eaac2dcc2326180f6a187e4ce0b6ddae62fabd56ad1643beca144b1fb840004000",
    );
  });

  it("fails closed for wrong block boundaries and forged initial state", () => {
    const initial = initialMidgardBlake2b256TraceControl(129);
    expect(
      advanceMidgardBlake2b256Trace({
        control: initial,
        block: Buffer.alloc(127),
      }),
    ).toBeNull();
    expect(
      advanceMidgardBlake2b256Trace({
        control: { ...initial, chainingValue: Buffer.alloc(64) },
        block: Buffer.alloc(128),
      }),
    ).toBeNull();
  });
});
