import { blake2b as nobleBlake2b } from "@noble/hashes/blake2.js";
import { beforeAll, describe, expect, it } from "vitest";

import {
  computeHash28,
  computeHash32,
  midgardBlake2b,
  midgardBlake2bBackendCounts,
  midgardBlake2bReady,
} from "../src/index.js";

// Every Midgard Blake2b digest comes from libsodium once it is ready and from
// noble before that, so the two must agree on every input and every output
// length Midgard uses. A digest that depended on which backend computed it
// would make a commitment depend on process start-up timing.

const OUTPUT_LENGTHS = [28, 32, 64] as const;

const BLOCK = 128;
const SODIUM_CHUNK = 64 * 1024;

const BOUNDARY_LENGTHS = [
  0,
  1,
  2,
  BLOCK - 1,
  BLOCK,
  BLOCK + 1,
  2 * BLOCK - 1,
  2 * BLOCK,
  2 * BLOCK + 1,
  4_095,
  4_096,
  15_000,
  16 * BLOCK,
  SODIUM_CHUNK - 1,
  SODIUM_CHUNK,
  SODIUM_CHUNK + 1,
  2 * SODIUM_CHUNK,
  2 * SODIUM_CHUNK + 1,
  1024 * 1024,
  1024 * 1024 + 1,
];

/** xorshift32, so the "random" corpus is identical on every run. */
const prng = (seed: number) => {
  let state = seed >>> 0 || 1;
  return () => {
    state ^= state << 13;
    state >>>= 0;
    state ^= state >>> 17;
    state ^= state << 5;
    state >>>= 0;
    return state;
  };
};

const bytes = (length: number, next: () => number): Uint8Array => {
  const out = new Uint8Array(length);
  for (let i = 0; i < length; i++) out[i] = next() & 0xff;
  return out;
};

const noble = (message: Uint8Array, dkLen: number): string =>
  Buffer.from(nobleBlake2b(message, { dkLen })).toString("hex");

const midgard = (message: Uint8Array, dkLen: number): string =>
  Buffer.from(midgardBlake2b(message, { dkLen })).toString("hex");

describe("Midgard Blake2b backend", () => {
  beforeAll(async () => {
    expect(await midgardBlake2bReady).toBe(true);
  });

  it("matches the RFC 7693 Appendix A Blake2b-512 vector", () => {
    expect(midgard(Buffer.from("abc"), 64)).toBe(
      "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d1" +
        "7d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923",
    );
  });

  it.each(OUTPUT_LENGTHS)(
    "agrees with noble on every boundary length at %i output bytes",
    (dkLen) => {
      const next = prng(0x5eed + dkLen);
      for (const length of BOUNDARY_LENGTHS) {
        for (const message of [
          new Uint8Array(length),
          new Uint8Array(length).fill(0xff),
          bytes(length, next),
        ]) {
          expect(midgard(message, dkLen), `${length} bytes`).toBe(
            noble(message, dkLen),
          );
        }
      }
    },
  );

  it.each(OUTPUT_LENGTHS)(
    "agrees with noble on random messages at %i output bytes",
    (dkLen) => {
      const next = prng(0xb1a4e + dkLen);
      for (let i = 0; i < 400; i++) {
        const length = i < 300 ? next() % 1_025 : next() % (3 * SODIUM_CHUNK);
        const message = bytes(length, next);
        expect(midgard(message, dkLen), `${length} bytes`).toBe(
          noble(message, dkLen),
        );
      }
    },
  );

  it("hashes a view at its own offset, not its whole backing buffer", () => {
    const next = prng(0x0ff5e7);
    const backing = bytes(3 * SODIUM_CHUNK, next);
    for (const [start, end] of [
      [1, 2],
      [7, 7 + BLOCK],
      [13, 13 + SODIUM_CHUNK + 1],
      [SODIUM_CHUNK - 3, 3 * SODIUM_CHUNK - 5],
    ] as const) {
      const view = backing.subarray(start, end);
      const buffer = Buffer.from(backing.buffer, start, end - start);
      for (const dkLen of OUTPUT_LENGTHS) {
        expect(midgard(view, dkLen)).toBe(noble(view, dkLen));
        expect(midgard(buffer, dkLen)).toBe(noble(buffer, dkLen));
      }
    }
  });

  it("returns a digest that does not alias libsodium's heap", () => {
    const first = midgardBlake2b(Buffer.from("first"), { dkLen: 32 });
    const copy = Buffer.from(first).toString("hex");
    midgardBlake2b(Buffer.from("second"), { dkLen: 32 });
    expect(first.byteLength).toBe(32);
    expect(first.buffer.byteLength).toBe(32);
    expect(Buffer.from(first).toString("hex")).toBe(copy);
  });

  it("serves computeHash32 and computeHash28 from libsodium", () => {
    const message = Buffer.from("midgard");
    const before = midgardBlake2bBackendCounts();
    expect(computeHash32(message).toString("hex")).toBe(noble(message, 32));
    expect(computeHash28(message).toString("hex")).toBe(noble(message, 28));
    const after = midgardBlake2bBackendCounts();
    expect(after.sodium - before.sodium).toBe(2);
    expect(after.noble - before.noble).toBe(0);
  });

  it("leaves output lengths libsodium does not support to noble", () => {
    const message = Buffer.from("short output");
    const before = midgardBlake2bBackendCounts();
    for (const dkLen of [1, 8, 15]) {
      expect(midgard(message, dkLen)).toBe(noble(message, dkLen));
    }
    expect(() => midgardBlake2b(message, { dkLen: 65 })).toThrow();
    expect(() => midgardBlake2b(message, { dkLen: 0 })).toThrow();
    const after = midgardBlake2bBackendCounts();
    expect(after.sodium - before.sodium).toBe(0);
  });
});
