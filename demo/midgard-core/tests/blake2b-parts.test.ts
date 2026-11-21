import { blake2b as nobleBlake2b } from "@noble/hashes/blake2.js";
import sodium from "libsodium-wrappers-sumo";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import {
  midgardBlake2b,
  midgardBlake2b256,
  midgardBlake2bReady,
} from "../src/codec/blake2b.js";

// `midgardBlake2b256` hashes the concatenation of its parts in libsodium's
// heap, without building the concatenation, and the one-shot `midgardBlake2b`
// calls the same C export without the wrapper's per-call allocations. Every
// digest is held to noble over the whole message, and every returned digest
// to being the caller's own bytes.

const BLOCK = 128;
const SODIUM_CHUNK = 64 * 1024;

const LENGTHS = [
  0,
  1,
  31,
  32,
  33,
  BLOCK - 1,
  BLOCK,
  BLOCK + 1,
  2 * BLOCK + 1,
  4_096,
  SODIUM_CHUNK - 1,
  SODIUM_CHUNK,
  SODIUM_CHUNK + 1,
  2 * SODIUM_CHUNK + 7,
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

const noble = (message: Uint8Array, dkLen = 32): string =>
  Buffer.from(nobleBlake2b(message, { dkLen })).toString("hex");

/** Digest hex, or the error class and message, of `run`. */
const outcome = (run: () => Uint8Array): string => {
  try {
    return Buffer.from(run()).toString("hex");
  } catch (error) {
    return `${(error as Error).constructor.name}: ${(error as Error).message}`;
  }
};

/** What `parts` hashed to before `midgardBlake2b256`: each part read by
 * `Buffer.from`, concatenated and hashed whole. */
const concatenated = (parts: readonly unknown[]): string =>
  outcome(() =>
    nobleBlake2b(
      Buffer.concat(parts.map((part) => Buffer.from(part as Uint8Array))),
      { dkLen: 32 },
    ),
  );

const detachedView = (): Uint8Array => {
  const buffer = new ArrayBuffer(8);
  const view = new Uint8Array(buffer, 2, 4);
  structuredClone(buffer, { transfer: [buffer] });
  return view;
};

class TaggedBytes extends Uint8Array {}

/** Parts that are not plain `Uint8Array`s, each of which `Buffer.from`
 * reads (or refuses) in its own way. */
const unusualParts = (): [string, unknown][] => {
  const filled = new ArrayBuffer(32);
  new Uint8Array(filled).set(bytes(32, prng(0xab)));
  const shared = new Uint8Array(new SharedArrayBuffer(40), 3, 33);
  shared.set(bytes(33, prng(0x5ab)));
  return [
    ["ArrayBuffer(32)", filled],
    ["empty ArrayBuffer", new ArrayBuffer(0)],
    ["SharedArrayBuffer view", shared],
    ["ASCII string", "a".repeat(32)],
    ["multi-byte string", "\u00e9".repeat(32)],
    ["Uint16Array", Uint16Array.from([1, 300, 65_535, 0])],
    ["Int8Array", Int8Array.from([-1, 2, -128])],
    ["Float64Array", Float64Array.from([1.5, -2, 300])],
    ["Uint8ClampedArray", Uint8ClampedArray.from([0, 255, 7])],
    ["DataView", new DataView(new ArrayBuffer(8))],
    ["array", [1, 2, 300, -1]],
    ["array-like", { length: 3, 0: 9, 1: 8, 2: 7 }],
    ["Uint8Array subclass", TaggedBytes.from([4, 5, 6])],
    ["Proxy of a Buffer", new Proxy(Buffer.from("proxied"), {})],
    ["detached view", detachedView()],
    ["null", null],
    ["undefined", undefined],
    ["number", 32],
  ];
};

/** `message` cut at `cuts` (sorted offsets), as views of its buffer. */
const split = (message: Uint8Array, cuts: readonly number[]): Uint8Array[] => {
  const parts: Uint8Array[] = [];
  let start = 0;
  for (const cut of [...cuts, message.length]) {
    parts.push(message.subarray(start, cut));
    start = cut;
  }
  return parts;
};

describe("midgardBlake2b256 and the one-shot libsodium path", () => {
  beforeAll(async () => {
    expect(await midgardBlake2bReady).toBe(true);
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("matches the RFC 7693 Blake2b-512 vector and the empty Blake2b-256 digest", () => {
    expect(
      Buffer.from(midgardBlake2b(Buffer.from("abc"), { dkLen: 64 })).toString(
        "hex",
      ),
    ).toBe(
      "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d1" +
        "7d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923",
    );
    const empty =
      "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
    expect(midgardBlake2b256([]).toString("hex")).toBe(empty);
    expect(midgardBlake2b256([new Uint8Array(0)]).toString("hex")).toBe(empty);
  });

  it("agrees with noble over the concatenation for every cut of every boundary length", () => {
    const next = prng(0x9a575);
    for (const length of LENGTHS) {
      const message = bytes(length, next);
      const expected = noble(message);
      const cutSets: number[][] = [
        [],
        [0],
        [length],
        [Math.floor(length / 2)],
        [0, 0, length],
        length > 3 ? [1, length - 1] : [],
        length > SODIUM_CHUNK ? [SODIUM_CHUNK - 1, SODIUM_CHUNK + 1] : [],
      ];
      for (const cuts of cutSets) {
        expect(
          midgardBlake2b256(split(message, cuts)).toString("hex"),
          `${length} bytes cut at ${cuts.join(",")}`,
        ).toBe(expected);
      }
      expect(
        Buffer.from(midgardBlake2b(message, { dkLen: 32 })).toString("hex"),
      ).toBe(expected);
    }
  });

  it("agrees with noble on random messages cut into random parts", () => {
    const next = prng(0x7a275);
    for (let i = 0; i < 500; i++) {
      const length =
        i < 450 ? next() % 1_200 : SODIUM_CHUNK - 64 + (next() % 256);
      const message = bytes(length, next);
      const cuts = Array.from({ length: next() % 6 }, () =>
        length === 0 ? 0 : next() % (length + 1),
      ).sort((a, b) => a - b);
      expect(
        midgardBlake2b256(split(message, cuts)).toString("hex"),
        `${length} bytes cut at ${cuts.join(",")}`,
      ).toBe(noble(message));
      for (const dkLen of [16, 28, 32, 64]) {
        expect(
          Buffer.from(midgardBlake2b(message, { dkLen })).toString("hex"),
        ).toBe(noble(message, dkLen));
      }
    }
  });

  it("hashes each part at its own offset, not its whole backing buffer", () => {
    const backing = bytes(4 * BLOCK, prng(0x0ff5e7));
    const parts = [
      backing.subarray(3, 40),
      Buffer.from(backing.buffer, 200, 77),
      backing.subarray(BLOCK, 3 * BLOCK + 5),
    ];
    expect(midgardBlake2b256(parts).toString("hex")).toBe(
      noble(Buffer.concat(parts)),
    );
  });

  it("returns digests a later call never changes, on buffers of their own", () => {
    const first = midgardBlake2b256([Buffer.from("first")]);
    const firstHex = first.toString("hex");
    const oneShot = midgardBlake2b(Buffer.from("one"), { dkLen: 32 });
    const oneShotHex = Buffer.from(oneShot).toString("hex");
    const second = midgardBlake2b256([Buffer.from("second")]);
    midgardBlake2b(Buffer.from("two"), { dkLen: 64 });
    expect(Buffer.isBuffer(first)).toBe(true);
    expect(first.length).toBe(32);
    expect(first.toString("hex")).toBe(firstHex);
    expect(Buffer.from(oneShot).toString("hex")).toBe(oneShotHex);
    expect(oneShot.buffer.byteLength).toBe(32);
    second.fill(0);
    expect(midgardBlake2b256([Buffer.from("second")]).toString("hex")).toBe(
      noble(Buffer.from("second")),
    );
  });

  it("streams a message longer than one chunk instead of copying it whole", () => {
    // The export starts as a stub that replaces itself (and so a spy) on its
    // first call: one digest binds it before the spy is installed.
    midgardBlake2b(new Uint8Array(0), { dkLen: 32 });
    const streamed = vi.spyOn(sodium, "crypto_generichash_init");
    const oneShot = vi.spyOn(sodium.libsodium, "_crypto_generichash");
    const message = bytes(SODIUM_CHUNK + 1, prng(0x51));
    expect(midgardBlake2b256(split(message, [5])).toString("hex")).toBe(
      noble(message),
    );
    expect(streamed).toHaveBeenCalledTimes(1);
    expect(oneShot).not.toHaveBeenCalled();
    midgardBlake2b256([message.subarray(0, SODIUM_CHUNK)]);
    expect(oneShot).toHaveBeenCalledTimes(1);
    expect(sodium.libsodium._crypto_generichash).toBe(oneShot);
  });

  it("reads every other kind of part as Buffer.from does, refusals included", () => {
    const prefix = Buffer.from("domain");
    for (const [name, part] of unusualParts()) {
      for (const parts of [[part], [prefix, part], [part, prefix, part]]) {
        expect(
          outcome(() => midgardBlake2b256(parts as Uint8Array[])),
          `${name} in ${parts.length} parts`,
        ).toBe(concatenated(parts));
      }
    }
    expect(outcome(() => midgardBlake2b256([null as never]))).toMatch(
      /^TypeError: The first argument must be of type string or an instance of Buffer, ArrayBuffer, or Array or an Array-like Object\. Received null/,
    );
  });

  it("hashes a Proxy part whose getter re-enters the hasher", () => {
    // It re-enters on every read of its length, which copying it (into the
    // heap, or by `Buffer.from`) makes before reading any byte. Re-entering
    // mid-copy is no test of the hasher: `Buffer.from` copies a short view
    // into Node's shared pool, which the re-entrant digest's own allocation
    // would overwrite.
    const inner = Buffer.from("B".repeat(40));
    let reentries = 0;
    const proxy = new Proxy(inner, {
      get(target, property) {
        if (property === "length") {
          reentries++;
          midgardBlake2b256([Buffer.alloc(64, 0xee)]);
          midgardBlake2b(Buffer.alloc(64, 0xdd), { dkLen: 32 });
        }
        // Unbound, so `Buffer.from` reads the bytes through this trap
        // rather than from the target.
        return Reflect.get(target, property, target) as unknown;
      },
    });
    const prefix = Buffer.from("A".repeat(10));
    expect(midgardBlake2b256([prefix, proxy]).toString("hex")).toBe(
      noble(Buffer.concat([prefix, inner])),
    );
    expect(midgardBlake2b256([prefix, proxy, prefix]).toString("hex")).toBe(
      noble(Buffer.concat([prefix, inner, prefix])),
    );
    expect(reentries).toBeGreaterThan(0);
    reentries = 0;
    expect(
      Buffer.from(midgardBlake2b(proxy, { dkLen: 32 })).toString("hex"),
    ).toBe(noble(inner));
    expect(reentries).toBeGreaterThan(0);
  });

  it("hashes a part that views libsodium's heap, scratch region included", () => {
    midgardBlake2b(new Uint8Array(0), { dkLen: 32 });
    const oneShot = vi.spyOn(sodium.libsodium, "_crypto_generichash");
    midgardBlake2b256([Buffer.from("locate the scratch region")]);
    const [, , input] = oneShot.mock.calls[0]!;
    vi.restoreAllMocks();
    // Each digest overwrites the scratch region, so the view is refilled
    // before each one.
    const heap = sodium.libsodium.HEAPU8;
    const copy = Buffer.from(bytes(80, prng(0x4ea9)));
    const view = heap.subarray(input, input + 80);
    const prefix = Buffer.from("A".repeat(40));
    heap.set(copy, input);
    expect(midgardBlake2b256([prefix, view]).toString("hex")).toBe(
      noble(Buffer.concat([prefix, copy])),
    );
    heap.set(copy, input);
    expect(midgardBlake2b256([view, prefix, view]).toString("hex")).toBe(
      noble(Buffer.concat([copy, prefix, copy])),
    );
    heap.set(copy, input);
    expect(
      Buffer.from(midgardBlake2b(view, { dkLen: 32 })).toString("hex"),
    ).toBe(noble(copy));
  });

  it("hashes correctly after libsodium's heap grows", () => {
    const parts = [Buffer.from("before"), bytes(300, prng(0x6e0))];
    const expected = noble(Buffer.concat(parts));
    expect(midgardBlake2b256(parts).toString("hex")).toBe(expected);
    const oldView = sodium.libsodium.HEAPU8.subarray(0, 16);
    const before = sodium.libsodium.HEAPU8.buffer;
    // The wrapper's one-shot copies its whole message into the heap.
    for (
      let size = 8 * 1024 * 1024;
      sodium.libsodium.HEAPU8.buffer === before && size <= 64 * 1024 * 1024;
      size *= 2
    )
      sodium.crypto_generichash(32, new Uint8Array(size), null);
    expect(sodium.libsodium.HEAPU8.buffer).not.toBe(before);
    expect(oldView.length).toBe(0);
    expect(midgardBlake2b256(parts).toString("hex")).toBe(expected);
    expect(midgardBlake2b256([...parts, oldView]).toString("hex")).toBe(
      expected,
    );
    expect(
      Buffer.from(midgardBlake2b(Buffer.concat(parts), { dkLen: 32 })).toString(
        "hex",
      ),
    ).toBe(expected);
    const digest = midgardBlake2b256(parts);
    expect(digest.buffer).not.toBe(sodium.libsodium.HEAPU8.buffer);
  });
});

describe("midgardBlake2b256 before libsodium is ready", () => {
  afterEach(() => {
    vi.doUnmock("libsodium-wrappers-sumo");
    vi.resetModules();
  });

  it("hashes on noble with the same digests", async () => {
    vi.resetModules();
    vi.doMock("libsodium-wrappers-sumo", () => ({
      default: { ready: new Promise<void>(() => undefined) },
    }));
    const { midgardBlake2b256: early } = await import(
      "../src/codec/blake2b.js"
    );
    const next = prng(0xea51);
    for (const length of [0, 1, 32, BLOCK + 1, SODIUM_CHUNK + 1]) {
      const message = bytes(length, next);
      const digest = early(split(message, [Math.floor(length / 3)]));
      expect(Buffer.isBuffer(digest)).toBe(true);
      expect(digest.toString("hex")).toBe(noble(message));
    }
    const prefix = Buffer.from("domain");
    for (const [name, part] of unusualParts()) {
      expect(
        outcome(() => early([prefix, part as Uint8Array])),
        name,
      ).toBe(concatenated([prefix, part]));
    }
  });
});
