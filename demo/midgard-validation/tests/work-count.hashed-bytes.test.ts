import { createRequire } from "node:module";

import {
  computeHash28,
  computeHash32,
  hashMidgardCekMachineState,
  midgardBlake2b,
  midgardBlake2bReady,
  type MidgardCekMachineState,
  midgardValidationTraceLeafHash,
} from "@al-ft/midgard-core";
import { beforeAll, describe, expect, it } from "vitest";

import { measureWork } from "./helpers/work-count.js";

// The scaling gates read `measureWork`'s hashed-byte count, so it must count
// every digest on every path, including when it is the first thing in the
// process to hash after libsodium is ready: libsodium's C export starts as a
// stub that replaces itself on its first call.

const sodium = createRequire(
  new URL("../node_modules/@al-ft/midgard-core/package.json", import.meta.url),
)("libsodium-wrappers-sumo") as {
  libsodium: { _crypto_generichash: (...args: number[]) => number };
};

const STATE = {
  mode: "compute",
  executionIndex: 2n,
  focusRoot: Buffer.alloc(32, 1),
  environmentRoot: Buffer.alloc(32, 2),
  continuationRoot: Buffer.alloc(32, 3),
  auxiliary: 0n,
  cpu: 16_000n,
  memory: 100n,
} as MidgardCekMachineState;

/** The bytes `hashMidgardCekMachineState(STATE)` hashes: its domain, then
 * the CBOR array header and its 9 fields (version, mode and index of one byte
 * each, three 34-byte roots, auxiliary 0, cpu 16000, memory 100). */
const STATE_BYTES =
  "MidgardCekMachineStateV1".length + 1 + 1 + 1 + 1 + 3 * 34 + 1 + 3 + 2;

/** The bytes a trace leaf hashes: its domain and the 32-byte state hash. */
const LEAF_BYTES = "MidgardValidationTraceLeafV1".length + 32;

/** Digests on every path, and the bytes they hash. */
const hashOnEveryPath = (): number => {
  midgardValidationTraceLeafHash(Buffer.alloc(32, 9));
  midgardBlake2b(new Uint8Array(0), { dkLen: 32 });
  midgardBlake2b(Buffer.alloc(65_536), { dkLen: 32 });
  midgardBlake2b(Buffer.alloc(65_537), { dkLen: 64 });
  midgardBlake2b(Buffer.alloc(3 * 65_536 + 5), { dkLen: 32 });
  computeHash32(Buffer.alloc(100));
  computeHash28(Buffer.alloc(3));
  hashMidgardCekMachineState(STATE);
  return (
    LEAF_BYTES + 0 + 65_536 + 65_537 + 3 * 65_536 + 5 + 100 + 3 + STATE_BYTES
  );
};

describe("measureWork hashed-byte count", () => {
  beforeAll(async () => {
    expect(await midgardBlake2bReady).toBe(true);
  });

  it("counts every one-shot digest when it is the first to hash", async () => {
    const digests = 10;
    const { hashedBytes } = await measureWork(() => {
      for (let i = 0; i < digests; i++)
        midgardValidationTraceLeafHash(Buffer.alloc(32, i));
    });
    expect(hashedBytes).toBe(digests * LEAF_BYTES);
  });

  it("counts every digest while the export is still a self-replacing stub", async () => {
    // As libsodium's own stub does: the first call installs the real export.
    const module = sodium.libsodium;
    const bound = module._crypto_generichash;
    module._crypto_generichash = (...args) => {
      module._crypto_generichash = bound;
      return bound(...args);
    };
    try {
      const { hashedBytes } = await measureWork(() => {
        for (let i = 0; i < 10; i++)
          midgardValidationTraceLeafHash(Buffer.alloc(32, i));
      });
      expect(hashedBytes).toBe(10 * LEAF_BYTES);
    } finally {
      module._crypto_generichash = bound;
    }
  });

  it("counts every byte of every digest path exactly once", async () => {
    let expected = 0;
    const { hashedBytes } = await measureWork(() => {
      expected = hashOnEveryPath();
    });
    expect(hashedBytes).toBe(expected);
    const again = await measureWork(hashOnEveryPath);
    expect(again.hashedBytes).toBe(again.result);
  });

  it("refuses a run whose hook was replaced, and restores the export", async () => {
    midgardBlake2b(new Uint8Array(0), { dkLen: 32 });
    const before = sodium.libsodium._crypto_generichash;
    await expect(
      measureWork(() => {
        sodium.libsodium._crypto_generichash = before;
      }),
    ).rejects.toThrow("a Blake2b hook was replaced");
    expect(sodium.libsodium._crypto_generichash).toBe(before);
  });
});
