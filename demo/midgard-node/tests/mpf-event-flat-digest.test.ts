import { blake2b as nobleBlake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  createEventFlatDigestAdapter,
  EVENT_FLAT_DIGEST_UPDATE_CHUNK_BYTES,
  type EventFlatBlake2b,
  eventFlatDigest,
  prepareEventFlatDigest,
} from "../src/workers/utils/mpf-event-flat-digest.js";

const nobleStateImplementation = ({
  supported,
  loaded,
  onReady,
}: {
  readonly supported: boolean;
  readonly loaded: () => boolean;
  readonly onReady: (callback: () => void) => void;
}): EventFlatBlake2b => {
  const implementation = ((outputBytes: number) => {
    const chunks: Uint8Array[] = [];
    const state = {
      update(value: Uint8Array) {
        chunks.push(value);
        return state;
      },
      digest() {
        return nobleBlake2b(Buffer.concat(chunks), { dkLen: outputBytes });
      },
    };
    return state;
  }) as EventFlatBlake2b;
  Object.defineProperties(implementation, {
    WASM_SUPPORTED: { get: () => supported },
    WASM_LOADED: { get: loaded },
    ready: { value: onReady },
  });
  return implementation;
};

describe("event-flat digest adapter", () => {
  it("fails closed before readiness and becomes synchronously usable after ready", async () => {
    let loaded = false;
    let readyCallback: (() => void) | undefined;
    const adapter = createEventFlatDigestAdapter(
      nobleStateImplementation({
        supported: true,
        loaded: () => loaded,
        onReady: (callback) => {
          readyCallback = callback;
        },
      }),
    );
    expect(adapter.isReady()).toBe(false);
    expect(() => adapter.digest(Buffer.alloc(64))).toThrow(/not ready/);
    loaded = true;
    readyCallback!();
    await adapter.prepare();
    expect(adapter.isReady()).toBe(true);
    expect(adapter.digest(Buffer.alloc(64))).toStrictEqual(
      Buffer.from(nobleBlake2b(Buffer.alloc(64), { dkLen: 32 })),
    );
  });

  it("rejects unsupported and failed WebAssembly initialization", async () => {
    const unsupported = createEventFlatDigestAdapter(
      nobleStateImplementation({
        supported: false,
        loaded: () => false,
        onReady: () => {
          throw new Error("unsupported adapter must not request readiness");
        },
      }),
    );
    await expect(unsupported.prepare()).rejects.toThrow(/WebAssembly support/);
    expect(() => unsupported.digest(Buffer.alloc(64))).toThrow(
      /WebAssembly support/,
    );

    let readyCallback: (() => void) | undefined;
    const failed = createEventFlatDigestAdapter(
      nobleStateImplementation({
        supported: true,
        loaded: () => false,
        onReady: (callback) => {
          readyCallback = callback;
        },
      }),
    );
    readyCallback!();
    await expect(failed.prepare()).rejects.toThrow(/failed to initialize/);
    expect(() => failed.digest(Buffer.alloc(64))).toThrow(
      /failed to initialize/,
    );
  });

  it("matches noble across fixed and deterministic random 64-byte vectors", async () => {
    await prepareEventFlatDigest();
    let state = 0x9e3779b9;
    const vectors = [Buffer.alloc(64), Buffer.alloc(64, 0xff)];
    for (let vectorIndex = 0; vectorIndex < 256; vectorIndex += 1) {
      const value = Buffer.allocUnsafe(64);
      for (let index = 0; index < value.length; index += 4) {
        state =
          (Math.imul(state ^ (state >>> 15), 2_246_822_519) + 3_266_489_917) >>>
          0;
        value.writeUInt32LE(state, index);
      }
      vectors.push(value);
    }
    for (const vector of vectors) {
      expect(eventFlatDigest(vector)).toStrictEqual(
        Buffer.from(nobleBlake2b(vector, { dkLen: 32 })),
      );
    }
  });

  it("streams large updates in bounded chunks without changing the digest", async () => {
    const updateSizes: number[] = [];
    const implementation = ((outputBytes: number) => {
      const chunks: Uint8Array[] = [];
      const state = {
        update(value: Uint8Array) {
          updateSizes.push(value.length);
          chunks.push(value);
          return state;
        },
        digest() {
          return nobleBlake2b(Buffer.concat(chunks), { dkLen: outputBytes });
        },
      };
      return state;
    }) as EventFlatBlake2b;
    Object.defineProperties(implementation, {
      WASM_SUPPORTED: { value: true },
      WASM_LOADED: { value: true },
      ready: { value: (callback: () => void) => callback() },
    });
    const adapter = createEventFlatDigestAdapter(implementation);
    await adapter.prepare();
    const value = Buffer.alloc(EVENT_FLAT_DIGEST_UPDATE_CHUNK_BYTES + 17, 0x5a);

    expect(adapter.digest(value)).toStrictEqual(
      Buffer.from(nobleBlake2b(value, { dkLen: 32 })),
    );
    expect(updateSizes).toStrictEqual([
      EVENT_FLAT_DIGEST_UPDATE_CHUNK_BYTES,
      17,
    ]);
  });
});
