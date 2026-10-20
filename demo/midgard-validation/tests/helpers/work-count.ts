import { createRequire } from "node:module";

import { midgardBlake2bReady } from "@al-ft/midgard-core";

import { MidgardCekSemanticStore } from "../../src/cek-executor.semantic-store.js";

/**
 * Counts two kinds of deterministic work a CEK run or trace commitment does,
 * as a stand-in for its time that is the same on every machine and under any
 * load:
 *
 * - `hashedBytes`: the bytes every Blake2b digest hashes. The executor and the
 *   trace commitment are content-addressed, so re-hashing an unchanged
 *   subtree, a whole environment or a whole value on each step shows up here.
 * - `storeReads`: the reads of the executor's semantic store (`node`, `link`,
 *   `pairLink` and `blob`). Walks over Data values, lists and blobs read the
 *   store once per node, whether or not they hash, so an unmemoised list walk
 *   on each step shows up here.
 *
 * Super-linear work that neither hashes nor reads the store, such as copying a
 * growing array, is not counted; the non-gating wall-clock benchmarks under
 * `bench/` are the only check on it.
 *
 * Every Midgard digest goes through midgard-core's `midgardBlake2b`, which
 * hashes with libsodium once libsodium is ready. libsodium is midgard-core's
 * dependency, so it is resolved from there: the counter then wraps the very
 * module instance the hashing uses. The store is wrapped on its prototype, so
 * every store instance the run creates is counted.
 */

type Sodium = {
  crypto_generichash: (
    outputLength: number,
    message: Uint8Array,
    key: null,
  ) => Uint8Array;
  crypto_generichash_update: (state: unknown, message: Uint8Array) => void;
};

const sodium = createRequire(
  new URL(
    "../../node_modules/@al-ft/midgard-core/package.json",
    import.meta.url,
  ),
)("libsodium-wrappers-sumo") as Sodium;

const STORE_READS = ["node", "link", "pairLink", "blob"] as const;

export type Work<T> = {
  readonly result: T;
  /** Bytes hashed by every digest the run computed. */
  readonly hashedBytes: number;
  /** Reads of the CEK semantic store the run made. */
  readonly storeReads: number;
};

/**
 * Runs `run` and counts the bytes it hashes and the semantic-store reads it
 * makes. Refuses before libsodium is ready, when a digest would bypass the
 * hash counter.
 */
export const measureWork = async <T>(
  run: () => T | Promise<T>,
): Promise<Work<T>> => {
  if (!(await midgardBlake2bReady)) {
    throw new Error("libsodium is not ready; Blake2b work cannot be counted");
  }
  const oneShot = sodium.crypto_generichash;
  const update = sodium.crypto_generichash_update;
  const store = MidgardCekSemanticStore.prototype as unknown as Record<
    (typeof STORE_READS)[number],
    (this: unknown, ...args: unknown[]) => unknown
  >;
  const reads = STORE_READS.map((name) => [name, store[name]] as const);
  let hashedBytes = 0;
  let storeReads = 0;
  sodium.crypto_generichash = (outputLength, message, key) => {
    hashedBytes += message.length;
    return oneShot(outputLength, message, key);
  };
  sodium.crypto_generichash_update = (state, message) => {
    hashedBytes += message.length;
    update(state, message);
  };
  for (const [name, read] of reads) {
    store[name] = function (this: unknown, ...args: unknown[]) {
      storeReads += 1;
      return read.apply(this, args);
    };
  }
  try {
    const result = await run();
    return { result, hashedBytes, storeReads };
  } finally {
    sodium.crypto_generichash = oneShot;
    sodium.crypto_generichash_update = update;
    for (const [name, read] of reads) store[name] = read;
  }
};
