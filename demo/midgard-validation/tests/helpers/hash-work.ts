import { createRequire } from "node:module";

import { midgardBlake2bReady } from "@al-ft/midgard-core";

/**
 * Counts the Blake2b work a run does: the bytes every digest hashes.
 *
 * The CEK executor and the trace commitment are content-addressed, so the
 * work of a step is the bytes it hashes. A machine whose per-step work is
 * bounded by the nodes the step creates hashes bytes linear in its step count;
 * re-hashing an unchanged subtree, a whole environment or a whole value on
 * each step shows up as super-linear growth. Unlike wall-clock time, the count
 * is the same on every machine and under any load.
 *
 * Every Midgard digest goes through midgard-core's `midgardBlake2b`, which
 * hashes with libsodium once libsodium is ready. libsodium is midgard-core's
 * dependency, so it is resolved from there: the counter then wraps the very
 * module instance the hashing uses.
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

export type HashWork<T> = {
  readonly result: T;
  /** Bytes hashed by every digest the run computed. */
  readonly bytes: number;
};

/**
 * Runs `run` and counts the bytes it hashes. Refuses before libsodium is
 * ready, when a digest would bypass the counter.
 */
export const measureHashWork = async <T>(
  run: () => T | Promise<T>,
): Promise<HashWork<T>> => {
  if (!(await midgardBlake2bReady)) {
    throw new Error("libsodium is not ready; Blake2b work cannot be counted");
  }
  const oneShot = sodium.crypto_generichash;
  const update = sodium.crypto_generichash_update;
  let bytes = 0;
  sodium.crypto_generichash = (outputLength, message, key) => {
    bytes += message.length;
    return oneShot(outputLength, message, key);
  };
  sodium.crypto_generichash_update = (state, message) => {
    bytes += message.length;
    update(state, message);
  };
  try {
    const result = await run();
    return { result, bytes };
  } finally {
    sodium.crypto_generichash = oneShot;
    sodium.crypto_generichash_update = update;
  }
};
