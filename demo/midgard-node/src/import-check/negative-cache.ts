/**
 * The import checker's negative cache: the scripts whose run faulted twice,
 * the second time on a fresh instance. A run killed at its kill is not a
 * fault (a loaded machine stretches the kill), so it never caches its
 * script. A transaction running a cached script skips its import-time runs (`cache_skipped`) and gets them from the
 * backlog lane instead. An entry expires after its TTL (at most half the
 * block maturity), and the next transaction running that script is probed
 * again. The cache is in memory: a restart empties it, so every script is
 * probed again.
 */
import { Clock, Effect } from "effect";

/** The most scripts held; past it, the oldest entry leaves first. */
export const NEGATIVE_CACHE_MAX_ENTRIES = 100_000;

export type NegativeCache = Readonly<{
  /** Whether the script hashing to `scriptHash` (sha256) is cached now. */
  holds: (scriptHash: Uint8Array) => Effect.Effect<boolean>;
  /** Caches the script hashing to `scriptHash` until `ttlMs` from now. */
  add: (scriptHash: Uint8Array) => Effect.Effect<void>;
}>;

const keyOf = (scriptHash: Uint8Array) =>
  Buffer.from(scriptHash).toString("hex");

export const makeNegativeCache = (ttlMs: number): NegativeCache => {
  const expiries = new Map<string, number>();
  return {
    holds: (scriptHash) =>
      Clock.currentTimeMillis.pipe(
        Effect.map((now) => {
          const key = keyOf(scriptHash);
          const expiresAt = expiries.get(key);
          if (expiresAt === undefined) return false;
          if (now < expiresAt) return true;
          expiries.delete(key);
          return false;
        }),
      ),
    add: (scriptHash) =>
      Clock.currentTimeMillis.pipe(
        Effect.map((now) => {
          const key = keyOf(scriptHash);
          expiries.delete(key);
          expiries.set(key, now + ttlMs);
          if (expiries.size > NEGATIVE_CACHE_MAX_ENTRIES)
            expiries.delete(expiries.keys().next().value!);
        }),
      ),
  };
};
