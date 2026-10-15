/**
 * The import checker's negative cache: the scripts whose run faulted twice,
 * the second time on a fresh instance. A transaction running a cached
 * script skips its import-time runs (`cache_skipped`) and gets them from the
 * backlog lane instead. An entry expires after its TTL (at most half the
 * block maturity), and the next transaction running that script is probed
 * again. The cache is in memory: a restart empties it, so every script is
 * probed again.
 */
import { createHash } from "node:crypto";

import { type MidgardCekVerdictRequest } from "@al-ft/midgard-validation";
import { Clock, Effect } from "effect";

/** The most scripts held; past it, the oldest entry leaves first. */
export const NEGATIVE_CACHE_MAX_ENTRIES = 100_000;

export type NegativeCache = Readonly<{
  /** Whether `run`'s script is cached now. */
  holds: (run: MidgardCekVerdictRequest) => Effect.Effect<boolean>;
  /** Caches `run`'s script until `ttlMs` from now. */
  add: (run: MidgardCekVerdictRequest) => Effect.Effect<void>;
}>;

const scriptKey = (run: MidgardCekVerdictRequest) =>
  createHash("sha256").update(run.scriptBytes).digest("hex");

export const makeNegativeCache = (ttlMs: number): NegativeCache => {
  const expiries = new Map<string, number>();
  return {
    holds: (run) =>
      Clock.currentTimeMillis.pipe(
        Effect.map((now) => {
          const key = scriptKey(run);
          const expiresAt = expiries.get(key);
          if (expiresAt === undefined) return false;
          if (now < expiresAt) return true;
          expiries.delete(key);
          return false;
        }),
      ),
    add: (run) =>
      Clock.currentTimeMillis.pipe(
        Effect.map((now) => {
          const key = scriptKey(run);
          expiries.delete(key);
          expiries.set(key, now + ttlMs);
          if (expiries.size > NEGATIVE_CACHE_MAX_ENTRIES)
            expiries.delete(expiries.keys().next().value!);
        }),
      ),
  };
};
