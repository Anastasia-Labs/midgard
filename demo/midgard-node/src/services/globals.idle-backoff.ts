import { Effect, Ref } from "effect";

import type { Globals } from "./globals.globals.js";

/**
 * Exponential backoff for a fiber whose tick found nothing to do. A fiber
 * that is provably idle skips ticks until `skipUntilMs`; any sign of work
 * resets it, so the first tick after work appears runs at the normal cadence.
 */
export type IdleBackoffState = {
  readonly consecutiveIdleTicks: number;
  readonly skipUntilMs: number;
};

export const nextIdleBackoffState = (
  current: IdleBackoffState | undefined,
  options: { readonly baseMs: number; readonly maxMs: number },
  nowMs: number,
): IdleBackoffState => {
  const consecutiveIdleTicks = (current?.consecutiveIdleTicks ?? 0) + 1;
  // 2^n overflows to Infinity long before n does; min() keeps the cap.
  const delayMs = Math.min(
    options.maxMs,
    Math.max(0, options.baseMs) * 2 ** Math.min(consecutiveIdleTicks, 30),
  );
  return { consecutiveIdleTicks, skipUntilMs: nowMs + delayMs };
};

/** Whether `key` is still backing off at `nowMs`. */
export const idleBackoffActive = (
  globals: Pick<Globals, "IDLE_BACKOFF">,
  key: string,
  nowMs: number = Date.now(),
): Effect.Effect<boolean> =>
  Ref.get(globals.IDLE_BACKOFF).pipe(
    Effect.map((backoffs) => (backoffs.get(key)?.skipUntilMs ?? 0) > nowMs),
  );

/** Records an idle tick for `key` and returns the delay until its next run. */
export const recordIdleTick = (
  globals: Pick<Globals, "IDLE_BACKOFF">,
  key: string,
  options: { readonly baseMs: number; readonly maxMs: number },
  nowMs: number = Date.now(),
): Effect.Effect<number> =>
  Ref.modify(globals.IDLE_BACKOFF, (backoffs) => {
    const next = nextIdleBackoffState(backoffs.get(key), options, nowMs);
    const updated = new Map(backoffs);
    updated.set(key, next);
    return [next.skipUntilMs - nowMs, updated];
  });

export const resetIdleBackoff = (
  globals: Pick<Globals, "IDLE_BACKOFF">,
  key: string,
): Effect.Effect<void> =>
  Ref.update(globals.IDLE_BACKOFF, (backoffs) => {
    if (!backoffs.has(key)) return backoffs;
    const updated = new Map(backoffs);
    updated.delete(key);
    return updated;
  });
