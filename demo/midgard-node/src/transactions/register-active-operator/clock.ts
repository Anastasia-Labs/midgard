/**
 * Lifecycle-specific time helpers for operator transactions.
 * This module centralizes current-time resolution and slot-boundary alignment
 * so register/activate flows share the same clock semantics.
 */
import {
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { slotToUnixTimeForLucidOrEmulatorFallback } from "@/lucid-time.js";
import { alignUnixTimeToSlotBoundary } from "@/workers/utils/commit-end-time.js";

/**
 * Resolves the current wall-clock time for the active Lucid context.
 */
export const resolveCurrentTimeMs = (
  lucid: LucidEvolution,
): Effect.Effect<bigint, never> =>
  Effect.sync(() => currentTimeMsForLucidOrEmulatorFallback(lucid));

/**
 * Returns the current time in milliseconds for the active Lucid context,
 * falling back to emulator timing rules when necessary.
 */
export const currentTimeMsForLucidOrEmulatorFallback = (
  lucid: LucidEvolution,
): bigint =>
  BigInt(
    slotToUnixTimeForLucidOrEmulatorFallback(lucid, lucid.currentSlot()),
  );

/**
 * Aligns a unix timestamp to the slot boundary used by the active Lucid
 * context.
 */
export const alignUnixTimeMsToSlotBoundary = (
  lucid: LucidEvolution,
  unixTimeMs: bigint,
): bigint => BigInt(alignUnixTimeToSlotBoundary(lucid, Number(unixTimeMs)));
