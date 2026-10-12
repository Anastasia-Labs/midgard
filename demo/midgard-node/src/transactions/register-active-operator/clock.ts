/**
 * Lifecycle-specific time helpers for operator transactions.
 * This module centralizes current-time resolution and slot-boundary alignment
 * so register/activate flows share the same clock semantics.
 *
 * Two clocks, kept apart (plan §3.6): `resolveL1NowMs` is the L1 `slotNow`,
 * the only "now" for a decision (is the operator active, may it activate, is
 * a takeover due). `currentTimeMsForLucidOrEmulatorFallback` reads Lucid's
 * slot, which runs on the wall clock for a live network; it only picks the
 * validity bounds of a transaction being built.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { l1NowUnixTimeMs, type L1SlotUnknownError } from "../../l1-heads.js";
import { slotToUnixTimeForLucidOrEmulatorFallback } from "../../lucid-time.js";
import { alignUnixTimeToSlotBoundary } from "../../workers/utils/commit-end-time.js";

/** The L1 `slotNow` as POSIX ms: the "now" every operator decision reads. */
export const resolveL1NowMs = (
  lucid: LucidEvolution,
): Effect.Effect<bigint, L1SlotUnknownError> =>
  Effect.map(l1NowUnixTimeMs(lucid), BigInt);

/**
 * `resolveL1NowMs` for the operator decision `use` (activation, status),
 * failing as a retryable `StateQueueError` while the L1 slot is unknown.
 */
export const resolveL1NowMsOrRefuse = (
  lucid: LucidEvolution,
  use: string,
): Effect.Effect<bigint, SDK.StateQueueError> =>
  Effect.mapError(
    resolveL1NowMs(lucid),
    (cause) =>
      new SDK.StateQueueError({
        message: `Operator ${use} needs the L1 slot`,
        cause,
      }),
  );

/**
 * Returns the time of Lucid's current slot in milliseconds, falling back to
 * emulator timing rules when necessary. For validity bounds only: on a live
 * network it follows the wall clock.
 */
export const currentTimeMsForLucidOrEmulatorFallback = (
  lucid: LucidEvolution,
): bigint =>
  BigInt(slotToUnixTimeForLucidOrEmulatorFallback(lucid, lucid.currentSlot()));

/**
 * Aligns a unix timestamp to the slot boundary used by the active Lucid
 * context.
 */
export const alignUnixTimeMsToSlotBoundary = (
  lucid: LucidEvolution,
  unixTimeMs: bigint,
): bigint => BigInt(alignUnixTimeToSlotBoundary(lucid, Number(unixTimeMs)));
