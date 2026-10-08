import { Effect, Metric, Ref, Schedule } from "effect";

import { Globals } from "../services/globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import type { NativeMpfOwnerService } from "../services/mpf-native-owner/protocol.js";
import { fullIndexHealthOf } from "../services/mpf-native-owner/service.full-index-accounting.js";
import type { NativeOwnerRestartHealth } from "../services/mpf-native-owner/service.restart-policy.js";

export const NATIVE_MPF_OWNER_SUPERVISOR_SOURCE = "native_mpf_owner";

/** Failed child restarts exhausted the owner's window. */
export const NATIVE_MPF_OWNER_RESTART_EXHAUSTED =
  "native_mpf_owner_restart_exhausted";

/** A committed canonical recovery is not installed yet. */
export const NATIVE_MPF_OWNER_RECOVERY_PENDING =
  "native_mpf_owner_recovery_pending";

/** The source the owner's last refused promotion raises under. It holds no
 * fiber: whatever asked for the promotion (block commitment, local
 * finalization, a landed-block rebase, a journal replay) already failed or
 * held at the owner and retries on its own schedule. */
export const NATIVE_MPF_PROMOTION_INDEX_CAP_SOURCE =
  "native_mpf_promotion_index_cap";

/** The owner refused to promote a root whose full index is over
 * `FULL_INDEX_MAX_RECORDS` or `FULL_INDEX_MAX_BYTES`, because its next start
 * could not load it (`NativeMpfPromotionIndexCapExceeded`, whose message
 * names the cap and its value). Native MPF, the store and the durable root
 * marker stay at the last promoted root; nothing is written, so the SQL root
 * and the journals stay as the refused caller left them. Cleared once a
 * promotion that fits succeeds (a merge or withdrawals that shrink the
 * ledger), or a canonical restore installs another root. A promotion over a
 * cap keeps being refused until the node runs a build whose caps cover it. */
export const NATIVE_MPF_PROMOTION_INDEX_CAP_EXCEEDED =
  "native_mpf_promotion_index_cap_exceeded";

export const nativeMpfOwnerRestartsInWindowGauge = Metric.gauge(
  "midgard_native_mpf_owner_restarts_in_window",
  {
    description:
      "Native MPF owner child restarts started inside the owner's restart window.",
  },
);

const restartHealthOf = (
  owner: NativeMpfOwnerService,
): NativeOwnerRestartHealth | undefined => {
  const candidate = owner as Partial<{
    restartHealth: () => NativeOwnerRestartHealth;
  }>;
  return typeof candidate.restartHealth === "function"
    ? candidate.restartHealth()
    : undefined;
};

/**
 * Surfaces the live Architecture G native owner's refusal in readiness instead
 * of stopping the node. The owner restarts its own child from the durable root
 * marker, unboundedly after a death and again once failed restarts leave its
 * window, and reading its refusal each tick also starts a restart that is due.
 * While it refuses, every commit, merge and recovery refuses at the owner and
 * retries on its own schedule. The owner is re-read each tick because recovery
 * flows replace it; the reason clears once the owner serves again or is gone.
 * The restart rate is published, and logs once at warning when it reaches the
 * restart limit inside the window. A promotion the owner refused over a
 * full-index cap is raised under its own source the same way, and cleared
 * once the owner no longer holds it.
 */
export const nativeMpfOwnerSupervisorFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<number, never, Globals> => {
  let restartRateHigh = false;
  return Effect.gen(function* () {
    const globals = yield* Globals;
    const owner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
    const failure = owner?.terminalFailure();
    const health = owner === undefined ? undefined : restartHealthOf(owner);
    yield* Metric.set(
      nativeMpfOwnerRestartsInWindowGauge,
      health?.restartsInWindow ?? 0,
    );
    const high =
      health !== undefined &&
      health.restartsInWindow >= Math.max(1, health.restartLimit);
    if (high && !restartRateHigh)
      yield* Effect.logWarning(
        `Native MPF owner child restarted ${health.restartsInWindow.toString()} time(s) within ${health.restartWindowMs.toString()} ms`,
      );
    restartRateHigh = high;
    const promotionRefusal =
      owner === undefined
        ? undefined
        : fullIndexHealthOf(owner)?.promotionRefusal;
    if (promotionRefusal === undefined)
      yield* clearLivenessIncident(
        globals,
        NATIVE_MPF_PROMOTION_INDEX_CAP_SOURCE,
      );
    else
      yield* raiseLivenessIncident(
        globals,
        NATIVE_MPF_PROMOTION_INDEX_CAP_SOURCE,
        NATIVE_MPF_PROMOTION_INDEX_CAP_EXCEEDED,
        `${promotionRefusal.message}. The owner stays at its last promoted root, and every caller retries; a promotion that fits clears this. Operator action: a promotion over the cap keeps being refused until the node runs a build whose caps cover the ledger.`,
        { escalateAfterMs: 0 },
      );
    if (failure === undefined)
      return yield* clearLivenessIncident(
        globals,
        NATIVE_MPF_OWNER_SUPERVISOR_SOURCE,
      );
    yield* raiseLivenessIncident(
      globals,
      NATIVE_MPF_OWNER_SUPERVISOR_SOURCE,
      health?.exhausted === true
        ? NATIVE_MPF_OWNER_RESTART_EXHAUSTED
        : NATIVE_MPF_OWNER_RECOVERY_PENDING,
      failure.message,
    );
  }).pipe(Effect.repeat(schedule));
};
