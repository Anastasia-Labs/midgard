import { Effect, Metric, Ref, Schedule } from "effect";

import { Globals } from "../services/globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import type { NativeMpfOwnerService } from "../services/mpf-native-owner/protocol.js";
import type { NativeOwnerRestartHealth } from "../services/mpf-native-owner/service.restart-policy.js";

export const NATIVE_MPF_OWNER_SUPERVISOR_SOURCE = "native_mpf_owner";

/** Failed child restarts exhausted the owner's window. */
export const NATIVE_MPF_OWNER_RESTART_EXHAUSTED =
  "native_mpf_owner_restart_exhausted";

/** A committed canonical recovery is not installed yet. */
export const NATIVE_MPF_OWNER_RECOVERY_PENDING =
  "native_mpf_owner_recovery_pending";

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
 * restart limit inside the window.
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
