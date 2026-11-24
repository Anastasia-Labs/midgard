import { Effect, Metric, Ref, Schedule } from "effect";

import { Globals } from "../services/globals.globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import type { NativeMpfOwnerService } from "../services/mpf-native-owner/protocol.js";
import type { NativeOwnerRestartHealth } from "../services/mpf-native-owner/service.restart-policy.js";

export const NATIVE_MPF_OWNER_SUPERVISOR_SOURCE = "native_mpf_owner";

/** The owner holds: the same child failure repeated past its restart limit,
 * or one no restart repairs came (the binary is not the pinned one). It
 * restarts no more; the node stays up and unready until it is restarted. */
export const NATIVE_MPF_OWNER_STUCK = "native_mpf_owner_stuck";

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
 * marker with backoff after a failure, and reading its refusal each tick also
 * starts a restart that is due; the same failure repeated past the restart
 * limit holds it (`NATIVE_MPF_OWNER_STUCK`, logged at error), with no further
 * restarts. While it refuses, every commit, merge and recovery refuses at the
 * owner and retries on its own schedule. The owner is re-read each tick because recovery
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
      health?.held === true
        ? NATIVE_MPF_OWNER_STUCK
        : NATIVE_MPF_OWNER_RECOVERY_PENDING,
      failure.message,
      health?.held === true ? { escalateAfterMs: 0 } : {},
    );
  }).pipe(Effect.repeat(schedule));
};
