import { MAX_TIMER_MS } from "./journey-timing.journey-timing-for-plan.js";

/**
 * A family's time limit that does not run while its correction waits.
 *
 * The family's limit (`journeyTimeoutMs`, or a stack's
 * `stackJourneyTimeoutMs`) budgets every stage but the correction. The
 * correction is ended only by the workflow journal's progress
 * (`verifyJourneyCorrection`), however many transactions it takes, so the
 * limit pauses while `excluding` runs it. L1 keeps producing blocks
 * meanwhile, and the healthy replay after the correction must walk them too:
 * at replay utilization `u` (`healthyJourneyReplayTiming`) a pause of Δ adds
 * Δ·u/(1−u) of replay, so the limit resumes with that much more, and the
 * family is held Δ/(1−u) in all. `signal` aborts once the limit passes;
 * timers never exceed `MAX_TIMER_MS`. `dispose` stops it.
 */
export const journeyFamilyDeadline = ({
  limitMs,
  replayUtilization,
}: Readonly<{ limitMs: number; replayUtilization: number }>) => {
  if (
    !Number.isSafeInteger(limitMs) ||
    limitMs <= 0 ||
    limitMs > MAX_TIMER_MS ||
    !Number.isFinite(replayUtilization) ||
    replayUtilization < 0 ||
    replayUtilization >= 1
  )
    throw new Error(
      `Invalid family limit ${limitMs.toString()} ms or replay utilization ${replayUtilization.toString()}`,
    );
  const controller = new AbortController();
  let remainingMs = limitMs;
  let runningSince = performance.now();
  let timer: ReturnType<typeof setTimeout> | undefined;
  let paused = false;
  let disposed = false;
  const spend = () => {
    const now = performance.now();
    remainingMs -= now - runningSince;
    runningSince = now;
  };
  const arm = () => {
    if (disposed || paused || controller.signal.aborted) return;
    if (remainingMs <= 0) {
      controller.abort(
        new Error(`The family's ${limitMs.toString()} ms limit passed`),
      );
      return;
    }
    timer = setTimeout(
      () => {
        timer = undefined;
        spend();
        arm();
      },
      Math.min(Math.ceil(remainingMs), MAX_TIMER_MS),
    );
  };
  arm();
  return Object.freeze({
    signal: controller.signal,
    /** Runs `work` with the limit paused, then resumes it extended. */
    excluding: async <T>(work: () => Promise<T>): Promise<T> => {
      if (paused) throw new Error("The family's limit is already paused");
      spend();
      clearTimeout(timer);
      timer = undefined;
      paused = true;
      const pausedAt = performance.now();
      try {
        return await work();
      } finally {
        const now = performance.now();
        remainingMs +=
          ((now - pausedAt) * replayUtilization) / (1 - replayUtilization);
        runningSince = now;
        paused = false;
        arm();
      }
    },
    dispose: () => {
      disposed = true;
      clearTimeout(timer);
      timer = undefined;
    },
  });
};
