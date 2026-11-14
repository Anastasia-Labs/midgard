/**
 * The open-loop start scheduler shared by the calibration stage and the
 * measured load stage of `throughput-valid-stress.mjs`.
 *
 * It lives in its own module so the scheduling contract can be exercised
 * directly against a fake clock. The alternative — asserting substrings of the
 * 4,200-line stress script — proved nothing about runtime behaviour and broke
 * on every rename.
 *
 * The clock and the sleep are injected purely so a test can drive them; the
 * script passes the real ones.
 */

const realSleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const waitForAnyInFlight = async (inFlight) => {
  if (inFlight.size === 0) {
    return;
  }
  await Promise.race(inFlight);
};

/**
 * How many starts of an evenly spaced schedule are due at `nowPerfMs`.
 *
 * The first start is due at `startedAtPerfMs` itself, so the count is
 * `floor(elapsed / intervalMs) + 1`, clamped to `totalStarts`.
 */
export const scheduledStartCountDue = ({
  nowPerfMs,
  startedAtPerfMs,
  intervalMs,
  totalStarts,
}) => {
  if (nowPerfMs < startedAtPerfMs) {
    return 0;
  }
  return Math.min(
    totalStarts,
    Math.floor((nowPerfMs - startedAtPerfMs) / intervalMs) + 1,
  );
};

/**
 * Dispatch `totalStarts` starts on an evenly spaced schedule, batching every
 * start that became due during a coarse timer wake.
 *
 * Node timers do not resolve sub-millisecond deadlines, so a per-start
 * `sleep(intervalMs)` cannot hold a high rate: the scheduler sleeps once to the
 * next due deadline and then dispatches the whole accumulated batch without
 * further timers, bounded by `maxInFlight`.
 *
 * At `deadlinePerfMs` a hard-deadline stage stops after dispatching only what
 * fits in immediately available capacity; `allowPostDeadlineCatchUp` lets the
 * calibration stage finish its schedule instead and report the slip.
 */
export const runDeadlineBatchedSchedule = async ({
  totalStarts,
  startedAtPerfMs,
  deadlinePerfMs,
  intervalMs,
  maxInFlight,
  dispatchStart,
  allowPostDeadlineCatchUp = false,
  now = () => performance.now(),
  sleep = realSleep,
}) => {
  const inFlight = new Set();
  let nextStartIndex = 0;
  let maxObservedInFlight = 0;
  let lastDispatchedAtPerfMs = null;
  let stoppedWithoutCapacity = false;

  while (nextStartIndex < totalStarts) {
    while (
      inFlight.size >= maxInFlight &&
      (allowPostDeadlineCatchUp || now() < deadlinePerfMs)
    ) {
      await waitForAnyInFlight(inFlight);
    }

    const nowPerfMs = now();
    const deadlineReached = nowPerfMs >= deadlinePerfMs;
    const dueStarts = deadlineReached
      ? totalStarts
      : scheduledStartCountDue({
          nowPerfMs,
          startedAtPerfMs,
          intervalMs,
          totalStarts,
        });
    if (nextStartIndex >= dueStarts) {
      const nextDueAtPerfMs = startedAtPerfMs + nextStartIndex * intervalMs;
      const waitMs = Math.min(
        nextDueAtPerfMs - nowPerfMs,
        deadlinePerfMs - nowPerfMs,
      );
      if (waitMs > 0) {
        // One coarse wake intentionally accumulates every start that becomes
        // due; the next iteration dispatches that whole batch.
        await sleep(Math.max(1, Math.ceil(waitMs)));
      }
      continue;
    }

    let dispatchedAny = false;
    while (nextStartIndex < dueStarts && inFlight.size < maxInFlight) {
      const startIndex = nextStartIndex;
      const scheduledAtPerfMs = startedAtPerfMs + startIndex * intervalMs;
      const dispatched = dispatchStart({ startIndex, scheduledAtPerfMs });
      if (dispatched === null) {
        break;
      }
      let tracked;
      tracked = Promise.resolve(dispatched).finally(() => {
        inFlight.delete(tracked);
      });
      inFlight.add(tracked);
      nextStartIndex += 1;
      dispatchedAny = true;
      lastDispatchedAtPerfMs = now();
      maxObservedInFlight = Math.max(maxObservedInFlight, inFlight.size);
    }

    // A coarse timer may wake just beyond the hard deadline. Hard-deadline
    // stages dispatch the already-due final batch only into immediately
    // available capacity. Calibration may explicitly catch up instead: its
    // last-dispatch rate and schedule-slip gates expose any real shortfall.
    if (deadlineReached && !allowPostDeadlineCatchUp) {
      break;
    }

    if (nextStartIndex < dueStarts && !dispatchedAny) {
      if (inFlight.size === 0) {
        stoppedWithoutCapacity = true;
        break;
      }
      await waitForAnyInFlight(inFlight);
    }
  }

  await Promise.all(inFlight);
  return {
    scheduledStarts: nextStartIndex,
    missedStarts: totalStarts - nextStartIndex,
    maxObservedInFlight,
    lastDispatchedAtPerfMs,
    stoppedWithoutCapacity,
  };
};
