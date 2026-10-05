import {
  type AvailabilityResponderMissedDeadline,
  type AvailabilityResponderReport,
  availabilityResponderReportLine,
} from "./availability/responder.js";

export type AvailabilityResponseLoopTimers = {
  readonly setInterval: (run: () => void, ms: number) => unknown;
  readonly clearInterval: (handle: unknown) => void;
  readonly setTimeout: (run: () => void, ms: number) => unknown;
  readonly clearTimeout: (handle: unknown) => void;
};

/**
 * The responder's bounded retry burst (B6: bounded retries at the owning
 * operation). A failed drain (one that throws, or reports `failed`) is retried
 * after a doubling backoff capped at `backoffCeilingMs`, at most `retries`
 * times in a row. Once the burst is exhausted the loop holds: it schedules no
 * more fast retries, fails readiness with
 * `availability_responder_retries_exhausted`, and keeps draining on its poll
 * interval, so the first drain that no longer fails clears the reason. It
 * never exits. A burst therefore spends at most `retries x backoffCeilingMs`
 * of a challenge's response window before readiness reports it.
 */
export const AVAILABILITY_RESPONDER_RETRY_POLICY = {
  retries: 3,
  initialBackoffMs: 5_000,
  backoffCeilingMs: 10_000,
} as const;

/** The longest one retry burst can delay an answer, in milliseconds. */
export const availabilityResponderRetryBoundMs = (
  policy: Readonly<{
    retries: number;
    backoffCeilingMs: number;
  }> = AVAILABILITY_RESPONDER_RETRY_POLICY,
): number => policy.retries * policy.backoffCeilingMs;

export type AvailabilityResponseLoopEnforcement = Readonly<{
  pollIntervalCapMs: number;
  installed: () => void;
  scheduled: (expectedMonotonicMs: number) => void;
}>;

export type AvailabilityResponseLoopDeps = {
  /** One responder drain: every challenge ready to act on, nearest first. */
  readonly drain: () => Promise<AvailabilityResponderReport>;
  readonly write: (stream: "stdout" | "stderr", line: string) => void;
  readonly pollIntervalMs: number;
  readonly timers?: AvailabilityResponseLoopTimers;
  readonly retryPolicy?: typeof AVAILABILITY_RESPONDER_RETRY_POLICY;
  readonly enforcement?: AvailabilityResponseLoopEnforcement;
};

export type AvailabilityResponseLoop = {
  /** Starts the loop's own interval; the first drain runs at once. */
  readonly start: () => void;
  /** One drain, joining the one in flight if there is one. */
  readonly run: () => Promise<void>;
  /**
   * A drain, without waiting on it: the scan tick nudges the responder after
   * each scan. A nudge that lands during a drain queues one more drain for
   * when that one settles (several such nudges still queue one), since the
   * drain in flight may have read the cursor before the scan moved it.
   */
  readonly nudge: () => Promise<void>;
  /**
   * One readiness reason per challenge whose response deadline passed, one
   * for a held operation intent, and one once the retry burst is exhausted.
   */
  readonly reasons: () => readonly string[];
  readonly stop: () => void;
};

const defaultTimers: AvailabilityResponseLoopTimers = {
  setInterval: (run, ms) => setInterval(run, ms),
  clearInterval: (handle) => {
    clearInterval(handle as ReturnType<typeof setInterval>);
  },
  setTimeout: (run, ms) => setTimeout(run, ms),
  clearTimeout: (handle) => {
    clearTimeout(handle as ReturnType<typeof setTimeout>);
  },
};

/**
 * Runs the availability responder on its own interval, so a scan tick that
 * outlasts the poll interval cannot hold a challenge answer back past its
 * deadline. Each run drains the responder rather than taking one step, so
 * several open challenges are all answered, nearest deadline first.
 *
 * The responder is not gated on the L1 view the scan tick tracks: every step
 * reads its own authenticated boundary (the committee's cursor at the aligned
 * Kupmios tip) and waits as `awaiting_scan` otherwise, and answering a
 * challenge can only keep a payload available.
 *
 * A challenge found past its response deadline with a tranche unanswered is
 * one this committee can no longer answer. It is logged once as
 * `availability_challenge_deadline_missed` and makes the committee not ready
 * until a drain that discovered challenges no longer reports it (the
 * challenger timed it out, or it was settled).
 *
 * A failed drain is retried within AVAILABILITY_RESPONDER_RETRY_POLICY while
 * the loop runs; see there for what happens once that burst is exhausted.
 */
export const createAvailabilityResponseLoop = (
  deps: AvailabilityResponseLoopDeps,
): AvailabilityResponseLoop => {
  const timers = deps.timers ?? defaultTimers;
  const retryPolicy = deps.retryPolicy ?? AVAILABILITY_RESPONDER_RETRY_POLICY;
  if (
    deps.enforcement &&
    (!Number.isSafeInteger(deps.pollIntervalMs) ||
      deps.pollIntervalMs <= 0 ||
      deps.pollIntervalMs > deps.enforcement.pollIntervalCapMs)
  )
    throw new Error(
      "Availability poll interval exceeds its adopted capability",
    );
  let inFlight: Promise<void> | undefined;
  let followUp: Promise<void> | undefined;
  let interval: unknown;
  let missed = new Map<string, AvailabilityResponderMissedDeadline>();
  let held: string | undefined;
  let consecutiveFailures = 0;
  let retryTimer: unknown;
  let exhausted: string | undefined;

  const clearRetry = (): void => {
    if (retryTimer !== undefined) timers.clearTimeout(retryTimer);
    retryTimer = undefined;
  };

  // Counts drains that failed in a row. Each failure within the burst
  // schedules one backoff retry (while the loop runs); the one after the
  // burst marks it exhausted. Any drain that does not fail ends the burst.
  const noteOutcome = (failure: string | undefined): void => {
    if (failure === undefined) {
      consecutiveFailures = 0;
      exhausted = undefined;
      clearRetry();
      return;
    }
    consecutiveFailures += 1;
    if (consecutiveFailures > retryPolicy.retries) {
      clearRetry();
      if (exhausted === undefined)
        deps.write(
          "stderr",
          `${JSON.stringify({
            event: "availability_responder_retries_exhausted",
            retries: retryPolicy.retries,
            error: failure,
          })}\n`,
        );
      exhausted = failure;
      return;
    }
    if (interval === undefined || retryTimer !== undefined) return;
    const backoffMs = Math.min(
      retryPolicy.initialBackoffMs * 2 ** (consecutiveFailures - 1),
      retryPolicy.backoffCeilingMs,
    );
    retryTimer = timers.setTimeout(() => {
      retryTimer = undefined;
      void run();
    }, backoffMs);
  };

  // A held intent fails readiness until a drain reconciles without it. A
  // drain that never got past reconciliation (a lagging cursor, a throw)
  // proves nothing and keeps it.
  const noteHeld = (report: AvailabilityResponderReport): void => {
    if (report.status === "held") held = report.detail;
    else if (report.status !== "awaiting_scan") held = undefined;
  };

  const noteMissedDeadlines = (report: AvailabilityResponderReport): void => {
    const seen = new Map(
      (report.missedDeadlines ?? []).map((entry) => [entry.headerHash, entry]),
    );
    for (const [headerHash, entry] of seen) {
      if (missed.has(headerHash)) continue;
      deps.write(
        "stderr",
        `${JSON.stringify({ event: "availability_challenge_deadline_missed", ...entry })}\n`,
      );
    }
    // A drain that never reached discovery (a pending reconcile, a lagging
    // cursor) says nothing about the missed set, so it keeps the last one.
    if (report.challenges > 0 || report.status === "idle") missed = seen;
    else for (const [headerHash, entry] of seen) missed.set(headerHash, entry);
  };

  const drainOnce = async (): Promise<void> => {
    try {
      const report = await deps.drain();
      const logged = availabilityResponderReportLine(report);
      if (logged !== undefined) deps.write(logged.stream, logged.line);
      noteMissedDeadlines(report);
      noteHeld(report);
      noteOutcome(
        report.status === "failed"
          ? (report.detail ?? "availability responder step failed")
          : undefined,
      );
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      deps.write(
        "stderr",
        `${JSON.stringify({
          event: "availability_responder_failed",
          error: message,
        })}\n`,
      );
      noteOutcome(message);
    }
  };

  const run = (): Promise<void> => {
    inFlight ??= drainOnce().finally(() => {
      inFlight = undefined;
    });
    return inFlight;
  };

  return {
    start: () => {
      if (interval !== undefined) return;
      deps.enforcement?.installed();
      let scheduled = performance.now() + deps.pollIntervalMs;
      interval = timers.setInterval(() => {
        deps.enforcement?.scheduled(scheduled);
        scheduled += deps.pollIntervalMs;
        void run();
      }, deps.pollIntervalMs);
      void run();
    },
    run,
    nudge: () => {
      if (inFlight === undefined) return run();
      followUp ??= inFlight.then(() => {
        followUp = undefined;
        return run();
      });
      return Promise.resolve();
    },
    reasons: () => [
      ...[...missed.keys()].map(
        (headerHash) => `availability_challenge_deadline_missed:${headerHash}`,
      ),
      ...(held === undefined ? [] : [`availability_operation_held:${held}`]),
      ...(exhausted === undefined
        ? []
        : [`availability_responder_retries_exhausted:${exhausted}`]),
    ],
    stop: () => {
      if (interval !== undefined) timers.clearInterval(interval);
      interval = undefined;
      clearRetry();
    },
  };
};
