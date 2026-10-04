import {
  type AvailabilityResponderMissedDeadline,
  type AvailabilityResponderReport,
  availabilityResponderReportLine,
} from "./availability/responder.js";

export type AvailabilityResponseLoopTimers = {
  readonly setInterval: (run: () => void, ms: number) => unknown;
  readonly clearInterval: (handle: unknown) => void;
};

export type AvailabilityResponseLoopDeps = {
  /** One responder drain: every challenge ready to act on, nearest first. */
  readonly drain: () => Promise<AvailabilityResponderReport>;
  readonly write: (stream: "stdout" | "stderr", line: string) => void;
  readonly pollIntervalMs: number;
  readonly timers?: AvailabilityResponseLoopTimers;
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
   * One readiness reason per challenge whose response deadline passed, and
   * one for a held operation intent.
   */
  readonly reasons: () => readonly string[];
  readonly stop: () => void;
};

const defaultTimers: AvailabilityResponseLoopTimers = {
  setInterval: (run, ms) => setInterval(run, ms),
  clearInterval: (handle) => {
    clearInterval(handle as ReturnType<typeof setInterval>);
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
 */
export const createAvailabilityResponseLoop = (
  deps: AvailabilityResponseLoopDeps,
): AvailabilityResponseLoop => {
  const timers = deps.timers ?? defaultTimers;
  let inFlight: Promise<void> | undefined;
  let followUp: Promise<void> | undefined;
  let interval: unknown;
  let missed = new Map<string, AvailabilityResponderMissedDeadline>();
  let held: string | undefined;

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
    } catch (error) {
      deps.write(
        "stderr",
        `${JSON.stringify({
          event: "availability_responder_failed",
          error: error instanceof Error ? error.message : String(error),
        })}\n`,
      );
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
      interval = timers.setInterval(() => {
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
    ],
    stop: () => {
      if (interval !== undefined) timers.clearInterval(interval);
      interval = undefined;
    },
  };
};
