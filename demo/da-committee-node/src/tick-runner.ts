import type {
  CommitteeL1View,
  CommitteeRetentionReadinessSnapshot,
  CommitteeTickResult,
} from "./committee-service.js";
import type { RetentionL1View } from "./store/retention.js";

/**
 * Tick periods (one poll interval plus one tick each) an accepted L1 view may
 * age before readiness calls it stale.
 */
export const L1_VIEW_STALE_TICK_PERIODS = 3;

/**
 * The configured part of the readiness staleness bound: three poll intervals,
 * capped at the fatal deadline. The tick runner adds three times the duration
 * of the last tick that accepted a view, so the bound is three tick periods
 * of one interval plus one tick each, still capped at the fatal deadline.
 *
 * Ticks run one at a time, and a tick that outlasts its interval makes the
 * next one wait for the following interval, so consecutive ticks start at
 * most one interval plus one tick apart. A view accepted early in a tick is
 * then at most about two periods old when the second tick after it ends, so
 * three periods ride out two consecutive ticks that accept no view (a
 * transient read error, a scan outlasting its interval) and still flag a
 * committee that has stopped reading L1 long before the action deadline.
 */
export const l1ViewStaleMs = (config: {
  readonly pollIntervalMs: number;
  readonly l1ViewFatalMs: number;
}): number =>
  Math.min(
    config.l1ViewFatalMs,
    L1_VIEW_STALE_TICK_PERIODS * config.pollIntervalMs,
  );

/**
 * Raised by the single-pass retention step when no fresh authenticated L1 view
 * has been obtained for longer than the configured deadline. The tick loop
 * never raises it: past the deadline it reports `l1_view_unavailable` and
 * keeps ticking, since every decision, signature, submission and prune needs
 * a view its own tick accepted (see {@link createCommitteeTickRunner}).
 */
export class L1ViewUnavailableError extends Error {
  readonly l1ViewAgeMs: number;
  readonly l1ViewFatalMs: number;

  constructor(l1ViewAgeMs: number, l1ViewFatalMs: number) {
    super(
      `no authenticated L1 view for ${l1ViewAgeMs.toString()} ms (deadline ${l1ViewFatalMs.toString()} ms)`,
    );
    this.name = "L1ViewUnavailableError";
    this.l1ViewAgeMs = l1ViewAgeMs;
    this.l1ViewFatalMs = l1ViewFatalMs;
  }
}

export type CommitteeTickRunnerDeps = {
  readonly tick: () => Promise<CommitteeTickResult>;
  readonly runAvailabilityResponse: () => Promise<void>;
  /**
   * One read of the pooled DA bond after the tick, so pool alerts and the
   * readiness reason follow the pool even when no apply is pending. The
   * reader reports its own read failures; anything it throws is logged and
   * fails nothing else.
   */
  readonly readDaBondPool?: () => Promise<void>;
  /** One retention pass against a fresh L1 view. */
  readonly runRetention: (view: RetentionL1View) => Promise<void>;
  /**
   * The last accepted L1 view. Each acceptance is a new object, so a view
   * accepted during a tick is told apart from the one before it by identity,
   * never by wall-clock time, which can step backwards.
   */
  readonly latestL1View: () => CommitteeL1View | undefined;
  /**
   * When a tick last made authenticated progress toward an L1 view without
   * reaching one (catching up on a long state-queue history). The deadline
   * counts such progress as a fresh view, so a node catching up after
   * downtime is not stopped for being behind.
   */
  readonly latestL1ProgressAtMs: () => number | undefined;
  /** Replaces the retention readiness with `update` of the current one. */
  readonly setRetentionReadiness: (
    update: (
      previous: CommitteeRetentionReadinessSnapshot,
    ) => CommitteeRetentionReadinessSnapshot,
  ) => void;
  readonly l1ViewFatalMs: number;
  /**
   * Configured part of the age past which the last accepted view makes
   * readiness report `l1_view_stale` (see {@link l1ViewStaleMs}); the runner
   * adds three times the last view-accepting tick's duration, capped at
   * `l1ViewFatalMs`. A skipped pass on a younger view is readiness detail
   * only. Defaults to `l1ViewFatalMs`.
   */
  readonly l1ViewStaleMs?: number;
  /** Deadline base before the first L1 view is ever accepted. */
  readonly startedAtMs: number;
  readonly nowMs: () => number;
  readonly write: (stream: "stdout" | "stderr", line: string) => void;
  /**
   * A tick that takes longer than this, normally the poll interval, logs one
   * `committee_tick_slow` line with its phase timings. Fast ticks log nothing.
   */
  readonly slowTickMs?: number;
};

/**
 * Liveness of the tick loop for readiness and health. `l1ViewUnavailable` is
 * set while the last accepted view, or later progress toward one, is older
 * than `l1ViewFatalMs`; it clears by itself once a tick accepts a view.
 * `tickHung` is set while one tick has run longer than that deadline without
 * any L1 progress: the loop cannot start another tick behind it, so this is
 * the one state that fails health and lets the supervisor replace the
 * process.
 */
export type CommitteeTickLiveness = {
  readonly l1ViewUnavailable?: {
    readonly l1ViewAgeMs: number;
    readonly l1ViewFatalMs: number;
  };
  readonly tickHung?: { readonly inFlightMs: number };
  /** Retention passes skipped in a row for want of a fresh view. */
  readonly consecutiveRetentionSkips: number;
};

const errorText = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

type SkippedRetentionPass = NonNullable<
  CommitteeRetentionReadinessSnapshot["skippedPass"]
>;

/**
 * Readiness after a pass skipped on a view younger than the staleness bound:
 * the last pass's outcome stands with the skip as detail, except that an
 * `l1_view_stale` outcome is over (the view is fresh again), so it becomes
 * `skipped` until the next pass runs.
 */
const retentionReadinessAfterFreshSkip = (
  previous: CommitteeRetentionReadinessSnapshot,
  skippedPass: SkippedRetentionPass,
): CommitteeRetentionReadinessSnapshot =>
  previous.status === "l1_view_stale"
    ? {
        status: "skipped",
        scanned: 0,
        retained: 0,
        prunable: 0,
        alerting: 0,
        skippedPass,
      }
    : { ...previous, skippedPass };

export const createCommitteeTickRunner = (deps: CommitteeTickRunnerDeps) => {
  const configuredStaleMs = deps.l1ViewStaleMs ?? deps.l1ViewFatalMs;
  let tickInFlight = false;
  let viewBeforeInFlightTick: CommitteeL1View | undefined;
  /**
   * Duration of the last completed tick that accepted a view. Ticks that
   * accept none do not count, so slow failing reads never widen the bound.
   */
  let lastViewTickMs = 0;
  let inFlightSinceMs: number | undefined;
  let unavailableSinceMs: number | undefined;
  let consecutiveRetentionSkips = 0;

  /** The readiness staleness bound at the current tick cadence. */
  const staleBoundMs = (): number =>
    Math.min(
      deps.l1ViewFatalMs,
      configuredStaleMs + L1_VIEW_STALE_TICK_PERIODS * lastViewTickMs,
    );

  /**
   * Age of the last accepted L1 view or of later progress toward one, from
   * startup when there was neither.
   */
  const l1ViewAge = (): {
    readonly nowMs: number;
    readonly l1ViewAgeMs: number;
  } => {
    const nowMs = deps.nowMs();
    const freshest = Math.max(
      deps.latestL1View()?.observedAtMs ?? Number.NEGATIVE_INFINITY,
      deps.latestL1ProgressAtMs() ?? Number.NEGATIVE_INFINITY,
    );
    return {
      nowMs,
      l1ViewAgeMs:
        nowMs -
        (freshest === Number.NEGATIVE_INFINITY ? deps.startedAtMs : freshest),
    };
  };

  const assertWithinDeadline = (l1ViewAgeMs: number): void => {
    if (l1ViewAgeMs > deps.l1ViewFatalMs) {
      throw new L1ViewUnavailableError(l1ViewAgeMs, deps.l1ViewFatalMs);
    }
  };

  /**
   * Records a pass that has no L1 view from its own tick: nothing is pruned.
   * Readiness reports `l1_view_stale` only when no view was ever accepted or
   * the last one is older than the staleness bound; a skip on a younger view
   * is detail.
   */
  const skipWithoutL1View = (
    reason: SkippedRetentionPass["reason"],
    l1Error?: string,
  ): void => {
    const { nowMs, l1ViewAgeMs } = l1ViewAge();
    const staleMs = staleBoundMs();
    consecutiveRetentionSkips += 1;
    deps.write(
      "stderr",
      `${JSON.stringify({
        event: "retention_pass_skipped",
        reason,
        l1ViewAgeMs,
        l1ViewStaleMs: staleMs,
        l1ViewFatalMs: deps.l1ViewFatalMs,
        consecutiveSkips: consecutiveRetentionSkips,
        ...(l1Error === undefined ? {} : { error: l1Error }),
      })}\n`,
    );
    const view = deps.latestL1View();
    const checkedAt = new Date(nowMs).toISOString();
    // Readiness judges the view itself: catch-up progress defers the exit,
    // but it is no view.
    const viewAgeMs = nowMs - (view?.observedAtMs ?? deps.startedAtMs);
    if (view === undefined || viewAgeMs > staleMs) {
      deps.setRetentionReadiness(() => ({
        status: "l1_view_stale",
        checkedAt,
        l1ViewAgeMs: viewAgeMs,
        scanned: 0,
        retained: 0,
        prunable: 0,
        alerting: 0,
      }));
      return;
    }
    deps.setRetentionReadiness((previous) =>
      retentionReadinessAfterFreshSkip(previous, {
        reason,
        checkedAt,
        l1ViewAgeMs: viewAgeMs,
      }),
    );
  };

  /**
   * Runs retention only against an L1 view accepted during the current tick:
   * one other than `viewBeforeTick`, the view the tick started with.
   * Otherwise the pass is skipped and nothing is pruned. Resolves whether the
   * pass ran.
   */
  const retentionStep = async (
    viewBeforeTick: CommitteeL1View | undefined,
    l1Error?: string,
  ): Promise<boolean> => {
    const view = deps.latestL1View();
    if (view !== undefined && view !== viewBeforeTick) {
      consecutiveRetentionSkips = 0;
      await deps.runRetention(view);
      return true;
    }
    skipWithoutL1View("l1_view_unavailable", l1Error);
    return false;
  };

  /**
   * The single-pass retention step (`--once`): as the loop's, but a skipped
   * pass past the deadline throws `L1ViewUnavailableError`, since a single
   * pass has no later tick to recover in.
   */
  const runRetentionStep = async (
    viewBeforeTick: CommitteeL1View | undefined,
    l1Error?: string,
  ): Promise<void> => {
    if (!(await retentionStep(viewBeforeTick, l1Error))) {
      assertWithinDeadline(l1ViewAge().l1ViewAgeMs);
    }
  };

  /**
   * Logs entering and leaving the past-deadline state, once each. Nothing
   * here acts: the gate is that the next tick decides only on a view it
   * accepts itself.
   */
  const noteL1ViewAvailability = (): void => {
    const { nowMs, l1ViewAgeMs } = l1ViewAge();
    if (l1ViewAgeMs > deps.l1ViewFatalMs) {
      if (unavailableSinceMs !== undefined) return;
      unavailableSinceMs = nowMs;
      deps.write(
        "stderr",
        `${JSON.stringify({
          event: "l1_view_unavailable",
          l1ViewAgeMs,
          l1ViewFatalMs: deps.l1ViewFatalMs,
        })}\n`,
      );
    } else if (unavailableSinceMs !== undefined) {
      deps.write(
        "stderr",
        `${JSON.stringify({
          event: "l1_view_recovered",
          unavailableForMs: nowMs - unavailableSinceMs,
          l1ViewAgeMs,
        })}\n`,
      );
      unavailableSinceMs = undefined;
    }
  };

  /** The loop's liveness now, computed from the clock rather than a tick. */
  const liveness = (): CommitteeTickLiveness => {
    const { nowMs, l1ViewAgeMs } = l1ViewAge();
    const pastDeadline = l1ViewAgeMs > deps.l1ViewFatalMs;
    const inFlightMs =
      inFlightSinceMs === undefined ? undefined : nowMs - inFlightSinceMs;
    return {
      ...(pastDeadline
        ? {
            l1ViewUnavailable: {
              l1ViewAgeMs,
              l1ViewFatalMs: deps.l1ViewFatalMs,
            },
          }
        : {}),
      ...(pastDeadline &&
      inFlightMs !== undefined &&
      inFlightMs > deps.l1ViewFatalMs
        ? { tickHung: { inFlightMs } }
        : {}),
      consecutiveRetentionSkips,
    };
  };

  /**
   * One polling tick behind the process error boundary. Failures are logged
   * and the next tick retries; nothing here exits the process. Past the
   * L1-view deadline the tick still runs: the service decides, signs and
   * submits only on a view its own scan accepted, and retention prunes only
   * on a view accepted this tick, so a committee that cannot read L1 refuses
   * every such action and resumes by itself on the first accepted view. A
   * tick that finds the previous one still running does not start another.
   */
  const runTick = async (): Promise<void> => {
    if (tickInFlight) {
      deps.write(
        "stderr",
        `${JSON.stringify({ event: "committee_tick_overlap_prevented" })}\n`,
      );
      try {
        // The in-flight tick may already hold a fresh view and simply be slow
        // (L1 actuation runs inside it); only the deadline applies then, and
        // an `l1_view_stale` verdict from before that view is over. Otherwise
        // this interval has no view.
        const view = deps.latestL1View();
        if (view !== undefined && view !== viewBeforeInFlightTick) {
          const { nowMs } = l1ViewAge();
          deps.setRetentionReadiness((previous) =>
            previous.status === "l1_view_stale"
              ? retentionReadinessAfterFreshSkip(previous, {
                  reason: "tick_in_flight",
                  checkedAt: new Date(nowMs).toISOString(),
                  l1ViewAgeMs: nowMs - view.observedAtMs,
                })
              : previous,
          );
        } else skipWithoutL1View("tick_in_flight");
        noteL1ViewAvailability();
      } catch (error) {
        deps.write("stderr", `${errorText(error)}\n`);
      }
      return;
    }
    tickInFlight = true;
    const tickStartedAtMs = deps.nowMs();
    inFlightSinceMs = tickStartedAtMs;
    const viewBeforeTick = deps.latestL1View();
    viewBeforeInFlightTick = viewBeforeTick;
    let l1Error: string | undefined;
    // Where a slow tick spent its time; a phase that did not run is absent.
    const phaseMs: Record<string, number> = {};
    let phaseStartedAtMs = tickStartedAtMs;
    const endPhase = (phase: string): void => {
      const nowMs = deps.nowMs();
      phaseMs[phase] = nowMs - phaseStartedAtMs;
      phaseStartedAtMs = nowMs;
    };
    try {
      try {
        const result = await deps.tick();
        endPhase("tickMs");
        if (result.errors.length > 0) {
          l1Error = result.errors.join("; ");
          deps.write("stderr", `${JSON.stringify(result)}\n`);
        }
        await deps.runAvailabilityResponse();
        endPhase("availabilityResponseMs");
      } catch (error) {
        endPhase(
          phaseMs.tickMs === undefined ? "tickMs" : "availabilityResponseMs",
        );
        l1Error ??= errorText(error);
        deps.write(
          "stderr",
          `${error instanceof Error ? (error.stack ?? error.message) : String(error)}\n`,
        );
      }
      if (deps.readDaBondPool !== undefined) {
        try {
          await deps.readDaBondPool();
        } catch (error) {
          deps.write("stderr", `${errorText(error)}\n`);
        }
        endPhase("daBondPoolMs");
      }
      await retentionStep(viewBeforeTick, l1Error);
      endPhase("retentionMs");
    } catch (error) {
      endPhase("retentionMs");
      deps.write(
        "stderr",
        `${error instanceof Error ? (error.stack ?? error.message) : String(error)}\n`,
      );
    } finally {
      tickInFlight = false;
      inFlightSinceMs = undefined;
      noteL1ViewAvailability();
      const durationMs = deps.nowMs() - tickStartedAtMs;
      if (deps.latestL1View() !== viewBeforeTick)
        lastViewTickMs = Math.max(0, durationMs);
      if (deps.slowTickMs !== undefined && durationMs > deps.slowTickMs) {
        deps.write(
          "stderr",
          `${JSON.stringify({
            event: "committee_tick_slow",
            durationMs,
            slowTickMs: deps.slowTickMs,
            ...phaseMs,
          })}\n`,
        );
      }
    }
  };

  return { runTick, runRetentionStep, liveness };
};

/**
 * Starts the node's tick loop: the SIGINT/SIGTERM handlers first, then the
 * first tick, then one tick every `pollIntervalMs`.
 *
 * The handlers come first because the first tick can wait on L1 for a long
 * time; a stop during it still shuts down and exits 0. A stop before the
 * first tick finishes starts no interval. Resolves the interval, or
 * `undefined` when the node was stopped during the first tick.
 */
export const startCommitteeTickLoop = async (deps: {
  readonly runTick: () => Promise<void>;
  readonly pollIntervalMs: number;
  readonly shutdown: () => Promise<void>;
  readonly exit: (code: number) => void;
  readonly onSignal?: (
    signal: "SIGINT" | "SIGTERM",
    handler: () => void,
  ) => void;
}): Promise<ReturnType<typeof setInterval> | undefined> => {
  const onSignal =
    deps.onSignal ??
    ((signal: "SIGINT" | "SIGTERM", handler: () => void) => {
      process.once(signal, handler);
    });
  let stopping = false;
  const stop = (): void => {
    stopping = true;
    void deps.shutdown().then(() => deps.exit(0));
  };
  onSignal("SIGINT", stop);
  onSignal("SIGTERM", stop);
  await deps.runTick();
  if (stopping) {
    return undefined;
  }
  // runTick contains its own error boundary and overlap guard.
  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  return setInterval(deps.runTick, deps.pollIntervalMs);
};
