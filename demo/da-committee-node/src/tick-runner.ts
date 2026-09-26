import type {
  CommitteeL1View,
  CommitteeRetentionReadinessSnapshot,
  CommitteeTickResult,
} from "./committee-service.js";
import type { RetentionL1View } from "./store/retention.js";

/** Process exit code when the committee has lost its authenticated L1 view. */
export const L1_VIEW_UNAVAILABLE_EXIT_CODE = 70;

/**
 * Raised when no fresh authenticated L1 view has been obtained for longer than
 * the configured deadline. A committee that cannot read L1 can neither see new
 * headers, attest, nor decide retention, so it must stop rather than run on a
 * stale view.
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
  /** One retention pass against a fresh L1 view. */
  readonly runRetention: (view: RetentionL1View) => Promise<void>;
  readonly latestL1View: () => CommitteeL1View | undefined;
  /**
   * When a tick last made authenticated progress toward an L1 view without
   * reaching one (catching up on a long state-queue history). The deadline
   * counts such progress as a fresh view, so a node catching up after
   * downtime is not stopped for being behind.
   */
  readonly latestL1ProgressAtMs: () => number | undefined;
  readonly setRetentionReadiness: (
    snapshot: CommitteeRetentionReadinessSnapshot,
  ) => void;
  readonly l1ViewFatalMs: number;
  /** Deadline base before the first L1 view is ever accepted. */
  readonly startedAtMs: number;
  readonly nowMs: () => number;
  readonly write: (stream: "stdout" | "stderr", line: string) => void;
  readonly shutdown: () => Promise<void>;
  readonly exit: (code: number) => void;
  /**
   * Upper bound on the shutdown before the fatal exit. A hung tick can hold
   * resources `shutdown` waits on, so the exit never depends on it settling.
   */
  readonly shutdownGraceMs?: number;
};

const DEFAULT_SHUTDOWN_GRACE_MS = 10_000;

const errorText = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

export const createCommitteeTickRunner = (deps: CommitteeTickRunnerDeps) => {
  let tickInFlight = false;
  let inFlightTickStartedAtMs = 0;
  let exiting = false;

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
   * Records a pass that has no fresh L1 view: nothing is pruned, readiness
   * reports `l1_view_stale`, and past the deadline `L1ViewUnavailableError`
   * is thrown.
   */
  const skipWithoutL1View = (
    reason: "l1_view_unavailable" | "tick_in_flight",
    l1Error?: string,
  ): void => {
    const { nowMs, l1ViewAgeMs } = l1ViewAge();
    deps.write(
      "stderr",
      `${JSON.stringify({
        event: "retention_pass_skipped",
        reason,
        l1ViewAgeMs,
        l1ViewFatalMs: deps.l1ViewFatalMs,
        ...(l1Error === undefined ? {} : { error: l1Error }),
      })}\n`,
    );
    assertWithinDeadline(l1ViewAgeMs);
    deps.setRetentionReadiness({
      status: "l1_view_stale",
      checkedAt: new Date(nowMs).toISOString(),
      l1ViewAgeMs,
      scanned: 0,
      retained: 0,
      prunable: 0,
      alerting: 0,
    });
  };

  /**
   * Runs retention only against an L1 view accepted during the current tick.
   * Otherwise the pass is skipped, nothing is pruned, and once the last
   * accepted view is older than the deadline `L1ViewUnavailableError` is
   * thrown.
   */
  const runRetentionStep = async (
    tickStartedAtMs: number,
    l1Error?: string,
  ): Promise<void> => {
    const view = deps.latestL1View();
    if (view !== undefined && view.observedAtMs >= tickStartedAtMs) {
      await deps.runRetention(view);
      return;
    }
    skipWithoutL1View("l1_view_unavailable", l1Error);
  };

  /**
   * Shuts down and exits with `L1_VIEW_UNAVAILABLE_EXIT_CODE`, at most once.
   * The shutdown is bounded by `shutdownGraceMs`, so a hung tick holding a
   * resource cannot keep the process alive past the deadline.
   */
  const exitForUnavailableL1View = async (
    error: L1ViewUnavailableError,
  ): Promise<void> => {
    if (exiting) return;
    exiting = true;
    deps.write(
      "stderr",
      `${JSON.stringify({
        event: "l1_view_unavailable_exit",
        l1ViewAgeMs: error.l1ViewAgeMs,
        l1ViewFatalMs: error.l1ViewFatalMs,
        exitCode: L1_VIEW_UNAVAILABLE_EXIT_CODE,
      })}\n`,
    );
    let grace: ReturnType<typeof setTimeout> | undefined;
    try {
      await Promise.race([
        deps.shutdown(),
        new Promise<void>((resolve) => {
          grace = setTimeout(
            resolve,
            deps.shutdownGraceMs ?? DEFAULT_SHUTDOWN_GRACE_MS,
          );
          grace.unref?.();
        }),
      ]);
    } finally {
      clearTimeout(grace);
      deps.exit(L1_VIEW_UNAVAILABLE_EXIT_CODE);
    }
  };

  /**
   * One polling tick behind the process error boundary. Ordinary failures are
   * logged and the next tick retries; only `L1ViewUnavailableError` shuts the
   * process down with `L1_VIEW_UNAVAILABLE_EXIT_CODE`. A tick that finds the
   * previous one still running does not start another, but still applies the
   * L1-view deadline, so a hung L1 read cannot keep the process alive on a
   * stale view.
   */
  const runTick = async (): Promise<void> => {
    if (exiting) return;
    if (tickInFlight) {
      deps.write(
        "stderr",
        `${JSON.stringify({ event: "committee_tick_overlap_prevented" })}\n`,
      );
      try {
        // The in-flight tick may already hold a fresh view and simply be slow;
        // only the deadline applies then. Otherwise this interval has no view.
        const view = deps.latestL1View();
        if (view !== undefined && view.observedAtMs >= inFlightTickStartedAtMs)
          assertWithinDeadline(l1ViewAge().l1ViewAgeMs);
        else skipWithoutL1View("tick_in_flight");
      } catch (error) {
        if (error instanceof L1ViewUnavailableError) {
          await exitForUnavailableL1View(error);
          return;
        }
        deps.write("stderr", `${errorText(error)}\n`);
      }
      return;
    }
    tickInFlight = true;
    const tickStartedAtMs = deps.nowMs();
    inFlightTickStartedAtMs = tickStartedAtMs;
    let l1Error: string | undefined;
    try {
      try {
        const result = await deps.tick();
        if (result.errors.length > 0) {
          l1Error = result.errors.join("; ");
          deps.write("stderr", `${JSON.stringify(result)}\n`);
        }
        await deps.runAvailabilityResponse();
      } catch (error) {
        l1Error ??= errorText(error);
        deps.write(
          "stderr",
          `${error instanceof Error ? (error.stack ?? error.message) : String(error)}\n`,
        );
      }
      if (exiting) return;
      await runRetentionStep(tickStartedAtMs, l1Error);
    } catch (error) {
      if (error instanceof L1ViewUnavailableError) {
        await exitForUnavailableL1View(error);
        return;
      }
      deps.write(
        "stderr",
        `${error instanceof Error ? (error.stack ?? error.message) : String(error)}\n`,
      );
    } finally {
      tickInFlight = false;
    }
  };

  return { runTick, runRetentionStep };
};
