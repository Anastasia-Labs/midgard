import type { CommitteeL1SubmitterPreflightSnapshot } from "./committee-service.js";
import {
  type L1SubmitterPreflightResult,
  l1SubmitterPreflightResultToJson,
} from "./l1/submitter.js";
import { AutoFundPaymentUnsettledError } from "./l1/submitter.prune-in-flight-spends.js";

/** First wait before re-trying a preflight that could not be evaluated. */
export const L1_SUBMITTER_PREFLIGHT_RETRY_INITIAL_MS = 5_000;
/** Ceiling of that wait as it doubles. */
export const L1_SUBMITTER_PREFLIGHT_RETRY_MAX_MS = 60_000;
/** Period of the read-only re-check of a wallet that was evaluated short. */
export const L1_SUBMITTER_PREFLIGHT_RECHECK_MS = 60_000;

export type L1SubmitterPreflightMonitorTimers = {
  readonly setTimeout: (run: () => void, ms: number) => unknown;
  readonly clearTimeout: (handle: unknown) => void;
};

export type L1SubmitterPreflightMonitorDeps = {
  /**
   * One preflight evaluation. `autoFund` false must leave the funding key out:
   * the monitor asks for funding only until one evaluation has completed or
   * one funding payment may have been sent, so a wallet drained later is
   * reported, never refilled from here, and a payment that did not settle is
   * never sent twice.
   */
  readonly evaluate: (args: {
    readonly autoFund: boolean;
  }) => Promise<L1SubmitterPreflightResult>;
  readonly write: (line: string) => void;
  readonly timers?: L1SubmitterPreflightMonitorTimers;
  readonly retryInitialMs?: number;
  readonly retryMaxMs?: number;
  readonly recheckMs?: number;
};

export type L1SubmitterPreflightMonitor = {
  /** Runs the first evaluation; later ones follow on their own timer. */
  readonly start: () => Promise<void>;
  readonly snapshot: () => CommitteeL1SubmitterPreflightSnapshot;
  /** Readiness reasons the snapshot status alone does not carry. */
  readonly reasons: () => readonly string[];
  readonly stop: () => void;
};

const defaultTimers: L1SubmitterPreflightMonitorTimers = {
  setTimeout: (run, ms) => {
    const handle = setTimeout(run, ms);
    handle.unref();
    return handle;
  },
  clearTimeout: (handle) => {
    clearTimeout(handle as ReturnType<typeof setTimeout>);
  },
};

/**
 * Keeps the L1 submitter wallet preflight current instead of pinning the
 * result of the one run at startup.
 *
 * Two outcomes are kept apart. An evaluation that throws (a provider that is
 * briefly unreachable) could not say anything about the wallet: the snapshot
 * stays `not_run` with the error, readiness names it
 * `l1_submitter_preflight_unavailable`, and the evaluation is tried again
 * with a doubling backoff. An evaluation that completes and finds the wallet
 * short is `failed`, and is re-read on a slower period so a wallet the
 * operator tops up becomes ready without a restart. Once the wallet is ready
 * or funded the monitor stops: the per-round funding check takes over.
 */
export const createL1SubmitterPreflightMonitor = (
  deps: L1SubmitterPreflightMonitorDeps,
): L1SubmitterPreflightMonitor => {
  const timers = deps.timers ?? defaultTimers;
  const retryInitialMs =
    deps.retryInitialMs ?? L1_SUBMITTER_PREFLIGHT_RETRY_INITIAL_MS;
  const retryMaxMs = deps.retryMaxMs ?? L1_SUBMITTER_PREFLIGHT_RETRY_MAX_MS;
  const recheckMs = deps.recheckMs ?? L1_SUBMITTER_PREFLIGHT_RECHECK_MS;
  let snapshot: CommitteeL1SubmitterPreflightSnapshot = { status: "not_run" };
  let unavailable: string | undefined;
  let completedOnce = false;
  let fundingSent = false;
  let retryMs = retryInitialMs;
  let timer: unknown;
  let stopped = false;

  const schedule = (ms: number): void => {
    if (stopped) return;
    timer = timers.setTimeout(() => {
      timer = undefined;
      void evaluateOnce();
    }, ms);
  };

  const evaluateOnce = async (): Promise<void> => {
    let result: L1SubmitterPreflightResult;
    try {
      result = await deps.evaluate({
        autoFund: !completedOnce && !fundingSent,
      });
    } catch (error) {
      // The payment may still land; the read-only retries see it if it does.
      if (error instanceof AutoFundPaymentUnsettledError) fundingSent = true;
      const message = error instanceof Error ? error.message : String(error);
      if (unavailable === undefined) {
        deps.write(
          `${JSON.stringify({ event: "l1_submitter_preflight_unavailable", error: message })}\n`,
        );
      }
      unavailable = message;
      snapshot = { status: "not_run", error: message };
      schedule(retryMs);
      retryMs = Math.min(retryMs * 2, retryMaxMs);
      return;
    }
    if (unavailable !== undefined) {
      deps.write(
        `${JSON.stringify({ event: "l1_submitter_preflight_evaluated", status: result.status })}\n`,
      );
    }
    unavailable = undefined;
    retryMs = retryInitialMs;
    completedOnce = true;
    snapshot = {
      status: result.status,
      detail: l1SubmitterPreflightResultToJson(result),
    };
    if (result.status === "failed") schedule(recheckMs);
  };

  return {
    start: evaluateOnce,
    snapshot: () => snapshot,
    reasons: () =>
      unavailable === undefined
        ? []
        : [`l1_submitter_preflight_unavailable: ${unavailable}`],
    stop: () => {
      stopped = true;
      if (timer !== undefined) timers.clearTimeout(timer);
      timer = undefined;
    },
  };
};
