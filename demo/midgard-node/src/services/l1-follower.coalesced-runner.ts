import {
  type DriverHold,
  EVENTS_INGESTION_FAILED,
  isRetriedHold,
  isTransientFailureHold,
  localFaultSubject,
} from "../l1-events/driver.js";
import { failureHold } from "./l1-follower.failure-hold.js";
import { NODE_TRANSIENT_BUDGET_MS } from "./transient-exhaustion.js";

/** Retry delays for a held driver: capped exponential. */
const RETRY_INITIAL_MS = 500;
const RETRY_MAX_MS = 30_000;

/**
 * How many runs may end on a local fault (a block this node's Ed25519
 * verifier could not check) with no progress past the faulted blocks before
 * the runner stops: on the backoff above, about a minute of retries.
 */
export const LOCAL_FAULT_RUN_LIMIT = 8;

/**
 * Where processing stands for the local-fault bound: processing reports each
 * block it commits (`committed`), and the runner watches the blocks its runs
 * faulted on. Committing a watched block is progress past the fault.
 */
export type LocalFaultProgress = Readonly<{
  /** Processing committed block `headerHash`. */
  committed: (headerHash: string) => void;
  /** Watches `subjects`, the blocks a run faulted on. */
  watch: (subjects: Iterable<string>) => void;
  /** Whether a watched block was committed since the last call that said
   * so; if one was, watches none from then on. */
  passed: () => boolean;
}>;

/** A fresh `LocalFaultProgress`: no block watched, none committed. */
export const localFaultProgress = (): LocalFaultProgress => {
  const watched = new Set<string>();
  let passed = false;
  return {
    committed: (headerHash) => {
      if (watched.has(headerHash)) passed = true;
    },
    watch: (subjects) => {
      for (const subject of subjects) watched.add(subject);
    },
    passed: () => {
      if (!passed) return false;
      passed = false;
      watched.clear();
      return true;
    },
  };
};

export type CoalescedRunnerBound = Readonly<{
  /** How long runs may end on a transient failure in a row (default
   * `NODE_TRANSIENT_BUDGET_MS`). */
  budgetMs?: number;
  /** Wall-clock ms (default `Date.now`). */
  now?: () => number;
  /** How many runs may end on a local fault with no progress past the
   * blocks they faulted on (default `LOCAL_FAULT_RUN_LIMIT`). */
  localFaultRuns?: number;
  /** Processing's progress past the faulted blocks. Without it, nothing
   * starts the local-fault count over in this process. */
  progress?: LocalFaultProgress;
  /** Called once, with the transient failures or the local faults, when the
   * budget or the local-fault bound ran out. */
  onExhausted: (holds: readonly DriverHold[]) => void;
}>;

/**
 * Runs `run` once per trigger, coalesced: at most one run at a time and one
 * queued. A run that ends with a hold a retry can clear (`isRetriedHold`: a
 * wait, or a transient failure) is retried on a capped backoff until a run
 * clears them or `signal` aborts. A run whose holds are all `notRetried` (a
 * failure that is not transient, a refusal only a chain change lifts) is not
 * retried on a timer: the next trigger (a follower change) runs it again.
 *
 * A wait on another actor (the store, the L1 node, a peer, this node's own
 * commit path) has no deadline. A transient failure
 * (`isTransientFailureHold`: the database did not answer) does: once runs
 * have ended on one for `budgetMs` in a row, with no run between them free
 * of one, the runner stops (no further run) and calls `onExhausted`, and the
 * node exits non-zero (`transient-exhaustion.ts`).
 *
 * A local fault (`localFault`: this node's verifier could not check a block)
 * is bounded by runs: once `localFaultRuns` runs ended on one with no
 * progress past the blocks they faulted on (`progress`: processing committed
 * one of them), the runner stops and calls `onExhausted` the same way. A run
 * free of local faults (a wait, a transient failure), or one faulting on
 * another block the follower reached without that progress, keeps the
 * count; a run ending on a local fault keeps the transient budget's start.
 * The count is this process's, so a restarted node meeting the same fault
 * on the same block retries within the same bound and exits again; it never
 * waits on the fault unbounded, and never takes it for a verdict.
 */
export const coalescedRunner = (
  run: () => Promise<readonly DriverHold[]>,
  signal: AbortSignal,
  bound: CoalescedRunnerBound,
): (() => void) => {
  const budgetMs = bound.budgetMs ?? NODE_TRANSIENT_BUDGET_MS;
  const localFaultRuns = bound.localFaultRuns ?? LOCAL_FAULT_RUN_LIMIT;
  const now = bound.now ?? Date.now;
  let running = false;
  let queued = false;
  let exhausted = false;
  let transientSince: number | undefined;
  let faultRuns = 0;
  let retryMs = RETRY_INITIAL_MS;
  let timer: ReturnType<typeof setTimeout> | undefined;
  /** Whether `holds` ran the budget out; records their transient failures.
   * A run that ends on a local fault keeps the budget's start. */
  const transientSpent = (holds: readonly DriverHold[]): boolean => {
    const failing = holds.filter(isTransientFailureHold);
    if (failing.length === 0) {
      if (!holds.some((hold) => localFaultSubject(hold) !== undefined))
        transientSince = undefined;
      return false;
    }
    const at = now();
    transientSince ??= at;
    if (at - transientSince < budgetMs) return false;
    exhausted = true;
    bound.onExhausted(failing);
    return true;
  };
  /**
   * Whether `holds` ran the local-fault bound out; counts a run that ends on
   * a local fault. Only progress past the blocks the counted runs faulted on
   * starts the count over; a run free of local faults keeps it.
   */
  const localFaultSpent = (holds: readonly DriverHold[]): boolean => {
    if (bound.progress?.passed() === true) faultRuns = 0;
    const faults = holds.filter(
      (hold) => localFaultSubject(hold) !== undefined,
    );
    if (faults.length === 0) return false;
    bound.progress?.watch(faults.map((hold) => localFaultSubject(hold)!));
    faultRuns += 1;
    if (faultRuns < localFaultRuns) return false;
    exhausted = true;
    bound.onExhausted(faults);
    return true;
  };
  const spent = (holds: readonly DriverHold[]): boolean =>
    transientSpent(holds) || localFaultSpent(holds);
  const trigger = (): void => {
    if (signal.aborted || exhausted) return;
    if (timer !== undefined) {
      clearTimeout(timer);
      timer = undefined;
    }
    if (running) {
      queued = true;
      return;
    }
    running = true;
    void run()
      .catch((error: unknown) => [
        failureHold(EVENTS_INGESTION_FAILED, "driver run", error),
      ])
      .then((holds) => {
        running = false;
        if (spent(holds)) return;
        if (queued) {
          queued = false;
          trigger();
          return;
        }
        if (!holds.some(isRetriedHold) || signal.aborted) {
          retryMs = RETRY_INITIAL_MS;
          return;
        }
        timer = setTimeout(trigger, retryMs);
        timer.unref?.();
        retryMs = Math.min(retryMs * 2, RETRY_MAX_MS);
      });
  };
  signal.addEventListener("abort", () => {
    if (timer !== undefined) clearTimeout(timer);
  });
  return trigger;
};
