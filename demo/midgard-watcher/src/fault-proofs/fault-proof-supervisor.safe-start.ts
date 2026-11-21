import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import type { WatcherFaultProofJob } from "./fault-proof-supervisor.validate-job.js";
import {
  type WatcherDecisionHold,
  WatcherProofDecisionMissingError,
} from "./watcher-decision-hold.js";

/**
 * How a run of `job` starts against its header's deadline, which the header
 * fixes:
 *
 * - `on_time`: before its latest safe start, or deadline-free work.
 * - `late`: past its latest safe start but before the header's maturity.
 *   The latest safe start is a design margin (worst-case proof time before
 *   maturity), not the last moment a removal lands: the state queue accepts
 *   a completed fraud-proof removal until maturity. So the run keeps every
 *   right an on-time run has: a fresh submission for each later step, and a
 *   new attempt for a step whose transaction expired. It is marked late for
 *   the execution's alert.
 * - `reconcile`: at or past maturity while a transaction this process sent
 *   for the objective can still land (`sending`); the run only continues
 *   what is in flight.
 * - `observe`: at or past maturity with nothing in flight, as after a
 *   restart. No removal of ours can land any more, but one may have landed
 *   already: the chain is asked first. A landed removal completes the
 *   objective; otherwise it is held by name (`watcherProofStartDeadlinePassed`).
 */
export const watcherProofStartTiming = (
  job: WatcherFaultProofJob,
  sending: boolean,
  nowMs: bigint,
): "on_time" | "late" | "reconcile" | "observe" => {
  if (job.deadline === null || nowMs < BigInt(job.deadline.latestSafeStartAtMs))
    return "on_time";
  if (nowMs < BigInt(job.deadline.maturityAtMs)) return "late";
  return sending ? "reconcile" : "observe";
};

/** The hold of an objective at or past its header's maturity whose removal
 * has not landed: `fault_proof_start_deadline_passed` until its header
 * leaves the queue. */
export const watcherProofStartDeadlinePassed = (
  job: WatcherFaultProofJob,
): WatcherProofDecisionMissingError =>
  new WatcherProofDecisionMissingError({
    kind: "objective",
    category: job.category,
    headerHash: job.headerHash,
    decisionDigest: job.decisionDigest,
    detail: `${job.category}/${job.headerHash}`,
    readiness: "fault_proof_start_deadline_passed",
  });

/** Run outcomes after which a header's deadline no longer binds its
 * objective: its correction is included, or another record resolved it. */
const DEADLINE_SETTLED = new Set<unknown>([
  "completed",
  "terminal_included",
  "resolved_by_another_record",
]);

/**
 * The latest job of every objective the supervisor still drives against a
 * header deadline: queued, running, waiting between observations (as one
 * paused on prover funding does) or held by name. The supervisor's status
 * reads the earliest of these with its queued and active jobs, so an
 * objective that is neither queued nor running still counts toward
 * `deadlineHealth` and `remainingSafeStartMs`.
 */
export const createWatcherOpenProofDeadlines = () => {
  const open = new Map<string, WatcherFaultProofJob>();
  const keyOf = ({ category, headerHash }: WatcherProofObjective) =>
    `${category}/${headerHash}`;
  return Object.freeze({
    /** Records the job when it carries a header deadline. */
    track: (job: WatcherFaultProofJob): void => {
      if (job.deadline !== null) open.set(keyOf(job), job);
    },
    /** A run whose outcome ends the race against maturity drops it. */
    finished: (job: WatcherProofObjective, outcomeKind: unknown): void => {
      if (DEADLINE_SETTLED.has(outcomeKind)) open.delete(keyOf(job));
    },
    /** The supervisor stopped driving it; one held by name (in `holds`)
     * keeps counting until its header leaves the queue. */
    settled: (
      objective: WatcherProofObjective,
      holds: readonly WatcherDecisionHold[],
    ): void => {
      if (
        !holds.some(
          (hold) =>
            hold.kind === "objective" && keyOf(hold) === keyOf(objective),
        )
      )
        open.delete(keyOf(objective));
    },
    jobs: (): readonly WatcherFaultProofJob[] => [...open.values()],
  });
};
