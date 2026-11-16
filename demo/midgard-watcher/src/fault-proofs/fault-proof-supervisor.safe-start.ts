import type {
  WatcherProofExecution,
  WatcherProofObjective,
} from "./fault-proof-objective-journal.js";
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
 *   right an on-time run has, signed or not: a fresh submission for each
 *   later step, and a new attempt for a signed step whose validity lapsed.
 *   It is marked late for the execution's alert.
 * - `reconcile`: at or past maturity with a signed attempt; that attempt is
 *   only reconciled.
 *
 * At or past maturity no removal can land, so an objective with nothing
 * signed is held by name (`fault_proof_start_deadline_passed`) until its
 * header leaves the queue; that throws.
 */
export const watcherProofStartTiming = (
  job: WatcherFaultProofJob,
  execution: WatcherProofExecution | undefined,
  nowMs: bigint,
): "on_time" | "reconcile" | "late" => {
  if (job.deadline === null || nowMs < BigInt(job.deadline.latestSafeStartAtMs))
    return "on_time";
  if (nowMs < BigInt(job.deadline.maturityAtMs)) return "late";
  if (
    execution?.entries.some(
      ({ event }) => event.kind === "submission_intent",
    ) === true
  )
    return "reconcile";
  throw new WatcherProofDecisionMissingError({
    kind: "objective",
    category: job.category,
    headerHash: job.headerHash,
    decisionDigest: job.decisionDigest,
    detail: `${job.category}/${job.headerHash}`,
    readiness: "fault_proof_start_deadline_passed",
  });
};

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
    /** Its header merged or left the queue: the race against maturity is
     * over. True the first time, when it was still open. */
    departed: (objective: WatcherProofObjective): boolean =>
      open.delete(keyOf(objective)),
    jobs: (): readonly WatcherFaultProofJob[] => [...open.values()],
  });
};
