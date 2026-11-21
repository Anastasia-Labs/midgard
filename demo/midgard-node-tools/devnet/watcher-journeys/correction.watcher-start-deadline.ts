/** A job as the watcher's `/v1/status` names it. */
type JourneyWatcherJob = { category: string; headerHash: string };

/**
 * The watcher's `/v1/status` fields that say whether it can still start a
 * proof. A watcher still starting answers without `supervisor`.
 */
export type JourneyWatcherStartStatus = {
  supervisor?: {
    activeJob: JourneyWatcherJob | null;
    earliestDeadlineJob: JourneyWatcherJob | null;
    /** The earliest-deadline job's time left to its latest safe start. */
    remainingSafeStartMs: string | null;
    /** The watcher's holds; an objective's names its target and reason. */
    journalDecisionMissing: readonly {
      kind: string;
      category?: string;
      headerHash?: string;
      decisionDigest?: string | null;
      detail: string;
      readiness?: string;
    }[];
  } & Record<string, unknown>;
  /** The watcher's L1 holds; an objective's detail starts with its target. */
  l1Readiness?: readonly { reason: string; detail: string }[];
} & Record<string, unknown>;

/**
 * Why the watcher will never start the proof for this header, or undefined.
 * Read only while the workflow has signed no step: past its latest safe start
 * the watcher holds an objective with no signed attempt by name
 * (`fault_proof_start_deadline_passed`) and never runs it, so the proof wait
 * would otherwise end later in a generic no-progress stall.
 *
 * A job already running when its safe start passes keeps running, so only a
 * queued job is judged by `remainingSafeStartMs`; its next admission holds it.
 */
export const journeyStartDeadlinePassed = (
  { supervisor }: JourneyWatcherStartStatus,
  target: JourneyWatcherJob,
): string | undefined => {
  // A restarted watcher's startup status says nothing of its proofs yet.
  if (supervisor === undefined) return undefined;
  const named = `${target.category}/${target.headerHash}`;
  const isTarget = (job: JourneyWatcherJob | null) =>
    job !== null &&
    job.category === target.category &&
    job.headerHash === target.headerHash;
  if (
    supervisor.journalDecisionMissing.some(
      (hold) =>
        hold.kind === "objective" &&
        hold.readiness === "fault_proof_start_deadline_passed" &&
        isTarget({
          category: hold.category ?? "",
          headerHash: hold.headerHash ?? "",
        }),
    )
  )
    return `The watcher holds fault_proof_start_deadline_passed for ${named}: its latest safe start passed before it signed any step, so it will not prove it`;
  if (
    supervisor.remainingSafeStartMs !== null &&
    BigInt(supervisor.remainingSafeStartMs) <= 0n &&
    isTarget(supervisor.earliestDeadlineJob) &&
    !isTarget(supervisor.activeJob)
  )
    return `The watcher's latest safe start for ${named} passed with its job queued and no step signed (remainingSafeStartMs ${supervisor.remainingSafeStartMs}), so it will not prove it`;
  return undefined;
};

/**
 * Holds by which the watcher has itself given up on the objective: its
 * continuation reached no new awaited action for its quiet bound, or its
 * counterparty's deadline passed unanswered.
 */
const JOURNEY_FATAL_HOLDS: ReadonlySet<string> = new Set([
  "fault_proof_continuation_stalled",
  "fault_proof_counterparty_deadline_passed",
]);

/**
 * Holds that pause the objective until a later run clears them: a refused L1
 * read, unavailable prover funding, a held trace replay, or a quarantined
 * run awaiting its backoff. One that outlasts the watcher's quiet bound with
 * nothing of ours landing ends the correction.
 */
const isJourneyPausingHold = (reason: string) =>
  reason === "prover_funding_unavailable" ||
  reason === "validation_trace_replay_held" ||
  reason === "fault_proof_objective_quarantined" ||
  reason.startsWith("fault_proof_l1_refused:");

/** The watcher's holds on this target, sorted into fatal and pausing. */
export const journeyTargetHolds = (
  { l1Readiness }: JourneyWatcherStartStatus,
  target: JourneyWatcherJob,
): Readonly<{
  fatal: readonly string[];
  pausing: readonly string[];
}> => {
  const named = `${target.category}/${target.headerHash}:`;
  const held = (l1Readiness ?? []).filter(({ detail }) =>
    detail.startsWith(named),
  );
  return {
    fatal: held
      .filter(({ reason }) => JOURNEY_FATAL_HOLDS.has(reason))
      .map(({ reason, detail }) => `${reason} (${detail})`),
    pausing: held
      .filter(({ reason }) => isJourneyPausingHold(reason))
      .map(({ reason }) => reason),
  };
};
