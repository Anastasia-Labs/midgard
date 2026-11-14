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
