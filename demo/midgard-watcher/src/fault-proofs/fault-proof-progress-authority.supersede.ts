import {
  type FraudProofWorkflowJournalEntry,
  type HeaderFaultDecision,
  revokeWorkflowActuationPermit,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherProofRetention } from "../l1-follower/proof-retention.js";
import type { WatcherFaultDecisionJournal } from "./fault-decision-journal.js";
import type { WatcherProofObjectiveCleanup } from "./fault-proof-objective-cleanup.js";
import {
  isWatcherProofJournalRefusal,
  readWatcherProofExecution,
  type WatcherProofObjective,
} from "./fault-proof-objective-journal.js";
import type {
  WatcherDecisionSupersedeHoldReason,
  WatcherFaultProofProgressAuthority,
} from "./fault-proof-progress-authority.types.js";
import type { WatcherWorkflowJournal } from "./watcher-workflow-journal.js";

/**
 * Whether a signed attempt of the execution is still in flight: as the
 * workflow journal fold reads it, an action whose latest intent is neither
 * confirmed nor reconciled not found.
 */
const unresolvedAttempt = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): boolean => {
  const open = new Set<string>();
  for (const { event } of entries) {
    if (event.kind === "submission_intent") open.add(event.actionId);
    else if (event.kind === "reobserved") {
      open.clear();
      open.add(event.actionId);
    } else if (
      event.kind === "confirmed" ||
      (event.kind === "reconciled" && event.outcome === "not_found")
    )
      open.delete(event.actionId);
  }
  return open.size !== 0;
};

/**
 * The progress authority's supersede. A durable fault decision a fresh
 * classification of the same observation no longer reproduces is replaced
 * once nothing of it is in flight: no job of it is queued or active and
 * every signed attempt of it is confirmed or failed. Its unused prover
 * funding is released first, because the release reads its decision row;
 * then its workflow rows and its decision row are forgotten; then the fresh
 * fault is recorded as a live decision. A signed reservation of it is left
 * to the orphaned-funding settle, which returns its inputs once each of its
 * attempts is retired on L1; what an attempt left on L1 (a partial proof)
 * is abandoned. Removing the row is the supersession, so a decision that
 * returns later (A to B to A) is recorded afresh. The work is one
 * objective's rows and reservations, whoever moved the replay context.
 */
export const createProgressSupersede = (input: {
  readonly workflowJournal: WatcherWorkflowJournal;
  readonly deploymentFingerprint: string;
  readonly retention?: WatcherProofRetention;
  readonly onObjectiveSettled?: (objective: WatcherProofObjective) => void;
  /** Told when the superseded objective held a signed attempt, whose
   * funding the orphaned-funding settle now owns. */
  readonly onObjectiveUnattended?: (objective: WatcherProofObjective) => void;
  readonly releaseDecisionFunding?: (
    decision: HeaderFaultDecision,
  ) => Promise<boolean>;
  readonly initialize: () => Promise<void>;
  readonly cleanup: WatcherProofObjectiveCleanup;
  readonly objectives: ReadonlyMap<
    string,
    Readonly<{
      currentPermit?: WorkflowActuationPermit;
      historical?: Readonly<{ permit: WorkflowActuationPermit }>;
    }>
  >;
  /** Whether the objective is held (decision missing or rows unreadable). */
  readonly held: (key: string) => boolean;
  /** Drops the objective's in-memory entry. */
  readonly forgetObjective: (key: string) => void;
  readonly decisions: Map<string, HeaderFaultDecision>;
  readonly openDecisions: () => Promise<WatcherFaultDecisionJournal>;
  readonly pruneDecisions: () => void;
}): WatcherFaultProofProgressAuthority["supersede"] => {
  return async ({ prior, fresh }) => {
    await input.initialize();
    const key = `${prior.category}:${prior.headerHash}`;
    const holding = (
      reason: WatcherDecisionSupersedeHoldReason,
      detail = `${prior.category}/${prior.headerHash}`,
    ) => Object.freeze({ kind: "held" as const, reason, detail });
    if (input.held(key)) return holding("objective_held");
    if (input.cleanup.pending(prior)) return holding("job_pending");
    let execution;
    try {
      execution = readWatcherProofExecution({
        journal: input.workflowJournal,
        deploymentFingerprint: input.deploymentFingerprint,
        objective: prior,
      });
    } catch (error) {
      if (!isWatcherProofJournalRefusal(error)) throw error;
      return holding("objective_held");
    }
    if (execution !== undefined && unresolvedAttempt(execution.entries))
      return holding("submission_unresolved");
    if ((await input.releaseDecisionFunding?.(prior)) === false)
      return holding("funding_in_use");
    const signed =
      execution?.entries.some(
        ({ event }) => event.kind === "submission_intent",
      ) === true;
    const objective = input.objectives.get(key);
    input.forgetObjective(key);
    input.decisions.delete(prior.decisionDigest);
    if (objective !== undefined) {
      for (const permit of [
        objective.currentPermit,
        objective.historical?.permit,
      ])
        if (permit !== undefined)
          revokeWorkflowActuationPermit(permit, "fault decision superseded");
      input.onObjectiveSettled?.(prior);
      await input.retention?.release(prior);
    }
    await input.cleanup.forget(prior, {
      final: false,
      departed: true,
      decision: prior.decisionDigest,
    });
    const failure = input.cleanup
      .failures()
      .find(
        ({ category, headerHash }) =>
          category === prior.category && headerHash === prior.headerHash,
      );
    if (failure !== undefined) return holding("cleanup_failed", failure.detail);
    if (signed) input.onObjectiveUnattended?.(prior);
    if (fresh.decision === "fault_detected")
      await (await input.openDecisions()).appendLiveDecision(fresh);
    input.pruneDecisions();
    return Object.freeze({ kind: "superseded" as const });
  };
};
