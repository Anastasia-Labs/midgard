import {
  revokeWorkflowActuationPermit,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { HeaderFaultDecision } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";

import type { WatcherProofRetention } from "../l1-follower/proof-retention.js";
import type { WatcherFaultDecisionJournal } from "./fault-decision-journal.js";
import type { WatcherProofObjectiveCleanup } from "./fault-proof-objective-cleanup.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import type {
  WatcherDecisionSupersedeHoldReason,
  WatcherFaultProofProgressAuthority,
} from "./fault-proof-progress-authority.types.js";

/**
 * The progress authority's supersede. A durable fault decision a fresh
 * classification of the same observation no longer reproduces is replaced
 * once no job of it is queued or active. Its rows and its decision row are
 * forgotten; then the fresh fault is
 * recorded as a live decision. What an earlier transaction left on L1 (a
 * partial proof) is abandoned. Removing the row is the supersession, so a
 * decision that returns later (A to B to A) is recorded afresh. The work is
 * one objective's rows, whoever moved the replay context.
 */
export const createProgressSupersede = (input: {
  readonly retention?: WatcherProofRetention;
  readonly onObjectiveSettled?: (objective: WatcherProofObjective) => void;
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
    if (fresh.decision === "fault_detected")
      await (await input.openDecisions()).appendLiveDecision(fresh);
    input.pruneDecisions();
    return Object.freeze({ kind: "superseded" as const });
  };
};
