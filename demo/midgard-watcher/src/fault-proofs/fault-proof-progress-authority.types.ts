import type {
  HeaderDecision,
  HeaderFaultDecision,
  WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherProofCleanupFailure } from "./fault-proof-objective-cleanup.js";
import type {
  WatcherProofExecution,
  WatcherProofObjective,
} from "./fault-proof-objective-journal.js";
import type { WatcherProofCompletionMarker } from "./fault-proof-objective-table.js";
import type { WatcherFaultProofDeadline } from "./fault-proof-supervisor.js";
import type { WatcherDecisionHold } from "./watcher-decision-hold.js";

export type WatcherFaultProofProgressRequest = Readonly<{
  observation: WatcherAuthenticatedStateQueueObservation;
  rollbackGeneration: string;
  fault?: Readonly<{
    decision: HeaderFaultDecision;
    actuationPermit: WorkflowActuationPermit;
    deadline: WatcherFaultProofDeadline;
  }>;
}>;

export type WatcherFaultProofProgressContext = Readonly<{
  observationRevision: string;
  decision: HeaderFaultDecision;
  actuationPermit: WorkflowActuationPermit;
  deadline: WatcherFaultProofDeadline | null;
  rollbackGeneration: string;
}>;

/**
 * A durable fault decision a fresh full-scope classification of the same
 * authenticated observation no longer reproduces, and that fresh decision.
 */
export type WatcherDecisionSupersedeRequest = Readonly<{
  observation: WatcherAuthenticatedStateQueueObservation;
  prior: HeaderFaultDecision;
  fresh: HeaderDecision;
}>;

/**
 * What still drives the prior decision, so it is held unchanged:
 * - `correction_locked`: the finalized CorrectionLock is Locked under its
 *   category and header (the bridge checks this before asking);
 * - `objective_held`: its objective is held (decision missing, unreadable);
 * - `job_pending`: a job of its objective is queued or active;
 * - `submission_unresolved`: a signed attempt of its execution is neither
 *   confirmed nor failed yet;
 * - `funding_in_use`: a funding reservation of it is bound to a run;
 * - `cleanup_failed`: its rows could not be deleted; retried next pass.
 */
export type WatcherDecisionSupersedeHoldReason =
  | "correction_locked"
  | "objective_held"
  | "job_pending"
  | "submission_unresolved"
  | "funding_in_use"
  | "cleanup_failed";

export type WatcherDecisionSupersession =
  | Readonly<{ kind: "superseded" }>
  | Readonly<{
      kind: "held";
      reason: WatcherDecisionSupersedeHoldReason;
      detail: string;
    }>;

export type WatcherFaultProofProgressAuthority = Readonly<{
  admit(
    request: WatcherFaultProofProgressRequest,
  ): Promise<readonly WatcherFaultProofProgressContext[]>;
  revokeAuthority(reason: string): void;
  /** Open objectives, held ones included. */
  unfinishedCount(): number;
  /** Objectives held because their recorded decision is missing. */
  decisionHolds(): readonly WatcherDecisionHold[];
  /** Released or final objectives whose workflow rows a removal could not
   * delete (their rows stay): each admission retries them. */
  cleanupFailures(): readonly WatcherProofCleanupFailure[];
  /** Holds an objective whose execution names a missing decision. */
  holdObjective(hold: WatcherDecisionHold): Promise<void>;
  /** Records the completion; one verified beyond rollback recovery is
   * marked, and pruned with its workflow rows once its job is done. */
  markCompleted(
    objective: WatcherProofObjective,
    verified?: Readonly<{
      execution: WatcherProofExecution;
      confirmationDepth: number;
    }>,
  ): Promise<void>;
  /** Ends an objective whose header was removed against a record its run
   * did not create. No completion marker is written: it is not our
   * conviction. The objective is released as a departed one is, and
   * forgotten with its workflow rows once its job is done. */
  markResolved(objective: WatcherProofObjective): Promise<void>;
  /** The objective's marker when it matches this exact execution: verified
   * beyond rollback recovery, so final and never verified or held again. */
  completionMarker(
    objective: WatcherProofObjective,
    execution: WatcherProofExecution,
  ): WatcherProofCompletionMarker | null;
  updateExecution(input: {
    readonly objective: WatcherProofObjective;
    readonly execution: WatcherProofExecution;
  }): Promise<void>;
  /**
   * Replaces the prior decision when nothing of it is in flight: releases
   * its unused prover funding, forgets its workflow rows and its decision
   * row, then records a fresh fault as a live decision. Anything in flight
   * holds it unchanged; a later call supersedes it once that clears.
   */
  supersede(
    request: WatcherDecisionSupersedeRequest,
  ): Promise<WatcherDecisionSupersession>;
  reconcileExecution(input: {
    readonly objective: WatcherProofObjective;
    readonly execution: WatcherProofExecution;
    readonly rollbackGeneration: string;
  }): Promise<WorkflowActuationPermit>;
}>;
