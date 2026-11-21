import type { WorkflowActuationPermit } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type {
  HeaderDecision,
  HeaderFaultDecision,
} from "@al-ft/midgard-fault-proofs/workflow/header-classifier";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherProofCleanupFailure } from "./fault-proof-objective-cleanup.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import type {
  WatcherProofCompletionMarker,
  WatcherProofVerifiedCompletion,
} from "./fault-proof-objective-table.js";
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
 * - `cleanup_failed`: its rows could not be deleted; retried next pass.
 */
export type WatcherDecisionSupersedeHoldReason =
  | "correction_locked"
  | "objective_held"
  | "job_pending"
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
   * marked, and pruned once its job is done. */
  markCompleted(
    objective: WatcherProofObjective,
    verified?: WatcherProofVerifiedCompletion,
  ): Promise<void>;
  /** Ends an objective whose header was removed against a record its run
   * did not create. No completion marker is written: it is not our
   * conviction. The objective is released as a departed one is, and
   * forgotten once its job is done. */
  markResolved(objective: WatcherProofObjective): Promise<void>;
  /** The objective's marker: verified beyond rollback recovery, so final and
   * never verified or held again. */
  completionMarker(
    objective: WatcherProofObjective,
  ): WatcherProofCompletionMarker | null;
  /**
   * Replaces the prior decision when nothing still drives it: releases its
   * unused prover funding, forgets its rows and its decision row, then
   * records a fresh fault as a live decision. A hold keeps it unchanged; a
   * later call supersedes it once that clears.
   */
  supersede(
    request: WatcherDecisionSupersedeRequest,
  ): Promise<WatcherDecisionSupersession>;
}>;

/** The proof step of an objective whose header left the queue, from the
 * chain alone: `confirmed` once our removal is seen landed, `completed`
 * once it is final, `cancelled` when one seen landed is gone and no removal
 * of ours stands. */
export type WatcherProofDepartureStep = "confirmed" | "completed" | "cancelled";

/** That step, with the objective and rollback generation it is of. */
export type WatcherFaultProofDepartureStep = Readonly<{
  category: string;
  headerHash: string;
  decisionDigest: string;
  rollbackGeneration: string;
  step: WatcherProofDepartureStep;
}>;

/** The process's proof-materials roster: the objectives each admission
 * returns to drive, and each objective that is no longer driven. */
export type WatcherProofMaterialsRoster = Readonly<{
  admitted(objectives: readonly WatcherProofObjective[]): void;
  released(objective: WatcherProofObjective): void;
}>;

/** What the progress authority tells its owner about its objectives. */
export type WatcherFaultProofProgressListeners = Readonly<{
  /** Told when an objective stops being driven (`objectiveSettled`). */
  onObjectiveSettled?: (objective: WatcherProofObjective) => void;
  /** Whether a job of the objective is queued, running or handed over in
   * this process; its rows stay while it is. */
  jobInProcess?: (objective: WatcherProofObjective) => boolean;
  /** Told when the objective's rows are deleted. */
  onObjectiveRemoved?: (objective: WatcherProofObjective) => void;
  /** Told as an admission reaches its final return: its observation and the
   * contexts it returns. */
  onAdmitted?: (
    observation: WatcherAuthenticatedStateQueueObservation,
    contexts: readonly WatcherFaultProofProgressContext[],
  ) => void;
  /** Told each objective admitted, and each the listeners above report no
   * longer driven: settled, removed or forgotten. */
  proofMaterials?: WatcherProofMaterialsRoster;
}>;
