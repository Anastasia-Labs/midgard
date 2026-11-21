import { type CanonicalBlockClassification } from "./classification.js";
import {
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowTerminal,
} from "./journal.js";
import type { FraudProofResolvedByAnotherRecord } from "./resolved-by-another-record.js";

/**
 * The `pending` reason of a run whose next action needs a new submission while
 * it holds reconciliation-only authority (its latest safe start has passed).
 * The run follows the transactions it already sent and takes no further step
 * by itself; a person resubmits.
 */
export const FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED =
  "Canonical workflow requires fresh submission authority";

/**
 * The prefix of the `pending` reason of a run whose session saw no progress
 * within the continuation bound. The run hands its turn back and
 * resumes on the next observation; it never refuses for this.
 */
export const FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED =
  "Canonical workflow made no progress within its continuation bound";

/** A pending run's own readiness state, named by its family. */
export type FraudProofWorkflowNamedState = Readonly<{
  name: string;
  alarm: boolean;
}>;

/** An action of the run that did not build, and why: the operator's alarm. */
export type FraudProofWorkflowAlarm = Readonly<{
  actionId: string;
  reason: string;
}>;

export type FraudProofWorkflowRunResult =
  | {
      readonly kind: "no_fault_detected" | "unprovable_gap";
      readonly classification: CanonicalBlockClassification;
    }
  | {
      readonly kind: "terminal_included";
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly terminal: FraudProofWorkflowTerminal;
    }
  | {
      readonly kind: "completed";
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly terminal: FraudProofWorkflowTerminal;
    }
  | {
      /**
       * The header was removed against a fraud-proof record the chain does
       * not show as ours, and no transaction this process sent for the
       * objective can still land. The objective is done, but it is not our
       * conviction: there is no terminal and no reward is claimed.
       */
      readonly kind: "resolved_by_another_record";
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly resolution: FraudProofResolvedByAnotherRecord;
    }
  | {
      readonly kind: "pending" | "stalled";
      /** Keep the objective active and resume on fresh chain observation. */
      readonly resumeOnObservation?: true;
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly reason: string;
      /**
       * The last action the run submitted before it waits: a session counts
       * a new one as progress (`pending-continuation.ts`).
       */
      readonly awaitingActionId?: string;
      /**
       * Present when a stalled run failed while the adapter built the next
       * transaction. That transaction targets the live L1 tip while the stage
       * it acts on is release-final, so the stall may only mean the
       * authenticated observation still trails the tip; the caller may resume
       * once it has caught up.
       */
      readonly phase?: "preflight";
      /**
       * A state its family names for readiness, which is neither a wait on
       * the counterparty nor a stall: the run hands its turn back at once
       * and resumes on observation, and the objective is held under `name`
       * until a later run reports something else. An `alarm` state is one
       * the operator must act on.
       */
      readonly namedState?: FraudProofWorkflowNamedState;
      /**
       * The actions this run skipped because they did not build, each an
       * alarm, in the order met; absent when none was. The run went on to
       * the moves ranked after them, so the alarms stand even when one of
       * those landed.
       */
      readonly alarms?: readonly FraudProofWorkflowAlarm[];
    };
