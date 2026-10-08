import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";

/**
 * A proof objective or funding reservation whose recorded fault decision is
 * missing from the decision journal, or differs from the one its work names.
 * It happens when the journal database was moved aside, or when a fault was
 * detected again under a new decision digest. The watcher holds such work,
 * reports `journal_decision_missing` on readiness, and keeps running: a held
 * objective clears once its header leaves the finalized queue (its proof or
 * another landed, or it merged); a held reservation clears once the
 * follower shows its funding inputs final.
 */
export type WatcherDecisionHold =
  | Readonly<{
      kind: "objective";
      category: WatcherInstalledWorkflowCategory;
      headerHash: string;
      /** The decision the objective's workflow names, when it names one. */
      decisionDigest: string | null;
      detail: string;
      /**
       * Set when the hold is not a missing decision: the reason readiness
       * names instead of `journal_decision_missing`. The clearing rule is the
       * same (the header leaves the finalized queue).
       */
      readiness?: WatcherObjectiveHoldReadiness;
    }>
  | Readonly<{
      kind: "reservation";
      reservationId: string;
      decisionDigest: string;
      detail: string;
    }>;

/**
 * `validation_transcript_pre_follower`: a validation-trace dispute was
 * started from a transcript recorded before user events were read from the
 * follower's facts. The challenge rebuilt from the facts has another digest
 * than the one its open workflow journaled, so the dispute is held rather
 * than re-keyed.
 */
export type WatcherObjectiveHoldReadiness =
  "validation_transcript_pre_follower";

/** The readiness reason a hold names. */
export const watcherDecisionHoldReason = (
  hold: WatcherDecisionHold,
): WatcherObjectiveHoldReadiness | "journal_decision_missing" =>
  (hold.kind === "objective" ? hold.readiness : undefined) ??
  "journal_decision_missing";

/** Work whose recorded decision is missing: held, never a process failure. */
export class WatcherProofDecisionMissingError extends Error {
  constructor(readonly hold: WatcherDecisionHold) {
    super(hold.detail);
    this.name = "WatcherProofDecisionMissingError";
  }
}

export const isWatcherProofDecisionMissingError = (
  error: unknown,
): error is WatcherProofDecisionMissingError =>
  error instanceof WatcherProofDecisionMissingError;
