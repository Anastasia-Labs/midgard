import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";

/**
 * A proof objective the watcher holds by name: it takes no new work, the
 * watcher reports it on readiness and keeps running, and the hold clears
 * once its header leaves the finalized queue (its proof or another landed,
 * or it merged).
 */
export type WatcherDecisionHold = Readonly<{
  kind: "objective";
  category: WatcherInstalledWorkflowCategory;
  headerHash: string;
  /** The decision the objective's job names, when it names one. */
  decisionDigest: string | null;
  detail: string;
  /** The reason readiness names. */
  readiness: WatcherObjectiveHoldReadiness;
}>;

/**
 * - `fault_proof_start_deadline_passed`: the objective's header matured
 *   with no removal of ours landed, so no removal can land. (Past its latest
 *   safe start but before maturity it still runs, late.) Its detail is
 *   `<category>/<headerHash>`.
 */
export type WatcherObjectiveHoldReadiness = "fault_proof_start_deadline_passed";

/** The readiness reason a hold names. */
export const watcherDecisionHoldReason = (
  hold: WatcherDecisionHold,
): WatcherObjectiveHoldReadiness => hold.readiness;

/** Work held by name: never a process failure. */
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
