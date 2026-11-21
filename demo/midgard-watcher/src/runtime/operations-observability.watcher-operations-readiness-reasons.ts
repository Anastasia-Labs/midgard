/**
 * Every reason the watcher itself names in `readinessReasons`. The L1
 * follower and the fault-proof execution holds add their own reasons through
 * `l1Readiness`.
 */
export const WATCHER_OPERATIONS_READINESS_REASONS = Object.freeze([
  "supervisor_not_accepting",
  "recovery_incomplete",
  "launch_scope_incomplete",
  "deadline_at_risk",
  "deadline_unsafe",
  "l1_source_unavailable",
  "l1_source_stale",
  "retained_da_transport_failed",
  "active_alert",
  "journal_capacity",
  /** A fault-proof journal failed integrity; `supervisor.journalIntegrity`
   * names the failure. Held until an operator repairs the journals. */
  "journal_integrity",
  /** The journals could not be opened; `supervisor.journalUnavailable` names
   * the failure. Held while the open is retried; clears once it succeeds. */
  "journal_unavailable",
  /** A fault-proof objective's header matured with no removal of ours landed
   * (past its latest safe start it still runs, late); it is listed in
   * `supervisor.journalDecisionMissing` with this `readiness` and the detail
   * `<category>/<headerHash>`. Held, never run again, the process up; clears
   * once its header leaves the finalized queue. */
  "fault_proof_start_deadline_passed",
  /** A released or final objective's rows in watcher-journals.sqlite could
   * not be deleted
   * (its rows stay and keep their slot under the objective cap);
   * `supervisor.objectiveCleanupFailures` lists each with its failure. Every
   * admission retries; clears once the removal succeeds. */
  "fault_proof_objective_cleanup_failed",
  /** A journal database was busy or locked past its timeout;
   * `supervisor.journalBusy` names the failure. The work is requeued in
   * process from durable state after a 1-30 s backoff; clears then. */
  "journal_busy",
] as const);

export type WatcherOperationsReadinessReason =
  (typeof WATCHER_OPERATIONS_READINESS_REASONS)[number];
