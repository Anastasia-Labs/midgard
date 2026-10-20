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
  /** Proof work or a funding reservation names a decision the journal does
   * not hold; `supervisor.journalDecisionMissing` lists each. Held, never
   * run again; each clears once L1 facts resolve it. */
  "journal_decision_missing",
  /** A fault-proof objective's latest safe start, fixed by its header, passed
   * before it signed any attempt; it is listed in
   * `supervisor.journalDecisionMissing` with this `readiness` and the detail
   * `<category>/<headerHash>`. Held, never run again, the process up; clears
   * once its header leaves the finalized queue. */
  "fault_proof_start_deadline_passed",
  /** Startup could not read a fault-proof objective's workflow directory (a
   * symlinked path, a sequence gap a partial delete left, a foreign
   * execution, an I/O error); it is listed in
   * `supervisor.journalDecisionMissing` with this `readiness` and the detail
   * `<category>/<headerHash>: <failure>`. Held, never run again, the process
   * up; clears once its header leaves the finalized queue, which forgets its
   * rows. */
  "fault_proof_objective_unreadable",
  /** A funding reservation no workflow will drive again holds signed
   * attempts that are not retired past release finality; it is listed in
   * `supervisor.journalDecisionMissing` with this `readiness`, and its
   * detail names the reservation. Its `proof_submission_failure` alert is
   * active meanwhile. Clears once its attempts' own signed bytes show each
   * one retired past release finality and the reservation is released, or
   * once a fresh execution of its target takes them over. An objective
   * waiting on such a reservation reports the same reason. */
  "prover_funding_orphaned_reservation",
  /** A released or final objective's workflow directory could not be
   * removed (its rows stay and keep their slot under the objective cap), or
   * a removed directory's tombstone could not be deleted;
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
