import type { WatcherDaBondPoolObservation } from "../availability/pool-observation.js";
import type { WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";
import type { WatcherRetainedDaTransportStatus } from "../storage/retained-da-runtime.js";

export const WATCHER_OPERATIONS_OBSERVABILITY =
  "midgard-watcher-production-operations-observability-v1" as const;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const MAXIMUM_PAGE_SIZE = 100;

export const MAXIMUM_RETAINED_DIAGNOSTICS = 10_000;

export const WATCHER_ALERT_CODES = Object.freeze([
  "da_fetch_failure",
  "root_mismatch",
  "proof_submission_failure",
  "maturity_deadline_risk",
  "provider_disagreement",
  "chain_rollback",
  "deployment_fingerprint_mismatch",
  "proof_family_coverage_gap",
  "l1_source_stale",
  "da_bond_pool_under_backed",
  "da_bond_pool_withdrawing",
] as const);

export type WatcherAlertCode = (typeof WATCHER_ALERT_CODES)[number];

/**
 * Alerts that tell the operator about the deployment, not about this watcher's
 * health. They are listed in `activeAlerts` but never make the watcher
 * `not_ready` (spec #685 E5): a short or withdrawing DA bond pool pauses
 * attestations, while the watcher keeps verifying and challenging.
 */
export const WATCHER_INFORMATIONAL_ALERT_CODES: ReadonlySet<WatcherAlertCode> =
  new Set<WatcherAlertCode>([
    "da_bond_pool_under_backed",
    "da_bond_pool_withdrawing",
  ]);

/** The last pooled DA bond readout, as `/v1/status` serves it. */
export type WatcherOperationsDaBondPool = WatcherDaBondPoolObservation &
  Readonly<{ observedAtMs: string }>;

/** The latest failed pool read since the last good one, as `/v1/status` serves it. */
export type WatcherOperationsDaBondPoolReadFailure = Readonly<{
  error: string;
  failedAtMs: string;
}>;

export type WatcherOperationsDiagnosticKind =
  | "verification"
  | "da_fetch"
  | "proof_step"
  | "event"
  | "l1_source"
  | "alert";

export const WATCHER_PROOF_STAGE_KINDS = Object.freeze([
  "prepare",
  "init",
  "proof_step",
  "publication",
  "certification",
  "correction",
  "removal",
  "terminal",
] as const);

export type WatcherProofStageKind = (typeof WATCHER_PROOF_STAGE_KINDS)[number];

type Sequenced = Readonly<{ sequence: string }>;

export type WatcherVerificationDiagnostic = Sequenced &
  Readonly<{
    kind: "verification";
    subjectDigest: string;
    queuedAtMs: string;
    startedAtMs: string;
    completedAtMs: string;
    /** Elapsed duration measured with a monotonic clock, rounded up to milliseconds. */
    elapsedMs: string;
    outcome:
      | "verified"
      | "pending_da"
      | "unprovable_gap"
      | "fault_detected"
      | "fault_proven"
      | "removed_or_resolved"
      | "failed";
  }>;

export type WatcherDaFetchDiagnostic = Sequenced &
  Readonly<{
    kind: "da_fetch";
    subjectDigest: string;
    startedAtMs: string;
    completedAtMs: string;
    /** Elapsed duration measured with a monotonic clock, rounded up to milliseconds. */
    elapsedMs: string;
    outcome: "succeeded" | "failed" | "timed_out";
  }>;

export type WatcherProofStepDiagnostic = Sequenced &
  Readonly<{
    kind: "proof_step";
    decisionDigest: string;
    stage: WatcherProofStageKind;
    actionIdentityDigest: string;
    status:
      | "queued"
      | "preflight"
      | "submitted"
      | "confirmed"
      | "reconciling"
      | "completed"
      | "cancelled"
      | "failed";
    updatedAtMs: string;
  }>;

export type WatcherEventDiagnostic = Sequenced &
  Readonly<{
    kind: "event";
    eventDigest: string;
    eventKind: "deposit" | "withdrawal" | "forced_order" | "settlement";
    status: "unprocessed" | "processed" | "invalid";
    inclusionAtMs: string;
    updatedAtMs: string;
  }>;

export type WatcherL1SourceDiagnostic = Sequenced &
  Readonly<{
    kind: "l1_source";
    sourceIdentityDigest: string;
    sourceMode: "local_node" | "external_provider";
    status: "consistent" | "stale" | "disagreement";
    blockHash: string;
    blockNo: string;
    slot: string;
    observedAtMs: string;
  }>;

export type WatcherAlertDiagnostic = Sequenced &
  Readonly<{
    kind: "alert";
    code: WatcherAlertCode;
    subjectDigest: string;
    active: boolean;
    observedAtMs: string;
  }>;

export type WatcherOperationsDiagnostic =
  | WatcherVerificationDiagnostic
  | WatcherDaFetchDiagnostic
  | WatcherProofStepDiagnostic
  | WatcherEventDiagnostic
  | WatcherL1SourceDiagnostic
  | WatcherAlertDiagnostic;

export type WatcherOperationsStatus = Readonly<{
  schemaVersion: typeof WATCHER_OPERATIONS_OBSERVABILITY;
  deploymentFingerprint: string;
  observedAtMs: string;
  liveness: "live" | "stopping" | "stopped" | "blocked";
  readiness: "ready" | "not_ready";
  readinessReasons: readonly (
    | "supervisor_not_accepting"
    | "recovery_incomplete"
    | "launch_scope_incomplete"
    | "deadline_at_risk"
    | "deadline_unsafe"
    | "l1_source_unavailable"
    | "l1_source_stale"
    | "retained_da_transport_failed"
    | "active_alert"
  )[];
  retainedDaTransport: WatcherRetainedDaTransportStatus;
  launchScope: Readonly<{
    installedCategoryCount: string;
    requiredCategoryCount: string;
    complete: boolean;
  }>;
  supervisor: ReturnType<WatcherFaultProofSupervisor["status"]>;
  activeAlerts: readonly Readonly<{
    code: WatcherAlertCode;
    subjectDigest: string;
    observedAtMs: string;
  }>[];
  /** The pooled DA bond as last read, or `null` before the first read. */
  daBondPool: WatcherOperationsDaBondPool | null;
  /**
   * The latest failed pool read, or `null` once a read succeeds. Reported
   * only: it never adds a readiness reason (spec #685 E5), and `daBondPool`
   * keeps the last good readout, whose `observedAtMs` shows its age.
   */
  daBondPoolReadFailure: WatcherOperationsDaBondPoolReadFailure | null;
}>;

export type WatcherOperationsMetrics = Readonly<{
  schemaVersion: typeof WATCHER_OPERATIONS_OBSERVABILITY;
  observedAtMs: string;
  queuedProofCount: string;
  oldestQueuedProofAgeMs: string | null;
  verificationLatencyMs: Readonly<{
    sampleCount: string;
    p50: string | null;
    p95: string | null;
    maximum: string | null;
  }>;
  daLatencyMs: Readonly<{
    sampleCount: string;
    p50: string | null;
    p95: string | null;
    maximum: string | null;
  }>;
  deadlineHealth: "safe" | "at_risk" | "unsafe";
  remainingSafeStartMs: string | null;
  proofSteps: Readonly<{
    queued: string;
    preflight: string;
    submitted: string;
    confirmed: string;
    reconciling: string;
    completed: string;
    cancelled: string;
    failed: string;
  }>;
  unprocessedEventCount: string;
  oldestUnprocessedEventAgeMs: string | null;
  l1Sources: Readonly<{
    configured: string;
    fresh: string;
    stale: string;
    disagreement: string;
    maximumFreshnessAgeMs: string | null;
  }>;
  activeAlertCount: string;
}>;

export type WatcherOperationsPage = Readonly<{
  schemaVersion: typeof WATCHER_OPERATIONS_OBSERVABILITY;
  kind: WatcherOperationsDiagnosticKind;
  records: readonly WatcherOperationsDiagnostic[];
  nextCursor: string | null;
}>;

export type WatcherOperationsApi = Readonly<{
  status(): WatcherOperationsStatus;
  metrics(): WatcherOperationsMetrics;
  diagnostics(
    input: Readonly<{
      kind: WatcherOperationsDiagnosticKind;
      cursor?: string;
      limit?: number;
    }>,
  ): WatcherOperationsPage;
}>;

export type WatcherOperationsSink = Readonly<{
  recordVerification(
    value: Omit<WatcherVerificationDiagnostic, "kind" | "sequence">,
  ): void;
  recordDaFetch(
    value: Omit<WatcherDaFetchDiagnostic, "kind" | "sequence">,
  ): void;
  recordProofStep(
    value: Omit<WatcherProofStepDiagnostic, "kind" | "sequence">,
  ): void;
  recordEvent(value: Omit<WatcherEventDiagnostic, "kind" | "sequence">): void;
  recordL1Source(
    value: Omit<WatcherL1SourceDiagnostic, "kind" | "sequence">,
  ): void;
  setAlert(value: Omit<WatcherAlertDiagnostic, "kind" | "sequence">): void;
  /**
   * Keeps `readout` as the served pool readout and sets both pool alerts from
   * it, so they fire on a drain or `BeginWithdraw` and clear after a top-up or
   * cancel. An alert diagnostic is appended only when an alert's state
   * changes, so a steady pool does not flood the bounded diagnostics.
   */
  recordDaBondPool(
    readout: WatcherDaBondPoolObservation,
    subjectDigest: string,
    observedAtMs: string,
  ): void;
  /**
   * Serves a failed pool read until the next `recordDaBondPool`. The pool
   * readout and both pool alerts keep their last good values.
   */
  recordDaBondPoolReadFailure(error: string, failedAtMs: string): void;
}>;

export type WatcherOperationsObservability = Readonly<{
  schemaVersion: typeof WATCHER_OPERATIONS_OBSERVABILITY;
  api: WatcherOperationsApi;
  sink: WatcherOperationsSink;
  handleHttpRequest(request: Request): Promise<Response>;
}>;

export const natural = (value: string, label: string): bigint => {
  if (!NATURAL.test(value)) throw new Error(`${label} is invalid`);
  return BigInt(value);
};
