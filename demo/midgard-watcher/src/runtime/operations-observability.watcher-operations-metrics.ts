import type { WatcherDaBondPoolObservation } from "../availability/pool-observation.js";
import type { WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";
import {
  WATCHER_STATE_QUEUE_REMOVAL_KINDS,
  type WatcherStateQueueRemovalKind,
} from "../indexers/authenticated-state-queue-observation.parse-persisted-header.js";
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
 * Alerts that tell the operator about the deployment or the chain, not about
 * this watcher's health. They are listed in `activeAlerts` but never make the
 * watcher `not_ready` (spec #685 E5): a short or withdrawing DA bond pool
 * pauses attestations, while the watcher keeps verifying and challenging. An
 * L1 rollback is routine before finality and the watcher replays through it.
 */
export const WATCHER_INFORMATIONAL_ALERT_CODES: ReadonlySet<WatcherAlertCode> =
  new Set<WatcherAlertCode>([
    "da_bond_pool_under_backed",
    "da_bond_pool_withdrawing",
    "chain_rollback",
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
    /** The classified state-queue header, when the subject is one. */
    headerHash?: string;
    /**
     * SHA-256 of the header's public DA payload envelope, present once
     * classification reached a decision. Only fault decisions are journaled,
     * so this binds a verified header to the exact bytes it was replayed from.
     */
    payloadEnvelopeSha256?: string;
    /** The L1 merge an `unverified_merged` header was skipped for. */
    mergeTransactionHash?: string;
    /** The L1 removal an `unverified_removed` header was skipped for. */
    removalTransactionHash?: string;
    removalKind?: WatcherStateQueueRemovalKind;
    queuedAtMs: string;
    startedAtMs: string;
    completedAtMs: string;
    /** Elapsed duration measured with a monotonic clock, rounded up to milliseconds. */
    elapsedMs: string;
    outcome:
      | "verified"
      | "pending_da"
      | "pending_l1"
      | "unprovable_gap"
      | "fault_detected"
      | "fault_proven"
      | "removed_or_resolved"
      | "unverified_merged"
      | "unverified_removed"
      | "unverified_past_horizon"
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

export type WatcherOperationsReadinessReason =
  | "supervisor_not_accepting"
  | "recovery_incomplete"
  | "launch_scope_incomplete"
  | "deadline_at_risk"
  | "deadline_unsafe"
  | "l1_source_unavailable"
  | "l1_source_stale"
  | "retained_da_transport_failed"
  | "active_alert"
  | "journal_capacity";

export type WatcherOperationsL1Degradation = Readonly<{
  reason: string;
  count: string;
  detail: string;
}>;

export type WatcherOperationsStatus = Readonly<{
  schemaVersion: typeof WATCHER_OPERATIONS_OBSERVABILITY;
  deploymentFingerprint: string;
  observedAtMs: string;
  liveness: "live" | "stopping" | "stopped" | "blocked";
  readiness: "ready" | "not_ready";
  /**
   * The watcher's own reasons, then each named L1 reason of `l1Readiness`
   * (the follower's and the decision driver's, for example
   * `l1_follower_catching_up`, `wallet_seed_pending` or a rollback
   * intervention).
   */
  readinessReasons: readonly (WatcherOperationsReadinessReason | string)[];
  /** The named L1 reasons with their detail; empty when L1 holds nothing. */
  l1Readiness: readonly Readonly<{ reason: string; detail: string }>[];
  /**
   * Named L1 degradations, for example `l1_tx_inputs_unresolvable` for
   * recorded txs no open proof needs whose inputs can never resolve. Reported
   * only: they never add a readiness reason.
   */
  l1Degradations: readonly WatcherOperationsL1Degradation[];
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
  /**
   * Headers that left the L1 queue at release finality before this process
   * verified them, counted once per header and L1 transaction. They are
   * diagnostics, never a readiness reason.
   */
  unverifiedHeaders: Readonly<{
    merged: string;
    removed: string;
    /**
     * Still queued, but past their challengeability horizon with no public DA
     * payload served: nothing about them can be challenged any more.
     */
    pastHorizon: string;
  }>;
  /**
   * Classification attempts that waited on public DA (`pending_da`) or on the
   * local L1 source (`pending_l1`). Diagnostics, never a readiness reason.
   */
  deferredClassifications: string;
  /** The count of each named L1 degradation. Never a readiness reason. */
  l1Degradations: Readonly<Record<string, string>>;
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

export const verificationSubject = (
  value: Readonly<{
    subjectDigest: string;
    headerHash?: string;
    payloadEnvelopeSha256?: string;
    mergeTransactionHash?: string;
    removalTransactionHash?: string;
    removalKind?: string;
  }>,
): void => {
  if (!HEX_32.test(value.subjectDigest))
    throw new Error("verification subject digest is invalid");
  if (
    value.headerHash !== undefined &&
    !/^[0-9a-f]{56}$/u.test(value.headerHash)
  )
    throw new Error("verification header hash is invalid");
  if (
    value.payloadEnvelopeSha256 !== undefined &&
    !HEX_32.test(value.payloadEnvelopeSha256)
  )
    throw new Error("verification payload envelope digest is invalid");
  if (
    value.mergeTransactionHash !== undefined &&
    !HEX_32.test(value.mergeTransactionHash)
  )
    throw new Error("verification merge transaction hash is invalid");
  if (
    value.removalTransactionHash !== undefined &&
    !HEX_32.test(value.removalTransactionHash)
  )
    throw new Error("verification removal transaction hash is invalid");
  if (
    value.removalKind !== undefined &&
    !WATCHER_STATE_QUEUE_REMOVAL_KINDS.some(
      (kind) => kind === value.removalKind,
    )
  )
    throw new Error("verification removal kind is invalid");
};

/**
 * Validates a verification record and returns its latency sample. A deferred
 * header is retried on a later wake, so a `pending_da` or `pending_l1` wait is
 * not a classification latency and returns `null`; neither is an
 * `unverified_*` skip, which classifies nothing.
 */
export const verificationLatency = (
  value: Omit<WatcherVerificationDiagnostic, "kind" | "sequence">,
): bigint | null => {
  verificationSubject(value);
  natural(value.queuedAtMs, "verification queue time");
  natural(value.startedAtMs, "verification start time");
  natural(value.completedAtMs, "verification completion time");
  const latency = natural(value.elapsedMs, "verification elapsed time");
  return value.outcome === "pending_da" ||
    value.outcome === "pending_l1" ||
    value.outcome === "unverified_merged" ||
    value.outcome === "unverified_removed" ||
    value.outcome === "unverified_past_horizon"
    ? null
    : latency;
};

/** Counts `unverified_*` records and deferred classification attempts. */
export const unverifiedHeaderCounter = () => {
  const counts = { merged: 0n, removed: 0n, pastHorizon: 0n, deferred: 0n };
  return Object.freeze({
    record: (outcome: WatcherVerificationDiagnostic["outcome"]): void => {
      if (outcome === "unverified_merged") counts.merged += 1n;
      if (outcome === "unverified_removed") counts.removed += 1n;
      if (outcome === "unverified_past_horizon") counts.pastHorizon += 1n;
      if (outcome === "pending_da" || outcome === "pending_l1")
        counts.deferred += 1n;
    },
    summary: (): WatcherOperationsMetrics["unverifiedHeaders"] =>
      Object.freeze({
        merged: counts.merged.toString(),
        removed: counts.removed.toString(),
        pastHorizon: counts.pastHorizon.toString(),
      }),
    deferred: (): string => counts.deferred.toString(),
  });
};
