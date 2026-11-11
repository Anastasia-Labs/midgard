import type { WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";
import { NO_WATCHER_REPLAY_REFUSALS } from "../fault-proofs/replay-refusals.js";
import { watcherDecisionHoldReason } from "../fault-proofs/watcher-decision-hold.js";
import type { WatcherRetainedDaTransportStatus } from "../storage/retained-da-runtime.js";
import { countWatcherReadinessReasons } from "./operations-observability.count-readiness-reasons.js";
import { handleWatcherOperationsHttpRequest } from "./operations-observability.handle-http-request.js";
import {
  createWatcherOperationsSink,
  type WatcherAgeAnchor,
  type WatcherOperationsRecorder,
  type WatcherOperationsSinkInput,
} from "./operations-observability.sink.js";
import {
  WATCHER_OPERATIONS_OBSERVABILITY,
  type WatcherOperationsApi,
  type WatcherOperationsMetrics,
  type WatcherOperationsObservability,
  type WatcherOperationsStatus,
} from "./operations-observability.watcher-operations-metrics.js";

export type WatcherOperationsViewInput = Readonly<{
  supervisor: WatcherFaultProofSupervisor;
  launchScopeStatus: () => Readonly<{
    installedCategoryCount: number;
    requiredCategoryCount: number;
  }>;
  /** Must be read from the same durable EDF queue as supervisor.status(). */
  durableProofQueueStatus: () => Readonly<{
    queuedJobCount: number;
    oldestQueuedAtMs: string | null;
  }>;
  /** Live state of the application's shared retained-DA transport. */
  retainedDaTransportStatus: () => WatcherRetainedDaTransportStatus;
  /** The L1 follower's and decision driver's named reasons, read
   * synchronously (the runtime caches them on every follower change). */
  l1Readiness?: () => readonly Readonly<{
    reason: string;
    detail: string;
  }>[];
  /** Named L1 degradations, read synchronously; never readiness reasons. */
  l1Degradations?: () => readonly Readonly<{
    reason: string;
    count: number;
    detail: string;
  }>[];
  replayRefusals?: () => WatcherOperationsStatus["replayRefusals"];
}>;

/**
 * The operations view over a sink's record: status, readiness, metrics and
 * diagnostics, joined with the supervisor and the runtime's L1 reads.
 */
export const createWatcherOperationsView = (
  { sink, record }: WatcherOperationsRecorder,
  input: WatcherOperationsViewInput,
): WatcherOperationsObservability => {
  let queueAge: WatcherAgeAnchor | null = null;

  const launchScope = () => {
    const value = input.launchScopeStatus();
    if (
      !Number.isSafeInteger(value.installedCategoryCount) ||
      value.installedCategoryCount < 0 ||
      !Number.isSafeInteger(value.requiredCategoryCount) ||
      value.requiredCategoryCount < 1 ||
      value.installedCategoryCount > value.requiredCategoryCount
    ) {
      throw new Error("observability launch-scope status is invalid");
    }
    return Object.freeze({
      installedCategoryCount: value.installedCategoryCount.toString(),
      requiredCategoryCount: value.requiredCategoryCount.toString(),
      complete: value.installedCategoryCount === value.requiredCategoryCount,
    });
  };

  const l1Degradations = () =>
    Object.freeze(
      (input.l1Degradations?.() ?? []).map(({ reason, count, detail }) =>
        Object.freeze({ reason, count: count.toString(), detail }),
      ),
    );
  const status = (): WatcherOperationsStatus => {
    const observedAt = record.nowMs();
    const supervisor = input.supervisor.status();
    const scope = launchScope();
    const sourceReason = record.l1SourceReason();
    const recorded = record.status();
    const reasons: string[] = [];
    if (supervisor.phase !== "accepting")
      reasons.push("supervisor_not_accepting");
    if (!supervisor.recovered) reasons.push("recovery_incomplete");
    if (!scope.complete) reasons.push("launch_scope_incomplete");
    if (supervisor.deadlineHealth === "at_risk")
      reasons.push("deadline_at_risk");
    if (supervisor.deadlineHealth === "unsafe") reasons.push("deadline_unsafe");
    if (supervisor.journalIntegrity !== null) reasons.push("journal_integrity");
    if (supervisor.journalUnavailable !== null)
      reasons.push("journal_unavailable");
    if (supervisor.journalCapacity) reasons.push("journal_capacity");
    if (supervisor.objectiveCleanupFailures.length > 0)
      reasons.push("fault_proof_objective_cleanup_failed");
    const holds = supervisor.journalDecisionMissing;
    reasons.push(...new Set(holds.map(watcherDecisionHoldReason)));
    if (supervisor.journalBusy !== null) reasons.push("journal_busy");
    if (sourceReason !== null) reasons.push(sourceReason);
    const l1Readiness = Object.freeze(
      (input.l1Readiness?.() ?? []).map(({ reason, detail }) =>
        Object.freeze({ reason, detail }),
      ),
    );
    for (const { reason } of l1Readiness)
      if (!reasons.includes(reason)) reasons.push(reason);
    const retainedDaTransport = input.retainedDaTransportStatus();
    if (retainedDaTransport.state === "failed")
      reasons.push("retained_da_transport_failed");
    if (record.alertsHoldReadiness(observedAt)) reasons.push("active_alert");
    const liveness =
      supervisor.phase === "closed"
        ? "stopped"
        : supervisor.phase === "closing"
          ? "stopping"
          : supervisor.phase === "blocked"
            ? "blocked"
            : "live";
    return Object.freeze({
      schemaVersion: WATCHER_OPERATIONS_OBSERVABILITY,
      deploymentFingerprint: record.deploymentFingerprint,
      observedAtMs: observedAt.toString(),
      liveness,
      readiness: reasons.length === 0 ? "ready" : "not_ready",
      readinessReasons: Object.freeze(reasons),
      l1Readiness,
      l1Degradations: l1Degradations(),
      retainedDaTransport,
      launchScope: scope,
      supervisor,
      activeAlerts: recorded.activeAlerts,
      daBondPool: recorded.daBondPool,
      daBondPoolReadFailure: recorded.daBondPoolReadFailure,
      replayRefusals: input.replayRefusals?.() ?? NO_WATCHER_REPLAY_REFUSALS,
    });
  };

  const metrics = (): WatcherOperationsMetrics => {
    const observedAt = record.nowMs();
    const supervisor = input.supervisor.status();
    const recorded = record.metrics();
    const durableQueue = input.durableProofQueueStatus();
    if (
      !Number.isSafeInteger(durableQueue.queuedJobCount) ||
      durableQueue.queuedJobCount < 0 ||
      durableQueue.queuedJobCount !== supervisor.queuedJobCount ||
      (durableQueue.queuedJobCount === 0) !==
        (durableQueue.oldestQueuedAtMs === null)
    ) {
      throw new Error("durable proof queue status differs from supervisor");
    }
    if (durableQueue.oldestQueuedAtMs === null) queueAge = null;
    else if (queueAge?.origin !== durableQueue.oldestQueuedAtMs) {
      queueAge = record.anchorAge(durableQueue.oldestQueuedAtMs);
    }
    const queuedAge = queueAge === null ? null : record.ageMs(queueAge);
    return Object.freeze({
      schemaVersion: WATCHER_OPERATIONS_OBSERVABILITY,
      observedAtMs: observedAt.toString(),
      queuedProofCount: durableQueue.queuedJobCount.toString(),
      oldestQueuedProofAgeMs: queuedAge?.toString() ?? null,
      verificationLatencyMs: recorded.verificationLatencyMs,
      daLatencyMs: recorded.daLatencyMs,
      deadlineHealth: supervisor.deadlineHealth,
      remainingSafeStartMs: supervisor.remainingSafeStartMs,
      proofSteps: recorded.proofSteps,
      unprocessedEventCount: recorded.unprocessedEventCount,
      oldestUnprocessedEventAgeMs: recorded.oldestUnprocessedEventAgeMs,
      l1Sources: recorded.l1Sources,
      activeAlertCount: recorded.activeAlertCount,
      unverifiedHeaders: recorded.unverifiedHeaders,
      deferredClassifications: recorded.deferredClassifications,
      l1Degradations: Object.freeze(
        Object.fromEntries(l1Degradations().map((d) => [d.reason, d.count])),
      ),
      l1Readiness: countWatcherReadinessReasons(input.l1Readiness?.() ?? []),
    });
  };

  const api: WatcherOperationsApi = Object.freeze({
    status,
    metrics,
    diagnostics: record.diagnostics,
  });

  return Object.freeze({
    schemaVersion: WATCHER_OPERATIONS_OBSERVABILITY,
    api,
    sink,
    handleHttpRequest: (request: Request): Promise<Response> =>
      handleWatcherOperationsHttpRequest(request, api),
  });
};

/** A sink and its view at once, for callers with nothing to report before
 * the view's inputs exist. */
export const createWatcherOperationsObservability = (
  input: WatcherOperationsSinkInput & WatcherOperationsViewInput,
): WatcherOperationsObservability =>
  createWatcherOperationsView(createWatcherOperationsSink(input), input);
