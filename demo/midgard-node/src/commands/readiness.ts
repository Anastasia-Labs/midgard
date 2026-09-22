/**
 * Latest heartbeat timestamps reported by each long-running worker.
 */
export type WorkerHeartbeats = {
  readonly blockCommitment: number;
  readonly blockConfirmation: number;
  readonly merge: number;
  readonly depositFetch: number;
  readonly withdrawalFetch: number;
  readonly txQueueProcessor: number;
};

/**
 * Inputs required to evaluate node readiness.
 */
export type ReadinessInput = {
  readonly nowMillis: number;
  readonly maxHeartbeatAgeMs: number;
  readonly maxQueueDepth: number;
  readonly queueDepth: number;
  readonly workerHeartbeats: WorkerHeartbeats;
  readonly localFinalizationPending: boolean;
  readonly unresolvedBlockSubmissionAgeMs: number;
  readonly maxUnresolvedBlockSubmissionAgeMs: number;
  readonly dbHealthy: boolean;
  readonly awaitingForeignTipReconciliations: number;
  readonly validationPool?: {
    readonly configuredWorkers: number;
    readonly liveWorkers: number;
    readonly restartingWorkers: number;
    readonly oldestInFlightAgeMs: number;
    readonly jobTimeoutMs: number;
  };
  readonly stateQueueMutationLease?: {
    readonly active: boolean;
    readonly stale: boolean;
    readonly remainingMs: number | null;
    readonly holder: string | null;
  };
  /**
   * Retention deadline signal (GOAL_SPEC 9.4 / Q54): number of retained DA
   * records that are still challengeable and inside their alert threshold.
   */
  readonly retentionDeadlineAlerts?: number;
};

/**
 * Readiness outcome returned by the readiness endpoint/command.
 */
export type ReadinessResult = {
  readonly ready: boolean;
  readonly reasons: readonly string[];
};

/**
 * Evaluates whether the node is ready to serve traffic based on database
 * health, worker liveness, queue depth, and local recovery state.
 */
export const evaluateReadiness = (input: ReadinessInput): ReadinessResult => {
  const reasons: string[] = [];

  if (!input.dbHealthy) {
    reasons.push("db_unhealthy");
  }

  if (input.awaitingForeignTipReconciliations > 0) {
    reasons.push(
      `foreign_tip_reconciliation_awaiting:${input.awaitingForeignTipReconciliations.toString()}`,
    );
  }

  const heartbeatThreshold = Math.max(1, input.maxHeartbeatAgeMs);
  const heartbeatEntries: readonly [string, number][] = [
    ["blockCommitment", input.workerHeartbeats.blockCommitment],
    ["blockConfirmation", input.workerHeartbeats.blockConfirmation],
    ["merge", input.workerHeartbeats.merge],
    ["depositFetch", input.workerHeartbeats.depositFetch],
    ["withdrawalFetch", input.workerHeartbeats.withdrawalFetch],
    ["txQueueProcessor", input.workerHeartbeats.txQueueProcessor],
  ];

  for (const [workerName, heartbeatMillis] of heartbeatEntries) {
    const age = input.nowMillis - heartbeatMillis;
    if (age > heartbeatThreshold) {
      reasons.push(`stale_heartbeat:${workerName}:${age}`);
    }
  }

  const queueLimit = Math.max(0, input.maxQueueDepth);
  if (input.queueDepth > queueLimit) {
    reasons.push(`queue_depth_exceeded:${input.queueDepth}:${queueLimit}`);
  }

  if (input.localFinalizationPending) {
    reasons.push("local_finalization_pending");
  }

  if (input.validationPool !== undefined) {
    const pool = input.validationPool;
    if (
      pool.configuredWorkers > 0 &&
      pool.liveWorkers < pool.configuredWorkers
    ) {
      reasons.push(
        `validation_worker_pool_degraded:${pool.liveWorkers}:${pool.configuredWorkers}:${pool.restartingWorkers}`,
      );
    }
    if (pool.oldestInFlightAgeMs > Math.max(1, pool.jobTimeoutMs)) {
      reasons.push(
        `validation_worker_job_timeout:${pool.oldestInFlightAgeMs}:${pool.jobTimeoutMs}`,
      );
    }
  }

  if (
    input.unresolvedBlockSubmissionAgeMs >
    Math.max(0, input.maxUnresolvedBlockSubmissionAgeMs)
  ) {
    reasons.push(
      `unresolved_block_submission:${input.unresolvedBlockSubmissionAgeMs}:${input.maxUnresolvedBlockSubmissionAgeMs}`,
    );
  }

  if (input.stateQueueMutationLease?.stale === true) {
    reasons.push(
      `state_queue_lease_stale:${
        input.stateQueueMutationLease.holder ?? "unknown"
      }:${input.stateQueueMutationLease.remainingMs ?? "unknown"}`,
    );
  }

  if (
    input.retentionDeadlineAlerts !== undefined &&
    input.retentionDeadlineAlerts > 0
  ) {
    reasons.push(
      `retention_deadline_alert:${input.retentionDeadlineAlerts.toString()}`,
    );
  }

  return {
    ready: reasons.length === 0,
    reasons,
  };
};
