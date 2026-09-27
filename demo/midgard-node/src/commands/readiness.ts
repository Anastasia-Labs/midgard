/**
 * Latest heartbeat timestamps reported by each long-running worker.
 */
export type WorkerHeartbeats = {
  readonly blockCommitment: number;
  readonly blockConfirmation: number;
  readonly merge: number;
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
  /** The operator's attestation-timeout correction step. Every node runs it;
   * the field is optional only so callers without the fiber (tests) may omit
   * it. */
  readonly attestationTimeoutCorrection?: {
    readonly consecutiveFailures: number;
    readonly oldestUnattestedHeader: {
      readonly headerHash: string;
      readonly deadlineMs: number;
    } | null;
    readonly lastProgressAtMs: number;
    readonly lastQueueReadAtMs: number;
    /** Longest a healthy correction goes without progress. */
    readonly stallBoundMs: number;
    /** Longest the queue may go unread before a header could have come due
     * unseen. */
    readonly queueUnknownBoundMs: number;
  };
};

/**
 * Consecutive failed correction steps that make a pending timeout correction
 * unready. One failed tick is a transient provider blip the next tick retries;
 * three in a row (about 30 s at the default 10 s interval) is a correction
 * that is not happening.
 */
export const ATTESTATION_TIMEOUT_CORRECTION_FAILURE_THRESHOLD = 3;

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

  const correction = input.attestationTimeoutCorrection;
  if (correction !== undefined) {
    // Failing or stalling only matters while there is a correction to make:
    // an unattested header past its DA-attestation deadline.
    const unattested = correction.oldestUnattestedHeader;
    if (unattested !== null && input.nowMillis >= unattested.deadlineMs) {
      const overdueMs = input.nowMillis - unattested.deadlineMs;
      if (
        correction.consecutiveFailures >=
        ATTESTATION_TIMEOUT_CORRECTION_FAILURE_THRESHOLD
      ) {
        reasons.push(
          `attestation_timeout_correction_failing:${unattested.headerHash}:${correction.consecutiveFailures}:${overdueMs}`,
        );
      }
      // A step that neither fails nor finishes (hung waiting on a
      // confirmation) never raises the failure count.
      const sinceProgressMs = input.nowMillis - correction.lastProgressAtMs;
      if (sinceProgressMs > correction.stallBoundMs) {
        reasons.push(
          `attestation_timeout_correction_stalled:${unattested.headerHash}:${sinceProgressMs}:${correction.stallBoundMs}`,
        );
      }
    }
    // A queue the step cannot read is not a queue with nothing to correct.
    const sinceQueueReadMs = input.nowMillis - correction.lastQueueReadAtMs;
    if (sinceQueueReadMs > correction.queueUnknownBoundMs) {
      reasons.push(
        `attestation_timeout_queue_unknown:${sinceQueueReadMs}:${correction.queueUnknownBoundMs}`,
      );
    }
  }

  return {
    ready: reasons.length === 0,
    reasons,
  };
};
