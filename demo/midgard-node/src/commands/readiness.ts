/**
 * Latest heartbeat timestamps reported by each long-running worker.
 */
export type WorkerHeartbeats = {
  readonly blockCommitment: number;
  readonly blockConfirmation: number;
  readonly merge: number;
  readonly depositFetch: number;
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

  const heartbeatThreshold = Math.max(1, input.maxHeartbeatAgeMs);
  const heartbeatEntries: readonly [string, number][] = [
    ["blockCommitment", input.workerHeartbeats.blockCommitment],
    ["blockConfirmation", input.workerHeartbeats.blockConfirmation],
    ["merge", input.workerHeartbeats.merge],
    ["depositFetch", input.workerHeartbeats.depositFetch],
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

  if (
    input.unresolvedBlockSubmissionAgeMs >
    Math.max(0, input.maxUnresolvedBlockSubmissionAgeMs)
  ) {
    reasons.push(
      `unresolved_block_submission:${input.unresolvedBlockSubmissionAgeMs}:${input.maxUnresolvedBlockSubmissionAgeMs}`,
    );
  }

  return {
    ready: reasons.length === 0,
    reasons,
  };
};
