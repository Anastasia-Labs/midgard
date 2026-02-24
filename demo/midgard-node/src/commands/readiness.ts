export type WorkerHeartbeats = {
  readonly blockCommitment: number;
  readonly blockConfirmation: number;
  readonly merge: number;
  readonly depositFetch: number;
  readonly txQueueProcessor: number;
};

export type ReadinessInput = {
  readonly nowMillis: number;
  readonly maxHeartbeatAgeMs: number;
  readonly maxQueueDepth: number;
  readonly queueDepth: number;
  readonly workerHeartbeats: WorkerHeartbeats;
  readonly localFinalizationPending: boolean;
  readonly dbHealthy: boolean;
};

export type ReadinessResult = {
  readonly ready: boolean;
  readonly reasons: readonly string[];
};

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

  return {
    ready: reasons.length === 0,
    reasons,
  };
};
