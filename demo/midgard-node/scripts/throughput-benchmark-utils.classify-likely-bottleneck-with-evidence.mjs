export const classifyLikelyBottleneckWithEvidence = ({
  submitted,
  submitErrors,
  queueFullResponses = 0,
  acceptedDelta,
  rejectedDelta,
  commitTxDelta,
  mergeBlockDelta,
  targetAcceptedTps,
  avgAcceptedTps,
  clientSelfCheck,
  endCounters,
  waitForCommit,
  waitForMerge,
  scheduleLagMs = null,
  missedStarts = 0,
  inFlightHighWater = 0,
  submitConcurrency = 0,
  backlogSlopePerSec = 0,
  requiredMetricsMissing = [],
}) => {
  const evidence = {
    submitted,
    submitErrors,
    queueFullResponses,
    acceptedDelta,
    rejectedDelta,
    commitTxDelta,
    mergeBlockDelta,
    targetAcceptedTps,
    avgAcceptedTps,
    scheduleLagP95Ms: scheduleLagMs?.p95 ?? null,
    missedStarts,
    inFlightHighWater,
    submitConcurrency,
    backlogSlopePerSec,
    requiredMetricsMissing,
  };
  if (
    clientSelfCheck !== null &&
    clientSelfCheck.required === true &&
    clientSelfCheck.targetRate > 0 &&
    clientSelfCheck.achievedRate <
      (clientSelfCheck.minRequiredRate ?? clientSelfCheck.targetRate)
  ) {
    return {
      label: "benchmark-client limited",
      rule: "client self-check achieved rate below required rate",
      evidence,
    };
  }
  if (
    scheduleLagMs !== null &&
    scheduleLagMs.p95 !== null &&
    scheduleLagMs.p95 > 100
  ) {
    return {
      label: "benchmark-client limited",
      rule: "open-loop schedule lag p95 exceeded 100ms",
      evidence,
    };
  }
  if (
    submitConcurrency > 0 &&
    inFlightHighWater >= Math.floor(submitConcurrency * 0.98)
  ) {
    return {
      label: "benchmark-client limited",
      rule: "submit in-flight high-water reached configured concurrency",
      evidence,
    };
  }
  if (submitted <= 0) {
    return {
      label: "funding/workload exhausted",
      rule: "no transactions were submitted in the measured window",
      evidence,
    };
  }
  if (submitErrors > 0 || queueFullResponses > 0) {
    return {
      label: "HTTP ingress limited",
      rule: "measured-stage submit errors or queue-full responses were observed",
      evidence,
    };
  }
  if (requiredMetricsMissing.length > 0) {
    return {
      label: "metrics unavailable",
      rule: "required benchmark metrics were absent from Prometheus output",
      evidence,
    };
  }
  if (acceptedDelta + rejectedDelta < submitted) {
    const queueDepth = Number(endCounters.validationQueueDepth ?? 0);
    return {
      label: queueDepth > 0 ? "queue scheduling limited" : "Phase A/B limited",
      rule:
        queueDepth > 0
          ? "validation queue depth remained non-zero after measured submissions"
          : "accepted plus rejected count did not catch submitted count",
      evidence: {
        ...evidence,
        validationQueueDepth: queueDepth,
      },
    };
  }
  if (backlogSlopePerSec > 0.1) {
    return {
      label: "node throughput limited",
      rule: "validation backlog had a positive measured-window slope",
      evidence,
    };
  }
  if (rejectedDelta > 0) {
    return {
      label: "validation/workload limited",
      rule: "unexpected validation rejections were observed",
      evidence,
    };
  }
  if (waitForCommit && commitTxDelta < acceptedDelta) {
    return {
      label:
        Number(endCounters.unconfirmedSubmittedBlockPending ?? 0) > 0
          ? "L1 confirmation limited"
          : "commit limited",
      rule: "committed transaction count did not catch accepted count",
      evidence,
    };
  }
  if (waitForMerge && mergeBlockDelta <= 0 && commitTxDelta > 0) {
    return {
      label: "merge limited",
      rule: "commitment progressed but merge block counter did not advance",
      evidence,
    };
  }
  if (
    Number.isFinite(targetAcceptedTps) &&
    targetAcceptedTps > 0 &&
    avgAcceptedTps < targetAcceptedTps
  ) {
    return {
      label: "node throughput limited",
      rule: "average accepted TPS was below target accepted TPS",
      evidence,
    };
  }
  return {
    label: "no bottleneck detected",
    rule: "candidate met measured throughput and backlog criteria",
    evidence,
  };
};

export const classifyLikelyBottleneck = ({
  submitted,
  submitErrors,
  acceptedDelta,
  rejectedDelta,
  commitTxDelta,
  mergeBlockDelta,
  targetAcceptedTps,
  avgAcceptedTps,
  clientSelfCheck,
  endCounters,
  waitForCommit,
  waitForMerge,
}) => {
  if (
    clientSelfCheck !== null &&
    clientSelfCheck.required === true &&
    clientSelfCheck.targetRate > 0 &&
    clientSelfCheck.achievedRate <
      (clientSelfCheck.minRequiredRate ?? clientSelfCheck.targetRate)
  ) {
    return "benchmark-client limited";
  }
  if (submitted <= 0) {
    return "funding/workload exhausted";
  }
  if (submitErrors > 0) {
    return "HTTP ingress limited";
  }
  if (acceptedDelta + rejectedDelta < submitted) {
    const queueDepth = Number(endCounters.validationQueueDepth ?? 0);
    return queueDepth > 0 ? "queue scheduling limited" : "Phase A/B limited";
  }
  if (rejectedDelta > 0) {
    return "validation/workload limited";
  }
  if (waitForCommit && commitTxDelta < acceptedDelta) {
    return Number(endCounters.unconfirmedSubmittedBlockPending ?? 0) > 0
      ? "L1 confirmation limited"
      : "commit limited";
  }
  if (waitForMerge && mergeBlockDelta <= 0 && commitTxDelta > 0) {
    return "merge limited";
  }
  if (
    Number.isFinite(targetAcceptedTps) &&
    targetAcceptedTps > 0 &&
    avgAcceptedTps < targetAcceptedTps
  ) {
    return "node throughput limited";
  }
  return "no bottleneck detected";
};

export const createPhaseRecorder = (clock = () => Date.now()) => {
  const phases = [];
  let current = null;
  return {
    start(name) {
      if (current !== null) {
        current.endMs = clock();
        current.durationMs = current.endMs - current.startMs;
        phases.push(current);
      }
      current = { name, startMs: clock(), endMs: null, durationMs: null };
    },
    end() {
      if (current !== null) {
        current.endMs = clock();
        current.durationMs = current.endMs - current.startMs;
        phases.push(current);
        current = null;
      }
    },
    list() {
      return current === null ? [...phases] : [...phases, { ...current }];
    },
  };
};
