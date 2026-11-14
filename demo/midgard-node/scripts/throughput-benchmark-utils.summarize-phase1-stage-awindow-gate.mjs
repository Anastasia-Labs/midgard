/**
 * Evaluates the Phase 1 five-minute Stage-A gate from an observer-only
 * checkpoint taken while a longer open-loop stage continues to consume the
 * same corpus cursors. This deliberately accepts already-summarized latency
 * values so the live checkpoint only records array lengths; sorting millions
 * of samples happens after measured traffic has stopped.
 */
export const summarizePhase1StageAWindowGate = ({
  checkpointAvailable,
  checkpointError = null,
  checkpointRequestedAfterMs,
  checkpointObservedAfterMs,
  checkpointMaxJitterMs = 1_000,
  measuredDurationSec,
  minDurationSec = 300,
  targetRateTps,
  durablyAdmitted,
  acceptedDelta,
  rejectedDelta,
  duplicateSuccesses,
  otherSuccesses,
  submitErrors,
  queueFullResponses,
  submitLatencyMs,
  scheduleLagMs,
  scheduledStarts,
  missedStarts,
  offeredRateMinRatio = 0.98,
  acceptedRateMinRatio = 0.99,
  submitLatencyP99MaxMs = 1_000,
  scheduleLagP95MaxMs = 100,
  scheduleLagP99MaxMs = 250,
  missedStartMaxRatio = 0.001,
  missingRequiredMetrics = [],
  streamContinuity,
}) => {
  const reasons = [];
  if (!checkpointAvailable) {
    reasons.push(
      checkpointError === null
        ? "five-minute checkpoint missing"
        : `five-minute checkpoint failed: ${checkpointError}`,
    );
  }
  const targetWindowMs = minDurationSec * 1_000;
  if (!Number.isFinite(checkpointRequestedAfterMs)) {
    reasons.push("checkpoint_requested_after_ms missing");
  } else if (checkpointRequestedAfterMs < targetWindowMs) {
    reasons.push(
      `checkpoint_requested_after_ms ${checkpointRequestedAfterMs.toFixed(3)} < target ${targetWindowMs.toFixed(3)}`,
    );
  } else if (
    checkpointRequestedAfterMs >
    targetWindowMs + checkpointMaxJitterMs
  ) {
    reasons.push(
      `checkpoint_requested_after_ms ${checkpointRequestedAfterMs.toFixed(3)} > latest ${(
        targetWindowMs + checkpointMaxJitterMs
      ).toFixed(3)}`,
    );
  }
  if (!Number.isFinite(checkpointObservedAfterMs)) {
    reasons.push("checkpoint_observed_after_ms missing");
  } else if (
    checkpointObservedAfterMs < checkpointRequestedAfterMs ||
    checkpointObservedAfterMs > targetWindowMs + checkpointMaxJitterMs
  ) {
    reasons.push(
      `checkpoint_observed_after_ms ${checkpointObservedAfterMs.toFixed(3)} outside request/latest bounds`,
    );
  }
  if (!Number.isFinite(measuredDurationSec)) {
    reasons.push("measured_duration_sec missing");
  } else if (measuredDurationSec < minDurationSec) {
    reasons.push(
      `measured_duration_sec ${measuredDurationSec.toFixed(3)} < ${minDurationSec}`,
    );
  }

  const durationForRate =
    Number.isFinite(measuredDurationSec) && measuredDurationSec > 0
      ? measuredDurationSec
      : null;
  const durablyAdmittedPerSec =
    durationForRate === null ? null : durablyAdmitted / durationForRate;
  const acceptedPerSec =
    durationForRate === null ? null : acceptedDelta / durationForRate;
  if (
    durablyAdmittedPerSec === null ||
    durablyAdmittedPerSec < targetRateTps * offeredRateMinRatio
  ) {
    reasons.push(
      durablyAdmittedPerSec === null
        ? "durably_admitted_per_sec missing"
        : `durably_admitted_per_sec ${durablyAdmittedPerSec.toFixed(2)} < ${(targetRateTps * offeredRateMinRatio).toFixed(2)}`,
    );
  }
  if (
    acceptedPerSec === null ||
    acceptedPerSec < targetRateTps * acceptedRateMinRatio
  ) {
    reasons.push(
      acceptedPerSec === null
        ? "accepted_per_sec missing"
        : `accepted_per_sec ${acceptedPerSec.toFixed(2)} < ${(targetRateTps * acceptedRateMinRatio).toFixed(2)}`,
    );
  }
  if (duplicateSuccesses > 0) {
    reasons.push(`duplicate_successes=${duplicateSuccesses}`);
  }
  if (otherSuccesses > 0) {
    reasons.push(`other_successes=${otherSuccesses}`);
  }
  if (submitErrors > 0) {
    reasons.push(`submit_errors=${submitErrors}`);
  }
  if (queueFullResponses > 0) {
    reasons.push(`queue_full_responses=${queueFullResponses}`);
  }
  if (rejectedDelta > 0) {
    reasons.push(`unexpected_rejections=${rejectedDelta}`);
  }
  if (missingRequiredMetrics.length > 0) {
    reasons.push(
      `missing_required_metrics=${missingRequiredMetrics.join(",")}`,
    );
  }
  if (submitLatencyMs?.p99 === null || submitLatencyMs?.p99 === undefined) {
    reasons.push("submit_latency_p99_ms missing");
  } else if (submitLatencyMs.p99 > submitLatencyP99MaxMs) {
    reasons.push(
      `submit_latency_p99_ms ${submitLatencyMs.p99.toFixed(2)} > ${submitLatencyP99MaxMs}`,
    );
  }
  if (scheduleLagMs?.p95 === null || scheduleLagMs?.p95 === undefined) {
    reasons.push("schedule_lag_p95_ms missing");
  } else if (scheduleLagMs.p95 > scheduleLagP95MaxMs) {
    reasons.push(
      `schedule_lag_p95_ms ${scheduleLagMs.p95.toFixed(2)} > ${scheduleLagP95MaxMs}`,
    );
  }
  if (scheduleLagMs?.p99 === null || scheduleLagMs?.p99 === undefined) {
    reasons.push("schedule_lag_p99_ms missing");
  } else if (scheduleLagMs.p99 > scheduleLagP99MaxMs) {
    reasons.push(
      `schedule_lag_p99_ms ${scheduleLagMs.p99.toFixed(2)} > ${scheduleLagP99MaxMs}`,
    );
  }
  const missedStartRatio =
    scheduledStarts + missedStarts > 0
      ? missedStarts / (scheduledStarts + missedStarts)
      : null;
  if (missedStartRatio === null) {
    reasons.push("missed_start_ratio missing");
  } else if (missedStartRatio > missedStartMaxRatio) {
    reasons.push(
      `missed_start_ratio ${missedStartRatio.toFixed(6)} > ${missedStartMaxRatio}`,
    );
  }
  if (streamContinuity?.passed !== true) {
    reasons.push(
      `stream_continuity_failed: ${streamContinuity?.reason ?? "missing continuity proof"}`,
    );
  }

  return {
    enabled: true,
    passed: reasons.length === 0,
    reasons,
    checkpointAvailable,
    checkpointError,
    checkpointRequestedAfterMs,
    checkpointObservedAfterMs,
    checkpointMaxJitterMs,
    measuredDurationSec,
    minDurationSec,
    targetRateTps,
    durablyAdmitted,
    durablyAdmittedPerSec,
    acceptedDelta,
    acceptedPerSec,
    rejectedDelta,
    duplicateSuccesses,
    otherSuccesses,
    submitErrors,
    queueFullResponses,
    submitLatencyMs,
    submitLatencyP99MaxMs,
    scheduleLagMs,
    scheduleLagP95MaxMs,
    scheduleLagP99MaxMs,
    scheduledStarts,
    missedStarts,
    missedStartRatio,
    missedStartMaxRatio,
    offeredRateMinRatio,
    acceptedRateMinRatio,
    missingRequiredMetrics,
    streamContinuity,
  };
};

export const summarizeOpenLoopCheckpointProgress = ({
  targetRateTps,
  durationSec,
  dispatchedStarts,
}) => {
  const expectedStarts = Math.max(
    0,
    Math.ceil(Number(targetRateTps) * Number(durationSec)),
  );
  const scheduledStarts = Math.max(0, Math.floor(Number(dispatchedStarts)));
  return {
    expectedStarts,
    scheduledStarts,
    missedStarts: Math.max(0, expectedStarts - scheduledStarts),
  };
};

export const gaugeSlopePerSec = (samples, counterKey) => {
  const points = samples
    .map((sample) => ({
      x: Number(sample.timestampMs),
      y: Number(sample.counters[counterKey] ?? 0),
    }))
    .filter((point) => Number.isFinite(point.x) && Number.isFinite(point.y));
  if (points.length < 2) {
    return 0;
  }
  const firstX = points[0].x;
  const normalized = points.map((point) => ({
    x: (point.x - firstX) / 1000,
    y: point.y,
  }));
  const meanX =
    normalized.reduce((sum, point) => sum + point.x, 0) / normalized.length;
  const meanY =
    normalized.reduce((sum, point) => sum + point.y, 0) / normalized.length;
  let numerator = 0;
  let denominator = 0;
  for (const point of normalized) {
    numerator += (point.x - meanX) * (point.y - meanY);
    denominator += (point.x - meanX) ** 2;
  }
  return denominator === 0 ? 0 : numerator / denominator;
};

export const interpolateCounterEvents = (samples, counterKey) => {
  const events = [];
  for (let index = 1; index < samples.length; index += 1) {
    const previous = samples[index - 1];
    const current = samples[index];
    const previousValue = Number(previous.counters[counterKey] ?? 0);
    const currentValue = Number(current.counters[counterKey] ?? 0);
    const delta = Math.floor(currentValue - previousValue);
    const elapsedMs = current.timestampMs - previous.timestampMs;
    if (delta <= 0 || elapsedMs <= 0) {
      continue;
    }
    for (let ordinal = 1; ordinal <= delta; ordinal += 1) {
      events.push(previous.timestampMs + (elapsedMs * ordinal) / delta);
    }
  }
  return events;
};
