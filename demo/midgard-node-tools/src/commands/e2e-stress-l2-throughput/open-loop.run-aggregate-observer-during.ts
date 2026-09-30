import {
  type NoOpCalibrationSummary,
  type OpenLoopPlacementProof,
  type OpenLoopSubmitSummary,
} from "../stress-open-loop.js";
import { type StressMetrics } from "../stress-stage-metrics.js";
import { appendEvent } from "./artifact-files.js";
import { errorMessage } from "./runtime.js";
import {
  type E2EL2StressClassification,
  type E2EL2StressConfig,
  type E2EL2StressRuntime,
} from "./types.js";

type AggregateObserverRunSummary = {
  readonly sampleCount: number;
  readonly errorCount: number;
  readonly overloaded: boolean;
};

export const runAggregateObserverDuring = async <A>({
  config,
  runtime,
  action,
  eventsNdjsonPath,
  sleepImpl,
  now,
}: {
  readonly config: E2EL2StressConfig;
  readonly runtime: E2EL2StressRuntime;
  readonly action: () => Promise<A>;
  readonly eventsNdjsonPath: string;
  readonly sleepImpl: (ms: number) => Promise<void>;
  readonly now: () => Date;
}): Promise<{
  readonly result: A;
  readonly observer: AggregateObserverRunSummary;
}> => {
  if (runtime.collectAggregateObserverSample === undefined) {
    return {
      result: await action(),
      observer: { sampleCount: 0, errorCount: 0, overloaded: false },
    };
  }
  let done = false;
  let sampleCount = 0;
  let errorCount = 0;
  let overloaded = false;
  const observer = (async () => {
    while (!done) {
      const sampleStartedAt = now();
      const sampleStartedMs = sampleStartedAt.getTime();
      try {
        const sample = await runtime.collectAggregateObserverSample!({
          at: sampleStartedAt.toISOString(),
          runId: config.runId,
          loadModel: config.loadModel,
        });
        sampleCount += 1;
        const durationMs = Math.max(0, now().getTime() - sampleStartedMs);
        if (durationMs > config.aggregateObserverIntervalMs) {
          overloaded = true;
        }
        await appendEvent(eventsNdjsonPath, {
          event: "stress.aggregate_observer.sample",
          at: sampleStartedAt.toISOString(),
          durationMs,
          overloaded: durationMs > config.aggregateObserverIntervalMs,
          sample,
        });
      } catch (error) {
        errorCount += 1;
        await appendEvent(eventsNdjsonPath, {
          event: "stress.aggregate_observer.error",
          at: now().toISOString(),
          error: errorMessage(error),
        });
      }
      await sleepImpl(config.aggregateObserverIntervalMs);
    }
  })();
  try {
    const result = await action();
    done = true;
    await observer;
    return {
      result,
      observer: { sampleCount, errorCount, overloaded },
    };
  } catch (error) {
    done = true;
    await observer;
    throw error;
  }
};

export const classifyOpenLoopRun = ({
  metrics,
  submission,
  calibration,
  placement,
  observer,
}: {
  readonly metrics: StressMetrics;
  readonly submission: OpenLoopSubmitSummary;
  readonly calibration?: NoOpCalibrationSummary;
  readonly placement: OpenLoopPlacementProof;
  readonly observer: AggregateObserverRunSummary;
}): E2EL2StressClassification => {
  if (
    !placement.validForUpperBoundClaim ||
    calibration?.passed === false ||
    submission.submittedOfferedRatio < 0.98
  ) {
    return "client_overloaded";
  }
  if (observer.overloaded || observer.errorCount > 0) {
    return "observer_overloaded";
  }
  if (
    metrics.durableAdmission.status !== "complete" ||
    metrics.durableAdmission.count < submission.submittedCount
  ) {
    return "admission_bottleneck";
  }
  if (
    metrics.l2Admission.status !== "complete" ||
    metrics.l2Admission.count < submission.submittedCount
  ) {
    return "validation_bottleneck";
  }
  if (metrics.fullFinality.status === "complete") {
    return "full_pipeline_sustained";
  }
  return "ingress_ok_commit_failed";
};
