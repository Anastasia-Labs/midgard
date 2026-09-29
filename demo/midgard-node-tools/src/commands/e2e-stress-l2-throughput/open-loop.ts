import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { formatJson } from "midgard-node/commands/command-utils";
import { sleep } from "midgard-node/sleep";

import {
  buildOpenLoopPlacementProof,
  type NoOpCalibrationSummary,
  type OpenLoopCorpusPlan,
  type OpenLoopPlacementProof,
  type OpenLoopSubmitSummary,
  summarizeOpenLoopSubmissions,
} from "../stress-open-loop.js";
import {
  buildStressMetrics,
  type StressMetrics,
  type StressStageMetricDbSources,
} from "../stress-stage-metrics.js";
import {
  appendEvent,
  readCorpusRowsForRecords,
  readJsonFile,
  readNdjsonLines,
  readSubmitRecords,
} from "./artifact-files.js";
import { artifactConfig } from "./config.js";
import { E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION } from "./constants.js";
import { canonicalEnginePaths, runCanonicalEngineProcess } from "./engine.js";
import { measurementPolicyForConfig } from "./policy.js";
import { renderStressSummaryMarkdown } from "./report.js";
import { abortReason, errorMessage, signalWasAborted } from "./runtime.js";
import { parseE2EL2StressSummary } from "./summary-artifact.js";
import {
  type E2EL2StressClassification,
  type E2EL2StressConfig,
  type E2EL2StressRunResult,
  type E2EL2StressRuntime,
  type E2EL2StressTransaction,
} from "./types.js";

type AggregateObserverRunSummary = {
  readonly sampleCount: number;
  readonly errorCount: number;
  readonly overloaded: boolean;
};

const runAggregateObserverDuring = async <A>({
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

const classifyOpenLoopRun = ({
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

export const runOpenLoopUpperBoundStress = async (
  config: E2EL2StressConfig,
  runtime: E2EL2StressRuntime,
): Promise<E2EL2StressRunResult> => {
  if (config.corpusPath === undefined) {
    throw new Error("open-loop upper-bound stress requires a corpusPath");
  }
  const sleepImpl = runtime.sleep ?? sleep;
  const now = runtime.now ?? (() => new Date());
  const signal = runtime.abortSignal;

  await mkdir(config.outDir, { recursive: true });
  const configJsonPath = join(config.outDir, "config.json");
  const eventsNdjsonPath = join(config.outDir, "events.ndjson");
  const summaryJsonPath = join(config.outDir, "summary.json");
  const summaryMarkdownPath = join(config.outDir, "summary.md");
  await writeFile(configJsonPath, `${formatJson(artifactConfig(config))}\n`, {
    encoding: "utf8",
    flag: "w",
  });

  const placement = buildOpenLoopPlacementProof();
  const startedAtDate = now();
  const startedAt = startedAtDate.toISOString();
  const enginePaths = canonicalEnginePaths(config.outDir);
  await appendEvent(eventsNdjsonPath, {
    event: "stress_started",
    at: startedAt,
    runId: config.runId,
    loadModel: config.loadModel,
    workloadProfile: config.workloadProfile,
    corpusShape: config.corpusShape,
    corpusSliceId: config.corpusSliceId,
    placement,
  });

  const { result: engineResult, observer } = await runAggregateObserverDuring({
    config,
    runtime,
    eventsNdjsonPath,
    sleepImpl,
    now,
    action: () =>
      (runtime.runCanonicalEngine ?? runCanonicalEngineProcess)({
        config,
        paths: enginePaths,
        signal,
      }),
  });

  const submissionFinishedAtDate = now();
  const submissionFinishedAt = submissionFinishedAtDate.toISOString();

  const engineReport = (await readJsonFile(enginePaths.engineReportJson)) as {
    readonly calibration?: { readonly noOp?: NoOpCalibrationSummary | null };
    readonly summary?: { readonly firstErrors?: readonly string[] };
  };
  const submitRecords = await readSubmitRecords(
    enginePaths.submitRecordsNdjson,
  );
  const corpusRowsByTxHash = await readCorpusRowsForRecords({
    corpusPath: config.corpusPath,
    records: submitRecords,
  });
  const corpusRows = submitRecords.flatMap((record) => {
    const row = corpusRowsByTxHash.get(record.txHash);
    return row === undefined ? [] : [row];
  });
  const corpus: OpenLoopCorpusPlan = {
    rows: corpusRows,
    requiredTransactionCount: submitRecords.length,
    selectedTransactionCount: submitRecords.length,
    corpusShape: config.corpusShape,
    corpusSliceId: config.corpusSliceId,
  };
  const summaryStartedAtMs =
    submitRecords.length === 0
      ? startedAtDate.getTime()
      : Math.min(...submitRecords.map((record) => record.scheduledAtMs));
  const summaryFinishedAtMs =
    submitRecords.length === 0
      ? submissionFinishedAtDate.getTime()
      : Math.max(
          ...submitRecords.map(
            (record) => record.submittedAtMs + record.latencyMs,
          ),
        );
  const submitResult = {
    records: submitRecords,
    summary: summarizeOpenLoopSubmissions({
      records: submitRecords,
      offeredCount: submitRecords.length,
      targetRateTps: config.targetRateTps,
      maxInFlight: config.openLoopMaxInFlight,
      maxObservedInFlight: config.openLoopMaxInFlight,
      startedAtMs: summaryStartedAtMs,
      finishedAtMs: summaryFinishedAtMs,
      startedAtIso: new Date(summaryStartedAtMs).toISOString(),
      finishedAtIso: new Date(summaryFinishedAtMs).toISOString(),
    }),
  };
  const calibration = engineReport.calibration?.noOp ?? undefined;
  if (calibration !== undefined) {
    await appendEvent(eventsNdjsonPath, {
      event: "stress.no_op_calibration.finished",
      at: now().toISOString(),
      calibration,
    });
  }
  if (engineResult.exitCode !== 0 && submitRecords.length === 0) {
    throw new Error(
      `canonical stress engine exited with ${engineResult.exitCode.toString()} before submitting records`,
    );
  }
  let translatedStageIndex = 0;
  for await (const line of readNdjsonLines(enginePaths.engineEventsNdjson)) {
    const event = JSON.parse(line) as {
      readonly event?: string;
      readonly at?: string;
      readonly [key: string]: unknown;
    };
    if (event.event === "stage_started") {
      await appendEvent(eventsNdjsonPath, {
        event: "stress.observer.started",
        at: event.at ?? now().toISOString(),
        stageName: event.name,
        targetRateTps: event.targetRateTps,
      });
    } else if (event.event === "counter_sample") {
      await appendEvent(eventsNdjsonPath, {
        event: "stress.aggregate_observer.sample",
        at: event.at ?? now().toISOString(),
        source: "canonical_engine",
        counters: event.counters,
        phase: event.phase,
      });
    } else if (event.event === "stage_finished") {
      translatedStageIndex += 1;
      await appendEvent(eventsNdjsonPath, {
        event:
          translatedStageIndex === 1
            ? "stress_submission_finished"
            : "stress.observer.finished",
        at: event.at ?? now().toISOString(),
        stageName: event.name,
        submitted: event.submitted,
        submitErrors: event.submitErrors,
        abortedCorpusExhausted: event.aborted_corpus_exhausted,
      });
    }
  }
  const submittedTxHashes = submitResult.records.flatMap((record) =>
    record.statusCode !== null &&
    record.statusCode >= 200 &&
    record.statusCode < 300
      ? [record.txHash]
      : [],
  );
  let dbMetricSources: StressStageMetricDbSources | undefined;
  if (runtime.collectStageMetricSources !== undefined) {
    dbMetricSources = await runtime.collectStageMetricSources({
      txHashes: submittedTxHashes,
    });
    await appendEvent(eventsNdjsonPath, {
      event: "stress.stage_metrics.db_sources_collected",
      at: now().toISOString(),
      txHashCount: submittedTxHashes.length,
      l2AdmissionRows: dbMetricSources.l2Admissions.length,
      l1CommitRows: dbMetricSources.l1Commits.length,
      immutableRows: dbMetricSources.immutableObservations.length,
      residueRows: dbMetricSources.residue.length,
    });
  }
  const admissionByTxHash = new Map(
    (dbMetricSources?.l2Admissions ?? []).map((row) => [row.txHash, row]),
  );
  const corpusByTxHash = new Map(corpus.rows.map((row) => [row.txHash, row]));
  const transactions: E2EL2StressTransaction[] = submitResult.records
    .map((record, index): E2EL2StressTransaction => {
      const corpusRow = corpusByTxHash.get(record.txHash)!;
      const successfulSubmit =
        record.statusCode !== null &&
        record.statusCode >= 200 &&
        record.statusCode < 300 &&
        record.error === null;
      const admission = admissionByTxHash.get(record.txHash);
      const acceptedAt =
        admission?.status === "accepted" ? admission.terminalAt : null;
      return {
        index,
        phase: "stress",
        txHash: successfulSubmit ? record.txHash : null,
        senderAddress: corpusRow.senderWalletId,
        destinationAddress: corpusRow.outputOutrefs.join(","),
        selectedInputs: [corpusRow.selectedInputOutref],
        submission: successfulSubmit
          ? {
              status: "submitted",
              submittedAt: new Date(record.submittedAtMs).toISOString(),
              durationMs: record.latencyMs,
            }
          : {
              status: "failed",
              submittedAt: null,
              error:
                record.error ??
                `POST /submit returned ${record.statusCode?.toString() ?? "no_status"}`,
            },
        acceptance:
          admission?.status === "accepted"
            ? {
                status: "accepted",
                ...(acceptedAt === null ? {} : { acceptedAt }),
              }
            : admission?.status === "rejected"
              ? { status: "rejected" }
              : successfulSubmit
                ? { status: "not_observed" }
                : { status: "not_submitted" },
        finality: { status: "not_observed" },
        workerIndex: 0,
        walletSeedSource: corpusRow.senderWalletId,
      };
    })
    .sort((left, right) => left.index - right.index);
  const finishedAtDate = now();
  const finishedAt = finishedAtDate.toISOString();
  const submittedCount = transactions.filter(
    (tx) => tx.submission.status === "submitted",
  ).length;
  const acceptedCount = transactions.filter(
    (tx) => tx.acceptance.status === "accepted",
  ).length;
  const submissionFailedCount = transactions.filter(
    (tx) => tx.submission.status === "failed",
  ).length;
  const rejectedCount = transactions.filter(
    (tx) => tx.acceptance.status === "rejected",
  ).length;
  const metrics = buildStressMetrics({
    requestedCount: corpus.selectedTransactionCount,
    submittedCount,
    acceptedCount,
    observedCommittedCount: 0,
    startedAt,
    submissionFinishedAt,
    finishedAt,
    transactions,
    ...(dbMetricSources === undefined ? {} : { dbSources: dbMetricSources }),
    ...(runtime.fullFinalityDrainProof === undefined
      ? {}
      : { fullFinalityDrainProof: runtime.fullFinalityDrainProof }),
  });
  const baseClassification = classifyOpenLoopRun({
    metrics,
    submission: submitResult.summary,
    calibration,
    placement,
    observer,
  });
  const groundTruth =
    runtime.collectGroundTruthMetrics === undefined
      ? undefined
      : await runtime.collectGroundTruthMetrics({
          windowStart: startedAt,
          windowEnd: finishedAt,
          txHashSample: submittedTxHashes.slice(0, 1_000),
          offeredCount: corpus.selectedTransactionCount,
          calibrationProofRef: enginePaths.noopCalibrationJson,
        });
  const fingerprint =
    groundTruth?.fingerprint ??
    (runtime.collectEnvironmentFingerprint === undefined
      ? undefined
      : await runtime.collectEnvironmentFingerprint({
          calibrationProofRef: enginePaths.noopCalibrationJson,
        }));
  const classification: E2EL2StressClassification = submitRecords.some(
    (record) => record.error?.includes("duplicate_or_mismatched_response"),
  )
    ? "duplicate_submission"
    : engineResult.exitCode !== 0 &&
        JSON.stringify(engineReport).includes("corpus_exhausted")
      ? "corpus_exhausted"
      : baseClassification;
  const summary = parseE2EL2StressSummary({
    schemaVersion: E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION,
    runId: config.runId,
    status: signalWasAborted(signal) ? "interrupted" : "completed",
    ...(signalWasAborted(signal)
      ? { interruptedReason: abortReason(signal) }
      : {}),
    loadModel: config.loadModel,
    workloadProfile: config.workloadProfile,
    corpusShape: config.corpusShape,
    classification,
    rateSemantics: "offered_tps_uncalibrated",
    burstCycleRatePerSecond: null,
    mode: config.mode,
    measurementPolicy: measurementPolicyForConfig(config),
    openLoop: {
      targetRateTps: config.targetRateTps,
      durationMs: config.openLoopDurationMs,
      maxInFlight: config.openLoopMaxInFlight,
      corpus,
      submission: submitResult.summary,
      ...(calibration === undefined ? {} : { calibration }),
      placement,
    },
    requestedCount: corpus.selectedTransactionCount,
    notStartedCount: Math.max(
      0,
      corpus.selectedTransactionCount - transactions.length,
    ),
    submittedCount,
    submissionFailedCount,
    acceptedCount,
    acceptanceNotObservedCount: transactions.filter(
      (tx) => tx.acceptance.status === "not_observed",
    ).length,
    acceptanceTimedOutCount: 0,
    finalityTimedOutCount: 0,
    observedCommittedCount: 0,
    unknownFinalityCount: acceptedCount,
    rejectedCount,
    concurrency: config.openLoopMaxInFlight,
    finalityObserver: {
      mode: "aggregate-window",
      maxConcurrentRequests: 0,
      maxObservedConcurrentRequests: 0,
      observedTransactionCount: transactions.length,
      pollRequestCount: 0,
      batchCount: observer.sampleCount,
      errorCount: observer.errorCount,
    },
    startedAt,
    submissionFinishedAt,
    finishedAt,
    submissionDurationMs: Math.max(
      0,
      submissionFinishedAtDate.getTime() - startedAtDate.getTime(),
    ),
    durationMs: Math.max(0, finishedAtDate.getTime() - startedAtDate.getTime()),
    metrics,
    ...(groundTruth === undefined ? {} : { groundTruth }),
    ...(fingerprint === undefined ? {} : { fingerprint }),
    latencyMs: {
      submitP50: submitResult.summary.scheduleSlipMs.p50,
      submitP95: submitResult.summary.scheduleSlipMs.p95,
      acceptanceP50: 0,
      acceptanceP95: 0,
      commitP50: 0,
      commitP95: 0,
    },
    artifactPaths: {
      configJson: configJsonPath,
      eventsNdjson: eventsNdjsonPath,
      summaryJson: summaryJsonPath,
      summaryMarkdown: summaryMarkdownPath,
      engineReportJson: enginePaths.engineReportJson,
      engineEventsNdjson: enginePaths.engineEventsNdjson,
      submitRecordsNdjson: enginePaths.submitRecordsNdjson,
      noOpCalibrationJson: enginePaths.noopCalibrationJson,
    },
    transactions,
  });
  await writeFile(summaryJsonPath, `${formatJson(summary)}\n`, "utf8");
  await writeFile(summaryMarkdownPath, renderStressSummaryMarkdown(summary), {
    encoding: "utf8",
  });
  await appendEvent(eventsNdjsonPath, {
    event: "stress_finished",
    at: finishedAt,
    summaryJsonPath,
    summaryMarkdownPath,
    submittedCount,
    acceptedCount,
    rejectedCount,
    submissionFailedCount,
    classification,
  });
  return {
    summary,
    configJsonPath,
    eventsNdjsonPath,
    summaryJsonPath,
    summaryMarkdownPath,
  };
};
