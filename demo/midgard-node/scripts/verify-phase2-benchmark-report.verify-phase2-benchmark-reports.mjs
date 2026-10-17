import {
  parseChunkAbRunStartedAt,
  verifyChunkAbIdentityAndOrder,
  verifyInterleavedExperimentOrder,
  verifyMatchingChunkAbExperiment,
  verifyMatchingExperiment,
} from "./verify-phase2-benchmark-report.verify-chunk-ab-identity-and-order.mjs";
import { verifyScriptHeavyReport } from "./verify-phase2-benchmark-report.verify-script-heavy-report.mjs";
import {
  approximatelyEqual,
  atLeast,
  atMost,
  below,
  equal,
  equalJson,
  fail,
  finite,
  FULL_GATE_CORPUS_CAPACITY_TPS,
  FULL_GATE_FINAL_FLUSH_ALLOWANCE_MS,
  FULL_GATE_MINIMUM_CORPUS_ROWS,
  FULL_GATE_REPLICA_DURATION_MS,
  median,
  nonEmptyArray,
  nonEmptyString,
  object,
  positiveSafeInteger,
  verifyEightPhysicalCores,
  verifyExactNodeContainerImage,
  verifyStageBReport,
} from "./verify-phase2-benchmark-report.verify-stage-breport.mjs";

const verifyChunk128DefaultAuthorization = (reportValues) => {
  if (reportValues.length !== 7) {
    fail(
      "authorize-chunk128-default requires the six chunk-ab reports followed by one chunk-128 script-heavy report",
    );
  }
  const chunkAb = verifyPhase2BenchmarkReports(
    "chunk-ab",
    reportValues.slice(0, 6),
  );
  const scriptHeavy = verifyPhase2BenchmarkReports(
    "script-heavy-chunk128",
    reportValues.slice(6),
  );
  equal(
    scriptHeavy.chunkAbExperimentId,
    chunkAb.experimentId,
    "script-heavy chunkAbExperimentId",
  );
  for (const field of ["corpusPath", "corpusSha256", "corpusRowCount"]) {
    equal(
      scriptHeavy[field],
      chunkAb.reports[0][field],
      `script-heavy ${field}`,
    );
  }
  for (const field of [
    "affinityLogicalCpuIds",
    "affinityPhysicalCoreIds",
    "cpuModel",
  ]) {
    equalJson(
      scriptHeavy[field],
      chunkAb.reports[0][field],
      `script-heavy ${field}`,
    );
  }
  equal(
    scriptHeavy.availableParallelism,
    chunkAb.reports[0].availableParallelism,
    "script-heavy availableParallelism",
  );
  equal(
    scriptHeavy.nodeVersion,
    chunkAb.reports[0].nodeVersion,
    "script-heavy nodeVersion",
  );
  for (const field of [
    "expectedNodeImage",
    "expectedNodeImageId",
    "nodeImage",
    "nodeImageId",
  ]) {
    equal(
      scriptHeavy[field],
      chunkAb.reports[0][field],
      `script-heavy ${field}`,
    );
  }
  equal(
    scriptHeavy.poolSize,
    chunkAb.reports[0].poolSize,
    "script-heavy poolSize",
  );
  equal(
    scriptHeavy.signatureVerifier,
    chunkAb.reports[0].signatureVerifier,
    "script-heavy signatureVerifier",
  );

  const candidateGeneratedAt = Date.parse(
    nonEmptyString(scriptHeavy.generatedAtIso, "script-heavy generatedAtIso"),
  );
  if (
    !Number.isFinite(candidateGeneratedAt) ||
    new Date(candidateGeneratedAt).toISOString() !== scriptHeavy.generatedAtIso
  ) {
    fail("script-heavy generatedAtIso must be a canonical UTC timestamp");
  }
  const lastChunkAbGeneratedAt = Date.parse(
    chunkAb.reports[chunkAb.reports.length - 1].generatedAtIso,
  );
  if (candidateGeneratedAt <= lastChunkAbGeneratedAt) {
    fail(
      "script-heavy report must be generated after all six chunk-ab reports",
    );
  }
  const identityMatch = /^cab_(\d{8})t(\d{6})z$/u.exec(chunkAb.experimentId);
  if (identityMatch === null) {
    fail("chunk-ab experiment identity is malformed");
  }
  const experimentStartedAt = parseChunkAbRunStartedAt(
    identityMatch[1],
    identityMatch[2],
    "chunk-ab experiment identity",
  );
  if (candidateGeneratedAt - experimentStartedAt > 86_400_000) {
    fail(
      "script-heavy generatedAtIso must fall within 24 hours after its chunk-ab run identity",
    );
  }
  return {
    chunkAb,
    scriptHeavy,
    productionDefaultChangeAuthorized: true,
    priorProductionDefaultChunkSize: 64,
    authorizedProductionDefaultChunkSize: 128,
  };
};

export const verifyPhase2BenchmarkReports = (
  mode,
  reportValues,
  { expectedFullCorpus } = {},
) => {
  if (!Array.isArray(reportValues)) fail("reports must be an array");
  switch (mode) {
    case "rehearsal": {
      if (reportValues.length < 1)
        fail("rehearsal requires at least one report");
      return reportValues.map((report) =>
        verifyStageBReport(report, {
          minimumAcceptedTps: 10_500,
          shortAssert: true,
        }),
      );
    }
    case "write-behind-ab": {
      if (reportValues.length !== 6) {
        fail(
          "write-behind-ab requires three control reports followed by three candidate reports",
        );
      }
      const controls = reportValues.slice(0, 3).map((report) =>
        verifyStageBReport(report, {
          minimumAcceptedTps: 10_000,
          shortAssert: true,
          writeBehindMaxBatch: 1_000,
        }),
      );
      const candidates = reportValues.slice(3).map((report) =>
        verifyStageBReport(report, {
          minimumAcceptedTps: 10_000,
          shortAssert: true,
          writeBehindMaxBatch: 2_048,
        }),
      );
      verifyMatchingExperiment([...controls, ...candidates]);
      const experimentId = verifyInterleavedExperimentOrder(
        controls,
        candidates,
      );
      const controlMedian = median(
        controls.map((report) => report.acceptedTps),
      );
      const candidateMedian = median(
        candidates.map((report) => report.acceptedTps),
      );
      atLeast(candidateMedian, 10_500, "candidate median acceptedTps");
      atLeast(
        candidateMedian / controlMedian - 1,
        0.03,
        "candidate median throughput improvement",
      );
      return {
        experimentId,
        controls,
        candidates,
        controlMedian,
        candidateMedian,
      };
    }
    case "chunk-ab": {
      if (reportValues.length !== 6) {
        fail(
          "chunk-ab requires exactly six reports in 64,128,64,128,64,128 order",
        );
      }
      const expectedChunks = [64, 128, 64, 128, 64, 128];
      reportValues.forEach((report, index) => {
        equal(
          object(report, `reports[${index}]`).chunkSize,
          expectedChunks[index],
          `reports[${index}].chunkSize`,
        );
      });
      const reports = reportValues.map((report, index) =>
        verifyStageBReport(report, {
          minimumAcceptedTps: 10_000,
          minimumReplicaAcceptedTps: 10_000,
          shortAssert: true,
          chunkSize: expectedChunks[index],
          writeBehindMaxBatch: 1_000,
        }),
      );
      nonEmptyString(reports[0].corpusPath, "reports[0].corpusPath");
      const corpusSha256 = nonEmptyString(
        reports[0].corpusSha256,
        "reports[0].corpusSha256",
      );
      if (!/^[0-9a-f]{64}$/u.test(corpusSha256)) {
        fail("reports[0].corpusSha256 must be an exact lowercase SHA-256");
      }
      nonEmptyString(reports[0].cpuModel, "reports[0].cpuModel");
      reports.forEach((report, index) => {
        equal(
          report.minimumAcceptedTps,
          10_000,
          `reports[${index}].minimumAcceptedTps`,
        );
      });
      positiveSafeInteger(
        reports[0].corpusRowCount,
        "reports[0].corpusRowCount",
      );
      positiveSafeInteger(
        reports[0].expectedLedgerRows,
        "reports[0].expectedLedgerRows",
      );
      const experimentId = verifyChunkAbIdentityAndOrder(reports);
      verifyMatchingChunkAbExperiment(reports);
      const chunk64Reports = reports.filter(
        (report) => report.chunkSize === 64,
      );
      const chunk128Reports = reports.filter(
        (report) => report.chunkSize === 128,
      );
      const chunk64Median = median(
        chunk64Reports.map((report) => report.acceptedTps),
      );
      const chunk128Median = median(
        chunk128Reports.map((report) => report.acceptedTps),
      );
      atLeast(chunk128Median, 10_500, "chunk-128 median acceptedTps");
      atLeast(
        chunk128Median,
        chunk64Median * 1.03,
        "chunk-128 median acceptedTps for 3% improvement",
      );
      return {
        experimentId,
        reports,
        chunk64Median,
        chunk128Median,
        productionDefaultChangeAuthorized: false,
        requiredDefaultChangeGate: "separate chunk-128 script-heavy gate",
      };
    }
    case "production-default": {
      if (reportValues.length !== 1)
        fail("production-default requires exactly one report");
      return verifyStageBReport(reportValues[0], {
        minimumAcceptedTps: 10_000,
        shortAssert: true,
        chunkSize: 64,
        writeBehindMaxBatch: 1_000,
        minimumReplicaAcceptedTps: 10_000,
      });
    }
    case "full": {
      if (reportValues.length !== 1) fail("full requires exactly one report");
      if (
        expectedFullCorpus === undefined ||
        typeof expectedFullCorpus.sha256 !== "string" ||
        !/^[0-9a-f]{64}$/u.test(expectedFullCorpus.sha256) ||
        !Number.isSafeInteger(expectedFullCorpus.rowCount) ||
        expectedFullCorpus.rowCount < FULL_GATE_MINIMUM_CORPUS_ROWS
      ) {
        fail(
          `full requires an exact declared corpus SHA-256 and at least ${FULL_GATE_MINIMUM_CORPUS_ROWS.toLocaleString("en-US")} rows per continuous replica (${FULL_GATE_CORPUS_CAPACITY_TPS.toLocaleString("en-US")} tx/s capacity for ${String(FULL_GATE_REPLICA_DURATION_MS / 1_000)} seconds)`,
        );
      }
      const report = verifyStageBReport(reportValues[0], {
        minimumAcceptedTps: 10_000,
        minimumDurationMs: 600_000,
        shortAssert: false,
        chunkSize: 64,
        writeBehindMaxBatch: 1_000,
        minimumReplicaAcceptedTps: 10_000,
        minimumReplicaDurationMs: 300_000,
      });
      equal(report.corpusSha256, expectedFullCorpus.sha256, "corpusSha256");
      equal(
        report.corpusRowCount,
        expectedFullCorpus.rowCount,
        "corpusRowCount",
      );
      report.replicas.forEach((replica, index) => {
        const label = `replicas[${index}]`;
        equal(
          replica.depositIngestionIntervalMs,
          5_000,
          `${label}.depositIngestionIntervalMs`,
        );
        const activeDurationMs = finite(
          replica.depositIngestionActiveDurationMs,
          `${label}.depositIngestionActiveDurationMs`,
        );
        atMost(
          activeDurationMs,
          replica.durationMs,
          `${label}.depositIngestionActiveDurationMs`,
        );
        atLeast(
          activeDurationMs,
          replica.durationMs - FULL_GATE_FINAL_FLUSH_ALLOWANCE_MS,
          `${label}.depositIngestionActiveDurationMs`,
        );
        atLeast(
          replica.writeBehindFinalFlushMs,
          0,
          `${label}.writeBehindFinalFlushMs`,
        );
        atMost(
          replica.writeBehindFinalFlushMs,
          FULL_GATE_FINAL_FLUSH_ALLOWANCE_MS,
          `${label}.writeBehindFinalFlushMs`,
        );
        const minimumDepositIngestions = Math.max(
          1,
          Math.floor(
            (replica.durationMs - FULL_GATE_FINAL_FLUSH_ALLOWANCE_MS) / 5_000,
          ) - 1,
        );
        atLeast(
          replica.depositIngestions,
          minimumDepositIngestions,
          `${label}.depositIngestions`,
        );
        equal(
          replica.ledgerCacheDeltaApplies,
          0,
          `${label}.ledgerCacheDeltaApplies`,
        );
        equal(
          replica.ledgerCacheFullReloads,
          0,
          `${label}.ledgerCacheFullReloads`,
        );
        atLeast(
          replica.worstDepositWindowThroughputRatio,
          0.95,
          `${label}.worstDepositWindowThroughputRatio`,
        );
      });
      return report;
    }
    case "script-heavy": {
      if (reportValues.length !== 1)
        fail("script-heavy requires exactly one report");
      return verifyScriptHeavyReport(reportValues[0], { chunkSize: 64 });
    }
    case "script-heavy-chunk128": {
      if (reportValues.length !== 1) {
        fail("script-heavy-chunk128 requires exactly one report");
      }
      return verifyScriptHeavyReport(reportValues[0], {
        chunkSize: 128,
        candidate: true,
      });
    }
    case "authorize-chunk128-default": {
      return verifyChunk128DefaultAuthorization(reportValues);
    }
    case "leak-soak": {
      if (reportValues.length !== 1)
        fail("leak-soak requires exactly one report");
      const report = object(reportValues[0], "report");
      equal(report.leakSoakGateAsserted, true, "leakSoakGateAsserted");
      equal(report.pinnedEightCore, true, "pinnedEightCore");
      equal(report.containerIdentityProved, true, "containerIdentityProved");
      equal(report.nodeVersion, "v22.22.2", "nodeVersion");
      verifyExactNodeContainerImage(report);
      equal(report.availableParallelism, 8, "availableParallelism");
      verifyEightPhysicalCores(report);
      equal(report.poolSize, 6, "poolSize");
      equal(report.batchSize, 512, "batchSize");
      equal(report.chunkSize, 64, "chunkSize");
      equal(report.signatureVerifier, "node", "signatureVerifier");
      equal(report.targetTps, 2_500, "targetTps");
      equal(
        report.steadyStateWarmupMsRequested,
        300_000,
        "steadyStateWarmupMsRequested",
      );
      atLeast(
        report.steadyStateWarmupMsObserved,
        report.steadyStateWarmupMsRequested,
        "steadyStateWarmupMsObserved",
      );
      equal(
        report.memoryMeasurementExcludesWarmup,
        true,
        "memoryMeasurementExcludesWarmup",
      );
      equal(report.steadyStateWarmupRejected, 0, "steadyStateWarmupRejected");
      atLeast(report.steadyStateWarmupAccepted, 1, "steadyStateWarmupAccepted");
      atLeast(report.steadyStateWarmupBatches, 1, "steadyStateWarmupBatches");
      equal(
        report.steadyStateWarmupAccepted,
        report.batchSize * report.steadyStateWarmupBatches,
        "steadyStateWarmupAccepted",
      );
      approximatelyEqual(
        report.steadyStateWarmupAcceptedTps,
        report.steadyStateWarmupAccepted /
          (report.steadyStateWarmupMsObserved / 1_000),
        "steadyStateWarmupAcceptedTps",
      );
      atLeast(
        report.steadyStateWarmupAcceptedTps,
        report.targetTps * 0.999,
        "steadyStateWarmupAcceptedTps",
      );
      equal(report.rejected, 0, "rejected");
      equal(report.verdictMatchesInline, true, "verdictMatchesInline");
      atLeast(report.accepted, 1, "accepted");
      atLeast(report.batches, 1, "batches");
      equal(report.accepted, report.batchSize * report.batches, "accepted");
      equal(report.durationMsRequested, 86_400_000, "durationMsRequested");
      atLeast(
        report.durationMsObserved,
        report.durationMsRequested,
        "durationMsObserved",
      );
      approximatelyEqual(
        report.acceptedTps,
        report.accepted / (report.durationMsObserved / 1_000),
        "acceptedTps",
      );
      atLeast(report.acceptedTps, 2_500, "acceptedTps");
      below(report.rssGrowthRatio, 0.1, "rssGrowthRatio");
      const samples = nonEmptyArray(report.rssSamples, "rssSamples");
      atLeast(
        samples.length,
        Math.floor(report.durationMsObserved / 60_000) + 1,
        "rssSamples.length",
      );
      samples.forEach((sampleValue, index) => {
        const sample = object(sampleValue, `rssSamples[${index}]`);
        atLeast(sample.elapsedMs, 0, `rssSamples[${index}].elapsedMs`);
        atLeast(sample.rssBytes, 1, `rssSamples[${index}].rssBytes`);
        equal(
          sample.processRssPerWorkerAverageBytes,
          sample.rssBytes / report.poolSize,
          `rssSamples[${index}].processRssPerWorkerAverageBytes`,
        );
        if (index > 0) {
          const previous = object(
            samples[index - 1],
            `rssSamples[${index - 1}]`,
          );
          const gap = sample.elapsedMs - previous.elapsedMs;
          if (gap <= 0 || gap > 90_000) {
            fail(
              `rssSamples[${index}].elapsedMs must be monotone with <= 90000ms cadence gap, got ${gap}`,
            );
          }
        }
      });
      const first = object(samples[0], "rssSamples[0]");
      const last = object(samples.at(-1), `rssSamples[${samples.length - 1}]`);
      atMost(first.elapsedMs, 1_000, "rssSamples[0].elapsedMs");
      equal(last.elapsedMs, report.durationMsObserved, "final RSS elapsedMs");
      equal(first.rssBytes, report.rssBaselineBytes, "rssBaselineBytes");
      equal(last.rssBytes, report.rssFinalBytes, "rssFinalBytes");
      equal(
        report.rssGrowthRatio,
        Math.max(0, report.rssFinalBytes - report.rssBaselineBytes) /
          Math.max(1, report.rssBaselineBytes),
        "rssGrowthRatio",
      );
      equal(
        report.everyWorkerMemoryGrowthUnderTenPercent,
        true,
        "everyWorkerMemoryGrowthUnderTenPercent",
      );
      const workerSamples = nonEmptyArray(
        report.workerMemorySamples,
        "workerMemorySamples",
      );
      atLeast(
        workerSamples.length,
        Math.floor(report.durationMsObserved / 60_000) + 1,
        "workerMemorySamples.length",
      );
      const baselineByIndex = new Map();
      workerSamples.forEach((sampleValue, sampleIndex) => {
        const sample = object(
          sampleValue,
          `workerMemorySamples[${sampleIndex}]`,
        );
        const workers = nonEmptyArray(
          sample.workers,
          `workerMemorySamples[${sampleIndex}].workers`,
        );
        equal(
          workers.length,
          report.poolSize,
          `workerMemorySamples[${sampleIndex}].workers.length`,
        );
        const indices = new Set();
        const threads = new Set();
        workers.forEach((workerValue, workerOffset) => {
          const worker = object(
            workerValue,
            `workerMemorySamples[${sampleIndex}].workers[${workerOffset}]`,
          );
          atLeast(worker.workerIndex, 0, "workerIndex");
          atLeast(worker.threadId, 1, "threadId");
          atLeast(worker.usedHeapBytes, 1, "usedHeapBytes");
          atLeast(worker.externalBytes, 0, "externalBytes");
          equal(
            worker.comparableFootprintBytes,
            worker.usedHeapBytes + worker.externalBytes,
            "comparableFootprintBytes",
          );
          indices.add(worker.workerIndex);
          threads.add(worker.threadId);
          if (sampleIndex === 0) {
            baselineByIndex.set(worker.workerIndex, worker);
          } else {
            equal(
              worker.threadId,
              baselineByIndex.get(worker.workerIndex)?.threadId,
              `worker ${worker.workerIndex} stable threadId`,
            );
          }
        });
        equal(indices.size, report.poolSize, "worker index distinct count");
        equal(threads.size, report.poolSize, "worker thread distinct count");
        if (sampleIndex === 0) {
          atMost(sample.elapsedMs, 1_000, "workerMemorySamples[0].elapsedMs");
        } else {
          const previous = object(
            workerSamples[sampleIndex - 1],
            `workerMemorySamples[${sampleIndex - 1}]`,
          );
          const gap = sample.elapsedMs - previous.elapsedMs;
          if (gap <= 0 || gap > 90_000) {
            fail(
              `workerMemorySamples[${sampleIndex}].elapsedMs must be monotone with <= 90000ms cadence gap, got ${gap}`,
            );
          }
        }
      });
      const finalWorkerSample = object(
        workerSamples.at(-1),
        `workerMemorySamples[${workerSamples.length - 1}]`,
      );
      equal(
        finalWorkerSample.elapsedMs,
        report.durationMsObserved,
        "final worker memory elapsedMs",
      );
      const workerGrowth = nonEmptyArray(
        report.workerMemoryGrowth,
        "workerMemoryGrowth",
      );
      equal(workerGrowth.length, report.poolSize, "workerMemoryGrowth.length");
      const finalWorkersByIndex = new Map(
        nonEmptyArray(finalWorkerSample.workers, "final worker samples").map(
          (worker) => [worker.workerIndex, worker],
        ),
      );
      const growthIndices = new Set();
      workerGrowth.forEach((growthValue, index) => {
        const growth = object(growthValue, `workerMemoryGrowth[${index}]`);
        growthIndices.add(growth.workerIndex);
        const baseline = baselineByIndex.get(growth.workerIndex);
        const final = finalWorkersByIndex.get(growth.workerIndex);
        equal(
          growth.baselineThreadId,
          baseline?.threadId,
          `workerMemoryGrowth[${index}].baselineThreadId`,
        );
        equal(
          growth.finalThreadId,
          final?.threadId,
          `workerMemoryGrowth[${index}].finalThreadId`,
        );
        equal(
          growth.baselineComparableFootprintBytes,
          baseline?.comparableFootprintBytes,
          `workerMemoryGrowth[${index}].baselineComparableFootprintBytes`,
        );
        equal(
          growth.finalComparableFootprintBytes,
          final?.comparableFootprintBytes,
          `workerMemoryGrowth[${index}].finalComparableFootprintBytes`,
        );
        equal(
          growth.stableIdentity,
          true,
          `workerMemoryGrowth[${index}].stableIdentity`,
        );
        equal(
          growth.baselineThreadId,
          growth.finalThreadId,
          `workerMemoryGrowth[${index}] thread identity`,
        );
        equal(
          growth.growthRatio,
          Math.max(
            0,
            growth.finalComparableFootprintBytes -
              growth.baselineComparableFootprintBytes,
          ) / Math.max(1, growth.baselineComparableFootprintBytes),
          `workerMemoryGrowth[${index}].growthRatio`,
        );
        below(
          growth.growthRatio,
          0.1,
          `workerMemoryGrowth[${index}].growthRatio`,
        );
      });
      equal(
        growthIndices.size,
        report.poolSize,
        "workerMemoryGrowth worker index distinct count",
      );
      return report;
    }
    default:
      fail(`unknown mode ${JSON.stringify(mode)}`);
  }
};
