import "vitest";
import "../scripts/verify-phase2-benchmark-report.mjs";
import "./phase2-benchmark-report.stage-breport.js";

import { describe, expect, it } from "vitest";

import {
  verifyPhase2BenchmarkReports,
  verifyStageBReport,
} from "../scripts/verify-phase2-benchmark-report.mjs";
import {
  chunkAbReports,
  nodeImageId,
  otherNodeImageId,
  scriptHeavyReport,
  setCorpusRowCount,
  stageBReport,
  writeBehindAbReports,
} from "./phase2-benchmark-report.stage-breport.js";

describe("Phase 2 benchmark report gate", () => {
  it("accepts an exact Stage B report and rejects loss or topology drift", () => {
    expect(verifyStageBReport(stageBReport())).toBeDefined();
    expect(() =>
      verifyStageBReport(stageBReport({ lostTransactions: 1 })),
    ).toThrow(/lostTransactions/u);
    expect(() =>
      verifyStageBReport(stageBReport({ wholeSystemPinnedEightCore: false })),
    ).toThrow(/wholeSystemPinnedEightCore/u);
    expect(() =>
      verifyStageBReport(stageBReport({ reuseDatabases: true })),
    ).toThrow(/reuseDatabases/u);
    expect(() =>
      verifyStageBReport(stageBReport({ warmupIterations: 0 })),
    ).toThrow(/warmupIterations/u);
    expect(() =>
      verifyStageBReport(
        stageBReport({ disableTxDeltaWriteBehindDiagnostic: true }),
      ),
    ).toThrow(/disableTxDeltaWriteBehindDiagnostic/u);
    expect(() =>
      verifyStageBReport(stageBReport({ nodeVersion: "v22.21.0" })),
    ).toThrow(/nodeVersion/u);
    expect(() =>
      verifyStageBReport(stageBReport({ expectedNodeImageId: undefined })),
    ).toThrow(/expectedNodeImageId/u);
    expect(() =>
      verifyStageBReport(stageBReport({ nodeImageId: otherNodeImageId })),
    ).toThrow(/nodeImageId/u);
    const wrongIds = stageBReport();
    wrongIds.replicas[0]!.missingExpectedTxIds = 1;
    expect(() => verifyStageBReport(wrongIds)).toThrow(/missingExpectedTxIds/u);
  });

  it("requires three matched write-behind control and candidate runs", () => {
    const { controls, candidates } = writeBehindAbReports();
    expect(
      verifyPhase2BenchmarkReports("write-behind-ab", [
        ...controls,
        ...candidates,
      ]),
    ).toMatchObject({
      experimentId: "wab_20260714t110000z",
      controlMedian: 10_200,
      candidateMedian: 10_600,
    });
    expect(() =>
      verifyPhase2BenchmarkReports("write-behind-ab", [
        ...controls,
        ...candidates.slice(0, 2),
      ]),
    ).toThrow(/three control reports/u);

    const mixed = writeBehindAbReports();
    for (const replica of mixed.candidates[1]!.replicas) {
      replica.database = String(replica.database).replace(
        "wab_20260714t110000z",
        "wab_20260714t110001z",
      );
    }
    expect(() =>
      verifyPhase2BenchmarkReports("write-behind-ab", [
        ...mixed.controls,
        ...mixed.candidates,
      ]),
    ).toThrow(/experiment identity/u);

    const stale = writeBehindAbReports();
    stale.candidates[2]!.generatedAtIso = "2026-07-15T11:00:01.000Z";
    expect(() =>
      verifyPhase2BenchmarkReports("write-behind-ab", [
        ...stale.controls,
        ...stale.candidates,
      ]),
    ).toThrow(/within 24 hours/u);

    const nonCanonical = writeBehindAbReports();
    nonCanonical.controls[0]!.generatedAtIso = "2026-07-14 11:00:01Z";
    expect(() =>
      verifyPhase2BenchmarkReports("write-behind-ab", [
        ...nonCanonical.controls,
        ...nonCanonical.candidates,
      ]),
    ).toThrow(/canonical UTC timestamp/u);
  });

  it("accepts only a bound, interleaved six-report chunk A/B experiment", () => {
    expect(
      verifyPhase2BenchmarkReports("chunk-ab", chunkAbReports()),
    ).toMatchObject({
      experimentId: "cab_20260714t120000z",
      chunk64Median: 10_300,
      chunk128Median: 10_700,
      productionDefaultChangeAuthorized: false,
      requiredDefaultChangeGate: "separate chunk-128 script-heavy gate",
    });
    expect(() =>
      verifyPhase2BenchmarkReports("chunk-ab", chunkAbReports().slice(0, 5)),
    ).toThrow(/exactly six reports/u);

    const wrongOrder = chunkAbReports();
    [wrongOrder[1], wrongOrder[2]] = [wrongOrder[2]!, wrongOrder[1]!];
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", wrongOrder)).toThrow(
      /reports\[1\]\.chunkSize/u,
    );

    const nonMonotone = chunkAbReports();
    nonMonotone[2]!.generatedAtIso = nonMonotone[1]!.generatedAtIso;
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", nonMonotone)).toThrow(
      /strict generatedAtIso order/u,
    );

    const stale = chunkAbReports();
    stale[5]!.generatedAtIso = "2026-07-15T12:00:01.000Z";
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", stale)).toThrow(
      /within 24 hours/u,
    );

    const mixedRun = chunkAbReports();
    for (const replica of mixedRun[3]!.replicas) {
      replica.database = String(replica.database).replace(
        "cab_20260714t120000z",
        "cab_20260714t120001z",
      );
    }
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", mixedRun)).toThrow(
      /experiment identity/u,
    );

    const malformedIdentity = chunkAbReports();
    malformedIdentity[0]!.replicas[0]!.database = "replica_0";
    expect(() =>
      verifyPhase2BenchmarkReports("chunk-ab", malformedIdentity),
    ).toThrow(/must identify cab_/u);

    const mismatchedPair = chunkAbReports();
    mismatchedPair[0]!.replicas[1]!.database =
      "midgard_phase2_bench_cab_20260714t120000z_chunk64_1_c";
    expect(() =>
      verifyPhase2BenchmarkReports("chunk-ab", mismatchedPair),
    ).toThrow(/replicas\[1\]\.database/u);
  });

  it("fails every chunk A/B rate and median threshold closed", () => {
    const slowReport = chunkAbReports();
    slowReport[0]!.acceptedTps = 9_999;
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", slowReport)).toThrow(
      /acceptedTps/u,
    );

    const slowReplica = chunkAbReports();
    slowReplica[0]!.replicas[1]!.acceptedTps = 9_999;
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", slowReplica)).toThrow(
      /replicas\[1\]\.acceptedTps/u,
    );

    expect(() =>
      verifyPhase2BenchmarkReports(
        "chunk-ab",
        chunkAbReports({
          chunk64Tps: [10_000, 10_100, 10_200],
          chunk128Tps: [10_400, 10_499, 10_500],
        }),
      ),
    ).toThrow(/chunk-128 median acceptedTps must be >= 10500/u);

    expect(() =>
      verifyPhase2BenchmarkReports(
        "chunk-ab",
        chunkAbReports({
          chunk64Tps: [10_100, 10_200, 10_300],
          chunk128Tps: [10_500, 10_500, 10_600],
        }),
      ),
    ).toThrow(/3% improvement/u);
  });

  it("rejects immutable Node image drift inside a Stage B experiment", () => {
    const reports = chunkAbReports();
    const drifted = reports[4]!;
    drifted.expectedNodeImageId = otherNodeImageId;
    drifted.nodeImageId = otherNodeImageId;
    drifted.containerIdentity = {
      ...drifted.containerIdentity,
      imageId: otherNodeImageId,
    };
    expect(verifyStageBReport(drifted)).toBeDefined();
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", reports)).toThrow(
      /expectedNodeImageId/u,
    );
  });

  it.each([
    ["corpus identity", { corpusSha256: "bb".repeat(32) }, /corpusSha256/u],
    ["corpus path", { corpusPath: "/workspace/other.ndjson" }, /corpusPath/u],
    ["corpus rows", { corpusRowCount: 25_601 }, /corpusRowCount/u],
    ["CPU topology", { cpuModel: "different CPU" }, /cpuModel/u],
    [
      "CPU affinity",
      { affinityLogicalCpuIds: [1, 0, 2, 3, 4, 5, 6, 7] },
      /affinityLogicalCpuIds/u,
    ],
    ["batch size", { batchSize: 4_096 }, /batchSize/u],
    ["drain loops", { drainLoops: 3 }, /drainLoops/u],
    ["worker count", { poolSize: 5 }, /poolSize/u],
    ["warmups", { warmupIterations: 1 }, /warmupIterations/u],
    [
      "write-behind batch",
      { writeBehindMaxBatch: 2_048 },
      /writeBehindMaxBatch/u,
    ],
    ["declared floor", { minimumAcceptedTps: 10_500 }, /minimumAcceptedTps/u],
    ["database reuse", { reuseDatabases: true }, /reuseDatabases/u],
    [
      "delta diagnostic",
      { disableTxDeltaWriteBehindDiagnostic: true },
      /disableTxDeltaWriteBehindDiagnostic/u,
    ],
  ])("rejects chunk A/B %s drift", (_label, override, expected) => {
    const reports = chunkAbReports();
    if ("corpusRowCount" in override) {
      setCorpusRowCount(reports[4]!, Number(override.corpusRowCount));
    } else {
      Object.assign(reports[4]!, override);
    }
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", reports)).toThrow(
      expected,
    );
  });

  it("rejects malformed chunk A/B corpus bindings", () => {
    const badHash = chunkAbReports();
    for (const report of badHash) report.corpusSha256 = "AA".repeat(32);
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", badHash)).toThrow(
      /exact lowercase SHA-256/u,
    );

    const missingPath = chunkAbReports();
    for (const report of missingPath) report.corpusPath = "";
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", missingPath)).toThrow(
      /corpusPath/u,
    );

    const unsafeRows = chunkAbReports();
    for (const report of unsafeRows) {
      setCorpusRowCount(report, Number.MAX_SAFE_INTEGER + 1);
    }
    expect(() => verifyPhase2BenchmarkReports("chunk-ab", unsafeRows)).toThrow(
      /positive safe integer/u,
    );
  });

  it("enforces production-default and five-minute gates", () => {
    const productionDefault = stageBReport({ chunkSize: 64 });
    expect(
      verifyPhase2BenchmarkReports("production-default", [productionDefault]),
    ).toBeDefined();

    const aggregatePassWithSlowReplica = stageBReport({
      chunkSize: 64,
      acceptedTps: 10_100,
    });
    const aggregateDurationMs = aggregatePassWithSlowReplica.durationMs;
    const slowReplica = aggregatePassWithSlowReplica.replicas[0]!;
    const fastReplica = aggregatePassWithSlowReplica.replicas[1]!;
    slowReplica.acceptedTps = 9_900;
    slowReplica.durationMs =
      (Number(slowReplica.accepted) / Number(slowReplica.acceptedTps)) * 1_000;
    fastReplica.durationMs =
      aggregateDurationMs - Number(slowReplica.durationMs);
    fastReplica.acceptedTps =
      Number(fastReplica.accepted) / (Number(fastReplica.durationMs) / 1_000);
    expect(() =>
      verifyPhase2BenchmarkReports("production-default", [
        aggregatePassWithSlowReplica,
      ]),
    ).toThrow(/replicas\[0\]\.acceptedTps/u);

    expect(() =>
      verifyPhase2BenchmarkReports("full", [
        stageBReport({ chunkSize: 64, shortAssert: false }),
      ]),
    ).toThrow(/exact declared corpus/u);
    const fullCorpus = {
      sha256: "bb".repeat(32),
      rowCount: 3_800_000,
    };
    const fullReplicaTps = fullCorpus.rowCount / 300;
    const fullReport = () =>
      stageBReport({
        chunkSize: 64,
        shortAssert: false,
        corpusSha256: fullCorpus.sha256,
        corpusRowCount: fullCorpus.rowCount,
        expectedAccepted: fullCorpus.rowCount * 2,
        accepted: fullCorpus.rowCount * 2,
        acceptedTps: fullReplicaTps,
        durationMs: 600_000,
      });
    expect(
      verifyPhase2BenchmarkReports("full", [fullReport()], {
        expectedFullCorpus: fullCorpus,
      }),
    ).toBeDefined();
    // Follower deposit ingestion neither reloads nor delta-applies the cache.
    for (const field of [
      "ledgerCacheFullReloads",
      "ledgerCacheDeltaApplies",
    ] as const) {
      const cacheTouching = fullReport();
      cacheTouching.replicas[0]![field] = 1;
      expect(() =>
        verifyPhase2BenchmarkReports("full", [cacheTouching], {
          expectedFullCorpus: fullCorpus,
        }),
      ).toThrow(new RegExp(field, "u"));
    }
    const splitDuration = stageBReport({
      chunkSize: 64,
      shortAssert: false,
      corpusSha256: fullCorpus.sha256,
      corpusRowCount: fullCorpus.rowCount,
      expectedAccepted: fullCorpus.rowCount * 2,
      accepted: fullCorpus.rowCount * 2,
      acceptedTps: fullCorpus.rowCount / 150,
      durationMs: 300_000,
    });
    expect(() =>
      verifyPhase2BenchmarkReports("full", [splitDuration], {
        expectedFullCorpus: fullCorpus,
      }),
    ).toThrow(/durationMs/u);

    const undersizedCorpus = {
      sha256: "cc".repeat(32),
      rowCount: 3_063_808,
    };
    expect(() =>
      verifyPhase2BenchmarkReports(
        "full",
        [
          stageBReport({
            chunkSize: 64,
            shortAssert: false,
            corpusSha256: undersizedCorpus.sha256,
            corpusRowCount: undersizedCorpus.rowCount,
          }),
        ],
        { expectedFullCorpus: undersizedCorpus },
      ),
    ).toThrow(/3,780,000 rows/u);

    const paddedDuration = stageBReport({
      chunkSize: 64,
      shortAssert: false,
      corpusSha256: fullCorpus.sha256,
      corpusRowCount: fullCorpus.rowCount,
      expectedAccepted: fullCorpus.rowCount * 2,
      accepted: fullCorpus.rowCount * 2,
      acceptedTps: fullReplicaTps,
      durationMs: 800_000,
    });
    expect(() =>
      verifyPhase2BenchmarkReports("full", [paddedDuration], {
        expectedFullCorpus: fullCorpus,
      }),
    ).toThrow(/acceptedTps must equal measured/u);

    const truncatedDepositWindow = stageBReport({
      chunkSize: 64,
      shortAssert: false,
      corpusSha256: fullCorpus.sha256,
      corpusRowCount: fullCorpus.rowCount,
      expectedAccepted: fullCorpus.rowCount * 2,
      accepted: fullCorpus.rowCount * 2,
      acceptedTps: fullReplicaTps,
      durationMs: 600_000,
    });
    truncatedDepositWindow.replicas[0]!.depositIngestionActiveDurationMs = 1;
    truncatedDepositWindow.replicas[0]!.depositIngestions = 1;
    truncatedDepositWindow.replicas[0]!.mempoolLedgerRows =
      truncatedDepositWindow.expectedLedgerRows + 1;
    expect(() =>
      verifyPhase2BenchmarkReports("full", [truncatedDepositWindow], {
        expectedFullCorpus: fullCorpus,
      }),
    ).toThrow(/depositIngestionActiveDurationMs/u);
  });

  it("authorizes chunk 128 only from bound chunk A/B and candidate script evidence", () => {
    const candidate = scriptHeavyReport();
    expect(
      verifyPhase2BenchmarkReports("script-heavy-chunk128", [candidate]),
    ).not.toHaveProperty("productionDefaultChangeAuthorized");
    expect(
      verifyPhase2BenchmarkReports("authorize-chunk128-default", [
        ...chunkAbReports(),
        candidate,
      ]),
    ).toMatchObject({
      productionDefaultChangeAuthorized: true,
      priorProductionDefaultChunkSize: 64,
      authorizedProductionDefaultChunkSize: 128,
      chunkAb: {
        experimentId: "cab_20260714t120000z",
        productionDefaultChangeAuthorized: false,
      },
      scriptHeavy: { chunkSize: 128 },
    });
    expect(() =>
      verifyPhase2BenchmarkReports("authorize-chunk128-default", [candidate]),
    ).toThrow(/six chunk-ab reports/u);

    const differentImageCandidate = scriptHeavyReport({
      expectedNodeImageId: otherNodeImageId,
      nodeImageId: otherNodeImageId,
      containerIdentity: {
        ...candidate.containerIdentity,
        imageId: otherNodeImageId,
      },
    });
    expect(
      verifyPhase2BenchmarkReports("script-heavy-chunk128", [
        differentImageCandidate,
      ]),
    ).toBeDefined();
    expect(() =>
      verifyPhase2BenchmarkReports("authorize-chunk128-default", [
        ...chunkAbReports(),
        differentImageCandidate,
      ]),
    ).toThrow(/expectedNodeImageId/u);
  });

  it.each([
    [
      "experiment identity",
      { chunkAbExperimentId: "cab_20260714t120001z" },
      /chunkAbExperimentId/u,
    ],
    ["corpus", { corpusSha256: "bb".repeat(32) }, /corpusSha256/u],
    ["topology", { cpuModel: "different CPU" }, /cpuModel/u],
    ["runtime", { expectedNodeImage: "node:23.0.0" }, /expectedNodeImage/u],
    [
      "state patch parity",
      { statePatchMatchesInline: false },
      /statePatchMatchesInline/u,
    ],
    [
      "Plutus language",
      { everyTransactionIsPlutusV3: false },
      /everyTransactionIsPlutusV3/u,
    ],
    ["candidate chunk", { chunkSize: 64 }, /chunkSize/u],
    [
      "candidate duration",
      { durationMsRequested: 299_999 },
      /durationMsRequested/u,
    ],
  ])(
    "rejects chunk-128 authorization with mismatched %s",
    (_label, override, expected) => {
      expect(() =>
        verifyPhase2BenchmarkReports("authorize-chunk128-default", [
          ...chunkAbReports(),
          scriptHeavyReport(override),
        ]),
      ).toThrow(expected);
    },
  );

  it("rejects stale or out-of-order candidate script evidence", () => {
    expect(() =>
      verifyPhase2BenchmarkReports("authorize-chunk128-default", [
        ...chunkAbReports(),
        scriptHeavyReport({ generatedAtIso: "2026-07-14T12:00:06.000Z" }),
      ]),
    ).toThrow(/generated after all six/u);
    expect(() =>
      verifyPhase2BenchmarkReports("authorize-chunk128-default", [
        ...chunkAbReports(),
        scriptHeavyReport({ generatedAtIso: "2026-07-15T12:00:01.000Z" }),
      ]),
    ).toThrow(/within 24 hours/u);
  });

  it("enforces script latency and leak-soak evidence", () => {
    const scriptReport = scriptHeavyReport({
      gateMode: "production_default_chunk64",
      chunkSize: 64,
      chunkAbExperimentId: undefined,
      corpusPath: undefined,
      corpusManifestPath: undefined,
      corpusSha256: undefined,
      corpusRowCount: undefined,
    });
    expect(
      verifyPhase2BenchmarkReports("script-heavy", [scriptReport]),
    ).toBeDefined();
    expect(() =>
      verifyPhase2BenchmarkReports("script-heavy", [
        {
          ...scriptReport,
          eventLoopDelayP99Ms: 50,
        },
      ]),
    ).toThrow(/eventLoopDelayP99Ms/u);

    for (const [field, value] of [
      ["gateMode", undefined],
      ["gateMode", "chunk128_candidate"],
      ["everyTransactionIsPlutusV3", undefined],
      ["everyTransactionIsPlutusV3", false],
      ["statePatchMatchesInline", undefined],
      ["statePatchMatchesInline", false],
      ["expectedNodeImageId", undefined],
      ["expectedNodeImageId", `sha256:${"ef".repeat(32)}`],
      ["nodeImageId", undefined],
      ["nodeImageId", `sha256:${"ef".repeat(32)}`],
      ["nodeImage", "node:22.21.0"],
      ["containerIdentity", undefined],
    ] as const) {
      expect(() =>
        verifyPhase2BenchmarkReports("script-heavy", [
          { ...scriptReport, [field]: value },
        ]),
      ).toThrow();
    }
    expect(() =>
      verifyPhase2BenchmarkReports("script-heavy", [
        {
          ...scriptReport,
          containerIdentity: {
            ...scriptReport.containerIdentity,
            imageId: undefined,
          },
        },
      ]),
    ).toThrow(/containerIdentity\.imageId/u);
    expect(() =>
      verifyPhase2BenchmarkReports("script-heavy", [
        {
          ...scriptReport,
          containerIdentity: {
            ...scriptReport.containerIdentity,
            imageId: `sha256:${"ef".repeat(32)}`,
          },
        },
      ]),
    ).toThrow(/containerIdentity\.imageId/u);

    const rssSamples = Array.from({ length: 1_441 }, (_, index) => ({
      elapsedMs: index * 60_000,
      rssBytes: 1_000_000 + Math.floor((index / 1_440) * 99_000),
      processRssPerWorkerAverageBytes:
        (1_000_000 + Math.floor((index / 1_440) * 99_000)) / 6,
    }));
    const workerMemorySamples = rssSamples.map((sample, sampleIndex) => ({
      elapsedMs: sample.elapsedMs,
      workers: Array.from({ length: 6 }, (_, workerIndex) => {
        const comparableFootprintBytes =
          100_000 + Math.floor((sampleIndex / 1_440) * 9_000);
        return {
          workerIndex,
          threadId: workerIndex + 1,
          usedHeapBytes: comparableFootprintBytes - 20_000,
          externalBytes: 20_000,
          comparableFootprintBytes,
        };
      }),
    }));
    const workerMemoryGrowth = Array.from({ length: 6 }, (_, workerIndex) => ({
      workerIndex,
      baselineThreadId: workerIndex + 1,
      finalThreadId: workerIndex + 1,
      stableIdentity: true,
      baselineComparableFootprintBytes: 100_000,
      finalComparableFootprintBytes: 109_000,
      growthRatio: 0.09,
    }));
    const validLeakReport = {
      leakSoakGateAsserted: true,
      pinnedEightCore: true,
      containerIdentityProved: true,
      nodeVersion: "v22.22.2",
      expectedNodeImage: "node:22.22.2",
      expectedNodeImageId: nodeImageId,
      nodeImage: "node:22.22.2",
      nodeImageId,
      containerIdentity: {
        proved: true,
        image: "node:22.22.2",
        imageId: nodeImageId,
        id: "cd".repeat(32),
      },
      availableParallelism: 8,
      affinityLogicalCpuIds: Array.from({ length: 8 }, (_, index) => index),
      affinityPhysicalCoreIds: Array.from(
        { length: 8 },
        (_, index) => `0:${index.toString()}`,
      ),
      poolSize: 6,
      batchSize: 512,
      chunkSize: 64,
      signatureVerifier: "node",
      targetTps: 2_500,
      steadyStateWarmupMsRequested: 300_000,
      steadyStateWarmupMsObserved: 300_032,
      steadyStateWarmupAccepted: 750_080,
      steadyStateWarmupRejected: 0,
      steadyStateWarmupBatches: 1_465,
      steadyStateWarmupAcceptedTps: 2_500,
      memoryMeasurementExcludesWarmup: true,
      accepted: 216_000_000,
      batches: 421_875,
      rejected: 0,
      verdictMatchesInline: true,
      durationMsRequested: 86_400_000,
      durationMsObserved: 86_400_000,
      acceptedTps: 2_500,
      rssGrowthRatio: 0.099,
      rssBaselineBytes: 1_000_000,
      rssFinalBytes: 1_099_000,
      rssSamples,
      workerMemorySamples,
      workerMemoryGrowth,
      everyWorkerMemoryGrowthUnderTenPercent: true,
    };
    expect(
      verifyPhase2BenchmarkReports("leak-soak", [validLeakReport]),
    ).toBeDefined();
    expect(() =>
      verifyPhase2BenchmarkReports("leak-soak", [
        {
          ...validLeakReport,
          accepted: 512,
          batches: 1,
          acceptedTps: 2_500,
        },
      ]),
    ).toThrow(/acceptedTps must equal measured/u);
    expect(() =>
      verifyPhase2BenchmarkReports("leak-soak", [
        {
          ...validLeakReport,
          accepted: 215_999_488,
          batches: 421_874,
          acceptedTps: 215_999_488 / 86_400,
        },
      ]),
    ).toThrow(/acceptedTps/u);
    expect(() =>
      verifyPhase2BenchmarkReports("leak-soak", [
        { ...validLeakReport, steadyStateWarmupMsRequested: 299_999 },
      ]),
    ).toThrow(/steadyStateWarmupMsRequested/u);
    expect(() =>
      verifyPhase2BenchmarkReports("leak-soak", [
        { ...validLeakReport, steadyStateWarmupAccepted: 750_079 },
      ]),
    ).toThrow(/steadyStateWarmupAccepted/u);
    expect(() =>
      verifyPhase2BenchmarkReports("leak-soak", [
        { ...validLeakReport, steadyStateWarmupAcceptedTps: 2_500.5 },
      ]),
    ).toThrow(/steadyStateWarmupAcceptedTps must equal measured/u);
    expect(() =>
      verifyPhase2BenchmarkReports("leak-soak", [
        { ...validLeakReport, durationMsRequested: 86_399_999 },
      ]),
    ).toThrow(/durationMsRequested/u);
    expect(() =>
      verifyPhase2BenchmarkReports("leak-soak", [
        { ...validLeakReport, memoryMeasurementExcludesWarmup: false },
      ]),
    ).toThrow(/memoryMeasurementExcludesWarmup/u);
    const replacedWorkerSamples = workerMemorySamples.map((sample, index) => ({
      ...sample,
      workers:
        index === workerMemorySamples.length - 1
          ? sample.workers.map((worker) =>
              worker.workerIndex === 0 ? { ...worker, threadId: 99 } : worker,
            )
          : sample.workers,
    }));
    expect(() =>
      verifyPhase2BenchmarkReports("leak-soak", [
        { ...validLeakReport, workerMemorySamples: replacedWorkerSamples },
      ]),
    ).toThrow(/stable threadId/u);
    const growingWorkers = workerMemoryGrowth.map((worker) =>
      worker.workerIndex === 0
        ? {
            ...worker,
            finalComparableFootprintBytes: 120_000,
            growthRatio: 0.2,
          }
        : worker,
    );
    const growingWorkerSamples = workerMemorySamples.map((sample, index) => ({
      ...sample,
      workers:
        index === workerMemorySamples.length - 1
          ? sample.workers.map((worker) =>
              worker.workerIndex === 0
                ? {
                    ...worker,
                    usedHeapBytes: 100_000,
                    comparableFootprintBytes: 120_000,
                  }
                : worker,
            )
          : sample.workers,
    }));
    expect(() =>
      verifyPhase2BenchmarkReports("leak-soak", [
        {
          ...validLeakReport,
          workerMemorySamples: growingWorkerSamples,
          workerMemoryGrowth: growingWorkers,
        },
      ]),
    ).toThrow(/growthRatio/u);
  });
});
