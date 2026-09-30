export const nodeImageId = `sha256:${"ab".repeat(32)}`;

export const otherNodeImageId = `sha256:${"ef".repeat(32)}`;

export const stageBReport = (overrides: Record<string, unknown> = {}) => {
  const report = {
    generatedAtIso: "2026-07-10T00:00:00.000Z",
    gateAsserted: true,
    wholeSystemPinnedEightCore: true,
    pinnedEightCore: true,
    nodePinnedEightCore: true,
    availableParallelism: 8,
    affinityLogicalCpuIds: Array.from({ length: 8 }, (_, index) => index),
    affinityPhysicalCoreIds: Array.from(
      { length: 8 },
      (_, index) => `0:${index.toString()}`,
    ),
    cpuModel: "Phase 2 test CPU",
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
    expectedPostgresImage: "postgres:15.15-alpine",
    nodeContainerProved: true,
    postgresImagePinned: true,
    postgresDataIsEphemeral: true,
    connectedToDeclaredPostgresContainer: true,
    reuseDatabases: false,
    replicaCount: 2,
    poolSize: 6,
    signatureVerifier: "node",
    drainLoops: 4,
    batchSize: 2_048,
    chunkSize: 128,
    writeBehindMaxBatch: 1_000,
    shortAssert: true,
    minimumAcceptedTps: 10_000,
    corpusPath: "/workspace/corpus.ndjson",
    corpusSha256: "aa".repeat(32),
    corpusRowCount: 25_600,
    warmupIterations: 2,
    disableTxDeltaWriteBehindDiagnostic: false,
    expectedAccepted: 51_200,
    accepted: 51_200,
    expectedLedgerRows: 25_700,
    lostTransactions: 0,
    acceptedTps: 10_600,
    durationMs: 5_000,
    p99BatchMs: 750,
    phaseASpeedup: 5,
    serializationRatio: 0.01,
    replicas: [] as Record<string, unknown>[],
    ...overrides,
  };
  if (!("durationMs" in overrides)) {
    report.durationMs = (report.accepted / report.acceptedTps) * 1_000;
  }
  report.replicas = Array.from({ length: 2 }, (_, index) => ({
    database: `replica_${index}`,
    writeBehindMaxBatch: report.writeBehindMaxBatch,
    writeBehindFinalFlushMs: 500,
    durationMs: report.durationMs / 2,
    depositProjectionDeltaIntervalMs: 5_000,
    depositProjectionActiveDurationMs: report.durationMs / 2,
    depositProjectionDeltaBumps: Math.max(
      0,
      Math.floor(report.durationMs / 2 / 5_000) - 1,
    ),
    ledgerCacheDeltaApplies: Math.max(
      0,
      Math.floor(report.durationMs / 2 / 5_000) - 1,
    ),
    ledgerCacheFullReloads: 0,
    worstBumpThroughputRatio: 0.96,
    accepted: report.corpusRowCount,
    rejected: 0,
    acceptedAdmissionRows: report.corpusRowCount,
    queuedAdmissionRows: 0,
    validatingAdmissionRows: 0,
    rejectedAdmissionRows: 0,
    admissionPayloadRows: report.corpusRowCount,
    mempoolRows: report.corpusRowCount,
    mempoolLedgerRows:
      report.expectedLedgerRows +
      Math.max(0, Math.floor(report.durationMs / 2 / 5_000) - 1),
    cachedLedgerRows: report.expectedLedgerRows,
    missingExpectedTxIds: 0,
    unexpectedAcceptedTxIds: 0,
    acceptedTps: report.acceptedTps,
    p99BatchMs: 750,
    serializationRatio: 0.01,
  }));
  return report;
};

export const chunkAbReports = ({
  chunk64Tps = [10_200, 10_300, 10_400],
  chunk128Tps = [10_600, 10_700, 10_800],
}: {
  readonly chunk64Tps?: readonly [number, number, number];
  readonly chunk128Tps?: readonly [number, number, number];
} = {}) => {
  const startedAt = Date.parse("2026-07-14T12:00:00.000Z");
  return Array.from({ length: 6 }, (_, index) => {
    const chunkSize = index % 2 === 0 ? 64 : 128;
    const replicaNumber = Math.floor(index / 2) + 1;
    const acceptedTps =
      chunkSize === 64
        ? chunk64Tps[replicaNumber - 1]!
        : chunk128Tps[replicaNumber - 1]!;
    const report = stageBReport({
      acceptedTps,
      chunkSize,
      generatedAtIso: new Date(startedAt + (index + 1) * 1_000).toISOString(),
    });
    const databaseBase =
      `midgard_phase2_bench_cab_20260714t120000z_` +
      `chunk${chunkSize.toString()}_${replicaNumber.toString()}`;
    report.replicas[0]!.database = `${databaseBase}_a`;
    report.replicas[1]!.database = `${databaseBase}_b`;
    return report;
  });
};

export const writeBehindAbReports = () => {
  const startedAt = Date.parse("2026-07-14T11:00:00.000Z");
  const controls = [10_100, 10_200, 10_300].map((acceptedTps, index) => {
    const report = stageBReport({
      acceptedTps,
      generatedAtIso: new Date(
        startedAt + (index * 2 + 1) * 1_000,
      ).toISOString(),
    });
    const databaseBase = `midgard_phase2_bench_wab_20260714t110000z_control_${(
      index + 1
    ).toString()}`;
    report.replicas[0]!.database = `${databaseBase}_a`;
    report.replicas[1]!.database = `${databaseBase}_b`;
    return report;
  });
  const candidates = [10_500, 10_600, 10_700].map((acceptedTps, index) => {
    const report = stageBReport({
      acceptedTps,
      writeBehindMaxBatch: 2_048,
      generatedAtIso: new Date(
        startedAt + (index * 2 + 2) * 1_000,
      ).toISOString(),
    });
    const databaseBase = `midgard_phase2_bench_wab_20260714t110000z_candidate_${(
      index + 1
    ).toString()}`;
    report.replicas[0]!.database = `${databaseBase}_a`;
    report.replicas[1]!.database = `${databaseBase}_b`;
    return report;
  });
  return { controls, candidates };
};

export const scriptHeavyReport = (overrides: Record<string, unknown> = {}) => ({
  generatedAtIso: "2026-07-14T12:00:07.000Z",
  gateAsserted: true,
  gateMode: "chunk128_candidate",
  pinnedEightCore: true,
  containerIdentityProved: true,
  cpuModel: "Phase 2 test CPU",
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
  chunkSize: 128,
  signatureVerifier: "node",
  everyTransactionHasPlutusSpend: true,
  everyTransactionIsPlutusV3: true,
  uplcInWorkers: true,
  verdictMatchesInline: true,
  statePatchMatchesInline: true,
  batchSize: 256,
  batches: 1_200,
  accepted: 307_200,
  rejected: 0,
  durationMsRequested: 300_000,
  durationMsObserved: 300_000,
  eventLoopDelayP99Ms: 49,
  chunkAbExperimentId: "cab_20260714t120000z",
  corpusPath: "/workspace/corpus.ndjson",
  corpusManifestPath: "/workspace/corpus.ndjson.manifest.json",
  corpusSha256: "aa".repeat(32),
  corpusRowCount: 25_600,
  ...overrides,
});

export const setCorpusRowCount = (
  report: ReturnType<typeof stageBReport>,
  corpusRowCount: number,
) => {
  report.corpusRowCount = corpusRowCount;
  report.expectedAccepted = corpusRowCount * 2;
  report.accepted = corpusRowCount * 2;
  report.durationMs = (report.accepted / report.acceptedTps) * 1_000;
  for (const replica of report.replicas) {
    replica.accepted = corpusRowCount;
    replica.acceptedAdmissionRows = corpusRowCount;
    replica.admissionPayloadRows = corpusRowCount;
    replica.mempoolRows = corpusRowCount;
    replica.durationMs = report.durationMs / 2;
  }
};
