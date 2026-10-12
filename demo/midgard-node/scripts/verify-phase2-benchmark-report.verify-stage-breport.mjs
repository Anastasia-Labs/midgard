export const fail = (message) => {
  throw new Error(`Phase 2 benchmark gate failed: ${message}`);
};

export const object = (value, label) => {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    fail(`${label} must be an object`);
  }
  return value;
};

export const finite = (value, label) => {
  if (typeof value !== "number" || !Number.isFinite(value)) {
    fail(`${label} must be a finite number`);
  }
  return value;
};

export const equal = (actual, expected, label) => {
  if (actual !== expected) {
    fail(
      `${label} must be ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`,
    );
  }
};

export const equalJson = (actual, expected, label) => {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    fail(`${label} must exactly match the first report`);
  }
};

export const atLeast = (actual, minimum, label) => {
  if (finite(actual, label) < minimum) {
    fail(`${label} must be >= ${minimum}, got ${actual}`);
  }
};

export const atMost = (actual, maximum, label) => {
  if (finite(actual, label) > maximum) {
    fail(`${label} must be <= ${maximum}, got ${actual}`);
  }
};

export const below = (actual, maximum, label) => {
  if (finite(actual, label) >= maximum) {
    fail(`${label} must be < ${maximum}, got ${actual}`);
  }
};

export const nonEmptyArray = (value, label) => {
  if (!Array.isArray(value) || value.length === 0) {
    fail(`${label} must be a non-empty array`);
  }
  return value;
};

export const nonEmptyString = (value, label) => {
  if (typeof value !== "string" || value.length === 0) {
    fail(`${label} must be a non-empty string`);
  }
  return value;
};

export const positiveSafeInteger = (value, label) => {
  if (!Number.isSafeInteger(value) || value < 1) {
    fail(`${label} must be a positive safe integer`);
  }
  return value;
};

const exactSha256ImageId = (value, label) => {
  const imageId = nonEmptyString(value, label);
  if (!/^sha256:[0-9a-f]{64}$/u.test(imageId)) {
    fail(`${label} must be an exact lowercase sha256:<64 hex> image ID`);
  }
  return imageId;
};

export const approximatelyEqual = (actual, expected, label) => {
  finite(actual, label);
  finite(expected, `${label} expected`);
  const tolerance = Math.max(1e-9, Math.abs(expected) * 1e-9);
  if (Math.abs(actual - expected) > tolerance) {
    fail(`${label} must equal measured ${expected}, got ${actual}`);
  }
};

export const FULL_GATE_REPLICA_DURATION_MS = 300_000;

export const FULL_GATE_CORPUS_CAPACITY_TPS = 12_600;

export const FULL_GATE_FINAL_FLUSH_ALLOWANCE_MS = 5_000;

export const FULL_GATE_MINIMUM_CORPUS_ROWS =
  (FULL_GATE_REPLICA_DURATION_MS / 1_000) * FULL_GATE_CORPUS_CAPACITY_TPS;

export const verifyEightPhysicalCores = (report, prefix = "") => {
  const field = (name) => (prefix === "" ? name : `${prefix}.${name}`);
  const logical = nonEmptyArray(
    report.affinityLogicalCpuIds,
    field("affinityLogicalCpuIds"),
  );
  equal(logical.length, 8, `${field("affinityLogicalCpuIds")}.length`);
  equal(
    new Set(logical).size,
    8,
    `${field("affinityLogicalCpuIds")} distinct count`,
  );
  equal(
    new Set(
      nonEmptyArray(
        report.affinityPhysicalCoreIds,
        field("affinityPhysicalCoreIds"),
      ),
    ).size,
    8,
    `${field("affinityPhysicalCoreIds")} distinct count`,
  );
};

export const verifyExactNodeContainerImage = (report) => {
  equal(report.expectedNodeImage, "node:22.22.2", "expectedNodeImage");
  equal(report.nodeImage, "node:22.22.2", "nodeImage");
  const expectedImageId = exactSha256ImageId(
    report.expectedNodeImageId,
    "expectedNodeImageId",
  );
  const containerIdentity = object(
    report.containerIdentity,
    "containerIdentity",
  );
  equal(containerIdentity.proved, true, "containerIdentity.proved");
  equal(containerIdentity.image, "node:22.22.2", "containerIdentity.image");
  const imageId = exactSha256ImageId(
    containerIdentity.imageId,
    "containerIdentity.imageId",
  );
  equal(imageId, expectedImageId, "containerIdentity.imageId");
  equal(
    exactSha256ImageId(report.nodeImageId, "nodeImageId"),
    expectedImageId,
    "nodeImageId",
  );
};

export const median = (values) => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.floor(sorted.length / 2)];
};

const verifyReplica = (
  replica,
  report,
  index,
  { minimumAcceptedTps = 0, minimumDurationMs = 0 } = {},
) => {
  const label = `replicas[${index}]`;
  object(replica, label);
  equal(
    replica.writeBehindMaxBatch,
    report.writeBehindMaxBatch,
    `${label}.writeBehindMaxBatch`,
  );
  atLeast(replica.accepted, 1, `${label}.accepted`);
  equal(replica.rejected, 0, `${label}.rejected`);
  equal(
    replica.acceptedAdmissionRows,
    replica.accepted,
    `${label}.acceptedAdmissionRows`,
  );
  equal(replica.queuedAdmissionRows, 0, `${label}.queuedAdmissionRows`);
  equal(replica.validatingAdmissionRows, 0, `${label}.validatingAdmissionRows`);
  equal(replica.rejectedAdmissionRows, 0, `${label}.rejectedAdmissionRows`);
  equal(
    replica.admissionPayloadRows,
    replica.accepted,
    `${label}.admissionPayloadRows`,
  );
  equal(replica.mempoolRows, replica.accepted, `${label}.mempoolRows`);
  equal(
    replica.cachedLedgerRows,
    report.expectedLedgerRows,
    `${label}.cachedLedgerRows`,
  );
  equal(
    replica.mempoolLedgerRows,
    report.expectedLedgerRows + replica.depositIngestions,
    `${label}.mempoolLedgerRows including projected deposits`,
  );
  equal(replica.missingExpectedTxIds, 0, `${label}.missingExpectedTxIds`);
  equal(replica.unexpectedAcceptedTxIds, 0, `${label}.unexpectedAcceptedTxIds`);
  atLeast(replica.acceptedTps, minimumAcceptedTps, `${label}.acceptedTps`);
  atLeast(replica.durationMs, minimumDurationMs, `${label}.durationMs`);
  approximatelyEqual(
    replica.acceptedTps,
    replica.accepted / (replica.durationMs / 1_000),
    `${label}.acceptedTps`,
  );
  atMost(replica.p99BatchMs, 1_000, `${label}.p99BatchMs`);
  atMost(replica.serializationRatio, 0.1, `${label}.serializationRatio`);
};

export const verifyStageBReport = (
  reportValue,
  {
    minimumAcceptedTps = 10_000,
    minimumDurationMs = 0,
    shortAssert,
    chunkSize,
    writeBehindMaxBatch,
    minimumReplicaAcceptedTps = 0,
    minimumReplicaDurationMs = 0,
  } = {},
) => {
  const report = object(reportValue, "report");
  equal(report.gateAsserted, true, "gateAsserted");
  equal(report.wholeSystemPinnedEightCore, true, "wholeSystemPinnedEightCore");
  equal(report.pinnedEightCore, true, "pinnedEightCore");
  equal(report.nodePinnedEightCore, true, "nodePinnedEightCore");
  equal(report.availableParallelism, 8, "availableParallelism");
  verifyEightPhysicalCores(report);
  equal(report.nodeVersion, "v22.22.2", "nodeVersion");
  verifyExactNodeContainerImage(report);
  equal(
    report.expectedPostgresImage,
    "postgres:15.15-alpine",
    "expectedPostgresImage",
  );
  equal(report.nodeContainerProved, true, "nodeContainerProved");
  equal(report.postgresImagePinned, true, "postgresImagePinned");
  equal(report.postgresDataIsEphemeral, true, "postgresDataIsEphemeral");
  equal(
    report.connectedToDeclaredPostgresContainer,
    true,
    "connectedToDeclaredPostgresContainer",
  );
  equal(report.reuseDatabases, false, "reuseDatabases");
  equal(report.replicaCount, 2, "replicaCount");
  equal(report.warmupIterations, 2, "warmupIterations");
  equal(
    report.disableTxDeltaWriteBehindDiagnostic,
    false,
    "disableTxDeltaWriteBehindDiagnostic",
  );
  equal(report.poolSize, 6, "poolSize");
  equal(report.signatureVerifier, "node", "signatureVerifier");
  equal(report.drainLoops, 4, "drainLoops");
  equal(report.batchSize, 2_048, "batchSize");
  if (shortAssert !== undefined)
    equal(report.shortAssert, shortAssert, "shortAssert");
  if (chunkSize !== undefined) equal(report.chunkSize, chunkSize, "chunkSize");
  if (writeBehindMaxBatch !== undefined) {
    equal(
      report.writeBehindMaxBatch,
      writeBehindMaxBatch,
      "writeBehindMaxBatch",
    );
  }
  equal(report.lostTransactions, 0, "lostTransactions");
  atLeast(report.corpusRowCount, 1, "corpusRowCount");
  equal(report.expectedAccepted, report.corpusRowCount * 2, "expectedAccepted");
  equal(report.accepted, report.expectedAccepted, "accepted");
  atLeast(report.acceptedTps, minimumAcceptedTps, "acceptedTps");
  atLeast(report.durationMs, minimumDurationMs, "durationMs");
  atMost(report.p99BatchMs, 1_000, "p99BatchMs");
  atLeast(report.phaseASpeedup, 4, "phaseASpeedup");
  atMost(report.serializationRatio, 0.1, "serializationRatio");
  if (!Array.isArray(report.replicas) || report.replicas.length !== 2) {
    fail("replicas must contain exactly two Stage B replicas");
  }
  report.replicas.forEach((replica, index) =>
    verifyReplica(replica, report, index, {
      minimumAcceptedTps: minimumReplicaAcceptedTps,
      minimumDurationMs: minimumReplicaDurationMs,
    }),
  );
  approximatelyEqual(
    report.durationMs,
    report.replicas.reduce(
      (total, replica) =>
        total + finite(replica.durationMs, "replica duration"),
      0,
    ),
    "durationMs",
  );
  approximatelyEqual(
    report.acceptedTps,
    report.accepted / (report.durationMs / 1_000),
    "acceptedTps",
  );
  return report;
};
