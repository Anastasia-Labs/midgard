import { readFile } from "node:fs/promises";
import { pathToFileURL } from "node:url";

const fail = (message) => {
  throw new Error(`Phase 2 benchmark gate failed: ${message}`);
};

const object = (value, label) => {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    fail(`${label} must be an object`);
  }
  return value;
};

const finite = (value, label) => {
  if (typeof value !== "number" || !Number.isFinite(value)) {
    fail(`${label} must be a finite number`);
  }
  return value;
};

const equal = (actual, expected, label) => {
  if (actual !== expected) {
    fail(
      `${label} must be ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`,
    );
  }
};

const equalJson = (actual, expected, label) => {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    fail(`${label} must exactly match the first report`);
  }
};

const atLeast = (actual, minimum, label) => {
  if (finite(actual, label) < minimum) {
    fail(`${label} must be >= ${minimum}, got ${actual}`);
  }
};

const atMost = (actual, maximum, label) => {
  if (finite(actual, label) > maximum) {
    fail(`${label} must be <= ${maximum}, got ${actual}`);
  }
};

const below = (actual, maximum, label) => {
  if (finite(actual, label) >= maximum) {
    fail(`${label} must be < ${maximum}, got ${actual}`);
  }
};

const nonEmptyArray = (value, label) => {
  if (!Array.isArray(value) || value.length === 0) {
    fail(`${label} must be a non-empty array`);
  }
  return value;
};

const nonEmptyString = (value, label) => {
  if (typeof value !== "string" || value.length === 0) {
    fail(`${label} must be a non-empty string`);
  }
  return value;
};

const positiveSafeInteger = (value, label) => {
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

const approximatelyEqual = (actual, expected, label) => {
  finite(actual, label);
  finite(expected, `${label} expected`);
  const tolerance = Math.max(1e-9, Math.abs(expected) * 1e-9);
  if (Math.abs(actual - expected) > tolerance) {
    fail(`${label} must equal measured ${expected}, got ${actual}`);
  }
};

const FULL_GATE_REPLICA_DURATION_MS = 300_000;
const FULL_GATE_CORPUS_CAPACITY_TPS = 12_600;
const FULL_GATE_FINAL_FLUSH_ALLOWANCE_MS = 5_000;
const FULL_GATE_MINIMUM_CORPUS_ROWS =
  (FULL_GATE_REPLICA_DURATION_MS / 1_000) * FULL_GATE_CORPUS_CAPACITY_TPS;

const verifyEightPhysicalCores = (report, prefix = "") => {
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

const verifyExactNodeContainerImage = (report) => {
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

const median = (values) => {
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
    report.expectedLedgerRows + replica.depositProjectionDeltaBumps,
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

const verifyMatchingExperiment = (reports) => {
  const fields = [
    "corpusPath",
    "corpusSha256",
    "corpusRowCount",
    "expectedAccepted",
    "expectedLedgerRows",
    "poolSize",
    "chunkSize",
    "drainLoops",
    "batchSize",
    "warmupIterations",
    "minimumAcceptedTps",
    "reuseDatabases",
    "shortAssert",
    "signatureVerifier",
    "expectedNodeImageId",
    "nodeImage",
    "nodeImageId",
    "disableTxDeltaWriteBehindDiagnostic",
  ];
  for (const field of fields) {
    for (let index = 1; index < reports.length; index += 1) {
      equal(
        reports[index][field],
        reports[0][field],
        `reports[${index}].${field}`,
      );
    }
  }
  for (const field of [
    "affinityLogicalCpuIds",
    "affinityPhysicalCoreIds",
    "cpuModel",
    "nodeVersion",
    "expectedNodeImage",
    "expectedPostgresImage",
  ]) {
    for (let index = 1; index < reports.length; index += 1) {
      equalJson(
        reports[index][field],
        reports[0][field],
        `reports[${index}].${field}`,
      );
    }
  }
};

const verifyInterleavedExperimentOrder = (controls, candidates) => {
  const databasePattern =
    /^midgard_phase2_bench_(wab_(\d{8})t(\d{6})z)_(control|candidate)_([123])$/u;
  const ordered = controls.flatMap((control, index) => [
    control,
    candidates[index],
  ]);
  const seenDatabases = new Set();
  let experimentId;
  let experimentStartedAt;
  let previous = Number.NEGATIVE_INFINITY;
  ordered.forEach((report, index) => {
    const generatedAtIso = nonEmptyString(
      report.generatedAtIso,
      `write-behind reports[${index}].generatedAtIso`,
    );
    const generatedAt = Date.parse(generatedAtIso);
    if (
      !Number.isFinite(generatedAt) ||
      new Date(generatedAt).toISOString() !== generatedAtIso
    ) {
      fail(
        `write-behind reports[${index}].generatedAtIso must be a canonical UTC timestamp`,
      );
    }
    if (generatedAt <= previous) {
      fail(
        `write-behind reports must have strict control/candidate interleaved generatedAtIso order at position ${index}`,
      );
    }
    previous = generatedAt;

    const expectedKind = index % 2 === 0 ? "control" : "candidate";
    const expectedReplica = Math.floor(index / 2) + 1;
    const firstDatabase = nonEmptyString(
      report.replicas[0].database,
      `write-behind reports[${index}].replicas[0].database`,
    );
    const match = firstDatabase.endsWith("_a")
      ? databasePattern.exec(firstDatabase.slice(0, -2))
      : null;
    if (match === null) {
      fail(
        `write-behind reports[${index}].replicas[0].database must identify wab_<UTC timestamp>_${expectedKind}_${expectedReplica.toString()}_a`,
      );
    }
    const [, currentExperimentId, date, time, kind, replica] = match;
    equal(kind, expectedKind, `write-behind reports[${index}] database kind`);
    equal(
      Number(replica),
      expectedReplica,
      `write-behind reports[${index}] database replica identity`,
    );
    if (experimentId === undefined) {
      experimentId = currentExperimentId;
      experimentStartedAt = parseChunkAbRunStartedAt(
        date,
        time,
        `write-behind reports[${index}] database identity`,
      );
    } else {
      equal(
        currentExperimentId,
        experimentId,
        `write-behind reports[${index}] experiment identity`,
      );
    }
    const secondDatabase = `${firstDatabase.slice(0, -2)}_b`;
    equal(
      report.replicas[1].database,
      secondDatabase,
      `write-behind reports[${index}].replicas[1].database`,
    );
    for (const database of [firstDatabase, secondDatabase]) {
      if (seenDatabases.has(database)) {
        fail(`database identity ${JSON.stringify(database)} must be unique`);
      }
      seenDatabases.add(database);
    }
    if (
      generatedAt < experimentStartedAt ||
      generatedAt - experimentStartedAt > 86_400_000
    ) {
      fail(
        `write-behind reports[${index}].generatedAtIso must fall within 24 hours after its run identity`,
      );
    }
  });
  return experimentId;
};

const chunkAbDatabaseIdentityPattern =
  /^midgard_phase2_bench_(cab_(\d{8})t(\d{6})z)_chunk(64|128)_([123])$/u;

const parseChunkAbRunStartedAt = (date, time, label) => {
  const value = `${date.slice(0, 4)}-${date.slice(4, 6)}-${date.slice(6, 8)}T${time.slice(0, 2)}:${time.slice(2, 4)}:${time.slice(4, 6)}.000Z`;
  const parsed = Date.parse(value);
  if (!Number.isFinite(parsed) || new Date(parsed).toISOString() !== value) {
    fail(`${label} contains an invalid UTC run timestamp`);
  }
  return parsed;
};

const verifyChunkAbIdentityAndOrder = (reports) => {
  const expectedChunks = [64, 128, 64, 128, 64, 128];
  const seenDatabases = new Set();
  let experimentId;
  let experimentStartedAt;
  let previousGeneratedAt = Number.NEGATIVE_INFINITY;

  reports.forEach((report, index) => {
    equal(
      report.chunkSize,
      expectedChunks[index],
      `reports[${index}].chunkSize`,
    );
    const generatedAtIso = nonEmptyString(
      report.generatedAtIso,
      `reports[${index}].generatedAtIso`,
    );
    const generatedAt = Date.parse(generatedAtIso);
    if (
      !Number.isFinite(generatedAt) ||
      new Date(generatedAt).toISOString() !== generatedAtIso
    ) {
      fail(
        `reports[${index}].generatedAtIso must be a canonical UTC timestamp`,
      );
    }
    if (generatedAt <= previousGeneratedAt) {
      fail(
        `chunk-ab reports must be supplied in strict generatedAtIso order at position ${index}`,
      );
    }
    previousGeneratedAt = generatedAt;

    const replicas = report.replicas;
    const firstDatabase = nonEmptyString(
      replicas[0].database,
      `reports[${index}].replicas[0].database`,
    );
    const match = firstDatabase.endsWith("_a")
      ? chunkAbDatabaseIdentityPattern.exec(firstDatabase.slice(0, -2))
      : null;
    if (match === null) {
      fail(
        `reports[${index}].replicas[0].database must identify cab_<UTC timestamp>_chunk${expectedChunks[index].toString()}_${(Math.floor(index / 2) + 1).toString()}_a`,
      );
    }
    const [, currentExperimentId, date, time, identityChunk, identityReplica] =
      match;
    equal(
      Number(identityChunk),
      expectedChunks[index],
      `reports[${index}] database chunk identity`,
    );
    equal(
      Number(identityReplica),
      Math.floor(index / 2) + 1,
      `reports[${index}] database replica identity`,
    );
    if (experimentId === undefined) {
      experimentId = currentExperimentId;
      experimentStartedAt = parseChunkAbRunStartedAt(
        date,
        time,
        `reports[${index}] database identity`,
      );
    } else {
      equal(
        currentExperimentId,
        experimentId,
        `reports[${index}] experiment identity`,
      );
    }
    const expectedSecondDatabase = `${firstDatabase.slice(0, -2)}_b`;
    equal(
      replicas[1].database,
      expectedSecondDatabase,
      `reports[${index}].replicas[1].database`,
    );
    for (const database of [firstDatabase, expectedSecondDatabase]) {
      if (seenDatabases.has(database)) {
        fail(`database identity ${JSON.stringify(database)} must be unique`);
      }
      seenDatabases.add(database);
    }
    if (
      generatedAt < experimentStartedAt ||
      generatedAt - experimentStartedAt > 86_400_000
    ) {
      fail(
        `reports[${index}].generatedAtIso must fall within 24 hours after its chunk-ab run identity`,
      );
    }
  });

  return experimentId;
};

const verifyMatchingChunkAbExperiment = (reports) => {
  for (const field of [
    "corpusPath",
    "corpusSha256",
    "corpusRowCount",
    "expectedAccepted",
    "expectedLedgerRows",
    "poolSize",
    "drainLoops",
    "batchSize",
    "warmupIterations",
    "writeBehindMaxBatch",
    "minimumAcceptedTps",
    "reuseDatabases",
    "disableTxDeltaWriteBehindDiagnostic",
    "replicaCount",
    "shortAssert",
    "signatureVerifier",
  ]) {
    for (let index = 1; index < reports.length; index += 1) {
      equal(
        reports[index][field],
        reports[0][field],
        `reports[${index}].${field}`,
      );
    }
  }
  for (const field of [
    "affinityLogicalCpuIds",
    "affinityPhysicalCoreIds",
    "cpuModel",
    "nodeVersion",
    "expectedNodeImage",
    "expectedNodeImageId",
    "nodeImage",
    "nodeImageId",
    "expectedPostgresImage",
  ]) {
    for (let index = 1; index < reports.length; index += 1) {
      equalJson(
        reports[index][field],
        reports[0][field],
        `reports[${index}].${field}`,
      );
    }
  }
};

const verifyScriptHeavyReport = (
  reportValue,
  { chunkSize, candidate = false },
) => {
  const report = object(reportValue, "report");
  equal(report.gateAsserted, true, "gateAsserted");
  equal(report.pinnedEightCore, true, "pinnedEightCore");
  equal(report.containerIdentityProved, true, "containerIdentityProved");
  equal(report.nodeVersion, "v22.22.2", "nodeVersion");
  verifyExactNodeContainerImage(report);
  equal(report.availableParallelism, 8, "availableParallelism");
  verifyEightPhysicalCores(report);
  equal(report.poolSize, 6, "poolSize");
  equal(report.batchSize, 256, "batchSize");
  equal(report.chunkSize, chunkSize, "chunkSize");
  equal(report.signatureVerifier, "node", "signatureVerifier");
  equal(
    report.everyTransactionHasPlutusSpend,
    true,
    "everyTransactionHasPlutusSpend",
  );
  equal(report.uplcInWorkers, true, "uplcInWorkers");
  equal(report.verdictMatchesInline, true, "verdictMatchesInline");
  equal(
    report.gateMode,
    candidate ? "chunk128_candidate" : "production_default_chunk64",
    "gateMode",
  );
  equal(report.everyTransactionIsPlutusV3, true, "everyTransactionIsPlutusV3");
  equal(report.statePatchMatchesInline, true, "statePatchMatchesInline");
  equal(report.rejected, 0, "rejected");
  atLeast(report.batches, 1, "batches");
  atLeast(report.accepted, 1, "accepted");
  equal(report.accepted, report.batchSize * report.batches, "accepted");
  atLeast(report.durationMsObserved, 300_000, "durationMsObserved");
  below(report.eventLoopDelayP99Ms, 50, "eventLoopDelayP99Ms");

  if (candidate) {
    atLeast(report.durationMsRequested, 300_000, "durationMsRequested");
    atLeast(
      report.durationMsObserved,
      report.durationMsRequested,
      "durationMsObserved against requested duration",
    );
    nonEmptyString(report.chunkAbExperimentId, "chunkAbExperimentId");
    nonEmptyString(report.corpusPath, "corpusPath");
    nonEmptyString(report.corpusManifestPath, "corpusManifestPath");
    const corpusSha256 = nonEmptyString(report.corpusSha256, "corpusSha256");
    if (!/^[0-9a-f]{64}$/u.test(corpusSha256)) {
      fail("corpusSha256 must be an exact lowercase SHA-256");
    }
    positiveSafeInteger(report.corpusRowCount, "corpusRowCount");
  }
  return report;
};

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
          replica.depositProjectionDeltaIntervalMs,
          5_000,
          `${label}.depositProjectionDeltaIntervalMs`,
        );
        const activeDurationMs = finite(
          replica.depositProjectionActiveDurationMs,
          `${label}.depositProjectionActiveDurationMs`,
        );
        atMost(
          activeDurationMs,
          replica.durationMs,
          `${label}.depositProjectionActiveDurationMs`,
        );
        atLeast(
          activeDurationMs,
          replica.durationMs - FULL_GATE_FINAL_FLUSH_ALLOWANCE_MS,
          `${label}.depositProjectionActiveDurationMs`,
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
        const minimumBumps = Math.max(
          1,
          Math.floor(
            (replica.durationMs - FULL_GATE_FINAL_FLUSH_ALLOWANCE_MS) / 5_000,
          ) - 1,
        );
        atLeast(
          replica.depositProjectionDeltaBumps,
          minimumBumps,
          `${label}.depositProjectionDeltaBumps`,
        );
        equal(
          replica.ledgerCacheDeltaApplies,
          replica.depositProjectionDeltaBumps,
          `${label}.ledgerCacheDeltaApplies`,
        );
        equal(
          replica.ledgerCacheFullReloads,
          0,
          `${label}.ledgerCacheFullReloads`,
        );
        atLeast(
          replica.worstBumpThroughputRatio,
          0.95,
          `${label}.worstBumpThroughputRatio`,
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

const main = async () => {
  const [mode, ...paths] = process.argv.slice(2);
  if (mode === undefined) {
    fail(
      "usage: verify-phase2-benchmark-report.mjs <mode> <report.json> [...]",
    );
  }
  const reports = await Promise.all(
    paths.map(async (path) => JSON.parse(await readFile(path, "utf8"))),
  );
  const expectedFullCorpus =
    mode !== "full"
      ? undefined
      : {
          sha256: process.env.PHASE2_EXPECTED_FULL_CORPUS_SHA256 ?? "",
          rowCount: Number(
            process.env.PHASE2_EXPECTED_FULL_CORPUS_ROWS ?? Number.NaN,
          ),
        };
  const result = verifyPhase2BenchmarkReports(mode, reports, {
    expectedFullCorpus,
  });
  process.stdout.write(
    `${JSON.stringify({ mode, passed: true, result }, null, 2)}\n`,
  );
};

if (
  process.argv[1] !== undefined &&
  pathToFileURL(process.argv[1]).href === import.meta.url
) {
  await main();
}
