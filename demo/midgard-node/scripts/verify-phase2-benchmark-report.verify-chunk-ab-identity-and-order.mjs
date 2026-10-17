import {
  equal,
  equalJson,
  fail,
  nonEmptyString,
} from "./verify-phase2-benchmark-report.verify-stage-breport.mjs";

export const verifyMatchingExperiment = (reports) => {
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

export const verifyInterleavedExperimentOrder = (controls, candidates) => {
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

export const chunkAbDatabaseIdentityPattern =
  /^midgard_phase2_bench_(cab_(\d{8})t(\d{6})z)_chunk(64|128)_([123])$/u;

export const parseChunkAbRunStartedAt = (date, time, label) => {
  const value = `${date.slice(0, 4)}-${date.slice(4, 6)}-${date.slice(6, 8)}T${time.slice(0, 2)}:${time.slice(2, 4)}:${time.slice(4, 6)}.000Z`;
  const parsed = Date.parse(value);
  if (!Number.isFinite(parsed) || new Date(parsed).toISOString() !== value) {
    fail(`${label} contains an invalid UTC run timestamp`);
  }
  return parsed;
};

export const verifyChunkAbIdentityAndOrder = (reports) => {
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

export const verifyMatchingChunkAbExperiment = (reports) => {
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
