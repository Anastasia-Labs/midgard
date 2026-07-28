import { isAbsolute, resolve } from "node:path";

type JsonRecord = Record<string, unknown>;

export type ArchitectureGPhase1FormalBindingIdentityV1 = {
  readonly schemaVersion: "midgard-architecture-g-phase1-formal-binding-identity-v1";
  readonly path: string;
  readonly sha256: string;
  readonly deploymentManifestId: string;
  readonly nodeImageId: string;
  readonly nodeContainerId: string;
  readonly walletSetSha256: string;
  readonly fundingSetSha256: string;
  readonly corpus: {
    readonly path: string;
    readonly indexPath: string;
    readonly manifestPath: string;
    readonly sliceId: string;
    readonly corpusSha256: string;
    readonly indexSha256: string;
    readonly manifestSha256: string;
  };
  readonly generationResult: {
    readonly path: string;
    readonly sha256: string;
    readonly schemaVersion: "midgard-stress-corpus-generation-v1";
  };
  readonly harness: {
    readonly scenarioId: string;
    readonly engineId: string;
  };
};

export type ArchitectureGRuntimeIdentityV1 = {
  readonly schemaVersion: "midgard-architecture-g-runtime-identity-v1";
  readonly version: string;
  readonly execPath: string;
  readonly executableSha256: string;
};

export type ArchitectureGCommitCandidateSeedInputV1 = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-seed-v1";
  readonly phase1FormalBinding: ArchitectureGPhase1FormalBindingIdentityV1;
  readonly runtimeIdentity: ArchitectureGRuntimeIdentityV1;
  readonly corpusSlicePath: string;
  readonly corpusSliceSha256: string;
  readonly fundingMapPath: string;
  readonly fundingMapSha256: string;
  readonly expectedTransactionCount: number;
  readonly firstTimestampIso: string;
};

export type ArchitectureGCorpusFundingV1 = {
  readonly schemaVersion: "midgard-architecture-g-corpus-funding-v1";
  readonly corpusSha256: string;
  readonly sliceSha256: string;
  readonly entries: readonly {
    readonly walletId: string;
    readonly outref: string;
    readonly outputCbor: string;
  }[];
};

export type ArchitectureGCommitCandidateSeedResultV1 = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-seed-result-v1";
  readonly databaseName: string;
  readonly corpusSliceSha256: string;
  readonly mempoolTxCount: number;
  readonly fundingCount: number;
  readonly terminalLedgerCount: number;
  readonly deltaCount: number;
};

export type ArchitectureGCommitCandidateInputV1 = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-input-v1";
  readonly phase1FormalBinding: ArchitectureGPhase1FormalBindingIdentityV1;
  readonly runtimeIdentity: ArchitectureGRuntimeIdentityV1;
  readonly levelPath: string;
  readonly binaryPath: string;
  readonly binarySha256: string;
  readonly sidecarPath: string;
  readonly expectedTransactionCount: number;
  readonly corpusSha256: string;
  readonly corpusSliceSha256: string;
  readonly fundingMapSha256: string;
  readonly fixtureCreationPath: string;
  readonly fixtureCreationSha256: string;
  readonly fixtureInitialUtxoCount: number;
  readonly baseUtxoPayloadAggregate: {
    readonly entryCount: number;
    readonly encodedTupleBytes: number;
  };
  readonly workerInput: {
    readonly data: {
      readonly availableConfirmedBlock: "";
      readonly availableLocalFinalizationBlock: "";
      readonly currentBlockStartTimeMs: number;
      readonly localFinalizationPending: false;
      readonly ledgerStoreLeaseOwner: string;
      readonly mempoolTxsCountSoFar: 0;
      readonly sizeOfProcessedTxsSoFar: 0;
      readonly baseSnapshotId: string;
      readonly stateQueueHasUnmergedTail: true;
      readonly speculativeBuild: {
        readonly base: {
          readonly headerHash: string;
          readonly utxosRoot: string;
          readonly blockEndTimeMs: number;
          readonly submittedTxHash: string;
        };
        readonly watermarks: {
          readonly depositMs: number;
          readonly withdrawalMs: number;
          readonly txOrderMs: number;
          readonly refreshedAtMs: number;
        };
        readonly excludedMempoolTxIds: readonly string[];
        readonly excludedDepositEventIds: readonly string[];
        readonly excludedForcedTransactionEventIds: readonly string[];
        readonly excludedWithdrawalEventIds: readonly string[];
      };
    };
  };
};

export type ArchitectureGFixtureCreationV1 = {
  readonly fixtureCreated: true;
  readonly fixturePath: string;
  readonly initialUtxoCount: number;
  readonly marker: string;
  readonly durationMs: number;
  readonly diagnostics: Readonly<Record<string, number>>;
  readonly utxoPayloadAggregate: {
    readonly entryCount: number;
    readonly encodedTupleBytes: number;
  };
  readonly canonicalFunding: null | {
    readonly path: string;
    readonly sha256: string;
    readonly entryCount: number;
  };
};

const architectureGFixtureDiagnosticKeys = [
  "entries",
  "storePuts",
  "storeDels",
  "serialiseCalls",
  "serialiseMs",
  "deferredMaterializedEstimatedBytes",
  "deferredMaterializedActualBytes",
  "deferredLazyReads",
  "deferredLazySerialiseMs",
  "deferredLazySerialisedBytes",
  "arenaCheckpointCalls",
  "arenaCheckpointMs",
  "arenaCheckpointNodes",
  "arenaCheckpointBytes",
  "pathCacheEntries",
  "pathCacheBytes",
  "pathCacheHits",
  "liveArenaPrunedNodes",
  "liveArenaPromotedNodes",
  "liveArenaPromotedBytes",
  "retainedSnapshotAuthentications",
  "retainedSnapshotAuthenticationMs",
  "transientLiveNodes",
  "transientLiveBytes",
  "transientDirtyNodes",
  "transientSnapshotsCaptured",
  "eventAtomicFinalizations",
  "eventAtomicDirtyNodes",
  "eventAtomicMaxDirtyNodes",
  "levelGets",
  "levelGetManyCalls",
  "levelGetManyMaxKeys",
  "levelGetMs",
  "jsonCodecMs",
  "overlayHits",
  "readCacheHits",
  "levelBatchWrites",
  "bytesFlushed",
  "overlayEntries",
  "overlayBytes",
  "overlaySpills",
  "overlaySpillMs",
  "flushMs",
] as const;

const exactRecord = (
  value: unknown,
  keys: readonly string[],
  label: string,
): JsonRecord => {
  if (
    value === null ||
    typeof value !== "object" ||
    Array.isArray(value) ||
    JSON.stringify(Object.keys(value).sort()) !==
      JSON.stringify([...keys].sort())
  ) {
    throw new Error(`${label} must contain exactly: ${keys.join(", ")}`);
  }
  return value as JsonRecord;
};

const nonEmptyString = (
  value: unknown,
  label: string,
  maxLength = 4096,
): string => {
  if (
    typeof value !== "string" ||
    value.trim().length === 0 ||
    value.length > maxLength
  ) {
    throw new Error(`${label} must be a bounded nonempty string`);
  }
  return value;
};

const hash = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${label} must be a lowercase SHA-256 digest`);
  }
  return value;
};

const absolutePath = (value: unknown, label: string): string => {
  const path = nonEmptyString(value, label);
  if (!isAbsolute(path) || resolve(path) !== path) {
    throw new Error(`${label} must be a canonical absolute path`);
  }
  return path;
};

const positiveSafeInteger = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) <= 0) {
    throw new Error(`${label} must be a positive safe integer`);
  }
  return value as number;
};

const nonNegativeSafeInteger = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a nonnegative safe integer`);
  }
  return value as number;
};

const positiveFiniteNumber = (value: unknown, label: string): number => {
  if (!Number.isFinite(value) || (value as number) <= 0) {
    throw new Error(`${label} must be a positive finite number`);
  }
  return value as number;
};

const nonNegativeFiniteNumber = (value: unknown, label: string): number => {
  if (!Number.isFinite(value) || (value as number) < 0) {
    throw new Error(`${label} must be a nonnegative finite number`);
  }
  return value as number;
};

const canonicalTimestamp = (value: unknown, label: string): string => {
  const parsed = typeof value === "string" ? new Date(value) : null;
  if (
    typeof value !== "string" ||
    !/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/u.test(value) ||
    parsed === null ||
    !Number.isFinite(parsed.getTime()) ||
    parsed.toISOString() !== value
  ) {
    throw new Error(`${label} must be a canonical UTC timestamp`);
  }
  return value;
};

const decodePhase1FormalBindingIdentity = (
  value: unknown,
): ArchitectureGPhase1FormalBindingIdentityV1 => {
  const identity = exactRecord(
    value,
    [
      "schemaVersion",
      "path",
      "sha256",
      "deploymentManifestId",
      "nodeImageId",
      "nodeContainerId",
      "walletSetSha256",
      "fundingSetSha256",
      "corpus",
      "generationResult",
      "harness",
    ],
    "Architecture G Phase 1 formal-binding identity",
  );
  const corpus = exactRecord(
    identity.corpus,
    [
      "path",
      "indexPath",
      "manifestPath",
      "sliceId",
      "corpusSha256",
      "indexSha256",
      "manifestSha256",
    ],
    "Architecture G Phase 1 corpus identity",
  );
  const generationResult = exactRecord(
    identity.generationResult,
    ["path", "sha256", "schemaVersion"],
    "Architecture G Phase 1 generation-result identity",
  );
  const harness = exactRecord(
    identity.harness,
    ["scenarioId", "engineId"],
    "Architecture G Phase 1 harness identity",
  );
  if (
    identity.schemaVersion !==
      "midgard-architecture-g-phase1-formal-binding-identity-v1" ||
    generationResult.schemaVersion !== "midgard-stress-corpus-generation-v1"
  ) {
    throw new Error("Unsupported Architecture G Phase 1 identity");
  }
  absolutePath(identity.path, "formalBinding.path");
  hash(identity.sha256, "formalBinding.sha256");
  nonEmptyString(
    identity.deploymentManifestId,
    "formalBinding.deploymentManifestId",
  );
  nonEmptyString(identity.nodeImageId, "formalBinding.nodeImageId");
  nonEmptyString(identity.nodeContainerId, "formalBinding.nodeContainerId");
  hash(identity.walletSetSha256, "formalBinding.walletSetSha256");
  hash(identity.fundingSetSha256, "formalBinding.fundingSetSha256");
  absolutePath(corpus.path, "formalBinding.corpus.path");
  absolutePath(corpus.indexPath, "formalBinding.corpus.indexPath");
  absolutePath(corpus.manifestPath, "formalBinding.corpus.manifestPath");
  nonEmptyString(corpus.sliceId, "formalBinding.corpus.sliceId");
  hash(corpus.corpusSha256, "formalBinding.corpus.corpusSha256");
  hash(corpus.indexSha256, "formalBinding.corpus.indexSha256");
  hash(corpus.manifestSha256, "formalBinding.corpus.manifestSha256");
  absolutePath(generationResult.path, "formalBinding.generationResult.path");
  hash(generationResult.sha256, "formalBinding.generationResult.sha256");
  hash(harness.scenarioId, "formalBinding.harness.scenarioId");
  hash(harness.engineId, "formalBinding.harness.engineId");
  return identity as ArchitectureGPhase1FormalBindingIdentityV1;
};

const decodeRuntimeIdentity = (
  value: unknown,
): ArchitectureGRuntimeIdentityV1 => {
  const identity = exactRecord(
    value,
    ["schemaVersion", "version", "execPath", "executableSha256"],
    "Architecture G runtime identity",
  );
  if (identity.schemaVersion !== "midgard-architecture-g-runtime-identity-v1") {
    throw new Error("Unsupported Architecture G runtime identity");
  }
  nonEmptyString(identity.version, "runtimeIdentity.version");
  absolutePath(identity.execPath, "runtimeIdentity.execPath");
  hash(identity.executableSha256, "runtimeIdentity.executableSha256");
  return identity as ArchitectureGRuntimeIdentityV1;
};

export const decodeArchitectureGCommitCandidateSeedInputV1 = (
  value: unknown,
): ArchitectureGCommitCandidateSeedInputV1 => {
  const input = exactRecord(
    value,
    [
      "schemaVersion",
      "phase1FormalBinding",
      "runtimeIdentity",
      "corpusSlicePath",
      "corpusSliceSha256",
      "fundingMapPath",
      "fundingMapSha256",
      "expectedTransactionCount",
      "firstTimestampIso",
    ],
    "Architecture G commit-candidate seed input",
  );
  if (
    input.schemaVersion !== "midgard-architecture-g-commit-candidate-seed-v1"
  ) {
    throw new Error("Unsupported Architecture G candidate seed input");
  }
  decodePhase1FormalBindingIdentity(input.phase1FormalBinding);
  decodeRuntimeIdentity(input.runtimeIdentity);
  absolutePath(input.corpusSlicePath, "seedInput.corpusSlicePath");
  hash(input.corpusSliceSha256, "seedInput.corpusSliceSha256");
  absolutePath(input.fundingMapPath, "seedInput.fundingMapPath");
  hash(input.fundingMapSha256, "seedInput.fundingMapSha256");
  positiveSafeInteger(
    input.expectedTransactionCount,
    "seedInput.expectedTransactionCount",
  );
  canonicalTimestamp(input.firstTimestampIso, "seedInput.firstTimestampIso");
  return input as ArchitectureGCommitCandidateSeedInputV1;
};

export const decodeArchitectureGCommitCandidateInputV1 = (
  value: unknown,
): ArchitectureGCommitCandidateInputV1 => {
  const input = exactRecord(
    value,
    [
      "schemaVersion",
      "phase1FormalBinding",
      "runtimeIdentity",
      "levelPath",
      "binaryPath",
      "binarySha256",
      "sidecarPath",
      "expectedTransactionCount",
      "corpusSha256",
      "corpusSliceSha256",
      "fundingMapSha256",
      "fixtureCreationPath",
      "fixtureCreationSha256",
      "fixtureInitialUtxoCount",
      "baseUtxoPayloadAggregate",
      "workerInput",
    ],
    "Architecture G commit-candidate input",
  );
  const phase1FormalBinding = decodePhase1FormalBindingIdentity(
    input.phase1FormalBinding,
  );
  decodeRuntimeIdentity(input.runtimeIdentity);
  const aggregate = exactRecord(
    input.baseUtxoPayloadAggregate,
    ["entryCount", "encodedTupleBytes"],
    "Architecture G candidate base UTxO aggregate",
  );
  const workerInput = exactRecord(
    input.workerInput,
    ["data"],
    "Architecture G candidate worker input",
  );
  const data = exactRecord(
    workerInput.data,
    [
      "availableConfirmedBlock",
      "availableLocalFinalizationBlock",
      "currentBlockStartTimeMs",
      "localFinalizationPending",
      "ledgerStoreLeaseOwner",
      "mempoolTxsCountSoFar",
      "sizeOfProcessedTxsSoFar",
      "baseSnapshotId",
      "stateQueueHasUnmergedTail",
      "speculativeBuild",
    ],
    "Architecture G candidate worker data",
  );
  const speculativeBuild = exactRecord(
    data.speculativeBuild,
    [
      "base",
      "watermarks",
      "excludedMempoolTxIds",
      "excludedDepositEventIds",
      "excludedForcedTransactionEventIds",
      "excludedWithdrawalEventIds",
    ],
    "Architecture G candidate speculative build",
  );
  const base = exactRecord(
    speculativeBuild.base,
    ["headerHash", "utxosRoot", "blockEndTimeMs", "submittedTxHash"],
    "Architecture G candidate speculative base",
  );
  const watermarks = exactRecord(
    speculativeBuild.watermarks,
    ["depositMs", "withdrawalMs", "txOrderMs", "refreshedAtMs"],
    "Architecture G candidate barrier watermarks",
  );
  if (
    input.schemaVersion !== "midgard-architecture-g-commit-candidate-input-v1"
  ) {
    throw new Error("Unsupported Architecture G commit-candidate input");
  }
  for (const [pathValue, label] of [
    [input.levelPath, "candidateInput.levelPath"],
    [input.binaryPath, "candidateInput.binaryPath"],
    [input.sidecarPath, "candidateInput.sidecarPath"],
    [input.fixtureCreationPath, "candidateInput.fixtureCreationPath"],
  ] as const) {
    absolutePath(pathValue, label);
  }
  for (const [hashValue, label] of [
    [input.binarySha256, "candidateInput.binarySha256"],
    [input.corpusSha256, "candidateInput.corpusSha256"],
    [input.corpusSliceSha256, "candidateInput.corpusSliceSha256"],
    [input.fundingMapSha256, "candidateInput.fundingMapSha256"],
    [input.fixtureCreationSha256, "candidateInput.fixtureCreationSha256"],
  ] as const) {
    hash(hashValue, label);
  }
  positiveSafeInteger(
    input.expectedTransactionCount,
    "candidateInput.expectedTransactionCount",
  );
  const fixtureInitialUtxoCount = positiveSafeInteger(
    input.fixtureInitialUtxoCount,
    "candidateInput.fixtureInitialUtxoCount",
  );
  if (
    input.corpusSha256 !== phase1FormalBinding.corpus.corpusSha256 ||
    aggregate.entryCount !== fixtureInitialUtxoCount ||
    data.availableConfirmedBlock !== "" ||
    data.availableLocalFinalizationBlock !== "" ||
    data.localFinalizationPending !== false ||
    !/^commit:[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u.test(
      data.ledgerStoreLeaseOwner as string,
    ) ||
    data.mempoolTxsCountSoFar !== 0 ||
    data.sizeOfProcessedTxsSoFar !== 0 ||
    data.stateQueueHasUnmergedTail !== true
  ) {
    throw new Error("Architecture G candidate input identity is invalid");
  }
  positiveSafeInteger(
    aggregate.encodedTupleBytes,
    "candidateInput.baseUtxoPayloadAggregate.encodedTupleBytes",
  );
  const currentBlockStartTimeMs = positiveSafeInteger(
    data.currentBlockStartTimeMs,
    "candidateInput.currentBlockStartTimeMs",
  );
  const submittedTxHash = hash(
    base.submittedTxHash,
    "candidateInput.speculativeBuild.base.submittedTxHash",
  );
  hash(base.utxosRoot, "candidateInput.speculativeBuild.base.utxosRoot");
  if (
    typeof base.headerHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(base.headerHash) ||
    base.headerHash !== submittedTxHash.slice(0, 56) ||
    base.blockEndTimeMs !== currentBlockStartTimeMs ||
    data.baseSnapshotId !== `architecture-g-candidate:${submittedTxHash}`
  ) {
    throw new Error("Architecture G candidate speculative base is invalid");
  }
  const watermarkValues = [
    positiveSafeInteger(watermarks.depositMs, "watermarks.depositMs"),
    positiveSafeInteger(watermarks.withdrawalMs, "watermarks.withdrawalMs"),
    positiveSafeInteger(watermarks.txOrderMs, "watermarks.txOrderMs"),
  ];
  const refreshedAtMs = positiveSafeInteger(
    watermarks.refreshedAtMs,
    "watermarks.refreshedAtMs",
  );
  if (
    Math.max(...watermarkValues) > refreshedAtMs ||
    currentBlockStartTimeMs >= Math.min(...watermarkValues)
  ) {
    throw new Error("Architecture G candidate barrier watermarks are invalid");
  }
  for (const field of [
    "excludedMempoolTxIds",
    "excludedDepositEventIds",
    "excludedForcedTransactionEventIds",
    "excludedWithdrawalEventIds",
  ] as const) {
    if (
      !Array.isArray(speculativeBuild[field]) ||
      speculativeBuild[field].length !== 0
    ) {
      throw new Error(
        `Architecture G candidate ${field} must be an exact empty array`,
      );
    }
  }
  return input as ArchitectureGCommitCandidateInputV1;
};

export const decodeArchitectureGFixtureCreationV1 = ({
  value,
  expectedFixturePath,
  expectedMarker,
  expectedUtxos,
  expectedAggregate,
  expectedFundingMapSha256,
}: {
  readonly value: unknown;
  readonly expectedFixturePath: string;
  readonly expectedMarker: string;
  readonly expectedUtxos: number;
  readonly expectedAggregate: {
    readonly entryCount: number;
    readonly encodedTupleBytes: number;
  };
  readonly expectedFundingMapSha256: string;
}): ArchitectureGFixtureCreationV1 => {
  const artifact = exactRecord(
    value,
    [
      "fixtureCreated",
      "fixturePath",
      "initialUtxoCount",
      "marker",
      "durationMs",
      "diagnostics",
      "utxoPayloadAggregate",
      "canonicalFunding",
    ],
    "Architecture G fixture-creation artifact",
  );
  const aggregate = exactRecord(
    artifact.utxoPayloadAggregate,
    ["entryCount", "encodedTupleBytes"],
    "Architecture G fixture payload aggregate",
  );
  const diagnostics = exactRecord(
    artifact.diagnostics,
    architectureGFixtureDiagnosticKeys,
    "Architecture G fixture diagnostics",
  );
  for (const field of architectureGFixtureDiagnosticKeys) {
    if (field.endsWith("Ms")) {
      nonNegativeFiniteNumber(
        diagnostics[field],
        `fixtureCreation.diagnostics.${field}`,
      );
    } else {
      nonNegativeSafeInteger(
        diagnostics[field],
        `fixtureCreation.diagnostics.${field}`,
      );
    }
  }
  const fixturePath = absolutePath(
    artifact.fixturePath,
    "fixtureCreation.fixturePath",
  );
  const expectedPath = absolutePath(expectedFixturePath, "expectedFixturePath");
  const marker = hash(artifact.marker, "fixtureCreation.marker");
  const expectedRoot = hash(expectedMarker, "expectedMarker");
  const initialUtxoCount = positiveSafeInteger(
    artifact.initialUtxoCount,
    "fixtureCreation.initialUtxoCount",
  );
  const expectedCount = positiveSafeInteger(expectedUtxos, "expectedUtxos");
  const aggregateEntryCount = positiveSafeInteger(
    aggregate.entryCount,
    "fixtureCreation.utxoPayloadAggregate.entryCount",
  );
  const aggregateEncodedTupleBytes = positiveSafeInteger(
    aggregate.encodedTupleBytes,
    "fixtureCreation.utxoPayloadAggregate.encodedTupleBytes",
  );
  const expectedAggregateEntryCount = positiveSafeInteger(
    expectedAggregate.entryCount,
    "expectedAggregate.entryCount",
  );
  const expectedAggregateEncodedTupleBytes = positiveSafeInteger(
    expectedAggregate.encodedTupleBytes,
    "expectedAggregate.encodedTupleBytes",
  );
  positiveFiniteNumber(artifact.durationMs, "fixtureCreation.durationMs");
  const fundingMapSha256 = hash(
    expectedFundingMapSha256,
    "expectedFundingMapSha256",
  );
  const canonicalFunding = exactRecord(
    artifact.canonicalFunding,
    ["path", "sha256", "entryCount"],
    "Architecture G fixture canonical-funding identity",
  );
  absolutePath(canonicalFunding.path, "fixtureCreation.canonicalFunding.path");
  const canonicalFundingSha256 = hash(
    canonicalFunding.sha256,
    "fixtureCreation.canonicalFunding.sha256",
  );
  positiveSafeInteger(
    canonicalFunding.entryCount,
    "fixtureCreation.canonicalFunding.entryCount",
  );
  if (
    artifact.fixtureCreated !== true ||
    fixturePath !== expectedPath ||
    marker !== expectedRoot ||
    initialUtxoCount !== expectedCount ||
    aggregateEntryCount !== expectedCount ||
    aggregateEntryCount !== expectedAggregateEntryCount ||
    aggregateEncodedTupleBytes !== expectedAggregateEncodedTupleBytes ||
    canonicalFundingSha256 !== fundingMapSha256
  ) {
    throw new Error(
      "Architecture G fixture creation does not bind the candidate path, root, cardinality, payload aggregate, and canonical funding",
    );
  }
  return artifact as ArchitectureGFixtureCreationV1;
};

export const decodeArchitectureGCorpusFundingV1 = ({
  value,
  expectedCorpusSha256,
  expectedSliceSha256,
}: {
  readonly value: unknown;
  readonly expectedCorpusSha256: string;
  readonly expectedSliceSha256: string;
}): ArchitectureGCorpusFundingV1 => {
  hash(expectedCorpusSha256, "expectedCorpusSha256");
  hash(expectedSliceSha256, "expectedSliceSha256");
  const funding = exactRecord(
    value,
    ["schemaVersion", "corpusSha256", "sliceSha256", "entries"],
    "Architecture G corpus funding",
  );
  if (
    funding.schemaVersion !== "midgard-architecture-g-corpus-funding-v1" ||
    funding.corpusSha256 !== expectedCorpusSha256 ||
    funding.sliceSha256 !== expectedSliceSha256 ||
    !Array.isArray(funding.entries) ||
    funding.entries.length === 0
  ) {
    throw new Error("Architecture G corpus funding identity is invalid");
  }
  const walletIds = new Set<string>();
  const outrefs = new Set<string>();
  for (const [index, value] of funding.entries.entries()) {
    const entry = exactRecord(
      value,
      ["walletId", "outref", "outputCbor"],
      `Architecture G funding entry ${index.toString()}`,
    );
    const walletId = nonEmptyString(
      entry.walletId,
      `funding.entries[${index.toString()}].walletId`,
    );
    const outref = nonEmptyString(
      entry.outref,
      `funding.entries[${index.toString()}].outref`,
    );
    const outputCbor = nonEmptyString(
      entry.outputCbor,
      `funding.entries[${index.toString()}].outputCbor`,
      1_048_576,
    );
    if (
      walletIds.has(walletId) ||
      outrefs.has(outref) ||
      outref !== outref.toLowerCase() ||
      outputCbor !== outputCbor.toLowerCase() ||
      !/^[0-9a-f]{64}#(?:0|[1-9]\d*)$/u.test(outref) ||
      outputCbor.length % 2 !== 0 ||
      outputCbor.length > 1_048_576 ||
      Buffer.from(outputCbor, "hex").toString("hex") !== outputCbor
    ) {
      throw new Error(
        `Architecture G funding entry ${index.toString()} is invalid or duplicated`,
      );
    }
    walletIds.add(walletId);
    outrefs.add(outref);
  }
  return funding as ArchitectureGCorpusFundingV1;
};

export const validateArchitectureGCommitCandidateSeedResultV1 = ({
  value,
  expectedDatabaseName,
  expectedCorpusSliceSha256,
  expectedTransactionCount,
}: {
  readonly value: unknown;
  readonly expectedDatabaseName: string;
  readonly expectedCorpusSliceSha256: string;
  readonly expectedTransactionCount: number;
}): ArchitectureGCommitCandidateSeedResultV1 => {
  const result = exactRecord(
    value,
    [
      "schemaVersion",
      "databaseName",
      "corpusSliceSha256",
      "mempoolTxCount",
      "fundingCount",
      "terminalLedgerCount",
      "deltaCount",
    ],
    "Architecture G commit-candidate seed result",
  );
  const expectedCount = positiveSafeInteger(
    expectedTransactionCount,
    "expectedTransactionCount",
  );
  const fundingCount = positiveSafeInteger(
    result.fundingCount,
    "seedResult.fundingCount",
  );
  const terminalLedgerCount = positiveSafeInteger(
    result.terminalLedgerCount,
    "seedResult.terminalLedgerCount",
  );
  if (
    result.schemaVersion !==
      "midgard-architecture-g-commit-candidate-seed-result-v1" ||
    typeof expectedDatabaseName !== "string" ||
    !/^midgard_phase3_arch_g_[a-z0-9_]+$/u.test(expectedDatabaseName) ||
    result.databaseName !== expectedDatabaseName ||
    result.corpusSliceSha256 !==
      hash(expectedCorpusSliceSha256, "expectedCorpusSliceSha256") ||
    result.mempoolTxCount !== expectedCount ||
    result.deltaCount !== expectedCount ||
    result.fundingCount !== fundingCount ||
    result.terminalLedgerCount !== terminalLedgerCount
  ) {
    throw new Error("Architecture G commit-candidate seed result is invalid");
  }
  return result as ArchitectureGCommitCandidateSeedResultV1;
};
