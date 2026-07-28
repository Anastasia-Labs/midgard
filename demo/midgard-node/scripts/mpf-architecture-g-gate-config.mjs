import { lstatSync, readFileSync, readdirSync } from "node:fs";
import { createHash } from "node:crypto";
import { isAbsolute, resolve } from "node:path";

import {
  loadPhase1FormalBindingSync,
  PHASE1_FORMAL_GENERATION_RESULT_SCHEMA,
  sha256FileSync,
} from "./phase1-formal-identity.mjs";

export const ARCHITECTURE_G_FORMAL_GATE_CONFIG = Object.freeze({
  "50k": Object.freeze({ runs: 20, transactions: 50_000 }),
  growth: Object.freeze({ runs: 3, transactions: 10_000 }),
});

const ARCHITECTURE_G_SOURCE_FILES = Object.freeze([
  "../pnpm-lock.yaml",
  "../lucid-midgard/package.json",
  "../midgard-core/package.json",
  "../midgard-sdk/package.json",
  "../midgard-validation/package.json",
  ".env.example",
  "Dockerfile",
  "docker-compose.yaml",
  "package.json",
  "native/mpf-event-flat-wasm/Cargo.lock",
  "native/mpf-event-flat-wasm/Cargo.toml",
  "tsconfig.json",
  "tsup.config.ts",
]);

const ARCHITECTURE_G_SOURCE_DIRECTORIES = Object.freeze([
  "src",
  "scripts",
  "native/mpf-event-flat-wasm/src",
  "../patches",
  "../lucid-midgard/src",
  "../midgard-core/src",
  "../midgard-sdk/src",
  "../midgard-validation/src",
]);

const requireExactObjectKeys = (value, keys, label) => {
  if (
    value === null ||
    typeof value !== "object" ||
    Array.isArray(value) ||
    JSON.stringify(Object.keys(value).sort()) !==
      JSON.stringify([...keys].sort())
  ) {
    throw new Error(`${label} must contain exactly: ${keys.join(", ")}`);
  }
  return value;
};

const isCanonicalTimestamp = (value) =>
  typeof value === "string" &&
  /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/u.test(value) &&
  new Date(value).toISOString() === value;

const isNonNegativeFiniteNumber = (value) =>
  Number.isFinite(value) && value >= 0;

const isNonNegativeSafeInteger = (value) =>
  Number.isSafeInteger(value) && value >= 0;

const regularFilesUnder = (cwd, path) => {
  const resolvedPath = resolve(cwd, path);
  let root;
  try {
    root = lstatSync(resolvedPath);
  } catch (cause) {
    throw new Error(
      `Architecture G source scope directory is missing or unreadable: ${path}`,
      { cause },
    );
  }
  if (!root.isDirectory()) {
    throw new Error(
      `Architecture G source scope traversal root must be a real directory: ${path}`,
    );
  }
  return readdirSync(resolvedPath, { withFileTypes: true }).flatMap((entry) => {
    const entryPath = `${path}/${entry.name}`;
    if (entry.isDirectory()) return regularFilesUnder(cwd, entryPath);
    if (entry.isFile()) return [entryPath];
    throw new Error(
      `Architecture G source scope contains unsupported filesystem entry: ${entryPath}`,
    );
  });
};

export const discoverArchitectureGSourceFiles = ({
  cwd = process.cwd(),
  fixedFiles = ARCHITECTURE_G_SOURCE_FILES,
  directories = ARCHITECTURE_G_SOURCE_DIRECTORIES,
} = {}) =>
  [
    ...fixedFiles,
    ...directories.flatMap((path) => regularFilesUnder(cwd, path)),
  ].sort();

export const validateArchitectureGSourceFileList = ({ expected, current }) => {
  const validList = (value) =>
    Array.isArray(value) &&
    value.length > 0 &&
    value.every((path) => typeof path === "string" && path.length > 0) &&
    new Set(value).size === value.length;
  if (
    !validList(expected) ||
    !validList(current) ||
    JSON.stringify([...expected].sort()) !== JSON.stringify([...current].sort())
  ) {
    throw new Error("Architecture G root/candidate source file scope mismatch");
  }
  return [...current].sort();
};

export const validateArchitectureGFixtureCreationEvidence = ({
  artifact,
  expectedFixturePath,
  expectedMarker,
  expectedUtxos,
}) => {
  requireExactObjectKeys(
    artifact,
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
  const aggregate = requireExactObjectKeys(
    artifact.utxoPayloadAggregate,
    ["entryCount", "encodedTupleBytes"],
    "Architecture G fixture payload aggregate",
  );
  const diagnosticKeys = [
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
  ];
  requireExactObjectKeys(
    artifact.diagnostics,
    diagnosticKeys,
    "Architecture G fixture diagnostics",
  );
  if (
    Object.entries(artifact.diagnostics).some(([field, value]) =>
      field.endsWith("Ms")
        ? !isNonNegativeFiniteNumber(value)
        : !isNonNegativeSafeInteger(value),
    )
  ) {
    throw new Error("Architecture G fixture diagnostics are invalid");
  }
  if (artifact.canonicalFunding !== null) {
    requireExactObjectKeys(
      artifact.canonicalFunding,
      ["path", "sha256", "entryCount"],
      "Architecture G fixture canonical-funding identity",
    );
    if (
      !isCanonicalAbsolutePath(artifact.canonicalFunding.path) ||
      !isHash(artifact.canonicalFunding.sha256) ||
      !isPositiveSafeInteger(artifact.canonicalFunding.entryCount)
    ) {
      throw new Error(
        "Architecture G fixture canonical-funding identity is invalid",
      );
    }
  }
  if (
    artifact?.fixtureCreated !== true ||
    !isCanonicalAbsolutePath(expectedFixturePath) ||
    artifact.fixturePath !== expectedFixturePath ||
    !isCanonicalAbsolutePath(artifact.fixturePath) ||
    !isHash(expectedMarker) ||
    artifact.marker !== expectedMarker ||
    !isPositiveSafeInteger(expectedUtxos) ||
    artifact.initialUtxoCount !== expectedUtxos ||
    aggregate.entryCount !== expectedUtxos ||
    !isPositiveSafeInteger(aggregate.encodedTupleBytes) ||
    !Number.isFinite(artifact.durationMs) ||
    artifact.durationMs <= 0
  ) {
    throw new Error(
      `Fixture creation evidence does not bind path, marker, cardinality, and payload aggregate for ${expectedUtxos.toString()} UTxOs`,
    );
  }
  return aggregate;
};

export const validateArchitectureGCrossGateSourceIdentity = ({
  expected,
  current,
}) => {
  const fields = ["gitHead", "sourceSha256", "diffSha256", "gitStatusSha256"];
  requireExactObjectKeys(expected, fields, "Expected source identity");
  requireExactObjectKeys(current, fields, "Current source identity");
  for (const field of fields) {
    const expectedValue = expected[field];
    const validIdentity =
      field === "gitHead"
        ? /^(?:[0-9a-f]{40}|[0-9a-f]{64})$/u.test(expectedValue)
        : /^[0-9a-f]{64}$/u.test(expectedValue);
    if (!validIdentity || current?.[field] !== expectedValue) {
      throw new Error(
        `Architecture G root/candidate source identity mismatch: ${field}`,
      );
    }
  }
  return current;
};

export const validateArchitectureGCrossGateFixtureIdentity = ({
  rootGateGroup,
  fixtureBefore,
  fixtureSize,
}) => {
  const expected = rootGateGroup?.fixtureAfter;
  if (
    rootGateGroup?.initialUtxos !== fixtureSize ||
    typeof expected?.path !== "string" ||
    expected.path.length === 0 ||
    fixtureBefore?.path !== expected.path ||
    !isHash(expected?.marker) ||
    fixtureBefore?.marker !== expected.marker ||
    !isHash(expected?.logicalSha256) ||
    fixtureBefore?.logicalSha256 !== expected.logicalSha256 ||
    expected?.records !== fixtureSize + 1 ||
    fixtureBefore?.records !== expected.records
  ) {
    throw new Error(
      `Architecture G root/candidate fixture identity mismatch at ${fixtureSize.toString()} UTxOs`,
    );
  }
  return fixtureBefore;
};

const completeRootTuple = (result) => ({
  utxoRoot: result?.utxoRoot,
  rawTxRoot: result?.rawTxRoot,
  txRoot: result?.txRoot,
  transitionTraceRoot: result?.transitionTraceRoot,
  eventToStepRoot: result?.eventToStepRoot,
  depositsRoot: result?.depositsRoot,
  withdrawalsRoot: result?.withdrawalsRoot,
  forcedTransactionsRoot: result?.forcedTransactionsRoot,
  transitionRoots: result?.transitionRoots,
});

const jsonEqual = (left, right) =>
  JSON.stringify(left) === JSON.stringify(right);

const isHash = (value) =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);

const isCanonicalAbsolutePath = (value) =>
  typeof value === "string" &&
  value.length > 0 &&
  isAbsolute(value) &&
  resolve(value) === value;

const isNonEmptyString = (value) =>
  typeof value === "string" && value.trim().length > 0;

const jsonFile = (path, label) => {
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch (cause) {
    throw new Error(`Unable to read ${label} ${path}`, { cause });
  }
};

export const validateArchitectureGPhase1FormalBindingIdentity = (identity) => {
  requireExactObjectKeys(
    identity,
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
    "Architecture G Phase 1 formal binding identity",
  );
  requireExactObjectKeys(
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
  requireExactObjectKeys(
    identity.generationResult,
    ["path", "sha256", "schemaVersion"],
    "Architecture G Phase 1 generation-result identity",
  );
  requireExactObjectKeys(
    identity.harness,
    ["scenarioId", "engineId"],
    "Architecture G Phase 1 harness identity",
  );
  if (
    identity?.schemaVersion !==
      "midgard-architecture-g-phase1-formal-binding-identity-v1" ||
    !isCanonicalAbsolutePath(identity.path) ||
    !isHash(identity.sha256) ||
    !isNonEmptyString(identity.deploymentManifestId) ||
    !isNonEmptyString(identity.nodeImageId) ||
    !isNonEmptyString(identity.nodeContainerId) ||
    !isHash(identity.walletSetSha256) ||
    !isHash(identity.fundingSetSha256) ||
    !isCanonicalAbsolutePath(identity.corpus?.path) ||
    !isCanonicalAbsolutePath(identity.corpus?.indexPath) ||
    !isCanonicalAbsolutePath(identity.corpus?.manifestPath) ||
    !isNonEmptyString(identity.corpus?.sliceId) ||
    !isHash(identity.corpus?.corpusSha256) ||
    !isHash(identity.corpus?.indexSha256) ||
    !isHash(identity.corpus?.manifestSha256) ||
    !isCanonicalAbsolutePath(identity.generationResult?.path) ||
    !isHash(identity.generationResult?.sha256) ||
    identity.generationResult?.schemaVersion !==
      PHASE1_FORMAL_GENERATION_RESULT_SCHEMA ||
    !isHash(identity.harness?.scenarioId) ||
    !isHash(identity.harness?.engineId)
  ) {
    throw new Error(
      "Architecture G Phase 1 formal binding identity is invalid",
    );
  }
  return identity;
};

export const captureArchitectureGPhase1FormalBindingIdentity = ({
  bindingPath,
  bindingSha256,
  cwd = process.cwd(),
}) => {
  if (!isCanonicalAbsolutePath(bindingPath)) {
    throw new Error(
      "Architecture G requires an explicit canonical absolute Phase 1 formal binding path",
    );
  }
  if (!isHash(bindingSha256)) {
    throw new Error(
      "Architecture G requires an explicit lowercase Phase 1 formal binding SHA-256",
    );
  }
  const binding = loadPhase1FormalBindingSync(bindingPath);
  if (binding.sha256 !== bindingSha256) {
    throw new Error("Architecture G Phase 1 formal binding SHA-256 mismatch");
  }
  const document = binding.document;
  for (const [path, expectedSha256, label] of [
    [document.corpus.path, document.corpus.corpusSha256, "corpus"],
    [document.corpus.indexPath, document.corpus.indexSha256, "corpus index"],
    [
      document.corpus.manifestPath,
      document.corpus.manifestSha256,
      "corpus manifest",
    ],
    [
      document.generationResult.path,
      document.generationResult.sha256,
      "generation result",
    ],
  ]) {
    if (sha256FileSync(path) !== expectedSha256) {
      throw new Error(`Architecture G Phase 1 ${label} SHA-256 mismatch`);
    }
  }
  const manifest = jsonFile(document.corpus.manifestPath, "Phase 1 manifest");
  if (
    manifest.files?.corpus?.sha256 !== document.corpus.corpusSha256 ||
    manifest.files?.index?.sha256 !== document.corpus.indexSha256 ||
    manifest.walletSetIdentity?.walletSetSha256 !== document.walletSetSha256 ||
    manifest.walletSetIdentity?.fundingSetSha256 !== document.fundingSetSha256
  ) {
    throw new Error(
      "Architecture G Phase 1 manifest identity does not match the formal binding",
    );
  }
  const generationResult = jsonFile(
    document.generationResult.path,
    "Phase 1 generation result",
  );
  if (
    generationResult.schemaVersion !== PHASE1_FORMAL_GENERATION_RESULT_SCHEMA ||
    generationResult.verified?.corpusSha256 !== document.corpus.corpusSha256 ||
    generationResult.verified?.indexSha256 !== document.corpus.indexSha256 ||
    generationResult.verified?.walletSetIdentity?.walletSetSha256 !==
      document.walletSetSha256 ||
    generationResult.verified?.walletSetIdentity?.fundingSetSha256 !==
      document.fundingSetSha256
  ) {
    throw new Error(
      "Architecture G Phase 1 generation result identity does not match the formal binding",
    );
  }
  const currentHarness = {
    scenarioId: sha256FileSync(resolve(cwd, "scripts/benchmark-scenario.mjs")),
    engineId: sha256FileSync(
      resolve(cwd, "scripts/throughput-valid-stress.mjs"),
    ),
  };
  if (
    currentHarness.scenarioId !== document.harness.scenarioId ||
    currentHarness.engineId !== document.harness.engineId
  ) {
    throw new Error(
      "Architecture G Phase 1 formal binding uses a stale harness identity",
    );
  }
  return validateArchitectureGPhase1FormalBindingIdentity({
    schemaVersion: "midgard-architecture-g-phase1-formal-binding-identity-v1",
    path: binding.path,
    sha256: binding.sha256,
    deploymentManifestId: document.deploymentManifestId,
    nodeImageId: document.nodeImageId,
    nodeContainerId: document.nodeContainerId,
    walletSetSha256: document.walletSetSha256,
    fundingSetSha256: document.fundingSetSha256,
    corpus: document.corpus,
    generationResult: {
      ...document.generationResult,
      schemaVersion: generationResult.schemaVersion,
    },
    harness: currentHarness,
  });
};

export const validateArchitectureGRuntimeIdentity = ({
  identity,
  expectedVersion,
  expectedExecutableSha256,
}) => {
  requireExactObjectKeys(
    identity,
    ["schemaVersion", "version", "execPath", "executableSha256"],
    "Architecture G runtime identity",
  );
  if (
    identity?.schemaVersion !== "midgard-architecture-g-runtime-identity-v1" ||
    !isNonEmptyString(identity.version) ||
    !isCanonicalAbsolutePath(identity.execPath) ||
    !isHash(identity.executableSha256)
  ) {
    throw new Error("Architecture G runtime identity is invalid");
  }
  if (!isNonEmptyString(expectedVersion) || !isHash(expectedExecutableSha256)) {
    throw new Error(
      "Architecture G runtime identity must be pinned by version and executable SHA-256",
    );
  }
  if (
    identity.version !== expectedVersion ||
    identity.executableSha256 !== expectedExecutableSha256
  ) {
    throw new Error("Architecture G pinned runtime identity mismatch");
  }
  return identity;
};

export const captureArchitectureGRuntimeIdentity = ({
  expectedVersion,
  expectedExecutableSha256,
}) =>
  validateArchitectureGRuntimeIdentity({
    identity: {
      schemaVersion: "midgard-architecture-g-runtime-identity-v1",
      version: process.version,
      execPath: resolve(process.execPath),
      executableSha256: createHash("sha256")
        .update(readFileSync(process.execPath))
        .digest("hex"),
    },
    expectedVersion,
    expectedExecutableSha256,
  });

export const validateArchitectureGCrossGateEvidenceIdentity = ({
  expected,
  current,
  label,
}) => {
  if (JSON.stringify(expected) !== JSON.stringify(current)) {
    throw new Error(`Architecture G root/candidate ${label} identity mismatch`);
  }
  return current;
};

const isPositiveSafeInteger = (value) =>
  Number.isSafeInteger(value) && value > 0;

const ARCHITECTURE_G_OWNER_DIAGNOSTIC_KEYS = Object.freeze([
  "ownerEpoch",
  "durableRoot",
  "residentNodes",
  "residentEdges",
  "residentBytes",
  "activeGenerations",
  "generatedNodes",
  "generatedBytes",
  "rssBytes",
  "peakRssBytes",
  "childRestarts",
]);

const validateArchitectureGOwnerDiagnostics = (owner, label) => {
  requireExactObjectKeys(owner, ARCHITECTURE_G_OWNER_DIAGNOSTIC_KEYS, label);
  requireExactObjectKeys(owner.ownerEpoch, ["type", "data"], `${label} epoch`);
  if (
    owner.ownerEpoch.type !== "Buffer" ||
    !Array.isArray(owner.ownerEpoch.data) ||
    owner.ownerEpoch.data.length !== 16 ||
    !owner.ownerEpoch.data.every(
      (byte) => Number.isInteger(byte) && byte >= 0 && byte <= 255,
    ) ||
    !isHash(owner.durableRoot) ||
    !ARCHITECTURE_G_OWNER_DIAGNOSTIC_KEYS.slice(2).every((field) =>
      isNonNegativeSafeInteger(owner[field]),
    )
  ) {
    throw new Error(`${label} is invalid`);
  }
  return owner;
};

const validateArchitectureGRootGateResultShape = (result) => {
  requireExactObjectKeys(
    result,
    [
      "engine",
      "transactionCount",
      "initialUtxoCount",
      "workloadSha256",
      "canonicalCorpusSlice",
      "canonicalFunding",
      "levelBackedInitialView",
      "reusedLevelFixture",
      "ledgerOpCount",
      "startupMs",
      "durationMs",
      "buildPlusCaptureMs",
      "phaseMs",
      "utxoRoot",
      "rawTxRoot",
      "txRoot",
      "transitionTraceRoot",
      "eventToStepRoot",
      "depositsRoot",
      "withdrawalsRoot",
      "forcedTransactionsRoot",
      "transitionRoots",
      "nativePhaseMs",
      "pathHydration",
      "confirmedLedgerFullScans",
      "binarySha256",
      "cpuAffinity",
      "ownerBefore",
      "ownerAfter",
      "probePath",
      "probeSha256",
    ],
    "Architecture G root-gate result",
  );
  for (const [value, keys, label] of [
    [
      result.canonicalCorpusSlice,
      ["path", "sha256", "rowCount"],
      "Architecture G result corpus slice",
    ],
    [
      result.canonicalFunding,
      ["path", "sha256", "entryCount"],
      "Architecture G result funding identity",
    ],
    [
      result.phaseMs,
      [
        "transactionSourceRoot",
        "transitionTraceBuild",
        "transactionMpfApply",
        "auxiliaryRoots",
      ],
      "Architecture G result phase timings",
    ],
    [
      result.nativePhaseMs,
      [
        "validation",
        "eventLogEncode",
        "ownerApply",
        "ownerProofArena",
        "ownerMutation",
        "memberAssembly",
        "retainedRoots",
      ],
      "Architecture G result native phase timings",
    ],
    [
      result.pathHydration,
      [
        "prefetchMs",
        "uniquePaths",
        "nodesRequested",
        "hydrationHits",
        "hydrationMisses",
        "loadedNodes",
        "maxInFlight",
        "maxBatchKeys",
        "maxFrontierPaths",
        "retainedBytesEstimate",
        "chunkCount",
        "checkpointMs",
        "authenticationMs",
        "materializeMs",
        "collapseMs",
        "checkpointSerializedNodes",
        "checkpointSerializedBytes",
        "verifiedUpperNodes",
        "retainedUpperNodes",
        "collapsedNodes",
        "peakDecodedNodes",
      ],
      "Architecture G result path-hydration diagnostics",
    ],
  ]) {
    requireExactObjectKeys(value, keys, label);
  }
  for (const [owner, label] of [
    [result.ownerBefore, "Architecture G owner-before diagnostics"],
    [result.ownerAfter, "Architecture G owner-after diagnostics"],
  ]) {
    validateArchitectureGOwnerDiagnostics(owner, label);
  }
  const hydrationTimingFields = new Set([
    "prefetchMs",
    "checkpointMs",
    "authenticationMs",
    "materializeMs",
    "collapseMs",
  ]);
  if (
    Object.entries(result.pathHydration).some(([field, value]) =>
      hydrationTimingFields.has(field)
        ? !isNonNegativeFiniteNumber(value)
        : !isNonNegativeSafeInteger(value),
    )
  ) {
    throw new Error(
      "Architecture G path-hydration diagnostics contain an invalid value",
    );
  }
  if (!Array.isArray(result.transitionRoots)) {
    throw new Error("Architecture G transition roots must be an array");
  }
  for (const transition of result.transitionRoots) {
    requireExactObjectKeys(
      transition,
      ["pre", "post"],
      "Architecture G transition-root pair",
    );
  }
  return result;
};

const percentile = (values, quantile) => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.max(0, Math.ceil(sorted.length * quantile) - 1)];
};

export const validateArchitectureGCanonicalCorpusIdentity = ({
  canonicalCorpus,
  phase1FormalBinding,
  transactions,
}) => {
  requireExactObjectKeys(
    canonicalCorpus,
    [
      "corpusPath",
      "manifestPath",
      "manifestSha256",
      "corpusSha256",
      "indexPath",
      "indexSha256",
      "verificationPath",
      "verificationSha256",
      "corpusManifestRowCount",
      "parentSliceId",
      "parentSliceRowsSeen",
      "parentSliceChainCount",
      "verifiedCorpusChainCount",
      "sliceChainsContiguous",
      "chainsCrossSliceBoundaries",
      "selectionAlgorithm",
      "sourceCorpusRowRange",
      "sourceSliceOrdinalRange",
      "completeChainCount",
      "finalChainPrefixLength",
      "fundingRootOutrefs",
      "fundingRoots",
      "fundingRootsSha256",
      "fundingMapPath",
      "fundingMapSha256",
      "fundingEntryCount",
      "slicePath",
      "sliceSha256",
      "sliceRowCount",
    ],
    "Architecture G canonical corpus identity",
  );
  requireExactObjectKeys(
    canonicalCorpus.sourceCorpusRowRange,
    ["start", "end"],
    "Architecture G corpus source-row range",
  );
  requireExactObjectKeys(
    canonicalCorpus.sourceSliceOrdinalRange,
    ["start", "end"],
    "Architecture G corpus slice-ordinal range",
  );
  const phase1Corpus = phase1FormalBinding.corpus;
  const phase1GenerationResult = phase1FormalBinding.generationResult;
  if (!Array.isArray(canonicalCorpus.fundingRoots)) {
    throw new Error("Architecture G canonical funding roots must be an array");
  }
  for (const fundingRoot of canonicalCorpus.fundingRoots) {
    requireExactObjectKeys(
      fundingRoot,
      ["walletId", "outref"],
      "Architecture G canonical funding root",
    );
  }
  const fundingWalletIds = canonicalCorpus.fundingRoots.map(
    (root) => root.walletId,
  );
  const fundingOutrefs = canonicalCorpus.fundingRoots.map(
    (root) => root.outref,
  );
  const expectedFundingRootsSha256 = createHash("sha256")
    .update(JSON.stringify(canonicalCorpus.fundingRoots))
    .digest("hex");
  if (
    !isPositiveSafeInteger(transactions) ||
    ![
      canonicalCorpus?.manifestSha256,
      canonicalCorpus?.corpusSha256,
      canonicalCorpus?.indexSha256,
      canonicalCorpus?.verificationSha256,
      canonicalCorpus?.fundingRootsSha256,
      canonicalCorpus?.fundingMapSha256,
      canonicalCorpus?.sliceSha256,
    ].every(isHash) ||
    ![
      canonicalCorpus?.corpusManifestRowCount,
      canonicalCorpus?.parentSliceRowsSeen,
      canonicalCorpus?.parentSliceChainCount,
      canonicalCorpus?.verifiedCorpusChainCount,
      canonicalCorpus?.completeChainCount,
      canonicalCorpus?.fundingEntryCount,
      canonicalCorpus?.sliceRowCount,
    ].every(isPositiveSafeInteger) ||
    canonicalCorpus.corpusManifestRowCount < canonicalCorpus.sliceRowCount ||
    canonicalCorpus.parentSliceRowsSeen < canonicalCorpus.sliceRowCount ||
    canonicalCorpus.sliceRowCount !== transactions ||
    !isCanonicalAbsolutePath(canonicalCorpus.corpusPath) ||
    !isCanonicalAbsolutePath(canonicalCorpus.manifestPath) ||
    !isCanonicalAbsolutePath(canonicalCorpus.indexPath) ||
    !isCanonicalAbsolutePath(canonicalCorpus.verificationPath) ||
    !isCanonicalAbsolutePath(canonicalCorpus.slicePath) ||
    !isCanonicalAbsolutePath(canonicalCorpus.fundingMapPath) ||
    canonicalCorpus.corpusPath !== phase1Corpus.path ||
    canonicalCorpus.corpusSha256 !== phase1Corpus.corpusSha256 ||
    canonicalCorpus.indexPath !== phase1Corpus.indexPath ||
    canonicalCorpus.indexSha256 !== phase1Corpus.indexSha256 ||
    canonicalCorpus.manifestPath !== phase1Corpus.manifestPath ||
    canonicalCorpus.manifestSha256 !== phase1Corpus.manifestSha256 ||
    canonicalCorpus.parentSliceId !== phase1Corpus.sliceId ||
    canonicalCorpus.verificationPath !== phase1GenerationResult.path ||
    canonicalCorpus.verificationSha256 !== phase1GenerationResult.sha256 ||
    canonicalCorpus.sliceChainsContiguous !== true ||
    canonicalCorpus.chainsCrossSliceBoundaries !== false ||
    canonicalCorpus.selectionAlgorithm !== "named-slice-file-order-prefix-v1" ||
    !isPositiveSafeInteger(canonicalCorpus.sourceCorpusRowRange.start) ||
    !isPositiveSafeInteger(canonicalCorpus.sourceCorpusRowRange.end) ||
    canonicalCorpus.sourceCorpusRowRange.end -
      canonicalCorpus.sourceCorpusRowRange.start +
      1 <
      transactions ||
    canonicalCorpus.sourceSliceOrdinalRange.start !== 1 ||
    canonicalCorpus.sourceSliceOrdinalRange.end !== transactions ||
    !isNonNegativeSafeInteger(canonicalCorpus.finalChainPrefixLength) ||
    canonicalCorpus.finalChainPrefixLength > transactions ||
    !Array.isArray(canonicalCorpus.fundingRootOutrefs) ||
    canonicalCorpus.fundingRootOutrefs.length !==
      canonicalCorpus.fundingEntryCount ||
    !canonicalCorpus.fundingRootOutrefs.every(isNonEmptyString) ||
    new Set(canonicalCorpus.fundingRootOutrefs).size !==
      canonicalCorpus.fundingRootOutrefs.length ||
    canonicalCorpus.fundingRoots.length !== canonicalCorpus.fundingEntryCount ||
    !fundingWalletIds.every(isNonEmptyString) ||
    !fundingOutrefs.every(
      (outref) =>
        typeof outref === "string" &&
        /^[0-9a-f]{64}#(?:0|[1-9]\d*)$/u.test(outref),
    ) ||
    new Set(fundingWalletIds).size !== fundingWalletIds.length ||
    new Set(fundingOutrefs).size !== fundingOutrefs.length ||
    !jsonEqual(fundingOutrefs, canonicalCorpus.fundingRootOutrefs) ||
    canonicalCorpus.fundingRootsSha256 !== expectedFundingRootsSha256
  ) {
    throw new Error("Architecture G canonical corpus identity is invalid");
  }
  return {
    canonicalCorpus,
    canonicalSlice: {
      path: canonicalCorpus.slicePath,
      sha256: canonicalCorpus.sliceSha256,
      rowCount: canonicalCorpus.sliceRowCount,
    },
    canonicalFunding: {
      path: canonicalCorpus.fundingMapPath,
      sha256: canonicalCorpus.fundingMapSha256,
      entryCount: canonicalCorpus.fundingEntryCount,
    },
  };
};

export const validateArchitectureGCorpusPreparationV1 = ({
  artifact,
  transactions,
}) => {
  requireExactObjectKeys(
    artifact,
    [
      "schemaVersion",
      "formalGateEvidence",
      "phase1FormalBinding",
      "runtimeIdentity",
      "canonicalCorpus",
    ],
    "Architecture G corpus-preparation artifact",
  );
  validateArchitectureGPhase1FormalBindingIdentity(
    artifact.phase1FormalBinding,
  );
  validateArchitectureGRuntimeIdentity({
    identity: artifact.runtimeIdentity,
    expectedVersion: artifact.runtimeIdentity?.version,
    expectedExecutableSha256: artifact.runtimeIdentity?.executableSha256,
  });
  if (
    artifact.schemaVersion !== "midgard-architecture-g-corpus-preparation-v1" ||
    artifact.formalGateEvidence !== false
  ) {
    throw new Error("Architecture G corpus-preparation identity is invalid");
  }
  validateArchitectureGCanonicalCorpusIdentity({
    canonicalCorpus: artifact.canonicalCorpus,
    phase1FormalBinding: artifact.phase1FormalBinding,
    transactions,
  });
  return artifact;
};

export const validateArchitectureGRootGateSummary = ({
  summary,
  mode,
  runs,
  transactions,
  cpuSet,
}) => {
  requireExactObjectKeys(
    summary,
    [
      "schemaVersion",
      "formal",
      "profile",
      "requiredCardinality",
      "generatedAt",
      "mode",
      "freshProcessRunsPerFixture",
      "transactionCount",
      "phase1FormalBinding",
      "runtimeIdentity",
      "canonicalCorpus",
      "binaryPath",
      "binarySha256",
      "probePath",
      "probeSha256",
      "gitHead",
      "sourceSha256",
      "diffSha256",
      "gitStatusSha256",
      "gitStatusEntries",
      "sourceFiles",
      "cpuSet",
      "nodeOptions",
      "cgroup",
      "percentileMethod",
      "groups",
      "verdict",
    ],
    "Architecture G production root-gate summary",
  );
  requireExactObjectKeys(
    summary.requiredCardinality,
    ["runs", "transactions"],
    "Architecture G required cardinality",
  );
  validateArchitectureGPhase1FormalBindingIdentity(
    summary?.phase1FormalBinding,
  );
  validateArchitectureGRuntimeIdentity({
    identity: summary?.runtimeIdentity,
    expectedVersion: summary?.runtimeIdentity?.version,
    expectedExecutableSha256: summary?.runtimeIdentity?.executableSha256,
  });
  if (
    summary?.schemaVersion !==
      "midgard-architecture-g-production-root-gate-v1" ||
    summary.formal !== true ||
    summary.profile !== "formal" ||
    summary.mode !== mode ||
    !jsonEqual(
      summary.requiredCardinality,
      ARCHITECTURE_G_FORMAL_GATE_CONFIG[mode],
    ) ||
    summary.freshProcessRunsPerFixture !== runs ||
    summary.transactionCount !== transactions ||
    summary.cpuSet !== cpuSet ||
    !isCanonicalTimestamp(summary.generatedAt)
  ) {
    throw new Error("Architecture G root gate summary identity is invalid");
  }
  if (
    !isCanonicalAbsolutePath(summary.probePath) ||
    !isHash(summary.probeSha256) ||
    !isCanonicalAbsolutePath(summary.binaryPath) ||
    !isHash(summary.binarySha256)
  ) {
    throw new Error("Architecture G root gate executable identity is invalid");
  }
  requireExactObjectKeys(
    summary.cgroup,
    ["membership", "memoryMaxPath", "memoryMax"],
    "Architecture G root gate cgroup identity",
  );
  const statusEntries = summary.gitStatusEntries;
  const sourceFiles = summary.sourceFiles;
  const canonicalStatusBytes = Buffer.from(
    Array.isArray(statusEntries) && statusEntries.length > 0
      ? `${statusEntries.join("\0")}\0`
      : "",
  );
  if (
    !/^(?:[0-9a-f]{40}|[0-9a-f]{64})$/u.test(summary.gitHead) ||
    !isHash(summary.sourceSha256) ||
    !isHash(summary.diffSha256) ||
    !isHash(summary.gitStatusSha256) ||
    !Array.isArray(statusEntries) ||
    !statusEntries.every(
      (entry) =>
        typeof entry === "string" &&
        entry.length > 0 &&
        entry.length <= 4096 &&
        !entry.includes("\0"),
    ) ||
    createHash("sha256").update(canonicalStatusBytes).digest("hex") !==
      summary.gitStatusSha256 ||
    !Array.isArray(sourceFiles) ||
    sourceFiles.length === 0 ||
    !sourceFiles.every(
      (path) =>
        typeof path === "string" &&
        path.length > 0 &&
        path.length <= 4096 &&
        !path.includes("\0"),
    ) ||
    new Set(sourceFiles).size !== sourceFiles.length ||
    !jsonEqual(sourceFiles, [...sourceFiles].sort()) ||
    summary.nodeOptions !== "--max-old-space-size=4096" ||
    !isNonEmptyString(summary.cgroup.membership) ||
    !isNonEmptyString(summary.cgroup.memoryMaxPath) ||
    !isNonEmptyString(summary.cgroup.memoryMax) ||
    summary.percentileMethod !==
      "nearest-rank: sorted[max(0, ceil(N*q)-1)]; q=0.5 median, q=0.95 p95"
  ) {
    throw new Error("Architecture G root gate provenance is invalid");
  }
  const {
    canonicalCorpus,
    canonicalSlice: expectedCanonicalSlice,
    canonicalFunding: expectedCanonicalFunding,
  } = validateArchitectureGCanonicalCorpusIdentity({
    canonicalCorpus: summary.canonicalCorpus,
    phase1FormalBinding: summary.phase1FormalBinding,
    transactions,
  });
  const expectedSizes =
    mode === "50k" ? [1_000_000] : [100_000, 300_000, 1_000_000];
  if (
    !Array.isArray(summary.groups) ||
    summary.groups.length !== expectedSizes.length ||
    !jsonEqual(
      summary.groups.map((group) => group?.initialUtxos),
      expectedSizes,
    )
  ) {
    throw new Error("Architecture G root gate fixture groups are incomplete");
  }
  for (const group of summary.groups) {
    requireExactObjectKeys(
      group,
      [
        "initialUtxos",
        "fixtureCreation",
        "fixtureBefore",
        "fixtureAfter",
        "roots",
        "durationMs",
        "results",
      ],
      "Architecture G root-gate fixture group",
    );
    const fixture = group.fixtureBefore;
    const after = group.fixtureAfter;
    const creation = group.fixtureCreation;
    requireExactObjectKeys(
      creation,
      ["path", "sha256", "initialUtxoCount", "marker", "utxoPayloadAggregate"],
      "Architecture G fixture-creation identity",
    );
    requireExactObjectKeys(
      creation.utxoPayloadAggregate,
      ["entryCount", "encodedTupleBytes"],
      "Architecture G fixture payload aggregate",
    );
    for (const [value, label] of [
      [fixture, "Architecture G fixture-before identity"],
      [after, "Architecture G fixture-after identity"],
    ]) {
      requireExactObjectKeys(
        value,
        ["path", "directoryBytes", "logicalSha256", "records", "marker"],
        label,
      );
    }
    if (
      creation?.initialUtxoCount !== group.initialUtxos ||
      creation?.marker !== fixture?.marker ||
      creation?.utxoPayloadAggregate?.entryCount !== group.initialUtxos ||
      !Number.isSafeInteger(
        creation?.utxoPayloadAggregate?.encodedTupleBytes,
      ) ||
      creation.utxoPayloadAggregate.encodedTupleBytes <= 0 ||
      !isCanonicalAbsolutePath(creation?.path) ||
      !isHash(creation?.sha256) ||
      !isCanonicalAbsolutePath(fixture?.path) ||
      !isPositiveSafeInteger(fixture?.directoryBytes) ||
      !isHash(fixture?.marker) ||
      !isHash(fixture?.logicalSha256) ||
      fixture?.records !== group.initialUtxos + 1 ||
      after?.path !== fixture.path ||
      after?.directoryBytes !== fixture.directoryBytes ||
      fixture?.marker !== after?.marker ||
      fixture?.logicalSha256 !== after?.logicalSha256 ||
      fixture?.records !== after?.records
    ) {
      throw new Error(
        `Architecture G root gate fixture evidence is invalid at ${String(group.initialUtxos)}`,
      );
    }
    if (!Array.isArray(group.results) || group.results.length !== runs) {
      throw new Error(
        `Architecture G root gate run count is invalid at ${String(group.initialUtxos)}`,
      );
    }
    const expectedRoots = group.roots;
    requireExactObjectKeys(
      expectedRoots,
      [
        "utxoRoot",
        "rawTxRoot",
        "txRoot",
        "transitionTraceRoot",
        "eventToStepRoot",
        "depositsRoot",
        "withdrawalsRoot",
        "forcedTransactionsRoot",
        "transitionRoots",
      ],
      "Architecture G root-gate complete roots",
    );
    requireExactObjectKeys(
      group.durationMs,
      ["min", "median", "p95", "max"],
      "Architecture G root-gate duration aggregate",
    );
    if (
      ![
        expectedRoots?.utxoRoot,
        expectedRoots?.rawTxRoot,
        expectedRoots?.txRoot,
        expectedRoots?.transitionTraceRoot,
        expectedRoots?.eventToStepRoot,
        expectedRoots?.depositsRoot,
        expectedRoots?.withdrawalsRoot,
        expectedRoots?.forcedTransactionsRoot,
      ].every(isHash) ||
      !Array.isArray(expectedRoots?.transitionRoots) ||
      expectedRoots.transitionRoots.length !== transactions
    ) {
      throw new Error(
        `Architecture G root gate complete roots are invalid at ${String(group.initialUtxos)}`,
      );
    }
    for (const result of group.results) {
      validateArchitectureGRootGateResultShape(result);
      if (
        result?.engine !== "architecture_g" ||
        result?.probePath !== summary.probePath ||
        result?.probeSha256 !== summary.probeSha256 ||
        result?.binarySha256 !== summary.binarySha256 ||
        !jsonEqual(result?.canonicalCorpusSlice, expectedCanonicalSlice) ||
        !jsonEqual(result?.canonicalFunding, expectedCanonicalFunding) ||
        result?.cpuAffinity !== cpuSet ||
        result?.transactionCount !== transactions ||
        result?.initialUtxoCount !== group.initialUtxos ||
        result?.levelBackedInitialView !== true ||
        result?.reusedLevelFixture !== true ||
        !isPositiveSafeInteger(result?.ledgerOpCount) ||
        !isNonNegativeFiniteNumber(result?.startupMs) ||
        result?.confirmedLedgerFullScans !== 0 ||
        !Number.isFinite(result?.durationMs) ||
        result.durationMs <= 0 ||
        result.buildPlusCaptureMs !== result.durationMs ||
        !Object.values(result.phaseMs).every(isNonNegativeFiniteNumber) ||
        !Object.values(result.nativePhaseMs).every(isNonNegativeFiniteNumber) ||
        !Object.values(result.pathHydration).every(isNonNegativeFiniteNumber) ||
        !isHash(result.workloadSha256) ||
        !Array.isArray(result.transitionRoots) ||
        result.transitionRoots.length !== transactions ||
        !result.transitionRoots.every(
          (transition) => isHash(transition?.pre) && isHash(transition?.post),
        ) ||
        !isHash(result.ownerBefore?.durableRoot) ||
        result.ownerBefore.durableRoot !== fixture.marker ||
        result.ownerAfter?.durableRoot !== fixture.marker ||
        !jsonEqual(
          result.ownerBefore?.ownerEpoch,
          result.ownerAfter?.ownerEpoch,
        ) ||
        result.ownerBefore?.childRestarts !==
          result.ownerAfter?.childRestarts ||
        result.transitionRoots[0]?.pre !== result.ownerBefore?.durableRoot ||
        result.transitionRoots.at(-1)?.post !== result.utxoRoot ||
        !jsonEqual(completeRootTuple(result), expectedRoots)
      ) {
        throw new Error(
          `Architecture G root gate result evidence is invalid at ${String(group.initialUtxos)}`,
        );
      }
      for (let index = 1; index < result.transitionRoots.length; index += 1) {
        if (
          result.transitionRoots[index]?.pre !==
          result.transitionRoots[index - 1]?.post
        ) {
          throw new Error(
            `Architecture G root gate transition chain is invalid at ${String(group.initialUtxos)}`,
          );
        }
      }
    }
    const durations = group.results.map((result) => result.durationMs);
    const expectedDuration = {
      min: Math.min(...durations),
      median: percentile(durations, 0.5),
      p95: percentile(durations, 0.95),
      max: Math.max(...durations),
    };
    if (!jsonEqual(group.durationMs, expectedDuration)) {
      throw new Error(
        `Architecture G root gate duration evidence is invalid at ${String(group.initialUtxos)}`,
      );
    }
  }
  if (
    mode === "growth" &&
    (summary.groups.some((group) =>
      group.results.some((result) => !isHash(result?.workloadSha256)),
    ) ||
      new Set(
        summary.groups.flatMap((group) =>
          group.results.map((result) => result.workloadSha256),
        ),
      ).size !== 1)
  ) {
    throw new Error("Architecture G growth workload identity is invalid");
  }
  const expectedVerdict =
    mode === "50k"
      ? {
          pass: summary.groups[0].durationMs.p95 < 10_000,
          gate: "50k_complete_root_build_p95_under_10s",
          p95Ms: summary.groups[0].durationMs.p95,
          limitMs: 10_000,
        }
      : (() => {
          const medians = summary.groups.map(
            (group) => group.durationMs.median,
          );
          const minimumMedianMs = Math.min(...medians);
          const maximumMedianMs = Math.max(...medians);
          const maxMinSlopePercent =
            ((maximumMedianMs - minimumMedianMs) / minimumMedianMs) * 100;
          return {
            pass: maxMinSlopePercent <= 10,
            gate: "100k_300k_1m_max_min_build_slope_within_10_percent",
            maxMinSlopePercent,
            minimumMedianMs,
            maximumMedianMs,
            limitAbsolutePercent: 10,
          };
        })();
  requireExactObjectKeys(
    summary.verdict,
    mode === "50k"
      ? ["pass", "gate", "p95Ms", "limitMs"]
      : [
          "pass",
          "gate",
          "maxMinSlopePercent",
          "minimumMedianMs",
          "maximumMedianMs",
          "limitAbsolutePercent",
        ],
    "Architecture G root-gate verdict",
  );
  if (!expectedVerdict.pass || !jsonEqual(summary.verdict, expectedVerdict)) {
    throw new Error("Architecture G root gate verdict is invalid or failed");
  }
  return summary;
};

const positiveSafeInteger = (value, label) => {
  if (typeof value !== "string" || !/^[1-9]\d*$/u.test(value)) {
    throw new Error(`${label} must be a positive base-10 integer`);
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a positive safe integer`);
  }
  return parsed;
};

export const resolveArchitectureGGateConfig = ({
  mode,
  profile,
  runs,
  transactions,
}) => {
  if (mode !== "50k" && mode !== "growth") {
    throw new Error("Use --mode=50k or --mode=growth");
  }
  if (profile !== "formal" && profile !== "smoke") {
    throw new Error("Use --profile=formal or --profile=smoke");
  }
  const required = ARCHITECTURE_G_FORMAL_GATE_CONFIG[mode];
  const resolvedRuns = positiveSafeInteger(
    runs ?? required.runs.toString(),
    "--runs",
  );
  const resolvedTransactions = positiveSafeInteger(
    transactions ?? required.transactions.toString(),
    "--transactions",
  );
  if (
    profile === "formal" &&
    (resolvedRuns !== required.runs ||
      resolvedTransactions !== required.transactions)
  ) {
    throw new Error(
      `Formal ${mode} gate requires --runs=${required.runs.toString()} and --transactions=${required.transactions.toString()}; use --profile=smoke for reduced diagnostics`,
    );
  }
  return {
    mode,
    profile,
    formal: profile === "formal",
    runs: resolvedRuns,
    transactions: resolvedTransactions,
    required,
  };
};

export const validateArchitectureGCommitCandidateInputV1 = (input) => {
  requireExactObjectKeys(
    input,
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
  validateArchitectureGPhase1FormalBindingIdentity(input.phase1FormalBinding);
  validateArchitectureGRuntimeIdentity({
    identity: input.runtimeIdentity,
    expectedVersion: input.runtimeIdentity?.version,
    expectedExecutableSha256: input.runtimeIdentity?.executableSha256,
  });
  requireExactObjectKeys(
    input.baseUtxoPayloadAggregate,
    ["entryCount", "encodedTupleBytes"],
    "Architecture G candidate base UTxO aggregate",
  );
  requireExactObjectKeys(
    input.workerInput,
    ["data"],
    "Architecture G candidate worker input",
  );
  const data = requireExactObjectKeys(
    input.workerInput.data,
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
  const speculativeBuild = requireExactObjectKeys(
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
  const base = requireExactObjectKeys(
    speculativeBuild.base,
    ["headerHash", "utxosRoot", "blockEndTimeMs", "submittedTxHash"],
    "Architecture G candidate speculative base",
  );
  const watermarks = requireExactObjectKeys(
    speculativeBuild.watermarks,
    ["depositMs", "withdrawalMs", "txOrderMs", "refreshedAtMs"],
    "Architecture G candidate barrier watermarks",
  );
  const excludedFields = [
    "excludedMempoolTxIds",
    "excludedDepositEventIds",
    "excludedForcedTransactionEventIds",
    "excludedWithdrawalEventIds",
  ];
  if (
    input.schemaVersion !==
      "midgard-architecture-g-commit-candidate-input-v1" ||
    ![
      input.binarySha256,
      input.corpusSha256,
      input.corpusSliceSha256,
      input.fundingMapSha256,
      input.fixtureCreationSha256,
    ].every(isHash) ||
    ![
      input.levelPath,
      input.binaryPath,
      input.sidecarPath,
      input.fixtureCreationPath,
    ].every(isCanonicalAbsolutePath) ||
    !isPositiveSafeInteger(input.expectedTransactionCount) ||
    !isPositiveSafeInteger(input.fixtureInitialUtxoCount) ||
    input.baseUtxoPayloadAggregate.entryCount !==
      input.fixtureInitialUtxoCount ||
    !isPositiveSafeInteger(input.baseUtxoPayloadAggregate.encodedTupleBytes) ||
    input.corpusSha256 !== input.phase1FormalBinding.corpus.corpusSha256 ||
    data.availableConfirmedBlock !== "" ||
    data.availableLocalFinalizationBlock !== "" ||
    !isPositiveSafeInteger(data.currentBlockStartTimeMs) ||
    data.localFinalizationPending !== false ||
    !/^commit:[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u.test(
      data.ledgerStoreLeaseOwner,
    ) ||
    data.mempoolTxsCountSoFar !== 0 ||
    data.sizeOfProcessedTxsSoFar !== 0 ||
    data.stateQueueHasUnmergedTail !== true ||
    !isHash(base.submittedTxHash) ||
    base.headerHash !== base.submittedTxHash.slice(0, 56) ||
    !isHash(base.utxosRoot) ||
    base.blockEndTimeMs !== data.currentBlockStartTimeMs ||
    data.baseSnapshotId !==
      `architecture-g-candidate:${base.submittedTxHash}` ||
    !Object.values(watermarks).every(isPositiveSafeInteger) ||
    Math.max(
      watermarks.depositMs,
      watermarks.withdrawalMs,
      watermarks.txOrderMs,
    ) > watermarks.refreshedAtMs ||
    base.blockEndTimeMs >=
      Math.min(
        watermarks.depositMs,
        watermarks.withdrawalMs,
        watermarks.txOrderMs,
      ) ||
    excludedFields.some(
      (field) =>
        !Array.isArray(speculativeBuild[field]) ||
        speculativeBuild[field].length !== 0,
    )
  ) {
    throw new Error("Architecture G commit-candidate input is invalid");
  }
  return input;
};

export const validateCommitCandidateProbeResult = ({
  result,
  transactions,
  cpuSet,
  fixtureSize,
  inputPath,
  inputSha256,
  probePath,
  probeSha256,
  binarySha256,
}) => {
  requireExactObjectKeys(
    result,
    [
      "schemaVersion",
      "probePath",
      "probeSha256",
      "inputPath",
      "inputSha256",
      "expectedTransactionCount",
      "corpusSha256",
      "corpusSliceSha256",
      "fundingMapSha256",
      "fixtureCreationSha256",
      "fixtureInitialUtxoCount",
      "baseUtxoPayloadAggregate",
      "binarySha256",
      "cpuAffinity",
      "durationMs",
      "confirmedLedgerFullScans",
      "journalRowsBefore",
      "journalRowsAfter",
      "candidateConfig",
      "providerBoundaryAttempts",
      "submissionAttempts",
      "candidate",
      "ownerBefore",
      "ownerAfter",
    ],
    "Architecture G commit-candidate probe result",
  );
  requireExactObjectKeys(
    result.baseUtxoPayloadAggregate,
    ["entryCount", "encodedTupleBytes"],
    "Commit-candidate base UTxO payload aggregate",
  );
  requireExactObjectKeys(
    result.candidateConfig,
    [
      "mpfEngine",
      "scratchBuild",
      "payloadRootCheck",
      "parallelRoots",
      "costModel",
      "mempoolRetrievePageSize",
      "maxL2TxCount",
      "maxLedgerOpCount",
      "maxTransitionStepCount",
    ],
    "Commit-candidate configuration evidence",
  );
  requireExactObjectKeys(
    result.candidate,
    [
      "candidateId",
      "baseHeaderHash",
      "endTimeMs",
      "builtAtMs",
      "buildDurationMs",
      "invalidationKey",
      "watermarks",
      "expectedUserEventCounts",
      "expectedL2TransactionCount",
      "roots",
    ],
    "Commit-candidate summary",
  );
  requireExactObjectKeys(
    result.candidate.watermarks,
    ["depositMs", "withdrawalMs", "txOrderMs", "refreshedAtMs"],
    "Commit-candidate barrier watermarks",
  );
  requireExactObjectKeys(
    result.candidate.expectedUserEventCounts,
    ["deposits", "forcedTransactions", "withdrawals"],
    "Commit-candidate expected user-event counts",
  );
  const rootKeys = [
    "utxos",
    "rawTransactions",
    "transactions",
    "deposits",
    "forcedTransactions",
    "withdrawals",
    "transitionTrace",
    "eventToStep",
  ];
  requireExactObjectKeys(
    result.candidate.roots,
    rootKeys,
    "Commit-candidate roots",
  );
  validateArchitectureGOwnerDiagnostics(
    result.ownerBefore,
    "Commit-candidate owner-before diagnostics",
  );
  validateArchitectureGOwnerDiagnostics(
    result.ownerAfter,
    "Commit-candidate owner-after diagnostics",
  );
  if (
    result?.schemaVersion !== "midgard-architecture-g-commit-candidate-probe-v1"
  ) {
    throw new Error("Unsupported commit-candidate probe result schema");
  }
  if (
    result.expectedTransactionCount !== transactions ||
    result.candidate?.expectedL2TransactionCount !== transactions
  ) {
    throw new Error("Commit-candidate probe transaction count drifted");
  }
  if (result.cpuAffinity !== cpuSet) {
    throw new Error("Commit-candidate probe CPU affinity drifted");
  }
  if (
    result.inputPath !== inputPath ||
    !isCanonicalAbsolutePath(result.inputPath) ||
    !isHash(inputSha256) ||
    result.inputSha256 !== inputSha256
  ) {
    throw new Error("Commit-candidate probe input identity drifted");
  }
  if (
    result.probePath !== probePath ||
    !isCanonicalAbsolutePath(result.probePath) ||
    !isHash(probeSha256) ||
    result.probeSha256 !== probeSha256 ||
    !isHash(binarySha256) ||
    result.binarySha256 !== binarySha256
  ) {
    throw new Error("Commit-candidate executable identity drifted");
  }
  for (const field of [
    "corpusSha256",
    "corpusSliceSha256",
    "fundingMapSha256",
    "fixtureCreationSha256",
    "binarySha256",
  ]) {
    if (!/^[0-9a-f]{64}$/u.test(result[field] ?? "")) {
      throw new Error(`Commit-candidate probe ${field} is invalid`);
    }
  }
  if (
    result.fixtureInitialUtxoCount !== fixtureSize ||
    result.baseUtxoPayloadAggregate?.entryCount !== fixtureSize ||
    !Number.isSafeInteger(result.baseUtxoPayloadAggregate?.encodedTupleBytes) ||
    result.baseUtxoPayloadAggregate.encodedTupleBytes <= 0
  ) {
    throw new Error(
      "Commit-candidate probe fixture aggregate/cardinality drifted",
    );
  }
  const candidateConfig = result.candidateConfig;
  if (
    candidateConfig.mpfEngine !== "architecture_g" ||
    candidateConfig.scratchBuild !== "fromlist" ||
    candidateConfig.payloadRootCheck !== "off" ||
    candidateConfig.parallelRoots !== true ||
    candidateConfig.costModel !== "ewma" ||
    !isPositiveSafeInteger(candidateConfig.mempoolRetrievePageSize) ||
    candidateConfig.mempoolRetrievePageSize < transactions ||
    !isPositiveSafeInteger(candidateConfig.maxL2TxCount) ||
    candidateConfig.maxL2TxCount < transactions ||
    !isPositiveSafeInteger(candidateConfig.maxLedgerOpCount) ||
    candidateConfig.maxLedgerOpCount < transactions * 3 ||
    !isPositiveSafeInteger(candidateConfig.maxTransitionStepCount) ||
    candidateConfig.maxTransitionStepCount < transactions
  ) {
    throw new Error("Commit-candidate configuration evidence is invalid");
  }
  if (result.confirmedLedgerFullScans !== 0) {
    throw new Error("Commit-candidate probe performed a confirmed-ledger scan");
  }
  if (
    result.providerBoundaryAttempts !== 0 ||
    result.submissionAttempts !== result.providerBoundaryAttempts
  ) {
    throw new Error(
      "Commit-candidate probe crossed the provider/submission boundary",
    );
  }
  const candidate = result.candidate;
  const watermarkValues = Object.values(candidate.watermarks);
  const minimumWatermarkMs = Math.min(...watermarkValues);
  if (
    !/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u.test(
      candidate.candidateId,
    ) ||
    typeof candidate.baseHeaderHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(candidate.baseHeaderHash) ||
    !isPositiveSafeInteger(candidate.endTimeMs) ||
    !isPositiveSafeInteger(candidate.builtAtMs) ||
    !watermarkValues.every(isNonNegativeSafeInteger) ||
    !Object.values(candidate.expectedUserEventCounts).every(
      isNonNegativeSafeInteger,
    ) ||
    candidate.invalidationKey !==
      `${candidate.baseHeaderHash}:${candidate.endTimeMs.toString()}:${minimumWatermarkMs.toString()}`
  ) {
    throw new Error("Commit-candidate identity or barrier evidence is invalid");
  }
  if (result.journalRowsBefore !== 0 || result.journalRowsAfter !== 0) {
    throw new Error(
      "Commit-candidate probe requires a fresh empty pending journal and must leave it empty",
    );
  }
  if (!Number.isFinite(result.durationMs) || result.durationMs <= 0) {
    throw new Error("Commit-candidate probe duration is invalid");
  }
  if (
    !Number.isFinite(result.candidate?.buildDurationMs) ||
    result.candidate.buildDurationMs <= 0
  ) {
    throw new Error("Commit-candidate worker build duration is invalid");
  }
  if (
    result.ownerBefore.durableRoot !== result.ownerAfter.durableRoot ||
    !jsonEqual(result.ownerBefore.ownerEpoch, result.ownerAfter.ownerEpoch) ||
    result.ownerBefore.childRestarts !== result.ownerAfter.childRestarts
  ) {
    throw new Error("Commit-candidate native owner identity drifted");
  }
  const roots = result.candidate?.roots;
  if (rootKeys.some((field) => !isHash(roots[field]))) {
    throw new Error("Commit-candidate roots are invalid");
  }
  return roots;
};
