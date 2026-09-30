import { lstatSync, readdirSync } from "node:fs";
import { isAbsolute, resolve } from "node:path";

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

export const ARCHITECTURE_G_SOURCE_DIRECTORIES = Object.freeze([
  "src",
  "scripts",
  "native/mpf-event-flat-wasm/src",
  "../patches",
  "../lucid-midgard/src",
  "../midgard-core/src",
  "../midgard-sdk/src",
  "../midgard-validation/src",
]);

export const requireExactObjectKeys = (value, keys, label) => {
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

export const isCanonicalTimestamp = (value) =>
  typeof value === "string" &&
  /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/u.test(value) &&
  new Date(value).toISOString() === value;

export const isNonNegativeFiniteNumber = (value) =>
  Number.isFinite(value) && value >= 0;

export const isNonNegativeSafeInteger = (value) =>
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

export const isHash = (value) =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);

export const isCanonicalAbsolutePath = (value) =>
  typeof value === "string" &&
  value.length > 0 &&
  value.length <= 4096 &&
  !value.includes("\0") &&
  isAbsolute(value) &&
  resolve(value) === value;

export const isPositiveSafeInteger = (value) =>
  Number.isSafeInteger(value) && value > 0;
