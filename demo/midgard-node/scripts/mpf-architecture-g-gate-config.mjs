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
  if (
    artifact?.fixtureCreated !== true ||
    artifact.fixturePath !== expectedFixturePath ||
    artifact.marker !== expectedMarker ||
    artifact.initialUtxoCount !== expectedUtxos ||
    artifact.utxoPayloadAggregate?.entryCount !== expectedUtxos ||
    !Number.isSafeInteger(artifact.utxoPayloadAggregate?.encodedTupleBytes) ||
    artifact.utxoPayloadAggregate.encodedTupleBytes <= 0
  ) {
    throw new Error(
      `Fixture creation evidence does not bind path, marker, cardinality, and payload aggregate for ${expectedUtxos.toString()} UTxOs`,
    );
  }
  return artifact.utxoPayloadAggregate;
};

export const validateArchitectureGCrossGateSourceIdentity = ({
  expected,
  current,
}) => {
  for (const field of [
    "gitHead",
    "sourceSha256",
    "diffSha256",
    "gitStatusSha256",
  ]) {
    if (
      typeof expected?.[field] !== "string" ||
      expected[field].length === 0 ||
      current?.[field] !== expected[field]
    ) {
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

const percentile = (values, quantile) => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.max(0, Math.ceil(sorted.length * quantile) - 1)];
};

export const validateArchitectureGRootGateSummary = ({
  summary,
  mode,
  runs,
  transactions,
  cpuSet,
}) => {
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
    summary.cpuSet !== cpuSet
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
  const canonicalCorpus = summary.canonicalCorpus;
  const phase1Corpus = summary.phase1FormalBinding.corpus;
  const phase1GenerationResult = summary.phase1FormalBinding.generationResult;
  if (
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
      canonicalCorpus?.finalChainPrefixLength,
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
    canonicalCorpus.verificationSha256 !== phase1GenerationResult.sha256
  ) {
    throw new Error(
      "Architecture G root gate canonical corpus identity is invalid",
    );
  }
  const expectedCanonicalSlice = {
    path: canonicalCorpus.slicePath,
    sha256: canonicalCorpus.sliceSha256,
    rowCount: canonicalCorpus.sliceRowCount,
  };
  const expectedCanonicalFunding = {
    path: canonicalCorpus.fundingMapPath,
    sha256: canonicalCorpus.fundingMapSha256,
    entryCount: canonicalCorpus.fundingEntryCount,
  };
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
    const fixture = group.fixtureBefore;
    const after = group.fixtureAfter;
    const creation = group.fixtureCreation;
    if (
      creation?.initialUtxoCount !== group.initialUtxos ||
      creation?.marker !== fixture?.marker ||
      creation?.utxoPayloadAggregate?.entryCount !== group.initialUtxos ||
      !Number.isSafeInteger(
        creation?.utxoPayloadAggregate?.encodedTupleBytes,
      ) ||
      creation.utxoPayloadAggregate.encodedTupleBytes <= 0 ||
      !isHash(creation?.sha256) ||
      !isHash(fixture?.marker) ||
      !isHash(fixture?.logicalSha256) ||
      fixture?.records !== group.initialUtxos + 1 ||
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
      if (
        result?.probePath !== summary.probePath ||
        result?.probeSha256 !== summary.probeSha256 ||
        result?.binarySha256 !== summary.binarySha256 ||
        !jsonEqual(result?.canonicalCorpusSlice, expectedCanonicalSlice) ||
        !jsonEqual(result?.canonicalFunding, expectedCanonicalFunding) ||
        result?.cpuAffinity !== cpuSet ||
        result?.transactionCount !== transactions ||
        result?.initialUtxoCount !== group.initialUtxos ||
        result?.confirmedLedgerFullScans !== 0 ||
        !Number.isFinite(result?.durationMs) ||
        result.durationMs <= 0 ||
        !Array.isArray(result.transitionRoots) ||
        result.transitionRoots.length !== transactions ||
        !result.transitionRoots.every(
          (transition) => isHash(transition?.pre) && isHash(transition?.post),
        ) ||
        !isHash(result.ownerBefore?.durableRoot) ||
        result.ownerBefore.durableRoot !== fixture.marker ||
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
    !isHash(inputSha256) ||
    result.inputSha256 !== inputSha256
  ) {
    throw new Error("Commit-candidate probe input identity drifted");
  }
  if (
    result.probePath !== probePath ||
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
  const roots = result.candidate?.roots;
  if (
    typeof roots !== "object" ||
    roots === null ||
    Object.values(roots).some(
      (root) => typeof root !== "string" || !/^[0-9a-f]{64}$/u.test(root),
    )
  ) {
    throw new Error("Commit-candidate roots are invalid");
  }
  return roots;
};
