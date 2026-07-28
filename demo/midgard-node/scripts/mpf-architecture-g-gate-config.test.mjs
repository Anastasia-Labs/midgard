import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import {
  captureArchitectureGPhase1FormalBindingIdentity,
  captureArchitectureGRuntimeIdentity,
  discoverArchitectureGSourceFiles,
  resolveArchitectureGGateConfig,
  validateArchitectureGCommitCandidateInputV1,
  validateArchitectureGCorpusPreparationV1,
  validateArchitectureGCrossGateEvidenceIdentity,
  validateArchitectureGCrossGateFixtureIdentity,
  validateArchitectureGCrossGateSourceIdentity,
  validateArchitectureGFixtureCreationEvidence,
  validateArchitectureGRootGateSummary,
  validateArchitectureGRuntimeIdentity,
  validateArchitectureGSourceFileList,
  validateCommitCandidateProbeResult,
} from "./mpf-architecture-g-gate-config.mjs";

test("candidate gate requires exact root-gate source identity", () => {
  const identity = {
    gitHead: "11".repeat(20),
    sourceSha256: "22".repeat(32),
    diffSha256: "33".repeat(32),
    gitStatusSha256: "44".repeat(32),
  };
  assert.equal(
    validateArchitectureGCrossGateSourceIdentity({
      expected: identity,
      current: identity,
    }),
    identity,
  );
  for (const field of Object.keys(identity)) {
    assert.throws(() =>
      validateArchitectureGCrossGateSourceIdentity({
        expected: identity,
        current: { ...identity, [field]: "different" },
      }),
    );
  }
  assert.throws(() =>
    validateArchitectureGCrossGateSourceIdentity({
      expected: {},
      current: identity,
    }),
  );
  assert.throws(() =>
    validateArchitectureGCrossGateSourceIdentity({
      expected: identity,
      current: { ...identity, unknown: true },
    }),
  );
});

test("candidate gate binds the full run-local fixture identity before execution", () => {
  const fixtureSize = 1_000_000;
  const fixture = {
    path: "/fixtures/utxos-1000000-level",
    marker: "11".repeat(32),
    logicalSha256: "22".repeat(32),
    records: fixtureSize + 1,
  };
  const rootGateGroup = {
    initialUtxos: fixtureSize,
    fixtureAfter: fixture,
  };
  assert.equal(
    validateArchitectureGCrossGateFixtureIdentity({
      rootGateGroup,
      fixtureBefore: fixture,
      fixtureSize,
    }),
    fixture,
  );
  for (const mutate of [
    (value) => void (value.path = "/fixtures/different"),
    (value) => void (value.marker = "33".repeat(32)),
    (value) => void (value.logicalSha256 = "44".repeat(32)),
    (value) => void (value.records = fixtureSize),
  ]) {
    const invalid = structuredClone(fixture);
    mutate(invalid);
    assert.throws(() =>
      validateArchitectureGCrossGateFixtureIdentity({
        rootGateGroup,
        fixtureBefore: invalid,
        fixtureSize,
      }),
    );
  }
  assert.throws(() =>
    validateArchitectureGCrossGateFixtureIdentity({
      rootGateGroup: {
        ...rootGateGroup,
        fixtureAfter: { ...fixture, records: fixtureSize },
      },
      fixtureBefore: fixture,
      fixtureSize,
    }),
  );
});

test("candidate source discovery rejects an untracked file added after the root gate", () => {
  const cwd = mkdtempSync(join(tmpdir(), "midgard-architecture-g-source-"));
  try {
    mkdirSync(join(cwd, "scripts"));
    writeFileSync(join(cwd, "package.json"), "{}\n");
    writeFileSync(join(cwd, "scripts/existing.mjs"), "export {};\n");
    const options = {
      cwd,
      fixedFiles: ["package.json"],
      directories: ["scripts"],
    };
    const expected = discoverArchitectureGSourceFiles(options);
    assert.deepEqual(
      validateArchitectureGSourceFileList({ expected, current: expected }),
      ["package.json", "scripts/existing.mjs"],
    );

    writeFileSync(join(cwd, "scripts/new-untracked.mjs"), "export {};\n");
    const current = discoverArchitectureGSourceFiles(options);
    assert.throws(
      () => validateArchitectureGSourceFileList({ expected, current }),
      /source file scope mismatch/u,
    );
  } finally {
    rmSync(cwd, { recursive: true, force: true });
  }
});

test("canonical source discovery rejects symlinks instead of omitting them", () => {
  const cwd = mkdtempSync(join(tmpdir(), "midgard-architecture-g-symlink-"));
  try {
    mkdirSync(join(cwd, "scripts"));
    writeFileSync(join(cwd, "scripts/existing.mjs"), "export {};\n");
    symlinkSync("existing.mjs", join(cwd, "scripts/untracked-link.mjs"));
    assert.throws(
      () =>
        discoverArchitectureGSourceFiles({
          cwd,
          fixedFiles: ["package.json"],
          directories: ["scripts"],
        }),
      /unsupported filesystem entry: scripts\/untracked-link\.mjs/u,
    );
  } finally {
    rmSync(cwd, { recursive: true, force: true });
  }
});

test("canonical source discovery rejects a symlinked traversal root", () => {
  const cwd = mkdtempSync(join(tmpdir(), "midgard-architecture-g-root-link-"));
  try {
    mkdirSync(join(cwd, "actual-scripts"));
    writeFileSync(join(cwd, "actual-scripts/existing.mjs"), "export {};\n");
    symlinkSync("actual-scripts", join(cwd, "scripts"));
    assert.throws(
      () =>
        discoverArchitectureGSourceFiles({
          cwd,
          fixedFiles: ["package.json"],
          directories: ["scripts"],
        }),
      /traversal root must be a real directory: scripts/u,
    );
    assert.throws(
      () =>
        discoverArchitectureGSourceFiles({
          cwd,
          fixedFiles: ["package.json"],
          directories: ["missing"],
        }),
      /source scope directory is missing or unreadable: missing/u,
    );
  } finally {
    rmSync(cwd, { recursive: true, force: true });
  }
});

const hash = (byte) => byte.toString(16).padStart(2, "0").repeat(32);

const phase1FormalBindingIdentity = {
  schemaVersion: "midgard-architecture-g-phase1-formal-binding-identity-v1",
  path: "/evidence/phase1-formal-binding.json",
  sha256: hash(80),
  deploymentManifestId: "deployment-manifest-id",
  nodeImageId: "sha256:node-image",
  nodeContainerId: "node-container-id",
  walletSetSha256: hash(81),
  fundingSetSha256: hash(82),
  corpus: {
    path: "/evidence/corpus.ndjson",
    indexPath: "/evidence/corpus.ndjson.index.ndjson",
    manifestPath: "/evidence/corpus.ndjson.manifest.json",
    sliceId: "phase1-live",
    corpusSha256: hash(83),
    indexSha256: hash(84),
    manifestSha256: hash(85),
  },
  generationResult: {
    path: "/evidence/generation-result.json",
    sha256: hash(86),
    schemaVersion: "midgard-stress-corpus-generation-v1",
  },
  harness: { scenarioId: hash(87), engineId: hash(88) },
};

const runtimeIdentity = {
  schemaVersion: "midgard-architecture-g-runtime-identity-v1",
  version: "v22.22.2",
  execPath: "/opt/node-v22.22.2/bin/node",
  executableSha256: hash(89),
};

test("Phase 3 evidence resolves the explicit current Phase 1 binding and rejects every stale identity edge", () => {
  const cwd = mkdtempSync(join(tmpdir(), "midgard-arch-g-phase1-binding-"));
  try {
    mkdirSync(join(cwd, "scripts"));
    const scenarioBytes = "export const scenario = true;\n";
    const engineBytes = "export const engine = true;\n";
    writeFileSync(join(cwd, "scripts/benchmark-scenario.mjs"), scenarioBytes);
    writeFileSync(
      join(cwd, "scripts/throughput-valid-stress.mjs"),
      engineBytes,
    );
    const digest = (bytes) => createHash("sha256").update(bytes).digest("hex");
    const corpusPath = join(cwd, "corpus.ndjson");
    const indexPath = join(cwd, "corpus.index.ndjson");
    const manifestPath = join(cwd, "corpus.manifest.json");
    const generationResultPath = join(cwd, "generation-result.json");
    const bindingPath = join(cwd, "binding.json");
    const corpusBytes = '{"tx":"00"}\n';
    const indexBytes = '{"chainId":"wallet-0"}\n';
    const walletSetIdentity = {
      walletSetSha256: hash(90),
      fundingSetSha256: hash(91),
    };
    writeFileSync(corpusPath, corpusBytes);
    writeFileSync(indexPath, indexBytes);
    const manifest = {
      files: {
        corpus: { sha256: digest(corpusBytes) },
        index: { sha256: digest(indexBytes) },
      },
      walletSetIdentity,
    };
    writeFileSync(manifestPath, JSON.stringify(manifest));
    const generationResult = {
      schemaVersion: "midgard-stress-corpus-generation-v1",
      verified: {
        corpusSha256: digest(corpusBytes),
        indexSha256: digest(indexBytes),
        walletSetIdentity,
      },
    };
    writeFileSync(generationResultPath, JSON.stringify(generationResult));
    const livePreflightEntries = Array.from({ length: 5 }, (_, index) => ({
      walletId: `wallet-${index.toString()}`,
      l2Address: `addr_test1_${index.toString()}`,
      firstInputOutref: `${index.toString(16).padStart(64, "0")}#0`,
      outputCborSha256: hash(92 + index),
    }));
    const binding = {
      schemaVersion: "midgard-phase1-live-corpus-binding-v1",
      deploymentManifestId: "deployment-id",
      nodeImageId: "sha256:node-image",
      nodeContainerId: "node-container-id",
      walletSetSha256: walletSetIdentity.walletSetSha256,
      fundingSetSha256: walletSetIdentity.fundingSetSha256,
      corpus: {
        path: corpusPath,
        indexPath,
        manifestPath,
        sliceId: "phase1-live",
        corpusSha256: digest(corpusBytes),
        indexSha256: digest(indexBytes),
        manifestSha256: digest(JSON.stringify(manifest)),
      },
      generationResult: {
        path: generationResultPath,
        sha256: digest(JSON.stringify(generationResult)),
      },
      livePreflight: {
        algorithm: "sha256-corpus-chain-id-order-v1",
        sampleSize: 5,
        entries: livePreflightEntries,
      },
      harness: {
        scenarioId: digest(scenarioBytes),
        engineId: digest(engineBytes),
      },
      stressCorpusEnv: {
        STRESS_CORPUS_INDEX_PATH: indexPath,
        STRESS_CORPUS_MANIFEST_PATH: manifestPath,
        STRESS_CORPUS_PATH: corpusPath,
        STRESS_CORPUS_READAHEAD_ROWS: "50",
        STRESS_CORPUS_SHAPE: "chain",
        STRESS_CORPUS_SLICE_ID: "phase1-live",
      },
    };
    const bindingBytes = `${JSON.stringify(binding)}\n`;
    writeFileSync(bindingPath, bindingBytes);
    const bindingSha256 = digest(bindingBytes);
    const identity = captureArchitectureGPhase1FormalBindingIdentity({
      bindingPath,
      bindingSha256,
      cwd,
    });
    assert.equal(identity.sha256, bindingSha256);
    assert.deepEqual(identity.harness, binding.harness);
    assert.equal(identity.deploymentManifestId, binding.deploymentManifestId);
    assert.throws(
      () =>
        captureArchitectureGPhase1FormalBindingIdentity({
          bindingPath: "",
          bindingSha256,
          cwd,
        }),
      /explicit canonical absolute/u,
    );
    assert.throws(
      () =>
        captureArchitectureGPhase1FormalBindingIdentity({
          bindingPath,
          bindingSha256: hash(99),
          cwd,
        }),
      /binding SHA-256 mismatch/u,
    );
    writeFileSync(join(cwd, "scripts/benchmark-scenario.mjs"), "stale\n");
    assert.throws(
      () =>
        captureArchitectureGPhase1FormalBindingIdentity({
          bindingPath,
          bindingSha256,
          cwd,
        }),
      /stale harness identity/u,
    );
    writeFileSync(join(cwd, "scripts/benchmark-scenario.mjs"), scenarioBytes);
    writeFileSync(corpusPath, "tampered\n");
    assert.throws(
      () =>
        captureArchitectureGPhase1FormalBindingIdentity({
          bindingPath,
          bindingSha256,
          cwd,
        }),
      /corpus SHA-256 mismatch/u,
    );
  } finally {
    rmSync(cwd, { recursive: true, force: true });
  }
});

test("Phase 3 evidence requires a pinned runtime and exact cross-gate identities", () => {
  const executableSha256 = createHash("sha256")
    .update(readFileSync(process.execPath))
    .digest("hex");
  const captured = captureArchitectureGRuntimeIdentity({
    expectedVersion: process.version,
    expectedExecutableSha256: executableSha256,
  });
  assert.equal(captured.version, process.version);
  assert.equal(captured.executableSha256, executableSha256);
  assert.throws(() =>
    captureArchitectureGRuntimeIdentity({
      expectedVersion: "",
      expectedExecutableSha256: executableSha256,
    }),
  );
  assert.throws(() =>
    captureArchitectureGRuntimeIdentity({
      expectedVersion: process.version,
      expectedExecutableSha256: hash(98),
    }),
  );
  assert.throws(() =>
    validateArchitectureGRuntimeIdentity({
      identity: captured,
      expectedVersion: `${process.version}-different`,
      expectedExecutableSha256: executableSha256,
    }),
  );
  assert.equal(
    validateArchitectureGCrossGateEvidenceIdentity({
      expected: phase1FormalBindingIdentity,
      current: phase1FormalBindingIdentity,
      label: "Phase 1 formal binding",
    }),
    phase1FormalBindingIdentity,
  );
  for (const current of [
    {
      ...phase1FormalBindingIdentity,
      deploymentManifestId: "different",
    },
    {
      ...phase1FormalBindingIdentity,
      corpus: {
        ...phase1FormalBindingIdentity.corpus,
        corpusSha256: hash(97),
      },
    },
    {
      ...phase1FormalBindingIdentity,
      harness: { ...phase1FormalBindingIdentity.harness, engineId: hash(96) },
    },
  ]) {
    assert.throws(() =>
      validateArchitectureGCrossGateEvidenceIdentity({
        expected: phase1FormalBindingIdentity,
        current,
        label: "Phase 1 formal binding",
      }),
    );
  }
  assert.throws(() =>
    validateArchitectureGCrossGateEvidenceIdentity({
      expected: runtimeIdentity,
      current: { ...runtimeIdentity, version: "v22.21.0" },
      label: "runtime",
    }),
  );
});

const nearestRank = (values, quantile) => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.max(0, Math.ceil(sorted.length * quantile) - 1)];
};

const rootGateExecutableIdentity = {
  probePath: "/release/dist/mpf-engine-probe.js",
  probeSha256: hash(70),
  binaryPath: "/release/native/architecture-g-owner",
  binarySha256: hash(71),
};

const rootGateProvenance = {
  generatedAt: "2026-07-27T00:00:00.000Z",
  gitHead: "ab".repeat(20),
  sourceSha256: hash(60),
  diffSha256: hash(61),
  gitStatusSha256: createHash("sha256").update("").digest("hex"),
  gitStatusEntries: [],
  sourceFiles: ["package.json", "scripts/mpf-architecture-g-gate.mjs"],
  nodeOptions: "--max-old-space-size=4096",
  cgroup: {
    membership: "0::/midgard",
    memoryMaxPath: "/sys/fs/cgroup/midgard/memory.max",
    memoryMax: "4294967296",
  },
  percentileMethod:
    "nearest-rank: sorted[max(0, ceil(N*q)-1)]; q=0.5 median, q=0.95 p95",
};

const rootGateFundingRoots = [
  { walletId: "wallet-0", outref: `${"aa".repeat(32)}#0` },
  { walletId: "wallet-1", outref: `${"bb".repeat(32)}#1` },
];

const rootGateCanonicalCorpus = {
  corpusPath: "/evidence/corpus.ndjson",
  manifestPath: "/evidence/corpus.ndjson.manifest.json",
  manifestSha256: hash(72),
  corpusSha256: hash(73),
  indexPath: "/evidence/corpus.ndjson.index.ndjson",
  indexSha256: hash(74),
  verificationPath: "/evidence/generate-result.json",
  verificationSha256: hash(75),
  fundingRootsSha256: createHash("sha256")
    .update(JSON.stringify(rootGateFundingRoots))
    .digest("hex"),
  fundingMapPath: "/evidence/canonical-corpus-funding.json",
  fundingMapSha256: hash(77),
  fundingEntryCount: 2,
  slicePath: "/evidence/canonical-corpus-slice.ndjson",
  sliceSha256: hash(78),
  sliceRowCount: 2,
  corpusManifestRowCount: 100,
  parentSliceRowsSeen: 100,
  parentSliceChainCount: 10,
  verifiedCorpusChainCount: 10,
  completeChainCount: 1,
  finalChainPrefixLength: 1,
  sliceChainsContiguous: true,
  chainsCrossSliceBoundaries: false,
  selectionAlgorithm: "named-slice-file-order-prefix-v1",
  sourceCorpusRowRange: { start: 11, end: 12 },
  sourceSliceOrdinalRange: { start: 1, end: 2 },
  fundingRootOutrefs: rootGateFundingRoots.map((root) => root.outref),
  fundingRoots: structuredClone(rootGateFundingRoots),
};

const rootGateOwnerDiagnostics = (durableRoot) => ({
  ownerEpoch: { type: "Buffer", data: Array(16).fill(7) },
  durableRoot,
  residentNodes: 10,
  residentEdges: 9,
  residentBytes: 1024,
  activeGenerations: 0,
  generatedNodes: 20,
  generatedBytes: 2048,
  rssBytes: 4096,
  peakRssBytes: 8192,
  childRestarts: 0,
});

const rootGatePathHydration = {
  prefetchMs: 0,
  uniquePaths: 2,
  nodesRequested: 0,
  hydrationHits: 0,
  hydrationMisses: 0,
  loadedNodes: 0,
  maxInFlight: 0,
  maxBatchKeys: 0,
  maxFrontierPaths: 0,
  retainedBytesEstimate: 0,
  chunkCount: 1,
  checkpointMs: 0,
  authenticationMs: 0,
  materializeMs: 0,
  collapseMs: 0,
  checkpointSerializedNodes: 0,
  checkpointSerializedBytes: 0,
  verifiedUpperNodes: 0,
  retainedUpperNodes: 0,
  collapsedNodes: 0,
  peakDecodedNodes: 0,
};

const rootGateGroup = ({ initialUtxos, durations, transactions = 2 }) => {
  const durableRoot = hash(31);
  const transitionRoots = Array.from({ length: transactions }, (_, index) => ({
    pre: index === 0 ? durableRoot : hash(40 + index),
    post: index === transactions - 1 ? hash(1) : hash(41 + index),
  }));
  const roots = {
    utxoRoot: hash(1),
    rawTxRoot: hash(2),
    txRoot: hash(3),
    transitionTraceRoot: hash(4),
    eventToStepRoot: hash(5),
    depositsRoot: hash(6),
    withdrawalsRoot: hash(7),
    forcedTransactionsRoot: hash(8),
    transitionRoots,
  };
  const results = durations.map((durationMs) => ({
    ...structuredClone(roots),
    engine: "architecture_g",
    probePath: rootGateExecutableIdentity.probePath,
    probeSha256: rootGateExecutableIdentity.probeSha256,
    binarySha256: rootGateExecutableIdentity.binarySha256,
    canonicalCorpusSlice: {
      path: rootGateCanonicalCorpus.slicePath,
      sha256: rootGateCanonicalCorpus.sliceSha256,
      rowCount: rootGateCanonicalCorpus.sliceRowCount,
    },
    canonicalFunding: {
      path: rootGateCanonicalCorpus.fundingMapPath,
      sha256: rootGateCanonicalCorpus.fundingMapSha256,
      entryCount: rootGateCanonicalCorpus.fundingEntryCount,
    },
    cpuAffinity: "28-31",
    transactionCount: transactions,
    initialUtxoCount: initialUtxos,
    levelBackedInitialView: true,
    reusedLevelFixture: true,
    ledgerOpCount: transactions * 2,
    startupMs: 1,
    confirmedLedgerFullScans: 0,
    durationMs,
    buildPlusCaptureMs: durationMs,
    phaseMs: {
      transactionSourceRoot: 1,
      transitionTraceBuild: 2,
      transactionMpfApply: 3,
      auxiliaryRoots: 4,
    },
    nativePhaseMs: {
      validation: 1,
      eventLogEncode: 2,
      ownerApply: 3,
      ownerProofArena: 4,
      ownerMutation: 5,
      memberAssembly: 6,
      retainedRoots: 7,
    },
    pathHydration: structuredClone(rootGatePathHydration),
    workloadSha256: hash(50),
    ownerBefore: rootGateOwnerDiagnostics(durableRoot),
    ownerAfter: rootGateOwnerDiagnostics(durableRoot),
  }));
  return {
    initialUtxos,
    fixtureCreation: {
      path: `/evidence/fixture-create-${initialUtxos.toString()}.json`,
      sha256: hash(30),
      initialUtxoCount: initialUtxos,
      marker: hash(31),
      utxoPayloadAggregate: {
        entryCount: initialUtxos,
        encodedTupleBytes: initialUtxos * 80,
      },
    },
    fixtureBefore: {
      path: `/fixtures/utxos-${initialUtxos.toString()}-level`,
      directoryBytes: initialUtxos * 80,
      marker: hash(31),
      logicalSha256: hash(32),
      records: initialUtxos + 1,
    },
    fixtureAfter: {
      path: `/fixtures/utxos-${initialUtxos.toString()}-level`,
      directoryBytes: initialUtxos * 80,
      marker: hash(31),
      logicalSha256: hash(32),
      records: initialUtxos + 1,
    },
    roots,
    durationMs: {
      min: Math.min(...durations),
      median: nearestRank(durations, 0.5),
      p95: nearestRank(durations, 0.95),
      max: Math.max(...durations),
    },
    results,
  };
};

const rootGateSummary = (mode = "50k") => {
  const groups =
    mode === "50k"
      ? [rootGateGroup({ initialUtxos: 1_000_000, durations: [100, 110] })]
      : [
          rootGateGroup({ initialUtxos: 100_000, durations: [100, 110] }),
          rootGateGroup({ initialUtxos: 300_000, durations: [101, 109] }),
          rootGateGroup({ initialUtxos: 1_000_000, durations: [102, 108] }),
        ];
  const medians = groups.map((group) => group.durationMs.median);
  const minimumMedianMs = Math.min(...medians);
  const maximumMedianMs = Math.max(...medians);
  const boundPhase1Identity = structuredClone(phase1FormalBindingIdentity);
  boundPhase1Identity.corpus = {
    path: rootGateCanonicalCorpus.corpusPath,
    indexPath: rootGateCanonicalCorpus.indexPath,
    manifestPath: rootGateCanonicalCorpus.manifestPath,
    sliceId: "phase1-live",
    corpusSha256: rootGateCanonicalCorpus.corpusSha256,
    indexSha256: rootGateCanonicalCorpus.indexSha256,
    manifestSha256: rootGateCanonicalCorpus.manifestSha256,
  };
  boundPhase1Identity.generationResult = {
    path: rootGateCanonicalCorpus.verificationPath,
    sha256: rootGateCanonicalCorpus.verificationSha256,
    schemaVersion: "midgard-stress-corpus-generation-v1",
  };
  const canonicalCorpus = structuredClone(rootGateCanonicalCorpus);
  canonicalCorpus.parentSliceId = boundPhase1Identity.corpus.sliceId;
  return {
    schemaVersion: "midgard-architecture-g-production-root-gate-v1",
    formal: true,
    profile: "formal",
    ...structuredClone(rootGateProvenance),
    mode,
    requiredCardinality:
      mode === "50k"
        ? { runs: 20, transactions: 50_000 }
        : { runs: 3, transactions: 10_000 },
    phase1FormalBinding: boundPhase1Identity,
    runtimeIdentity: structuredClone(runtimeIdentity),
    ...rootGateExecutableIdentity,
    canonicalCorpus,
    freshProcessRunsPerFixture: 2,
    transactionCount: 2,
    cpuSet: "28-31",
    groups,
    verdict:
      mode === "50k"
        ? {
            pass: true,
            gate: "50k_complete_root_build_p95_under_10s",
            p95Ms: 110,
            limitMs: 10_000,
          }
        : {
            pass: true,
            gate: "100k_300k_1m_max_min_build_slope_within_10_percent",
            maxMinSlopePercent:
              ((maximumMedianMs - minimumMedianMs) / minimumMedianMs) * 100,
            minimumMedianMs,
            maximumMedianMs,
            limitAbsolutePercent: 10,
          },
  };
};

const validateRootGateSummary = (summary) =>
  validateArchitectureGRootGateSummary({
    summary,
    mode: summary.mode,
    runs: 2,
    transactions: 2,
    cpuSet: "28-31",
  });

test("corpus preparation uses the exact root-gate canonical corpus language", () => {
  const summary = rootGateSummary();
  const valid = {
    schemaVersion: "midgard-architecture-g-corpus-preparation-v1",
    formalGateEvidence: false,
    phase1FormalBinding: structuredClone(summary.phase1FormalBinding),
    runtimeIdentity: structuredClone(summary.runtimeIdentity),
    canonicalCorpus: structuredClone(summary.canonicalCorpus),
  };
  const validate = (artifact) =>
    validateArchitectureGCorpusPreparationV1({
      artifact,
      transactions: 2,
    });
  assert.equal(validate(valid), valid);
  for (const mutate of [
    (value) => void (value.unknown = true),
    (value) => void (value.formalGateEvidence = true),
    (value) => void (value.phase1FormalBinding.unknown = true),
    (value) => void (value.runtimeIdentity.unknown = true),
    (value) => void (value.canonicalCorpus.unknown = true),
    (value) => void (value.canonicalCorpus.sourceCorpusRowRange.unknown = true),
    (value) => void (value.canonicalCorpus.corpusSha256 = hash(90)),
    (value) => void (value.canonicalCorpus.sliceSha256 = "bad"),
    (value) => void (value.canonicalCorpus.sliceRowCount = 1),
    (value) => void value.canonicalCorpus.fundingRootOutrefs.pop(),
    (value) => void (value.canonicalCorpus.fundingRoots[0].unknown = true),
    (value) =>
      void (value.canonicalCorpus.fundingRoots[1].walletId =
        value.canonicalCorpus.fundingRoots[0].walletId),
    (value) => void (value.canonicalCorpus.fundingRootsSha256 = hash(91)),
  ]) {
    const invalid = structuredClone(valid);
    mutate(invalid);
    assert.throws(() => validate(invalid));
  }
});

test("formal root summary validator recomputes all 50k evidence and rejects mutations", () => {
  const valid = rootGateSummary();
  assert.equal(validateRootGateSummary(valid), valid);
  for (const mutate of [
    (value) => void value.groups.pop(),
    (value) => void value.groups[0].results.pop(),
    (value) => void (value.groups[0].fixtureCreation.initialUtxoCount = 99),
    (value) => void (value.groups[0].fixtureBefore.records = 99),
    (value) => void (value.groups[0].fixtureAfter.logicalSha256 = hash(60)),
    (value) => void (value.groups[0].results[0].cpuAffinity = "0"),
    (value) =>
      void (value.groups[0].results[0].ownerBefore.durableRoot = hash(60)),
    (value) => void (value.groups[0].results[0].confirmedLedgerFullScans = 1),
    (value) => void value.groups[0].results[0].transitionRoots.pop(),
    (value) =>
      void (value.groups[0].results[0].transitionRoots[1].pre = hash(61)),
    (value) => void (value.groups[0].results[0].txRoot = hash(62)),
    (value) => void (value.groups[0].roots.rawTxRoot = "missing"),
    (value) => void (value.groups[0].durationMs.p95 = 109),
    (value) => void (value.verdict.p95Ms = 109),
    (value) => void (value.verdict.pass = false),
  ]) {
    const invalid = structuredClone(valid);
    mutate(invalid);
    assert.throws(() => validateRootGateSummary(invalid));
  }
});

test("formal root summary validator recomputes growth slope and workload identity", () => {
  const valid = rootGateSummary("growth");
  assert.equal(validateRootGateSummary(valid), valid);
  for (const mutate of [
    (value) => void (value.groups[2].durationMs.median = 103),
    (value) => void (value.verdict.maxMinSlopePercent = 3),
    (value) => void (value.groups[2].results[0].workloadSha256 = hash(63)),
  ]) {
    const invalid = structuredClone(valid);
    mutate(invalid);
    assert.throws(() => validateRootGateSummary(invalid));
  }
});

test("formal root summary rejects incomplete, extended, or noncanonical V1 documents", () => {
  const mutations = [
    (value) => void (value.unknown = true),
    (value) => void delete value.generatedAt,
    (value) => void (value.generatedAt = "2026-07-27T00:00:00Z"),
    (value) => void (value.requiredCardinality.unknown = true),
    (value) => void (value.phase1FormalBinding.unknown = true),
    (value) => void (value.phase1FormalBinding.corpus.unknown = true),
    (value) => void (value.runtimeIdentity.unknown = true),
    (value) => void (value.canonicalCorpus.unknown = true),
    (value) => void (value.canonicalCorpus.sourceCorpusRowRange.unknown = true),
    (value) => void (value.canonicalCorpus.sliceChainsContiguous = false),
    (value) => void value.canonicalCorpus.fundingRootOutrefs.pop(),
    (value) => void (value.canonicalCorpus.fundingRoots[0].unknown = true),
    (value) =>
      void (value.canonicalCorpus.fundingRoots[0].outref = `${hash(98)}#0`),
    (value) => void (value.cgroup.unknown = true),
    (value) => void (value.gitStatusSha256 = hash(99)),
    (value) => void value.sourceFiles.reverse(),
    (value) => void (value.groups[0].unknown = true),
    (value) => void (value.groups[0].fixtureCreation.unknown = true),
    (value) =>
      void (value.groups[0].fixtureCreation.utxoPayloadAggregate.unknown = true),
    (value) => void (value.groups[0].fixtureBefore.unknown = true),
    (value) => void (value.groups[0].roots.unknown = true),
    (value) => void (value.groups[0].durationMs.unknown = true),
    (value) => void (value.groups[0].results[0].unknown = true),
    (value) =>
      void (value.groups[0].results[0].canonicalCorpusSlice.unknown = true),
    (value) => void (value.groups[0].results[0].phaseMs.unknown = true),
    (value) => void (value.groups[0].results[0].nativePhaseMs.unknown = true),
    (value) => void (value.groups[0].results[0].pathHydration.unknown = true),
    (value) =>
      void (value.groups[0].results[0].pathHydration.uniquePaths = 1.5),
    (value) => void (value.groups[0].results[0].ownerBefore.unknown = true),
    (value) =>
      void value.groups[0].results[0].ownerBefore.ownerEpoch.data.pop(),
    (value) => void (value.groups[0].results[0].ownerAfter.childRestarts = 1),
    (value) =>
      void (value.groups[0].results[0].transitionRoots[0].unknown = true),
    (value) => void (value.verdict.unknown = true),
  ];
  for (const mutate of mutations) {
    const invalid = rootGateSummary();
    mutate(invalid);
    assert.throws(() => validateRootGateSummary(invalid));
  }
});

test("formal root summary binds canonical nonempty git-status bytes", () => {
  const valid = rootGateSummary();
  valid.gitStatusEntries = [
    " M scripts/mpf-architecture-g-gate.mjs",
    "?? logs/evidence.json",
  ];
  valid.gitStatusSha256 = createHash("sha256")
    .update(`${valid.gitStatusEntries.join("\0")}\0`)
    .digest("hex");
  assert.equal(validateRootGateSummary(valid), valid);

  valid.gitStatusEntries.reverse();
  assert.throws(() => validateRootGateSummary(valid));
});

test("formal root summary validator rejects a contradictory profile", () => {
  const invalid = rootGateSummary();
  invalid.profile = "smoke";
  assert.throws(() => validateRootGateSummary(invalid));
});

test("formal root summary validator requires the exact declared cardinality", () => {
  const valid = rootGateSummary();
  for (const field of ["runs", "transactions"]) {
    const invalid = structuredClone(valid);
    invalid.requiredCardinality[field] += 1;
    assert.throws(() => validateRootGateSummary(invalid));
  }
});

test("formal root summary validator requires canonical top-level executable identity", () => {
  const valid = rootGateSummary();
  for (const [field, invalidValue] of [
    ["probePath", "relative/probe.js"],
    ["probeSha256", "bad"],
    ["binaryPath", "/release/../different-owner"],
    ["binarySha256", "bad"],
  ]) {
    const invalid = structuredClone(valid);
    invalid[field] = invalidValue;
    assert.throws(() => validateRootGateSummary(invalid));
  }
});

test("formal root summary validator requires retained Phase 1 and pinned runtime identities", () => {
  const valid = rootGateSummary();
  for (const mutate of [
    (value) => void delete value.phase1FormalBinding,
    (value) => void (value.phase1FormalBinding.sha256 = "bad"),
    (value) => void (value.phase1FormalBinding.harness.engineId = "bad"),
    (value) => void (value.phase1FormalBinding.corpus.corpusSha256 = hash(95)),
    (value) =>
      void (value.phase1FormalBinding.generationResult.sha256 = hash(94)),
    (value) => void (value.canonicalCorpus.parentSliceId = "different"),
    (value) => void delete value.runtimeIdentity,
    (value) => void (value.runtimeIdentity.execPath = "relative/node"),
    (value) => void (value.runtimeIdentity.executableSha256 = "bad"),
  ]) {
    const invalid = structuredClone(valid);
    mutate(invalid);
    assert.throws(() => validateRootGateSummary(invalid));
  }
});

test("formal root summary validator binds every run to executable identity", () => {
  const valid = rootGateSummary();
  for (const field of ["probePath", "probeSha256", "binarySha256"]) {
    const invalid = structuredClone(valid);
    invalid.groups[0].results[0][field] = "different";
    assert.throws(() => validateRootGateSummary(invalid));
  }
});

test("formal root summary validator requires every canonical corpus hash and count", () => {
  const valid = rootGateSummary();
  for (const field of [
    "manifestSha256",
    "corpusSha256",
    "indexSha256",
    "verificationSha256",
    "fundingRootsSha256",
    "fundingMapSha256",
    "sliceSha256",
  ]) {
    const invalid = structuredClone(valid);
    invalid.canonicalCorpus[field] = "bad";
    assert.throws(() => validateRootGateSummary(invalid));
  }
  for (const field of [
    "corpusManifestRowCount",
    "parentSliceRowsSeen",
    "parentSliceChainCount",
    "verifiedCorpusChainCount",
    "completeChainCount",
    "fundingEntryCount",
    "sliceRowCount",
  ]) {
    const invalid = structuredClone(valid);
    invalid.canonicalCorpus[field] = 0;
    assert.throws(() => validateRootGateSummary(invalid));
  }
  valid.canonicalCorpus.finalChainPrefixLength = 0;
  assert.equal(validateRootGateSummary(valid), valid);
});

for (const field of [
  "corpusPath",
  "manifestPath",
  "indexPath",
  "verificationPath",
]) {
  test(`formal root summary validator requires canonical absolute ${field}`, () => {
    const invalid = rootGateSummary();
    invalid.canonicalCorpus[field] = `relative/${field}`;
    assert.throws(() => validateRootGateSummary(invalid));
  });
}

test("formal root summary validator binds every run to canonical workload identity", () => {
  const valid = rootGateSummary();
  for (const [section, field] of [
    ["canonicalCorpusSlice", "path"],
    ["canonicalCorpusSlice", "sha256"],
    ["canonicalCorpusSlice", "rowCount"],
    ["canonicalFunding", "path"],
    ["canonicalFunding", "sha256"],
    ["canonicalFunding", "entryCount"],
  ]) {
    const invalid = structuredClone(valid);
    invalid.groups[0].results[0][section][field] =
      typeof invalid.groups[0].results[0][section][field] === "number"
        ? 999
        : "different";
    assert.throws(() => validateRootGateSummary(invalid));
  }
});

test("fixture evidence binds actual path, marker, cardinality, and aggregate", () => {
  const diagnostics = Object.fromEntries(
    [
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
    ].map((field) => [field, 0]),
  );
  const artifact = {
    fixtureCreated: true,
    fixturePath: "/fixtures/1m",
    marker: "ab".repeat(32),
    initialUtxoCount: 1_000_000,
    durationMs: 100,
    diagnostics,
    utxoPayloadAggregate: {
      entryCount: 1_000_000,
      encodedTupleBytes: 80_000_000,
    },
    canonicalFunding: {
      path: "/evidence/canonical-corpus-funding.json",
      sha256: hash(123),
      entryCount: 50_000,
    },
  };
  assert.deepEqual(
    validateArchitectureGFixtureCreationEvidence({
      artifact,
      expectedFixturePath: artifact.fixturePath,
      expectedMarker: artifact.marker,
      expectedUtxos: 1_000_000,
    }),
    artifact.utxoPayloadAggregate,
  );
  for (const invalid of [
    { ...artifact, fixturePath: "/fixtures/100k" },
    { ...artifact, marker: "cd".repeat(32) },
    { ...artifact, initialUtxoCount: 100_000 },
    {
      ...artifact,
      utxoPayloadAggregate: {
        ...artifact.utxoPayloadAggregate,
        entryCount: 100_000,
      },
    },
    { ...artifact, unknown: true },
    { ...artifact, diagnostics: { ...diagnostics, unknown: 0 } },
    {
      ...artifact,
      canonicalFunding: { ...artifact.canonicalFunding, unknown: true },
    },
    {
      ...artifact,
      diagnostics: { ...diagnostics, serialiseMs: Number.NaN },
    },
  ]) {
    assert.throws(() =>
      validateArchitectureGFixtureCreationEvidence({
        artifact: invalid,
        expectedFixturePath: artifact.fixturePath,
        expectedMarker: artifact.marker,
        expectedUtxos: 1_000_000,
      }),
    );
  }
  const withoutCanonicalFunding = { ...artifact, canonicalFunding: null };
  assert.deepEqual(
    validateArchitectureGFixtureCreationEvidence({
      artifact: withoutCanonicalFunding,
      expectedFixturePath: artifact.fixturePath,
      expectedMarker: artifact.marker,
      expectedUtxos: 1_000_000,
    }),
    artifact.utxoPayloadAggregate,
  );
});

test("formal gate cardinalities are exact and cannot be reduced", () => {
  assert.deepEqual(
    resolveArchitectureGGateConfig({ mode: "50k", profile: "formal" }),
    {
      mode: "50k",
      profile: "formal",
      formal: true,
      runs: 20,
      transactions: 50_000,
      required: { runs: 20, transactions: 50_000 },
    },
  );
  assert.throws(
    () =>
      resolveArchitectureGGateConfig({
        mode: "50k",
        profile: "formal",
        runs: "1",
        transactions: "1",
      }),
    /Formal 50k gate requires --runs=20 and --transactions=50000/u,
  );
  assert.throws(
    () =>
      resolveArchitectureGGateConfig({
        mode: "growth",
        profile: "formal",
        runs: "2",
        transactions: "10000",
      }),
    /Formal growth gate requires --runs=3 and --transactions=10000/u,
  );
});

test("reduced diagnostics have a non-formal schema", () => {
  const config = resolveArchitectureGGateConfig({
    mode: "growth",
    profile: "smoke",
    runs: "1",
    transactions: "25",
  });
  assert.equal(config.formal, false);
  assert.equal(config.runs, 1);
  assert.equal(config.transactions, 25);
});

test("numeric arguments reject partial, unsafe, zero, and negative values", () => {
  for (const value of ["1x", "0", "-1", "9007199254740992", "1.5", ""]) {
    assert.throws(() =>
      resolveArchitectureGGateConfig({
        mode: "50k",
        profile: "smoke",
        runs: value,
        transactions: "1",
      }),
    );
  }
});

const candidateInputDocument = () => {
  const submittedTxHash = hash(124);
  const blockEndTimeMs = 1_700_000_000_000;
  return {
    schemaVersion: "midgard-architecture-g-commit-candidate-input-v1",
    phase1FormalBinding: structuredClone(phase1FormalBindingIdentity),
    runtimeIdentity: structuredClone(runtimeIdentity),
    levelPath: "/fixtures/utxos-1000000-level",
    binaryPath: "/release/native/architecture-g-owner",
    binarySha256: hash(125),
    sidecarPath: "/fixtures/utxos-1000000-level.candidate.sidecar",
    expectedTransactionCount: 50_000,
    corpusSha256: phase1FormalBindingIdentity.corpus.corpusSha256,
    corpusSliceSha256: hash(126),
    fundingMapSha256: hash(127),
    fixtureCreationPath: "/evidence/fixture-create-1000000.json",
    fixtureCreationSha256: hash(128),
    fixtureInitialUtxoCount: 1_000_000,
    baseUtxoPayloadAggregate: {
      entryCount: 1_000_000,
      encodedTupleBytes: 80_000_000,
    },
    workerInput: {
      data: {
        availableConfirmedBlock: "",
        availableLocalFinalizationBlock: "",
        currentBlockStartTimeMs: blockEndTimeMs,
        localFinalizationPending: false,
        ledgerStoreLeaseOwner: "commit:123e4567-e89b-42d3-a456-426614174000",
        mempoolTxsCountSoFar: 0,
        sizeOfProcessedTxsSoFar: 0,
        baseSnapshotId: `architecture-g-candidate:${submittedTxHash}`,
        stateQueueHasUnmergedTail: true,
        speculativeBuild: {
          base: {
            headerHash: submittedTxHash.slice(0, 56),
            utxosRoot: hash(129),
            blockEndTimeMs,
            submittedTxHash,
          },
          watermarks: {
            depositMs: blockEndTimeMs + 180_000,
            withdrawalMs: blockEndTimeMs + 180_000,
            txOrderMs: blockEndTimeMs + 180_000,
            refreshedAtMs: blockEndTimeMs + 180_000,
          },
          excludedMempoolTxIds: [],
          excludedDepositEventIds: [],
          excludedForcedTransactionEventIds: [],
          excludedWithdrawalEventIds: [],
        },
      },
    },
  };
};

test("candidate input validator accepts only the complete producer language", () => {
  const valid = candidateInputDocument();
  assert.equal(validateArchitectureGCommitCandidateInputV1(valid), valid);
  for (const mutate of [
    (value) => void (value.unknown = true),
    (value) => void (value.phase1FormalBinding.unknown = true),
    (value) => void (value.runtimeIdentity.unknown = true),
    (value) => void (value.baseUtxoPayloadAggregate.unknown = true),
    (value) => void (value.workerInput.unknown = true),
    (value) => void (value.workerInput.data.unknown = true),
    (value) => void delete value.workerInput.data.ledgerStoreLeaseOwner,
    (value) =>
      void (value.workerInput.data.ledgerStoreLeaseOwner = "commit:shared"),
    (value) => void (value.workerInput.data.speculativeBuild.unknown = true),
    (value) =>
      void (value.workerInput.data.speculativeBuild.base.unknown = true),
    (value) =>
      void (value.workerInput.data.speculativeBuild.watermarks.unknown = true),
    (value) =>
      void (value.workerInput.data.speculativeBuild.base.headerHash = hash(1)),
    (value) =>
      void (value.workerInput.data.speculativeBuild.base.utxosRoot = "bad"),
    (value) =>
      void value.workerInput.data.speculativeBuild.excludedMempoolTxIds.push(
        hash(2),
      ),
    (value) => void (value.levelPath = "relative/fixture"),
    (value) => void (value.corpusSha256 = hash(3)),
    (value) => void (value.baseUtxoPayloadAggregate.entryCount = 999_999),
    (value) =>
      void (value.workerInput.data.speculativeBuild.watermarks.depositMs =
        value.workerInput.data.currentBlockStartTimeMs),
  ]) {
    const invalid = structuredClone(valid);
    mutate(invalid);
    assert.throws(() => validateArchitectureGCommitCandidateInputV1(invalid));
  }
});

const candidateProbeResult = () => ({
  schemaVersion: "midgard-architecture-g-commit-candidate-probe-v1",
  probePath: "/probes/mpf-commit-candidate-probe.js",
  probeSha256: "77".repeat(32),
  inputPath: "/inputs/candidate-input.json",
  inputSha256: "66".repeat(32),
  expectedTransactionCount: 50_000,
  cpuAffinity: "2-9",
  corpusSha256: "11".repeat(32),
  corpusSliceSha256: "22".repeat(32),
  fundingMapSha256: "33".repeat(32),
  fixtureCreationSha256: "55".repeat(32),
  fixtureInitialUtxoCount: 1_000_000,
  baseUtxoPayloadAggregate: {
    entryCount: 1_000_000,
    encodedTupleBytes: 80_000_000,
  },
  binarySha256: "44".repeat(32),
  durationMs: 9_000,
  confirmedLedgerFullScans: 0,
  providerBoundaryAttempts: 0,
  submissionAttempts: 0,
  journalRowsBefore: 0,
  journalRowsAfter: 0,
  candidateConfig: {
    mpfEngine: "architecture_g",
    scratchBuild: "fromlist",
    payloadRootCheck: "off",
    parallelRoots: true,
    costModel: "ewma",
    mempoolRetrievePageSize: 50_000,
    maxL2TxCount: 50_000,
    maxLedgerOpCount: 150_000,
    maxTransitionStepCount: 50_000,
  },
  candidate: {
    candidateId: "123e4567-e89b-42d3-a456-426614174000",
    baseHeaderHash: hash(121),
    endTimeMs: 1_700_000_000_000,
    builtAtMs: 1_700_000_000_100,
    expectedL2TransactionCount: 50_000,
    buildDurationMs: 8_900,
    invalidationKey: `${hash(121)}:1700000000000:1699999999000`,
    watermarks: {
      depositMs: 1_699_999_999_000,
      withdrawalMs: 1_699_999_999_100,
      txOrderMs: 1_699_999_999_200,
      refreshedAtMs: 1_700_000_000_050,
    },
    expectedUserEventCounts: {
      deposits: 0,
      forcedTransactions: 0,
      withdrawals: 0,
    },
    roots: Object.fromEntries(
      [
        "utxos",
        "rawTransactions",
        "transactions",
        "deposits",
        "forcedTransactions",
        "withdrawals",
        "transitionTrace",
        "eventToStep",
      ].map((name, index) => [
        name,
        (index + 1).toString(16).padStart(2, "0").repeat(32),
      ]),
    ),
  },
  ownerBefore: rootGateOwnerDiagnostics(hash(120)),
  ownerAfter: rootGateOwnerDiagnostics(hash(120)),
});

test("candidate result validator binds count, affinity, no-scan, no-submit, journal, and roots", () => {
  const valid = candidateProbeResult();
  assert.deepEqual(
    validateCommitCandidateProbeResult({
      result: valid,
      transactions: 50_000,
      cpuSet: "2-9",
      fixtureSize: 1_000_000,
      inputPath: valid.inputPath,
      inputSha256: valid.inputSha256,
      probePath: valid.probePath,
      probeSha256: valid.probeSha256,
      binarySha256: valid.binarySha256,
    }),
    valid.candidate.roots,
  );
  for (const mutate of [
    (value) => void (value.confirmedLedgerFullScans = 1),
    (value) => void (value.providerBoundaryAttempts = 1),
    (value) => void (value.submissionAttempts = 1),
    (value) => void (value.journalRowsBefore = 1),
    (value) => void (value.journalRowsAfter = 1),
    (value) => void (value.candidate.expectedL2TransactionCount = 49_999),
    (value) => void (value.cpuAffinity = "0"),
    (value) => void (value.inputPath = "/inputs/different.json"),
    (value) => void (value.inputSha256 = "bad"),
    (value) => void (value.probePath = "/probes/different.js"),
    (value) => void (value.probeSha256 = "bad"),
    (value) => void (value.binarySha256 = "bad"),
    (value) => void (value.fundingMapSha256 = "bad"),
    (value) => void (value.fixtureCreationSha256 = "bad"),
    (value) => void (value.baseUtxoPayloadAggregate.entryCount = 99),
    (value) => void (value.candidateConfig.scratchBuild = "overlay"),
    (value) => void (value.candidateConfig.maxLedgerOpCount = 149_999),
    (value) => void (value.candidate.candidateId = "not-a-uuid"),
    (value) => void (value.candidate.baseHeaderHash = "bad"),
    (value) => void (value.candidate.invalidationKey = "stale"),
    (value) => void (value.candidate.watermarks.depositMs = -1),
    (value) => void (value.candidate.expectedUserEventCounts.deposits = -1),
    (value) => void (value.ownerAfter.durableRoot = hash(122)),
    (value) => void (value.candidate.roots.utxos = "bad"),
  ]) {
    const invalid = structuredClone(valid);
    mutate(invalid);
    assert.throws(() =>
      validateCommitCandidateProbeResult({
        result: invalid,
        transactions: 50_000,
        cpuSet: "2-9",
        fixtureSize: 1_000_000,
        inputPath: valid.inputPath,
        inputSha256: valid.inputSha256,
        probePath: valid.probePath,
        probeSha256: valid.probeSha256,
        binarySha256: valid.binarySha256,
      }),
    );
  }
});

test("candidate result validator rejects incomplete or extended V1 documents", () => {
  const valid = candidateProbeResult();
  for (const mutate of [
    (value) => void (value.unknown = true),
    (value) => void delete value.candidateConfig,
    (value) => void (value.baseUtxoPayloadAggregate.unknown = true),
    (value) => void (value.candidateConfig.unknown = true),
    (value) => void (value.candidate.unknown = true),
    (value) => void (value.candidate.watermarks.unknown = true),
    (value) => void (value.candidate.expectedUserEventCounts.unknown = true),
    (value) => void (value.candidate.roots.unknown = hash(123)),
    (value) => void (value.ownerBefore.unknown = true),
    (value) => void (value.ownerAfter.ownerEpoch.unknown = true),
  ]) {
    const invalid = structuredClone(valid);
    mutate(invalid);
    assert.throws(() =>
      validateCommitCandidateProbeResult({
        result: invalid,
        transactions: 50_000,
        cpuSet: "2-9",
        fixtureSize: 1_000_000,
        inputPath: valid.inputPath,
        inputSha256: valid.inputSha256,
        probePath: valid.probePath,
        probeSha256: valid.probeSha256,
        binarySha256: valid.binarySha256,
      }),
    );
  }
});
