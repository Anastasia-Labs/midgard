import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  chmodSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  statSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import path from "node:path";
import { describe, it } from "node:test";
import { fileURLToPath } from "node:url";

import {
  capturePhase1CorpusIdentity,
  createSecretScanningLog,
  scanSubmitRecords,
  sha256File,
} from "./phase3-architecture-g-closure-lib.mjs";
import {
  resolvePhase3SoakTiming,
  writePhase3SoakSetupFailureReport,
} from "./phase3-architecture-g-soak.mjs";
import {
  consumePhase3SoakCorpusPreflight,
  createPhase3SoakCorpusPreflight,
  establishPhase3SoakPreflight,
  phase3SoakSourceIdentitySha256,
} from "./phase3-architecture-g-soak-preflight.mjs";
import {
  CORPUS_PREFIX_EVIDENCE_SCHEMA,
  loadCorpusIndex,
  loadCorpusManifest,
  openStreamingCorpusReader,
  scanCorpusPrefixEvidence,
  selectCorpusIndexEntries,
} from "./throughput-valid-stress-corpus.mjs";
import {
  PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA,
  PHASE3_NODE_PRE_LIFECYCLE_REVALIDATION_SCHEMA,
  capturePhase3ProcessIdentity,
  captureTrustedPhase3DockerRuntime,
  createPhase3LoadGeneratorIsolation,
  createPhase3NodePreLifecycleRevalidation,
  validatePhase3LoadGeneratorIsolationDocument,
  validatePhase3NodePreLifecycleRevalidationDocument,
} from "./phase3-architecture-g-load-generator-isolation.mjs";
import {
  evaluatePhase3ArchitectureGSoakReport,
  PHASE3_ARCHITECTURE_G_SAMPLE_INTERVAL_MS,
  PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC,
  PHASE3_ARCHITECTURE_G_SOAK_SCENARIO,
  PHASE3_ARCHITECTURE_G_SOAK_SCHEMA,
} from "./verify-phase3-architecture-g-soak-report.mjs";

const hash = (character) => character.repeat(64);
const dockerRuntime = () => ({
  schemaVersion: "midgard-phase3-trusted-docker-runtime-v1",
  client: {
    path: "/usr/bin/docker",
    realPath: "/trusted/docker",
    sha256: hash("d"),
    bytes: 1_024,
    mode: 0o755,
    uid: 0,
    gid: 0,
    dev: "1",
    ino: "2",
  },
  socket: {
    path: "/var/run/docker.sock",
    realPath: "/run/docker.sock",
    endpoint: "unix:///var/run/docker.sock",
    mode: 0o660,
    uid: 0,
    gid: 999,
    dev: "3",
    ino: "4",
  },
  daemon: {
    id: "daemon-id",
    name: "docker-desktop",
    serverVersion: "29.2.0",
    operatingSystem: "Docker Desktop",
    osType: "linux",
    architecture: "x86_64",
  },
  environment: {
    inheritedDockerVariables: [],
    pathResolutionRealPath: "/trusted/docker",
    daemonEndpoint: "unix:///var/run/docker.sock",
    home: "/nonexistent",
  },
});
const soakCliPath = fileURLToPath(
  new URL("./phase3-architecture-g-soak.mjs", import.meta.url),
);

const runCliSetupFailure = ({ args, env, evidenceDirectory, phase }) => {
  const result = spawnSync(process.execPath, [soakCliPath, ...args], {
    cwd: path.dirname(path.dirname(soakCliPath)),
    env: { ...process.env, ...env },
    encoding: "utf8",
    maxBuffer: 4 * 1024 * 1024,
  });
  assert.equal(
    result.status,
    1,
    JSON.stringify({ error: result.error?.message, signal: result.signal }),
  );
  const retainedReport = JSON.parse(
    readFileSync(path.join(evidenceDirectory, "report.json"), "utf8"),
  );
  const retainedVerification = JSON.parse(
    readFileSync(path.join(evidenceDirectory, "verification.json"), "utf8"),
  );
  assert.equal(retainedReport.termination.phase, phase);
  assert.equal(retainedReport.startedAtMs, null);
  assert.equal(retainedVerification.phase, phase);
  assert.equal(retainedVerification.passed, false);
};

const corpusRow = ({ chain, index, input }) => {
  const bytes = Buffer.from([chain, index]);
  const txHash = (chain * 16 + index).toString(16).padStart(2, "0").repeat(32);
  return {
    txHash,
    canonicalCborHex: bytes.toString("hex"),
    canonicalCborSha256: createHash("sha256").update(bytes).digest("hex"),
    canonicalCborByteLength: bytes.length,
    senderWalletId: `wallet-${chain.toString()}`,
    selectedInputOutref: input,
    outputOutrefs: [`${txHash}#0`, `${txHash}#1`],
    planShape: "chain",
    parentTxHash: null,
    corpusSliceId: "default",
  };
};

const makeCorpusPreflightFixture = async (directory) => {
  const corpusPath = path.join(directory, "corpus.ndjson");
  const indexPath = path.join(directory, "corpus.index.ndjson");
  const manifestPath = path.join(directory, "corpus.manifest.json");
  const phase1Path = path.join(directory, "phase1.json");
  const outPath = path.join(directory, "preflight.json");
  const rows = [
    corpusRow({ chain: 1, index: 1, input: `${"a".repeat(64)}#0` }),
    corpusRow({ chain: 2, index: 1, input: `${"b".repeat(64)}#0` }),
  ];
  const lines = rows.map((row) => `${JSON.stringify(row)}\n`);
  writeFileSync(corpusPath, lines.join(""));
  const stableCorpusTime = new Date(1_700_000_000_000);
  utimesSync(corpusPath, stableCorpusTime, stableCorpusTime);
  const firstBytes = Buffer.byteLength(lines[0]);
  const index = [
    {
      corpusSliceId: "default",
      planShape: "chain",
      chainId: "wallet-1",
      startByteOffset: 0,
      endByteOffset: firstBytes,
      rowCount: 1,
    },
    {
      corpusSliceId: "default",
      planShape: "chain",
      chainId: "wallet-2",
      startByteOffset: firstBytes,
      endByteOffset: Buffer.byteLength(lines.join("")),
      rowCount: 1,
    },
  ];
  writeFileSync(indexPath, `${index.map(JSON.stringify).join("\n")}\n`);
  const manifest = {
    schemaVersion: "midgard-stress-corpus-manifest-v1",
    chainCount: 2,
    chainDepth: 1,
    sliceSummary: [{ corpusSliceId: "default", walletCount: 2, rowCount: 2 }],
    files: {
      corpus: {
        sha256: sha256File(corpusPath),
        rowCount: 2,
      },
      index: {
        sha256: sha256File(indexPath),
        rowCount: 2,
      },
    },
  };
  writeFileSync(manifestPath, `${JSON.stringify(manifest)}\n`);
  const corpusIdentity = {
    path: corpusPath,
    indexPath,
    manifestPath,
    sliceId: "default",
    corpusSha256: sha256File(corpusPath),
    indexSha256: sha256File(indexPath),
    manifestSha256: sha256File(manifestPath),
  };
  const phase1Binding = {
    corpus: corpusIdentity,
    stressCorpusEnv: {
      STRESS_CORPUS_PATH: corpusPath,
      STRESS_CORPUS_INDEX_PATH: indexPath,
      STRESS_CORPUS_MANIFEST_PATH: manifestPath,
      STRESS_CORPUS_SLICE_ID: "default",
      STRESS_CORPUS_SHAPE: "chain",
    },
  };
  writeFileSync(phase1Path, `${JSON.stringify(phase1Binding)}\n`);
  const phase1BindingSha256 = sha256File(phase1Path);
  const sourceIdentity = { sourceTreeSha256: hash("c") };
  const artifact = await createPhase3SoakCorpusPreflight({
    outPath,
    phase1Binding,
    phase1BindingPath: phase1Path,
    phase1BindingSha256,
    sourceIdentity,
    corpusIdentity,
    corpusSliceId: "default",
    corpusShape: "chain",
  });
  const loadedManifest = await loadCorpusManifest(manifestPath);
  const fullIndex = await loadCorpusIndex(indexPath);
  const selectedEntries = selectCorpusIndexEntries({
    index: fullIndex,
    corpusSliceId: "default",
    corpusShape: "chain",
    maxChains: null,
  });
  return {
    artifact,
    corpusIdentity,
    phase1BindingSha256,
    sourceIdentity,
    loadedManifest,
    fullIndex,
    selectedEntries,
  };
};

const sample = (elapsedMs, overrides = {}) => ({
  observedAtMs: 1_750_000_000_000 + elapsedMs,
  elapsedMs,
  readiness: { httpStatus: 200, ready: true, reasons: [] },
  metrics: {
    auditDivergence: 0,
    auditAgeMs: 1_000,
    auditCompletedAtMs: 1_749_999_999_000,
    confirmedLedgerFullScanTotal: 9,
    validationWorkerTimeoutTotal: 3,
    l1ControlPlaneTimeoutTotal: 1,
    timeoutInsteadOfBackpressureTotal: 4,
    daPublicationBacklog: 0,
    mergeQueueDepth: 0,
  },
  owner: {
    durableRoot: hash("a"),
    residentNodes: 1_000_000,
    residentBytes: 700 * 1024 ** 2,
    activeGenerations: 0,
    generatedNodes: 500_000,
    generatedBytes: 500 * 1024 ** 2,
    rssBytes: 800 * 1024 ** 2,
    peakRssBytes: 900 * 1024 ** 2,
    childRestarts: 0,
  },
  process: {
    pid: 42,
    startTicks: "123456",
    rssBytes: Math.round(1024 ** 3 * (1 + 0.05 * (elapsedMs / 86_400_000))),
  },
  ...overrides,
});

const report = ({
  durationSec = 86_400,
  intervalMs = 60_000,
  testOnly = false,
} = {}) => {
  const durationMs = durationSec * 1_000;
  const logicalSubmitAttempts = durationSec * 5_000;
  const samples = [];
  const auditPeriodMs = 5 * 60 * 60_000;
  const makeSample = (elapsedMs) => {
    const value = sample(elapsedMs);
    const completedAuditCount = Math.floor(elapsedMs / auditPeriodMs);
    value.metrics.auditCompletedAtMs =
      1_750_000_000_000 - 1_000 + completedAuditCount * auditPeriodMs;
    value.metrics.auditAgeMs =
      value.observedAtMs - value.metrics.auditCompletedAtMs;
    value.metrics.confirmedLedgerFullScanTotal += completedAuditCount;
    return value;
  };
  for (let elapsedMs = 0; elapsedMs <= durationMs; elapsedMs += intervalMs) {
    samples.push(makeSample(elapsedMs));
  }
  if (samples.at(-1).elapsedMs < durationMs)
    samples.push(makeSample(durationMs));
  const value = {
    schemaVersion: PHASE3_ARCHITECTURE_G_SOAK_SCHEMA,
    scenario: PHASE3_ARCHITECTURE_G_SOAK_SCENARIO,
    testOnly,
    configuredDurationSec: durationSec,
    sampleIntervalMs: intervalMs,
    startedAtMs: 1_750_000_000_000,
    completedAtMs: 1_750_000_000_000 + durationMs,
    preflight: {
      startedAtMs: 1_749_999_999_800,
      completedAtMs: 1_749_999_999_975,
      durationMs: 175,
      lifecycleStartedAtMs: 1_750_000_000_000,
    },
    identity: {
      source: {
        gitCommit: "1".repeat(40),
        gitStatusSha256: hash("2"),
        trackedDiffSha256: hash("3"),
        sourceTreeSha256: hash("4"),
        sourceTreeFileCount: 100,
        nodeVersion: "v22.22.2",
        nodeExecutablePath: "/runtime/node-v22.22.2",
        nodeExecutableSha256: hash("0"),
      },
      runtime: {
        path: "/artifacts/runtime.json",
        sha256: hash("7"),
        schemaVersion: "midgard-phase4-environment-artifact-v1",
        deploymentManifestSha256: hash("8"),
        nodeImageId: `sha256:${hash("9")}`,
      },
      deployment: {
        path: "/artifacts/deployment.json",
        sha256: hash("8"),
        schemaVersion: "midgard-deployment-manifest-v1",
        manifestId: hash("a"),
      },
      ownerBinary: {
        path: "/artifacts/architecture-g-owner",
        sha256: hash("b"),
        expectedSha256: hash("b"),
        sha256ManifestPath: "/artifacts/architecture-g-owner.sha256",
        sha256ManifestSha256: hash("c"),
      },
      phase1: {
        path: "/artifacts/phase1-binding.json",
        sha256: hash("d"),
        schemaVersion: "midgard-phase1-live-corpus-binding-v1",
        deploymentManifestId: hash("a"),
        nodeImageId: `sha256:${hash("9")}`,
        nodeContainerId: hash("5"),
        corpus: {
          path: "/artifacts/corpus.ndjson",
          indexPath: "/artifacts/corpus.index.ndjson",
          manifestPath: "/artifacts/corpus.manifest.json",
          sliceId: "default",
          corpusSha256: hash("1"),
          indexSha256: hash("2"),
          manifestSha256: hash("3"),
        },
      },
      corpusPreflight: {
        path: "/artifacts/corpus-preflight.json",
        sha256: hash("a"),
        bytes: 1_024,
        schemaVersion: "midgard-phase3-soak-corpus-preflight-v1",
        sourceTreeSha256: hash("4"),
        sourceIdentitySha256: hash("9"),
        phase1BindingSha256: hash("d"),
        files: {},
        selection: { indexEntryCount: 1 },
        validation: {},
      },
      loadGeneratorIsolation: {
        path: "/artifacts/load-generator-isolation.json",
        sha256: hash("8"),
        bytes: 2_048,
        schemaVersion: PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA,
        placement: "measured-bounded-cgroup-v2",
        cohosted: true,
        clockOffsetMs: 0,
        loadGeneratorCpusAllowedList: "0-3",
        loadGeneratorEffectiveUid: 1000,
        nodeCpusAllowedList: "28-31",
        nodeContainerId: hash("5"),
        nodeImageId: `sha256:${hash("9")}`,
        nodeHostPid: 42,
        nodeStartTicks: "123456",
        readyUrl: "http://127.0.0.1:3000/readyz",
        metricsUrl: "http://127.0.0.1:9464/metrics",
        dockerClientRealPath: "/trusted/docker",
        dockerClientSha256: hash("d"),
        dockerSocketRealPath: "/run/docker.sock",
        dockerSocketDev: "3",
        dockerSocketIno: "4",
        dockerDaemonId: "daemon-id",
      },
      nodePreLifecycleRevalidation: {
        path: "/artifacts/node-pre-lifecycle-revalidation.json",
        sha256: hash("6"),
        bytes: 2_048,
        schemaVersion: PHASE3_NODE_PRE_LIFECYCLE_REVALIDATION_SCHEMA,
        observedAtMs: 1_749_999_999_950,
        isolationPath: "/artifacts/load-generator-isolation.json",
        isolationSha256: hash("8"),
        nodeContainerId: hash("5"),
        nodeImageId: `sha256:${hash("9")}`,
        nodeHostPid: 42,
        nodeStartTicks: "123456",
        nodeRestartCount: 0,
        nodeHealthStatus: "healthy",
        readyUrl: "http://127.0.0.1:3000/readyz",
        metricsUrl: "http://127.0.0.1:9464/metrics",
        dockerClientSha256: hash("d"),
        dockerSocketDev: "3",
        dockerSocketIno: "4",
        dockerDaemonId: "daemon-id",
      },
      tooling: {
        runnerPath: "/workspace/phase3-architecture-g-soak.mjs",
        runnerSha256: hash("5"),
        verifierPath: "/workspace/verify-phase3-architecture-g-soak-report.mjs",
        verifierSha256: hash("6"),
      },
    },
    sourceAtCompletion: null,
    workload: {
      scriptPath: "/workspace/throughput-valid-stress.mjs",
      scriptSha256: hash("e"),
      reportPath: "/artifacts/workload-report.json",
      reportSha256: hash("f"),
      reportBytes: 42,
      reportSummary: {
        scenario: PHASE3_ARCHITECTURE_G_SOAK_SCENARIO,
        scenarioClass: "B",
        benchmarkMode: "open",
        formalBenchmark: true,
        targetAcceptedTps: 5_000,
        openLoopRateTps: 5_000,
        measuredDurationSec: durationSec,
        warmupTxs: 0,
        warmupSec: 0,
        cooldownSec: 0,
        drainTimeoutSec: 600,
        offeredRateMinRatio: 0.98,
        acceptedRateMinRatio: 0.99,
        nodeSaturationMinRatio: 1,
        loadGenerator: {
          placement: "measured-cgroup",
          cohosted: true,
          clockOffsetMs: 0,
          isolation: null,
        },
        calibration: null,
        corpus: {
          path: "/artifacts/corpus.ndjson",
          indexPath: "/artifacts/corpus.index.ndjson",
          manifestPath: "/artifacts/corpus.manifest.json",
          sliceId: "default",
          artifactIdentity: {
            corpusSha256: hash("1"),
            indexSha256: hash("2"),
            manifestSha256: hash("3"),
          },
          preflight: {
            path: "/artifacts/corpus-preflight.json",
            sha256: hash("a"),
            bytes: 1_024,
            schemaVersion: "midgard-phase3-soak-corpus-preflight-v1",
            sourceTreeSha256: hash("4"),
            sourceIdentitySha256: hash("9"),
            phase1BindingSha256: hash("d"),
          },
          consumption: {
            schemaVersion: CORPUS_PREFIX_EVIDENCE_SCHEMA,
            rowCount: logicalSubmitAttempts,
            chains: [
              {
                chainIndex: 0,
                chainId: "wallet-1",
                rowCount: logicalSubmitAttempts,
                prefixSha256: hash("7"),
              },
            ],
          },
        },
        measuredElapsedSec: durationSec,
        offeredRatePerSec: 5_000,
        acceptedRatePerSec: 4_950,
        submitted: logicalSubmitAttempts,
        logicalSubmitAttempts,
        physicalSubmitAttempts: logicalSubmitAttempts,
        submitErrors: 0,
        rejectedDelta: 0,
        missingRequiredMetrics: [],
        allPrimaryStagesPassed: true,
        allPrimaryDrainsCompleted: true,
        primaryStageMeasurements: [
          {
            name: "measured-open",
            targetRateTps: 5_000,
            startedAtMs: 1_750_000_000_000,
            endedAtMs: 1_750_000_000_000 + durationMs,
            measuredElapsedSec: durationSec,
            logicalSubmitAttempts,
            physicalSubmitAttempts: logicalSubmitAttempts,
            submitted: logicalSubmitAttempts,
            submitErrors: 0,
            offeredRatePerSec: 5_000,
            acceptedRatePerSec: 4_950,
            nodeSaturationRatio: 5_000 / 4_950,
            nodeSaturationMinRatio: 1,
            nodeSaturationPassed: true,
            drainCompleted: true,
            drainElapsedMs: 0,
          },
        ],
      },
      submitRecords: {
        path: "/artifacts/submit-records.ndjson",
        sha256: hash("e"),
        bytes: 42,
        recordCount: logicalSubmitAttempts,
        successCount: logicalSubmitAttempts,
        errorCount: 0,
        timeoutCount: 0,
        attemptSequenceSha256: hash("f"),
      },
    },
    observation: {
      workloadSpawnedAtMs: 1_750_000_000_000,
      workloadExitedAtMs: 1_750_000_000_000 + durationMs,
      firstSampleAtMs: 1_750_000_000_000,
      lastSampleAtMs: 1_750_000_000_000 + durationMs,
    },
    termination: {
      completed: true,
      reason: "duration_completed",
      workloadExitCode: 0,
      workloadSignal: null,
      earlyExit: false,
      error: null,
    },
    samples,
  };
  const sourceIdentitySha256 = phase3SoakSourceIdentitySha256(
    value.identity.source,
  );
  value.identity.corpusPreflight.sourceIdentitySha256 = sourceIdentitySha256;
  value.workload.reportSummary.corpus.preflight.sourceIdentitySha256 =
    sourceIdentitySha256;
  value.preflight.initialReadiness = {
    ...structuredClone(samples[0]),
    observedAtMs: 1_749_999_999_900,
    elapsedMs: null,
  };
  value.preflight.nodePreLifecycleRevalidation =
    value.identity.nodePreLifecycleRevalidation;
  value.samples[0] = {
    ...structuredClone(value.preflight.initialReadiness),
    elapsedMs: 0,
  };
  value.observation.firstSampleAtMs = value.samples[0].observedAtMs;
  value.workload.reportSummary.loadGenerator.isolation =
    value.identity.loadGeneratorIsolation;
  value.sourceAtCompletion = { ...value.identity.source };
  return value;
};

describe("Phase 3 Architecture G 24-hour soak verifier", () => {
  it("accepts a complete 24-hour report with bounded samples and identities", () => {
    const result = evaluatePhase3ArchitectureGSoakReport(report());
    assert.equal(result.passed, true, result.reasons.join("\n"));
    assert.equal(result.metrics.sampleCount, 1_441);
    assert.equal(result.metrics.confirmedLedgerFullScanDelta, 4);
    assert.equal(result.metrics.backgroundAuditFullScanCount, 4);
    assert.equal(result.metrics.unplannedConfirmedLedgerFullScanDelta, 0);
    assert.equal(result.metrics.timeoutInsteadOfBackpressureDelta, 0);
    assert.ok(result.metrics.processMemoryGrowthRatio < 0.1);
  });

  it("fails closed on every safety, liveness, cadence, and cap regression", () => {
    const cases = [
      ["divergence", (value) => (value.samples[5].metrics.auditDivergence = 1)],
      [
        "stale audit",
        (value) => (value.samples[5].metrics.auditAgeMs = 99_999_999),
      ],
      [
        "missing metric",
        (value) => delete value.samples[5].metrics.daPublicationBacklog,
      ],
      [
        "NaN metric",
        (value) => (value.samples[5].metrics.mergeQueueDepth = Number.NaN),
      ],
      ["missing sample", (value) => value.samples.splice(10, 1)],
      ["not ready", (value) => (value.samples[5].readiness.ready = false)],
      [
        "full scan",
        (value) =>
          (value.samples.at(-1).metrics.confirmedLedgerFullScanTotal += 1),
      ],
      [
        "timeout",
        (value) =>
          (value.samples.at(-1).metrics.timeoutInsteadOfBackpressureTotal += 1),
      ],
      [
        "counter reset",
        (value) => (value.samples[5].metrics.validationWorkerTimeoutTotal = 0),
      ],
      [
        "DA growth",
        (value) =>
          value.samples.forEach(
            (entry, index) => (entry.metrics.daPublicationBacklog = index),
          ),
      ],
      [
        "merge growth",
        (value) =>
          value.samples.forEach(
            (entry, index) => (entry.metrics.mergeQueueDepth = index),
          ),
      ],
      [
        "owner nodes",
        (value) => (value.samples[5].owner.residentNodes = 2_000_001),
      ],
      [
        "owner RSS",
        (value) => (value.samples[5].owner.rssBytes = 2 * 1024 ** 3 + 1),
      ],
      [
        "generated nodes",
        (value) => (value.samples[5].owner.generatedNodes = 1_000_001),
      ],
      [
        "generated bytes",
        (value) => (value.samples[5].owner.generatedBytes = 1024 ** 3 + 1),
      ],
      ["owner restart", (value) => (value.samples[5].owner.childRestarts = 1)],
      [
        "process restart",
        (value) => (value.samples[5].process.startTicks = "654321"),
      ],
      ["decoy node PID", (value) => (value.samples[5].process.pid = 43)],
      [
        "isolation container drift",
        (value) =>
          (value.identity.loadGeneratorIsolation.nodeContainerId = hash("6")),
      ],
      [
        "root load generator",
        (value) =>
          (value.identity.loadGeneratorIsolation.loadGeneratorEffectiveUid = 0),
      ],
      [
        "process growth",
        (value) =>
          (value.samples.at(-1).process.rssBytes = Math.round(
            value.samples[0].process.rssBytes * 1.101,
          )),
      ],
      ["early exit", (value) => (value.termination.earlyExit = true)],
      [
        "preflight overlaps lifecycle",
        (value) => (value.preflight.completedAtMs = value.startedAtMs + 1),
      ],
      [
        "readiness observed after lifecycle",
        (value) =>
          (value.preflight.initialReadiness.observedAtMs =
            value.preflight.lifecycleStartedAtMs + 1),
      ],
      [
        "readiness served by decoy process",
        (value) => (value.preflight.initialReadiness.process.pid = 43),
      ],
      [
        "node reinspection after lifecycle",
        (value) =>
          (value.identity.nodePreLifecycleRevalidation.observedAtMs =
            value.preflight.lifecycleStartedAtMs + 1),
      ],
      [
        "recaptured first lifecycle sample",
        (value) => (value.samples[0].observedAtMs = value.startedAtMs + 1),
      ],
      [
        "preflight identity drift",
        (value) =>
          (value.workload.reportSummary.corpus.preflight.sha256 = hash("0")),
      ],
      [
        "workload error",
        (value) => (value.workload.reportSummary.submitErrors = 1),
      ],
      [
        "target drift",
        (value) => (value.workload.reportSummary.targetAcceptedTps = 4_999),
      ],
      [
        "stage target drift",
        (value) =>
          (value.workload.reportSummary.primaryStageMeasurements[0].targetRateTps = 4_999),
      ],
      [
        "achieved offered rate",
        (value) => (value.workload.reportSummary.offeredRatePerSec = 4_899),
      ],
      [
        "achieved accepted rate",
        (value) => (value.workload.reportSummary.acceptedRatePerSec = 4_949),
      ],
      [
        "inherited threshold drift",
        (value) => (value.workload.reportSummary.offeredRateMinRatio = 0.5),
      ],
      [
        "warmup injection",
        (value) => (value.workload.reportSummary.warmupTxs = 1),
      ],
      [
        "forged saturation ratio",
        (value) =>
          (value.workload.reportSummary.primaryStageMeasurements[0].nodeSaturationRatio = 1),
      ],
      [
        "measured duration short",
        (value) => (value.workload.reportSummary.measuredElapsedSec -= 1),
      ],
      [
        "sampling misses drain",
        (value) =>
          (value.observation.workloadExitedAtMs =
            value.observation.lastSampleAtMs + 1),
      ],
      [
        "submit cardinality drift",
        (value) => (value.workload.submitRecords.recordCount -= 1),
      ],
      [
        "physical attempt drift",
        (value) =>
          (value.workload.reportSummary.primaryStageMeasurements[0].physicalSubmitAttempts -= 1),
      ],
      [
        "lifecycle grace exceeded",
        (value) =>
          (value.observation.workloadExitedAtMs =
            value.observation.workloadSpawnedAtMs +
            value.configuredDurationSec * 1_000 +
            15 * 60_000 +
            1),
      ],
      [
        "corpus hash drift",
        (value) =>
          (value.workload.reportSummary.corpus.artifactIdentity.corpusSha256 =
            hash("0")),
      ],
      [
        "submit timeout residue",
        (value) => (value.workload.submitRecords.timeoutCount = 1),
      ],
      [
        "source drift",
        (value) => (value.sourceAtCompletion.sourceTreeSha256 = hash("0")),
      ],
      [
        "preflight source identity digest drift",
        (value) =>
          (value.identity.corpusPreflight.sourceIdentitySha256 = hash("0")),
      ],
      [
        "unbound binary",
        (value) => (value.identity.ownerBinary.expectedSha256 = hash("0")),
      ],
    ];
    for (const [label, mutate] of cases) {
      const value = report();
      mutate(value);
      const result = evaluatePhase3ArchitectureGSoakReport(value);
      assert.equal(result.passed, false, `${label} unexpectedly passed`);
    }
  });

  it("does not allow the test-only duration path to satisfy production", () => {
    const value = report({ durationSec: 2, intervalMs: 500, testOnly: true });
    assert.equal(evaluatePhase3ArchitectureGSoakReport(value).passed, false);
    assert.equal(
      evaluatePhase3ArchitectureGSoakReport(value, {
        allowTestOnlyDuration: true,
      }).passed,
      true,
    );
  });
});

describe("Phase 3 submit-record streaming scanner", () => {
  const record = ({ txHash = hash("a"), error = null } = {}) => ({
    txHash,
    scheduledAtMs: 1_750_000_000_000,
    submittedAtMs: 1_750_000_000_001,
    scheduleSlipMs: 1,
    latencyMs: 2,
    statusCode: error === null ? 200 : null,
    responseTxId: error === null ? txHash : null,
    error,
  });

  it("records exact bytes, digest, cardinality, and timeout count", async () => {
    const directory = mkdtempSync("/tmp/midgard-phase3-submit-");
    try {
      const filePath = path.join(directory, "submit.ndjson");
      writeFileSync(
        filePath,
        `${JSON.stringify(record())}\n${JSON.stringify(
          record({
            txHash: hash("b"),
            error: "timed out waiting for admission",
          }),
        )}\n`,
      );
      const result = await scanSubmitRecords(filePath);
      assert.equal(result.recordCount, 2);
      assert.equal(result.successCount, 1);
      assert.equal(result.errorCount, 1);
      assert.equal(result.timeoutCount, 1);
      assert.ok(result.bytes > 0);
      assert.match(result.sha256, /^[0-9a-f]{64}$/u);
      assert.match(result.attemptSequenceSha256, /^[0-9a-f]{64}$/u);
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });

  it("rejects malformed, empty, and oversized NDJSON without whole-file reads", async () => {
    const directory = mkdtempSync("/tmp/midgard-phase3-submit-");
    try {
      const malformed = path.join(directory, "malformed.ndjson");
      const empty = path.join(directory, "empty.ndjson");
      const oversized = path.join(directory, "oversized.ndjson");
      writeFileSync(malformed, "not-json\n");
      writeFileSync(empty, "");
      writeFileSync(
        oversized,
        `${JSON.stringify(record({ error: "x".repeat(1024 * 1024) }))}\n`,
      );
      await assert.rejects(scanSubmitRecords(malformed), /invalid/u);
      await assert.rejects(scanSubmitRecords(empty), /no records/u);
      await assert.rejects(scanSubmitRecords(oversized), /exceeds/u);
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });

  it("rejects arbitrary JSON and malformed attempt identities", async () => {
    const directory = mkdtempSync("/tmp/midgard-phase3-submit-");
    try {
      const arbitrary = path.join(directory, "arbitrary.ndjson");
      const badIdentity = path.join(directory, "bad-identity.ndjson");
      writeFileSync(arbitrary, `${JSON.stringify({ error: null })}\n`);
      writeFileSync(
        badIdentity,
        `${JSON.stringify(record({ txHash: "not-a-tx-hash" }))}\n`,
      );
      await assert.rejects(scanSubmitRecords(arbitrary), /schema/u);
      await assert.rejects(scanSubmitRecords(badIdentity), /schema/u);
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
});

describe("Phase 3 retained driver-log secret scanner", () => {
  it("redacts before retention and marks secret-bearing evidence failed", async () => {
    const directory = mkdtempSync("/tmp/midgard-phase3-log-");
    try {
      const filePath = path.join(directory, "driver.log");
      const log = createSecretScanningLog(filePath);
      const secret = "never-retain-this-value";
      log.stream.write("diagnostic line\n");
      log.stream.write(`POSTGRES_PASSWORD=${secret}\n`);
      log.stream.end();
      const artifact = await log.complete();
      const retained = readFileSync(filePath, "utf8");
      assert.match(retained, /diagnostic line/u);
      assert.match(retained, /REDACTED/u);
      assert.doesNotMatch(retained, new RegExp(secret, "u"));
      assert.equal(artifact.secretScan.passed, false);
      assert.equal(artifact.secretScan.sensitiveLineCount, 1);
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
});

describe("Phase 3 closure corpus identity", () => {
  it("binds all three immutable artifacts and rejects path or byte drift", () => {
    const directory = mkdtempSync("/tmp/midgard-phase3-corpus-");
    try {
      const corpusPath = path.join(directory, "corpus.ndjson");
      const indexPath = path.join(directory, "corpus.index.ndjson");
      const manifestPath = path.join(directory, "corpus.manifest.json");
      writeFileSync(corpusPath, '{"tx":"01"}\n');
      writeFileSync(indexPath, '{"slice":"default"}\n');
      writeFileSync(manifestPath, '{"schemaVersion":"fixture"}\n');
      const phase1 = {
        corpus: {
          path: corpusPath,
          indexPath,
          manifestPath,
          sliceId: "default",
          corpusSha256: sha256File(corpusPath),
          indexSha256: sha256File(indexPath),
          manifestSha256: sha256File(manifestPath),
        },
        stressCorpusEnv: {
          STRESS_CORPUS_PATH: corpusPath,
          STRESS_CORPUS_INDEX_PATH: indexPath,
          STRESS_CORPUS_MANIFEST_PATH: manifestPath,
          STRESS_CORPUS_SLICE_ID: "default",
        },
      };
      assert.deepEqual(capturePhase1CorpusIdentity(phase1), phase1.corpus);

      const wrongPath = structuredClone(phase1);
      wrongPath.stressCorpusEnv.STRESS_CORPUS_PATH = indexPath;
      assert.throws(
        () => capturePhase1CorpusIdentity(wrongPath),
        /environment diverges/u,
      );

      writeFileSync(corpusPath, '{"tx":"tampered"}\n');
      assert.throws(
        () => capturePhase1CorpusIdentity(phase1),
        /bound SHA-256/u,
      );
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
});

describe("Phase 3 soak full-corpus preflight", () => {
  const consume = (fixture, overrides = {}) =>
    consumePhase3SoakCorpusPreflight({
      artifactPath: fixture.artifact.path,
      artifactSha256: fixture.artifact.sha256,
      expectedSourceIdentitySha256: fixture.artifact.sourceIdentitySha256,
      expectedPhase1BindingSha256: fixture.phase1BindingSha256,
      corpusPath: fixture.corpusIdentity.path,
      indexPath: fixture.corpusIdentity.indexPath,
      manifestPath: fixture.corpusIdentity.manifestPath,
      manifest: fixture.loadedManifest,
      fullIndex: fixture.fullIndex,
      selectedEntries: fixture.selectedEntries,
      corpusSliceId: "default",
      corpusShape: "chain",
      ...overrides,
    });

  it("binds full SHA, cardinality, uniqueness, manifest, and source identity", async () => {
    const directory = mkdtempSync("/tmp/midgard-phase3-preflight-");
    try {
      const fixture = await makeCorpusPreflightFixture(directory);
      const consumed = consume(fixture);
      assert.equal(consumed.validation.rowCount, 2);
      assert.equal(consumed.validation.uniqueTxHashes, 2);
      assert.equal(consumed.validation.uniqueSelectedInputs, 2);
      assert.equal(consumed.artifactIdentity.sha256, fixture.artifact.sha256);
      const reader = openStreamingCorpusReader({
        corpusPath: fixture.corpusIdentity.path,
        indexEntries: fixture.selectedEntries,
        readAheadRows: 1,
      });
      await reader[0].takeNextTx();
      await reader[1].takeNextTx();
      const consumption = reader.consumptionSnapshot();
      await reader.close();
      const scanned = await scanCorpusPrefixEvidence({
        corpusPath: fixture.corpusIdentity.path,
        fullIndex: fixture.fullIndex,
        selectedEntries: fixture.selectedEntries,
        consumption,
        expectedCorpusSha256: fixture.corpusIdentity.corpusSha256,
      });
      assert.equal(scanned.consumedRowCount, 2);
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });

  it("rejects stale source, path/mtime drift, tampering, and reduced selection", async () => {
    for (const mutate of [
      (fixture) => ({ expectedSourceIdentitySha256: hash("d") }),
      (fixture) => {
        writeFileSync(fixture.corpusIdentity.path, "tampered\n");
        return {};
      },
      (fixture) => {
        const alternatePath = path.join(
          path.dirname(fixture.corpusIdentity.path),
          "alternate-corpus.ndjson",
        );
        writeFileSync(alternatePath, readFileSync(fixture.corpusIdentity.path));
        return { corpusPath: alternatePath };
      },
      (fixture) => {
        utimesSync(
          fixture.corpusIdentity.path,
          new Date(1_000),
          new Date(1_000),
        );
        return {};
      },
      (fixture) => {
        writeFileSync(fixture.artifact.path, '{"tampered":true}\n');
        return {};
      },
      (fixture) => ({ selectedEntries: fixture.selectedEntries.slice(0, 1) }),
    ]) {
      const directory = mkdtempSync("/tmp/midgard-phase3-preflight-");
      try {
        const fixture = await makeCorpusPreflightFixture(directory);
        assert.throws(() => consume(fixture, mutate(fixture)));
      } finally {
        rmSync(directory, { recursive: true, force: true });
      }
    }
  });

  it("detects same-size corpus replacement with restored mtime in consumed-prefix evidence", async () => {
    const directory = mkdtempSync("/tmp/midgard-phase3-preflight-");
    try {
      const fixture = await makeCorpusPreflightFixture(directory);
      const original = readFileSync(fixture.corpusIdentity.path);
      const originalStat = statSync(fixture.corpusIdentity.path);
      const originalLines = original.toString("utf8").trimEnd().split("\n");
      const replacement = Buffer.from(
        `${JSON.stringify(
          corpusRow({ chain: 1, index: 2, input: `${"a".repeat(64)}#0` }),
        )}\n${originalLines[1]}\n`,
      );
      assert.equal(replacement.byteLength, original.byteLength);
      writeFileSync(fixture.corpusIdentity.path, replacement);
      utimesSync(
        fixture.corpusIdentity.path,
        originalStat.atime,
        originalStat.mtime,
      );
      assert.doesNotThrow(() => consume(fixture));

      const reader = openStreamingCorpusReader({
        corpusPath: fixture.corpusIdentity.path,
        indexEntries: fixture.selectedEntries,
        readAheadRows: 1,
      });
      await reader[0].takeNextTx();
      const consumption = reader.consumptionSnapshot();
      await reader.close();

      writeFileSync(fixture.corpusIdentity.path, original);
      utimesSync(
        fixture.corpusIdentity.path,
        originalStat.atime,
        originalStat.mtime,
      );
      await assert.rejects(
        scanCorpusPrefixEvidence({
          corpusPath: fixture.corpusIdentity.path,
          fullIndex: fixture.fullIndex,
          selectedEntries: fixture.selectedEntries,
          consumption,
          expectedCorpusSha256: fixture.corpusIdentity.corpusSha256,
        }),
        /consumed corpus prefix changed/u,
      );
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });

  it("assigns lifecycle time only after readiness, metrics, and reinspection complete", async () => {
    const timestamps = [10, 20, 30];
    const completedSteps = [];
    const established = await establishPhase3SoakPreflight({
      runPreflight: async () => {
        completedSteps.push("corpus", "isolation", "readiness-metrics");
        await Promise.resolve();
        completedSteps.push("docker-reinspection");
        return { sha256: hash("e") };
      },
      now: () => timestamps.shift(),
    });
    assert.deepEqual(completedSteps, [
      "corpus",
      "isolation",
      "readiness-metrics",
      "docker-reinspection",
    ]);
    assert.equal(established.completedAtMs, 20);
    assert.equal(established.lifecycleStartedAtMs, 30);

    let clockCalls = 0;
    let readinessAttempted = false;
    await assert.rejects(
      establishPhase3SoakPreflight({
        runPreflight: async () => {
          readinessAttempted = true;
          await Promise.resolve();
          throw new Error("initial readiness and metrics failed");
        },
        now: () => {
          clockCalls += 1;
          return clockCalls;
        },
      }),
      /initial readiness and metrics failed/u,
    );
    assert.equal(readinessAttempted, true);
    assert.equal(clockCalls, 1);
  });
});

describe("Phase 3 soak timing", () => {
  it("hard-codes the production duration and interval", () => {
    assert.deepEqual(resolvePhase3SoakTiming({}), {
      durationSec: PHASE3_ARCHITECTURE_G_SOAK_DURATION_SEC,
      sampleIntervalMs: PHASE3_ARCHITECTURE_G_SAMPLE_INTERVAL_MS,
      testOnly: false,
    });
  });

  it("rejects short-duration injection outside an explicit test process", () => {
    assert.throws(
      () =>
        resolvePhase3SoakTiming({
          PHASE3_SOAK_TEST_DURATION_SEC: "1",
          PHASE3_SOAK_TEST_SAMPLE_INTERVAL_MS: "100",
        }),
      /require NODE_ENV=test/u,
    );
  });

  it("permits only bounded test timing under both test guards", () => {
    assert.deepEqual(
      resolvePhase3SoakTiming({
        NODE_ENV: "test",
        PHASE3_SOAK_TEST_MODE: "1",
        PHASE3_SOAK_TEST_DURATION_SEC: "2",
        PHASE3_SOAK_TEST_SAMPLE_INTERVAL_MS: "500",
      }),
      { durationSec: 2, sampleIntervalMs: 500, testOnly: true },
    );
  });
});

describe("Phase 3 pre-lifecycle evidence and load-generator isolation", () => {
  it("rejects a start-tick change during the final process identity field capture", () => {
    const pid = 42;
    const procRoot = `/proc/${pid.toString()}`;
    let namespaceCaptured = false;
    let statReads = 0;
    const processStat = (startTicks) => {
      const fields = Array.from({ length: 20 }, () => "0");
      fields[0] = "S";
      fields[19] = startTicks;
      return `${pid.toString()} (node) ${fields.join(" ")}\n`;
    };
    const readFileSync = (filePath) => {
      if (filePath === `${procRoot}/stat`) {
        statReads += 1;
        return processStat(namespaceCaptured ? "101" : "100");
      }
      if (filePath === `${procRoot}/status`) {
        return "Uid:\t1000\t1000\t1000\t1000\nCpus_allowed_list:\t0-3\n";
      }
      if (filePath === `${procRoot}/cgroup`) return "0::/phase3-test\n";
      if (filePath === `${procRoot}/cmdline`) return Buffer.from("node\0");
      if (filePath === "/sys/fs/cgroup/phase3-test/memory.max") {
        return "8589934592\n";
      }
      if (filePath === "/sys/fs/cgroup/phase3-test/cpu.max") {
        return "max 100000\n";
      }
      if (filePath === "/sys/fs/cgroup/phase3-test/cpuset.cpus.effective") {
        return "0-3\n";
      }
      if (filePath === "/proc/sys/kernel/random/boot_id") {
        return "boot-id\n";
      }
      throw new Error(`unexpected fixture read ${filePath}`);
    };
    const readlinkSync = (filePath) => {
      if (filePath === `${procRoot}/exe`) return "/usr/bin/node";
      if (filePath === `${procRoot}/ns/pid`) {
        namespaceCaptured = true;
        return "pid:[42]";
      }
      throw new Error(`unexpected fixture link ${filePath}`);
    };

    assert.throws(
      () => capturePhase3ProcessIdentity(pid, { readFileSync, readlinkSync }),
      /PID 42 changed during identity capture/u,
    );
    assert.equal(namespaceCaptured, true);
    assert.equal(statReads, 2);
  });

  it("rejects hostile Docker environment overrides and PATH interception", async () => {
    for (const name of ["DOCKER_HOST", "DOCKER_CONTEXT", "DOCKER_CONFIG"]) {
      await assert.rejects(
        captureTrustedPhase3DockerRuntime({
          env: { ...process.env, [name]: "hostile" },
          execDocker: async () => {
            throw new Error("must not execute Docker");
          },
        }),
        new RegExp(`${name} must be unset`, "u"),
      );
    }
    const directory = mkdtempSync("/tmp/midgard-phase3-hostile-docker-");
    try {
      const fakeDocker = path.join(directory, "docker");
      writeFileSync(fakeDocker, "#!/bin/sh\nexit 0\n");
      chmodSync(fakeDocker, 0o755);
      await assert.rejects(
        captureTrustedPhase3DockerRuntime({
          env: { ...process.env, PATH: `${directory}:/usr/bin:/bin` },
          execDocker: async () => {
            throw new Error("must not execute hostile Docker");
          },
        }),
        /PATH does not resolve Docker to the trusted absolute client/u,
      );
      await assert.rejects(
        captureTrustedPhase3DockerRuntime({
          env: { ...process.env, PATH: `.:/usr/bin:/bin` },
          execDocker: async () => {
            throw new Error("must not execute Docker");
          },
        }),
        /empty or relative/u,
      );
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }

    const calls = [];
    const runtime = await captureTrustedPhase3DockerRuntime({
      env: process.env,
      execDocker: async (command, args, options) => {
        calls.push({ command, args, options });
        return {
          stdout: JSON.stringify({
            ID: "daemon-id",
            Name: "docker-desktop",
            ServerVersion: "29.2.0",
            OperatingSystem: "Docker Desktop",
            OSType: "linux",
            Architecture: "x86_64",
          }),
        };
      },
    });
    assert.equal(calls.length, 1);
    assert.equal(calls[0].command, runtime.client.realPath);
    assert.equal(calls[0].options.env.PATH, "/usr/bin:/bin");
    assert.equal(
      calls[0].options.env.DOCKER_HOST,
      "unix:///var/run/docker.sock",
    );
    assert.equal(calls[0].options.env.HOME, "/nonexistent");
    assert.equal(calls[0].options.env.DOCKER_CONTEXT, undefined);
    assert.equal(calls[0].options.env.DOCKER_CONFIG, undefined);
  });

  it("retains real CLI output, timing, and endpoint parsing failures with exact phases", () => {
    const parent = mkdtempSync("/tmp/midgard-phase3-cli-failure-");
    try {
      const outputFallback = path.join(parent, "missing-output-argument");
      runCliSetupFailure({
        args: [],
        env: { PHASE3_SOAK_FAILURE_OUT_DIR: outputFallback },
        evidenceDirectory: outputFallback,
        phase: "output-directory",
      });

      const timingOut = path.join(parent, "timing");
      runCliSetupFailure({
        args: ["--out-dir", timingOut],
        env: {
          NODE_ENV: "test",
          PHASE3_SOAK_TEST_MODE: "1",
          PHASE3_SOAK_TEST_DURATION_SEC: "0",
          PHASE3_SOAK_TEST_SAMPLE_INTERVAL_MS: "500",
        },
        evidenceDirectory: timingOut,
        phase: "timing",
      });

      const readinessOut = path.join(parent, "readiness-endpoint");
      runCliSetupFailure({
        args: ["--out-dir", readinessOut],
        env: {
          NODE_ENV: "test",
          PHASE3_SOAK_TEST_MODE: "1",
          PHASE3_SOAK_TEST_DURATION_SEC: "2",
          PHASE3_SOAK_TEST_SAMPLE_INTERVAL_MS: "500",
        },
        evidenceDirectory: readinessOut,
        phase: "endpoint-arguments",
      });

      const metricsOut = path.join(parent, "metrics-endpoint");
      runCliSetupFailure({
        args: [
          "--out-dir",
          metricsOut,
          "--ready-url",
          "http://127.0.0.1:3000/readyz",
        ],
        env: {
          NODE_ENV: "test",
          PHASE3_SOAK_TEST_MODE: "1",
          PHASE3_SOAK_TEST_DURATION_SEC: "2",
          PHASE3_SOAK_TEST_SAMPLE_INTERVAL_MS: "500",
        },
        evidenceDirectory: metricsOut,
        phase: "endpoint-arguments",
      });
    } finally {
      rmSync(parent, { recursive: true, force: true });
    }
  });

  it("retains failed report and verification artifacts for every setup phase", () => {
    for (const phase of [
      "output-directory",
      "timing",
      "runtime",
      "endpoint-arguments",
      "arguments",
      "closure-identity",
      "corpus-preflight",
      "source-revalidation",
      "load-generator-isolation",
      "initial-readiness",
      "node-pre-lifecycle-reinspection",
    ]) {
      const directory = mkdtempSync("/tmp/midgard-phase3-setup-failure-");
      try {
        const reportPath = path.join(directory, "report.json");
        const verificationPath = path.join(directory, "verification.json");
        writePhase3SoakSetupFailureReport({
          reportPath,
          verificationPath,
          timing: {
            durationSec: 2,
            sampleIntervalMs: 500,
            testOnly: true,
          },
          phase,
          error: new Error(`${phase} failed`),
        });
        const retainedReport = JSON.parse(readFileSync(reportPath, "utf8"));
        const retainedVerification = JSON.parse(
          readFileSync(verificationPath, "utf8"),
        );
        assert.equal(retainedReport.termination.phase, phase);
        assert.equal(retainedReport.termination.completed, false);
        assert.equal(retainedVerification.phase, phase);
        assert.equal(retainedVerification.passed, false);
        assert.equal(retainedVerification.reportSha256, sha256File(reportPath));
      } finally {
        rmSync(directory, { recursive: true, force: true });
      }
    }
  });

  it("binds Docker-inspected node identity and rejects root or decoy processes", async () => {
    const processIdentity = ({
      pid,
      cgroup,
      cpusAllowedList,
      pidNamespace,
      effectiveUid = 1000,
    }) => ({
      pid,
      startTicks: pid.toString(),
      uid: {
        real: effectiveUid,
        effective: effectiveUid,
        savedSet: effectiveUid,
        fileSystem: effectiveUid,
      },
      executable: "/usr/local/bin/node",
      commandLineSha256: hash("f"),
      cgroup: `0::${cgroup}`,
      cgroupV2: {
        path: cgroup,
        memoryMax: String(8 * 1024 ** 3),
        cpuMax: "max 100000",
        cpusetEffective: cpusAllowedList,
      },
      cpusAllowedList,
      pidNamespace,
      bootId: "boot-id",
    });
    const document = {
      schemaVersion: PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA,
      capturedAtMs: 1,
      docker: dockerRuntime(),
      placement: "measured-bounded-cgroup-v2",
      cohosted: true,
      clock: { source: "shared-linux-kernel", offsetMs: 0, bootId: "boot-id" },
      loadGenerator: processIdentity({
        pid: 100,
        cgroup: "/loadgen",
        cpusAllowedList: "0-3",
        pidNamespace: "pid:[1]",
      }),
      nodeContainer: {
        phase1ContainerId: hash("a"),
        phase1ImageId: `sha256:${hash("b")}`,
        inspectedContainerId: hash("a"),
        inspectedImageId: `sha256:${hash("b")}`,
        configuredImageReference: "midgard-node:phase3",
        hostPid: 200,
        hostProcessStartTicks: "200",
        running: true,
        status: "running",
        healthStatus: "healthy",
        startedAt: "2026-07-14T12:00:00.000000000Z",
        restartCount: 0,
        engine: "architecture_g",
        healthcheckCommand: [
          "CMD",
          "node",
          "fetch('http://127.0.0.1:3000/readyz')",
        ],
        readyEndpoint: {
          url: "http://127.0.0.1:3000/readyz",
          protocol: "http:",
          hostname: "127.0.0.1",
          hostPort: "3000",
          pathname: "/readyz",
          containerPort: "3000/tcp",
          publishedHostIp: "0.0.0.0",
        },
        metricsEndpoint: {
          url: "http://127.0.0.1:9464/metrics",
          protocol: "http:",
          hostname: "127.0.0.1",
          hostPort: "9464",
          pathname: "/metrics",
          containerPort: "9464/tcp",
          publishedHostIp: "0.0.0.0",
        },
      },
      node: processIdentity({
        pid: 200,
        cgroup: "/node",
        cpusAllowedList: "28-31",
        pidNamespace: "pid:[2]",
      }),
      checks: {
        distinctCgroup: true,
        distinctPidNamespace: true,
        disjointCpuAffinity: true,
        sharedBootClock: true,
        nonRootLoadGenerator: true,
        exactPhase1Container: true,
        exactPhase1Image: true,
        hostPidFromDockerInspect: true,
        readinessPublishedByNodeContainer: true,
        metricsPublishedByNodeContainer: true,
        stableAfterProcCapture: true,
      },
    };
    assert.equal(
      validatePhase3LoadGeneratorIsolationDocument(document),
      document,
    );
    const directory = mkdtempSync("/tmp/midgard-phase3-isolation-");
    try {
      const outPath = path.join(directory, "isolation.json");
      const loadGeneratorIdentity = {
        ...document.loadGenerator,
        pid: process.pid,
        startTicks: process.pid.toString(),
      };
      let createInspectCount = 0;
      const summary = await createPhase3LoadGeneratorIsolation({
        outPath,
        phase1NodeContainerId: document.nodeContainer.phase1ContainerId,
        phase1NodeImageId: document.nodeContainer.phase1ImageId,
        readyUrl: document.nodeContainer.readyEndpoint.url,
        metricsUrl: document.nodeContainer.metricsEndpoint.url,
        env: {
          STRESS_LOAD_GENERATOR_PLACEMENT: "measured-cgroup",
          STRESS_LOADGEN_COHOSTED: "true",
          STRESS_CLOCK_OFFSET_MS: "0",
        },
        captureDockerRuntime: async () => structuredClone(document.docker),
        inspectContainer: async () => {
          createInspectCount += 1;
          return structuredClone(document.nodeContainer);
        },
        readProcessIdentity: (pid) =>
          pid === process.pid ? loadGeneratorIdentity : document.node,
      });
      assert.equal(summary.nodeContainerId, hash("a"));
      assert.equal(summary.nodeHostPid, 200);
      assert.equal(summary.loadGeneratorEffectiveUid, 1000);
      assert.equal(createInspectCount, 2);
      const retained = JSON.parse(readFileSync(outPath, "utf8"));
      assert.equal(retained.node.pid, retained.nodeContainer.hostPid);
      assert.equal(retained.node.startTicks, "200");
      validatePhase3LoadGeneratorIsolationDocument(retained, {
        expectedNodeContainerId: hash("a"),
        expectedNodeImageId: `sha256:${hash("b")}`,
      });
      const revalidationPath = path.join(directory, "revalidation.json");
      let revalidationInspectCount = 0;
      const revalidation = await createPhase3NodePreLifecycleRevalidation({
        outPath: revalidationPath,
        isolationArtifactPath: outPath,
        isolationArtifactSha256: summary.sha256,
        env: {},
        captureDockerRuntime: async () => structuredClone(document.docker),
        inspectContainer: async () => {
          revalidationInspectCount += 1;
          return structuredClone(document.nodeContainer);
        },
        readProcessIdentity: (pid) =>
          pid === process.pid ? loadGeneratorIdentity : document.node,
      });
      assert.equal(revalidation.nodeContainerId, hash("a"));
      assert.equal(revalidation.nodeHostPid, 200);
      assert.equal(revalidationInspectCount, 2);
      const retainedRevalidation = JSON.parse(
        readFileSync(revalidationPath, "utf8"),
      );
      validatePhase3NodePreLifecycleRevalidationDocument(
        retainedRevalidation,
        retained,
      );

      let unstableInspectCount = 0;
      await assert.rejects(
        createPhase3LoadGeneratorIsolation({
          outPath: path.join(directory, "unstable-isolation.json"),
          phase1NodeContainerId: document.nodeContainer.phase1ContainerId,
          phase1NodeImageId: document.nodeContainer.phase1ImageId,
          readyUrl: document.nodeContainer.readyEndpoint.url,
          metricsUrl: document.nodeContainer.metricsEndpoint.url,
          env: {
            STRESS_LOAD_GENERATOR_PLACEMENT: "measured-cgroup",
            STRESS_LOADGEN_COHOSTED: "true",
            STRESS_CLOCK_OFFSET_MS: "0",
          },
          captureDockerRuntime: async () => structuredClone(document.docker),
          inspectContainer: async () => {
            unstableInspectCount += 1;
            return {
              ...structuredClone(document.nodeContainer),
              restartCount:
                unstableInspectCount === 1
                  ? document.nodeContainer.restartCount
                  : document.nodeContainer.restartCount + 1,
            };
          },
          readProcessIdentity: (pid) =>
            pid === process.pid ? loadGeneratorIdentity : document.node,
        }),
        /container changed during process identity capture/u,
      );
      await assert.rejects(
        createPhase3NodePreLifecycleRevalidation({
          outPath: path.join(directory, "unstable-revalidation.json"),
          isolationArtifactPath: outPath,
          isolationArtifactSha256: summary.sha256,
          env: {},
          captureDockerRuntime: async () => structuredClone(document.docker),
          inspectContainer: async () => ({
            ...structuredClone(document.nodeContainer),
            healthStatus: "unhealthy",
          }),
          readProcessIdentity: (pid) =>
            pid === process.pid ? loadGeneratorIdentity : document.node,
        }),
        /container changed before lifecycle revalidation/u,
      );
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
    assert.throws(
      () =>
        validatePhase3LoadGeneratorIsolationDocument({
          ...document,
          loadGenerator: {
            ...document.loadGenerator,
            cgroup: "0::/",
            cgroupV2: {
              ...document.loadGenerator.cgroupV2,
              path: "/",
              memoryMax: "max",
            },
          },
        }),
      /formal load generator/u,
    );
    assert.throws(
      () =>
        validatePhase3LoadGeneratorIsolationDocument({
          ...document,
          loadGenerator: {
            ...document.loadGenerator,
            uid: { ...document.loadGenerator.uid, effective: 0 },
          },
        }),
      /formal load generator/u,
    );
    assert.throws(
      () =>
        validatePhase3LoadGeneratorIsolationDocument({
          ...document,
          nodeContainer: { ...document.nodeContainer, hostPid: 201 },
        }),
      /Phase 1 node-container binding/u,
    );
    assert.throws(
      () =>
        validatePhase3LoadGeneratorIsolationDocument(document, {
          expectedNodeContainerId: hash("c"),
          expectedNodeImageId: `sha256:${hash("b")}`,
        }),
      /Phase 1 node-container binding/u,
    );
  });
});
