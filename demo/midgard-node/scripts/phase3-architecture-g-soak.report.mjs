import {
  PHASE3_LOAD_GENERATOR_ISOLATION_SCHEMA,
  PHASE3_NODE_PRE_LIFECYCLE_REVALIDATION_SCHEMA,
} from "./phase3-architecture-g-load-generator-isolation.mjs";
import {
  hash,
  sample,
} from "./phase3-architecture-g-soak.make-corpus-preflight-fixture.mjs";
import { phase3SoakSourceIdentitySha256 } from "./phase3-architecture-g-soak-preflight.mjs";
import { CORPUS_PREFIX_EVIDENCE_SCHEMA } from "./throughput-valid-stress-corpus.mjs";
import {
  PHASE3_ARCHITECTURE_G_SOAK_SCENARIO,
  PHASE3_ARCHITECTURE_G_SOAK_SCHEMA,
} from "./verify-phase3-architecture-g-soak-report.mjs";

export const report = ({
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
        files: Object.fromEntries(
          ["corpus", "index", "manifest"].map((name, index) => [
            name,
            {
              path: `/artifacts/${name}`,
              bytes: 1_024 + index,
              mtimeMs: 1_700_000_000_000,
              dev: "1",
              ino: (index + 1).toString(),
              sha256: hash(String(index + 1)),
            },
          ]),
        ),
        selection: {
          corpusSliceId: "default",
          corpusShape: "chain",
          indexEntryCount: 1,
          rowCount: logicalSubmitAttempts,
          indexEntriesSha256: hash("7"),
        },
        validation: {
          rowCount: logicalSubmitAttempts,
          uniqueTxHashes: logicalSubmitAttempts,
          uniqueSelectedInputs: logicalSubmitAttempts,
        },
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
          shape: "chain",
          validation: {
            rowCount: logicalSubmitAttempts,
            uniqueTxHashes: logicalSubmitAttempts,
            uniqueSelectedInputs: logicalSubmitAttempts,
          },
          artifactIdentity: {
            corpusSha256: hash("1"),
            indexSha256: hash("2"),
            manifestSha256: hash("3"),
            manifestExpectedCorpusSha256: hash("1"),
            manifestExpectedIndexSha256: hash("2"),
            manifestMatchesArtifacts: true,
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
