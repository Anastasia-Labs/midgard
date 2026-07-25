import assert from "node:assert/strict";
import { describe, it } from "node:test";

import {
  PHASE3_FINAL_TREE_AUTHORIZATION,
  PHASE3_FINAL_TREE_SCENARIO,
  PHASE3_FINAL_TREE_SCHEMA,
  PHASE3_FINAL_TREE_SUITES,
  evaluatePhase3FinalTreeReport,
} from "./verify-phase3-architecture-g-final-tree-report.mjs";
import {
  PHASE3_RELEASE_IMAGE_AUTHORIZATION,
  PHASE3_RELEASE_IMAGE_SCENARIO,
  PHASE3_RELEASE_IMAGE_SCHEMA,
  evaluatePhase3ReleaseImageReport,
} from "./verify-phase3-architecture-g-release-image-report.mjs";
import {
  PHASE3_LIVE_E2E_AUTHORIZATION,
  PHASE3_LIVE_E2E_SCENARIO,
  PHASE3_LIVE_E2E_SCHEMA,
  PHASE3_LIVE_STEP_IDS,
  PHASE3_LIVE_STEP_SCHEMA,
  evaluatePhase3LiveE2EReport,
} from "./verify-phase3-architecture-g-live-e2e-report.mjs";

const hash = (character) => character.repeat(64);
const tx = hash;
const artifact = (name) => ({
  path: `/evidence/${name}`,
  sha256: hash("a"),
  bytes: 42,
});
const logArtifact = (name) => ({
  ...artifact(name),
  secretScan: {
    schemaVersion: "midgard-secret-scanned-log-v1",
    passed: true,
    sensitiveLineCount: 0,
    oversizedLineCount: 0,
    retainedLineCount: 1,
  },
});
const source = () => ({
  gitCommit: "1".repeat(40),
  gitStatusSha256: hash("2"),
  trackedDiffSha256: hash("3"),
  sourceTreeSha256: hash("4"),
  sourceTreeFileCount: 100,
  nodeVersion: "v22.22.2",
  nodeExecutablePath: "/runtime/node-v22.22.2",
  nodeExecutableSha256: hash("0"),
});
const identity = () => ({
  source: source(),
  runtime: {
    path: "/evidence/runtime.json",
    sha256: hash("5"),
    schemaVersion: "midgard-phase4-environment-artifact-v1",
    deploymentManifestSha256: hash("6"),
    nodeImageId: `sha256:${hash("7")}`,
  },
  deployment: {
    path: "/evidence/deployment.json",
    sha256: hash("6"),
    schemaVersion: "midgard-deployment-manifest-v1",
    manifestId: hash("8"),
  },
  phase1: {
    path: "/evidence/phase1.json",
    sha256: hash("9"),
    schemaVersion: "midgard-phase1-live-corpus-binding-v1",
    deploymentManifestId: hash("8"),
    nodeImageId: `sha256:${hash("7")}`,
    nodeContainerId: "container-id",
    corpus: {
      path: "/evidence/corpus.ndjson",
      indexPath: "/evidence/corpus.index.ndjson",
      manifestPath: "/evidence/corpus.manifest.json",
      sliceId: "default",
      corpusSha256: hash("1"),
      indexSha256: hash("2"),
      manifestSha256: hash("3"),
    },
  },
  ownerBinary: {
    path: "/evidence/architecture-g-owner",
    sha256: hash("b"),
    expectedSha256: hash("b"),
    sha256ManifestPath: "/evidence/architecture-g-owner.sha256",
    sha256ManifestSha256: hash("c"),
  },
  tooling: {
    runnerPath: "/workspace/runner.mjs",
    runnerSha256: hash("d"),
    verifierPath: "/workspace/verifier.mjs",
    verifierSha256: hash("e"),
  },
});

const finalTreeReport = () => {
  const boundIdentity = identity();
  return {
    schemaVersion: PHASE3_FINAL_TREE_SCHEMA,
    scenario: PHASE3_FINAL_TREE_SCENARIO,
    authorization: PHASE3_FINAL_TREE_AUTHORIZATION,
    database: {
      host: "127.0.0.1",
      name: "midgard_phase3_arch_g_final_tree_fixture",
    },
    startedAtMs: 100,
    completedAtMs: 200,
    identity: boundIdentity,
    sourceAtCompletion: { ...boundIdentity.source },
    suites: PHASE3_FINAL_TREE_SUITES.map((suite, index) => ({
      ...suite,
      argv: [...suite.argv],
      coverage: [...suite.coverage],
      startedAtMs: 110 + index,
      completedAtMs: 120 + index,
      exitCode: 0,
      signal: null,
      timedOut: false,
      completed: true,
      stdout: artifact(`${suite.id}.stdout`),
      stderr: artifact(`${suite.id}.stderr`),
    })),
    verdict: "passed",
  };
};

const releaseReport = () => {
  const boundIdentity = identity();
  return {
    schemaVersion: PHASE3_RELEASE_IMAGE_SCHEMA,
    scenario: PHASE3_RELEASE_IMAGE_SCENARIO,
    authorization: PHASE3_RELEASE_IMAGE_AUTHORIZATION,
    observedAtMs: 100,
    identity: boundIdentity,
    sourceAtCompletion: { ...boundIdentity.source },
    image: {
      reference: "midgard-node:phase3",
      inspectedReferences: ["midgard-node:phase3"],
      imageId: `sha256:${hash("7")}`,
      containerImageId: `sha256:${hash("7")}`,
      containerConfiguredReference: "midgard-node:phase3",
      healthcheckCommand: ["CMD", "node", "fetch('/readyz')"],
    },
    filesystem: {
      nativeEntries: ["architecture-g-owner", "architecture-g-owner.sha256"],
      ownerExecutable: true,
      ownerElf64LittleEndian: true,
      ownerSha256: hash("b"),
      manifestOwnerSha256: hash("b"),
      manifestSha256: hash("f"),
      hasStaticSymbolTable: false,
      debugSections: [],
      compilerPaths: {
        cargo: null,
        rustc: null,
        gcc: null,
        cc: null,
        clang: null,
        make: null,
      },
    },
    runtime: {
      nodeVersion: "v22.22.2",
      engine: "architecture_g",
      ownerBinaryPath: "/app/native/architecture-g-owner",
      configuredOwnerSha256: hash("b"),
      dockerMemoryLimitBytes: 8 * 1024 ** 3,
      cgroupMemoryLimitBytes: 8 * 1024 ** 3,
      v8HeapLimitBytes: 4 * 1024 ** 3,
      containerRunning: true,
      containerHealth: "healthy",
      readiness: { httpStatus: 200, ready: true, reasons: [] },
    },
    verdict: "passed",
  };
};

const stepEvidence = (id) => {
  switch (id) {
    case "fresh-deployment-preflight":
      return {
        runMode: "fresh",
        engine: "architecture_g",
        localUplc: true,
        provider: "Kupmios",
        cleanDeployment: true,
        readiness: { httpStatus: 200, ready: true, reasons: [] },
      };
    case "deposit-projection":
      return {
        txHash: tx("1"),
        eventId: "deposit-1",
        confirmed: true,
        projected: true,
        balanceBeforeLovelace: 0,
        balanceAfterLovelace: 12_000_000,
      };
    case "l2-submit":
      return {
        transactions: [
          { txHash: tx("2"), status: "accepted" },
          { txHash: tx("3"), status: "committed" },
        ],
        submissionErrors: 0,
      };
    case "da-attestation":
      return {
        headers: [
          {
            headerHash: tx("4"),
            payloadMetadataSha256: hash("5"),
            payloadCborSha256: hash("6"),
            watcherStatus: "attested",
            attestationTxHashes: [tx("7"), tx("8"), tx("9")],
          },
        ],
      };
    case "merge-finalization":
      return {
        automaticMerge: true,
        committedTxHashes: [tx("2"), tx("3")],
        finalizedHeaderHashes: [tx("4")],
        stateQueueDepth: 0,
        unfinishedMutationJobs: 0,
      };
    case "db-balance":
      return {
        counts: {
          consumedDeposits: 1,
          acceptedAdmissions: 2,
          immutableRows: 2,
          confirmedLedgerRows: 3,
          mempoolRows: 0,
          processedMempoolRows: 0,
          blockRows: 0,
          unfinishedMutationJobs: 0,
        },
        balanceAssertions: ["a", "b", "c"].map((addressHash) => ({
          addressHash,
          expectedLovelace: 1_000_000,
          actualLovelace: 1_000_000,
        })),
      };
    case "owner-child-restart":
      return {
        signal: "SIGKILL",
        ownerPidBefore: 12,
        ownerPidAfter: 13,
        nodePid: 1,
        childRestartsBefore: 0,
        childRestartsAfter: 1,
        nodeProcessRestarted: false,
        readinessRestored: true,
      };
    case "post-submit-recovery":
      return {
        headerHash: tx("a"),
        submissionTxHash: tx("d"),
        baseRoot: tx("e"),
        candidateRoot: tx("f"),
        eventLogDigest: tx("1"),
        ownerBinarySha256: hash("b"),
        replayEventCount: 2,
        killedAfterSubmission: true,
        killedBeforePromotion: true,
        ownerEpochBefore: "01".repeat(16),
        ownerEpochAfter: "02".repeat(16),
        authoritativeMarkerAfter: tx("f"),
        replayedCandidateRoot: tx("f"),
        journalStatus: "finalized",
        l2Status: "committed",
        auditDivergence: 0,
        recoveryLogMarker:
          "Architecture G recovered post-submit promotion after native child restart",
      };
    case "final-readiness":
      return {
        node: { httpStatus: 200, ready: true, reasons: [] },
        da: { httpStatus: 200, ready: true, reasons: [] },
        allL2Committed: true,
        stateQueueDepth: 0,
        unfinishedMutationJobs: 0,
        unexpectedErrorCount: 0,
      };
    default:
      throw new Error(`unknown step ${id}`);
  }
};

const liveReport = () => {
  const boundIdentity = identity();
  const binding = {
    runtimeSha256: boundIdentity.runtime.sha256,
    deploymentSha256: boundIdentity.deployment.sha256,
    phase1Sha256: boundIdentity.phase1.sha256,
    ownerSha256: boundIdentity.ownerBinary.sha256,
  };
  return {
    schemaVersion: PHASE3_LIVE_E2E_SCHEMA,
    scenario: PHASE3_LIVE_E2E_SCENARIO,
    authorization: PHASE3_LIVE_E2E_AUTHORIZATION,
    startedAtMs: 100,
    completedAtMs: 200,
    identity: boundIdentity,
    sourceAtCompletion: { ...boundIdentity.source },
    commandManifest: artifact("commands.json"),
    steps: PHASE3_LIVE_STEP_IDS.map((id, index) => ({
      id,
      driver: {
        path: `/drivers/${id}`,
        sha256: hash("f"),
        args: [],
        cwd: "/workspace",
        timeoutMs: 60_000,
      },
      exitCode: 0,
      signal: null,
      timedOut: false,
      driverStable: true,
      completed: true,
      stdout: logArtifact(`${id}.stdout`),
      stderr: logArtifact(`${id}.stderr`),
      resultArtifact: artifact(`${id}.result`),
      result: {
        schemaVersion: PHASE3_LIVE_STEP_SCHEMA,
        stepId: id,
        verdict: "passed",
        completed: true,
        binding,
        startedAtMs: 110 + index,
        completedAtMs: 120 + index,
        evidence: stepEvidence(id),
      },
    })),
    verdict: "passed",
  };
};

describe("Phase 3 final-tree report verifier", () => {
  it("accepts the exact combined crash/recovery and shared Phase 4 suites", () => {
    const result = evaluatePhase3FinalTreeReport(finalTreeReport(), {
      checkArtifacts: false,
    });
    assert.equal(result.passed, true, result.reasons.join("\n"));
  });

  it("rejects missing suites, command drift, failed promotion, and source drift", () => {
    const cases = [
      (value) => value.suites.pop(),
      (value) => value.suites[2].argv.push("--changed"),
      (value) => (value.suites[2].exitCode = 1),
      (value) => (value.sourceAtCompletion.sourceTreeSha256 = hash("0")),
    ];
    for (const mutate of cases) {
      const value = finalTreeReport();
      mutate(value);
      assert.equal(
        evaluatePhase3FinalTreeReport(value, { checkArtifacts: false }).passed,
        false,
      );
    }
  });
});

describe("Phase 3 release-image report verifier", () => {
  it("accepts only a stripped, pinned, healthy image with cgroup headroom", () => {
    const result = evaluatePhase3ReleaseImageReport(releaseReport(), {
      checkArtifacts: false,
    });
    assert.equal(result.passed, true, result.reasons.join("\n"));
  });

  it("rejects compiler residue, symbols, SHA drift, low headroom, and readiness failure", () => {
    const cases = [
      (value) => (value.filesystem.compilerPaths.cargo = "/usr/bin/cargo"),
      (value) => (value.filesystem.hasStaticSymbolTable = true),
      (value) => (value.filesystem.ownerSha256 = hash("0")),
      (value) => (value.runtime.cgroupMemoryLimitBytes = 6 * 1024 ** 3),
      (value) => (value.runtime.readiness.ready = false),
      (value) => (value.runtime.nodeVersion = "v22.15.0"),
      (value) => (value.image.reference = "midgard-node:other"),
      (value) =>
        (value.image.containerConfiguredReference = "midgard-node:other"),
    ];
    for (const mutate of cases) {
      const value = releaseReport();
      mutate(value);
      assert.equal(
        evaluatePhase3ReleaseImageReport(value, { checkArtifacts: false })
          .passed,
        false,
      );
    }
  });
});

describe("Phase 3 clean live E2E report verifier", () => {
  it("accepts the exact deposit through restart/replay/finality sequence", () => {
    const result = evaluatePhase3LiveE2EReport(liveReport(), {
      checkArtifacts: false,
    });
    assert.equal(result.passed, true, result.reasons.join("\n"));
  });

  it("rejects skips, failed DA, manual merge, weak DB evidence, and replay drift", () => {
    const cases = [
      (value) => value.steps.splice(3, 1),
      (value) =>
        (value.steps[3].result.evidence.headers[0].watcherStatus = "pending"),
      (value) => (value.steps[4].result.evidence.automaticMerge = false),
      (value) => (value.steps[5].result.evidence.counts.mempoolRows = 1),
      (value) =>
        (value.steps[7].result.evidence.replayedCandidateRoot = tx("0")),
      (value) =>
        (value.steps[7].result.evidence.ownerEpochBefore = "01".repeat(15)),
      (value) =>
        (value.steps[7].result.evidence.ownerEpochAfter =
          value.steps[7].result.evidence.ownerEpochBefore),
      (value) => (value.steps[1].result.evidence.seedPhrase = "forbidden"),
      (value) => (value.steps[1].stdout.secretScan.passed = false),
      (value) => (value.steps[1].stderr.secretScan.sensitiveLineCount = 1),
      (value) => (value.identity.phase1.corpus.corpusSha256 = "not-a-hash"),
    ];
    for (const mutate of cases) {
      const value = liveReport();
      mutate(value);
      assert.equal(
        evaluatePhase3LiveE2EReport(value, { checkArtifacts: false }).passed,
        false,
      );
    }
  });
});
