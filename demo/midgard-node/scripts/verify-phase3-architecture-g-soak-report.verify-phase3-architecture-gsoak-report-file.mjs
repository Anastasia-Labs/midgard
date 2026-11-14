import fs from "node:fs";
import path from "node:path";

import {
  evaluateClosureIdentityArtifacts,
  scanSubmitRecords,
  sha256File,
  summarizePhase3WorkloadReport,
} from "./phase3-architecture-g-closure-lib.mjs";
import {
  validatePhase3LoadGeneratorIsolationDocument,
  validatePhase3NodePreLifecycleRevalidationDocument,
  validateTrustedPhase3DockerRuntimeArtifacts,
} from "./phase3-architecture-g-load-generator-isolation.mjs";
import {
  PHASE3_SOAK_CORPUS_PREFLIGHT_SCHEMA,
  phase3SoakSourceIdentitySha256,
} from "./phase3-architecture-g-soak-preflight.mjs";
import {
  loadCorpusIndex,
  scanCorpusPrefixEvidence,
  selectCorpusIndexEntries,
} from "./throughput-valid-stress-corpus.mjs";
import { evaluatePhase3ArchitectureGSoakReport } from "./verify-phase3-architecture-g-soak-report.evaluate-phase3-architecture-gsoak-report.mjs";
import { sha256 } from "./verify-phase3-architecture-g-soak-report.validate-sample-shape.mjs";

export const verifyPhase3ArchitectureGSoakReportFile = async (reportPath) => {
  const bytes = fs.readFileSync(reportPath);
  const report = JSON.parse(bytes.toString("utf8"));
  const evaluation = evaluatePhase3ArchitectureGSoakReport(report);
  const artifactReasons = evaluateClosureIdentityArtifacts(report?.identity, {
    skipPhase1Corpus: true,
  });
  const checkArtifact = (
    label,
    artifactPath,
    expectedSha256,
    expectedBytes = null,
  ) => {
    try {
      const stat = fs.lstatSync(artifactPath);
      if (
        !path.isAbsolute(artifactPath) ||
        !stat.isFile() ||
        stat.isSymbolicLink() ||
        (expectedBytes !== null && stat.size !== expectedBytes) ||
        sha256File(artifactPath) !== expectedSha256
      ) {
        artifactReasons.push(
          `${label} artifact bytes do not match the bound SHA-256`,
        );
      }
    } catch {
      artifactReasons.push(
        `${label} artifact is unavailable for offline verification`,
      );
    }
  };
  checkArtifact(
    "runtime",
    report?.identity?.runtime?.path,
    report?.identity?.runtime?.sha256,
  );
  checkArtifact(
    "deployment",
    report?.identity?.deployment?.path,
    report?.identity?.deployment?.sha256,
  );
  checkArtifact(
    "Phase 1",
    report?.identity?.phase1?.path,
    report?.identity?.phase1?.sha256,
  );
  checkArtifact(
    "corpus preflight",
    report?.identity?.corpusPreflight?.path,
    report?.identity?.corpusPreflight?.sha256,
    report?.identity?.corpusPreflight?.bytes,
  );
  checkArtifact(
    "load-generator isolation",
    report?.identity?.loadGeneratorIsolation?.path,
    report?.identity?.loadGeneratorIsolation?.sha256,
    report?.identity?.loadGeneratorIsolation?.bytes,
  );
  checkArtifact(
    "pre-lifecycle node revalidation",
    report?.identity?.nodePreLifecycleRevalidation?.path,
    report?.identity?.nodePreLifecycleRevalidation?.sha256,
    report?.identity?.nodePreLifecycleRevalidation?.bytes,
  );
  let isolationDocument = null;
  try {
    isolationDocument = JSON.parse(
      fs.readFileSync(report?.identity?.loadGeneratorIsolation?.path, "utf8"),
    );
    validatePhase3LoadGeneratorIsolationDocument(isolationDocument, {
      expectedNodeContainerId: report?.identity?.phase1?.nodeContainerId,
      expectedNodeImageId: report?.identity?.phase1?.nodeImageId,
    });
    const isolationSummary = report?.identity?.loadGeneratorIsolation;
    if (
      isolationDocument.schemaVersion !== isolationSummary?.schemaVersion ||
      isolationDocument.placement !== isolationSummary?.placement ||
      isolationDocument.loadGenerator.cpusAllowedList !==
        isolationSummary?.loadGeneratorCpusAllowedList ||
      isolationDocument.loadGenerator.uid.effective !==
        isolationSummary?.loadGeneratorEffectiveUid ||
      isolationDocument.node.cpusAllowedList !==
        isolationSummary?.nodeCpusAllowedList ||
      isolationDocument.nodeContainer.phase1ContainerId !==
        isolationSummary?.nodeContainerId ||
      isolationDocument.nodeContainer.phase1ImageId !==
        isolationSummary?.nodeImageId ||
      isolationDocument.node.pid !== isolationSummary?.nodeHostPid ||
      isolationDocument.node.startTicks !== isolationSummary?.nodeStartTicks ||
      isolationDocument.nodeContainer.readyEndpoint.url !==
        isolationSummary?.readyUrl ||
      isolationDocument.nodeContainer.metricsEndpoint.url !==
        isolationSummary?.metricsUrl ||
      isolationDocument.docker.client.realPath !==
        isolationSummary?.dockerClientRealPath ||
      isolationDocument.docker.client.sha256 !==
        isolationSummary?.dockerClientSha256 ||
      isolationDocument.docker.socket.realPath !==
        isolationSummary?.dockerSocketRealPath ||
      isolationDocument.docker.socket.dev !==
        isolationSummary?.dockerSocketDev ||
      isolationDocument.docker.socket.ino !==
        isolationSummary?.dockerSocketIno ||
      isolationDocument.docker.daemon.id !== isolationSummary?.dockerDaemonId
    ) {
      artifactReasons.push(
        "load-generator isolation artifact diverges from report identity",
      );
    }
    validateTrustedPhase3DockerRuntimeArtifacts(isolationDocument.docker);
  } catch {
    artifactReasons.push(
      "load-generator isolation artifact is unavailable or invalid",
    );
  }
  try {
    const revalidationDocument = JSON.parse(
      fs.readFileSync(
        report?.identity?.nodePreLifecycleRevalidation?.path,
        "utf8",
      ),
    );
    validatePhase3NodePreLifecycleRevalidationDocument(
      revalidationDocument,
      isolationDocument,
    );
    const expected = report?.identity?.nodePreLifecycleRevalidation;
    if (
      revalidationDocument.schemaVersion !== expected?.schemaVersion ||
      revalidationDocument.observedAtMs !== expected?.observedAtMs ||
      revalidationDocument.isolation.path !== expected?.isolationPath ||
      revalidationDocument.isolation.sha256 !== expected?.isolationSha256 ||
      revalidationDocument.nodeContainer.phase1ContainerId !==
        expected?.nodeContainerId ||
      revalidationDocument.nodeContainer.phase1ImageId !==
        expected?.nodeImageId ||
      revalidationDocument.node.pid !== expected?.nodeHostPid ||
      revalidationDocument.node.startTicks !== expected?.nodeStartTicks ||
      revalidationDocument.nodeContainer.restartCount !==
        expected?.nodeRestartCount ||
      revalidationDocument.nodeContainer.healthStatus !==
        expected?.nodeHealthStatus ||
      revalidationDocument.nodeContainer.readyEndpoint.url !==
        expected?.readyUrl ||
      revalidationDocument.nodeContainer.metricsEndpoint.url !==
        expected?.metricsUrl ||
      revalidationDocument.docker.client.sha256 !==
        expected?.dockerClientSha256 ||
      revalidationDocument.docker.socket.dev !== expected?.dockerSocketDev ||
      revalidationDocument.docker.socket.ino !== expected?.dockerSocketIno ||
      revalidationDocument.docker.daemon.id !== expected?.dockerDaemonId
    ) {
      artifactReasons.push(
        "pre-lifecycle node revalidation artifact diverges from report identity",
      );
    }
  } catch {
    artifactReasons.push(
      "pre-lifecycle node revalidation artifact is unavailable or invalid",
    );
  }
  let preflight = null;
  try {
    preflight = JSON.parse(
      fs.readFileSync(report?.identity?.corpusPreflight?.path, "utf8"),
    );
    const expected = report?.identity?.corpusPreflight;
    if (
      preflight?.schemaVersion !== PHASE3_SOAK_CORPUS_PREFLIGHT_SCHEMA ||
      preflight?.sourceIdentity?.sourceTreeSha256 !==
        report?.identity?.source?.sourceTreeSha256 ||
      JSON.stringify(preflight?.sourceIdentity) !==
        JSON.stringify(report?.identity?.source) ||
      preflight?.sourceIdentitySha256 !== expected?.sourceIdentitySha256 ||
      phase3SoakSourceIdentitySha256(preflight?.sourceIdentity) !==
        expected?.sourceIdentitySha256 ||
      preflight?.phase1Binding?.path !== report?.identity?.phase1?.path ||
      preflight?.phase1Binding?.sha256 !== report?.identity?.phase1?.sha256 ||
      JSON.stringify(preflight?.files) !== JSON.stringify(expected?.files) ||
      JSON.stringify(preflight?.selection) !==
        JSON.stringify(expected?.selection) ||
      JSON.stringify(preflight?.validation) !==
        JSON.stringify(expected?.validation)
    ) {
      artifactReasons.push(
        "corpus preflight contents do not match the bound report identity",
      );
    }
    for (const [label, file] of Object.entries(preflight?.files ?? {})) {
      const stat = fs.lstatSync(file.path);
      if (
        !stat.isFile() ||
        stat.isSymbolicLink() ||
        stat.size !== file.bytes ||
        stat.mtimeMs !== file.mtimeMs ||
        stat.dev.toString() !== file.dev ||
        stat.ino.toString() !== file.ino
      ) {
        artifactReasons.push(
          `${label} changed after the bound full corpus preflight`,
        );
      }
    }
  } catch {
    artifactReasons.push(
      "corpus preflight is unavailable or malformed for offline verification",
    );
  }
  checkArtifact(
    "owner binary",
    report?.identity?.ownerBinary?.path,
    report?.identity?.ownerBinary?.sha256,
  );
  checkArtifact(
    "owner SHA-256 manifest",
    report?.identity?.ownerBinary?.sha256ManifestPath,
    report?.identity?.ownerBinary?.sha256ManifestSha256,
  );
  checkArtifact(
    "workload script",
    report?.workload?.scriptPath,
    report?.workload?.scriptSha256,
  );
  checkArtifact(
    "workload report",
    report?.workload?.reportPath,
    report?.workload?.reportSha256,
    report?.workload?.reportBytes,
  );
  try {
    const recomputedSummary = summarizePhase3WorkloadReport(
      JSON.parse(fs.readFileSync(report?.workload?.reportPath, "utf8")),
    );
    if (
      JSON.stringify(recomputedSummary) !==
      JSON.stringify(report?.workload?.reportSummary)
    ) {
      artifactReasons.push(
        "workload summary does not match the bound workload report",
      );
    }
  } catch {
    artifactReasons.push("workload report cannot be independently summarized");
  }
  checkArtifact(
    "submit records",
    report?.workload?.submitRecords?.path,
    report?.workload?.submitRecords?.sha256,
    report?.workload?.submitRecords?.bytes,
  );
  let scannedSubmitRecords = null;
  try {
    scannedSubmitRecords = await scanSubmitRecords(
      report?.workload?.submitRecords?.path,
    );
    if (
      scannedSubmitRecords.sha256 !== report?.workload?.submitRecords?.sha256 ||
      scannedSubmitRecords.bytes !== report?.workload?.submitRecords?.bytes ||
      scannedSubmitRecords.recordCount !==
        report?.workload?.submitRecords?.recordCount ||
      scannedSubmitRecords.successCount !==
        report?.workload?.submitRecords?.successCount ||
      scannedSubmitRecords.errorCount !==
        report?.workload?.submitRecords?.errorCount ||
      scannedSubmitRecords.timeoutCount !==
        report?.workload?.submitRecords?.timeoutCount ||
      scannedSubmitRecords.attemptSequenceSha256 !==
        report?.workload?.submitRecords?.attemptSequenceSha256
    ) {
      artifactReasons.push(
        "submit-record scan does not match the bound evidence identity",
      );
    }
  } catch {
    artifactReasons.push(
      "submit-record evidence is unavailable or malformed for offline verification",
    );
  }
  try {
    const fullIndex = await loadCorpusIndex(
      report?.identity?.phase1?.corpus?.indexPath,
    );
    const selectedEntries = selectCorpusIndexEntries({
      index: fullIndex,
      corpusSliceId: report?.workload?.reportSummary?.corpus?.sliceId,
      corpusShape: report?.workload?.reportSummary?.corpus?.shape,
      maxChains: null,
    });
    const scannedCorpus = await scanCorpusPrefixEvidence({
      corpusPath: report?.identity?.phase1?.corpus?.path,
      fullIndex,
      selectedEntries,
      consumption: report?.workload?.reportSummary?.corpus?.consumption,
      expectedCorpusSha256: preflight?.files?.corpus?.sha256,
    });
    if (
      scannedCorpus.consumedRowCount !== scannedSubmitRecords?.recordCount ||
      scannedCorpus.corpusSha256 !==
        report?.identity?.phase1?.corpus?.corpusSha256
    ) {
      artifactReasons.push(
        "consumed corpus prefix does not match submit-attempt cardinality or bound identity",
      );
    }
  } catch (error) {
    artifactReasons.push(
      `consumed corpus prefix cannot be verified against the bound corpus: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  checkArtifact(
    "soak runner",
    report?.identity?.tooling?.runnerPath,
    report?.identity?.tooling?.runnerSha256,
  );
  checkArtifact(
    "soak verifier",
    report?.identity?.tooling?.verifierPath,
    report?.identity?.tooling?.verifierSha256,
  );
  const reasons = [...new Set([...evaluation.reasons, ...artifactReasons])];
  return {
    ...evaluation,
    passed: reasons.length === 0,
    reasons,
    reportPath: path.resolve(reportPath),
    reportSha256: sha256(bytes),
  };
};
