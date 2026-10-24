import fs from "node:fs";

import { parsePhase1FormalBindingDocument } from "./phase1-formal-identity.mjs";
import {
  capturePhase1CorpusIdentity,
  normalizedImageId,
  sourceIdentity,
} from "./phase3-architecture-g-closure-lib.create-secret-scanning-log.mjs";
import {
  assertRegularFile,
  GIT_SHA,
  isCanonicalAbsolutePath,
  NODE_VERSION,
  readJson,
  SHA256,
  sha256File,
} from "./phase3-architecture-g-closure-lib.evaluate-exact-closure-identity-shape.mjs";
import {
  canonicalJsonSha256,
  decodePhase4EnvironmentArtifactV1,
} from "./phase4-environment-fingerprint-lib.mjs";

export const captureClosureIdentity = async ({
  packageRoot,
  runtimePath,
  deploymentPath,
  phase1Path,
  ownerBinaryPath,
  ownerSha256ManifestPath,
  runnerPath,
  verifierPath,
}) => {
  for (const [label, filePath] of [
    ["runtime fingerprint", runtimePath],
    ["deployment manifest", deploymentPath],
    ["Phase 1 binding", phase1Path],
    ["owner binary", ownerBinaryPath],
    ["owner SHA-256 manifest", ownerSha256ManifestPath],
    ["runner", runnerPath],
    ["verifier", verifierPath],
  ]) {
    assertRegularFile(filePath, label);
  }
  const runtime = decodePhase4EnvironmentArtifactV1(readJson(runtimePath));
  const deployment = readJson(deploymentPath);
  const phase1 = parsePhase1FormalBindingDocument(
    readJson(phase1Path),
    phase1Path,
  );
  const ownerSha256 = sha256File(ownerBinaryPath);
  const expectedOwnerSha256 = fs
    .readFileSync(ownerSha256ManifestPath, "utf8")
    .trim()
    .split(/\s+/u)[0]
    ?.toLowerCase();
  const deploymentSha256 = sha256File(deploymentPath);
  const phase1Corpus = capturePhase1CorpusIdentity(phase1);
  if (
    runtime?.schemaVersion !== "midgard-phase4-environment-artifact-v1" ||
    runtime?.document?.schemaVersion !== "midgard-phase4-environment-v1" ||
    runtime?.documentSha256 !== canonicalJsonSha256(runtime.document) ||
    runtime?.document?.deploymentManifest?.sha256 !== deploymentSha256
  ) {
    throw new Error(
      "runtime fingerprint does not bind the deployment manifest",
    );
  }
  if (
    phase1?.schemaVersion !== "midgard-phase1-live-corpus-binding-v1" ||
    phase1?.deploymentManifestId !== deployment?.manifestId ||
    normalizedImageId(phase1?.nodeImageId) !==
      normalizedImageId(runtime?.document?.node?.imageId)
  ) {
    throw new Error(
      "Phase 1 binding does not match runtime/deployment identity",
    );
  }
  if (
    !SHA256.test(expectedOwnerSha256 ?? "") ||
    ownerSha256 !== expectedOwnerSha256
  ) {
    throw new Error("owner binary does not match its SHA-256 manifest");
  }
  return {
    source: await sourceIdentity(packageRoot),
    runtime: {
      path: runtimePath,
      sha256: sha256File(runtimePath),
      schemaVersion: runtime.schemaVersion,
      deploymentManifestSha256: deploymentSha256,
      nodeImageId: phase1.nodeImageId,
    },
    deployment: {
      path: deploymentPath,
      sha256: deploymentSha256,
      schemaVersion: deployment.schemaVersion,
      manifestId: deployment.manifestId,
    },
    phase1: {
      path: phase1Path,
      sha256: sha256File(phase1Path),
      schemaVersion: phase1.schemaVersion,
      deploymentManifestId: phase1.deploymentManifestId,
      nodeImageId: phase1.nodeImageId,
      nodeContainerId: phase1.nodeContainerId,
      corpus: phase1Corpus,
    },
    ownerBinary: {
      path: ownerBinaryPath,
      sha256: ownerSha256,
      expectedSha256: expectedOwnerSha256,
      sha256ManifestPath: ownerSha256ManifestPath,
      sha256ManifestSha256: sha256File(ownerSha256ManifestPath),
    },
    tooling: {
      runnerPath,
      runnerSha256: sha256File(runnerPath),
      verifierPath,
      verifierSha256: sha256File(verifierPath),
    },
  };
};

export const evaluateClosureIdentity = (identity) => {
  const reasons = [];
  const source = identity?.source;
  if (
    !GIT_SHA.test(source?.gitCommit ?? "") ||
    !SHA256.test(source?.gitStatusSha256 ?? "") ||
    !SHA256.test(source?.trackedDiffSha256 ?? "") ||
    !SHA256.test(source?.sourceTreeSha256 ?? "") ||
    !Number.isSafeInteger(source?.sourceTreeFileCount) ||
    source.sourceTreeFileCount <= 0 ||
    source?.nodeVersion !== NODE_VERSION
  ) {
    reasons.push(
      `source identity is incomplete or Node is not ${NODE_VERSION}`,
    );
  }
  if (
    !isCanonicalAbsolutePath(source?.nodeExecutablePath) ||
    !SHA256.test(source?.nodeExecutableSha256 ?? "")
  ) {
    reasons.push("Node executable identity is incomplete");
  }
  for (const label of ["runtime", "deployment", "phase1"]) {
    const value = identity?.[label];
    if (
      !isCanonicalAbsolutePath(value?.path) ||
      !SHA256.test(value?.sha256 ?? "") ||
      typeof value?.schemaVersion !== "string" ||
      value.schemaVersion.length === 0
    ) {
      reasons.push(`${label} identity is incomplete`);
    }
  }
  if (
    identity?.runtime?.schemaVersion !==
      "midgard-phase4-environment-artifact-v1" ||
    identity?.deployment?.schemaVersion !== "midgard-deployment-manifest-v1" ||
    identity?.phase1?.schemaVersion !==
      "midgard-phase1-live-corpus-binding-v1" ||
    !SHA256.test(identity?.deployment?.manifestId ?? "") ||
    !/^sha256:[0-9a-f]{64}$/u.test(identity?.phase1?.nodeImageId ?? "")
  ) {
    reasons.push("runtime/deployment/Phase 1 schemas or IDs are invalid");
  }
  if (
    identity?.runtime?.deploymentManifestSha256 !==
      identity?.deployment?.sha256 ||
    normalizedImageId(identity?.runtime?.nodeImageId) !==
      normalizedImageId(identity?.phase1?.nodeImageId) ||
    identity?.phase1?.deploymentManifestId !==
      identity?.deployment?.manifestId ||
    !/^sha256:[0-9a-f]{64}$/u.test(identity?.runtime?.nodeImageId ?? "") ||
    !SHA256.test(identity?.phase1?.nodeContainerId ?? "") ||
    typeof identity?.phase1?.corpus?.sliceId !== "string" ||
    identity.phase1.corpus.sliceId.length === 0 ||
    identity.phase1.corpus.sliceId !== identity.phase1.corpus.sliceId.trim()
  ) {
    reasons.push("runtime, deployment, and Phase 1 identities diverge");
  }
  for (const [pathField, shaField] of [
    ["path", "corpusSha256"],
    ["indexPath", "indexSha256"],
    ["manifestPath", "manifestSha256"],
  ]) {
    const corpus = identity?.phase1?.corpus;
    if (
      !isCanonicalAbsolutePath(corpus?.[pathField]) ||
      !SHA256.test(corpus?.[shaField] ?? "")
    ) {
      reasons.push("Phase 1 corpus identity is incomplete");
      break;
    }
  }
  const owner = identity?.ownerBinary;
  if (
    !isCanonicalAbsolutePath(owner?.path) ||
    !SHA256.test(owner?.sha256 ?? "") ||
    owner.sha256 !== owner?.expectedSha256 ||
    !isCanonicalAbsolutePath(owner?.sha256ManifestPath) ||
    !SHA256.test(owner?.sha256ManifestSha256 ?? "")
  ) {
    reasons.push("pinned owner binary identity is invalid");
  }
  const tooling = identity?.tooling;
  if (
    !isCanonicalAbsolutePath(tooling?.runnerPath) ||
    !SHA256.test(tooling?.runnerSha256 ?? "") ||
    !isCanonicalAbsolutePath(tooling?.verifierPath) ||
    !SHA256.test(tooling?.verifierSha256 ?? "")
  ) {
    reasons.push("runner/verifier identity is incomplete");
  }
  return reasons;
};
