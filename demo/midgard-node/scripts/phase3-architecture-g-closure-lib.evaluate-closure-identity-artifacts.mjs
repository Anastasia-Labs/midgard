import fs from "node:fs";
import path from "node:path";

import { parsePhase1FormalBindingDocument } from "./phase1-formal-identity.mjs";
import { normalizedImageId } from "./phase3-architecture-g-closure-lib.create-secret-scanning-log.mjs";
import {
  readJson,
  SHA256,
  sha256File,
} from "./phase3-architecture-g-closure-lib.evaluate-exact-closure-identity-shape.mjs";
import {
  canonicalJsonSha256,
  decodePhase4EnvironmentArtifactV1,
} from "./phase4-environment-fingerprint-lib.mjs";

export const evaluateClosureIdentityArtifacts = (
  identity,
  { skipPhase1Corpus = false } = {},
) => {
  const reasons = [];
  const artifacts = [
    [
      "Node executable",
      identity?.source?.nodeExecutablePath,
      identity?.source?.nodeExecutableSha256,
    ],
    ["runtime", identity?.runtime?.path, identity?.runtime?.sha256],
    ["deployment", identity?.deployment?.path, identity?.deployment?.sha256],
    ["Phase 1", identity?.phase1?.path, identity?.phase1?.sha256],
    [
      "Phase 1 corpus",
      identity?.phase1?.corpus?.path,
      identity?.phase1?.corpus?.corpusSha256,
    ],
    [
      "Phase 1 corpus index",
      identity?.phase1?.corpus?.indexPath,
      identity?.phase1?.corpus?.indexSha256,
    ],
    [
      "Phase 1 corpus manifest",
      identity?.phase1?.corpus?.manifestPath,
      identity?.phase1?.corpus?.manifestSha256,
    ],
    ["owner", identity?.ownerBinary?.path, identity?.ownerBinary?.sha256],
    [
      "owner SHA manifest",
      identity?.ownerBinary?.sha256ManifestPath,
      identity?.ownerBinary?.sha256ManifestSha256,
    ],
    ["runner", identity?.tooling?.runnerPath, identity?.tooling?.runnerSha256],
    [
      "verifier",
      identity?.tooling?.verifierPath,
      identity?.tooling?.verifierSha256,
    ],
  ];
  for (const [label, filePath, expectedSha256] of artifacts) {
    if (skipPhase1Corpus && label === "Phase 1 corpus") continue;
    if (
      typeof filePath !== "string" ||
      !path.isAbsolute(filePath) ||
      !SHA256.test(expectedSha256 ?? "") ||
      !fs.existsSync(filePath)
    ) {
      reasons.push(`${label} bound artifact is missing`);
      continue;
    }
    const stat = fs.lstatSync(filePath);
    if (!stat.isFile() || stat.isSymbolicLink()) {
      reasons.push(`${label} bound artifact is not a regular file`);
    } else if (sha256File(filePath) !== expectedSha256) {
      reasons.push(`${label} bound artifact SHA-256 changed`);
    }
  }
  try {
    const runtime = decodePhase4EnvironmentArtifactV1(
      readJson(identity.runtime.path),
    );
    const deployment = readJson(identity.deployment.path);
    const phase1 = parsePhase1FormalBindingDocument(
      readJson(identity.phase1.path),
      identity.phase1.path,
    );
    const deploymentSha256 = sha256File(identity.deployment.path);
    if (
      runtime?.schemaVersion !== "midgard-phase4-environment-artifact-v1" ||
      runtime?.document?.schemaVersion !== "midgard-phase4-environment-v1" ||
      runtime?.documentSha256 !== canonicalJsonSha256(runtime.document) ||
      runtime?.document?.deploymentManifest?.sha256 !== deploymentSha256 ||
      deployment?.manifestId !== identity.deployment.manifestId ||
      phase1?.schemaVersion !== "midgard-phase1-live-corpus-binding-v1" ||
      phase1?.deploymentManifestId !== deployment?.manifestId ||
      normalizedImageId(phase1?.nodeImageId) !==
        normalizedImageId(runtime?.document?.node?.imageId) ||
      phase1?.nodeContainerId !== identity.phase1.nodeContainerId ||
      phase1?.corpus?.path !== identity.phase1.corpus.path ||
      phase1?.corpus?.indexPath !== identity.phase1.corpus.indexPath ||
      phase1?.corpus?.manifestPath !== identity.phase1.corpus.manifestPath ||
      phase1?.corpus?.sliceId !== identity.phase1.corpus.sliceId ||
      phase1?.corpus?.corpusSha256 !== identity.phase1.corpus.corpusSha256 ||
      phase1?.corpus?.indexSha256 !== identity.phase1.corpus.indexSha256 ||
      phase1?.corpus?.manifestSha256 !== identity.phase1.corpus.manifestSha256
    ) {
      reasons.push(
        "bound runtime/deployment/Phase 1 artifact contents diverge",
      );
    }
    const manifestOwnerSha256 = fs
      .readFileSync(identity.ownerBinary.sha256ManifestPath, "utf8")
      .trim()
      .split(/\s+/u)[0]
      ?.toLowerCase();
    if (manifestOwnerSha256 !== identity.ownerBinary.sha256) {
      reasons.push("bound owner SHA manifest content diverges");
    }
  } catch {
    reasons.push("bound identity artifact content is unreadable");
  }
  return reasons;
};

export const sameSourceIdentity = (left, right) =>
  left?.gitCommit === right?.gitCommit &&
  left?.gitStatusSha256 === right?.gitStatusSha256 &&
  left?.trackedDiffSha256 === right?.trackedDiffSha256 &&
  left?.sourceTreeSha256 === right?.sourceTreeSha256 &&
  left?.sourceTreeFileCount === right?.sourceTreeFileCount &&
  left?.nodeVersion === right?.nodeVersion &&
  left?.nodeExecutablePath === right?.nodeExecutablePath &&
  left?.nodeExecutableSha256 === right?.nodeExecutableSha256;
