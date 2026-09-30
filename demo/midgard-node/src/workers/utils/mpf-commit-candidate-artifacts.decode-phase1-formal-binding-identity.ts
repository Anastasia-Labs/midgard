import {
  boundedNonEmptyString,
  canonicalAbsolutePath,
  canonicalUtcTimestamp,
  exactKeysRecord,
  positiveSafeInteger,
  sha256Digest,
} from "../../artifact-schema.js";
import {
  type ArchitectureGCommitCandidateSeedInput,
  type ArchitectureGPhase1FormalBindingIdentity,
  type ArchitectureGRuntimeIdentity,
} from "./mpf-commit-candidate-artifacts.architecture-gcommit-candidate-input.js";

export const decodePhase1FormalBindingIdentity = (
  value: unknown,
): ArchitectureGPhase1FormalBindingIdentity => {
  const identity = exactKeysRecord(
    value,
    "Architecture G Phase 1 formal-binding identity",
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
  );
  const corpus = exactKeysRecord(
    identity.corpus,
    "Architecture G Phase 1 corpus identity",
    [
      "path",
      "indexPath",
      "manifestPath",
      "sliceId",
      "corpusSha256",
      "indexSha256",
      "manifestSha256",
    ],
  );
  const generationResult = exactKeysRecord(
    identity.generationResult,
    "Architecture G Phase 1 generation-result identity",
    ["path", "sha256", "schemaVersion"],
  );
  const harness = exactKeysRecord(
    identity.harness,
    "Architecture G Phase 1 harness identity",
    ["scenarioId", "engineId"],
  );
  if (
    identity.schemaVersion !==
      "midgard-architecture-g-phase1-formal-binding-identity-v1" ||
    generationResult.schemaVersion !== "midgard-stress-corpus-generation-v1"
  ) {
    throw new Error("Unsupported Architecture G Phase 1 identity");
  }
  canonicalAbsolutePath(identity.path, "formalBinding.path");
  sha256Digest(identity.sha256, "formalBinding.sha256");
  boundedNonEmptyString(
    identity.deploymentManifestId,
    "formalBinding.deploymentManifestId",
  );
  boundedNonEmptyString(identity.nodeImageId, "formalBinding.nodeImageId");
  boundedNonEmptyString(
    identity.nodeContainerId,
    "formalBinding.nodeContainerId",
  );
  sha256Digest(identity.walletSetSha256, "formalBinding.walletSetSha256");
  sha256Digest(identity.fundingSetSha256, "formalBinding.fundingSetSha256");
  canonicalAbsolutePath(corpus.path, "formalBinding.corpus.path");
  canonicalAbsolutePath(corpus.indexPath, "formalBinding.corpus.indexPath");
  canonicalAbsolutePath(
    corpus.manifestPath,
    "formalBinding.corpus.manifestPath",
  );
  boundedNonEmptyString(corpus.sliceId, "formalBinding.corpus.sliceId");
  sha256Digest(corpus.corpusSha256, "formalBinding.corpus.corpusSha256");
  sha256Digest(corpus.indexSha256, "formalBinding.corpus.indexSha256");
  sha256Digest(corpus.manifestSha256, "formalBinding.corpus.manifestSha256");
  canonicalAbsolutePath(
    generationResult.path,
    "formalBinding.generationResult.path",
  );
  sha256Digest(
    generationResult.sha256,
    "formalBinding.generationResult.sha256",
  );
  sha256Digest(harness.scenarioId, "formalBinding.harness.scenarioId");
  sha256Digest(harness.engineId, "formalBinding.harness.engineId");
  return identity as ArchitectureGPhase1FormalBindingIdentity;
};

export const decodeRuntimeIdentity = (
  value: unknown,
): ArchitectureGRuntimeIdentity => {
  const identity = exactKeysRecord(value, "Architecture G runtime identity", [
    "schemaVersion",
    "version",
    "execPath",
    "executableSha256",
  ]);
  if (identity.schemaVersion !== "midgard-architecture-g-runtime-identity-v1") {
    throw new Error("Unsupported Architecture G runtime identity");
  }
  boundedNonEmptyString(identity.version, "runtimeIdentity.version");
  canonicalAbsolutePath(identity.execPath, "runtimeIdentity.execPath");
  sha256Digest(identity.executableSha256, "runtimeIdentity.executableSha256");
  return identity as ArchitectureGRuntimeIdentity;
};

export const decodeArchitectureGCommitCandidateSeedInput = (
  value: unknown,
): ArchitectureGCommitCandidateSeedInput => {
  const input = exactKeysRecord(
    value,
    "Architecture G commit-candidate seed input",
    [
      "schemaVersion",
      "phase1FormalBinding",
      "runtimeIdentity",
      "corpusSlicePath",
      "corpusSliceSha256",
      "fundingMapPath",
      "fundingMapSha256",
      "expectedTransactionCount",
      "firstTimestampIso",
    ],
  );
  if (
    input.schemaVersion !== "midgard-architecture-g-commit-candidate-seed-v1"
  ) {
    throw new Error("Unsupported Architecture G candidate seed input");
  }
  decodePhase1FormalBindingIdentity(input.phase1FormalBinding);
  decodeRuntimeIdentity(input.runtimeIdentity);
  canonicalAbsolutePath(input.corpusSlicePath, "seedInput.corpusSlicePath");
  sha256Digest(input.corpusSliceSha256, "seedInput.corpusSliceSha256");
  canonicalAbsolutePath(input.fundingMapPath, "seedInput.fundingMapPath");
  sha256Digest(input.fundingMapSha256, "seedInput.fundingMapSha256");
  positiveSafeInteger(
    input.expectedTransactionCount,
    "seedInput.expectedTransactionCount",
  );
  canonicalUtcTimestamp(input.firstTimestampIso, "seedInput.firstTimestampIso");
  return input as ArchitectureGCommitCandidateSeedInput;
};
