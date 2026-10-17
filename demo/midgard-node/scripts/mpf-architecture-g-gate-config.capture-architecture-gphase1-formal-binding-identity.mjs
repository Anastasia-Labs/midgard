import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  isCanonicalAbsolutePath,
  isCanonicalTimestamp,
  isHash,
  isPositiveSafeInteger,
  requireExactObjectKeys,
} from "./mpf-architecture-g-gate-config.validate-architecture-gfixture-creation-evidence.mjs";
import {
  loadPhase1FormalBindingSync,
  PHASE1_FORMAL_GENERATION_RESULT_SCHEMA,
  sha256FileSync,
} from "./phase1-formal-identity.mjs";

export const completeRootTuple = (result) => ({
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

export const jsonEqual = (left, right) =>
  JSON.stringify(left) === JSON.stringify(right);

export const isBoundedNonEmptyString = (value, maxLength) =>
  typeof value === "string" &&
  value.trim().length > 0 &&
  value.length <= maxLength &&
  !value.includes("\0");

export const isNonEmptyString = (value) => isBoundedNonEmptyString(value, 4096);

const jsonFile = (path, label) => {
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch (cause) {
    throw new Error(`Unable to read ${label} ${path}`, { cause });
  }
};

export const validateArchitectureGPhase1FormalBindingIdentity = (identity) => {
  requireExactObjectKeys(
    identity,
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
    "Architecture G Phase 1 formal binding identity",
  );
  requireExactObjectKeys(
    identity.corpus,
    [
      "path",
      "indexPath",
      "manifestPath",
      "sliceId",
      "corpusSha256",
      "indexSha256",
      "manifestSha256",
    ],
    "Architecture G Phase 1 corpus identity",
  );
  requireExactObjectKeys(
    identity.generationResult,
    ["path", "sha256", "schemaVersion"],
    "Architecture G Phase 1 generation-result identity",
  );
  requireExactObjectKeys(
    identity.harness,
    ["scenarioId", "engineId"],
    "Architecture G Phase 1 harness identity",
  );
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
  requireExactObjectKeys(
    identity,
    ["schemaVersion", "version", "execPath", "executableSha256"],
    "Architecture G runtime identity",
  );
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

export const validateArchitectureGCommitCandidateSeedInputV1 = (input) => {
  requireExactObjectKeys(
    input,
    [
      "schemaVersion",
      "phase1FormalBinding",
      "runtimeIdentity",
      "corpusSlicePath",
      "corpusSliceSha256",
      "fundingMapPath",
      "fundingMapSha256",
      "expectedTransactionCount",
      "fixtureInitialUtxoCount",
      "firstTimestampIso",
    ],
    "Architecture G commit-candidate seed input",
  );
  validateArchitectureGPhase1FormalBindingIdentity(input.phase1FormalBinding);
  validateArchitectureGRuntimeIdentity({
    identity: input.runtimeIdentity,
    expectedVersion: input.runtimeIdentity?.version,
    expectedExecutableSha256: input.runtimeIdentity?.executableSha256,
  });
  if (
    input.schemaVersion !== "midgard-architecture-g-commit-candidate-seed-v1" ||
    !isCanonicalAbsolutePath(input.corpusSlicePath) ||
    !isHash(input.corpusSliceSha256) ||
    !isCanonicalAbsolutePath(input.fundingMapPath) ||
    !isHash(input.fundingMapSha256) ||
    !isPositiveSafeInteger(input.expectedTransactionCount) ||
    !isPositiveSafeInteger(input.fixtureInitialUtxoCount) ||
    !isCanonicalTimestamp(input.firstTimestampIso)
  ) {
    throw new Error("Architecture G commit-candidate seed input is invalid");
  }
  return input;
};
