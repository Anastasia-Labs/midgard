import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
} from "@al-ft/midgard-core/consensus-profile";
import {
  computeDeploymentManifestId as computeDeploymentManifestV1Id,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  type DeploymentManifest,
  normalizeDeploymentManifestJsonValue,
  parseDeploymentManifestEconomics,
  verifyDeploymentManifestIdentity,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  requireExactKeys,
  requireIsoTimestamp,
  requireLowercaseHex,
  requireNonEmptyString,
  requireNonNegativeSafeInteger,
  requireObject,
} from "./deployment-manifest.require-out-ref-string.js";
import {
  validateContracts,
  validateReferenceScripts,
  validateSteps,
} from "./deployment-manifest.validate-contracts.js";
import {
  validateDaIdentity,
  validateValidationDispute,
} from "./deployment-manifest.validate-da-identity.js";
import { validateReferenceScriptAuthPolicy } from "./deployment-manifest.validate-fraud-proof-catalogue.js";

const validateDeploymentManifestCommon = (
  candidate: Record<string, unknown>,
): void => {
  const createdAt = requireIsoTimestamp(candidate.createdAt, "createdAt");
  const updatedAt = requireIsoTimestamp(candidate.updatedAt, "updatedAt");
  if (updatedAt < createdAt) {
    throw new Error("Deployment manifest updatedAt must not precede createdAt");
  }
  requireNonEmptyString(
    candidate.referenceScriptDeployAddress,
    "referenceScriptDeployAddress",
  );
  const hubOracleOneShot = requireObject(
    candidate.hubOracleOneShot,
    "hubOracleOneShot",
  );
  requireExactKeys(
    hubOracleOneShot,
    ["txHash", "outputIndex", "outRef", "status"],
    [],
    "hubOracleOneShot",
  );
  const txHash = requireLowercaseHex(
    hubOracleOneShot.txHash,
    32,
    "hubOracleOneShot.txHash",
  );
  const outputIndex = requireNonNegativeSafeInteger(
    hubOracleOneShot.outputIndex,
    "hubOracleOneShot.outputIndex",
  );
  const expectedOutRef = `${txHash}#${outputIndex.toString()}`;
  if (hubOracleOneShot.outRef !== expectedOutRef) {
    throw new Error(
      `Deployment manifest hubOracleOneShot.outRef mismatch: expected ${expectedOutRef}`,
    );
  }
  if (hubOracleOneShot.status !== "consumed_by_init") {
    throw new Error(
      "Deployment manifest hubOracleOneShot.status must be consumed_by_init",
    );
  }
  const referenceScriptAuthPolicy = requireObject(
    candidate.referenceScriptAuthPolicy,
    "referenceScriptAuthPolicy",
  );
  validateReferenceScriptAuthPolicy(referenceScriptAuthPolicy);
  const cardanoProtocolParameters = requireObject(
    candidate.cardanoProtocolParameters,
    "cardanoProtocolParameters",
  );
  requireExactKeys(
    cardanoProtocolParameters,
    ["snapshot", "digest"],
    [],
    "cardanoProtocolParameters",
  );
  const cardanoSnapshot = normalizeDeploymentManifestJsonValue(
    cardanoProtocolParameters.snapshot,
    "cardanoProtocolParameters.snapshot",
  );
  const cardanoDigest = requireLowercaseHex(
    cardanoProtocolParameters.digest,
    32,
    "cardanoProtocolParameters.digest",
  );
  const expectedCardanoDigest =
    computeDeploymentManifestJsonDigest(cardanoSnapshot);
  if (cardanoDigest !== expectedCardanoDigest) {
    throw new Error(
      `Deployment manifest cardanoProtocolParameters.digest mismatch: expected ${expectedCardanoDigest}`,
    );
  }
  const genesis = requireObject(candidate.genesis, "genesis");
  requireExactKeys(genesis, ["headerHash", "utxoSetDigest"], [], "genesis");
  requireLowercaseHex(genesis.headerHash, 28, "genesis.headerHash");
  requireLowercaseHex(genesis.utxoSetDigest, 32, "genesis.utxoSetDigest");

  const contracts = requireObject(candidate.contracts, "contracts");
  validateContracts(contracts);
  const referenceScriptAuthContract = requireObject(
    contracts.referenceScriptAuthMint,
    "contracts.referenceScriptAuthMint",
  );
  if (
    referenceScriptAuthContract.scriptHash !==
    referenceScriptAuthPolicy.policyId
  ) {
    throw new Error(
      "Deployment manifest contracts.referenceScriptAuthMint.scriptHash must match referenceScriptAuthPolicy.policyId",
    );
  }
  validateReferenceScripts(
    requireObject(candidate.referenceScripts, "referenceScripts"),
    referenceScriptAuthPolicy,
    contracts,
  );
  validateDaIdentity(requireObject(candidate.da, "da"));
  const artifacts = requireObject(candidate.artifacts, "artifacts");
  requireExactKeys(artifacts, ["blueprintHash"], [], "artifacts");
  requireLowercaseHex(artifacts.blueprintHash, 32, "artifacts.blueprintHash");
  validateSteps(requireObject(candidate.steps, "steps"));
  validateValidationDispute(
    requireObject(candidate.validationDispute, "validationDispute"),
  );
  const l1Finality = requireObject(candidate.l1Finality, "l1Finality");
  requireExactKeys(
    l1Finality,
    ["confirmationDepth", "automaticRecoveryMaxDepth", "deepRollbackPolicy"],
    [],
    "l1Finality",
  );
  for (const [key, expected] of Object.entries(
    DEPLOYMENT_MANIFEST_L1_FINALITY,
  )) {
    if (l1Finality[key] !== expected) {
      throw new Error(
        `Deployment manifest l1Finality.${key} must equal ${String(expected)}`,
      );
    }
  }
  parseDeploymentManifestEconomics(candidate.economics);
  const manifestId = requireNonEmptyString(candidate.manifestId, "manifestId");
  if (!/^[0-9a-f]{64}$/.test(manifestId)) {
    throw new Error(
      "Deployment manifest manifestId must be lowercase SHA-256 hex",
    );
  }
  const { manifestId: _manifestId, ...identityInput } = candidate;
  const expectedManifestId = computeDeploymentManifestV1Id(identityInput);
  if (manifestId !== expectedManifestId) {
    throw new Error(
      `Deployment manifest id mismatch: expected ${expectedManifestId}, found ${manifestId}`,
    );
  }
};

// manifestIds whose node-side common validation already succeeded in this
// process, with the core's finalized verification after it. Skipping it for
// one is sound for the reason the core gives for its own finalized cache:
// verifyDeploymentManifestIdentity runs uncached on every parse, re-hashing
// the manifest's full normalized content and requiring manifestId to equal
// that hash, so a changed manifest either fails identity verification or
// arrives under a new manifestId and misses this cache. The common checks
// are a pure function of that content and module constants. The checks
// above them (exact root keys, consensus profile) stay uncached; they are
// cheap. The repeated cost this avoids is the per-contract script hashing,
// which a running node paid on every correction-observer load and save.
const NODE_VERIFIED_MANIFEST_ID_CACHE_LIMIT = 64;
const nodeVerifiedManifestIds = new Set<string>();

const rememberNodeVerifiedManifestId = (manifestId: string): void => {
  if (nodeVerifiedManifestIds.size >= NODE_VERIFIED_MANIFEST_ID_CACHE_LIMIT) {
    const oldest = nodeVerifiedManifestIds.values().next().value;
    if (oldest !== undefined) nodeVerifiedManifestIds.delete(oldest);
  }
  nodeVerifiedManifestIds.add(manifestId);
};

export const parseDeploymentManifestValue = (
  value: unknown,
): DeploymentManifest => {
  const candidate = verifyDeploymentManifestIdentity(value);
  requireExactKeys(
    candidate,
    [
      "schemaVersion",
      "manifestId",
      "consensusProfile",
      "consensusProfileDigest",
      "network",
      "cardanoProtocolParameters",
      "genesis",
      "createdAt",
      "updatedAt",
      "referenceScriptDeployAddress",
      "hubOracleOneShot",
      "referenceScriptAuthPolicy",
      "contracts",
      "referenceScripts",
      "da",
      "artifacts",
      "steps",
      "validationDispute",
      "l1Finality",
      "economics",
      "deploymentProfile",
      "deploymentProfileDigest",
      "availabilityChallenge",
    ],
    [],
    "value",
  );
  if (!isMidgardConsensusProfile(candidate.consensusProfile)) {
    throw new Error(
      "Deployment manifest consensusProfile must exactly match canonical V1",
    );
  }
  if (candidate.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_DIGEST) {
    throw new Error(
      "Deployment manifest consensusProfileDigest must exactly match canonical V1",
    );
  }
  const manifestId = candidate.manifestId as string;
  if (nodeVerifiedManifestIds.has(manifestId))
    return verifyFinalizedDeploymentManifest(candidate);
  validateDeploymentManifestCommon(candidate);
  const manifest = verifyFinalizedDeploymentManifest(candidate);
  rememberNodeVerifiedManifestId(manifestId);
  return manifest;
};
