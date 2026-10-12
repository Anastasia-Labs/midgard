import { MIDGARD_CONSENSUS_PROFILE } from ".././consensus-profile.js";
import { parseDeploymentManifestEventHistoryRecipe } from "./event-history.js";
import {
  deriveScriptHashCached,
  validateFinalizedContracts,
  validateFinalizedReferenceScripts,
} from "./finalized.validate-finalized-contracts.js";
import {
  validateFinalizedDa,
  VERIFIED_FINALIZED_MANIFEST_ID_CACHE_LIMIT,
  verifiedFinalizedManifestIds,
  verifyReferenceScriptPublicationAuthority,
} from "./finalized.validate-finalized-da.js";
import {
  computeDeploymentManifestJsonDigest,
  verifyDeploymentManifestIdentity,
} from "./identity.js";
import {
  requireExactKeys,
  requireHex,
  requireInteger,
  requireIsoTimestamp,
  requireRecord,
  requireString,
} from "./primitives.js";
import { parseDeploymentManifestCardanoProtocolParameters } from "./protocol-parameters.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES } from "./reference-script-tokens.js";
import {
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  DEPLOYMENT_MANIFEST_STEP_NAMES,
  type DeploymentManifest,
} from "./types.js";

export const verifyFinalizedDeploymentManifest = (
  value: unknown,
): DeploymentManifest => {
  const candidate = verifyDeploymentManifestIdentity(value);
  const verifiedManifestId = candidate.manifestId as string;
  if (verifiedFinalizedManifestIds.has(verifiedManifestId)) {
    return candidate as DeploymentManifest;
  }
  const createdAt = requireIsoTimestamp(candidate.createdAt, "createdAt");
  const updatedAt = requireIsoTimestamp(candidate.updatedAt, "updatedAt");
  if (updatedAt < createdAt) {
    throw new Error("Deployment manifest updatedAt must not precede createdAt");
  }
  requireString(
    candidate.referenceScriptDeployAddress,
    "referenceScriptDeployAddress",
  );

  const cardano = requireRecord(
    candidate.cardanoProtocolParameters,
    "Deployment manifest cardanoProtocolParameters",
  );
  requireExactKeys(
    cardano,
    ["snapshot", "digest"],
    [],
    "cardanoProtocolParameters",
  );
  const cardanoDigest = requireHex(
    cardano.digest,
    32,
    "cardanoProtocolParameters.digest",
  );
  parseDeploymentManifestCardanoProtocolParameters(cardano.snapshot);
  const expectedCardanoDigest = computeDeploymentManifestJsonDigest(
    cardano.snapshot,
  );
  if (cardanoDigest !== expectedCardanoDigest) {
    throw new Error(
      `Deployment manifest cardanoProtocolParameters.digest mismatch: expected ${expectedCardanoDigest}`,
    );
  }

  const genesis = requireRecord(
    candidate.genesis,
    "Deployment manifest genesis",
  );
  requireExactKeys(genesis, ["headerHash", "utxoSetDigest"], [], "genesis");
  requireHex(genesis.headerHash, 28, "genesis.headerHash");
  requireHex(genesis.utxoSetDigest, 32, "genesis.utxoSetDigest");

  const oneShot = requireRecord(
    candidate.hubOracleOneShot,
    "Deployment manifest hubOracleOneShot",
  );
  requireExactKeys(
    oneShot,
    ["txHash", "outputIndex", "outRef", "status"],
    [],
    "hubOracleOneShot",
  );
  const oneShotTxHash = requireHex(
    oneShot.txHash,
    32,
    "hubOracleOneShot.txHash",
  );
  const oneShotOutputIndex = requireInteger(
    oneShot.outputIndex,
    "hubOracleOneShot.outputIndex",
  );
  const expectedOneShotOutRef = `${oneShotTxHash}#${oneShotOutputIndex.toString()}`;
  if (oneShot.outRef !== expectedOneShotOutRef) {
    throw new Error(
      `Deployment manifest hubOracleOneShot.outRef must equal ${expectedOneShotOutRef}`,
    );
  }
  if (oneShot.status !== "consumed_by_init") {
    throw new Error(
      "Deployment manifest hubOracleOneShot.status must be consumed_by_init",
    );
  }

  const authPolicy = requireRecord(
    candidate.referenceScriptAuthPolicy,
    "Deployment manifest referenceScriptAuthPolicy",
  );
  requireExactKeys(
    authPolicy,
    ["policyId", "nativeScript", "tokenNames", "postTimelockAudit"],
    [],
    "referenceScriptAuthPolicy",
  );
  const policyId = requireHex(
    authPolicy.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  const nativeScript = requireRecord(
    authPolicy.nativeScript,
    "Deployment manifest referenceScriptAuthPolicy.nativeScript",
  );
  requireExactKeys(
    nativeScript,
    [
      "type",
      "cborHex",
      "expiresAtSlot",
      "expiresAtUnixTime",
      "timelockDurationMs",
    ],
    [],
    "referenceScriptAuthPolicy.nativeScript",
  );
  if (nativeScript.type !== "Native") {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.nativeScript.type must be Native",
    );
  }
  const nativeScriptCbor = requireHex(
    nativeScript.cborHex,
    undefined,
    "referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  const expiresAtSlot = requireInteger(
    nativeScript.expiresAtSlot,
    "referenceScriptAuthPolicy.nativeScript.expiresAtSlot",
  );
  requireInteger(
    nativeScript.expiresAtUnixTime,
    "referenceScriptAuthPolicy.nativeScript.expiresAtUnixTime",
  );
  requireInteger(
    nativeScript.timelockDurationMs,
    "referenceScriptAuthPolicy.nativeScript.timelockDurationMs",
    1,
  );
  const derivedPolicyId = deriveScriptHashCached("Native", nativeScriptCbor);
  if (derivedPolicyId !== policyId) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.policyId mismatch: expected ${derivedPolicyId}`,
    );
  }
  const tokenNames = requireRecord(
    authPolicy.tokenNames,
    "Deployment manifest referenceScriptAuthPolicy.tokenNames",
  );
  const roles = Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES);
  requireExactKeys(
    tokenNames,
    roles,
    [],
    "referenceScriptAuthPolicy.tokenNames",
  );
  for (const role of roles) {
    const expected =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
      ];
    if (tokenNames[role] !== expected) {
      throw new Error(
        `Deployment manifest referenceScriptAuthPolicy.tokenNames.${role} must equal ${expected}`,
      );
    }
  }
  const audit = requireRecord(
    authPolicy.postTimelockAudit,
    "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit",
  );
  requireExactKeys(
    audit,
    ["required", "rule"],
    [],
    "referenceScriptAuthPolicy.postTimelockAudit",
  );
  if (typeof audit.required !== "boolean") {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit.required must be a boolean",
    );
  }
  verifyReferenceScriptPublicationAuthority({
    cborHex: nativeScriptCbor,
    expiresAtSlot,
    postTimelockAuditRequired: audit.required,
  });
  requireString(audit.rule, "referenceScriptAuthPolicy.postTimelockAudit.rule");

  const contracts = requireRecord(
    candidate.contracts,
    "Deployment manifest contracts",
  );
  validateFinalizedContracts(contracts);
  for (const [name, kind] of [
    ["deposit", "Deposit"],
    ["withdrawal", "Withdrawal"],
  ] as const) {
    const mint = requireRecord(
      contracts[`${name}Mint`],
      `contracts.${name}Mint`,
    );
    const spend = requireRecord(
      contracts[`${name}Spend`],
      `contracts.${name}Spend`,
    );
    const recipe = parseDeploymentManifestEventHistoryRecipe(
      mint.eventHistoryRecipe,
    );
    const hub = requireRecord(
      contracts.hubOracleMint,
      "contracts.hubOracleMint",
    );
    if (
      recipe.kind !== kind ||
      recipe.hubPolicyId !== hub.scriptHash ||
      recipe.initializationNonce.txHash !== oneShotTxHash ||
      recipe.initializationNonce.outputIndex !== oneShotOutputIndex ||
      mint.scriptHash !== spend.scriptHash
    )
      throw new Error(
        `Deployment manifest ${name} history recipe or list roles differ from its deployment`,
      );
  }
  const authContract = requireRecord(
    contracts.referenceScriptAuthMint,
    "contracts.referenceScriptAuthMint",
  );
  if (authContract.scriptHash !== policyId) {
    throw new Error(
      "Deployment manifest contracts.referenceScriptAuthMint.scriptHash must match referenceScriptAuthPolicy.policyId",
    );
  }
  validateFinalizedReferenceScripts(
    requireRecord(
      candidate.referenceScripts,
      "Deployment manifest referenceScripts",
    ),
    authPolicy,
    contracts,
  );
  validateFinalizedDa(candidate.da);

  const artifacts = requireRecord(
    candidate.artifacts,
    "Deployment manifest artifacts",
  );
  requireExactKeys(artifacts, ["blueprintHash"], [], "artifacts");
  requireHex(artifacts.blueprintHash, 32, "artifacts.blueprintHash");

  const steps = requireRecord(candidate.steps, "Deployment manifest steps");
  requireExactKeys(steps, DEPLOYMENT_MANIFEST_STEP_NAMES, [], "steps");
  const supportedStepStatuses = new Set([
    "pending",
    "in_progress",
    "submitted",
    "complete",
    "attached",
    "failed",
    "blocked_requires_fresh_redeploy",
  ]);
  for (const stepName of DEPLOYMENT_MANIFEST_STEP_NAMES) {
    const field = `steps.${stepName}`;
    const step = requireRecord(steps[stepName], field);
    requireExactKeys(step, ["status"], ["txHash"], field);
    if (!supportedStepStatuses.has(String(step.status))) {
      throw new Error(`Deployment manifest ${field}.status is unsupported`);
    }
    if (step.txHash !== undefined) {
      requireHex(step.txHash, 32, `${field}.txHash`);
    }
  }
  for (const requiredStep of [
    "prepareHubOracleNonce",
    "deployNodeRuntimeReferenceScripts",
    "initProtocol",
    "availabilityRegistration",
  ]) {
    const step = requireRecord(steps[requiredStep], `steps.${requiredStep}`);
    if (step.status !== "complete") {
      throw new Error(
        `Deployment manifest steps.${requiredStep}.status must be complete`,
      );
    }
  }

  const dispute = requireRecord(
    candidate.validationDispute,
    "Deployment manifest validationDispute",
  );
  requireExactKeys(
    dispute,
    ["version", "responseWindowMs", "maxBisectionRounds", "maturityMs"],
    [],
    "validationDispute",
  );
  const expectedDispute = {
    version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
    responseWindowMs:
      MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
    maxBisectionRounds:
      MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
    maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
  } as const;
  for (const [key, expected] of Object.entries(expectedDispute)) {
    if (dispute[key] !== expected) {
      throw new Error(
        `Deployment manifest validationDispute.${key} must equal ${expected.toString()}`,
      );
    }
  }

  const l1Finality = requireRecord(
    candidate.l1Finality,
    "Deployment manifest l1Finality",
  );
  requireExactKeys(
    l1Finality,
    [
      "confirmationDepth",
      "commitEventDepth",
      "automaticRecoveryMaxDepth",
      "deepRollbackPolicy",
    ],
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
  if (
    verifiedFinalizedManifestIds.size >=
    VERIFIED_FINALIZED_MANIFEST_ID_CACHE_LIMIT
  ) {
    const oldest = verifiedFinalizedManifestIds.values().next().value;
    if (oldest !== undefined) {
      verifiedFinalizedManifestIds.delete(oldest);
    }
  }
  verifiedFinalizedManifestIds.add(verifiedManifestId);
  return candidate as DeploymentManifest;
};
