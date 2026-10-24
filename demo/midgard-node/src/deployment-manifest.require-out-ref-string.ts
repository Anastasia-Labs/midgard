import { computeDeploymentManifestId as computeDeploymentManifestV1Id } from "@al-ft/midgard-core/deployment-manifest-identity";
import { hashHexWithBlake2b } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "./deployment-manifest.deployment-manifest-reference-script-contract-by-role.js";

export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES = Object.freeze(
  Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
);

export const DEPLOYMENT_MANIFEST_STEP_NAMES = Object.freeze([
  "prepareHubOracleNonce",
  "deployNodeRuntimeReferenceScripts",
  "initProtocol",
  "phasRegistration",
  "availabilityRegistration",
  "operatorRegistration",
  "operatorActivation",
] as const);

export const DEPLOYMENT_MANIFEST_STEP_STATUSES = Object.freeze([
  "pending",
  "in_progress",
  "submitted",
  "complete",
  "attached",
  "failed",
  "blocked_requires_fresh_redeploy",
] as const);

const DEPLOYMENT_MANIFEST_SCRIPT_TYPES = Object.freeze([
  "Native",
  "PlutusV1",
  "PlutusV2",
  "PlutusV3",
] as const);

type DeploymentManifestOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export const computeDeploymentManifestDaCommitteeSignersHash = (
  committeeVkeys: readonly string[],
): string => Effect.runSync(hashHexWithBlake2b(committeeVkeys.join(""), 32));

export const computeDeploymentManifestId = computeDeploymentManifestV1Id;

export const requireObject = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value === "object" && value !== null && !Array.isArray(value)) {
    return value as Record<string, unknown>;
  }
  throw new Error(`Deployment manifest ${field} must be an object`);
};

export const requireExactKeys = (
  value: Record<string, unknown>,
  requiredKeys: readonly string[],
  optionalKeys: readonly string[],
  field: string,
): void => {
  const allowed = new Set([...requiredKeys, ...optionalKeys]);
  for (const key of Object.keys(value)) {
    if (!allowed.has(key)) {
      throw new Error(`Deployment manifest ${field}.${key} is unexpected`);
    }
  }
  for (const key of requiredKeys) {
    if (!Object.hasOwn(value, key)) {
      throw new Error(`Deployment manifest ${field}.${key} is required`);
    }
  }
};

export const requireNonEmptyString = (
  value: unknown,
  field: string,
): string => {
  if (typeof value === "string" && value.length > 0) {
    return value;
  }
  throw new Error(`Deployment manifest ${field} must be a non-empty string`);
};

export const requireLowercaseHex = (
  value: unknown,
  bytes: number,
  field: string,
): string => {
  const parsed = requireNonEmptyString(value, field);
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(parsed)) {
    throw new Error(
      `Deployment manifest ${field} must be ${bytes.toString()}-byte lowercase hex`,
    );
  }
  return parsed;
};

export const requireNonNegativeSafeInteger = (
  value: unknown,
  field: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      `Deployment manifest ${field} must be a non-negative safe integer`,
    );
  }
  return value;
};

export const requireIsoTimestamp = (value: unknown, field: string): string => {
  const parsed = requireNonEmptyString(value, field);
  const timestamp = new Date(parsed);
  if (
    !Number.isFinite(timestamp.getTime()) ||
    timestamp.toISOString() !== parsed
  ) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical ISO timestamp`,
    );
  }
  return parsed;
};

export const requireOutRef = (
  value: unknown,
  field: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  const outRef = requireObject(value, field);
  requireExactKeys(outRef, ["txHash", "outputIndex"], [], field);
  return {
    txHash: requireLowercaseHex(outRef.txHash, 32, `${field}.txHash`),
    outputIndex: requireNonNegativeSafeInteger(
      outRef.outputIndex,
      `${field}.outputIndex`,
    ),
  };
};

export const requireLowercaseVariableHex = (
  value: unknown,
  field: string,
): string => {
  const parsed = requireNonEmptyString(value, field);
  if (!/^(?:[0-9a-f]{2})+$/u.test(parsed)) {
    throw new Error(
      `Deployment manifest ${field} must be non-empty even-length lowercase hex`,
    );
  }
  return parsed;
};

export const requirePositiveSafeInteger = (
  value: unknown,
  field: string,
): number => {
  const parsed = requireNonNegativeSafeInteger(value, field);
  if (parsed === 0) {
    throw new Error(
      `Deployment manifest ${field} must be a positive safe integer`,
    );
  }
  return parsed;
};

export const requireOutRefString = (
  value: unknown,
  field: string,
): DeploymentManifestOutRef => {
  const parsed = requireNonEmptyString(value, field);
  const match = /^([0-9a-f]{64})#([0-9]+)$/u.exec(parsed);
  if (match === null) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical lowercase transaction outref`,
    );
  }
  const outputIndex = Number(match[2]);
  if (
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0 ||
    outputIndex.toString() !== match[2]
  ) {
    throw new Error(
      `Deployment manifest ${field} output index must be canonical`,
    );
  }
  return { txHash: match[1], outputIndex };
};

export const requireScriptType = (
  value: unknown,
  field: string,
): (typeof DEPLOYMENT_MANIFEST_SCRIPT_TYPES)[number] => {
  if (
    typeof value === "string" &&
    DEPLOYMENT_MANIFEST_SCRIPT_TYPES.some((entry) => entry === value)
  ) {
    return value as (typeof DEPLOYMENT_MANIFEST_SCRIPT_TYPES)[number];
  }
  throw new Error(
    `Deployment manifest ${field} must be Native, PlutusV1, PlutusV2, or PlutusV3`,
  );
};
