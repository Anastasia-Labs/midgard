import { sha256 } from "@noble/hashes/sha2.js";
import { bytesToHex } from "@noble/hashes/utils.js";

import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
} from ".././consensus-profile.js";
import {
  SELECTED_DEPLOYMENT_PROFILE,
  verifyDeploymentProfileBinding,
} from ".././deployment-profile.js";
import { prototypeOf } from ".././narrowing.js";
import {
  parseDeploymentManifestAvailabilityChallenge,
  parseDeploymentManifestEconomics,
} from "./availability.js";
import { requireRecord } from "./primitives.js";
import {
  DEPLOYMENT_MANIFEST_ROOT_KEYS,
  type DeploymentManifestJsonValue,
} from "./types.js";

export const normalizeDeploymentManifestJsonValueInternal = (
  value: unknown,
  field: string,
  stringifyBigInt: boolean,
): DeploymentManifestJsonValue => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return value;
  }
  if (typeof value === "bigint") {
    if (stringifyBigInt) {
      return value.toString(10);
    }
    throw new Error(`${field} must contain only JSON-safe values`);
  }
  if (typeof value === "number") {
    if (!Number.isFinite(value)) {
      throw new Error(`${field} must contain only finite numbers`);
    }
    return value;
  }
  if (Array.isArray(value)) {
    return value.map((entry, index) =>
      normalizeDeploymentManifestJsonValueInternal(
        entry,
        `${field}[${index.toString()}]`,
        stringifyBigInt,
      ),
    );
  }
  if (typeof value !== "object" || value === null) {
    throw new Error(`${field} must contain only JSON-safe values`);
  }
  const prototype = prototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error(`${field} must contain only plain records`);
  }
  if (Reflect.ownKeys(value).length !== Object.keys(value).length) {
    throw new Error(`${field} must contain only string keys`);
  }
  return Object.fromEntries(
    Object.entries(value as Record<string, unknown>).map(([key, entry]) => {
      if (entry === undefined) {
        throw new Error(`${field}.${key} must not be undefined`);
      }
      return [
        key,
        normalizeDeploymentManifestJsonValueInternal(
          entry,
          `${field}.${key}`,
          stringifyBigInt,
        ),
      ];
    }),
  );
};

export const normalizeDeploymentManifestJsonValue = (
  value: unknown,
  field = "value",
): DeploymentManifestJsonValue =>
  normalizeDeploymentManifestJsonValueInternal(
    value,
    `Deployment manifest ${field}`,
    true,
  );

export const stableJson = (value: DeploymentManifestJsonValue): string => {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  return `{${Object.entries(value)
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
    .map(([key, entry]) => `${JSON.stringify(key)}:${stableJson(entry)}`)
    .join(",")}}`;
};

export const computeDeploymentManifestJsonDigest = (value: unknown): string => {
  const normalized = normalizeDeploymentManifestJsonValueInternal(
    value,
    "Deployment manifest JSON digest input",
    false,
  );
  return bytesToHex(sha256(new TextEncoder().encode(stableJson(normalized))));
};

const exactRoot = (candidate: Record<string, unknown>): void => {
  const expected = new Set<string>(DEPLOYMENT_MANIFEST_ROOT_KEYS);
  for (const key of Object.keys(candidate)) {
    if (!expected.has(key)) {
      throw new Error(`Deployment manifest value.${key} is unexpected`);
    }
  }
  for (const key of DEPLOYMENT_MANIFEST_ROOT_KEYS) {
    if (!Object.prototype.hasOwnProperty.call(candidate, key)) {
      throw new Error(`Deployment manifest value.${key} is required`);
    }
  }
};

export const computeDeploymentManifestId = (
  identityInput: Record<string, unknown>,
): string => {
  if (Object.prototype.hasOwnProperty.call(identityInput, "manifestId")) {
    throw new Error("Deployment manifest identity input must omit manifestId");
  }
  const normalized = normalizeDeploymentManifestJsonValueInternal(
    identityInput,
    "Deployment manifest identity input",
    false,
  );
  return bytesToHex(sha256(new TextEncoder().encode(stableJson(normalized))));
};

export const verifyDeploymentManifestIdentity = (
  value: unknown,
): Record<string, unknown> => {
  const candidate = requireRecord(value, "Deployment manifest value");
  if (candidate.schemaVersion !== MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION) {
    throw new Error(
      `Deployment manifest schemaVersion must be ${MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION}`,
    );
  }
  exactRoot(candidate);
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
  verifyDeploymentProfileBinding(
    candidate.deploymentProfile,
    candidate.deploymentProfileDigest,
    candidate.network,
  );
  const economics = parseDeploymentManifestEconomics(candidate.economics);
  if (
    stableJson(economics) !== stableJson(SELECTED_DEPLOYMENT_PROFILE.economics)
  ) {
    throw new Error(
      "Deployment manifest economics must match deployment profile",
    );
  }
  parseDeploymentManifestAvailabilityChallenge(candidate.availabilityChallenge);
  if (
    typeof candidate.manifestId !== "string" ||
    !/^[0-9a-f]{64}$/u.test(candidate.manifestId)
  ) {
    throw new Error(
      "Deployment manifest manifestId must be lowercase SHA-256 hex",
    );
  }
  const { manifestId, ...identityInput } = candidate;
  const expectedManifestId = computeDeploymentManifestId(identityInput);
  if (manifestId !== expectedManifestId) {
    throw new Error(
      `Deployment manifest id mismatch: expected ${expectedManifestId}, found ${manifestId}`,
    );
  }
  return candidate;
};
