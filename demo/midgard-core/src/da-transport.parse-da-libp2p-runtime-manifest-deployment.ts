import { MidgardTxCodecErrorCodes } from "./codec/errors.js";
import { ensureHash32 } from "./codec/hash.js";
import {
  DA_DEPLOYMENT_FINGERPRINT_LENGTH,
  DA_HEADER_HASH_LENGTH,
  DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE,
  type DaLibp2pRuntimeManifest,
} from "./da-transport.da-libp2p-runtime-manifest.js";
import {
  bytesValue,
  ensureByteLength,
  fail,
} from "./da-transport.enum-label.js";

export const optionalBytesValue = (
  value: unknown,
  fieldName: string,
): Buffer | null => (value == null ? null : bytesValue(value, fieldName));

export const ensureNonEmptyBytes = (
  value: Uint8Array,
  fieldName: string,
): Buffer => {
  const bytes = Buffer.from(value);
  if (bytes.length === 0) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be non-empty`,
    );
  }
  return bytes;
};

export const ensureDaDeploymentFingerprint = (
  value: Uint8Array,
  fieldName = "deployment_fingerprint",
): Buffer =>
  ensureByteLength(value, DA_DEPLOYMENT_FINGERPRINT_LENGTH, fieldName);

export const ensureDaHeaderHash = (
  value: Uint8Array,
  fieldName = "header_hash",
): Buffer => ensureByteLength(value, DA_HEADER_HASH_LENGTH, fieldName);

export const ensureDaHash32 = (value: Uint8Array, fieldName = "hash"): Buffer =>
  ensureHash32(value, fieldName);

export const ensureDaPayloadHash = (
  value: Uint8Array,
  fieldName = "payload_hash",
): Buffer => ensureDaHash32(value, fieldName);

export const normalizeDaDeploymentFingerprintHex = (
  value: string | Uint8Array,
): string => {
  if (typeof value !== "string") {
    return ensureDaDeploymentFingerprint(value).toString("hex");
  }
  const normalized = value.toLowerCase();
  if (!/^[0-9a-f]{64}$/.test(normalized)) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "deployment_fingerprint must be 32-byte hex",
      value,
    );
  }
  return normalized;
};

export const daDeploymentFingerprintFromHex = (value: string): Buffer =>
  Buffer.from(normalizeDaDeploymentFingerprintHex(value), "hex");

export const recordValue = (
  value: unknown,
  fieldName: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a JSON object`,
    );
  }
  return value as Record<string, unknown>;
};

export const nonEmptyStringValue = (
  value: unknown,
  fieldName: string,
): string => {
  if (typeof value !== "string" || value.trim().length === 0) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a non-empty string`,
    );
  }
  return value as string;
};

export const stringArrayValue = (
  value: unknown,
  fieldName: string,
  allowEmpty = false,
): readonly string[] => {
  if (!Array.isArray(value)) {
    return fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a string array`,
    );
  }
  if (!allowEmpty && value.length === 0) {
    return fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a non-empty string array`,
    );
  }
  return value.map((entry, index) =>
    nonEmptyStringValue(entry, `${fieldName}[${index.toString()}]`),
  );
};

export const safeIntegerValue = (
  value: unknown,
  fieldName: string,
  minimum: number,
  maximum = Number.MAX_SAFE_INTEGER,
): number => {
  if (
    typeof value !== "number" ||
    !Number.isSafeInteger(value) ||
    value < minimum ||
    value > maximum
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a safe integer from ${minimum.toString()} through ${maximum.toString()}`,
    );
  }
  return value as number;
};

export const exactRecordKeys = (
  value: Record<string, unknown>,
  keys: readonly string[],
  fieldName: string,
): void => {
  const expected = new Set(keys);
  for (const key of Object.keys(value)) {
    if (!expected.has(key)) {
      fail(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        `${fieldName}.${key} is unexpected`,
      );
    }
  }
  for (const key of keys) {
    if (!Object.prototype.hasOwnProperty.call(value, key)) {
      fail(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        `${fieldName}.${key} is required`,
      );
    }
  }
};

export const parseDaLibp2pRuntimeManifestDeployment = (
  value: unknown,
  fieldName: string,
): DaLibp2pRuntimeManifest["deployment"] => {
  const deployment = recordValue(value, fieldName);
  exactRecordKeys(
    deployment,
    [
      "fingerprint",
      "contract_deployment_manifest_id",
      "contract_deployment_info_sha256",
      "identity_source",
    ],
    fieldName,
  );
  if (
    deployment.identity_source !== DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.identity_source must be ${DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE}`,
      String(deployment.identity_source),
    );
  }
  const fingerprint = normalizeDaDeploymentFingerprintHex(
    nonEmptyStringValue(deployment.fingerprint, `${fieldName}.fingerprint`),
  );
  const contractDeploymentManifestId = normalizeDaDeploymentFingerprintHex(
    nonEmptyStringValue(
      deployment.contract_deployment_manifest_id,
      `${fieldName}.contract_deployment_manifest_id`,
    ),
  );
  if (fingerprint !== contractDeploymentManifestId) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.fingerprint must equal deployment.contract_deployment_manifest_id`,
      `fingerprint=${fingerprint},contract_deployment_manifest_id=${contractDeploymentManifestId}`,
    );
  }
  return {
    fingerprint,
    contract_deployment_manifest_id: contractDeploymentManifestId,
    contract_deployment_info_sha256: normalizeDaDeploymentFingerprintHex(
      nonEmptyStringValue(
        deployment.contract_deployment_info_sha256,
        `${fieldName}.contract_deployment_info_sha256`,
      ),
    ),
    identity_source: DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE,
  };
};

export const parseDaLibp2pRuntimeTopology = (
  value: unknown,
  fieldName: string,
): DaLibp2pRuntimeManifest["runtime_topology"] => {
  const topology = recordValue(value, fieldName);
  if (topology.target === "producer") {
    exactRecordKeys(
      topology,
      ["target", "profile", "producer_peer_id"],
      fieldName,
    );
    return {
      target: "producer",
      profile: nonEmptyStringValue(topology.profile, `${fieldName}.profile`),
      producer_peer_id: nonEmptyStringValue(
        topology.producer_peer_id,
        `${fieldName}.producer_peer_id`,
      ),
    };
  }
  if (topology.target === "committee") {
    exactRecordKeys(
      topology,
      ["target", "profile", "producer_peer_id", "local_signer_index"],
      fieldName,
    );
    return {
      target: "committee",
      profile: nonEmptyStringValue(topology.profile, `${fieldName}.profile`),
      producer_peer_id: nonEmptyStringValue(
        topology.producer_peer_id,
        `${fieldName}.producer_peer_id`,
      ),
      local_signer_index: safeIntegerValue(
        topology.local_signer_index,
        `${fieldName}.local_signer_index`,
        0,
        255,
      ),
    };
  }
  return fail(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    `${fieldName}.target must be producer or watcher`,
    String(topology.target),
  );
};
