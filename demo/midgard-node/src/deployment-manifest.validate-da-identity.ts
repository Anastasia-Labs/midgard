import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  isNonInteractiveTestingProfile,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";

import {
  computeDeploymentManifestDaCommitteeSignersHash,
  requireExactKeys,
  requireLowercaseHex,
  requireObject,
  requirePositiveSafeInteger,
} from "./deployment-manifest.require-out-ref-string.js";

export const validateDaIdentity = (
  candidate: Record<string, unknown>,
): void => {
  requireExactKeys(
    candidate,
    ["committeeVkeys", "committeeSignersHash", "threshold", "transportProfile"],
    [],
    "da",
  );
  if (
    !Array.isArray(candidate.committeeVkeys) ||
    candidate.committeeVkeys.length === 0
  ) {
    throw new Error(
      "Deployment manifest da.committeeVkeys must be a non-empty array",
    );
  }
  const committeeVkeys = candidate.committeeVkeys.map((vkey, index) =>
    requireLowercaseHex(vkey, 32, `da.committeeVkeys[${index.toString()}]`),
  );
  if (new Set(committeeVkeys).size !== committeeVkeys.length) {
    throw new Error(
      "Deployment manifest da.committeeVkeys must not contain duplicates",
    );
  }
  const committeeSignersHash = requireLowercaseHex(
    candidate.committeeSignersHash,
    32,
    "da.committeeSignersHash",
  );
  const expectedCommitteeSignersHash =
    computeDeploymentManifestDaCommitteeSignersHash(committeeVkeys);
  if (committeeSignersHash !== expectedCommitteeSignersHash) {
    throw new Error(
      `Deployment manifest da.committeeSignersHash mismatch: expected ${expectedCommitteeSignersHash}`,
    );
  }
  const threshold = requirePositiveSafeInteger(
    candidate.threshold,
    "da.threshold",
  );
  if (threshold > committeeVkeys.length) {
    throw new Error(
      "Deployment manifest da.threshold must not exceed committee size",
    );
  }
  const transportProfile = requireObject(
    candidate.transportProfile,
    "da.transportProfile",
  );
  requireExactKeys(
    transportProfile,
    [
      "protocolVersion",
      "runtimeManifestSchemaVersion",
      "envelopeEncoding",
      "zstdLevel",
      "limits",
      "retentionDays",
    ],
    [],
    "da.transportProfile",
  );
  if (transportProfile.protocolVersion !== DA_TRANSPORT_PROTOCOL_VERSION) {
    throw new Error(
      `Deployment manifest da.transportProfile.protocolVersion must equal ${DA_TRANSPORT_PROTOCOL_VERSION.toString()}`,
    );
  }
  if (
    transportProfile.runtimeManifestSchemaVersion !==
    DA_RUNTIME_MANIFEST_SCHEMA_VERSION
  ) {
    throw new Error(
      `Deployment manifest da.transportProfile.runtimeManifestSchemaVersion must equal ${DA_RUNTIME_MANIFEST_SCHEMA_VERSION}`,
    );
  }
  if (
    transportProfile.envelopeEncoding !== "identity" &&
    transportProfile.envelopeEncoding !== "zstd"
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.envelopeEncoding must be identity or zstd",
    );
  }
  const zstdLevel = requirePositiveSafeInteger(
    transportProfile.zstdLevel,
    "da.transportProfile.zstdLevel",
  );
  if (zstdLevel > 19) {
    throw new Error(
      "Deployment manifest da.transportProfile.zstdLevel must not exceed 19",
    );
  }
  const limits = requireObject(
    transportProfile.limits,
    "da.transportProfile.limits",
  );
  requireExactKeys(
    limits,
    Object.keys(DA_TRANSPORT_LIMITS),
    [],
    "da.transportProfile.limits",
  );
  for (const [key, expected] of Object.entries(DA_TRANSPORT_LIMITS)) {
    if (limits[key] !== expected) {
      throw new Error(
        "Deployment manifest da.transportProfile.limits must exactly match canonical V1",
      );
    }
  }
  const retentionDays = requirePositiveSafeInteger(
    transportProfile.retentionDays,
    "da.transportProfile.retentionDays",
  );
  if (retentionDays < DA_TRANSPORT_LIMITS.minimumRetentionDays) {
    throw new Error(
      `Deployment manifest da.transportProfile.retentionDays must be at least ${DA_TRANSPORT_LIMITS.minimumRetentionDays.toString()}`,
    );
  }
};

/**
 * Whether a profile's block maturity fits its dispute model. A non-interactive
 * testing profile must make interactive opening impossible on chain (maturity
 * shorter than the whole bisection schedule); every other profile must leave
 * room for the full interactive dispute (the canonical floor).
 */
export const validationDisputeMaturityFitsProfile = (
  profileName: string,
  maturityMs: number,
  limits: Readonly<{
    maxValidationBisectionRounds: number;
    validationDisputeResponseWindowMs: number;
    minValidationDisputeMaturityMs: number;
  }>,
): boolean =>
  isNonInteractiveTestingProfile(profileName)
    ? maturityMs <
      (2 * limits.maxValidationBisectionRounds + 2) *
        limits.validationDisputeResponseWindowMs
    : maturityMs >= limits.minValidationDisputeMaturityMs;

export const validateValidationDispute = (
  candidate: Record<string, unknown>,
): void => {
  requireExactKeys(
    candidate,
    ["version", "responseWindowMs", "maxBisectionRounds", "maturityMs"],
    [],
    "validationDispute",
  );
  if (
    candidate.version !== MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion ||
    candidate.responseWindowMs !==
      MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs ||
    candidate.maxBisectionRounds !==
      MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds
  ) {
    throw new Error(
      "Deployment manifest validationDispute must exactly match canonical V1",
    );
  }
  if (
    candidate.maturityMs !== MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs ||
    !validationDisputeMaturityFitsProfile(
      SELECTED_DEPLOYMENT_PROFILE.name,
      candidate.maturityMs as number,
      MIDGARD_CONSENSUS_PROFILE.limits,
    )
  ) {
    throw new Error(
      "Deployment manifest validationDispute.maturityMs must equal the canonical V1 maturity and satisfy the selected profile's dispute schedule requirements",
    );
  }
};
