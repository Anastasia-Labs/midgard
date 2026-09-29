import { MIDGARD_CONSENSUS_PROFILE } from ".././consensus-profile.js";
import {
  daBondManifestAmounts,
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE,
} from ".././deployment-profile.js";
import { requireRecord } from "./primitives.js";
import {
  type DeploymentManifestAvailabilityChallenge,
  type DeploymentManifestEconomics,
} from "./types.js";

/**
 * Parse the release-bound operator/fraud-proof economics block without a node
 * package dependency. Only the two compiled launch profiles and their exact
 * tuples are admissible; a network label is never consulted.
 */
export const parseDeploymentManifestEconomics = (
  value: unknown,
): DeploymentManifestEconomics => {
  const candidate = requireRecord(value, "Deployment manifest economics");
  const required = [
    "profile",
    "requiredBondLovelace",
    "slashingPenaltyLovelace",
    "inactivitySlashingPenaltyLovelace",
    "fraudProverRewardLovelace",
    "proverCollateralFloorLovelace",
  ] as const;
  if (
    Object.keys(candidate).length !== required.length ||
    required.some(
      (key) => !Object.prototype.hasOwnProperty.call(candidate, key),
    )
  ) {
    throw new Error(
      `Deployment manifest economics must contain exactly ${required.join(", ")}`,
    );
  }
  if (
    candidate.profile !== "public-preprod-launch-v1" &&
    candidate.profile !== "bounded-acceptance-v1"
  ) {
    throw new Error(
      "Deployment manifest economics.profile must be public-preprod-launch-v1 or bounded-acceptance-v1",
    );
  }
  const profile = candidate.profile;
  const expected = DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE[profile];
  for (const key of required.slice(1)) {
    const observed = candidate[key];
    if (!Number.isSafeInteger(observed) || observed !== expected[key]) {
      throw new Error(
        `Deployment manifest economics.${key} must equal ${expected[key].toString()} for ${profile}`,
      );
    }
  }
  if (
    expected.requiredBondLovelace !==
    expected.slashingPenaltyLovelace + expected.fraudProverRewardLovelace
  ) {
    throw new Error(
      "Deployment manifest economics required bond must equal slash plus reward",
    );
  }
  if (
    expected.requiredBondLovelace -
      expected.inactivitySlashingPenaltyLovelace <=
    0
  ) {
    throw new Error(
      "Deployment manifest economics required bond minus inactivity penalty must be positive",
    );
  }
  return expected;
};

const exactAvailabilityInteger = (value: unknown, field: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) <= 0) {
    throw new Error(`${field} must be a positive safe integer`);
  }
  return value as number;
};

/**
 * Absolute Q58 response-publication safety ceiling. This is deliberately
 * separate from the 4,095-byte transaction-field proof chunk bound: the
 * activated DA response chunk length is release-authenticated and must be
 * justified by the signed transaction-size measurement artifact.
 */
export const MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES = 15_148;

/**
 * Parse the release-authenticated Q58 geometry, response classes and fee/bond
 * ceilings. These values are deployment identity: neither a network label nor
 * caller metadata may choose them after the scripts are applied.
 */
export const parseDeploymentManifestAvailabilityChallenge = (
  value: unknown,
): DeploymentManifestAvailabilityChallenge => {
  const candidate = requireRecord(
    value,
    "Deployment manifest availabilityChallenge",
  );
  const required = [
    "responseClasses",
    "responseGeometry",
    "daBondLovelace",
    "challengerBondLovelace",
    "maxOpenFeeLovelace",
    "maxPublicationFeeLovelace",
    "maxSettlementFeeLovelace",
    "maxCloseFeeLovelace",
    "maxTimeoutFeeLovelace",
    "daSlashPenaltyLovelace",
    "daBondMinTopUpLovelace",
    "daBondPoolFloorLovelace",
    "challengeRecordLovelace",
  ] as const;
  if (
    Object.keys(candidate).length !== required.length ||
    required.some(
      (key) => !Object.prototype.hasOwnProperty.call(candidate, key),
    )
  ) {
    throw new Error(
      `Deployment manifest availabilityChallenge must contain exactly ${required.join(", ")}`,
    );
  }

  const responseClasses = requireRecord(
    candidate.responseClasses,
    "Deployment manifest availabilityChallenge.responseClasses",
  );
  const expectedClasses = {
    smallPayloadMaxBytes: 65_536,
    smallResponseWindowMs:
      SELECTED_DEPLOYMENT_PROFILE.timing.da_small_response_window_ms,
    fullPayloadMaxBytes: 67_108_864,
    fullResponseWindowMs:
      SELECTED_DEPLOYMENT_PROFILE.timing.da_full_response_window_ms,
  } as const;
  if (
    Object.keys(responseClasses).length !== Object.keys(expectedClasses).length
  ) {
    throw new Error(
      "Deployment manifest availabilityChallenge.responseClasses must contain exactly the canonical V1 response class fields",
    );
  }
  for (const [key, expected] of Object.entries(expectedClasses)) {
    if (responseClasses[key] !== expected) {
      throw new Error(
        `Deployment manifest availabilityChallenge.responseClasses.${key} must equal ${expected.toString()}`,
      );
    }
  }

  const geometry = requireRecord(
    candidate.responseGeometry,
    "Deployment manifest availabilityChallenge.responseGeometry",
  );
  const geometryKeys = [
    "chunkByteLength",
    "trancheByteLength",
    "maxTrancheCount",
  ] as const;
  if (
    Object.keys(geometry).length !== geometryKeys.length ||
    geometryKeys.some(
      (key) => !Object.prototype.hasOwnProperty.call(geometry, key),
    )
  ) {
    throw new Error(
      `Deployment manifest availabilityChallenge.responseGeometry must contain exactly ${geometryKeys.join(", ")}`,
    );
  }
  const chunkByteLength = exactAvailabilityInteger(
    geometry.chunkByteLength,
    "Deployment manifest availabilityChallenge.responseGeometry.chunkByteLength",
  );
  const trancheByteLength = exactAvailabilityInteger(
    geometry.trancheByteLength,
    "Deployment manifest availabilityChallenge.responseGeometry.trancheByteLength",
  );
  const maxTrancheCount = exactAvailabilityInteger(
    geometry.maxTrancheCount,
    "Deployment manifest availabilityChallenge.responseGeometry.maxTrancheCount",
  );
  if (
    chunkByteLength > MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES ||
    trancheByteLength < expectedClasses.smallPayloadMaxBytes ||
    trancheByteLength > expectedClasses.fullPayloadMaxBytes ||
    maxTrancheCount > MIDGARD_CONSENSUS_PROFILE.limits.maxOutputCount ||
    Math.ceil(expectedClasses.fullPayloadMaxBytes / trancheByteLength) >
      maxTrancheCount
  ) {
    throw new Error(
      "Deployment manifest availabilityChallenge.responseGeometry violates canonical V1 safety/coverage bounds",
    );
  }

  const daBondLovelace = exactAvailabilityInteger(
    candidate.daBondLovelace,
    "Deployment manifest availabilityChallenge.daBondLovelace",
  );
  const challengerBondLovelace = exactAvailabilityInteger(
    candidate.challengerBondLovelace,
    "Deployment manifest availabilityChallenge.challengerBondLovelace",
  );
  const maxPublicationFeeLovelace = exactAvailabilityInteger(
    candidate.maxPublicationFeeLovelace,
    "Deployment manifest availabilityChallenge.maxPublicationFeeLovelace",
  );
  const maxOpenFeeLovelace = exactAvailabilityInteger(
    candidate.maxOpenFeeLovelace,
    "Deployment manifest availabilityChallenge.maxOpenFeeLovelace",
  );
  const maxSettlementFeeLovelace = exactAvailabilityInteger(
    candidate.maxSettlementFeeLovelace,
    "Deployment manifest availabilityChallenge.maxSettlementFeeLovelace",
  );
  const maxCloseFeeLovelace = exactAvailabilityInteger(
    candidate.maxCloseFeeLovelace,
    "Deployment manifest availabilityChallenge.maxCloseFeeLovelace",
  );
  const maxTimeoutFeeLovelace = exactAvailabilityInteger(
    candidate.maxTimeoutFeeLovelace,
    "Deployment manifest availabilityChallenge.maxTimeoutFeeLovelace",
  );
  const daSlashPenaltyLovelace = exactAvailabilityInteger(
    candidate.daSlashPenaltyLovelace,
    "Deployment manifest availabilityChallenge.daSlashPenaltyLovelace",
  );
  const daBondMinTopUpLovelace = exactAvailabilityInteger(
    candidate.daBondMinTopUpLovelace,
    "Deployment manifest availabilityChallenge.daBondMinTopUpLovelace",
  );
  const daBondPoolFloorLovelace = exactAvailabilityInteger(
    candidate.daBondPoolFloorLovelace,
    "Deployment manifest availabilityChallenge.daBondPoolFloorLovelace",
  );
  const challengeRecordLovelace = exactAvailabilityInteger(
    candidate.challengeRecordLovelace,
    "Deployment manifest availabilityChallenge.challengeRecordLovelace",
  );
  // The pooled DA bond amounts are profile constants (config/deployments),
  // not deploy-time choices; the generator has already validated their
  // relations. The challenger bond stays deploy-time and independent.
  const daBondAmounts = {
    daBondLovelace,
    daSlashPenaltyLovelace,
    daBondMinTopUpLovelace,
    daBondPoolFloorLovelace,
    challengeRecordLovelace,
  };
  for (const [key, expected] of Object.entries(daBondManifestAmounts())) {
    if (daBondAmounts[key as keyof typeof daBondAmounts] !== expected) {
      throw new Error(
        `Deployment manifest availabilityChallenge.${key} must equal the selected deployment profile's value ${expected.toString()}`,
      );
    }
  }
  let publicationCount = 0;
  for (
    let offset = 0;
    offset < expectedClasses.fullPayloadMaxBytes;
    offset += trancheByteLength
  ) {
    publicationCount += Math.ceil(
      Math.min(
        trancheByteLength,
        expectedClasses.fullPayloadMaxBytes - offset,
      ) / chunkByteLength,
    );
  }
  if (
    publicationCount * maxPublicationFeeLovelace +
      maxTrancheCount * maxSettlementFeeLovelace +
      Math.max(maxCloseFeeLovelace, maxTimeoutFeeLovelace) >=
    challengerBondLovelace
  ) {
    throw new Error(
      "Deployment manifest availabilityChallenge challenger bond must cover every maximum-size publication, tranche-settlement, and terminal fee ceiling",
    );
  }
  return Object.freeze({
    responseClasses: Object.freeze(expectedClasses),
    responseGeometry: Object.freeze({
      chunkByteLength,
      trancheByteLength,
      maxTrancheCount,
    }),
    daBondLovelace,
    challengerBondLovelace,
    maxOpenFeeLovelace,
    maxPublicationFeeLovelace,
    maxSettlementFeeLovelace,
    maxCloseFeeLovelace,
    maxTimeoutFeeLovelace,
    daSlashPenaltyLovelace,
    daBondMinTopUpLovelace,
    daBondPoolFloorLovelace,
    challengeRecordLovelace,
  });
};
