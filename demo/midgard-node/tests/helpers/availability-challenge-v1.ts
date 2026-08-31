import type { DeploymentManifestV1AvailabilityChallenge } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import * as SDK from "@al-ft/midgard-sdk";

/** Explicit bounded-acceptance Q58 release candidate used by node tests. */
export const TEST_AVAILABILITY_CHALLENGE_V1 = Object.freeze({
  responseClasses: Object.freeze({
    smallPayloadMaxBytes: 65_536,
    smallResponseWindowMs: 3_600_000,
    fullPayloadMaxBytes: 67_108_864,
    fullResponseWindowMs: 172_800_000,
  }),
  responseGeometry: Object.freeze({
    chunkByteLength: 14_020,
    trancheByteLength: 4_194_304,
    maxTrancheCount: 16,
  }),
  daBondLovelace: 10_000_000_000,
  challengerBondLovelace: 10_000_000_000,
  maxOpenFeeLovelace: 500_000,
  maxPublicationFeeLovelace: 500_000,
  maxSettlementFeeLovelace: 500_000,
  maxCloseFeeLovelace: 1_000_000,
  maxTimeoutFeeLovelace: 1_200_000,
  bondOwnerCredential: "77".repeat(28),
}) satisfies DeploymentManifestV1AvailabilityChallenge;

export const TEST_AVAILABILITY_PARAMETERS_V1 = SDK.daAvailabilityParametersV1({
  responseGeometry: SDK.availabilityResponseGeometryV1(
    TEST_AVAILABILITY_CHALLENGE_V1.responseGeometry,
  ),
  daBondLovelace: BigInt(TEST_AVAILABILITY_CHALLENGE_V1.daBondLovelace),
  challengerBondLovelace: BigInt(
    TEST_AVAILABILITY_CHALLENGE_V1.challengerBondLovelace,
  ),
  maxOpenFeeLovelace: BigInt(TEST_AVAILABILITY_CHALLENGE_V1.maxOpenFeeLovelace),
  maxPublicationFeeLovelace: BigInt(
    TEST_AVAILABILITY_CHALLENGE_V1.maxPublicationFeeLovelace,
  ),
  maxSettlementFeeLovelace: BigInt(
    TEST_AVAILABILITY_CHALLENGE_V1.maxSettlementFeeLovelace,
  ),
  maxCloseFeeLovelace: BigInt(
    TEST_AVAILABILITY_CHALLENGE_V1.maxCloseFeeLovelace,
  ),
  maxTimeoutFeeLovelace: BigInt(
    TEST_AVAILABILITY_CHALLENGE_V1.maxTimeoutFeeLovelace,
  ),
});
