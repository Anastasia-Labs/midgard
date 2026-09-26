import type { DeploymentManifestAvailabilityChallenge } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";

/** Explicit bounded-acceptance Q58 release candidate used by node tests. */
export const TEST_AVAILABILITY_CHALLENGE = Object.freeze({
  responseClasses: Object.freeze({
    smallPayloadMaxBytes: 65_536,
    smallResponseWindowMs:
      SELECTED_DEPLOYMENT_PROFILE.timing.da_small_response_window_ms,
    fullPayloadMaxBytes: 67_108_864,
    fullResponseWindowMs:
      SELECTED_DEPLOYMENT_PROFILE.timing.da_full_response_window_ms,
  }),
  responseGeometry: Object.freeze({
    chunkByteLength: 14_020,
    trancheByteLength: 4_194_304,
    maxTrancheCount: 16,
  }),
  daBondLovelace: 12_000_000_000,
  challengerBondLovelace: 12_000_000_000,
  maxOpenFeeLovelace: 2_000_000,
  maxPublicationFeeLovelace: 2_000_000,
  maxSettlementFeeLovelace: 2_000_000,
  maxCloseFeeLovelace: 2_000_000,
  maxTimeoutFeeLovelace: 3_000_000,
  bondOwnerCredential: "77".repeat(28),
}) satisfies DeploymentManifestAvailabilityChallenge;

export const TEST_AVAILABILITY_PARAMETERS = SDK.daAvailabilityParameters({
  responseGeometry: SDK.availabilityResponseGeometry(
    TEST_AVAILABILITY_CHALLENGE.responseGeometry,
  ),
  daBondLovelace: BigInt(TEST_AVAILABILITY_CHALLENGE.daBondLovelace),
  challengerBondLovelace: BigInt(
    TEST_AVAILABILITY_CHALLENGE.challengerBondLovelace,
  ),
  maxOpenFeeLovelace: BigInt(TEST_AVAILABILITY_CHALLENGE.maxOpenFeeLovelace),
  maxPublicationFeeLovelace: BigInt(
    TEST_AVAILABILITY_CHALLENGE.maxPublicationFeeLovelace,
  ),
  maxSettlementFeeLovelace: BigInt(
    TEST_AVAILABILITY_CHALLENGE.maxSettlementFeeLovelace,
  ),
  maxCloseFeeLovelace: BigInt(TEST_AVAILABILITY_CHALLENGE.maxCloseFeeLovelace),
  maxTimeoutFeeLovelace: BigInt(
    TEST_AVAILABILITY_CHALLENGE.maxTimeoutFeeLovelace,
  ),
});
