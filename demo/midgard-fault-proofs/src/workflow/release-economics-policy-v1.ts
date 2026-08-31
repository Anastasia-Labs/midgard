import { createHash } from "node:crypto";

import {
  type DeploymentManifestV1EconomicsProfile,
  parseDeploymentManifestV1Economics,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";

export const FRAUD_PROOF_RELEASE_ECONOMICS_AUTHORITY_V1 =
  "midgard-fraud-proof-release-economics-authority-v1" as const;
export const FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_V1_SCHEMA_VERSION =
  "midgard-fraud-proof-release-economics-policy-v1" as const;

export type ReleaseFraudProofEconomicsPolicyV1 = {
  readonly profile: DeploymentManifestV1EconomicsProfile;
  readonly requiredBondLovelace: string;
  readonly slashingPenaltyLovelace: string;
  readonly fraudProverRewardLovelace: string;
  readonly inactivitySlashingPenaltyLovelace: string;
  readonly proverCollateralFloorLovelace: string;
};

/**
 * Deployment-manifest authenticated economics. A workflow must never derive
 * these values from its caller's network label or a local SDK default.
 */
export type VerifiedFraudProofReleaseEconomicsPolicyV1 = {
  readonly schemaVersion: typeof FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_V1_SCHEMA_VERSION;
  readonly deploymentIdentityDigest: string;
  readonly releaseIdentityDigest: string;
  readonly policyDigest: string;
  readonly policy: ReleaseFraudProofEconomicsPolicyV1;
};

export interface FraudProofReleaseEconomicsAuthorityV1 {
  readonly authorityVersion: typeof FRAUD_PROOF_RELEASE_ECONOMICS_AUTHORITY_V1;
  verifyForWorkflow(input: {
    readonly deploymentFingerprint: string;
  }): Promise<VerifiedFraudProofReleaseEconomicsPolicyV1>;
}

const DIGEST = /^[0-9a-f]{64}$/u;
const NATURAL = /^(0|[1-9][0-9]*)$/u;

const canonicalPolicyJson = (
  policy: ReleaseFraudProofEconomicsPolicyV1,
): string =>
  JSON.stringify({
    profile: policy.profile,
    fraudProverRewardLovelace: policy.fraudProverRewardLovelace,
    inactivitySlashingPenaltyLovelace: policy.inactivitySlashingPenaltyLovelace,
    proverCollateralFloorLovelace: policy.proverCollateralFloorLovelace,
    requiredBondLovelace: policy.requiredBondLovelace,
    slashingPenaltyLovelace: policy.slashingPenaltyLovelace,
  });

export const computeFraudProofReleaseEconomicsPolicyDigestV1 = (
  policy: ReleaseFraudProofEconomicsPolicyV1,
): string =>
  createHash("sha256").update(canonicalPolicyJson(policy)).digest("hex");

export const validateVerifiedFraudProofReleaseEconomicsPolicyV1 = (
  value: VerifiedFraudProofReleaseEconomicsPolicyV1,
): VerifiedFraudProofReleaseEconomicsPolicyV1 => {
  const policyKeys = [
    "profile",
    "requiredBondLovelace",
    "slashingPenaltyLovelace",
    "fraudProverRewardLovelace",
    "inactivitySlashingPenaltyLovelace",
    "proverCollateralFloorLovelace",
  ] as const;
  if (
    value.policy === null ||
    typeof value.policy !== "object" ||
    Array.isArray(value.policy) ||
    Object.keys(value.policy).length !== policyKeys.length ||
    policyKeys.some(
      (key) => !Object.prototype.hasOwnProperty.call(value.policy, key),
    )
  ) {
    throw new Error(
      `release economics policy must contain exactly ${policyKeys.join(", ")}`,
    );
  }
  if (
    value.schemaVersion !==
    FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_V1_SCHEMA_VERSION
  ) {
    throw new Error("release economics policy has an unsupported schema");
  }
  if (
    !DIGEST.test(value.deploymentIdentityDigest) ||
    !DIGEST.test(value.releaseIdentityDigest) ||
    !DIGEST.test(value.policyDigest)
  ) {
    throw new Error("release economics identity digests must be 32-byte hex");
  }
  if (
    [
      value.policy.requiredBondLovelace,
      value.policy.slashingPenaltyLovelace,
      value.policy.fraudProverRewardLovelace,
      value.policy.inactivitySlashingPenaltyLovelace,
      value.policy.proverCollateralFloorLovelace,
    ].some((amount) => !NATURAL.test(amount))
  ) {
    throw new Error(
      "release economics policy is not a canonical launch profile",
    );
  }
  parseDeploymentManifestV1Economics({
    profile: value.policy.profile,
    requiredBondLovelace: Number(value.policy.requiredBondLovelace),
    slashingPenaltyLovelace: Number(value.policy.slashingPenaltyLovelace),
    fraudProverRewardLovelace: Number(value.policy.fraudProverRewardLovelace),
    inactivitySlashingPenaltyLovelace: Number(
      value.policy.inactivitySlashingPenaltyLovelace,
    ),
    proverCollateralFloorLovelace: Number(
      value.policy.proverCollateralFloorLovelace,
    ),
  });
  if (
    BigInt(value.policy.requiredBondLovelace) !==
    BigInt(value.policy.slashingPenaltyLovelace) +
      BigInt(value.policy.fraudProverRewardLovelace)
  ) {
    throw new Error(
      "release economics bond does not equal penalty plus reward",
    );
  }
  const digest = computeFraudProofReleaseEconomicsPolicyDigestV1(value.policy);
  if (digest !== value.policyDigest) {
    throw new Error("release economics policy digest mismatch");
  }
  return Object.freeze({
    ...value,
    policy: Object.freeze({ ...value.policy }),
  });
};
