import { createHash } from "node:crypto";

export const FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1 =
  "midgard-fraud-proof-release-finality-authority-v1" as const;
export const FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION =
  "midgard-fraud-proof-release-finality-policy-v1" as const;

export type ReleaseL1FinalityPolicyV1 = {
  readonly confirmationDepth: 30;
  readonly automaticRecoveryMaxDepth: 2160;
  readonly deepRollbackPolicy: "automated_rewind_replay_incident-v1";
};

/**
 * Manifest-verified finality identity returned by the deployment authority.
 * The workflow never accepts a caller-selected depth.
 */
export type VerifiedFraudProofReleaseFinalityPolicyV1 = {
  readonly schemaVersion: typeof FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION;
  readonly deploymentIdentityDigest: string;
  readonly releaseIdentityDigest: string;
  readonly policyDigest: string;
  readonly policy: ReleaseL1FinalityPolicyV1;
};

/** Implemented by the node-side finalized deployment-manifest authority. */
export interface FraudProofReleaseFinalityAuthorityV1 {
  readonly authorityVersion: typeof FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1;
  verifyForWorkflow(input: {
    readonly deploymentFingerprint: string;
  }): Promise<VerifiedFraudProofReleaseFinalityPolicyV1>;
}

const DIGEST = /^[0-9a-f]{64}$/u;

const canonicalPolicyJson = (policy: ReleaseL1FinalityPolicyV1): string =>
  JSON.stringify({
    automaticRecoveryMaxDepth: policy.automaticRecoveryMaxDepth,
    confirmationDepth: policy.confirmationDepth,
    deepRollbackPolicy: policy.deepRollbackPolicy,
  });

export const computeFraudProofReleaseFinalityPolicyDigestV1 = (
  policy: ReleaseL1FinalityPolicyV1,
): string =>
  createHash("sha256").update(canonicalPolicyJson(policy)).digest("hex");

export const validateVerifiedFraudProofReleaseFinalityPolicyV1 = (
  value: VerifiedFraudProofReleaseFinalityPolicyV1,
): VerifiedFraudProofReleaseFinalityPolicyV1 => {
  if (
    value.schemaVersion !==
    FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION
  ) {
    throw new Error("release finality policy has an unsupported schema");
  }
  if (
    !DIGEST.test(value.deploymentIdentityDigest) ||
    !DIGEST.test(value.releaseIdentityDigest) ||
    !DIGEST.test(value.policyDigest)
  ) {
    throw new Error("release finality identity digests must be 32-byte hex");
  }
  if (
    value.policy.confirmationDepth !== 30 ||
    value.policy.automaticRecoveryMaxDepth !== 2160 ||
    value.policy.deepRollbackPolicy !== "automated_rewind_replay_incident-v1"
  ) {
    throw new Error(
      "release finality policy does not match the canonical launch profile",
    );
  }
  const policyDigest = computeFraudProofReleaseFinalityPolicyDigestV1(
    value.policy,
  );
  if (value.policyDigest !== policyDigest) {
    throw new Error("release finality policy digest mismatch");
  }
  return Object.freeze({
    ...value,
    policy: Object.freeze({ ...value.policy }),
  });
};
