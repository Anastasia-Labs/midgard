import { createHash } from "node:crypto";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

export const FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY =
  "midgard-fraud-proof-release-finality-authority-v1" as const;
export const FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION =
  "midgard-fraud-proof-release-finality-policy-v1" as const;

export type ReleaseL1FinalityPolicy = {
  readonly confirmationDepth: number;
  readonly automaticRecoveryMaxDepth: 2160;
  readonly deepRollbackPolicy: "automated_rewind_replay_incident-v1";
};

/**
 * The release policy of a manifest `l1Finality` (or of any value carrying its
 * fields): the rollback fields only. The commit-event depth binds event
 * inclusion, not release. Every producer of a release identity projects
 * through here: workflow journals record the identity and recovery compares it
 * whole, so an extra field passed through structurally would make a recovered
 * workflow differ from its own deployment and wedge it.
 */
export const releaseL1FinalityPolicyOf = (
  l1Finality: ReleaseL1FinalityPolicy,
): ReleaseL1FinalityPolicy =>
  Object.freeze({
    confirmationDepth: l1Finality.confirmationDepth,
    automaticRecoveryMaxDepth: l1Finality.automaticRecoveryMaxDepth,
    deepRollbackPolicy: l1Finality.deepRollbackPolicy,
  });

/** The selected profile's release policy. */
export const RELEASE_L1_FINALITY_POLICY: ReleaseL1FinalityPolicy =
  releaseL1FinalityPolicyOf(DEPLOYMENT_MANIFEST_L1_FINALITY);

/**
 * Manifest-verified finality identity returned by the deployment authority.
 * The workflow never accepts a caller-selected depth.
 */
export type VerifiedFraudProofReleaseFinalityPolicy = {
  readonly schemaVersion: typeof FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION;
  readonly deploymentIdentityDigest: string;
  readonly blueprintHash: string;
  readonly policyDigest: string;
  readonly policy: ReleaseL1FinalityPolicy;
};

/** Implemented by the node-side finalized deployment-manifest authority. */
export interface FraudProofReleaseFinalityAuthority {
  readonly authorityVersion: typeof FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY;
  verifyForWorkflow(input: {
    readonly deploymentFingerprint: string;
  }): Promise<VerifiedFraudProofReleaseFinalityPolicy>;
}

const DIGEST = /^[0-9a-f]{64}$/u;

const canonicalPolicyJson = (policy: ReleaseL1FinalityPolicy): string =>
  JSON.stringify({
    automaticRecoveryMaxDepth: policy.automaticRecoveryMaxDepth,
    confirmationDepth: policy.confirmationDepth,
    deepRollbackPolicy: policy.deepRollbackPolicy,
  });

export const computeFraudProofReleaseFinalityPolicyDigest = (
  policy: ReleaseL1FinalityPolicy,
): string =>
  createHash("sha256").update(canonicalPolicyJson(policy)).digest("hex");

export const validateVerifiedFraudProofReleaseFinalityPolicy = (
  value: VerifiedFraudProofReleaseFinalityPolicy,
): VerifiedFraudProofReleaseFinalityPolicy => {
  if (
    value.schemaVersion !== FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION
  ) {
    throw new Error("release finality policy has an unsupported schema");
  }
  if (
    !DIGEST.test(value.deploymentIdentityDigest) ||
    !DIGEST.test(value.blueprintHash) ||
    !DIGEST.test(value.policyDigest)
  ) {
    throw new Error("release finality identity digests must be 32-byte hex");
  }
  if (
    value.policy.confirmationDepth !==
      DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth ||
    value.policy.automaticRecoveryMaxDepth !== 2160 ||
    value.policy.deepRollbackPolicy !== "automated_rewind_replay_incident-v1"
  ) {
    throw new Error(
      "release finality policy does not match the canonical launch profile",
    );
  }
  const policyDigest = computeFraudProofReleaseFinalityPolicyDigest(
    value.policy,
  );
  if (value.policyDigest !== policyDigest) {
    throw new Error("release finality policy digest mismatch");
  }
  // The canonical identity is exactly the fields the digests bind; anything
  // else a producer carried structurally is unauthenticated and is dropped so
  // it can never reach a workflow journal.
  return Object.freeze({
    schemaVersion: value.schemaVersion,
    deploymentIdentityDigest: value.deploymentIdentityDigest,
    blueprintHash: value.blueprintHash,
    policyDigest: value.policyDigest,
    policy: releaseL1FinalityPolicyOf(value.policy),
  });
};
