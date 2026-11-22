import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthority,
} from "../../src/workflow/release-finality-policy.js";
import { terminalVerifierOf } from "./terminal-verifier.js";

export const DEPLOYMENT_FINGERPRINT = "d1".repeat(32);

export const RELEASE_FINALITY_POLICY = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };

export const releaseFinalityAuthority = (
  overrides: Partial<{
    readonly deploymentIdentityDigest: string;
    readonly blueprintHash: string;
  }> = {},
): FraudProofReleaseFinalityAuthority => ({
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest:
      overrides.deploymentIdentityDigest ?? DEPLOYMENT_FINGERPRINT,
    blueprintHash: overrides.blueprintHash ?? "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
      RELEASE_FINALITY_POLICY,
    ),
    policy: RELEASE_FINALITY_POLICY,
  }),
});

/** Accepts every candidate terminal. */
export const terminalVerifier = terminalVerifierOf(
  async ({ candidate }) => candidate,
);
