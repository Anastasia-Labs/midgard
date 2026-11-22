import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthority,
} from "../../src/workflow/release-finality-policy.js";
import { terminalVerifierOf } from "./terminal-verifier.js";

export const HUB_ORACLE_POLICY_ID = "d1".repeat(28);

export const RELEASE_FINALITY_POLICY = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };

export const releaseFinalityAuthority =
  (): FraudProofReleaseFinalityAuthority => ({
    authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
    verifyForWorkflow: async () => ({
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
      hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
      blueprintHash: "e1".repeat(32),
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
