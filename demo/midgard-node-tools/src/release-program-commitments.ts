import { createHash } from "node:crypto";

import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";

/** The program commitment every Midgard release binds. */
export const COMPUTATION_THREAD_POLICY_COMMITMENT =
  "computation-thread-policy-v1";

/**
 * The proof-program commitments a release of this deployment signs: the
 * computation-thread policy, committed as the watcher journeys commit it.
 */
export const deploymentProgramCommitments = (
  manifest: Pick<DeploymentManifest, "contracts">,
) => {
  const policyId = manifest.contracts.computationThreadMint?.scriptHash;
  if (policyId === undefined)
    throw new Error("the deployment has no computation-thread policy");
  return {
    [COMPUTATION_THREAD_POLICY_COMMITMENT]: createHash("sha256")
      .update(JSON.stringify({ computationThreadPolicyId: policyId }))
      .digest("hex"),
  };
};
