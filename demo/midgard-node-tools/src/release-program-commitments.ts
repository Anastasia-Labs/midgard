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

/**
 * A release input's program commitments with the computation-thread
 * commitment derived from the manifest; a supplied value that differs from
 * the derivation is refused, never signed. The release signing command and
 * the full stack's watcher release both sign these.
 */
export const releaseProgramCommitments = (
  supplied: Readonly<Record<string, string>>,
  manifest: Pick<DeploymentManifest, "contracts">,
) => {
  const derived = deploymentProgramCommitments(manifest);
  const value = supplied[COMPUTATION_THREAD_POLICY_COMMITMENT];
  if (
    value !== undefined &&
    value !== derived[COMPUTATION_THREAD_POLICY_COMMITMENT]
  )
    throw new Error(
      `programCommitments.${COMPUTATION_THREAD_POLICY_COMMITMENT} differs from the deployment's computation-thread policy`,
    );
  return { ...supplied, ...derived };
};
