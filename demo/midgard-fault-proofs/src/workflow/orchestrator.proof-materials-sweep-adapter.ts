import { isFinal } from "@al-ft/midgard-l1-follower/heads";

import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowRegistry,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  lucidProofMaterialsSweepPort,
  withProofMaterialsSweep,
} from "./proof-materials-sweep.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import type { WorkflowObjectiveWallet } from "./workflow-run.js";

/**
 * The adapter of `category` with the sweep of unneeded proof materials in
 * front of its ending, for every family alike: an ending at a depth past
 * the automatic-recovery window is final at release depth. The workflow
 * sweeps through its restricted wallet and the proof-materials view of its
 * signer, both installed on its objective wallet (`workflow-wallet.ts`); a workflow
 * without a restricted wallet sweeps nothing.
 */
export const adapterWithMaterialsSweep = (
  registry: FraudProofWorkflowRegistry,
  category: FraudProofFamilyWorkflowAdapter["category"],
  {
    objectiveWallet,
    releaseFinality,
  }: {
    readonly objectiveWallet: WorkflowObjectiveWallet;
    readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  },
): FraudProofFamilyWorkflowAdapter => {
  const base = registry.get(category);
  if (base === undefined)
    throw new Error(`classified family ${category} has no workflow adapter`);
  return withProofMaterialsSweep({
    base,
    view: () => objectiveWallet.proofMaterialsView(),
    port: () => {
      const wallet = objectiveWallet.sweepWallet();
      return wallet === undefined
        ? undefined
        : lucidProofMaterialsSweepPort(wallet);
    },
    // Release depth is one block past the recovery window's final depth,
    // the orchestrator's release threshold.
    final: (depth) =>
      isFinal(depth, {
        securityParameter: releaseFinality.policy.automaticRecoveryMaxDepth + 1,
      }),
  });
};
