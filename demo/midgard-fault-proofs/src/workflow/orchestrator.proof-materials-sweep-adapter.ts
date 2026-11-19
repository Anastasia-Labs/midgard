import { isFinal } from "@al-ft/midgard-l1-follower/heads";

import { workflowProofMaterialsSweepWallet } from "./funding-reservation-permit.restrict-workflow-funding-signer.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowRegistry,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  lucidProofMaterialsSweepPort,
  withProofMaterialsSweep,
} from "./proof-materials-sweep.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";

/**
 * The adapter of `category` with the sweep of its proof materials in front
 * of its ending, for every family alike: an ending at a depth past the
 * automatic-recovery window is final at release depth. The workflow sweeps
 * through the restricted wallet of its funding reservation, read on its
 * journal; a workflow without one sweeps nothing.
 */
export const adapterWithMaterialsSweep = (
  registry: FraudProofWorkflowRegistry,
  category: FraudProofFamilyWorkflowAdapter["category"],
  {
    journal,
    terminalVerifier,
    releaseFinality,
  }: {
    readonly journal: object;
    readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
    readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  },
): FraudProofFamilyWorkflowAdapter => {
  const base = registry.get(category);
  if (base === undefined)
    throw new Error(`classified family ${category} has no workflow adapter`);
  return withProofMaterialsSweep({
    base,
    port: () => {
      const wallet = workflowProofMaterialsSweepWallet(journal);
      return wallet === undefined
        ? undefined
        : lucidProofMaterialsSweepPort({
            ...wallet,
            observeSignedTransaction: async (input) =>
              await terminalVerifier.observeSignedTransaction(input),
          });
    },
    // Release depth is one block past the recovery window's final depth,
    // the orchestrator's release threshold.
    final: (depth) =>
      isFinal(depth, {
        securityParameter: releaseFinality.policy.automaticRecoveryMaxDepth + 1,
      }),
  });
};
