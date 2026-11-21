import type { UTxO } from "@lucid-evolution/lucid";

import type { LinearFaultStepContract } from "../linear-fault-family.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type {
  FamilyCategory,
  FamilyDeploymentContext,
  FaultProofWitnessRole,
} from "./family-definition.js";
import type { FraudProofFamilyWorkflowAdapter } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  createLinearCancelReclaimPort,
  type LinearCancelContracts,
} from "./own-thread-reclaim.linear-cancel-port.js";
import { withOwnThreadReclaim } from "./own-thread-reclaim.with-own-thread-reclaim.js";

/**
 * The family adapter with the reclaim of our own threads in front of every
 * resolution by another record, over the deployment's bound contracts and
 * reference scripts. A deployment whose L1 port cannot list our threads
 * keeps the adapter as it is: a resolution that still finds a thread of
 * ours then holds the objective under the operator's alarm.
 */
export const withDeploymentOwnThreadReclaim = <
  Category extends FamilyCategory,
  Witness extends FaultProofWitnessRole,
  Certificate extends boolean,
  StepCount extends number,
>({
  category,
  base,
  deployment,
}: {
  readonly category: Category;
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly deployment: FamilyDeploymentContext<
    Category,
    Witness,
    Certificate,
    StepCount
  >;
}): FraudProofFamilyWorkflowAdapter => {
  const { binding, l1, references } = deployment;
  if (l1.observeOwnThreads === undefined) return base;
  // Contracts are read when a reclaim is first needed: a deployment that is
  // never asked to reclaim never depends on them.
  const contracts = (): LinearCancelContracts => {
    const resolved = binding.resolvedContracts.contracts as unknown as Readonly<
      Record<
        string,
        { readonly steps: readonly LinearFaultStepContract[] } | undefined
      >
    > & {
      readonly computationThread: LinearCancelContracts["computationThread"];
    };
    const chain = resolved[category];
    if (chain === undefined)
      throw new Error(`${category} deployment binds no step contracts`);
    return {
      steps: chain.steps,
      computationThread: resolved.computationThread,
    };
  };
  return withOwnThreadReclaim({
    category,
    base,
    releaseFinality: binding.releaseFinality,
    reclaim: createLinearCancelReclaimPort({
      family: category,
      lucid: deployment.lucid,
      signer: deployment.signer,
      categoryId: binding.definition.categoryId,
      contracts,
      stepReferences: references.steps as readonly UTxO[],
      witnessReferenceScripts:
        references.witnesses as FaultProofWitnessReferenceScripts,
      l1: {
        observeOwnThreads: async (input) => await l1.observeOwnThreads!(input),
        observeSignedTransaction: async (input) =>
          await l1.observeSignedTransaction(input),
        ...(l1.rebroadcastSignedTransaction === undefined
          ? {}
          : {
              rebroadcastSignedTransaction: async (input) =>
                await l1.rebroadcastSignedTransaction!(input),
            }),
      },
    }),
  });
};
