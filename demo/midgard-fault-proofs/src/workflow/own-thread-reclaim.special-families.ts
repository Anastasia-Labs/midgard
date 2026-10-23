import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { CONSERVATION_POSITIONS } from "../value-not-preserved/contracts.js";
import type {
  ManifestBoundValueConservationWorkflow,
  ManifestBoundValueConservationWorkflowConfig,
} from "../value-not-preserved/workflow.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofFamilyL1ObservationPort } from "./family-l1-observation.js";
import type {
  ManifestBoundMissingSignatureWorkflow,
  ManifestBoundMissingSignatureWorkflowConfig,
} from "./missing-signature.create-bound-transaction-port.js";
import type { FraudProofFamilyWorkflowAdapter } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  createLinearCancelReclaimPort,
  type LinearCancelContracts,
} from "./own-thread-reclaim.linear-cancel-port.js";
import { withOwnThreadReclaim } from "./own-thread-reclaim.with-own-thread-reclaim.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";

/**
 * The reclaim of our own threads in front of a family whose thread runs past
 * its four linear steps into further step scripts, in the order its L1 port
 * lists them. Its cancel is the linear cancel over that whole step list. An
 * L1 port that cannot list our threads leaves the adapter as it is.
 */
const withExtendedChainOwnThreadReclaim = ({
  category,
  family,
  base,
  l1,
  releaseFinality,
  lucid,
  signer,
  categoryId,
  contracts,
  stepReferences,
  witnessReferenceScripts,
}: {
  readonly category: "missingSignature" | "valueNotPreserved";
  readonly family: string;
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly l1: FraudProofFamilyL1ObservationPort<
    "missingSignature" | "valueNotPreserved"
  >;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly categoryId: string;
  readonly contracts: () => LinearCancelContracts;
  readonly stepReferences: readonly UTxO[];
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): FraudProofFamilyWorkflowAdapter => {
  const { observeOwnThreads, observeSignedTransaction } = l1;
  if (observeOwnThreads === undefined || observeSignedTransaction === undefined)
    return base;
  return withOwnThreadReclaim({
    category,
    base,
    releaseFinality,
    reclaim: createLinearCancelReclaimPort({
      family,
      lucid,
      signer,
      categoryId,
      contracts,
      stepReferences,
      witnessReferenceScripts,
      l1: {
        observeOwnThreads: async (input) => await l1.observeOwnThreads!(input),
        observeSignedTransaction: async (input) =>
          await l1.observeSignedTransaction!(input),
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

/** Missing-signature with the reclaim of our threads: its accepted chain,
 * then the forced door, signer and witness steps. */
export const withMissingSignatureOwnThreadReclaim = (
  workflow: ManifestBoundMissingSignatureWorkflow,
  config: ManifestBoundMissingSignatureWorkflowConfig,
): ManifestBoundMissingSignatureWorkflow => {
  const { binding } = workflow;
  const { forced } = config.referenceScripts;
  return Object.freeze({
    ...workflow,
    adapter: withExtendedChainOwnThreadReclaim({
      category: "missingSignature",
      family: "missing-signature",
      base: workflow.adapter,
      l1: workflow.l1,
      releaseFinality: binding.releaseFinality,
      lucid: config.lucid,
      signer: config.signer,
      categoryId: binding.resolvedContracts.category.categoryId,
      contracts: () => {
        const chain = binding.resolvedContracts.contracts.missingSignature;
        if (chain === undefined)
          throw new Error("missing-signature deployment binds no steps");
        return {
          steps: [
            ...chain.steps,
            chain.forcedStep,
            chain.forcedSigner,
            chain.forcedWitness,
          ],
          computationThread:
            binding.resolvedContracts.contracts.computationThread,
        };
      },
      stepReferences: [
        ...config.referenceScripts.steps,
        ...(forced === undefined
          ? []
          : [forced.bind, forced.signer, forced.witness]),
      ],
      witnessReferenceScripts: config.referenceScripts.witnesses,
    }),
  });
};

/** Value conservation with the reclaim of our threads: its legacy chain,
 * then one union step per conservation position. */
export const withValueConservationOwnThreadReclaim = (
  workflow: ManifestBoundValueConservationWorkflow,
  config: ManifestBoundValueConservationWorkflowConfig,
): ManifestBoundValueConservationWorkflow => {
  const { binding, contracts } = workflow;
  return Object.freeze({
    ...workflow,
    adapter: withExtendedChainOwnThreadReclaim({
      category: "valueNotPreserved",
      family: "value conservation",
      base: workflow.adapter,
      l1: workflow.l1,
      releaseFinality: binding.releaseFinality,
      lucid: config.lucid,
      signer: config.signer,
      categoryId: binding.resolvedContracts.category.categoryId,
      contracts: () => ({
        steps: [
          ...contracts.steps,
          ...CONSERVATION_POSITIONS.map((position) => contracts[position]),
        ],
        computationThread: contracts.computationThread,
      }),
      stepReferences: [
        ...config.referenceScripts.steps,
        ...CONSERVATION_POSITIONS.map(
          (position) => config.referenceScripts.union[position],
        ),
      ],
      witnessReferenceScripts: config.referenceScripts.witnesses,
    }),
  });
};
