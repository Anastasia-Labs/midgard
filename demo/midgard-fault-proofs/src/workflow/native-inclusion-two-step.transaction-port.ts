import {
  FraudProofComputationThreadStepDatum,
  InvalidRangeStep02Datum,
} from "@al-ft/midgard-sdk";

import type { InvalidRangeContracts } from "../invalid-range/contracts.js";
import { ZeroInputStep02DatumSchema } from "../zero-input/schemas.js";
import {
  INVALID_RANGE_COMPLETE_CANONICAL_REPLAY,
  ZERO_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "./complete-replay.js";
import {
  defineLinearFamily,
  type LinearFamilyPrerequisiteInput,
} from "./family-definition.js";
import { type LinearFamilyTransactionPort } from "./linear-family-adapter.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import { admitNativeInclusionTwoStepArtifact } from "./native-inclusion-two-step.admit-native-inclusion-two-step-artifact.js";
import {
  createTransactionPort,
  type ManifestBoundInvalidRangeWorkflow,
  type ManifestBoundInvalidRangeWorkflowConfig,
  type ManifestBoundZeroInputWorkflow,
  type ManifestBoundZeroInputWorkflowConfig,
  stepReferenceOutRef,
  zeroInputContracts,
} from "./native-inclusion-two-step.create-transaction-port.js";
import { type NativeInclusionTwoStepCategory } from "./native-inclusion-two-step.parse-artifact.js";
import {
  type AssemblyContext,
  WITNESS_ROLES,
} from "./native-inclusion-two-step.prepare-native-inclusion-two-step-artifact.js";

const invalidRangeContracts = <Category extends NativeInclusionTwoStepCategory>(
  context: AssemblyContext<Category>,
): InvalidRangeContracts => {
  const { binding } = context;
  const chain = binding.resolvedContracts.contracts.invalidRange;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error("invalidRange deployment chain is incomplete");
  }
  return {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: [
        "fraud_proofs/invalid_range/step_01.main.spend",
        "fraud_proofs/invalid_range/step_02.main.spend",
      ][index]!,
      referenceOutRef: stepReferenceOutRef(context, index),
    })) as unknown as InvalidRangeContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
  };
};

/**
 * The one transaction port both definitions share. The category selects
 * which contract chain is resolved; the other stays null so the port's
 * per-category branches refuse a mismatched artifact.
 */
const transactionPort = <Category extends NativeInclusionTwoStepCategory>(
  category: Category,
  context: AssemblyContext<Category>,
): LinearFamilyTransactionPort<Category> => {
  const { binding } = context;
  return createTransactionPort({
    category,
    lucid: context.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: context.signer,
    headerHash: binding.definition.headerHash,
    referenceScripts: context.references,
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
    zeroInputContracts:
      category === "zeroInput" ? zeroInputContracts(context) : null,
    invalidRangeContracts:
      category === "invalidRange" ? invalidRangeContracts(context) : null,
    categoryId: binding.resolvedContracts.category.categoryId,
  });
};

/** Only an accepted-source step-01 publishes its membership proof as chunks. */
const acceptedStep01ProofCbor = ({
  action,
  artifact,
}: LinearFamilyPrerequisiteInput): string | null => {
  const admitted = admitNativeInclusionTwoStepArtifact(artifact);
  return action.input.stage === "step_01" &&
    admitted.artifact.sourceKind === "accepted"
    ? admitted.artifact.txMembershipProofCbor
    : null;
};

export const INVALID_RANGE_FAMILY_DEFINITION = defineLinearFamily({
  category: "invalidRange",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    InvalidRangeStep02Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: () => INVALID_RANGE_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "linear",
    transactionPort: (context) => transactionPort("invalidRange", context),
  },
  proofChunk: (_context, input) => acceptedStep01ProofCbor(input),
});

export const ZERO_INPUT_FAMILY_DEFINITION = defineLinearFamily({
  category: "zeroInput",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    ZeroInputStep02DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: () => ZERO_INPUT_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "linear",
    transactionPort: (context) => transactionPort("zeroInput", context),
  },
  proofChunk: (_context, input) => acceptedStep01ProofCbor(input),
});

export const createManifestBoundInvalidRangeWorkflow = (
  config: ManifestBoundInvalidRangeWorkflowConfig,
): Promise<ManifestBoundInvalidRangeWorkflow> =>
  assembleManifestBoundFamilyWorkflow(INVALID_RANGE_FAMILY_DEFINITION, config);

export const createManifestBoundZeroInputWorkflow = (
  config: ManifestBoundZeroInputWorkflowConfig,
): Promise<ManifestBoundZeroInputWorkflow> =>
  assembleManifestBoundFamilyWorkflow(ZERO_INPUT_FAMILY_DEFINITION, config);

export const runOrResumeManifestBoundInvalidRangeWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;

export const runOrResumeManifestBoundZeroInputWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
