import {
  CommittedFieldShapeStep02Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";

import type { CommittedFieldShapeContracts } from "../committed-field-shape/contracts.js";
import {
  type AssemblyContext,
  type BoundCommittedFieldShapeTransactionsConfig,
  type CommittedFieldShapeBuilderSet,
  createBoundTransactionPort,
  type ManifestBoundCommittedFieldShapeWorkflow,
  type ManifestBoundCommittedFieldShapeWorkflowConfig,
  productionBuilders,
  WITNESS_ROLES,
} from "./committed-field-shape.create-bound-transaction-port.js";
import { COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import { defineLinearFamily } from "./family-definition.js";
import { type LinearFamilyTransactionPort } from "./linear-family-adapter.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";

/**
 * The step-02 verdict binds the field-preimage certificate policy id, so the
 * manifest must publish the policy; the family never executes the minting
 * script, so it does not bind that reference script and the definition does
 * not require the certificate.
 */
const contracts = (context: AssemblyContext): CommittedFieldShapeContracts => {
  const { binding } = context;
  const chain = binding.resolvedContracts.contracts.committedFieldShape;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error(
      "committed-field-shape manifest binding omitted required contracts",
    );
  }
  return Object.freeze({
    steps: chain.steps,
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
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
};

export const COMMITTED_FIELD_SHAPE_FAMILY_DEFINITION = defineLinearFamily({
  category: "committedFieldShape",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    CommittedFieldShapeStep02Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: () => COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "linear",
    transactionPort: (context) =>
      createBoundTransactionPort({
        config: {
          lucid: context.lucid,
          blueprint: context.binding.blueprint,
          network: context.binding.network,
          signer: context.signer,
          headerHash: context.binding.definition.headerHash,
          contracts: contracts(context),
          category: context.binding.resolvedContracts.category,
          catalogue: context.binding.catalogue,
          referenceScripts: context.references,
          stateQueueMutationLeaseCoordinator:
            context.stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            context.binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
          deploymentInfo: context.binding.deploymentInfo,
        },
        builders: productionBuilders,
      }),
  },
});

export const createManifestBoundCommittedFieldShapeWorkflow = (
  config: ManifestBoundCommittedFieldShapeWorkflowConfig,
): Promise<ManifestBoundCommittedFieldShapeWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    COMMITTED_FIELD_SHAPE_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundCommittedFieldShapeWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;

export const unsafeCreateCommittedFieldShapeTransactionPortForTest = (input: {
  readonly config: BoundCommittedFieldShapeTransactionsConfig;
  readonly builders: CommittedFieldShapeBuilderSet;
}): LinearFamilyTransactionPort<"committedFieldShape"> =>
  createBoundTransactionPort(input);
