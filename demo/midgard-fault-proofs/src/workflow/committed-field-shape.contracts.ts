import {
  CommittedFieldShapeStep01Datum,
  CommittedFieldShapeStep02Datum,
} from "@al-ft/midgard-sdk";

import type { CommittedFieldShapeContracts } from "../committed-field-shape/contracts.js";
import {
  type AssemblyContext,
  type BoundCommittedFieldShapeTransactionsConfig,
  type CommittedFieldShapeBuilderSet,
  createBoundTransactionPort,
  fieldCarriageRequirementForAction,
  type ManifestBoundCommittedFieldShapeWorkflow,
  type ManifestBoundCommittedFieldShapeWorkflowConfig,
  productionBuilders,
  WITNESS_ROLES,
} from "./committed-field-shape.create-bound-transaction-port.js";
import { COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import { type CursorFamilyTransactionPort } from "./cursor-family-adapter.js";
import { COMMITTED_FIELD_SHAPE_CURSOR_SPEC } from "./cursor-family-spec.js";
import { defineFamily } from "./family-definition.js";
import { createAuthenticatedFieldCarriagePrerequisitePort } from "./field-carriage-prerequisite.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";

/**
 * The step-02 verdict binds the field-preimage certificate policy id, and a
 * step-01 slot above the tier-1 frontier is carried by §8.6 certified chunks,
 * so the family binds both the policy and its minting reference script.
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

/**
 * The transaction port resolves the authenticated field carriage before it
 * builds step-01, from the same requirement the assembly decorates the
 * adapter with.
 */
const fieldCarriagePort = (context: AssemblyContext) =>
  createAuthenticatedFieldCarriagePrerequisitePort({
    category: "committedFieldShape",
    lucid: context.lucid,
    network: context.binding.network,
    signer: context.signer,
    publications: context.l1.publications,
    requirementForAction: (input) =>
      fieldCarriageRequirementForAction(context, input),
  });

export const COMMITTED_FIELD_SHAPE_FAMILY_DEFINITION = defineFamily({
  category: "committedFieldShape",
  stepDatumSchemas: [
    CommittedFieldShapeStep01Datum,
    CommittedFieldShapeStep02Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: COMMITTED_FIELD_SHAPE_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofCommittedFieldShape",
      "fraudProofCommittedFieldShapeStep02",
    ],
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
          fieldCarriage: fieldCarriagePort(context),
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
  fieldCarriage: [{ requirementForAction: fieldCarriageRequirementForAction }],
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
}): CursorFamilyTransactionPort<"committedFieldShape"> =>
  createBoundTransactionPort(input);
