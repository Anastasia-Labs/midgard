import {
  CanonicalDecodabilityStep02Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";

import {
  type AssemblyContext,
  parseArtifact,
  WITNESS_ROLES,
} from "./canonical-decodability.admit-canonical-decodability-artifact.js";
import {
  contracts,
  createTransactionPort,
  fieldCarriageRequirementForAction,
  type ManifestBoundCanonicalDecodabilityWorkflow,
  type ManifestBoundCanonicalDecodabilityWorkflowConfig,
} from "./canonical-decodability.create-transaction-port.js";
import { CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import { defineLinearFamily } from "./family-definition.js";
import { createAuthenticatedFieldCarriagePrerequisitePort } from "./field-carriage-prerequisite.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";

/**
 * The transaction port resolves the authenticated field carriage before it
 * builds step-01. The port is a stateless view over the raw-L1 publication
 * observer, so the transaction port holds its own instance built from the
 * same requirement the assembly decorates the adapter with.
 */
const fieldCarriagePort = (context: AssemblyContext) =>
  createAuthenticatedFieldCarriagePrerequisitePort({
    category: "canonicalDecodability",
    lucid: context.lucid,
    network: context.binding.network,
    signer: context.signer,
    publications: context.l1.publications,
    requirementForAction: (input) =>
      fieldCarriageRequirementForAction(context, input),
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await context.l1.transactionConfirmed({ headerHash, txHash }),
  });

export const CANONICAL_DECODABILITY_FAMILY_DEFINITION = defineLinearFamily({
  category: "canonicalDecodability",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    CanonicalDecodabilityStep02Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "linear",
    transactionPort: (context) =>
      createTransactionPort({
        lucid: context.lucid,
        blueprint: context.binding.blueprint,
        deploymentInfo: context.binding.deploymentInfo,
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
      }),
  },
  fieldCarriage: [{ requirementForAction: fieldCarriageRequirementForAction }],
  proofChunk: (_context, { action, artifact }) =>
    action.input.stage === "step_01"
      ? parseArtifact(artifact).txMembershipProofCbor
      : null,
});

export const createManifestBoundCanonicalDecodabilityWorkflow = (
  config: ManifestBoundCanonicalDecodabilityWorkflowConfig,
): Promise<ManifestBoundCanonicalDecodabilityWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    CANONICAL_DECODABILITY_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundCanonicalDecodabilityWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
