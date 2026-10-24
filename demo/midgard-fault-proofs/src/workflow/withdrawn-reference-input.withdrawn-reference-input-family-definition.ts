import {
  FraudProofComputationThreadStepDatum,
  WithdrawnReferenceInputStep02Datum,
  WithdrawnReferenceInputStep03Datum,
} from "@al-ft/midgard-sdk";

import { WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import { defineLinearFamily } from "./family-definition.js";
import { type FieldCarriageRequirement } from "./field-carriage-prerequisite.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import {
  admitWithdrawnReferenceInputArtifact,
  WITNESS_ROLES,
} from "./withdrawn-reference-input.admit-withdrawn-reference-input-artifact.js";
import {
  actionInput,
  contracts,
  createTransactionPort,
  type ManifestBoundWithdrawnReferenceInputWorkflow,
  type ManifestBoundWithdrawnReferenceInputWorkflowConfig,
} from "./withdrawn-reference-input.create-transaction-port.js";

export const WITHDRAWN_REFERENCE_INPUT_FAMILY_DEFINITION = defineLinearFamily({
  category: "withdrawnReferenceInput",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    WithdrawnReferenceInputStep02Datum,
    WithdrawnReferenceInputStep03Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
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
        certificate: context.certificate,
        stateQueueMutationLeaseCoordinator:
          context.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          context.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      }),
  },
  // Step-02 opens the accepted transaction's reference inputs.
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        if (actionInput(action).stage !== "step_02") return null;
        const admitted = await admitWithdrawnReferenceInputArtifact(
          artifact,
          context.signer.paymentKeyHash,
        );
        return {
          planned: admitted.referencePlan,
          compactCbor: admitted.referencePlan.nativeTxCompactCbor,
          certificate: {
            policyId: context.certificate.policyId,
            mintingScript: context.certificate.mintingScript,
            referenceScriptUtxo:
              context.references.fieldPreimageCertificateMint,
          },
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
});

export const createManifestBoundWithdrawnReferenceInputWorkflow = (
  config: ManifestBoundWithdrawnReferenceInputWorkflowConfig,
): Promise<ManifestBoundWithdrawnReferenceInputWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    WITHDRAWN_REFERENCE_INPUT_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundWithdrawnReferenceInputWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
