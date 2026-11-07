import {
  FraudProofComputationThreadStepDatum,
  ReferenceInputNoIdxStep02Datum,
  ReferenceInputNoIdxStep03Datum,
  ReferenceInputNoIdxStep04Datum,
} from "@al-ft/midgard-sdk";

import { REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import { defineLinearFamily } from "./family-definition.js";
import { type FieldCarriageRequirement } from "./field-carriage-prerequisite.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import {
  admitReferenceInputNoIdxArtifact,
  WITNESS_ROLES,
} from "./reference-input-no-idx.admit-reference-input-no-idx-artifact.js";
import {
  actionInput,
  createTransactionPort,
  fieldPreimageCertificate,
  type ManifestBoundReferenceInputNoIdxWorkflow,
  type ManifestBoundReferenceInputNoIdxWorkflowConfig,
} from "./reference-input-no-idx.create-transaction-port.js";

export const REFERENCE_INPUT_NO_IDX_FAMILY_DEFINITION = defineLinearFamily({
  category: "referenceInputNoIdx",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    ReferenceInputNoIdxStep02Datum,
    ReferenceInputNoIdxStep03Datum,
    ReferenceInputNoIdxStep04Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
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
        referenceScripts: context.references,
        certificate: context.certificate,
        stateQueueMutationLeaseCoordinator:
          context.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          context.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      }),
  },
  // Step-02 opens the bad transaction's reference inputs; step-04 opens the
  // producing transaction's outputs.
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        const input = actionInput(action);
        const admitted = admitReferenceInputNoIdxArtifact(
          artifact,
          context.signer.paymentKeyHash,
        );
        const planned =
          input.stage === "step_02"
            ? admitted.referenceInputFieldPlan
            : input.stage === "step_04"
              ? admitted.outputFieldPlan
              : null;
        if (planned === null) return null;
        return {
          planned,
          compactCbor:
            input.stage === "step_02"
              ? admitted.artifact.badTx.nativeTxCompactCbor
              : admitted.artifact.producingTx.nativeTxCompactCbor,
          certificate: fieldPreimageCertificate(context),
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
  proofChunk: (context, { action, artifact }) => {
    const input = actionInput(action);
    const admitted = admitReferenceInputNoIdxArtifact(
      artifact,
      context.signer.paymentKeyHash,
    );
    return input.stage === "step_01"
      ? admitted.artifact.badTx.txMembershipProofCbor
      : input.stage === "step_03"
        ? admitted.artifact.producingTx.txMembershipProofCbor
        : null;
  },
});

export const createManifestBoundReferenceInputNoIdxWorkflow = (
  config: ManifestBoundReferenceInputNoIdxWorkflowConfig,
): Promise<ManifestBoundReferenceInputNoIdxWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    REFERENCE_INPUT_NO_IDX_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundReferenceInputNoIdxWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
