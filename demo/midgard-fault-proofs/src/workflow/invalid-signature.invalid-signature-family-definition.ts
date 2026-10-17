import {
  FraudProofComputationThreadStepDatum,
  InvalidSignatureStep02Datum,
} from "@al-ft/midgard-sdk";

import { INVALID_SIGNATURE_FORCED_ARTIFACT } from "../invalid-signature/artifact.js";
import { INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import { defineLinearFamily } from "./family-definition.js";
import type { FieldCarriageRequirement } from "./field-carriage-prerequisite.js";
import {
  admitWorkflowArtifact,
  WITNESS_ROLES,
} from "./invalid-signature.admit-invalid-signature-artifact.js";
import {
  createTransactionPort,
  type ManifestBoundInvalidSignatureWorkflow,
  type ManifestBoundInvalidSignatureWorkflowConfig,
} from "./invalid-signature.create-transaction-port.js";
import {
  record,
  witnessSetCbor,
} from "./invalid-signature.parse-address-witnesses.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";

export const INVALID_SIGNATURE_FAMILY_DEFINITION = defineLinearFamily({
  category: "invalidSignature",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    InvalidSignatureStep02Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
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
  // Step-02 opens the address-witness field of the accepted or forced
  // transaction.
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        const input = record(
          action.input,
          "invalid-signature field prerequisite action",
        );
        if (input.stage !== "step_02") return null;
        const admitted = await admitWorkflowArtifact(
          artifact,
          context.signer.paymentKeyHash,
        );
        return {
          planned: admitted.fieldPlan,
          compactCbor: admitted.artifact.nativeTxCompactCbor,
          witnessSetCompactCbor: witnessSetCbor(admitted.witnessSet),
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
  proofChunk: async (context, { action, artifact }) => {
    const input = record(
      action.input,
      "invalid-signature proof prerequisite action",
    );
    return input.stage === "step_01" &&
      artifact.schemaVersion !== INVALID_SIGNATURE_FORCED_ARTIFACT
      ? (await admitWorkflowArtifact(artifact, context.signer.paymentKeyHash))
          .artifact.txMembershipProofCbor
      : null;
  },
});

export const createManifestBoundInvalidSignatureWorkflow = (
  config: ManifestBoundInvalidSignatureWorkflowConfig,
): Promise<ManifestBoundInvalidSignatureWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    INVALID_SIGNATURE_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundInvalidSignatureWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
