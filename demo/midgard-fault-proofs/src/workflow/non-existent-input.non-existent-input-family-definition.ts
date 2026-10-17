import {
  FraudProofComputationThreadStepDatum,
  NonExistentInputStep02ThreadDatum,
  NonExistentInputStep03ThreadDatum,
  NonExistentInputStep04ThreadDatum,
} from "@al-ft/midgard-sdk";

import {
  admitNonExistentInputForcedArtifact,
  NON_EXISTENT_INPUT_FORCED_ARTIFACT,
} from "../non-existent-input/artifact.js";
import { nonExistentInputForcedFieldPlan } from "../non-existent-input/submit.js";
import { NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import { defineLinearFamily } from "./family-definition.js";
import { type FieldCarriageRequirement } from "./field-carriage-prerequisite.js";
import { admitLedgerAbsenceArtifact } from "./ledger-absence-artifact.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import {
  actionInput,
  type BoundConfig,
  WITNESS_ROLES,
} from "./non-existent-input.capture-removal.js";
import {
  createTransactionPort,
  type ManifestBoundNonExistentInputWorkflow,
  type ManifestBoundNonExistentInputWorkflowConfig,
} from "./non-existent-input.create-transaction-port.js";

const fieldPreimageCertificate = (context: BoundConfig) => ({
  policyId: context.certificate.policyId,
  mintingScript: context.certificate.mintingScript,
  referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
});

export const NON_EXISTENT_INPUT_FAMILY_DEFINITION = defineLinearFamily({
  category: "nonExistentInput",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    NonExistentInputStep02ThreadDatum,
    NonExistentInputStep03ThreadDatum,
    NonExistentInputStep04ThreadDatum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "linear",
    transactionPort: createTransactionPort,
  },
  // Step-02 carries the disputed transaction's field preimage: the forced
  // artifact opens the forced source, the ledger-absence artifact opens the
  // accepted compact transaction.
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        if (actionInput(action).stage !== "step_02") return null;
        if (artifact.schemaVersion === NON_EXISTENT_INPUT_FORCED_ARTIFACT) {
          const prepared = await admitNonExistentInputForcedArtifact(artifact);
          return {
            planned: nonExistentInputForcedFieldPlan(
              prepared,
              context.signer.paymentKeyHash,
            ),
            compactCbor:
              prepared.forcedSource.membership.value.submitted_source
                .compact_cbor,
            witnessSetCompactCbor:
              prepared.forcedSource.membership.value.submitted_source
                .witness_set_compact_cbor,
            certificate: fieldPreimageCertificate(context),
          } satisfies FieldCarriageRequirement;
        }
        const admitted = admitLedgerAbsenceArtifact(
          artifact,
          context.signer.paymentKeyHash,
        );
        return {
          planned: admitted.fieldPlan,
          compactCbor: admitted.artifact.badTx.nativeTxCompactCbor,
          certificate: fieldPreimageCertificate(context),
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
  proofChunk: (context, { action, artifact }) => {
    if (artifact.schemaVersion === NON_EXISTENT_INPUT_FORCED_ARTIFACT)
      return null;
    const stage = actionInput(action).stage;
    const admitted = admitLedgerAbsenceArtifact(
      artifact,
      context.signer.paymentKeyHash,
    );
    return stage === "step_01"
      ? admitted.artifact.badTx.txMembershipProofCbor
      : stage === "step_03"
        ? admitted.artifact.ledgerNonMembershipProofCbor
        : stage === "step_04"
          ? admitted.artifact.txsNonMembershipProofCbor
          : null;
  },
});

export const createManifestBoundNonExistentInputWorkflow = (
  config: ManifestBoundNonExistentInputWorkflowConfig,
): Promise<ManifestBoundNonExistentInputWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    NON_EXISTENT_INPUT_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundNonExistentInputWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
