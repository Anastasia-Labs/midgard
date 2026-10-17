import {
  FraudProofComputationThreadStepDatum,
  NoReferenceInputStep02ThreadDatum,
  NoReferenceInputStep03ThreadDatum,
  NoReferenceInputStep04ThreadDatum,
} from "@al-ft/midgard-sdk";

import {
  admitNoReferenceInputForcedArtifact,
  NO_REFERENCE_INPUT_FORCED_ARTIFACT,
} from "../no-reference-input/artifact.js";
import { noReferenceInputForcedFieldPlan } from "../no-reference-input/submit.js";
import { NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  defineLinearFamily,
  type ManifestBoundLinearFamilyWorkflow,
} from "./family-definition.js";
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
} from "./no-reference-input.capture-removal.js";
import {
  createTransactionPort,
  type ManifestBoundNoReferenceInputWorkflowConfig,
} from "./no-reference-input.create-transaction-port.js";

export type ManifestBoundNoReferenceInputWorkflow =
  ManifestBoundLinearFamilyWorkflow<"noReferenceInput", true>;

const fieldPreimageCertificate = (context: BoundConfig) => ({
  policyId: context.certificate.policyId,
  mintingScript: context.certificate.mintingScript,
  referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
});

export const NO_REFERENCE_INPUT_FAMILY_DEFINITION = defineLinearFamily({
  category: "noReferenceInput",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    NoReferenceInputStep02ThreadDatum,
    NoReferenceInputStep03ThreadDatum,
    NoReferenceInputStep04ThreadDatum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
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
        if (artifact.schemaVersion === NO_REFERENCE_INPUT_FORCED_ARTIFACT) {
          const prepared = await admitNoReferenceInputForcedArtifact(artifact);
          return {
            planned: noReferenceInputForcedFieldPlan(
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
    if (artifact.schemaVersion === NO_REFERENCE_INPUT_FORCED_ARTIFACT)
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

export const createManifestBoundNoReferenceInputWorkflow = (
  config: ManifestBoundNoReferenceInputWorkflowConfig,
): Promise<ManifestBoundNoReferenceInputWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    NO_REFERENCE_INPUT_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundNoReferenceInputWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
