import {
  FraudProofComputationThreadStepDatum,
  NoReferenceInputStep02ThreadDatum,
  NoReferenceInputStep03ThreadDatum,
  NoReferenceInputStep04ThreadDatum,
} from "@al-ft/midgard-sdk";

import { NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import { defineLinearFamily } from "./family-definition.js";
import { inputExistenceFieldCarriageRequirements } from "./input-existence-field-carriage.js";
import {
  admitLedgerAbsenceArtifact,
  inputExistenceProofForStage,
} from "./ledger-absence-artifact.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import {
  actionInput,
  WITNESS_ROLES,
} from "./no-reference-input.capture-removal.js";
import {
  createTransactionPort,
  type ManifestBoundNoReferenceInputWorkflow,
  type ManifestBoundNoReferenceInputWorkflowConfig,
} from "./no-reference-input.create-transaction-port.js";

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
  adapter: { kind: "linear", transactionPort: createTransactionPort },
  fieldCarriage: inputExistenceFieldCarriageRequirements(
    (action) => actionInput(action).stage === "step_02",
  ),
  proofChunk: (context, { action, artifact }) =>
    inputExistenceProofForStage(
      admitLedgerAbsenceArtifact(artifact, context.signer.paymentKeyHash)
        .prepared,
      actionInput(action).stage as string,
    ),
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

export type { ManifestBoundNoReferenceInputWorkflow } from "./no-reference-input.create-transaction-port.js";
