import {
  FraudProofComputationThreadStepDatum,
  NonExistentInputStep02ThreadDatum,
  NonExistentInputStep03ThreadDatum,
  NonExistentInputStep04ThreadDatum,
} from "@al-ft/midgard-sdk";

import { NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
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
} from "./non-existent-input.capture-removal.js";
import {
  createTransactionPort,
  type ManifestBoundNonExistentInputWorkflow,
  type ManifestBoundNonExistentInputWorkflowConfig,
} from "./non-existent-input.create-transaction-port.js";

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

export const createManifestBoundNonExistentInputWorkflow = (
  config: ManifestBoundNonExistentInputWorkflowConfig,
): Promise<ManifestBoundNonExistentInputWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    NON_EXISTENT_INPUT_FAMILY_DEFINITION,
    config,
  );
export const runOrResumeManifestBoundNonExistentInputWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
