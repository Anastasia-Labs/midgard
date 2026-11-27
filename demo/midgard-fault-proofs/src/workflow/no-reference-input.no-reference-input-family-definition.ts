import {
  FraudProofComputationThreadStepDatum,
  NoReferenceInputStep02ThreadDatum,
  NoReferenceInputStep03ThreadDatum,
  NoReferenceInputStep04ThreadDatum,
} from "@al-ft/midgard-sdk/fraud-proof";

import { NO_REFERENCE_INPUT_KIND } from "../no-reference-input/wrongful-rejection.js";
import { NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.detect-double-withdraws.js";
import { defineLinearFamily } from "./family-definition.family-definition.js";
import {
  inputExistenceRemoval,
  type InputExistenceWorkflowReferenceScripts,
  WITNESS_ROLES,
} from "./input-existence.capture-removal.js";
import {
  inputExistenceTransactionPort,
  type ManifestBoundInputExistenceWorkflow,
  type ManifestBoundInputExistenceWorkflowConfig,
} from "./input-existence.create-transaction-port.js";
import { inputExistenceFieldCarriageRequirements } from "./input-existence-field-carriage.js";
import {
  admitLedgerAbsenceArtifact,
  inputExistenceProofForStage,
} from "./ledger-absence-artifact.js";
import { assembleManifestBoundFamilyWorkflow } from "./manifest-bound-family-assembly.assemble-bound-manifest-bound-family-workflow.js";
import { runOrResumeManifestBoundFamilyWorkflow } from "./manifest-bound-family-assembly.run-or-resume-manifest-bound-family-workflow.js";

export type NoReferenceInputWorkflowReferenceScripts =
  InputExistenceWorkflowReferenceScripts<"noReferenceInput">;
export type ManifestBoundNoReferenceInputWorkflowConfig =
  ManifestBoundInputExistenceWorkflowConfig<"noReferenceInput">;
export type ManifestBoundNoReferenceInputWorkflow =
  ManifestBoundInputExistenceWorkflow<"noReferenceInput">;

const { actionInput } = inputExistenceRemoval(NO_REFERENCE_INPUT_KIND);

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
    transactionPort: inputExistenceTransactionPort(NO_REFERENCE_INPUT_KIND),
  },
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
