import { inputExistenceForcedSteps } from "../input-existence/forced-step.js";
import type { InputExistenceContracts } from "../input-existence/prepare-step-carriage.js";
import { NO_REFERENCE_INPUT_KIND } from "./wrongful-rejection.js";

export type NoReferenceInputForcedContracts = InputExistenceContracts;
export const {
  fieldPlan: noReferenceInputForcedFieldPlan,
  states: noReferenceInputForcedStates,
  submit: submitNoReferenceInputForcedStep,
} = inputExistenceForcedSteps(NO_REFERENCE_INPUT_KIND);
