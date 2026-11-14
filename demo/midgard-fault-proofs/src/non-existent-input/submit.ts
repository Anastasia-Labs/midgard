import { inputExistenceForcedSteps } from "../input-existence/forced-step.js";
import type { InputExistenceContracts } from "../input-existence/prepare-step-carriage.js";
import { NON_EXISTENT_INPUT_KIND } from "./wrongful-rejection.js";

export type NonExistentInputForcedContracts = InputExistenceContracts;
export const {
  fieldPlan: nonExistentInputForcedFieldPlan,
  states: nonExistentInputForcedStates,
  submit: submitNonExistentInputForcedStep,
} = inputExistenceForcedSteps(NON_EXISTENT_INPUT_KIND);
