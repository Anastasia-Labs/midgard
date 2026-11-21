import { inputExistenceForcedSteps } from "../input-existence/forced-step.js";
import { NON_EXISTENT_INPUT_KIND } from "./wrongful-rejection.js";

export const {
  fieldPlan: nonExistentInputForcedFieldPlan,
  states: nonExistentInputForcedStates,
  submit: submitNonExistentInputForcedStep,
} = inputExistenceForcedSteps(NON_EXISTENT_INPUT_KIND);
