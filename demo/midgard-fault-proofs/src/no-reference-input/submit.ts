import { inputExistenceForcedSteps } from "../input-existence/forced-step.js";
import { NO_REFERENCE_INPUT_KIND } from "./wrongful-rejection.js";

export const {
  fieldPlan: noReferenceInputForcedFieldPlan,
  states: noReferenceInputForcedStates,
  submit: submitNoReferenceInputForcedStep,
} = inputExistenceForcedSteps(NO_REFERENCE_INPUT_KIND);
