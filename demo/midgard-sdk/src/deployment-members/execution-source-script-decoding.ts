import { spend } from "./recipes.types.js";

export const executionSourceScriptDecodingRecipes = {
  fraudProofExecutionSourceScriptDecoding: spend(
    (c) => c.fraudProofContracts.executionSourceScriptDecoding.firstStep,
  ),
  fraudProofExecutionSourceScriptDecodingStep02: spend(
    (c) => c.fraudProofContracts.executionSourceScriptDecoding.steps[1],
  ),
  fraudProofExecutionSourceScriptDecodingStep03: spend(
    (c) => c.fraudProofContracts.executionSourceScriptDecoding.steps[2],
  ),
  fraudProofExecutionSourceScriptDecodingStep04: spend(
    (c) => c.fraudProofContracts.executionSourceScriptDecoding.steps[3],
  ),
  fraudProofExecutionSourceScriptDecodingStep05: spend(
    (c) => c.fraudProofContracts.executionSourceScriptDecoding.steps[4],
  ),
} as const;
