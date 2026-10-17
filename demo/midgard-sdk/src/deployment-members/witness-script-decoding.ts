import { spend } from "./recipes.types.js";

export const witnessScriptDecodingRecipes = {
  fraudProofWitnessScriptDecoding: spend(
    (c) => c.fraudProofContracts.witnessScriptDecoding.firstStep,
  ),
  fraudProofWitnessScriptDecodingStep02: spend(
    (c) => c.fraudProofContracts.witnessScriptDecoding.steps[1],
  ),
  fraudProofWitnessScriptDecodingStep03: spend(
    (c) => c.fraudProofContracts.witnessScriptDecoding.steps[2],
  ),
  fraudProofWitnessScriptDecodingStep04: spend(
    (c) => c.fraudProofContracts.witnessScriptDecoding.steps[3],
  ),
} as const;
