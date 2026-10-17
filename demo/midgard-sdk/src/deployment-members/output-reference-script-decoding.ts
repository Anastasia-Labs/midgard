import { spend } from "./recipes.types.js";

export const outputReferenceScriptDecodingRecipes = {
  fraudProofOutputReferenceScriptDecoding: spend(
    (c) => c.fraudProofContracts.outputReferenceScriptDecoding.firstStep,
  ),
  fraudProofOutputReferenceScriptDecodingStep02: spend(
    (c) => c.fraudProofContracts.outputReferenceScriptDecoding.steps[1],
  ),
  fraudProofOutputReferenceScriptDecodingStep03: spend(
    (c) => c.fraudProofContracts.outputReferenceScriptDecoding.steps[2],
  ),
  fraudProofOutputReferenceScriptDecodingStep04: spend(
    (c) => c.fraudProofContracts.outputReferenceScriptDecoding.steps[3],
  ),
  fraudProofOutputReferenceScriptDecodingStep05: spend(
    (c) => c.fraudProofContracts.outputReferenceScriptDecoding.steps[4],
  ),
  fraudProofOutputReferenceScriptDecodingStep06: spend(
    (c) => c.fraudProofContracts.outputReferenceScriptDecoding.steps[5],
  ),
} as const;
