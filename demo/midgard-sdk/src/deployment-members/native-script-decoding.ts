import { spend } from "./recipes.types.js";

export const nativeScriptDecodingRecipes = {
  fraudProofNativeScriptDecoding: spend(
    (c) => c.fraudProofContracts.nativeScriptDecoding.firstStep,
  ),
  fraudProofNativeScriptDecodingStep02: spend(
    (c) => c.fraudProofContracts.nativeScriptDecoding.steps[1],
  ),
  fraudProofNativeScriptDecodingStep03OpenSubject: spend(
    (c) => c.fraudProofContracts.nativeScriptDecoding.steps[2],
  ),
  fraudProofNativeScriptDecodingStep03BindDescriptor: spend(
    (c) => c.fraudProofContracts.nativeScriptDecoding.steps[3],
  ),
  fraudProofNativeScriptDecodingStep03AdvanceOrClose: spend(
    (c) => c.fraudProofContracts.nativeScriptDecoding.steps[4],
  ),
  fraudProofNativeScriptDecodingStep04: spend(
    (c) => c.fraudProofContracts.nativeScriptDecoding.steps[5],
  ),
} as const;
