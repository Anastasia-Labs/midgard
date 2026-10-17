import { spend } from "./recipes.types.js";

export const nativeScriptInvalidRecipes = {
  fraudProofNativeScriptInvalid: spend(
    (c) => c.fraudProofContracts.nativeScriptInvalid.firstStep,
  ),
  fraudProofNativeScriptInvalidStep02: spend(
    (c) => c.fraudProofContracts.nativeScriptInvalid.steps[1],
  ),
  fraudProofNativeScriptInvalidStep03: spend(
    (c) => c.fraudProofContracts.nativeScriptInvalid.steps[2],
  ),
  fraudProofNativeScriptInvalidStep04: spend(
    (c) => c.fraudProofContracts.nativeScriptInvalid.steps[3],
  ),
  fraudProofNativeScriptInvalidStep05: spend(
    (c) => c.fraudProofContracts.nativeScriptInvalid.steps[4],
  ),
} as const;
