import { spend } from "./recipes.types.js";

export const scriptIntegrityHashMismatchRecipes = {
  fraudProofScriptIntegrityHashMismatch: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMismatch.firstStep,
  ),
  fraudProofScriptIntegrityHashMismatchStep02: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMismatch.steps[1],
  ),
  fraudProofScriptIntegrityHashMismatchStep03: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMismatch.steps[2],
  ),
  fraudProofScriptIntegrityHashMismatchStep04: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMismatch.steps[3],
  ),
  fraudProofScriptIntegrityHashMismatchStep05: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMismatch.steps[4],
  ),
} as const;
