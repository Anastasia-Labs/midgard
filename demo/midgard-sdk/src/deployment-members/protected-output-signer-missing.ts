import { spend } from "./recipes.types.js";

export const protectedOutputSignerMissingRecipes = {
  fraudProofProtectedOutputSignerMissing: spend(
    (c) => c.fraudProofContracts.protectedOutputSignerMissing.firstStep,
  ),
  fraudProofProtectedOutputSignerMissingStep02: spend(
    (c) => c.fraudProofContracts.protectedOutputSignerMissing.steps[1],
  ),
  fraudProofProtectedOutputSignerMissingStep03: spend(
    (c) => c.fraudProofContracts.protectedOutputSignerMissing.steps[2],
  ),
  fraudProofProtectedOutputSignerMissingStep04: spend(
    (c) => c.fraudProofContracts.protectedOutputSignerMissing.steps[3],
  ),
  fraudProofProtectedOutputSignerMissingStep05: spend(
    (c) => c.fraudProofContracts.protectedOutputSignerMissing.steps[4],
  ),
} as const;
