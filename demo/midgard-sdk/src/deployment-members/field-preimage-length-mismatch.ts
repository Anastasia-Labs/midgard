import { spend } from "./recipes.types.js";

export const fieldPreimageLengthMismatchRecipes = {
  fraudProofFieldPreimageLengthMismatch: spend(
    (c) => c.fraudProofContracts.fieldPreimageLengthMismatch.firstStep,
  ),
  fraudProofFieldPreimageLengthMismatchStep02Accepted: spend(
    (c) => c.fraudProofContracts.fieldPreimageLengthMismatch.acceptedStep02,
  ),
  fraudProofFieldPreimageLengthMismatchStep02Forced: spend(
    (c) => c.fraudProofContracts.fieldPreimageLengthMismatch.forcedStep02,
  ),
  fraudProofFieldPreimageLengthMismatchStep03: spend(
    (c) => c.fraudProofContracts.fieldPreimageLengthMismatch.steps[3],
  ),
} as const;
