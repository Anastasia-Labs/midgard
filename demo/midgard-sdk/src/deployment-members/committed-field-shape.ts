import { spend } from "./recipes.types.js";

export const committedFieldShapeRecipes = {
  fraudProofCommittedFieldShape: spend(
    (c) => c.fraudProofContracts.committedFieldShape.firstStep,
  ),
  fraudProofCommittedFieldShapeStep02: spend(
    (c) => c.fraudProofContracts.committedFieldShape.steps[1],
  ),
} as const;
