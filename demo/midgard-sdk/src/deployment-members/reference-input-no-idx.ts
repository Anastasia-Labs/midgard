import { spend } from "./recipes.types.js";

export const referenceInputNoIdxRecipes = {
  fraudProofReferenceInputNoIdx: spend(
    (c) => c.fraudProofContracts.referenceInputNoIdx.firstStep,
  ),
  fraudProofReferenceInputNoIdxStep02: spend(
    (c) => c.fraudProofContracts.referenceInputNoIdx.steps[1],
  ),
  fraudProofReferenceInputNoIdxStep03: spend(
    (c) => c.fraudProofContracts.referenceInputNoIdx.steps[2],
  ),
  fraudProofReferenceInputNoIdxStep04: spend(
    (c) => c.fraudProofContracts.referenceInputNoIdx.steps[3],
  ),
} as const;
