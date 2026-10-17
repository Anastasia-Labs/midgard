import { spend } from "./recipes.types.js";

export const missingSignatureRecipes = {
  fraudProofMissingSignature: spend(
    (c) => c.fraudProofContracts.missingSignature.firstStep,
  ),
  fraudProofMissingSignatureStep02: spend(
    (c) => c.fraudProofContracts.missingSignature.steps[1],
  ),
  fraudProofMissingSignatureStep03: spend(
    (c) => c.fraudProofContracts.missingSignature.steps[2],
  ),
  fraudProofMissingSignatureStep04: spend(
    (c) => c.fraudProofContracts.missingSignature.steps[3],
  ),
  fraudProofMissingSignatureForcedStep: spend(
    (c) => c.fraudProofContracts.missingSignature.forcedStep,
  ),
  fraudProofMissingSignatureForcedSigner: spend(
    (c) => c.fraudProofContracts.missingSignature.forcedSigner,
  ),
  fraudProofMissingSignatureForcedWitness: spend(
    (c) => c.fraudProofContracts.missingSignature.forcedWitness,
  ),
} as const;
