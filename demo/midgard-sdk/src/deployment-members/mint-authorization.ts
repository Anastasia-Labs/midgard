import { spend } from "./recipes.types.js";

export const mintAuthorizationRecipes = {
  fraudProofMintAuthorization: spend(
    (c) => c.fraudProofContracts.mintAuthorization.firstStep,
  ),
  fraudProofMintAuthorizationStep02: spend(
    (c) => c.fraudProofContracts.mintAuthorization.steps[1],
  ),
  fraudProofMintAuthorizationStep03: spend(
    (c) => c.fraudProofContracts.mintAuthorization.steps[2],
  ),
  fraudProofMintAuthorizationStep04: spend(
    (c) => c.fraudProofContracts.mintAuthorization.steps[3],
  ),
  fraudProofMintAuthorizationStep05: spend(
    (c) => c.fraudProofContracts.mintAuthorization.steps[4],
  ),
  fraudProofMintAuthorizationStep06: spend(
    (c) => c.fraudProofContracts.mintAuthorization.steps[5],
  ),
  fraudProofMintAuthorizationStep07: spend(
    (c) => c.fraudProofContracts.mintAuthorization.steps[6],
  ),
} as const;
