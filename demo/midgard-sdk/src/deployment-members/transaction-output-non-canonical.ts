import { spend } from "./recipes.types.js";

export const transactionOutputNonCanonicalRecipes = {
  fraudProofTransactionOutputNonCanonical: spend(
    (c) => c.fraudProofContracts.transactionOutputNonCanonical.firstStep,
  ),
  fraudProofTransactionOutputNonCanonicalStep02: spend(
    (c) => c.fraudProofContracts.transactionOutputNonCanonical.steps[1],
  ),
  fraudProofTransactionOutputNonCanonicalStep03: spend(
    (c) => c.fraudProofContracts.transactionOutputNonCanonical.steps[2],
  ),
  fraudProofTransactionOutputNonCanonicalStep03Datum: spend(
    (c) => c.fraudProofContracts.transactionOutputNonCanonical.steps[3],
  ),
  fraudProofTransactionOutputNonCanonicalStep04: spend(
    (c) => c.fraudProofContracts.transactionOutputNonCanonical.steps[4],
  ),
} as const;
