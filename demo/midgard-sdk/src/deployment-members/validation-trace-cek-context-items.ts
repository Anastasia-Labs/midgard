import { spend } from "./recipes.types.js";

export const validationTraceCekContextItemsRecipes = {
  validationTraceDisputeCekContextItemEntry: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextItemStages.entry,
  ),
  validationTraceDisputeCekContextItemSettlement: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextItemStages
        .settlement,
  ),
} as const;
