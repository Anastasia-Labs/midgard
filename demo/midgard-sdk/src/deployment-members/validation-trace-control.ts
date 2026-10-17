import { spend } from "./recipes.types.js";

const control = (select: Parameters<typeof spend>[0]) =>
  spend(
    select,
    (contracts) => contracts.fraudProofs.validationTraceDispute !== undefined,
  );

export const validationTraceControlRecipes = {
  validationTraceDispute: control((c) => c.fraudProofs.validationTraceDispute),
  validationTraceDisputeSource: control(
    (c) => c.fraudProofs.validationTraceDispute.source,
  ),
  validationTraceDisputeGame: control(
    (c) => c.fraudProofs.validationTraceDispute.game,
  ),
  validationTraceDisputeBoundary: control(
    (c) => c.fraudProofs.validationTraceDispute.boundary,
  ),
  validationTraceDisputeTimeout: control(
    (c) => c.fraudProofs.validationTraceDispute.timeout,
  ),
  validationTraceDisputeAward: control(
    (c) => c.fraudProofs.validationTraceDispute.award,
  ),
} as const;
