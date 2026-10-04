import { performance } from "node:perf_hooks";

/** Local monotonic deadline; wire hops receive only its remaining duration. */
export const historyProofDeadline = (budgetMs: number): number | null =>
  Number.isSafeInteger(budgetMs) && budgetMs > 0 && budgetMs <= 120_000
    ? performance.now() + budgetMs
    : null;
export const historyProofRemaining = (deadline: number): number =>
  Math.max(0, Math.floor(deadline - performance.now()));
