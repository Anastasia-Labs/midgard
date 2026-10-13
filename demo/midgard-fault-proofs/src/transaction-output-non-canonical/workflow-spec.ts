import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

/**
 * Steps 1-5 are step-01, step-02, step-03 (the envelope scan), step-03 datum
 * (the inline datum read) and step-04 (the terminal). The scan finishes on
 * the datum step when the output carries an inline datum and its envelope is
 * canonical, otherwise on the terminal.
 */
export const TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC = Object.freeze({
  category: "transactionOutputNonCanonical",
  stepCount: 5,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([3, 4, 5] as const),
    4: Object.freeze([4, 5] as const),
    5: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"transactionOutputNonCanonical">;
