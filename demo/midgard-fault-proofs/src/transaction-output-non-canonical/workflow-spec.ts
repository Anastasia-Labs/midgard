import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

export const TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC = Object.freeze({
  category: "transactionOutputNonCanonical",
  stepCount: 4,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([3, 4] as const),
    4: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"transactionOutputNonCanonical">;
