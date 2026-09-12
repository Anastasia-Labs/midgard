import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

export const RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC = Object.freeze({
  category: "resolvedOutputNonCanonical",
  stepCount: 5,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze([4, 5] as const),
    5: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"resolvedOutputNonCanonical">;
