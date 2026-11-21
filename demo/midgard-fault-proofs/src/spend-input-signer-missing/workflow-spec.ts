import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

export const SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC = Object.freeze({
  category: "spendInputSignerMissing",
  stepCount: 4,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3, 4] as const),
    3: Object.freeze([3, 4] as const),
    4: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"spendInputSignerMissing">;
