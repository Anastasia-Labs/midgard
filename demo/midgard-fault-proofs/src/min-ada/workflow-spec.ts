import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

/** Exact authenticated L1 cursor topology for Q27 MIN-ADA-TX/UTXO. */
export const MIN_ADA_CURSOR_SPEC = Object.freeze({
  category: "minAda",
  stepCount: 5,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([4, 5] as const),
    4: Object.freeze([5] as const),
    5: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"minAda">;
