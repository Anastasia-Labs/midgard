import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

/**
 * Steps 1-4 are the binding step, the field-envelope walk, the item scan and
 * the terminal. The walk loops once per 32-item checkpoint and the scan once
 * per item window, so both admit themselves as successors.
 */
export const MINT_ITEM_NON_CANONICAL_CURSOR_SPEC = Object.freeze({
  category: "mintItemNonCanonical",
  stepCount: 4,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([2, 3] as const),
    3: Object.freeze([3, 4] as const),
    4: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"mintItemNonCanonical">;
