import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

export const FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC = Object.freeze({
  category: "fieldItemWidthIllegal",
  stepCount: 3,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"fieldItemWidthIllegal">;
