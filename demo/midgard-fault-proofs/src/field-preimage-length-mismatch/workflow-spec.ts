import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

/** Physical manifest ordinals: first, accepted branch, forced branch, terminal. */
export const FIELD_PREIMAGE_LENGTH_CURSOR_SPEC = Object.freeze({
  category: "fieldPreimageLengthMismatch",
  stepCount: 4,
  successors: Object.freeze({
    1: Object.freeze([2, 3] as const),
    2: Object.freeze([4] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"fieldPreimageLengthMismatch">;
