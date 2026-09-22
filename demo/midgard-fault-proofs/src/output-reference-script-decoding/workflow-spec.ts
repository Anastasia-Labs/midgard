import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

export const OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC = Object.freeze({
  category: "outputReferenceScriptDecoding",
  stepCount: 6,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([3, 4] as const),
    4: Object.freeze([5] as const),
    5: Object.freeze([5, 6] as const),
    6: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"outputReferenceScriptDecoding">;
