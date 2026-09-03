import type { CursorFamilySpec } from "../workflow/production-cursor-family-state-v1.js";

/**
 * Exact authenticated L1 cursor topology for ID32. Scripts 1..6 are the six
 * logical evaluator stages. Scripts 7..13 are the accepted-direction
 * canonical purpose/source reconstruction prelude.
 */
export const EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC = Object.freeze({
  category: "executionNativeScriptInvalid",
  stepCount: 13,
  successors: Object.freeze({
    1: Object.freeze([2, 7] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze(["proof_token", 5] as const),
    5: Object.freeze([5, 6] as const),
    6: Object.freeze([6, "proof_token"] as const),
    7: Object.freeze([8] as const),
    8: Object.freeze([8, 9, 12] as const),
    9: Object.freeze([9, 10, 12] as const),
    10: Object.freeze([10, 11, 12] as const),
    11: Object.freeze([11, 12] as const),
    12: Object.freeze([12, 13, 3] as const),
    13: Object.freeze([13, 3] as const),
  }),
}) satisfies CursorFamilySpec<"executionNativeScriptInvalid">;
