import type { CursorFamilySpec } from "../../src/workflow/cursor-family-state.js";

/**
 * A thirteen-step cursor topology with a branching first step, a mid-chain
 * proof token, self-looping streamed steps and a prelude that rejoins the
 * main chain. The cursor-state and adapter tests drive the generic machinery
 * through it; it is not the topology of any installed family.
 */
export const THIRTEEN_STEP_CURSOR_SPEC = Object.freeze({
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
