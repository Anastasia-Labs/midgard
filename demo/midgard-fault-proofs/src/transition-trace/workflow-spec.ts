import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

/** The route chooses one final; accepted/deposit finals retain their NFT while folding. */
export const TRANSITION_TRACE_CURSOR_SPEC = Object.freeze({
  category: "transitionTrace",
  stepCount: 9,
  successors: Object.freeze({
    1: Object.freeze([2, 3, 4, 5, 6, 7, 8, 9] as const),
    2: Object.freeze(["proof_token"] as const),
    3: Object.freeze(["proof_token"] as const),
    4: Object.freeze(["proof_token"] as const),
    5: Object.freeze(["proof_token"] as const),
    6: Object.freeze([6, "proof_token"] as const),
    7: Object.freeze([7, "proof_token"] as const),
    8: Object.freeze(["proof_token"] as const),
    9: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"transitionTrace">;
