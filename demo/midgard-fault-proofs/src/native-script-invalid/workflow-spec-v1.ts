import type { ProductionCursorFamilySpecV1 } from "../workflow/production-cursor-family-state-v1.js";

/** Exact authenticated L1 cursor topology for Q34's direct/staged routes. */
export const NATIVE_SCRIPT_INVALID_CURSOR_SPEC_V1 = Object.freeze({
  category: "nativeScriptInvalid",
  stepCount: 5,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze(["proof_token", 4] as const),
    4: Object.freeze([4, 5] as const),
    5: Object.freeze([5, "proof_token"] as const),
  }),
}) satisfies ProductionCursorFamilySpecV1<"nativeScriptInvalid">;
