import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";

/** Exact authenticated L1 cursor topology for Q33's direct/staged routes. */
export const MISSING_NATIVE_SCRIPT_UTXO_CURSOR_SPEC = Object.freeze({
  category: "missingNativeScriptUtxo",
  stepCount: 7,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze([5] as const),
    5: Object.freeze(["proof_token", 6] as const),
    6: Object.freeze([6, 7] as const),
    7: Object.freeze([7, "proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"missingNativeScriptUtxo">;
