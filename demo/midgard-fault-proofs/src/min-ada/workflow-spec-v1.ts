import type { ProductionCursorFamilySpecV1 } from "../workflow/production-cursor-family-state-v1.js";

/** Exact authenticated L1 cursor topology for Q27 MIN-ADA-TX/UTXO. */
export const MIN_ADA_CURSOR_SPEC_V1 = Object.freeze({
  category: "minAda",
  stepCount: 5,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3, 5] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze([5] as const),
    5: Object.freeze(["proof_token"] as const),
  }),
}) satisfies ProductionCursorFamilySpecV1<"minAda">;
