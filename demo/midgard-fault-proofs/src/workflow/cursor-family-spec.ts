import type { CursorFamilySpec } from "./cursor-family-state.js";

/**
 * Closed chain-topology declarations for the non-linear/long launch families.
 * These describe only authenticated L1 cursor transitions. They deliberately
 * do not imply detector, evidence, transaction-port, or application readiness.
 */
export const NATIVE_SCRIPT_DECODING_CURSOR_SPEC = Object.freeze({
  category: "nativeScriptDecoding",
  stepCount: 6,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([4, 6] as const),
    4: Object.freeze([5, 6] as const),
    5: Object.freeze([5, 6] as const),
    6: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"nativeScriptDecoding">;

export const VALUE_NOT_PRESERVED_CURSOR_SPEC = Object.freeze({
  category: "valueNotPreserved",
  stepCount: 4,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([2, 3] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"valueNotPreserved">;

export const MINT_AUTHORIZATION_CURSOR_SPEC = Object.freeze({
  category: "mintAuthorization",
  stepCount: 7,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([2, 3] as const),
    3: Object.freeze([4, 5, 6, 7] as const),
    4: Object.freeze([4, 5] as const),
    5: Object.freeze(["proof_token"] as const),
    6: Object.freeze([6, 5] as const),
    7: Object.freeze([7, 4] as const),
  }),
}) satisfies CursorFamilySpec<"mintAuthorization">;

export const WITHDRAWAL_MISTAG_CURSOR_SPEC = Object.freeze({
  category: "withdrawalMistag",
  stepCount: 5,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze([5] as const),
    5: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"withdrawalMistag">;

/**
 * `missingRedeemer` walks the shared direct-derivation graph: the source walk
 * loops between its resolved, authenticate, map and witness roles; each
 * purpose route and scan repeats over bounded pages; the family entry chooses
 * the observed route or claim custody; adjudication ends at the terminal
 * directly or after a claimed-redeemer scan.
 */
export const MISSING_REDEEMER_CURSOR_SPEC = Object.freeze({
  category: "missingRedeemer",
  stepCount: 39,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze([5] as const),
    5: Object.freeze([6, 8] as const),
    6: Object.freeze([7] as const),
    7: Object.freeze([5, 10] as const),
    8: Object.freeze([9, 5] as const),
    9: Object.freeze([9, 10] as const),
    10: Object.freeze([7, 11] as const),
    11: Object.freeze([12] as const),
    12: Object.freeze([13, 27] as const),
    13: Object.freeze([14, 16, 18, 23] as const),
    14: Object.freeze([15] as const),
    15: Object.freeze([15, 23] as const),
    16: Object.freeze([16, 17] as const),
    17: Object.freeze([17, 23] as const),
    18: Object.freeze([19] as const),
    19: Object.freeze([19, 20] as const),
    20: Object.freeze([20, 21] as const),
    21: Object.freeze([22] as const),
    22: Object.freeze([22, 23] as const),
    23: Object.freeze([24] as const),
    24: Object.freeze([25] as const),
    25: Object.freeze([25, 26] as const),
    26: Object.freeze([26, 27] as const),
    27: Object.freeze([28, 30, 32, 36] as const),
    28: Object.freeze([29] as const),
    29: Object.freeze([29, 36] as const),
    30: Object.freeze([30, 31] as const),
    31: Object.freeze([31, 36] as const),
    32: Object.freeze([32, 33] as const),
    33: Object.freeze([33, 34] as const),
    34: Object.freeze([35] as const),
    35: Object.freeze([35, 36] as const),
    36: Object.freeze([37, 39] as const),
    37: Object.freeze([37, 38] as const),
    38: Object.freeze([38, 39] as const),
    39: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"missingRedeemer">;

/**
 * `scriptIntegrityHashMismatch` walks the shared bind and source graph, then
 * the all-purposes walk: spends alternate between taking and resolving pages;
 * each variable field certifies before it is walked; an empty field goes
 * straight to the next field's opening, and the exhausted receive field ends
 * at the terminal.
 */
export const SCRIPT_INTEGRITY_HASH_MISMATCH_CURSOR_SPEC = Object.freeze({
  category: "scriptIntegrityHashMismatch",
  stepCount: 21,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze([5] as const),
    5: Object.freeze([6, 8] as const),
    6: Object.freeze([7] as const),
    7: Object.freeze([5, 10] as const),
    8: Object.freeze([9, 5] as const),
    9: Object.freeze([9, 10] as const),
    10: Object.freeze([7, 11] as const),
    11: Object.freeze([12] as const),
    12: Object.freeze([13, 15] as const),
    13: Object.freeze([14] as const),
    14: Object.freeze([13, 15] as const),
    15: Object.freeze([15, 16, 17] as const),
    16: Object.freeze([16, 17] as const),
    17: Object.freeze([18, 19] as const),
    18: Object.freeze([18, 19] as const),
    19: Object.freeze([19, 20, 21] as const),
    20: Object.freeze([20, 21] as const),
    21: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"scriptIntegrityHashMismatch">;

/**
 * `canonicalDecodability` walks the committed field one bounded batch per
 * step-01 transaction, returning to step 01 while items remain and moving to
 * step 02 with the verdict.
 */
export const CANONICAL_DECODABILITY_CURSOR_SPEC = Object.freeze({
  category: "canonicalDecodability",
  stepCount: 2,
  successors: Object.freeze({
    1: Object.freeze([1, 2] as const),
    2: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"canonicalDecodability">;

/**
 * `committedFieldShape` walks the committed field one bounded batch per
 * step-01 transaction, returning to step 01 while items remain and moving to
 * step 02 with the slot's shape verdict.
 */
export const COMMITTED_FIELD_SHAPE_CURSOR_SPEC = Object.freeze({
  category: "committedFieldShape",
  stepCount: 2,
  successors: Object.freeze({
    1: Object.freeze([1, 2] as const),
    2: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"committedFieldShape">;

export const CURSOR_FAMILY_SPECS = Object.freeze({
  nativeScriptDecoding: NATIVE_SCRIPT_DECODING_CURSOR_SPEC,
  withdrawalMistag: WITHDRAWAL_MISTAG_CURSOR_SPEC,
  valueNotPreserved: VALUE_NOT_PRESERVED_CURSOR_SPEC,
  mintAuthorization: MINT_AUTHORIZATION_CURSOR_SPEC,
  missingRedeemer: MISSING_REDEEMER_CURSOR_SPEC,
  scriptIntegrityHashMismatch: SCRIPT_INTEGRITY_HASH_MISMATCH_CURSOR_SPEC,
  canonicalDecodability: CANONICAL_DECODABILITY_CURSOR_SPEC,
  committedFieldShape: COMMITTED_FIELD_SHAPE_CURSOR_SPEC,
});
