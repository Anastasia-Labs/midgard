import type { CursorFamilySpec } from "./production-cursor-family-state-v1.js";

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

export const MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC = Object.freeze({
  category: "missingNativeScriptTx",
  stepCount: 8,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze([5] as const),
    5: Object.freeze([6] as const),
    6: Object.freeze(["proof_token", 7] as const),
    7: Object.freeze([7, 8] as const),
    8: Object.freeze([8, "proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"missingNativeScriptTx">;

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
  stepCount: 5,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze([4] as const),
    4: Object.freeze([4, 5] as const),
    5: Object.freeze(["proof_token"] as const),
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

export const CROSS_BLOCK_DUPLICATE_EVENT_CURSOR_SPEC = Object.freeze({
  category: "crossBlockDuplicateEvent",
  stepCount: 2,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"crossBlockDuplicateEvent">;

export const CURSOR_FAMILY_SPECS = Object.freeze({
  nativeScriptDecoding: NATIVE_SCRIPT_DECODING_CURSOR_SPEC,
  missingNativeScriptTx: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
  withdrawalMistag: WITHDRAWAL_MISTAG_CURSOR_SPEC,
  crossBlockDuplicateEvent: CROSS_BLOCK_DUPLICATE_EVENT_CURSOR_SPEC,
  valueNotPreserved: VALUE_NOT_PRESERVED_CURSOR_SPEC,
  mintAuthorization: MINT_AUTHORIZATION_CURSOR_SPEC,
});
