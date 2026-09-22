/**
 * Narrowing helpers for the handful of standard-library predicates that hand
 * back `any`.
 *
 * `Array.isArray`, `Object.getPrototypeOf` and `JSON.parse` are all declared to
 * produce `any`, so the very checks written to make untrusted input safe are
 * what let `any` back in — and from there it spreads silently through every
 * value derived from it. Each helper here does the same runtime check with a
 * result type that stops at `unknown`.
 *
 * These exist so the `no-unsafe-*` ESLint rules can stay on: see the ratcheted
 * package list in `demo/eslint.config.mjs`.
 */

/**
 * `Array.isArray` is declared `value is any[]`, so narrowing an `unknown` with
 * it produces an array of `any`. This keeps the elements `unknown`.
 */
export const isUnknownArray = (value: unknown): value is readonly unknown[] =>
  Array.isArray(value);

/**
 * `Object.getPrototypeOf` is declared to return `any`. A plain-record check
 * only ever compares the result against `Object.prototype` or `null`, so
 * `unknown` carries everything a caller needs.
 */
export const prototypeOf = (value: object): unknown =>
  Object.getPrototypeOf(value) as unknown;

/**
 * True for a `{}`-prototype object with no symbol keys — the shape Midgard
 * admits wherever it hashes or canonicalises a decoded record. Arrays and
 * class instances are rejected.
 */
export const isPlainRecord = (
  value: unknown,
): value is Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return false;
  }
  const prototype = prototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    return false;
  }
  return Reflect.ownKeys(value).length === Object.keys(value).length;
};

/**
 * `JSON.parse` is declared to return `any`. Decoded JSON is untrusted input and
 * has to be validated regardless, so `unknown` is the honest result type.
 */
export const parseJsonUnknown = (text: string): unknown =>
  JSON.parse(text) as unknown;
