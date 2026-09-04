/**
 * Narrows `value` to a plain record whose own enumerable key set is exactly
 * `expected` — the fail-closed shape check the report decoders share.
 */
export const exactObjectKeys = (
  value: unknown,
  expected: readonly string[],
): value is Record<string, unknown> =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  Object.keys(value).length === expected.length &&
  expected.every((key) => Object.hasOwn(value, key));
