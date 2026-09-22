import { isUnknownArray, prototypeOf } from "./narrowing.js";

/**
 * Canonical strict-JSON encoding for records whose bytes are hashed or
 * compared.
 *
 * `capability-parity.ts` and `consensus-profile.ts` each carried a
 * character-for-character copy of this walk, differing only in the noun their
 * error messages used, and both fed a digest that must not move. One
 * implementation with a `subject` label keeps the messages they had and makes
 * the encoding itself reviewable in one place.
 *
 * Canonical means: object keys sorted by {@link String.localeCompare}, numbers
 * restricted to safe integers, arrays required to be dense, objects required to
 * be plain records with string keys only. Anything else throws rather than
 * silently hashing to something a reader would not predict — `undefined`
 * members and symbol keys are exactly the values `JSON.stringify` would drop.
 */

/** A value admitted into a canonically-encoded record. */
export type CanonicalJsonValue =
  | null
  | boolean
  | number
  | string
  | readonly CanonicalJsonValue[]
  | { readonly [key: string]: CanonicalJsonValue };

/**
 * Rewrites `value` into its canonical form, or throws a `TypeError` naming
 * `subject` if it holds anything the canonical encoding does not admit.
 */
export const canonicalJsonValue = (
  value: unknown,
  subject: string,
): CanonicalJsonValue => {
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "boolean"
  ) {
    return value;
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      throw new TypeError(`${subject} numbers must be safe integers`);
    }
    return value;
  }
  if (isUnknownArray(value)) {
    if (Object.keys(value).length !== value.length) {
      throw new TypeError(`${subject} arrays must be dense`);
    }
    return value.map((child) => canonicalJsonValue(child, subject));
  }
  if (typeof value !== "object") {
    throw new TypeError(`${subject} contains an unsupported value`);
  }
  const prototype = prototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new TypeError(`${subject} objects must be plain records`);
  }
  if (Reflect.ownKeys(value).length !== Object.keys(value).length) {
    throw new TypeError(`${subject} contains a non-string key`);
  }
  return Object.fromEntries(
    Object.entries(value)
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([key, child]): readonly [string, CanonicalJsonValue] => [
        key,
        canonicalJsonValue(child, subject),
      ]),
  );
};

/** {@link canonicalJsonValue}, serialised. This is the byte string that gets hashed. */
export const canonicalJson = (value: unknown, subject: string): string =>
  JSON.stringify(canonicalJsonValue(value, subject));
