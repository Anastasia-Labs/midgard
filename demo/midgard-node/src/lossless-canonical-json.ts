/**
 * Canonical lossless JSON: object keys sorted, integers exact (bigints as
 * integer tokens), anything that is not losslessly decoded JSON refused. Two
 * equal values always serialize to the same text, so the text can be hashed
 * and compared.
 */
import JSONBig from "json-bigint";

const lossless = JSONBig({ useNativeBigInt: true, strict: true });

/** The Shelley response uses integer JSON quantities and string ratios. Reject
 * already-rounded Number values rather than assigning them an exact identity.
 * Bigints are serialized as integer tokens, distinctly from JSON strings. */
const canonicalValue = (value: unknown): unknown => {
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "boolean" ||
    typeof value === "bigint"
  )
    return value;
  if (typeof value === "number" && Number.isSafeInteger(value)) return value;
  if (Array.isArray(value)) return value.map(canonicalValue);
  if (
    typeof value === "object" &&
    value !== null &&
    (Object.getPrototypeOf(value) === Object.prototype ||
      Object.getPrototypeOf(value) === null)
  )
    return Object.fromEntries(
      Object.keys(value)
        .sort()
        .map((key) => [
          key,
          canonicalValue((value as Record<string, unknown>)[key]),
        ]),
    );
  throw new Error(
    "History source identity requires losslessly decoded JSON values",
  );
};
export const losslessCanonicalJson = (value: unknown): string =>
  lossless.stringify(canonicalValue(value));
