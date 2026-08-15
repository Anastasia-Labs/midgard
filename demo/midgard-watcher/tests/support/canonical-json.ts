/**
 * Canonical JSON serialization, as the watcher test suites recompute it.
 *
 * The suites re-derive digests the watcher itself computed, so they need the
 * same key-ordering rule the production canonicalizer uses. Two spellings grew
 * up independently and they are *not* interchangeable, so both live here
 * rather than being collapsed into one:
 *
 * - {@link canonicalJson} is permissive: any number is stringified as-is.
 * - {@link canonicalJsonForTest} is strict: it rejects non-safe-integer numbers
 *   and any value JSON cannot carry, so a fixture that drifts into a float or
 *   an `undefined` fails loudly instead of silently digesting something else.
 *
 * Suites that assert on digests of hand-built fixtures use the strict form;
 * suites that digest values already round-tripped through the wire use the
 * permissive one.
 */
import { createHash } from "node:crypto";

/** Permissive canonical JSON — every number is emitted via `toString()`. */
export const canonicalJson = (value: unknown): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value);
  }
  if (typeof value === "number") {
    return value.toString();
  }
  if (Array.isArray(value)) {
    return `[${value.map(canonicalJson).join(",")}]`;
  }
  const record = value as Record<string, unknown>;
  return `{${Object.keys(record)
    .sort()
    .map((key) => `${JSON.stringify(key)}:${canonicalJson(record[key])}`)
    .join(",")}}`;
};

/** SHA-256 over {@link canonicalJson}. */
export const canonicalDigest = (value: unknown): string =>
  createHash("sha256").update(canonicalJson(value), "utf8").digest("hex");

/**
 * Strict canonical JSON — rejects anything the canonical wire form cannot
 * carry, so a malformed fixture surfaces as a throw rather than a wrong digest.
 */
export const canonicalJsonForTest = (value: unknown): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value) as string;
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      throw new Error("unsupported test number");
    }
    return value.toString();
  }
  if (Array.isArray(value)) {
    return `[${value.map(canonicalJsonForTest).join(",")}]`;
  }
  if (typeof value === "object" && value !== null) {
    const record = value as Record<string, unknown>;
    return `{${Object.keys(record)
      .sort()
      .map(
        (key) => `${JSON.stringify(key)}:${canonicalJsonForTest(record[key])}`,
      )
      .join(",")}}`;
  }
  throw new Error("unsupported test value");
};

/** SHA-256 over {@link canonicalJsonForTest}. */
export const sha256Canonical = (value: unknown): string =>
  createHash("sha256")
    .update(canonicalJsonForTest(value), "utf8")
    .digest("hex");

/**
 * Reverse every object's key order, recursively.
 *
 * Used to prove the watcher's parsers key off content rather than encounter
 * order: a re-keyed wire value must still verify.
 */
export const reorderWireKeys = (value: unknown): unknown => {
  if (Array.isArray(value)) {
    return value.map(reorderWireKeys);
  }
  if (typeof value === "object" && value !== null) {
    return Object.fromEntries(
      Object.entries(value as Record<string, unknown>)
        .reverse()
        .map(([key, member]) => [key, reorderWireKeys(member)]),
    );
  }
  return value;
};
