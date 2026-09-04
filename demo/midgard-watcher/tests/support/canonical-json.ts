/**
 * Canonical JSON serialization, as the watcher test suites recompute it.
 *
 * The suites re-derive digests the watcher itself computed, so they must use
 * the production canonicalizer — `watcherCanonicalJson` in
 * `src/storage/durable-store.ts`, the one hardened walk every watcher digest
 * and equality now goes through. Two independent test spellings used to live
 * here (one permissive about numbers, one strict); a fixture that drifts into a
 * float or an `undefined` now fails loudly on the same rule production applies.
 */
import {
  watcherCanonicalJson,
  watcherSha256CanonicalJson,
} from "../../src/storage/durable-store.js";

/** The production canonical JSON string. */
export const canonicalJson = (value: unknown): string =>
  watcherCanonicalJson(value);

/** SHA-256 over {@link canonicalJson}. */
export const canonicalDigest = (value: unknown): string =>
  watcherSha256CanonicalJson(value);

/** Alias kept for the suites that name the strict spelling. */
export const canonicalJsonForTest = canonicalJson;

/** SHA-256 over {@link canonicalJsonForTest}. */
export const sha256Canonical = canonicalDigest;

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
