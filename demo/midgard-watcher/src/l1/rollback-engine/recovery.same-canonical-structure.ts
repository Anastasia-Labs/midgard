import { exactArray, exactPlainRecord } from "./records.js";
import { evaluateWatcherPostFinalityRecoveryInternal } from "./recovery.evaluate-watcher-post-finality-recovery-internal.js";
import { rejectPostFinalityRecovery } from "./recovery.verify-post-finality-path.js";
import {
  type WatcherPostFinalityRecoveryInput,
  type WatcherPostFinalityRecoveryResult,
} from "./types.js";

export const evaluateWatcherPostFinalityRecovery = (
  input: WatcherPostFinalityRecoveryInput,
): WatcherPostFinalityRecoveryResult => {
  try {
    return evaluateWatcherPostFinalityRecoveryInternal(input);
  } catch {
    return rejectPostFinalityRecovery("recovery_path_malformed");
  }
};

const sameCanonicalStructure = (
  value: unknown,
  canonical: unknown,
  visiting: WeakSet<object> = new WeakSet<object>(),
  depth = 0,
): boolean => {
  if (
    value === null ||
    canonical === null ||
    typeof value !== "object" ||
    typeof canonical !== "object"
  ) {
    return value === canonical;
  }
  if (depth > 256 || visiting.has(value)) {
    return false;
  }
  visiting.add(value);
  try {
    if (Array.isArray(canonical)) {
      const members = exactArray(value);
      return (
        members !== null &&
        members.length === canonical.length &&
        canonical.every((expected, index) =>
          sameCanonicalStructure(members[index], expected, visiting, depth + 1),
        )
      );
    }
    if (Array.isArray(value)) {
      return false;
    }
    const canonicalRecord = canonical as Record<string, unknown>;
    const record = exactPlainRecord(value, Object.keys(canonicalRecord));
    return (
      record !== null &&
      Object.keys(canonicalRecord).every((key) =>
        sameCanonicalStructure(
          record[key],
          canonicalRecord[key],
          visiting,
          depth + 1,
        ),
      )
    );
  } finally {
    visiting.delete(value);
  }
};

/**
 * Shared W13 trust boundary. Candidate self-hashes are not
 * authority: the exact recovery input is replayed and only a safe,
 * byte-equivalent canonical result shape is accepted.
 */
export const parseWatcherPostFinalityRecoveryResult = (
  value: unknown,
  input: WatcherPostFinalityRecoveryInput,
): WatcherPostFinalityRecoveryResult | null => {
  try {
    const expected = evaluateWatcherPostFinalityRecovery(input);
    return sameCanonicalStructure(value, expected) ? expected : null;
  } catch {
    return null;
  }
};
