import { WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX } from "../l1-follower/fault-proof-l1-source.js";

/**
 * The persisted sourceId of the watcher's state-queue observations, kept
 * byte-identical to the earlier source so persisted records still match.
 */
export const watcherObservationSourceId = (
  manifestId: string,
  authorityNodeId: string,
): string =>
  `${WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX}${[
    "watcher-native-crosscheck",
    manifestId,
    authorityNodeId,
  ].join("/")}`;
