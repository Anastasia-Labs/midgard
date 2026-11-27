import { WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX } from "../l1-follower/fault-proof-l1-source.js";

/**
 * The persisted sourceId of the watcher's state-queue observations: the
 * fault-proof source prefix, then `watcher-native-crosscheck`, the
 * deployment's hub-oracle policy id and the local L1 node's authority id,
 * joined by `/`.
 */
export const watcherObservationSourceId = (
  hubOraclePolicyId: string,
  authorityNodeId: string,
): string =>
  `${WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX}${[
    "watcher-native-crosscheck",
    hubOraclePolicyId,
    authorityNodeId,
  ].join("/")}`;
