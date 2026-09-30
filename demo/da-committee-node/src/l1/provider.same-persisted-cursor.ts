import {
  type ChainSyncCursor,
  samePersistedCanonicalPoint,
} from "./provider.parse-persisted-chain-sync-state.js";

export const samePersistedCursor = (
  left: ChainSyncCursor,
  right: ChainSyncCursor,
): boolean =>
  left.sequence === right.sequence &&
  left.rollbackGeneration === right.rollbackGeneration &&
  samePersistedCanonicalPoint(left.point, right.point);
