import type { WatcherProofRetention } from "../../src/l1-follower/proof-retention.js";

/** A retention with no follower store behind it: supervisor tests that read no L1 history. */
export const storelessProofRetention: WatcherProofRetention = Object.freeze({
  pin: async () => undefined,
  release: async () => undefined,
  holdUnits: async () => undefined,
  pinned: async () => [],
});
