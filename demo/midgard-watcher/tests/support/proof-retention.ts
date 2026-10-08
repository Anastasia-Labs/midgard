import type { WatcherProofRetention } from "../../src/l1-follower/proof-retention.js";

/**
 * A retention with no follower store behind it: supervisor tests that read
 * no L1 history. Its k is the mainnet and preprod security parameter.
 */
export const storelessProofRetention: WatcherProofRetention = Object.freeze({
  securityParameter: 2_160,
  pin: async () => ({ kind: "pinned" }) as const,
  release: async () => undefined,
  holdUnits: async () => ({ kind: "held" }) as const,
  pinned: async () => [],
  degradations: () => [],
});
