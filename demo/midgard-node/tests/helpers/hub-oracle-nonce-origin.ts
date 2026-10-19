import type { L1Origin } from "@al-ft/midgard-core/l1-origin";

import type { HubOracleNonceOriginSource } from "../../src/commands/prepare-hub-oracle-nonce.l1-origin.js";

/** The local node's tip the nonce tests settle and record by default. */
export const SETTLED_ORIGIN: L1Origin = {
  slot: 4_200,
  blockHash: "0f".repeat(32),
};

/** An origin source whose node already holds `origin` under enough blocks. */
export const settledOriginSource = (
  origin: L1Origin = SETTLED_ORIGIN,
): HubOracleNonceOriginSource => ({
  observer: {
    tip: async () => origin,
    settle: async () => "settled",
  },
  depth: 10,
});
