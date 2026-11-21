import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";

import type { WatcherL1Clock } from "../../src/fault-proofs/wallet-only-send-open.js";

/** An L1 chain with no applied block: the restart wait never opens. */
export const NO_APPLIED_BLOCK_L1_CLOCK: WatcherL1Clock = Object.freeze({
  slotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preview,
  appliedSlot: () => null,
});
