import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { depositEventsRetainedBlock } from "./support/transition-trace-retained.js";

// A private devnet's chain starts long after Preprod's, so the same block time
// names a very different slot under its mapping.
const devnetSlotConfig = {
  zeroTime: 1_794_855_980_000,
  zeroSlot: 0,
  slotLength: 1000,
};

describe("a retained deposit block on a non-Preprod chain", () => {
  it("commits at the slot its own chain's mapping gives for its end time", async () => {
    const block = await depositEventsRetainedBlock({
      l1SlotConfig: devnetSlotConfig,
      operatorVkey: "11".repeat(28),
      startTime: 1_794_858_270_999n,
      endTime: 1_794_858_742_999n,
      blockSlot: 2762n,
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      priorLedger: [],
      events: [],
    });

    expect(block.header.blockSlot).toBe(2762n);
  });
});
