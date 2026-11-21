/**
 * The node-ledger provider's slot configuration: `slotConfig()` reads the
 * ledger on every call, `cachedSlotConfig()` keeps the first successful
 * read and retries after a failed one.
 */
import { type LedgerQuery } from "@al-ft/l1-node-transport";
import { fakeL1NodeTransport } from "@al-ft/l1-node-transport/testing";
import { describe, expect, it } from "vitest";

import { LedgerProvider } from "../../src/provider/index.js";
import { DEVNET_LEDGER } from "../support/provider.js";

const DEVNET_SLOT_CONFIG = {
  zeroTime: Date.parse("2026-10-07T17:14:48Z"),
  zeroSlot: 0,
  slotLength: 1000,
};

/** A node answering the slot-configuration queries, failing the first `failures` reads. */
const slotConfigNode = (failures: number) => {
  const node = { reads: 0, failures };
  const transport = fakeL1NodeTransport({
    withLedgerState: async (_at, use) =>
      await use({
        query: async (query: LedgerQuery) => {
          if (query.query === "system_start") {
            node.reads += 1;
            if (node.failures > 0) {
              node.failures -= 1;
              throw new Error("the node is restarting");
            }
            return Buffer.from(DEVNET_LEDGER.lsq.system_start, "hex");
          }
          if (query.query === "era_history")
            return Buffer.from(DEVNET_LEDGER.lsq.era_history, "hex");
          throw new Error(`unexpected query ${query.query}`);
        },
      }),
  });
  return { node, provider: new LedgerProvider({ transport }) };
};

describe("LedgerProvider slot configuration", () => {
  it("reads the ledger on every slotConfig() call", async () => {
    const { node, provider } = slotConfigNode(0);
    expect(await provider.slotConfig()).toEqual(DEVNET_SLOT_CONFIG);
    expect(await provider.slotConfig()).toEqual(DEVNET_SLOT_CONFIG);
    expect(node.reads).toBe(2);
  });

  it("caches the first successful read and retries after a failed one", async () => {
    const { node, provider } = slotConfigNode(1);
    await expect(provider.cachedSlotConfig()).rejects.toThrow();
    expect(await provider.cachedSlotConfig()).toEqual(DEVNET_SLOT_CONFIG);
    expect(await provider.cachedSlotConfig()).toEqual(DEVNET_SLOT_CONFIG);
    expect(node.reads).toBe(2);
    // The cache is the cached method's own: slotConfig() still reads.
    expect(await provider.slotConfig()).toEqual(DEVNET_SLOT_CONFIG);
    expect(node.reads).toBe(3);
  });

  it("shares one read between concurrent callers", async () => {
    const { node, provider } = slotConfigNode(0);
    const [first, second] = await Promise.all([
      provider.cachedSlotConfig(),
      provider.cachedSlotConfig(),
    ]);
    expect(first).toEqual(DEVNET_SLOT_CONFIG);
    expect(second).toBe(first);
    expect(node.reads).toBe(1);
  });
});
