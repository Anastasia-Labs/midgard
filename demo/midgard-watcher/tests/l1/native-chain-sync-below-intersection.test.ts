import { describe, expect, it } from "vitest";

import {
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncEvent,
  watcherNativeChainSyncEventReceipt,
} from "../../src/l1/native-chain-sync.js";
import { start, waitFor } from "./native-chain-sync.config.js";
describe("native below-intersection rollback", () => {
  it("continues after an authenticated rollback below the session intersection", async () => {
    const events: WatcherNativeChainSyncEvent[] = [];
    const runtime = await start("below_intersection", async (event) => {
      expect(watcherNativeChainSyncEventReceipt(event)).not.toBeNull();
      events.push(event);
    });
    try {
      await waitFor(() => events.length === 3);
      expect(events[1]).toMatchObject({
        kind: "roll_backward",
        point: { slot: "90", blockHash: "dd".repeat(32) },
      });
      expect(events[2]).toMatchObject({
        kind: "roll_forward",
        prevHash: "dd".repeat(32),
        blockNo: "9",
      });
      expect(
        watcherNativeChainSyncAuthorityDetails(runtime.authority),
      ).not.toBeNull();
    } finally {
      await runtime.close();
    }
  });
});
