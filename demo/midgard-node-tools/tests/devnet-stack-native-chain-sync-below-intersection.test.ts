import {
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncEvent,
  watcherNativeChainSyncEventReceipt,
} from "midgard-watcher";
import { describe, expect, it } from "vitest";

import { waitFor } from "./helpers/native-chain-sync.config.js";
import { start } from "./helpers/native-chain-sync.fake-transport.js";
describe("native below-intersection rollback", () => {
  it("continues after an authenticated rollback below the session intersection", async () => {
    const events: WatcherNativeChainSyncEvent[] = [];
    const runtime = await start("below_intersection", async (event) => {
      expect(watcherNativeChainSyncEventReceipt(event)).not.toBeNull();
      events.push(event);
    });
    try {
      // The intersection acknowledgement, the block, the rollback below the
      // intersection and the block on the other branch.
      await waitFor(() => events.length === 4);
      expect(events[2]).toMatchObject({
        kind: "roll_backward",
        point: { slot: "90", blockHash: "dd".repeat(32) },
      });
      expect(events[3]).toMatchObject({
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
