import { describe, expect, it } from "vitest";

import {
  startWatcherNativeChainSync,
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncRuntime,
} from "../../src/l1/native-chain-sync.js";
import { parseWatcherConfig } from "../../src/runtime/config.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

describe("native fixture initial stream acknowledgement", () => {
  it("emits the selected intersection once, ahead of every forward frame", async () => {
    const fixture = await createSyntheticUserEventOriginFixture({
      nativeTipMode: "controlled",
      nativeStreamInitialAcknowledgement: true,
    });
    let stream: WatcherNativeChainSyncRuntime | undefined;
    try {
      // The tip the helper reports is set here, from the fixture's own block
      // registry, so nothing in the expected frames is read back out of the
      // supervisor's view of the stream it is being tested on.
      const tip = {
        kind: "point" as const,
        blockHash: fixture.emptySuccessorBlock.point.blockHash,
        blockNo: fixture.emptySuccessorBlock.point.blockNo,
        slot: fixture.emptySuccessorBlock.point.slot,
      };
      await fixture.setNativeTip({
        blockHash: tip.blockHash,
        blockNo: tip.blockNo,
        slot: tip.slot,
      });

      const events: WatcherNativeChainSyncEvent[] = [];
      const intersection = {
        kind: "point" as const,
        blockHash: fixture.activationBlock.point.blockHash,
        slot: fixture.activationBlock.point.slot,
      };
      stream = await startWatcherNativeChainSync({
        watcherConfig: parseWatcherConfig(fixture.watcherConfig),
        binaryPath: fixture.nativeChainSyncBinaryPath,
        startupTimeoutMs: 10_000,
        intersection,
        onEvent: async (event) => {
          events.push(event);
        },
      });
      await expect.poll(() => events.length, { timeout: 10_000 }).toBe(2);

      expect(
        watcherNativeChainSyncAuthorityDetails(stream.authority)
          ?.selectedIntersection,
      ).toEqual(intersection);
      expect(events[0]).toEqual({
        schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
        kind: "roll_backward",
        point: intersection,
        tip,
      });
      expect(events[1]).toEqual({
        schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
        kind: "roll_forward",
        blockHash: fixture.emptySuccessorBlock.point.blockHash,
        blockType: "7",
        prevHash: fixture.emptySuccessorBlock.parentPoint.blockHash,
        slot: fixture.emptySuccessorBlock.point.slot,
        blockNo: fixture.emptySuccessorBlock.point.blockNo,
        rawBlockCbor: fixture.emptySuccessorBlock.nativeBlock.rawBlockCbor,
        tip,
      });

      // The supervisor's ordering guard admitted the acknowledgement without
      // terminating the stream: a later block still arrives, and no second
      // acknowledgement is ever forwarded.
      const appended = await fixture.appendNativeBlock({
        transactions: [],
        slot: Number(fixture.emptySuccessorBlock.point.slot) + 1200,
      });
      await expect.poll(() => events.length, { timeout: 10_000 }).toBe(3);
      expect(events[2]).toMatchObject({
        kind: "roll_forward",
        blockHash: appended.point.blockHash,
        rawBlockCbor: appended.nativeBlock.rawBlockCbor,
      });
      expect(events.map(({ kind }) => kind)).toEqual([
        "roll_backward",
        "roll_forward",
        "roll_forward",
      ]);
      expect(
        watcherNativeChainSyncAuthorityDetails(stream.authority),
      ).not.toBeNull();
    } finally {
      await stream?.close();
      await fixture.close();
    }
  });
});
