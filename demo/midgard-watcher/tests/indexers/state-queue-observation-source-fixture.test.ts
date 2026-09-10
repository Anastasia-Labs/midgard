import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import { createWatcherLocalKupmiosNativeObservationRuntime } from "../../src/l1/local-kupmios-native-observation.js";
import { createWatcherLocalKupmiosRawSource } from "../../src/l1/local-kupmios-raw-source.js";
import { readWatcherNativeChainSyncEventReceipt } from "../../src/l1/native-chain-sync.js";
import {
  createSyntheticStateQueueHeader,
  createSyntheticStateQueueObservationFixture,
} from "../support/state-queue-observation-fixture.js";

// Synthetic transport fixture coverage; no ledger execution or public-chain claim.
describe("real state-queue observation source with synthetic local transports", () => {
  it("admits ordinary Init and Commit and freshly reobserves the same inclusion", async () => {
    const header = {
      ...createSyntheticStateQueueHeader(),
      utxosRoot: "19".repeat(32),
    };
    const headerSnapshot = { ...header };
    const pendingFixture = createSyntheticStateQueueObservationFixture({
      header,
    });
    header.utxosRoot = "20".repeat(32);
    const fixture = await pendingFixture;
    expect(fixture.header).toEqual(headerSnapshot);
    try {
      const first = await fixture.observeFresh();
      assertWatcherStateQueueObservation(first.initialObservation);
      assertWatcherStateQueueObservation(first.observation);
      assertWatcherStateQueueHeaderObservation(first.header);
      expect(first.initialObservation.finalizedHeaders).toHaveLength(0);
      expect(first.observation.finalizedHeaders).toHaveLength(1);
      expect(first.header.headerCborHex).toBe(
        Data.to(headerSnapshot, SDK.Header),
      );
      expect(first.header.headerHash).toBe(fixture.headerHash);
      expect(first.header.observedBlockHash).toBe(
        fixture.commitBlock.point.blockHash,
      );
      expect(first.observation.finalizedCorrectionLock?.datum).toBe("Idle");
      expect(() =>
        assertWatcherStateQueueObservation({ ...first.observation }),
      ).toThrow();
      expect(() =>
        assertWatcherStateQueueHeaderObservation({ ...first.header }),
      ).toThrow();
      const nativeAuthority = readWatcherNativeChainSyncEventReceipt(
        first.nativeEventReceipt,
      ).authority;
      const watcherConfig = fixture.transport.watcherConfig;
      const foreignSource = createWatcherLocalKupmiosRawSource({
        watcherConfig: {
          ...watcherConfig,
          l1: {
            ...watcherConfig.l1,
            source: {
              ...watcherConfig.l1.source,
              authorityNodeId: "other-local-node",
            },
          },
        },
        deploymentIdentity: fixture.transport.deploymentIdentity,
      });
      for (const rawSource of [
        { ...first.localRuntime.rawSource },
        foreignSource,
      ]) {
        await expect(
          createWatcherLocalKupmiosNativeObservationRuntime({
            watcherConfig,
            deploymentIdentity: fixture.transport.deploymentIdentity,
            nativeAuthority,
            rawSource,
          }),
        ).rejects.toThrow(
          "raw source differs from the admitted native topology",
        );
      }
      await first.close();
      await expect(
        first.stateQueueSource.observe({
          nativeBlock: first.nativeBlock,
          localObservation: first.localObservation,
          previous: first.initialObservation,
        }),
      ).rejects.toThrow();
      const second = await fixture.observeFresh();
      expect(second.header).toEqual(first.header);
      expect(second.header.finalityDepth).toBe("30");
      expect(second.observation.observationDigest).toBe(
        first.observation.observationDigest,
      );
      expect(
        BigInt(second.localObservation.block.chainPoint.depth),
      ).toBeGreaterThan(BigInt(first.localObservation.block.chainPoint.depth));
      expect(second.observation).not.toBe(first.observation);
      expect(second.localObservation).not.toBe(first.localObservation);
      expect(second.nativeEventReceipt).not.toBe(first.nativeEventReceipt);
      await second.close();
    } finally {
      await fixture.close();
    }
  }, 60_000);
});
