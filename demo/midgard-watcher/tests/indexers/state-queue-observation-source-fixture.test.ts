import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import {
  assertWatcherLocalKupmiosNativeObservation,
  createWatcherLocalKupmiosNativeObservationRuntime,
  guardWatcherLocalKupmiosNativeObservation,
} from "../../src/l1/local-kupmios-native-observation.js";
import { createWatcherLocalKupmiosRawSource } from "../../src/l1/local-kupmios-raw-source.js";
import { readWatcherNativeChainSyncEventReceipt } from "../../src/l1/native-chain-sync.js";
import { createWatcherResolvedBlockObservationSource } from "../../src/l1/resolved-block-observation.js";
import {
  type CapturedWatcherObservation,
  observeCapturedBlock,
} from "../../src/runtime/chain-coordinator.observe-captured-block.js";
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
    const fixtureFetch = globalThis.fetch;
    // Kupo's head stays inside the security window above the observed blocks,
    // so every checkpoint is young enough to be re-read instead of memoized.
    const youngHead = (offset: bigint) =>
      (
        BigInt(fixture.transport.emptySuccessorBlock.point.slot) + offset
      ).toString();
    let checkpointSlot = youngHead(10_000n);
    let changeDuringCapture = false;
    let changeOnceDuringCapture = false;
    let checkpointReads = 0;
    vi.stubGlobal("fetch", async (...args: Parameters<typeof fetch>) => {
      const response = await fixtureFetch(...args);
      if (response.headers.has("X-Most-Recent-Checkpoint")) {
        checkpointReads += 1;
        if (changeDuringCapture)
          checkpointSlot = (BigInt(checkpointSlot) + 1n).toString();
        if (changeOnceDuringCapture && checkpointReads === 2) {
          checkpointSlot = (BigInt(checkpointSlot) + 1n).toString();
          changeOnceDuringCapture = false;
        }
        response.headers.set("X-Most-Recent-Checkpoint", checkpointSlot);
      }
      return response;
    });
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
      // A later observation must acquire its own provider snapshot, even if
      // the shared queue source last captured a much older indexer head.
      checkpointSlot = youngHead(20_000n);
      const advancedObservation = await first.localRuntime.observe({
        block: first.nativeBlock,
        depth: first.localObservation.block.chainPoint.depth,
      });
      assertWatcherLocalKupmiosNativeObservation(
        advancedObservation,
        first.nativeBlock,
      );
      changeDuringCapture = true;
      checkpointReads = 0;
      await expect(
        first.localRuntime.observe({
          block: first.nativeBlock,
          depth: first.localObservation.block.chainPoint.depth,
        }),
      ).rejects.toThrow(
        "Kupo advanced or rolled back during raw snapshot capture",
      );
      // The pinned head refuses the first response after the change; the
      // remaining reads were already in flight. One lookup fewer than before
      // the source began retaining data it had already read.
      expect(checkpointReads).toBe(5);
      changeDuringCapture = false;
      changeOnceDuringCapture = true;
      checkpointReads = 0;
      const refreshedQueue = await first.stateQueueSource.bootstrap();
      expect(refreshedQueue.previous.finalizedHeaders).toEqual(
        first.observation.finalizedHeaders,
      );
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
      expect(second.header.finalityDepth).toBe(
        DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth.toString(),
      );
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

  it("preserves real native admission through fresh, cached and deeper coordinator captures", async () => {
    const fixture = await createSyntheticStateQueueObservationFixture();
    try {
      const first = await fixture.observeFresh();
      const firstEvent = readWatcherNativeChainSyncEventReceipt(
        first.nativeEventReceipt,
      ).event;
      if (firstEvent.kind !== "roll_forward" || firstEvent.tip.kind !== "point")
        throw new Error("Expected the actual Commit roll-forward receipt");
      let generation = 0;
      let stopped = false;
      const read = vi.fn(
        (input: Parameters<typeof first.localRuntime.observe>[0]) =>
          first.localRuntime.observe(input),
      );
      const capture = observeCapturedBlock({
        captured: new Map<string, CapturedWatcherObservation>(),
        observation: { ...first.localRuntime, observe: read },
        generation: () => generation,
        stopped: () => stopped,
      });
      const resolvedSource = createWatcherResolvedBlockObservationSource({
        deploymentIdentity: fixture.transport.deploymentIdentity,
        rawSource: first.localRuntime.rawSource,
      });
      const fresh = await capture(first.nativeBlock, firstEvent);
      assertWatcherLocalKupmiosNativeObservation(fresh, first.nativeBlock);
      await expect(
        resolvedSource.observe({
          nativeBlock: first.nativeBlock,
          localObservation: fresh,
        }),
      ).resolves.toBeDefined();
      const cached = await capture(first.nativeBlock, firstEvent);
      assertWatcherLocalKupmiosNativeObservation(cached, first.nativeBlock);
      await expect(
        resolvedSource.observe({
          nativeBlock: first.nativeBlock,
          localObservation: cached,
        }),
      ).resolves.toBeDefined();
      expect(read).toHaveBeenCalledTimes(1);
      expect(cached.block).toBe(fresh.block);

      // This fixture issues another real native query at its advanced synthetic
      // canonical tip; the receipt supplies the changed depth, not a scalar patch.
      const later = await fixture.observeFresh();
      const laterEvent = readWatcherNativeChainSyncEventReceipt(
        later.nativeEventReceipt,
      ).event;
      if (laterEvent.kind !== "roll_forward" || laterEvent.tip.kind !== "point")
        throw new Error(
          "Expected the later actual Commit roll-forward receipt",
        );
      expect(BigInt(laterEvent.tip.blockNo)).toBeGreaterThan(
        BigInt(firstEvent.tip.blockNo),
      );
      const deeper = await capture(first.nativeBlock, laterEvent);
      expect(read).toHaveBeenCalledTimes(2);
      expect(BigInt(deeper.block.chainPoint.depth)).toBeGreaterThan(
        BigInt(fresh.block.chainPoint.depth),
      );
      assertWatcherLocalKupmiosNativeObservation(deeper, first.nativeBlock);
      await expect(
        resolvedSource.observe({
          nativeBlock: first.nativeBlock,
          localObservation: deeper,
        }),
      ).resolves.toBeDefined();

      const scope = SDK.createDaAvailabilityReadScope({
        attemptTimeoutMs: 10_000,
      });
      const scopedSource = createWatcherResolvedBlockObservationSource({
        deploymentIdentity: fixture.transport.deploymentIdentity,
        rawSource: first.localRuntime.rawSource,
        assertCurrent: scope.assertCurrent,
      });
      try {
        await expect(
          scopedSource.observe({
            nativeBlock: first.nativeBlock,
            localObservation: cached,
          }),
        ).resolves.toBeDefined();
      } finally {
        scope.close();
      }
      await expect(
        scopedSource.observe({
          nativeBlock: first.nativeBlock,
          localObservation: cached,
        }),
      ).rejects.toThrow("Availability read scope closed");

      for (const candidate of [fresh, cached, deeper]) {
        expect(() =>
          assertWatcherLocalKupmiosNativeObservation(
            { ...candidate },
            first.nativeBlock,
          ),
        ).toThrow("is not admitted for the native block");
        expect(() =>
          assertWatcherLocalKupmiosNativeObservation(candidate, {
            ...first.nativeBlock,
            rawBlockCbor: "80",
          }),
        ).toThrow("is not admitted for the native block");
      }
      expect(() =>
        guardWatcherLocalKupmiosNativeObservation({
          observation: { ...first.localObservation },
          nativeBlock: first.nativeBlock,
          assertCurrent: () => undefined,
        }),
      ).toThrow("is not admitted for the native block");

      const child = guardWatcherLocalKupmiosNativeObservation({
        observation: cached,
        nativeBlock: first.nativeBlock,
        assertCurrent: () => undefined,
      });
      generation += 1;
      for (const candidate of [fresh, cached, deeper, child])
        expect(() =>
          assertWatcherLocalKupmiosNativeObservation(
            candidate,
            first.nativeBlock,
          ),
        ).toThrow("native observation generation changed");
      await expect(
        resolvedSource.observe({
          nativeBlock: first.nativeBlock,
          localObservation: child,
        }),
      ).rejects.toThrow("native observation generation changed");
      await expect(capture(first.nativeBlock, laterEvent)).rejects.toThrow(
        "native observation generation changed",
      );
      const afterRollback = observeCapturedBlock({
        captured: new Map<string, CapturedWatcherObservation>(),
        observation: first.localRuntime,
        generation: () => generation,
        stopped: () => stopped,
      });
      const live = await afterRollback(first.nativeBlock, laterEvent);
      stopped = true;
      expect(() =>
        assertWatcherLocalKupmiosNativeObservation(live, first.nativeBlock),
      ).toThrow("native observation generation changed");
      stopped = false;
      first.localRuntime.close();
      expect(() =>
        assertWatcherLocalKupmiosNativeObservation(live, first.nativeBlock),
      ).toThrow("native source attestation expired");
    } finally {
      await fixture.close();
    }
  }, 60_000);
});
