import "./user-event-history.local-deposit-transcript-semantic-renewal.js";

import { describe, expect, it } from "vitest";

import {
  createWatcherLocalUserEventPublisher,
  recoverWatcherLocalUserEventPublisher,
  resumeWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import { readWatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  createWatcherDurableRuntime,
  persistWatcherUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import {
  watcherCanonicalJson,
  watcherSameCanonicalJson,
} from "../../src/storage/durable-store.js";
import { makeWatcherUserEventCheckpoint } from "../../src/storage/user-event-checkpoint.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";
import { expectSameArchiveBytes } from "./user-event-history.native-history-retirement-frontier.js";

describe("local user-event materialized history (synthetic local blocks)", () => {
  it("rotates protected anchors beyond 128 blocks, retains old event provenance and cold replays each indexed segment once", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      let runtime = durable.runtime;
      let publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime,
        archive: durable.archive,
      });
      const blocks = [fixture.activationBlock, fixture.emptySuccessorBlock];
      await publisher.publish(pair);
      await pair.close();
      const emptyPair = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(emptyPair);
      await emptyPair.close();
      const lifecycle = historyLifecycle(facts);
      const eventBlock = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      blocks.push(eventBlock);
      let head = eventBlock;
      while (blocks.length < 128) {
        head = await fixture.makeBlock({ parent: head, transactions: [] });
        blocks.push(head);
      }
      for (const block of blocks.slice(2)) {
        const finalized = await fixture.openFinalizedBlock(block);
        try {
          await publisher.publish(finalized);
        } finally {
          await finalized.close();
        }
      }
      const before = publisher.read();
      expect(before.retainedEntries).toBe(128);
      expect(before.store.l1Observations).toHaveLength(128);
      const originalObjects = new Map(
        [...durable.objects].map(([digest, bytes]) => [
          digest,
          Uint8Array.from(bytes),
        ]),
      );
      const successorBlock = await fixture.makeBlock({
        parent: head,
        transactions: [],
      });
      blocks.push(successorBlock);
      let successor = await fixture.openFinalizedBlock(successorBlock);
      await expect(publisher.publish(successor)).rejects.toThrow(
        "semantic anchor rotation required",
      );
      const anchorPair = await fixture.openFinalizedBlock(head);
      let releaseWrite!: () => void;
      let enteredWrite!: () => void;
      const released = new Promise<void>((resolve) => {
        releaseWrite = resolve;
      });
      const entered = new Promise<void>((resolve) => {
        enteredWrite = resolve;
      });
      durable.setBeforePut(async () => {
        durable.setBeforePut(null);
        enteredWrite();
        await released;
      });
      const rotating = publisher.rotate(anchorPair);
      await entered;
      expect(publisher.read()).toMatchObject({
        status: "publication_pending",
        retainedEntries: 128,
        anchorDue: true,
        store: { revision: before.store.revision },
      });
      await expect(publisher.publish(successor)).rejects.toThrow(
        "already in flight",
      );
      durable.interruptNextReadBack();
      releaseWrite();
      await expect(rotating).rejects.toThrow();
      expect(watcherCanonicalJson(publisher.read().store)).toBe(
        watcherCanonicalJson(before.store),
      );
      await expect(publisher.rotate(anchorPair)).rejects.toThrow(
        "trusted-head read-back differs",
      );
      publisher.close();
      await anchorPair.close();
      await successor.close();
      runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const recoveryOrigin = await openOrigin(fixture);
      const recoveryRequests: string[] = [];
      const recoveryBlocks = new Map(
        blocks.map((block) => [block.point.blockHash, block]),
      );
      publisher = await recoverWatcherLocalUserEventPublisher({
        ...recoveryOrigin.input,
        origin: recoveryOrigin.origin,
        referenceAuthority: recoveryOrigin.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = recoveryBlocks.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected recovery replay point");
          recoveryRequests.push(point.blockHash);
          return await fixture.openFinalizedBlock(block);
        },
      });
      await recoveryOrigin.pair.close();
      expect(recoveryRequests).toEqual(
        blocks.slice(1, 128).map((block) => block.point.blockHash),
      );
      const anchored = publisher.read();
      expect(anchored.retainedEntries).toBe(64);
      expect(anchored.anchorDue).toBe(false);
      expect(anchored.retainedArchive.objects).toBe(
        anchored.checkpoint!.requiredArchiveDigests.length,
      );
      expect(anchored.retainedArchive.bytes).toBeGreaterThan(0);
      expect(anchored.retainedArchive.nodes).toBeGreaterThan(0);
      expect(anchored.store.l1Observations).toHaveLength(65);
      expect(anchored.snapshot.terminalEvents[0]).toMatchObject({
        eventId: lifecycle.expectedEventId,
        terminalStatus: "absorbed",
      });
      expect(anchored.snapshot.snapshotDigest).not.toBe(
        before.snapshot.snapshotDigest,
      );
      expect(anchored.store.revision).toBe(
        (BigInt(before.store.revision) + 1n).toString(),
      );
      expect(anchored.checkpoint!.requiredArchiveDigests.length).toBeLessThan(
        before.checkpoint!.requiredArchiveDigests.length,
      );
      await anchorPair.close();
      const postAnchor = await fixture.openFinalizedBlock(head);
      const authority = await publisher.eventAuthority({
        ...postAnchor,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(authority)).event,
      ).toEqual(anchored.snapshot.terminalEvents[0]);
      await postAnchor.close();
      successor = await fixture.openFinalizedBlock(successorBlock);
      await publisher.publish(successor);
      await successor.close();
      head = successorBlock;
      // Five real protected rotations exercise immediate and power-of-two ancestor links.
      for (let index = 0; index < 4; index += 1) {
        const current = await fixture.openFinalizedBlock(head);
        try {
          await publisher.rotate(current);
        } finally {
          await current.close();
        }
        if (index < 3) {
          head = await fixture.makeBlock({ parent: head, transactions: [] });
          blocks.push(head);
          const finalized = await fixture.openFinalizedBlock(head);
          try {
            await publisher.publish(finalized);
          } finally {
            await finalized.close();
          }
        }
      }
      const final = publisher.read();
      expect(final.retainedEntries).toBe(64);
      expect(final.store.l1Observations).toHaveLength(65);
      const savedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(runtime),
      );
      const savedPayload: {
        anchor: { indexDigest: string; indexSequence: string };
      } = JSON.parse(Buffer.from(savedHead.payload!).toString("utf8"));
      expect(savedPayload.anchor.indexSequence).toBe("4");
      const archivedIndex: {
        ancestorDigests: readonly string[];
        materializedStoreDigest: string;
      } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(savedPayload.anchor.indexDigest))!,
        ).toString("utf8"),
      );
      expect(archivedIndex.ancestorDigests).toHaveLength(3);
      publisher.close();
      const firstIndex: { sourcePayloadDigest: string } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(archivedIndex.ancestorDigests[2]!))!,
        ).toString("utf8"),
      );
      for (const digest of [
        savedHead.checkpoint!.payloadDigest,
        archivedIndex.materializedStoreDigest,
        firstIndex.sourcePayloadDigest,
      ]) {
        const originalBytes = durable.objects.get(digest)!;
        expect(originalBytes).toBeDefined();
        const missingOrigin = await openOrigin(fixture);
        durable.objects.delete(digest);
        try {
          await expect(
            recoverWatcherLocalUserEventPublisher({
              ...missingOrigin.input,
              origin: missingOrigin.origin,
              referenceAuthority: missingOrigin.pair.referenceAuthority,
              runtime,
              archive: durable.archive,
              replayBlock: async () => {
                throw new Error(
                  "missing archive dependency must fail before replay",
                );
              },
            }),
          ).rejects.toThrow(/absent|missing/);
        } finally {
          durable.objects.set(digest, originalBytes);
          await missingOrigin.pair.close();
        }
      }
      runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const fresh = await openOrigin(fixture);
      const requested: string[] = [];
      const byHash = new Map(
        blocks.map((block) => [block.point.blockHash, block]),
      );
      const resumed = await recoverWatcherLocalUserEventPublisher({
        ...fresh.input,
        origin: fresh.origin,
        referenceAuthority: fresh.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = byHash.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected indexed replay point");
          requested.push(point.blockHash);
          return await fixture.openFinalizedBlock(block);
        },
      });
      await fresh.pair.close();
      expect(requested).toEqual(
        blocks.slice(1).map((block) => block.point.blockHash),
      );
      expect(resumed.read()).toMatchObject({
        cursor: head.point,
        retainedEntries: 64,
        store: { revision: final.store.revision },
      });
      expect(resumed.read().store.l1Observations).toHaveLength(65);
      const freshHead = await fixture.openFinalizedBlock(head);
      const retained = await resumed.eventAuthority({
        ...freshHead,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(retained)).event,
      ).toMatchObject({
        eventId: lifecycle.expectedEventId,
        terminalStatus: "absorbed",
      });
      await freshHead.close();
      for (const [digest, bytes] of originalObjects)
        expectSameArchiveBytes(await durable.archive.read(digest), bytes);
      const nextBlock = await fixture.makeBlock({
        parent: head,
        transactions: [],
      });
      const next = await fixture.openFinalizedBlock(nextBlock);
      await resumed.publish(next);
      expect(resumed.read().cursor).toEqual(nextBlock.point);
      await next.close();
      resumed.close();
      byHash.set(nextBlock.point.blockHash, nextBlock);
      blocks.push(nextBlock);
      const secondRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const secondOrigin = await openOrigin(fixture);
      const secondRequests: string[] = [];
      const secondResumed = await resumeWatcherLocalUserEventPublisher({
        ...secondOrigin.input,
        origin: secondOrigin.origin,
        referenceAuthority: secondOrigin.pair.referenceAuthority,
        runtime: secondRuntime,
        archive: durable.archive,
        readHead: async (point) => {
          const block = byHash.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected second indexed replay point");
          secondRequests.push(point.blockHash);
          return await fixture.openFinalizedBlock(block);
        },
      });
      await secondOrigin.pair.close();
      expect(secondRequests).toEqual([nextBlock.point.blockHash]);
      expect(secondResumed.read().cursor).toEqual(nextBlock.point);
      expect(secondResumed.read().retainedEntries).toBe(65);
      secondResumed.close();
      runtime = secondRuntime;
      // A structurally protected but incorrect ancestry link grants no replay authority.
      const currentProtected = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(runtime),
      );
      const currentCheckpoint = currentProtected.checkpoint!;
      const currentPayload: {
        anchor: { indexDigest: string };
        [field: string]: unknown;
      } = JSON.parse(Buffer.from(currentProtected.payload!).toString("utf8"));
      const indexValue: {
        ancestorDigests: readonly string[];
        [field: string]: unknown;
      } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(currentPayload.anchor.indexDigest))!,
        ).toString("utf8"),
      );
      const badIndex = {
        ...indexValue,
        ancestorDigests: [
          indexValue.ancestorDigests[0]!,
          indexValue.ancestorDigests[1]!,
          indexValue.ancestorDigests[0]!,
        ],
      };
      const badIndexBytes = Buffer.from(watcherCanonicalJson(badIndex), "utf8");
      const badIndexDigest = await durable.archive.put(badIndexBytes);
      const badPayloadBytes = Buffer.from(
        watcherCanonicalJson({
          ...currentPayload,
          anchor: { ...currentPayload.anchor, indexDigest: badIndexDigest },
        }),
        "utf8",
      );
      const badPayloadDigest = await durable.archive.put(badPayloadBytes);
      const badCheckpoint = makeWatcherUserEventCheckpoint({
        ...currentCheckpoint,
        checkpointSequence: (
          BigInt(currentCheckpoint.checkpointSequence) + 1n
        ).toString(),
        predecessorCheckpointDigest: currentCheckpoint.checkpointDigest,
        payloadDigest: badPayloadDigest,
        requiredArchiveDigests: [
          ...new Set([
            ...currentCheckpoint.requiredArchiveDigests,
            badIndexDigest,
            badPayloadDigest,
          ]),
        ].sort(),
      });
      await persistWatcherUserEventCheckpoint(runtime, {
        expectedCheckpointDigest: currentCheckpoint.checkpointDigest,
        expectedCheckpointSequence: currentCheckpoint.checkpointSequence,
        nextCheckpoint: badCheckpoint,
      });
      const corruptRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const corruptOrigin = await openOrigin(fixture);
      await expect(
        recoverWatcherLocalUserEventPublisher({
          ...corruptOrigin.input,
          origin: corruptOrigin.origin,
          referenceAuthority: corruptOrigin.pair.referenceAuthority,
          runtime: corruptRuntime,
          archive: durable.archive,
          replayBlock: async () => {
            throw new Error("corrupt archive must fail before replay");
          },
        }),
      ).rejects.toThrow("ancestor sequence differs");
      await corruptOrigin.pair.close();
    } finally {
      await fixture.close();
    }
  }, 600_000);
});
