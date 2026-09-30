import "./user-event-history.retired-history-ids-across-durable-restart.js";

import { describe, expect, it } from "vitest";

import {
  createWatcherLocalUserEventPublisher,
  recoverWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import {
  readWatcherLocalUserEventAuthority,
  type WatcherLocalUserEventEntry,
  type WatcherUserEventObservation,
  type WatcherUserEventSnapshot,
} from "../../src/indexers/user-event-indexer.js";
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
  watcherSha256CanonicalJson,
} from "../../src/storage/durable-store.js";
import { makeWatcherUserEventCheckpoint } from "../../src/storage/user-event-checkpoint.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";
import { expectSameArchiveBytes } from "./user-event-history.native-history-retirement-frontier.js";

describe("explicit local user-event semantic recovery (synthetic local blocks)", () => {
  it("replays actual archived history across repeated durable reopen and refuses protected semantic corruption", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      const emptyPair = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(emptyPair);
      const lifecycle = historyLifecycle(facts);
      const eventBlock = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const eventPair = await fixture.openFinalizedBlock(eventBlock);
      await publisher.publish(eventPair);
      const original = publisher.read();
      const originalObjects = new Map(
        [...durable.objects].map(([digest, bytes]) => [
          digest,
          Uint8Array.from(bytes),
        ]),
      );
      await pair.close();
      await eventPair.close();
      await emptyPair.close();
      publisher.close();
      const blocks = new Map([
        [
          fixture.emptySuccessorBlock.point.blockHash,
          fixture.emptySuccessorBlock,
        ],
        [eventBlock.point.blockHash, eventBlock],
      ]);
      const requests: string[] = [];
      const replayBlock = async (point: typeof eventBlock.point) => {
        const block = blocks.get(point.blockHash);
        if (
          block === undefined ||
          !watcherSameCanonicalJson(point, block.point)
        )
          throw new Error("unexpected replay point");
        requests.push(point.blockHash);
        return await fixture.openFinalizedBlock(block);
      };
      const reopen = async () => {
        const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
        const fresh = await openOrigin(fixture);
        try {
          const resumed = await recoverWatcherLocalUserEventPublisher({
            ...fresh.input,
            origin: fresh.origin,
            referenceAuthority: fresh.pair.referenceAuthority,
            runtime,
            archive: durable.archive,
            replayBlock,
          });
          return { resumed, runtime };
        } finally {
          await fresh.pair.close();
        }
      };
      const first = await reopen();
      expect(requests).toEqual([
        fixture.emptySuccessorBlock.point.blockHash,
        eventBlock.point.blockHash,
      ]);
      expect(first.resumed.read().checkpoint!.checkpointSequence).toBe(
        (BigInt(original.checkpoint!.checkpointSequence) + 1n).toString(),
      );
      expect(first.resumed.read().snapshot.terminalEvents[0]).toMatchObject({
        eventId: lifecycle.expectedEventId,
        terminalStatus: "absorbed",
        eventCborHex: original.snapshot.terminalEvents[0]!.eventCborHex,
      });
      expect(first.resumed.read().snapshot.snapshotDigest).not.toBe(
        original.snapshot.snapshotDigest,
      );
      const firstFresh = await fixture.openFinalizedBlock(eventBlock);
      const firstAuthority = await first.resumed.eventAuthority({
        ...firstFresh,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(firstAuthority))
          .checkpointDigest,
      ).toBe(first.resumed.read().checkpoint!.checkpointDigest);
      await firstFresh.close();
      first.resumed.close();
      // A second restart reads an explicit readmission payload, with the original closure intact.
      const second = await reopen();
      expect(requests).toEqual([
        fixture.emptySuccessorBlock.point.blockHash,
        eventBlock.point.blockHash,
        fixture.emptySuccessorBlock.point.blockHash,
        eventBlock.point.blockHash,
      ]);
      const successorBlock = await fixture.makeBlock({
        parent: eventBlock,
        transactions: [],
      });
      blocks.set(successorBlock.point.blockHash, successorBlock);
      const successor = await fixture.openFinalizedBlock(successorBlock);
      await second.resumed.publish(successor);
      expect(second.resumed.read().cursor).toEqual(successorBlock.point);
      await successor.close();
      second.resumed.close();
      // An ordinary successor payload must also remain semantically restartable.
      const third = await reopen();
      expect(requests.slice(-2)).toEqual([
        eventBlock.point.blockHash,
        successorBlock.point.blockHash,
      ]);
      expect(third.resumed.read().cursor).toEqual(successorBlock.point);
      for (const [digest, bytes] of originalObjects) {
        expectSameArchiveBytes(await durable.archive.read(digest), bytes);
        expect(
          third.resumed.read().checkpoint!.requiredArchiveDigests,
        ).toContain(digest);
      }
      const finalBlock = await fixture.makeBlock({
        parent: successorBlock,
        transactions: [],
      });
      blocks.set(finalBlock.point.blockHash, finalBlock);
      const finalPair = await fixture.openFinalizedBlock(finalBlock);
      await third.resumed.publish(finalPair);
      await finalPair.close();
      third.resumed.close();
      // The real lower structural publisher can protect bytes but cannot grant
      // event semantics. Rehash every affected final-entry binding and require
      // fresh whole-block replay to detect the changed inclusion time.
      const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(third.runtime),
      );
      const checkpoint = protectedHead.checkpoint!;
      const payload: {
        head: WatcherLocalUserEventEntry;
        retainedEntries: readonly WatcherLocalUserEventEntry[];
        snapshot: WatcherUserEventSnapshot;
      } = JSON.parse(Buffer.from(protectedHead.payload!).toString("utf8"));
      let priorObservation: WatcherUserEventObservation | undefined;
      for (const bytes of durable.objects.values()) {
        const entryArchive: {
          entry?: WatcherLocalUserEventEntry;
          observation?: WatcherUserEventObservation;
        } = JSON.parse(Buffer.from(bytes).toString("utf8"));
        if (entryArchive.entry?.entryDigest === payload.head.entryDigest)
          priorObservation = entryArchive.observation;
      }
      if (priorObservation === undefined)
        throw new Error("archived final observation is absent");
      const { snapshotDigest: _snapshotDigest, ...snapshotFields } =
        payload.snapshot;
      const terminal = snapshotFields.terminalEvents[0]!;
      const badSnapshotFields = {
        ...snapshotFields,
        terminalEvents: [
          {
            ...terminal,
            inclusionTime: (BigInt(terminal.inclusionTime) + 1n).toString(),
          },
        ],
      };
      const badSnapshot = {
        ...badSnapshotFields,
        snapshotDigest: watcherSha256CanonicalJson(badSnapshotFields),
      };
      const { observationDigest: _observationDigest, ...observationFields } =
        priorObservation;
      const badObservationFields = {
        ...observationFields,
        snapshot: badSnapshot,
      };
      const badObservation = {
        ...badObservationFields,
        observationDigest: watcherSha256CanonicalJson(badObservationFields),
      };
      const { entryDigest: _entryDigest, ...entryFields } = payload.head;
      const badEntryFields = {
        ...entryFields,
        snapshotDigest: badSnapshot.snapshotDigest,
        observationDigest: badObservation.observationDigest,
      };
      const badEntry = {
        ...badEntryFields,
        entryDigest: watcherSha256CanonicalJson(badEntryFields),
      };
      const badEntryDigest = await durable.archive.put(
        Buffer.from(
          watcherCanonicalJson({
            entry: badEntry,
            observation: badObservation,
          }),
          "utf8",
        ),
      );
      const badPayloadDigest = await durable.archive.put(
        Buffer.from(
          watcherCanonicalJson({
            ...payload,
            head: badEntry,
            snapshot: badSnapshot,
            retainedEntries: [
              ...payload.retainedEntries.slice(0, -1),
              badEntry,
            ],
          }),
          "utf8",
        ),
      );
      await persistWatcherUserEventCheckpoint(third.runtime, {
        expectedCheckpointDigest: checkpoint.checkpointDigest,
        expectedCheckpointSequence: checkpoint.checkpointSequence,
        nextCheckpoint: makeWatcherUserEventCheckpoint({
          ...checkpoint,
          checkpointSequence: (
            BigInt(checkpoint.checkpointSequence) + 1n
          ).toString(),
          predecessorCheckpointDigest: checkpoint.checkpointDigest,
          payloadDigest: badPayloadDigest,
          requiredArchiveDigests: [
            ...new Set([
              ...checkpoint.requiredArchiveDigests,
              badEntryDigest,
              badPayloadDigest,
            ]),
          ].sort(),
        }),
      });
      await expect(reopen()).rejects.toThrow(
        "fresh semantic replay differs from the archived event fold",
      );
      for (const [digest, bytes] of originalObjects)
        expectSameArchiveBytes(await durable.archive.read(digest), bytes);
    } finally {
      await fixture.close();
    }
  }, 120_000);
});
