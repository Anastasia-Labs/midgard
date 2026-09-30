import "./user-event-history.native-withdrawal-payout-retirement.js";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createWatcherLocalUserEventPublisher,
  resumeWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import {
  assertWatcherLocalUserEventAuthorityCurrent,
  readWatcherLocalUserEventAuthority,
} from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  createWatcherDurableRuntime,
  persistWatcherUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import { makeWatcherUserEventCheckpoint } from "../../src/storage/user-event-checkpoint.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";
import { expectSameArchiveBytes } from "./user-event-history.native-history-retirement-frontier.js";

describe("retired history IDs across durable restart", () => {
  it.each(["deposit", "withdrawal"] as const)(
    "refuses a distinct native %s re-admission of a retired ID after reopening",
    async (kind) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      const pairs: Awaited<ReturnType<typeof fixture.openFinalizedBlock>>[] =
        [];
      let publisher:
        | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
        | undefined;
      try {
        const initial = await openOrigin(fixture);
        pairs.push(initial.pair);
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(initial.pair.finality).policy,
        );
        publisher = await createWatcherLocalUserEventPublisher({
          ...initial.input,
          origin: initial.origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        await publisher.publish(initial.pair);
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        pairs.push(empty);
        await publisher.publish(empty);
        const lifecycle = historyLifecycle(initial.facts, false, { kind });
        const admission = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [lifecycle.create],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        const admitted = await fixture.openFinalizedBlock(admission);
        pairs.push(admitted);
        await publisher.publish(admitted);
        expect(publisher.read().snapshot.activeEvents).toHaveLength(1);
        const retirement = await fixture.makeBlock({
          parent: admission,
          transactions: [lifecycle.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            lifecycle.settlementBody,
          ],
        });
        const retired = await fixture.openFinalizedBlock(retirement);
        pairs.push(retired);
        await publisher.publish(retired);
        const saved = publisher.read();
        expect(saved.snapshot.activeEvents).toHaveLength(0);
        expect(saved.snapshot.terminalEvents).toHaveLength(1);
        expect(saved.snapshot.terminalEvents[0]).toMatchObject({
          eventId: lifecycle.expectedEventId,
          terminalStatus: kind === "deposit" ? "absorbed" : "refunded",
          terminalFinalityStatus: "final",
        });
        publisher.close();
        publisher = undefined;
        for (const pair of pairs) await pair.close();
        pairs.length = 0;
        let runtime = await createWatcherDurableRuntime(durable.runtimeInput);
        const reopen = async () => {
          const fresh = await openOrigin(fixture);
          pairs.push(fresh.pair);
          return resumeWatcherLocalUserEventPublisher({
            ...fresh.input,
            origin: fresh.origin,
            referenceAuthority: fresh.pair.referenceAuthority,
            runtime,
            archive: durable.archive,
            readHead: async (point) => {
              expect(point).toEqual(retirement.point);
              return fixture.openFinalizedBlock(retirement);
            },
          });
        };
        const casBefore = durable.casCount();
        publisher = await reopen();
        expect(publisher.read().checkpoint).toEqual(saved.checkpoint);
        expect(publisher.read().snapshot).toEqual(saved.snapshot);
        const terminalPair = await fixture.openFinalizedBlock(retirement);
        pairs.push(terminalPair);
        const terminalAuthority = await publisher.eventAuthority({
          ...terminalPair,
          kind,
          eventId: lifecycle.expectedEventId,
        });
        expect(
          (await readWatcherLocalUserEventAuthority(terminalAuthority)).event,
        ).toEqual(saved.snapshot.terminalEvents[0]);
        const before = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(runtime),
        );

        // A different transaction/outref with the exact retired ID exercises
        // the ID guard, not a repeated transaction-hash check. This synthetic
        // native frame does not claim that Cardano permits nonce re-spending.
        const original = CML.Transaction.from_cbor_hex(lifecycle.create);
        const reusedBody = original.body();
        reusedBody.set_validity_interval_start(0n);
        const reused = CML.Transaction.new(
          reusedBody,
          original.witness_set(),
          true,
        ).to_canonical_cbor_hex();
        const reusedHash = CML.hash_transaction(reusedBody).to_hex();
        expect(reusedHash).not.toBe(lifecycle.createId);
        expect(reusedBody.outputs().get(0).to_cbor_hex()).toBe(
          original.body().outputs().get(0).to_cbor_hex(),
        );
        const duplicate = await fixture.makeBlock({
          parent: retirement,
          transactions: [reused],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        const duplicatePair = await fixture.openFinalizedBlock(duplicate);
        pairs.push(duplicatePair);
        await expect(publisher.publish(duplicatePair)).rejects.toThrow(
          "whole-block event semantics differ",
        );
        const after = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(runtime),
        );
        expect(after.checkpoint).toEqual(before.checkpoint);
        expect(after.trustedHead).toEqual(before.trustedHead);
        expectSameArchiveBytes(after.payload, before.payload!);
        expect(durable.casCount()).toBe(casBefore);
        expect(publisher.read().snapshot).toEqual(saved.snapshot);
        expect(publisher.read().cursor).toEqual(retirement.point);
        expect(
          (await readWatcherLocalUserEventAuthority(terminalAuthority)).event,
        ).toEqual(saved.snapshot.terminalEvents[0]);

        publisher.close();
        publisher = undefined;
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(terminalAuthority),
        ).toThrow();
        for (const pair of pairs) await pair.close();
        pairs.length = 0;
        runtime = await createWatcherDurableRuntime(durable.runtimeInput);
        publisher = await reopen();
        expect(publisher.read().checkpoint).toEqual(saved.checkpoint);
        expect(publisher.read().snapshot).toEqual(saved.snapshot);
        expect(publisher.read().cursor).toEqual(retirement.point);
        expect(durable.casCount()).toBe(casBefore);
      } finally {
        publisher?.close();
        await Promise.allSettled(pairs.map((pair) => pair.close()));
        await fixture.close();
      }
    },
    120_000,
  );
});

describe("durable local user-event restart", () => {
  it("restores validated events using only the current head, preserves progress, and continues incrementally", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const initial = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(initial.pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...initial.input,
        origin: initial.origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(initial.pair);
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      await empty.close();
      const lifecycle = historyLifecycle(initial.facts);
      const block = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const pair = await fixture.openFinalizedBlock(block);
      await publisher.publish(pair);
      const original = publisher.read();
      publisher.close();
      await initial.pair.close();
      await pair.close();
      const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const fresh = await openOrigin(fixture);
      const requests: string[] = [];
      const reopened = await resumeWatcherLocalUserEventPublisher({
        ...fresh.input,
        origin: fresh.origin,
        referenceAuthority: fresh.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        readHead: async (point) => {
          requests.push(point.blockHash);
          expect(point).toEqual(block.point);
          return fixture.openFinalizedBlock(block);
        },
      });
      expect(requests).toEqual([block.point.blockHash]);
      expect(reopened.read().checkpoint).toEqual(original.checkpoint);
      expect(reopened.read().snapshot).toEqual(original.snapshot);
      expect(reopened.read().store).toEqual(original.store);
      const head = await fixture.openFinalizedBlock(block);
      const authority = await reopened.eventAuthority({
        ...head,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(authority)).checkpointDigest,
      ).toBe(original.checkpoint!.checkpointDigest);
      await head.close();
      const nextBlock = await fixture.makeBlock({
        parent: block,
        transactions: [],
      });
      const nextPair = await fixture.openFinalizedBlock(nextBlock);
      await reopened.publish(nextPair);
      expect(reopened.read().cursor).toEqual(nextBlock.point);
      expect(reopened.read().checkpoint!.checkpointSequence).toBe(
        (BigInt(original.checkpoint!.checkpointSequence) + 1n).toString(),
      );
      reopened.close();
      await nextPair.close();
      // A valid pair for another block cannot corroborate the protected head.
      await expect(
        resumeWatcherLocalUserEventPublisher({
          ...fresh.input,
          origin: fresh.origin,
          referenceAuthority: fresh.pair.referenceAuthority,
          runtime,
          archive: durable.archive,
          readHead: async () => fixture.openFinalizedBlock(block),
        }),
      ).rejects.toThrow("saved head is no longer canonical");
      const current = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(runtime),
      );
      const checkpoint = current.checkpoint!;
      const nextCheckpoint = makeWatcherUserEventCheckpoint({
        ...checkpoint,
        checkpointSequence: (
          BigInt(checkpoint.checkpointSequence) + 1n
        ).toString(),
        predecessorCheckpointDigest: checkpoint.checkpointDigest,
      });
      // A caller-created object cannot stamp semantic validation.
      await expect(
        persistWatcherUserEventCheckpoint(runtime, {
          expectedCheckpointDigest: checkpoint.checkpointDigest,
          expectedCheckpointSequence: checkpoint.checkpointSequence,
          nextCheckpoint,
          validationCandidate: {},
        }),
      ).rejects.toThrow();
      await persistWatcherUserEventCheckpoint(runtime, {
        expectedCheckpointDigest: checkpoint.checkpointDigest,
        expectedCheckpointSequence: checkpoint.checkpointSequence,
        nextCheckpoint,
      });
      await expect(
        resumeWatcherLocalUserEventPublisher({
          ...fresh.input,
          origin: fresh.origin,
          referenceAuthority: fresh.pair.referenceAuthority,
          runtime,
          archive: durable.archive,
          readHead: async () => {
            throw new Error("must refuse before native reads");
          },
        }),
      ).rejects.toThrow("restart requires durable semantic validation");
      await fresh.pair.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);
});
