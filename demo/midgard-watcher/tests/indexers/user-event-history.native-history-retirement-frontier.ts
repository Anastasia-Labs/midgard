import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import { createWatcherLocalUserEventPublisher } from "../../src/indexers/user-event-history.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

// Vitest's structural `toEqual` walks byte arrays element by element through
// its generic iterable-equality path, which costs seconds per megabyte on
// archive objects. Compare bytes as bytes.
export const expectSameArchiveBytes = (
  actual: Uint8Array | null,
  expected: Uint8Array,
): void => {
  expect(actual).not.toBeNull();
  expect(Buffer.compare(actual!, expected), "archive bytes differ").toBe(0);
};

export const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

describe("native history retirement frontier", () => {
  it.each([
    { kind: "deposit", offset: -1n },
    { kind: "deposit", offset: 0n },
    { kind: "deposit", offset: 1n },
    { kind: "withdrawal", offset: -1n },
    { kind: "withdrawal", offset: 0n },
    { kind: "withdrawal", offset: 1n },
  ] as const)(
    "binds $kind retirement to confirmed inclusion frontier offset $offset",
    async ({ kind, offset }) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      let publisher:
        | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
        | undefined;
      try {
        const initial = await openOrigin(fixture);
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
        await initial.pair.close();
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        await publisher.publish(empty);
        await empty.close();
        const lifecycle = historyLifecycle(initial.facts, false, {
          kind,
          confirmedEndOffset: offset,
        });
        const admission = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [lifecycle.create],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        const admitted = await fixture.openFinalizedBlock(admission);
        await publisher.publish(admitted);
        await admitted.close();
        const original = publisher.read().snapshot.activeEvents[0]!;
        expect(original.kind).toBe(kind);
        const readCheckpoint = async () =>
          readWatcherProtectedUserEventCheckpointReceipt(
            await readWatcherProtectedUserEventCheckpoint(durable.runtime),
          );
        const before = await readCheckpoint();
        const casBefore = durable.casCount();
        const snapshotBefore = publisher.read().snapshot;
        const retirement = await fixture.makeBlock({
          parent: admission,
          transactions: [lifecycle.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            lifecycle.settlementBody,
          ],
        });
        const retired = await fixture.openFinalizedBlock(retirement);
        try {
          if (offset < 0n) {
            await expect(publisher.publish(retired)).rejects.toThrow(
              "whole-block event semantics differ",
            );
            const after = await readCheckpoint();
            expect(after.checkpoint).toEqual(before.checkpoint);
            expect(after.trustedHead).toEqual(before.trustedHead);
            expectSameArchiveBytes(after.payload, before.payload!);
            expect(durable.casCount()).toBe(casBefore);
            expect(publisher.read().snapshot).toEqual(snapshotBefore);
            expect(publisher.read().snapshot.activeEvents).toHaveLength(1);
            expect(publisher.read().snapshot.terminalEvents).toHaveLength(0);
          } else {
            await publisher.publish(retired);
            expect(publisher.read().snapshot.activeEvents).toHaveLength(0);
            const terminal = publisher.read().snapshot.terminalEvents;
            expect(terminal).toHaveLength(1);
            expect(terminal[0]).toMatchObject({
              eventId: original.eventId,
              eventCborHex: original.eventCborHex,
              historyPayloadCborHex: original.historyPayloadCborHex,
              inclusionTime: original.inclusionTime,
              originPointDigest: original.originPointDigest,
              terminalStatus: kind === "deposit" ? "absorbed" : "refunded",
              terminalFinalityStatus: "final",
            });
            const after = await readCheckpoint();
            expect(after.checkpoint?.checkpointSequence).toBe(
              (BigInt(before.checkpoint!.checkpointSequence) + 1n).toString(),
            );
            expect(after.checkpoint?.rollbackGeneration).toBe(
              before.checkpoint?.rollbackGeneration,
            );
            expect(durable.casCount()).toBe(casBefore + 1);
          }
        } finally {
          await retired.close();
        }
      } finally {
        publisher?.close();
        await fixture.close();
      }
    },
    120_000,
  );
});
