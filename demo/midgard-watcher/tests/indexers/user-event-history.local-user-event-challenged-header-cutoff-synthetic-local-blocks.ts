import "./user-event-history.local-event-replay-authority-derivation.js";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createWatcherLocalUserEventPublisher,
  recoverWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import { readWatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  createWatcherDurableRuntime,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import { watcherSameCanonicalJson } from "../../src/storage/durable-store.js";
import {
  durableFixture,
  historyLifecycle,
  historyPointerContinuation,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";
import { type SyntheticUserEventBlock } from "../support/user-event-origin-fixture.js";

describe("local user-event challenged-header cutoff (synthetic local blocks)", () => {
  it.each(
    (["deposit", "withdrawal"] as const).flatMap((kind) =>
      (["before_creation", "before_pointer", "later_pointer"] as const).map(
        (order) => ({ kind, order }),
      ),
    ),
  )(
    "keeps immutable $kind admission at header cutoff after pointer movement: $order",
    async ({ kind, order }) => {
      const state: {
        lifecycle: ReturnType<typeof historyLifecycle> | null;
        pointer: string | null;
      } = { lifecycle: null, pointer: null };
      const fixture = await createSyntheticStateQueueObservationFixture({
        composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
          const origin = await openOrigin(transport);
          state.lifecycle = historyLifecycle(origin.facts, false, { kind });
          state.pointer = historyPointerContinuation(
            origin.facts,
            state.lifecycle,
            kind,
          );
          await origin.pair.close();
          const create = state.lifecycle.create;
          return {
            transactions:
              order === "before_creation"
                ? [commitTransactionCbor, create, state.pointer]
                : order === "before_pointer"
                  ? [create, commitTransactionCbor, state.pointer]
                  : [create, commitTransactionCbor],
            creatingBodies: [transport.initializationBodyCbor],
          };
        },
      });
      try {
        const lifecycle = state.lifecycle!;
        const pointer = state.pointer!;
        const head =
          order === "later_pointer"
            ? await fixture.transport.makeBlock({
                parent: fixture.commitBlock,
                transactions: [pointer],
                creatingBodies: [fixture.transport.initializationBodyCbor],
              })
            : fixture.commitBlock;
        const { pair, input, origin } = await openOrigin(fixture.transport);
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
        await pair.close();
        for (const block of [
          fixture.transport.emptySuccessorBlock,
          fixture.initializationBlock,
          fixture.commitBlock,
          ...(head === fixture.commitBlock ? [] : [head]),
        ]) {
          const finalized = await fixture.transport.openFinalizedBlock(block);
          try {
            await publisher.publish(finalized);
          } finally {
            await finalized.close();
          }
        }
        const pointerId = CML.hash_transaction(
          CML.Transaction.from_cbor_hex(pointer).body(),
        ).to_hex();
        expect(publisher.read().snapshot.activeEvents).toHaveLength(1);
        expect(publisher.read().snapshot.activeEvents[0]).toMatchObject({
          kind,
          eventId: lifecycle.expectedEventId,
          transactionHash: pointerId,
          outRef: `${pointerId}#0`,
          originBlockHash: fixture.commitBlock.point.blockHash,
        });
        const captured = await fixture.observeFresh();
        const fresh = await fixture.transport.openFinalizedBlock(head);
        try {
          const request = {
            ...fresh,
            kind,
            eventId: lifecycle.expectedEventId,
            throughHeader: captured.header,
          };
          if (order === "before_creation") {
            await expect(publisher.eventAuthority(request)).rejects.toThrow(
              "origin occurs after the challenged header",
            );
          } else {
            const receipt = await publisher.eventAuthority(request);
            const scoped = await readWatcherLocalUserEventAuthority(receipt);
            expect(scoped.event).toMatchObject({
              kind,
              eventId: lifecycle.expectedEventId,
              transactionHash: pointerId,
              outRef: `${pointerId}#0`,
              eventCborHex:
                publisher.read().snapshot.activeEvents[0]!.eventCborHex,
            });
            expect("terminalStatus" in scoped.event).toBe(false);
            expect(scoped.throughHeader).toMatchObject({
              observedBlockHash: fixture.commitBlock.point.blockHash,
              observedTransactionHash: captured.header.observedTransactionHash,
              transactionIndex: "1",
            });
          }
        } finally {
          await fresh.close();
        }
      } finally {
        await fixture.close();
      }
    },
  );

  it.each(["before_creation", "before_terminal", "after_terminal"] as const)(
    "uses actual same-block SQ/event order: %s",
    async (order) => {
      const state: { lifecycle: ReturnType<typeof historyLifecycle> | null } = {
        lifecycle: null,
      };
      const fixture = await createSyntheticStateQueueObservationFixture({
        composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
          const origin = await openOrigin(transport);
          state.lifecycle = historyLifecycle(origin.facts);
          await origin.pair.close();
          const { create, consume, settlementBody } = state.lifecycle;
          return {
            transactions:
              order === "before_creation"
                ? [commitTransactionCbor, create, consume]
                : order === "before_terminal"
                  ? [create, commitTransactionCbor, consume]
                  : [create, consume, commitTransactionCbor],
            creatingBodies: [transport.initializationBodyCbor, settlementBody],
          };
        },
      });
      try {
        const lifecycle = state.lifecycle!;
        const { pair, input, origin } = await openOrigin(fixture.transport);
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
        await pair.close();
        for (const block of [
          fixture.transport.emptySuccessorBlock,
          fixture.initializationBlock,
          fixture.commitBlock,
        ]) {
          const finalized = await fixture.transport.openFinalizedBlock(block);
          try {
            await publisher.publish(finalized);
          } finally {
            await finalized.close();
          }
        }
        const captured = await fixture.observeFresh();
        const fresh = await fixture.transport.openFinalizedBlock(
          fixture.commitBlock,
        );
        const request = {
          ...fresh,
          kind: "deposit" as const,
          eventId: lifecycle.expectedEventId,
          throughHeader: captured.header,
        };
        if (order === "before_creation")
          await expect(publisher.eventAuthority(request)).rejects.toThrow(
            "origin occurs after the challenged header",
          );
        else {
          const receipt = await publisher.eventAuthority(request);
          const scoped = await readWatcherLocalUserEventAuthority(receipt);
          expect(scoped.throughHeader).toMatchObject({
            headerHash: captured.header.headerHash,
            headerCborHex: captured.header.headerCborHex,
            observedTransactionHash: captured.header.observedTransactionHash,
            observedBlockHash: fixture.commitBlock.point.blockHash,
            transactionIndex: order === "before_terminal" ? "1" : "2",
          });
          expect(scoped.event.eventId).toBe(lifecycle.expectedEventId);
          expect("terminalStatus" in scoped.event).toBe(
            order === "after_terminal",
          );
          if (order === "after_terminal")
            expect(scoped.event).toMatchObject({ terminalStatus: "absorbed" });
          expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
          await expect(
            publisher.eventAuthority({
              ...request,
              throughHeader: { ...captured.header },
            }),
          ).rejects.toThrow("not admitted by the production source");
          await fresh.close();
          await expect(
            readWatcherLocalUserEventAuthority(receipt),
          ).rejects.toThrow();
        }
        await fresh.close();
        await captured.close();
        publisher.close();
      } finally {
        await fixture.close();
      }
    },
    120_000,
  );

  it("scopes an older sealed header without future terminal facts and retains the same cutoff through cold replay", async () => {
    const state: {
      lifecycle: ReturnType<typeof historyLifecycle> | null;
      creation: SyntheticUserEventBlock | null;
    } = { lifecycle: null, creation: null };
    const fixture = await createSyntheticStateQueueObservationFixture({
      composeCommitBlock: async ({
        transport,
        initializationBlock,
        commitTransactionCbor,
      }) => {
        const origin = await openOrigin(transport);
        state.lifecycle = historyLifecycle(origin.facts);
        await origin.pair.close();
        state.creation = await transport.makeBlock({
          parent: initializationBlock,
          transactions: [state.lifecycle.create],
          creatingBodies: [
            transport.initializationBodyCbor,
            state.lifecycle.settlementBody,
          ],
        });
        return {
          transactions: [commitTransactionCbor],
          parent: state.creation,
        };
      },
    });
    try {
      const lifecycle = state.lifecycle!;
      const terminal = await fixture.transport.makeBlock({
        parent: fixture.commitBlock,
        transactions: [lifecycle.consume],
        creatingBodies: [
          fixture.transport.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const blocks = [
        fixture.transport.activationBlock,
        fixture.transport.emptySuccessorBlock,
        fixture.initializationBlock,
        state.creation!,
        fixture.commitBlock,
        terminal,
      ];
      let head = terminal;
      for (let index = 0; index < 67; index += 1) {
        head = await fixture.transport.makeBlock({
          parent: head,
          transactions: [],
        });
        blocks.push(head);
      }
      const captured = await fixture.observeFresh();
      const { pair, input, origin } = await openOrigin(fixture.transport);
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
      await pair.close();
      for (const block of blocks.slice(1)) {
        const finalized = await fixture.transport.openFinalizedBlock(block);
        try {
          await publisher.publish(finalized);
        } finally {
          await finalized.close();
        }
      }
      const toRotate = await fixture.transport.openFinalizedBlock(head);
      await publisher.rotate(toRotate);
      await toRotate.close();
      expect(publisher.read().retainedEntries).toBe(64);
      expect(
        publisher
          .read()
          .store.chainPoints.some(
            (point) => point.blockHash === fixture.commitBlock.point.blockHash,
          ),
      ).toBe(false);
      const fresh = await fixture.transport.openFinalizedBlock(head);
      const receipt = await publisher.eventAuthority({
        ...fresh,
        kind: "deposit",
        eventId: lifecycle.expectedEventId,
        throughHeader: captured.header,
      });
      const scoped = await readWatcherLocalUserEventAuthority(receipt);
      expect(scoped.throughHeader).toMatchObject({
        headerHash: captured.header.headerHash,
        observedTransactionHash: captured.header.observedTransactionHash,
        observedBlockHash: fixture.commitBlock.point.blockHash,
        transactionIndex: "0",
      });
      expect("terminalStatus" in scoped.event).toBe(false);
      expect(scoped.historyEntryDigests).toContain(
        scoped.throughHeader!.historyEntryDigest,
      );
      expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
      const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(durable.runtime),
      );
      const payload: { anchor: { indexDigest: string } } = JSON.parse(
        Buffer.from(protectedHead.payload!).toString("utf8"),
      );
      const archiveIndex: { sourcePayloadDigest: string } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(payload.anchor.indexDigest))!,
        ).toString("utf8"),
      );
      const originalPayload = durable.objects.get(
        archiveIndex.sourcePayloadDigest,
      )!;
      durable.objects.delete(archiveIndex.sourcePayloadDigest);
      await expect(
        publisher.eventAuthority({
          ...fresh,
          kind: "deposit",
          eventId: lifecycle.expectedEventId,
          throughHeader: captured.header,
        }),
      ).rejects.toThrow("absent or corrupt");
      durable.objects.set(archiveIndex.sourcePayloadDigest, originalPayload);
      await fresh.close();
      publisher.close();
      await captured.close();
      const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const newOrigin = await openOrigin(fixture.transport);
      const byHash = new Map(
        blocks.map((block) => [block.point.blockHash, block]),
      );
      const resumed = await recoverWatcherLocalUserEventPublisher({
        ...newOrigin.input,
        origin: newOrigin.origin,
        referenceAuthority: newOrigin.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = byHash.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected cutoff replay point");
          return await fixture.transport.openFinalizedBlock(block);
        },
      });
      await newOrigin.pair.close();
      const newCaptured = await fixture.observeFresh();
      const newFresh = await fixture.transport.openFinalizedBlock(head);
      const renewed = await resumed.eventAuthority({
        ...newFresh,
        kind: "deposit",
        eventId: lifecycle.expectedEventId,
        throughHeader: newCaptured.header,
      });
      const newScoped = await readWatcherLocalUserEventAuthority(renewed);
      expect("terminalStatus" in newScoped.event).toBe(false);
      const { historyEntryDigest: oldDigest, ...oldCutoff } =
        scoped.throughHeader!;
      const { historyEntryDigest: newDigest, ...newCutoff } =
        newScoped.throughHeader!;
      expect(newCutoff).toEqual(oldCutoff);
      expect(newDigest).not.toBe(oldDigest);
      expect(newScoped.event).toMatchObject({
        eventId: scoped.event.eventId,
        eventCborHex: scoped.event.eventCborHex,
        transactionHash: scoped.event.transactionHash,
      });
      await newFresh.close();
      await newCaptured.close();
      resumed.close();
    } finally {
      await fixture.close();
    }
  }, 300_000);
});
