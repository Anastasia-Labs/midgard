import "./user-event-history.owned-user-event-runtime-ordinary-unavailable-candidates.js";

import { describe, expect, it } from "vitest";

import {
  createWatcherLocalUserEventPublisher,
  replaceWatcherLocalUserEventPublisher,
  resumeWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import { assertWatcherLocalUserEventAuthorityCurrent } from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  createWatcherDurableRuntime,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import {
  durableFixture,
  historyLifecycle,
  historyPointerContinuation,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

describe("canonical semantic rollback replacement", () => {
  it.each([
    "admission",
    "pointer",
    "retirement",
    "withdrawal payout retirement",
  ] as const)(
    "replays %s rollback from fresh native blocks, publishes once and restores on restart",
    async (scenario) => {
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
        const kind =
          scenario === "withdrawal payout retirement"
            ? "withdrawal"
            : "deposit";
        const lifecycle = historyLifecycle(initial.facts, false, {
          kind,
          withdrawalPayout: kind === "withdrawal",
        });
        const common =
          scenario === "admission"
            ? fixture.emptySuccessorBlock
            : await fixture.makeBlock({
                parent: fixture.emptySuccessorBlock,
                transactions: [lifecycle.create],
                creatingBodies: [fixture.initializationBodyCbor],
              });
        if (scenario !== "admission") {
          const pair = await fixture.openFinalizedBlock(common);
          await publisher.publish(pair);
          await pair.close();
        }
        let operation =
          scenario === "admission" ? lifecycle.create : lifecycle.consume;
        if (scenario === "pointer")
          operation = historyPointerContinuation(
            initial.facts,
            lifecycle,
            kind,
          );
        const old = await fixture.makeBlock({
          parent: common,
          transactions: [operation],
          creatingBodies: [
            fixture.initializationBodyCbor,
            lifecycle.settlementBody,
          ],
        });
        const oldPair = await fixture.openFinalizedBlock(old);
        await publisher.publish(oldPair);
        const prior = publisher.read();
        if (kind === "withdrawal")
          expect(prior.snapshot.terminalEvents[0]).toMatchObject({
            kind: "withdrawal",
            terminalStatus: "payout_initialized",
            terminalFinalityStatus: "final",
          });
        const authorityPair = await fixture.openFinalizedBlock(old);
        const issued = await publisher.eventAuthority({
          ...authorityPair,
          kind,
          eventId: lifecycle.expectedEventId,
        });
        publisher.suspend();
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(issued),
        ).toThrow();
        const replacement = await fixture.makeBlock({
          parent: common,
          transactions: [],
          slot: Number(old.point.slot) + 1,
        });
        await fixture.selectCanonicalBranch(replacement.point);
        const fresh = await openOrigin(fixture);
        const replay = [
          fixture.emptySuccessorBlock,
          ...(scenario === "admission" ? [] : [common]),
          replacement,
        ];
        const request = {
          ...fresh.input,
          origin: fresh.origin,
          referenceAuthority: fresh.pair.referenceAuthority,
          runtime: durable.runtime,
          archive: durable.archive,
          replayCanonical: async function* () {
            for (const block of replay)
              yield await fixture.openFinalizedBlock(block);
          },
        };
        const casBefore = durable.casCount();
        const recovered = await replaceWatcherLocalUserEventPublisher(request);
        expect(durable.casCount()).toBe(casBefore + 1);
        expect(recovered.read().checkpoint).toMatchObject({
          rollbackGeneration: "1",
          predecessorCheckpointDigest: prior.checkpoint!.checkpointDigest,
          checkpointSequence: (
            BigInt(prior.checkpoint!.checkpointSequence) + 1n
          ).toString(),
        });
        expect(recovered.read().snapshot.terminalEvents).toHaveLength(0);
        expect(recovered.read().snapshot.activeEvents).toHaveLength(
          scenario === "admission" ? 0 : 1,
        );
        if (scenario !== "admission")
          expect(recovered.read().snapshot.activeEvents[0]!.outRef).toBe(
            `${lifecycle.createId}#0`,
          );
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(issued),
        ).toThrow();
        const checkpoint = recovered.read().checkpoint;
        recovered.close();
        publisher.close();
        await authorityPair.close();
        await oldPair.close();
        await initial.pair.close();
        await fresh.pair.close();
        const restarted = await createWatcherDurableRuntime(
          durable.runtimeInput,
        );
        const restartOrigin = await openOrigin(fixture);
        const resumed = await resumeWatcherLocalUserEventPublisher({
          ...restartOrigin.input,
          origin: restartOrigin.origin,
          referenceAuthority: restartOrigin.pair.referenceAuthority,
          runtime: restarted,
          archive: durable.archive,
          readHead: () => fixture.openFinalizedBlock(replacement),
        });
        expect(resumed.read().checkpoint).toEqual(checkpoint);
        expect(durable.casCount()).toBe(casBefore + 1);
        resumed.close();
        await restartOrigin.pair.close();
      } finally {
        await fixture.close();
      }
    },
    120_000,
  );

  it("requires a positive conflicting full-height branch and preserves the protected head on failed replay", async () => {
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
      const pair = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(pair);
      const checkpoint = publisher.read().checkpoint;
      const fresh = await openOrigin(fixture);
      const base = {
        ...fresh.input,
        origin: fresh.origin,
        referenceAuthority: fresh.pair.referenceAuthority,
        runtime: durable.runtime,
        archive: durable.archive,
      };
      await expect(
        replaceWatcherLocalUserEventPublisher({
          ...base,
          replayCanonical: async function* () {
            yield await fixture.openFinalizedBlock(fixture.emptySuccessorBlock);
          },
        }),
      ).rejects.toThrow("no conflicting");
      await expect(
        replaceWatcherLocalUserEventPublisher({
          ...base,
          replayCanonical: async function* () {
            yield await Promise.reject(new Error("source unavailable"));
          },
        }),
      ).rejects.toThrow("source unavailable");
      expect(
        readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        ).checkpoint,
      ).toEqual(checkpoint);
      publisher.close();
      await pair.close();
      await initial.pair.close();
      await fresh.pair.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);
});
