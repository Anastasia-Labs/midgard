import "./user-event-history.explicit-local-user-event-semantic-recovery-synthetic-local-blocks.js";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it, vi } from "vitest";

import {
  createWatcherLocalUserEventPublisher,
  recoverWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import { readWatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import { watcherSameCanonicalJson } from "../../src/storage/durable-store.js";
import {
  assertWatcherAuthenticatedReplayTranscript,
  createWatcherAuthenticatedReplayTranscript,
  replayWatcherAuthenticatedReplayTranscript,
  watcherAuthenticatedReplayTranscriptCborHex,
} from "../../src/verification/authenticated-replay-transcript.js";
import { readWatcherReplayTranscriptRecords } from "../../src/verification/replay-transcript-records.js";
import { makeLocalDepositReplayFixture } from "../support/local-event-replay-fixture.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

// Both fixture transports retain real private admission. Switching their test
// network boundaries only selects which synthetic chain answers a fresh query.
describe("local deposit transcript semantic renewal", () => {
  it("replays an ordinary deposit through fresh durable owner and actual queue admission", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    const eventNetwork = {
      fetch: globalThis.fetch,
      WebSocket: globalThis.WebSocket,
    };
    let queue:
      | Awaited<ReturnType<typeof createSyntheticStateQueueObservationFixture>>
      | undefined;
    let resumed:
      | Awaited<ReturnType<typeof recoverWatcherLocalUserEventPublisher>>
      | undefined;
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
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
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
      const firstFresh = await fixture.openFinalizedBlock(eventBlock);
      const firstAuthority = await publisher.eventAuthority({
        ...firstFresh,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      const before = await readWatcherLocalUserEventAuthority(firstAuthority);
      const replayInput = await makeLocalDepositReplayFixture(
        firstAuthority,
        fixture.deploymentIdentity.programCommitments,
      );
      queue = await createSyntheticStateQueueObservationFixture({
        header: replayInput.observation.header,
        ruleBundleCommitment: replayInput.ruleBundleCommitment,
      });
      const queueNetwork = {
        fetch: globalThis.fetch,
        WebSocket: globalThis.WebSocket,
      };
      const initialQueue = await queue.observeFresh();
      expect(queue.transport.deploymentIdentity.manifestId).toBe(
        fixture.deploymentIdentity.manifestId,
      );
      const transcriptInput = {
        deploymentIdentity: queue.transport.deploymentIdentity,
        stateQueueObservation: initialQueue.observation,
        header: initialQueue.header,
        payloadEnvelopeCbor: replayInput.payloadEnvelopeCbor,
        daProvenance: replayInput.daProvenance,
        priorState: replayInput.priorState,
        eventAuthorities: replayInput.eventAuthorities,
        ruleBundle: replayInput.ruleBundle,
        ruleBundleCommitment: replayInput.ruleBundleCommitment,
      };
      const original = await createWatcherAuthenticatedReplayTranscript({
        ...transcriptInput,
        coordinate: { domain: "transition_step", index: "0" },
      });
      assertWatcherAuthenticatedReplayTranscript(original);
      const persistedTranscriptCborHex =
        watcherAuthenticatedReplayTranscriptCborHex(original);
      await initialQueue.close();
      publisher.close();
      await pair.close();
      await empty.close();
      await eventPair.close();
      await firstFresh.close();

      vi.stubGlobal("fetch", eventNetwork.fetch);
      vi.stubGlobal("WebSocket", eventNetwork.WebSocket);
      const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const freshOrigin = await openOrigin(fixture);
      const blocks = [fixture.emptySuccessorBlock, eventBlock];
      resumed = await recoverWatcherLocalUserEventPublisher({
        ...freshOrigin.input,
        origin: freshOrigin.origin,
        referenceAuthority: freshOrigin.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = blocks.find((candidate) =>
            watcherSameCanonicalJson(candidate.point, point),
          );
          if (block === undefined)
            throw new Error("unexpected transcript replay point");
          return await fixture.openFinalizedBlock(block);
        },
      });
      const renewedPair = await fixture.openFinalizedBlock(eventBlock);
      const renewedAuthority = await resumed.eventAuthority({
        ...renewedPair,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      const after = await readWatcherLocalUserEventAuthority(renewedAuthority);
      expect(after.checkpointDigest).not.toBe(before.checkpointDigest);
      expect(after.snapshotDigest).not.toBe(before.snapshotDigest);
      expect(after.event).toMatchObject({
        eventCborHex: before.event.eventCborHex,
        outputCborHex: before.event.outputCborHex,
        originBlockHash: before.event.originBlockHash,
        originSlot: before.event.originSlot,
        originBlockNo: before.event.originBlockNo,
      });
      vi.stubGlobal("fetch", queueNetwork.fetch);
      vi.stubGlobal("WebSocket", queueNetwork.WebSocket);
      const renewedQueue = await queue.observeFresh();
      expect(renewedQueue.observation).not.toBe(initialQueue.observation);
      const authority = replayInput.eventAuthorities![0]!;
      if (authority.localUserEvent === undefined)
        throw new Error("local deposit transcript requires local authority");
      const renewedInput = {
        ...transcriptInput,
        stateQueueObservation: renewedQueue.observation,
        header: renewedQueue.header,
        daProvenance: {
          ...replayInput.daProvenance,
          sourceId: "fresh-permissionless-da-peer",
        },
        eventAuthorities: [{ ...authority, localUserEvent: renewedAuthority }],
        persistedTranscriptCborHex,
      };
      const renewed =
        await replayWatcherAuthenticatedReplayTranscript(renewedInput);
      assertWatcherAuthenticatedReplayTranscript(renewed);
      expect(renewed.transcriptDigest).not.toBe(original.transcriptDigest);
      expect(renewed.eventAuthorityRecordsCborHex).not.toEqual(
        original.eventAuthorityRecordsCborHex,
      );
      expect(renewed.blockReplayResultDigest).not.toBe(
        original.blockReplayResultDigest,
      );
      expect(renewed.payloadEnvelopeSha256).toBe(
        original.payloadEnvelopeSha256,
      );
      expect(renewed.coordinate).toEqual(original.coordinate);
      const originalRecords = await readWatcherReplayTranscriptRecords(
        persistedTranscriptCborHex,
        DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
      );
      const renewedRecords = await readWatcherReplayTranscriptRecords(
        watcherAuthenticatedReplayTranscriptCborHex(renewed),
        DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
      );
      expect(renewedRecords.blockReplay.priorStateRoot).toBe(
        originalRecords.blockReplay.priorStateRoot,
      );
      expect(renewedRecords.blockReplay.postStateRoot).toBe(
        originalRecords.blockReplay.postStateRoot,
      );
      expect(renewedRecords.blockReplay.action).toBe("accept");
      expect(renewedRecords.blockReplay.eventRoots).toMatchObject([
        { stepIndex: 0, phase: "Deposit", mutationCount: 1 },
      ]);
      expect(watcherAuthenticatedReplayTranscriptCborHex(original)).toBe(
        persistedTranscriptCborHex,
      );
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...renewedInput,
          eventAuthorities: replayInput.eventAuthorities,
        }),
      ).rejects.toThrow();
      resumed.close();
      await renewedPair.close();
      await freshOrigin.pair.close();
    } finally {
      resumed?.close();
      await queue?.close();
      await fixture.close();
    }
  }, 120_000);
});
