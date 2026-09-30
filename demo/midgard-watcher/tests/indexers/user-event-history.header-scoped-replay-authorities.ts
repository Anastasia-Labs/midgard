import "./user-event-history.local-user-event-challenged-header-cutoff-synthetic-local-blocks.js";

import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it, vi } from "vitest";

import {
  createWatcherLocalUserEventPublisher,
  recoverWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import {
  readWatcherLocalUserEventAuthority,
  type WatcherLocalUserEventHeaderCutoff,
} from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import {
  watcherSameCanonicalJson,
  watcherSha256CanonicalJson,
} from "../../src/storage/durable-store.js";
import {
  assertWatcherAuthenticatedReplayTranscript,
  createWatcherAuthenticatedReplayTranscript,
  replayWatcherAuthenticatedReplayTranscript,
  watcherAuthenticatedReplayTranscriptCborHex,
  watcherReplayRawRecordCborHex,
} from "../../src/verification/authenticated-replay-transcript.js";
import {
  evaluateWatcherBlockReplay,
  watcherBlockReplayDownstreamInputDigest,
  watcherBlockReplayEventAuthorityManifest,
} from "../../src/verification/block-replay.js";
import { readWatcherReplayTranscriptRecords } from "../../src/verification/replay-transcript-records.js";
import { makeLocalDepositReplayFixture } from "../support/local-event-replay-fixture.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";
import { ordinaryDepositReplayTemplate } from "./user-event-history.ordinary-deposit-replay-template.js";

describe("header-scoped replay authorities", () => {
  it("binds actual same-block event cutoff to W25, transcript and fresh semantic recovery", async () => {
    const template = await ordinaryDepositReplayTemplate();
    let eventId = "";
    const queue = await createSyntheticStateQueueObservationFixture({
      ...template,
      composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
        const source = await openOrigin(transport);
        try {
          const deposit = historyLifecycle(source.facts);
          eventId = deposit.expectedEventId;
          return {
            transactions: [
              deposit.create,
              commitTransactionCbor,
              deposit.consume,
            ],
            creatingBodies: [
              transport.initializationBodyCbor,
              deposit.settlementBody,
            ],
          };
        } finally {
          await source.pair.close();
        }
      },
    });
    const fixture = queue.transport;
    let publisher:
      | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
      | undefined;
    let resumed:
      | Awaited<ReturnType<typeof recoverWatcherLocalUserEventPublisher>>
      | undefined;
    try {
      const { pair, input, origin } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      const historyBlocks = [
        fixture.emptySuccessorBlock,
        queue.initializationBlock,
        queue.commitBlock,
      ];
      for (const block of historyBlocks)
        await publisher.publish(await fixture.openFinalizedBlock(block));
      expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
      const captured = await queue.observeFresh();
      const freshPair = await fixture.openFinalizedBlock(queue.commitBlock);
      const authority = await publisher.eventAuthority({
        ...freshPair,
        throughHeader: captured.header,
        kind: "deposit",
        eventId,
      });
      const local = await readWatcherLocalUserEventAuthority(authority);
      expect("terminalStatus" in local.event).toBe(false);
      expect(local.throughHeader).toMatchObject({
        headerHash: queue.headerHash,
        queueOutRef: captured.header.queueOutRef,
        transactionIndex: "1",
      });
      expect(local.historyEntryDigests).toContain(
        local.throughHeader!.historyEntryDigest,
      );
      const replay = await makeLocalDepositReplayFixture(
        authority,
        fixture.deploymentIdentity.programCommitments,
      );
      const result = await evaluateWatcherBlockReplay(replay);
      expect(result).toMatchObject({
        action: "accept",
        reasonCodes: [],
        eventRoots: [{ phase: "Deposit", mutationCount: 1 }],
      });
      expect(
        await evaluateWatcherBlockReplay({
          ...replay,
          observation: {
            ...replay.observation,
            chainPoint: {
              ...replay.observation.chainPoint,
              blockHash: h32(0xfe),
            },
          },
        }),
      ).toMatchObject({
        action: "error",
        reasonCodes: ["user_event_authority_identity_mismatch"],
      });
      const transcriptInput = {
        deploymentIdentity: fixture.deploymentIdentity,
        stateQueueObservation: captured.observation,
        header: captured.header,
        payloadEnvelopeCbor: replay.payloadEnvelopeCbor,
        daProvenance: replay.daProvenance,
        priorState: replay.priorState,
        ruleBundle: replay.ruleBundle,
        ruleBundleCommitment: replay.ruleBundleCommitment,
        eventAuthorities: replay.eventAuthorities,
      };
      const original = await createWatcherAuthenticatedReplayTranscript({
        ...transcriptInput,
        coordinate: { domain: "transition_step", index: "0" },
      });
      const persistedTranscriptCborHex =
        watcherAuthenticatedReplayTranscriptCborHex(original);
      const originalRecords = await readWatcherReplayTranscriptRecords(
        persistedTranscriptCborHex,
        DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
      );
      const originalEvent = originalRecords.events[0]!;
      if (
        originalEvent.origin.source !== "local_publication" ||
        originalEvent.origin.throughHeader === null
      )
        throw new Error("scoped transcript lacks a cutoff");
      const originalOrigin = originalEvent.origin;
      const cutoff = originalOrigin.throughHeader!;
      const rewriteCutoff = (
        changes: Partial<WatcherLocalUserEventHeaderCutoff>,
      ) => {
        const eventRecord = {
          ...originalEvent,
          origin: {
            ...originalOrigin,
            throughHeader: { ...cutoff, ...changes },
          },
        };
        const blockReplay = {
          ...originalRecords.blockReplay,
          authorityManifestDigest: watcherSha256CanonicalJson([
            watcherBlockReplayEventAuthorityManifest(eventRecord),
          ]),
        };
        const { resultDigest: _oldResult, ...replayMaterial } = {
          ...blockReplay,
          downstreamPrerequisite: {
            ...blockReplay.downstreamPrerequisite,
            inputDigest: watcherBlockReplayDownstreamInputDigest(blockReplay),
          },
        };
        const rewrittenReplay = {
          ...replayMaterial,
          resultDigest: watcherSha256CanonicalJson(replayMaterial),
        };
        const { transcriptDigest: _oldTranscript, ...transcriptMaterial } = {
          ...original,
          blockReplayRecordCborHex:
            watcherReplayRawRecordCborHex(rewrittenReplay),
          blockReplayResultDigest: rewrittenReplay.resultDigest,
          eventAuthorityRecordsCborHex: [
            watcherReplayRawRecordCborHex(eventRecord),
          ],
        };
        return watcherReplayRawRecordCborHex({
          ...transcriptMaterial,
          transcriptDigest:
            computeDeploymentManifestJsonDigest(transcriptMaterial),
        });
      };
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...transcriptInput,
          persistedTranscriptCborHex: rewriteCutoff({
            historyEntryDigest: h32(0xfd),
          }),
        }),
      ).rejects.toThrow("event cutoff history membership");
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...transcriptInput,
          persistedTranscriptCborHex: rewriteCutoff({
            queueOutRef: `${h32(0xfc)}#0`,
          }),
        }),
      ).rejects.toThrow("event cutoff queueOutRef");
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...transcriptInput,
          persistedTranscriptCborHex: rewriteCutoff({ transactionIndex: "2" }),
        }),
      ).rejects.toThrow("differs from fresh authenticated replay semantics");
      publisher.close();
      await captured.close();
      await freshPair.close();
      const newRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const freshOrigin = await openOrigin(fixture);
      resumed = await recoverWatcherLocalUserEventPublisher({
        ...freshOrigin.input,
        origin: freshOrigin.origin,
        referenceAuthority: freshOrigin.pair.referenceAuthority,
        runtime: newRuntime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = historyBlocks.find((candidate) =>
            watcherSameCanonicalJson(candidate.point, point),
          );
          if (block === undefined)
            throw new Error("unexpected cutoff replay point");
          return await fixture.openFinalizedBlock(block);
        },
      });
      const renewedCapture = await queue.observeFresh();
      const renewedAuthority = await resumed.eventAuthority({
        ...(await fixture.openFinalizedBlock(queue.commitBlock)),
        throughHeader: renewedCapture.header,
        kind: "deposit",
        eventId,
      });
      const renewedLocal =
        await readWatcherLocalUserEventAuthority(renewedAuthority);
      expect(renewedLocal.throughHeader).toMatchObject({
        headerHash: cutoff.headerHash,
        headerCborHex: cutoff.headerCborHex,
        queueOutRef: cutoff.queueOutRef,
        observedTransactionHash: cutoff.observedTransactionHash,
        observedBlockHash: cutoff.observedBlockHash,
        observedSlot: cutoff.observedSlot,
        observedBlockNo: cutoff.observedBlockNo,
        transactionIndex: cutoff.transactionIndex,
      });
      expect(renewedLocal.throughHeader!.historyEntryDigest).not.toBe(
        cutoff.historyEntryDigest,
      );
      const eventAuthority = replay.eventAuthorities![0]!;
      if (eventAuthority.localUserEvent === undefined)
        throw new Error("scoped replay requires local capability");
      const renewed = await replayWatcherAuthenticatedReplayTranscript({
        ...transcriptInput,
        stateQueueObservation: renewedCapture.observation,
        header: renewedCapture.header,
        eventAuthorities: [
          { ...eventAuthority, localUserEvent: renewedAuthority },
        ],
        persistedTranscriptCborHex,
      });
      assertWatcherAuthenticatedReplayTranscript(renewed);
      expect(renewed.transcriptDigest).not.toBe(original.transcriptDigest);
      expect(watcherAuthenticatedReplayTranscriptCborHex(original)).toBe(
        persistedTranscriptCborHex,
      );
      // A second actual queue contains the same header at another native point.
      const network = {
        fetch: globalThis.fetch,
        WebSocket: globalThis.WebSocket,
      };
      const otherQueue =
        await createSyntheticStateQueueObservationFixture(template);
      try {
        const other = await otherQueue.observeFresh();
        expect(other.header.headerHash).toBe(renewedCapture.header.headerHash);
        expect(other.header.observedBlockHash).not.toBe(
          renewedCapture.header.observedBlockHash,
        );
        await expect(
          createWatcherAuthenticatedReplayTranscript({
            ...transcriptInput,
            stateQueueObservation: other.observation,
            header: other.header,
            eventAuthorities: [
              { ...eventAuthority, localUserEvent: renewedAuthority },
            ],
            coordinate: { domain: "transition_step", index: "0" },
          }),
        ).rejects.toThrow();
      } finally {
        await otherQueue.close();
        vi.stubGlobal("fetch", network.fetch);
        vi.stubGlobal("WebSocket", network.WebSocket);
      }
    } finally {
      publisher?.close();
      resumed?.close();
      await queue.close();
    }
  }, 120_000);
});
