import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  assertWatcherValidationReplayCaptureCurrent,
  captureWatcherValidationReplayTranscript,
  refreshWatcherValidationReplayCapture,
} from "../../src/fault-proofs/replay-transcript-capture.js";
import { watcherAuthenticatedReplayTranscriptCborHex } from "../../src/verification/authenticated-replay-transcript.js";
import { readWatcherReplayTranscriptRecords } from "../../src/verification/replay-transcript-records.js";
import {
  classifyValidationCapture,
  setupValidationCapture,
} from "../support/validation-capture-fixture.js";

// Retained predecessor/classifier-origin context remains the existing ordinary
// classifier fixture. The selected header is admitted from a follower store's
// state-queue observation, and the forced order is read from the same store's
// follower facts.
describe("validation transcript capture over follower user events", () => {
  it.each(["normal", "forced"] as const)(
    "captures and cold re-admits the existing %s retained Plutus decision",
    async (kind) => {
      const context = await setupValidationCapture(kind);
      try {
        const observed = await context.queue.observeFresh();
        const { userEvents } = observed.follower;
        const decision = await classifyValidationCapture(context, observed);
        const input = {
          deploymentAuthority: context.deploymentAuthority,
          stateQueueObservation: observed.observation,
          header: observed.header,
          decision,
          userEvents,
        };
        const capture = await captureWatcherValidationReplayTranscript(input);
        expect(capture.decisionDigest).toBe(decision.decisionDigest);
        expect(capture.transcript.eventAuthorityRecordsCborHex).toHaveLength(
          kind === "forced" ? 1 : 0,
        );
        const persisted = watcherAuthenticatedReplayTranscriptCborHex(
          capture.transcript,
        );
        const records = await readWatcherReplayTranscriptRecords(
          persisted,
          DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
        );
        if (kind === "forced") {
          const identity = context.queue.deployment.deploymentIdentity;
          expect(records.events[0]!.origin).toEqual({
            source: "follower_facts",
            deploymentManifestId: identity.manifestId,
            blueprintHash: identity.blueprintHash,
            throughHeader: {
              headerHash: observed.header.headerHash,
              headerCborHex: observed.header.headerCborHex,
              queueOutRef: observed.header.queueOutRef,
              observedTransactionHash: observed.header.observedTransactionHash,
              observedBlockHash: observed.header.observedBlockHash,
              observedSlot: observed.header.observedSlot,
              observedBlockNo: observed.header.observedBlockNo,
              // The order precedes the commit in the commit's block.
              transactionIndex: "1",
            },
          });
        }
        await refreshWatcherValidationReplayCapture(capture);
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent(capture),
        ).not.toThrow();
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent({ ...capture }),
        ).toThrow();
        await expect(
          captureWatcherValidationReplayTranscript({
            ...input,
            header: { ...observed.header },
          }),
        ).rejects.toThrow();
        await expect(
          captureWatcherValidationReplayTranscript({
            ...input,
            decision: { ...decision },
          }),
        ).rejects.toThrow();
        // Rewinding to the Init block removes the commit block, the cutoff.
        // The capture is retired whether or not it read any user event: the
        // header's cutoff fences it as well as each event's capability.
        await observed.follower.rewindTo(
          context.queue.initializationBlock.point,
        );
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent(capture),
        ).toThrow(/retired by an L1 rewind/u);
        await expect(
          refreshWatcherValidationReplayCapture(capture),
        ).rejects.toThrow(/retired by an L1 rewind/u);
        await observed.close();
        await expect(
          refreshWatcherValidationReplayCapture(capture),
        ).rejects.toThrow();

        const deploymentAuthority = await context.loadAuthority();
        const fresh = await context.queue.observeFresh();
        const renewedDecision = await classifyValidationCapture(
          context,
          fresh,
          deploymentAuthority,
        );
        const renewed = await captureWatcherValidationReplayTranscript({
          deploymentAuthority,
          stateQueueObservation: fresh.observation,
          header: fresh.header,
          decision: renewedDecision,
          userEvents: fresh.follower.userEvents,
          persistedTranscriptCborHex: persisted,
        });
        expect(renewed.transcript.coordinate).toEqual(
          capture.transcript.coordinate,
        );
        expect(renewed.transcript.headerHash).toBe(
          capture.transcript.headerHash,
        );
        await refreshWatcherValidationReplayCapture(renewed);
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent(renewed),
        ).not.toThrow();
        // Closing the follower source retires every capture it fenced, with
        // or without user events: its rewinds are no longer heard.
        await fresh.close();
        expect(() =>
          assertWatcherValidationReplayCaptureCurrent(renewed),
        ).toThrow(/retired by an L1 rewind/u);
        await expect(
          refreshWatcherValidationReplayCapture(renewed),
        ).rejects.toThrow(/retired by an L1 rewind/u);
      } finally {
        await context.close();
      }
    },
    180_000,
  );
});
