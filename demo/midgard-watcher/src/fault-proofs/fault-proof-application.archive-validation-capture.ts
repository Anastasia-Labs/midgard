import type { WatcherReplayTranscriptStore } from "../storage/replay-transcript-store.js";
import { captureWatcherValidationReplayTranscript } from "./replay-transcript-capture.js";

/** The capture and its durable dependent-operation pin commit together. */
export const archiveWatcherValidationCapture = async (
  input: Parameters<typeof captureWatcherValidationReplayTranscript>[0] & {
    readonly replayTranscriptStore: WatcherReplayTranscriptStore;
  },
) => {
  const { header, replayTranscriptStore } = input;
  const archived = await replayTranscriptStore.read({
    deploymentFingerprint:
      input.deploymentAuthority.deploymentIdentity.manifestId,
    headerHash: header.headerHash,
    inclusionPoint: {
      transactionHash: header.observedTransactionHash,
      blockHash: header.observedBlockHash,
      blockNo: header.observedBlockNo,
      slot: header.observedSlot,
      chainPointId: header.observedChainPointId,
    },
  });
  const capture = await captureWatcherValidationReplayTranscript({
    ...input,
    ...(archived === null
      ? {}
      : { persistedTranscriptCborHex: archived.persistedTranscriptCborHex }),
  });
  if (
    !(await replayTranscriptStore.compareAndSwap({
      expectedTranscriptDigest: archived?.headTranscriptDigest ?? null,
      transcript: capture.transcript,
      lifecycle: {
        header,
        operationDigest: input.decision.decisionDigest,
        retentionWindow: input.deploymentAuthority.retentionWindow,
        deploymentIdentity: input.deploymentAuthority.deploymentIdentity,
      },
    }))
  ) {
    throw new Error(
      "validation transcript head changed during capture; classify again",
    );
  }
  return capture;
};
