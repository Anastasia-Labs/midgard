import type { WatcherReplayTranscriptStore } from "../storage/replay-transcript-store.js";
import { decodeWatcherReplayRawRecord } from "../verification/replay-transcript-records.w25-keys.js";
import { captureWatcherValidationReplayTranscript } from "./replay-transcript-capture.js";

/**
 * The transcript schema before W3 bound user events to follower facts. Its
 * event records came from the deleted local event history, so a persisted v1
 * head cannot be replayed against current authority: it is recaptured fresh
 * (same header, same coordinate) and appended on top of the v1 head. The
 * transcript digest is local to this store; the decision is not re-made.
 */
const PRE_FOLLOWER_TRANSCRIPT =
  "midgard-watcher-production-authenticated-replay-transcript-v1";

const isPreFollowerTranscript = (cborHex: string): boolean => {
  const decoded = decodeWatcherReplayRawRecord(cborHex);
  return (
    typeof decoded === "object" &&
    decoded !== null &&
    (decoded as { schemaVersion?: unknown }).schemaVersion ===
      PRE_FOLLOWER_TRANSCRIPT
  );
};

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
    ...(archived === null ||
    isPreFollowerTranscript(archived.persistedTranscriptCborHex)
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
