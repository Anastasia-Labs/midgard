import type { WatcherReplayTranscriptStore } from "../storage/replay-transcript-store.js";
import { decodeWatcherReplayRawRecord } from "../verification/replay-transcript-records.w25-keys.js";
import { captureWatcherValidationReplayTranscript } from "./replay-transcript-capture.js";

/**
 * The transcript schema before W3 bound user events to follower facts. Its
 * event records came from the deleted local event history, so a persisted v1
 * head cannot be replayed against current authority: it is recaptured fresh
 * (same header, same coordinate) and appended on top of the v1 head. The
 * transcript digest is local to this store; the decision is not re-made.
 *
 * The transcript digest is bound into the challenge digest, which a started
 * validation-trace workflow journals and binds its submissions to. While a
 * proof started from the v1 head is open, the head is not re-keyed: the
 * capture is held (`validation_transcript_pre_follower`) until the header
 * leaves the finalized queue.
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

export type WatcherValidationCaptureArchive =
  | Readonly<{
      kind: "captured";
      capture: Awaited<
        ReturnType<typeof captureWatcherValidationReplayTranscript>
      >;
    }>
  | Readonly<{
      /** A v1 head with an open proof: not re-keyed, the decision is held. */
      kind: "held_pre_follower";
      preFollowerTranscriptDigest: string;
      detail: string;
    }>;

/** The capture and its durable dependent-operation pin commit together. */
export const archiveWatcherValidationCapture = async (
  input: Parameters<typeof captureWatcherValidationReplayTranscript>[0] & {
    readonly replayTranscriptStore: WatcherReplayTranscriptStore;
  },
): Promise<WatcherValidationCaptureArchive> => {
  const { header, replayTranscriptStore } = input;
  const identity = {
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
  };
  const archived = await replayTranscriptStore.read(identity);
  const preFollower =
    archived !== null &&
    isPreFollowerTranscript(archived.persistedTranscriptCborHex);
  if (preFollower && (await replayTranscriptStore.proofOperationOpen(identity)))
    return Object.freeze({
      kind: "held_pre_follower",
      preFollowerTranscriptDigest: archived.headTranscriptDigest,
      detail: `validationTraceDispute/${header.headerHash}: a proof started from the pre-follower transcript ${archived.headTranscriptDigest} is open; held until the header leaves the finalized queue`,
    });
  const capture = await captureWatcherValidationReplayTranscript({
    ...input,
    ...(archived === null || preFollower
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
  return Object.freeze({ kind: "captured", capture });
};
