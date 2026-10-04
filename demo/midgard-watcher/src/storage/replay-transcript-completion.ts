import { verifyCompletedFraudProofWorkflow } from "@al-ft/midgard-fault-proofs";

import {
  assertWatcherValidationReplayCaptureCurrent,
  type captureWatcherValidationReplayTranscript,
} from "../fault-proofs/replay-transcript-capture.js";
import type { WatcherReplayTranscriptStore } from "./replay-transcript-store.js";

export type WatcherReplayTranscriptClassification = Readonly<{
  deploymentFingerprint: string;
  headerHash: string;
  operationDigest: string;
  headTranscriptDigest: string;
}>;
const classifications = new WeakSet<object>();
export const assertWatcherReplayTranscriptClassification = (
  value: WatcherReplayTranscriptClassification,
): void => {
  if (!classifications.has(value))
    throw new Error("replay transcript classification is not admitted");
};
const admitClassification = (value: WatcherReplayTranscriptClassification) => {
  const admitted = Object.freeze({ ...value });
  classifications.add(admitted);
  return admitted;
};
/** Completion is the privately admitted current capture, never a finished flag. */
export const watcherReplayTranscriptClassification = (
  capture: Awaited<ReturnType<typeof captureWatcherValidationReplayTranscript>>,
) => {
  assertWatcherValidationReplayCaptureCurrent(capture);
  return admitClassification({
    deploymentFingerprint: capture.transcript.deploymentFingerprint,
    headerHash: capture.transcript.headerHash,
    operationDigest: capture.decisionDigest,
    headTranscriptDigest: capture.transcript.transcriptDigest,
  });
};
export const unsafeAdmitWatcherReplayTranscriptClassificationForTest =
  admitClassification;

export type WatcherReplayTranscriptCompletion = Readonly<{
  deploymentFingerprint: string;
  headerHash: string;
  operationDigest: string;
  completedAtSlot: string;
}>;

const completions = new WeakSet<object>();
export const assertWatcherReplayTranscriptCompletion = (
  completion: WatcherReplayTranscriptCompletion,
): void => {
  if (!completions.has(completion)) {
    throw new Error("replay transcript operation completion is not admitted");
  }
};

const admit = (value: WatcherReplayTranscriptCompletion) => {
  if (
    !/^[0-9a-f]{64}$/u.test(value.deploymentFingerprint) ||
    !/^[0-9a-f]{56}$/u.test(value.headerHash) ||
    !/^[0-9a-f]{64}$/u.test(value.operationDigest) ||
    !/^(?:0|[1-9][0-9]{0,19})$/u.test(value.completedAtSlot)
  ) {
    throw new Error("replay transcript completion identity is invalid");
  }
  const completion = Object.freeze({ ...value });
  completions.add(completion);
  return completion;
};

/** Only a fresh exact-journal, canonical raw-L1 terminal can finish a pin. */
export const verifyCompletedWatcherReplayTranscriptWorkflow = async (
  input: Parameters<typeof verifyCompletedFraudProofWorkflow>[0] & {
    readonly replayTranscriptStore?: WatcherReplayTranscriptStore;
  },
) => {
  const result = await verifyCompletedFraudProofWorkflow(input);
  if (
    result.kind === "applicable" &&
    input.replayTranscriptStore !== undefined
  ) {
    await input.replayTranscriptStore.completeOperation(
      admit({
        deploymentFingerprint: input.binding.deploymentFingerprint,
        headerHash: input.binding.definition.headerHash,
        operationDigest: input.decisionDigest,
        completedAtSlot: result.terminal.observedAt.slot,
      }),
    );
  }
  return result;
};

/** Test-only seam; no production consumer uses it. */
export const unsafeAdmitWatcherReplayTranscriptCompletionForTest = admit;
