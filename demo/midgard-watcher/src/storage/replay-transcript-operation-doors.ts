import {
  assertWatcherReplayTranscriptClassification,
  assertWatcherReplayTranscriptCompletion,
  type WatcherReplayTranscriptClassification,
  type WatcherReplayTranscriptCompletion,
} from "./replay-transcript-completion.js";
import type { WatcherReplayTranscriptLifecyclePin } from "./replay-transcript-lifecycle.js";

export const createWatcherReplayTranscriptOperationDoors = (input: {
  readonly mutate: (
    operationDigest: string,
    change: (
      identity: readonly string[],
      pin: WatcherReplayTranscriptLifecyclePin,
      head: string,
    ) => WatcherReplayTranscriptLifecyclePin,
  ) => void;
}) => {
  const capture = (
    authority: WatcherReplayTranscriptClassification,
    started: boolean,
  ) => {
    assertWatcherReplayTranscriptClassification(authority);
    input.mutate(authority.operationDigest, (fields, pin, head) => {
      if (
        fields[0] !== authority.deploymentFingerprint ||
        fields[1] !== authority.headerHash ||
        head !== authority.headTranscriptDigest
      )
        throw new Error("replay transcript classification head differs");
      if (pin.completed_slot !== null)
        throw new Error("replay transcript completed operation cannot reopen");
      return {
        ...pin,
        classification_complete: 1,
        proof_started: started ? 1 : pin.proof_started,
      };
    });
  };
  return Object.freeze({
    completeClassification: async (
      authority: WatcherReplayTranscriptClassification,
    ): Promise<void> => capture(authority, false),
    beginProofOperation: async (
      authority: WatcherReplayTranscriptClassification,
    ): Promise<void> => capture(authority, true),
    completeOperation: async (
      completion: WatcherReplayTranscriptCompletion,
    ): Promise<void> => {
      assertWatcherReplayTranscriptCompletion(completion);
      input.mutate(completion.operationDigest, (fields, pin) => {
        if (
          fields[0] !== completion.deploymentFingerprint ||
          fields[1] !== completion.headerHash
        )
          throw new Error("replay transcript completion identity differs");
        if (BigInt(completion.completedAtSlot) < BigInt(fields[5]!))
          throw new Error("replay transcript completion precedes inclusion");
        if (
          pin.completed_slot !== null &&
          pin.completed_slot !== completion.completedAtSlot
        )
          throw new Error(
            "replay transcript completion changed canonical point",
          );
        return {
          ...pin,
          completed_slot: completion.completedAtSlot,
          proof_started: 1,
        };
      });
    },
  });
};
