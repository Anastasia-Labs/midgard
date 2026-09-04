import {
  replayStateQueueAuthenticatedCheckpoints,
  type StateQueueAuthenticatedReplayCheckpoint,
  type StateQueueAuthenticatedTransition,
} from "@al-ft/midgard-sdk";

import type {
  ObservedStateQueueSnapshot,
  StateQueueHeaderRecord,
} from "../domain.js";

export type TerminalRetentionObservationConfig = {
  readonly deploymentFingerprint: string;
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly finalityDepth: number;
  /**
   * Durable queue/cursor recorded by a prior authenticated scan. Transition
   * replay is deliberately unavailable without this anchor: a standalone,
   * canonical-shaped transition is not proof that it extends our chain.
   */
  readonly replayAnchor?: {
    readonly deploymentIdentityDigest: string;
    readonly stateQueuePolicyId: string;
    readonly queue: StateQueueAuthenticatedReplayCheckpoint["previousQueue"];
    readonly blockNo: string;
    readonly transactionIndex: string;
  };
};

/**
 * Applies exact SDK-authenticated L1 transition history to retained headers.
 * The final root snapshot is only a replay consistency check; disappearance is
 * never classified as merge/removal.
 */
export const terminalRetentionOutcomes = (
  previous: readonly StateQueueHeaderRecord[],
  current: readonly StateQueueHeaderRecord[],
  checkpointInputs: readonly StateQueueAuthenticatedReplayCheckpoint[],
  snapshot: ObservedStateQueueSnapshot | undefined,
  config: TerminalRetentionObservationConfig,
): readonly StateQueueHeaderRecord[] => {
  const previousByHash = new Map(
    previous.map((record) => [record.headerHash, record]),
  );
  const currentByHash = new Map(
    current.map((record) => [record.headerHash, record]),
  );
  const terminalByHash = new Map<string, StateQueueHeaderRecord>();
  for (const record of previous) {
    if (record.deploymentFingerprint !== config.deploymentFingerprint) {
      throw new Error(
        `stored state-queue header ${record.headerHash} belongs to a foreign deployment`,
      );
    }
    if (
      !currentByHash.has(record.headerHash) &&
      (record.status === "merged" || record.status === "removed")
    ) {
      terminalByHash.set(record.headerHash, record);
    }
  }

  if (checkpointInputs.length > 0 && snapshot === undefined) {
    throw new Error(
      "state-queue checkpoint history has no final root snapshot",
    );
  }
  if (checkpointInputs.length > 0 && config.replayAnchor === undefined) {
    throw new Error(
      "state-queue checkpoint history has no durable prior queue/cursor anchor",
    );
  }
  if (
    config.replayAnchor !== undefined &&
    (config.replayAnchor.deploymentIdentityDigest !==
      config.deploymentIdentityDigest ||
      config.replayAnchor.stateQueuePolicyId !== config.stateQueuePolicyId)
  ) {
    throw new Error("state-queue durable replay anchor release mismatch");
  }
  const replay =
    config.replayAnchor === undefined || checkpointInputs.length === 0
      ? null
      : replayStateQueueAuthenticatedCheckpoints({
          deploymentIdentityDigest: config.deploymentIdentityDigest,
          stateQueuePolicyId: config.stateQueuePolicyId,
          minimumFinalityDepth: BigInt(config.finalityDepth),
          anchor: config.replayAnchor,
          checkpoints: checkpointInputs,
        });
  if (checkpointInputs.length > 0 && replay === null) {
    throw new Error(
      "state-queue checkpoint history is non-canonical or does not extend the durable cursor",
    );
  }
  const transitions: readonly StateQueueAuthenticatedTransition[] =
    replay?.terminals ?? [];
  let latestMergedHeaderHash: string | undefined;
  const seenHeaders = new Set<string>();
  for (const transition of transitions) {
    if (transition.transitionKind === "merge") {
      if (transition.removedHeaderHashes.length !== 1) {
        throw new Error("merge transition must remove exactly one header");
      }
      latestMergedHeaderHash = transition.removedHeaderHashes[0];
    }
    for (const headerHash of transition.removedHeaderHashes) {
      if (seenHeaders.has(headerHash)) {
        throw new Error("terminal transition history removes a header twice");
      }
      if (currentByHash.has(headerHash)) {
        throw new Error(
          "terminal transition header remains in the final queue",
        );
      }
      const prior = previousByHash.get(headerHash);
      if (prior !== undefined) {
        terminalByHash.set(headerHash, {
          ...prior,
          status: transition.transitionKind === "merge" ? "merged" : "removed",
          observedChainPoint: {
            slot: Number(transition.slot),
            blockHash: transition.blockHash,
            blockHeight: Number(transition.blockNo),
            depth: Number(transition.finalityDepth),
            finalized: true,
            providerSource: "authenticated_state_queue_transition_v1",
          },
          finalized: true,
          updatedAt: new Date().toISOString(),
        });
      }
      seenHeaders.add(headerHash);
    }
  }
  const expectedFinalQueue =
    snapshot === undefined
      ? undefined
      : [
          { headerHash: null, outRef: snapshot.confirmedStateOutRef },
          ...current.map(({ headerHash, stateQueueOutRef }) => ({
            headerHash,
            outRef: stateQueueOutRef,
          })),
        ];
  if (
    snapshot !== undefined &&
    ((replay !== null &&
      JSON.stringify(replay.queue) !== JSON.stringify(expectedFinalQueue)) ||
      (latestMergedHeaderHash !== undefined &&
        snapshot.confirmedHeaderHash !== latestMergedHeaderHash) ||
      (checkpointInputs.length > 0 &&
        (snapshot.observedChainPoint.depth ?? 0) < config.finalityDepth))
  ) {
    throw new Error(
      "state-queue checkpoint replay does not match the exact finalized queue/root snapshot",
    );
  }
  return [...current, ...terminalByHash.values()].sort((left, right) =>
    left.headerHash.localeCompare(right.headerHash),
  );
};
