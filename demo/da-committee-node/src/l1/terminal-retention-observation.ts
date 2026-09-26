import {
  replayStateQueueAuthenticatedCheckpoints,
  type StateQueueAuthenticatedReplayCheckpoint,
  type StateQueueAuthenticatedTransition,
} from "@al-ft/midgard-sdk";

import type {
  ObservedStateQueueSnapshot,
  StateQueueHeaderRecord,
} from "../domain.js";
import {
  L1SourceIntegrityError,
  StateQueueHistoryNotExtendingAnchorError,
} from "./source-integrity.js";

export type TerminalRetentionObservationConfig = {
  readonly deploymentFingerprint: string;
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly finalityDepth: number;
  /**
   * Queue/cursor the history extends: the durable anchor recorded by a prior
   * authenticated scan, or the scanner's not-yet-final bootstrap candidate.
   * Transition replay is deliberately unavailable without an anchor: a
   * standalone, canonical-shaped transition is not proof that it extends our
   * chain.
   */
  readonly replayAnchor?: {
    readonly deploymentIdentityDigest: string;
    readonly stateQueuePolicyId: string;
    readonly queue: StateQueueAuthenticatedReplayCheckpoint["previousQueue"];
    readonly blockNo: string;
    readonly transactionIndex: string;
  };
};

export type StateQueueReplayCursor = Readonly<{
  queue: StateQueueAuthenticatedReplayCheckpoint["previousQueue"];
  blockNo: string;
  transactionIndex: string;
}>;

/**
 * One authenticated replay checkpoint's effect on a queued header's output:
 * it spent `fromOutRef` and either recreated the header at `toOutRef` (an
 * append continuing the tail, a datum update, a correction relinking a
 * neighbour, ...) or, when `toOutRef` is absent, took the header out of the
 * queue. Derived from the checkpoint's authenticated previous and next queues,
 * whatever its kind.
 */
export type StateQueueOutputStep = Readonly<{
  fromOutRef: string;
  toOutRef?: string;
  slot: number;
  blockHash: string;
}>;

export type TerminalRetentionObservation = Readonly<{
  /** Current queue headers plus every header with a final terminal outcome. */
  records: readonly StateQueueHeaderRecord[];
  /**
   * Headers that an authenticated checkpoint younger than the finality depth
   * moved to another output or took out of the queue, and headers whose
   * final history leads to an output the snapshot does not report final.
   * Their new output or terminal outcome is not yet canonical, nor have they
   * disappeared: it is recorded on a later tick, once all of it is final.
   */
  deferredHeaderHashes: readonly string[];
  /**
   * For each header a final checkpoint moved or removed, those steps, oldest
   * first. They are the only evidence that lets a persisted decision's output
   * change.
   */
  finalSteps: ReadonlyMap<string, readonly StateQueueOutputStep[]>;
  /**
   * Queue and cursor after the final checkpoints, when at least one is final.
   * Only a final point may become the durable replay anchor: a rollback
   * shallower than the finality depth cannot undo it. It stops before the
   * first final checkpoint of a deferred header, so that header's history is
   * replayed again, whole, once it is all final: its final steps would
   * otherwise pass behind the anchor while its recorded output stays put,
   * and nothing could explain the change later.
   */
  finalAnchor?: StateQueueReplayCursor;
}>;

const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

const assertAnchorRelease = (
  config: TerminalRetentionObservationConfig,
): void => {
  if (
    config.replayAnchor !== undefined &&
    (config.replayAnchor.deploymentIdentityDigest !==
      config.deploymentIdentityDigest ||
      config.replayAnchor.stateQueuePolicyId !== config.stateQueuePolicyId)
  ) {
    throw new L1SourceIntegrityError(
      "state-queue durable replay anchor release mismatch",
    );
  }
};

/**
 * Authenticates `checkpoints` as history extending the anchor, whole at
 * depth one and its final prefix at the finality depth. History the SDK does
 * not accept, because it does not extend the anchor or because a checkpoint's
 * content is not canonical, is `StateQueueHistoryNotExtendingAnchorError`.
 */
const authenticateHistory = (
  checkpoints: readonly StateQueueAuthenticatedReplayCheckpoint[],
  config: TerminalRetentionObservationConfig,
) => {
  const replayFrom = (
    replayed: readonly StateQueueAuthenticatedReplayCheckpoint[],
    minimumFinalityDepth: bigint,
  ) =>
    config.replayAnchor === undefined || replayed.length === 0
      ? null
      : replayStateQueueAuthenticatedCheckpoints({
          deploymentIdentityDigest: config.deploymentIdentityDigest,
          stateQueuePolicyId: config.stateQueuePolicyId,
          minimumFinalityDepth,
          anchor: config.replayAnchor,
          checkpoints: replayed,
        });
  // Every checkpoint is at least one block deep, so replaying the whole
  // history at depth one checks it without requiring any of it to be final.
  const replay = replayFrom(checkpoints, 1n);
  if (checkpoints.length > 0 && replay === null) {
    throw new StateQueueHistoryNotExtendingAnchorError(
      "state-queue checkpoint history is non-canonical or does not extend the durable cursor",
    );
  }
  // A checkpoint's finality depth counts its own block, where an output's
  // depth counts only the blocks after it, so a checkpoint is final exactly
  // when the outputs it created are: it is more than the finality depth deep.
  // Depth only shrinks along ordered history, so the final checkpoints are a
  // prefix of it.
  const finalCheckpointDepth = BigInt(config.finalityDepth) + 1n;
  const firstNotFinal = checkpoints.findIndex(
    ({ finalityDepth }) =>
      !NATURAL.test(finalityDepth) ||
      BigInt(finalityDepth) < finalCheckpointDepth,
  );
  const finalCheckpoints =
    firstNotFinal < 0 ? checkpoints : checkpoints.slice(0, firstNotFinal);
  const finalReplay = replayFrom(finalCheckpoints, finalCheckpointDepth);
  if (finalCheckpoints.length > 0 && finalReplay === null) {
    throw new StateQueueHistoryNotExtendingAnchorError(
      "state-queue final checkpoint history is non-canonical or does not extend the durable cursor",
    );
  }
  return { replay, finalCheckpoints, finalReplay };
};

/**
 * What an authenticated checkpoint did to each header: the difference between
 * its previous and next queue.
 */
const stepsOf = (checkpoint: StateQueueAuthenticatedReplayCheckpoint) => {
  const next = new Map(
    checkpoint.nextQueue.map(({ headerHash, outRef }) => [headerHash, outRef]),
  );
  return checkpoint.previousQueue.flatMap(({ headerHash, outRef }) => {
    const toOutRef = next.get(headerHash);
    return headerHash === null || toOutRef === outRef
      ? []
      : [
          {
            headerHash,
            step: {
              fromOutRef: outRef,
              ...(toOutRef === undefined ? {} : { toOutRef }),
              slot: Number(checkpoint.slot),
              blockHash: checkpoint.blockHash,
            } satisfies StateQueueOutputStep,
          },
        ];
  });
};

const stepsByHeader = (
  checkpoints: readonly StateQueueAuthenticatedReplayCheckpoint[],
): Map<string, StateQueueOutputStep[]> => {
  const steps = new Map<string, StateQueueOutputStep[]>();
  for (const checkpoint of checkpoints) {
    for (const { headerHash, step } of stepsOf(checkpoint)) {
      steps.set(headerHash, [...(steps.get(headerHash) ?? []), step]);
    }
  }
  return steps;
};

/**
 * Checks the terminal transitions of authenticated history and records the
 * outcome of every header a final one took out of the queue, for headers
 * with a stored record. The first `finalTransitionCount` transitions are
 * final. Returns the header the last merge removed, if any.
 */
const applyTerminalTransitions = (
  transitions: readonly StateQueueAuthenticatedTransition[],
  finalTransitionCount: number,
  previousByHash: ReadonlyMap<string, StateQueueHeaderRecord>,
  currentByHash: ReadonlyMap<string, StateQueueHeaderRecord>,
  finalSteps: ReadonlyMap<string, readonly StateQueueOutputStep[]>,
  terminalByHash: Map<string, StateQueueHeaderRecord>,
): string | undefined => {
  let latestMergedHeaderHash: string | undefined;
  const seenHeaders = new Set<string>();
  for (const [index, transition] of transitions.entries()) {
    if (transition.transitionKind === "merge") {
      if (transition.removedHeaderHashes.length !== 1) {
        throw new L1SourceIntegrityError(
          "merge transition must remove exactly one header",
        );
      }
      latestMergedHeaderHash = transition.removedHeaderHashes[0];
    }
    for (const headerHash of transition.removedHeaderHashes) {
      if (seenHeaders.has(headerHash)) {
        throw new L1SourceIntegrityError(
          "terminal transition history removes a header twice",
        );
      }
      if (currentByHash.has(headerHash)) {
        throw new L1SourceIntegrityError(
          "terminal transition header remains in the final queue",
        );
      }
      seenHeaders.add(headerHash);
      if (index >= finalTransitionCount) {
        continue;
      }
      const prior = previousByHash.get(headerHash);
      if (prior !== undefined) {
        terminalByHash.set(headerHash, {
          ...prior,
          // The output the transition spent, which a move earlier in this
          // history may have made newer than the stored one.
          stateQueueOutRef: finalSteps.get(headerHash)!.at(-1)!.fromOutRef,
          status: transition.transitionKind === "merge" ? "merged" : "removed",
          observedChainPoint: {
            slot: Number(transition.slot),
            blockHash: transition.blockHash,
            blockHeight: Number(transition.blockNo),
            // An output's depth: the blocks after the transition's own.
            depth: Number(transition.finalityDepth) - 1,
            finalized: true,
            providerSource: "authenticated_state_queue_transition_v1",
          },
          finalized: true,
          updatedAt: new Date().toISOString(),
        });
      }
    }
  }
  return latestMergedHeaderHash;
};

const assertOwnDeployment = (
  previous: readonly StateQueueHeaderRecord[],
  config: TerminalRetentionObservationConfig,
): void => {
  for (const record of previous) {
    if (record.deploymentFingerprint !== config.deploymentFingerprint) {
      throw new L1SourceIntegrityError(
        `stored state-queue header ${record.headerHash} belongs to a foreign deployment`,
      );
    }
  }
};

/**
 * Applies exact SDK-authenticated L1 transition history to retained headers.
 * The whole history, final or not, must extend the anchor and reproduce the
 * root snapshot exactly; anything else is an integrity failure. Terminal
 * outcomes come only from the final prefix of the history. History younger
 * than the finality depth is not a failure: its terminal outcomes are
 * deferred to a later tick.
 */
export const terminalRetentionOutcomes = (
  previous: readonly StateQueueHeaderRecord[],
  current: readonly StateQueueHeaderRecord[],
  checkpointInputs: readonly StateQueueAuthenticatedReplayCheckpoint[],
  snapshot: ObservedStateQueueSnapshot | undefined,
  config: TerminalRetentionObservationConfig,
): TerminalRetentionObservation => {
  assertOwnDeployment(previous, config);
  const previousByHash = new Map(
    previous.map((record) => [record.headerHash, record]),
  );
  const currentByHash = new Map(
    current.map((record) => [record.headerHash, record]),
  );
  const terminalByHash = new Map<string, StateQueueHeaderRecord>();
  for (const record of previous) {
    if (
      !currentByHash.has(record.headerHash) &&
      (record.status === "merged" || record.status === "removed")
    ) {
      terminalByHash.set(record.headerHash, record);
    }
  }

  if (checkpointInputs.length > 0 && snapshot === undefined) {
    throw new L1SourceIntegrityError(
      "state-queue checkpoint history has no final root snapshot",
    );
  }
  if (checkpointInputs.length > 0 && config.replayAnchor === undefined) {
    throw new L1SourceIntegrityError(
      "state-queue checkpoint history has no durable prior queue/cursor anchor",
    );
  }
  assertAnchorRelease(config);
  const { replay, finalCheckpoints, finalReplay } = authenticateHistory(
    checkpointInputs,
    config,
  );
  const transitions: readonly StateQueueAuthenticatedTransition[] =
    replay?.terminals ?? [];
  const finalTransitionCount = finalReplay?.terminals.length ?? 0;
  const replayed = replay === null ? [] : checkpointInputs;
  const deferredHeaderHashes = new Set(
    replayed
      .slice(finalCheckpoints.length)
      .flatMap((checkpoint) =>
        stepsOf(checkpoint).map(({ headerHash }) => headerHash),
      ),
  );
  const finalSteps = stepsByHeader(replayed.slice(0, finalCheckpoints.length));
  // The snapshot and the history are judged at one tip, so a final step
  // always lands on a final output. Should they ever disagree, the header
  // waits like one with young history rather than being judged on the
  // disagreement.
  for (const [headerHash, steps] of finalSteps) {
    const landing = steps.at(-1)!.toOutRef;
    const record = currentByHash.get(headerHash);
    if (
      landing !== undefined &&
      record?.stateQueueOutRef === landing &&
      !record.finalized
    ) {
      deferredHeaderHashes.add(headerHash);
    }
  }
  const anchorCheckpointCount = replayed
    .slice(0, finalCheckpoints.length)
    .findIndex((checkpoint) =>
      stepsOf(checkpoint).some(({ headerHash }) =>
        deferredHeaderHashes.has(headerHash),
      ),
    );
  const anchorCheckpoint =
    finalReplay === null
      ? undefined
      : replayed[
          (anchorCheckpointCount < 0
            ? finalCheckpoints.length
            : anchorCheckpointCount) - 1
        ];
  const latestMergedHeaderHash = applyTerminalTransitions(
    transitions,
    finalTransitionCount,
    previousByHash,
    currentByHash,
    finalSteps,
    terminalByHash,
  );
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
        snapshot.confirmedHeaderHash !== latestMergedHeaderHash))
  ) {
    throw new L1SourceIntegrityError(
      "state-queue checkpoint replay does not match the exact queue/root snapshot",
    );
  }
  return {
    records: [...current, ...terminalByHash.values()].sort((left, right) =>
      left.headerHash.localeCompare(right.headerHash),
    ),
    deferredHeaderHashes: [...deferredHeaderHashes],
    finalSteps,
    ...(anchorCheckpoint === undefined
      ? {}
      : {
          finalAnchor: {
            queue: anchorCheckpoint.nextQueue,
            blockNo: anchorCheckpoint.blockNo,
            transactionIndex: anchorCheckpoint.transactionIndex,
          },
        }),
  };
};

export type StateQueueCatchUpObservation = Readonly<{
  /** Queue and cursor after the last final checkpoint walked. */
  finalAnchor: StateQueueReplayCursor;
  /**
   * For each header a final walked checkpoint moved or removed, those steps,
   * oldest first.
   */
  finalSteps: ReadonlyMap<string, readonly StateQueueOutputStep[]>;
  /** How each header a final walked checkpoint took out of the queue left. */
  terminalStatuses: ReadonlyMap<string, "merged" | "removed">;
  /** The terminal records of those headers that have a stored record. */
  terminalRecords: readonly StateQueueHeaderRecord[];
}>;

/**
 * Applies the first part of a history too long for one scan: `checkpoints`
 * extend the anchor but stop short of the snapshot. It is authenticated as
 * in `terminalRetentionOutcomes`, and only its final checkpoints are used:
 * the anchor moves past all of them. Every header they moved or removed is
 * reported with its steps, so its observation can follow it past the anchor.
 * Returns `undefined` when none of them is final, so no progress is made.
 */
export const catchUpRetentionOutcomes = (
  previous: readonly StateQueueHeaderRecord[],
  checkpoints: readonly StateQueueAuthenticatedReplayCheckpoint[],
  config: TerminalRetentionObservationConfig,
): StateQueueCatchUpObservation | undefined => {
  assertOwnDeployment(previous, config);
  if (checkpoints.length === 0 || config.replayAnchor === undefined) {
    throw new Error(
      "state-queue catch-up needs walked history and the anchor it extends",
    );
  }
  assertAnchorRelease(config);
  const { replay, finalCheckpoints, finalReplay } = authenticateHistory(
    checkpoints,
    config,
  );
  const lastFinal = finalCheckpoints.at(-1);
  if (replay === null || finalReplay === null || lastFinal === undefined) {
    return undefined;
  }
  const finalSteps = stepsByHeader(finalCheckpoints);
  const terminalByHash = new Map<string, StateQueueHeaderRecord>();
  applyTerminalTransitions(
    replay.terminals,
    finalReplay.terminals.length,
    new Map(previous.map((record) => [record.headerHash, record])),
    new Map(),
    finalSteps,
    terminalByHash,
  );
  const terminalStatuses = new Map<string, "merged" | "removed">();
  for (const transition of finalReplay.terminals) {
    for (const headerHash of transition.removedHeaderHashes) {
      terminalStatuses.set(
        headerHash,
        transition.transitionKind === "merge" ? "merged" : "removed",
      );
    }
  }
  return {
    finalAnchor: {
      queue: lastFinal.nextQueue,
      blockNo: lastFinal.blockNo,
      transactionIndex: lastFinal.transactionIndex,
    },
    finalSteps,
    terminalStatuses,
    terminalRecords: [...terminalByHash.values()],
  };
};
