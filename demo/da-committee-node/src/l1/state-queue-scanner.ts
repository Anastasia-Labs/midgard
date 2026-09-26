import {
  isMidgardConsensusProfile,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import type {
  ObservedStateQueueNode,
  ObservedStateQueueSnapshot,
  StateQueueHeaderRecord,
} from "../domain.js";
import { bytesToHex, normalizeHex } from "../utils/hex.js";
import { classifyDaAttestationMarker } from "./attestation-marker.js";
import type { ChainSyncCursor, ChainSyncEvent } from "./provider.js";
import {
  L1SourceIntegrityError,
  StateQueueHistoryNotExtendingAnchorError,
} from "./source-integrity.js";
import {
  catchUpRetentionOutcomes,
  type StateQueueCatchUpObservation,
  type TerminalRetentionObservation,
  terminalRetentionOutcomes,
} from "./terminal-retention-observation.js";

export interface StateQueueProvider {
  fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]>;
  fetchStateQueueSnapshot?(): Promise<ObservedStateQueueSnapshot>;
  /**
   * Authenticated history from `anchor` toward `current`, each checkpoint's
   * finality depth counted from `tipBlockNo`: the tip the snapshot holding
   * `current` was read at, so it is judged at the same tip as that snapshot.
   * It stops at `current` or after `limit` checkpoints, whichever is first;
   * a history cut short by `limit` is resumed from its final part.
   */
  fetchStateQueueReplayCheckpoints?(
    anchor: readonly SDK.StateQueueTransitionNode[],
    current: readonly SDK.StateQueueTransitionNode[],
    tipBlockNo: number,
    limit: number,
  ): Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpoint[]>;
  currentChainSyncCursor?(): Promise<ChainSyncCursor>;
  replayChainSyncEvents?(
    afterSequence: number,
  ): Promise<readonly ChainSyncEvent[]>;
}

/** State-queue checkpoints one L1 block can hold, at most, in practice. */
const STATE_QUEUE_REPLAY_CHECKPOINTS_PER_BLOCK = 16;

/**
 * Checkpoints one scan replays at most: a history twice the finality window
 * long, at a busy rate. A longer history, left by downtime or by an anchor
 * held back, is walked over several scans, each moving the anchor past the
 * final part of what it walked.
 */
export const stateQueueReplayWalkLimit = (finalityDepth: number): number =>
  2 * (finalityDepth + 1) * STATE_QUEUE_REPLAY_CHECKPOINTS_PER_BLOCK;

/**
 * What a scan whose history was too long to replay whole did: it moved the
 * durable anchor past the final checkpoints it walked, and made no other
 * observation. The next scan resumes from `anchor`.
 */
export type StateQueueCatchUp = Readonly<{
  anchor: StateQueueReplayAnchor;
  walkedCheckpoints: number;
  /**
   * The queue's headers as the scan's snapshot shows them. Replay carries
   * outputs, not datums: a header's status at an output the snapshot shows
   * it at is the one thing a catch-up can know about that output's datum.
   */
  snapshotRecords: readonly StateQueueHeaderRecord[];
}> &
  Pick<
    StateQueueCatchUpObservation,
    "finalSteps" | "terminalStatuses" | "terminalRecords"
  >;

export type StateQueueReplayAnchor = Readonly<{
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  queue: readonly SDK.StateQueueTransitionNode[];
  blockNo: string;
  transactionIndex: string;
}>;

export type StateQueueScanConfig = {
  readonly deploymentFingerprint: string;
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly daAttestationPolicyId: string;
  readonly finalityDepth: number;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly previousHeaders?: readonly StateQueueHeaderRecord[];
  /** The durable replay anchor recorded by a prior scan; always final. */
  readonly terminalReplayAnchor?: StateQueueReplayAnchor;
  /**
   * A not-yet-final bootstrap candidate from a prior scan, used only while
   * there is no durable anchor. It is never persisted: a rollback can undo
   * it, and then the scan discards it and bootstraps again.
   */
  readonly provisionalReplayAnchor?: StateQueueReplayAnchor;
  /**
   * Receives the durable replay anchor after this scan. Only a queue that is
   * final is ever recorded, so a rollback shallower than the finality depth
   * can never leave the anchor naming outputs that no longer exist.
   */
  readonly recordReplayAnchor?: (anchor: StateQueueReplayAnchor) => void;
  /**
   * Called when the scan discards the bootstrap candidate because
   * authenticated history does not extend it, with that failure's message.
   */
  readonly recordDiscardedReplayAnchorCandidate?: (
    candidate: StateQueueReplayAnchor,
    reason: string,
  ) => void;
  /** Receives the bootstrap candidate to hold for the next scan, if any. */
  readonly recordProvisionalReplayAnchor?: (
    anchor: StateQueueReplayAnchor | undefined,
  ) => void;
  /**
   * Receives what authenticated replay did to queued headers: the headers a
   * not-yet-final checkpoint moved or took out of the queue, whose new
   * output or outcome is deferred until that checkpoint is final, and the
   * final steps that explain every other output change.
   */
  readonly recordReplayedHeaderSteps?: (
    replayed: Pick<
      TerminalRetentionObservation,
      "deferredHeaderHashes" | "finalSteps"
    >,
  ) => void;
  /**
   * Receives the retention exemption sets of this scan's L1 snapshot: the
   * `ConfirmedState` header hash and the hash of every live queue header.
   * Called only when the provider returned a full state-queue snapshot.
   */
  readonly recordL1View?: (view: StateQueueL1View) => void;
  /**
   * Receives the progress of a scan whose history was longer than one scan
   * replays. Such a scan records nothing else and returns no records: the
   * snapshot is not reached, so nothing about it is observed.
   */
  readonly recordCatchUp?: (catchUp: StateQueueCatchUp) => void;
};

export type StateQueueL1View = Readonly<{
  confirmedHeaderHash: string;
  liveQueueHeaderHashes: readonly string[];
}>;

export const scanStateQueue = async (
  provider: StateQueueProvider,
  config: StateQueueScanConfig,
): Promise<readonly StateQueueHeaderRecord[]> => {
  const snapshot =
    provider.fetchStateQueueSnapshot === undefined
      ? undefined
      : await provider.fetchStateQueueSnapshot();
  const nodes = snapshot?.nodes ?? (await provider.fetchStateQueueNodes());
  const current = nodes
    .filter((node) => node.linkedListKey !== "Empty")
    .map((node) => validateObservedNode(node, config));
  const finalQueue =
    snapshot === undefined
      ? []
      : [
          { headerHash: null, outRef: snapshot.confirmedStateOutRef },
          ...current.map(({ headerHash, stateQueueOutRef }) => ({
            headerHash,
            outRef: stateQueueOutRef,
          })),
        ];
  const observationConfig = {
    deploymentFingerprint: config.deploymentFingerprint,
    deploymentIdentityDigest: config.deploymentIdentityDigest,
    stateQueuePolicyId: config.stateQueuePolicyId,
    finalityDepth: config.finalityDepth,
  };
  const walkLimit = stateQueueReplayWalkLimit(config.finalityDepth);
  const replayFrom = async (anchor: StateQueueReplayAnchor) => {
    const queueChanged =
      JSON.stringify(anchor.queue) !== JSON.stringify(finalQueue);
    let checkpoints: readonly SDK.StateQueueAuthenticatedReplayCheckpoint[] =
      [];
    if (provider.fetchStateQueueReplayCheckpoints !== undefined) {
      if (snapshot?.tipBlockNo === undefined) {
        throw new Error(
          "state-queue snapshot carries no tip height to judge the finality of replayed history against",
        );
      }
      checkpoints = await provider.fetchStateQueueReplayCheckpoints(
        anchor.queue,
        finalQueue,
        snapshot.tipBlockNo,
        walkLimit,
      );
    } else if (queueChanged) {
      // A missing capability says nothing about the chain: an observation
      // failure, not an integrity one.
      throw new Error(
        "state-queue provider has no authenticated ordered history source",
      );
    }
    if (checkpoints.length > walkLimit) {
      throw new Error(
        `state-queue provider returned ${checkpoints.length.toString()} replay checkpoints where at most ${walkLimit.toString()} were asked for`,
      );
    }
    if (queueChanged && checkpoints.length === 0) {
      throw new L1SourceIntegrityError(
        "state-queue changed without an authenticated replay checkpoint",
      );
    }
    if (
      checkpoints.length === walkLimit &&
      JSON.stringify(checkpoints.at(-1)!.nextQueue) !==
        JSON.stringify(finalQueue)
    ) {
      const caughtUp = catchUpRetentionOutcomes(
        config.previousHeaders ?? [],
        checkpoints,
        { ...observationConfig, replayAnchor: anchor },
      );
      if (caughtUp === undefined) {
        throw new Error(
          `state-queue replay is catching up, but none of the next ${walkLimit.toString()} checkpoints is final yet`,
        );
      }
      return { checkpointCount: checkpoints.length, caughtUp };
    }
    return {
      checkpointCount: checkpoints.length,
      observation: terminalRetentionOutcomes(
        config.previousHeaders ?? [],
        current,
        checkpoints,
        snapshot,
        { ...observationConfig, replayAnchor: anchor },
      ),
    };
  };
  const durableAnchor = config.terminalReplayAnchor;
  let provisionalAnchor =
    durableAnchor === undefined ? config.provisionalReplayAnchor : undefined;
  let replayed: Awaited<ReturnType<typeof replayFrom>> | undefined;
  if (durableAnchor !== undefined) {
    replayed = await replayFrom(durableAnchor);
  } else if (provisionalAnchor !== undefined) {
    try {
      replayed = await replayFrom(provisionalAnchor);
    } catch (error) {
      // The candidate was not final, so history that no longer extends it
      // proves only that it was rolled back: bootstrap again from this
      // snapshot. Any other failure stands, as it would from a durable anchor.
      if (!(error instanceof StateQueueHistoryNotExtendingAnchorError)) {
        throw error;
      }
      config.recordDiscardedReplayAnchorCandidate?.(
        provisionalAnchor,
        error.message,
      );
      provisionalAnchor = undefined;
    }
  }
  if (replayed?.caughtUp !== undefined) {
    if (config.recordCatchUp === undefined) {
      throw new Error(
        "state-queue history is longer than one scan replays, and this scan cannot record catch-up progress",
      );
    }
    config.recordCatchUp({
      anchor: {
        deploymentIdentityDigest: config.deploymentIdentityDigest,
        stateQueuePolicyId: config.stateQueuePolicyId,
        ...replayed.caughtUp.finalAnchor,
      },
      walkedCheckpoints: replayed.checkpointCount,
      snapshotRecords: current,
      finalSteps: replayed.caughtUp.finalSteps,
      terminalStatuses: replayed.caughtUp.terminalStatuses,
      terminalRecords: replayed.caughtUp.terminalRecords,
    });
    return [];
  }
  const observation =
    replayed?.observation ??
    terminalRetentionOutcomes(
      config.previousHeaders ?? [],
      current,
      [],
      snapshot,
      observationConfig,
    );
  config.recordReplayedHeaderSteps?.({
    deferredHeaderHashes: observation.deferredHeaderHashes,
    finalSteps: observation.finalSteps,
  });
  if (snapshot !== undefined && config.recordL1View !== undefined) {
    config.recordL1View({
      confirmedHeaderHash: normalizeHex(snapshot.confirmedHeaderHash, {
        fieldName: "state queue confirmed header hash",
      }),
      liveQueueHeaderHashes: current.map(({ headerHash }) => headerHash),
    });
  }
  if (snapshot !== undefined) {
    const identity = {
      deploymentIdentityDigest: config.deploymentIdentityDigest,
      stateQueuePolicyId: config.stateQueuePolicyId,
    };
    // Every output the snapshot's queue names is final, so the queue itself
    // is.
    const snapshotFinal =
      (snapshot.observedChainPoint.finalized === true ||
        (snapshot.observedChainPoint.depth ?? 0) >= config.finalityDepth) &&
      current.every(({ finalized }) => finalized);
    const bootstrapBlockNo = Math.max(
      snapshot.observedChainPoint.blockHeight ?? 0,
      ...current.map(
        ({ observedChainPoint }) => observedChainPoint.blockHeight ?? 0,
      ),
    );
    const candidate: StateQueueReplayAnchor =
      provisionalAnchor ??
      ({
        ...identity,
        queue: finalQueue,
        blockNo: bootstrapBlockNo.toString(),
        transactionIndex: "0",
      } satisfies StateQueueReplayAnchor);
    const durable: StateQueueReplayAnchor | undefined =
      observation.finalAnchor !== undefined
        ? { ...identity, ...observation.finalAnchor }
        : durableAnchor !== undefined
          ? durableAnchor
          : (replayed?.checkpointCount ?? 0) === 0 && snapshotFinal
            ? candidate
            : undefined;
    if (durable !== undefined) {
      config.recordReplayAnchor?.(durable);
      config.recordProvisionalReplayAnchor?.(undefined);
    } else {
      config.recordProvisionalReplayAnchor?.(candidate);
    }
  }
  return observation.records;
};

export const hashBlockHeader = (header: SDK.Header): string => {
  const headerCborHex = Data.to(header, SDK.Header);
  return bytesToHex(blake2b(Buffer.from(headerCborHex, "hex"), { dkLen: 28 }));
};

const validateObservedNode = (
  node: ObservedStateQueueNode,
  config: StateQueueScanConfig,
): StateQueueHeaderRecord => {
  const validationErrors: string[] = [];
  if (!isMidgardConsensusProfile(config.consensusProfile)) {
    validationErrors.push("consensus_profile_mismatch");
  }
  const computedHeaderHash = hashBlockHeader(node.header);
  const linkedListKey = normalizeHex(node.linkedListKey, {
    fieldName: "state queue linked-list key",
    byteLength: 28,
  });
  if (linkedListKey !== computedHeaderHash) {
    validationErrors.push("linked_list_key_mismatch");
  }
  if (!node.assetName.startsWith(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX)) {
    validationErrors.push("block_asset_prefix_mismatch");
  } else {
    const suffix = node.assetName.slice(
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
    );
    if (suffix !== computedHeaderHash) {
      validationErrors.push("block_asset_suffix_mismatch");
    }
  }
  const attestationMarker = classifyDaAttestationMarker(node.daAttestation);
  const status =
    validationErrors.length > 0
      ? "conflicted"
      : attestationMarker.kind === "unattested"
        ? "unattested"
        : "attested";
  return {
    deploymentFingerprint: config.deploymentFingerprint,
    headerHash: computedHeaderHash,
    stateQueueOutRef: node.outRef,
    blockAssetName: node.assetName,
    rawStateQueueDatumCbor: node.rawDatumCbor,
    header: node.header,
    computedHeaderHash,
    daAttestation: node.daAttestation,
    observedChainPoint: node.chainPoint,
    finalized:
      node.chainPoint.finalized === true ||
      (node.chainPoint.depth ?? 0) >= config.finalityDepth,
    status,
    validationErrors,
    updatedAt: new Date().toISOString(),
  };
};
