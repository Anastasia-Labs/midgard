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
import { terminalRetentionOutcomes } from "./terminal-retention-observation.js";

export interface StateQueueProvider {
  fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]>;
  fetchStateQueueSnapshot?(): Promise<ObservedStateQueueSnapshot>;
  fetchStateQueueReplayCheckpoints?(
    anchor: readonly SDK.StateQueueTransitionNode[],
    current: readonly SDK.StateQueueTransitionNode[],
  ): Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpoint[]>;
  currentChainSyncCursor?(): Promise<ChainSyncCursor>;
  replayChainSyncEvents?(
    afterSequence: number,
  ): Promise<readonly ChainSyncEvent[]>;
}

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
  readonly terminalReplayAnchor?: StateQueueReplayAnchor;
  readonly recordReplayAnchor?: (anchor: StateQueueReplayAnchor) => void;
};

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
  const replayAnchor = config.terminalReplayAnchor;
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
  const checkpoints =
    provider.fetchStateQueueReplayCheckpoints === undefined ||
    replayAnchor === undefined
      ? []
      : await provider.fetchStateQueueReplayCheckpoints(
          replayAnchor.queue,
          finalQueue,
        );
  if (
    replayAnchor !== undefined &&
    JSON.stringify(replayAnchor.queue) !== JSON.stringify(finalQueue) &&
    checkpoints.length === 0
  ) {
    throw new Error(
      "state-queue changed without an authenticated replay checkpoint",
    );
  }
  const records = terminalRetentionOutcomes(
    config.previousHeaders ?? [],
    current,
    checkpoints,
    snapshot,
    {
      deploymentFingerprint: config.deploymentFingerprint,
      deploymentIdentityDigest: config.deploymentIdentityDigest,
      stateQueuePolicyId: config.stateQueuePolicyId,
      finalityDepth: config.finalityDepth,
      ...(config.terminalReplayAnchor === undefined
        ? {}
        : { replayAnchor: config.terminalReplayAnchor }),
    },
  );
  if (snapshot !== undefined && config.recordReplayAnchor !== undefined) {
    const last = checkpoints.at(-1);
    const bootstrapBlockNo = Math.max(
      snapshot.observedChainPoint.blockHeight ?? 0,
      ...current.map(
        ({ observedChainPoint }) => observedChainPoint.blockHeight ?? 0,
      ),
    );
    config.recordReplayAnchor({
      deploymentIdentityDigest: config.deploymentIdentityDigest,
      stateQueuePolicyId: config.stateQueuePolicyId,
      queue: finalQueue,
      blockNo:
        last?.blockNo ?? replayAnchor?.blockNo ?? bootstrapBlockNo.toString(),
      transactionIndex:
        last?.transactionIndex ?? replayAnchor?.transactionIndex ?? "0",
    });
  }
  return records;
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
