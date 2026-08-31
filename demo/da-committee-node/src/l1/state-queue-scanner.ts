import {
  isMidgardConsensusProfileV1,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import type {
  ObservedStateQueueNode,
  ObservedStateQueueSnapshotV1,
  StateQueueHeaderRecord,
} from "../domain.js";
import { bytesToHex, normalizeHex } from "../utils/hex.js";
import { classifyDaAttestationMarker } from "./attestation-marker.js";
import type { ChainSyncCursor, ChainSyncEvent } from "./provider.js";
import { terminalRetentionOutcomesV1 } from "./terminal-retention-observation-v1.js";

export interface StateQueueProvider {
  fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]>;
  fetchStateQueueSnapshot?(): Promise<ObservedStateQueueSnapshotV1>;
  fetchStateQueueReplayCheckpoints?(
    anchor: readonly SDK.StateQueueTransitionNodeV1[],
    current: readonly SDK.StateQueueTransitionNodeV1[],
  ): Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpointV1[]>;
  currentChainSyncCursor?(): Promise<ChainSyncCursor>;
  replayChainSyncEvents?(
    afterSequence: number,
  ): Promise<readonly ChainSyncEvent[]>;
}

export type StateQueueReplayAnchorV1 = Readonly<{
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  queue: readonly SDK.StateQueueTransitionNodeV1[];
  blockNo: string;
  transactionIndex: string;
}>;

export type StateQueueScanConfig = {
  readonly deploymentFingerprint: string;
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly daAttestationPolicyId: string;
  readonly finalityDepth: number;
  readonly consensusProfile: MidgardConsensusProfileV1;
  readonly previousHeaders?: readonly StateQueueHeaderRecord[];
  readonly terminalReplayAnchor?: StateQueueReplayAnchorV1;
  readonly recordReplayAnchor?: (anchor: StateQueueReplayAnchorV1) => void;
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
  const records = terminalRetentionOutcomesV1(
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

export const hashBlockHeaderV1 = (header: SDK.HeaderV1): string => {
  const headerCborHex = Data.to(header, SDK.HeaderV1);
  return bytesToHex(blake2b(Buffer.from(headerCborHex, "hex"), { dkLen: 28 }));
};

const validateObservedNode = (
  node: ObservedStateQueueNode,
  config: StateQueueScanConfig,
): StateQueueHeaderRecord => {
  const validationErrors: string[] = [];
  if (!isMidgardConsensusProfileV1(config.consensusProfile)) {
    validationErrors.push("consensus_profile_mismatch");
  }
  const computedHeaderHash = hashBlockHeaderV1(node.header);
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
