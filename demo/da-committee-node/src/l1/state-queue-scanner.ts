import {
  isMidgardConsensusProfileV1,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import type {
  ObservedStateQueueNode,
  StateQueueHeaderRecord,
} from "../domain.js";
import { bytesToHex, normalizeHex } from "../utils/hex.js";
import { classifyDaAttestationMarker } from "./attestation-marker.js";

export interface StateQueueProvider {
  fetchStateQueueNodes(): Promise<readonly ObservedStateQueueNode[]>;
}

export type StateQueueScanConfig = {
  readonly deploymentFingerprint: string;
  readonly daAttestationPolicyId: string;
  readonly finalityDepth: number;
  readonly consensusProfile: MidgardConsensusProfileV1;
};

export const scanStateQueue = async (
  provider: StateQueueProvider,
  config: StateQueueScanConfig,
): Promise<readonly StateQueueHeaderRecord[]> => {
  const nodes = await provider.fetchStateQueueNodes();
  return nodes
    .filter((node) => node.linkedListKey !== "Empty")
    .map((node) => validateObservedNode(node, config));
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
  const attestationMarker = classifyDaAttestationMarker(
    node.daAttestation,
    config.daAttestationPolicyId,
  );
  const unexpectedAttestation =
    attestationMarker.kind === "already_attested_foreign" ||
    attestationMarker.kind === "invalid";
  const status =
    validationErrors.length > 0 || unexpectedAttestation
      ? "conflicted"
      : attestationMarker.kind === "unattested"
        ? "unattested"
        : "attested";
  if (status === "conflicted" && unexpectedAttestation) {
    validationErrors.push("unexpected_da_attestation_marker");
  }
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
