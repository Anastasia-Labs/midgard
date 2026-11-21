import { type DaLibp2pRuntimeManifest } from "@al-ft/midgard-core/da-transport";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  type DaParamsConfig,
  type Env,
  type Libp2pDaTransportConfig,
} from "./config.committee-config.js";
import {
  positiveInt,
  signerIndex,
} from "./config.l1-submitter-preflight-config.js";
import {
  optionalNonEmpty,
  splitList,
} from "./config.operational-provider-identity.js";
import {
  isRecord,
  parseLibp2pDaCommitteePeers,
  rejectLibp2pDaUrlEnvOverrides,
  rejectUrlShapedLibp2pDaConfig,
  requiredMultiaddrList,
} from "./config.parse-libp2p-da-committee-peers.js";
import type { DaCommitteeMember } from "./domain.js";
import { bytesToHex, hexToBytes, normalizeHex } from "./utils/hex.js";

export const libp2pDaTransportConfig = ({
  env,
  runtimeManifest,
  deploymentFingerprint,
}: {
  readonly env: Env;
  readonly runtimeManifest: DaLibp2pRuntimeManifest;
  readonly deploymentFingerprint: string;
}): Libp2pDaTransportConfig => {
  rejectLibp2pDaUrlEnvOverrides(env);
  if (runtimeManifest.runtime_topology.target !== "committee") {
    throw new Error("runtime_topology.target must be committee");
  }
  const { da_committee: daCommittee, da_transport: daTransport } =
    runtimeManifest;
  rejectUrlShapedLibp2pDaConfig(daTransport, "da_transport");
  rejectUrlShapedLibp2pDaConfig(daCommittee, "da_committee");
  const threshold = daCommittee.threshold;
  const peers = parseLibp2pDaCommitteePeers(daCommittee);
  return {
    kind: "libp2p",
    deploymentFingerprint,
    noHttpDaTransport: true,
    threshold,
    listenMultiaddrs: requiredMultiaddrList(
      daTransport.listen_multiaddrs,
      "da_transport.listen_multiaddrs",
      { requirePeerId: false },
    ),
    announceMultiaddrs: requiredMultiaddrList(
      daTransport.announce_multiaddrs,
      "da_transport.announce_multiaddrs",
      { requirePeerId: true },
    ),
    bootstrapMultiaddrs: requiredMultiaddrList(
      daTransport.bootstrap_multiaddrs,
      "da_transport.bootstrap_multiaddrs",
      { requirePeerId: true },
    ),
    gossip: {
      strictSign: true,
      emitSelf: false,
      allowedTopicsOnly: true,
      maxGossipMessageBytes: daTransport.gossip.max_gossip_message_bytes,
    },
    limits: {
      maxPayloadBytes: daTransport.limits.max_payload_bytes,
      maxInlineResponseBytes: daTransport.limits.max_inline_response_bytes,
      maxChunkBytes: daTransport.limits.max_chunk_bytes,
      maxStreamsPerPeer: daTransport.limits.max_streams_per_peer,
      requestTimeoutMs: daTransport.limits.request_timeout_ms,
    },
    retentionDays: daTransport.retention_days,
    peers,
  };
};

export const libp2pPrivateKeySourceConfig = (env: Env): string => {
  const source = optionalNonEmpty(env.DA_LIBP2P_PRIVATE_KEY_SOURCE);
  if (source === undefined) {
    throw new Error(
      "DA_LIBP2P_PRIVATE_KEY_SOURCE is required in libp2p DA mode",
    );
  }
  validateLibp2pPrivateKeySource(source);
  return source;
};

export const rejectPublicRetainedDaCoHosting = (env: Env): void => {
  if (
    env.DA_PUBLIC_RETAINED_DA_ENABLED !== undefined ||
    env.DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE !== undefined ||
    env.DA_PUBLIC_RETAINED_DA_DATABASE_URL !== undefined ||
    env.DA_PUBLIC_RETAINED_DA_DATABASE_ROLE !== undefined
  ) {
    throw new Error(
      "public retained-DA must run as the dedicated midgard-public-retained-da process, not inside da-committee-node",
    );
  }
};

const validateLibp2pPrivateKeySource = (source: string): void => {
  if (source.startsWith("seed:")) {
    normalizeHex(source.slice("seed:".length), {
      fieldName: "DA_LIBP2P_PRIVATE_KEY_SOURCE seed",
      byteLength: 32,
    });
    return;
  }
  if (source.startsWith("hex:")) {
    const encoded = source.slice("hex:".length);
    if (encoded.length === 0) {
      throw new Error("DA_LIBP2P_PRIVATE_KEY_SOURCE must include a hex key");
    }
    normalizeHex(encoded, {
      fieldName: "DA_LIBP2P_PRIVATE_KEY_SOURCE protobuf key",
    });
    return;
  }
  if (source.startsWith("file:")) {
    if (source.slice("file:".length).trim() === "") {
      throw new Error("DA_LIBP2P_PRIVATE_KEY_SOURCE must include a file path");
    }
    return;
  }
  throw new Error(
    "DA_LIBP2P_PRIVATE_KEY_SOURCE must use seed:, hex:, or file:",
  );
};

export const validateLibp2pCommitteeMatchesDaParams = (
  transport: Libp2pDaTransportConfig,
  daParams: DaParamsConfig,
): void => {
  const committeeKeys = hexToBytes(daParams.committeeHex, "DA committee");
  const signerCount = committeeKeys.length / 32;
  for (const peer of transport.peers) {
    if (peer.signerIndex >= signerCount) {
      throw new Error(
        `da_committee member signer_index ${peer.signerIndex.toString()} is outside DA committee`,
      );
    }
    const expectedKey = bytesToHex(
      committeeKeys.subarray(peer.signerIndex * 32, peer.signerIndex * 32 + 32),
    );
    if (peer.daVkey !== expectedKey) {
      throw new Error(
        `da_committee member signer_index ${peer.signerIndex.toString()} da_vkey does not match DA committee`,
      );
    }
  }
  if (transport.threshold !== daParams.threshold) {
    throw new Error("da_committee.threshold must match DA params threshold");
  }
};

// @midgard-no-http-da-transport:end

export const daParamsConfig = (
  env: Env,
  runtimeManifest: DaLibp2pRuntimeManifest,
  daCommitteeMembers: readonly DaCommitteeMember[],
): DaParamsConfig => {
  const memberKeys = daCommitteeMembers.map((member) => member.vkey);
  const committeeHex = normalizeHex(memberKeys.join(""), {
    fieldName: "DA committee",
  });
  if (committeeHex.length === 0 || committeeHex.length % 64 !== 0) {
    throw new Error("DA committee must be packed 32-byte verification keys");
  }
  if (
    env.DA_COMMITTEE_HEX !== undefined &&
    normalizeHex(env.DA_COMMITTEE_HEX, {
      fieldName: "DA_COMMITTEE_HEX",
    }) !== committeeHex
  ) {
    throw new Error("DA_COMMITTEE_HEX must exactly match da_committee.members");
  }
  const computedCommitteeHash = bytesToHex(
    blake2b(hexToBytes(committeeHex, "DA committee"), { dkLen: 32 }),
  );
  if (
    env.DA_COMMITTEE_SIGNERS_HASH !== undefined &&
    normalizeHex(env.DA_COMMITTEE_SIGNERS_HASH, {
      fieldName: "DA_COMMITTEE_SIGNERS_HASH",
      byteLength: 32,
    }) !== computedCommitteeHash
  ) {
    throw new Error(
      "DA_COMMITTEE_SIGNERS_HASH must exactly match da_committee.members",
    );
  }
  const committeeSignersHash = computedCommitteeHash;
  const threshold = runtimeManifest.da_committee.threshold;
  if (
    env.DA_THRESHOLD !== undefined &&
    positiveInt(env.DA_THRESHOLD, "DA_THRESHOLD") !== threshold
  ) {
    throw new Error("DA_THRESHOLD must exactly match da_committee.threshold");
  }
  return { committeeHex, committeeSignersHash, threshold };
};

export const parseL1SubmitterSignerIndexes = (
  env: Env,
  members: readonly DaCommitteeMember[],
): readonly number[] => {
  const configured = optionalNonEmpty(env.DA_L1_SUBMITTER_SIGNER_INDEXES);
  if (configured !== undefined) {
    return splitList(configured).map(signerIndex);
  }
  const submitterMembers = members
    .filter((member) => member.canSubmitL1)
    .map((member) => member.index);
  if (submitterMembers.length > 0) {
    return submitterMembers;
  }
  return [];
};

export const parseJsonObject = (
  raw: string,
  path: string,
): Record<string, unknown> => {
  const parsed = JSON.parse(raw) as unknown;
  if (!isRecord(parsed)) {
    throw new Error(`${path} must contain a JSON object`);
  }
  return parsed;
};
