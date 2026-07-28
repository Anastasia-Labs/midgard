import { writeFile } from "node:fs/promises";

import {
  DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE,
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  type DaLibp2pRuntimeManifest,
  normalizeDaDeploymentFingerprintHex,
  parseDaLibp2pRuntimeManifest,
} from "@al-ft/midgard-core/da-transport";
import { multiaddr } from "@multiformats/multiaddr";

import { readFinalizedDeploymentIdentity } from "@/commands/contract-deployment-info.js";
import { loadDaLibp2pIdentity } from "@/da/libp2p-identity.js";

export const DA_LIBP2P_RUNTIME_PROFILES = [
  "host",
  "producer-container-watcher-host",
  "compose",
  "public",
] as const;

export type DaLibp2pRuntimeProfile =
  (typeof DA_LIBP2P_RUNTIME_PROFILES)[number];

export type DaLibp2pRuntimeManifestTarget = "producer" | "watcher";

export type DaLibp2pRuntimeCommitteeMemberInput = {
  readonly signerIndex: number;
  readonly daVkey: string;
  readonly libp2pPrivateKeySource: string;
  readonly roles: readonly string[];
};

export type DaLibp2pRuntimeManifestOptions = {
  readonly target: DaLibp2pRuntimeManifestTarget;
  readonly profile: DaLibp2pRuntimeProfile;
  readonly contractDeploymentInfoPath: string;
  readonly network: string;
  readonly producerPrivateKeySource: string;
  readonly committeeMembers: readonly DaLibp2pRuntimeCommitteeMemberInput[];
  readonly threshold: number;
  readonly localSignerIndex?: number;
  readonly producerPort?: number;
  readonly watcherPort?: number;
  readonly producerServiceName?: string;
  readonly watcherServiceName?: string;
  readonly producerPublicHost?: string;
  readonly watcherPublicHost?: string;
};

const DEFAULT_PRODUCER_PORT = 39002;
const DEFAULT_WATCHER_PORT = 39001;
const DEFAULT_PRODUCER_SERVICE = "midgard-node";
const DEFAULT_WATCHER_SERVICE = "da-committee-node";

export const generateDaLibp2pRuntimeManifest = async (
  options: DaLibp2pRuntimeManifestOptions,
): Promise<DaLibp2pRuntimeManifest> => {
  validateProfile(options.profile);
  const deploymentIdentity = await readFinalizedDeploymentIdentity(
    options.contractDeploymentInfoPath,
  );
  const deploymentFingerprint = normalizeDaDeploymentFingerprintHex(
    deploymentIdentity.manifestId,
  );
  const producerIdentity = await loadDaLibp2pIdentity(
    options.producerPrivateKeySource,
  );
  const members = await Promise.all(
    options.committeeMembers.map(async (member) => {
      const identity = await loadDaLibp2pIdentity(
        member.libp2pPrivateKeySource,
      );
      return {
        ...member,
        peerId: identity.peerId,
      };
    }),
  );
  validateCommittee(members, options.threshold);
  const localMember =
    options.target === "watcher"
      ? memberForSignerIndex(members, options.localSignerIndex)
      : undefined;
  const ports = {
    producer: options.producerPort ?? DEFAULT_PRODUCER_PORT,
    watcher: options.watcherPort ?? DEFAULT_WATCHER_PORT,
  };
  validatePort(ports.producer, "producer port");
  validatePort(ports.watcher, "watcher port");
  const hosts = hostsForProfile(options);
  if (options.profile === "public") {
    rejectLocalRuntimeHost(hosts.producer, "producer public host");
    rejectLocalRuntimeHost(hosts.watcher, "watcher public host");
  }

  const producerAddr = multiaddrForHost(
    hosts.producer,
    ports.producer,
    producerIdentity.peerId,
  );
  const memberMultiaddrs = members.map((member) =>
    isProducerMember(member, producerIdentity.peerId)
      ? producerAddr
      : multiaddrForHost(hosts.watcher, ports.watcher, member.peerId),
  );
  const localListen =
    options.target === "producer"
      ? listenAddr(listenHostForProfile(options.profile), ports.producer)
      : listenAddr(listenHostForProfile(options.profile), ports.watcher);
  const localAnnounce =
    options.target === "producer"
      ? producerAddr
      : multiaddrForHost(hosts.watcher, ports.watcher, localMember!.peerId);
  const bootstrapMultiaddrs =
    options.target === "producer"
      ? memberMultiaddrs.filter(
          (_addr, index) => members[index]!.peerId !== producerIdentity.peerId,
        )
      : [producerAddr];

  return parseDaLibp2pRuntimeManifest({
    schemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
    network: options.network,
    deployment: {
      fingerprint: deploymentFingerprint,
      contract_deployment_manifest_id: deploymentFingerprint,
      contract_deployment_info_sha256:
        deploymentIdentity.contractDeploymentInfoSha256,
      identity_source: DA_LIBP2P_RUNTIME_MANIFEST_IDENTITY_SOURCE,
    },
    runtime_topology: {
      target: options.target,
      profile: options.profile,
      producer_peer_id: producerIdentity.peerId,
      ...(localMember === undefined
        ? {}
        : { local_signer_index: localMember.signerIndex }),
    },
    da_transport: {
      kind: "libp2p",
      no_http_da_transport: true,
      listen_multiaddrs: [localListen],
      announce_multiaddrs: [localAnnounce],
      bootstrap_multiaddrs: bootstrapMultiaddrs,
      gossip: {
        strict_sign: true,
        emit_self: false,
        allowed_topics_only: true,
        max_gossip_message_bytes: DA_TRANSPORT_LIMITS_V1.maxGossipMessageBytes,
      },
      limits: {
        max_payload_bytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
        max_inline_response_bytes:
          DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
        max_chunk_bytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
        max_streams_per_peer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
        request_timeout_ms: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
      },
      retention_days: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
    },
    da_committee: {
      threshold: options.threshold,
      members: members.map((member, index) => ({
        signer_index: member.signerIndex,
        da_vkey: normalizeHex(member.daVkey, 32, "da_vkey"),
        peer_id: member.peerId,
        multiaddrs: [memberMultiaddrs[index]!],
        roles: [...member.roles].sort(),
      })),
    },
  });
};

const isProducerMember = (
  member: DaLibp2pRuntimeCommitteeMemberInput & { readonly peerId: string },
  producerPeerId: string,
): boolean =>
  member.peerId === producerPeerId || member.roles.includes("producer");

export const writeDaLibp2pRuntimeManifest = async (
  path: string,
  manifest: DaLibp2pRuntimeManifest,
): Promise<void> => {
  await writeFile(path, `${JSON.stringify(manifest, null, 2)}\n`, "utf8");
};

function validateProfile(
  profile: string,
): asserts profile is DaLibp2pRuntimeProfile {
  if (!(DA_LIBP2P_RUNTIME_PROFILES as readonly string[]).includes(profile)) {
    throw new Error(`unsupported DA libp2p address profile: ${profile}`);
  }
}

const hostsForProfile = (
  options: Pick<
    DaLibp2pRuntimeManifestOptions,
    | "profile"
    | "producerServiceName"
    | "watcherServiceName"
    | "producerPublicHost"
    | "watcherPublicHost"
  >,
): { readonly producer: string; readonly watcher: string } => {
  switch (options.profile) {
    case "host":
      return { producer: "127.0.0.1", watcher: "127.0.0.1" };
    case "producer-container-watcher-host":
      return { producer: "127.0.0.1", watcher: "host.docker.internal" };
    case "compose":
      return {
        producer: options.producerServiceName ?? DEFAULT_PRODUCER_SERVICE,
        watcher: options.watcherServiceName ?? DEFAULT_WATCHER_SERVICE,
      };
    case "public":
      if (
        options.producerPublicHost === undefined ||
        options.watcherPublicHost === undefined
      ) {
        throw new Error(
          "public DA libp2p profile requires producer and watcher public hosts",
        );
      }
      return {
        producer: options.producerPublicHost,
        watcher: options.watcherPublicHost,
      };
  }
};

const listenHostForProfile = (profile: DaLibp2pRuntimeProfile): string =>
  profile === "host" ? "127.0.0.1" : "0.0.0.0";

const listenAddr = (host: string, port: number): string =>
  `/ip4/${host}/tcp/${port.toString()}`;

const multiaddrForHost = (
  host: string,
  port: number,
  peerId: string,
): string => {
  const protocol = /^\d+\.\d+\.\d+\.\d+$/.test(host) ? "ip4" : "dns4";
  const addr = `/${protocol}/${host}/tcp/${port.toString()}/p2p/${peerId}`;
  return multiaddr(addr).toString();
};

const validateCommittee = (
  members: readonly (DaLibp2pRuntimeCommitteeMemberInput & {
    readonly peerId: string;
  })[],
  threshold: number,
): void => {
  if (!Number.isSafeInteger(threshold) || threshold <= 0) {
    throw new Error("DA libp2p threshold must be a positive safe integer");
  }
  const committeeMembers = members.filter((member) =>
    member.roles.includes("committee"),
  );
  if (committeeMembers.length < threshold) {
    throw new Error("DA libp2p threshold exceeds committee size");
  }
  const signerIndexes = new Set<number>();
  const peerIds = new Set<string>();
  for (const member of members) {
    if (
      !Number.isSafeInteger(member.signerIndex) ||
      member.signerIndex < 0 ||
      member.signerIndex > 255
    ) {
      throw new Error("DA libp2p signer index must fit in one byte");
    }
    if (signerIndexes.has(member.signerIndex)) {
      throw new Error(
        `duplicate signer index ${member.signerIndex.toString()}`,
      );
    }
    signerIndexes.add(member.signerIndex);
    if (peerIds.has(member.peerId)) {
      throw new Error(`duplicate libp2p peer id ${member.peerId}`);
    }
    peerIds.add(member.peerId);
    if (member.roles.length === 0) {
      throw new Error("DA libp2p committee member roles must be non-empty");
    }
  }
};

const memberForSignerIndex = (
  members: readonly (DaLibp2pRuntimeCommitteeMemberInput & {
    readonly peerId: string;
  })[],
  signerIndex: number | undefined,
): DaLibp2pRuntimeCommitteeMemberInput & { readonly peerId: string } => {
  const selected =
    signerIndex === undefined
      ? members[0]
      : members.find((member) => member.signerIndex === signerIndex);
  if (selected === undefined) {
    throw new Error("watcher runtime manifest local signer index is unknown");
  }
  return selected;
};

const validatePort = (port: number, label: string): void => {
  if (!Number.isSafeInteger(port) || port <= 0 || port > 65_535) {
    throw new Error(`${label} must be a valid TCP port`);
  }
};

const rejectLocalRuntimeHost = (host: string, label: string): void => {
  if (
    host === "localhost" ||
    host === "host.docker.internal" ||
    host.startsWith("127.") ||
    host === "0.0.0.0"
  ) {
    throw new Error(`${label} must not be local-only in public profile`);
  }
};

const normalizeHex = (
  value: string,
  byteLength: number,
  label: string,
): string => {
  const normalized = value.trim().toLowerCase();
  if (!/^[0-9a-f]+$/.test(normalized) || normalized.length !== byteLength * 2) {
    throw new Error(`${label} must be ${byteLength.toString()} bytes of hex`);
  }
  return normalized;
};
