import { type DaStreamChunk } from "@al-ft/midgard-core/da-stream-codec";
import {
  type DaCapabilitiesResponse,
  type DaLibp2pRuntimeManifest,
  type DaMetadataByHeaderResponse,
  type DaPayloadSubmitResponse,
} from "@al-ft/midgard-core/da-transport";
import { Metric } from "effect";

import { DaPayloadsDB } from "../database/index.js";

export const MANIFEST_PATH_ENV = "MIDGARD_DEPLOYMENT_MANIFEST_PATH";

export const LIBP2P_PRIVATE_KEY_SOURCE_ENV = "DA_LIBP2P_PRIVATE_KEY_SOURCE";

export const ACCEPTED_RESPONSE_STATUSES = new Set(["accepted", "duplicate"]);

export const daPublishPeerDurationTimer = Metric.timer(
  "da_publish_peer_duration_ms",
  "Per-peer DA payload submit duration",
);

export const daPublishThresholdDurationTimer = Metric.timer(
  "da_publish_threshold_duration_ms",
  "Time until the DA transport acceptance threshold is reached",
);

export const daPublishAllPeerDurationTimer = Metric.timer(
  "da_publish_all_peer_duration_ms",
  "Time until all DA peer submit attempts settle",
);

export const daPublishStragglerCounter = Metric.counter(
  "da_publish_straggler_total",
  {
    description: "Detached DA peer submit attempts by eventual outcome",
  },
);

export const daPublishRejectedCounter = Metric.counter(
  "da_publish_rejected_total",
  {
    description: "DA payload submit rejections by reason code",
  },
);

export const daPublishConflictCounter = Metric.counter(
  "da_publish_conflict_total",
  {
    description: "Sticky DA payload conflicts reported by committee peers",
  },
);

export const FORBIDDEN_LIBP2P_DA_CONFIG_KEYS = new Set([
  "baseUrl",
  "base_url",
  "baseUrls",
  "base_urls",
  "endpoint",
  "endpoints",
  "url",
  "urls",
  "httpEndpoint",
  "http_endpoint",
  "committeeEndpoint",
  "committee_endpoint",
  "daEndpoint",
  "da_endpoint",
  "gateway",
  "objectStore",
  "object_store",
  "bucket",
  "s3",
]);

export type DaProducerCommitteePeer = {
  readonly signerIndex: number;
  readonly daVkey: string;
  readonly peerId: string;
  readonly multiaddrs: readonly string[];
  readonly roles: readonly string[];
};

export type DaProducerPublicationManifest = {
  readonly deploymentFingerprint: string;
  readonly contractDeploymentManifestId: string;
  readonly localPrivateKeySource: string;
  readonly threshold: number;
  readonly requestTimeoutMs: number;
  readonly maxPayloadBytes: number;
  readonly maxInlineResponseBytes: number;
  readonly maxChunkBytes: number;
  readonly maxStreamsPerPeer: number;
  readonly maxGossipMessageBytes: number;
  readonly listenMultiaddrs: readonly string[];
  readonly announceMultiaddrs: readonly string[];
  readonly bootstrapMultiaddrs: readonly string[];
  readonly committeePeers: readonly DaProducerCommitteePeer[];
};

export type DaProducerPeerResult = {
  readonly peerId: string;
  readonly signerIndex: number;
  readonly protocolId: string;
  readonly status: DaPayloadSubmitResponse["status"] | "transport_error";
  readonly payloadHash: string;
  readonly error?: string;
};

export type DaProducerAnnouncementResult = {
  readonly topic: string;
  readonly payloadHash: string;
  readonly recipients: readonly string[];
};

export type DaProducerPublicationReport = {
  readonly configured: boolean;
  readonly headerHash: string;
  readonly payloadHash: string;
  readonly deploymentFingerprint?: string;
  readonly threshold?: number;
  readonly acceptedPeers: number;
  readonly peerResults: readonly DaProducerPeerResult[];
  /** Stable completion handle; peerResults/acceptedPeers are threshold-time snapshots. */
  readonly allPeerResults?: Promise<readonly DaProducerPeerResult[]>;
  readonly announcement?: DaProducerAnnouncementResult;
  readonly reason?: string;
};

export type DaRetainedPayloadLookup = (
  headerHash: Buffer,
) => Promise<DaPayloadsDB.Row | undefined>;

export type DaRetainedPayloadServer = {
  readonly configured: boolean;
  readonly deploymentFingerprint?: string;
  readonly localPeerId?: string;
  readonly listenMultiaddrs?: readonly string[];
  readonly announceMultiaddrs?: readonly string[];
  readonly close?: () => Promise<void>;
  readonly reason?: string;
};

export type DaLibp2pPreflightPeerStatus =
  | "reachable"
  | "not_found"
  | "protocol_error"
  | "transport_error";

export type DaLibp2pPreflightPeerResult = {
  readonly peerId: string;
  readonly signerIndex: number;
  readonly address: readonly string[];
  readonly protocolId: string;
  readonly status: DaLibp2pPreflightPeerStatus;
  readonly metadataStatus?: DaMetadataByHeaderResponse["status"];
  readonly error?: string;
};

export type DaLibp2pPreflightMode = "bind-listen" | "dial-only";

export type DaLibp2pPreflightFailure = {
  readonly phase: "listen" | "identity" | "dial" | "protocol" | "ordering";
  readonly kind?:
    | "producer_port_already_bound"
    | "identity_mismatch"
    | "peer_unreachable"
    | "protocol_mismatch"
    | "unexpected";
  readonly peerId?: string;
  readonly signerIndex?: number;
  readonly error: string;
  readonly remediation?: string;
};

export type DaLibp2pPreflightListenCheck = {
  readonly checked: boolean;
  readonly status: "bound" | "skipped" | "failed";
  readonly listenMultiaddrs: readonly string[];
  readonly announceMultiaddrs: readonly string[];
  readonly error?: string;
};

export type DaLibp2pPreflightReport = {
  readonly configured: boolean;
  readonly mode: DaLibp2pPreflightMode;
  readonly deploymentFingerprint?: string;
  readonly localPeerId?: string;
  readonly threshold?: number;
  readonly reachableCommitteePeers: number;
  readonly reachableCommitteeSignerIndexes: readonly number[];
  readonly listenCheck: DaLibp2pPreflightListenCheck;
  readonly failures: readonly DaLibp2pPreflightFailure[];
  readonly warnings: readonly string[];
  readonly passed: boolean;
  readonly peerResults: readonly DaLibp2pPreflightPeerResult[];
  readonly reason?: string;
};

export type DaProducerProbeTransport = {
  readonly localPeerId: () => string | Promise<string>;
  readonly request: (
    peer: DaProducerCommitteePeer,
    protocolId: string,
    payload: Uint8Array,
    timeoutMs: number,
  ) => Promise<Uint8Array>;
  readonly close?: () => Promise<void>;
};

export type DaEnvelopeCapabilityMode = "identity" | "zstd";

export type DaEnvelopeCapabilityPeerResult = {
  readonly peerId: string;
  readonly signerIndex: number;
  readonly capable: boolean;
  readonly capabilities?: DaCapabilitiesResponse;
  readonly error?: string;
};

export type DaProducerTransport = DaProducerProbeTransport & {
  readonly sign: (message: Uint8Array) => Uint8Array | Promise<Uint8Array>;
  readonly publish: (
    topic: string,
    payload: Uint8Array,
  ) => Promise<{ readonly recipients: readonly string[] }>;
  readonly requestFramed?: (
    peer: DaProducerCommitteePeer,
    protocolId: string,
    frame: Uint8Array,
    timeoutMs: number,
    maxChunkBytes: number,
  ) => Promise<Uint8Array>;
};

export type DaProducerStream = AsyncIterable<DaStreamChunk> & {
  send(data: Uint8Array): boolean;
  onDrain?(): Promise<void>;
  close?(): Promise<void> | void;
  abort?(error: Error): void;
};

export type DaProducerStreamHandler = (
  stream: DaProducerStream,
) => Promise<void>;

export type DaProducerTransportOptions = {
  readonly mode?: DaLibp2pPreflightMode;
  readonly requestHandlers?: ReadonlyMap<string, DaProducerStreamHandler>;
};

export class DaPayloadPublicationError extends Error {
  readonly report: DaProducerPublicationReport;

  constructor(message: string, report: DaProducerPublicationReport) {
    super(message);
    this.name = "DaPayloadPublicationError";
    this.report = report;
  }
}

export const parseCommitteePeers = (
  daCommittee: DaLibp2pRuntimeManifest["da_committee"],
): readonly DaProducerCommitteePeer[] => {
  const members = daCommittee.members;
  const seenSignerIndexes = new Set<number>();
  const seenPeerIds = new Set<string>();
  return members
    .map((member, index) => {
      const signerIndex = member.signer_index;
      if (seenSignerIndexes.has(signerIndex)) {
        throw new Error(
          `duplicate da_committee.members signer_index ${signerIndex.toString()}`,
        );
      }
      seenSignerIndexes.add(signerIndex);
      const peerId = member.peer_id.trim();
      if (seenPeerIds.has(peerId)) {
        throw new Error(`duplicate da_committee.members peer_id ${peerId}`);
      }
      seenPeerIds.add(peerId);
      const multiaddrs = member.multiaddrs.map((address) => address.trim());
      for (const multiaddr of multiaddrs) {
        if (!multiaddr.endsWith(`/p2p/${peerId}`)) {
          throw new Error(
            `da_committee.members[${index.toString()}].multiaddrs peer id must match ${peerId}`,
          );
        }
      }
      const roles = member.roles.map((role) => role.trim());
      return {
        signerIndex,
        daVkey: normalizeHex32(
          member.da_vkey,
          `da_committee.members[${index.toString()}].da_vkey`,
        ),
        peerId,
        multiaddrs,
        roles,
      };
    })
    .sort((left, right) => left.signerIndex - right.signerIndex);
};

export const normalizeHex32 = (value: string, fieldName: string): string => {
  return normalizeHexBytes(value, fieldName, 32);
};

export const normalizeHexBytes = (
  value: string,
  fieldName: string,
  byteLength?: number,
): string => {
  const normalized = value.trim().toLowerCase();
  if (!/^[0-9a-f]*$/.test(normalized) || normalized.length % 2 !== 0) {
    throw new Error(`${fieldName} must be even-length hex`);
  }
  if (byteLength !== undefined && normalized.length !== byteLength * 2) {
    throw new Error(`${fieldName} must be 32-byte hex`);
  }
  return normalized;
};
