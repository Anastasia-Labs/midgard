import { type DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import { type EvidenceProvenance } from "@al-ft/midgard-sdk";

import { type WatcherConfig } from "../runtime/config.js";
import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { type WatcherDaProofInput } from "./durable-store.js";

/**
 * The public DA request and record shapes: what one libp2p exchange carries,
 * and the verified public DA records the canonical block store persists.
 */

export const WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION =
  "midgard-watcher-public-da-client-v1" as const;

export type WatcherPublicDaRequest = Readonly<{
  peerIdentity: string;
  peerId: string;
  multiaddr: string;
  protocol: DaRequestResponseProtocol;
  protocolId: string;
  requestCbor: Buffer;
  timeoutMs: number;
  signal: AbortSignal;
  /** Explicit Custom config and its existing verified identity; never a boolean bypass. */
  customNetwork?: Readonly<{
    watcherConfig: WatcherConfig;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  }>;
}>;

/**
 * One public DA request-response exchange. Implementations dial the supplied
 * public libp2p multiaddress and deployment-scoped protocol ID.
 */
export interface WatcherPublicDaLibp2pTransportV1 {
  request(request: WatcherPublicDaRequest): Promise<Uint8Array>;
}

export type WatcherPublicDaAttemptStatus =
  | "deadline_exceeded"
  | "invalid_content"
  | "not_found"
  | "peer_conflict"
  | "peer_rejected"
  | "success"
  | "timeout"
  | "transport_error";

export type WatcherPublicDaAttempt = Readonly<{
  peerIdentity: string;
  protocol: DaRequestResponseProtocol;
  status: WatcherPublicDaAttemptStatus;
}>;

export type WatcherPublicDaPayload = Readonly<{
  schemaVersion: typeof WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION;
  deploymentFingerprint: string;
  headerHash: string;
  payloadHash: string;
  payloadEnvelopeCbor: Buffer;
  innerPayloadCbor: Buffer;
  sourcePeerIdentity: string;
  sourcePeerId: string;
  provenance: EvidenceProvenance;
  durableInput: WatcherDaProofInput;
  attempts: readonly WatcherPublicDaAttempt[];
}>;

export type WatcherPublicDaProofBundle = Readonly<{
  schemaVersion: typeof WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION;
  deploymentFingerprint: string;
  headerHash: string;
  proofBundleHash: string;
  proofBundleBytes: Buffer;
  sourcePeerIdentity: string;
  sourcePeerId: string;
  provenance: EvidenceProvenance;
  durableInput: WatcherDaProofInput;
  attempts: readonly WatcherPublicDaAttempt[];
}>;

export type WatcherPublicDaTraceStep = Readonly<{
  schemaVersion: typeof WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION;
  deploymentFingerprint: string;
  headerHash: string;
  stepIndex: number;
  transitionStepBytes: Buffer;
  transitionStepSha256: string;
  membershipProofBytes: Buffer;
  membershipProofSha256: string;
  sourcePeerIdentity: string;
  sourcePeerId: string;
  provenance: EvidenceProvenance;
  attempts: readonly WatcherPublicDaAttempt[];
}>;

export type WatcherPublicDaEventToStep = Readonly<{
  schemaVersion: typeof WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION;
  deploymentFingerprint: string;
  headerHash: string;
  eventKey: Buffer;
  eventToStepEntryBytes: Buffer | null;
  eventToStepEntrySha256: string | null;
  membershipOrNonmembershipProofBytes: Buffer;
  membershipOrNonmembershipProofSha256: string;
  sourcePeerIdentity: string;
  sourcePeerId: string;
  provenance: EvidenceProvenance;
  attempts: readonly WatcherPublicDaAttempt[];
}>;
