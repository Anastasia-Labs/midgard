import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  type DaCapabilitiesResponse,
  type DaPayloadChunkManifest,
  DaRequestResponseProtocol,
} from "@al-ft/midgard-core/da-transport";
import {
  assertSecurityGradeEvidence,
  DA_PAYLOAD_VERSION,
  decodeDaPayload,
  type EvidenceProvenance,
} from "@al-ft/midgard-sdk";

import {
  type WatcherConfig,
  type WatcherDaPeerConfig,
} from "../runtime/config.js";
import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { type WatcherDaProofInput } from "./durable-store.js";

export const WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION =
  "midgard-watcher-public-da-client-v1" as const;

export const LOWER_HEX_28 = /^[0-9a-f]{56}$/u;

export const LOWER_HEX_32 = /^[0-9a-f]{64}$/u;

export const MAX_EVENT_KEY_BYTES = 4_096;

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
 * The only I/O authority accepted by the W20 client. Implementations dial the
 * supplied public libp2p multiaddress and deployment-scoped protocol ID.
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

export type WatcherPublicDaClientErrorCode =
  | "all_peers_failed"
  | "deadline_exceeded"
  | "invalid_configuration"
  | "invalid_request";

export class WatcherPublicDaClientError extends Error {
  readonly code: WatcherPublicDaClientErrorCode;
  readonly attempts: readonly WatcherPublicDaAttempt[];

  constructor(
    code: WatcherPublicDaClientErrorCode,
    attempts: readonly WatcherPublicDaAttempt[] = [],
    options: { readonly cause?: unknown } = {},
  ) {
    super(
      `Watcher public DA request failed: ${code}${describeCause(options.cause)}`,
      "cause" in options ? { cause: options.cause } : undefined,
    );
    this.name = "WatcherPublicDaClientErrorV1";
    this.code = code;
    this.attempts = Object.freeze([...attempts]);
  }
}

/**
 * Renders the underlying failure into the message without discarding the
 * structured `cause`. A configuration fault and a genuine programming bug both
 * surface as `invalid_configuration`, so the cause is the only signal that
 * distinguishes "the operator supplied bad input" from "this client is broken".
 */
const describeCause = (cause: unknown): string => {
  if (cause === undefined) {
    return "";
  }
  if (cause instanceof Error) {
    return ` (caused by ${cause.name}: ${cause.message})`;
  }
  return ` (caused by ${JSON.stringify(cause) ?? "undefined"})`;
};

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

export type NegotiatedLimits = Readonly<{
  maxPayloadBytes: number;
  maxInlineResponseBytes: number;
  maxChunkBytes: number;
}>;

export type PeerSuccess<T> = Readonly<{
  value: T;
  protocol: DaRequestResponseProtocol;
}>;

export class PeerFailure extends Error {
  readonly status: Exclude<WatcherPublicDaAttemptStatus, "success">;
  readonly protocol: DaRequestResponseProtocol;

  constructor(
    status: Exclude<WatcherPublicDaAttemptStatus, "success">,
    protocol: DaRequestResponseProtocol,
  ) {
    super(status);
    this.name = "PeerFailure";
    this.status = status;
    this.protocol = protocol;
  }
}

/**
 * The only clock this client reads. Production wiring keeps the real
 * monotonic clock and the real timer queue; the seam exists so that deadline
 * behaviour can be exercised as state instead of as a race between wall-clock
 * timers, which is not decidable under load.
 */
export type WatcherPublicDaClock = Readonly<{
  /** Monotonic milliseconds. Must never move backwards. */
  now: () => number;
  setTimeout: (callback: () => void, delayMs: number) => unknown;
  clearTimeout: (handle: unknown) => void;
}>;

export const REAL_PUBLIC_DA_CLOCK: WatcherPublicDaClock = Object.freeze({
  now: () => performance.now(),
  setTimeout: (callback: () => void, delayMs: number) =>
    setTimeout(callback, delayMs),
  clearTimeout: (handle: unknown) => {
    clearTimeout(handle as ReturnType<typeof setTimeout>);
  },
});

export type PermitWaiter = {
  readonly resolve: () => void;
  readonly reject: (error: WatcherPublicDaClientError) => void;
  timer: unknown;
};

export const decodeResponse = <T>(
  bytes: Uint8Array,
  decode: (value: Uint8Array) => T,
  protocol: DaRequestResponseProtocol,
): T => {
  try {
    return decode(bytes);
  } catch {
    return invalidContent(protocol);
  }
};

export function invalidContent(protocol: DaRequestResponseProtocol): never {
  throw new PeerFailure("invalid_content", protocol);
}

export const requiredValue = <T>(
  value: T | null,
  protocol: DaRequestResponseProtocol,
): T => {
  if (value === null) {
    invalidContent(protocol);
  }
  return value;
};

/** Every successful wire result crosses Q03 before it becomes a caller input. */
export const admittedPublicDaProvenance = (
  peer: WatcherDaPeerConfig,
): EvidenceProvenance =>
  assertSecurityGradeEvidence({
    trustClass: "public_or_permissionless_da",
    sourceId: peer.peerId,
    grade: "security",
  });

export const strictInnerPayload = async (
  payloadEnvelopeCbor: Buffer,
  headerHash: string,
  maxPayloadBytes: number,
): Promise<Buffer> => {
  try {
    const innerPayloadCbor = (
      await unwrapDaPayload(payloadEnvelopeCbor, { maxPayloadBytes })
    ).innerBytes;
    const payload = decodeDaPayload(innerPayloadCbor);
    if (
      payload.version !== DA_PAYLOAD_VERSION ||
      payload.block_body.header_hash !== headerHash
    ) {
      invalidContent(DaRequestResponseProtocol.payloadByHeader);
    }
    return innerPayloadCbor;
  } catch (error) {
    if (error instanceof PeerFailure) {
      throw error;
    }
    return invalidContent(DaRequestResponseProtocol.payloadByHeader);
  }
};

export const validateCapabilities = (
  capabilities: DaCapabilitiesResponse,
): void => {
  const positive = [
    capabilities.maxPayloadBytes,
    capabilities.maxInlineResponseBytes,
    capabilities.maxChunkBytes,
    capabilities.maxStreamsPerPeer,
    capabilities.requestTimeoutMs,
  ].every((value) => Number.isSafeInteger(value) && value > 0);
  if (
    !positive ||
    capabilities.maxInlineResponseBytes > capabilities.maxPayloadBytes ||
    capabilities.maxChunkBytes > capabilities.maxPayloadBytes
  ) {
    invalidContent(DaRequestResponseProtocol.capabilities);
  }
};

export const validateChunkManifest = (
  manifest: DaPayloadChunkManifest,
  payloadHash: Buffer,
  limits: NegotiatedLimits,
): void => {
  if (
    !manifest.payloadHash.equals(payloadHash) ||
    !Number.isSafeInteger(manifest.totalBytes) ||
    manifest.totalBytes <= 0 ||
    manifest.totalBytes > limits.maxPayloadBytes ||
    !Number.isSafeInteger(manifest.chunkSize) ||
    manifest.chunkSize <= 0 ||
    manifest.chunkSize > limits.maxChunkBytes ||
    manifest.chunkHashes.length === 0 ||
    manifest.chunkHashes.length !==
      Math.ceil(manifest.totalBytes / manifest.chunkSize)
  ) {
    invalidContent(DaRequestResponseProtocol.payloadByHeader);
  }
};
