import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  type DaCapabilitiesResponse,
  daDeploymentFingerprintFromHex,
  type DaPayloadChunkManifest,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaCapabilitiesResponseCbor,
  decodeDaEventToStepByEventResponseCbor,
  decodeDaPayloadByHeaderResponseCbor,
  decodeDaPayloadChunkResponseCbor,
  decodeDaProofBundleByHeaderResponseCbor,
  decodeDaTraceStepByIndexResponseCbor,
  encodeDaCapabilitiesRequestCbor,
  encodeDaEventToStepByEventRequestCbor,
  encodeDaPayloadByHeaderRequestCbor,
  encodeDaPayloadChunkRequestCbor,
  encodeDaProofBundleByHeaderRequestCbor,
  encodeDaTraceStepByIndexRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import {
  assertDeploymentMarkerMatches,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import {
  assertSecurityGradeEvidence,
  DA_PAYLOAD_VERSION,
  decodeDaPayload,
  type EvidenceProvenance,
} from "@al-ft/midgard-sdk";

import {
  parseWatcherConfig,
  type WatcherConfig,
  type WatcherDaPeerConfig,
} from "../runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import {
  makeWatcherDurablePayload,
  type WatcherDaProofInput,
} from "./durable-store.js";

export const WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION =
  "midgard-watcher-public-da-client-v1" as const;

const LOWER_HEX_28 = /^[0-9a-f]{56}$/u;
const LOWER_HEX_32 = /^[0-9a-f]{64}$/u;
const MAX_EVENT_KEY_BYTES = 4_096;

export type WatcherPublicDaRequest = Readonly<{
  peerIdentity: string;
  peerId: string;
  multiaddr: string;
  protocol: DaRequestResponseProtocol;
  protocolId: string;
  requestCbor: Buffer;
  timeoutMs: number;
  signal: AbortSignal;
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

type NegotiatedLimits = Readonly<{
  maxPayloadBytes: number;
  maxInlineResponseBytes: number;
  maxChunkBytes: number;
}>;

type PeerSuccess<T> = Readonly<{
  value: T;
  protocol: DaRequestResponseProtocol;
}>;

class PeerFailure extends Error {
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

const REAL_PUBLIC_DA_CLOCK: WatcherPublicDaClock = Object.freeze({
  now: () => performance.now(),
  setTimeout: (callback: () => void, delayMs: number) =>
    setTimeout(callback, delayMs),
  clearTimeout: (handle: unknown) => {
    clearTimeout(handle as ReturnType<typeof setTimeout>);
  },
});

type PermitWaiter = {
  readonly resolve: () => void;
  readonly reject: (error: WatcherPublicDaClientError) => void;
  timer: unknown;
};

export class WatcherPublicDaClient {
  readonly deploymentFingerprint: string;

  private readonly config: WatcherConfig;
  private readonly deploymentFingerprintBytes: Buffer;
  private readonly transport: WatcherPublicDaLibp2pTransportV1;
  private readonly clock: WatcherPublicDaClock;
  private activeRequests = 0;
  private readonly permitWaiters: PermitWaiter[] = [];

  constructor(options: {
    readonly config: WatcherConfig;
    readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    readonly transport: WatcherPublicDaLibp2pTransportV1;
    /** Test seam. Omitted in production, where the real clock is used. */
    readonly clock?: WatcherPublicDaClock;
  }) {
    try {
      this.config = parseWatcherConfig(options.config);
      this.deploymentFingerprint = options.deploymentIdentity.manifestId;
      this.deploymentFingerprintBytes = daDeploymentFingerprintFromHex(
        this.deploymentFingerprint,
      );
      assertDeploymentMarkerMatches(
        makeDeploymentMarker(this.deploymentFingerprint),
        options.deploymentIdentity.durableMarker,
        "watcher public DA client",
      );
      if (this.config.targetNetwork !== options.deploymentIdentity.network) {
        throw new Error("target network mismatch");
      }
      if (
        typeof options.transport !== "object" ||
        options.transport === null ||
        typeof options.transport.request !== "function"
      ) {
        throw new Error("invalid libp2p transport");
      }
      if (
        options.clock !== undefined &&
        (typeof options.clock.now !== "function" ||
          typeof options.clock.setTimeout !== "function" ||
          typeof options.clock.clearTimeout !== "function")
      ) {
        throw new Error("invalid clock");
      }
    } catch (cause) {
      // Never swallow the cause: network mismatch, a rejected deployment
      // marker, an unusable transport, and an outright bug in this constructor
      // all collapse into `invalid_configuration`, and only the chained cause
      // keeps a real defect from being misread as operator misconfiguration.
      throw new WatcherPublicDaClientError("invalid_configuration", [], {
        cause,
      });
    }
    this.transport = options.transport;
    this.clock = options.clock ?? REAL_PUBLIC_DA_CLOCK;
  }

  async fetchPayloadByHeader(input: {
    readonly headerHash: string;
    readonly acceptedPayloadHashes?: readonly string[];
  }): Promise<WatcherPublicDaPayload> {
    const headerHash = exactHex(input.headerHash, LOWER_HEX_28);
    const acceptedPayloadHashes =
      input.acceptedPayloadHashes === undefined
        ? null
        : uniqueHashes(input.acceptedPayloadHashes);
    return this.withPermit(async (deadlineAt) =>
      this.fetchAcrossPeers(deadlineAt, async (peer, limits) => {
        const response = decodeResponse(
          await this.request(
            peer,
            DaRequestResponseProtocol.payloadByHeader,
            encodeDaPayloadByHeaderRequestCbor({
              deploymentFingerprint: this.deploymentFingerprintBytes,
              headerHash: Buffer.from(headerHash, "hex"),
              acceptedPayloadHashes:
                acceptedPayloadHashes === null
                  ? null
                  : acceptedPayloadHashes.map((hash) =>
                      Buffer.from(hash, "hex"),
                    ),
              maxInlineBytes: limits.maxInlineResponseBytes,
            }),
            deadlineAt,
          ),
          decodeDaPayloadByHeaderResponseCbor,
          DaRequestResponseProtocol.payloadByHeader,
        );
        assertEqualBytes(
          response.headerHash,
          Buffer.from(headerHash, "hex"),
          DaRequestResponseProtocol.payloadByHeader,
        );
        if (response.status === "not_found") {
          throw new PeerFailure(
            "not_found",
            DaRequestResponseProtocol.payloadByHeader,
          );
        }
        if (response.status === "conflict") {
          throw new PeerFailure(
            "peer_conflict",
            DaRequestResponseProtocol.payloadByHeader,
          );
        }
        if (response.status === "rejected") {
          throw new PeerFailure(
            "peer_rejected",
            DaRequestResponseProtocol.payloadByHeader,
          );
        }
        const payloadHash = boundedBytes(
          response.payloadHash,
          32,
          DaRequestResponseProtocol.payloadByHeader,
        );
        if (
          acceptedPayloadHashes !== null &&
          !acceptedPayloadHashes.includes(payloadHash.toString("hex"))
        ) {
          invalidContent(DaRequestResponseProtocol.payloadByHeader);
        }
        let payloadEnvelopeCbor: Buffer;
        if (response.status === "found_inline") {
          if (response.chunkManifest !== null) {
            invalidContent(DaRequestResponseProtocol.payloadByHeader);
          }
          payloadEnvelopeCbor = boundedBytes(
            response.payloadBytes,
            limits.maxInlineResponseBytes,
            DaRequestResponseProtocol.payloadByHeader,
          );
          if (payloadEnvelopeCbor.length > limits.maxInlineResponseBytes) {
            invalidContent(DaRequestResponseProtocol.payloadByHeader);
          }
        } else {
          if (response.payloadBytes !== null) {
            invalidContent(DaRequestResponseProtocol.payloadByHeader);
          }
          const chunkManifest = requiredValue(
            response.chunkManifest,
            DaRequestResponseProtocol.payloadByHeader,
          );
          payloadEnvelopeCbor = await this.fetchPayloadChunks(
            peer,
            Buffer.from(headerHash, "hex"),
            payloadHash,
            chunkManifest,
            limits,
            deadlineAt,
          );
        }
        if (
          payloadEnvelopeCbor.length === 0 ||
          payloadEnvelopeCbor.length > limits.maxPayloadBytes ||
          !computeDaSha256Hash(payloadEnvelopeCbor).equals(payloadHash)
        ) {
          invalidContent(DaRequestResponseProtocol.payloadByHeader);
        }
        const innerPayloadCbor = await strictInnerPayload(
          payloadEnvelopeCbor,
          headerHash,
          limits.maxPayloadBytes,
        );
        const payloadHashHex = payloadHash.toString("hex");
        return {
          protocol: DaRequestResponseProtocol.payloadByHeader,
          value: {
            schemaVersion: WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION,
            deploymentFingerprint: this.deploymentFingerprint,
            headerHash,
            payloadHash: payloadHashHex,
            payloadEnvelopeCbor,
            innerPayloadCbor,
            sourcePeerIdentity: peer.identity,
            sourcePeerId: peer.peerId,
            provenance: admittedPublicDaProvenance(peer),
            durableInput: durableInput(
              "da_payload",
              payloadHashHex,
              payloadEnvelopeCbor,
            ),
          },
        };
      }),
    );
  }

  async fetchProofBundleByHeader(input: {
    readonly headerHash: string;
  }): Promise<WatcherPublicDaProofBundle> {
    const headerHash = exactHex(input.headerHash, LOWER_HEX_28);
    return this.withPermit(async (deadlineAt) =>
      this.fetchAcrossPeers(deadlineAt, async (peer, limits) => {
        const response = decodeResponse(
          await this.request(
            peer,
            DaRequestResponseProtocol.proofBundleByHeader,
            encodeDaProofBundleByHeaderRequestCbor({
              deploymentFingerprint: this.deploymentFingerprintBytes,
              headerHash: Buffer.from(headerHash, "hex"),
              maxInlineBytes: limits.maxInlineResponseBytes,
            }),
            deadlineAt,
          ),
          decodeDaProofBundleByHeaderResponseCbor,
          DaRequestResponseProtocol.proofBundleByHeader,
        );
        assertEqualBytes(
          response.headerHash,
          Buffer.from(headerHash, "hex"),
          DaRequestResponseProtocol.proofBundleByHeader,
        );
        if (response.status === "not_found") {
          throw new PeerFailure(
            "not_found",
            DaRequestResponseProtocol.proofBundleByHeader,
          );
        }
        if (response.status === "rejected") {
          throw new PeerFailure(
            "peer_rejected",
            DaRequestResponseProtocol.proofBundleByHeader,
          );
        }
        if (
          response.status !== "found_inline" ||
          response.proofBundleHash === null ||
          response.proofBundleBytes === null ||
          response.chunkManifest !== null
        ) {
          invalidContent(DaRequestResponseProtocol.proofBundleByHeader);
        }
        const proofBundleBytes = boundedBytes(
          response.proofBundleBytes,
          limits.maxInlineResponseBytes,
          DaRequestResponseProtocol.proofBundleByHeader,
        );
        const proofBundleHash = boundedBytes(
          response.proofBundleHash,
          32,
          DaRequestResponseProtocol.proofBundleByHeader,
        );
        if (
          proofBundleBytes.length === 0 ||
          proofBundleBytes.length > limits.maxInlineResponseBytes ||
          !computeDaSha256Hash(proofBundleBytes).equals(proofBundleHash)
        ) {
          invalidContent(DaRequestResponseProtocol.proofBundleByHeader);
        }
        const proofBundleHashHex = proofBundleHash.toString("hex");
        return {
          protocol: DaRequestResponseProtocol.proofBundleByHeader,
          value: {
            schemaVersion: WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION,
            deploymentFingerprint: this.deploymentFingerprint,
            headerHash,
            proofBundleHash: proofBundleHashHex,
            proofBundleBytes,
            sourcePeerIdentity: peer.identity,
            sourcePeerId: peer.peerId,
            provenance: admittedPublicDaProvenance(peer),
            durableInput: durableInput(
              "proof_input",
              proofBundleHashHex,
              proofBundleBytes,
            ),
          },
        };
      }),
    );
  }

  async fetchTraceStepByIndex(input: {
    readonly headerHash: string;
    readonly stepIndex: number;
  }): Promise<WatcherPublicDaTraceStep> {
    const headerHash = exactHex(input.headerHash, LOWER_HEX_28);
    const stepIndex = exactNatural(input.stepIndex);
    return this.withPermit(async (deadlineAt) =>
      this.fetchAcrossPeers(deadlineAt, async (peer, limits) => {
        const response = decodeResponse(
          await this.request(
            peer,
            DaRequestResponseProtocol.traceStepByIndex,
            encodeDaTraceStepByIndexRequestCbor({
              deploymentFingerprint: this.deploymentFingerprintBytes,
              headerHash: Buffer.from(headerHash, "hex"),
              stepIndex,
            }),
            deadlineAt,
          ),
          decodeDaTraceStepByIndexResponseCbor,
          DaRequestResponseProtocol.traceStepByIndex,
        );
        assertEqualBytes(
          response.headerHash,
          Buffer.from(headerHash, "hex"),
          DaRequestResponseProtocol.traceStepByIndex,
        );
        if (response.stepIndex !== stepIndex) {
          invalidContent(DaRequestResponseProtocol.traceStepByIndex);
        }
        if (response.status === "not_found") {
          throw new PeerFailure(
            "not_found",
            DaRequestResponseProtocol.traceStepByIndex,
          );
        }
        if (response.status === "rejected") {
          throw new PeerFailure(
            "peer_rejected",
            DaRequestResponseProtocol.traceStepByIndex,
          );
        }
        if (
          response.transitionStepBytes === null ||
          response.membershipProofBytes === null
        ) {
          invalidContent(DaRequestResponseProtocol.traceStepByIndex);
        }
        const transitionStepBytes = boundedBytes(
          response.transitionStepBytes,
          limits.maxPayloadBytes,
          DaRequestResponseProtocol.traceStepByIndex,
        );
        const membershipProofBytes = boundedBytes(
          response.membershipProofBytes,
          limits.maxPayloadBytes,
          DaRequestResponseProtocol.traceStepByIndex,
        );
        if (
          transitionStepBytes.length + membershipProofBytes.length >
          limits.maxPayloadBytes
        ) {
          invalidContent(DaRequestResponseProtocol.traceStepByIndex);
        }
        return {
          protocol: DaRequestResponseProtocol.traceStepByIndex,
          value: {
            schemaVersion: WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION,
            deploymentFingerprint: this.deploymentFingerprint,
            headerHash,
            stepIndex,
            transitionStepBytes,
            transitionStepSha256:
              computeDaSha256Hash(transitionStepBytes).toString("hex"),
            membershipProofBytes,
            membershipProofSha256:
              computeDaSha256Hash(membershipProofBytes).toString("hex"),
            sourcePeerIdentity: peer.identity,
            sourcePeerId: peer.peerId,
            provenance: admittedPublicDaProvenance(peer),
          },
        };
      }),
    );
  }

  async fetchEventToStepByEvent(input: {
    readonly headerHash: string;
    readonly eventKey: string | Uint8Array;
  }): Promise<WatcherPublicDaEventToStep> {
    const headerHash = exactHex(input.headerHash, LOWER_HEX_28);
    const eventKey = exactEventKey(input.eventKey);
    return this.withPermit(async (deadlineAt) =>
      this.fetchAcrossPeers(deadlineAt, async (peer, limits) => {
        const response = decodeResponse(
          await this.request(
            peer,
            DaRequestResponseProtocol.eventToStepByEvent,
            encodeDaEventToStepByEventRequestCbor({
              deploymentFingerprint: this.deploymentFingerprintBytes,
              headerHash: Buffer.from(headerHash, "hex"),
              eventKey,
            }),
            deadlineAt,
          ),
          decodeDaEventToStepByEventResponseCbor,
          DaRequestResponseProtocol.eventToStepByEvent,
        );
        assertEqualBytes(
          response.headerHash,
          Buffer.from(headerHash, "hex"),
          DaRequestResponseProtocol.eventToStepByEvent,
        );
        assertEqualBytes(
          response.eventKey,
          eventKey,
          DaRequestResponseProtocol.eventToStepByEvent,
        );
        if (response.status === "not_found") {
          throw new PeerFailure(
            "not_found",
            DaRequestResponseProtocol.eventToStepByEvent,
          );
        }
        if (response.status === "rejected") {
          throw new PeerFailure(
            "peer_rejected",
            DaRequestResponseProtocol.eventToStepByEvent,
          );
        }
        if (response.membershipOrNonmembershipProofBytes === null) {
          invalidContent(DaRequestResponseProtocol.eventToStepByEvent);
        }
        const entry =
          response.eventToStepEntryBytes === null
            ? null
            : boundedBytes(
                response.eventToStepEntryBytes,
                limits.maxPayloadBytes,
                DaRequestResponseProtocol.eventToStepByEvent,
              );
        const proof = boundedBytes(
          response.membershipOrNonmembershipProofBytes,
          limits.maxPayloadBytes,
          DaRequestResponseProtocol.eventToStepByEvent,
        );
        if ((entry?.length ?? 0) + proof.length > limits.maxPayloadBytes) {
          invalidContent(DaRequestResponseProtocol.eventToStepByEvent);
        }
        return {
          protocol: DaRequestResponseProtocol.eventToStepByEvent,
          value: {
            schemaVersion: WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION,
            deploymentFingerprint: this.deploymentFingerprint,
            headerHash,
            eventKey: Buffer.from(eventKey),
            eventToStepEntryBytes: entry,
            eventToStepEntrySha256:
              entry === null
                ? null
                : computeDaSha256Hash(entry).toString("hex"),
            membershipOrNonmembershipProofBytes: proof,
            membershipOrNonmembershipProofSha256:
              computeDaSha256Hash(proof).toString("hex"),
            sourcePeerIdentity: peer.identity,
            sourcePeerId: peer.peerId,
            provenance: admittedPublicDaProvenance(peer),
          },
        };
      }),
    );
  }

  private async fetchAcrossPeers<T>(
    deadlineAt: number,
    fetch: (
      peer: WatcherDaPeerConfig,
      limits: NegotiatedLimits,
    ) => Promise<PeerSuccess<Omit<T, "attempts">>>,
  ): Promise<T> {
    const attempts: WatcherPublicDaAttempt[] = [];
    for (const peer of this.config.da.peers) {
      try {
        const limits = await this.negotiate(peer, deadlineAt);
        const success = await fetch(peer, limits);
        attempts.push({
          peerIdentity: peer.identity,
          protocol: success.protocol,
          status: "success",
        });
        return Object.freeze({
          ...success.value,
          attempts: Object.freeze(attempts),
        }) as T;
      } catch (error) {
        const failure =
          error instanceof PeerFailure
            ? error
            : new PeerFailure(
                "transport_error",
                DaRequestResponseProtocol.capabilities,
              );
        attempts.push({
          peerIdentity: peer.identity,
          protocol: failure.protocol,
          status: failure.status,
        });
        if (failure.status === "deadline_exceeded") {
          throw new WatcherPublicDaClientError("deadline_exceeded", attempts);
        }
      }
    }
    // Tie-break: the peer list can run out in the same instant the fetch
    // budget does — the last dial only ever gets `min(remaining, timeout)`, so
    // a peer failure and an exhausted deadline can both hold at once. A spent
    // budget is the stronger fact: it says the caller's deadline contract was
    // violated, which is actionable (raise `daFetchMs`, or shorten the peer
    // list), whereas `all_peers_failed` would claim the peer set was fully and
    // fairly evaluated when the deadline is exactly what stopped it.
    if (this.clock.now() >= deadlineAt) {
      throw new WatcherPublicDaClientError("deadline_exceeded", attempts);
    }
    throw new WatcherPublicDaClientError("all_peers_failed", attempts);
  }

  private async negotiate(
    peer: WatcherDaPeerConfig,
    deadlineAt: number,
  ): Promise<NegotiatedLimits> {
    const response = decodeResponse(
      await this.request(
        peer,
        DaRequestResponseProtocol.capabilities,
        encodeDaCapabilitiesRequestCbor({
          deploymentFingerprint: this.deploymentFingerprintBytes,
        }),
        deadlineAt,
      ),
      decodeDaCapabilitiesResponseCbor,
      DaRequestResponseProtocol.capabilities,
    );
    if (
      !response.deploymentFingerprint.equals(this.deploymentFingerprintBytes)
    ) {
      invalidContent(DaRequestResponseProtocol.capabilities);
    }
    validateCapabilities(response);
    return {
      maxPayloadBytes: Math.min(
        response.maxPayloadBytes,
        DA_TRANSPORT_LIMITS.maxPayloadBytes,
      ),
      maxInlineResponseBytes: Math.min(
        response.maxInlineResponseBytes,
        DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
      ),
      maxChunkBytes: Math.min(
        response.maxChunkBytes,
        DA_TRANSPORT_LIMITS.maxChunkBytes,
      ),
    };
  }

  private async fetchPayloadChunks(
    peer: WatcherDaPeerConfig,
    headerHash: Buffer,
    payloadHash: Buffer,
    manifest: DaPayloadChunkManifest,
    limits: NegotiatedLimits,
    deadlineAt: number,
  ): Promise<Buffer> {
    validateChunkManifest(manifest, payloadHash, limits);
    const chunks: Buffer[] = [];
    for (
      let chunkIndex = 0;
      chunkIndex < manifest.chunkHashes.length;
      chunkIndex += 1
    ) {
      const response = decodeResponse(
        await this.request(
          peer,
          DaRequestResponseProtocol.payloadChunk,
          encodeDaPayloadChunkRequestCbor({
            deploymentFingerprint: this.deploymentFingerprintBytes,
            headerHash,
            payloadHash,
            chunkIndex,
          }),
          deadlineAt,
        ),
        decodeDaPayloadChunkResponseCbor,
        DaRequestResponseProtocol.payloadChunk,
      );
      if (response.status === "not_found") {
        throw new PeerFailure(
          "not_found",
          DaRequestResponseProtocol.payloadChunk,
        );
      }
      if (response.status === "rejected") {
        throw new PeerFailure(
          "peer_rejected",
          DaRequestResponseProtocol.payloadChunk,
        );
      }
      if (
        !response.headerHash.equals(headerHash) ||
        !response.payloadHash.equals(payloadHash) ||
        response.chunkIndex !== chunkIndex ||
        response.chunkBytes === null ||
        response.chunkHash === null
      ) {
        invalidContent(DaRequestResponseProtocol.payloadChunk);
      }
      const chunk = boundedBytes(
        response.chunkBytes,
        limits.maxChunkBytes,
        DaRequestResponseProtocol.payloadChunk,
      );
      const responseChunkHash = boundedBytes(
        response.chunkHash,
        32,
        DaRequestResponseProtocol.payloadChunk,
      );
      const expectedHash = manifest.chunkHashes[chunkIndex]!;
      if (
        !responseChunkHash.equals(expectedHash) ||
        !computeDaSha256Hash(chunk).equals(expectedHash)
      ) {
        invalidContent(DaRequestResponseProtocol.payloadChunk);
      }
      const expectedLength =
        chunkIndex === manifest.chunkHashes.length - 1
          ? manifest.totalBytes -
            manifest.chunkSize * (manifest.chunkHashes.length - 1)
          : manifest.chunkSize;
      if (chunk.length !== expectedLength) {
        invalidContent(DaRequestResponseProtocol.payloadChunk);
      }
      chunks.push(chunk);
    }
    return Buffer.concat(chunks);
  }

  private async request(
    peer: WatcherDaPeerConfig,
    protocol: DaRequestResponseProtocol,
    requestCbor: Buffer,
    deadlineAt: number,
  ): Promise<Buffer> {
    // A dial needs at least a whole millisecond of budget to be worth making:
    // the transport timeout is expressed in whole milliseconds, so anything
    // shorter would have to be rounded UP into budget the fetch does not own.
    // Spending that sliver on another peer is also how an exhausted fetch used
    // to end up reporting a peer failure instead of its deadline — the peer
    // list could run out before the budget check ever saw a non-positive
    // remainder (#535).
    const remainingMs = deadlineAt - this.clock.now();
    if (remainingMs < 1) {
      throw new PeerFailure("deadline_exceeded", protocol);
    }
    const timeoutMs = Math.max(
      1,
      Math.floor(
        Math.min(
          remainingMs,
          this.config.da.requestTimeoutMs,
          DA_TRANSPORT_LIMITS.requestTimeoutMs,
        ),
      ),
    );
    const controller = new AbortController();
    let timer: unknown;
    try {
      const timeout = new Promise<never>((_, reject) => {
        timer = this.clock.setTimeout(() => {
          controller.abort();
          reject(new PeerFailure("timeout", protocol));
        }, timeoutMs);
      });
      const response = await Promise.race([
        this.transport.request({
          peerIdentity: peer.identity,
          peerId: peer.peerId,
          multiaddr: peer.multiaddr,
          protocol,
          protocolId: daRequestResponseProtocolId(
            this.deploymentFingerprint,
            protocol,
          ),
          requestCbor: Buffer.from(requestCbor),
          timeoutMs,
          signal: controller.signal,
        }),
        timeout,
      ]);
      if (!(response instanceof Uint8Array)) {
        throw new PeerFailure("invalid_content", protocol);
      }
      const bytes = Buffer.from(response);
      if (
        bytes.length === 0 ||
        bytes.length > DA_TRANSPORT_LIMITS.maxPayloadBytes
      ) {
        throw new PeerFailure("invalid_content", protocol);
      }
      return bytes;
    } catch (error) {
      if (error instanceof PeerFailure) {
        throw error;
      }
      throw new PeerFailure(
        controller.signal.aborted ? "timeout" : "transport_error",
        protocol,
      );
    } finally {
      if (timer !== undefined) {
        this.clock.clearTimeout(timer);
      }
    }
  }

  private async withPermit<T>(
    task: (deadlineAt: number) => Promise<T>,
  ): Promise<T> {
    const deadlineAt = this.clock.now() + this.config.deadlines.daFetchMs;
    await this.acquirePermit(deadlineAt);
    try {
      if (this.clock.now() >= deadlineAt) {
        throw new WatcherPublicDaClientError("deadline_exceeded");
      }
      return await task(deadlineAt);
    } finally {
      this.releasePermit();
    }
  }

  private async acquirePermit(deadlineAt: number): Promise<void> {
    if (this.activeRequests < this.config.da.maxConcurrency) {
      this.activeRequests += 1;
      return;
    }
    const remainingMs = deadlineAt - this.clock.now();
    if (remainingMs <= 0) {
      throw new WatcherPublicDaClientError("deadline_exceeded");
    }
    await new Promise<void>((resolve, reject) => {
      const waiter: PermitWaiter = {
        resolve,
        reject,
        timer: this.clock.setTimeout(() => {
          const index = this.permitWaiters.indexOf(waiter);
          if (index >= 0) {
            this.permitWaiters.splice(index, 1);
          }
          reject(new WatcherPublicDaClientError("deadline_exceeded"));
        }, remainingMs),
      };
      this.permitWaiters.push(waiter);
    });
  }

  private releasePermit(): void {
    this.activeRequests -= 1;
    const waiter = this.permitWaiters.shift();
    if (waiter !== undefined) {
      this.clock.clearTimeout(waiter.timer);
      this.activeRequests += 1;
      waiter.resolve();
    }
  }
}

const decodeResponse = <T>(
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

function invalidContent(protocol: DaRequestResponseProtocol): never {
  throw new PeerFailure("invalid_content", protocol);
}

const requiredValue = <T>(
  value: T | null,
  protocol: DaRequestResponseProtocol,
): T => {
  if (value === null) {
    invalidContent(protocol);
  }
  return value;
};

/** Every successful wire result crosses Q03 before it becomes a caller input. */
const admittedPublicDaProvenance = (
  peer: WatcherDaPeerConfig,
): EvidenceProvenance =>
  assertSecurityGradeEvidence({
    trustClass: "public_or_permissionless_da",
    sourceId: peer.peerId,
    grade: "security",
  });

const strictInnerPayload = async (
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

const validateCapabilities = (capabilities: DaCapabilitiesResponse): void => {
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

const validateChunkManifest = (
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

const exactHex = (value: string, pattern: RegExp): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  return value;
};

const uniqueHashes = (values: readonly string[]): readonly string[] => {
  if (!Array.isArray(values) || values.length === 0 || values.length > 64) {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  const normalized = values.map((value) => exactHex(value, LOWER_HEX_32));
  if (new Set(normalized).size !== normalized.length) {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  return Object.freeze(normalized);
};

const exactNatural = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  return value;
};

const exactEventKey = (value: string | Uint8Array): Buffer => {
  let bytes: Buffer;
  if (typeof value === "string") {
    if (!/^(?:[0-9a-f]{2})+$/u.test(value)) {
      throw new WatcherPublicDaClientError("invalid_request");
    }
    bytes = Buffer.from(value, "hex");
  } else if (value instanceof Uint8Array) {
    bytes = Buffer.from(value);
  } else {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  if (bytes.length === 0 || bytes.length > MAX_EVENT_KEY_BYTES) {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  return bytes;
};

const assertEqualBytes = (
  actual: Uint8Array,
  expected: Uint8Array,
  protocol: DaRequestResponseProtocol,
): void => {
  if (!Buffer.from(actual).equals(expected)) {
    invalidContent(protocol);
  }
};

const boundedBytes = (
  value: Uint8Array | null,
  maximum: number,
  protocol: DaRequestResponseProtocol,
): Buffer => {
  if (value === null) {
    invalidContent(protocol);
  }
  const bytes = Buffer.from(value);
  if (bytes.length === 0 || bytes.length > maximum) {
    invalidContent(protocol);
  }
  return bytes;
};

const durableInput = (
  kind: WatcherDaProofInput["kind"],
  inputId: string,
  bytes: Buffer,
): WatcherDaProofInput =>
  Object.freeze({
    inputId,
    kind,
    payload: makeWatcherDurablePayload(bytes.toString("hex")),
  });
