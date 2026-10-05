import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  daDeploymentFingerprintFromHex,
  type DaPayloadChunkManifest,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaEventToStepByEventResponseCbor,
  decodeDaPayloadByHeaderResponseCbor,
  decodeDaPayloadChunkResponseCbor,
  decodeDaProofBundleByHeaderResponseCbor,
  decodeDaTraceStepByIndexResponseCbor,
  encodeDaEventToStepByEventRequestCbor,
  encodeDaPayloadByHeaderRequestCbor,
  encodeDaPayloadChunkRequestCbor,
  encodeDaProofBundleByHeaderRequestCbor,
  encodeDaTraceStepByIndexRequestCbor,
} from "@al-ft/midgard-core/da-transport";

import {
  type WatcherConfig,
  type WatcherDaPeerConfig,
} from "../runtime/config.js";
import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import {
  assertEqualBytes,
  boundedBytes,
  durableInput,
  exactEventKey,
  exactHex,
  exactNatural,
  uniqueHashes,
} from "./public-da-client.exact-event-key.js";
import {
  assertPublicDaClientTransport,
  type ManifestPublicDaClientOptions,
  parsePublicDaClientAuthority,
  type PublicDaFetchConfig,
} from "./public-da-client.manifest-config.js";
import { negotiatePublicDaLimits } from "./public-da-client.negotiate.js";
import {
  admittedPublicDaProvenance,
  decodeResponse,
  invalidContent,
  LOWER_HEX_28,
  type NegotiatedLimits,
  PeerFailure,
  type PeerSuccess,
  type PermitWaiter,
  REAL_PUBLIC_DA_CLOCK,
  requiredValue,
  strictInnerPayload,
  validateChunkManifest,
  WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION,
  type WatcherPublicDaAttempt,
  WatcherPublicDaClientError,
  type WatcherPublicDaClock,
  type WatcherPublicDaEventToStep,
  type WatcherPublicDaLibp2pTransportV1,
  type WatcherPublicDaPayload,
  type WatcherPublicDaProofBundle,
  type WatcherPublicDaRequest,
  type WatcherPublicDaTraceStep,
} from "./public-da-client.strict-inner-payload.js";
import {
  closePublicDaRequests,
  validateWithinDeadline,
} from "./public-da-client.validate-inner-payload.js";

export class WatcherPublicDaClient {
  readonly deploymentFingerprint: string;

  private readonly config: PublicDaFetchConfig;
  private readonly deploymentFingerprintBytes: Buffer;
  private readonly transport: WatcherPublicDaLibp2pTransportV1;
  private readonly clock: WatcherPublicDaClock;
  private readonly customNetwork?: WatcherPublicDaRequest["customNetwork"];
  private readonly manifestNetwork?: WatcherPublicDaRequest["manifestNetwork"];
  private activeRequests = 0;
  private readonly permitWaiters: PermitWaiter[] = [];
  private readonly requestControllers = new Set<AbortController>();
  private closed = false;

  constructor(
    options: (
      | {
          readonly config: WatcherConfig;
          readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
        }
      | ManifestPublicDaClientOptions
    ) & {
      readonly transport: WatcherPublicDaLibp2pTransportV1;
      /** Test seam. Omitted in production, where the real clock is used. */
      readonly clock?: WatcherPublicDaClock;
    },
  ) {
    try {
      const authority = parsePublicDaClientAuthority(options);
      this.config = authority.config;
      this.deploymentFingerprint = authority.fingerprint;
      this.customNetwork = authority.customNetwork;
      this.manifestNetwork = authority.manifestNetwork;
      this.deploymentFingerprintBytes = daDeploymentFingerprintFromHex(
        this.deploymentFingerprint,
      );
      assertPublicDaClientTransport(options);
    } catch (cause) {
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
    /** Reject a peer's content before choosing it over another peer. */
    readonly validateInnerPayload?: (
      bytes: Buffer,
      signal: AbortSignal,
    ) => Promise<void>;
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
        if (input.validateInnerPayload !== undefined) {
          try {
            await validateWithinDeadline(
              input.validateInnerPayload,
              innerPayloadCbor,
              deadlineAt,
              {
                clock: this.clock,
                controllers: this.requestControllers,
                isClosed: () => this.closed,
              },
            );
          } catch (error) {
            if (error instanceof PeerFailure || this.closed) throw error;
            invalidContent(DaRequestResponseProtocol.payloadByHeader);
          }
        }
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
      if (this.closed) throw new WatcherPublicDaClientError("closed");
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
        if (this.closed) throw new WatcherPublicDaClientError("closed");
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
    // An exhausted fetch budget takes precedence over peer exhaustion: it means
    // the deadline stopped evaluation before the peer set was fairly tried.
    if (this.clock.now() >= deadlineAt) {
      throw new WatcherPublicDaClientError("deadline_exceeded", attempts);
    }
    throw new WatcherPublicDaClientError("all_peers_failed", attempts);
  }

  private negotiate(
    peer: WatcherDaPeerConfig,
    deadlineAt: number,
  ): Promise<NegotiatedLimits> {
    return negotiatePublicDaLimits(this.deploymentFingerprintBytes, (cbor) =>
      this.request(
        peer,
        DaRequestResponseProtocol.capabilities,
        cbor,
        deadlineAt,
      ),
    );
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
    if (this.closed) throw new WatcherPublicDaClientError("closed");
    // Transport timeouts use whole milliseconds. Rounding a sub-millisecond
    // remainder up would spend budget this fetch does not own (#535).
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
    this.requestControllers.add(controller);
    let timer: unknown;
    let onAbort: (() => void) | undefined;
    try {
      const aborted = new Promise<never>((_, reject) => {
        onAbort = () => reject(new Error("Public DA client closed"));
        controller.signal.addEventListener("abort", onAbort, { once: true });
      });
      const timeout = new Promise<never>((_, reject) => {
        timer = this.clock.setTimeout(() => {
          reject(new PeerFailure("timeout", protocol));
          controller.abort();
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
          ...(this.customNetwork === undefined
            ? {}
            : {
                customNetwork: this.customNetwork,
              }),
          ...(this.manifestNetwork === undefined
            ? {}
            : { manifestNetwork: this.manifestNetwork }),
        }),
        timeout,
        aborted,
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
      this.requestControllers.delete(controller);
      if (onAbort !== undefined)
        controller.signal.removeEventListener("abort", onAbort);
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
      if (this.closed) throw new WatcherPublicDaClientError("closed");
      if (this.clock.now() >= deadlineAt) {
        throw new WatcherPublicDaClientError("deadline_exceeded");
      }
      return await task(deadlineAt);
    } finally {
      this.releasePermit();
    }
  }

  private async acquirePermit(deadlineAt: number): Promise<void> {
    if (this.closed) throw new WatcherPublicDaClientError("closed");
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

  close(): void {
    this.closed = true;
    closePublicDaRequests(
      this.requestControllers,
      this.permitWaiters,
      this.clock,
    );
  }
}
