import {
  DA_TRANSPORT_LIMITS,
  daDeploymentFingerprintFromHex,
  type DaPayloadChunkManifest,
  DaRequestResponseProtocol,
  decodeDaEventToStepByEventResponseCbor,
  decodeDaMetadataByHeaderResponseCbor,
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
  admitRetainedDaProvenance,
  bytesFromHexOrBytes,
  type DaLibp2pRetainedDaSourceOptions,
  normalizeHeaderHash,
  type RetainedDaEventToStep,
  type RetainedDaFetchAttempt,
  type RetainedDaFetchAttemptStatus,
  type RetainedDaLibp2pPeer,
  type RetainedDaLibp2pTransport,
  type RetainedDaPayloadFetchOptions,
  type RetainedDaPayloadSource,
  type RetainedDaPayloadSourceResult,
  type RetainedDaProofBundle,
  type RetainedDaTraceStep,
  type SourceFailure,
  type SourceSuccess,
} from "./fetch.admit-retained-da-provenance.js";
import {
  assertHash,
  InvalidRetainedDaResponseError,
  PeerConflictDaResponseError,
  PeerRejectedDaRequestError,
  statusFromError,
} from "./fetch.retained-da-response-errors.js";

type PeerPayload = {
  readonly payloadEnvelopeCbor: Buffer;
  readonly metadata?: unknown;
};

export class DaLibp2pRetainedDaSource implements RetainedDaPayloadSource {
  readonly sourceId: string;

  private readonly deploymentFingerprint: Buffer;
  private readonly peers: readonly RetainedDaLibp2pPeer[];
  private readonly transport: RetainedDaLibp2pTransport;
  private readonly timeoutMs: number;
  private readonly maxInlineResponseBytes: number;
  private readonly maxChunkBytes: number;

  constructor(options: DaLibp2pRetainedDaSourceOptions) {
    this.sourceId = options.sourceId ?? "libp2p";
    this.deploymentFingerprint = daDeploymentFingerprintFromHex(
      options.deploymentFingerprint,
    );
    this.peers = options.peers;
    this.transport = options.transport;
    this.timeoutMs = options.timeoutMs ?? DA_TRANSPORT_LIMITS.requestTimeoutMs;
    this.maxInlineResponseBytes =
      options.maxInlineResponseBytes ??
      DA_TRANSPORT_LIMITS.maxInlineResponseBytes;
    this.maxChunkBytes =
      options.maxChunkBytes ?? DA_TRANSPORT_LIMITS.maxChunkBytes;
  }

  async fetchPayloadByHeaderHash(
    headerHash: string,
    options: RetainedDaPayloadFetchOptions = {},
  ): Promise<RetainedDaPayloadSourceResult> {
    const normalizedHeaderHash = normalizeHeaderHash(headerHash);
    const headerHashBytes = Buffer.from(normalizedHeaderHash, "hex");
    const attempts: RetainedDaFetchAttempt[] = [];

    for (const peer of this.peers) {
      let result: PeerPayload | undefined;
      try {
        result = await this.fetchPayloadFromPeer(peer, headerHashBytes);
      } catch (error) {
        attempts.push(
          this.attemptFromError(
            peer,
            DaRequestResponseProtocol.payloadByHeader,
            error,
          ),
        );
        continue;
      }
      if (result === undefined) {
        attempts.push(
          this.attempt({
            peer,
            protocol: DaRequestResponseProtocol.payloadByHeader,
            status: "not_found",
            detail: "payload not found",
          }),
        );
        continue;
      }
      // Outside the transport catch: a verifier fault is not a peer failure.
      const verdict = await options.verifyPayload?.(result.payloadEnvelopeCbor);
      if (verdict?.ok === false) {
        attempts.push(
          this.attempt({
            peer,
            protocol: DaRequestResponseProtocol.payloadByHeader,
            status: "failed_verification",
            detail: verdict.reason,
          }),
        );
        continue;
      }
      return {
        ok: true,
        provenance: admitRetainedDaProvenance(this.sourceId, peer.peerId),
        sourceId: this.sourceId,
        sourcePeerId: peer.peerId,
        payloadEnvelopeCbor: result.payloadEnvelopeCbor,
        metadata: result.metadata,
        attempts,
      };
    }

    return { ok: false, sourceId: this.sourceId, attempts };
  }

  async fetchProofBundleByHeaderHash(
    headerHash: string,
  ): Promise<SourceSuccess<RetainedDaProofBundle> | SourceFailure> {
    const normalizedHeaderHash = normalizeHeaderHash(headerHash);
    const headerHashBytes = Buffer.from(normalizedHeaderHash, "hex");
    const attempts: RetainedDaFetchAttempt[] = [];

    for (const peer of this.peers) {
      try {
        const response = decodeDaProofBundleByHeaderResponseCbor(
          await this.request(
            peer,
            DaRequestResponseProtocol.proofBundleByHeader,
            encodeDaProofBundleByHeaderRequestCbor({
              deploymentFingerprint: this.deploymentFingerprint,
              headerHash: headerHashBytes,
              maxInlineBytes: this.maxInlineResponseBytes,
            }),
          ),
        );
        if (response.status === "not_found") {
          attempts.push(
            this.attempt({
              peer,
              protocol: DaRequestResponseProtocol.proofBundleByHeader,
              status: "not_found",
              detail: response.reasonCode ?? "proof bundle not found",
            }),
          );
          continue;
        }
        if (response.status === "rejected") {
          throw new PeerRejectedDaRequestError(
            response.reasonCode ?? "peer rejected proof bundle request",
          );
        }
        if (response.status === "found_chunked") {
          throw new InvalidRetainedDaResponseError(
            "chunked proof bundles require a proof-bundle chunk retrieval protocol",
          );
        }
        if (response.proofBundleHash === null) {
          throw new InvalidRetainedDaResponseError(
            "proof bundle response is missing proof_bundle_hash",
          );
        }
        if (response.proofBundleBytes === null) {
          throw new InvalidRetainedDaResponseError(
            "inline proof bundle response is missing proof_bundle_bytes",
          );
        }
        const proofBundleBytes = Buffer.from(response.proofBundleBytes);
        assertHash(
          proofBundleBytes,
          response.proofBundleHash,
          "proof bundle hash mismatch",
        );
        return {
          ok: true,
          provenance: admitRetainedDaProvenance(this.sourceId, peer.peerId),
          sourceId: this.sourceId,
          sourcePeerId: peer.peerId,
          proofBundleHash: Buffer.from(response.proofBundleHash),
          proofBundleBytes,
          attempts,
        };
      } catch (error) {
        attempts.push(
          this.attemptFromError(
            peer,
            DaRequestResponseProtocol.proofBundleByHeader,
            error,
          ),
        );
      }
    }

    return { ok: false, sourceId: this.sourceId, attempts };
  }

  async fetchTraceStepByIndex({
    headerHash,
    stepIndex,
  }: {
    readonly headerHash: string;
    readonly stepIndex: number;
  }): Promise<SourceSuccess<RetainedDaTraceStep> | SourceFailure> {
    const normalizedHeaderHash = normalizeHeaderHash(headerHash);
    const headerHashBytes = Buffer.from(normalizedHeaderHash, "hex");
    const attempts: RetainedDaFetchAttempt[] = [];

    for (const peer of this.peers) {
      try {
        const response = decodeDaTraceStepByIndexResponseCbor(
          await this.request(
            peer,
            DaRequestResponseProtocol.traceStepByIndex,
            encodeDaTraceStepByIndexRequestCbor({
              deploymentFingerprint: this.deploymentFingerprint,
              headerHash: headerHashBytes,
              stepIndex,
            }),
          ),
        );
        if (response.status === "not_found") {
          attempts.push(
            this.attempt({
              peer,
              protocol: DaRequestResponseProtocol.traceStepByIndex,
              status: "not_found",
              detail: "trace step not found",
            }),
          );
          continue;
        }
        if (response.status === "rejected") {
          throw new PeerRejectedDaRequestError(
            "peer rejected trace step request",
          );
        }
        if (
          response.transitionStepBytes === null ||
          response.membershipProofBytes === null
        ) {
          throw new InvalidRetainedDaResponseError(
            "trace step response is missing step or membership proof bytes",
          );
        }
        return {
          ok: true,
          provenance: admitRetainedDaProvenance(this.sourceId, peer.peerId),
          sourceId: this.sourceId,
          sourcePeerId: peer.peerId,
          stepIndex,
          transitionStepBytes: Buffer.from(response.transitionStepBytes),
          membershipProofBytes: Buffer.from(response.membershipProofBytes),
          attempts,
        };
      } catch (error) {
        attempts.push(
          this.attemptFromError(
            peer,
            DaRequestResponseProtocol.traceStepByIndex,
            error,
          ),
        );
      }
    }

    return { ok: false, sourceId: this.sourceId, attempts };
  }

  async fetchEventToStepByEvent({
    headerHash,
    eventKey,
  }: {
    readonly headerHash: string;
    readonly eventKey: string | Uint8Array;
  }): Promise<SourceSuccess<RetainedDaEventToStep> | SourceFailure> {
    const normalizedHeaderHash = normalizeHeaderHash(headerHash);
    const headerHashBytes = Buffer.from(normalizedHeaderHash, "hex");
    const eventKeyBytes = bytesFromHexOrBytes(eventKey, "event_key");
    const attempts: RetainedDaFetchAttempt[] = [];

    for (const peer of this.peers) {
      try {
        const response = decodeDaEventToStepByEventResponseCbor(
          await this.request(
            peer,
            DaRequestResponseProtocol.eventToStepByEvent,
            encodeDaEventToStepByEventRequestCbor({
              deploymentFingerprint: this.deploymentFingerprint,
              headerHash: headerHashBytes,
              eventKey: eventKeyBytes,
            }),
          ),
        );
        if (response.status === "not_found") {
          attempts.push(
            this.attempt({
              peer,
              protocol: DaRequestResponseProtocol.eventToStepByEvent,
              status: "not_found",
              detail: "event-to-step entry not found",
            }),
          );
          continue;
        }
        if (response.status === "rejected") {
          throw new PeerRejectedDaRequestError(
            "peer rejected event-to-step request",
          );
        }
        if (response.membershipOrNonmembershipProofBytes === null) {
          throw new InvalidRetainedDaResponseError(
            "event-to-step response is missing proof bytes",
          );
        }
        return {
          ok: true,
          provenance: admitRetainedDaProvenance(this.sourceId, peer.peerId),
          sourceId: this.sourceId,
          sourcePeerId: peer.peerId,
          eventKey: eventKeyBytes,
          eventToStepEntryBytes:
            response.eventToStepEntryBytes === null
              ? null
              : Buffer.from(response.eventToStepEntryBytes),
          membershipOrNonmembershipProofBytes: Buffer.from(
            response.membershipOrNonmembershipProofBytes,
          ),
          attempts,
        };
      } catch (error) {
        attempts.push(
          this.attemptFromError(
            peer,
            DaRequestResponseProtocol.eventToStepByEvent,
            error,
          ),
        );
      }
    }

    return { ok: false, sourceId: this.sourceId, attempts };
  }

  private async fetchPayloadFromPeer(
    peer: RetainedDaLibp2pPeer,
    headerHash: Buffer,
  ): Promise<PeerPayload | undefined> {
    const response = decodeDaPayloadByHeaderResponseCbor(
      await this.request(
        peer,
        DaRequestResponseProtocol.payloadByHeader,
        encodeDaPayloadByHeaderRequestCbor({
          deploymentFingerprint: this.deploymentFingerprint,
          headerHash,
          acceptedPayloadHashes: null,
          maxInlineBytes: this.maxInlineResponseBytes,
        }),
      ),
    );

    switch (response.status) {
      case "found_inline": {
        if (response.payloadHash === null || response.payloadBytes === null) {
          throw new InvalidRetainedDaResponseError(
            "inline payload response is missing payload bytes",
          );
        }
        const payloadEnvelopeCbor = Buffer.from(response.payloadBytes);
        assertHash(
          payloadEnvelopeCbor,
          response.payloadHash,
          "payload hash mismatch",
        );
        return {
          payloadEnvelopeCbor,
          metadata: await this.fetchMetadata(peer, headerHash),
        };
      }
      case "found_chunked": {
        if (response.payloadHash === null || response.chunkManifest === null) {
          throw new InvalidRetainedDaResponseError(
            "chunked payload response is missing chunk manifest",
          );
        }
        const payloadEnvelopeCbor = await this.fetchPayloadChunks(
          peer,
          headerHash,
          response.payloadHash,
          response.chunkManifest,
        );
        assertHash(
          payloadEnvelopeCbor,
          response.payloadHash,
          "payload hash mismatch",
        );
        return {
          payloadEnvelopeCbor,
          metadata: await this.fetchMetadata(peer, headerHash),
        };
      }
      case "not_found":
        return undefined;
      case "conflict":
        throw new PeerConflictDaResponseError(
          response.reasonCode ?? "peer reported payload conflict",
        );
      case "rejected":
        throw new PeerRejectedDaRequestError(
          response.reasonCode ?? "peer rejected payload request",
        );
    }
  }

  private async fetchPayloadChunks(
    peer: RetainedDaLibp2pPeer,
    headerHash: Buffer,
    payloadHash: Buffer,
    manifest: DaPayloadChunkManifest,
  ): Promise<Buffer> {
    if (manifest.chunkSize > this.maxChunkBytes) {
      throw new InvalidRetainedDaResponseError(
        "payload chunk manifest exceeds retained DA chunk size limit",
      );
    }

    const chunks: Buffer[] = [];
    for (let index = 0; index < manifest.chunkHashes.length; index += 1) {
      const response = decodeDaPayloadChunkResponseCbor(
        await this.request(
          peer,
          DaRequestResponseProtocol.payloadChunk,
          encodeDaPayloadChunkRequestCbor({
            deploymentFingerprint: this.deploymentFingerprint,
            headerHash,
            payloadHash,
            chunkIndex: index,
          }),
        ),
      );
      if (response.status !== "found" || response.chunkBytes === null) {
        throw new InvalidRetainedDaResponseError(
          `payload chunk ${index.toString()} was not found`,
        );
      }
      const chunk = Buffer.from(response.chunkBytes);
      if (chunk.length > this.maxChunkBytes) {
        throw new InvalidRetainedDaResponseError(
          "payload chunk exceeds retained DA chunk size limit",
        );
      }
      const expectedChunkHash = manifest.chunkHashes[index]!;
      assertHash(chunk, expectedChunkHash, "payload chunk hash mismatch");
      if (
        response.chunkHash !== null &&
        !Buffer.from(response.chunkHash).equals(expectedChunkHash)
      ) {
        throw new InvalidRetainedDaResponseError(
          "payload chunk response hash does not match manifest",
        );
      }
      chunks.push(chunk);
    }
    const payloadEnvelopeCbor = Buffer.concat(chunks);
    if (payloadEnvelopeCbor.length !== manifest.totalBytes) {
      throw new InvalidRetainedDaResponseError(
        "chunked payload total size does not match manifest",
      );
    }
    return payloadEnvelopeCbor;
  }

  private async fetchMetadata(
    peer: RetainedDaLibp2pPeer,
    headerHash: Buffer,
  ): Promise<unknown> {
    try {
      const response = decodeDaMetadataByHeaderResponseCbor(
        await this.request(
          peer,
          DaRequestResponseProtocol.metadataByHeader,
          encodeDaPayloadByHeaderRequestCbor({
            deploymentFingerprint: this.deploymentFingerprint,
            headerHash,
            acceptedPayloadHashes: null,
            maxInlineBytes: 0,
          }),
        ),
      );
      return response.status === "found" ? response : undefined;
    } catch {
      return undefined;
    }
  }

  private request(
    peer: RetainedDaLibp2pPeer,
    protocol: DaRequestResponseProtocol,
    payload: Buffer,
  ): Promise<Uint8Array> {
    return this.transport.request({
      peer,
      protocol,
      payload,
      timeoutMs: this.timeoutMs,
    });
  }

  private attempt({
    peer,
    protocol,
    status,
    detail,
  }: {
    readonly peer: RetainedDaLibp2pPeer;
    readonly protocol: DaRequestResponseProtocol;
    readonly status: RetainedDaFetchAttemptStatus;
    readonly detail: string;
  }): RetainedDaFetchAttempt {
    return {
      sourceId: this.sourceId,
      sourcePeerId: peer.peerId,
      protocol,
      status,
      detail,
    };
  }

  private attemptFromError(
    peer: RetainedDaLibp2pPeer,
    protocol: DaRequestResponseProtocol,
    error: unknown,
  ): RetainedDaFetchAttempt {
    return this.attempt({
      peer,
      protocol,
      status: statusFromError(error),
      detail: error instanceof Error ? error.message : String(error),
    });
  }
}
