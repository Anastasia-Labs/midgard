import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
  daDeploymentFingerprintFromHex,
  type DaPayloadChunkManifestV1,
  DaRequestResponseProtocol,
  decodeDaEventToStepByEventResponseV1Cbor,
  decodeDaMetadataByHeaderResponseV1Cbor,
  decodeDaPayloadByHeaderResponseV1Cbor,
  decodeDaPayloadChunkResponseV1Cbor,
  decodeDaProofBundleByHeaderResponseV1Cbor,
  decodeDaTraceStepByIndexResponseV1Cbor,
  encodeDaEventToStepByEventRequestV1Cbor,
  encodeDaPayloadByHeaderRequestV1Cbor,
  encodeDaPayloadChunkRequestV1Cbor,
  encodeDaProofBundleByHeaderRequestV1Cbor,
  encodeDaTraceStepByIndexRequestV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import {
  admitEvidenceProvenanceV1,
  assertSecurityGradeEvidenceV1,
  CanonicalEvidenceRejectionV1,
  type EvidenceProvenanceV1,
  requireEvidenceTrustClassV1,
} from "@al-ft/midgard-sdk";

import { transitionTraceError } from "./errors.js";

export type RetainedDaLibp2pPeer = {
  readonly peerId: string;
};

export type RetainedDaLibp2pRequest = {
  readonly peer: RetainedDaLibp2pPeer;
  readonly protocol: DaRequestResponseProtocol;
  readonly payload: Buffer;
  readonly timeoutMs: number;
};

export interface RetainedDaLibp2pTransport {
  request(args: RetainedDaLibp2pRequest): Promise<Uint8Array>;
}

export type RetainedDaFetchAttemptStatus =
  | "not_found"
  | "transport_error"
  | "timeout"
  | "invalid_content"
  | "rejected"
  | "conflict";

export type RetainedDaFetchAttempt = {
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly protocol: DaRequestResponseProtocol;
  readonly status: RetainedDaFetchAttemptStatus;
  readonly detail: string;
};

export type RetainedDaPayloadFetchResult = {
  readonly provenance: EvidenceProvenanceV1;
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly payloadEnvelopeCbor: Buffer;
  readonly metadata?: unknown;
  readonly attempts: readonly RetainedDaFetchAttempt[];
};

export type RetainedDaProofBundle = {
  readonly provenance: EvidenceProvenanceV1;
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly proofBundleHash: Buffer;
  readonly proofBundleBytes: Buffer;
  readonly attempts: readonly RetainedDaFetchAttempt[];
};

export type RetainedDaTraceStep = {
  readonly provenance: EvidenceProvenanceV1;
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly stepIndex: number;
  readonly transitionStepBytes: Buffer;
  readonly membershipProofBytes: Buffer;
  readonly attempts: readonly RetainedDaFetchAttempt[];
};

export type RetainedDaEventToStep = {
  readonly provenance: EvidenceProvenanceV1;
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly eventKey: Buffer;
  readonly eventToStepEntryBytes: Buffer | null;
  readonly membershipOrNonmembershipProofBytes: Buffer;
  readonly attempts: readonly RetainedDaFetchAttempt[];
};

export type RetainedDaPayloadSourceResult =
  | ({
      readonly ok: true;
    } & RetainedDaPayloadFetchResult)
  | {
      readonly ok: false;
      readonly sourceId: string;
      readonly attempts: readonly RetainedDaFetchAttempt[];
    };

export interface RetainedDaPayloadSource {
  readonly sourceId: string;
  fetchPayloadByHeaderHash(
    headerHash: string,
  ): Promise<RetainedDaPayloadSourceResult>;
}

export type FetchRetainedDaPayloadOptions = {
  readonly headerHash: string;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
};

export type DaLibp2pRetainedDaSourceOptions = {
  readonly sourceId?: string;
  readonly deploymentFingerprint: string;
  readonly peers: readonly RetainedDaLibp2pPeer[];
  readonly transport: RetainedDaLibp2pTransport;
  readonly timeoutMs?: number;
  readonly maxInlineResponseBytes?: number;
  readonly maxChunkBytes?: number;
};

type SourceSuccess<T> = { readonly ok: true } & T;
type SourceFailure = {
  readonly ok: false;
  readonly sourceId: string;
  readonly attempts: readonly RetainedDaFetchAttempt[];
};

/**
 * Public retained-DA records are security inputs. Construct and admit their
 * provenance at the transport boundary, before any payload or proof bytes can
 * leave this module.
 */
const admitRetainedDaProvenanceV1 = (
  sourceId: string,
  sourcePeerId: string,
): EvidenceProvenanceV1 =>
  assertSecurityGradeEvidenceV1(
    admitEvidenceProvenanceV1({
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: `${sourceId}/${sourcePeerId}`,
        grade: "security",
      },
    }),
  );

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
    this.timeoutMs =
      options.timeoutMs ?? DA_TRANSPORT_LIMITS_V1.requestTimeoutMs;
    this.maxInlineResponseBytes =
      options.maxInlineResponseBytes ??
      DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes;
    this.maxChunkBytes =
      options.maxChunkBytes ?? DA_TRANSPORT_LIMITS_V1.maxChunkBytes;
  }

  async fetchPayloadByHeaderHash(
    headerHash: string,
  ): Promise<RetainedDaPayloadSourceResult> {
    const normalizedHeaderHash = normalizeHeaderHash(headerHash);
    const headerHashBytes = Buffer.from(normalizedHeaderHash, "hex");
    const attempts: RetainedDaFetchAttempt[] = [];

    for (const peer of this.peers) {
      try {
        const result = await this.fetchPayloadFromPeer(peer, headerHashBytes);
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
        return {
          ok: true,
          provenance: admitRetainedDaProvenanceV1(this.sourceId, peer.peerId),
          sourceId: this.sourceId,
          sourcePeerId: peer.peerId,
          payloadEnvelopeCbor: result.payloadEnvelopeCbor,
          metadata: result.metadata,
          attempts,
        };
      } catch (error) {
        attempts.push(
          this.attemptFromError(
            peer,
            DaRequestResponseProtocol.payloadByHeader,
            error,
          ),
        );
      }
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
        const response = decodeDaProofBundleByHeaderResponseV1Cbor(
          await this.request(
            peer,
            DaRequestResponseProtocol.proofBundleByHeader,
            encodeDaProofBundleByHeaderRequestV1Cbor({
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
          provenance: admitRetainedDaProvenanceV1(this.sourceId, peer.peerId),
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
        const response = decodeDaTraceStepByIndexResponseV1Cbor(
          await this.request(
            peer,
            DaRequestResponseProtocol.traceStepByIndex,
            encodeDaTraceStepByIndexRequestV1Cbor({
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
          provenance: admitRetainedDaProvenanceV1(this.sourceId, peer.peerId),
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
        const response = decodeDaEventToStepByEventResponseV1Cbor(
          await this.request(
            peer,
            DaRequestResponseProtocol.eventToStepByEvent,
            encodeDaEventToStepByEventRequestV1Cbor({
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
          provenance: admitRetainedDaProvenanceV1(this.sourceId, peer.peerId),
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
  ): Promise<
    | {
        readonly payloadEnvelopeCbor: Buffer;
        readonly metadata?: unknown;
      }
    | undefined
  > {
    const response = decodeDaPayloadByHeaderResponseV1Cbor(
      await this.request(
        peer,
        DaRequestResponseProtocol.payloadByHeader,
        encodeDaPayloadByHeaderRequestV1Cbor({
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
    manifest: DaPayloadChunkManifestV1,
  ): Promise<Buffer> {
    if (manifest.chunkSize > this.maxChunkBytes) {
      throw new InvalidRetainedDaResponseError(
        "payload chunk manifest exceeds retained DA chunk size limit",
      );
    }

    const chunks: Buffer[] = [];
    for (let index = 0; index < manifest.chunkHashes.length; index += 1) {
      const response = decodeDaPayloadChunkResponseV1Cbor(
        await this.request(
          peer,
          DaRequestResponseProtocol.payloadChunk,
          encodeDaPayloadChunkRequestV1Cbor({
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
      const response = decodeDaMetadataByHeaderResponseV1Cbor(
        await this.request(
          peer,
          DaRequestResponseProtocol.metadataByHeader,
          encodeDaPayloadByHeaderRequestV1Cbor({
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

export const fetchRetainedDaPayloadByHeaderHash = async ({
  headerHash,
  sources,
  retries = 1,
}: FetchRetainedDaPayloadOptions): Promise<RetainedDaPayloadFetchResult> => {
  const normalizedHeaderHash = normalizeHeaderHash(headerHash);
  const attempts: RetainedDaFetchAttempt[] = [];

  for (const source of sources) {
    for (let attempt = 0; attempt <= retries; attempt += 1) {
      const result =
        await source.fetchPayloadByHeaderHash(normalizedHeaderHash);
      attempts.push(...result.attempts);
      if (result.ok) {
        const provenance = requireEvidenceTrustClassV1({
          provenance: assertSecurityGradeEvidenceV1(result.provenance),
          expected: "public_or_permissionless_da",
          code: "da_evidence_wrong_trust_class",
        });
        const expectedSourceId = `${result.sourceId}/${result.sourcePeerId}`;
        if (provenance.sourceId !== expectedSourceId) {
          throw new CanonicalEvidenceRejectionV1(
            "da_evidence_wrong_trust_class",
            `provenance.sourceId=${provenance.sourceId} expected=${expectedSourceId}`,
          );
        }
        return {
          provenance,
          sourceId: result.sourceId,
          sourcePeerId: result.sourcePeerId,
          payloadEnvelopeCbor: result.payloadEnvelopeCbor,
          metadata: result.metadata,
          attempts,
        };
      }
      if (attempt < retries) {
        await sleep(50 * 2 ** attempt);
      }
    }
  }

  throw transitionTraceError(
    "fetchFailed",
    `Unable to fetch retained DA payload for header_hash ${normalizedHeaderHash}: ${attempts
      .map(
        (attempt) =>
          `${attempt.sourceId}/${attempt.sourcePeerId} ${attempt.protocol} ${attempt.status} ${attempt.detail}`,
      )
      .join("; ")}`,
  );
};

class InvalidRetainedDaResponseError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "InvalidRetainedDaResponseError";
  }
}

class PeerRejectedDaRequestError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "PeerRejectedDaRequestError";
  }
}

class PeerConflictDaResponseError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "PeerConflictDaResponseError";
  }
}

const normalizeHeaderHash = (value: string): string =>
  normalizeHex(value, {
    fieldName: "header_hash",
    byteLength: 28,
    trim: true,
  });

const bytesFromHexOrBytes = (
  value: string | Uint8Array,
  fieldName: string,
): Buffer => {
  if (typeof value !== "string") {
    return Buffer.from(value);
  }
  return Buffer.from(
    normalizeHex(value, { fieldName, trim: true, allowEmpty: false }),
    "hex",
  );
};

const assertHash = (
  bytes: Buffer,
  expectedHash: Buffer,
  message: string,
): void => {
  if (!computeDaSha256Hash(bytes).equals(expectedHash)) {
    throw new InvalidRetainedDaResponseError(message);
  }
};

const statusFromError = (error: unknown): RetainedDaFetchAttemptStatus => {
  if (error instanceof PeerRejectedDaRequestError) {
    return "rejected";
  }
  if (error instanceof PeerConflictDaResponseError) {
    return "conflict";
  }
  if (error instanceof InvalidRetainedDaResponseError) {
    return "invalid_content";
  }
  if (error instanceof Error && error.name === "TimeoutError") {
    return "timeout";
  }
  return "transport_error";
};

const sleep = async (ms: number): Promise<void> => {
  await new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
};
