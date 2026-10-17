import {
  DaPayloadEnvelopeError,
  unwrapDaPayload,
} from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
  daDeploymentFingerprintFromHex,
  type DaPayloadChunkManifest,
  type DaPayloadSubmitRequest,
  decodeDaCapabilitiesRequestCbor,
  decodeDaPayloadByHeaderRequestCbor,
  decodeDaPayloadChunkRequestCbor,
  decodeDaPayloadSubmitRequestCbor,
  encodeDaCapabilitiesResponseCbor,
  encodeDaMetadataByHeaderResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
  encodeDaPayloadChunkResponseCbor,
  encodeDaPayloadSubmitResponseCbor,
  normalizeDaDeploymentFingerprintHex,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type { DaPayloadRecord } from "../../domain.js";
import {
  hasPayloadBytes,
  libp2pSubmittedDaPayloadRecord,
} from "../../store.js";
import {
  chunkManifestFor,
  containsHash,
  metadataForPayload,
  normalizeHashOrEmpty,
  optionalHash,
} from "./payload-protocols.metadata-for-payload.js";
import {
  DaLibp2pPayloadProtocolError,
  type DaLibp2pPayloadProtocolHandlersOptions,
  type DaLibp2pPayloadProtocolLimits,
  type DaLibp2pPayloadProtocolStore,
  type DaLibp2pPublicRetainedDaPayloadStore,
  decodeRequest,
  emptyMetadataResponse,
  encodePayloadChunkNotFound,
  encodeSubmitAccepted,
  encodeSubmitConflict,
  isSettledPayload,
  metadataAbsentResponse,
  payloadByHeaderAbsentResponse,
  payloadBytesFromRecord,
  type RetainedDaPayloadAdmission,
  sameStoredPayload,
  type StoredPayloadResolution,
  validateChunkManifest,
  validateLimits,
} from "./payload-protocols.validate-limits.js";

export class DaLibp2pPayloadProtocolHandlers<
  Store extends
    DaLibp2pPublicRetainedDaPayloadStore = DaLibp2pPayloadProtocolStore,
> {
  private readonly deploymentFingerprint: string;
  private readonly deploymentFingerprintBytes: Buffer;
  private readonly limits: DaLibp2pPayloadProtocolLimits;
  private readonly log: (message: string) => void;
  private readonly now: () => Date;
  private readonly store: Store;

  constructor(options: DaLibp2pPayloadProtocolHandlersOptions<Store>) {
    this.deploymentFingerprint = normalizeDaDeploymentFingerprintHex(
      options.deploymentFingerprint,
    );
    this.deploymentFingerprintBytes = daDeploymentFingerprintFromHex(
      this.deploymentFingerprint,
    );
    this.store = options.store;
    this.limits = {
      maxPayloadBytes:
        options.limits?.maxPayloadBytes ?? DA_TRANSPORT_LIMITS.maxPayloadBytes,
      maxInlineResponseBytes:
        options.limits?.maxInlineResponseBytes ??
        DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
      maxChunkBytes:
        options.limits?.maxChunkBytes ?? DA_TRANSPORT_LIMITS.maxChunkBytes,
      maxStreamsPerPeer:
        options.limits?.maxStreamsPerPeer ??
        DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
      requestTimeoutMs:
        options.limits?.requestTimeoutMs ??
        DA_TRANSPORT_LIMITS.requestTimeoutMs,
    };
    this.now = options.now ?? (() => new Date());
    this.log = options.log ?? ((message) => console.warn(message));
    validateLimits(this.limits);
  }

  async handleCapabilities(requestCbor: Uint8Array): Promise<Buffer> {
    decodeRequest(
      () => decodeDaCapabilitiesRequestCbor(requestCbor),
      "capabilities request",
    );
    // A foreign fingerprint is answered, not aborted: the response carries
    // the local fingerprint, which every prober compares against its own.
    return encodeDaCapabilitiesResponseCbor({
      deploymentFingerprint: this.deploymentFingerprintBytes,
      transportProtocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
      payloadSchemaVersions: [1],
      envelopeContentEncodings: [0, 1],
      maxPayloadBytes: this.limits.maxPayloadBytes,
      maxInlineResponseBytes: this.limits.maxInlineResponseBytes,
      maxChunkBytes: this.limits.maxChunkBytes,
      maxStreamsPerPeer: this.limits.maxStreamsPerPeer,
      requestTimeoutMs: this.limits.requestTimeoutMs,
    });
  }

  async handlePayloadSubmit(
    this: DaLibp2pPayloadProtocolHandlers<DaLibp2pPayloadProtocolStore>,
    requestCbor: Uint8Array,
  ): Promise<Buffer> {
    const request = decodeRequest(
      () => decodeDaPayloadSubmitRequestCbor(requestCbor),
      "payload-submit request",
    );
    const headerHash = request.headerHash;
    const payloadHash = request.payloadHash;
    const rejected = (reasonCode: string): Buffer =>
      encodeDaPayloadSubmitResponseCbor({
        status: "rejected",
        headerHash,
        payloadHash,
        reasonCode,
        retryAfterMs: null,
      });

    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return rejected("deployment_fingerprint_mismatch");
    }
    if (request.mode === "chunked") {
      const manifestResult = this.validateSubmitManifest(request);
      return encodeDaPayloadSubmitResponseCbor({
        status: manifestResult.ok ? "deferred" : "rejected",
        headerHash,
        payloadHash,
        reasonCode: manifestResult.reasonCode,
        retryAfterMs: null,
      });
    }
    if (request.payloadBytes === null) {
      return rejected("missing_inline_payload_bytes");
    }
    if (request.chunkManifest !== null) {
      return rejected("inline_submit_must_not_include_chunk_manifest");
    }
    const payloadBytes = Buffer.from(request.payloadBytes);
    const checked = await this.checkInlineRetainedPayload(
      request,
      payloadBytes,
    );
    if (!checked.ok) {
      return rejected(checked.reasonCode);
    }

    const headerHashHex = headerHash.toString("hex");
    const payloadHashHex = checked.admission.rawEnvelopeSha256.toString("hex");
    const existing = await this.store.getDaPayload(headerHashHex);
    if (existing !== undefined && existing.validationStatus === "conflicted") {
      return encodeSubmitConflict(headerHash, payloadHash, "stored_conflict");
    }
    if (existing !== undefined && hasPayloadBytes(existing)) {
      if (sameStoredPayload(existing, payloadBytes, payloadHashHex)) {
        return encodeDaPayloadSubmitResponseCbor({
          status: "duplicate",
          headerHash,
          payloadHash,
          reasonCode: null,
          retryAfterMs: null,
        });
      }
      if (isSettledPayload(existing)) {
        // First settled bytes win.  The refusal is reported here rather than
        // written into the record that gates attestation and availability.
        this.log(
          `refused payload-submit for ${headerHashHex}: sha256 ${payloadHashHex} conflicts with ${existing.validationStatus} sha256 ${existing.payloadSha256}`,
        );
        return encodeSubmitConflict(
          headerHash,
          payloadHash,
          "conflicting_payload_bytes",
        );
      }
    }

    const saved = await this.retainInlinePayloadUnverified(
      headerHashHex,
      payloadHashHex,
      payloadBytes,
      checked.admission.payloadSchemaVersion,
    );
    return saved.validationStatus === "conflicted"
      ? encodeSubmitConflict(
          headerHash,
          payloadHash,
          "conflicting_payload_bytes",
        )
      : encodeSubmitAccepted(headerHash, payloadHash);
  }

  async handlePayloadByHeader(requestCbor: Uint8Array): Promise<Buffer> {
    const request = decodeRequest(
      () => decodeDaPayloadByHeaderRequestCbor(requestCbor),
      "payload-by-header request",
    );
    const headerHash = request.headerHash;
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return encodeDaPayloadByHeaderResponseCbor({
        status: "rejected",
        headerHash,
        payloadHash: null,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: "deployment_fingerprint_mismatch",
      });
    }

    const resolved = await this.resolveStoredPayload(headerHash);
    if (resolved.kind !== "found") {
      return encodeDaPayloadByHeaderResponseCbor(
        payloadByHeaderAbsentResponse(headerHash, resolved),
      );
    }
    if (
      request.acceptedPayloadHashes !== null &&
      !containsHash(request.acceptedPayloadHashes, resolved.payloadHash)
    ) {
      return encodeDaPayloadByHeaderResponseCbor({
        status: "conflict",
        headerHash,
        payloadHash: resolved.payloadHash,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: "payload_hash_not_accepted",
      });
    }

    const inlineLimit = Math.min(
      request.maxInlineBytes,
      this.limits.maxInlineResponseBytes,
    );
    if (resolved.payloadBytes.length <= inlineLimit) {
      return encodeDaPayloadByHeaderResponseCbor({
        status: "found_inline",
        headerHash,
        payloadHash: resolved.payloadHash,
        payloadBytes: resolved.payloadBytes,
        chunkManifest: null,
        reasonCode: null,
      });
    }

    return encodeDaPayloadByHeaderResponseCbor({
      status: "found_chunked",
      headerHash,
      payloadHash: resolved.payloadHash,
      payloadBytes: null,
      chunkManifest: this.chunkManifestFor(resolved.payloadBytes),
      reasonCode: null,
    });
  }

  async handlePayloadChunk(requestCbor: Uint8Array): Promise<Buffer> {
    const request = decodeRequest(
      () => decodeDaPayloadChunkRequestCbor(requestCbor),
      "payload-chunk request",
    );
    const headerHash = request.headerHash;
    const payloadHash = request.payloadHash;
    const rejected = (): Buffer =>
      encodeDaPayloadChunkResponseCbor({
        status: "rejected",
        headerHash,
        payloadHash,
        chunkIndex: request.chunkIndex,
        chunkBytes: null,
        chunkHash: null,
      });

    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return rejected();
    }
    const resolved = await this.resolveStoredPayload(headerHash);
    if (resolved.kind !== "found") {
      return encodePayloadChunkNotFound(
        headerHash,
        payloadHash,
        request.chunkIndex,
      );
    }
    if (!resolved.payloadHash.equals(payloadHash)) {
      return encodePayloadChunkNotFound(
        headerHash,
        payloadHash,
        request.chunkIndex,
      );
    }

    const offset = request.chunkIndex * this.limits.maxChunkBytes;
    if (offset >= resolved.payloadBytes.length) {
      return encodePayloadChunkNotFound(
        headerHash,
        payloadHash,
        request.chunkIndex,
      );
    }
    const chunkBytes = resolved.payloadBytes.subarray(
      offset,
      Math.min(
        offset + this.limits.maxChunkBytes,
        resolved.payloadBytes.length,
      ),
    );
    const chunkHash = computeDaSha256Hash(chunkBytes);
    return encodeDaPayloadChunkResponseCbor({
      status: "found",
      headerHash,
      payloadHash,
      chunkIndex: request.chunkIndex,
      chunkBytes,
      chunkHash,
    });
  }

  async handleMetadataByHeader(requestCbor: Uint8Array): Promise<Buffer> {
    const request = decodeRequest(
      () => decodeDaPayloadByHeaderRequestCbor(requestCbor),
      "metadata-by-header request",
    );
    const headerHash = request.headerHash;
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return encodeDaMetadataByHeaderResponseCbor({
        ...emptyMetadataResponse(headerHash),
        status: "rejected",
      });
    }

    const resolved = await this.resolveStoredPayload(headerHash);
    if (resolved.kind !== "found") {
      return encodeDaMetadataByHeaderResponseCbor(
        metadataAbsentResponse(headerHash, resolved),
      );
    }
    if (
      request.acceptedPayloadHashes !== null &&
      !containsHash(request.acceptedPayloadHashes, resolved.payloadHash)
    ) {
      return encodeDaMetadataByHeaderResponseCbor({
        ...emptyMetadataResponse(headerHash),
        status: "conflict",
        payloadHash: resolved.payloadHash,
      });
    }

    const metadata = await metadataForPayload(
      headerHash,
      resolved.payloadHash,
      resolved.payloadBytes,
      resolved.record,
    );
    return encodeDaMetadataByHeaderResponseCbor(metadata);
  }

  private validateSubmitManifest(request: DaPayloadSubmitRequest):
    | { readonly ok: true; readonly reasonCode: "chunked_submit_deferred" }
    | {
        readonly ok: false;
        readonly reasonCode: string;
      } {
    if (request.payloadBytes !== null) {
      return { ok: false, reasonCode: "chunked_submit_must_not_inline_bytes" };
    }
    if (request.chunkManifest === null) {
      return { ok: false, reasonCode: "missing_chunk_manifest" };
    }
    const manifestCheck = validateChunkManifest(
      request.chunkManifest,
      this.limits,
    );
    if (!manifestCheck.ok) {
      return manifestCheck;
    }
    if (!request.chunkManifest.payloadHash.equals(request.payloadHash)) {
      return { ok: false, reasonCode: "chunk_manifest_payload_hash_mismatch" };
    }
    return { ok: true, reasonCode: "chunked_submit_deferred" };
  }

  private async checkInlineRetainedPayload(
    request: DaPayloadSubmitRequest,
    payloadBytes: Buffer,
  ): Promise<
    | { readonly ok: true; readonly admission: RetainedDaPayloadAdmission }
    | { readonly ok: false; readonly reasonCode: string }
  > {
    if (payloadBytes.length === 0) {
      return { ok: false, reasonCode: "empty_payload" };
    }
    if (payloadBytes.length > this.limits.maxPayloadBytes) {
      return { ok: false, reasonCode: "payload_too_large" };
    }
    if (request.payloadSchemaVersion !== Number(SDK.DA_PAYLOAD_VERSION)) {
      return { ok: false, reasonCode: "payload_schema_version_mismatch" };
    }
    const actualPayloadHash = computeDaSha256Hash(payloadBytes);
    if (!actualPayloadHash.equals(request.payloadHash)) {
      return { ok: false, reasonCode: "payload_hash_mismatch" };
    }
    try {
      // This validates only the canonical outer envelope, bounded
      // decompression, and the envelope's inner-byte hash.  Deliberately do
      // not decode, traverse, or compare the inner DA transaction body here:
      // the watcher is the sole semantic gate before any protocol eligibility.
      await unwrapDaPayload(payloadBytes, {
        maxPayloadBytes: this.limits.maxPayloadBytes,
      });
    } catch (cause) {
      return {
        ok: false,
        reasonCode:
          cause instanceof DaPayloadEnvelopeError
            ? cause.reasonCode
            : "payload_envelope_check_failed",
      };
    }
    return {
      ok: true,
      admission: {
        payloadSchemaVersion: 1,
        rawEnvelopeSha256: actualPayloadHash,
      },
    };
  }

  private async retainInlinePayloadUnverified(
    this: DaLibp2pPayloadProtocolHandlers<DaLibp2pPayloadProtocolStore>,
    headerHash: string,
    payloadHash: string,
    payloadBytes: Buffer,
    payloadSchemaVersion: 1,
  ): Promise<DaPayloadRecord> {
    return this.store.saveDaPayload(
      libp2pSubmittedDaPayloadRecord({
        deploymentFingerprint: this.deploymentFingerprint,
        headerHash,
        payloadSchemaVersion,
        payloadCbor: payloadBytes,
        payloadSha256: payloadHash,
        receivedAt: this.now(),
      }),
    );
  }

  private async resolveStoredPayload(
    headerHash: Buffer,
  ): Promise<StoredPayloadResolution> {
    const record = await this.store.getDaPayload(headerHash.toString("hex"));
    if (record === undefined || !hasPayloadBytes(record)) {
      return { kind: "missing" };
    }
    if (record.validationStatus === "conflicted") {
      return {
        kind: "conflict",
        payloadHash: optionalHash(record.payloadSha256),
      };
    }
    const payloadBytes = payloadBytesFromRecord(record);
    if (payloadBytes === null) {
      return { kind: "invalid", reasonCode: "stored_payload_bytes_malformed" };
    }
    if (payloadBytes.length > this.limits.maxPayloadBytes) {
      return { kind: "invalid", reasonCode: "stored_payload_too_large" };
    }
    const payloadHash = computeDaSha256Hash(payloadBytes);
    if (
      payloadHash.toString("hex") !== normalizeHashOrEmpty(record.payloadSha256)
    ) {
      return { kind: "invalid", reasonCode: "stored_payload_hash_mismatch" };
    }
    return {
      kind: "found",
      record,
      payloadBytes,
      payloadHash,
    };
  }

  private chunkManifestFor(payloadBytes: Buffer): DaPayloadChunkManifest {
    const manifest = chunkManifestFor(payloadBytes, this.limits.maxChunkBytes);
    const check = validateChunkManifest(manifest, this.limits);
    if (!check.ok) {
      throw new DaLibp2pPayloadProtocolError(
        `generated invalid chunk manifest: ${check.reasonCode}`,
      );
    }
    return manifest;
  }

  private matchesDeployment(value: Buffer): boolean {
    return value.equals(this.deploymentFingerprintBytes);
  }
}
