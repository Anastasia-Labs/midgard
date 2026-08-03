import {
  DaPayloadEnvelopeError,
  unwrapDaPayloadV1,
} from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
  daDeploymentFingerprintFromHex,
  type DaMetadataByHeaderResponseV1,
  type DaPayloadByHeaderResponseV1,
  type DaPayloadChunkManifestV1,
  type DaPayloadSubmitRequestV1,
  decodeDaCapabilitiesRequestV1Cbor,
  decodeDaPayloadByHeaderRequestV1Cbor,
  decodeDaPayloadChunkRequestV1Cbor,
  decodeDaPayloadSubmitRequestV1Cbor,
  encodeDaCapabilitiesResponseV1Cbor,
  encodeDaMetadataByHeaderResponseV1Cbor,
  encodeDaPayloadByHeaderResponseV1Cbor,
  encodeDaPayloadChunkResponseV1Cbor,
  encodeDaPayloadSubmitResponseV1Cbor,
  normalizeDaDeploymentFingerprintHex,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type { DaPayloadRecord, PayloadRootSet } from "../../domain.js";
import {
  hasPayloadBytes,
  libp2pSubmittedDaPayloadRecord,
  type WatcherStore,
} from "../../store.js";
import { bytesToHex, hexToBytes, normalizeHex } from "../../utils/hex.js";

export type DaLibp2pPayloadProtocolStore = Pick<
  WatcherStore,
  "getDaPayload" | "saveDaPayload"
>;

/** Read-only authority required by the public retained-DA listener. */
export type DaLibp2pPublicRetainedDaPayloadStore = Pick<
  WatcherStore,
  "getDaPayload"
>;

export type DaLibp2pPayloadProtocolLimits = {
  readonly maxPayloadBytes: number;
  readonly maxInlineResponseBytes: number;
  readonly maxChunkBytes: number;
  readonly maxStreamsPerPeer: number;
  readonly requestTimeoutMs: number;
};

export type DaLibp2pPayloadProtocolHandlersOptions<
  Store extends
    DaLibp2pPublicRetainedDaPayloadStore = DaLibp2pPayloadProtocolStore,
> = {
  readonly deploymentFingerprint: string | Uint8Array;
  readonly store: Store;
  readonly limits?: Partial<DaLibp2pPayloadProtocolLimits>;
  readonly now?: () => Date;
};

/**
 * What an accepted inline payload-submit has established.
 *
 * This is deliberately only durable retention of a bounded, canonical outer
 * envelope whose raw bytes are SHA-256-bound to the request.  It is not a
 * decoded DA payload and must never be used as an attestation, signing, or
 * proof-artifact eligibility signal.  The watcher owns the sole strict inner
 * payload validation step.
 */
type RetainedDaPayloadAdmissionV1 = {
  readonly payloadSchemaVersion: 1;
  readonly rawEnvelopeSha256: Buffer;
};

export class DaLibp2pPayloadProtocolError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "DaLibp2pPayloadProtocolError";
  }
}

export class DaLibp2pPayloadProtocolHandlers<
  Store extends
    DaLibp2pPublicRetainedDaPayloadStore = DaLibp2pPayloadProtocolStore,
> {
  private readonly deploymentFingerprint: string;
  private readonly deploymentFingerprintBytes: Buffer;
  private readonly limits: DaLibp2pPayloadProtocolLimits;
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
        options.limits?.maxPayloadBytes ??
        DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      maxInlineResponseBytes:
        options.limits?.maxInlineResponseBytes ??
        DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
      maxChunkBytes:
        options.limits?.maxChunkBytes ?? DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
      maxStreamsPerPeer:
        options.limits?.maxStreamsPerPeer ??
        DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
      requestTimeoutMs:
        options.limits?.requestTimeoutMs ??
        DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
    };
    this.now = options.now ?? (() => new Date());
    validateLimits(this.limits);
  }

  async handleCapabilities(requestCbor: Uint8Array): Promise<Buffer> {
    const request = decodeRequest(
      () => decodeDaCapabilitiesRequestV1Cbor(requestCbor),
      "capabilities request",
    );
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      throw new DaLibp2pPayloadProtocolError(
        "capabilities request deployment fingerprint mismatch",
      );
    }
    return encodeDaCapabilitiesResponseV1Cbor({
      deploymentFingerprint: this.deploymentFingerprintBytes,
      transportProtocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
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
      () => decodeDaPayloadSubmitRequestV1Cbor(requestCbor),
      "payload-submit request",
    );
    const headerHash = request.headerHash;
    const payloadHash = request.payloadHash;
    const rejected = (reasonCode: string): Buffer =>
      encodeDaPayloadSubmitResponseV1Cbor({
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
      return encodeDaPayloadSubmitResponseV1Cbor({
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
        return encodeDaPayloadSubmitResponseV1Cbor({
          status: "duplicate",
          headerHash,
          payloadHash,
          reasonCode: null,
          retryAfterMs: null,
        });
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
      () => decodeDaPayloadByHeaderRequestV1Cbor(requestCbor),
      "payload-by-header request",
    );
    const headerHash = request.headerHash;
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return encodeDaPayloadByHeaderResponseV1Cbor({
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
      return encodeDaPayloadByHeaderResponseV1Cbor(
        payloadByHeaderAbsentResponse(headerHash, resolved),
      );
    }
    if (
      request.acceptedPayloadHashes !== null &&
      !containsHash(request.acceptedPayloadHashes, resolved.payloadHash)
    ) {
      return encodeDaPayloadByHeaderResponseV1Cbor({
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
      return encodeDaPayloadByHeaderResponseV1Cbor({
        status: "found_inline",
        headerHash,
        payloadHash: resolved.payloadHash,
        payloadBytes: resolved.payloadBytes,
        chunkManifest: null,
        reasonCode: null,
      });
    }

    return encodeDaPayloadByHeaderResponseV1Cbor({
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
      () => decodeDaPayloadChunkRequestV1Cbor(requestCbor),
      "payload-chunk request",
    );
    const headerHash = request.headerHash;
    const payloadHash = request.payloadHash;
    const rejected = (): Buffer =>
      encodeDaPayloadChunkResponseV1Cbor({
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
    return encodeDaPayloadChunkResponseV1Cbor({
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
      () => decodeDaPayloadByHeaderRequestV1Cbor(requestCbor),
      "metadata-by-header request",
    );
    const headerHash = request.headerHash;
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return encodeDaMetadataByHeaderResponseV1Cbor({
        ...emptyMetadataResponse(headerHash),
        status: "rejected",
      });
    }

    const resolved = await this.resolveStoredPayload(headerHash);
    if (resolved.kind !== "found") {
      return encodeDaMetadataByHeaderResponseV1Cbor(
        metadataAbsentResponse(headerHash, resolved),
      );
    }
    if (
      request.acceptedPayloadHashes !== null &&
      !containsHash(request.acceptedPayloadHashes, resolved.payloadHash)
    ) {
      return encodeDaMetadataByHeaderResponseV1Cbor({
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
    return encodeDaMetadataByHeaderResponseV1Cbor(metadata);
  }

  private validateSubmitManifest(request: DaPayloadSubmitRequestV1):
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
    request: DaPayloadSubmitRequestV1,
    payloadBytes: Buffer,
  ): Promise<
    | { readonly ok: true; readonly admission: RetainedDaPayloadAdmissionV1 }
    | { readonly ok: false; readonly reasonCode: string }
  > {
    if (payloadBytes.length === 0) {
      return { ok: false, reasonCode: "empty_payload" };
    }
    if (payloadBytes.length > this.limits.maxPayloadBytes) {
      return { ok: false, reasonCode: "payload_too_large" };
    }
    if (request.payloadSchemaVersion !== Number(SDK.DA_PAYLOAD_V1_VERSION)) {
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
      await unwrapDaPayloadV1(payloadBytes, {
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

  private chunkManifestFor(payloadBytes: Buffer): DaPayloadChunkManifestV1 {
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

type StoredPayloadResolution =
  | { readonly kind: "missing" }
  | { readonly kind: "conflict"; readonly payloadHash: Buffer | null }
  | { readonly kind: "invalid"; readonly reasonCode: string }
  | {
      readonly kind: "found";
      readonly record: DaPayloadRecord;
      readonly payloadBytes: Buffer;
      readonly payloadHash: Buffer;
    };

const decodeRequest = <T>(decode: () => T, label: string): T => {
  try {
    return decode();
  } catch (cause) {
    throw new DaLibp2pPayloadProtocolError(`invalid ${label}`, { cause });
  }
};

const validateLimits = (limits: DaLibp2pPayloadProtocolLimits): void => {
  if (
    !Number.isSafeInteger(limits.maxPayloadBytes) ||
    limits.maxPayloadBytes <= 0
  ) {
    throw new Error("maxPayloadBytes must be a positive safe integer");
  }
  if (
    !Number.isSafeInteger(limits.maxInlineResponseBytes) ||
    limits.maxInlineResponseBytes < 0
  ) {
    throw new Error(
      "maxInlineResponseBytes must be a non-negative safe integer",
    );
  }
  if (
    !Number.isSafeInteger(limits.maxChunkBytes) ||
    limits.maxChunkBytes <= 0
  ) {
    throw new Error("maxChunkBytes must be a positive safe integer");
  }
  if (limits.maxChunkBytes > limits.maxPayloadBytes) {
    throw new Error("maxChunkBytes must not exceed maxPayloadBytes");
  }
  if (
    !Number.isSafeInteger(limits.maxStreamsPerPeer) ||
    limits.maxStreamsPerPeer <= 0
  ) {
    throw new Error("maxStreamsPerPeer must be a positive safe integer");
  }
  if (
    !Number.isSafeInteger(limits.requestTimeoutMs) ||
    limits.requestTimeoutMs <= 0
  ) {
    throw new Error("requestTimeoutMs must be a positive safe integer");
  }
};

const validateChunkManifest = (
  manifest: DaPayloadChunkManifestV1,
  limits: DaLibp2pPayloadProtocolLimits,
):
  | { readonly ok: true }
  | { readonly ok: false; readonly reasonCode: string } => {
  if (manifest.totalBytes === 0) {
    return { ok: false, reasonCode: "empty_payload" };
  }
  if (manifest.totalBytes > limits.maxPayloadBytes) {
    return { ok: false, reasonCode: "payload_too_large" };
  }
  if (manifest.chunkSize === 0) {
    return { ok: false, reasonCode: "zero_chunk_size" };
  }
  if (manifest.chunkSize > limits.maxChunkBytes) {
    return { ok: false, reasonCode: "chunk_too_large" };
  }
  const expectedChunkCount = Math.ceil(
    manifest.totalBytes / manifest.chunkSize,
  );
  if (manifest.chunkHashes.length !== expectedChunkCount) {
    return { ok: false, reasonCode: "chunk_count_mismatch" };
  }
  return { ok: true };
};

const encodeSubmitAccepted = (
  headerHash: Buffer,
  payloadHash: Buffer,
): Buffer =>
  encodeDaPayloadSubmitResponseV1Cbor({
    status: "accepted",
    headerHash,
    payloadHash,
    reasonCode: null,
    retryAfterMs: null,
  });

const encodeSubmitConflict = (
  headerHash: Buffer,
  payloadHash: Buffer,
  reasonCode: string,
): Buffer =>
  encodeDaPayloadSubmitResponseV1Cbor({
    status: "conflict",
    headerHash,
    payloadHash,
    reasonCode,
    retryAfterMs: null,
  });

const sameStoredPayload = (
  record: DaPayloadRecord,
  payloadBytes: Buffer,
  payloadHash: string,
): boolean =>
  record.payloadSha256 === payloadHash &&
  record.payloadCborHex === payloadBytes.toString("hex");

const payloadBytesFromRecord = (record: DaPayloadRecord): Buffer | null => {
  try {
    return hexToBytes(record.payloadCborHex, "stored payload CBOR");
  } catch {
    return null;
  }
};

const payloadByHeaderAbsentResponse = (
  headerHash: Buffer,
  resolution: Exclude<StoredPayloadResolution, { readonly kind: "found" }>,
): DaPayloadByHeaderResponseV1 => {
  switch (resolution.kind) {
    case "missing":
      return {
        status: "not_found",
        headerHash,
        payloadHash: null,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: null,
      };
    case "conflict":
      return {
        status: "conflict",
        headerHash,
        payloadHash: resolution.payloadHash,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: "stored_conflict",
      };
    case "invalid":
      return {
        status: "rejected",
        headerHash,
        payloadHash: null,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: resolution.reasonCode,
      };
  }
};

const encodePayloadChunkNotFound = (
  headerHash: Buffer,
  payloadHash: Buffer,
  chunkIndex: number,
): Buffer =>
  encodeDaPayloadChunkResponseV1Cbor({
    status: "not_found",
    headerHash,
    payloadHash,
    chunkIndex,
    chunkBytes: null,
    chunkHash: null,
  });

const emptyMetadataResponse = (
  headerHash: Buffer,
): Omit<DaMetadataByHeaderResponseV1, "status"> => ({
  headerHash,
  payloadHash: null,
  payloadSchemaVersion: null,
  payloadBytes: null,
  rootSummaryHash: null,
  proofBundleHash: null,
  transitionTraceRoot: null,
  eventToStepRoot: null,
  retainedUntilSlot: null,
  localStatus: null,
});

const metadataAbsentResponse = (
  headerHash: Buffer,
  resolution: Exclude<StoredPayloadResolution, { readonly kind: "found" }>,
): DaMetadataByHeaderResponseV1 => {
  switch (resolution.kind) {
    case "missing":
      return {
        ...emptyMetadataResponse(headerHash),
        status: "not_found",
      };
    case "conflict":
      return {
        ...emptyMetadataResponse(headerHash),
        status: "conflict",
        payloadHash: resolution.payloadHash,
      };
    case "invalid":
      return {
        ...emptyMetadataResponse(headerHash),
        status: "rejected",
      };
  }
};

const metadataForPayload = async (
  headerHash: Buffer,
  payloadHash: Buffer,
  payloadBytes: Buffer,
  record: DaPayloadRecord,
): Promise<DaMetadataByHeaderResponseV1> => {
  if (record.payloadSchemaVersion !== Number(SDK.DA_PAYLOAD_V1_VERSION)) {
    throw new DaLibp2pPayloadProtocolError(
      "stored payload schema version is not canonical V1",
    );
  }
  await unwrapDaPayloadV1(payloadBytes, {
    maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
  });
  // Metadata establishes only that the retained bytes are a valid bounded
  // outer envelope.  Inner DA parsing and header/root validation stay in the
  // watcher so a fetched record cannot be mistaken for a verified payload.
  return {
    status: "found",
    headerHash,
    payloadHash,
    payloadSchemaVersion: 1,
    payloadBytes: payloadBytes.length,
    rootSummaryHash:
      record.rootSummary === undefined
        ? null
        : rootSummaryHash(record.rootSummary),
    proofBundleHash: null,
    transitionTraceRoot:
      record.rootSummary === undefined
        ? null
        : hexToBytes(
            record.rootSummary.transitionTraceRoot,
            "transition trace root",
            32,
          ),
    eventToStepRoot:
      record.rootSummary === undefined
        ? null
        : hexToBytes(
            record.rootSummary.eventToStepRoot,
            "event to step root",
            32,
          ),
    retainedUntilSlot: null,
    localStatus: localPayloadStatus(record),
  };
};

const localPayloadStatus = (
  record: DaPayloadRecord,
): DaMetadataByHeaderResponseV1["localStatus"] => {
  switch (record.validationStatus) {
    case "verified":
      return "verified";
    case "conflicted":
      return "conflict";
    case "fetched":
    case "missing_da":
    case "malformed_da":
    case "root_mismatch":
      return "staged";
  }
};

const chunkManifestFor = (
  payloadBytes: Buffer,
  chunkSize: number,
): DaPayloadChunkManifestV1 => {
  const chunkHashes: Buffer[] = [];
  for (let offset = 0; offset < payloadBytes.length; offset += chunkSize) {
    chunkHashes.push(
      computeDaSha256Hash(payloadBytes.subarray(offset, offset + chunkSize)),
    );
  }
  return {
    payloadHash: computeDaSha256Hash(payloadBytes),
    totalBytes: payloadBytes.length,
    chunkSize,
    chunkHashes,
  };
};

const rootSummaryHash = (rootSummary: PayloadRootSet): Buffer =>
  computeDaSha256Hash(
    Buffer.concat([
      hexToBytes(rootSummary.utxosRoot, "utxos root", 32),
      hexToBytes(rootSummary.withdrawalsRoot, "withdrawals root", 32),
      hexToBytes(
        rootSummary.forcedTransactionsRoot,
        "forced transactions root",
        32,
      ),
      hexToBytes(rootSummary.transactionsRoot, "transactions root", 32),
      hexToBytes(rootSummary.depositsRoot, "deposits root", 32),
      hexToBytes(rootSummary.transitionTraceRoot, "transition trace root", 32),
      hexToBytes(rootSummary.eventToStepRoot, "event to step root", 32),
    ]),
  );

const containsHash = (hashes: readonly Buffer[], target: Buffer): boolean =>
  hashes.some((hash) => hash.equals(target));

const optionalHash = (value: string): Buffer | null => {
  try {
    return hexToBytes(value, "payload hash", 32);
  } catch {
    return null;
  }
};

const normalizeHashOrEmpty = (value: string): string => {
  try {
    return normalizeHex(value, { fieldName: "payload hash", byteLength: 32 });
  } catch {
    return "";
  }
};

export const daPayloadHashHex = (payloadBytes: Uint8Array): string =>
  bytesToHex(computeDaSha256Hash(payloadBytes));
