import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
  daDeploymentFingerprintFromHex,
  type DaMetadataByHeaderResponseV1,
  type DaPayloadByHeaderResponseV1,
  type DaPayloadChunkManifestV1,
  type DaPayloadSubmitRequestV1,
  decodeDaPayloadByHeaderRequestV1Cbor,
  decodeDaPayloadChunkRequestV1Cbor,
  decodeDaPayloadSubmitRequestV1Cbor,
  encodeDaMetadataByHeaderResponseV1Cbor,
  encodeDaPayloadByHeaderResponseV1Cbor,
  encodeDaPayloadChunkResponseV1Cbor,
  encodeDaPayloadSubmitResponseV1Cbor,
  normalizeDaDeploymentFingerprintHex,
} from "@al-ft/midgard-core/da-transport";

import type { DaPayloadRecord, PayloadRootSet } from "../../domain.js";
import {
  hasPayloadBytes,
  libp2pSubmittedDaPayloadRecord,
  type WatcherStore,
} from "../../store.js";
import { bytesToHex, hexToBytes, normalizeHex } from "../../utils/hex.js";
import { decodeDaPayloadV2Strict } from "../payload.js";

export type DaLibp2pPayloadProtocolStore = Pick<
  WatcherStore,
  "getDaPayload" | "saveDaPayload"
>;

export type DaLibp2pPayloadProtocolLimits = {
  readonly maxPayloadBytes: number;
  readonly maxInlineResponseBytes: number;
  readonly maxChunkBytes: number;
};

export type DaLibp2pPayloadProtocolHandlersOptions = {
  readonly deploymentFingerprint: string | Uint8Array;
  readonly store: DaLibp2pPayloadProtocolStore;
  readonly limits?: Partial<DaLibp2pPayloadProtocolLimits>;
  readonly now?: () => Date;
};

export class DaLibp2pPayloadProtocolError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "DaLibp2pPayloadProtocolError";
  }
}

export class DaLibp2pPayloadProtocolHandlers {
  private readonly deploymentFingerprint: string;
  private readonly deploymentFingerprintBytes: Buffer;
  private readonly limits: DaLibp2pPayloadProtocolLimits;
  private readonly now: () => Date;
  private readonly store: DaLibp2pPayloadProtocolStore;

  constructor(options: DaLibp2pPayloadProtocolHandlersOptions) {
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
    };
    this.now = options.now ?? (() => new Date());
    validateLimits(this.limits);
  }

  async handlePayloadSubmit(requestCbor: Uint8Array): Promise<Buffer> {
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
    const checked = this.checkInlineSubmitPayload(request, payloadBytes);
    if (!checked.ok) {
      return rejected(checked.reasonCode);
    }

    const headerHashHex = headerHash.toString("hex");
    const payloadHashHex = payloadHash.toString("hex");
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
      const saved = await this.saveInlinePayload(
        headerHashHex,
        payloadHashHex,
        payloadBytes,
      );
      return saved.validationStatus === "conflicted"
        ? encodeSubmitConflict(
            headerHash,
            payloadHash,
            "conflicting_payload_bytes",
          )
        : encodeSubmitAccepted(headerHash, payloadHash);
    }

    const saved = await this.saveInlinePayload(
      headerHashHex,
      payloadHashHex,
      payloadBytes,
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

    const metadata = metadataForPayload(
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

  private checkInlineSubmitPayload(
    request: DaPayloadSubmitRequestV1,
    payloadBytes: Buffer,
  ):
    | { readonly ok: true }
    | { readonly ok: false; readonly reasonCode: string } {
    if (payloadBytes.length === 0) {
      return { ok: false, reasonCode: "empty_payload" };
    }
    if (payloadBytes.length > this.limits.maxPayloadBytes) {
      return { ok: false, reasonCode: "payload_too_large" };
    }
    const actualPayloadHash = computeDaSha256Hash(payloadBytes);
    if (!actualPayloadHash.equals(request.payloadHash)) {
      return { ok: false, reasonCode: "payload_hash_mismatch" };
    }
    try {
      const payload = decodeDaPayloadV2Strict(payloadBytes);
      if (
        payload.block_body.header_hash !== request.headerHash.toString("hex")
      ) {
        return { ok: false, reasonCode: "payload_header_hash_mismatch" };
      }
      if (Number(payload.version) !== request.payloadSchemaVersion) {
        return { ok: false, reasonCode: "payload_schema_version_mismatch" };
      }
    } catch (cause) {
      return {
        ok: false,
        reasonCode:
          cause instanceof Error && cause.name === "DaPayloadValidationError"
            ? "malformed_payload"
            : "payload_decode_failed",
      };
    }
    return { ok: true };
  }

  private async saveInlinePayload(
    headerHash: string,
    payloadHash: string,
    payloadBytes: Buffer,
  ): Promise<DaPayloadRecord> {
    return this.store.saveDaPayload(
      libp2pSubmittedDaPayloadRecord({
        deploymentFingerprint: this.deploymentFingerprint,
        headerHash,
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

const metadataForPayload = (
  headerHash: Buffer,
  payloadHash: Buffer,
  payloadBytes: Buffer,
  record: DaPayloadRecord,
): DaMetadataByHeaderResponseV1 => {
  const payload = decodeDaPayloadV2Strict(payloadBytes);
  return {
    status: "found",
    headerHash,
    payloadHash,
    payloadSchemaVersion: Number(payload.version),
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
