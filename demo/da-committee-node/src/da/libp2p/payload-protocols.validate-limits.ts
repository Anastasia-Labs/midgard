import {
  computeDaSha256Hash,
  type DaMetadataByHeaderResponse,
  type DaPayloadByHeaderResponse,
  type DaPayloadChunkManifest,
  encodeDaPayloadChunkResponseCbor,
  encodeDaPayloadSubmitResponseCbor,
} from "@al-ft/midgard-core/da-transport";

import type { DaPayloadRecord, PayloadRootSet } from "../../domain.js";
import { type CommitteeStore } from "../../store.js";
import { hexToBytes } from "../../utils/hex.js";

export type DaLibp2pPayloadProtocolStore = Pick<
  CommitteeStore,
  "getDaPayload" | "saveDaPayload"
>;

/** Read-only authority required by the public retained-DA listener. */
export type DaLibp2pPublicRetainedDaPayloadStore = Pick<
  CommitteeStore,
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
  /**
   * Where a refused payload-submit over a settled record is reported.
   * Defaults to `console.warn`.
   */
  readonly log?: (message: string) => void;
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
export type RetainedDaPayloadAdmission = {
  readonly payloadSchemaVersion: 1;
  readonly rawEnvelopeSha256: Buffer;
};

export class DaLibp2pPayloadProtocolError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "DaLibp2pPayloadProtocolError";
  }
}

export type StoredPayloadResolution =
  | { readonly kind: "missing" }
  | { readonly kind: "conflict"; readonly payloadHash: Buffer | null }
  | { readonly kind: "invalid"; readonly reasonCode: string }
  | {
      readonly kind: "found";
      readonly record: DaPayloadRecord;
      readonly payloadBytes: Buffer;
      readonly payloadHash: Buffer;
    };

export const decodeRequest = <T>(decode: () => T, label: string): T => {
  try {
    return decode();
  } catch (cause) {
    throw new DaLibp2pPayloadProtocolError(`invalid ${label}`, { cause });
  }
};

export const validateLimits = (limits: DaLibp2pPayloadProtocolLimits): void => {
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

export const validateChunkManifest = (
  manifest: DaPayloadChunkManifest,
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

export const encodeSubmitAccepted = (
  headerHash: Buffer,
  payloadHash: Buffer,
): Buffer =>
  encodeDaPayloadSubmitResponseCbor({
    status: "accepted",
    headerHash,
    payloadHash,
    reasonCode: null,
    retryAfterMs: null,
  });

export const encodeSubmitConflict = (
  headerHash: Buffer,
  payloadHash: Buffer,
  reasonCode: string,
): Buffer =>
  encodeDaPayloadSubmitResponseCbor({
    status: "conflict",
    headerHash,
    payloadHash,
    reasonCode,
    retryAfterMs: null,
  });

export const sameStoredPayload = (
  record: DaPayloadRecord,
  payloadBytes: Buffer,
  payloadHash: string,
): boolean =>
  record.payloadSha256 === payloadHash &&
  record.payloadCborHex === payloadBytes.toString("hex");

/**
 * Whether a stored record has already been judged against its header.  A
 * payload-submit never writes over such a record: the verified bytes are the
 * ones attestation and availability answers depend on, and a rejected record
 * keeps its verdict rather than being replaced by bytes nothing has checked.
 */
export const isSettledPayload = (record: DaPayloadRecord): boolean =>
  record.validationStatus === "verified" ||
  record.validationStatus === "malformed_da" ||
  record.validationStatus === "root_mismatch";

export const payloadBytesFromRecord = (
  record: DaPayloadRecord,
): Buffer | null => {
  try {
    return hexToBytes(record.payloadCborHex, "stored payload CBOR");
  } catch {
    return null;
  }
};

export const payloadByHeaderAbsentResponse = (
  headerHash: Buffer,
  resolution: Exclude<StoredPayloadResolution, { readonly kind: "found" }>,
): DaPayloadByHeaderResponse => {
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

export const encodePayloadChunkNotFound = (
  headerHash: Buffer,
  payloadHash: Buffer,
  chunkIndex: number,
): Buffer =>
  encodeDaPayloadChunkResponseCbor({
    status: "not_found",
    headerHash,
    payloadHash,
    chunkIndex,
    chunkBytes: null,
    chunkHash: null,
  });

export const emptyMetadataResponse = (
  headerHash: Buffer,
): Omit<DaMetadataByHeaderResponse, "status"> => ({
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

export const metadataAbsentResponse = (
  headerHash: Buffer,
  resolution: Exclude<StoredPayloadResolution, { readonly kind: "found" }>,
): DaMetadataByHeaderResponse => {
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

export const localPayloadStatus = (
  record: DaPayloadRecord,
): DaMetadataByHeaderResponse["localStatus"] => {
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

export const rootSummaryHash = (rootSummary: PayloadRootSet): Buffer =>
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
