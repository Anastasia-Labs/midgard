import {
  DaPayloadEnvelopeError,
  unwrapDaPayload,
} from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  type DaMetadataByHeaderResponse,
  type DaPayloadChunkManifest,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type { DaPayloadRecord } from "../../domain.js";
import { hexToBytes, normalizeHex } from "../../utils/hex.js";
import {
  localPayloadStatus,
  metadataAbsentResponse,
  rootSummaryHash,
} from "./payload-protocols.validate-limits.js";

export const metadataForPayload = async (
  headerHash: Buffer,
  payloadHash: Buffer,
  payloadBytes: Buffer,
  record: DaPayloadRecord,
): Promise<DaMetadataByHeaderResponse> => {
  // Metadata establishes only that the retained bytes are a valid bounded
  // outer envelope.  Inner DA parsing and header/root validation stay in the
  // watcher so a fetched record cannot be mistaken for a verified payload.
  // Bytes that fail either check are answered as rejected, never by aborting
  // the stream.
  const unservable = metadataAbsentResponse(headerHash, {
    kind: "invalid",
    reasonCode: "stored_payload_bytes_malformed",
  });
  if (record.payloadSchemaVersion !== Number(SDK.DA_PAYLOAD_VERSION)) {
    return unservable;
  }
  try {
    await unwrapDaPayload(payloadBytes, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
  } catch (cause) {
    if (cause instanceof DaPayloadEnvelopeError) {
      return unservable;
    }
    throw cause;
  }
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

export const chunkManifestFor = (
  payloadBytes: Buffer,
  chunkSize: number,
): DaPayloadChunkManifest => {
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

export const containsHash = (
  hashes: readonly Buffer[],
  target: Buffer,
): boolean => hashes.some((hash) => hash.equals(target));

export const optionalHash = (value: string): Buffer | null => {
  try {
    return hexToBytes(value, "payload hash", 32);
  } catch {
    return null;
  }
};

export const normalizeHashOrEmpty = (value: string): string => {
  try {
    return normalizeHex(value, { fieldName: "payload hash", byteLength: 32 });
  } catch {
    return "";
  }
};
