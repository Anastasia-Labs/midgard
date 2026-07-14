import { DA_TRANSPORT_LIMITS_V1 } from "./da-transport.js";

export type DaPayloadEmissionMode = "off" | "identity" | "zstd";

const assertSafeLength = (value: number, fieldName: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new RangeError(`${fieldName} must be a non-negative safe integer`);
  }
  return value;
};

/** Canonical-CBOR argument bytes for a non-negative integer/length. */
export const canonicalCborArgumentSize = (value: number): number => {
  const length = assertSafeLength(value, "CBOR argument");
  if (length < 24) return 1;
  if (length <= 0xff) return 2;
  if (length <= 0xffff) return 3;
  if (length <= 0xffff_ffff) return 5;
  return 9;
};

export const canonicalCborByteStringSize = (byteLength: number): number => {
  const length = assertSafeLength(byteLength, "CBOR byte string length");
  return canonicalCborArgumentSize(length) + length;
};

/**
 * Mirrors ZSTD_COMPRESSBOUND from zstd.h for the safe-integer payload domain.
 * This is a worst-case single-frame bound, not an expected compression ratio.
 */
export const zstdCompressBound = (sourceBytes: number): number => {
  const length = assertSafeLength(sourceBytes, "zstd source length");
  const smallInputMargin =
    length < 128 * 1024 ? Math.floor((128 * 1024 - length) / 2048) : 0;
  const result = length + Math.floor(length / 256) + smallInputMargin;
  if (!Number.isSafeInteger(result)) {
    throw new RangeError("zstd compression bound exceeds safe integer range");
  }
  return result;
};

/** Exact canonical-CBOR bytes for DaPayloadEnvelopeV3. */
export const daPayloadEnvelopeV3EncodedSize = ({
  innerBytes,
  bodyBytes,
}: {
  readonly innerBytes: number;
  readonly bodyBytes: number;
}): number =>
  1 + // fixed array(5)
  1 + // version=3
  1 + // content encoding
  canonicalCborArgumentSize(innerBytes) +
  canonicalCborByteStringSize(32) +
  canonicalCborByteStringSize(bodyBytes);

/** Exact canonical-CBOR body bytes for an inline DaPayloadSubmitRequestV1. */
export const daPayloadSubmitV1EncodedSize = ({
  payloadBytes,
  payloadSchemaVersion,
}: {
  readonly payloadBytes: number;
  readonly payloadSchemaVersion: 2 | 3;
}): number => {
  if (payloadSchemaVersion !== 2 && payloadSchemaVersion !== 3) {
    throw new RangeError("DA payload schema version must be 2 or 3");
  }
  return (
    1 + // fixed array(7)
    canonicalCborByteStringSize(32) + // deployment fingerprint
    canonicalCborByteStringSize(28) + // header hash
    canonicalCborByteStringSize(32) + // payload hash
    canonicalCborArgumentSize(payloadSchemaVersion) +
    1 + // inline enum=0
    canonicalCborByteStringSize(payloadBytes) +
    1 // null chunk manifest
  );
};

export type DaPayloadV1SizeProjection = {
  readonly mode: DaPayloadEmissionMode;
  readonly schemaVersion: 2 | 3;
  readonly innerBytes: number;
  readonly storedBytesUpperBound: number;
  readonly requestBytesUpperBound: number;
};

export const projectDaPayloadV1Sizes = (
  innerBytes: number,
  mode: DaPayloadEmissionMode,
): DaPayloadV1SizeProjection => {
  const inner = assertSafeLength(innerBytes, "DA payload inner bytes");
  const schemaVersion = mode === "off" ? 2 : 3;
  const storedBytesUpperBound =
    mode === "off"
      ? inner
      : daPayloadEnvelopeV3EncodedSize({
          innerBytes: inner,
          bodyBytes: mode === "identity" ? inner : zstdCompressBound(inner),
        });
  return {
    mode,
    schemaVersion,
    innerBytes: inner,
    storedBytesUpperBound,
    requestBytesUpperBound: daPayloadSubmitV1EncodedSize({
      payloadBytes: storedBytesUpperBound,
      payloadSchemaVersion: schemaVersion,
    }),
  };
};

/**
 * Maximum inner DaPayloadV2 bytes that provably fit both the stored-artifact
 * and inline-request V1 bounds for the selected emission mode.
 */
export const maxDaPayloadV1InnerBytes = (
  mode: DaPayloadEmissionMode,
  frameLimit = DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
): number => {
  const limit = assertSafeLength(frameLimit, "DA V1 frame limit");
  let low = 0;
  let high = limit;
  while (low < high) {
    const candidate = Math.ceil((low + high) / 2);
    const projection = projectDaPayloadV1Sizes(candidate, mode);
    if (
      projection.innerBytes <= limit &&
      projection.storedBytesUpperBound <= limit &&
      projection.requestBytesUpperBound <= limit
    ) {
      low = candidate;
    } else {
      high = candidate - 1;
    }
  }
  return low;
};
