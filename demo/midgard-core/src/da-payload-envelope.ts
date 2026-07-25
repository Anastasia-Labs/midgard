import { sha256 } from "@noble/hashes/sha2.js";

import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
} from "./codec/cbor.js";
import {
  compressDaPayloadZstd,
  decompressDaPayloadZstd,
} from "./da-compression.js";

export const DA_PAYLOAD_ENVELOPE_V1_VERSION = 1 as const;
export const DA_PAYLOAD_INNER_V1_SCHEMA_VERSION = 1 as const;

export const DaPayloadContentEncoding = {
  identity: 0,
  zstd: 1,
} as const;

export type DaPayloadContentEncoding =
  (typeof DaPayloadContentEncoding)[keyof typeof DaPayloadContentEncoding];

export type DaPayloadEnvelopeV1 = {
  readonly version: typeof DA_PAYLOAD_ENVELOPE_V1_VERSION;
  readonly contentEncoding: DaPayloadContentEncoding;
  readonly innerBytes: number;
  readonly innerSha256: Buffer;
  readonly body: Buffer;
};

export type DaPayloadEnvelopeMode = "identity" | "zstd";

export type DaPayloadEnvelopeTimingStage =
  | "envelope_parse"
  | "envelope_decompress"
  | "inner_hash";

export type DaPayloadEnvelopeTimingOptions = {
  readonly monotonicNow?: () => number;
  readonly onStageTiming?: (
    stage: DaPayloadEnvelopeTimingStage,
    durationMs: number,
  ) => void;
};

export type UnwrappedDaPayloadV1 = {
  readonly schemaVersion: typeof DA_PAYLOAD_INNER_V1_SCHEMA_VERSION;
  readonly innerBytes: Buffer;
  readonly storedBytes: Buffer;
  readonly contentEncoding: DaPayloadContentEncoding;
};

export type DaPayloadEnvelopeReasonCode =
  | "payload_too_large"
  | "empty_payload"
  | "malformed_envelope"
  | "wrong_envelope_version"
  | "unknown_content_encoding"
  | "declared_inner_too_large"
  | "decompression_failed"
  | "inner_length_mismatch"
  | "inner_hash_mismatch";

export class DaPayloadEnvelopeError extends Error {
  readonly reasonCode: DaPayloadEnvelopeReasonCode;
  readonly cause?: unknown;

  constructor(
    reasonCode: DaPayloadEnvelopeReasonCode,
    message: string,
    options?: { readonly cause?: unknown },
  ) {
    super(message);
    this.name = "DaPayloadEnvelopeError";
    this.reasonCode = reasonCode;
    if (options?.cause !== undefined) {
      this.cause = options.cause;
    }
  }
}

const fail = (
  reasonCode: DaPayloadEnvelopeReasonCode,
  message: string,
  options?: { readonly cause?: unknown },
): never => {
  throw new DaPayloadEnvelopeError(reasonCode, message, options);
};

const exactSafePositiveNumber = (value: unknown, fieldName: string): number => {
  const parsed = asBigInt(value, fieldName);
  if (parsed <= 0n || parsed > BigInt(Number.MAX_SAFE_INTEGER)) {
    return fail(
      "declared_inner_too_large",
      `${fieldName} must be a positive safe integer`,
    );
  }
  return Number(parsed);
};

const hash = (bytes: Uint8Array): Buffer => Buffer.from(sha256(bytes));

const asBufferView = (bytes: Uint8Array): Buffer =>
  Buffer.isBuffer(bytes)
    ? bytes
    : Buffer.from(bytes.buffer, bytes.byteOffset, bytes.byteLength);

const readMonotonicNow = (
  timing: DaPayloadEnvelopeTimingOptions,
): number | undefined => {
  try {
    return (timing.monotonicNow ?? (() => performance.now()))();
  } catch {
    return undefined;
  }
};

const recordTiming = (
  timing: DaPayloadEnvelopeTimingOptions,
  stage: DaPayloadEnvelopeTimingStage,
  startedAt: number | undefined,
): void => {
  if (startedAt === undefined) return;
  const completedAt = readMonotonicNow(timing);
  if (completedAt === undefined) return;
  try {
    timing.onStageTiming?.(stage, completedAt - startedAt);
  } catch {
    // Observability must not change payload validation semantics.
  }
};

const encodeValue = (envelope: DaPayloadEnvelopeV1): Buffer =>
  encodeCbor([
    BigInt(envelope.version),
    BigInt(envelope.contentEncoding),
    BigInt(envelope.innerBytes),
    envelope.innerSha256,
    envelope.body,
  ]);

export const encodeDaPayloadEnvelopeV1 = (
  envelope: DaPayloadEnvelopeV1,
): Buffer => encodeValue(envelope);

export const decodeDaPayloadEnvelopeV1 = (
  bytes: Uint8Array,
  timing: DaPayloadEnvelopeTimingOptions = {},
): DaPayloadEnvelopeV1 => {
  const startedAt = readMonotonicNow(timing);
  try {
    let decoded: unknown;
    try {
      decoded = decodeSingleCbor(bytes);
    } catch (cause) {
      return fail(
        "malformed_envelope",
        "failed to decode DA payload envelope",
        {
          cause,
        },
      );
    }
    let fields: unknown[];
    try {
      fields = asArray(decoded, "DaPayloadEnvelopeV1");
    } catch (cause) {
      return fail(
        "malformed_envelope",
        "DA payload envelope must be an array",
        {
          cause,
        },
      );
    }
    if (fields.length !== 5) {
      return fail(
        "malformed_envelope",
        `DA payload envelope must contain 5 fields, got ${fields.length.toString()}`,
      );
    }
    let envelope: DaPayloadEnvelopeV1;
    try {
      const version = Number(asBigInt(fields[0], "envelope.version"));
      if (version !== DA_PAYLOAD_ENVELOPE_V1_VERSION) {
        return fail(
          "wrong_envelope_version",
          `expected DA payload envelope version 1, got ${version.toString()}`,
        );
      }
      const contentEncoding = Number(
        asBigInt(fields[1], "envelope.content_encoding"),
      );
      if (
        contentEncoding !== DaPayloadContentEncoding.identity &&
        contentEncoding !== DaPayloadContentEncoding.zstd
      ) {
        return fail(
          "unknown_content_encoding",
          `unknown DA payload content encoding ${contentEncoding.toString()}`,
        );
      }
      const innerBytes = exactSafePositiveNumber(
        fields[2],
        "envelope.inner_bytes",
      );
      const innerSha256 = asBufferView(
        asBytes(fields[3], "envelope.inner_sha256"),
      );
      if (innerSha256.length !== 32) {
        return fail(
          "malformed_envelope",
          `envelope.inner_sha256 must be 32 bytes, got ${innerSha256.length.toString()}`,
        );
      }
      const body = asBufferView(asBytes(fields[4], "envelope.body"));
      envelope = {
        version: DA_PAYLOAD_ENVELOPE_V1_VERSION,
        contentEncoding,
        innerBytes,
        innerSha256,
        body,
      };
    } catch (cause) {
      if (cause instanceof DaPayloadEnvelopeError) {
        throw cause;
      }
      return fail(
        "malformed_envelope",
        "DA payload envelope is not canonical",
        {
          cause,
        },
      );
    }
    return envelope;
  } finally {
    recordTiming(timing, "envelope_parse", startedAt);
  }
};

export const wrapDaPayloadV1 = async (
  innerBytes: Uint8Array,
  {
    mode,
    zstdLevel = 3,
  }: {
    readonly mode: DaPayloadEnvelopeMode;
    readonly zstdLevel?: number;
  },
): Promise<Buffer> => {
  const inner = Buffer.from(innerBytes);
  if (inner.length === 0) {
    return fail("empty_payload", "cannot wrap an empty DA payload");
  }
  const contentEncoding =
    mode === "identity"
      ? DaPayloadContentEncoding.identity
      : DaPayloadContentEncoding.zstd;
  const body =
    mode === "identity" ? inner : await compressDaPayloadZstd(inner, zstdLevel);
  return encodeDaPayloadEnvelopeV1({
    version: DA_PAYLOAD_ENVELOPE_V1_VERSION,
    contentEncoding,
    innerBytes: inner.length,
    innerSha256: hash(inner),
    body,
  });
};

export const unwrapDaPayloadV1 = async (
  bytes: Uint8Array,
  {
    maxPayloadBytes,
    decompress = decompressDaPayloadZstd,
    timing = {},
  }: {
    readonly maxPayloadBytes: number;
    readonly decompress?: (
      bytes: Uint8Array,
      maxOutputLength: number,
    ) => Promise<Buffer>;
    readonly timing?: DaPayloadEnvelopeTimingOptions;
  },
): Promise<UnwrappedDaPayloadV1> => {
  const storedBytes = Buffer.from(bytes);
  if (storedBytes.length === 0) {
    return fail("empty_payload", "DA payload is empty");
  }
  if (storedBytes.length > maxPayloadBytes) {
    return fail(
      "payload_too_large",
      `DA payload bytes ${storedBytes.length.toString()} exceed ${maxPayloadBytes.toString()}`,
    );
  }
  const envelope = decodeDaPayloadEnvelopeV1(storedBytes, timing);
  if (envelope.innerBytes > maxPayloadBytes) {
    return fail(
      "declared_inner_too_large",
      `declared inner bytes ${envelope.innerBytes.toString()} exceed ${maxPayloadBytes.toString()}`,
    );
  }
  let innerBytes: Buffer;
  if (envelope.contentEncoding === DaPayloadContentEncoding.identity) {
    innerBytes = envelope.body;
  } else {
    const decompressStartedAt = readMonotonicNow(timing);
    try {
      innerBytes = await decompress(envelope.body, envelope.innerBytes);
    } catch (cause) {
      return fail(
        "decompression_failed",
        "zstd DA payload decompression failed within declared output limit",
        { cause },
      );
    } finally {
      recordTiming(timing, "envelope_decompress", decompressStartedAt);
    }
  }
  if (innerBytes.length !== envelope.innerBytes) {
    return fail(
      "inner_length_mismatch",
      `DA payload inner length ${innerBytes.length.toString()} does not match declared ${envelope.innerBytes.toString()}`,
    );
  }
  const hashStartedAt = readMonotonicNow(timing);
  const innerHashMatches = hash(innerBytes).equals(envelope.innerSha256);
  recordTiming(timing, "inner_hash", hashStartedAt);
  if (!innerHashMatches) {
    return fail("inner_hash_mismatch", "DA payload inner SHA-256 mismatch");
  }
  return {
    schemaVersion: DA_PAYLOAD_INNER_V1_SCHEMA_VERSION,
    innerBytes,
    storedBytes,
    contentEncoding: envelope.contentEncoding,
  };
};
