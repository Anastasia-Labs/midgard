import { CML } from "@lucid-evolution/lucid";

import {
  EVEN_HEX,
  HEX_28,
  HEX_32,
  type KupoPoint,
  MAX_MATCHES,
  type OgmiosTip,
} from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";

/**
 * 2160 blocks at the mainnet average of one block per 20 seconds. Kupo
 * checkpoints at least this far below its head are past the security
 * parameter and are memoized per source instance.
 */
export const IMMUTABLE_CHECKPOINT_SLOT_DISTANCE = 43_200;

export const MAX_RESPONSE_BYTES = 64 * 1024 * 1024;

export const LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS = Object.freeze({
  targetTransactions: 4_096,
  transactionReferences: 4_096,
  referenceOccurrences: 65_536,
  creatingBodies: 4_096,
  publicBytes: 1_048_576,
  targetTransactionBytes: 67_108_864,
  evidenceBytes: 67_108_864,
  responseBytes: MAX_RESPONSE_BYTES,
  inspectedMembers: MAX_MATCHES,
});

// Passed explicitly through one captured read. No ordinary operation shares
// its counters or owns entries inserted into this operation's raw cache.
export type ReferenceReadScope = {
  responseBytes: number;
  inspectedMembers: number;
  head: KupoPoint | undefined;
  readonly rawBlocks: Map<string, Promise<ReturnType<typeof parseOgmiosBlock>>>;
};

export const referenceResponseLimit = (
  configured: number,
  scope?: ReferenceReadScope,
): number => {
  if (scope === undefined) return configured;
  const remaining =
    LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.responseBytes -
    scope.responseBytes;
  if (remaining <= 0)
    throw new Error("reference acquisition response byte budget exhausted");
  return Math.min(configured, remaining);
};

export const debitReferenceResponse = (
  scope: ReferenceReadScope | undefined,
  bytes: number,
): void => {
  if (scope === undefined) return;
  if (
    !Number.isSafeInteger(bytes) ||
    bytes < 0 ||
    bytes >
      LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.responseBytes -
        scope.responseBytes
  )
    throw new Error("reference acquisition response byte budget exceeded");
  scope.responseBytes += bytes;
};

export const debitReferenceMembers = (
  scope: ReferenceReadScope | undefined,
  members: number,
): void => {
  if (scope === undefined) return;
  if (
    !Number.isSafeInteger(members) ||
    members < 0 ||
    members >
      LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.inspectedMembers -
        scope.inspectedMembers
  )
    throw new Error("reference acquisition inspected-member budget exceeded");
  scope.inspectedMembers += members;
};

export const boundedReferenceCbor = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length / 2 > LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.publicBytes
  )
    throw new Error(
      `${label} exceeds reference acquisition public-byte bounds`,
    );
  return cbor(value, label);
};

export const abortSignalAborted = Object.getOwnPropertyDescriptor(
  AbortSignal.prototype,
  "aborted",
)!.get!;

export const validateSourceSignal = (signal: AbortSignal | undefined): void => {
  if (signal === undefined) return;
  try {
    abortSignalAborted.call(signal);
  } catch {
    throw new Error("raw-source signal must be a platform AbortSignal");
  }
};

export const throwIfSourceAborted = (signal: AbortSignal | undefined): void => {
  if (signal !== undefined && abortSignalAborted.call(signal)) {
    throw new DOMException("local Kupmios raw source aborted", "AbortError");
  }
};

export const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

export const exactKeys = (
  value: unknown,
  required: readonly string[],
  optional: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const allowed = new Set([...required, ...optional]);
  if (
    required.some((key) => !(key in parsed)) ||
    Object.keys(parsed).some((key) => !allowed.has(key))
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

export const naturalNumber = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a natural safe integer`);
  }
  return value as number;
};

export const digest = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !HEX_32.test(value)) {
    throw new Error(`${label} must be 32-byte lowercase hex`);
  }
  return value;
};

export const nullableDigest = (value: unknown, label: string): string | null =>
  value === null ? null : digest(value, label);

export const nullableScriptHash = (
  value: unknown,
  label: string,
): string | null => {
  if (value === null) return null;
  if (typeof value !== "string" || !HEX_28.test(value)) {
    throw new Error(`${label} must be 28-byte lowercase hex`);
  }
  return value;
};

export const cbor = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !EVEN_HEX.test(value)) {
    throw new Error(`${label} must be non-empty lowercase CBOR hex`);
  }
  return value;
};

export const canonicalAddress = (value: unknown, label: string): string => {
  if (typeof value !== "string") {
    throw new Error(`${label} must be a Cardano address`);
  }
  try {
    if (CML.Address.from_bech32(value).to_bech32() !== value) {
      throw new Error("non-canonical address");
    }
  } catch {
    throw new Error(`${label} must be a canonical Cardano address`);
  }
  return value;
};

export const normalizeHttpUrl = (value: string): string => {
  const parsed = new URL(value.trim());
  if (parsed.protocol === "ws:") parsed.protocol = "http:";
  if (parsed.protocol === "wss:") parsed.protocol = "https:";
  parsed.hash = "";
  return parsed.toString().replace(/\/$/u, "");
};

export const normalizeWebSocketUrl = (value: string): string => {
  const parsed = new URL(value.trim());
  if (parsed.protocol === "http:") parsed.protocol = "ws:";
  if (parsed.protocol === "https:") parsed.protocol = "wss:";
  parsed.hash = "";
  return parsed.toString().replace(/\/$/u, "");
};

export const assertLoopbackUrl = (value: string, label: string): void => {
  const hostname = new URL(value).hostname.toLowerCase();
  if (
    hostname !== "127.0.0.1" &&
    hostname !== "localhost" &&
    hostname !== "::1" &&
    hostname !== "[::1]"
  ) {
    throw new Error(`${label} must be a loopback endpoint`);
  }
};

export const joinUrl = (base: string, path: string): string =>
  `${base.replace(/\/+$/u, "")}/${path.replace(/^\/+/u, "")}`;

export type JsonHttpResponse = {
  readonly value: unknown;
  readonly checkpointHeaders: KupoPoint | null;
};

export const parseOgmiosBlock = (
  value: unknown,
  label: string,
  referenceScope?: ReferenceReadScope,
): {
  readonly point: OgmiosTip;
  readonly parentBlockHash: string | null;
  readonly transactions: readonly unknown[];
} => {
  const parsed = record(value, label);
  if (!Array.isArray(parsed.transactions)) {
    throw new Error(`${label}.transactions must be an array`);
  }
  debitReferenceMembers(referenceScope, parsed.transactions.length);
  return {
    point: {
      slot: naturalNumber(parsed.slot, `${label}.slot`),
      blockHash: digest(parsed.id, `${label}.id`),
      blockNo: naturalNumber(parsed.height, `${label}.height`),
    },
    parentBlockHash:
      parsed.ancestor === "genesis"
        ? null
        : digest(parsed.ancestor, `${label}.ancestor`),
    transactions: parsed.transactions,
  };
};
