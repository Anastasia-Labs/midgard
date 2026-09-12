import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import JSONBig from "json-bigint";

import {
  LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE,
  LocalKupmiosCheckpointChangedError,
  type LocalKupmiosFraudProofRawSource,
  settleLocalKupmiosReads,
  withLocalKupmiosSourceCapture,
} from "./local-kupmios-raw-l1-authority.js";
import {
  admitFraudProofRawL1Point,
  admitFraudProofRawL1Transaction,
  admitFraudProofRawL1Utxo,
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import {
  inspectSignedWorkflowTransaction,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "./signed-transaction-reconciliation.js";

export const OGMIOS_RAW_TRANSACTION_CBOR_FLAG =
  "--include-transaction-cbor" as const;
export const LOCAL_KUPMIOS_HTTP_OGMIOS_SOURCE =
  "midgard-local-kupo-http-ogmios-ws-source-v1" as const;
export const LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT =
  "midgard-local-kupmios-raw-block-at-point-v1" as const;

export { LocalKupmiosCheckpointChangedError } from "./local-kupmios-raw-l1-authority.js";
export type {
  SignedTransactionRecoveryObservation,
  SignedWorkflowTransaction,
} from "./signed-transaction-reconciliation.js";

/** Exact Kupo canonical-chain refusal; safe for bounded rollback-prefix search. */
export class LocalKupmiosExactPointNotCanonicalError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "LocalKupmiosExactPointNotCanonicalV1Error";
  }
}

export type LocalKupmiosRawBlockAtPoint = Readonly<{
  schemaVersion: typeof LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT;
  sourceId: string;
  point: FraudProofRawL1Point;
  parentBlockHash: string | null;
  kupoCheckpoint: Readonly<{ slot: number; blockHash: string }>;
  transactions: readonly Readonly<{
    txHash: string;
    transactionCbor: string;
  }>[];
}>;

export type LocalKupmiosReferenceBodiesAtPoint = Readonly<{
  targetBlock: LocalKupmiosRawBlockAtPoint;
  creatingTransactionBodies: readonly string[];
}>;

const admittedReferenceBodyReaders = new WeakMap<
  object,
  (point: FraudProofRawL1Point) => Promise<LocalKupmiosReferenceBodiesAtPoint>
>();

const admittedHistoricalPageReaders = new WeakMap<
  LocalKupmiosFraudProofRawSource,
  Readonly<{
    address: LocalKupmiosFraudProofRawSource["scanAddressPage"];
    history: LocalKupmiosFraudProofRawSource["scanUnitHistoryPage"];
  }>
>();

export type LocalKupmiosAdmittedPredecessorPoint = Readonly<{
  sourceId: string;
  point: FraudProofRawL1Point;
  predecessorPoint: FraudProofRawL1Point;
}>;

const admittedPredecessorReaders = new WeakMap<
  object,
  (point: FraudProofRawL1Point) => Promise<LocalKupmiosAdmittedPredecessorPoint>
>();

export type LocalKupmiosAdmittedBoundary = Readonly<{
  kupoCheckpoint: FraudProofRawL1Point;
  ogmiosTip: FraudProofRawL1Point;
  confirmationDepth: number;
}>;

export type LocalKupmiosAdmittedUnitHistory = Readonly<{
  checkpoint: FraudProofRawL1Point;
  transactions: readonly Readonly<{
    txHash: string;
    inclusionPoint: FraudProofRawL1Point;
  }>[];
}>;

const admittedHttpOgmiosSources = new WeakSet<object>();
const signedTransactionRecoveryReaders = new WeakMap<
  object,
  (
    input: SignedWorkflowTransaction,
  ) => Promise<SignedTransactionRecoveryObservation>
>();
const signedTransactionRebroadcasters = new WeakMap<
  object,
  (
    input: SignedWorkflowTransaction,
    authorize: (input: SignedWorkflowTransaction) => Promise<void>,
  ) => Promise<string>
>();

/** Concrete local source admission; copied or structural provider objects cannot authorize recovery. */
export const readAdmittedLocalKupmiosSignedTransactionRecovery = async (
  input: SignedWorkflowTransaction & {
    readonly source: LocalKupmiosFraudProofRawSource;
  },
): Promise<SignedTransactionRecoveryObservation> => {
  const read = signedTransactionRecoveryReaders.get(input.source);
  if (read === undefined)
    throw new Error(
      "Signed recovery requires admitted local Kupo/Ogmios authority",
    );
  inspectSignedWorkflowTransaction(input);
  return withLocalKupmiosSourceCapture(input.source, async () => {
    for (let attempt = 0; ; attempt += 1) {
      try {
        return await read(input);
      } catch (cause) {
        if (
          !(cause instanceof LocalKupmiosCheckpointChangedError) ||
          attempt >= 2
        )
          throw cause;
        // readBoundary resets all capture caches; no partial input or inclusion
        // evidence crosses into the next independently pinned attempt.
      }
    }
  });
};

/** Replay recorded witnesses and body without rebuilding, evaluating, or signing a replacement. */
export const rebroadcastAdmittedLocalKupmiosSignedTransaction = async (
  input: SignedWorkflowTransaction & {
    readonly source: LocalKupmiosFraudProofRawSource;
    readonly authorizeResubmission: (
      input: SignedWorkflowTransaction,
    ) => Promise<void>;
  },
): Promise<string> => {
  const submit = signedTransactionRebroadcasters.get(input.source);
  if (submit === undefined)
    throw new Error(
      "Signed rebroadcast requires admitted local Ogmios authority",
    );
  inspectSignedWorkflowTransaction(input);
  return submit(input, input.authorizeResubmission);
};

export type LocalKupmiosHttpOgmiosRawSourceDetails = Readonly<{
  sourceId: string;
  kupoHttpUrl: string;
  ogmiosUrl: string;
  deploymentIdentityDigest: string;
  blueprintHash: string;
  finalityPolicyDigest: string;
  confirmationDepth: 30;
  automaticRecoveryMaxDepth: 2160;
}>;

const admittedHttpOgmiosSourceDetails = new WeakMap<
  object,
  LocalKupmiosHttpOgmiosRawSourceDetails
>();

/**
 * Returns the immutable authority binding captured by the concrete loopback
 * constructor. Structural source copies deliberately have no details.
 */
export const localKupmiosHttpOgmiosRawSourceDetails = (
  source: LocalKupmiosFraudProofRawSource,
): LocalKupmiosHttpOgmiosRawSourceDetails | null =>
  admittedHttpOgmiosSourceDetails.get(source) ?? null;

export type FraudProofRawL1Fetch = (
  input: string,
  init?: RequestInit,
) => Promise<Response>;

export type FraudProofRawL1WebSocketLike = {
  send(data: string): void;
  close(code?: number, reason?: string): void;
  addEventListener(
    type: string,
    listener: (event: never) => void,
    options?: { once?: boolean },
  ): void;
  removeEventListener(type: string, listener: (event: never) => void): void;
};

export type FraudProofRawL1WebSocketFactory = (
  url: string,
) => FraudProofRawL1WebSocketLike;

export type LocalKupmiosHttpOgmiosSourceConfig = {
  readonly sourceId: string;
  readonly kupoHttpUrl: string;
  readonly ogmiosUrl: string;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly fetchImpl?: FraudProofRawL1Fetch;
  readonly webSocketFactory?: FraudProofRawL1WebSocketFactory;
  readonly timeoutMs?: number;
  readonly blockScanLimit?: number;
  readonly signal?: AbortSignal;
  readonly maxResponseBytes?: number;
};

type KupoPoint = {
  readonly slot: number;
  readonly blockHash: string;
};

type KupoSpentPoint = KupoPoint & {
  readonly txHash: string;
};

type KupoMatch = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly assets: Readonly<Record<string, bigint>>;
  readonly createdAt: KupoPoint;
  readonly spentAt: KupoSpentPoint | null;
  readonly datumHash: string | null;
  readonly datumType: "hash" | "inline" | null;
  readonly datum: string | null;
  readonly scriptHash: string | null;
  readonly script: unknown;
};

type OgmiosTip = {
  readonly slot: number;
  readonly blockHash: string;
  readonly blockNo: number;
};

type OgmiosRawTransactionAtPoint = {
  readonly txHash: string;
  readonly transactionCbor: string;
  readonly point: FraudProofRawL1Point;
};

const losslessJson = JSONBig({ useNativeBigInt: true, strict: true });

const DEFAULT_TIMEOUT_MS = 20_000;
const DEFAULT_BLOCK_SCAN_LIMIT = 2_000;
const MAX_MATCHES = 100_000;
const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_28 = /^[0-9a-f]{56}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(0|[1-9][0-9]*)$/u;
/**
 * 2160 blocks at the mainnet average of one block per 20 seconds. Kupo
 * checkpoints at least this far below its head are past the security
 * parameter and are memoized per source instance.
 */
const IMMUTABLE_CHECKPOINT_SLOT_DISTANCE = 43_200;
const MAX_RESPONSE_BYTES = 64 * 1024 * 1024;

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
type ReferenceReadScope = {
  responseBytes: number;
  inspectedMembers: number;
  head: KupoPoint | undefined;
  readonly rawBlocks: Map<string, Promise<ReturnType<typeof parseOgmiosBlock>>>;
};
const referenceResponseLimit = (
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
const debitReferenceResponse = (
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
const debitReferenceMembers = (
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
const boundedReferenceCbor = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length / 2 > LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.publicBytes
  )
    throw new Error(
      `${label} exceeds reference acquisition public-byte bounds`,
    );
  return cbor(value, label);
};

const abortSignalAborted = Object.getOwnPropertyDescriptor(
  AbortSignal.prototype,
  "aborted",
)!.get!;

const validateSourceSignal = (signal: AbortSignal | undefined): void => {
  if (signal === undefined) return;
  try {
    abortSignalAborted.call(signal);
  } catch {
    throw new Error("raw-source signal must be a platform AbortSignal");
  }
};

const throwIfSourceAborted = (signal: AbortSignal | undefined): void => {
  if (signal !== undefined && abortSignalAborted.call(signal)) {
    throw new DOMException("local Kupmios raw source aborted", "AbortError");
  }
};

const record = (
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

const exactKeys = (
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

const naturalNumber = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a natural safe integer`);
  }
  return value as number;
};

const digest = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !HEX_32.test(value)) {
    throw new Error(`${label} must be 32-byte lowercase hex`);
  }
  return value;
};

const nullableDigest = (value: unknown, label: string): string | null =>
  value === null ? null : digest(value, label);

const nullableScriptHash = (value: unknown, label: string): string | null => {
  if (value === null) return null;
  if (typeof value !== "string" || !HEX_28.test(value)) {
    throw new Error(`${label} must be 28-byte lowercase hex`);
  }
  return value;
};

const cbor = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !EVEN_HEX.test(value)) {
    throw new Error(`${label} must be non-empty lowercase CBOR hex`);
  }
  return value;
};

const canonicalAddress = (value: unknown, label: string): string => {
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

const normalizeHttpUrl = (value: string): string => {
  const parsed = new URL(value.trim());
  if (parsed.protocol === "ws:") parsed.protocol = "http:";
  if (parsed.protocol === "wss:") parsed.protocol = "https:";
  parsed.hash = "";
  return parsed.toString().replace(/\/$/u, "");
};

const normalizeWebSocketUrl = (value: string): string => {
  const parsed = new URL(value.trim());
  if (parsed.protocol === "http:") parsed.protocol = "ws:";
  if (parsed.protocol === "https:") parsed.protocol = "wss:";
  parsed.hash = "";
  return parsed.toString().replace(/\/$/u, "");
};

const assertLoopbackUrl = (value: string, label: string): void => {
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

const joinUrl = (base: string, path: string): string =>
  `${base.replace(/\/+$/u, "")}/${path.replace(/^\/+/u, "")}`;

type JsonHttpResponse = {
  readonly value: unknown;
  readonly checkpointHeaders: KupoPoint | null;
};

const fetchJson = async ({
  fetchImpl,
  url,
  timeoutMs,
  init,
  signal,
  maxResponseBytes,
  referenceScope,
}: {
  readonly fetchImpl: FraudProofRawL1Fetch;
  readonly url: string;
  readonly timeoutMs: number;
  readonly init?: RequestInit;
  readonly signal: AbortSignal | undefined;
  readonly maxResponseBytes: number;
  readonly referenceScope?: ReferenceReadScope;
}): Promise<JsonHttpResponse> => {
  throwIfSourceAborted(signal);
  const responseLimit = referenceResponseLimit(
    maxResponseBytes,
    referenceScope,
  );
  const controller = new AbortController();
  let reader: ReadableStreamDefaultReader<Uint8Array> | undefined;
  const cancelReader = (): void => {
    if (reader !== undefined) {
      void reader.cancel("raw-source request cancelled").catch(() => undefined);
    }
  };
  const abort = (): void => {
    controller.abort();
    cancelReader();
  };
  const acceptBytes = (bytes: number): void => {
    try {
      debitReferenceResponse(referenceScope, bytes);
    } catch (error) {
      abort();
      throw error;
    }
  };
  signal?.addEventListener("abort", abort, { once: true });
  const timer = setTimeout(abort, timeoutMs);
  try {
    throwIfSourceAborted(signal);
    const response = await fetchImpl(url, {
      ...init,
      signal: controller.signal,
    });
    throwIfSourceAborted(signal);
    controller.signal.throwIfAborted();
    const contentLength = response.headers.get("content-length");
    if (
      contentLength !== null &&
      (!NATURAL.test(contentLength) || Number(contentLength) > responseLimit)
    ) {
      controller.abort();
      if (response.body !== null) {
        void response.body
          .cancel("raw-source response byte bound exceeded")
          .catch(() => undefined);
      }
      throw new Error(`response from ${url} exceeds the raw-source byte bound`);
    }
    const chunks: Buffer[] = [];
    let byteLength = 0;
    if (response.body === null) {
      const body = await response.arrayBuffer();
      throwIfSourceAborted(signal);
      controller.signal.throwIfAborted();
      byteLength = body.byteLength;
      acceptBytes(byteLength);
      chunks.push(Buffer.from(body));
    } else {
      reader = response.body.getReader();
      while (true) {
        const next = await reader.read();
        throwIfSourceAborted(signal);
        controller.signal.throwIfAborted();
        if (next.done) break;
        byteLength += next.value.byteLength;
        if (byteLength > responseLimit) {
          controller.abort();
          cancelReader();
          throw new Error(
            `response from ${url} exceeds the raw-source byte bound`,
          );
        }
        acceptBytes(next.value.byteLength);
        chunks.push(Buffer.from(next.value));
      }
    }
    if (byteLength > responseLimit) {
      throw new Error(`response from ${url} exceeds the raw-source byte bound`);
    }
    const body = Buffer.concat(chunks, byteLength).toString("utf8");
    throwIfSourceAborted(signal);
    controller.signal.throwIfAborted();
    if (!response.ok) {
      throw new Error(
        `HTTP ${response.status.toString()} from ${url}: ${body.slice(0, 256)}`,
      );
    }
    try {
      const checkpointSlot = response.headers.get("x-most-recent-checkpoint");
      const checkpointEtag = response.headers.get("etag");
      let checkpointHeaders: KupoPoint | null = null;
      if (checkpointSlot !== null || checkpointEtag !== null) {
        if (
          checkpointSlot === null ||
          !NATURAL.test(checkpointSlot) ||
          checkpointEtag === null ||
          !HEX_32.test(checkpointEtag)
        ) {
          throw new Error(
            `response from ${url} has malformed Kupo checkpoint headers`,
          );
        }
        checkpointHeaders = {
          slot: Number(checkpointSlot),
          blockHash: checkpointEtag,
        };
      }
      return {
        value: losslessJson.parse(body) as unknown,
        checkpointHeaders,
      };
    } catch (cause) {
      throw new Error(
        `malformed JSON or checkpoint headers from ${url}: ${String(cause)}`,
      );
    }
  } finally {
    clearTimeout(timer);
    signal?.removeEventListener("abort", abort);
    reader?.releaseLock();
  }
};

const parseKupoPoint = (value: unknown, label: string): KupoPoint => {
  const parsed = exactKeys(value, ["slot_no", "header_hash"], [], label);
  return {
    slot: naturalNumber(parsed.slot_no, `${label}.slot_no`),
    blockHash: digest(parsed.header_hash, `${label}.header_hash`),
  };
};

const parseKupoSpentPoint = (
  value: unknown,
  label: string,
): KupoSpentPoint | null => {
  if (value === null) return null;
  const parsed = exactKeys(
    value,
    ["transaction_id", "input_index", "slot_no", "header_hash"],
    ["redeemer"],
    label,
  );
  naturalNumber(parsed.input_index, `${label}.input_index`);
  if (
    parsed.redeemer !== undefined &&
    parsed.redeemer !== null &&
    (typeof parsed.redeemer !== "string" || !EVEN_HEX.test(parsed.redeemer))
  ) {
    throw new Error(`${label}.redeemer must be CBOR hex when present`);
  }
  return {
    slot: naturalNumber(parsed.slot_no, `${label}.slot_no`),
    blockHash: digest(parsed.header_hash, `${label}.header_hash`),
    txHash: digest(parsed.transaction_id, `${label}.transaction_id`),
  };
};

const kupoQuantity = (value: unknown, label: string): bigint => {
  if (typeof value === "bigint" && value >= 0n) return value;
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0)
    return BigInt(value);
  if (typeof value === "string" && NATURAL.test(value)) return BigInt(value);
  throw new Error(`${label} must be an exact nonnegative quantity`);
};

const parseKupoMatch = (value: unknown, label: string): KupoMatch => {
  const parsed = exactKeys(
    value,
    [
      "transaction_index",
      "transaction_id",
      "output_index",
      "address",
      "value",
      "datum_hash",
      "script_hash",
      "created_at",
      "spent_at",
      "datum",
      "script",
    ],
    ["datum_type"],
    label,
  );
  naturalNumber(parsed.transaction_index, `${label}.transaction_index`);
  const valueRecord = exactKeys(
    parsed.value,
    ["coins", "assets"],
    [],
    `${label}.value`,
  );
  const assets = record(valueRecord.assets, `${label}.value.assets`);
  const normalizedAssets: Record<string, bigint> = {
    lovelace: kupoQuantity(valueRecord.coins, `${label}.value.coins`),
  };
  for (const [unit, quantity] of Object.entries(assets)) {
    if (!/^[0-9a-f]{56}\.(?:[0-9a-f]{2}){0,32}$/u.test(unit)) {
      throw new Error(`${label}.value.assets is not canonical Kupo value JSON`);
    }
    normalizedAssets[unit.replace(".", "")] = kupoQuantity(
      quantity,
      `${label}.value.assets.${unit}`,
    );
  }
  const datumHash = nullableDigest(parsed.datum_hash, `${label}.datum_hash`);
  let datumType: "hash" | "inline" | null = null;
  if (datumHash !== null) {
    if (parsed.datum_type !== "hash" && parsed.datum_type !== "inline") {
      throw new Error(
        `${label}.datum_type is required for a datum-bearing output`,
      );
    }
    datumType = parsed.datum_type;
  } else if (parsed.datum_type !== undefined) {
    throw new Error(`${label}.datum_type is invalid without datum_hash`);
  }
  if (
    parsed.datum !== null &&
    (typeof parsed.datum !== "string" || !EVEN_HEX.test(parsed.datum))
  ) {
    throw new Error(`${label}.datum must be resolved CBOR or null`);
  }
  return {
    txHash: digest(parsed.transaction_id, `${label}.transaction_id`),
    outputIndex: naturalNumber(parsed.output_index, `${label}.output_index`),
    address: canonicalAddress(parsed.address, `${label}.address`),
    assets: normalizedAssets,
    createdAt: parseKupoPoint(parsed.created_at, `${label}.created_at`),
    spentAt: parseKupoSpentPoint(parsed.spent_at, `${label}.spent_at`),
    datumHash,
    datumType,
    datum: parsed.datum as string | null,
    scriptHash: nullableScriptHash(parsed.script_hash, `${label}.script_hash`),
    script: parsed.script,
  };
};

const parseKupoMatches = (
  value: unknown,
  label: string,
  referenceScope?: ReferenceReadScope,
): readonly KupoMatch[] => {
  if (!Array.isArray(value) || value.length > MAX_MATCHES) {
    throw new Error(`${label} must be a bounded Kupo match array`);
  }
  debitReferenceMembers(referenceScope, value.length);
  return value.map((entry, index) =>
    parseKupoMatch(entry, `${label}[${index.toString()}]`),
  );
};

const rawPoint = ({
  slot,
  blockHash,
  blockNo,
}: OgmiosTip): FraudProofRawL1Point => {
  const input = {
    slot: slot.toString(),
    blockHash,
    blockNo: blockNo.toString(),
  };
  return {
    ...input,
    pointId: computeFraudProofRawL1PointId(input),
  };
};

const parseOgmiosTip = (value: unknown, label: string): OgmiosTip => {
  const result = exactKeys(value, ["slot", "id", "height"], [], label);
  return {
    slot: naturalNumber(result.slot, `${label}.result.slot`),
    blockHash: digest(result.id, `${label}.result.id`),
    blockNo: naturalNumber(result.height, `${label}.result.height`),
  };
};

type OgmiosSession = {
  request(
    method: string,
    params: Readonly<Record<string, unknown>>,
  ): Promise<unknown>;
  close(): Promise<void>;
};

const defaultWebSocketFactory: FraudProofRawL1WebSocketFactory = (url) =>
  new WebSocket(url) as unknown as FraudProofRawL1WebSocketLike;

// Each Ogmios WebSocket opens node-to-client connections. Bound physical
// sessions across captures/sources; closing sockets still consume their permit.
const MAXIMUM_OGMIOS_SESSIONS = 4;
const ogmiosSessionBudgets = new Map<
  string,
  { active: number; waiting: Set<() => void> }
>();
const acquireOgmiosSession = async (
  url: string,
  timeoutMs: number,
  signal: AbortSignal | undefined,
): Promise<() => void> => {
  throwIfSourceAborted(signal);
  const endpoint = new URL(url).origin;
  let budget = ogmiosSessionBudgets.get(endpoint);
  if (budget === undefined) {
    budget = { active: 0, waiting: new Set() };
    ogmiosSessionBudgets.set(endpoint, budget);
  }
  const selected = budget;
  await new Promise<void>((resolve, reject) => {
    const cleanup = (): void => {
      clearTimeout(timer);
      signal?.removeEventListener("abort", onAbort);
      selected.waiting.delete(admit);
    };
    const fail = (error: Error): void => {
      cleanup();
      reject(error);
    };
    const onAbort = (): void =>
      fail(new DOMException("local Kupmios raw source aborted", "AbortError"));
    const admit = (): void => {
      cleanup();
      selected.active += 1;
      resolve();
    };
    const timer = setTimeout(
      () => fail(new Error("Ogmios session capacity wait timed out")),
      timeoutMs,
    );
    signal?.addEventListener("abort", onAbort, { once: true });
    if (signal !== undefined && abortSignalAborted.call(signal)) onAbort();
    else if (selected.active < MAXIMUM_OGMIOS_SESSIONS) admit();
    else selected.waiting.add(admit);
  });
  let released = false;
  return () => {
    if (released) return;
    released = true;
    selected.active -= 1;
    selected.waiting.values().next().value?.();
    if (selected.active === 0 && selected.waiting.size === 0)
      ogmiosSessionBudgets.delete(endpoint);
  };
};

const openOgmiosSession = async ({
  url,
  timeoutMs,
  webSocketFactory,
  signal,
  maxResponseBytes,
  referenceScope,
}: {
  readonly url: string;
  readonly timeoutMs: number;
  readonly webSocketFactory: FraudProofRawL1WebSocketFactory;
  readonly signal: AbortSignal | undefined;
  readonly maxResponseBytes: number | undefined;
  readonly referenceScope?: ReferenceReadScope;
}): Promise<OgmiosSession> => {
  throwIfSourceAborted(signal);
  referenceResponseLimit(
    maxResponseBytes ?? MAX_RESPONSE_BYTES,
    referenceScope,
  );
  const releaseCapacity = await acquireOgmiosSession(url, timeoutMs, signal);
  let socket: FraudProofRawL1WebSocketLike;
  try {
    throwIfSourceAborted(signal);
    socket = webSocketFactory(url);
  } catch (error) {
    releaseCapacity();
    throw error;
  }
  const openedMonotonicMs = performance.now();
  let physicallyClosed = false;
  let resolveClosed!: () => void;
  const closed = new Promise<void>((resolve) => {
    resolveClosed = resolve;
  });
  const pending = new Map<
    number,
    {
      method: string;
      resolve(value: unknown): void;
      reject(error: Error): void;
    }
  >();
  let lastMethod: string | null = null;
  let nextId = 0;
  let terminal: Error | null = null;
  let opening = true;
  let resolveOpening!: () => void;
  let rejectOpening!: (error: Error) => void;
  const opened = new Promise<void>((resolve, reject) => {
    resolveOpening = resolve;
    rejectOpening = reject;
  });
  const listeners: [string, (event: never) => void][] = [];
  const listen = (type: string, listener: (event: never) => void): void => {
    listeners.push([type, listener]);
    socket.addEventListener(type, listener);
  };
  // Every terminal path owns the whole concrete session. In particular an RPC
  // timeout now closes it immediately instead of waiting for the caller's
  // finally block; no later request can reuse a timed-out or failed session.
  const terminate = (error: Error): void => {
    if (terminal !== null) return;
    terminal = error;
    clearTimeout(openingTimer);
    signal?.removeEventListener("abort", onAbort);
    for (const [type, listener] of listeners) {
      socket.removeEventListener(type, listener);
    }
    listeners.length = 0;
    if (opening) {
      opening = false;
      rejectOpening(error);
    }
    for (const waiter of pending.values()) waiter.reject(error);
    pending.clear();
    if (physicallyClosed) return;
    try {
      socket.close();
    } catch {
      // A socket already failing/closing must not replace the terminal error.
    }
  };
  const onAbort = (): void => {
    terminate(
      new DOMException("local Kupmios raw source aborted", "AbortError"),
    );
  };
  listen("message", ((event: { data: unknown }) => {
    if (terminal !== null) return;
    if (typeof event.data !== "string") {
      terminate(new Error("Ogmios sent a non-text frame"));
      return;
    }
    // The platform WebSocket has already buffered this frame. This limit only
    // bounds text accepted for JSON parsing, not transport-frame allocation.
    if (
      maxResponseBytes !== undefined &&
      Buffer.byteLength(event.data, "utf8") > maxResponseBytes
    ) {
      terminate(new Error("Ogmios response exceeds the raw-source byte bound"));
      return;
    }
    try {
      debitReferenceResponse(
        referenceScope,
        Buffer.byteLength(event.data, "utf8"),
      );
    } catch (cause) {
      terminate(
        cause instanceof Error
          ? cause
          : new Error("reference acquisition frame budget exceeded"),
      );
      return;
    }
    let message: { id?: unknown; result?: unknown; error?: unknown };
    try {
      message = JSON.parse(event.data) as typeof message;
    } catch (cause) {
      terminate(new Error(`Ogmios sent malformed JSON: ${String(cause)}`));
      return;
    }
    if (
      typeof message !== "object" ||
      message === null ||
      Array.isArray(message)
    ) {
      terminate(new Error("Ogmios sent a non-object JSON response"));
      return;
    }
    if (typeof message.id !== "number") return;
    const waiter = pending.get(message.id);
    if (waiter === undefined) return;
    pending.delete(message.id);
    if (message.error !== undefined) {
      waiter.reject(
        new Error(`Ogmios error: ${JSON.stringify(message.error)}`),
      );
    } else {
      waiter.resolve(message.result);
    }
  }) as (event: never) => void);
  listen("error", (() =>
    terminate(
      new Error(
        opening ? "Ogmios socket failed while opening" : "Ogmios socket failed",
      ),
    )) as (event: never) => void);
  // Keep the close listener until the physical transport ends, including after
  // local termination removed all RPC listeners. Do not release on close().
  const onClose = ((event: {
    code?: number;
    reason?: string;
    wasClean?: boolean;
  }) => {
    if (physicallyClosed) return;
    physicallyClosed = true;
    socket.removeEventListener("close", onClose);
    releaseCapacity();
    resolveClosed();
    const detail = {
      phase: opening ? "opening" : "active",
      pendingMethods: [...pending.values()].map(({ method }) => method),
      lastMethod,
      elapsedMs: Math.ceil(performance.now() - openedMonotonicMs),
      code: event.code,
      reason: event.reason?.slice(0, 256),
      wasClean: event.wasClean,
    };
    terminate(new Error(`Ogmios socket closed: ${JSON.stringify(detail)}`));
  }) as (event: never) => void;
  socket.addEventListener("close", onClose);
  listen("open", (() => {
    if (terminal !== null || !opening) return;
    opening = false;
    clearTimeout(openingTimer);
    resolveOpening();
  }) as (event: never) => void);
  const openingTimer = setTimeout(() => {
    terminate(
      new Error(`Ogmios socket did not open within ${timeoutMs.toString()}ms`),
    );
  }, timeoutMs);
  signal?.addEventListener("abort", onAbort, { once: true });
  if (signal !== undefined && abortSignalAborted.call(signal)) onAbort();
  await opened;
  const assertSessionOpen = (): void => {
    throwIfSourceAborted(signal);
    if (terminal !== null) throw terminal;
  };
  return {
    request: async (method, params) => {
      assertSessionOpen();
      referenceResponseLimit(
        maxResponseBytes ?? MAX_RESPONSE_BYTES,
        referenceScope,
      );
      const id = nextId;
      nextId += 1;
      const result = await new Promise<unknown>((resolve, reject) => {
        const timer = setTimeout(() => {
          terminate(new Error(`Ogmios ${method} timed out`));
        }, timeoutMs);
        lastMethod = method;
        pending.set(id, {
          method,
          resolve: (value) => {
            clearTimeout(timer);
            resolve(value);
          },
          reject: (error) => {
            clearTimeout(timer);
            reject(error);
          },
        });
        try {
          socket.send(JSON.stringify({ jsonrpc: "2.0", method, params, id }));
        } catch (cause) {
          terminate(
            cause instanceof Error
              ? cause
              : new Error("Ogmios socket send failed"),
          );
        }
      });
      assertSessionOpen();
      return result;
    },
    close: async () => {
      terminate(new Error("Ogmios session closed"));
      let timer: ReturnType<typeof setTimeout> | undefined;
      try {
        await Promise.race([
          closed,
          new Promise<never>((_resolve, reject) => {
            timer = setTimeout(
              () =>
                reject(
                  Object.assign(
                    new Error("Ogmios physical socket close timed out"),
                    { cause: terminal },
                  ),
                ),
              timeoutMs,
            );
          }),
        ]);
      } finally {
        clearTimeout(timer);
      }
    },
  };
};

const sameKupoPoint = (left: KupoPoint, right: KupoPoint): boolean =>
  left.slot === right.slot && left.blockHash === right.blockHash;

const sameRawPoint = (
  left: FraudProofRawL1Point,
  right: FraudProofRawL1Point,
): boolean =>
  left.slot === right.slot &&
  left.blockHash === right.blockHash &&
  left.blockNo === right.blockNo &&
  left.pointId === right.pointId;

const parseOgmiosBlock = (
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

export const requireOgmiosRawTransactionCbor = ({
  value,
  expectedTxHash,
  label,
}: {
  readonly value: unknown;
  readonly expectedTxHash: string;
  readonly label: string;
}): string => {
  const parsed = record(value, label);
  const reported = digest(parsed.id, `${label}.id`);
  if (reported !== expectedTxHash) {
    throw new Error(`${label}.id disagrees with the requested transaction`);
  }
  if (!("cbor" in parsed)) {
    throw new Error(
      `${label}.cbor is missing; Ogmios must run with ${OGMIOS_RAW_TRANSACTION_CBOR_FLAG}`,
    );
  }
  const transactionCbor = cbor(parsed.cbor, `${label}.cbor`);
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  } catch (cause) {
    throw new Error(
      `${label}.cbor is not a Cardano transaction: ${String(cause)}`,
    );
  }
  const body = transaction.body();
  const bodyHash = CML.hash_transaction(body);
  try {
    if (bodyHash.to_hex() !== expectedTxHash) {
      throw new Error(`${label}.cbor hashes to a different transaction`);
    }
    return transactionCbor;
  } finally {
    bodyHash.free();
    body.free();
    transaction.free();
  }
};

const admitLocalKupmiosRawBlockAtPoint = ({
  value,
  source,
  requestedPoint,
}: {
  readonly value: unknown;
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly requestedPoint: FraudProofRawL1Point;
}): LocalKupmiosRawBlockAtPoint => {
  const parsed = exactKeys(
    value,
    [
      "schemaVersion",
      "sourceId",
      "point",
      "parentBlockHash",
      "kupoCheckpoint",
      "transactions",
    ],
    [],
    "local Kupmios raw block",
  );
  if (parsed.schemaVersion !== LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT) {
    throw new Error("local Kupmios raw block schema changed");
  }
  if (parsed.sourceId !== source.sourceId) {
    throw new Error("local Kupmios raw block changed its admitted source");
  }
  const point = admitFraudProofRawL1Point(
    parsed.point,
    "local Kupmios raw block point",
  );
  const parentBlockHash =
    parsed.parentBlockHash === null
      ? null
      : digest(parsed.parentBlockHash, "local Kupmios raw block parent hash");
  if (!sameRawPoint(point, requestedPoint)) {
    throw new Error("local Kupmios raw block changed the requested point");
  }
  const checkpoint = exactKeys(
    parsed.kupoCheckpoint,
    ["slot", "blockHash"],
    [],
    "local Kupmios raw block Kupo checkpoint",
  );
  const kupoCheckpoint = Object.freeze({
    slot: naturalNumber(
      checkpoint.slot,
      "local Kupmios raw block Kupo checkpoint slot",
    ),
    blockHash: digest(
      checkpoint.blockHash,
      "local Kupmios raw block Kupo checkpoint hash",
    ),
  });
  if (
    kupoCheckpoint.slot !== Number(point.slot) ||
    kupoCheckpoint.blockHash !== point.blockHash
  ) {
    throw new Error(
      "local Kupmios raw block Kupo checkpoint differs from Ogmios point",
    );
  }
  if (
    !Array.isArray(parsed.transactions) ||
    parsed.transactions.length > MAX_MATCHES
  ) {
    throw new Error("local Kupmios raw block transactions are not bounded");
  }
  const transactions = Object.freeze(
    parsed.transactions.map((value, index) => {
      const transaction = exactKeys(
        value,
        ["txHash", "transactionCbor"],
        [],
        `local Kupmios raw block transaction ${index.toString()}`,
      );
      const transactionHash = digest(
        transaction.txHash,
        `local Kupmios raw block transaction ${index.toString()} hash`,
      );
      return Object.freeze({
        txHash: transactionHash,
        transactionCbor: requireOgmiosRawTransactionCbor({
          value: {
            id: transactionHash,
            cbor: transaction.transactionCbor,
          },
          expectedTxHash: transactionHash,
          label: `local Kupmios raw block transaction ${index.toString()}`,
        }),
      });
    }),
  );
  if (
    new Set(transactions.map(({ txHash }) => txHash)).size !==
    transactions.length
  ) {
    throw new Error(
      "local Kupmios raw block contains duplicate transaction ids",
    );
  }
  return Object.freeze({
    schemaVersion: LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
    sourceId: source.sourceId,
    point,
    parentBlockHash,
    kupoCheckpoint,
    transactions,
  });
};

/**
 * Reads an exact raw block only from a source minted by the concrete loopback
 * HTTP/WS constructor, then independently re-admits its point and every ordered
 * transaction CBOR. Structural test doubles cannot cross this boundary.
 */
export const readAdmittedLocalKupmiosRawBlockAtPoint = async ({
  source,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
}): Promise<LocalKupmiosRawBlockAtPoint> => {
  if (!admittedHttpOgmiosSources.has(source)) {
    throw new Error(
      "exact raw block read requires the admitted local Kupo/Ogmios source",
    );
  }
  const requestedPoint = admitFraudProofRawL1Point(
    point,
    "requested local Kupmios raw block point",
  );
  return admitLocalKupmiosRawBlockAtPoint({
    value: await source.readBlockAtPoint({ point: requestedPoint }),
    source,
    requestedPoint,
  });
};

/** Reads a direct predecessor through the concrete source's captured readers. */
export const readAdmittedLocalKupmiosPredecessorPoint = async ({
  source,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
}): Promise<LocalKupmiosAdmittedPredecessorPoint> => {
  const read = admittedPredecessorReaders.get(source);
  if (read === undefined) {
    throw new Error(
      "predecessor point read requires the admitted local Kupo/Ogmios source",
    );
  }
  return await read(
    admitFraudProofRawL1Point(point, "requested local Kupmios child point"),
  );
};

/** Captured exact-target reference preimages; no resolved-input or native handle. */
export const readAdmittedLocalKupmiosReferenceBodiesAtPoint = async ({
  source,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
}): Promise<LocalKupmiosReferenceBodiesAtPoint> => {
  const read = admittedReferenceBodyReaders.get(source);
  if (read === undefined)
    throw new Error(
      "reference bodies require the admitted local Kupo/Ogmios source",
    );
  return await read(
    admitFraudProofRawL1Point(point, "requested reference target point"),
  );
};

/** Establishes and re-admits the concrete source's fresh release-final point. */
export const readAdmittedLocalKupmiosBoundary = async ({
  source,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
}): Promise<LocalKupmiosAdmittedBoundary> => {
  if (!admittedHttpOgmiosSources.has(source)) {
    throw new Error(
      "release boundary read requires the admitted local Kupo/Ogmios source",
    );
  }
  const value = exactKeys(
    await source.readBoundary(),
    ["kupoCheckpoint", "ogmiosTip"],
    [],
    "local Kupmios release boundary",
  );
  const kupoCheckpoint = admitFraudProofRawL1Point(
    value.kupoCheckpoint,
    "local Kupmios release Kupo checkpoint",
  );
  const ogmiosTip = admitFraudProofRawL1Point(
    value.ogmiosTip,
    "local Kupmios release Ogmios tip",
  );
  const confirmationDepth =
    Number(ogmiosTip.blockNo) - Number(kupoCheckpoint.blockNo) + 1;
  const details = admittedHttpOgmiosSourceDetails.get(source)!;
  if (
    !Number.isSafeInteger(confirmationDepth) ||
    confirmationDepth < details.confirmationDepth ||
    confirmationDepth > details.automaticRecoveryMaxDepth
  ) {
    throw new Error("local Kupmios boundary is outside release finality");
  }
  return Object.freeze({ kupoCheckpoint, ogmiosTip, confirmationDepth });
};

/** Reads complete unit history at the active or an admitted historical point. */
export const readAdmittedLocalKupmiosUnitHistoryAtPoint = async ({
  source,
  unit,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly unit: string;
  readonly point: FraudProofRawL1Point;
}): Promise<LocalKupmiosAdmittedUnitHistory> => {
  if (!admittedHttpOgmiosSources.has(source)) {
    throw new Error(
      "unit-history read requires the admitted local Kupo/Ogmios source",
    );
  }
  if (!/^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u.test(unit)) {
    throw new Error("unit-history read requires a canonical Cardano unit");
  }
  const checkpoint = admitFraudProofRawL1Point(
    point,
    "requested local Kupmios unit-history point",
  );
  const page = exactKeys(
    await admittedHistoricalPageReaders.get(source)!.history({
      unit,
      fromGenesis: true,
      throughPoint: checkpoint,
      after: null,
    }),
    ["checkpoint", "transactions", "nextCursor", "complete"],
    [],
    "local Kupmios unit-history page",
  );
  const returnedCheckpoint = admitFraudProofRawL1Point(
    page.checkpoint,
    "local Kupmios unit-history checkpoint",
  );
  if (
    returnedCheckpoint.pointId !== checkpoint.pointId ||
    page.nextCursor !== null ||
    page.complete !== true ||
    !Array.isArray(page.transactions)
  ) {
    throw new Error("local Kupmios unit history is incomplete or substituted");
  }
  const details = admittedHttpOgmiosSourceDetails.get(source)!;
  if (page.transactions.length > details.automaticRecoveryMaxDepth) {
    throw new Error("local Kupmios unit history exceeds its release bound");
  }
  const transactions = page.transactions.map((entry, index) => {
    const parsed = exactKeys(
      entry,
      ["txHash", "inclusionPoint"],
      [],
      `local Kupmios unit-history transaction ${index.toString()}`,
    );
    const txHash = digest(
      parsed.txHash,
      `local Kupmios unit-history transaction ${index.toString()} hash`,
    );
    const inclusionPoint = admitFraudProofRawL1Point(
      parsed.inclusionPoint,
      `local Kupmios unit-history transaction ${index.toString()} point`,
    );
    if (Number(inclusionPoint.blockNo) > Number(checkpoint.blockNo)) {
      throw new Error("local Kupmios unit history crosses its checkpoint");
    }
    return Object.freeze({ txHash, inclusionPoint });
  });
  if (
    new Set(transactions.map(({ txHash }) => txHash)).size !==
    transactions.length
  ) {
    throw new Error(
      "local Kupmios unit history contains duplicate transactions",
    );
  }
  return Object.freeze({
    checkpoint: returnedCheckpoint,
    transactions: Object.freeze(transactions),
  });
};

/**
 * Reads the exact unspent address set at one admitted historical point from
 * the same concrete local Kupo/Ogmios source. This is the compaction anchor
 * for restart recovery; caller-authored UTxO snapshots cannot cross it.
 */
export const readAdmittedLocalKupmiosAddressUtxosAtPoint = async ({
  source,
  address,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly address: string;
  readonly point: FraudProofRawL1Point;
}): Promise<readonly FraudProofRawL1Utxo[]> => {
  if (!admittedHttpOgmiosSources.has(source)) {
    throw new Error(
      "exact address snapshot requires the admitted local Kupo/Ogmios source",
    );
  }
  const canonicalAddress = CML.Address.from_bech32(address).to_bech32();
  if (canonicalAddress !== address) {
    throw new Error("exact address snapshot requires a canonical address");
  }
  const throughPoint = admitFraudProofRawL1Point(
    point,
    "requested local Kupmios address point",
  );
  const page = exactKeys(
    await admittedHistoricalPageReaders.get(source)!.address({
      address,
      throughPoint,
      after: null,
    }),
    ["checkpoint", "utxos", "nextCursor", "complete"],
    [],
    "local Kupmios exact address snapshot",
  );
  if (
    !sameRawPoint(
      admitFraudProofRawL1Point(
        page.checkpoint,
        "local Kupmios address checkpoint",
      ),
      throughPoint,
    ) ||
    page.nextCursor !== null ||
    page.complete !== true ||
    !Array.isArray(page.utxos) ||
    page.utxos.length > MAX_MATCHES
  ) {
    throw new Error("local Kupmios address snapshot is incomplete");
  }
  const utxos = page.utxos.map((value, index) =>
    admitFraudProofRawL1Utxo(
      value,
      `local Kupmios address UTxO ${index.toString()}`,
    ),
  );
  if (new Set(utxos.map(({ outRef }) => outRef)).size !== utxos.length) {
    throw new Error(
      "local Kupmios address snapshot contains duplicate outrefs",
    );
  }
  return Object.freeze(utxos);
};

/**
 * Exact resolved transaction read from the same concrete loopback source as
 * the admitted raw-block path. Both provider claims and every resolved input
 * are re-admitted before the result crosses the package boundary.
 */
export const readAdmittedLocalKupmiosUtxosByOutRefAtPoint = async (input: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
  readonly outRefs: readonly string[];
}): Promise<readonly FraudProofRawL1Utxo[]> => {
  if (
    !admittedHttpOgmiosSources.has(input.source) ||
    input.source.readOutRefsAtPoint === undefined
  ) {
    throw new Error(
      "Exact outref reads require admitted local Kupo/Ogmios authority",
    );
  }
  const point = admitFraudProofRawL1Point(
    input.point,
    "exact outref checkpoint",
  );
  if (
    input.outRefs.length > MAX_MATCHES ||
    new Set(input.outRefs).size !== input.outRefs.length ||
    input.outRefs.some((ref) => !/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(ref))
  )
    throw new Error("Invalid exact outref request");
  const value = await input.source.readOutRefsAtPoint({
    point,
    outRefs: input.outRefs,
  });
  if (!Array.isArray(value) || value.length > input.outRefs.length)
    throw new Error("Exact outref source returned an invalid set");
  const outputs = value.map((output, index) =>
    admitFraudProofRawL1Utxo(output, `exact outref ${index}`),
  );
  if (
    new Set(outputs.map(({ outRef }) => outRef)).size !== outputs.length ||
    outputs.some(({ outRef }) => !input.outRefs.includes(outRef))
  ) {
    throw new Error("Exact outref source substituted the requested set");
  }
  return Object.freeze(outputs);
};

export const pinAdmittedLocalKupmiosBoundaryAtPoint = async (input: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
}): Promise<void> => {
  if (
    !admittedHttpOgmiosSources.has(input.source) ||
    input.source.pinBoundaryAtPoint === undefined
  ) {
    throw new Error(
      "Boundary pinning requires admitted local Kupo/Ogmios authority",
    );
  }
  const point = admitFraudProofRawL1Point(
    input.point,
    "requested exact boundary",
  );
  const returned = admitFraudProofRawL1Point(
    await input.source.pinBoundaryAtPoint({ point }),
    "pinned exact boundary",
  );
  if (!sameRawPoint(point, returned))
    throw new Error("Local source substituted the requested boundary");
};

export const readAdmittedLocalKupmiosTransactionInclusion = async (input: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly txHash: string;
}): Promise<FraudProofRawL1Point | null> => {
  if (
    !admittedHttpOgmiosSources.has(input.source) ||
    input.source.resolveTransactionInclusion === undefined
  ) {
    throw new Error(
      "Transaction inclusion requires admitted local Kupo/Ogmios history",
    );
  }
  const value = await input.source.resolveTransactionInclusion({
    txHash: digest(input.txHash, "requested inclusion transaction"),
  });
  return value === null
    ? null
    : admitFraudProofRawL1Point(value, "local transaction inclusion");
};

export const readAdmittedLocalKupmiosRawTransaction = async ({
  source,
  txHash,
  expectedInclusionPoint,
  minimumConfirmationDepth,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly txHash: string;
  readonly expectedInclusionPoint: FraudProofRawL1Point;
  readonly minimumConfirmationDepth: number;
}): Promise<FraudProofRawL1Transaction> => {
  if (!admittedHttpOgmiosSources.has(source)) {
    throw new Error(
      "exact raw transaction read requires the admitted local Kupo/Ogmios source",
    );
  }
  const transactionHash = digest(txHash, "requested transaction hash");
  const inclusionPoint = admitFraudProofRawL1Point(
    expectedInclusionPoint,
    "requested transaction inclusion point",
  );
  if (
    !Number.isSafeInteger(minimumConfirmationDepth) ||
    minimumConfirmationDepth <= 0
  ) {
    throw new Error("minimum transaction confirmation depth is invalid");
  }
  const value = exactKeys(
    await source.readTransaction({
      txHash: transactionHash,
      expectedInclusionPoint: inclusionPoint,
    }),
    ["kupo", "ogmios"],
    [],
    `local Kupmios transaction ${transactionHash}`,
  );
  const kupo = exactKeys(
    value.kupo,
    ["txHash", "inclusionPoint"],
    [],
    `local Kupo transaction ${transactionHash}`,
  );
  if (
    digest(kupo.txHash, "local Kupo transaction hash") !== transactionHash ||
    !sameRawPoint(
      admitFraudProofRawL1Point(
        kupo.inclusionPoint,
        "local Kupo transaction inclusion point",
      ),
      inclusionPoint,
    )
  ) {
    throw new Error("Kupo substituted the requested transaction identity");
  }
  const admitted = admitFraudProofRawL1Transaction(
    value.ogmios,
    `local Ogmios transaction ${transactionHash}`,
    minimumConfirmationDepth,
  );
  if (
    admitted.txHash !== transactionHash ||
    !sameRawPoint(admitted.inclusionPoint, inclusionPoint)
  ) {
    throw new Error("Ogmios substituted the requested transaction identity");
  }
  return Object.freeze(admitted);
};

const transactionOutput = ({
  transactionCbor,
  outputIndex,
  label,
}: {
  readonly transactionCbor: string;
  readonly outputIndex: number;
  readonly label: string;
}): CML.TransactionOutput => {
  const outputs = CML.Transaction.from_cbor_hex(transactionCbor)
    .body()
    .outputs();
  if (outputIndex >= outputs.len()) {
    throw new Error(`${label} names an absent transaction output`);
  }
  return outputs.get(outputIndex);
};

const rawUtxoFromOutput = ({
  txHash,
  outputIndex,
  output,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly output: CML.TransactionOutput;
}): FraudProofRawL1Utxo => ({
  outRef: `${txHash}#${outputIndex.toString()}`,
  outputCbor: output.to_canonical_cbor_hex(),
  datumCbor: output.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null,
  referenceScriptCbor: output.script_ref()?.to_canonical_cbor_hex() ?? null,
});

const assertMatchOutput = ({
  match,
  output,
  label,
}: {
  readonly match: KupoMatch;
  readonly output: CML.TransactionOutput;
  readonly label: string;
}): void => {
  if (output.address().to_bech32() !== match.address) {
    throw new Error(`${label} Kupo address disagrees with transaction CBOR`);
  }
  const actualAssets = coreToTxOutput(output).assets;
  const actualEntries = Object.entries(actualAssets)
    .filter(([, quantity]) => quantity !== 0n)
    .sort(([left], [right]) => left.localeCompare(right));
  const kupoEntries = Object.entries(match.assets)
    .filter(([, quantity]) => quantity !== 0n)
    .sort(([left], [right]) => left.localeCompare(right));
  if (
    actualEntries.length !== kupoEntries.length ||
    actualEntries.some(
      ([unit, quantity], index) =>
        unit !== kupoEntries[index]?.[0] ||
        quantity !== kupoEntries[index]?.[1],
    )
  ) {
    throw new Error(`${label} Kupo value disagrees with transaction CBOR`);
  }
  const inlineData = output.datum()?.as_datum();
  // Kupo returns the ledger's original datum bytes. Re-encoding equivalent
  // Plutus data changes its hash (for example, definite/indefinite lists).
  const inlineDatum = inlineData?.to_cbor_hex() ?? null;
  const datumHash = output.datum_hash()?.to_hex() ?? null;
  if (match.datumType === "inline") {
    if (
      inlineDatum === null ||
      inlineDatum !== match.datum ||
      inlineData === undefined ||
      CML.hash_plutus_data(inlineData).to_hex() !== match.datumHash
    ) {
      throw new Error(
        `${label} Kupo inline datum disagrees with transaction CBOR`,
      );
    }
  } else if (match.datumType === "hash") {
    if (datumHash !== match.datumHash || inlineDatum !== null) {
      throw new Error(
        `${label} Kupo datum hash disagrees with transaction CBOR`,
      );
    }
    if (
      match.datum !== null &&
      CML.hash_plutus_data(
        CML.PlutusData.from_cbor_hex(match.datum),
      ).to_hex() !== match.datumHash
    ) {
      throw new Error(
        `${label} Kupo resolved datum does not hash to datum_hash`,
      );
    }
  } else if (inlineDatum !== null || datumHash !== null) {
    throw new Error(`${label} Kupo omitted a transaction datum`);
  }
  const actualScript = output.script_ref();
  if ((actualScript === undefined) !== (match.scriptHash === null)) {
    throw new Error(
      `${label} Kupo reference-script presence disagrees with CBOR`,
    );
  }
  if (match.scriptHash === null && match.script !== null) {
    throw new Error(
      `${label} Kupo returned script bytes without a script hash`,
    );
  }
  if (actualScript !== undefined) {
    if (actualScript.hash().to_hex() !== match.scriptHash)
      throw new Error(`${label} Kupo reference-script identity is malformed`);
    const resolved = exactKeys(
      match.script,
      ["language", "script"],
      [],
      `${label}.script`,
    );
    const scriptCbor = cbor(resolved.script, `${label}.script.script`);
    let kupoScript: CML.Script;
    switch (resolved.language) {
      case "native":
        kupoScript = CML.Script.new_native(
          CML.NativeScript.from_cbor_hex(scriptCbor),
        );
        break;
      case "plutus:v1":
        kupoScript = CML.Script.new_plutus_v1(
          CML.PlutusV1Script.from_raw_bytes(Buffer.from(scriptCbor, "hex")),
        );
        break;
      case "plutus:v2":
        kupoScript = CML.Script.new_plutus_v2(
          CML.PlutusV2Script.from_raw_bytes(Buffer.from(scriptCbor, "hex")),
        );
        break;
      case "plutus:v3":
        kupoScript = CML.Script.new_plutus_v3(
          CML.PlutusV3Script.from_raw_bytes(Buffer.from(scriptCbor, "hex")),
        );
        break;
      default:
        throw new Error(
          `${label} Kupo reference script has an unsupported language`,
        );
    }
    if (
      kupoScript.hash().to_hex() !== match.scriptHash ||
      kupoScript.to_canonical_cbor_hex() !==
        actualScript.to_canonical_cbor_hex()
    ) {
      throw new Error(
        `${label} Kupo reference script disagrees with transaction CBOR`,
      );
    }
  }
};

/** Strict wire-shape and byte-identity admission used by the live source. */
export const admitKupoMatchAgainstTransactionOutput = ({
  match,
  outputCbor,
  label = "Kupo match",
}: {
  readonly match: unknown;
  readonly outputCbor: string;
  readonly label?: string;
}): void => {
  let output: CML.TransactionOutput;
  try {
    output = CML.TransactionOutput.from_cbor_hex(
      cbor(outputCbor, `${label}.outputCbor`),
    );
  } catch (cause) {
    throw new Error(`${label} output CBOR is invalid: ${String(cause)}`);
  }
  assertMatchOutput({
    match: parseKupoMatch(match, label),
    output,
    label,
  });
};

const transactionInputs = (
  list: CML.TransactionInputList | undefined,
): readonly { readonly txHash: string; readonly outputIndex: number }[] => {
  if (list === undefined) return [];
  const result: { txHash: string; outputIndex: number }[] = [];
  for (let index = 0; index < list.len(); index += 1) {
    const input = list.get(index);
    const outputIndex = Number(input.index());
    if (!Number.isSafeInteger(outputIndex)) {
      throw new Error(
        "transaction input index exceeds JavaScript's safe range",
      );
    }
    result.push({ txHash: input.transaction_id().to_hex(), outputIndex });
  }
  return result;
};

export const createLocalKupmiosHttpOgmiosRawSource = (
  config: LocalKupmiosHttpOgmiosSourceConfig,
): LocalKupmiosFraudProofRawSource => {
  const kupoHttpUrl = normalizeHttpUrl(config.kupoHttpUrl);
  const ogmiosWebSocketUrl = normalizeWebSocketUrl(config.ogmiosUrl);
  const ogmiosHttpUrl = normalizeHttpUrl(config.ogmiosUrl);
  assertLoopbackUrl(kupoHttpUrl, "Kupo URL");
  assertLoopbackUrl(ogmiosWebSocketUrl, "Ogmios URL");
  if (
    config.sourceId.length === 0 ||
    config.sourceId.trim() !== config.sourceId
  ) {
    throw new Error("Kupmios sourceId must be canonical and non-empty");
  }
  const sourceId = `${LOCAL_KUPMIOS_HTTP_OGMIOS_SOURCE}:${config.sourceId}`;
  const fetchImpl = config.fetchImpl ?? fetch;
  const webSocketFactory = config.webSocketFactory ?? defaultWebSocketFactory;
  const timeoutMs = config.timeoutMs ?? DEFAULT_TIMEOUT_MS;
  const blockScanLimit = config.blockScanLimit ?? DEFAULT_BLOCK_SCAN_LIMIT;
  const signal = config.signal;
  const maxResponseBytes = config.maxResponseBytes;
  validateSourceSignal(signal);
  throwIfSourceAborted(signal);
  if (
    maxResponseBytes !== undefined &&
    (!Number.isSafeInteger(maxResponseBytes) ||
      maxResponseBytes <= 0 ||
      maxResponseBytes > MAX_RESPONSE_BYTES)
  ) {
    throw new Error(
      "raw-source maxResponseBytes must be positive and at most 64 MiB",
    );
  }
  if (!Number.isSafeInteger(blockScanLimit) || blockScanLimit <= 0) {
    throw new Error("Ogmios blockScanLimit must be positive");
  }

  let pinnedKupoResponseHead: KupoPoint | undefined;
  // Kupo's most recent checkpoint, as seen by any response of this instance.
  let latestKupoHeadSlot = -1;
  const getKupoJson = async (
    path: string,
    referenceScope?: ReferenceReadScope,
  ): Promise<unknown> => {
    const response = await fetchJson({
      fetchImpl,
      url: joinUrl(kupoHttpUrl, path),
      timeoutMs,
      signal,
      maxResponseBytes: maxResponseBytes ?? MAX_RESPONSE_BYTES,
      referenceScope,
      init: {
        headers: {
          accept: "application/json;asset-quantity=string",
        },
      },
    });
    if (response.checkpointHeaders === null) {
      throw new Error("Kupo response omitted X-Most-Recent-Checkpoint or ETag");
    }
    latestKupoHeadSlot = Math.max(
      latestKupoHeadSlot,
      response.checkpointHeaders.slot,
    );
    if (referenceScope !== undefined) {
      if (referenceScope.head === undefined)
        referenceScope.head = Object.freeze({ ...response.checkpointHeaders });
      else if (!sameKupoPoint(referenceScope.head, response.checkpointHeaders))
        throw new LocalKupmiosCheckpointChangedError(
          "Kupo changed during reference acquisition",
        );
    }
    if (pinnedKupoResponseHead === undefined) {
      pinnedKupoResponseHead = response.checkpointHeaders;
    } else if (
      !sameKupoPoint(pinnedKupoResponseHead, response.checkpointHeaders)
    ) {
      throw new LocalKupmiosCheckpointChangedError(
        `Kupo advanced or rolled back during raw snapshot capture: ${path} (${pinnedKupoResponseHead.slot} -> ${response.checkpointHeaders.slot})`,
      );
    }
    return response.value;
  };

  const queryTip = async (): Promise<OgmiosTip> => {
    // queryNetwork/tip omits height. Chain-sync returns one atomic tip with
    // its actual block number, avoiding a race between separate tip queries.
    const session = await openOgmiosSession({
      url: ogmiosWebSocketUrl,
      timeoutMs,
      webSocketFactory,
      signal,
      maxResponseBytes,
    });
    try {
      const result = exactKeys(
        await session.request("findIntersection", { points: ["origin"] }),
        ["intersection", "tip"],
        [],
        "Ogmios tip intersection",
      );
      if (result.intersection !== "origin")
        throw new Error("Ogmios tip query did not intersect origin");
      return parseOgmiosTip(result.tip, "Ogmios chain-sync tip");
    } finally {
      await session.close();
    }
  };

  // A checkpoint deeper than the security parameter below Kupo's head can
  // never change, so it is answered from memory without touching the pinned
  // head. Younger checkpoints are always re-read.
  const immutableCheckpoints = new Map<number, KupoPoint>();
  const getKupoCheckpoint = async (
    slot: number,
    referenceScope?: ReferenceReadScope,
  ): Promise<KupoPoint> => {
    const memoized = immutableCheckpoints.get(slot);
    if (memoized !== undefined) return memoized;
    const checkpoint = parseKupoPoint(
      await getKupoJson(`/checkpoints/${slot.toString()}`, referenceScope),
      `Kupo checkpoint ${slot.toString()}`,
    );
    if (slot <= latestKupoHeadSlot - IMMUTABLE_CHECKPOINT_SLOT_DISTANCE) {
      immutableCheckpoints.set(slot, checkpoint);
    }
    return checkpoint;
  };

  const readPredecessorCheckpoint = async (
    target: KupoPoint,
    referenceScope?: ReferenceReadScope,
  ): Promise<KupoPoint> => {
    if (target.slot === 0) throw new Error("cannot chain-sync before genesis");
    const ancestor = await getKupoCheckpoint(target.slot - 1, referenceScope);
    if (ancestor.slot >= target.slot) {
      throw new Error("Kupo did not return an earlier ancestor checkpoint");
    }
    return ancestor;
  };

  const rawBlockCache = new Map<
    string,
    Promise<{
      readonly point: OgmiosTip;
      readonly parentBlockHash: string | null;
      readonly transactions: readonly unknown[];
    }>
  >();
  const readBlock = async (
    target: KupoPoint,
    referenceScope?: ReferenceReadScope,
  ): Promise<{
    readonly point: OgmiosTip;
    readonly parentBlockHash: string | null;
    readonly transactions: readonly unknown[];
  }> => {
    throwIfSourceAborted(signal);
    const key = `${target.slot.toString()}:${target.blockHash}`;
    const cache = referenceScope?.rawBlocks ?? rawBlockCache;
    const cached =
      cache.get(key) ??
      (referenceScope === undefined ? undefined : rawBlockCache.get(key));
    if (cached !== undefined) {
      const block = await cached;
      throwIfSourceAborted(signal);
      debitReferenceMembers(referenceScope, block.transactions.length);
      return block;
    }
    const read = (async () => {
      const ancestor = await readPredecessorCheckpoint(target, referenceScope);
      const session = await openOgmiosSession({
        url: ogmiosWebSocketUrl,
        timeoutMs,
        webSocketFactory,
        signal,
        maxResponseBytes,
        referenceScope,
      });
      try {
        const intersection = record(
          await session.request("findIntersection", {
            points: [{ slot: ancestor.slot, id: ancestor.blockHash }],
          }),
          "Ogmios findIntersection result",
        );
        const found = record(
          intersection.intersection,
          "Ogmios findIntersection result.intersection",
        );
        if (
          naturalNumber(found.slot, "Ogmios intersection slot") !==
            ancestor.slot ||
          digest(found.id, "Ogmios intersection id") !== ancestor.blockHash
        ) {
          throw new Error("Ogmios did not intersect the Kupo ancestor");
        }
        let acknowledged = false;
        for (let scanned = 0; scanned < blockScanLimit; scanned += 1) {
          const next = record(
            await session.request("nextBlock", {}),
            "Ogmios nextBlock result",
          );
          if (next.direction === "backward") {
            if (acknowledged) {
              throw new Error("Ogmios rolled back during raw transaction scan");
            }
            acknowledged = true;
            scanned -= 1;
            continue;
          }
          if (next.direction !== "forward") {
            throw new Error("Ogmios nextBlock has no supported direction");
          }
          acknowledged = true;
          const block = parseOgmiosBlock(
            next.block,
            "Ogmios nextBlock.block",
            referenceScope,
          );
          if (block.point.blockHash === target.blockHash) {
            if (block.point.slot !== target.slot) {
              throw new Error("Kupo/Ogmios block slot disagreement");
            }
            return block;
          }
          if (block.point.slot > target.slot) {
            throw new Error("Ogmios passed the Kupo block without finding it");
          }
        }
        throw new Error("Ogmios block scan exceeded its safety bound");
      } finally {
        await session.close();
      }
    })();
    cache.set(key, read);
    try {
      const block = await read;
      throwIfSourceAborted(signal);
      return block;
    } catch (cause) {
      cache.delete(key);
      throw cause;
    }
  };

  const readRawTransaction = async (
    {
      txHash,
      point,
    }: {
      readonly txHash: string;
      readonly point: KupoPoint;
    },
    referenceScope?: ReferenceReadScope,
  ): Promise<OgmiosRawTransactionAtPoint> => {
    const block = await readBlock(point, referenceScope);
    const candidates = block.transactions.filter(
      (entry) => record(entry, "Ogmios block transaction").id === txHash,
    );
    if (candidates.length !== 1) {
      throw new Error(`Ogmios block does not contain exactly one ${txHash}`);
    }
    if (referenceScope !== undefined)
      boundedReferenceCbor(
        record(candidates[0], "creating transaction").cbor,
        "creating full transaction",
      );
    return {
      txHash,
      transactionCbor: requireOgmiosRawTransactionCbor({
        value: candidates[0],
        expectedTxHash: txHash,
        label: `Ogmios transaction ${txHash}`,
      }),
      point: rawPoint(block.point),
    };
  };

  const fetchMatches = async (
    pattern: string,
    referenceScope?: ReferenceReadScope,
  ): Promise<readonly KupoMatch[]> =>
    parseKupoMatches(
      await getKupoJson(
        `/matches/${encodeURIComponent(pattern)}?resolve_hashes&order=oldest_first`,
        referenceScope,
      ),
      `Kupo matches ${pattern}`,
      referenceScope,
    );

  const fetchOutRefMatch = async (
    {
      txHash,
      outputIndex,
    }: {
      readonly txHash: string;
      readonly outputIndex: number;
    },
    referenceScope?: ReferenceReadScope,
  ): Promise<KupoMatch> => {
    const matches = (
      await fetchMatches(`${outputIndex.toString()}@${txHash}`, referenceScope)
    ).filter(
      (candidate) =>
        candidate.txHash === txHash && candidate.outputIndex === outputIndex,
    );
    if (matches.length !== 1) {
      throw new Error(
        `Kupo has no unique match for ${txHash}#${outputIndex.toString()}`,
      );
    }
    return matches[0]!;
  };

  const utxoFromMatch = async (
    match: KupoMatch,
  ): Promise<FraudProofRawL1Utxo> => {
    const transaction = await readRawTransaction({
      txHash: match.txHash,
      point: match.createdAt,
    });
    const output = transactionOutput({
      transactionCbor: transaction.transactionCbor,
      outputIndex: match.outputIndex,
      label: `Kupo output ${match.txHash}#${match.outputIndex.toString()}`,
    });
    assertMatchOutput({
      match,
      output,
      label: `Kupo output ${match.txHash}#${match.outputIndex.toString()}`,
    });
    return rawUtxoFromOutput({
      txHash: match.txHash,
      outputIndex: match.outputIndex,
      output,
    });
  };

  const pointCache = new Map<string, Promise<FraudProofRawL1Point>>();
  const admittedPoint = async (
    point: KupoPoint,
  ): Promise<FraudProofRawL1Point> => {
    throwIfSourceAborted(signal);
    const key = `${point.slot.toString()}:${point.blockHash}`;
    const cached = pointCache.get(key);
    if (cached !== undefined) {
      const admitted = await cached;
      throwIfSourceAborted(signal);
      return admitted;
    }
    const read = readBlock(point).then((block) => rawPoint(block.point));
    pointCache.set(key, read);
    const admitted = await read;
    throwIfSourceAborted(signal);
    return admitted;
  };

  let activeBoundary:
    | {
        readonly point: FraudProofRawL1Point;
        readonly tip: FraudProofRawL1Point;
      }
    | undefined;
  const addressCache = new Map<
    string,
    Promise<readonly FraudProofRawL1Utxo[]>
  >();
  const historyCache = new Map<
    string,
    Promise<
      readonly {
        readonly txHash: string;
        readonly inclusionPoint: FraudProofRawL1Point;
      }[]
    >
  >();

  const assertBoundary = (point: FraudProofRawL1Point): void => {
    throwIfSourceAborted(signal);
    if (
      activeBoundary === undefined ||
      !sameRawPoint(activeBoundary.point, point)
    ) {
      throw new Error("Kupmios source call is outside its pinned boundary");
    }
  };

  const readBlockAtPoint = async (
    {
      point: requested,
    }: {
      readonly point: FraudProofRawL1Point;
    },
    referenceScope?: ReferenceReadScope,
  ): Promise<LocalKupmiosRawBlockAtPoint> => {
    throwIfSourceAborted(signal);
    const point = admitFraudProofRawL1Point(
      requested,
      "local Kupmios exact block point",
    );
    const slot = Number(point.slot);
    if (!Number.isSafeInteger(slot)) {
      throw new Error("local Kupmios exact block slot exceeds safe range");
    }
    const expectedKupoPoint = {
      slot,
      blockHash: point.blockHash,
    };
    const before = await getKupoCheckpoint(slot, referenceScope);
    if (!sameKupoPoint(before, expectedKupoPoint)) {
      throw new LocalKupmiosExactPointNotCanonicalError(
        "Kupo exact checkpoint does not contain the requested block",
      );
    }
    const block = await readBlock(before, referenceScope);
    const observedPoint = rawPoint(block.point);
    if (!sameRawPoint(observedPoint, point)) {
      throw new Error("Ogmios exact block point differs from the request");
    }
    if (
      referenceScope !== undefined &&
      block.transactions.length >
        LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.targetTransactions
    )
      throw new Error("reference target transaction count exceeds bounds");
    let targetBytes = 0;
    const transactions = block.transactions.map((value, index) => {
      const transaction = record(
        value,
        `Ogmios exact block transaction ${index.toString()}`,
      );
      const transactionHash = digest(
        transaction.id,
        `Ogmios exact block transaction ${index.toString()}.id`,
      );
      if (referenceScope !== undefined) {
        const bytes =
          boundedReferenceCbor(
            transaction.cbor,
            "reference target full transaction",
          ).length / 2;
        if (
          bytes >
          LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.targetTransactionBytes -
            targetBytes
        )
          throw new Error("reference target transaction bytes exceed bounds");
        targetBytes += bytes;
      }
      return {
        txHash: transactionHash,
        transactionCbor: requireOgmiosRawTransactionCbor({
          value: transaction,
          expectedTxHash: transactionHash,
          label: `Ogmios exact block transaction ${index.toString()}`,
        }),
      };
    });
    if (
      new Set(transactions.map(({ txHash }) => txHash)).size !==
      transactions.length
    ) {
      throw new Error("Ogmios exact block contains duplicate transaction ids");
    }
    const after = await getKupoCheckpoint(slot, referenceScope);
    throwIfSourceAborted(signal);
    if (
      !sameKupoPoint(after, expectedKupoPoint) ||
      !sameKupoPoint(after, before)
    ) {
      throw new LocalKupmiosExactPointNotCanonicalError(
        "Kupo rolled back during exact raw block capture",
      );
    }
    return {
      schemaVersion: LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
      sourceId,
      point: observedPoint,
      parentBlockHash: block.parentBlockHash,
      kupoCheckpoint: after,
      transactions,
    };
  };

  const readReferenceBodies = async (
    point: FraudProofRawL1Point,
  ): Promise<LocalKupmiosReferenceBodiesAtPoint> => {
    const scope: ReferenceReadScope = {
      responseBytes: 0,
      inspectedMembers: 0,
      head:
        pinnedKupoResponseHead === undefined
          ? undefined
          : Object.freeze({ ...pinnedKupoResponseHead }),
      rawBlocks: new Map(),
    };
    try {
      throwIfSourceAborted(signal);
      const target = await readBlockAtPoint({ point }, scope);
      throwIfSourceAborted(signal);
      const required = new Map<string, Map<number, number>>();
      let referenceOccurrences = 0;
      for (const raw of target.transactions) {
        let transaction: CML.Transaction | undefined;
        let body: CML.TransactionBody | undefined;
        let references: CML.TransactionInputList | undefined;
        try {
          transaction = CML.Transaction.from_cbor_hex(raw.transactionCbor);
          if (transaction.to_cbor_hex() !== raw.transactionCbor)
            throw new Error(
              "reference target transaction encoding is not preserved",
            );
          if (!transaction.is_valid()) continue;
          body = transaction.body();
          references = body.reference_inputs();
          const count = references?.len() ?? 0;
          if (
            count >
              LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.transactionReferences ||
            count >
              LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.referenceOccurrences -
                referenceOccurrences
          )
            throw new Error("reference target input roster exceeds bounds");
          referenceOccurrences += count;
          const unique = new Set<string>();
          for (let index = 0; index < count; index += 1) {
            const input = references!.get(index);
            const id = input.transaction_id();
            try {
              const txHash = id.to_hex();
              const outputIndex = Number(input.index());
              if (!Number.isSafeInteger(outputIndex) || outputIndex < 0)
                throw new Error("reference output index exceeds safe range");
              const outRef = `${txHash}#${outputIndex.toString()}`;
              if (unique.has(outRef))
                throw new Error("reference target input roster is not unique");
              unique.add(outRef);
              let outputs = required.get(txHash);
              if (outputs === undefined) {
                if (
                  required.size >=
                  LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.creatingBodies
                )
                  throw new Error(
                    "reference creating-body collection exceeds bounds",
                  );
                outputs = new Map();
                required.set(txHash, outputs);
              }
              outputs.set(outputIndex, (outputs.get(outputIndex) ?? 0) + 1);
            } finally {
              id.free();
              input.free();
            }
          }
        } finally {
          references?.free();
          body?.free();
          transaction?.free();
        }
      }
      let evidenceBytes = 0;
      const bodies: string[] = [];
      for (const [txHash, requiredOutputs] of [...required].sort(
        ([left], [right]) => left.localeCompare(right),
      )) {
        let creatingPoint: KupoPoint | undefined;
        for (const outputIndex of requiredOutputs.keys()) {
          const match = await fetchOutRefMatch({ txHash, outputIndex }, scope);
          throwIfSourceAborted(signal);
          if (match.createdAt.slot > Number(target.point.slot))
            throw new Error("reference creating point is after its target");
          if (
            creatingPoint !== undefined &&
            !sameKupoPoint(creatingPoint, match.createdAt)
          )
            throw new Error(
              "reference creating transaction has inconsistent points",
            );
          creatingPoint = match.createdAt;
        }
        if (creatingPoint === undefined)
          throw new Error(
            "reference creating transaction has no requested output",
          );
        const before = await getKupoCheckpoint(creatingPoint.slot, scope);
        if (!sameKupoPoint(before, creatingPoint))
          throw new LocalKupmiosExactPointNotCanonicalError(
            "reference creating checkpoint differs from its match",
          );
        const raw = await readRawTransaction(
          { txHash, point: creatingPoint },
          scope,
        );
        throwIfSourceAborted(signal);
        if (
          raw.point.blockHash !== creatingPoint.blockHash ||
          raw.point.slot !== creatingPoint.slot.toString() ||
          BigInt(raw.point.blockNo) > BigInt(target.point.blockNo)
        )
          throw new Error(
            "reference creating transaction point differs from its lookup",
          );
        const after = await getKupoCheckpoint(creatingPoint.slot, scope);
        if (!sameKupoPoint(after, before))
          throw new LocalKupmiosExactPointNotCanonicalError(
            "reference creating checkpoint changed during acquisition",
          );
        let transaction: CML.Transaction | undefined;
        let body: CML.TransactionBody | undefined;
        let outputs: CML.TransactionOutputList | undefined;
        let bodyHash: CML.TransactionHash | undefined;
        try {
          transaction = CML.Transaction.from_cbor_hex(raw.transactionCbor);
          if (transaction.to_cbor_hex() !== raw.transactionCbor)
            throw new Error(
              "reference creating transaction encoding is not preserved",
            );
          body = transaction.body();
          const bodyCbor = boundedReferenceCbor(
            body.to_cbor_hex(),
            "reference creating body",
          );
          bodyHash = CML.hash_transaction(body);
          if (bodyHash.to_hex() !== txHash)
            throw new Error(
              "reference creating body differs from requested ledger identity",
            );
          const bodyBytes = bodyCbor.length / 2;
          if (
            bodyBytes >
            LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.evidenceBytes -
              evidenceBytes
          )
            throw new Error(
              "reference creating-body/output byte budget exceeded",
            );
          evidenceBytes += bodyBytes;
          outputs = body.outputs();
          for (const [outputIndex, occurrences] of requiredOutputs) {
            const output =
              outputIndex < outputs.len()
                ? outputs.get(outputIndex)
                : outputIndex === outputs.len()
                  ? body.collateral_return()
                  : undefined;
            if (output === undefined)
              throw new Error("reference creating output index does not exist");
            try {
              const outputBytes =
                boundedReferenceCbor(
                  output.to_canonical_cbor_hex(),
                  "reference selected output",
                ).length / 2;
              if (
                outputBytes >
                Math.floor(
                  (LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.evidenceBytes -
                    evidenceBytes) /
                    occurrences,
                )
              )
                throw new Error(
                  "reference creating-body/output byte budget exceeded",
                );
              evidenceBytes += outputBytes * occurrences;
            } finally {
              output.free();
            }
          }
          bodies.push(bodyCbor);
        } finally {
          bodyHash?.free();
          outputs?.free();
          body?.free();
          transaction?.free();
        }
      }
      const after = await readBlockAtPoint({ point }, scope);
      throwIfSourceAborted(signal);
      if (
        !sameRawPoint(after.point, target.point) ||
        after.parentBlockHash !== target.parentBlockHash ||
        after.transactions.length !== target.transactions.length ||
        after.transactions.some(
          (transaction, index) =>
            transaction.txHash !== target.transactions[index]!.txHash ||
            transaction.transactionCbor !==
              target.transactions[index]!.transactionCbor,
        )
      )
        throw new Error("reference acquisition complete target changed");
      return Object.freeze({
        targetBlock: Object.freeze({
          ...after,
          point: Object.freeze({ ...after.point }),
          kupoCheckpoint: Object.freeze({ ...after.kupoCheckpoint }),
          transactions: Object.freeze(
            after.transactions.map((transaction) =>
              Object.freeze({ ...transaction }),
            ),
          ),
        }),
        creatingTransactionBodies: Object.freeze(bodies),
      });
    } finally {
      scope.rawBlocks.clear();
    }
  };

  const scanAddressPage: LocalKupmiosFraudProofRawSource["scanAddressPage"] =
    async ({ address, throughPoint, after }) => {
      if (after !== null) {
        throw new Error("Kupo match streams have no continuation cursor");
      }
      const key = `${throughPoint.pointId}:${address}`;
      let cached = addressCache.get(key);
      if (cached === undefined) {
        cached = (async () => {
          const matches = await fetchMatches(address);
          const current = matches.filter((match) => {
            if (match.createdAt.slot > Number(throughPoint.slot)) return false;
            if (
              match.createdAt.slot === Number(throughPoint.slot) &&
              match.createdAt.blockHash !== throughPoint.blockHash
            ) {
              throw new Error("Kupo address history forks at the pinned slot");
            }
            if (match.spentAt === null) return true;
            if (match.spentAt.slot > Number(throughPoint.slot)) return true;
            if (
              match.spentAt.slot === Number(throughPoint.slot) &&
              match.spentAt.blockHash !== throughPoint.blockHash
            ) {
              throw new Error(
                "Kupo address spend history forks at the pinned slot",
              );
            }
            return false;
          });
          return await settleLocalKupmiosReads(current.map(utxoFromMatch));
        })();
        addressCache.set(key, cached);
      }
      const utxos = await cached;
      throwIfSourceAborted(signal);
      return {
        checkpoint: throughPoint,
        utxos,
        nextCursor: null,
        complete: true,
      };
    };

  const scanUnitHistoryPage: LocalKupmiosFraudProofRawSource["scanUnitHistoryPage"] =
    async ({ unit, throughPoint, after }) => {
      if (after !== null) {
        throw new Error("Kupo match streams have no continuation cursor");
      }
      if (!/^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u.test(unit)) {
        throw new Error("unit history request is not a canonical Cardano unit");
      }
      const key = `${throughPoint.pointId}:${unit}`;
      let cached = historyCache.get(key);
      if (cached === undefined) {
        cached = (async () => {
          const pattern = `${unit.slice(0, 56)}.${unit.slice(56)}`;
          const matches = await fetchMatches(pattern);
          const points = new Map<string, KupoPoint>();
          for (const match of matches) {
            if (match.createdAt.slot <= Number(throughPoint.slot)) {
              points.set(match.txHash, match.createdAt);
            }
            if (
              match.spentAt !== null &&
              match.spentAt.slot <= Number(throughPoint.slot)
            ) {
              const previous = points.get(match.spentAt.txHash);
              if (
                previous !== undefined &&
                !sameKupoPoint(previous, match.spentAt)
              ) {
                throw new Error(
                  "Kupo unit history assigns one transaction to two points",
                );
              }
              points.set(match.spentAt.txHash, match.spentAt);
            }
          }
          return await settleLocalKupmiosReads(
            [...points.entries()]
              .sort(([left], [right]) => left.localeCompare(right))
              .map(async ([txHash, point]) => ({
                txHash,
                inclusionPoint: await admittedPoint(point),
              })),
          );
        })();
        historyCache.set(key, cached);
      }
      const transactions = await cached;
      throwIfSourceAborted(signal);
      return {
        checkpoint: throughPoint,
        transactions,
        nextCursor: null,
        complete: true,
      };
    };

  const readAtHistoricalPoint = async <T>(
    point: FraudProofRawL1Point,
    read: () => Promise<T>,
  ): Promise<T> => {
    const boundary = activeBoundary;
    assertBoundary(boundary?.point ?? point);
    if (boundary === undefined)
      throw new Error("Kupmios boundary is not pinned");
    if (sameRawPoint(point, boundary.point)) return await read();
    const blockDistance = BigInt(boundary.tip.blockNo) - BigInt(point.blockNo);
    if (
      BigInt(point.blockNo) > BigInt(boundary.point.blockNo) ||
      BigInt(point.slot) > BigInt(boundary.point.slot) ||
      blockDistance + 1n <
        BigInt(config.releaseFinality.policy.confirmationDepth) ||
      blockDistance >
        BigInt(config.releaseFinality.policy.automaticRecoveryMaxDepth)
    ) {
      throw new Error(
        "historical Kupmios point is outside the pinned release recovery window",
      );
    }
    // Exact Kupo checkpoints and Ogmios bytes authenticate this historical
    // context while retaining the active capture's provider head and tip.
    await readBlockAtPoint({ point });
    const result = await read();
    await readBlockAtPoint({ point });
    assertBoundary(boundary.point);
    return result;
  };

  const source: LocalKupmiosFraudProofRawSource = {
    sourceVersion: LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE,
    sourceId,
    kupoHttpUrl,
    ogmiosWebSocketUrl,
    readOutRefsAtPoint: async ({ point, outRefs }) => {
      assertBoundary(point);
      const result: FraudProofRawL1Utxo[] = [];
      for (const outRef of outRefs) {
        const [txHash, index] = outRef.split("#");
        const matches = await fetchMatches(`${index}@${txHash}`);
        if (
          matches.length > 1 ||
          matches.some(
            (match) =>
              match.txHash !== txHash || match.outputIndex.toString() !== index,
          )
        ) {
          throw new Error("Kupo substituted exact output-reference history");
        }
        const match = matches[0];
        if (match === undefined || match.createdAt.slot > Number(point.slot))
          continue;
        if (
          match.createdAt.slot === Number(point.slot) &&
          match.createdAt.blockHash !== point.blockHash
        ) {
          throw new LocalKupmiosExactPointNotCanonicalError(
            "Output creation forks at pinned point",
          );
        }
        if (
          match.spentAt !== null &&
          match.spentAt.slot <= Number(point.slot)
        ) {
          if (
            match.spentAt.slot === Number(point.slot) &&
            match.spentAt.blockHash !== point.blockHash
          ) {
            throw new LocalKupmiosExactPointNotCanonicalError(
              "Output spend forks at pinned point",
            );
          }
          continue;
        }
        result.push(await utxoFromMatch(match));
      }
      return result;
    },
    pinBoundaryAtPoint: async ({ point }) => {
      pinnedKupoResponseHead = undefined;
      const exact = admitFraudProofRawL1Point(
        point,
        "exact native availability boundary",
      );
      const checkpoint = await getKupoCheckpoint(Number(exact.slot));
      const block = await readBlock(checkpoint);
      if (!sameRawPoint(rawPoint(block.point), exact)) {
        throw new LocalKupmiosExactPointNotCanonicalError(
          "Exact boundary differs from canonical Kupo/Ogmios block",
        );
      }
      const tip = await queryTip();
      const depth = tip.blockNo - Number(exact.blockNo) + 1;
      if (
        depth < config.releaseFinality.policy.confirmationDepth ||
        depth > config.releaseFinality.policy.automaticRecoveryMaxDepth
      ) {
        throw new Error(
          "Exact boundary is outside the release finality/recovery window",
        );
      }
      activeBoundary = { point: exact, tip: rawPoint(tip) };
      addressCache.clear();
      historyCache.clear();
      return exact;
    },
    resolveTransactionInclusion: async ({ txHash }) => {
      const matches = await fetchMatches(
        `*@${digest(txHash, "transaction inclusion hash")}`,
      );
      if (matches.length === 0) return null;
      const first = matches[0]!;
      const indices = matches
        .map((match) => match.outputIndex)
        .sort((left, right) => left - right);
      if (
        matches.some(
          (match) =>
            match.txHash !== txHash ||
            !sameKupoPoint(match.createdAt, first.createdAt),
        ) ||
        indices.some((index, position) => index !== position)
      ) {
        throw new Error(
          "Transaction inclusion history is incomplete or substituted",
        );
      }
      const point = await admittedPoint(first.createdAt);
      const canonical = await getKupoCheckpoint(Number(point.slot));
      if (!sameKupoPoint(canonical, first.createdAt)) {
        throw new LocalKupmiosExactPointNotCanonicalError(
          "Transaction inclusion is no longer canonical",
        );
      }
      return point;
    },
    readBoundary: async () => {
      throwIfSourceAborted(signal);
      pinnedKupoResponseHead = undefined;
      activeBoundary = undefined;
      addressCache.clear();
      historyCache.clear();
      rawBlockCache.clear();
      pointCache.clear();
      const tip = await queryTip();
      const minimum = config.releaseFinality.policy.confirmationDepth;
      const maximum = config.releaseFinality.policy.automaticRecoveryMaxDepth;
      let lookbackSlots = Math.max(1, minimum - 1);
      let newerSlot = tip.slot + 1;
      for (let attempt = 0; attempt < 12; attempt += 1) {
        const lookupSlot = Math.max(0, tip.slot - lookbackSlots);
        const checkpoint = await getKupoCheckpoint(lookupSlot);
        const point = await admittedPoint(checkpoint);
        throwIfSourceAborted(signal);
        const depth = tip.blockNo - Number(point.blockNo) + 1;
        if (depth >= minimum) {
          // Slot density varies by chain and by leader election. Refine the
          // bracket by observed block height instead of treating seconds as
          // confirmations; an unnecessarily old boundary can predate activation.
          let selected = point;
          let lower = checkpoint.slot;
          let upper = newerSlot - 1;
          while (depth !== minimum && lower < upper) {
            const probe = Math.floor(lower + (upper - lower + 1) / 2);
            const candidate = await admittedPoint(
              await getKupoCheckpoint(probe),
            );
            throwIfSourceAborted(signal);
            const candidateDepth = tip.blockNo - Number(candidate.blockNo) + 1;
            if (candidateDepth >= minimum) {
              selected = candidate;
              lower = probe;
              if (candidateDepth === minimum) break;
            } else {
              upper = probe - 1;
            }
          }
          if (tip.blockNo - Number(selected.blockNo) + 1 > maximum) break;
          activeBoundary = { point: selected, tip: rawPoint(tip) };
          return {
            kupoCheckpoint: activeBoundary.point,
            ogmiosTip: activeBoundary.tip,
          };
        }
        if (lookupSlot === 0) break;
        newerSlot = lookupSlot;
        lookbackSlots *= 2;
      }
      throw new Error(
        "Kupo/Ogmios could not establish a release-final boundary within the automatic recovery window",
      );
    },
    readBlockAtPoint,
    scanAddressPage: async (input) => {
      assertBoundary(input.throughPoint);
      return await scanAddressPage(input);
    },
    scanUnitHistoryPage: async (input) => {
      assertBoundary(input.throughPoint);
      return await scanUnitHistoryPage(input);
    },
    readTransaction: async ({ txHash, expectedInclusionPoint }) => {
      assertBoundary(activeBoundary?.point ?? expectedInclusionPoint);
      const matches = await fetchMatches(`*@${txHash}`);
      if (
        matches.length === 0 ||
        matches.some((match) => match.txHash !== txHash)
      ) {
        throw new Error(
          `Kupo has no complete transaction-output match for ${txHash}`,
        );
      }
      const indices = matches
        .map((match) => match.outputIndex)
        .sort((left, right) => left - right);
      if (indices.some((value, index) => value !== index)) {
        throw new Error(
          `Kupo transaction-output set for ${txHash} is incomplete`,
        );
      }
      const raw = await readRawTransaction({
        txHash,
        point: {
          slot: Number(expectedInclusionPoint.slot),
          blockHash: expectedInclusionPoint.blockHash,
        },
      });
      if (!sameRawPoint(raw.point, expectedInclusionPoint)) {
        throw new Error(`Ogmios placed ${txHash} at a substituted chain point`);
      }
      const transaction = CML.Transaction.from_cbor_hex(raw.transactionCbor);
      if (!transaction.is_valid()) {
        throw new Error(`transaction ${txHash} is phase-2 invalid`);
      }
      const resolve = async (input: {
        readonly txHash: string;
        readonly outputIndex: number;
      }): Promise<FraudProofRawL1Utxo> =>
        await utxoFromMatch(await fetchOutRefMatch(input));
      const body = transaction.body();
      const resolvedInputs = await settleLocalKupmiosReads(
        transactionInputs(body.inputs()).map(resolve),
      );
      const resolvedReferenceInputs = await settleLocalKupmiosReads(
        transactionInputs(body.reference_inputs()).map(resolve),
      );
      throwIfSourceAborted(signal);
      const witnessSet = transaction.witness_set();
      const redeemers = witnessSet.redeemers();
      const tip = activeBoundary?.tip;
      if (tip === undefined) throw new Error("Kupmios boundary is not pinned");
      const confirmationDepth =
        Number(tip.blockNo) - Number(expectedInclusionPoint.blockNo) + 1;
      const ogmios: FraudProofRawL1Transaction = {
        txHash,
        bodyCbor: body.to_cbor_hex(),
        witnessSetCbor: witnessSet.to_cbor_hex(),
        redeemersCbor: redeemers?.to_canonical_cbor_hex() ?? null,
        isValid: true,
        inclusionPoint: expectedInclusionPoint,
        confirmationDepth,
        resolvedInputs,
        resolvedReferenceInputs,
      };
      return {
        kupo: { txHash, inclusionPoint: expectedInclusionPoint },
        ogmios,
      };
    },
    confirmCanonicalPoint: async ({ point }) => {
      assertBoundary(point);
      const [checkpoint, tip] = await settleLocalKupmiosReads([
        getKupoCheckpoint(Number(point.slot)),
        queryTip(),
      ] as const);
      let canonical = sameKupoPoint(checkpoint, {
        slot: Number(point.slot),
        blockHash: point.blockHash,
      });
      if (canonical) {
        rawBlockCache.delete(`${point.slot}:${point.blockHash}`);
        pointCache.delete(`${point.slot}:${point.blockHash}`);
        const block = await readBlock({
          slot: Number(point.slot),
          blockHash: point.blockHash,
        });
        canonical = sameRawPoint(rawPoint(block.point), point);
      }
      throwIfSourceAborted(signal);
      if (tip.blockNo < Number(point.blockNo)) canonical = false;
      return { canonical, point };
    },
  };
  admittedHistoricalPageReaders.set(
    source,
    Object.freeze({
      address: (input) =>
        readAtHistoricalPoint(input.throughPoint, () => scanAddressPage(input)),
      history: (input) =>
        readAtHistoricalPoint(input.throughPoint, () =>
          scanUnitHistoryPage(input),
        ),
    }),
  );
  admittedReferenceBodyReaders.set(source, readReferenceBodies);
  admittedPredecessorReaders.set(source, async (requestedPoint) => {
    const child = await readBlockAtPoint({ point: requestedPoint });
    if (child.parentBlockHash === null) {
      throw new Error("local Kupmios child has no block predecessor");
    }
    const checkpoint = await readPredecessorCheckpoint(child.kupoCheckpoint);
    const predecessor = await readBlockAtPoint({
      point: await admittedPoint(checkpoint),
    });
    if (
      predecessor.point.blockHash !== child.parentBlockHash ||
      BigInt(predecessor.point.blockNo) + 1n !== BigInt(child.point.blockNo) ||
      BigInt(predecessor.point.slot) >= BigInt(child.point.slot)
    ) {
      throw new Error("local Kupmios blocks do not form a direct predecessor");
    }
    const after = await getKupoCheckpoint(Number(child.point.slot));
    throwIfSourceAborted(signal);
    if (!sameKupoPoint(after, child.kupoCheckpoint)) {
      throw new LocalKupmiosExactPointNotCanonicalError(
        "Kupo rolled back during predecessor point capture",
      );
    }
    return Object.freeze({
      sourceId,
      point: Object.freeze(
        admitFraudProofRawL1Point(child.point, "local Kupmios child point"),
      ),
      predecessorPoint: Object.freeze(
        admitFraudProofRawL1Point(
          predecessor.point,
          "local Kupmios predecessor point",
        ),
      ),
    });
  });
  signedTransactionRecoveryReaders.set(source, async (input) => {
    const signed = inspectSignedWorkflowTransaction(input);
    const boundary = await readAdmittedLocalKupmiosBoundary({ source });
    const canonicalPoint = boundary.ogmiosTip;
    const releaseFinalPoint = boundary.kupoCheckpoint;
    const inputs: { outRef: string; outputCbor: string }[] = [];
    const result = (
      status: SignedTransactionRecoveryObservation["status"],
      reason: string,
    ): SignedTransactionRecoveryObservation =>
      Object.freeze({
        transactionHash: input.transactionHash,
        signedTransactionCborHex: input.signedTransactionCborHex,
        status,
        reason,
        canonicalPoint,
        releaseFinalPoint,
        inputs: Object.freeze(inputs),
      });
    const finish = async (
      status: SignedTransactionRecoveryObservation["status"],
      reason: string,
    ) => {
      const confirmation = exactKeys(
        await source.confirmCanonicalPoint({ point: releaseFinalPoint }),
        ["canonical", "point"],
        [],
        "signed recovery canonical confirmation",
      );
      if (
        confirmation.canonical !== true ||
        !sameRawPoint(
          admitFraudProofRawL1Point(
            confirmation.point,
            "signed recovery confirmed point",
          ),
          releaseFinalPoint,
        )
      )
        throw new LocalKupmiosCheckpointChangedError(
          "Signed recovery release-final boundary rolled back",
        );
      const after = await queryTip();
      if (!sameRawPoint(rawPoint(after), canonicalPoint))
        throw new LocalKupmiosCheckpointChangedError(
          "Canonical tip changed during signed transaction recovery",
        );
      return result(status, reason);
    };
    const tipCheckpoint = await getKupoCheckpoint(Number(canonicalPoint.slot));
    if (
      !sameKupoPoint(tipCheckpoint, {
        slot: Number(canonicalPoint.slot),
        blockHash: canonicalPoint.blockHash,
      })
    )
      return result("unknown", "Kupo has not indexed the exact canonical tip");
    const inclusion = await source.resolveTransactionInclusion!({
      txHash: input.transactionHash,
    });
    if (inclusion !== null) {
      const point = admitFraudProofRawL1Point(
        inclusion,
        "signed recovery inclusion",
      );
      const raw = await readRawTransaction({
        txHash: input.transactionHash,
        point: { slot: Number(point.slot), blockHash: point.blockHash },
      });
      const included = CML.Transaction.from_cbor_hex(raw.transactionCbor);
      if (
        !included.is_valid() ||
        included.body().to_cbor_hex() !== signed.body.to_cbor_hex() ||
        included.witness_set().to_canonical_cbor_hex() !==
          signed.transaction.witness_set().to_canonical_cbor_hex()
      )
        throw new Error(
          "Canonical transaction differs from the recorded signed body",
        );
      return finish(
        "included",
        "Exact recorded transaction body is on the canonical chain",
      );
    }
    let status: SignedTransactionRecoveryObservation["status"] = "rebroadcast";
    let reason =
      "Canonical transaction absent and every recorded input remains unspent";
    for (const outRef of signed.inputOutRefs) {
      const [transactionHash, index] = outRef.split("#");
      const matches = await fetchMatches(`${index}@${transactionHash}`);
      if (
        matches.length !== 1 ||
        matches[0]!.txHash !== transactionHash ||
        matches[0]!.outputIndex.toString() !== index
      ) {
        status = "unknown";
        reason = "A recorded input lacks exact canonical creation history";
        break;
      }
      const match = matches[0]!;
      const output = await utxoFromMatch(match);
      inputs.push({ outRef, outputCbor: output.outputCbor });
      if (match.spentAt !== null) {
        const spending = await readRawTransaction({
          txHash: match.spentAt.txHash,
          point: match.spentAt,
        });
        const spendingBody = CML.Transaction.from_cbor_hex(
          spending.transactionCbor,
        ).body();
        const spent = spendingBody.inputs();
        if (
          !Array.from({ length: spent.len() }, (_, index) =>
            spent.get(index),
          ).some(
            (entry) =>
              `${entry.transaction_id().to_hex()}#${entry.index().toString()}` ===
              outRef,
          )
        )
          throw new Error(
            "Kupo input spend lacks its exact canonical consuming transaction",
          );
        status =
          BigInt(match.spentAt.slot) <= BigInt(releaseFinalPoint.slot)
            ? "conflict"
            : "pending";
        reason = "A recorded input is spent by another canonical transaction";
        break;
      }
    }
    if (status !== "rebroadcast") return finish(status, reason);
    // Missing TTL prevents expiry-based replacement, but does not prevent
    // observing the mempool or replaying the exact still-valid signed body.
    if (
      signed.expiresAtSlot !== undefined &&
      BigInt(releaseFinalPoint.slot) >= signed.expiresAtSlot
    )
      return finish(
        "expired",
        "Recorded TTL passed at the canonical release-final boundary, transaction absent, all exact inputs unspent",
      );
    if (
      signed.expiresAtSlot !== undefined &&
      BigInt(canonicalPoint.slot) >= signed.expiresAtSlot
    )
      return finish(
        "pending",
        "Recorded TTL passed at the tip; release-final expiry proof is not yet available",
      );
    if (
      signed.validFromSlot !== undefined &&
      BigInt(canonicalPoint.slot) < signed.validFromSlot
    )
      return finish(
        "pending",
        "Recorded lower validity bound has not reached the canonical tip",
      );
    const mempool = await openOgmiosSession({
      url: ogmiosWebSocketUrl,
      timeoutMs,
      webSocketFactory,
      signal,
      maxResponseBytes,
    });
    try {
      const acquired = record(
        await mempool.request("acquireMempool", {}),
        "signed recovery mempool snapshot",
      );
      if (
        acquired.acquired !== "mempool" ||
        naturalNumber(acquired.slot, "mempool snapshot slot") <
          Number(canonicalPoint.slot)
      )
        return finish(
          "unknown",
          "Mempool snapshot predates the observed canonical tip",
        );
      const present = await mempool.request("hasTransaction", {
        id: input.transactionHash,
      });
      if (typeof present !== "boolean")
        throw new Error("Invalid mempool transaction verdict");
      if (present)
        return finish(
          "pending",
          "Recorded transaction remains in the node mempool",
        );
    } finally {
      await mempool.close();
    }
    return finish(status, reason);
  });
  signedTransactionRebroadcasters.set(source, async (input, authorize) => {
    const session = await openOgmiosSession({
      url: ogmiosWebSocketUrl,
      timeoutMs,
      webSocketFactory,
      signal,
      maxResponseBytes,
    });
    try {
      // All transport setup awaits precede the live authorization checkpoint.
      await authorize(input);
      const submitted = record(
        await session.request("submitTransaction", {
          transaction: { cbor: input.signedTransactionCborHex },
        }),
        "recorded transaction submission",
      );
      const transaction = record(
        submitted.transaction,
        "recorded submission transaction",
      );
      const hash = digest(transaction.id, "recorded submission hash");
      if (hash !== input.transactionHash)
        throw new Error(
          "Recorded transaction rebroadcast returned a different hash",
        );
      return hash;
    } finally {
      await session.close();
    }
  });
  admittedHttpOgmiosSources.add(source);
  admittedHttpOgmiosSourceDetails.set(
    source,
    Object.freeze({
      sourceId,
      kupoHttpUrl,
      ogmiosUrl: ogmiosHttpUrl,
      deploymentIdentityDigest: config.releaseFinality.deploymentIdentityDigest,
      blueprintHash: config.releaseFinality.blueprintHash,
      finalityPolicyDigest: config.releaseFinality.policyDigest,
      confirmationDepth: config.releaseFinality.policy.confirmationDepth,
      automaticRecoveryMaxDepth:
        config.releaseFinality.policy.automaticRecoveryMaxDepth,
    }),
  );
  return source;
};
