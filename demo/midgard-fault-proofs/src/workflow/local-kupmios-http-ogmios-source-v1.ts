import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import {
  LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE,
  type LocalKupmiosFraudProofRawSource,
} from "./local-kupmios-raw-l1-authority-v1.js";
import {
  admitFraudProofRawL1Point,
  admitFraudProofRawL1Transaction,
  admitFraudProofRawL1Utxo,
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
} from "./raw-l1-snapshot-v1.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy-v1.js";

export const OGMIOS_RAW_TRANSACTION_CBOR_FLAG =
  "--include-transaction-cbor" as const;
export const LOCAL_KUPMIOS_HTTP_OGMIOS_SOURCE =
  "midgard-local-kupo-http-ogmios-ws-source-v1" as const;
export const LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT =
  "midgard-local-kupmios-raw-block-at-point-v1" as const;

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

export type LocalKupmiosHttpOgmiosRawSourceDetails = Readonly<{
  sourceId: string;
  kupoHttpUrl: string;
  ogmiosUrl: string;
  deploymentIdentityDigest: string;
  releaseIdentityDigest: string;
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

const DEFAULT_TIMEOUT_MS = 20_000;
const DEFAULT_BLOCK_SCAN_LIMIT = 2_000;
const MAX_MATCHES = 100_000;
const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_28 = /^[0-9a-f]{56}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(0|[1-9][0-9]*)$/u;
const MAX_RESPONSE_BYTES = 64 * 1024 * 1024;

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
}: {
  readonly fetchImpl: FraudProofRawL1Fetch;
  readonly url: string;
  readonly timeoutMs: number;
  readonly init?: RequestInit;
}): Promise<JsonHttpResponse> => {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  try {
    const response = await fetchImpl(url, {
      ...init,
      signal: controller.signal,
    });
    const contentLength = response.headers.get("content-length");
    if (
      contentLength !== null &&
      (!NATURAL.test(contentLength) ||
        Number(contentLength) > MAX_RESPONSE_BYTES)
    ) {
      throw new Error(`response from ${url} exceeds the raw-source byte bound`);
    }
    const chunks: Buffer[] = [];
    let byteLength = 0;
    if (response.body === null) {
      const body = await response.arrayBuffer();
      byteLength = body.byteLength;
      chunks.push(Buffer.from(body));
    } else {
      const reader = response.body.getReader();
      while (true) {
        const next = await reader.read();
        if (next.done) break;
        byteLength += next.value.byteLength;
        if (byteLength > MAX_RESPONSE_BYTES) {
          controller.abort();
          await reader.cancel("raw-source response byte bound exceeded");
          throw new Error(
            `response from ${url} exceeds the raw-source byte bound`,
          );
        }
        chunks.push(Buffer.from(next.value));
      }
    }
    if (byteLength > MAX_RESPONSE_BYTES) {
      throw new Error(`response from ${url} exceeds the raw-source byte bound`);
    }
    const body = Buffer.concat(chunks, byteLength).toString("utf8");
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
          !/^"[0-9a-f]{64}"$/u.test(checkpointEtag)
        ) {
          throw new Error(
            `response from ${url} has malformed Kupo checkpoint headers`,
          );
        }
        checkpointHeaders = {
          slot: Number(checkpointSlot),
          blockHash: checkpointEtag.slice(1, -1),
        };
      }
      return {
        value: JSON.parse(body) as unknown,
        checkpointHeaders,
      };
    } catch (cause) {
      throw new Error(
        `malformed JSON or checkpoint headers from ${url}: ${String(cause)}`,
      );
    }
  } finally {
    clearTimeout(timer);
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
  if (
    typeof valueRecord.coins !== "string" ||
    !NATURAL.test(valueRecord.coins)
  ) {
    throw new Error(`${label}.value.coins must be canonical lovelace`);
  }
  const assets = record(valueRecord.assets, `${label}.value.assets`);
  const normalizedAssets: Record<string, bigint> = {
    lovelace: BigInt(valueRecord.coins as string),
  };
  for (const [unit, quantity] of Object.entries(assets)) {
    if (
      !/^[0-9a-f]{56}\.(?:[0-9a-f]{2}){0,32}$/u.test(unit) ||
      typeof quantity !== "string" ||
      !NATURAL.test(quantity)
    ) {
      throw new Error(`${label}.value.assets is not canonical Kupo value JSON`);
    }
    normalizedAssets[unit.replace(".", "")] = BigInt(quantity);
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
): readonly KupoMatch[] => {
  if (!Array.isArray(value) || value.length > MAX_MATCHES) {
    throw new Error(`${label} must be a bounded Kupo match array`);
  }
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
  const envelope = exactKeys(value, ["jsonrpc", "id", "result"], [], label);
  if (envelope.jsonrpc !== "2.0") {
    throw new Error(`${label}.jsonrpc is unsupported`);
  }
  const result = exactKeys(
    envelope.result,
    ["slot", "id", "height"],
    [],
    `${label}.result`,
  );
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
  close(): void;
};

const defaultWebSocketFactory: FraudProofRawL1WebSocketFactory = (url) =>
  new WebSocket(url) as unknown as FraudProofRawL1WebSocketLike;

const openOgmiosSession = async ({
  url,
  timeoutMs,
  webSocketFactory,
}: {
  readonly url: string;
  readonly timeoutMs: number;
  readonly webSocketFactory: FraudProofRawL1WebSocketFactory;
}): Promise<OgmiosSession> => {
  const socket = webSocketFactory(url);
  const pending = new Map<
    number,
    { resolve(value: unknown): void; reject(error: Error): void }
  >();
  let nextId = 0;
  let terminal: Error | null = null;
  const fail = (error: Error): void => {
    terminal ??= error;
    for (const waiter of pending.values()) waiter.reject(error);
    pending.clear();
  };
  socket.addEventListener("message", ((event: { data: unknown }) => {
    if (typeof event.data !== "string") {
      fail(new Error("Ogmios sent a non-text frame"));
      return;
    }
    let message: { id?: unknown; result?: unknown; error?: unknown };
    try {
      message = JSON.parse(event.data) as typeof message;
    } catch (cause) {
      fail(new Error(`Ogmios sent malformed JSON: ${String(cause)}`));
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
  socket.addEventListener("error", (() =>
    fail(new Error("Ogmios socket failed"))) as (event: never) => void);
  socket.addEventListener("close", (() =>
    fail(new Error("Ogmios socket closed"))) as (event: never) => void);
  await new Promise<void>((resolve, reject) => {
    const timer = setTimeout(() => {
      socket.close();
      reject(
        new Error(
          `Ogmios socket did not open within ${timeoutMs.toString()}ms`,
        ),
      );
    }, timeoutMs);
    socket.addEventListener(
      "open",
      (() => {
        clearTimeout(timer);
        resolve();
      }) as (event: never) => void,
      { once: true },
    );
    socket.addEventListener(
      "error",
      (() => {
        clearTimeout(timer);
        reject(new Error("Ogmios socket failed while opening"));
      }) as (event: never) => void,
      { once: true },
    );
  });
  return {
    request: async (method, params) => {
      if (terminal !== null) throw terminal;
      const id = nextId;
      nextId += 1;
      return await new Promise<unknown>((resolve, reject) => {
        const timer = setTimeout(() => {
          pending.delete(id);
          reject(new Error(`Ogmios ${method} timed out`));
        }, timeoutMs);
        pending.set(id, {
          resolve: (value) => {
            clearTimeout(timer);
            resolve(value);
          },
          reject: (error) => {
            clearTimeout(timer);
            reject(error);
          },
        });
        socket.send(JSON.stringify({ jsonrpc: "2.0", method, params, id }));
      });
    },
    close: () => socket.close(),
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
): {
  readonly point: OgmiosTip;
  readonly parentBlockHash: string | null;
  readonly transactions: readonly unknown[];
} => {
  const parsed = record(value, label);
  if (!Array.isArray(parsed.transactions)) {
    throw new Error(`${label}.transactions must be an array`);
  }
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
  if (CML.hash_transaction(transaction.body()).to_hex() !== expectedTxHash) {
    throw new Error(`${label}.cbor hashes to a different transaction`);
  }
  return transactionCbor;
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

/** Reads one complete, release-bounded unit history at the pinned boundary. */
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
    await source.scanUnitHistoryPage({
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
    await source.scanAddressPage({
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
  const inlineDatum =
    output.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null;
  const datumHash = output.datum_hash()?.to_hex() ?? null;
  if (match.datumType === "inline") {
    if (
      inlineDatum === null ||
      inlineDatum !== match.datum ||
      CML.hash_plutus_data(
        CML.PlutusData.from_cbor_hex(inlineDatum),
      ).to_hex() !== match.datumHash
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
    if (
      actualScript.hash().to_hex() !== match.scriptHash ||
      typeof match.script !== "string" ||
      !EVEN_HEX.test(match.script)
    ) {
      throw new Error(`${label} Kupo reference-script identity is malformed`);
    }
    let kupoScript: CML.Script;
    try {
      kupoScript = CML.Script.from_cbor_hex(match.script);
    } catch {
      throw new Error(
        `${label} Kupo reference script is not canonical CML CBOR`,
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
  if (!Number.isSafeInteger(blockScanLimit) || blockScanLimit <= 0) {
    throw new Error("Ogmios blockScanLimit must be positive");
  }

  let pinnedKupoResponseHead: KupoPoint | undefined;
  const getKupoJson = async (path: string): Promise<unknown> => {
    const response = await fetchJson({
      fetchImpl,
      url: joinUrl(kupoHttpUrl, path),
      timeoutMs,
      init: {
        headers: {
          accept: "application/json;asset-quantity=string",
        },
      },
    });
    if (response.checkpointHeaders === null) {
      throw new Error("Kupo response omitted X-Most-Recent-Checkpoint or ETag");
    }
    if (pinnedKupoResponseHead === undefined) {
      pinnedKupoResponseHead = response.checkpointHeaders;
    } else if (
      !sameKupoPoint(pinnedKupoResponseHead, response.checkpointHeaders)
    ) {
      throw new Error(
        "Kupo advanced or rolled back during raw snapshot capture",
      );
    }
    return response.value;
  };

  const queryTip = async (): Promise<OgmiosTip> =>
    parseOgmiosTip(
      (
        await fetchJson({
          fetchImpl,
          url: ogmiosHttpUrl,
          timeoutMs,
          init: {
            method: "POST",
            headers: { "content-type": "application/json" },
            body: JSON.stringify({
              jsonrpc: "2.0",
              method: "queryNetwork/tip",
              id: "midgard-fraud-proof-raw-tip-v1",
            }),
          },
        })
      ).value,
      "Ogmios tip",
    );

  const getKupoCheckpoint = async (slot: number): Promise<KupoPoint> =>
    parseKupoPoint(
      await getKupoJson(`/checkpoints/${slot.toString()}`),
      `Kupo checkpoint ${slot.toString()}`,
    );

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
  ): Promise<{
    readonly point: OgmiosTip;
    readonly parentBlockHash: string | null;
    readonly transactions: readonly unknown[];
  }> => {
    const key = `${target.slot.toString()}:${target.blockHash}`;
    const cached = rawBlockCache.get(key);
    if (cached !== undefined) return await cached;
    const read = (async () => {
      if (target.slot === 0)
        throw new Error("cannot chain-sync before genesis");
      const ancestor = await getKupoCheckpoint(target.slot - 1);
      if (sameKupoPoint(ancestor, target)) {
        throw new Error("Kupo did not return an ancestor checkpoint");
      }
      const session = await openOgmiosSession({
        url: ogmiosWebSocketUrl,
        timeoutMs,
        webSocketFactory,
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
          const block = parseOgmiosBlock(next.block, "Ogmios nextBlock.block");
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
        session.close();
      }
    })();
    rawBlockCache.set(key, read);
    try {
      return await read;
    } catch (cause) {
      rawBlockCache.delete(key);
      throw cause;
    }
  };

  const readRawTransaction = async ({
    txHash,
    point,
  }: {
    readonly txHash: string;
    readonly point: KupoPoint;
  }): Promise<OgmiosRawTransactionAtPoint> => {
    const block = await readBlock(point);
    const candidates = block.transactions.filter(
      (entry) => record(entry, "Ogmios block transaction").id === txHash,
    );
    if (candidates.length !== 1) {
      throw new Error(`Ogmios block does not contain exactly one ${txHash}`);
    }
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

  const fetchMatches = async (pattern: string): Promise<readonly KupoMatch[]> =>
    parseKupoMatches(
      await getKupoJson(
        `/matches/${encodeURIComponent(pattern)}?resolve_hashes&order=oldest_first`,
      ),
      `Kupo matches ${pattern}`,
    );

  const fetchOutRefMatch = async ({
    txHash,
    outputIndex,
  }: {
    readonly txHash: string;
    readonly outputIndex: number;
  }): Promise<KupoMatch> => {
    const matches = (
      await fetchMatches(`${outputIndex.toString()}@${txHash}`)
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
    const key = `${point.slot.toString()}:${point.blockHash}`;
    const cached = pointCache.get(key);
    if (cached !== undefined) return await cached;
    const read = readBlock(point).then((block) => rawPoint(block.point));
    pointCache.set(key, read);
    return await read;
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
    if (
      activeBoundary === undefined ||
      !sameRawPoint(activeBoundary.point, point)
    ) {
      throw new Error("Kupmios source call is outside its pinned boundary");
    }
  };

  const source: LocalKupmiosFraudProofRawSource = {
    sourceVersion: LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE,
    sourceId,
    kupoHttpUrl,
    ogmiosWebSocketUrl,
    readBoundary: async () => {
      pinnedKupoResponseHead = undefined;
      activeBoundary = undefined;
      addressCache.clear();
      historyCache.clear();
      rawBlockCache.clear();
      pointCache.clear();
      const tip = await queryTip();
      const minimum = config.releaseFinality.policy.confirmationDepth;
      const maximum = config.releaseFinality.policy.automaticRecoveryMaxDepth;
      let lookbackSlots = Math.max(64, minimum * 20);
      for (let attempt = 0; attempt < 12; attempt += 1) {
        const lookupSlot = Math.max(0, tip.slot - lookbackSlots);
        const checkpoint = await getKupoCheckpoint(lookupSlot);
        const point = await admittedPoint(checkpoint);
        const depth = tip.blockNo - Number(point.blockNo) + 1;
        if (depth >= minimum && depth <= maximum) {
          activeBoundary = { point, tip: rawPoint(tip) };
          return {
            kupoCheckpoint: activeBoundary.point,
            ogmiosTip: activeBoundary.tip,
          };
        }
        if (depth > maximum) break;
        lookbackSlots *= 2;
      }
      throw new Error(
        "Kupo/Ogmios could not establish a release-final boundary within the automatic recovery window",
      );
    },
    readBlockAtPoint: async ({ point: requested }) => {
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
      const before = await getKupoCheckpoint(slot);
      if (!sameKupoPoint(before, expectedKupoPoint)) {
        throw new LocalKupmiosExactPointNotCanonicalError(
          "Kupo exact checkpoint does not contain the requested block",
        );
      }
      const block = await readBlock(before);
      const observedPoint = rawPoint(block.point);
      if (!sameRawPoint(observedPoint, point)) {
        throw new Error("Ogmios exact block point differs from the request");
      }
      const transactions = block.transactions.map((value, index) => {
        const transaction = record(
          value,
          `Ogmios exact block transaction ${index.toString()}`,
        );
        const transactionHash = digest(
          transaction.id,
          `Ogmios exact block transaction ${index.toString()}.id`,
        );
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
        throw new Error(
          "Ogmios exact block contains duplicate transaction ids",
        );
      }
      const after = await getKupoCheckpoint(slot);
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
    },
    scanAddressPage: async ({ address, throughPoint, after }) => {
      assertBoundary(throughPoint);
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
          return await Promise.all(current.map(utxoFromMatch));
        })();
        addressCache.set(key, cached);
      }
      return {
        checkpoint: throughPoint,
        utxos: await cached,
        nextCursor: null,
        complete: true,
      };
    },
    scanUnitHistoryPage: async ({ unit, throughPoint, after }) => {
      assertBoundary(throughPoint);
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
          return await Promise.all(
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
      return {
        checkpoint: throughPoint,
        transactions: await cached,
        nextCursor: null,
        complete: true,
      };
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
      const resolvedInputs = await Promise.all(
        transactionInputs(body.inputs()).map(resolve),
      );
      const resolvedReferenceInputs = await Promise.all(
        transactionInputs(body.reference_inputs()).map(resolve),
      );
      const witnessSet = transaction.witness_set();
      const redeemers = witnessSet.redeemers();
      const tip = activeBoundary?.tip;
      if (tip === undefined) throw new Error("Kupmios boundary is not pinned");
      const confirmationDepth =
        Number(tip.blockNo) - Number(expectedInclusionPoint.blockNo) + 1;
      const ogmios: FraudProofRawL1Transaction = {
        txHash,
        bodyCbor: body.to_canonical_cbor_hex(),
        witnessSetCbor: witnessSet.to_canonical_cbor_hex(),
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
      const [checkpoint, tip] = await Promise.all([
        getKupoCheckpoint(Number(point.slot)),
        queryTip(),
      ]);
      let canonical = sameKupoPoint(checkpoint, {
        slot: Number(point.slot),
        blockHash: point.blockHash,
      });
      if (canonical) {
        try {
          rawBlockCache.delete(`${point.slot}:${point.blockHash}`);
          pointCache.delete(`${point.slot}:${point.blockHash}`);
          const block = await readBlock({
            slot: Number(point.slot),
            blockHash: point.blockHash,
          });
          canonical = sameRawPoint(rawPoint(block.point), point);
        } catch {
          canonical = false;
        }
      }
      if (tip.blockNo < Number(point.blockNo)) canonical = false;
      return { canonical, point };
    },
  };
  admittedHttpOgmiosSources.add(source);
  admittedHttpOgmiosSourceDetails.set(
    source,
    Object.freeze({
      sourceId,
      kupoHttpUrl,
      ogmiosUrl: ogmiosHttpUrl,
      deploymentIdentityDigest: config.releaseFinality.deploymentIdentityDigest,
      releaseIdentityDigest: config.releaseFinality.releaseIdentityDigest,
      finalityPolicyDigest: config.releaseFinality.policyDigest,
      confirmationDepth: config.releaseFinality.policy.confirmationDepth,
      automaticRecoveryMaxDepth:
        config.releaseFinality.policy.automaticRecoveryMaxDepth,
    }),
  );
  return source;
};
