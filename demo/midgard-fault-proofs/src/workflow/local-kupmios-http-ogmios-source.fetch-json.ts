import {
  canonicalAddress,
  debitReferenceMembers,
  debitReferenceResponse,
  digest,
  exactKeys,
  type JsonHttpResponse,
  naturalNumber,
  nullableDigest,
  nullableScriptHash,
  record,
  type ReferenceReadScope,
  referenceResponseLimit,
  throwIfSourceAborted,
} from "./local-kupmios-http-ogmios-source.parse-ogmios-block.js";
import {
  EVEN_HEX,
  type FraudProofRawL1Fetch,
  HEX_32,
  isNetworkFailure,
  type KupoMatch,
  type KupoPoint,
  type KupoSpentPoint,
  LocalKupmiosTransportUnavailableError,
  losslessJson,
  MAX_MATCHES,
  NATURAL,
  type OgmiosTip,
} from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";
import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
} from "./raw-l1-snapshot.js";

export const fetchJson = async ({
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
  let timedOut = false;
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
  const timer = setTimeout(() => {
    timedOut = true;
    abort();
  }, timeoutMs);
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
      const message = `HTTP ${response.status.toString()} from ${url}: ${body.slice(0, 256)}`;
      // A busy or restarting provider, not an answer about the chain.
      if (
        response.status === 408 ||
        response.status === 425 ||
        response.status === 429 ||
        response.status >= 500
      )
        throw new LocalKupmiosTransportUnavailableError(message);
      throw new Error(message);
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
  } catch (cause) {
    // Cancellation belongs to the owner, even when it races a network timeout.
    throwIfSourceAborted(signal);
    if (timedOut || isNetworkFailure(cause))
      throw new LocalKupmiosTransportUnavailableError(
        timedOut
          ? `request to ${url} timed out`
          : `transport to ${url} is unavailable`,
        { cause },
      );
    throw cause;
  } finally {
    clearTimeout(timer);
    signal?.removeEventListener("abort", abort);
    reader?.releaseLock();
  }
};

export const parseKupoPoint = (value: unknown, label: string): KupoPoint => {
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

const KUPO_ASSET_KEY = /^[0-9a-f]{56}(?:\.(?:[0-9a-f]{2}){1,32})?$/u;

const kupoQuantity = (value: unknown, label: string): bigint => {
  if (typeof value === "bigint" && value >= 0n) return value;
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0)
    return BigInt(value);
  if (typeof value === "string" && NATURAL.test(value)) return BigInt(value);
  throw new Error(`${label} must be an exact nonnegative quantity`);
};

export const parseKupoMatch = (value: unknown, label: string): KupoMatch => {
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
    // Kupo writes an empty asset name as the bare policy id, never "<policy>.".
    if (!KUPO_ASSET_KEY.test(unit)) {
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

export const parseKupoMatches = (
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

export const rawPoint = ({
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
