import { normalizeOgmiosHttpUrl } from "@al-ft/midgard-core/ogmios-slot";
import { type OutRefLike } from "@al-ft/midgard-core/out-ref";

/**
 * The shared Ogmios and Kupo client pieces the node's remaining Kupmios
 * readers use (the history source, the ledger snapshot, the state-queue
 * correction observer, availability challenges, and node-tools journeys):
 * chain points, URL normalisation, timed JSON fetches, and the narrowed
 * fetch and WebSocket surfaces a test can stand up. Forced-order carriage
 * no longer reads through here: it is the L1 follower's (plan §12, N10).
 * These readers go with their owning tickets (N1-close, N4, U4).
 */

/** A point on the chain, spelled the way both Ogmios and Kupo spell it. */
export type L1ChainPoint = {
  readonly slot: number;
  readonly headerHash: string;
};

/** One transaction, as much of it as the Kupmios readers need. */
export type ObservedL1Transaction = {
  readonly txHash: string;
  /** Spending inputs in the ledger order presented by Ogmios. */
  readonly spentInputs?: readonly OutRefLike[];
  /** Reference inputs in the order the observation presented them. */
  readonly referenceInputs: readonly OutRefLike[];
  /** Minting policy ids, ascending — the domain of a mint redeemer's index. */
  readonly mintPolicyIds: readonly string[];
  /** Redeemer payloads by `(purpose, index)`, base16 Plutus data. */
  readonly redeemers: readonly ObservedL1Redeemer[];
};

/** The canonical block which carried an observed transaction. */
export type ObservedL1TransactionAtPoint = ObservedL1Transaction & {
  readonly blockPoint: L1ChainPoint & { readonly blockNo: number };
  readonly transactionIndex: number;
  /** Selected-chain tip from the same chain-sync response carrying this block. */
  readonly selectedChainTip?: {
    readonly id: string;
    readonly slot: number;
    readonly height: number;
  };
  /** The raw transaction, when Ogmios serves it (`--include-transaction-cbor`). */
  readonly transactionCbor?: string;
};

export type ObservedL1Redeemer = {
  readonly purpose: string;
  readonly index: number;
  readonly redeemer: string;
};

export type FetchLike = (
  input: string,
  init?: RequestInit,
) => Promise<Response>;

/** The WHATWG surface this module uses, narrowed so a test can stand one up. */
export type WebSocketLike = {
  send(data: string): void;
  close(code?: number, reason?: string): void;
  addEventListener(
    type: string,
    listener: (event: never) => void,
    options?: { once?: boolean },
  ): void;
};

export type WebSocketFactory = (url: string) => WebSocketLike;

/** How far forward a point-fetch rolls before refusing by name. */
export const DEFAULT_L1_BLOCK_SCAN_LIMIT = 1_000;

/** Per-request timeout for both surfaces. */
export const DEFAULT_L1_READ_TIMEOUT_MS = 20_000;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const joinUrl = (base: string, path: string): string =>
  `${base.replace(/\/+$/u, "")}/${path.replace(/^\/+/u, "")}`;

/**
 * Kupo's HTTP base. It is the same normalization the local-Ogmios slot reader
 * uses — `ws:`/`wss:` are folded back to HTTP because an operator who points one
 * L1 key at a WebSocket URL tends to point both.
 */
export const normalizeKupoHttpUrl = (url: string): string =>
  normalizeOgmiosHttpUrl(url);

/** Ogmios's chain-sync endpoint. Chain-sync is stateful, so it is WebSocket. */
export const normalizeOgmiosWebSocketUrl = (url: string): string => {
  const parsed = new URL(url.trim());
  if (parsed.protocol === "http:") {
    parsed.protocol = "ws:";
  } else if (parsed.protocol === "https:") {
    parsed.protocol = "wss:";
  }
  parsed.hash = "";
  return parsed.toString().replace(/\/$/u, "");
};

export const fetchJsonWithTimeout = async (
  fetchImpl: FetchLike,
  url: string,
  timeoutMs: number,
): Promise<unknown> => {
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), timeoutMs);
  try {
    const response = await fetchImpl(url, { signal: controller.signal });
    const body = await response.text();
    if (!response.ok) {
      throw new Error(
        `HTTP ${response.status.toString()} from ${url}: ${body.slice(0, 256)}`,
      );
    }
    try {
      return JSON.parse(body) as unknown;
    } catch (cause) {
      throw new Error(`Malformed JSON from ${url}`, { cause });
    }
  } finally {
    clearTimeout(timeout);
  }
};

export const exactSlot = (value: unknown, label: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${label} is not an absolute slot number`);
  }
  return value;
};

export const exactHeaderHash = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !HEX_32.test(value)) {
    throw new Error(`${label} is not a block header hash`);
  }
  return value;
};

export const exactPoint = (value: unknown, label: string): L1ChainPoint => {
  const record = value as { slot_no?: unknown; header_hash?: unknown };
  return {
    slot: exactSlot(record.slot_no, `${label}.slot_no`),
    headerHash: exactHeaderHash(record.header_hash, `${label}.header_hash`),
  };
};

/**
 * Kupo's `Match`, narrowed to what the Kupmios readers need.
 *
 * **The datum bytes ride the match itself, under `?resolve_hashes`.** From v2.10.0
 * that flag instruments the server "to perform joins on datums and scripts to
 * retrieve any known values associated to hashes", and the schema is exact about
 * what it does to the shape: `datum` — like `script` — "is only and always present
 * (yet may be `null`) if `?resolve_hashes` was set". The reader stands on both
 * halves of that sentence. *Absent* means the flag was not honoured at all, which
 * only a Kupo below the deployment floor does. *Present and `null`* means Kupo has
 * no bytes for a hash it does hold a reference to. Neither is an output without a
 * datum, and neither may be read as one.
 *
 * `datum_type` still says which kind of datum an output carried — `inline` for one
 * the ledger put in the output, `hash` for one it only referenced — and is "only
 * present when `datum_hash` is not `null`".
 */
export type KupoMatch = {
  readonly transaction_id?: unknown;
  readonly output_index?: unknown;
  readonly created_at?: unknown;
  readonly spent_at?: unknown;
  readonly datum_hash?: unknown;
  readonly datum_type?: unknown;
  readonly datum?: unknown;
};
