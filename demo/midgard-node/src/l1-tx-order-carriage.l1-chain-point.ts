import {
  type MidgardFieldCarriage,
  type ResolvedCarriageReferenceInput,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { type OutRefLike } from "@al-ft/midgard-core/out-ref";

import { normalizeOgmiosHttpUrl } from "./local-ledger-slot.js";

/**
 * The node's own read of a forced order's `docs/spec/midgard-tx.md` §8 carriage
 * off L1 — Ogmios chain-sync for the order-creation transaction's mint redeemer,
 * Kupo for the chain point that locates it and for the reference-input datums it
 * names, and nothing else.
 *
 * **Why this module exists.** §8.11 makes the order **mint** the only on-chain
 * reader of an order's material: once it has authenticated a field's preimage
 * against the committed hash, "those bytes are permanent L1 history — which is
 * what the operator and the node's ingestion walk read". The ingestion walk in
 * `fibers/fetch-and-insert-tx-order-utxos.ts` could not read them. Its only L1
 * source was the `Lucid` service, whose `Kupmios` provider returns outputs and
 * never a transaction's witness set, so a material-bearing order was refused by
 * name and only the canonically-empty one ingested (#599).
 *
 * **The dependency boundary is Ogmios + Kupo, and it is binding** (#599 owner
 * ruling, 2026-08-13). `midgard-watcher` already sees witness sets, and having
 * the node consume its normalized block view was considered and **rejected**: the
 * watcher must stay runnable by anyone without an operator node, and a
 * node→watcher edge inverts that. Both endpoints this module speaks to are
 * already in the node's configuration (`L1_OGMIOS_KEY`, `L1_KUPO_KEY`).
 *
 * **The read is a targeted point-fetch, not a follower.** The fiber already
 * holds the order UTxO; Kupo's match for it carries `created_at {slot_no,
 * header_hash}`, the exact chain point of the transaction that created it. Kupo's
 * `/checkpoints/{slot}` answers with the closest checkpoint *before* a slot —
 * documented as being "particularly useful to find ancestors to known slots" —
 * which is the intersection point chain-sync needs, because `findIntersection`
 * sets the read pointer *at* a point and `nextBlock` then delivers what follows
 * it. From that ancestor the scan rolls forward to the block whose header hash is
 * the one Kupo named, and takes the transaction out of it.
 *
 * **What is duplicated, and from where.** `midgard-watcher`'s `l1-adapter.ts`
 * decodes witness sets and redeemers from an Ogmios source, and its
 * `user-event-indexer.ts` decodes this exact wrapped tx-order mint redeemer and
 * re-derives §8.11's exhaustion and burn-empty-vector rules. Neither is imported:
 * the package boundary above is the point, and the ruling makes the duplication
 * the correct outcome rather than a shortcut. The mirrored pieces are named at
 * their sites so a future consolidation is findable — `decodeMintRedeemer`
 * (watcher) ↔ {@link txOrderMintCarriageVector} here, and the watcher's
 * `forcedOrderMaterialFieldCount` ↔ the fiber's own
 * `forcedOrderMaterialFieldCountV1`. The redeemer's *schema* is not duplicated: it
 * is `@al-ft/midgard-sdk`'s `TxOrderMintRedeemer`, the shared source of truth
 * both packages decode against.
 *
 * **This module supplies bytes; it authenticates nothing.** Everything it
 * returns is a claim, and `reconstructTxOrderMaterialV1` opens each entry through
 * the §8.8 door against the *payload's own* §4 commitments. That is what makes a
 * wrong redeemer, a wrong reference-input order, a stale Kupo view or a hostile
 * Ogmios into a refusal rather than a corruption: no path here can widen what the
 * walk accepts, only fail to find bytes it would have accepted.
 *
 * **Deployment requirements, all three of them operator-visible.** *(i)* Kupo must
 * index the order address *and* the §8 carriage the order references: raw carriage
 * and certificates live at the order creator's own wallet address (§8.11's custody
 * rule), which no operator can enumerate in advance, so a pattern-restricted Kupo
 * cannot resolve tier 2/3 carriage — `docker-compose.kupmios.yaml` runs Kupo
 * with `--match "*"`, which satisfies this. *(ii)* Kupo must **not** run
 * `--prune-utxo`: a pruning index deletes matches once their output is spent,
 * while §8.7 lets a creator reclaim a carriage UTxO at any time and §8.11 keeps
 * that UTxO's bytes normative L1 history regardless — so under pruning, every
 * tier-2/3 order whose carriage has been reclaimed silently stops being readable,
 * which is why the queries below are never filtered to `unspent`. *(iii)* Kupo
 * must be **v2.10.0 or newer** (2025-01-03), which is the release that added the
 * `?resolve_hashes` query flag: this reader asks for its matches with that flag
 * and takes the datum bytes off the match itself, in one request per output. The
 * floor is a hard one *because* an older Kupo does not reject an unknown query
 * flag — it silently ignores it and answers with a match that has no `datum`
 * field at all. {@link fetchKupoMatch} refuses that answer by name rather than
 * reading it as "this output carries no datum", so a mis-deployed index fails the
 * read loudly on its first request instead of quietly emptying carriage indices.
 * `docker-compose.kupmios.yaml` pins v2.11.0 by digest.
 */

/** A point on the chain, spelled the way both Ogmios and Kupo spell it. */
export type L1ChainPoint = {
  readonly slot: number;
  readonly headerHash: string;
};

/**
 * The §8 carriage an order's material rides, as the ingestion walk receives it.
 *
 * `carriage` is positional over the order's **non-empty** fields in ascending
 * field index — byte-for-byte the same vector the order's mint redeemer carried,
 * because it is the same claim read back. `referenceInputs` are the resolved
 * carriage UTxOs the `RawUtxo`/`Certified` entries index into, in the order the
 * ledger presented them to the mint.
 */
export type TxOrderMaterialCarriage = {
  readonly carriage: readonly MidgardFieldCarriage[];
  readonly referenceInputs?: readonly ResolvedCarriageReferenceInput[];
};

/** One transaction, as much of it as sourcing a §8 carriage needs. */
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

/**
 * Transport knobs for the read. Everything that decides *what* is read — which
 * order, which policy, which endpoints — comes from the node's configuration and
 * the visible UTxO, never from here.
 */
export type TxOrderCarriageReadOptions = {
  readonly fetchImpl?: FetchLike;
  readonly webSocketFactory?: WebSocketFactory;
  readonly timeoutMs?: number;
  readonly blockScanLimit?: number;
};

/**
 * How far forward a point-fetch will roll before giving up.
 *
 * Kupo keeps checkpoints densely near the tip and sparsely behind it, so the
 * ancestor it answers with for a *recent* order — the only kind an ingestion walk
 * meets, since it reconciles the visible order set every tick — is a handful of
 * blocks back. The bound exists so that a deep or pruned checkpoint turns into a
 * named refusal instead of an unbounded scan.
 */
export const DEFAULT_TX_ORDER_CARRIAGE_BLOCK_SCAN_LIMIT = 1_000;

/** Per-request timeout for both surfaces. */
export const DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS = 20_000;

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
 * Kupo's `Match`, narrowed to what a carriage read needs.
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
