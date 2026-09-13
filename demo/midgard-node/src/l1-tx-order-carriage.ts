import {
  type MidgardFieldCarriage,
  type MidgardFieldPreimageCertificate,
  type ResolvedCarriageReferenceInput,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { compareOutRefs, type OutRefLike } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

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
 * cannot resolve tier 2/3 carriage — `l1-services/docker-compose.yml` runs Kupo
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
 * `l1-services/docker-compose.yml` pins v2.11.0.
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

const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_28 = /^[0-9a-f]{56}$/u;

const joinUrl = (base: string, path: string): string =>
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

const fetchJsonWithTimeout = async (
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

const exactSlot = (value: unknown, label: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${label} is not an absolute slot number`);
  }
  return value;
};

const exactHeaderHash = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !HEX_32.test(value)) {
    throw new Error(`${label} is not a block header hash`);
  }
  return value;
};

const exactPoint = (value: unknown, label: string): L1ChainPoint => {
  const record = value as { slot_no?: unknown; header_hash?: unknown };
  return {
    slot: exactSlot(record.slot_no, `${label}.slot_no`),
    headerHash: exactHeaderHash(record.header_hash, `${label}.header_hash`),
  };
};

/**
 * The chain point at which an output was created, from Kupo's match for it.
 *
 * The pattern is Kupo's `{output_index}@{transaction_id}` output-reference form,
 * and the query is deliberately **not** filtered to unspent matches: an order's
 * own UTxO is unspent when the walk sees it, but this same read locates carriage
 * that §8.7 lets its creator reclaim at any time, and history is what §8.11 says
 * the material is.
 */
export const fetchKupoCreationPoint = async ({
  kupoUrl,
  outRef,
  fetchImpl = fetch,
  timeoutMs = DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
}: {
  readonly kupoUrl: string;
  readonly outRef: OutRefLike;
  readonly fetchImpl?: FetchLike;
  readonly timeoutMs?: number;
}): Promise<L1ChainPoint> => {
  const match = await fetchKupoMatch({
    kupoUrl,
    outRef,
    fetchImpl,
    timeoutMs,
  });
  return exactPoint(
    (match as { created_at?: unknown }).created_at,
    `kupo.match(${outRef.txHash}#${outRef.outputIndex.toString()}).created_at`,
  );
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
type KupoMatch = {
  readonly transaction_id?: unknown;
  readonly output_index?: unknown;
  readonly created_at?: unknown;
  readonly spent_at?: unknown;
  readonly datum_hash?: unknown;
  readonly datum_type?: unknown;
  readonly datum?: unknown;
};

export type KupoSpend = Readonly<{
  point: L1ChainPoint;
  transactionId: string;
  inputIndex: number;
  redeemer: string | null;
}>;

/**
 * Reads the exact canonical spend attached by Kupo to an output match. A null
 * result means the output is currently unspent on Kupo's selected chain; a
 * malformed partial spend is refused rather than treated as absence.
 */
export const fetchKupoSpend = async ({
  kupoUrl,
  outRef,
  fetchImpl = fetch,
  timeoutMs = DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
}: {
  readonly kupoUrl: string;
  readonly outRef: OutRefLike;
  readonly fetchImpl?: FetchLike;
  readonly timeoutMs?: number;
}): Promise<KupoSpend | null> => {
  const match = await fetchKupoMatch({
    kupoUrl,
    outRef,
    fetchImpl,
    timeoutMs,
  });
  if (match.spent_at === null) return null;
  if (typeof match.spent_at !== "object" || match.spent_at === undefined) {
    throw new Error(
      `Kupo match for ${outRef.txHash}#${outRef.outputIndex.toString()} omitted its required spent_at field`,
    );
  }
  const spent = match.spent_at as {
    transaction_id?: unknown;
    input_index?: unknown;
    redeemer?: unknown;
  };
  if (
    typeof spent.transaction_id !== "string" ||
    !HEX_32.test(spent.transaction_id)
  ) {
    throw new Error("Kupo spent_at.transaction_id is not a transaction id");
  }
  if (
    typeof spent.input_index !== "number" ||
    !Number.isSafeInteger(spent.input_index) ||
    spent.input_index < 0
  ) {
    throw new Error("Kupo spent_at.input_index is not an input index");
  }
  if (spent.redeemer !== undefined && typeof spent.redeemer !== "string") {
    throw new Error("Kupo spent_at.redeemer is not base16 data");
  }
  return Object.freeze({
    point: exactPoint(match.spent_at, "kupo.match.spent_at"),
    transactionId: spent.transaction_id,
    inputIndex: spent.input_index,
    redeemer: spent.redeemer ?? null,
  });
};

const fetchKupoMatch = async ({
  kupoUrl,
  outRef,
  fetchImpl,
  timeoutMs,
}: {
  readonly kupoUrl: string;
  readonly outRef: OutRefLike;
  readonly fetchImpl: FetchLike;
  readonly timeoutMs: number;
}): Promise<KupoMatch> => {
  const url = joinUrl(
    normalizeKupoHttpUrl(kupoUrl),
    `/matches/${outRef.outputIndex.toString()}@${outRef.txHash}?resolve_hashes`,
  );
  const body = await fetchJsonWithTimeout(fetchImpl, url, timeoutMs);
  if (!Array.isArray(body)) {
    throw new Error(`Kupo returned no match array for ${url}`);
  }
  const matches = (body as readonly KupoMatch[]).filter(
    (match) =>
      match.transaction_id === outRef.txHash &&
      match.output_index === outRef.outputIndex,
  );
  const [match] = matches;
  if (match === undefined) {
    throw new Error(
      `Kupo has no match for ${outRef.txHash}#${outRef.outputIndex.toString()}`,
    );
  }
  // The deployment floor, checked on the wire rather than assumed. `datum` is
  // "only and always present" under `?resolve_hashes`, so a match without the key
  // is an index that ignored the flag — a Kupo older than v2.10.0. This is
  // asserted on *every* match, not only on the ones that turn out to carry
  // carriage, so a mis-deployed index is named on the first request of the first
  // read rather than by whichever later order is the first to reference a datum.
  if (!("datum" in match)) {
    throw new Error(
      `Kupo did not resolve hashes for ${url}: the match carries no \`datum\` ` +
        "field, which is what an index older than v2.10.0 answers — it ignores " +
        "the `?resolve_hashes` flag instead of rejecting it. Run Kupo v2.10.0 " +
        "or newer (l1-services/docker-compose.yml pins v2.11.0).",
    );
  }
  return match;
};

/**
 * A chain point strictly before `slot`, to intersect chain-sync at.
 *
 * `findIntersection` positions the read pointer *at* the point it finds and
 * `nextBlock` then yields what comes after it, so intersecting at the block that
 * created the order would skip that block. Kupo's flexible checkpoint lookup
 * answers with the most recent checkpoint before a slot, which is exactly the
 * ancestor this needs.
 */
export const fetchKupoAncestorPoint = async ({
  kupoUrl,
  slot,
  fetchImpl = fetch,
  timeoutMs = DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
}: {
  readonly kupoUrl: string;
  readonly slot: number;
  readonly fetchImpl?: FetchLike;
  readonly timeoutMs?: number;
}): Promise<L1ChainPoint> => {
  const exact = exactSlot(slot, "ancestor lookup slot");
  if (exact === 0) {
    throw new Error("the genesis slot has no ancestor checkpoint");
  }
  const url = joinUrl(
    normalizeKupoHttpUrl(kupoUrl),
    `/checkpoints/${(exact - 1).toString()}`,
  );
  const body = await fetchJsonWithTimeout(fetchImpl, url, timeoutMs);
  if (body === null || typeof body !== "object") {
    throw new Error(
      `Kupo has no checkpoint before slot ${exact.toString()}; the order's ` +
        "creating block is behind this index's checkpoint horizon",
    );
  }
  return exactPoint(body, `kupo.checkpoint(${(exact - 1).toString()})`);
};

type OgmiosSession = {
  readonly request: (
    method: string,
    params: Record<string, unknown>,
  ) => Promise<unknown>;
  readonly close: () => void;
};

const defaultWebSocketFactory: WebSocketFactory = (url) =>
  new WebSocket(url) as unknown as WebSocketLike;

const openOgmiosSession = async ({
  url,
  timeoutMs,
  webSocketFactory,
}: {
  readonly url: string;
  readonly timeoutMs: number;
  readonly webSocketFactory: WebSocketFactory;
}): Promise<OgmiosSession> => {
  const socket = webSocketFactory(url);
  const pending = new Map<
    number,
    { resolve: (value: unknown) => void; reject: (error: Error) => void }
  >();
  let terminal: Error | null = null;
  let nextId = 0;

  const failAll = (error: Error): void => {
    terminal ??= error;
    for (const waiter of pending.values()) {
      waiter.reject(error);
    }
    pending.clear();
  };

  socket.addEventListener("message", ((event: { data: unknown }) => {
    if (typeof event.data !== "string") {
      failAll(new Error("Ogmios chain-sync sent a non-text frame"));
      return;
    }
    let message: {
      id?: unknown;
      result?: unknown;
      error?: unknown;
    };
    try {
      message = JSON.parse(event.data) as typeof message;
    } catch (cause) {
      failAll(new Error("Ogmios chain-sync sent malformed JSON", { cause }));
      return;
    }
    if (typeof message.id !== "number") {
      // Unsolicited or unmatchable: nothing correlates it to a request, so it
      // cannot be answered and must not be silently treated as one.
      return;
    }
    const waiter = pending.get(message.id);
    if (waiter === undefined) {
      return;
    }
    pending.delete(message.id);
    if (message.error !== undefined) {
      waiter.reject(
        new Error(`Ogmios chain-sync error: ${JSON.stringify(message.error)}`),
      );
      return;
    }
    waiter.resolve(message.result);
  }) as (event: never) => void);
  socket.addEventListener("error", (() => {
    failAll(new Error("Ogmios chain-sync socket failed"));
  }) as (event: never) => void);
  socket.addEventListener("close", (() => {
    failAll(new Error("Ogmios chain-sync socket closed"));
  }) as (event: never) => void);

  await new Promise<void>((resolve, reject) => {
    const timer = setTimeout(() => {
      socket.close();
      reject(new Error(`Ogmios chain-sync did not open within ${timeoutMs}ms`));
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
        reject(new Error("Ogmios chain-sync socket failed while opening"));
      }) as (event: never) => void,
      { once: true },
    );
  });

  return {
    request: async (method, params) => {
      if (terminal !== null) {
        throw terminal;
      }
      const id = nextId;
      nextId += 1;
      return await new Promise<unknown>((resolve, reject) => {
        const timer = setTimeout(() => {
          pending.delete(id);
          reject(
            new Error(`Ogmios ${method} did not answer within ${timeoutMs}ms`),
          );
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
        try {
          socket.send(JSON.stringify({ jsonrpc: "2.0", method, params, id }));
        } catch (cause) {
          clearTimeout(timer);
          pending.delete(id);
          reject(new Error(`Failed to send Ogmios ${method}`, { cause }));
        }
      });
    },
    close: () => {
      socket.close();
    },
  };
};

const exactOutRef = (value: unknown, label: string): OutRefLike => {
  const record = value as { transaction?: { id?: unknown }; index?: unknown };
  const txHash = record.transaction?.id;
  if (typeof txHash !== "string" || !HEX_32.test(txHash)) {
    throw new Error(`${label}.transaction.id is not a transaction id`);
  }
  const outputIndex = record.index;
  if (
    typeof outputIndex !== "number" ||
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0
  ) {
    throw new Error(`${label}.index is not an output index`);
  }
  return { txHash, outputIndex };
};

/**
 * Ogmios's JSON transaction view, narrowed to what a carriage read needs.
 *
 * The raw transaction CBOR is deliberately not used: Ogmios only emits a
 * transaction's `cbor` when the *server* was started with
 * `--include-transaction-cbor`, which `l1-services/docker-compose.yml` does not
 * pass and no node can require of an operator's endpoint. `redeemers` and
 * `references` are unconditional, so the read stands on fields that are always
 * there. (`midgard-watcher` takes the other route — its `l1-adapter.ts`
 * re-derives everything from raw bytes — because it is given the bytes.)
 */
const parseObservedTransaction = (
  value: unknown,
  label: string,
): ObservedL1Transaction => {
  const record = value as {
    id?: unknown;
    inputs?: unknown;
    references?: unknown;
    mint?: unknown;
    redeemers?: unknown;
  };
  const txHash = record.id;
  if (typeof txHash !== "string" || !HEX_32.test(txHash)) {
    throw new Error(`${label}.id is not a transaction id`);
  }
  const spentInputs =
    record.inputs === undefined
      ? []
      : Array.isArray(record.inputs)
        ? record.inputs.map((entry, index) =>
            exactOutRef(entry, `${label}.inputs[${index.toString()}]`),
          )
        : (() => {
            throw new Error(`${label}.inputs is not an array`);
          })();
  const references =
    record.references === undefined
      ? []
      : Array.isArray(record.references)
        ? record.references.map((entry, index) =>
            exactOutRef(entry, `${label}.references[${index.toString()}]`),
          )
        : (() => {
            throw new Error(`${label}.references is not an array`);
          })();
  // A mint redeemer's `index` is positional over the mint's policy ids as the
  // ledger orders them — ascending by policy id — so the domain is rebuilt by
  // sorting rather than by trusting the order a JSON object happens to enumerate.
  const mint = record.mint;
  const mintPolicyIds =
    mint === undefined || mint === null
      ? []
      : Object.keys(mint as Record<string, unknown>)
          .filter((policyId) => HEX_28.test(policyId))
          .sort();
  const redeemers =
    record.redeemers === undefined
      ? []
      : Array.isArray(record.redeemers)
        ? record.redeemers.map((entry, index) => {
            const redeemer = entry as {
              redeemer?: unknown;
              validator?: { purpose?: unknown; index?: unknown };
            };
            const payload = redeemer.redeemer;
            const purpose = redeemer.validator?.purpose;
            const pointer = redeemer.validator?.index;
            if (typeof payload !== "string") {
              throw new Error(
                `${label}.redeemers[${index.toString()}].redeemer is not base16 data`,
              );
            }
            if (typeof purpose !== "string") {
              throw new Error(
                `${label}.redeemers[${index.toString()}].validator.purpose is missing`,
              );
            }
            if (
              typeof pointer !== "number" ||
              !Number.isSafeInteger(pointer) ||
              pointer < 0
            ) {
              throw new Error(
                `${label}.redeemers[${index.toString()}].validator.index is not an index`,
              );
            }
            return { purpose, index: pointer, redeemer: payload };
          })
        : (() => {
            throw new Error(`${label}.redeemers is not an array`);
          })();
  return {
    txHash,
    spentInputs,
    referenceInputs: references,
    mintPolicyIds,
    redeemers,
  };
};

/**
 * Rolls chain-sync from `intersection` forward to `blockPoint` and returns the
 * named transaction out of that block.
 *
 * **Rollbacks fail the read; they never widen it.** The first `nextBlock` after
 * an intersection is always a roll *backward* to the intersection itself, and
 * that one is expected. A later backward roll means the chain moved under the
 * scan, so the block Kupo named may no longer be on it — the read refuses, the
 * order is not ingested this pass, and the next reconciliation tick sees whatever
 * the chain settled on. That is the same exposure ingestion already had: an
 * order's *existence* is decided by the `utxosAt` view the walk starts from, and
 * this read only supplies bytes that must hash to that order's own committed
 * field hashes.
 */
export const readOgmiosBlockTransaction = async ({
  ogmiosUrl,
  intersection,
  blockPoint,
  txHash,
  webSocketFactory = defaultWebSocketFactory,
  timeoutMs = DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
  blockScanLimit = DEFAULT_TX_ORDER_CARRIAGE_BLOCK_SCAN_LIMIT,
}: {
  readonly ogmiosUrl: string;
  readonly intersection: L1ChainPoint;
  readonly blockPoint: L1ChainPoint;
  readonly txHash: string;
  readonly webSocketFactory?: WebSocketFactory;
  readonly timeoutMs?: number;
  readonly blockScanLimit?: number;
}): Promise<ObservedL1TransactionAtPoint> => {
  const session = await openOgmiosSession({
    url: normalizeOgmiosWebSocketUrl(ogmiosUrl),
    timeoutMs,
    webSocketFactory,
  });
  try {
    const found = (await session.request("findIntersection", {
      points: [{ slot: intersection.slot, id: intersection.headerHash }],
    })) as { intersection?: unknown };
    if (found.intersection === undefined) {
      throw new Error(
        `Ogmios found no intersection at slot ${intersection.slot.toString()}`,
      );
    }
    let rolledBack = false;
    for (let scanned = 0; scanned < blockScanLimit; scanned += 1) {
      const next = (await session.request("nextBlock", {})) as {
        direction?: unknown;
        block?: unknown;
      };
      if (next.direction === "backward") {
        if (rolledBack) {
          throw new Error(
            "the chain rolled back while reading the order's creating block",
          );
        }
        rolledBack = true;
        // The intersection acknowledgement is not a scanned block.
        scanned -= 1;
        continue;
      }
      if (next.direction !== "forward") {
        throw new Error("Ogmios nextBlock answered with no direction");
      }
      const block = next.block as {
        id?: unknown;
        slot?: unknown;
        height?: unknown;
        transactions?: unknown;
      };
      const blockId = block.id;
      if (typeof blockId !== "string") {
        throw new Error("Ogmios nextBlock answered with an unidentified block");
      }
      if (blockId !== blockPoint.headerHash) {
        if (typeof block.slot === "number" && block.slot > blockPoint.slot) {
          throw new Error(
            `chain-sync passed slot ${blockPoint.slot.toString()} without ` +
              `reaching block ${blockPoint.headerHash}`,
          );
        }
        continue;
      }
      const transactions = Array.isArray(block.transactions)
        ? block.transactions
        : [];
      const index = transactions.findIndex(
        (transaction) => (transaction as { id?: unknown }).id === txHash,
      );
      if (index === -1) {
        throw new Error(
          `block ${blockPoint.headerHash} does not contain transaction ${txHash}`,
        );
      }
      const blockNo = exactSlot(block.height, "ogmios.block.height");
      return {
        ...parseObservedTransaction(
          transactions[index],
          `ogmios.block(${blockPoint.headerHash}).transactions[${index.toString()}]`,
        ),
        blockPoint: {
          slot: exactSlot(block.slot, "ogmios.block.slot"),
          headerHash: exactHeaderHash(block.id, "ogmios.block.id"),
          blockNo,
        },
        transactionIndex: index,
      };
    }
    throw new Error(
      `chain-sync did not reach block ${blockPoint.headerHash} within ` +
        `${blockScanLimit.toString()} blocks of its Kupo checkpoint ancestor`,
    );
  } finally {
    session.close();
  }
};

/**
 * The redeemer the tx-order policy ran, out of an observed transaction.
 *
 * **The index is positional over the mint's policy ids in ascending order**, which
 * is how the ledger builds a minting redeemer's pointer: the mint field is a map
 * keyed by policy id, and the purpose index is that key's position in it. So the
 * selection is "the redeemer whose pointer names *this* policy", never "the
 * transaction's mint redeemer" — an order transaction that mints a second policy
 * below the tx-order one puts the tx-order redeemer at index 1, and either
 * shortcut would read a pointer that belongs to something else.
 *
 * Both refusals are fail-closed and neither is recoverable by guessing: a
 * transaction that mints nothing under the policy is not the order's creating
 * transaction, and a mint under the policy with no redeemer for it could not have
 * run the tx-order validator at all.
 */
export const txOrderMintRedeemer = (
  transaction: ObservedL1Transaction,
  txOrderPolicyId: string,
): string => {
  const policyIndex = transaction.mintPolicyIds.indexOf(txOrderPolicyId);
  if (policyIndex === -1) {
    throw new Error(
      `transaction ${transaction.txHash} mints nothing under the tx-order ` +
        `policy ${txOrderPolicyId}`,
    );
  }
  const mintRedeemer = transaction.redeemers.find(
    (redeemer) => redeemer.purpose === "mint" && redeemer.index === policyIndex,
  );
  if (mintRedeemer === undefined) {
    throw new Error(
      `transaction ${transaction.txHash} carries no mint redeemer for the ` +
        "tx-order policy",
    );
  }
  return mintRedeemer.redeemer;
};

/**
 * The order's §8 carriage vector, out of its own mint redeemer.
 *
 * Mirrors `midgard-watcher`'s `decodeMintRedeemer` for the `forced_order` case:
 * the tx-order policy does not take `user_events.MintRedeemer` bare — #594 gave
 * it its own `MintRedeemer`, wrapping that enum beside the §8 vector — so the
 * decode is against `TxOrderMintRedeemer` and a bare-enum redeemer at this
 * policy is a failure rather than an empty carriage. The schema itself is the
 * SDK's, so the two packages cannot drift apart on the wire format.
 */
export const txOrderMintCarriageVector = (
  redeemerCbor: string,
): readonly MidgardFieldCarriage[] => {
  const decoded = Data.from(
    redeemerCbor,
    SDK.TxOrderMintRedeemer,
  ) as SDK.TxOrderMintRedeemer;
  return decoded.material_carriage.map((entry): MidgardFieldCarriage => {
    if ("Inline" in entry) {
      return {
        carriage: "Inline",
        preimage: Buffer.from(entry.Inline.preimage, "hex"),
      };
    }
    if ("RawUtxo" in entry) {
      return {
        carriage: "RawUtxo",
        refInputIndex: exactRefInputIndex(entry.RawUtxo.ref_input_index),
      };
    }
    return {
      carriage: "Certified",
      certRefInputIndex: exactRefInputIndex(
        entry.Certified.cert_ref_input_index,
      ),
      chunkRefInputIndices:
        entry.Certified.chunk_ref_input_indices.map(exactRefInputIndex),
    };
  });
};

const exactRefInputIndex = (value: bigint): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(
      `reference-input index ${value.toString()} is out of range`,
    );
  }
  return Number(value);
};

/**
 * Turns one resolved carriage UTxO into what the §8.8 door reads off it.
 *
 * Both shapes are recognised by decoding, never by address or by position: §8.5
 * raw carriage is a nothing-but-bytes inline datum, and an §8.6 manifest is the
 * certificate record. A reference input that is neither — the hub oracle, a
 * reference script, anything else the order transaction reads — resolves to an
 * empty entry, which keeps every index in the vector pointing at the same input
 * the mint saw while giving the door nothing to open there.
 */
export const resolveCarriageReferenceInput = (
  datumCbor: string | null,
): ResolvedCarriageReferenceInput => {
  if (datumCbor === null) {
    return {};
  }
  try {
    return { inlineDatumBytes: SDK.fieldPreimagePublicationBytes(datumCbor) };
  } catch {
    // Not raw carriage. The one other thing a carriage index can name is a
    // manifest, so that is what is tried next.
  }
  try {
    const certificate = Data.from(
      datumCbor,
      SDK.FieldPreimageCertificate,
    ) as SDK.FieldPreimageCertificate;
    return {
      certificate: {
        owner: Buffer.from(certificate.owner, "hex"),
        txId: Buffer.from(certificate.tx_id, "hex"),
        fieldIndex: Number(certificate.field_index),
        fieldHash: Buffer.from(certificate.field_hash, "hex"),
        totalLength: Number(certificate.total_length),
        chunkDigests: certificate.chunk_digests.map((digest) =>
          Buffer.from(digest, "hex"),
        ),
      } satisfies MidgardFieldPreimageCertificate,
    };
  } catch {
    return {};
  }
};

/**
 * Resolves an observed transaction's reference inputs into the positional list
 * the redeemer's indices point into.
 *
 * **The order is the ledger's, re-derived rather than trusted.** Reference inputs
 * are a set, and what the validator was handed is that set in canonical
 * `(txHash, outputIndex)` order — which is the same discipline every positional
 * redeemer in the SDK keeps (`resolveChunkReferenceIndicesV1` sorts for exactly
 * this reason). Sorting here rather than taking the observation's order means a
 * provider that enumerates a set differently cannot shift an index.
 */
export const resolveCarriageReferenceInputs = async ({
  kupoUrl,
  referenceInputs,
  fetchImpl = fetch,
  timeoutMs = DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
}: {
  readonly kupoUrl: string;
  readonly referenceInputs: readonly OutRefLike[];
  readonly fetchImpl?: FetchLike;
  readonly timeoutMs?: number;
}): Promise<readonly ResolvedCarriageReferenceInput[]> => {
  const ordered = [...referenceInputs].sort(compareOutRefs);
  const resolved: ResolvedCarriageReferenceInput[] = [];
  for (const outRef of ordered) {
    const match = await fetchKupoMatch({
      kupoUrl,
      outRef,
      fetchImpl,
      timeoutMs,
    });
    // §8.5 requires an inline datum, and `datum_type` is the only thing that
    // distinguishes one: an output that merely *referenced* a datum is not
    // carriage even when Kupo happens to hold the preimage, so it resolves to
    // nothing rather than to bytes the ledger never put in the output. An output
    // with no datum at all omits the field entirely, which lands here too.
    if (match.datum_type !== "inline" || typeof match.datum_hash !== "string") {
      resolved.push(resolveCarriageReferenceInput(null));
      continue;
    }
    // The output has told us it carries an inline datum, so anything other than
    // bytes here is a Kupo that could not produce them — `null` is its documented
    // answer for a datum it does not hold. That is a *failure*, never an empty
    // resolution: an emptied carriage index reads as "this input carries no
    // carriage", which silently turns a readable order into an unreadable one
    // instead of into a read the next reconciliation tick retries.
    if (typeof match.datum !== "string") {
      throw new Error(
        `Kupo resolved no datum for hash ${match.datum_hash} on ` +
          `${outRef.txHash}#${outRef.outputIndex.toString()}`,
      );
    }
    resolved.push(resolveCarriageReferenceInput(match.datum));
  }
  return Object.freeze(resolved);
};

/**
 * The whole read: from the order's own UTxO to the §8 carriage its mint
 * authenticated.
 */
export const observeTxOrderMaterialCarriage = async ({
  ogmiosUrl,
  kupoUrl,
  txOrderOutRef,
  txOrderPolicyId,
  fetchImpl = fetch,
  webSocketFactory = defaultWebSocketFactory,
  timeoutMs = DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
  blockScanLimit = DEFAULT_TX_ORDER_CARRIAGE_BLOCK_SCAN_LIMIT,
}: {
  readonly ogmiosUrl: string;
  readonly kupoUrl: string;
  readonly txOrderOutRef: OutRefLike;
  readonly txOrderPolicyId: string;
  readonly fetchImpl?: FetchLike;
  readonly webSocketFactory?: WebSocketFactory;
  readonly timeoutMs?: number;
  readonly blockScanLimit?: number;
}): Promise<TxOrderMaterialCarriage> => {
  const createdAt = await fetchKupoCreationPoint({
    kupoUrl,
    outRef: txOrderOutRef,
    fetchImpl,
    timeoutMs,
  });
  const ancestor = await fetchKupoAncestorPoint({
    kupoUrl,
    slot: createdAt.slot,
    fetchImpl,
    timeoutMs,
  });
  const transaction = await readOgmiosBlockTransaction({
    ogmiosUrl,
    intersection: ancestor,
    blockPoint: createdAt,
    txHash: txOrderOutRef.txHash,
    webSocketFactory,
    timeoutMs,
    blockScanLimit,
  });
  const carriage = txOrderMintCarriageVector(
    txOrderMintRedeemer(transaction, txOrderPolicyId),
  );
  // Tier 1 names no reference input, so an all-inline order needs no Kupo
  // resolution at all and does not pay for one.
  const referenceInputs = carriage.every((entry) => entry.carriage === "Inline")
    ? []
    : await resolveCarriageReferenceInputs({
        kupoUrl,
        referenceInputs: transaction.referenceInputs,
        fetchImpl,
        timeoutMs,
      });
  return { carriage, referenceInputs };
};

/**
 * The Effect wrapper the ingestion walk calls. Every failure — a missing Kupo
 * match, a pruned checkpoint, a rollback mid-scan, an undecodable redeemer —
 * arrives as the one `LucidError` the walk already fails with, because from the
 * walk's side they are the same event: this order's material could not be read
 * this pass.
 */
export const observeTxOrderMaterialCarriageProgram = (options: {
  readonly ogmiosUrl: string;
  readonly kupoUrl: string;
  readonly txOrderOutRef: OutRefLike;
  readonly txOrderPolicyId: string;
  readonly fetchImpl?: FetchLike;
  readonly webSocketFactory?: WebSocketFactory;
  readonly timeoutMs?: number;
  readonly blockScanLimit?: number;
}): Effect.Effect<TxOrderMaterialCarriage, SDK.LucidError> =>
  Effect.tryPromise({
    try: () => observeTxOrderMaterialCarriage(options),
    catch: (cause) =>
      new SDK.LucidError({
        message:
          "Failed to read a forced order's §8 carriage from L1 (Ogmios chain-sync + Kupo)",
        cause,
      }),
  });
