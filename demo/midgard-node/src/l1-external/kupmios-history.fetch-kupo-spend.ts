import { type OutRefLike } from "@al-ft/midgard-core/out-ref";

import {
  DEFAULT_L1_READ_TIMEOUT_MS,
  exactPoint,
  exactSlot,
  fetchJsonWithTimeout,
  type FetchLike,
  HEX_32,
  joinUrl,
  type KupoMatch,
  type L1ChainPoint,
  normalizeKupoHttpUrl,
  type WebSocketFactory,
  type WebSocketLike,
} from "./kupmios-history.l1-chain-point.js";
import { KupoNotYetIndexed } from "./kupmios-history.source-unavailable.js";

/**
 * The chain point at which an output was created, from Kupo's match for it.
 *
 * The pattern is Kupo's `{output_index}@{transaction_id}` output-reference form,
 * and the query is deliberately **not** filtered to unspent matches: the
 * readers locate outputs that may have been spent since they were created.
 */
export const fetchKupoCreationPoint = async ({
  kupoUrl,
  outRef,
  fetchImpl = fetch,
  timeoutMs = DEFAULT_L1_READ_TIMEOUT_MS,
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
 * Which transaction spent an output, and where on Kupo's selected chain.
 *
 * **Only those two facts, on purpose.** Kupo's `spent_at` also carries
 * `input_index` and `redeemer`, and neither is surfaced because in Kupo v2.11.0
 * neither describes the spent output once its spending transaction has more than
 * one input. `matchBlock` (`src/Kupo/Data/Pattern.hs`) numbers a transaction's
 * inputs with a *right* fold over their ascending set, so the largest input gets
 * index 0: it reports `input_index = n - 1 - i` for the input at ledger position
 * `i`, and looks the spend redeemer up at that mirrored pointer. The redeemer it
 * reports is therefore some *other* input's: `null` for a Plutus spend whose
 * mirror is a key input, another script's bytes when the mirror is a script
 * input, and a script's redeemer on a key input. Upstream master still does this
 * (fix proposed in CardanoSolutions/kupo#210, unmerged). The v2.11.0 API schema
 * separately makes `redeemer` nullable (`BinaryData | null`) — `null` is also
 * the answer for an output spent without a redeemer, and for rows indexed before
 * the v2.7.0 migration.
 *
 * Seen live on preprod: the scheduler UTxO spent by `AppointFirstOperator` sits
 * at ledger input 1 of 2 and ran spend redeemer 1, while Kupo reported
 * `input_index: 0, redeemer: null` — the redeemer slot of the key input beside
 * it.
 *
 * So a Kupo `null` redeemer is not evidence of a key spend, and a Kupo string is
 * not evidence of which script ran. Anything that needs a spend's redeemer reads
 * it off the transaction itself — {@link readOgmiosBlockTransaction}'s
 * `redeemers`, whose `validator.index` is the ledger's pointer — which is where
 * the state-queue correction observer already takes every redeemer it decodes.
 */
export type KupoSpend = Readonly<{
  point: L1ChainPoint;
  transactionId: string;
}>;

/** Non-empty base16 bytes, as Kupo encodes a Plutus `BinaryData`. */
export const BASE16_BYTES = /^(?:[0-9a-f]{2})+$/u;

/**
 * Reads the exact canonical spend attached by Kupo to an output match. A null
 * result means the output is currently unspent on Kupo's selected chain; a
 * malformed partial spend is refused rather than treated as absence.
 *
 * `input_index` and `redeemer` are still checked against Kupo's schema, so an
 * answer that is not a v2.11 `SpentAt` is refused, but their values are
 * discarded: see {@link KupoSpend} for why neither can be trusted.
 */
export const fetchKupoSpend = async ({
  kupoUrl,
  outRef,
  fetchImpl = fetch,
  timeoutMs = DEFAULT_L1_READ_TIMEOUT_MS,
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
  // `null` is schema-legal and is what Kupo serves even for a Plutus spend (see
  // KupoSpend); it is accepted here and, like any redeemer value, never read.
  if (
    spent.redeemer !== undefined &&
    spent.redeemer !== null &&
    (typeof spent.redeemer !== "string" || !BASE16_BYTES.test(spent.redeemer))
  ) {
    throw new Error("Kupo spent_at.redeemer is not base16 data");
  }
  return Object.freeze({
    point: exactPoint(match.spent_at, "kupo.match.spent_at"),
    transactionId: spent.transaction_id,
  });
};

export const fetchKupoMatch = async ({
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
  // Absence from a lagging index is not absence from the chain: the caller
  // decides whether this read may wait for the index to catch up.
  if (match === undefined) {
    throw new KupoNotYetIndexed(
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
        "or newer (docker-compose.kupmios.yaml pins v2.11.0).",
    );
  }
  return match;
};

/**
 * A chain point strictly before `slot`, to intersect chain-sync at.
 *
 * `findIntersection` positions the read pointer *at* the point it finds and
 * `nextBlock` then yields what comes after it, so intersecting at the block that
 * created the output would skip that block. Kupo's flexible checkpoint lookup
 * answers with the most recent checkpoint before a slot, which is exactly the
 * ancestor this needs.
 */
export const fetchKupoAncestorPoint = async ({
  kupoUrl,
  slot,
  fetchImpl = fetch,
  timeoutMs = DEFAULT_L1_READ_TIMEOUT_MS,
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
    throw new KupoNotYetIndexed(
      `Kupo has no checkpoint before slot ${exact.toString()}; the order's ` +
        "creating block is behind this index's checkpoint horizon",
    );
  }
  return exactPoint(body, `kupo.checkpoint(${(exact - 1).toString()})`);
};

export type OgmiosSession = {
  readonly request: (
    method: string,
    params: Record<string, unknown>,
    options?: { readonly timeoutMs: number | null },
  ) => Promise<unknown>;
  readonly close: () => void;
};

export const defaultWebSocketFactory: WebSocketFactory = (url) =>
  new WebSocket(url) as unknown as WebSocketLike;
