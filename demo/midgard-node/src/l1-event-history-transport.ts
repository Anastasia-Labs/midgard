import { setTimeout as delay } from "node:timers/promises";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import { CML } from "@lucid-evolution/lucid";
import JSONBig from "json-bigint";

import type { BoundHistoryCapture } from "./l1-event-history-projection.js";
import {
  type EventHistorySourceBinding,
  verifyEventHistoryCaptureHub,
} from "./l1-event-history-source.js";
import {
  fetchKupoAncestorPoint,
  fetchKupoCreationPoint,
  type FetchLike,
  normalizeOgmiosWebSocketUrl,
  openOgmiosSession,
  type WebSocketFactory,
  type WebSocketLike,
} from "./l1-kupmios.js";
import type { LedgerSnapshotPoint } from "./l1-ledger-snapshot.js";
import {
  KupoNotYetIndexed,
  L1SourceUnavailable,
} from "./l1-source-unavailable.js";

export type HistoryTransportOptions = Readonly<{
  kupoUrl: string;
  ogmiosUrl: string;
  signal: AbortSignal;
  timeoutMs: number;
  blockScanLimit: number;
  maximumResponseBytes: number;
  maximumTransactionBytes: number;
  fetchImpl?: FetchLike;
  webSocketFactory?: WebSocketFactory;
  /** How long one read waits for Kupo to index a point Ogmios already
   * served before it reports the source unavailable. */
  indexLagCeilingMs?: number;
  /** Called with the lag signal while a read waits on it, then with
   * undefined once that read stops waiting. */
  onIndexLag?: (waiting: KupoNotYetIndexed | undefined) => void;
}>;
export const DEFAULT_INDEX_LAG_CEILING_MS = 60_000;
const INDEX_LAG_INITIAL_DELAY_MS = 250;
const INDEX_LAG_MAXIMUM_DELAY_MS = 5_000;
const lossless = JSONBig({ useNativeBigInt: true, strict: true });
const object = (value: unknown): Record<string, unknown> => {
  if (value === null || typeof value !== "object" || Array.isArray(value))
    throw new Error("History locator expected an object");
  return value as Record<string, unknown>;
};
const natural = (value: unknown): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
    throw new Error("History locator expected a safe natural coordinate");
  return value;
};
const hash = (value: unknown): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value))
    throw new Error("History locator expected an exact hash");
  return value;
};
const point = (value: unknown): LedgerSnapshotPoint => {
  const raw = object(value);
  return Object.freeze({ id: hash(raw.id), slot: natural(raw.slot) });
};
const equal = (a: LedgerSnapshotPoint, b: LedgerSnapshotPoint) =>
  a.id === b.id && a.slot === b.slot;

const bounded = (options: HistoryTransportOptions) => {
  for (const value of [
    options.timeoutMs,
    options.blockScanLimit,
    options.maximumResponseBytes,
    options.maximumTransactionBytes,
  ])
    if (!Number.isSafeInteger(value) || value <= 0)
      throw new Error("History transport requires positive safe bounds");
  options.signal.throwIfAborted();
  const signal = AbortSignal.any([
    options.signal,
    AbortSignal.timeout(options.timeoutMs),
  ]);
  const fetchImpl: FetchLike = async (url, init) => {
    const response = await (options.fetchImpl ?? fetch)(url, {
      ...init,
      signal:
        init?.signal == null ? signal : AbortSignal.any([signal, init.signal]),
    });
    if (response.body === null) return response;
    const reader = response.body.getReader();
    const abort = () => {
      void reader.cancel(signal.reason).catch(() => undefined);
    };
    signal.addEventListener("abort", abort, { once: true });
    if (signal.aborted) abort();
    const chunks: Uint8Array[] = [];
    let size = 0;
    try {
      while (true) {
        signal.throwIfAborted();
        const part = await reader.read();
        if (part.done) break;
        size += part.value.byteLength;
        if (size > options.maximumResponseBytes)
          throw new Error("History Kupo response exceeds its byte bound");
        chunks.push(part.value);
      }
    } finally {
      signal.removeEventListener("abort", abort);
      // Cancellation closes this reader immediately. Its underlying source's
      // cleanup promise must not extend our deadline or hide the read failure.
      void reader.cancel().catch(() => undefined);
      reader.releaseLock();
    }
    signal.throwIfAborted();
    return new Response(Buffer.concat(chunks), {
      status: response.status,
      statusText: response.statusText,
      headers: response.headers,
    });
  };
  return { signal, fetchImpl };
};

/** An unreachable, slow or overloaded Kupo says nothing about the chain. Our
 * own cancellation, and every answer Kupo did give, keep their meaning. */
const kupoUnavailable = (options: HistoryTransportOptions, cause: unknown) => {
  if (
    options.signal.aborted ||
    !(cause instanceof Error) ||
    cause instanceof L1SourceUnavailable
  )
    return cause;
  const transient =
    cause.name === "TimeoutError" ||
    cause.name === "AbortError" ||
    (cause instanceof TypeError && cause.message === "fetch failed") ||
    /^HTTP (?:429|5\d\d) /u.test(cause.message);
  return transient ? new L1SourceUnavailable(cause.message, { cause }) : cause;
};

/** Kupo's creating point and the checkpoint before it. A Kupo behind the node
 * answers "no match" or "no checkpoint" for a block Ogmios already served, so
 * those two answers wait on a doubling delay up to the lag ceiling. A
 * checkpoint at or after its target is an answer, and is refused at once. */
const readKupoPoints = async (
  options: HistoryTransportOptions,
  outRef: OutRefLike,
) => {
  const ceilingMs = options.indexLagCeilingMs ?? DEFAULT_INDEX_LAG_CEILING_MS;
  if (!Number.isSafeInteger(ceilingMs) || ceilingMs < 0)
    throw new Error("History index lag ceiling must be a safe natural");
  const started = performance.now();
  let wait = INDEX_LAG_INITIAL_DELAY_MS;
  let waiting = false;
  try {
    while (true) {
      try {
        const { fetchImpl } = bounded(options);
        const target = await fetchKupoCreationPoint({
          kupoUrl: options.kupoUrl,
          outRef,
          fetchImpl,
          timeoutMs: options.timeoutMs,
        });
        const ancestor = await fetchKupoAncestorPoint({
          kupoUrl: options.kupoUrl,
          slot: target.slot,
          fetchImpl,
          timeoutMs: options.timeoutMs,
        });
        return {
          selected: { id: ancestor.headerHash, slot: ancestor.slot },
          targetPoint: { id: target.headerHash, slot: target.slot },
        };
      } catch (cause) {
        if (!(cause instanceof KupoNotYetIndexed))
          throw kupoUnavailable(options, cause);
        options.signal.throwIfAborted();
        if (performance.now() - started + wait > ceilingMs)
          throw new L1SourceUnavailable(
            `${cause.message}; Kupo did not index it within ${ceilingMs.toString()}ms`,
            { cause },
          );
        waiting = true;
        options.onIndexLag?.(cause);
        await delay(wait, undefined, { signal: options.signal });
        wait = Math.min(wait * 2, INDEX_LAG_MAXIMUM_DELAY_MS);
      }
    }
  } finally {
    if (waiting) options.onIndexLag?.(undefined);
  }
};

/** Point navigation and preimage carriage only. This does not authenticate a
 * branch. The source owner must freshly intersect its bound follower at the
 * returned predecessor and admit the complete activation block there.
 */
const readLocatedTransaction = async (
  options: HistoryTransportOptions,
  outRef: OutRefLike,
) => {
  hash(outRef.txHash);
  natural(outRef.outputIndex);
  const { selected, targetPoint } = await readKupoPoints(options, outRef);
  if (selected.slot >= targetPoint.slot)
    throw new Error("History locator checkpoint is not before its target");
  // A fresh deadline: a Kupo lag wait does not spend the Ogmios scan's bound.
  const { signal } = bounded(options);
  const session = await openOgmiosSession({
    url: normalizeOgmiosWebSocketUrl(options.ogmiosUrl),
    timeoutMs: options.timeoutMs,
    signal,
    webSocketFactory:
      options.webSocketFactory ??
      ((url) => new WebSocket(url) as unknown as WebSocketLike),
    parseMessage: (text) => {
      if (Buffer.byteLength(text) > options.maximumResponseBytes)
        throw new Error("History Ogmios response exceeds its byte bound");
      return lossless.parse(text) as unknown;
    },
  });
  try {
    const found = object(
      await session.request("findIntersection", { points: [selected] }),
    );
    if (!equal(point(found.intersection), selected))
      throw new Error("History locator returned a different intersection");
    let previous: LedgerSnapshotPoint & { readonly height?: number } = selected;
    let acknowledgement = true;
    let count = 0;
    while (count < options.blockScanLimit) {
      signal.throwIfAborted();
      const response = object(await session.request("nextBlock", {}));
      if (response.direction === "backward") {
        if (!acknowledgement || !equal(point(response.point), selected))
          // A navigation read only; the bound follower adjudicates rollbacks.
          throw new L1SourceUnavailable(
            "History locator rolled back during its read",
          );
        acknowledgement = false;
        continue;
      }
      acknowledgement = false;
      if (response.direction !== "forward")
        throw new Error("History locator received an invalid direction");
      count += 1;
      const block = object(response.block);
      const next = { ...point(block), height: natural(block.height) };
      if (
        block.type !== "praos" ||
        hash(block.ancestor) !== previous.id ||
        next.slot <= previous.slot ||
        (previous.height !== undefined && next.height !== previous.height + 1)
      )
        throw new Error("History locator broke consecutive block ancestry");
      if (equal(next, targetPoint)) {
        if (!Array.isArray(block.transactions))
          throw new Error("History locator block omitted transactions");
        const matches = block.transactions
          .map(object)
          .filter((entry) => entry.id === outRef.txHash);
        if (matches.length !== 1)
          throw new Error(
            "History locator lacks one exact creating transaction",
          );
        signal.throwIfAborted();
        return Object.freeze({
          point: Object.freeze(next),
          predecessor: Object.freeze({ id: previous.id, slot: previous.slot }),
          transaction: matches[0]!,
        });
      }
      if (next.slot >= targetPoint.slot)
        throw new Error(
          "History locator passed or contradicted its creating point",
        );
      previous = next;
    }
    throw new Error("History locator exceeded its block scan bound");
  } finally {
    session.close();
  }
};

/** Return exact creating BODY bytes. Existence/ordinary validity are not taken
 * from this transport or the archived transaction's is_valid flag. The caller
 * must use verifyEventHistoryReferenceBody with the bound observing reference.
 */
export const readEventHistoryCreatingBody = async (
  options: HistoryTransportOptions,
  ref: OutRefLike,
): Promise<string> => {
  const found = await readLocatedTransaction(options, ref);
  const cbor = found.transaction.cbor;
  if (
    typeof cbor !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(cbor) ||
    cbor.length / 2 > options.maximumTransactionBytes
  )
    throw new Error(
      "History creating transaction CBOR is missing or exceeds its bound; Ogmios must include transaction CBOR",
    );
  const transaction = CML.Transaction.from_cbor_hex(cbor);
  try {
    if (transaction.to_cbor_hex() !== cbor)
      throw new Error(
        "History creating transaction encoding was not preserved",
      );
    const body = transaction.body();
    try {
      const bodyCbor = body.to_cbor_hex();
      if (
        computeHash32(Buffer.from(bodyCbor, "hex")).toString("hex") !==
        ref.txHash
      )
        throw new Error(
          "History creating body does not match the requested transaction",
        );
      options.signal.throwIfAborted();
      return bodyCbor;
    } finally {
      body.free();
    }
  } finally {
    transaction.free();
  }
};

/** The canonical hub address is permanently unspendable. Its current outref
 * supplies a candidate initialization locator even when Kupo prunes spent
 * nonce records. A delayed first lock can produce a different candidate, so
 * ONLY bound replay's shared nonce/mint/two-list checks establish activation.
 */
export const locateEventHistoryActivation = async (
  options: HistoryTransportOptions,
  input: {
    readonly binding: EventHistorySourceBinding;
    readonly capture: BoundHistoryCapture;
    readonly expectedTransactionHash?: string;
  },
) => {
  if (input.capture.bindingDigest !== input.binding.digest)
    throw new Error("History activation locator capture binding differs");
  const hub = verifyEventHistoryCaptureHub(
    input.capture.history.ledger,
    input.binding,
  );
  if (
    input.expectedTransactionHash !== undefined &&
    hub.txHash !== input.expectedTransactionHash
  )
    throw new Error(
      "History hub creator differs from the manifest initialization transaction",
    );
  const found = await readLocatedTransaction(options, hub);
  options.signal.throwIfAborted();
  return Object.freeze({
    point: found.point,
    predecessor: found.predecessor,
    transactionHash: hub.txHash,
  });
};
