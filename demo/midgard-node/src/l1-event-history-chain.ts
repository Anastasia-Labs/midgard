import { setTimeout as delay } from "node:timers/promises";

import JSONBig from "json-bigint";

import type { LedgerSnapshotPoint } from "./l1-ledger-snapshot.js";
import {
  normalizeOgmiosWebSocketUrl,
  openOgmiosSession,
  type WebSocketFactory,
  type WebSocketLike,
} from "./l1-tx-order-carriage.js";

export type HistoryChainTip = LedgerSnapshotPoint & { readonly height: number };
export type HistoryChainBlock = Readonly<{
  point: HistoryChainTip;
  parent: string;
  /** Lossless transaction observations still require their own strict decoder. */
  body: Readonly<Record<string, unknown>>;
}>;

const object = (value: unknown): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error("History ChainSync expected an object");
  return value as Record<string, unknown>;
};
const natural = (value: unknown): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
    throw new Error("History ChainSync expected a safe natural number");
  return value;
};
const hash = (value: unknown): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value))
    throw new Error("History ChainSync expected a lowercase block hash");
  return value;
};
const point = (value: unknown): LedgerSnapshotPoint => {
  const raw = object(value);
  return Object.freeze({ slot: natural(raw.slot), id: hash(raw.id) });
};
const tip = (value: unknown): HistoryChainTip | "origin" =>
  value === "origin"
    ? value
    : Object.freeze({ ...point(value), height: natural(object(value).height) });
const equal = (a: LedgerSnapshotPoint, b: LedgerSnapshotPoint) =>
  a.slot === b.slot && a.id === b.id;

/** Ogmios v6 answers queryNetwork/tip with a point only; the height comes from
 * queryNetwork/blockHeight. The height is bound to the tip only when two tip
 * reads bracketing it agree; undefined means the chain moved in between. */
const readNetworkTip = async (
  request: (
    method: string,
    params: Record<string, unknown>,
  ) => Promise<unknown>,
  signal: AbortSignal,
): Promise<HistoryChainTip | "origin" | undefined> => {
  const networkTip = async () => {
    const value = await request("queryNetwork/tip", {});
    signal.throwIfAborted();
    return value === "origin" ? value : point(value);
  };
  const before = await networkTip();
  const rawHeight = await request("queryNetwork/blockHeight", {});
  signal.throwIfAborted();
  const height = rawHeight === "origin" ? rawHeight : natural(rawHeight);
  const after = await networkTip();
  if (before === "origin" || after === "origin") {
    if (before !== after) return undefined;
    if (height !== "origin")
      throw new Error("History network tip is origin but has a block height");
    return "origin";
  }
  if (!equal(before, after)) return undefined;
  if (height === "origin")
    throw new Error("History network tip has a point but no block height");
  return Object.freeze({ ...before, height });
};

/** Stop waiting without detaching a rejecting consumer promise. Its owner must
 * also cancel/fence that work when onUnavailable fires; this wait owns no SQL. */
const waitForConsumer = (work: void | Promise<void>, signal: AbortSignal) =>
  new Promise<void>((resolve, reject) => {
    const abort = () =>
      reject(
        signal.reason instanceof Error
          ? signal.reason
          : new Error("History consumer wait aborted", {
              cause: signal.reason,
            }),
      );
    signal.addEventListener("abort", abort, { once: true });
    Promise.resolve(work).then(
      () => {
        signal.removeEventListener("abort", abort);
        resolve();
      },
      (cause: unknown) => {
        signal.removeEventListener("abort", abort);
        reject(
          cause instanceof Error
            ? cause
            : new Error("History consumer failed", { cause }),
        );
      },
    );
    if (signal.aborted) abort();
  });

const assertAtOrBeforeTip = (
  selected: LedgerSnapshotPoint & { readonly height?: number },
  observed: HistoryChainTip | "origin",
) => {
  if (
    observed === "origin" ||
    selected.slot > observed.slot ||
    (selected.slot === observed.slot && selected.id !== observed.id) ||
    (selected.id === observed.id && selected.slot !== observed.slot) ||
    (selected.height !== undefined &&
      (selected.height > observed.height ||
        (selected.height === observed.height && !equal(selected, observed)) ||
        (equal(selected, observed) && selected.height !== observed.height)))
  )
    throw new Error("History ChainSync point contradicts its response tip");
};

/** Follow one selected branch from an approved retained intersection. This is
 * transport/ancestry evidence, not source authentication or event eligibility.
 * The owner must verify its source/deployment binding before using observations.
 * Rollback/unavailable callbacks synchronously fence work. onForward may return
 * a persistence acknowledgement: no further nextBlock is requested until it
 * settles, while the heartbeat remains active. Its owner cancels/fences slow
 * work on source loss; a pending acknowledgement cannot delay revocation.
 * On any failure the owner is notified before the socket is closed. Reconnect
 * requires a new invocation and a newly verified retained intersection.
 */
export const followEventHistoryChain = async ({
  ogmiosUrl,
  intersections,
  retainedPointLimit,
  requestTimeoutMs = 20_000,
  heartbeatIntervalMs = 5_000,
  signal,
  onIntersection,
  onForward,
  onRollback,
  onTip,
  onUnavailable,
  verifySession,
  webSocketFactory = (url) => new WebSocket(url) as unknown as WebSocketLike,
}: {
  readonly ogmiosUrl: string;
  readonly intersections: readonly LedgerSnapshotPoint[];
  readonly retainedPointLimit: number;
  readonly requestTimeoutMs?: number;
  readonly heartbeatIntervalMs?: number;
  readonly signal: AbortSignal;
  readonly onIntersection: (point: LedgerSnapshotPoint) => void;
  readonly onForward: (block: HistoryChainBlock) => void | Promise<void>;
  readonly onRollback: (point: LedgerSnapshotPoint | "origin") => void;
  /** A responsive socket alone does not mean the follower has caught up. The
   * owner may renew readiness only against its admitted path and this tip. */
  readonly onTip: (tip: HistoryChainTip | "origin") => void;
  readonly onUnavailable: (cause: unknown) => void;
  /** Source-bound callers verify this exact socket before admitting ancestry. */
  readonly verifySession?: (
    session: Pick<Awaited<ReturnType<typeof openOgmiosSession>>, "request">,
  ) => Promise<void>;
  readonly webSocketFactory?: WebSocketFactory;
}): Promise<void> => {
  if (
    intersections.length === 0 ||
    !Number.isSafeInteger(retainedPointLimit) ||
    retainedPointLimit < 2 ||
    !Number.isSafeInteger(requestTimeoutMs) ||
    requestTimeoutMs <= 0 ||
    !Number.isSafeInteger(heartbeatIntervalMs) ||
    heartbeatIntervalMs <= 0
  )
    throw new Error("Invalid history ChainSync bounds or empty intersections");
  const requested = intersections.map(point);
  const stopped = new AbortController();
  const lifetime = AbortSignal.any([signal, stopped.signal]);
  const lossless = JSONBig({ useNativeBigInt: true, strict: true });
  let session: Awaited<ReturnType<typeof openOgmiosSession>> | undefined;
  let heartbeat: Promise<void> | undefined;
  let receiving: Promise<void> | undefined;
  try {
    session = await openOgmiosSession({
      url: normalizeOgmiosWebSocketUrl(ogmiosUrl),
      timeoutMs: requestTimeoutMs,
      signal: lifetime,
      webSocketFactory,
      parseMessage: (text) => lossless.parse(text) as unknown,
    });
    const opened = session;
    await verifySession?.(opened);
    lifetime.throwIfAborted();
    const found = object(
      await opened.request("findIntersection", { points: requested }),
    );
    lifetime.throwIfAborted();
    const intersection = point(found.intersection);
    if (!requested.some((candidate) => equal(candidate, intersection)))
      throw new Error("History ChainSync returned an unrequested intersection");
    const intersectionTip = tip(found.tip);
    assertAtOrBeforeTip(intersection, intersectionTip);
    let path: (LedgerSnapshotPoint & { readonly height?: number })[] = [
      intersectionTip !== "origin" && equal(intersection, intersectionTip)
        ? intersectionTip
        : intersection,
    ];
    onIntersection(intersection);
    onTip(intersectionTip);

    const receive = async () => {
      let initialAcknowledgement = true;
      while (true) {
        lifetime.throwIfAborted();
        const response = object(
          await opened.request("nextBlock", {}, { timeoutMs: null }),
        );
        lifetime.throwIfAborted();
        const observedTip = tip(response.tip);
        const current = path[path.length - 1]!;
        if (response.direction === "backward") {
          const rollback =
            response.point === "origin" ? "origin" : point(response.point);
          const index =
            rollback === "origin"
              ? -1
              : path.findIndex((entry) => equal(entry, rollback));
          if (rollback !== "origin")
            assertAtOrBeforeTip(path[index] ?? rollback, observedTip);
          // Ogmios may acknowledge the selected intersection before forwarding.
          if (
            !(
              initialAcknowledgement &&
              rollback !== "origin" &&
              equal(rollback, current)
            )
          ) {
            onRollback(rollback);
            if (index < 0)
              throw new Error(
                "History ChainSync rollback exceeds retained ancestry",
              );
            path = path.slice(0, index + 1);
          }
        } else if (response.direction === "forward") {
          const body = object(response.block);
          if (body.type !== "praos")
            throw new Error("History ChainSync requires a post-Byron block");
          const next = tip(body);
          if (next === "origin")
            throw new Error("History ChainSync block has no point");
          const parent = hash(body.ancestor);
          assertAtOrBeforeTip(next, observedTip);
          if (
            parent !== current.id ||
            next.slot <= current.slot ||
            (current.height !== undefined && next.height !== current.height + 1)
          )
            throw new Error(
              "History ChainSync forward breaks retained ancestry",
            );
          path.push(next);
          if (path.length > retainedPointLimit) path.shift();
          await waitForConsumer(
            onForward(
              Object.freeze({ point: next, parent, body: Object.freeze(body) }),
            ),
            lifetime,
          );
          lifetime.throwIfAborted();
        } else throw new Error("History ChainSync has an invalid direction");
        initialAcknowledgement = false;
        onTip(observedTip);
      }
    };
    heartbeat = (async () => {
      while (true) {
        await delay(heartbeatIntervalMs, undefined, { signal: lifetime });
        // A moving chain publishes no heartbeat tip: the socket has answered,
        // and nextBlock carries the moved frontier.
        const observedTip = await readNetworkTip(opened.request, lifetime);
        if (observedTip !== undefined) onTip(observedTip);
      }
    })();
    // Exactly one nextBlock remains pending at a stable tip. A separate bounded
    // network query detects dead transports without abandoning that request.
    receiving = receive();
    await Promise.race([receiving, heartbeat]);
  } catch (cause) {
    onUnavailable(cause);
    throw cause;
  } finally {
    stopped.abort();
    session?.close();
    // The race attached handlers to both loops; explicitly drain the heartbeat
    // so no timer/request survives this scope.
    await heartbeat?.catch(() => undefined);
    await receiving?.catch(() => undefined);
  }
};
