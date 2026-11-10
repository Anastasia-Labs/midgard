/**
 * Shares this node's pending L2 transactions with the other operators named
 * by `L2_TX_PEERS`, so whichever operator holds the next turn can commit
 * them.
 *
 * - Delta push: after the tx-queue processor accepts transactions, each peer
 *   gets the native ones it has not been sent yet, in admission order.
 * - Catch-up check: at start, after a failure and on each L1 advance, the
 *   node offers every pending id and sends only those the peer has never
 *   seen. Restarts, outages and partitions repair themselves this way, with
 *   no durable outbox.
 * - Each signed inventory answer names the operator that answered. After a
 *   catch-up the ids that operator showed it holds go to
 *   `Globals.L2_PEER_HELD_TXS`; a failed lane, or one with nothing pending,
 *   drops its entry. A node that is not the successor at a shift end reads
 *   it to let a successor that holds all its work act first.
 *
 * All state is in memory. Each peer has a lane of its own, a fiber with at
 * most one request in flight, so a down, slow or hung peer delays only its
 * own lane. Nothing here holds commits, the tx-queue processor or readiness;
 * a failure is a metric and a warning. A peer that refuses this node's key
 * (403: not in its active operator set) is retried at the longest backoff
 * and warned about once. With nothing pending a wake reads only the pending
 * tables and sends nothing.
 */
import {
  Cause,
  Duration,
  Effect,
  Metric,
  Option,
  Stream,
  SubscriptionRef,
} from "effect";

import { daLocalSigners } from "../da/local-signers.js";
import { L2PeerShareDB } from "../database/index.js";
import {
  decodePeerList,
  encodePeerList,
  PEER_CBOR_HEAD_BYTES,
  PEER_INVENTORY_MAX_IDS,
  PEER_INVENTORY_PATH,
  PEER_MEDIA_TYPE,
  PEER_TXS_MAX_BODY_BYTES,
  PEER_TXS_PATH,
  peerAuthHeaders,
  verifyPeerAnswer,
} from "../l2-peer/protocol.js";
import { recordPeerHeldTxs } from "../l2-peer/successor-holds-pending.js";
import { Globals, MidgardContracts, NodeConfig } from "../services/index.js";

/**
 * A full body (about 890 small txs) is checked and admitted in about 0.25 s
 * at worst on a single check worker (M3, 2026-11-08), so 10 s is 40x
 * headroom; a peer slower than that is stuck, and the lane backs off and
 * catches up by inventory. Below the receiver's own 30 s request bound
 * (`listen-public-http`).
 */
const PEER_REQUEST_TIMEOUT_MS = 10_000;

const forwardFailures = Metric.counter("l2_peer_forward_failures_total", {
  description: "Failed requests sharing pending L2 transactions with a peer",
  incremental: true,
});

type PeerLane = {
  readonly url: string;
  /** The last native admission sequence this peer was sent. */
  afterSeq: bigint;
  needsCatchUp: boolean;
  failures: number;
  retryAt: number;
  /** Whether the peer's last answer refused this node's key (403). */
  refused: boolean;
  /** The operator key hash that signed the peer's last inventory answer. */
  operator: string | undefined;
};

class PeerRequestError extends Error {
  constructor(
    message: string,
    readonly status: number = 0,
    readonly retryAfterMs: number = 0,
  ) {
    super(message);
  }
}

/** Packs txs into bodies the receiver takes: at most its body size each. */
const packBodies = (txs: readonly Buffer[]): Buffer[][] => {
  const bodies: Buffer[][] = [];
  let current: Buffer[] = [];
  let bytes = PEER_CBOR_HEAD_BYTES;
  for (const tx of txs) {
    const size = tx.length + PEER_CBOR_HEAD_BYTES;
    if (
      current.length > 0 &&
      (bytes + size > PEER_TXS_MAX_BODY_BYTES ||
        current.length === PEER_INVENTORY_MAX_IDS)
    ) {
      bodies.push(current);
      current = [];
      bytes = PEER_CBOR_HEAD_BYTES;
    }
    current.push(tx);
    bytes += size;
  }
  if (current.length > 0) bodies.push(current);
  return bodies;
};

export const l2TxPeerForwarderFiber = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  if (nodeConfig.L2_TX_PEERS.length === 0) return;
  const globals = yield* Globals;
  const deployment = (yield* MidgardContracts).hubOracle.policyId;
  const signer = daLocalSigners(nodeConfig).find(
    (local) => local.role === "operator",
  )!;
  const pageSize = Math.min(
    nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE,
    PEER_INVENTORY_MAX_IDS,
  );
  const lanes: PeerLane[] = nodeConfig.L2_TX_PEERS.map((url) => ({
    url,
    afterSeq: 0n,
    needsCatchUp: true,
    failures: 0,
    retryAt: 0,
    refused: false,
    operator: undefined,
  }));

  const post = (lane: PeerLane, path: string, items: readonly Buffer[]) =>
    Effect.tryPromise({
      try: async (signal) => {
        const body = encodePeerList(items);
        const response = await fetch(`${lane.url}${path}`, {
          method: "POST",
          headers: {
            "content-type": PEER_MEDIA_TYPE,
            ...peerAuthHeaders(signer, path, deployment, body),
          },
          body,
          signal: AbortSignal.any([
            signal,
            AbortSignal.timeout(PEER_REQUEST_TIMEOUT_MS),
          ]),
        });
        const answer = Buffer.from(await response.arrayBuffer());
        if (!response.ok)
          throw new PeerRequestError(
            `${path} answered ${response.status.toString()}`,
            response.status,
            1000 * Number(response.headers.get("retry-after") ?? "0") || 0,
          );
        return {
          answer,
          answeredBy: verifyPeerAnswer(
            Object.fromEntries(response.headers),
            path,
            deployment,
            body,
            answer,
          ),
        };
      },
      catch: (cause) =>
        cause instanceof PeerRequestError
          ? cause
          : new PeerRequestError(`${path} failed: ${String(cause)}`),
    });

  const sendTxs = (lane: PeerLane, txs: readonly Buffer[]) =>
    Effect.forEach(packBodies(txs), (body) => post(lane, PEER_TXS_PATH, body), {
      discard: true,
    });

  /** Records which operator the lane reaches and what it holds, or nothing. */
  const recordHeld = (
    lane: PeerLane,
    operator: string | undefined,
    held: ReadonlySet<string> | undefined,
  ) =>
    Effect.gen(function* () {
      const previous = lane.operator;
      lane.operator = operator;
      yield* recordPeerHeldTxs(globals, previous, undefined);
      if (operator !== undefined && held !== undefined)
        yield* recordPeerHeldTxs(globals, operator, held);
    });

  const catchUp = (lane: PeerLane) =>
    Effect.gen(function* () {
      const mark = yield* L2PeerShareDB.catchUpMark;
      const ids = yield* L2PeerShareDB.pendingIds;
      // The ids the answers showed the peer holds, kept only while one
      // operator signed every page; any other answer leaves it unidentified.
      const held = new Set<string>();
      let operator: string | null | undefined;
      for (let start = 0; start < ids.length; start += pageSize) {
        const page = ids.slice(start, start + pageSize);
        const { answer, answeredBy } = yield* post(
          lane,
          PEER_INVENTORY_PATH,
          page,
        );
        const missing = yield* Effect.try({
          try: () => decodePeerList(answer, PEER_INVENTORY_MAX_IDS),
          catch: () => new PeerRequestError("inventory answer is malformed"),
        });
        operator =
          operator === undefined || operator === answeredBy ? answeredBy : null;
        const missingHex = new Set(missing.map((id) => id.toString("hex")));
        for (const id of page) {
          const hex = id.toString("hex");
          if (!missingHex.has(hex)) held.add(hex);
        }
        const bytes = yield* L2PeerShareDB.pendingBytes(missing);
        yield* sendTxs(
          lane,
          missing.flatMap((id) => bytes.get(id.toString("hex")) ?? []),
        );
      }
      lane.needsCatchUp = false;
      if (mark !== null && mark > lane.afterSeq) lane.afterSeq = mark;
      yield* recordHeld(lane, operator ?? undefined, held);
    });

  const delta = (lane: PeerLane) =>
    Effect.gen(function* () {
      for (;;) {
        const rows = yield* L2PeerShareDB.nativePendingAfter(
          lane.afterSeq,
          pageSize,
        );
        yield* sendTxs(
          lane,
          rows.map((row) => row.tx),
        );
        const last = rows.at(-1);
        if (last !== undefined) lane.afterSeq = last.arrivalSeq;
        if (rows.length < pageSize) return;
      }
    });

  const attempt = (lane: PeerLane) =>
    Effect.gen(function* () {
      // Nothing pending: nothing to send and nothing to catch up on.
      if (!(yield* L2PeerShareDB.hasPending)) {
        lane.needsCatchUp = false;
        lane.failures = 0;
        yield* recordHeld(lane, lane.operator, undefined);
        return;
      }
      if (lane.needsCatchUp) yield* catchUp(lane);
      yield* delta(lane);
      lane.failures = 0;
      if (lane.refused) {
        lane.refused = false;
        yield* Effect.logInfo(`${lane.url} accepts this node's key again`);
      }
    }).pipe(
      Effect.catchAllCause((cause) =>
        Effect.gen(function* () {
          const failure = Option.getOrUndefined(Cause.failureOption(cause));
          const request =
            failure instanceof PeerRequestError ? failure : undefined;
          const refused = request?.status === 403;
          lane.failures++;
          lane.needsCatchUp = true;
          // A peer that did not answer shows nothing about what it holds.
          yield* recordHeld(lane, lane.operator, undefined);
          const backoff = refused
            ? nodeConfig.VALIDATION_RETRY_BACKOFF_MAX_MS
            : Math.min(
                nodeConfig.VALIDATION_RETRY_BACKOFF_MAX_MS,
                nodeConfig.VALIDATION_RETRY_BACKOFF_BASE_MS *
                  2 ** Math.min(lane.failures - 1, 30),
              );
          lane.retryAt =
            Date.now() + Math.max(backoff, request?.retryAfterMs ?? 0);
          yield* Metric.increment(forwardFailures);
          if (refused) {
            if (!lane.refused)
              yield* Effect.logWarning(
                `${lane.url} refuses this node's operator key (not in its active operator set); L2 tx sharing with it retries every ${nodeConfig.VALIDATION_RETRY_BACKOFF_MAX_MS.toString()} ms`,
              );
            lane.refused = true;
            return;
          }
          yield* Effect.logWarning(
            `L2 tx sharing with ${lane.url} failed (${lane.failures.toString()} in a row): ${
              request?.message ?? String(cause)
            }`,
          );
        }),
      ),
    );

  // Each lane waits for its own wakes, so a hung peer holds no other lane.
  const runLane = (lane: PeerLane) =>
    Effect.gen(function* () {
      let seenHead = yield* SubscriptionRef.get(globals.L1_HEAD_SEQUENCE);
      for (;;) {
        const seenGeneration = yield* SubscriptionRef.get(
          globals.L2_PEER_FORWARD_GENERATION,
        );
        if (lane.retryAt <= Date.now()) yield* attempt(lane);
        const wake = Stream.merge(
          globals.L1_HEAD_SEQUENCE.changes.pipe(
            Stream.filter((head) => head !== seenHead),
          ),
          globals.L2_PEER_FORWARD_GENERATION.changes.pipe(
            Stream.filter((generation) => generation !== seenGeneration),
          ),
        ).pipe(Stream.runHead);
        // Only a failed lane needs a timer; an idle one waits for a wake.
        yield* lane.failures > 0
          ? Effect.timeout(
              wake,
              Duration.millis(Math.max(0, lane.retryAt - Date.now())),
            ).pipe(Effect.ignore)
          : wake;
        const head = yield* SubscriptionRef.get(globals.L1_HEAD_SEQUENCE);
        if (head !== seenHead) {
          seenHead = head;
          lane.needsCatchUp = true;
        }
      }
    });

  yield* Effect.forEach(lanes, runLane, {
    concurrency: "unbounded",
    discard: true,
  });
});
