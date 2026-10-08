/** The owner reconnect scenario: the accepted emulator source behind a
 * switchable socket, the real authority SQL, and an owner whose reconnect
 * schedule, request deadline and completion step a test may override. */
import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Cause, Effect, Option, Runtime, Schedule } from "effect";
import { vi } from "vitest";

import * as Authority from "../../src/database/eventHistoryAuthority.js";
import type * as Journal from "../../src/database/eventHistoryJournal.js";
import { MempoolLedgerDB } from "../../src/database/index.js";
import type { WebSocketLike } from "../../src/l1-kupmios.js";
import {
  type HistoryOwnerChange,
  makeEventHistoryOwner,
} from "../../src/services/event-history-owner.js";
import type { HistorySourceReconnectBounds } from "../../src/services/event-history-owner.source-outage.js";
import type { HistoryRecoveryPreparation } from "../../src/services/event-history-recovery.js";
import { makeMempoolLedgerCacheService } from "../../src/services/mempool-ledger-cache.js";
import { provideDatabaseLayers } from "../utils.js";
import { historyOutputObservation } from "./history-projection-observations.js";
import { makeRollbackHistoryTransport } from "./history-rollback-transport.js";
import { openHistorySourceOwnerLifecycle } from "./history-source-owner-emulator.js";

export const truncate = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`TRUNCATE settlement_attempts, settlement_jobs, settlement_owners, event_history_l2_ledger_receipts, mempool_ledger,
    deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits,
    pending_block_finalization_withdrawals, event_history_cursor,
    event_history_block_applications, event_history_live_outputs,
    event_history_incarnations, event_history_replay_receipts,
    event_history_authority CASCADE`;
});
export const eventually = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  effect.pipe(
    Effect.retry(Schedule.spaced("10 millis")),
    Effect.timeout("20 seconds"),
  );

/** The accepted emulator source behind a socket switch: while down, every new
 * socket fails to open; with no intersection, findIntersection answers
 * Ogmios's IntersectionNotFound JSON-RPC error; with stalled tips, heartbeat
 * requests are never answered. Open sockets can be dropped. */
export const switchable = (
  source: ReturnType<typeof makeRollbackHistoryTransport>,
) => {
  const state: {
    down: boolean;
    noIntersection: boolean;
    stallTips: boolean;
    /** Called for every socket the owner opens, up or down. */
    onOpen?: () => void;
  } = { down: false, noIntersection: false, stallTips: false };
  const open = new Set<WebSocketLike>();
  const factory = source.options.webSocketFactory;
  const webSocketFactory = (): WebSocketLike => {
    state.onOpen?.();
    if (state.down) {
      const listeners = new Map<string, ((event: never) => void)[]>();
      queueMicrotask(() => {
        for (const listener of listeners.get("error") ?? [])
          listener(undefined as never);
      });
      return {
        addEventListener: (type, listener) =>
          listeners.set(type, [...(listeners.get(type) ?? []), listener]),
        send: () => {
          throw new Error("not open");
        },
        close: () => undefined,
      };
    }
    const inner = factory();
    const messages: ((event: never) => void)[] = [];
    const socket: WebSocketLike = {
      addEventListener: (type, listener, options) => {
        if (type === "message") messages.push(listener);
        if (type === "close")
          inner.addEventListener(
            type,
            (event) => {
              open.delete(socket);
              listener(event);
            },
            options,
          );
        else inner.addEventListener(type, listener, options);
      },
      send: (data) => {
        const request = JSON.parse(data) as { id: number; method: string };
        // An open socket that never answers a heartbeat.
        if (state.stallTips && request.method === "queryNetwork/tip") return;
        if (state.noIntersection && request.method === "findIntersection") {
          const error = JSON.stringify({
            jsonrpc: "2.0",
            method: "findIntersection",
            error: { code: 1000, message: "No intersection found." },
            id: request.id,
          });
          queueMicrotask(() => {
            for (const listener of messages) listener({ data: error } as never);
          });
          return;
        }
        inner.send(data);
      },
      close: (code, reason) => inner.close(code, reason),
    };
    open.add(socket);
    return socket;
  };
  return {
    state,
    options: { ...source.options, webSocketFactory },
    drop: () => {
      for (const socket of [...open]) socket.close();
    },
  };
};

export type ReconnectScenarioOptions = Readonly<{
  /** Runs before the owner starts, to reset a test file's own counters. */
  reset?: () => void;
  sourceReconnect?: Partial<HistorySourceReconnectBounds>;
  /** The source's per-request deadline, which also bounds a heartbeat. */
  timeoutMs?: number;
  prepareCompletion?: (
    checkpoint: Journal.Checkpoint,
    preparation: HistoryRecoveryPreparation,
  ) => Effect.Effect<void>;
  /** A pending reconciliation's reason while one holds the gate. */
  pending?: () => string | undefined;
  preparePendingReconciliation?: (
    checkpoint: Journal.Checkpoint,
    preparation: HistoryRecoveryPreparation,
  ) => Effect.Effect<void>;
}>;

export const scenario =
  (
    test: (run: {
      readonly owner: Effect.Effect.Success<
        ReturnType<typeof makeEventHistoryOwner<never, never>>
      >;
      readonly link: ReturnType<typeof switchable>;
      readonly source: ReturnType<typeof makeRollbackHistoryTransport>;
      readonly advance: Effect.Effect<{ id: string; slot: number }>;
      /** Extends the branch a rollbackTo left with a newly accepted batch. */
      readonly extend: Effect.Effect<{ id: string; slot: number }>;
      readonly forwards: () => string[];
      readonly changes: readonly HistoryOwnerChange[];
    }) => Effect.Effect<void, unknown, SqlClient.SqlClient>,
    options: ReconnectScenarioOptions = {},
  ) =>
  async () => {
    const h = await openHistorySourceOwnerLifecycle();
    await h.observer.flush();
    h.observer.restore();
    vi.useRealTimers();
    const addresses = [
      h.binding.hubAddress,
      ...Object.values(h.binding.deployments).flatMap((deployment) => [
        deployment.address,
        deployment.retentionAddress,
      ]),
    ];
    const interval = async () => {
      h.fixture.emulator.awaitBlock(1);
      return {
        observations: [],
        observedSlot: h.fixture.emulator.slot,
        observedHeight: h.fixture.emulator.blockHeight,
        outputs: (
          await Promise.all(
            addresses.map((address) =>
              h.fixture.operatorLucid.utxosAt(address),
            ),
          )
        )
          .flat()
          .map(historyOutputObservation),
      };
    };
    h.batches.push(await interval());
    const source = makeRollbackHistoryTransport(h);
    const link = switchable(source);
    options.reset?.();
    try {
      await Effect.runPromise(
        provideDatabaseLayers(
          Effect.scoped(
            Effect.gen(function* () {
              yield* truncate;
              const sql = yield* SqlClient.SqlClient;
              const cache = yield* makeMempoolLedgerCacheService(
                h.globals,
                MempoolLedgerDB.retrieveSpendable.pipe(
                  Effect.provideService(SqlClient.SqlClient, sql),
                ),
              );
              const changes: HistoryOwnerChange[] = [];
              const owner = yield* makeEventHistoryOwner({
                binding: h.binding,
                histories: SDK.requireEventHistoryContracts(
                  h.fixture.contracts,
                ),
                slotToUnixTime: h.fixture.operatorLucid.slotToUnixTime,
                transport: {
                  ...link.options,
                  timeoutMs: options.timeoutMs ?? link.options.timeoutMs,
                },
                heartbeatIntervalMs: 100,
                retainedPointLimit: 128,
                maximumReceiptBytes: 16 * 1024 * 1024,
                leaseDurationMs: 60_000,
                rollbackHorizon: 2160,
                sourceReconnect: {
                  initialMs: 50,
                  maxMs: 200,
                  outageLimitMs: 60_000,
                  ...options.sourceReconnect,
                },
                prepareCompletion: options.prepareCompletion,
                preparePendingReconciliation:
                  options.preparePendingReconciliation,
                ownerToken: randomUUID(),
                expectedInitializationTransactionHash:
                  h.deployment.initialization.txHash,
                cache,
                reconcile: (change) =>
                  Effect.sync(() => {
                    changes.push(change);
                    const reason = options.pending?.();
                    return reason === undefined
                      ? undefined
                      : { status: "pending" as const, reason };
                  }),
              });
              yield* owner.awaitReady.pipe(Effect.timeout("30 seconds"));
              yield* test({
                owner,
                link,
                source,
                advance: Effect.gen(function* () {
                  h.batches.push(yield* Effect.promise(interval));
                  return source.appendAccepted();
                }),
                extend: Effect.gen(function* () {
                  return source.appendFork(yield* Effect.promise(interval));
                }),
                changes,
                forwards: () =>
                  changes
                    .filter(({ kind }) => kind === "forward")
                    .map(({ after }) => after.head.id),
              });
            }),
          ),
        ),
      );
    } finally {
      source.close();
      h.observer.restore();
      vi.useRealTimers();
    }
  };

export const authorityRow = Authority.retrieve.pipe(
  Effect.map((row) => Option.getOrThrow(row)),
);
export const applications = (blockHash: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<{ total: string; block: string }>`
      SELECT count(*) FILTER (WHERE canonical) AS total,
        count(*) FILTER (WHERE block_hash = ${Buffer.from(blockHash, "hex")})
          AS block
      FROM event_history_block_applications`;
    return { total: Number(row!.total), block: Number(row!.block) };
  });
// A stop caused inside the owner's runtime arrives as a FiberFailure.
export const causeText = (cause: unknown) =>
  Runtime.isFiberFailure(cause)
    ? Cause.pretty(cause[Runtime.FiberFailureCauseId])
    : String(cause);
export const produces = (
  owner: Parameters<Parameters<typeof scenario>[0]>[0]["owner"],
) =>
  Effect.either(owner.runProducer(() => Effect.void)).pipe(
    Effect.map((result) => result._tag),
  );
