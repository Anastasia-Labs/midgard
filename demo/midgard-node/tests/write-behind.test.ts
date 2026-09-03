import { it as effectIt } from "@effect/vitest";
import {
  Deferred,
  Duration,
  Effect,
  Fiber,
  Metric,
  Option,
  Ref,
  TestClock,
} from "effect";
import { describe, expect, it } from "vitest";

import { addressHistoryInsertSqlDurationTimer } from "../src/database/addressHistory.js";
import {
  mempoolTxDeltasPreparationDurationTimer,
  mempoolTxDeltasSqlDurationTimer,
} from "../src/database/mempoolTxDeltas.js";
import type { WriteBehindItem } from "../src/services/write-behind.js";
import {
  persistWriteBehindInlineOverflowWithRetry,
  readWriteBehindTelemetry,
  recordWriteBehindTransactionTelemetry,
  summarizeWriteBehindTelemetry,
  takeWriteBehindProjectionBatch,
  takeWriteBehindRowBatch,
  writeBehindFlushDurationTimer,
  writeBehindInlineFallbackCounter,
} from "../src/services/write-behind.js";

const deltaItem = (count: number): WriteBehindItem => ({
  kind: "tx_deltas",
  deltas: Array.from({ length: count }, (_, index) => ({
    txId: Buffer.alloc(32, index),
    spent: [],
    produced: [],
  })),
});

const addressItem = (count: number): WriteBehindItem => ({
  kind: "address_history",
  entries: Array.from({ length: count }, (_, index) => ({
    tx_id: Buffer.alloc(32, index),
    address: `addr_test_${index.toString()}`,
  })),
});

const rowCount = (items: readonly WriteBehindItem[]): number =>
  items.reduce(
    (sum, item) =>
      sum +
      (item.kind === "tx_deltas" ? item.deltas.length : item.entries.length),
    0,
  );

describe("write-behind row batching", () => {
  it("caps a mixed-kind flush by rows and retains the exact remainder", () => {
    const result = takeWriteBehindRowBatch(
      [deltaItem(700), addressItem(700), deltaItem(50)],
      1_000,
    );

    expect(rowCount(result.batch)).toBe(1_000);
    expect(rowCount(result.remaining)).toBe(450);
    expect(result.batch.map((item) => item.kind)).toStrictEqual([
      "tx_deltas",
      "address_history",
    ]);
    expect(result.remaining.map((item) => item.kind)).toStrictEqual([
      "address_history",
      "tx_deltas",
    ]);
  });

  it("takes the cap from each projection in one transaction", () => {
    const result = takeWriteBehindProjectionBatch(
      [deltaItem(3), addressItem(4), deltaItem(2), addressItem(1)],
      4,
    );
    const deltaRows = result.batch
      .filter((item) => item.kind === "tx_deltas")
      .reduce((sum, item) => sum + item.deltas.length, 0);
    const addressRows = result.batch
      .filter((item) => item.kind === "address_history")
      .reduce((sum, item) => sum + item.entries.length, 0);
    expect(deltaRows).toBe(4);
    expect(addressRows).toBe(4);
    expect(rowCount(result.remaining)).toBe(2);
  });

  it("returns every row when the pending set is below the bound", () => {
    const result = takeWriteBehindRowBatch([deltaItem(2), addressItem(3)], 10);

    expect(rowCount(result.batch)).toBe(5);
    expect(result.remaining).toStrictEqual([]);
  });

  effectIt.effect(
    "retains inline overflow backpressure until a failed persistence retries successfully",
    () =>
      Effect.gen(function* () {
        const attempts = yield* Ref.make(0);
        const firstFailureObserved = yield* Deferred.make<void>();
        const retryStarted = yield* Deferred.make<void>();
        const allowRetryToPersist = yield* Deferred.make<void>();
        const persisted = yield* Ref.make(false);
        const persist = Effect.gen(function* () {
          const attempt = yield* Ref.updateAndGet(
            attempts,
            (current) => current + 1,
          );
          if (attempt === 1) {
            yield* Deferred.succeed(firstFailureObserved, undefined);
            return yield* Effect.fail("transient inline persistence failure");
          }
          yield* Deferred.succeed(retryStarted, undefined);
          yield* Deferred.await(allowRetryToPersist);
          yield* Ref.set(persisted, true);
        });

        const retryFiber = yield* Effect.fork(
          persistWriteBehindInlineOverflowWithRetry(persist, 10),
        );
        yield* Deferred.await(firstFailureObserved);
        expect(yield* Ref.get(persisted)).toBe(false);
        expect(Option.isNone(yield* Fiber.poll(retryFiber))).toBe(true);

        yield* TestClock.adjust(Duration.millis(10));
        yield* Deferred.await(retryStarted);
        expect(Option.isNone(yield* Fiber.poll(retryFiber))).toBe(true);
        yield* Deferred.succeed(allowRetryToPersist, undefined);
        yield* Fiber.join(retryFiber);

        expect(yield* Ref.get(attempts)).toBe(2);
        expect(yield* Ref.get(persisted)).toBe(true);
      }),
  );
});

describe("write-behind telemetry", () => {
  effectIt.effect(
    "reports finite nonnegative component deltas including a final-tail flush",
    () =>
      Effect.gen(function* () {
        const before = yield* readWriteBehindTelemetry;

        yield* writeBehindFlushDurationTimer(
          Effect.succeed(Duration.millis(7)),
        );
        yield* mempoolTxDeltasPreparationDurationTimer(
          Effect.succeed(Duration.millis(2)),
        );
        yield* mempoolTxDeltasSqlDurationTimer(
          Effect.succeed(Duration.millis(3)),
        );
        yield* addressHistoryInsertSqlDurationTimer(
          Effect.succeed(Duration.millis(1)),
        );
        yield* recordWriteBehindTransactionTelemetry(4, 10);

        // Mirrors the benchmark's explicit tail flush before its final metric
        // snapshot. Both successful transactions and all rows must be present.
        yield* writeBehindFlushDurationTimer(
          Effect.succeed(Duration.millis(5)),
        );
        yield* mempoolTxDeltasPreparationDurationTimer(
          Effect.succeed(Duration.millis(1)),
        );
        yield* mempoolTxDeltasSqlDurationTimer(
          Effect.succeed(Duration.millis(2)),
        );
        yield* addressHistoryInsertSqlDurationTimer(
          Effect.succeed(Duration.millis(1)),
        );
        yield* recordWriteBehindTransactionTelemetry(2, 8);
        yield* Metric.increment(writeBehindInlineFallbackCounter);

        const after = yield* readWriteBehindTelemetry;
        const report = summarizeWriteBehindTelemetry(before, after);

        expect(report).toStrictEqual({
          writeBehindFlushMs: 12,
          writeBehindFlushCount: 2,
          writeBehindFlushRows: 6,
          writeBehindTxDeltaPreparationCborMs: 3,
          writeBehindDeltaSqlMs: 5,
          writeBehindAddressSqlMs: 2,
          writeBehindTransactionMs: 18,
          writeBehindTransactionOverheadMs: 8,
          writeBehindInlineFallbackCount: 1,
        });
        for (const value of Object.values(report)) {
          expect(Number.isFinite(value)).toBe(true);
          expect(value).toBeGreaterThanOrEqual(0);
        }
      }).pipe(Effect.tagMetrics("write_behind_test", "final_tail_telemetry")),
  );
});
