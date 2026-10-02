import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect, Fiber, Schedule } from "effect";
import { expect, it } from "vitest";

import { DepositsDB } from "../src/database/index.js";
import {
  persistVisibleUserEventUTxOs,
  repeatVisibleUserEventIngestionFiber,
} from "../src/fibers/user-event-ingestion.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";
import { Globals } from "../src/services/index.js";
import {
  deterministicFixtureBytes,
  deterministicFixtureOutputReferenceId,
  deterministicFixtureTxHash,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

const visibleDeposit = (label: string): DepositsDB.Entry => ({
  [DepositsDB.Columns.ID]: deterministicFixtureOutputReferenceId(
    `ingestion-reconcile.${label}`,
  ),
  [DepositsDB.Columns.INFO]: deterministicFixtureBytes(
    `ingestion-reconcile.${label}.info`,
    48,
  ),
  [DepositsDB.Columns.INCLUSION_TIME]: new Date(
    Date.parse("2026-10-01T00:00:00.000Z"),
  ),
  [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: deterministicFixtureTxHash(
    `ingestion-reconcile.${label}.l1-tx`,
  ),
  [DepositsDB.Columns.LEDGER_TX_ID]: deterministicFixtureTxHash(
    `ingestion-reconcile.${label}.ledger-tx`,
  ),
  [DepositsDB.Columns.LEDGER_OUTPUT]: deterministicFixtureBytes(
    `ingestion-reconcile.${label}.output`,
    80,
  ),
  [DepositsDB.Columns.LEDGER_ADDRESS]:
    "addr_test1vzcsc5wzu3vsnjek2n80ayce53r4ha2g6wyetqddrp8z04q3yzv6k",
  [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
  [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
});

const VISIBLE_SET = ["a", "b", "c"].map(visibleDeposit);

const awaitCondition = <E, R>(condition: Effect.Effect<boolean, E, R>) =>
  Effect.gen(function* () {
    while (!(yield* condition)) yield* Effect.sleep(5);
  }).pipe(Effect.timeoutFail({ duration: 20_000, onTimeout: () => "timeout" }));

// The provider's full-set read outlasts the hold three times, then fits the
// grown hold; every completed read upserts the whole visible set again.
it("reconciles the full visible set past a provider slower than the initial hold, storing each deposit exactly once", async () => {
  const outcome = await Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const sql = yield* SqlClient.SqlClient;
        const globals = yield* Globals;
        let attempts = 0;
        let completed = 0;
        const reconcile = Effect.gen(function* () {
          attempts += 1;
          yield* Effect.sleep(300);
          yield* persistVisibleUserEventUTxOs({
            visibleUtxos: VISIBLE_SET,
            toEntry: (entry: DepositsDB.Entry) => Effect.succeed(entry),
            insertEntries: DepositsDB.insertEntries,
            emptyLogMessage: "none",
            foundLogMessage: (count) => `${count} found`,
          });
          completed += 1;
        });
        const fiber = yield* Effect.fork(
          repeatVisibleUserEventIngestionFiber({
            schedule: Schedule.spaced(1),
            startLogMessage: "start",
            spanName: "deposit_reconcile_test",
            action: reconcile,
            holdFloorMs: 50,
            holdCeilingMs: 10_000,
          }),
        );
        yield* awaitCondition(
          currentLivenessReasons(globals).pipe(
            Effect.map((reasons) =>
              reasons.includes(
                "user_event_ingestion_stalled:deposit_reconcile_test:3",
              ),
            ),
          ),
        );
        const rowsWhileStalled = yield* sql<{
          readonly count: string;
        }>`SELECT count(*)::text AS count FROM deposits_utxos`;
        yield* awaitCondition(Effect.sync(() => completed >= 3));
        const reasonsAfterRecovery = yield* currentLivenessReasons(globals);
        yield* Fiber.interrupt(fiber);
        const rows = yield* sql<{
          readonly event_id: Buffer;
          readonly status: string;
        }>`SELECT event_id, status FROM deposits_utxos ORDER BY event_id`;
        return {
          attempts,
          completed,
          rowsWhileStalled: Number(rowsWhileStalled[0]?.count),
          reasonsAfterRecovery,
          rows,
        };
      }).pipe(Effect.provide(Globals.Default)),
    ),
  );
  expect(outcome.rowsWhileStalled).toBe(0);
  expect(outcome.attempts).toBeGreaterThan(outcome.completed);
  expect(outcome.reasonsAfterRecovery).toEqual([]);
  // Three full reconciles of a three-deposit set: three rows, one per event.
  expect(outcome.rows.map((row) => row.event_id.toString("hex"))).toEqual(
    VISIBLE_SET.map((entry) => entry[DepositsDB.Columns.ID].toString("hex"))
      .slice()
      .sort(),
  );
  expect(new Set(outcome.rows.map((row) => row.status))).toEqual(
    new Set([DepositsDB.Status.Projected]),
  );
}, 60_000);
