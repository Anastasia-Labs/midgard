import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Duration, Effect, Logger } from "effect";
import { describe, expect } from "vitest";

import { StateQueueMutationLeasesDB as Leases } from "../src/database/index.js";
import {
  LEASE_SETTLE_ATTEMPTS,
  settleLeaseDurably,
} from "../src/database/stateQueueMutationLeases.settle.js";
import {
  type SqlStatementCall,
  withFailingStatements,
} from "./sql-fault-injection.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/** Each test starts from an empty database and leaves one behind. */
const isolatedLeases = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      // A lease a test leaves active would make a later file on this shard
      // find the scope busy.
      return yield* effect.pipe(
        Effect.ensuring(Effect.orDie(resetApplicationTables)),
      );
    }),
  );

/** A release or failure record written for a lease taken by `holder`. */
const isSettleWriteOf =
  (holder: string) =>
  ({ text, values }: SqlStatementCall) =>
    text.trimStart().startsWith("UPDATE") &&
    values.some(
      (value) => typeof value === "string" && value.startsWith(`${holder}:`),
    ) &&
    values.some(
      (value) =>
        value === Leases.Status.Released || value === Leases.Status.Failed,
    );

/** Runs `effect` with the first `count` settle writes for `holder` failing. */
const withSettleFailures = <A, E, R>(
  holder: string,
  count: { remaining: number },
  effect: Effect.Effect<A, E, R>,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const matches = isSettleWriteOf(holder);
    const failing = withFailingStatements(sql, (call) => {
      if (count.remaining <= 0 || !matches(call)) return false;
      count.remaining -= 1;
      return true;
    });
    return yield* Effect.provideService(effect, SqlClient.SqlClient, failing);
  });

const rowsOf = (holder: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Leases.Entry>`SELECT * FROM state_queue_mutation_leases
      WHERE holder = ${holder} ORDER BY acquired_at ASC`;
  });

const collectLogs = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const logs: string[] = [];
    const value = yield* effect.pipe(
      Effect.provide(
        Logger.replace(
          Logger.defaultLogger,
          Logger.make(({ message }) => {
            logs.push(
              (Array.isArray(message) ? message : [message])
                .map(String)
                .join(" "),
            );
          }),
        ),
      ),
    );
    return { value, logs };
  });

describe("state-queue mutation lease settlement", () => {
  it.live(
    "retries a release that fails transiently and lands it within the budget",
    () =>
      isolatedLeases(
        Effect.gen(function* () {
          const failures = { remaining: 2 };
          const startedAt = Date.now();
          const { value, logs } = yield* collectLogs(
            withSettleFailures(
              "transient",
              failures,
              Leases.tryWithLease("transient", (token) =>
                Effect.succeed(token),
              ),
            ),
          );
          const elapsedMs = Date.now() - startedAt;

          expect(value._tag).toBe("Ran");
          expect(failures.remaining).toBe(0);
          const [row] = yield* rowsOf("transient");
          expect(row?.status).toBe(Leases.Status.Released);
          expect(elapsedMs).toBeLessThan(10_000);
          expect(Leases.unsettledOwnLeaseTokens()).toEqual([]);
          expect(
            logs.filter((line) => line.includes("released write failed")),
          ).toHaveLength(2);
          expect(yield* Leases.retrieveActive()).toBeUndefined();
        }),
      ),
  );

  it.live("retries the failure record of a failed program", () =>
    isolatedLeases(
      Effect.gen(function* () {
        const failures = { remaining: 2 };
        const outcome = yield* withSettleFailures(
          "failing-program",
          failures,
          Leases.tryWithLease("failing-program", () =>
            Effect.fail(new Error("boom")),
          ),
        ).pipe(Effect.either);

        expect(outcome._tag).toBe("Left");
        expect(failures.remaining).toBe(0);
        const [row] = yield* rowsOf("failing-program");
        expect(row?.status).toBe(Leases.Status.Failed);
        expect(row?.last_error).toContain("boom");
      }),
    ),
  );

  it.live(
    "a release that never landed is retried by the next acquisition instead of blocking it until expiry",
    () =>
      isolatedLeases(
        Effect.gen(function* () {
          const failures = { remaining: LEASE_SETTLE_ATTEMPTS };
          const first = yield* withSettleFailures(
            "stranded",
            failures,
            Leases.tryWithLease("stranded", (token) => Effect.succeed(token)),
          );
          expect(first._tag).toBe("Ran");
          expect(failures.remaining).toBe(0);
          const token = first._tag === "Ran" ? first.value : "";
          // Every attempt failed: the default ten-minute lease is still held.
          expect((yield* rowsOf("stranded"))[0]?.status).toBe(
            Leases.Status.Active,
          );
          expect(Leases.unsettledOwnLeaseTokens()).toEqual([token]);

          const next = yield* Leases.tryAcquire({ holder: "next" });

          expect(next._tag).toBe("Acquired");
          expect((yield* rowsOf("stranded"))[0]?.status).toBe(
            Leases.Status.Released,
          );
          expect(Leases.unsettledOwnLeaseTokens()).toEqual([]);
          if (next._tag === "Acquired") yield* Leases.release(next.token);
        }),
      ),
  );

  it.live(
    "a retried settlement never ends a lease taken under another token",
    () =>
      isolatedLeases(
        Effect.gen(function* () {
          const holder = yield* Leases.tryAcquire({ holder: "live-holder" });
          expect(holder._tag).toBe("Acquired");
          const liveToken = holder._tag === "Acquired" ? holder.token : "";

          // Direct writes under a token that is not the live one.
          yield* Leases.release(`${liveToken}-other`);
          yield* Leases.markFailed("live-holder:wrong-token", "wrong token");
          yield* settleLeaseDurably("live-holder:wrong-token", "live-holder", {
            kind: "released",
          });
          expect((yield* Leases.retrieveActive())?.token).toBe(liveToken);
          yield* Leases.release(liveToken);

          // A stranded token whose lease expired while another holder took
          // the scope: retrying it at the next acquisition leaves that
          // holder's lease alone.
          const failures = { remaining: LEASE_SETTLE_ATTEMPTS + 1 };
          const stranded = yield* withSettleFailures(
            "expiring",
            failures,
            Leases.tryWithLease("expiring", (token) => Effect.succeed(token), {
              ttlMs: 200,
              renewIntervalMs: 10_000,
            }),
          );
          expect(stranded._tag).toBe("Ran");
          yield* Effect.sleep(Duration.millis(300));
          // The retry at this acquisition fails too, so the token stays
          // stranded past it.
          const successor = yield* withSettleFailures(
            "expiring",
            failures,
            Leases.tryAcquire({ holder: "successor" }),
          );
          expect(successor._tag).toBe("Acquired");
          expect(failures.remaining).toBe(0);
          expect(Leases.unsettledOwnLeaseTokens()).toHaveLength(1);

          const contender = yield* Leases.tryAcquire({ holder: "contender" });

          expect(contender._tag).toBe("Busy");
          expect(Leases.unsettledOwnLeaseTokens()).toEqual([]);
          expect((yield* Leases.retrieveActive())?.holder).toBe("successor");
          expect((yield* rowsOf("expiring"))[0]?.status).toBe(
            Leases.Status.Failed,
          );
        }),
      ),
  );
});
