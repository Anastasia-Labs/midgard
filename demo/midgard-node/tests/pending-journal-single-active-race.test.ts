import "./utils.js";

import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { Deferred, Effect, Fiber } from "effect";
import { describe, expect, it } from "vitest";

import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import {
  ACTIVE_PENDING_JOURNAL_REFUSAL,
  isSingleActiveIndexViolation,
  SINGLE_ACTIVE_INDEX,
} from "../src/database/pendingBlockFinalizations.single-active-refusal.js";
import {
  header,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const runIsolated = <A, E, R>(program: Effect.Effect<A, E, R>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        return yield* program.pipe(
          Effect.ensuring(Effect.orDie(resetApplicationTables)),
        );
      }),
    ) as Effect.Effect<A, unknown, never>,
  );

const pendingSubmissions = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [pending] = yield* sql<{ n: number }>`SELECT COUNT(*)::int AS n
    FROM pending_block_finalizations WHERE status = 'pending_submission'`;
  return pending?.n ?? -1;
});

type Side = "a" | "b";

/**
 * Both prepares pass the active-journal pre-check (each parks inside its own
 * transaction just before the insert), so the loser can only be stopped by
 * the single-active index. `first` inserts and commits before the other.
 */
const raceThroughThePreCheck = (first: Side) =>
  Effect.gen(function* () {
    const arrived = {
      a: yield* Deferred.make<void>(),
      b: yield* Deferred.make<void>(),
    };
    const go = {
      a: yield* Deferred.make<void>(),
      b: yield* Deferred.make<void>(),
    };
    const prepare = (side: Side) =>
      PendingBlockFinalizationsDB.preparePendingSubmission(
        journalFixture(header(`single-active-race-${side}`)),
        {
          beforeJournalInsert: Deferred.succeed(arrived[side], undefined).pipe(
            Effect.zipRight(Deferred.await(go[side])),
            Effect.as(undefined),
          ),
        },
      ).pipe(Effect.either, Effect.fork);
    const fibers = { a: yield* prepare("a"), b: yield* prepare("b") };
    yield* Deferred.await(arrived.a);
    yield* Deferred.await(arrived.b);
    const second: Side = first === "a" ? "b" : "a";
    yield* Deferred.succeed(go[first], undefined);
    const winner = yield* Fiber.join(fibers[first]);
    yield* Deferred.succeed(go[second], undefined);
    const loser = yield* Fiber.join(fibers[second]);
    return { winner, loser, pendingSubmissions: yield* pendingSubmissions };
  });

describe("single-active pending journal under a concurrent prepare", () => {
  it.each<Side>(["a", "b"])(
    "refuses the prepare that loses the index race (%s commits first)",
    async (first) => {
      const result = await runIsolated(raceThroughThePreCheck(first));

      expect(result.winner._tag).toBe("Right");
      expect(result.loser._tag).toBe("Left");
      const loser =
        result.loser._tag === "Left" ? result.loser.left : undefined;
      expect(loser).toMatchObject({
        _tag: "DatabaseError",
        message: ACTIVE_PENDING_JOURNAL_REFUSAL,
      });
      // The refusal came from the index, not the pre-check both passed.
      expect(inspect(loser, { depth: 12, colors: false })).toContain(
        `index=${SINGLE_ACTIVE_INDEX}`,
      );
      expect(result.pendingSubmissions).toBe(1);
    },
  );

  it("maps only the single-active index's unique violation", () => {
    const violation = (code: string, constraint_name: string) => ({
      code,
      constraint_name,
    });
    expect(
      isSingleActiveIndexViolation({
        cause: violation("23505", SINGLE_ACTIVE_INDEX),
      }),
    ).toBe(true);
    expect(
      isSingleActiveIndexViolation(violation("23505", "some_other_index")),
    ).toBe(false);
    expect(
      isSingleActiveIndexViolation(violation("40001", SINGLE_ACTIVE_INDEX)),
    ).toBe(false);
    expect(isSingleActiveIndexViolation(undefined)).toBe(false);
  });
});
