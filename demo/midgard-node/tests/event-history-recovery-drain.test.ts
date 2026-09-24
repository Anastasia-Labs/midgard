import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Deferred, Effect, Fiber, Option } from "effect";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../src/database/utils/common.js";
import { makeEventHistoryRecovery } from "../src/services/event-history-recovery.js";
import { Globals, publishMempoolLedgerDelta } from "../src/services/globals.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import { WriteBehind } from "../src/services/write-behind.js";
import { provideDatabaseLayers } from "./utils.js";

const deploymentIdentity = "a2".repeat(32);
const capture = {
  point: { slot: 100, id: "b2".repeat(32) },
  snapshotDigest: "c2".repeat(32),
};
const run = <A, E>(
  program: Effect.Effect<A, E, SqlClient.SqlClient | WriteBehind>,
) => Effect.runPromise(provideDatabaseLayers(program));
const row = (id: number): MempoolLedgerDB.EntryWithTimeStamp => ({
  [MempoolLedgerDB.Columns.TX_ID]: Buffer.alloc(32, id),
  [MempoolLedgerDB.Columns.OUTREF]: Buffer.alloc(36, id),
  [MempoolLedgerDB.Columns.OUTPUT]: Buffer.from([id]),
  [MempoolLedgerDB.Columns.ADDRESS]: "addr_test1_history_recovery_drain",
  [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: Buffer.alloc(32, id),
  [MempoolLedgerDB.Columns.TIMESTAMPTZ]: new Date(0),
});
const write = (id: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO history_recovery_drain_probe (id) VALUES (${id})`;
  });
const replace = (id: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM history_recovery_drain_probe`;
    yield* write(id);
  });

// Real PostgreSQL authority, cache loader and WriteBehind service. Only callback
// failures/barriers are controlled models. This does not exercise network source
// authentication, the txQueue fiber, or production runtime service wiring.
const fixture = (
  wrapDrain: (
    drain: Effect.Effect<void, DatabaseError>,
  ) => Effect.Effect<void, DatabaseError> = (drain) => drain,
) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const sql = yield* SqlClient.SqlClient;
    const writeBehind = yield* WriteBehind;
    const cache = yield* makeMempoolLedgerCacheService(
      globals,
      sql<{
        id: number;
      }>`SELECT id FROM history_recovery_drain_probe ORDER BY id`.pipe(
        Effect.map((rows) => rows.map(({ id }) => row(id))),
        sqlErrorToDatabaseError("history_recovery_drain_probe", "load probe"),
      ),
    );
    const owner = yield* makeEventHistoryRecovery({
      deploymentIdentity,
      ownerToken: randomUUID(),
      leaseDurationMs: 30_000,
      cache,
      drainBeforeRepair: wrapDrain(writeBehind.flushNow),
    });
    const enqueueOldProjection = writeBehind.enqueueAddressHistory([
      { tx_id: row(2).tx_id, address: row(2).address },
    ]);
    const projectionRows = sql`SELECT tx_id, address FROM address_history
      WHERE tx_id = ${row(2).tx_id} ORDER BY address`;
    const removeOldProjection = sql`DELETE FROM address_history
      WHERE tx_id = ${row(2).tx_id}`;
    const probeRows = sql`SELECT id FROM history_recovery_drain_probe ORDER BY id`;
    const assertRepaired = (id: number) =>
      Effect.gen(function* () {
        expect(yield* probeRows).toEqual([{ id }]);
        expect(yield* projectionRows).toEqual([]);
        expect((yield* writeBehind.depths).totalDepth).toBe(0);
        const keys = yield* cache.withPhaseBLock(
          cache.currentState.pipe(Effect.map((state) => [...state.keys()])),
        );
        expect(keys).toEqual([row(id).outref.toString("hex")]);
        const authority = yield* Authority.retrieve;
        expect(Option.isSome(authority) && authority.value.state).toBe("ready");
        yield* owner.runProducer((token) =>
          Authority.withReady(token, Effect.void),
        );
      });
    const assertFenced = Effect.gen(function* () {
      const authority = yield* Authority.retrieve;
      expect(Option.isSome(authority) && authority.value.state).toBe(
        "recovering",
      );
      expect(
        (yield* Effect.either(owner.runProducer(() => write(9))))._tag,
      ).toBe("Left");
      expect(
        (yield* Effect.either(cache.withClaimLock(Effect.void)))._tag,
      ).toBe("Left");
    });
    return {
      owner,
      cache,
      globals,
      sql,
      writeBehind,
      enqueueOldProjection,
      projectionRows,
      removeOldProjection,
      probeRows,
      assertRepaired,
      assertFenced,
    };
  });

beforeAll(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`CREATE TABLE IF NOT EXISTS history_recovery_drain_probe (id integer PRIMARY KEY)`;
    }),
  ),
);
beforeEach(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE event_history_authority, history_recovery_drain_probe`;
      yield* sql`DELETE FROM address_history WHERE tx_id = ${row(2).tx_id}`;
    }),
  ),
);
afterAll(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DROP TABLE history_recovery_drain_probe`;
      yield* sql`DELETE FROM address_history WHERE tx_id = ${row(2).tx_id}`;
    }),
  ),
);

describe("canonical recovery drains deferred writes before inverse SQL", () => {
  it.each(["persist", "complete"] as const)(
    "%s waits for postcommit enqueue, drains its real projection, and never reapplies it after repair",
    async (operation) =>
      run(
        Effect.gen(function* () {
          let drainCalls = 0;
          const f = yield* fixture((drain) =>
            Effect.sync(() => {
              drainCalls += 1;
            }).pipe(Effect.zipRight(drain)),
          );
          yield* f.owner.startup.complete(capture, replace(1));
          const initialDrainCalls = drainCalls;
          const committed = yield* Deferred.make<Authority.Token>();
          const releaseProducer = yield* Deferred.make<void>();
          const producer = yield* Effect.fork(
            f.owner
              .runProducer((token) =>
                Effect.gen(function* () {
                  yield* Authority.withReady(token, write(2));
                  yield* Deferred.succeed(committed, token);
                  yield* Deferred.await(releaseProducer);
                  // Admission has committed, but its producer lifetime still owns enqueue
                  // and postcommit cache publication. Recovery must drain both first.
                  yield* f.enqueueOldProjection;
                  yield* publishMempoolLedgerDelta(
                    f.globals,
                    {
                      full: false,
                      upserts: [[row(2).outref.toString("hex"), row(2).output]],
                      deletes: [],
                    },
                    16,
                  );
                }),
              )
              .pipe(Effect.either),
          );
          const oldToken = yield* Deferred.await(committed);
          const recovery = yield* f.owner.beginRecovery(
            "rollback after admission SQL before enqueue",
          );
          const repair = Effect.gen(function* () {
            // Durable evidence proves real flush happened before inverse SQL.
            expect(yield* f.projectionRows).toEqual([
              { tx_id: row(2).tx_id, address: row(2).address },
            ]);
            expect((yield* f.writeBehind.depths).totalDepth).toBe(0);
            yield* f.removeOldProjection;
            yield* replace(3);
          });
          const recovering = yield* Effect.fork(
            operation === "persist"
              ? recovery.persist(repair)
              : recovery.complete(capture, repair),
          );
          yield* Effect.yieldNow();
          expect(Option.isNone(yield* Fiber.poll(recovering))).toBe(true);
          expect(drainCalls).toBe(initialDrainCalls);
          expect(yield* f.probeRows).toEqual([{ id: 1 }, { id: 2 }]);
          expect(yield* f.projectionRows).toEqual([]);
          yield* f.assertFenced;
          expect(
            (yield* Effect.either(Authority.withReady(oldToken, write(8))))
              ._tag,
          ).toBe("Left");
          yield* Deferred.succeed(releaseProducer, undefined);
          expect((yield* Fiber.join(producer))._tag).toBe("Left");
          yield* Fiber.join(recovering);
          if (operation === "persist") {
            yield* f.assertFenced;
            expect(yield* f.probeRows).toEqual([{ id: 3 }]);
            yield* recovery.complete(capture, Effect.void);
          }
          yield* f.assertRepaired(3);
          // A later consumer/finalizer flush cannot resurrect the old projection.
          yield* f.writeBehind.flushNow;
          yield* f.assertRepaired(3);
          expect(
            (yield* Effect.either(Authority.withReady(oldToken, write(8))))
              ._tag,
          ).toBe("Left");
          expect(yield* f.probeRows).toEqual([{ id: 3 }]);
        }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
      ),
  );

  it.each(["persist", "complete"] as const)(
    "%s refuses repair and Ready after a controlled drain failure, retaining real queued work for retry",
    async (operation) =>
      run(
        Effect.gen(function* () {
          let failDrain = false;
          const failure = new DatabaseError({
            table: "address_history",
            message: "controlled drain failure before actual flush",
            cause: undefined,
          });
          const f = yield* fixture((drain) =>
            Effect.suspend(() => (failDrain ? Effect.fail(failure) : drain)),
          );
          yield* f.owner.startup.complete(capture, replace(1));
          yield* f.owner.runProducer(() => f.enqueueOldProjection);
          expect((yield* f.writeBehind.depths).totalDepth).toBe(1);
          failDrain = true;
          const recovery = yield* f.owner.beginRecovery(
            "retry failed projection drain",
          );
          const repair = Effect.gen(function* () {
            expect(yield* f.projectionRows).toEqual([
              { tx_id: row(2).tx_id, address: row(2).address },
            ]);
            yield* f.removeOldProjection;
            yield* replace(3);
          });
          const result = yield* Effect.either(
            operation === "persist"
              ? recovery.persist(repair)
              : recovery.complete(capture, repair),
          );
          expect(result._tag).toBe("Left");
          if (result._tag === "Left") expect(result.left).toBe(failure);
          yield* f.assertFenced;
          expect(yield* f.probeRows).toEqual([{ id: 1 }]);
          expect(yield* f.projectionRows).toEqual([]);
          expect((yield* f.writeBehind.depths).totalDepth).toBe(1);
          failDrain = false;
          if (operation === "persist") {
            yield* recovery.persist(repair);
            yield* f.assertFenced;
            yield* recovery.complete(capture, Effect.void);
          } else yield* recovery.complete(capture, repair);
          yield* f.assertRepaired(3);
          yield* f.writeBehind.flushNow;
          yield* f.assertRepaired(3);
        }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
      ),
  );

  it("a source supersession during a real drain fences the old repair and only its successor can reload and publish Ready", async () =>
    run(
      Effect.gen(function* () {
        const drained = yield* Deferred.make<void>();
        const releaseDrain = yield* Deferred.make<void>();
        let pauseDrain = false;
        const f = yield* fixture((drain) =>
          drain.pipe(
            Effect.zipRight(
              Effect.suspend(() =>
                pauseDrain
                  ? Deferred.succeed(drained, undefined).pipe(
                      Effect.zipRight(Deferred.await(releaseDrain)),
                    )
                  : Effect.void,
              ),
            ),
          ),
        );
        yield* f.owner.startup.complete(capture, replace(1));
        yield* f.owner.runProducer(() => f.enqueueOldProjection);
        pauseDrain = true;
        const first = yield* f.owner.beginRecovery("first source rollback");
        const old = yield* Effect.fork(
          first.complete(capture, replace(2)).pipe(Effect.either),
        );
        yield* Deferred.await(drained);
        expect(yield* f.projectionRows).toEqual([
          { tx_id: row(2).tx_id, address: row(2).address },
        ]);
        expect((yield* f.writeBehind.depths).totalDepth).toBe(0);
        // No SQL or cache lock is held during the callback barrier: source
        // revocation completes before the old callback is allowed to return.
        const successor = yield* f.owner.beginRecovery(
          "source superseded during drain",
        );
        yield* f.assertFenced;
        pauseDrain = false;
        yield* Deferred.succeed(releaseDrain, undefined);
        expect((yield* Fiber.join(old))._tag).toBe("Left");
        expect(yield* f.probeRows).toEqual([{ id: 1 }]);
        yield* f.assertFenced;
        const nextCapture = {
          ...capture,
          point: { slot: 90, id: "d2".repeat(32) },
        };
        yield* successor.complete(
          nextCapture,
          f.removeOldProjection.pipe(Effect.zipRight(replace(4))),
        );
        yield* f.assertRepaired(4);
        const authority = yield* Authority.retrieve;
        expect(
          Option.isSome(authority) &&
            authority.value.point_hash?.toString("hex"),
        ).toBe(nextCapture.point.id);
        yield* f.writeBehind.flushNow;
        yield* f.assertRepaired(4);
      }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
    ));
});
