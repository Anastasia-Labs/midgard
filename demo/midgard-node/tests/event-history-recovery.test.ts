import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Deferred, Effect, Fiber, Option } from "effect";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import { sqlErrorToDatabaseError } from "../src/database/utils/common.js";
import { makeEventHistoryRecovery } from "../src/services/event-history-recovery.js";
import { Globals, publishMempoolLedgerDelta } from "../src/services/globals.js";
import {
  makeMempoolLedgerCacheService,
  type MempoolLedgerCacheService,
} from "../src/services/mempool-ledger-cache.js";
import { provideDatabaseLayers } from "./utils.js";

const deploymentIdentity = "a1".repeat(32);
const capture = {
  point: { slot: 100, id: "b1".repeat(32) },
  snapshotDigest: "c1".repeat(32),
};
const run = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(program));
const row = (id: number): MempoolLedgerDB.EntryWithTimeStamp => ({
  [MempoolLedgerDB.Columns.TX_ID]: Buffer.alloc(32, id),
  [MempoolLedgerDB.Columns.OUTREF]: Buffer.alloc(36, id),
  [MempoolLedgerDB.Columns.OUTPUT]: Buffer.from([id]),
  [MempoolLedgerDB.Columns.ADDRESS]: "addr_test1_history_recovery",
  [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: Buffer.alloc(32, id),
  [MempoolLedgerDB.Columns.TIMESTAMPTZ]: new Date(0),
});
const write = (id: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO history_recovery_probe (id) VALUES (${id})`;
  });
const replace = (id: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM history_recovery_probe`;
    yield* write(id);
  });
const fixture = (
  ownerToken = randomUUID(),
  wrapCache: (cache: MempoolLedgerCacheService) => MempoolLedgerCacheService = (
    cache,
  ) => cache,
) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const sql = yield* SqlClient.SqlClient;
    const cache = yield* makeMempoolLedgerCacheService(
      globals,
      sql<{
        id: number;
      }>`SELECT id FROM history_recovery_probe ORDER BY id`.pipe(
        Effect.map((rows) => rows.map(({ id }) => row(id))),
        sqlErrorToDatabaseError("history_recovery_probe", "load probe"),
      ),
    );
    const owner = yield* makeEventHistoryRecovery({
      deploymentIdentity,
      ownerToken,
      leaseDurationMs: 30_000,
      cache: wrapCache(cache),
    });
    return { owner, cache, globals, sql };
  });

beforeAll(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`CREATE TABLE IF NOT EXISTS history_recovery_probe (id integer PRIMARY KEY)`;
    }),
  ),
);
beforeEach(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE event_history_authority, history_recovery_probe`;
    }),
  ),
);
afterAll(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DROP TABLE history_recovery_probe`;
    }),
  ),
);

describe("canonical history recovery composition", () => {
  it("cannot persist between completion's cache reload and Ready publication", async () => {
    await run(
      Effect.gen(function* () {
        const reloaded = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const { owner, cache, sql } = yield* fixture(
          randomUUID(),
          (original) => ({
            ...original,
            retireCanonicalEpoch: original.retireCanonicalEpoch.pipe(
              Effect.map((recovery) => ({
                ...recovery,
                runRecovery: (repair, publish) =>
                  recovery.runRecovery(
                    repair,
                    Deferred.succeed(reloaded, undefined).pipe(
                      Effect.zipRight(Deferred.await(release)),
                      Effect.zipRight(publish),
                    ),
                  ),
              })),
            ),
          }),
        );
        const completing = yield* Effect.fork(
          owner.startup.complete(capture, replace(1)),
        );
        yield* Deferred.await(reloaded);
        const persisting = yield* Effect.fork(
          owner.startup.persist(write(2)).pipe(Effect.either),
        );
        yield* Effect.yieldNow();
        expect(Option.isNone(yield* Fiber.poll(persisting))).toBe(true);
        expect(yield* sql`SELECT id FROM history_recovery_probe`).toEqual([
          { id: 1 },
        ]);
        yield* Deferred.succeed(release, undefined);
        yield* Fiber.join(completing);
        expect((yield* Fiber.join(persisting))._tag).toBe("Left");
        const keys = yield* cache.withPhaseBLock(
          cache.currentState.pipe(Effect.map((state) => [...state.keys()])),
        );
        expect(keys).toEqual([row(1).outref.toString("hex")]);
        expect(yield* sql`SELECT id FROM history_recovery_probe`).toEqual([
          { id: 1 },
        ]);
      }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
    );
  });

  it("persists bounded batches without readiness, rolls back failed batches, then reloads once at completion", async () => {
    await run(
      Effect.gen(function* () {
        const { owner, cache, sql } = yield* fixture();
        const token = yield* owner.startup.persist(
          Authority.requireRecoveryTransaction.pipe(Effect.tap(() => write(1))),
        );
        expect(
          (yield* Effect.either(Authority.withReady(token, write(2))))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(owner.runProducer(() => write(2))))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(cache.withClaimLock(Effect.void)))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(
            owner.startup.persist(
              write(2).pipe(Effect.zipRight(Effect.fail("batch failed"))),
            ),
          ))._tag,
        ).toBe("Left");
        expect(
          yield* sql`SELECT id FROM history_recovery_probe ORDER BY id`,
        ).toEqual([{ id: 1 }]);
        yield* owner.startup.persist(write(3));
        const durable = yield* Authority.retrieve;
        expect(Option.isSome(durable) && durable.value.state).toBe(
          "recovering",
        );
        yield* owner.startup.complete(capture, write(4));
        const keys = yield* cache.withPhaseBLock(
          cache.currentState.pipe(Effect.map((state) => [...state.keys()])),
        );
        expect(keys).toEqual(
          [1, 3, 4].map((id) => row(id).outref.toString("hex")),
        );
        expect(
          (yield* Effect.either(owner.startup.persist(write(5))))._tag,
        ).toBe("Left");
        expect(
          yield* sql`SELECT id FROM history_recovery_probe ORDER BY id`,
        ).toEqual([{ id: 1 }, { id: 3 }, { id: 4 }]);
      }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
    );
  });

  it("completes durable and local readiness when interrupted immediately after Ready commits", async () => {
    await run(
      Effect.gen(function* () {
        const published = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const { owner, cache } = yield* fixture(randomUUID(), (original) => ({
          ...original,
          retireCanonicalEpoch: original.retireCanonicalEpoch.pipe(
            Effect.map((recovery) => ({
              ...recovery,
              runRecovery: (repair, publish) =>
                recovery.runRecovery(
                  repair,
                  publish.pipe(
                    Effect.zipRight(Deferred.succeed(published, undefined)),
                    Effect.zipRight(Deferred.await(release)),
                  ),
                ),
            })),
          ),
        }));
        const completing = yield* Effect.fork(
          owner.startup.complete(capture, replace(1)),
        );
        yield* Deferred.await(published);
        const durable = yield* Authority.retrieve;
        expect(Option.isSome(durable) && durable.value.state).toBe("ready");
        yield* Fiber.interruptFork(completing);
        yield* Effect.yieldNow();
        expect(Option.isNone(yield* Fiber.poll(completing))).toBe(true);
        yield* Deferred.succeed(release, undefined);
        yield* Fiber.await(completing);
        yield* cache.withClaimLock(Effect.void);
        yield* owner.runProducer((token) =>
          Authority.withReady(token, write(2)),
        );
      }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
    );
  });

  it("shutdown joins late producer publication and a stale owner cannot suspend its successor", async () => {
    await run(
      Effect.gen(function* () {
        const { owner, sql } = yield* fixture();
        yield* owner.startup.complete(capture, replace(1));
        const entered = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        let published = false;
        const producer = yield* Effect.fork(
          owner
            .runProducer(() =>
              Effect.gen(function* () {
                yield* Deferred.succeed(entered, undefined);
                yield* Deferred.await(release);
                published = true;
              }),
            )
            .pipe(Effect.either),
        );
        yield* Deferred.await(entered);
        yield* sql`UPDATE event_history_authority SET lease_until = clock_timestamp() - interval '1 second'`;
        const successor = yield* Authority.acquire({
          deploymentIdentity,
          ownerToken: randomUUID(),
          leaseDurationMs: 30_000,
        });
        yield* Authority.publishReady(successor, capture);
        const closing = yield* Effect.fork(owner.close);
        yield* Effect.yieldNow();
        expect(Option.isNone(yield* Fiber.poll(closing))).toBe(true);
        expect(published).toBe(false);
        yield* Deferred.succeed(release, undefined);
        expect((yield* Fiber.join(producer))._tag).toBe("Left");
        yield* Fiber.join(closing);
        expect(published).toBe(true);
        yield* Authority.withReady(successor, write(2));
        const current = yield* Authority.retrieve;
        expect(
          Option.isSome(current) && Authority.tokenFromRow(current.value),
        ).toEqual(successor);
      }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
    );
  });

  it("revalidates saved Ready on startup and publishes only after durable repair and cache reload", async () => {
    const ownerToken = randomUUID();
    const old = await run(
      Authority.acquire({
        deploymentIdentity,
        ownerToken,
        leaseDurationMs: 30_000,
      }),
    );
    await run(Authority.publishReady(old, capture));
    await run(
      Effect.gen(function* () {
        const { owner, cache, sql } = yield* fixture(ownerToken);
        expect(
          (yield* Effect.either(owner.runProducer(() => write(1))))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(cache.withClaimLock(Effect.void)))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(Authority.withReady(old, write(2))))._tag,
        ).toBe("Left");
        yield* owner.startup.complete(capture, replace(3));
        const keys = yield* cache.withPhaseBLock(
          cache.currentState.pipe(Effect.map((state) => [...state.keys()])),
        );
        expect(keys).toEqual([row(3).outref.toString("hex")]);
        yield* owner.runProducer((token) =>
          Authority.withReady(token, write(4)),
        );
        expect(
          yield* sql`SELECT id FROM history_recovery_probe ORDER BY id`,
        ).toEqual([{ id: 3 }, { id: 4 }]);
      }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
    );
    const closed = await run(Authority.retrieve);
    expect(Option.isSome(closed) && closed.value.state).toBe("suspended");
  });

  it.each(["persist", "complete"] as const)(
    "commits revocation before %s drains a producer parked between SQL commit and cache publication",
    async (operation) => {
      await run(
        Effect.gen(function* () {
          const { owner, cache, globals } = yield* fixture();
          yield* owner.startup.complete(capture, replace(1));
          const committed = yield* Deferred.make<Authority.Token>();
          const release = yield* Deferred.make<void>();
          const oldProducer = yield* Effect.fork(
            owner
              .runProducer((token) =>
                Effect.gen(function* () {
                  yield* Authority.withReady(token, write(2));
                  yield* Deferred.succeed(committed, token);
                  yield* Deferred.await(release);
                  // This deliberately delayed postcommit delta must finish BEFORE reload.
                  yield* publishMempoolLedgerDelta(
                    globals,
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
          const recovery = yield* owner.beginRecovery("rollback after commit");
          const durable = yield* Authority.retrieve;
          expect(Option.isSome(durable) && durable.value.state).toBe(
            "recovering",
          );
          expect(
            (yield* Effect.either(Authority.withReady(oldToken, write(5))))
              ._tag,
          ).toBe("Left");
          let repaired = false;
          const repair = replace(3).pipe(
            Effect.tap(() =>
              Effect.sync(() => {
                repaired = true;
              }),
            ),
          );
          const recovering = yield* Effect.fork(
            operation === "persist"
              ? recovery.persist(repair)
              : recovery.complete(capture, repair),
          );
          yield* Effect.yieldNow();
          expect(repaired).toBe(false);
          expect(Option.isNone(yield* Fiber.poll(recovering))).toBe(true);
          expect(
            (yield* Effect.either(owner.runProducer(() => write(6))))._tag,
          ).toBe("Left");
          yield* Deferred.succeed(release, undefined);
          expect((yield* Fiber.join(oldProducer))._tag).toBe("Left");
          yield* Fiber.join(recovering);
          if (operation === "persist") {
            expect(
              (yield* Effect.either(cache.withClaimLock(Effect.void)))._tag,
            ).toBe("Left");
            yield* recovery.complete(capture, Effect.void);
          }
          const keys = yield* cache.withPhaseBLock(
            cache.currentState.pipe(Effect.map((state) => [...state.keys()])),
          );
          expect(keys).toEqual([row(3).outref.toString("hex")]);
          yield* owner.runProducer((token) =>
            Authority.withReady(token, write(4)),
          );
        }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
      );
    },
  );

  it.each(["persist", "complete"] as const)(
    "a newer rollback immediately fences %s holding the SQL repair lock",
    async (operation) => {
      await run(
        Effect.gen(function* () {
          const { owner, cache, sql } = yield* fixture();
          yield* owner.startup.complete(capture, replace(1));
          const first = yield* owner.beginRecovery("first rollback");
          const entered = yield* Deferred.make<void>();
          const release = yield* Deferred.make<void>();
          const repair = Effect.gen(function* () {
            yield* replace(2);
            yield* Deferred.succeed(entered, undefined);
            yield* Deferred.await(release);
          });
          const old = yield* Effect.fork(
            (operation === "persist"
              ? first.persist(repair)
              : first.complete(capture, repair)
            ).pipe(Effect.either),
          );
          yield* Deferred.await(entered);
          const newer = yield* Effect.fork(
            owner.beginRecovery("second rollback"),
          );
          yield* Effect.yieldNow();
          expect(
            (yield* Effect.either(owner.runProducer(() => write(7))))._tag,
          ).toBe("Left");
          yield* Deferred.succeed(release, undefined);
          expect((yield* Fiber.join(old))._tag).toBe("Left");
          expect(yield* sql`SELECT id FROM history_recovery_probe`).toEqual([
            { id: 1 },
          ]);
          const next = yield* Fiber.join(newer);
          expect(
            (yield* Effect.either(cache.withClaimLock(Effect.void)))._tag,
          ).toBe("Left");
          yield* next.complete(
            { ...capture, point: { slot: 90, id: "d1".repeat(32) } },
            replace(3),
          );
          yield* owner.runProducer((token) =>
            Authority.withReady(token, write(4)),
          );
        }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
      );
    },
  );

  it("rolls back failed repair, stays suspended, and permits a same-generation retry", async () => {
    await run(
      Effect.gen(function* () {
        const { owner, cache, sql } = yield* fixture();
        const failed = yield* Effect.either(
          owner.startup.complete(
            capture,
            write(1).pipe(Effect.zipRight(Effect.fail("repair failed"))),
          ),
        );
        expect(failed._tag).toBe("Left");
        expect(yield* sql`SELECT id FROM history_recovery_probe`).toEqual([]);
        expect(
          (yield* Effect.either(cache.withClaimLock(Effect.void)))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(owner.runProducer(() => write(2))))._tag,
        ).toBe("Left");
        yield* owner.startup.complete(capture, write(3));
        yield* owner.runProducer((token) =>
          Authority.withReady(token, write(4)),
        );
        expect(
          (yield* Effect.either(owner.startup.complete(capture, write(5))))
            ._tag,
        ).toBe("Left");
      }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
    );
  });

  it("an interrupted producer releases its drain registration and renewal failure fences local claims", async () => {
    await run(
      Effect.gen(function* () {
        const { owner, cache, sql } = yield* fixture();
        yield* owner.startup.complete(capture, replace(1));
        const entered = yield* Deferred.make<void>();
        const producer = yield* Effect.fork(
          owner.runProducer(() =>
            Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Effect.never),
            ),
          ),
        );
        yield* Deferred.await(entered);
        const recovery = yield* owner.beginRecovery("cancel old fetch");
        yield* Fiber.interrupt(producer);
        yield* recovery
          .complete(capture, replace(2))
          .pipe(Effect.timeout("1 second"));
        yield* sql`UPDATE event_history_authority SET lease_until = clock_timestamp() - interval '1 second'`;
        expect((yield* Effect.either(owner.renew))._tag).toBe("Left");
        expect(
          (yield* Effect.either(cache.withClaimLock(Effect.void)))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(owner.runProducer(() => write(3))))._tag,
        ).toBe("Left");
      }).pipe(Effect.scoped, Effect.provide(Globals.Default)),
    );
  });
});

it("prepares outside SQL while fenced and refuses a superseded preparation", async () => {
  await run(
    Effect.scoped(
      Effect.gen(function* () {
        const { owner } = yield* fixture();
        const entered = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const preparing = yield* Effect.fork(
          owner.startup
            .prepare(({ token, assertCurrent }) =>
              Effect.gen(function* () {
                expect(
                  Option.isNone(
                    yield* Effect.serviceOption(
                      SqlClient.TransactionConnection,
                    ),
                  ),
                ).toBe(true);
                yield* Authority.withRecovery(
                  token,
                  assertCurrent.pipe(Effect.zipRight(write(1))),
                );
                yield* Deferred.succeed(entered, undefined);
                yield* Deferred.await(release);
                yield* assertCurrent;
              }),
            )
            .pipe(Effect.either),
        );
        yield* Deferred.await(entered);
        const blocked = yield* Effect.either(
          owner.runProducer(() => Effect.void),
        );
        expect(blocked._tag).toBe("Left");
        const replacement = yield* owner.beginRecovery(
          "source changed during native preparation",
        );
        yield* Deferred.succeed(release, undefined);
        expect((yield* Fiber.join(preparing))._tag).toBe("Left");
        yield* replacement.complete(capture, replace(2));
        yield* owner.runProducer((token, current) =>
          Authority.withReady(token, current.pipe(Effect.zipRight(write(3)))),
        );
        const sql = yield* SqlClient.SqlClient;
        expect(
          yield* sql`SELECT id FROM history_recovery_probe ORDER BY id`,
        ).toEqual([{ id: 2 }, { id: 3 }]);
      }),
    ).pipe(Effect.provide(Globals.Default)),
  );
});
