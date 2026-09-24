import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Deferred, Effect, Fiber, Option } from "effect";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import { provideDatabaseLayers } from "./utils.js";

const deploymentIdentity = "aa".repeat(32);
const capture = {
  point: { slot: 100, id: "bb".repeat(32) },
  snapshotDigest: "cc".repeat(32),
};
const run = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(program));
const acquire = () =>
  Authority.acquire({
    deploymentIdentity,
    ownerToken: randomUUID(),
    leaseDurationMs: 30_000,
  });
const write = (id: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO history_authority_probe (id) VALUES (${id})`;
  });
const expire = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`UPDATE event_history_authority SET lease_until = clock_timestamp() - interval '1 second'`;
});

beforeAll(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`CREATE TABLE history_authority_probe (id integer PRIMARY KEY)`;
    }),
  ),
);
beforeEach(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE event_history_authority, history_authority_probe`;
    }),
  ),
);
afterAll(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DROP TABLE history_authority_probe`;
    }),
  ),
);

describe("durable canonical history generation fence", () => {
  it("recovery repair requires its exact live generation and cannot use a nested outer transaction", async () => {
    await run(
      Effect.gen(function* () {
        const token = yield* acquire();
        yield* Authority.withRecovery(token, write(1));
        const sql = yield* SqlClient.SqlClient;
        expect(
          (yield* Effect.either(
            sql.withTransaction(Authority.withRecovery(token, write(2))),
          ))._tag,
        ).toBe("Left");
        yield* Authority.publishReady(token, capture);
        expect(
          (yield* Effect.either(Authority.withRecovery(token, write(3))))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(
            sql.withTransaction(Authority.withReady(token, write(4))),
          ))._tag,
        ).toBe("Left");
        const next = yield* Authority.beginRecovery(token, "replay");
        expect(
          (yield* Effect.either(Authority.withRecovery(token, write(5))))._tag,
        ).toBe("Left");
        yield* Authority.withRecovery(next, write(6));
        expect(
          yield* sql`SELECT id FROM history_authority_probe ORDER BY id`,
        ).toEqual([{ id: 1 }, { id: 6 }]);
      }),
    );
  });

  it("refuses empty state and startup recovery, then protects real SQL mutations", async () => {
    await run(
      Effect.gen(function* () {
        const nonexistent = {
          deploymentIdentity,
          ownerToken: randomUUID(),
          generation: "0",
        };
        expect(
          (yield* Effect.either(Authority.withReady(nonexistent, write(1))))
            ._tag,
        ).toBe("Left");
        const token = yield* acquire();
        expect(
          (yield* Effect.either(Authority.withReady(token, write(2))))._tag,
        ).toBe("Left");
        yield* Authority.publishReady(token, capture);
        yield* Authority.withReady(token, write(3));
        const sql = yield* SqlClient.SqlClient;
        expect(yield* sql`SELECT id FROM history_authority_probe`).toEqual([
          { id: 3 },
        ]);
      }),
    );
  });

  it("persists recovery across independent scopes and rejects old fetch completion after generation replacement", async () => {
    const old = await run(acquire());
    await run(Authority.publishReady(old, capture));
    const next = await run(Authority.beginRecovery(old, "canonical rollback"));
    expect(BigInt(next.generation)).toBe(BigInt(old.generation) + 1n);
    const reopened = await run(Authority.retrieve);
    expect(Option.isSome(reopened) && reopened.value.state).toBe("recovering");
    await expect(run(Authority.publishReady(old, capture))).rejects.toThrow(
      /generation or owner changed/,
    );
    await expect(run(Authority.withReady(next, write(1)))).rejects.toThrow(
      /not ready/,
    );
    const replacement = {
      point: { slot: 90, id: "dd".repeat(32) },
      snapshotDigest: "ee".repeat(32),
    };
    await run(Authority.publishReady(next, replacement));
    await run(Authority.withReady(next, write(2)));
    await expect(run(Authority.withReady(old, write(3)))).rejects.toThrow(
      /generation or owner changed/,
    );
  });

  it("does not steal live ownership or overwrite a foreign deployment after expiry", async () => {
    const owner = await run(acquire());
    await expect(run(acquire())).rejects.toThrow(/live owner/);
    await run(expire);
    await expect(
      run(
        Authority.acquire({
          deploymentIdentity: "ff".repeat(32),
          ownerToken: randomUUID(),
          leaseDurationMs: 30_000,
        }),
      ),
    ).rejects.toThrow(/another deployment/);
    const successor = await run(acquire());
    expect(successor.ownerToken).not.toBe(owner.ownerToken);
    expect(BigInt(successor.generation)).toBe(BigInt(owner.generation) + 1n);
    await expect(run(Authority.publishReady(owner, capture))).rejects.toThrow(
      /generation or owner changed/,
    );
    await expect(run(Authority.withReady(successor, write(1)))).rejects.toThrow(
      /not ready/,
    );
  });

  it("same-owner reacquisition revokes persisted ready state instead of treating it as fresh authority", async () => {
    const token = await run(acquire());
    await run(Authority.publishReady(token, capture));
    const next = await run(
      Authority.acquire({
        deploymentIdentity,
        ownerToken: token.ownerToken,
        leaseDurationMs: 30_000,
      }),
    );
    expect(BigInt(next.generation)).toBe(BigInt(token.generation) + 1n);
    await expect(run(Authority.withReady(next, write(1)))).rejects.toThrow(
      /not ready/,
    );
  });

  it("suspension revokes old tokens and cannot be renewed or published without reacquisition", async () => {
    const token = await run(acquire());
    await run(Authority.publishReady(token, capture));
    await run(Authority.suspend(token, "Ogmios disconnected"));
    const row = await run(Authority.retrieve);
    if (Option.isNone(row)) throw new Error("Missing authority");
    expect(row.value.state).toBe("suspended");
    const suspended = Authority.tokenFromRow(row.value);
    await expect(run(Authority.renew(suspended, 30_000))).rejects.toThrow();
    await expect(
      run(Authority.publishReady(suspended, capture)),
    ).rejects.toThrow();
    await expect(run(Authority.suspend(token, "late close"))).rejects.toThrow(
      /generation or owner changed/,
    );
  });

  it("requires a new generation for backward or equal-height conflicting points", async () => {
    const token = await run(acquire());
    await run(Authority.publishReady(token, capture));
    for (const point of [
      { slot: 99, id: capture.point.id },
      { slot: 100, id: "dd".repeat(32) },
    ])
      await expect(
        run(Authority.publishReady(token, { ...capture, point })),
      ).rejects.toThrow(/new generation/);
    await run(
      Authority.publishReady(token, {
        ...capture,
        point: { ...capture.point, slot: 101 },
      }),
    );
  });

  it("rolls back a mutation whose lease expires during its transaction", async () => {
    await run(
      Effect.gen(function* () {
        const token = yield* acquire();
        yield* Authority.publishReady(token, capture);
        const result = yield* Effect.either(
          Authority.withReady(token, write(1).pipe(Effect.zipRight(expire))),
        );
        expect(result._tag).toBe("Left");
        const sql = yield* SqlClient.SqlClient;
        expect(yield* sql`SELECT id FROM history_authority_probe`).toEqual([]);
        // Both the probe write and in-transaction expiry rolled back together.
        yield* Authority.withReady(token, write(2));
      }),
    );
  });

  it("serializes generation revocation after the active SQL mutation, then refuses late writes", async () => {
    await run(
      Effect.gen(function* () {
        const token = yield* acquire();
        yield* Authority.publishReady(token, capture);
        const entered = yield* Deferred.make<void>();
        const finish = yield* Deferred.make<void>();
        const writer = yield* Effect.fork(
          Authority.withReady(
            token,
            Effect.gen(function* () {
              yield* write(1);
              yield* Deferred.succeed(entered, undefined);
              yield* Deferred.await(finish);
            }),
          ),
        );
        yield* Deferred.await(entered);
        const recovery = yield* Effect.fork(
          Authority.beginRecovery(token, "rollback arrival"),
        );
        yield* Effect.yieldNow();
        expect(Option.isNone(yield* Fiber.poll(recovery))).toBe(true);
        yield* Deferred.succeed(finish, undefined);
        yield* Fiber.join(writer);
        const next = yield* Fiber.join(recovery);
        expect(BigInt(next.generation)).toBe(BigInt(token.generation) + 1n);
        expect(
          (yield* Effect.either(Authority.withReady(token, write(2))))._tag,
        ).toBe("Left");
        const sql = yield* SqlClient.SqlClient;
        expect(yield* sql`SELECT id FROM history_authority_probe`).toEqual([
          { id: 1 },
        ]);
      }),
    );
  });
});
