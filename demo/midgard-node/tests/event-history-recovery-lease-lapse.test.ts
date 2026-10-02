/**
 * A history lease that lapses while this process still holds it (a stall
 * longer than the lease: a paused event loop, a slow database) and that
 * nobody else claimed is re-taken as a new Recovering generation, and the
 * lapsed work is superseded, so the owner reconnects into a fresh recovery
 * instead of stopping. A lease another owner claimed is never taken back, and
 * a live lease is never re-taken.
 */
import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Either, type Scope } from "effect";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import { sqlErrorToDatabaseError } from "../src/database/utils/common.js";
import {
  HistoryRecoverySuperseded,
  makeEventHistoryRecovery,
} from "../src/services/event-history-recovery.js";
import { Globals } from "../src/services/globals.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import { provideDatabaseLayers } from "./utils.js";

const deploymentIdentity = "a2".repeat(32);
const capture = {
  point: { slot: 100, id: "b2".repeat(32) },
  snapshotDigest: "c2".repeat(32),
};
const run = <A, E>(
  program: Effect.Effect<A, E, SqlClient.SqlClient | Globals | Scope.Scope>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      program.pipe(Effect.scoped, Effect.provide(Globals.Default)),
    ),
  );
const row = (id: number): MempoolLedgerDB.EntryWithTimeStamp => ({
  [MempoolLedgerDB.Columns.TX_ID]: Buffer.alloc(32, id),
  [MempoolLedgerDB.Columns.OUTREF]: Buffer.alloc(36, id),
  [MempoolLedgerDB.Columns.OUTPUT]: Buffer.from([id]),
  [MempoolLedgerDB.Columns.ADDRESS]: "addr_test1_history_lease_lapse",
  [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: Buffer.alloc(32, id),
  [MempoolLedgerDB.Columns.TIMESTAMPTZ]: new Date(0),
});
const write = (id: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO history_lease_lapse_probe (id) VALUES (${id})`;
  });
const replace = (id: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM history_lease_lapse_probe`;
    yield* write(id);
  });
const probe = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return (yield* sql<{
    id: number;
  }>`SELECT id FROM history_lease_lapse_probe ORDER BY id`).map(({ id }) => id);
});
const lapse = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`UPDATE event_history_authority SET lease_until = clock_timestamp() - interval '1 second'`;
});
const authority = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [current] = yield* sql<{
    owner_token: string;
    generation: string;
    state: string;
    lease_live: boolean;
  }>`SELECT owner_token, generation, state, lease_until > clock_timestamp() AS lease_live
    FROM event_history_authority WHERE singleton = true`;
  return current!;
});
const fixture = (ownerToken: string) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const sql = yield* SqlClient.SqlClient;
    const cache = yield* makeMempoolLedgerCacheService(
      globals,
      sql<{
        id: number;
      }>`SELECT id FROM history_lease_lapse_probe ORDER BY id`.pipe(
        Effect.map((rows) => rows.map(({ id }) => row(id))),
        sqlErrorToDatabaseError("history_lease_lapse_probe", "load probe"),
      ),
    );
    return yield* makeEventHistoryRecovery({
      deploymentIdentity,
      ownerToken,
      leaseDurationMs: 30_000,
      cache,
    });
  });
const superseded = (result: Either.Either<unknown, unknown>) =>
  Either.isLeft(result) && result.left instanceof HistoryRecoverySuperseded;

beforeAll(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`CREATE TABLE IF NOT EXISTS history_lease_lapse_probe (id integer PRIMARY KEY)`;
    }),
  ),
);
beforeEach(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE event_history_authority, history_lease_lapse_probe`;
    }),
  ),
);
afterAll(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DROP TABLE history_lease_lapse_probe`;
    }),
  ),
);

describe("history lease that lapsed while this process held it", () => {
  it("renewal re-takes it as a new Recovering generation, fences the lapsed Ready, and a fresh recovery completes", async () => {
    await run(
      Effect.gen(function* () {
        const ownerToken = randomUUID();
        const owner = yield* fixture(ownerToken);
        yield* owner.startup.complete(capture, replace(1));
        const before = yield* authority;
        yield* lapse;
        const renewal = yield* Effect.either(owner.renew);
        expect(superseded(renewal)).toBe(true);
        const after = yield* authority;
        expect(after.owner_token).toBe(ownerToken);
        expect(BigInt(after.generation)).toBe(BigInt(before.generation) + 1n);
        expect(after.state).toBe("recovering");
        expect(after.lease_live).toBe(true);
        expect(
          (yield* Effect.either(owner.runProducer(() => write(2))))._tag,
        ).toBe("Left");
        const recovery = yield* owner.beginRecovery("history lease lapsed");
        yield* recovery.complete(capture, replace(3));
        yield* owner.runProducer((token) =>
          Authority.withReady(token, write(4)),
        );
        yield* owner.renew;
        expect(yield* probe).toEqual([3, 4]);
      }),
    );
  });

  it("supersedes a recovery whose lease lapsed mid-work instead of failing it terminally, and never lets the lapsed handle run again", async () => {
    await run(
      Effect.gen(function* () {
        const ownerToken = randomUUID();
        const owner = yield* fixture(ownerToken);
        const before = yield* authority;
        yield* lapse;
        expect(
          superseded(yield* Effect.either(owner.startup.persist(write(1)))),
        ).toBe(true);
        expect(yield* probe).toEqual([]);
        const after = yield* authority;
        expect(after.owner_token).toBe(ownerToken);
        expect(BigInt(after.generation)).toBe(BigInt(before.generation) + 1n);
        expect(after.state).toBe("recovering");
        expect(after.lease_live).toBe(true);
        // The lapsed handle is superseded: one recovery at a time.
        expect(
          superseded(
            yield* Effect.either(owner.startup.complete(capture, replace(2))),
          ),
        ).toBe(true);
        const recovery = yield* owner.beginRecovery("history lease lapsed");
        yield* recovery.complete(capture, replace(3));
        expect(yield* probe).toEqual([3]);
      }),
    );
  });

  it("beginRecovery re-takes it instead of refusing", async () => {
    await run(
      Effect.gen(function* () {
        const owner = yield* fixture(randomUUID());
        yield* owner.startup.complete(capture, replace(1));
        yield* lapse;
        const recovery = yield* owner.beginRecovery("history lease lapsed");
        yield* recovery.complete(capture, replace(2));
        expect((yield* authority).state).toBe("ready");
        expect((yield* authority).lease_live).toBe(true);
        expect(yield* probe).toEqual([2]);
      }),
    );
  });

  it("supersedes a Ready append whose lease lapsed, committing nothing", async () => {
    await run(
      Effect.gen(function* () {
        const owner = yield* fixture(randomUUID());
        yield* owner.startup.complete(capture, replace(1));
        yield* lapse;
        expect(superseded(yield* Effect.either(owner.append(write(2))))).toBe(
          true,
        );
        expect(yield* probe).toEqual([1]);
        expect((yield* authority).state).toBe("recovering");
      }),
    );
  });

  it.each([
    ["live", false],
    ["lapsed in turn", true],
  ] as const)(
    "never takes back a lease another owner claimed after it lapsed, with the successor's lease %s",
    async (_, successorLapsed) => {
      await run(
        Effect.gen(function* () {
          const owner = yield* fixture(randomUUID());
          yield* owner.startup.complete(capture, replace(1));
          yield* lapse;
          const successor = randomUUID();
          yield* Authority.acquire({
            deploymentIdentity,
            ownerToken: successor,
            leaseDurationMs: 30_000,
          });
          if (successorLapsed) yield* lapse;
          const claimed = yield* authority;
          const renewal = yield* Effect.either(owner.renew);
          expect(Either.isLeft(renewal)).toBe(true);
          expect(superseded(renewal)).toBe(false);
          const recovery = yield* Effect.either(
            owner.beginRecovery("history lease lapsed"),
          );
          expect(Either.isLeft(recovery)).toBe(true);
          expect(superseded(recovery)).toBe(false);
          expect(yield* authority).toEqual(claimed);
          expect(claimed.owner_token).toBe(successor);
        }),
      );
    },
  );

  it("never re-takes a live lease when recovery work fails", async () => {
    await run(
      Effect.gen(function* () {
        const owner = yield* fixture(randomUUID());
        const before = yield* authority;
        const failed = yield* Effect.either(
          owner.startup.persist(Effect.fail("repair failed")),
        );
        expect(failed).toEqual(Either.left("repair failed"));
        expect(yield* authority).toEqual(before);
        yield* owner.startup.complete(capture, replace(1));
        expect(yield* probe).toEqual([1]);
      }),
    );
  });
});
