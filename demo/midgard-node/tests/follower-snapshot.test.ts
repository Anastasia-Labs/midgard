/**
 * `inFollowerSnapshot` over the node database: every statement of the work
 * reads one snapshot, a commit from another connection in between included,
 * and none can write. The control, a plain node transaction (READ COMMITTED),
 * reads that commit, which is the half-read the snapshot rules out.
 */
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Redacted } from "effect";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  followerSqlTx,
  inFollowerSnapshot,
} from "../src/database/follower-schema.js";
import { testDatabases } from "./helpers/l1-events-store.js";

const databases = testDatabases();
let connectionString: string;

const run = <A>(effect: Effect.Effect<A, unknown, SqlClient.SqlClient>) =>
  Effect.runPromise(
    Effect.provide(
      effect,
      PgClient.layer({ url: Redacted.make(connectionString) }),
    ),
  );

/** A commit from a connection of its own. */
const insertElsewhere = (value: number) =>
  run(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql`INSERT INTO snapshot_probe (value) VALUES (${value})`,
    ),
  );

const countIn = async (tx: {
  query: (text: string) => Promise<readonly Record<string, unknown>[]>;
}): Promise<number> =>
  Number((await tx.query("SELECT count(*) AS n FROM snapshot_probe"))[0]!.n);

beforeAll(async () => {
  connectionString = await databases.create();
  await run(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql`CREATE TABLE snapshot_probe (value integer NOT NULL)`,
    ),
  );
  await insertElsewhere(1);
}, 60_000);
afterAll(async () => {
  await databases.dropAll();
});

describe("inFollowerSnapshot (postgres)", () => {
  it("reads one snapshot across a commit from another connection", async () => {
    const counts = await run(
      inFollowerSnapshot(async (tx) => {
        const before = await countIn(tx);
        await insertElsewhere(2);
        return [before, await countIn(tx)];
      }),
    );
    expect(counts[1]).toBe(counts[0]);
    // The commit landed; a later read sees it.
    const after = await run(inFollowerSnapshot((tx) => countIn(tx)));
    expect(after).toBe(counts[0]! + 1);
  });

  it("control: a plain node transaction reads the commit midway", async () => {
    const counts = await run(
      Effect.flatMap(SqlClient.SqlClient, (sql) =>
        sql.withTransaction(
          Effect.flatMap(followerSqlTx, (tx) =>
            Effect.promise(async () => {
              const before = await countIn(tx);
              await insertElsewhere(3);
              return [before, await countIn(tx)];
            }),
          ),
        ),
      ),
    );
    expect(counts[1]).toBe(counts[0]! + 1);
  });

  it("refuses a write", async () => {
    const refused = await run(
      Effect.either(
        inFollowerSnapshot((tx) =>
          tx.exec("INSERT INTO snapshot_probe (value) VALUES (4)"),
        ),
      ),
    );
    expect(refused._tag).toBe("Left");
    expect(String((refused as { left: unknown }).left)).toMatch(/read-only/u);
  });

  it("fails inside a caller's transaction, where the isolation level is already set", async () => {
    const nested = await run(
      Effect.flatMap(SqlClient.SqlClient, (sql) =>
        Effect.either(
          sql.withTransaction(
            Effect.zipRight(
              sql`SELECT 1`,
              inFollowerSnapshot((tx) => countIn(tx)),
            ),
          ),
        ),
      ),
    );
    expect(nested._tag).toBe("Left");
  });
});
