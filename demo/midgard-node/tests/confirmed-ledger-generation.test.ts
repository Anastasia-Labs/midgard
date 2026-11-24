/**
 * The confirmed-ledger generation on the node database (migration 0038): a
 * sequence owned by no table, drawn by every frontier write and by every
 * statement that writes `confirmed_ledger`, whatever writes it.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { Frontier } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import {
  E0,
  fixture,
  FRONTIER_HASH,
  run,
} from "./commit-base-resolver.fixture.js";
import { resetApplicationTables } from "./utils.js";

const generation = Frontier.retrieveWithGeneration.pipe(
  Effect.map((frontier) => BigInt(frontier!.generation)),
);

/** The sequence's last drawn value. */
const drawn = Effect.flatMap(SqlClient.SqlClient, (sql) =>
  sql<{ last_value: string }>`
    SELECT last_value FROM node_confirmed_ledger_generation`.pipe(
    Effect.map(([row]) => BigInt(row!.last_value)),
  ),
);

/** Runs `statement` on `confirmed_ledger` outside any fold. */
const direct = (
  statement: (sql: SqlClient.SqlClient) => Effect.Effect<unknown, unknown>,
) => withFollowerWrite(Effect.flatMap(SqlClient.SqlClient, statement));

describe("the confirmed-ledger generation", { concurrent: false }, () => {
  it("draws a new generation on every direct SQL write to confirmed_ledger", async () => {
    await fixture({ frontier: true, row: {} });
    const seen = [await run(generation)];
    for (const statement of [
      // Each leaves `confirmed_ledger` as it was: the generation names the
      // writes, not the content.
      (sql: SqlClient.SqlClient) =>
        sql`UPDATE confirmed_ledger SET output = output`,
      (sql: SqlClient.SqlClient) =>
        sql`DELETE FROM confirmed_ledger WHERE false`,
      (sql: SqlClient.SqlClient) =>
        sql`INSERT INTO confirmed_ledger (tx_id, outref, output, address)
          SELECT tx_id, outref, output, address FROM confirmed_ledger
          ON CONFLICT (outref) DO NOTHING`,
      (sql: SqlClient.SqlClient) =>
        Effect.zipRight(
          sql`CREATE TEMP TABLE kept ON COMMIT DROP AS SELECT * FROM confirmed_ledger`,
          Effect.zipRight(
            sql`TRUNCATE confirmed_ledger`,
            sql`INSERT INTO confirmed_ledger SELECT * FROM kept`,
          ),
        ),
    ]) {
      await run(direct(statement));
      seen.push(await run(generation));
      expect(seen.at(-1)! > seen.at(-2)!).toBe(true);
    }
  }, 120_000);

  it("keeps drawing upward across a reset of the application tables", async () => {
    await fixture({ frontier: true });
    const before = await run(generation);
    await run(resetApplicationTables);
    await fixture({ frontier: true });
    expect((await run(generation)) > before).toBe(true);
  }, 60_000);

  it("writes no frontier on a write to a ledger with none, and the frontier written next draws above every earlier generation", async () => {
    const { r0 } = await fixture({ frontier: false });
    const before = await run(drawn);
    await run(
      direct(
        (sql) => sql`DELETE FROM confirmed_ledger WHERE outref = ${E0.outref}`,
      ),
    );
    expect(await run(Frontier.retrieveWithGeneration)).toBeUndefined();
    await run(
      withFollowerWrite(
        Frontier.upsert({ headerHash: FRONTIER_HASH, utxosRoot: r0 }),
      ),
    );
    expect((await run(generation)) > before).toBe(true);
  }, 60_000);
});
