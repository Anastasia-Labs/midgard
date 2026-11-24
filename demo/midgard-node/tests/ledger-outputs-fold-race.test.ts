/**
 * The ledger store's output reader against a fold that commits while a read
 * is under way. A fold moves a landed block's outputs into
 * `confirmed_ledger` and deletes its landed row in one transaction, so a
 * read that looked in `confirmed_ledger` before the fold and in the landed
 * rows after it would find the output in neither.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { foldMerge } from "../src/landed-blocks/confirmed-merges.js";
import { readLedgerOutputs } from "../src/landed-blocks/ledger-outputs.js";
import { Frontier, retrieveRows } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { E1, fixture, run } from "./commit-base-resolver.fixture.js";
import { resetApplicationTables } from "./utils.js";

beforeEach(async () => {
  await run(resetApplicationTables);
});

/** Waits until another connection waits for a lock on `node_landed_blocks`
 * in this database. */
const untilAReadWaits = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  for (let attempt = 0; attempt < 1_000; attempt += 1) {
    const [{ waiting }] = yield* sql<{ waiting: number }>`
      SELECT count(*)::int AS waiting
      FROM pg_locks l
      JOIN pg_class c ON c.oid = l.relation
      JOIN pg_database d ON d.oid = l.database
      WHERE c.relname = 'node_landed_blocks'
        AND d.datname = current_database()
        AND NOT l.granted`;
    if (waiting > 0) return;
    yield* Effect.sleep("10 millis");
  }
  return yield* Effect.die("no read waited on node_landed_blocks");
});

describe("ledger outputs read during a fold", { concurrent: false }, () => {
  it("answers an output the fold moves from a landed row into confirmed_ledger mid-read", async () => {
    const { headerHash } = await fixture({ frontier: true, row: {} });
    const produced = E1.outref.toString("hex");
    let read: Promise<(Buffer | undefined)[]> | undefined;
    await run(
      withFollowerWrite(
        Effect.gen(function* () {
          const [row] = yield* retrieveRows;
          const sql = yield* SqlClient.SqlClient;
          yield* sql.withTransaction(
            Effect.gen(function* () {
              // The fold holds the landed rows until the read is waiting on
              // them, then commits: the read's look at the landed rows
              // comes after the fold.
              yield* sql`LOCK TABLE node_landed_blocks IN ACCESS EXCLUSIVE MODE`;
              read = run(readLedgerOutputs([produced]));
              yield* untilAReadWaits;
              yield* foldMerge(row!, null);
            }),
          );
        }),
      ),
    );
    expect((await run(Frontier.retrieve))?.headerHash).toBe(headerHash);
    expect((await read!).map((output) => output?.toString("hex"))).toEqual([
      E1.output.toString("hex"),
    ]);
  });
});
