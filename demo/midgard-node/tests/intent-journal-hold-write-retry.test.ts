/**
 * The intent journal's refusal-hold write (`refusalHoldsOver().raise`), by
 * failure class: a write the database fails transiently (a connection-class
 * failure) is retried on `HOLD_WRITE_RETRIES` and lands once the database
 * answers; any other failure is not retried, and the hold stays named from
 * memory until the next record or refresh writes it.
 */
import type { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { refusalHoldsOver } from "../src/services/intent-journal.holds.js";
import { INTENT_CONTENT_REF_MISSING } from "../src/services/intent-journal.js";

const refused = () =>
  Object.assign(new Error("connect ECONNREFUSED 127.0.0.1:5433"), {
    code: "ECONNREFUSED",
  });
const broken = () =>
  Object.assign(new Error('relation "intent_refusal_holds" does not exist'), {
    code: "42P01",
  });

/**
 * A database whose first `failures` statements fail with `fail()`; the
 * guard ends a write that would otherwise be retried without bound.
 */
const database = (fail: () => Error, failures: number) => {
  let statements = 0;
  const sql = (() =>
    Effect.suspend(() => {
      statements += 1;
      if (statements > 50) throw new Error("retried without bound");
      return statements <= failures ? Effect.fail(fail()) : Effect.succeed([]);
    })) as unknown as SqlClient.SqlClient;
  return { sql, statements: () => statements };
};

const hold = { reason: INTENT_CONTENT_REF_MISSING, detail: "refused" };
const raise = (sql: SqlClient.SqlClient) => {
  const holds = refusalHoldsOver(sql);
  return Effect.runPromise(
    holds.raise("commit", hold, "ab".repeat(32), "00").pipe(Effect.as(holds)),
  );
};

describe("the intent journal's hold write, by failure class", () => {
  it("retries a transient failure and lands the write", async () => {
    const db = database(refused, 2);
    const holds = await raise(db.sql);
    expect(db.statements()).toBe(3);
    // Written: nothing is left to hand off.
    expect(holds.handOff()).toEqual([]);
  });

  it("does not retry a failure that is not transient", async () => {
    const db = database(broken, 50);
    const holds = await raise(db.sql);
    expect(db.statements()).toBe(1);
    expect(holds.holds().map(({ reason }) => reason)).toEqual([
      INTENT_CONTENT_REF_MISSING,
    ]);
    expect(holds.handOff()).toHaveLength(1);
  });

  it("gives up on a transient failure after its retries", async () => {
    const db = database(refused, 50);
    await raise(db.sql);
    expect(db.statements()).toBe(4);
  });
});
