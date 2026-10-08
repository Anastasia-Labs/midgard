/**
 * The L1 follower's schema in the node database (plan §5.4, N1). The node's
 * event rows bind to the follower's admission identity (`l1_event_keys`), and
 * the node's own SQL joins it, so the follower's tables are part of the
 * node's schema: `migrate` installs them, in its transaction, with the
 * follower's own migration ledger. The follower store applies the same sets
 * again when it starts, which is a no-op.
 */
import {
  applyMigrations,
  followerMigrations,
  type MigrationSet,
  postgresDialect,
  type SqlBackend,
  type SqlRow,
  type SqlTx,
  type SqlValue,
} from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect, Runtime } from "effect";

import { eventMigrations } from "../l1-events/schema.js";
import { splitSqlStatements } from "./migrations/runner.split-sql-statements.js";

/** The follower's own sets plus the node event projection's. */
export const NODE_FOLLOWER_SCHEMA: readonly MigrationSet[] = [
  followerMigrations("postgres"),
  eventMigrations("postgres"),
];

export const numbered = (text: string): string => {
  let index = 0;
  return text.replace(/\?/gu, () => `$${String((index += 1))}`);
};

/**
 * The follower's SQL interface over the node's connection: inside a
 * `withTransaction`, every statement runs on its transaction connection.
 */
export const followerSqlTx = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const run = Runtime.runPromise(yield* Effect.runtime<SqlClient.SqlClient>());
  const tx: SqlTx = {
    query: (text, params: readonly SqlValue[] = []) =>
      run(sql.unsafe<SqlRow>(numbered(text), params as never)).then((rows) => [
        ...rows,
      ]),
    exec: async (text) => {
      for (const statement of splitSqlStatements(text))
        await run(sql.unsafe(statement));
    },
  };
  return tx;
});

/**
 * Applies the follower schema in the caller's SQL transaction: the follower's
 * migration runner, over the node's open transaction connection.
 */
export const installFollowerSchema = Effect.gen(function* () {
  const tx = yield* followerSqlTx;
  const backend: SqlBackend = {
    dialect: postgresDialect,
    transaction: (_mode, work) => work(tx),
    acquireWriterLease: () =>
      Promise.reject(new Error("the schema install takes no writer lease")),
    close: () => Promise.resolve(),
  };
  return yield* Effect.tryPromise({
    try: () => applyMigrations(backend, NODE_FOLLOWER_SCHEMA),
    catch: (cause) => cause,
  });
});
