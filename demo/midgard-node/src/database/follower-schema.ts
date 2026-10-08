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
  intentMigrations,
  type MigrationSet,
  postgresDialect,
  type SqlBackend,
  type SqlRow,
  type SqlTx,
  type SqlValue,
  type View,
  viewValidIn,
} from "@al-ft/midgard-l1-follower";
import { eventMigrations } from "@al-ft/midgard-l1-follower/events";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect, Runtime } from "effect";

import { forcedOrderMigrations } from "../forced-orders/schema.js";
import { splitSqlStatements } from "./migrations/runner.split-sql-statements.js";

/**
 * The follower's own sets plus the node event, forced-order and intent
 * journal (§8.2, I1) projections'.
 */
export const NODE_FOLLOWER_SCHEMA: readonly MigrationSet[] = [
  followerMigrations("postgres"),
  eventMigrations("postgres"),
  forcedOrderMigrations("postgres"),
  intentMigrations("postgres"),
];

export const numbered = (text: string): string => {
  let index = 0;
  return text.replace(/\?/gu, () => `$${String((index += 1))}`);
};

/**
 * A `bytea[]` parameter as a Postgres array literal. The node's client
 * types a JS array by its first element, so a `Buffer[]` would reach the
 * server as one `bytea`; the literal goes untyped and the server types it
 * from the column or operator it meets.
 */
export const byteaArrayLiteral = (items: readonly Buffer[]): string =>
  `{${items.map((item) => `"\\\\x${item.toString("hex")}"`).join(",")}}`;

const bindParam = (value: SqlValue): unknown =>
  Array.isArray(value) ? byteaArrayLiteral(value) : value;

/**
 * The follower's SQL interface over the node's connection: inside a
 * `withTransaction`, every statement runs on its transaction connection.
 */
export const followerSqlTx = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const run = Runtime.runPromise(yield* Effect.runtime<SqlClient.SqlClient>());
  const tx: SqlTx = {
    query: (text, params: readonly SqlValue[] = []) =>
      run(
        sql.unsafe<SqlRow>(numbered(text), params.map(bindParam) as never),
      ).then((rows) => [...rows]),
    exec: async (text) => {
      for (const statement of splitSqlStatements(text))
        await run(sql.unsafe(statement));
    },
  };
  return tx;
});

/**
 * `viewValid(view)` (§8.1) in the caller's SQL transaction, through the
 * follower's `viewValidIn`: on Postgres it takes the cursor row `FOR SHARE`,
 * so no rewind commits between the check and the write it guards until that
 * transaction ends. The node's one view check.
 */
export const followerViewValid = (
  view: View,
): Effect.Effect<boolean, SqlError.SqlError, SqlClient.SqlClient> =>
  Effect.flatMap(followerSqlTx, (tx) =>
    Effect.tryPromise({
      try: () => viewValidIn(tx, postgresDialect, view),
      catch: (cause) =>
        new SqlError.SqlError({
          cause,
          message: "The follower view check failed",
        }),
    }),
  );

/**
 * Runs `work` over the follower's facts on one snapshot: a `REPEATABLE READ,
 * READ ONLY` transaction of the node database, so every statement of `work`
 * reads one chain state, however many blocks the follower applies meanwhile,
 * and none can write. It opens its own transaction: inside a caller's, the
 * isolation level can no longer be set and the transaction fails.
 */
export const inFollowerSnapshot = <A>(
  work: (tx: SqlTx) => Promise<A>,
): Effect.Effect<A, unknown, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY`;
        const tx = yield* followerSqlTx;
        return yield* Effect.tryPromise({
          try: () => work(tx),
          catch: (cause) => cause,
        });
      }),
    );
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
