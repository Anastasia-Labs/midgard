import { NodeConfig, User } from "@/config.js";
import { fromHex } from "@lucid-evolution/lucid";
import {
  Effect,
  pipe,
} from "effect";
import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { PostgresCheckpointDB } from "@/workers/db.js";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { InitDB, MempoolDB } from "@/database/index.js";

const playgroundFork =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* InitDB.initializeDb().pipe(Effect.provide(Database.layer));
    const db = new PostgresCheckpointDB(sql, MempoolDB.tableName);
    yield* db.openEffect();
    const trie = yield* Effect.tryPromise({
      try: () =>
        ETH.createMPT({
          db,
          useRootPersistence: true,
          valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
        }),
      catch: (e) => new Error(`${e}`),
    });
    yield* Effect.tryPromise({
      try: () => trie.put(fromHex("00"), fromHex("c0ffee")),
      catch: (e) => new Error(`${e}`),
    });
    const allRows = yield* db.getAllEffect()
    yield* Effect.logInfo(allRows)
  });

const program = pipe(
  playgroundFork,
  Effect.provide(Database.layer),
  Effect.provide(User.layer),
  Effect.provide(NodeConfig.layer),
);

export const runPlayground = Effect.gen(function* () {
  pipe(
    program,
    Effect.withSpan("midgard"),
    Effect.catchAllCause(Effect.logError),
    Effect.runPromise,
  );
});
