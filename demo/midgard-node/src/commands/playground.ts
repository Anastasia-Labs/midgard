import { NodeConfig, User } from "@/config.js";
import { fromHex, toHex } from "@lucid-evolution/lucid";
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
          db: db.db,
          useRootPersistence: true,
          valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
        }),
      catch: (e) => new Error(`${e}`),
    });
    yield* Effect.tryPromise({
      try: () => trie.put(fromHex("00"), fromHex("c0ffee")),
      catch: (e) => new Error(`${e}`),
    });
    yield* Effect.tryPromise({
      try: () => trie.put(fromHex("01"), fromHex("c0ffef")),
      catch: (e) => new Error(`${e}`),
    });
    trie.checkpoint();
    const allRowsBeforeEffects = yield* db.getAllEffect()
    yield* Effect.logInfo(
      allRowsBeforeEffects.map(
        ({key, value}) => ({key: toHex(key), value: toHex(value)})
      )
    );
    yield* Effect.all([
      Effect.logInfo("PUTTING 02"),
      Effect.tryPromise({
        try: () => trie.put(fromHex("02"), fromHex("c0fff0")),
        catch: (e) => new Error(`${e}`),
      }),
      Effect.logInfo("PUTTING 03"),
      Effect.tryPromise({
        try: () => trie.put(fromHex("03"), fromHex("c0fff1")),
        catch: (e) => new Error(`${e}`),
      }),
      Effect.logInfo("DELETING 04"),
      Effect.tryPromise({
        try: () => trie.del(fromHex("04")),
        catch: (e) => new Error(`${e}`),
      }),
      Effect.logInfo("COMMITTING..."),
      Effect.tryPromise({
        try: trie.commit,
        catch: (e) => new Error(`${e}`),
      }),
    ], { concurrency: 1 }).pipe(
      Effect.catchAll(
        (_e) => Effect.gen(function* () {
          yield* Effect.logInfo("REVERTING...");
          yield* Effect.logInfo(trie);
          // yield* Effect.tryPromise({
          //   try: trie.revert,
          //   catch: (e) => new Error(`${e}`)
          // });
        }),
      )
    );
    const val0 = yield* Effect.tryPromise({
      try: () => trie.get(fromHex("00")),
      catch: (e) => new Error(`${e}`),
    });
    const allRows = yield* db.getAllEffect()
    if (val0) {
      yield* Effect.logInfo(toHex(val0));
    } else {
      yield* Effect.logInfo("VALUE OF KEY \"00\" NOT FOUND.");
    }
    yield* Effect.logInfo(
      allRows.map(
        ({key, value}) => ({key: toHex(key), value: toHex(value)})
      )
    );
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
