import { SqlClient } from "@effect/sql";
import { BatchDBOp, bytesToHex, DB } from "@ethereumjs/util";
import { Effect, Layer, Ref } from "effect";
import { CheckpointDB } from "@ethereumjs/mpt";
import { LRUCache } from "lru-cache";
import { UtilsDB } from "@/database/index.js";
import { Database } from "@/services/database.js";

export class PostgresCheckpointDB
  extends CheckpointDB
  implements DB<Uint8Array, Uint8Array>
{
  cache: LRUCache<string, Uint8Array>;
  _client: Database;
  _tableName: string;

  constructor(
    client: Database,
    tableName: string,
    options: { cacheSize?: number } = {},
  ) {
    super({
      db: {
        get: async (key: string) => this.get(Buffer.from(key, "hex")),
        put: async (key: string, value: Uint8Array) =>
          this.put(Buffer.from(key, "hex"), value),
        del: async (key: string) => this.del(Buffer.from(key, "hex")),
        batch: async (ops) => this.batch(convertOps(ops)),
      } as DB<string, Uint8Array>,
    });
    // TODO: tune max cache size value
    this.cache = new LRUCache({ max: options.cacheSize ?? 100 });
    this._client = client;
    this._tableName = tableName;
  }

  openEffect = () => {
    const { _tableName } = this;
    return Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`SET client_min_messages = 'error'`;
      yield* UtilsDB.mkKeyValueCreateQuery(_tableName);
    });
  };

  async open() {
    return await this.openEffect().pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.runPromise,
    );
  }

  getEffect = (key: Uint8Array) => {
    const { cache, checkpoints, _tableName } = this;
    return Effect.gen(function* () {
      const keyHex = bytesToHex(key);
      if (cache.has(keyHex)) return cache.get(keyHex);

      for (let i = checkpoints.length - 1; i >= 0; i--) {
        const value = checkpoints[i].keyValueMap.get(keyHex);
        if (value !== undefined) return value;
      }
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ value: Uint8Array }>`
        SELECT value FROM ${sql(_tableName)}
        WHERE key = ${Buffer.from(key)}`;
      const value = rows[0]?.value;
      if (value) {
        cache.set(keyHex, value);
      }
      return value;
    });
  };

  async get(key: Uint8Array): Promise<Uint8Array | undefined> {
    return await this.getEffect(key).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.runPromise,
    );
  }

  getAllEffect = () => {
    const { _tableName } = this;
    return Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ key: Uint8Array; value: Uint8Array }>`
        SELECT * FROM ${sql(_tableName)}`;
      return rows.map((row) => ({
        key: Buffer.from(row.key),
        value: Buffer.from(row.value),
      }));
    });
  };

  putEffect = (key: Uint8Array, value: Uint8Array) => {
    const { cache, checkpoints, _tableName } = this;
    return Effect.gen(function* () {
      const keyHex = bytesToHex(key);
      cache.set(keyHex, value);

      if (checkpoints.length > 0) {
        checkpoints[checkpoints.length - 1].keyValueMap.set(keyHex, value);
      } else {
        const sql = yield* SqlClient.SqlClient;
        const rowsToInsert = { key: key, value: value };
        yield* sql`
          INSERT INTO ${sql(_tableName)} ${sql.insert(rowsToInsert)}
          ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value
          `;
      }
    });
  };

  async put(key: Uint8Array, value: Uint8Array): Promise<void> {
    await this.putEffect(key, value).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.runPromise,
    );
  }

  delEffect = (key: Uint8Array) => {
    const { cache, checkpoints, _tableName } = this;
    return Effect.gen(function* () {
      const keyHex = bytesToHex(key);
      cache.set(keyHex, undefined);
      if (checkpoints.length > 0) {
        checkpoints[checkpoints.length - 1].keyValueMap.set(keyHex, undefined);
      } else {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM ${sql(_tableName)} WHERE key = ${key}`;
      }
    });
  };

  async del(key: Uint8Array): Promise<void> {
    await this.delEffect(key).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.runPromise,
    );
  }

  clear = () => {
    const { _tableName } = this;
    return Effect.gen(function* () {
      yield* UtilsDB.clearTable(_tableName);
    });
  };

  async batch(opStack: BatchDBOp[]): Promise<void> {
    const { putEffect, delEffect } = this;
    return Effect.gen(function* () {
      for (const op of opStack) {
        if (op.type === "put") {
          yield* putEffect(op.key, op.value);
        } else {
          yield* delEffect(op.key);
        }
      }
    }).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.runPromise,
    );
  }

  checkpoint(root: Uint8Array): void {
    super.checkpoint(root);
    const savepointName = `mpt_savepoint_${this.checkpoints.length}`;
    Effect.runFork(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`SAVEPOINT ${sql(savepointName)}`;
      }).pipe(Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client))),
    );
  }

  async commit(): Promise<void> {
    if (this.checkpoints.length === 0) {
      return;
    }
    const savepointName = `mpt_savepoint_${this.checkpoints.length}`;
    await super.commit();
    await Effect.runPromise(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`RELEASE SAVEPOINT ${sql(savepointName)}`;
      }).pipe(Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client))),
    );
  }

  async revert(): Promise<Uint8Array> {
    if (this.checkpoints.length === 0) return super.revert();
    const savepointName = `mpt_savepoint_${this.checkpoints.length}`;
    await Effect.runPromise(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`ROLLBACK TO SAVEPOINT ${sql(savepointName)}`;
      }).pipe(Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client))),
    );
    const newRoot = await super.revert();
    return newRoot;
  }
}

const convertOps = (
  ops: BatchDBOp<string, Uint8Array>[],
): BatchDBOp<Uint8Array, Uint8Array>[] => {
  return ops.map((op) => {
    const base = {
      opts: {
        ...op.opts,
      },
    };
    return op.type === "put"
      ? {
          type: "put",
          key: Buffer.from(op.key, "hex"),
          value: op.value,
          ...base,
        }
      : {
          type: "del",
          key: Buffer.from(op.key, "hex"),
          ...base,
        };
  });
};

// const rootsPop = (rootsRef: Ref.Ref<Uint8Array[]>) =>
//   Ref.modify(rootsRef, (roots) => {
//     const newRoots = roots.slice(0, -1);
//     const last = roots[roots.length - 1];
//     return [last, newRoots];
//   });
