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
  _transactionDepthRef: Ref.Ref<number>;
  _rootsRef: Ref.Ref<Uint8Array[]>;
  _client: Database;
  _tableName: string;
  _referenceTableName: string | undefined;

  constructor(
    client: Database,
    tableName: string,
    referenceTableName?: string,
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
    this._referenceTableName = referenceTableName;
    this._transactionDepthRef = Ref.unsafeMake(0);
    this._rootsRef = Ref.unsafeMake<Uint8Array[]>([]);
  }

  openEffect = (copyFromReference?: true) => {
    const { _tableName, _referenceTableName, clear } = this;
    return Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`SET client_min_messages = 'error'`;
      yield* UtilsDB.mkKeyValueCreateQuery(_tableName);
      if (_referenceTableName && copyFromReference) {
        sql.withTransaction(
          Effect.gen(function* () {
            yield* UtilsDB.clearTable(_tableName);
            yield* sql`INSERT INTO ${sql(_tableName)} SELECT * FROM ${sql(_referenceTableName)}`;
          }),
        );
      } else {
        yield* clear();
      }
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
      return rows[0]?.value;
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

  getAllFromReferenceEffect = () => {
    const { _referenceTableName } = this;
    return Effect.gen(function* () {
      if (!_referenceTableName) {
        throw new Error("No reference tables set");
      }
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        key: Uint8Array;
        value: Uint8Array;
      }>`SELECT * FROM ${sql(_referenceTableName)}`;
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

  clearReference = () => {
    const { _tableName, _referenceTableName } = this;
    return Effect.gen(function* () {
      if (!_referenceTableName) {
        throw new Error("No reference tables set");
      }
      yield* UtilsDB.clearTable(_tableName);
    });
  };

  transferToReference = () => {
    const { _tableName, _referenceTableName, clearReference } = this;
    return Effect.gen(function* () {
      if (!_referenceTableName) {
        throw new Error("No reference tables set");
      }
      const sql = yield* SqlClient.SqlClient;
      const tableName = _tableName;
      const refTableName = _referenceTableName;
      yield* sql.withTransaction(
        Effect.gen(function* () {
          yield* clearReference();
          yield* sql`INSERT INTO ${sql(refTableName)} SELECT * FROM ${sql(tableName)}`;
        }),
      );
    });
  };

  async batch(opStack: BatchDBOp[]): Promise<void> {
    const { putEffect, delEffect } = this;
    return Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql.withTransaction(
        Effect.gen(function* () {
          for (const op of opStack) {
            if (op.type === "put") {
              yield* putEffect(op.key, op.value);
            } else {
              yield* delEffect(op.key);
            }
          }
        }),
      );
    }).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.scoped,
      Effect.runPromise,
    );
  }

  checkpoint(root: Uint8Array): void {
    super.checkpoint(root);
    const { _transactionDepthRef, _rootsRef } = this;
    Effect.runFork(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const depth = yield* Ref.get(_transactionDepthRef);
        // First checkpoint = start transaction
        if (depth === 0) {
          yield* sql`BEGIN`;
        }
        // Nested checkpoint = create savepoint
        else {
          yield* sql`SAVEPOINT root_${bytesToHex(root)}`;
        }
        yield* Ref.update(_transactionDepthRef, (n) => n + 1);
        yield* Ref.update(_rootsRef, (roots) => [...roots, root]);
      }).pipe(Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client))),
    );
  }

  async commit(): Promise<void> {
    const { _transactionDepthRef, _rootsRef } = this;
    const superCommit = super.commit;
    await Effect.runPromise(
      Effect.gen(function* () {
        const roots = yield* Ref.get(_rootsRef);
        if (roots.length === 0) return Effect.void;
        const root = yield* rootsPop(_rootsRef);
        superCommit();
        const sql = yield* SqlClient.SqlClient;
        yield* Ref.update(_transactionDepthRef, (n) => n - 1);
        const depth = yield* Ref.get(_transactionDepthRef);
        if (depth === 0) yield* sql`COMMIT`;
        else yield* sql`RELEASE SAVEPOINT root_${bytesToHex(root)}`;
        return Effect.void;
      }).pipe(Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client))),
    );
  }

  async revert(): Promise<Uint8Array> {
    const { _rootsRef, _transactionDepthRef } = this;
    const superRevert = super.revert;
    return await Effect.runPromise(
      Effect.gen(function* () {
        const roots = yield* Ref.get(_rootsRef);
        if (roots.length === 0) {
          throw new Error("No checkpoints to revert");
        }
        const root = yield* rootsPop(_rootsRef);
        superRevert();
        const sql = yield* SqlClient.SqlClient;
        const depth = yield* Ref.updateAndGet(
          _transactionDepthRef,
          (n) => n - 1,
        );

        // Full rollback if root checkpoint
        if (depth === 0) yield* sql`ROLLBACK`;
        else yield* sql`ROLLBACK TO SAVEPOINT root_${bytesToHex(root)}`;
        return root;
      }).pipe(Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client))),
    );
  }

  async conclude() {
    await Effect.runPromise(
      this.transferToReference().pipe(
        Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      ),
    );
  }

  shallowCopy(): PostgresCheckpointDB {
    return new PostgresCheckpointDB(
      this._client,
      this._tableName,
      this._referenceTableName,
      { cacheSize: this.cache.max },
    );
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

const rootsPop = (rootsRef: Ref.Ref<Uint8Array[]>) =>
  Ref.modify(rootsRef, (roots) => {
    const newRoots = roots.slice(0, -1);
    const last = roots[roots.length - 1];
    return [last, newRoots];
  });
