import { SqlClient } from "@effect/sql";
import { BatchDBOp, bytesToHex, DB } from "@ethereumjs/util";
import { Effect, Layer } from "effect";
import { CheckpointDB } from "@ethereumjs/mpt";
import { LRUCache } from "lru-cache";
import { UtilsDB } from "@/database/index.js";
import { Database } from "@/services/database.js";
import { findSpentAndProducedUTxOs } from "@/utils.js";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { toHex } from "@lucid-evolution/lucid";

// Key of the row which its value is the persisted trie root.
const rootKey = ETH.ROOT_DB_KEY;

export const makeMpts = () =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    Effect.logDebug("🔹 Creating ledger and mempool tries ...");
    // Since there is no way to create an MPT from txs that are already in
    // the mempool database, we have two options here:
    // 1. We could add txs to the MPT directly from the submit endpoint.
    //    However, this would require sharing the MPT object between the server and
    //    worker threads, which is not trivial. Also, exposing trie database details
    //    to the submit endpoint does not seem like good coding practice.
    // 2. We could use an in-memory database. Since only transaction CBORs are stored
    //    in the mempool, this should not add any overhead compared to inserting
    //    each tx twice (in the mempool and in the tree itself). So it shouldn't
    //    become a bottleneck. If it does, we could reconsider the first option.
    const mempoolTrie = yield* Effect.tryPromise({
      try: () => ETH.createMPT({}),
      catch: (e) => new Error(`${e}`),
    });

    // Ledger MPT from the other side should use a checkpoint database —
    // its MPT building operations are paired with database ones
    const ledgerCheckpointDB = new PostgresCheckpointDB(sql, "latest_ledger");
    yield* ledgerCheckpointDB.openEffect();
    const ledgerTrie = yield* Effect.tryPromise({
      try: () =>
        ETH.createMPT({
          db: ledgerCheckpointDB.db,
          useRootPersistence: true,
          valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
        }),
      catch: (e) => new Error(`${e}`),
    });
    const ledgerRootBeforeMempoolTxs = yield* Effect.tryPromise({
      try: () => ledgerTrie.get(rootKey),
      catch: (e) => new Error(`${e}`),
    }).pipe(Effect.orElse(() => Effect.succeed(ledgerTrie.EMPTY_TRIE_ROOT)));
    // Ensuring persisted root is stored in trie's private property
    yield* Effect.sync(() => ledgerTrie.root(ledgerRootBeforeMempoolTxs));
    return {
      ledgerTrie: ledgerTrie,
      mempoolTrie: mempoolTrie,
    };
  });

// Make mempool trie, and fill it with ledger trie with processed mempool txs
export const processMpts = (
  ledgerTrie: ETH.MerklePatriciaTrie,
  mempoolTrie: ETH.MerklePatriciaTrie,
  mempoolTxs: { key: Uint8Array; value: Uint8Array }[],
) =>
  Effect.gen(function* () {
    const mempoolTxHashes: Uint8Array[] = [];
    const batchDBOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfBlocksTxs = 0;
    yield* Effect.logInfo("🔹 Going through mempool txs and finding roots...");
    yield* Effect.forEach(mempoolTxs, ({ key: txHash, value: txCbor }) =>
      Effect.gen(function* () {
        mempoolTxHashes.push(txHash);
        sizeOfBlocksTxs += txCbor.length;
        yield* Effect.tryPromise({
          try: () => mempoolTrie.put(txHash, txCbor),
          catch: (e) => new Error(`${e}`),
        });
        const { spent, produced } = yield* findSpentAndProducedUTxOs(
          txCbor,
        ).pipe(Effect.withSpan("findSpentAndProducedUTxOs"));
        const delOps: ETH_UTILS.BatchDBOp[] = spent.map((outRef) => ({
          type: "del",
          key: outRef,
        }));
        const putOps: ETH_UTILS.BatchDBOp[] = produced.map(
          ({ key: outputReference, value: output }) => ({
            type: "put",
            key: outputReference,
            value: output,
          }),
        );
        yield* Effect.sync(() => batchDBOps.push(...[...delOps, ...putOps]));
      }),
    );

    yield* Effect.tryPromise({
      try: () => ledgerTrie.batch(batchDBOps),
      catch: (e) => new Error(`${e}`),
    });

    const utxoRoot = toHex(ledgerTrie.root());
    const txRoot = toHex(mempoolTrie.root());

    yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);
    yield* Effect.logInfo(`🔹 New transaction root found: ${txRoot}`);

    return {
      utxoRoot: utxoRoot,
      txRoot: txRoot,
      mempoolTxHashes: mempoolTxHashes,
      sizeOfBlocksTxs: sizeOfBlocksTxs,
    };
  });

export const withTrieTransaction = (
  trie: ETH.MerklePatriciaTrie,
  eff: Effect.Effect<any, any, any>,
) =>
  Effect.gen(function* () {
    yield* Effect.sync(() => trie.checkpoint());
    const sql = yield* SqlClient.SqlClient;
    const res = yield* sql.withTransaction(eff);
    yield* Effect.sync(() => trie.commit());
    return res;
  }).pipe(
    Effect.catchAll((e) =>
      Effect.gen(function* () {
        yield* Effect.tryPromise(() => trie.revert());
        yield* Effect.fail(e);
      }),
    ),
  );

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
      if (value) yield* Effect.sync(() => cache.set(keyHex, value));
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
      yield* Effect.sync(() => cache.set(keyHex, value));
    });
  };

  async put(key: Uint8Array, value: Uint8Array): Promise<void> {
    await this.putEffect(key, value).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.runPromise,
    );
  }

  putMultipleEffect = (ops: ETH_UTILS.PutBatch<Uint8Array, Uint8Array>[]) => {
    const { cache, checkpoints, _tableName } = this;
    return Effect.gen(function* () {
      if (ops.length === 0) return;
      if (checkpoints.length > 0) {
        const keyValueMap = checkpoints[checkpoints.length - 1].keyValueMap;
        for (const { key, value } of ops) {
          const keyHex = bytesToHex(key);
          keyValueMap.set(keyHex, value);
        }
      } else {
        const sql = yield* SqlClient.SqlClient;
        const rowsToInsert = ops.map(({ key, value }) => ({ key, value }));
        yield* sql`
        INSERT INTO ${sql(_tableName)} ${sql.insert(rowsToInsert)}
        ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value
      `;
      }
      yield* Effect.sync(() => {
        for (const { key, value } of ops) {
          const keyHex = bytesToHex(key);
          cache.set(keyHex, value);
        }
      });
    });
  };

  delEffect = (key: Uint8Array) => {
    const { cache, checkpoints, _tableName } = this;
    return Effect.gen(function* () {
      const keyHex = bytesToHex(key);
      if (checkpoints.length > 0) {
        checkpoints[checkpoints.length - 1].keyValueMap.set(keyHex, undefined);
      } else {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM ${sql(_tableName)} WHERE key = ${key}`;
      }
      yield* Effect.sync(() => cache.set(keyHex, undefined));
    });
  };

  async del(key: Uint8Array): Promise<void> {
    await this.delEffect(key).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.runPromise,
    );
  }

  delMultipleEffect = (ops: ETH_UTILS.DelBatch<Uint8Array>[]) => {
    const { cache, checkpoints, _tableName } = this;
    return Effect.gen(function* () {
      if (ops.length === 0) return;
      if (checkpoints.length > 0) {
        const keyValueMap = checkpoints[checkpoints.length - 1].keyValueMap;
        for (const { key } of ops) {
          const keyHex = bytesToHex(key);
          keyValueMap.set(keyHex, undefined);
        }
      } else {
        const sql = yield* SqlClient.SqlClient;
        const keys = ops.map((op) => op.key);
        yield* sql`
        DELETE FROM ${sql(_tableName)} WHERE key IN (${sql.in(keys)})
      `;
      }
      yield* Effect.sync(() => {
        for (const { key } of ops) {
          const keyHex = bytesToHex(key);
          cache.set(keyHex, undefined);
        }
      });
    });
  };

  clear = () => {
    const { _tableName } = this;
    return Effect.gen(function* () {
      yield* UtilsDB.clearTable(_tableName);
    });
  };

  async batch(opStack: BatchDBOp[]): Promise<void> {
    const { putMultipleEffect, delMultipleEffect } = this;
    const splittedOps = splitBatchOps(opStack);
    return Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql.withTransaction(
        Effect.gen(function* () {
          for (const ops of splittedOps) {
            if (isPutBatch(ops)) {
              yield* putMultipleEffect(ops);
            } else {
              yield* delMultipleEffect(ops);
            }
          }
        }),
      );
    }).pipe(
      Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client)),
      Effect.runPromise,
    );
  }

  async checkpoint(root: Uint8Array): Promise<void> {
    super.checkpoint(root);
    const savepointName = `mpt_savepoint_${this.checkpoints.length}`;
    await Effect.runPromise(
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
    await Effect.runPromise(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`RELEASE SAVEPOINT ${sql(savepointName)}`;
      }).pipe(Effect.provide(Layer.succeed(SqlClient.SqlClient, this._client))),
    );
    await super.commit();
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

function splitBatchOps(
  ops: BatchDBOp<Uint8Array, Uint8Array>[],
): (
  | ETH_UTILS.PutBatch<Uint8Array, Uint8Array>[]
  | ETH_UTILS.DelBatch<Uint8Array>[]
)[] {
  const result: (
    | ETH_UTILS.PutBatch<Uint8Array, Uint8Array>[]
    | ETH_UTILS.DelBatch<Uint8Array>[]
  )[] = [];
  let currentPutGroup: ETH_UTILS.PutBatch<Uint8Array, Uint8Array>[] = [];
  let currentDelGroup: ETH_UTILS.DelBatch<Uint8Array>[] = [];
  let currentType: "put" | "del" | null = null;

  for (const op of ops) {
    if (op.type !== currentType) {
      if (currentType === "put" && currentPutGroup.length > 0) {
        result.push(currentPutGroup);
        currentPutGroup = [];
      } else if (currentType === "del" && currentDelGroup.length > 0) {
        result.push(currentDelGroup);
        currentDelGroup = [];
      }

      currentType = op.type;
      if (op.type === "put") {
        currentPutGroup.push(op as ETH_UTILS.PutBatch<Uint8Array, Uint8Array>);
      } else {
        currentDelGroup.push(op as ETH_UTILS.DelBatch<Uint8Array>);
      }
    } else {
      if (op.type === "put") {
        currentPutGroup.push(op as ETH_UTILS.PutBatch<Uint8Array, Uint8Array>);
      } else {
        currentDelGroup.push(op as ETH_UTILS.DelBatch<Uint8Array>);
      }
    }
  }
  if (currentType === "put" && currentPutGroup.length > 0) {
    result.push(currentPutGroup);
  } else if (currentType === "del" && currentDelGroup.length > 0) {
    result.push(currentDelGroup);
  }
  return result;
}

function isPutBatch(
  ops: BatchDBOp<Uint8Array, Uint8Array>[],
): ops is ETH_UTILS.PutBatch<Uint8Array, Uint8Array>[] {
  return ops[0].type === "put";
}
