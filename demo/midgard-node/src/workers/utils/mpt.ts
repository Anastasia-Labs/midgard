import { SqlClient } from "@effect/sql";
import { BatchDBOp } from "@ethereumjs/util";
import { Data, Effect } from "effect";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { UTxO, toHex, utxoToCore } from "@lucid-evolution/lucid";
import { Level } from "level";
import { Database, NodeConfig } from "@/services/index.js";
import * as Tx from "@/database/utils/tx.js";
import * as Ledger from "@/database/utils/ledger.js";
import { FileSystemError, findSpentAndProducedUTxOs } from "@/utils.js";
import * as FS from "fs";
import * as SDK from "@al-ft/midgard-sdk";
import { DatabaseError } from "@/database/utils/common.js";

const LEVELDB_ENCODING_OPTS = {
  keyEncoding: ETH_UTILS.KeyEncoding.Bytes,
  valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
};

export const makeMpts: Effect.Effect<
  { ledgerTrie: MidgardMpt; mempoolTrie: MidgardMpt},
  MptError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const mempoolTrie = yield* MidgardMpt.create(nodeConfig.MEMPOOL_MPT_DB_PATH, "mempool")
  const ledgerTrie = yield* MidgardMpt.create(nodeConfig.LEDGER_MPT_DB_PATH, "ledger")
  const ledgerRootIsEmpty = yield* ledgerTrie.rootIsEmpty();
  if (ledgerRootIsEmpty) {
    yield* Effect.logInfo(
      "🔹 No previous ledger trie root found - inserting genesis utxos",
    );
    const ops: ETH_UTILS.BatchDBOp[] = yield* Effect.allSuccesses(
      nodeConfig.GENESIS_UTXOS.map((u: UTxO) =>
        Effect.gen(function* () {
          const core = yield* Effect.try(() => utxoToCore(u)).pipe(
            Effect.tapError((e) =>
              Effect.logError(`IGNORED ERROR WITH GENESIS UTXOS: ${e}`),
            ),
          );
          const op: ETH_UTILS.BatchDBOp = {
            type: "put",
            key: Buffer.from(core.input().to_cbor_bytes()),
            value: Buffer.from(core.output().to_cbor_bytes()),
          };
          return op;
        }),
      ),
    );
    yield* ledgerTrie.batch(ops)
    const rootAfterGenesis = yield* ledgerTrie.getRootHex();
    yield* Effect.logInfo(
      `🔹 New ledger trie root after inserting genesis utxos: ${rootAfterGenesis}`,
    );
  }
  return {
    ledgerTrie,
    mempoolTrie,
  };
});

export const deleteMempoolMpt = Effect.gen(function* () {
  const config = yield* NodeConfig;
  yield* deleteMPT(config.MEMPOOL_MPT_DB_PATH, "mempool")
})

export const deleteLedgerMpt = Effect.gen(function* () {
  const config = yield* NodeConfig;
  yield* deleteMPT(config.LEDGER_MPT_DB_PATH, "ledger")
})

const deleteMPT = (path: string, name: string): Effect.Effect<
  void,
  FileSystemError
> => Effect.try({
    try: () =>
      FS.rmSync(path, { recursive: true, force: true }),
    catch: (e) =>
      new FileSystemError({
        message: `Failed to delete ${name}'s LevelDB file from disk`,
        cause: e,
      }),
  }).pipe(Effect.withLogSpan(`Delete ${name} MPT`));

// Make mempool trie, and fill it with ledger trie with processed mempool txs
export const processMpts = (
  ledgerTrie: MidgardMpt,
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly Tx.Entry[],
): Effect.Effect<
  {
    utxoRoot: string;
    txRoot: string;
    mempoolTxHashes: Buffer[];
    sizeOfProcessedTxs: number;
  },
  MptError | SDK.Utils.CmlUnexpectedError,
  Database
> =>
  Effect.gen(function* () {
    const mempoolTxHashes: Buffer[] = [];
    const mempoolBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const batchDBOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfProcessedTxs = 0;
    yield* Effect.logInfo("🔹 Going through mempool txs and finding roots...");
    yield* Effect.forEach(mempoolTxs, (entry: Tx.Entry) =>
      Effect.gen(function* () {
        const txHash = entry[Tx.Columns.TX_ID];
        const txCbor = entry[Tx.Columns.TX];
        mempoolTxHashes.push(txHash);
        const { spent, produced } = yield* findSpentAndProducedUTxOs(
          txCbor,
          txHash,
        ).pipe(Effect.withSpan("findSpentAndProducedUTxOs"));
        sizeOfProcessedTxs += txCbor.length;
        const delOps: ETH_UTILS.BatchDBOp[] = spent.map((outRef) => ({
          type: "del",
          key: outRef,
        }));
        const putOps: ETH_UTILS.BatchDBOp[] = produced.map(
          (le: Ledger.MinimalEntry) => ({
            type: "put",
            key: le[Ledger.Columns.OUTREF],
            value: le[Ledger.Columns.OUTPUT],
          }),
        );
        yield* Effect.sync(() =>
          mempoolBatchOps.push({
            type: "put",
            key: txHash,
            value: txCbor,
          }),
        );
        yield* Effect.sync(() => batchDBOps.push(...delOps));
        yield* Effect.sync(() => batchDBOps.push(...putOps));
      }),
    );

    yield* Effect.all(
      [mempoolTrie.batch(mempoolBatchOps), ledgerTrie.batch(batchDBOps)],
      { concurrency: "unbounded" },
    );

    const txRoot = yield* mempoolTrie.getRootHex();
    const utxoRoot = yield* ledgerTrie.getRootHex();

    yield* Effect.logInfo(`🔹 New transaction root found: ${txRoot}`);
    yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);

    return {
      utxoRoot,
      txRoot,
      mempoolTxHashes: mempoolTxHashes,
      sizeOfProcessedTxs: sizeOfProcessedTxs,
    };
  });

export const withTrieTransaction = <A, E, R>(
  trie: MidgardMpt,
  eff: Effect.Effect<A, E, R>,
): Effect.Effect<void | A, E | DatabaseError | MptError, Database | R> =>
  Effect.gen(function* () {
    yield* trie.checkpoint();
    const sql = yield* SqlClient.SqlClient;
    const tx = sql.withTransaction(eff).pipe(
      Effect.catchTag("SqlError", (e) =>
        Effect.fail(
          new DatabaseError({
            message: "The effect executed within an SQL transaction failed",
            table: "<unknown>",
            cause: e,
          }),
        ),
      ),
    );
    const res = yield* tx;
    yield* trie.commit();
    return res;
  }).pipe(
    Effect.catchAll((e) =>
      Effect.gen(function* () {
        yield* trie.revert()
        yield* Effect.fail(e);
      }),
    ),
  );

export class LevelDB {
  _leveldb: Level<string, Uint8Array>;

  constructor(leveldb: Level<string, Uint8Array>) {
    this._leveldb = leveldb;
  }

  async open() {
    await this._leveldb.open();
  }

  async get(key: string) {
    return this._leveldb.get(key, LEVELDB_ENCODING_OPTS);
  }

  async put(key: string, val: Uint8Array) {
    await this._leveldb.put(key, val, LEVELDB_ENCODING_OPTS);
  }

  async del(key: string) {
    await this._leveldb.del(key, LEVELDB_ENCODING_OPTS);
  }

  async batch(opStack: BatchDBOp<string, Uint8Array>[]) {
    await this._leveldb.batch(opStack, LEVELDB_ENCODING_OPTS);
  }

  shallowCopy() {
    return new LevelDB(this._leveldb);
  }

  getDatabase() {
    return this._leveldb;
  }
}

export class MptError extends Data.TaggedError(
  "MptError",
)<SDK.Utils.GenericErrorFields> {
  static get(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred getting an entry from ${trie} trie`,
      cause,
    });
  }
  static put(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred inserting a new entry in ${trie} trie`,
      cause,
    });
  }
  static batch(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred during a batch operation on ${trie} trie`,
      cause,
    });
  }
  static del(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred deleting an entry from ${trie} trie`,
      cause,
    });
  }
  static trieCreate(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred creating ${trie} trie`,
      cause,
    });
  }
  static trieCommit(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred committing ${trie} trie`,
      cause,
    });
  }
  static trieRevert(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred reverting ${trie} trie`,
      cause,
    });
  }
}

export class MidgardMpt {
  public trie: ETH.MerklePatriciaTrie;
  public trieName: string
  public database: LevelDB;
  public databaseFilePath: string;

  private constructor(database: LevelDB, databaseFilePath: string, trie: ETH.MerklePatriciaTrie, trieName: string) {
    this.database = database;
    this.databaseFilePath = databaseFilePath;
    this.trie = trie;
    this.trieName = trieName;
  };

  public static create(levelDBFilePath: string, trieName: string): Effect.Effect<MidgardMpt, MptError> {
    return Effect.gen(function* () {
      const level = new Level<string, Uint8Array>(
        levelDBFilePath,
        LEVELDB_ENCODING_OPTS,
      );
      const levelDB = new LevelDB(level);
      const trie = yield* Effect.tryPromise({
        try: () => ETH.createMPT({
          db: levelDB,
          useRootPersistence: true,
          valueEncoding: LEVELDB_ENCODING_OPTS.valueEncoding,
          }),
          catch: (e) => MptError.trieCreate(trieName, e),
        })
      const wrapper = new MidgardMpt(levelDB, levelDBFilePath, trie, trieName)
      return wrapper;
    })
  };

  public delete(): Effect.Effect<void, FileSystemError> {
    return deleteMPT(this.databaseFilePath, this.trieName)
  };

  public batch(arg: ETH_UTILS.BatchDBOp[]): Effect.Effect<void, MptError> {
    const trieName = this.trieName;
    const trieBatch = this.trie.batch;
    return Effect.gen(function* () {
      return yield* Effect.tryPromise({
        try: () => trieBatch(arg),
        catch: (e) => MptError.batch(trieName, e),
      })
   })
  }

  public getRoot(): Effect.Effect<Uint8Array> {
   return Effect.sync(() => this.trie.root());
  };

  public getRootHex(): Effect.Effect<string> {
    return Effect.sync(() => toHex(this.trie.root()));
  }

  public rootIsEmpty(): Effect.Effect<boolean> {
    const getRoot = this.getRoot;
    const emptyRoot = this.trie.EMPTY_TRIE_ROOT
    return Effect.gen(function* () {
      const root = yield* getRoot();
      return root === emptyRoot;
   })
  }

  public checkpoint(): Effect.Effect<void> {
    return Effect.sync(() => this.trie.checkpoint())
  };

  public commit(): Effect.Effect<void, MptError> {
    return Effect.tryPromise({
      try: () => this.trie.commit(),
      catch: (e) => MptError.trieCommit(this.trieName, e),
    })
  };

  public revert(): Effect.Effect<void, MptError> {
    return Effect.tryPromise({
      try: () => this.trie.revert(),
      catch: (e) => MptError.trieRevert(this.trieName, e),
    })
  };

  public databaseStats() {
    return this.trie.database()._stats
  }
}