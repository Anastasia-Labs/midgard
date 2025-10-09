import { SqlClient } from "@effect/sql";
import { BatchDBOp } from "@ethereumjs/util";
import { Cause, Data, Effect } from "effect";
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
  {
    ledgerTrie: ETH.MerklePatriciaTrie;
    ledgerDB: Level<string, Uint8Array>;
    mempoolTrie: ETH.MerklePatriciaTrie;
    mempoolDB: Level<string, Uint8Array>;
  },
  MptError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const mempoolLevelDb = new Level<string, Uint8Array>(
    nodeConfig.MEMPOOL_MPT_DB_PATH,
    LEVELDB_ENCODING_OPTS,
  );
  const mempoolTrie: ETH.MerklePatriciaTrie = yield* Effect.tryPromise({
    try: () =>
      ETH.createMPT({
        db: new LevelDB(mempoolLevelDb),
        useRootPersistence: true,
        valueEncoding: LEVELDB_ENCODING_OPTS.valueEncoding,
      }),
    catch: (e) => MptError.trieCreate("mempool", e),
  });
  const ledgerLevelDb = new Level<string, Uint8Array>(
    nodeConfig.LEDGER_MPT_DB_PATH,
    LEVELDB_ENCODING_OPTS,
  );
  const ledgerTrie: ETH.MerklePatriciaTrie = yield* Effect.tryPromise({
    try: () =>
      ETH.createMPT({
        db: new LevelDB(ledgerLevelDb),
        useRootPersistence: true,
        valueEncoding: LEVELDB_ENCODING_OPTS.valueEncoding,
      }),
    catch: (e) => MptError.trieCreate("ledger", e),
  });
  const mempoolRootBeforeMempoolTxs = yield* Effect.sync(() =>
    mempoolTrie.root(),
  );

  const ledgerRootBeforeMempoolTxs = yield* Effect.gen(function* () {
    const previousRoot = yield* Effect.sync(() => ledgerTrie.root());
    if (previousRoot !== ledgerTrie.EMPTY_TRIE_ROOT) return previousRoot;
    else {
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
      yield* Effect.tryPromise({
        try: () => ledgerTrie.batch(ops),
        catch: (e) => MptError.batch("ledger", e),
      });
      const rootAfterGenesis = yield* Effect.sync(() => ledgerTrie.root());
      yield* Effect.logInfo(
        `🔹 New ledger trie root after inserting genesis utxos: ${toHex(rootAfterGenesis)}`,
      );
      return rootAfterGenesis;
    }
  });

  // Ensuring persisted root is stored in tries' private properties
  yield* Effect.sync(() => mempoolTrie.root(mempoolRootBeforeMempoolTxs));
  yield* Effect.sync(() => ledgerTrie.root(ledgerRootBeforeMempoolTxs));
  return {
    ledgerTrie,
    ledgerDB: ledgerLevelDb,
    mempoolTrie,
    mempoolDB: mempoolLevelDb,
  };
});

export const deleteMempoolMpt: Effect.Effect<
  void,
  FileSystemError,
  NodeConfig
> = Effect.gen(function* () {
  const config = yield* NodeConfig;
  yield* Effect.try({
    try: () =>
      FS.rmSync(config.MEMPOOL_MPT_DB_PATH, { recursive: true, force: true }),
    catch: (e) =>
      new FileSystemError({
        message: "Failed to delete mempool's LevelDB file from disk",
        cause: e,
      }),
  });
}).pipe(Effect.withLogSpan("Delete mempool MPT"));

export const deleteLedgerMpt: Effect.Effect<void, FileSystemError, NodeConfig> =
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    yield* Effect.try({
      try: () =>
        FS.rmSync(config.LEDGER_MPT_DB_PATH, { recursive: true, force: true }),
      catch: (e) =>
        new FileSystemError({
          message: "Failed to delete ledger's LevelDB file from disk",
          cause: e,
        }),
    });
  }).pipe(Effect.withLogSpan("Delete ledger MPT"));

// Make mempool trie, and fill it with ledger trie with processed mempool txs
export const processMpts = (
  ledgerTrie: ETH.MerklePatriciaTrie,
  mempoolTrie: ETH.MerklePatriciaTrie,
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
      [
        Effect.tryPromise({
          try: () => mempoolTrie.batch(mempoolBatchOps),
          catch: (e) => MptError.batch("mempool", e),
        }),
        Effect.tryPromise({
          try: () => ledgerTrie.batch(batchDBOps),
          catch: (e) => MptError.batch("ledger", e),
        }),
      ],
      { concurrency: "unbounded" },
    );

    const txRoot = toHex(mempoolTrie.root());
    const utxoRoot = toHex(ledgerTrie.root());

    yield* Effect.logInfo(`🔹 New transaction root found: ${txRoot}`);
    yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);

    return {
      utxoRoot: utxoRoot,
      txRoot: txRoot,
      mempoolTxHashes: mempoolTxHashes,
      sizeOfProcessedTxs: sizeOfProcessedTxs,
    };
  });

export const keyValueMptRoot = (
  keys: Buffer[],
  values: Buffer[],
): Effect.Effect<string, MptError, never> =>
  Effect.gen(function* () {
    const trie: ETH.MerklePatriciaTrie = yield* Effect.tryPromise({
      try: () =>
        ETH.createMPT({
          db: new ETH_UTILS.MapDB(),
        }),
      catch: (e) => MptError.trieCreate("keyValueMPT", e),
    });

    const ops: ETH_UTILS.BatchDBOp[] = yield* Effect.allSuccesses(
      keys.map((key: Buffer, i: number) =>
        Effect.gen(function* () {
          const op: ETH_UTILS.BatchDBOp = {
            type: "put",
            key: key,
            value: values[i], // Poor mans zip
          };
          return op;
        }),
      ),
    );

    yield* Effect.tryPromise({
      try: () => trie.batch(ops),
      catch: (e) => MptError.batch("keyValueMPT root", e),
    });
    const foundRoot = yield* Effect.sync(() => toHex(trie.root()));
    return foundRoot;
  });

export const withTrieTransaction = <A, E, R>(
  trie: ETH.MerklePatriciaTrie,
  eff: Effect.Effect<A, E, R>,
): Effect.Effect<void | A, E | DatabaseError | MptError, Database | R> =>
  Effect.gen(function* () {
    yield* Effect.sync(() => trie.checkpoint());
    const sql = yield* SqlClient.SqlClient;
    const x = sql.withTransaction(eff).pipe(
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
    const res = yield* x;
    yield* Effect.sync(() => trie.commit());
    return res;
  }).pipe(
    Effect.catchAll((e) =>
      Effect.gen(function* () {
        yield* Effect.tryPromise({
          try: () => trie.revert(),
          catch: (e) =>
            new MptError({
              message: "Failed to revert operations performed on an MPT",
              cause: e,
            }),
        });
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
}
