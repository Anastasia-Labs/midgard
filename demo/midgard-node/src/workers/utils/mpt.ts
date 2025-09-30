import { SqlClient } from "@effect/sql";
import { BatchDBOp } from "@ethereumjs/util";
import { Data, Effect } from "effect";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { UTxO, toHex, utxoToCore } from "@lucid-evolution/lucid";
import { Level } from "level";
import { NodeConfig } from "@/config.js";
import * as Tx from "@/database/utils/tx.js";
import * as Ledger from "@/database/utils/ledger.js";
import { Database } from "@/services/database.js";
import { CmlUnexpectedError, findSpentAndProducedUTxOs } from "@/utils.js";
import * as FS from "fs";

// Key of the row which its value is the persisted trie root.
const rootKey = ETH.ROOT_DB_KEY;

const LEVELDB_ENCODING_OPTS = {
  keyEncoding: ETH_UTILS.KeyEncoding.Bytes,
  valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
};

export const makeMpts: Effect.Effect<
  { ledgerTrie: ETH.MerklePatriciaTrie; mempoolTrie: ETH.MerklePatriciaTrie },
  MptError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
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
  //
  // ^ Old comment, we are now storing mempool's LevelDB on disk as well. This
  //   allows us to continue a work that's been done in cases of failures.
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
  // Ledger MPT from the other side should use a checkpoint database —
  // its MPT building operations are paired with database ones
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
  const mempoolRootBeforeMempoolTxs = yield* Effect.tryPromise(() =>
    mempoolTrie.get(rootKey),
  ).pipe(Effect.orElse(() => Effect.succeed(ledgerTrie.EMPTY_TRIE_ROOT)));
  const ledgerRootBeforeMempoolTxs = yield* Effect.tryPromise(() =>
    ledgerTrie.get(rootKey),
  ).pipe(
    Effect.orElse(() =>
      Effect.gen(function* () {
        yield* Effect.sync(() => ledgerTrie.root(ledgerTrie.EMPTY_TRIE_ROOT));
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
        return rootAfterGenesis;
      }),
    ),
  );
  // Ensuring persisted root is stored in tries' private properties
  yield* Effect.sync(() => mempoolTrie.root(mempoolRootBeforeMempoolTxs));
  yield* Effect.sync(() => ledgerTrie.root(ledgerRootBeforeMempoolTxs));
  return {
    ledgerTrie,
    mempoolTrie,
  };
});

export const deleteMempoolMpt: Effect.Effect<void, MptError, NodeConfig> =
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    yield* Effect.try({
      try: () =>
        FS.rmSync(config.MEMPOOL_MPT_DB_PATH, { recursive: true, force: true }),
      catch: (e) => MptError.trieDelete("mempool", e),
    });
  }).pipe(Effect.withLogSpan("Delete mempool MPT"));

export const deleteLedgerMpt: Effect.Effect<void, MptError, NodeConfig> =
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    yield* Effect.try({
      try: () =>
        FS.rmSync(config.LEDGER_MPT_DB_PATH, { recursive: true, force: true }),
      catch: (e) => MptError.trieDelete("ledger", e),
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
  MptError | CmlUnexpectedError,
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

export const keyValueMptRoot = (
  keys: Buffer[],
  values: Buffer[],
) =>
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
  })

export class MptError extends Data.TaggedError("MptError")<{
  readonly message: string;
  readonly cause?: unknown;
}> {
  static get(trie: string, cause?: unknown) {
    return new MptError({
      message: `An error occurred on ${trie} trie get operation`,
      cause,
    });
  }
  static batch(trie: string, cause?: unknown) {
    return new MptError({
      message: `An error occurred on ${trie} trie batch operation`,
      cause,
    });
  }
  static trieDelete(trie: string, cause?: unknown) {
    return new MptError({
      message: `An error occurred on whole ${trie} trie delete`,
      cause,
    });
  }
  static trieCreate(trie: string, cause?: unknown) {
    return new MptError({
      message: `An error occurred on ${trie} trie create`,
      cause,
    });
  }
}
