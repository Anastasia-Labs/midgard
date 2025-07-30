import { SqlClient } from "@effect/sql";
import { BatchDBOp } from "@ethereumjs/util";
import { Effect } from "effect";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { toHex } from "@lucid-evolution/lucid";
import { Level } from "level";
import { MemoryLevel } from "memory-level";
import { NodeConfig } from "@/config.js";
import { LedgerColumns, ProcessedTx, ProcessedTxColumns } from "@/database/utils.js";
import { Database } from "@/services/database.js";
// Key of the row which its value is the persisted trie root.
const rootKey = ETH.ROOT_DB_KEY;

const LEVELDB_ENCODING_OPTS = { keyEncoding: "view", valueEncoding: "view" };

export const makeMpts = () =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
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
    const levelDb = new Level<string, Uint8Array>(
      nodeConfig.MPT_DB_PATH,
      LEVELDB_ENCODING_OPTS,
    );
    const ledgerTrie = yield* Effect.tryPromise({
      try: () =>
        ETH.createMPT({
          db: new LevelDB(levelDb),
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
  mempoolTxs: readonly ProcessedTx[],
): Effect.Effect<
  {
    utxoRoot: string;
    txRoot: string;
    mempoolTxHashes: Buffer[];
    sizeOfBlocksTxs: number;
  },
  Error,
  Database
> =>
  Effect.gen(function* () {
    const mempoolTxHashes: Buffer[] = [];
    const batchDBOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfBlocksTxs = 0;
    yield* Effect.logInfo("🔹 Going through mempool txs and finding roots...");
    yield* Effect.forEach(
      mempoolTxs,
      (ptx: ProcessedTx) =>
        Effect.gen(function* () {
          const txHash = ptx[ProcessedTxColumns.TX_ID];
          const txCbor = ptx[ProcessedTxColumns.TX_CBOR];
          const spent = ptx[ProcessedTxColumns.INPUTS];
          const produced = ptx[ProcessedTxColumns.OUTPUTS];
          mempoolTxHashes.push(txHash);
          sizeOfBlocksTxs += txCbor.length;
          yield* Effect.tryPromise({
            try: () => mempoolTrie.put(txHash, txCbor),
            catch: (e) => new Error(`${e}`),
          });
          const delOps: ETH_UTILS.BatchDBOp[] = spent.map((outRef) => ({
            type: "del",
            key: outRef,
          }));
          // TODO: Use an actual outref rather than txHash.
          const putOps: ETH_UTILS.BatchDBOp[] = produced.map((o) => ({
            type: "put",
            key: txHash,
            value: o[LedgerColumns.OUTPUT],
          }));
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

export class LevelDB {
  _leveldb: MemoryLevel<string, Uint8Array>;

  constructor(leveldb: MemoryLevel<string, Uint8Array>) {
    this._leveldb = leveldb ?? new MemoryLevel(LEVELDB_ENCODING_OPTS);
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
}
