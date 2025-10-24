import { SqlClient } from "@effect/sql";
import { BatchDBOp } from "@ethereumjs/util";
import { Data, Effect } from "effect";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { UTxO, toHex, utxoToCore } from "@lucid-evolution/lucid";
import { Level } from "level";
import { Database, NodeConfig } from "@/services/index.js";
import { FileSystemError } from "@/utils.js";
import * as FS from "fs";
import * as SDK from "@al-ft/midgard-sdk";
import { DatabaseError } from "@/database/utils/common.js";

const LEVELDB_ENCODING_OPTS = {
  keyEncoding: ETH_UTILS.KeyEncoding.Bytes,
  valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
};

export const makeMpts: Effect.Effect<
  { ledgerTrie: MidgardMpt; mempoolTrie: MidgardMpt },
  MptError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const mempoolTrie = yield* MidgardMpt.create(
    "mempool",
    nodeConfig.MEMPOOL_MPT_DB_PATH,
  );
  const ledgerTrie = yield* MidgardMpt.create(
    "ledger",
    nodeConfig.LEDGER_MPT_DB_PATH,
  );
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
    yield* ledgerTrie.batch(ops);
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
  yield* deleteMpt(config.MEMPOOL_MPT_DB_PATH, "mempool");
});

export const deleteLedgerMpt = Effect.gen(function* () {
  const config = yield* NodeConfig;
  yield* deleteMpt(config.LEDGER_MPT_DB_PATH, "ledger");
});

export const deleteMpt = (
  path: string,
  name: string,
): Effect.Effect<void, FileSystemError> =>
  Effect.try({
    try: () => FS.rmSync(path, { recursive: true, force: true }),
    catch: (e) =>
      new FileSystemError({
        message: `Failed to delete ${name}'s LevelDB file from disk`,
        cause: e,
      }),
  }).pipe(Effect.withLogSpan(`Delete ${name} MPT`));

export const keyValueMptRoot = (
  keys: Buffer[],
  values: Buffer[],
): Effect.Effect<string, MptError, never> =>
  Effect.gen(function* () {
    const trie = yield* MidgardMpt.create("keyValueMPT");

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

    yield* trie.batch(ops);
    return yield* trie.getRootHex();
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
        yield* trie.revert();
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
      message: `An error occurred creating ${trie} trie: ${cause}`,
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
  static rootNotSet(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred getting ${trie} trie root, the root is ${typeof cause}`,
      cause,
    });
  }
}

export class MidgardMpt {
  public readonly trie: ETH.MerklePatriciaTrie;
  public readonly trieName: string;
  public readonly databaseAndPath?: {
    database: LevelDB;
    databaseFilePath: string;
  };

  private constructor(
    trie: ETH.MerklePatriciaTrie,
    trieName: string,
    databaseAndPath?: { database: LevelDB; databaseFilePath: string },
  ) {
    this.trie = trie;
    this.trieName = trieName;
    this.databaseAndPath = databaseAndPath;
  }

  /**
   * Create a Merkle Patricia Trie (MPT) with a LevelDB-backed database if
   * `levelDBFilePath` is provided, or an in-memory database otherwise.
   *
   * @param trieName - The name identifier for the trie instance.
   * @param levelDBFilePath - Optional file path for LevelDB persistence.
   * @returns An Effect that resolves to the created MidgardMpt instance or fails with MptError.
   */
  public static create(
    trieName: string,
    levelDBFilePath?: string,
  ): Effect.Effect<MidgardMpt, MptError> {
    return Effect.gen(function* () {
      let databaseAndPath:
        | { database: LevelDB; databaseFilePath: string }
        | undefined = undefined;
      let valueEncoding: ETH_UTILS.ValueEncoding | undefined = undefined;
      if (typeof levelDBFilePath === "string") {
        const level = new Level<string, Uint8Array>(
          levelDBFilePath,
          LEVELDB_ENCODING_OPTS,
        );
        const db = new LevelDB(level);
        databaseAndPath = { database: db, databaseFilePath: levelDBFilePath };
        valueEncoding = LEVELDB_ENCODING_OPTS.valueEncoding;
      }
      const trie = yield* Effect.tryPromise({
        try: () =>
          ETH.createMPT({
            db: databaseAndPath?.database,
            useRootPersistence: Boolean(databaseAndPath),
            valueEncoding,
          }),
        catch: (e) => MptError.trieCreate(trieName, e),
      });
      return new MidgardMpt(trie, trieName, databaseAndPath);
    });
  }

  public delete(): Effect.Effect<void, FileSystemError> {
    if (this.databaseAndPath) {
      return deleteMpt(this.databaseAndPath.databaseFilePath, this.trieName);
    } else {
      return Effect.succeed(Effect.void);
    }
  }

  public batch(arg: ETH_UTILS.BatchDBOp[]): Effect.Effect<void, MptError> {
    const trieName = this.trieName;
    const trieBatch = this.trie.batch(arg);
    return Effect.tryPromise({
      try: () => trieBatch,
      catch: (e) => MptError.batch(trieName, e),
    });
  }

  public getRoot(): Effect.Effect<Uint8Array, MptError> {
    const trieName = this.trieName;
    const root = this.trie.root();
    return Effect.gen(function* () {
      if (root === undefined || root === null)
        return yield* Effect.fail(MptError.rootNotSet(trieName, root));
      // Normalize to pure Uint8Array for type consistency
      // trie.root() returns different constructor types depending on the source:
      //   - Fresh (computed): Uint8Array
      //   - Persisted (loaded from Level.js after some changes): Buffer
      return root instanceof Uint8Array && !Buffer.isBuffer(root)
        ? root
        : new Uint8Array(root.buffer, root.byteOffset, root.byteLength);
    });
  }

  public getRootHex(): Effect.Effect<string, MptError> {
    return this.getRoot().pipe(Effect.map(toHex));
  }

  public rootIsEmpty(): Effect.Effect<boolean, MptError> {
    const getRootHex = this.getRootHex();
    const emptyRootHex = toHex(this.trie.EMPTY_TRIE_ROOT);
    return Effect.gen(function* () {
      const rootHex = yield* getRootHex;
      return rootHex === emptyRootHex;
    });
  }

  public checkpoint(): Effect.Effect<void> {
    return Effect.sync(() => this.trie.checkpoint());
  }

  public commit(): Effect.Effect<void, MptError> {
    return Effect.tryPromise({
      try: () => this.trie.commit(),
      catch: (e) => MptError.trieCommit(this.trieName, e),
    });
  }

  public revert(): Effect.Effect<void, MptError> {
    return Effect.tryPromise({
      try: () => this.trie.revert(),
      catch: (e) => MptError.trieRevert(this.trieName, e),
    });
  }

  public databaseStats() {
    return this.trie.database()._stats;
  }
}
