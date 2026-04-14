import { BatchDBOp } from "@ethereumjs/util";
import { Data, Effect } from "effect";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";
import { SqlClient } from "@effect/sql";
import { UTxO, toHex, utxoToCore, Script } from "@lucid-evolution/lucid";
import { Level } from "level";
import { Database, NodeConfig } from "@/services/index.js";
import * as Tx from "@/database/utils/tx.js";
import * as Ledger from "@/database/utils/ledger.js";
import * as MempoolTxDeltasDB from "@/database/mempoolTxDeltas.js";
import * as TxRejectionsDB from "@/database/txRejections.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as DepositsDB from "@/database/deposits.js";
import { FileSystemError, findSpentAndProducedUTxOs } from "@/utils.js";
import * as FS from "fs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";

const LEVELDB_ENCODING_OPTS = {
  keyEncoding: ETH_UTILS.KeyEncoding.Bytes,
  valueEncoding: ETH_UTILS.ValueEncoding.Bytes,
};

export const COMMIT_REJECT_CODE_DECODE_FAILED = "E_COMMIT_CBOR_DESERIALIZATION";

export type ResolvedTxDeltaForCommit =
  | {
      readonly _tag: "Decoded";
      readonly spent: readonly Buffer[];
      readonly produced: readonly Ledger.MinimalEntry[];
    }
  | {
      readonly _tag: "Rejected";
      readonly rejection: TxRejectionsDB.EntryNoTimestamp;
    };

export const resolveTxDeltaForCommit = (
  entry: Tx.EntryWithTimeStamp,
  existingDelta: MempoolTxDeltasDB.TxDelta | undefined,
): Effect.Effect<ResolvedTxDeltaForCommit, never> =>
  Effect.gen(function* () {
    if (existingDelta !== undefined) {
      return {
        _tag: "Decoded",
        spent: existingDelta.spent.map((outRef) => Buffer.from(outRef)),
        produced: existingDelta.produced.map((deltaEntry) => ({
          [Ledger.Columns.OUTREF]: Buffer.from(
            deltaEntry[Ledger.Columns.OUTREF],
          ),
          [Ledger.Columns.OUTPUT]: Buffer.from(
            deltaEntry[Ledger.Columns.OUTPUT],
          ),
        })),
      };
    }

    const txId = entry[Tx.Columns.TX_ID];
    const txCbor = entry[Tx.Columns.TX];
    const decoded = yield* findSpentAndProducedUTxOs(txCbor, txId).pipe(
      Effect.either,
    );
    if (decoded._tag === "Left") {
      return {
        _tag: "Rejected",
        rejection: {
          [TxRejectionsDB.Columns.TX_ID]: Buffer.from(txId),
          [TxRejectionsDB.Columns.REJECT_CODE]:
            COMMIT_REJECT_CODE_DECODE_FAILED,
          [TxRejectionsDB.Columns.REJECT_DETAIL]: decoded.left.message,
        },
      };
    }

    return {
      _tag: "Decoded",
      spent: decoded.right.spent,
      produced: decoded.right.produced,
    };
  });

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
        utxoToPutBatchOp(u).pipe(
          Effect.tapError((e) =>
            Effect.logError(`IGNORED ERROR WITH GENESIS UTXOS: ${e}`),
          ),
        ),
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

export const utxoToPutBatchOp = (
  utxo: UTxO,
): Effect.Effect<ETH_UTILS.BatchDBOp, SDK.CmlDeserializationError> =>
  Effect.gen(function* () {
    const core = yield* Effect.try({
      try: () => utxoToCore(utxo),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to convert UTxO to CML.TransactionOutput",
          cause: e,
        }),
    });
    const op: ETH_UTILS.BatchDBOp = {
      type: "put",
      key: Buffer.from(core.input().to_cbor_bytes()),
      value: Buffer.from(core.output().to_cbor_bytes()),
    };
    return op;
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

// Make mempool trie, and fill it with ledger trie with processed mempool txs
export type ProcessMptsConfig = {
  readonly currentBlockStartTime?: Date;
  readonly processedOnlyEndTime?: Date;
  readonly depositOnlyEndTime?: Date;
  readonly depositVisibilityBarrierTime?: Date;
};

export const resolveIncludedDepositEntriesForWindow = ({
  currentBlockStartTime,
  effectiveEndTime,
}: {
  readonly currentBlockStartTime: Date;
  readonly effectiveEndTime: Date;
}): Effect.Effect<readonly DepositsDB.Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pendingEntries =
          yield* DepositsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime);
        if (pendingEntries.length <= 0) {
          return [];
        }

        const overdueEntries = pendingEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.INCLUSION_TIME].getTime() <=
            currentBlockStartTime.getTime(),
        );
        const skippedAwaitingEntries = overdueEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting,
        );
        if (skippedAwaitingEntries.length > 0) {
          return yield* Effect.fail(
            new DatabaseError({
              table: DepositsDB.tableName,
              message:
                "Refusing to build a block because one or more deposits due for an earlier block were never assigned to a header",
              cause: skippedAwaitingEntries
                .map((entry) => entry[DepositsDB.Columns.ID].toString("hex"))
                .join(","),
            }),
          );
        }

        const replayableOverdueEntries = overdueEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] !== DepositsDB.Status.Awaiting,
        );
        if (replayableOverdueEntries.length > 0) {
          yield* Effect.logWarning(
            `Re-including ${replayableOverdueEntries.length} previously projected deposit UTxO(s) whose prior header assignment was abandoned before confirmation.`,
          );
        }

        const currentWindowEntries = pendingEntries.filter(
          (entry) =>
            currentBlockStartTime.getTime() <
            entry[DepositsDB.Columns.INCLUSION_TIME].getTime(),
        );
        let normalizedCurrentWindowEntries: readonly DepositsDB.Entry[] = [];
        if (currentWindowEntries.length > 0) {
          const awaitingEntries = currentWindowEntries.filter(
            (entry) =>
              entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting,
          );
          if (awaitingEntries.length > 0) {
            const mempoolEntries = yield* Effect.forEach(
              awaitingEntries,
              DepositsDB.toMempoolLedgerEntry,
            );
            yield* MempoolLedgerDB.reconcileDepositEntries(mempoolEntries);
            yield* DepositsDB.markAwaitingAsProjected(
              awaitingEntries.map((entry) => entry[DepositsDB.Columns.ID]),
            );
          }

          normalizedCurrentWindowEntries = currentWindowEntries.map((entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting
              ? {
                  ...entry,
                  [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
                }
              : entry,
          );
        }

        return [...replayableOverdueEntries, ...normalizedCurrentWindowEntries];
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      DepositsDB.tableName,
      "Failed to resolve deposits for the current block window",
    ),
  );

export const processMpts = (
  ledgerTrie: MidgardMpt,
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly Tx.EntryWithTimeStamp[],
  config?: ProcessMptsConfig,
): Effect.Effect<
  {
    utxoRoot: string;
    txRoot: string;
    mempoolTxHashes: Buffer[];
    processedMempoolTxs: readonly Tx.EntryWithTimeStamp[];
    sizeOfProcessedTxs: number;
    rejectedMempoolTxsCount: number;
    includedDepositEntriesCount: number;
    includedDepositEntries: readonly DepositsDB.Entry[];
    includedDepositEventIds: readonly Buffer[];
  },
  MptError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const processedMempoolTxs: Tx.EntryWithTimeStamp[] = [];
    const rejectedTxHashes: Buffer[] = [];
    const rejectionEntries: TxRejectionsDB.EntryNoTimestamp[] = [];
    const mempoolTxHashes: Buffer[] = [];
    const mempoolBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const batchDBOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfProcessedTxs = 0;
    const txDeltasByTxHash = yield* MempoolTxDeltasDB.retrieveByTxIds(
      mempoolTxs.map((entry) => entry[Tx.Columns.TX_ID]),
    );
    yield* Effect.logInfo("🔹 Going through mempool txs and finding roots...");
    yield* Effect.forEach(mempoolTxs, (entry: Tx.EntryWithTimeStamp) =>
      Effect.gen(function* () {
        const txHash = entry[Tx.Columns.TX_ID];
        const txCbor = entry[Tx.Columns.TX];
        const txHashHex = txHash.toString("hex");
        const existingDelta = txDeltasByTxHash.get(txHashHex);
        const resolved = yield* resolveTxDeltaForCommit(
          entry,
          existingDelta,
        ).pipe(Effect.withSpan("resolveTxDeltaForCommit"));
        if (resolved._tag === "Rejected") {
          rejectedTxHashes.push(Buffer.from(txHash));
          rejectionEntries.push(resolved.rejection);
          yield* Effect.logWarning(
            `Skipping malformed mempool tx ${txHashHex}: ${resolved.rejection[TxRejectionsDB.Columns.REJECT_DETAIL]}`,
          );
          return;
        }
        const { spent, produced } = resolved;
        mempoolTxHashes.push(txHash);
        processedMempoolTxs.push(entry);
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

    const effectiveEndTime =
      processedMempoolTxs[0]?.[Tx.Columns.TIMESTAMPTZ] ??
      config?.processedOnlyEndTime ??
      config?.depositOnlyEndTime;

    if (
      effectiveEndTime !== undefined &&
      config?.depositVisibilityBarrierTime !== undefined &&
      effectiveEndTime.getTime() > config.depositVisibilityBarrierTime.getTime()
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: DepositsDB.tableName,
          message:
            "Refusing to build a block because deposit ingestion is not confirmed up to the selected block end time",
          cause: `effective_end_time=${effectiveEndTime.toISOString()},deposit_visibility_barrier_time=${config.depositVisibilityBarrierTime.toISOString()}`,
        }),
      );
    }

    let includedDepositEntries: readonly DepositsDB.Entry[] = [];
    if (
      config?.currentBlockStartTime !== undefined &&
      effectiveEndTime !== undefined
    ) {
      includedDepositEntries = yield* resolveIncludedDepositEntriesForWindow({
        currentBlockStartTime: config.currentBlockStartTime,
        effectiveEndTime,
      });
    }
    const includedDepositEntriesCount = includedDepositEntries.length;
    const includedDepositEventIds = includedDepositEntries.map((entry) =>
      Buffer.from(entry[DepositsDB.Columns.ID]),
    );
    const depositLedgerEntries = yield* Effect.forEach(
      includedDepositEntries,
      DepositsDB.toLedgerEntry,
    );
    if (depositLedgerEntries.length > 0) {
      yield* Effect.logInfo(
        `🔹 Applying ${depositLedgerEntries.length} projected deposit UTxO(s) to the block pre-state.`,
      );
      yield* Effect.sync(() =>
        batchDBOps.unshift(
          ...depositLedgerEntries.map((entry) => ({
            type: "put" as const,
            key: entry[Ledger.Columns.OUTREF],
            value: entry[Ledger.Columns.OUTPUT],
          })),
        ),
      );
    }

    if (rejectedTxHashes.length > 0) {
      yield* Effect.logWarning(
        `Dropping ${rejectedTxHashes.length} malformed transaction(s) from MempoolDB`,
      );
      yield* Effect.all(
        [
          MempoolDB.clearTxs(rejectedTxHashes),
          TxRejectionsDB.insertMany(rejectionEntries),
        ],
        { concurrency: "unbounded" },
      );
    }

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
      processedMempoolTxs,
      sizeOfProcessedTxs: sizeOfProcessedTxs,
      rejectedMempoolTxsCount: rejectedTxHashes.length,
      includedDepositEntriesCount,
      includedDepositEntries,
      includedDepositEventIds,
    };
  });

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
): Effect.Effect<void | A, E | MptError, R> =>
  Effect.gen(function* () {
    yield* trie.checkpoint();
    const res = yield* eff;
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
  _location?: string;

  constructor(leveldb: Level<string, Uint8Array>, location?: string) {
    this._leveldb = leveldb;
    this._location = location;
  }

  /**
   * Opens the underlying LevelDB database.
   */
  async open() {
    await this._leveldb.open();
  }

  /**
   * Closes the underlying LevelDB database.
   */
  async close() {
    await this._leveldb.close();
  }

  /**
   * Reads a value from the underlying LevelDB database.
   */
  async get(key: string) {
    return this._leveldb.get(key, LEVELDB_ENCODING_OPTS);
  }

  /**
   * Writes a value into the underlying LevelDB database.
   */
  async put(key: string, val: Uint8Array) {
    await this._leveldb.put(key, val, LEVELDB_ENCODING_OPTS);
  }

  /**
   * Deletes a value from the underlying LevelDB database.
   */
  async del(key: string) {
    await this._leveldb.del(key, LEVELDB_ENCODING_OPTS);
  }

  /**
   * Executes a batch of LevelDB operations.
   */
  async batch(opStack: BatchDBOp<string, Uint8Array>[]) {
    await this._leveldb.batch(opStack, LEVELDB_ENCODING_OPTS);
  }

  /**
   * Builds a lightweight copy of the LevelDB wrapper.
   */
  shallowCopy() {
    if (typeof this._location === "string") {
      return new LevelDB(
        new Level<string, Uint8Array>(this._location, LEVELDB_ENCODING_OPTS),
        this._location,
      );
    }
    return new LevelDB(this._leveldb);
  }

  /**
   * Returns the underlying LevelDB instance.
   */
  getDatabase() {
    return this._leveldb;
  }
}

export class MptError extends Data.TaggedError(
  "MptError",
)<SDK.GenericErrorFields> {
  /**
   * Builds an error for trie read failures.
   */
  static get(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred getting an entry from ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie insert/update failures.
   */
  static put(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred inserting a new entry in ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie batch-update failures.
   */
  static batch(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred during a batch operation on ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie deletion failures.
   */
  static del(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred deleting an entry from ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie initialization failures.
   */
  static trieCreate(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred creating ${trie} trie: ${cause}`,
      cause,
    });
  }

  /**
   * Builds an error for trie commit failures.
   */
  static trieCommit(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred committing ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie close failures.
   */
  static trieClose(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred closing ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for trie rollback failures.
   */
  static trieRevert(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred reverting ${trie} trie`,
      cause,
    });
  }

  /**
   * Builds an error for missing trie-root state.
   */
  static rootNotSet(trie: string, cause: unknown) {
    return new MptError({
      message: `An error occurred getting ${trie} trie root, the root is ${typeof cause}`,
      cause,
    });
  }
}

export class MidgardMpt {
  public readonly trie: ETH.MerklePatriciaTrie;
  public readonly EMPTY_TRIE_ROOT_HEX: string;
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
    this.EMPTY_TRIE_ROOT_HEX = toHex(trie.EMPTY_TRIE_ROOT);
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
        const db = new LevelDB(level, levelDBFilePath);
        yield* Effect.tryPromise({
          try: () => db.open(),
          catch: (e) => MptError.trieCreate(trieName, e),
        });
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

  /**
   * Deletes the trie backing store when one exists.
   */
  public delete(): Effect.Effect<void, FileSystemError> {
    if (this.databaseAndPath) {
      return Effect.gen(this, function* () {
        yield* Effect.tryPromise({
          try: () => this.databaseAndPath!.database.close(),
          catch: (_e) => _e,
        }).pipe(Effect.catchAll(() => Effect.void));
        yield* deleteMpt(this.databaseAndPath!.databaseFilePath, this.trieName);
      });
    } else {
      return Effect.succeed(Effect.void);
    }
  }

  /**
   * Closes the trie backing store when one exists.
   */
  public close(): Effect.Effect<void, MptError> {
    if (this.databaseAndPath) {
      return Effect.tryPromise({
        try: () => this.databaseAndPath!.database.close(),
        catch: (e) => MptError.trieClose(this.trieName, e),
      });
    }
    return Effect.void;
  }

  /**
   * Executes a batch of LevelDB operations.
   */
  public batch(arg: ETH_UTILS.BatchDBOp[]): Effect.Effect<void, MptError> {
    const trieName = this.trieName;
    const trieBatch = this.trie.batch(arg);
    return Effect.tryPromise({
      try: () => trieBatch,
      catch: (e) => MptError.batch(trieName, e),
    });
  }

  /**
   * Returns the current trie root as a normalized byte array.
   */
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

  /**
   * Returns the current trie root as hexadecimal text.
   */
  public getRootHex(): Effect.Effect<string, MptError> {
    return this.getRoot().pipe(Effect.map(toHex));
  }

  /**
   * Checks whether the trie root matches the empty-root constant.
   */
  public rootIsEmpty(): Effect.Effect<boolean, MptError> {
    const getRootHex = this.getRootHex();
    const emptyRootHex = toHex(this.trie.EMPTY_TRIE_ROOT);
    return Effect.gen(function* () {
      const rootHex = yield* getRootHex;
      return rootHex === emptyRootHex;
    });
  }

  /**
   * Creates a checkpoint on the trie stack.
   */
  public checkpoint(): Effect.Effect<void> {
    return Effect.sync(() => this.trie.checkpoint());
  }

  /**
   * Commits the latest trie checkpoint.
   */
  public commit(): Effect.Effect<void, MptError> {
    return Effect.tryPromise({
      try: () => this.trie.commit(),
      catch: (e) => MptError.trieCommit(this.trieName, e),
    });
  }

  /**
   * Reverts the latest trie checkpoint.
   */
  public revert(): Effect.Effect<void, MptError> {
    return Effect.tryPromise({
      try: () => this.trie.revert(),
      catch: (e) => MptError.trieRevert(this.trieName, e),
    });
  }

  /**
   * Returns the internal database statistics exposed by the trie backend.
   */
  public databaseStats() {
    return this.trie.database()._stats;
  }
}

export const emptyRootHexProgram: Effect.Effect<string, MptError> = Effect.gen(
  function* () {
    const tempMpt = yield* MidgardMpt.create("temp");
    return tempMpt.EMPTY_TRIE_ROOT_HEX;
  },
);
