import { describe, expect, beforeAll } from "vitest";
import fs from "node:fs";
import path from "node:path";
import { toHex } from "@lucid-evolution/lucid";
import { it } from "@effect/vitest";
import { Duration, Effect, Fiber, TestClock } from "effect";
import { SqlClient } from "@effect/sql";
import { Database } from "../src/services/database.js";
import { Globals } from "../src/services/globals.js";
import { NodeConfig } from "../src/services/config.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import {
  // Block
  BlocksDB,

  // Address history
  AddressHistoryDB,

  // Tx
  ImmutableDB,
  ProcessedMempoolDB,
  MempoolDB,

  // Ledger
  LatestLedgerDB,
  MempoolLedgerDB,
  ConfirmedLedgerDB,
  DepositsDB,
  DepositIngestionCursorDB,
  PendingBlockFinalizationsDB,
  MutationJobsDB,
  TxAdmissionsDB,
  StateQueueMutationLeasesDB,

  // Utils
  CommonUtils,
  TxUtils,
  LedgerUtils,
} from "../src/database/index.js";
import { projectDepositsToMempoolLedger } from "../src/fibers/project-deposits-to-mempool-ledger.js";
import { ProcessedTx } from "../src/utils.js";
import { resolveIncludedDepositEntriesForWindow } from "../src/workers/utils/mpf.js";
import {
  deterministicFixtureBytes,
  deterministicFixtureOutputReferenceId,
  deterministicFixtureTxHash,
  expectLedgerUtxos,
  provideDatabaseLayers,
} from "./utils.js";
import {
  cardanoTxBytesToMidgardNativeTxFullBytes,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import {
  DepositStatusCommandError,
  resolveDepositStatusProgram,
} from "../src/commands/deposit-status.js";

const flushAll = Effect.gen(function* () {
  yield* Effect.all(
    [
      MempoolLedgerDB.clear,
      LatestLedgerDB.clear,
      ConfirmedLedgerDB.clear,
      BlocksDB.clear,
      ImmutableDB.clear,
      MempoolDB.clear,
      AddressHistoryDB.clear,
      ProcessedMempoolDB.clear,
      DepositsDB.clear,
      DepositIngestionCursorDB.clear,
      PendingBlockFinalizationsDB.clear,
      CommonUtils.clearTable(TxAdmissionsDB.tableName),
      CommonUtils.clearTable(MutationJobsDB.tableName),
      CommonUtils.clearTable(StateQueueMutationLeasesDB.tableName),
    ],
    { discard: true },
  );
});

const databaseFixtureBytes = (label: string, length: number): Buffer =>
  deterministicFixtureBytes(`database:${label}`, length);

const databaseTxHash = (label: string): Buffer =>
  deterministicFixtureTxHash(`database:${label}`);

const databaseOutputReferenceId = (
  label: string,
  outputIndex: number | bigint = 0n,
): Buffer =>
  deterministicFixtureOutputReferenceId(`database:${label}`, outputIndex);

beforeAll(async () => {
  await Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // Ensure a clean schema: drop tables (and thus indexes) if they exist
        yield* sql`
          DROP SCHEMA public CASCADE;
          CREATE SCHEMA public;`;
        yield* MigrationRunner.migrate({
          appVersion: "test",
          actor: "database.test",
        });
        yield* flushAll;
      }),
    ),
  );
});

describe("Database: initialization and basic operations", () => {
  it.effect("initialize and flush", (_) =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;
        // Smoke select to ensure connection works
        const sql = yield* SqlClient.SqlClient;
        const now = yield* sql<Date>`SELECT NOW()`;
        expect(now.length).toBeGreaterThan(0);
      }),
    ),
  );
});

describe("StateQueueMutationLeasesDB", () => {
  it.effect(
    "returns Busy instead of failing when the state-queue lease is already held",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const first = yield* StateQueueMutationLeasesDB.tryAcquire({
            holder: "first",
          });
          expect(first._tag).toBe("Acquired");
          if (first._tag !== "Acquired") {
            return;
          }

          const second = yield* StateQueueMutationLeasesDB.tryAcquire({
            holder: "second",
          });
          expect(second._tag).toBe("Busy");
          if (second._tag === "Busy") {
            expect(
              second.activeLease?.[StateQueueMutationLeasesDB.Columns.HOLDER],
            ).toBe("first");
          }

          yield* StateQueueMutationLeasesDB.release(first.token);
          const third = yield* StateQueueMutationLeasesDB.tryAcquire({
            holder: "third",
          });
          expect(third._tag).toBe("Acquired");
          if (third._tag === "Acquired") {
            yield* StateQueueMutationLeasesDB.release(third.token);
          }
        }),
      ),
  );

  it.effect(
    "tryWithLease releases successful work and marks failed work without leaving an active lease",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const success = yield* StateQueueMutationLeasesDB.tryWithLease(
            "success",
            (token) => Effect.succeed(token),
          );
          expect(success._tag).toBe("Ran");
          expect(yield* StateQueueMutationLeasesDB.retrieveActive()).toBe(
            undefined,
          );

          const failure = yield* StateQueueMutationLeasesDB.tryWithLease(
            "failure",
            () => Effect.fail(new Error("boom")),
          ).pipe(Effect.either);
          expect(failure._tag).toBe("Left");
          expect(yield* StateQueueMutationLeasesDB.retrieveActive()).toBe(
            undefined,
          );

          const sql = yield* SqlClient.SqlClient;
          const rows = yield* sql<{
            status: StateQueueMutationLeasesDB.Status;
            last_error: string | null;
          }>`SELECT status, last_error FROM ${sql(
            StateQueueMutationLeasesDB.tableName,
          )}
            WHERE holder = ${"failure"}
            ORDER BY acquired_at DESC
            LIMIT 1`;
          expect(rows[0]?.status).toBe(
            StateQueueMutationLeasesDB.Status.Failed,
          );
          expect(rows[0]?.last_error).toContain("boom");
        }),
      ),
  );

  it.effect(
    "tryWithLease keeps long-running state-queue work leased past the initial ttl",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const leaseFiber = yield* StateQueueMutationLeasesDB.tryWithLease(
            "long-running-work",
            (token) =>
              Effect.sleep(Duration.millis(90)).pipe(
                Effect.andThen(StateQueueMutationLeasesDB.revalidate(token)),
                Effect.as(token),
              ),
            {
              ttlMs: 40,
              renewIntervalMs: 10,
            },
          ).pipe(Effect.fork);

          yield* TestClock.adjust(Duration.millis(100));
          const result = yield* Fiber.join(leaseFiber);

          expect(result._tag).toBe("Ran");
          expect(yield* StateQueueMutationLeasesDB.retrieveActive()).toBe(
            undefined,
          );

          const sql = yield* SqlClient.SqlClient;
          const rows = yield* sql<{
            status: StateQueueMutationLeasesDB.Status;
          }>`
            SELECT status FROM ${sql(StateQueueMutationLeasesDB.tableName)}
            WHERE holder = ${"long-running-work"}
            ORDER BY acquired_at DESC
            LIMIT 1`;
          expect(rows[0]?.status).toBe(
            StateQueueMutationLeasesDB.Status.Released,
          );
        }),
      ),
  );
});

describe("BlocksDB", () => {
  it.effect(
    "insert, retrieve all, retrieve by header, retrieve by tx, clear block, clear all",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          // insert with some txs
          yield* BlocksDB.insert(blockHeader1, [tx1, tx2]);
          yield* BlocksDB.insert(blockHeader2, [tx3]);

          // retrieve tx hashes by header
          const txs =
            yield* BlocksDB.retrieveTxHashesByHeaderHash(blockHeader1);
          const txsHex = txs.map((row) => toHex(row));
          expect(new Set(txsHex)).toStrictEqual(
            new Set([toHex(tx1), toHex(tx2)]),
          );

          // retrieve header by tx hash
          const retrievedHeader =
            yield* BlocksDB.retrieveHeaderHashByTxHash(tx1);
          expect(toHex(retrievedHeader)).toEqual(toHex(blockHeader1));

          // retrieve all
          const all = yield* BlocksDB.retrieve;
          expect(
            new Set(
              all.map((a) => ({
                [BlocksDB.Columns.HEADER_HASH]: a[BlocksDB.Columns.HEADER_HASH],
                [BlocksDB.Columns.TX_ID]: a[BlocksDB.Columns.TX_ID],
              })),
            ),
          ).toStrictEqual(
            new Set([
              {
                [BlocksDB.Columns.HEADER_HASH]: blockHeader1,
                [BlocksDB.Columns.TX_ID]: tx1,
              },
              {
                [BlocksDB.Columns.HEADER_HASH]: blockHeader1,
                [BlocksDB.Columns.TX_ID]: tx2,
              },
              {
                [BlocksDB.Columns.HEADER_HASH]: blockHeader2,
                [BlocksDB.Columns.TX_ID]: tx3,
              },
            ]),
          );

          //clear block
          yield* BlocksDB.clearBlock(blockHeader1);
          const afterClear = yield* BlocksDB.retrieve;
          expect(
            new Set(
              afterClear.map((a) => ({
                [BlocksDB.Columns.HEADER_HASH]: a[BlocksDB.Columns.HEADER_HASH],
                [BlocksDB.Columns.TX_ID]: a[BlocksDB.Columns.TX_ID],
              })),
            ),
          ).toStrictEqual(
            new Set([
              {
                [BlocksDB.Columns.HEADER_HASH]: blockHeader2,
                [BlocksDB.Columns.TX_ID]: tx3,
              },
            ]),
          );

          // clear all
          yield* BlocksDB.clear;
          const afterClearAll = yield* BlocksDB.retrieve;
          expect(afterClearAll.length).toEqual(0);
        }),
      ),
  );
});

describe("MempoolDB", () => {
  it.effect(
    "insert, retrieve single, retrieve all, retrieve cbor by hash, retrieve cbors by hashes, retrieve count, clear txs, clear all",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const pTxId1 = databaseTxHash("mempool.tx-1");
          const pTx1 = databaseFixtureBytes("mempool.tx-1-cbor", 64);
          const pSpent1 = databaseFixtureBytes("mempool.tx-1-spent", 32);
          const processedTx1: ProcessedTx = {
            txId: pTxId1,
            txCbor: pTx1,
            spent: [pSpent1],
            produced: [ledgerEntry1],
          };
          const pTxId2 = databaseTxHash("mempool.tx-2");
          const pTx2 = databaseFixtureBytes("mempool.tx-2-cbor", 64);
          const pSpent2 = databaseFixtureBytes("mempool.tx-2-spent", 32);
          const processedTx2: ProcessedTx = {
            txId: pTxId2,
            txCbor: pTx2,
            spent: [pSpent2],
            produced: [ledgerEntry2],
          };

          // insert multiple
          yield* MempoolDB.insertMultiple([processedTx1, processedTx2]);

          // retrieve tx cbor by hash
          const gotOne = yield* MempoolDB.retrieveTxCborByHash(pTxId1);
          expect(toHex(gotOne)).toEqual(toHex(pTx1));

          // retrieve tx cbor by hashes
          const gotMany = yield* MempoolDB.retrieveTxCborsByHashes([
            pTxId1,
            pTxId2,
          ]);
          expect(new Set(gotMany.map((r) => toHex(r)))).toStrictEqual(
            new Set([toHex(pTx1), toHex(pTx2)]),
          );

          // retrieve all
          const gotAll = yield* MempoolDB.retrieve;
          expect(
            new Set(gotAll.map((e) => removeTimestampFromTxEntry(e))),
          ).toStrictEqual(
            new Set([
              {
                [TxUtils.Columns.TX_ID]: pTxId1,
                [TxUtils.Columns.TX]: pTx1,
              },
              {
                [TxUtils.Columns.TX_ID]: pTxId2,
                [TxUtils.Columns.TX]: pTx2,
              },
            ]),
          );

          // retrieve count
          const gotCount: bigint = yield* MempoolDB.retrieveTxCount;
          expect(gotCount).toEqual(2n);

          // clearTxs
          yield* MempoolDB.clearTxs([pTxId1]);
          const afterClear = yield* MempoolDB.retrieve;
          expect(
            new Set(afterClear.map((e) => removeTimestampFromTxEntry(e))),
          ).toStrictEqual(
            new Set([
              {
                [TxUtils.Columns.TX_ID]: pTxId2,
                [TxUtils.Columns.TX]: pTx2,
              },
            ]),
          );

          // clearAll
          yield* MempoolDB.clear;
          const afterClearAll = yield* MempoolDB.retrieve;
          expect(afterClearAll.length).toEqual(0);

          // insert single
          yield* flushAll;
          yield* MempoolDB.insert(processedTx1);
          const afterInsertOne = yield* MempoolDB.retrieve;
          expect(
            afterInsertOne.map((e) => removeTimestampFromTxEntry(e)),
          ).toStrictEqual([
            {
              [TxUtils.Columns.TX_ID]: pTxId1,
              [TxUtils.Columns.TX]: pTx1,
            },
          ]);
        }),
      ),
  );
});

describe("ProcessedMempoolDB", () => {
  it.effect(
    "insert tx, insert txs, retrieve all, retrieve cbor by hash, retrieve cbors by hashes, clear all",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          // insert txs
          yield* ProcessedMempoolDB.insertTxs([txEntry1, txEntry2]);

          // retrieve tx cbor by hash
          const gotOne = yield* ProcessedMempoolDB.retrieveTxCborByHash(txId1);
          expect(toHex(gotOne)).toEqual(toHex(tx1));

          // retrieve tx cbors by hashes
          const gotMany = yield* ProcessedMempoolDB.retrieveTxCborsByHashes([
            txId1,
            txId2,
          ]);
          expect(new Set(gotMany.map((r) => toHex(r)))).toStrictEqual(
            new Set([toHex(tx1), toHex(tx2)]),
          );

          // retrieve all
          const gotAll = yield* ProcessedMempoolDB.retrieve;
          expect(
            new Set(gotAll.map((e) => removeTimestampFromTxEntry(e))),
          ).toStrictEqual(
            new Set([
              {
                [TxUtils.Columns.TX_ID]: txId1,
                [TxUtils.Columns.TX]: tx1,
              },
              {
                [TxUtils.Columns.TX_ID]: txId2,
                [TxUtils.Columns.TX]: tx2,
              },
            ]),
          );

          // clear all
          yield* ProcessedMempoolDB.clear;
          const afterClearAll = yield* ProcessedMempoolDB.retrieve;
          expect(afterClearAll.length).toEqual(0);

          // insert single
          yield* ProcessedMempoolDB.insertTx(txEntry1);
          const afterInsertOne = yield* ProcessedMempoolDB.retrieve;
          expect(
            afterInsertOne.map((e) => removeTimestampFromTxEntry(e)),
          ).toStrictEqual([
            {
              [TxUtils.Columns.TX_ID]: txId1,
              [TxUtils.Columns.TX]: tx1,
            },
          ]);
        }),
      ),
  );
});

describe("ImmutableDB", () => {
  it.effect(
    "insert tx, insert txs, retrieve all, retrieve cbor by hash, retrieve cbor by hashes, clear all",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          // insert txs
          yield* ImmutableDB.insertTxs([txEntry1, txEntry2]);

          // retrieve tx cbor by hash
          const gotOne = yield* ImmutableDB.retrieveTxCborByHash(txId1);
          expect(toHex(gotOne)).toEqual(toHex(tx1));

          // retrieve tx cbors by hashes
          const gotMany = yield* ImmutableDB.retrieveTxCborsByHashes([
            txId1,
            txId2,
          ]);
          expect(new Set(gotMany.map((r) => toHex(r)))).toStrictEqual(
            new Set([toHex(tx1), toHex(tx2)]),
          );

          // retrieve all
          const gotAll: readonly TxUtils.EntryWithTimeStamp[] =
            yield* ImmutableDB.retrieve;
          expect(
            new Set(
              gotAll.map((e: TxUtils.EntryWithTimeStamp) =>
                removeTimestampFromTxEntry(e),
              ),
            ),
          ).toStrictEqual(
            new Set([
              {
                [TxUtils.Columns.TX_ID]: txId1,
                [TxUtils.Columns.TX]: tx1,
              },
              {
                [TxUtils.Columns.TX_ID]: txId2,
                [TxUtils.Columns.TX]: tx2,
              },
            ]),
          );

          // clear all
          yield* ImmutableDB.clear;
          const afterClearAll = yield* ImmutableDB.retrieve;
          expect(afterClearAll.length).toEqual(0);

          // insert single
          yield* ImmutableDB.insertTx(txEntry1);
          const afterInsertOne = yield* ImmutableDB.retrieve;
          expect(
            afterInsertOne.map((e) => removeTimestampFromTxEntry(e)),
          ).toStrictEqual([
            {
              [TxUtils.Columns.TX_ID]: txId1,
              [TxUtils.Columns.TX]: tx1,
            },
          ]);
        }),
      ),
  );

  it.effect("insertTxsValidatedNative accepts valid native payloads", (_) =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;
        const valid = makeValidNativeImmutableEntry();

        yield* ImmutableDB.insertTxsValidatedNative([valid]);
        const stored = yield* ImmutableDB.retrieve;
        expect(stored).toHaveLength(1);
        expect(
          stored[0][TxUtils.Columns.TX_ID].equals(valid[TxUtils.Columns.TX_ID]),
        ).toBe(true);
      }),
    ),
  );

  it.effect(
    "insertTxsValidatedNative rejects malformed or mismatched payloads",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;
          const valid = makeValidNativeImmutableEntry();
          const mismatchedTxId = Buffer.from(valid[TxUtils.Columns.TX_ID]);
          mismatchedTxId[0] ^= 0xff;
          const malformed: TxUtils.Entry = {
            [TxUtils.Columns.TX_ID]: Buffer.alloc(32, 7),
            [TxUtils.Columns.TX]: Buffer.alloc(64, 1),
          };
          const mismatch: TxUtils.Entry = {
            [TxUtils.Columns.TX_ID]: mismatchedTxId,
            [TxUtils.Columns.TX]: valid[TxUtils.Columns.TX],
          };

          const malformedResult = yield* Effect.either(
            ImmutableDB.insertTxsValidatedNative([malformed]),
          );
          expect(malformedResult._tag).toBe("Left");
          if (malformedResult._tag === "Left") {
            expect(malformedResult.left.message).toContain(
              "Failed native tx payload validation for immutable insertion",
            );
          }

          const mismatchResult = yield* Effect.either(
            ImmutableDB.insertTxsValidatedNative([mismatch]),
          );
          expect(mismatchResult._tag).toBe("Left");
          if (mismatchResult._tag === "Left") {
            expect(mismatchResult.left.message).toContain(
              "Failed native tx payload validation for immutable insertion",
            );
          }

          const remaining = yield* ImmutableDB.retrieve;
          expect(remaining).toHaveLength(0);
        }),
      ),
  );
});

describe("LatestLedgerDB", () => {
  it.effect("insert multiple, retrieve, clear UTxOs, clear all", () =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        // insert multiple
        yield* LatestLedgerDB.insertMultiple([ledgerEntry1, ledgerEntry2]);

        // retrieve all
        const all = yield* LatestLedgerDB.retrieve;
        expectLedgerUtxos(all, [ledgerEntry1, ledgerEntry2]);

        // clear UTxOs
        yield* LatestLedgerDB.clearUTxOs([
          ledgerEntry1[LedgerUtils.Columns.OUTREF],
        ]);
        const afterClear = yield* LatestLedgerDB.retrieve;
        expectLedgerUtxos(afterClear, [ledgerEntry2]);

        // clear all
        yield* LatestLedgerDB.clear;
        const afterClearAll = yield* LatestLedgerDB.retrieve;
        expect(afterClearAll.length).toEqual(0);
      }),
    ),
  );
});

describe("MempoolLedgerDB", () => {
  it.effect(
    "insert, retrieve by address, retrieve by outrefs, retrieve all, clearUTxOs, clearAll",
    () =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          // insert
          yield* MempoolLedgerDB.insert([ledgerEntry1, ledgerEntry2]);

          // retrieve by address
          const atAddress = yield* MempoolLedgerDB.retrieveByAddress(address1);
          expect(
            new Set(atAddress.map((e) => removeTimestampFromLedgerEntry(e))),
          ).toStrictEqual(new Set([ledgerEntry1]));

          // retrieve by outrefs
          const byOutRefs = yield* MempoolLedgerDB.retrieveByTxOutRefs([
            ledgerEntry2[LedgerUtils.Columns.OUTREF],
            databaseFixtureBytes("mempool-ledger.missing-outref", 36),
          ]);
          expect(
            new Set(byOutRefs.map((e) => removeTimestampFromLedgerEntry(e))),
          ).toStrictEqual(new Set([ledgerEntry2]));

          // retrieve by empty outref set
          const emptyOutRefs = yield* MempoolLedgerDB.retrieveByTxOutRefs([]);
          expect(emptyOutRefs).toStrictEqual([]);

          // retrieve all
          const all = yield* MempoolLedgerDB.retrieve;
          expect(
            new Set(all.map((e) => removeTimestampFromLedgerEntry(e))),
          ).toStrictEqual(new Set([ledgerEntry1, ledgerEntry2]));

          // clear UTxOs
          yield* MempoolLedgerDB.clearUTxOs([
            ledgerEntry1[LedgerUtils.Columns.OUTREF],
          ]);
          const afterClear = yield* MempoolLedgerDB.retrieve;
          expect(
            new Set(afterClear.map((e) => removeTimestampFromLedgerEntry(e))),
          ).toStrictEqual(new Set([ledgerEntry2]));

          // clear all
          yield* MempoolLedgerDB.clear;
          const afterClearAll = yield* MempoolLedgerDB.retrieve;
          expect(afterClearAll.length).toEqual(0);
        }),
      ),
  );
});

describe("ConfirmedLedgerDB", () => {
  it.effect("insert multiple, retrieve", () =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        // insert
        yield* ConfirmedLedgerDB.insertMultiple([ledgerEntry1, ledgerEntry2]);

        // retrieve all
        const all = yield* ConfirmedLedgerDB.retrieve;
        expect(
          new Set(all.map((e) => removeTimestampFromLedgerEntry(e))),
        ).toStrictEqual(new Set([ledgerEntry1, ledgerEntry2]));

        // clear UTxOs
        yield* ConfirmedLedgerDB.clearUTxOs([
          ledgerEntry1[LedgerUtils.Columns.OUTREF],
        ]);
        const afterClear = yield* ConfirmedLedgerDB.retrieve;
        expect(
          new Set(afterClear.map((e) => removeTimestampFromLedgerEntry(e))),
        ).toStrictEqual(new Set([ledgerEntry2]));

        // clear all
        yield* ConfirmedLedgerDB.clear;
        const afterClearAll = yield* ConfirmedLedgerDB.retrieve;
        expect(afterClearAll.length).toEqual(0);
      }),
    ),
  );
});

describe("AddressHistoryDB", () => {
  it.effect("insert, retrieve, clears tx hash, clear all", () =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const pTxId1 = databaseTxHash("address-history.tx-1");
        const pTx1 = databaseFixtureBytes("address-history.tx-1-cbor", 64);
        const pSpent1 = databaseFixtureBytes("address-history.tx-1-spent", 32);
        const processedTx1: ProcessedTx = {
          txId: pTxId1,
          txCbor: pTx1,
          spent: [pSpent1],
          produced: [ledgerEntry1],
        };
        const ahEntry1: AddressHistoryDB.Entry = {
          [LedgerUtils.Columns.TX_ID]: pTxId1,
          [LedgerUtils.Columns.ADDRESS]: address1,
        };
        const pTxId2 = databaseTxHash("address-history.tx-2");
        const pTx2 = databaseFixtureBytes("address-history.tx-2-cbor", 64);
        const pSpent2 = databaseFixtureBytes("address-history.tx-2-spent", 32);
        const processedTx2: ProcessedTx = {
          txId: pTxId2,
          txCbor: pTx2,
          spent: [pSpent2],
          produced: [ledgerEntry2],
        };
        const ahEntry2: AddressHistoryDB.Entry = {
          [LedgerUtils.Columns.TX_ID]: pTxId2,
          [LedgerUtils.Columns.ADDRESS]: address2,
        };

        // via mempool
        // insert
        yield* MempoolDB.insertMultiple([processedTx1, processedTx2]);
        yield* AddressHistoryDB.insertEntries([ahEntry1, ahEntry2]);

        // retrieve
        const expectedViaMempool = yield* AddressHistoryDB.retrieve(address1);
        expect(expectedViaMempool.map((t) => toHex(t))).toStrictEqual([
          toHex(pTx1),
        ]);

        // clears tx hash
        yield* AddressHistoryDB.delTxHash(pTxId1);
        const afterClear = yield* AddressHistoryDB.retrieve(address1);
        expect(afterClear).toStrictEqual([]);

        //clears all
        yield* AddressHistoryDB.clear;
        const afterClearAll1 = yield* AddressHistoryDB.retrieve(address1);
        const afterClearAll2 = yield* AddressHistoryDB.retrieve(address2);
        expect([...afterClearAll1, ...afterClearAll2]).toStrictEqual([]);

        // via immutable
        const txEntry1: TxUtils.Entry = {
          [TxUtils.Columns.TX_ID]: pTxId1,
          [TxUtils.Columns.TX]: pTx1,
        };
        const txEntry2: TxUtils.Entry = {
          [TxUtils.Columns.TX_ID]: pTxId2,
          [TxUtils.Columns.TX]: pTx2,
        };
        yield* flushAll;

        // insert
        yield* ImmutableDB.insertTxs([txEntry1, txEntry2]);
        yield* AddressHistoryDB.insertEntries([ahEntry1, ahEntry2]);

        // retrieve
        const expectedViaImmutable = yield* AddressHistoryDB.retrieve(address1);
        expect(expectedViaImmutable.map((t) => toHex(t))).toStrictEqual([
          toHex(pTx1),
        ]);

        // clears tx hash
        yield* AddressHistoryDB.delTxHash(pTxId1);
        const afterClearImmutable = yield* AddressHistoryDB.retrieve(address1);
        expect(afterClearImmutable).toStrictEqual([]);

        //clears all
        yield* AddressHistoryDB.clear;
        const afterClearAllImmutable1 =
          yield* AddressHistoryDB.retrieve(address1);
        const afterClearAllImmutable2 =
          yield* AddressHistoryDB.retrieve(address2);
        expect([
          ...afterClearAllImmutable1,
          ...afterClearAllImmutable2,
        ]).toStrictEqual([]);
      }),
    ),
  );

  it.effect("submit tx pipeline inserts a tx id in address db history", () =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;
        const thisWalletAddress = address2;
        const firstTxId = databaseTxHash("address-history.pipeline.tx-1");
        const firstProcessedTx: ProcessedTx = {
          txId: firstTxId,
          txCbor: databaseFixtureBytes(
            "address-history.pipeline.tx-1-cbor",
            64,
          ),
          spent: [
            databaseFixtureBytes("address-history.pipeline.tx-1-spent", 36),
          ],
          produced: [
            {
              [LedgerUtils.Columns.TX_ID]: firstTxId,
              [LedgerUtils.Columns.OUTREF]: databaseFixtureBytes(
                "address-history.pipeline.tx-1-output-1-outref",
                36,
              ),
              [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                "address-history.pipeline.tx-1-output-1",
                80,
              ),
              [LedgerUtils.Columns.ADDRESS]: address1,
            },
            {
              [LedgerUtils.Columns.TX_ID]: firstTxId,
              [LedgerUtils.Columns.OUTREF]: databaseFixtureBytes(
                "address-history.pipeline.tx-1-output-2-outref",
                36,
              ),
              [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                "address-history.pipeline.tx-1-output-2",
                80,
              ),
              [LedgerUtils.Columns.ADDRESS]: thisWalletAddress,
            },
          ],
        };
        yield* MempoolDB.insertMultiple([firstProcessedTx]);

        const sql = yield* SqlClient.SqlClient;
        const result1 =
          yield* sql<AddressHistoryDB.Entry>`SELECT * FROM address_history`;
        expect(
          result1.map((r) => r[LedgerUtils.Columns.ADDRESS]).sort(),
        ).toStrictEqual([address1, thisWalletAddress].sort());

        // two outputs for the same address should still produce one unique row
        yield* flushAll;
        const secondTxId = databaseTxHash("address-history.pipeline.tx-2");
        const secondProcessedTx: ProcessedTx = {
          txId: secondTxId,
          txCbor: databaseFixtureBytes(
            "address-history.pipeline.tx-2-cbor",
            64,
          ),
          spent: [
            databaseFixtureBytes("address-history.pipeline.tx-2-spent", 36),
          ],
          produced: [
            {
              [LedgerUtils.Columns.TX_ID]: secondTxId,
              [LedgerUtils.Columns.OUTREF]: databaseFixtureBytes(
                "address-history.pipeline.tx-2-output-1-outref",
                36,
              ),
              [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                "address-history.pipeline.tx-2-output-1",
                80,
              ),
              [LedgerUtils.Columns.ADDRESS]: thisWalletAddress,
            },
            {
              [LedgerUtils.Columns.TX_ID]: secondTxId,
              [LedgerUtils.Columns.OUTREF]: databaseFixtureBytes(
                "address-history.pipeline.tx-2-output-2-outref",
                36,
              ),
              [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                "address-history.pipeline.tx-2-output-2",
                80,
              ),
              [LedgerUtils.Columns.ADDRESS]: thisWalletAddress,
            },
          ],
        };
        yield* MempoolDB.insertMultiple([secondProcessedTx]);

        const result2 =
          yield* sql<AddressHistoryDB.Entry>`SELECT * FROM address_history`;
        expect(
          result2.map((r) => r[LedgerUtils.Columns.ADDRESS]),
        ).toStrictEqual([thisWalletAddress]);
      }),
    ),
  );
});

describe("DepositsDB and MempoolLedgerDB exact-once projection", () => {
  it.effect("rejects payload drift for the same deposit event_id", (_) =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const eventId = databaseOutputReferenceId("deposits.payload-drift", 0);
        const first = makeDepositEntry({
          [DepositsDB.Columns.ID]: eventId,
        });
        const conflicting = makeDepositEntry({
          [DepositsDB.Columns.ID]: eventId,
          [DepositsDB.Columns.INFO]: databaseFixtureBytes(
            "deposits.payload-drift.conflicting-info",
            48,
          ),
        });

        yield* DepositsDB.insertEntries([first]);
        const result = yield* Effect.either(
          DepositsDB.insertEntries([conflicting]),
        );

        expect(result._tag).toEqual("Left");
      }),
    ),
  );

  it.effect("retrieves one deposit by event_id", (_) =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const deposit = makeDepositEntry();
        yield* DepositsDB.insertEntries([deposit]);

        const retrieved = yield* DepositsDB.retrieveByEventId(
          deposit[DepositsDB.Columns.ID],
        );
        expect(retrieved._tag).toEqual("Some");
        if (retrieved._tag !== "Some") {
          throw new Error("expected deposit lookup to return a row");
        }

        expect(
          retrieved.value[DepositsDB.Columns.ID].equals(
            deposit[DepositsDB.Columns.ID],
          ),
        ).toEqual(true);
        expect(
          retrieved.value[DepositsDB.Columns.DEPOSIT_L1_TX_HASH].equals(
            deposit[DepositsDB.Columns.DEPOSIT_L1_TX_HASH],
          ),
        ).toEqual(true);
      }),
    ),
  );

  it.effect(
    "retrieves deposits by Cardano tx hash in deterministic order",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const sharedCardanoTxHash = databaseTxHash(
            "deposits.shared-cardano-tx.deterministic-order",
          );
          const first = makeDepositEntry({
            [DepositsDB.Columns.INCLUSION_TIME]: new Date(
              "2026-04-13T17:00:00.000Z",
            ),
            [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
            [DepositsDB.Columns.ID]: databaseOutputReferenceId(
              "deposits.deterministic-order.first",
              0,
            ),
          });
          const second = makeDepositEntry({
            [DepositsDB.Columns.INCLUSION_TIME]: new Date(
              "2026-04-13T17:00:01.000Z",
            ),
            [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
            [DepositsDB.Columns.ID]: databaseOutputReferenceId(
              "deposits.deterministic-order.second",
              1,
            ),
          });
          yield* DepositsDB.insertEntries([second, first]);

          const retrieved =
            yield* DepositsDB.retrieveByCardanoTxHash(sharedCardanoTxHash);

          expect(retrieved).toHaveLength(2);
          expect(
            retrieved[0]?.[DepositsDB.Columns.ID].equals(
              first[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);
          expect(
            retrieved[1]?.[DepositsDB.Columns.ID].equals(
              second[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);
        }),
      ),
  );

  it.effect(
    "rejects ambiguous cardanoTxHash lookups and requires eventId to disambiguate",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const sharedCardanoTxHash = databaseTxHash(
            "deposits.shared-cardano-tx.ambiguous-status",
          );
          yield* DepositsDB.insertEntries([
            makeDepositEntry({
              [DepositsDB.Columns.ID]: databaseOutputReferenceId(
                "deposits.ambiguous.first",
                0,
              ),
              [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
            }),
            makeDepositEntry({
              [DepositsDB.Columns.ID]: databaseOutputReferenceId(
                "deposits.ambiguous.second",
                1,
              ),
              [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
            }),
          ]);

          const result = yield* Effect.either(
            resolveDepositStatusProgram({
              cardanoTxHash: sharedCardanoTxHash,
            }),
          );

          expect(result._tag).toEqual("Left");
          if (result._tag !== "Left") {
            throw new Error("expected ambiguous lookup to fail");
          }
          expect(result.left).toBeInstanceOf(DepositStatusCommandError);
          if (!(result.left instanceof DepositStatusCommandError)) {
            throw new Error("expected DepositStatusCommandError");
          }
          expect(result.left.status).toEqual(409);
        }),
      ),
  );

  it.effect(
    "allows eventId to disambiguate a shared cardanoTxHash lookup",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const sharedCardanoTxHash = databaseTxHash(
            "deposits.shared-cardano-tx.event-id-disambiguates",
          );
          const first = makeDepositEntry({
            [DepositsDB.Columns.ID]: databaseOutputReferenceId(
              "deposits.disambiguates.first",
              0,
            ),
            [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
          });
          const second = makeDepositEntry({
            [DepositsDB.Columns.ID]: databaseOutputReferenceId(
              "deposits.disambiguates.second",
              1,
            ),
            [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
          });
          yield* DepositsDB.insertEntries([first, second]);

          const resolved = yield* resolveDepositStatusProgram({
            eventId: second[DepositsDB.Columns.ID],
            cardanoTxHash: sharedCardanoTxHash,
          });

          expect(
            resolved[DepositsDB.Columns.ID].equals(
              second[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);
        }),
      ),
  );

  it.effect(
    "projects a deposit into mempool_ledger exactly once by source_event_id",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const deposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([deposit]);
          const mempoolEntry = yield* DepositsDB.toMempoolLedgerEntry(deposit);

          yield* MempoolLedgerDB.insertDepositEntriesStrict([mempoolEntry]);
          yield* DepositsDB.markAwaitingAsProjected([
            deposit[DepositsDB.Columns.ID],
          ]);

          const duplicateResult = yield* Effect.either(
            MempoolLedgerDB.insertDepositEntriesStrict([mempoolEntry]),
          );
          expect(duplicateResult._tag).toEqual("Left");

          const mempoolRows = yield* MempoolLedgerDB.retrieve;
          expect(mempoolRows).toHaveLength(1);
          expect(
            mempoolRows[0]?.[MempoolLedgerDB.Columns.SOURCE_EVENT_ID]?.equals(
              deposit[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);

          const projectedRows = yield* DepositsDB.retrieveProjectedEntries();
          expect(projectedRows).toHaveLength(1);
          expect(projectedRows[0]?.[DepositsDB.Columns.STATUS]).toEqual(
            DepositsDB.Status.Projected,
          );
        }),
      ),
  );

  it.effect("projects only deposits whose inclusion time has arrived", (_) =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const pastDeposit = makeDepositEntry({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(
            "2020-01-01T00:00:00.000Z",
          ),
        });
        const futureDeposit = makeDepositEntry({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(
            "2099-01-01T00:00:00.000Z",
          ),
        });
        yield* DepositsDB.insertEntries([pastDeposit, futureDeposit]);

        yield* projectDepositsToMempoolLedger.pipe(
          Effect.provide(Globals.Default),
        );

        const projectedRows = yield* DepositsDB.retrieveProjectedEntries();
        expect(projectedRows).toHaveLength(1);
        expect(
          projectedRows[0]?.[DepositsDB.Columns.ID].equals(
            pastDeposit[DepositsDB.Columns.ID],
          ),
        ).toEqual(true);

        const awaitingRows = yield* DepositsDB.retrieveAwaitingEntries();
        expect(awaitingRows).toHaveLength(1);
        expect(
          awaitingRows[0]?.[DepositsDB.Columns.ID].equals(
            futureDeposit[DepositsDB.Columns.ID],
          ),
        ).toEqual(true);
      }),
    ),
  );

  it.effect(
    "rejects source_event_id payload drift during idempotent projection reconciliation",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const deposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([deposit]);
          const mempoolEntry = yield* DepositsDB.toMempoolLedgerEntry(deposit);
          yield* MempoolLedgerDB.insertDepositEntriesStrict([mempoolEntry]);

          const conflictingEntry = {
            ...mempoolEntry,
            [MempoolLedgerDB.Columns.OUTPUT]: databaseFixtureBytes(
              "deposits.projection-reconciliation.conflicting-output",
              80,
            ),
          };

          const result = yield* Effect.either(
            MempoolLedgerDB.reconcileDepositEntries([conflictingEntry]),
          );
          expect(result._tag).toEqual("Left");
        }),
      ),
  );

  it.effect(
    "assigns and clears a projected header hash for a projected deposit",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const deposit = makeDepositEntry();
          const headerHash = databaseFixtureBytes(
            "deposits.projected-header",
            28,
          );
          yield* DepositsDB.insertEntries([deposit]);
          yield* DepositsDB.markAwaitingAsProjected([
            deposit[DepositsDB.Columns.ID],
          ]);
          yield* DepositsDB.markProjectedByEventIds(
            [deposit[DepositsDB.Columns.ID]],
            headerHash,
          );

          const assignedRows = yield* DepositsDB.retrieveAllEntries();
          expect(
            assignedRows[0]?.[DepositsDB.Columns.PROJECTED_HEADER_HASH]?.equals(
              headerHash,
            ),
          ).toEqual(true);

          yield* DepositsDB.clearProjectedHeaderAssignmentByEventIds(
            [deposit[DepositsDB.Columns.ID]],
            headerHash,
          );

          const clearedRows = yield* DepositsDB.retrieveAllEntries();
          expect(
            clearedRows[0]?.[DepositsDB.Columns.PROJECTED_HEADER_HASH],
          ).toBeNull();
        }),
      ),
  );

  it.effect(
    "treats projection claiming as idempotent once a deposit is already projected",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const deposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([deposit]);
          yield* DepositsDB.markAwaitingAsProjected([
            deposit[DepositsDB.Columns.ID],
          ]);
          yield* DepositsDB.markAwaitingAsProjected([
            deposit[DepositsDB.Columns.ID],
          ]);

          const rows = yield* DepositsDB.retrieveAllEntries();
          expect(rows).toHaveLength(1);
          expect(rows[0]?.[DepositsDB.Columns.STATUS]).toEqual(
            DepositsDB.Status.Projected,
          );
          expect(
            rows[0]?.[DepositsDB.Columns.PROJECTED_HEADER_HASH],
          ).toBeNull();
        }),
      ),
  );

  it.effect(
    "re-includes an overdue projected deposit whose earlier header assignment was abandoned",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const currentBlockStartTime = new Date("2026-04-13T17:28:10.000Z");
          const overdueProjectedDeposit = makeDepositEntry({
            [DepositsDB.Columns.INCLUSION_TIME]: new Date(
              currentBlockStartTime.getTime() - 1_000,
            ),
            [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
          });
          yield* DepositsDB.insertEntries([overdueProjectedDeposit]);

          const included = yield* resolveIncludedDepositEntriesForWindow({
            currentBlockStartTime,
            effectiveEndTime: new Date(currentBlockStartTime.getTime() + 1_000),
          });

          expect(included).toHaveLength(1);
          expect(
            included[0]?.[DepositsDB.Columns.ID].equals(
              overdueProjectedDeposit[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);
          expect(included[0]?.[DepositsDB.Columns.STATUS]).toEqual(
            DepositsDB.Status.Projected,
          );
        }),
      ),
  );

  it.effect(
    "fails closed when an overdue deposit was never projected before its window closed",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const currentBlockStartTime = new Date("2026-04-13T17:28:10.000Z");
          const overdueAwaitingDeposit = makeDepositEntry({
            [DepositsDB.Columns.INCLUSION_TIME]: new Date(
              currentBlockStartTime.getTime() - 1_000,
            ),
          });
          yield* DepositsDB.insertEntries([overdueAwaitingDeposit]);

          const result = yield* Effect.either(
            resolveIncludedDepositEntriesForWindow({
              currentBlockStartTime,
              effectiveEndTime: new Date(
                currentBlockStartTime.getTime() + 1_000,
              ),
            }),
          );

          expect(result._tag).toEqual("Left");
        }),
      ),
  );
});

const blockHeader1 = databaseFixtureBytes("blocks.header-1", 32);
const blockHeader2 = databaseFixtureBytes("blocks.header-2", 32);

const txId1 = databaseTxHash("shared.tx-1");
const txId2 = databaseTxHash("shared.tx-2");

const tx1 = databaseFixtureBytes("shared.tx-1-cbor", 64);
const tx2 = databaseFixtureBytes("shared.tx-2-cbor", 64);
const tx3 = databaseFixtureBytes("shared.tx-3-cbor", 64);

const outref1 = databaseFixtureBytes("shared.outref-1", 36);
const outref2 = databaseFixtureBytes("shared.outref-2", 36);

const output1 = databaseFixtureBytes("shared.output-1", 80);
const output2 = databaseFixtureBytes("shared.output-2", 80);

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const firstFixture = (
  JSON.parse(fs.readFileSync(fixturePath, "utf8")) as readonly TxFixture[]
)[0];

const makeValidNativeImmutableEntry = (): TxUtils.Entry => {
  const nativeTx = cardanoTxBytesToMidgardNativeTxFullBytes(
    Buffer.from(firstFixture.cborHex, "hex"),
  );
  const txId = computeMidgardNativeTxId(decodeMidgardNativeTxFull(nativeTx));
  return {
    [TxUtils.Columns.TX_ID]: txId,
    [TxUtils.Columns.TX]: nativeTx,
  };
};

const address1 =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const address2 =
  "addr_test1vzcsc5wzu3vsnjek2n80ayce53r4ha2g6wyetqddrp8z04q3yzv6k";

const txEntry1: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId1,
  [TxUtils.Columns.TX]: tx1,
};

const txEntry2: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId2,
  [TxUtils.Columns.TX]: tx2,
};

const removeTimestampFromTxEntry = (
  e: TxUtils.Entry,
): TxUtils.EntryNoTimeStamp => {
  return {
    [TxUtils.Columns.TX_ID]: e[TxUtils.Columns.TX_ID],
    [TxUtils.Columns.TX]: e[TxUtils.Columns.TX],
  };
};

const ledgerEntry1: LedgerUtils.Entry = {
  [LedgerUtils.Columns.TX_ID]: txId1,
  [LedgerUtils.Columns.OUTREF]: outref1,
  [LedgerUtils.Columns.OUTPUT]: output1,
  [LedgerUtils.Columns.ADDRESS]: address1,
};

const ledgerEntry2: LedgerUtils.Entry = {
  [LedgerUtils.Columns.TX_ID]: txId2,
  [LedgerUtils.Columns.OUTREF]: outref2,
  [LedgerUtils.Columns.OUTPUT]: output2,
  [LedgerUtils.Columns.ADDRESS]: address2,
};

const removeTimestampFromLedgerEntry = (
  e: LedgerUtils.Entry,
): LedgerUtils.EntryNoTimeStamp => {
  return {
    [LedgerUtils.Columns.TX_ID]: e[LedgerUtils.Columns.TX_ID],
    [LedgerUtils.Columns.OUTREF]: e[LedgerUtils.Columns.OUTREF],
    [LedgerUtils.Columns.OUTPUT]: e[LedgerUtils.Columns.OUTPUT],
    [LedgerUtils.Columns.ADDRESS]: e[LedgerUtils.Columns.ADDRESS],
  };
};

const depositFixtureBaseTimeMs = Date.parse("2026-04-13T17:28:10.000Z");
let depositFixtureSequence = 0;

const makeDepositEntry = (
  overrides: Partial<DepositsDB.Entry> = {},
): DepositsDB.Entry => {
  const fixtureIndex = depositFixtureSequence;
  depositFixtureSequence += 1;
  const fixtureLabel = `entry-${fixtureIndex.toString().padStart(4, "0")}`;
  const eventId =
    overrides[DepositsDB.Columns.ID] ??
    databaseOutputReferenceId(`deposits.${fixtureLabel}`, fixtureIndex);
  return {
    [DepositsDB.Columns.ID]: eventId,
    [DepositsDB.Columns.INFO]:
      overrides[DepositsDB.Columns.INFO] ??
      databaseFixtureBytes(`deposits.${fixtureLabel}.info`, 48),
    [DepositsDB.Columns.INCLUSION_TIME]:
      overrides[DepositsDB.Columns.INCLUSION_TIME] ??
      new Date(depositFixtureBaseTimeMs + fixtureIndex),
    [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]:
      overrides[DepositsDB.Columns.DEPOSIT_L1_TX_HASH] ??
      databaseTxHash(`deposits.${fixtureLabel}.l1-tx`),
    [DepositsDB.Columns.LEDGER_TX_ID]:
      overrides[DepositsDB.Columns.LEDGER_TX_ID] ??
      databaseTxHash(`deposits.${fixtureLabel}.ledger-tx`),
    [DepositsDB.Columns.LEDGER_OUTPUT]:
      overrides[DepositsDB.Columns.LEDGER_OUTPUT] ??
      databaseFixtureBytes(`deposits.${fixtureLabel}.ledger-output`, 80),
    [DepositsDB.Columns.LEDGER_ADDRESS]:
      overrides[DepositsDB.Columns.LEDGER_ADDRESS] ?? address1,
    [DepositsDB.Columns.PROJECTED_HEADER_HASH]:
      overrides[DepositsDB.Columns.PROJECTED_HEADER_HASH] ?? null,
    [DepositsDB.Columns.STATUS]:
      overrides[DepositsDB.Columns.STATUS] ?? DepositsDB.Status.Awaiting,
  };
};
