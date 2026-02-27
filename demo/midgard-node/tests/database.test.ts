import { describe, expect, beforeAll } from "vitest";
import { fromHex, toHex, UTxO } from "@lucid-evolution/lucid";
import { it } from "@effect/vitest";
import { Effect, Option } from "effect";
import { SqlClient } from "@effect/sql";
import { Database } from "../src/services/database.js";
import { NodeConfig } from "../src/services/config.js";
import { Lucid } from "../src/services/lucid.js";
import * as InitDB from "../src/database/init.js";
import {
  // Block
  BlocksDB,

  // Address history
  AddressHistoryDB,

  // Tx
  ImmutableDB,
  ProcessedMempoolDB,
  MempoolDB,
  UnsubmittedBlocksDB,

  // Ledger
  LatestLedgerDB,
  MempoolLedgerDB,
  ConfirmedLedgerDB,

  // Utils
  TxUtils,
  LedgerUtils,
} from "../src/database/index.js";
import { breakDownTx, ProcessedTx } from "../src/utils.js";
import { provideDatabaseLayers } from "./utils.js";

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
      UnsubmittedBlocksDB.clear,
    ],
    { discard: true },
  );
});

const randomBytes = (n: number) =>
  Buffer.from(Array.from({ length: n }, () => Math.floor(Math.random() * 255)));

beforeAll(async () => {
  await Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // Ensure a clean schema: drop tables (and thus indexes) if they exist
        yield* sql`
          DROP SCHEMA public CASCADE;
          CREATE SCHEMA public;`;
        yield* InitDB.program;
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

describe("UnsubmittedBlocksDB", () => {
  it.effect(
    "upsert, retrieve, delete up to block, delete by blocks, clear all",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const unsubmittedBlock1 = randomBytes(32);
          const unsubmittedBlock2 = randomBytes(32);
          const unsubmittedBlock3 = randomBytes(32);
          const l1Cbor1 = randomBytes(128);
          const l1Cbor2 = randomBytes(96);
          const l1Cbor3 = randomBytes(72);
          const producedSnapshot1 = randomBytes(64);
          const producedSnapshot2 = randomBytes(80);
          const producedSnapshot3 = randomBytes(56);
          const walletUtxos1 = randomBytes(64);
          const walletUtxos2 = randomBytes(72);
          const walletUtxos3 = randomBytes(80);

          yield* UnsubmittedBlocksDB.upsert({
            [UnsubmittedBlocksDB.Columns.HEADER_HASH]: unsubmittedBlock1,
            [UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS]: walletUtxos1,
            [UnsubmittedBlocksDB.Columns.L1_CBOR]: l1Cbor1,
            [UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS]: producedSnapshot1,
          });
          yield* UnsubmittedBlocksDB.upsert({
            [UnsubmittedBlocksDB.Columns.HEADER_HASH]: unsubmittedBlock2,
            [UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS]: walletUtxos2,
            [UnsubmittedBlocksDB.Columns.L1_CBOR]: l1Cbor2,
            [UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS]: producedSnapshot2,
          });
          yield* UnsubmittedBlocksDB.upsert({
            [UnsubmittedBlocksDB.Columns.HEADER_HASH]: unsubmittedBlock3,
            [UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS]: walletUtxos3,
            [UnsubmittedBlocksDB.Columns.L1_CBOR]: l1Cbor3,
            [UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS]: producedSnapshot3,
          });

          const updatedL1Cbor1 = randomBytes(140);
          const updatedProducedSnapshot1 = randomBytes(88);
          const updatedWalletUtxos1 = randomBytes(88);
          yield* UnsubmittedBlocksDB.upsert({
            [UnsubmittedBlocksDB.Columns.HEADER_HASH]: unsubmittedBlock1,
            [UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS]: updatedWalletUtxos1,
            [UnsubmittedBlocksDB.Columns.L1_CBOR]: updatedL1Cbor1,
            [UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS]:
              updatedProducedSnapshot1,
          });

          const all = yield* UnsubmittedBlocksDB.retrieve;
          expect(all.length).toEqual(3);
          expect(
            all.map((e) => ({
              [UnsubmittedBlocksDB.Columns.HEADER_HASH]:
                e[UnsubmittedBlocksDB.Columns.HEADER_HASH],
              [UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS]:
                e[UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS],
              [UnsubmittedBlocksDB.Columns.L1_CBOR]:
                e[UnsubmittedBlocksDB.Columns.L1_CBOR],
              [UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS]:
                e[UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS],
            })),
          ).toStrictEqual([
            {
              [UnsubmittedBlocksDB.Columns.HEADER_HASH]: unsubmittedBlock1,
              [UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS]:
                updatedWalletUtxos1,
              [UnsubmittedBlocksDB.Columns.L1_CBOR]: updatedL1Cbor1,
              [UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS]:
                updatedProducedSnapshot1,
            },
            {
              [UnsubmittedBlocksDB.Columns.HEADER_HASH]: unsubmittedBlock2,
              [UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS]: walletUtxos2,
              [UnsubmittedBlocksDB.Columns.L1_CBOR]: l1Cbor2,
              [UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS]: producedSnapshot2,
            },
            {
              [UnsubmittedBlocksDB.Columns.HEADER_HASH]: unsubmittedBlock3,
              [UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS]: walletUtxos3,
              [UnsubmittedBlocksDB.Columns.L1_CBOR]: l1Cbor3,
              [UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS]: producedSnapshot3,
            },
          ]);

          yield* UnsubmittedBlocksDB.deleteUpToAndIncludingBlock(
            unsubmittedBlock2,
          );
          const afterDelete = yield* UnsubmittedBlocksDB.retrieve;
          expect(afterDelete.length).toEqual(1);
          expect(
            afterDelete[0][UnsubmittedBlocksDB.Columns.HEADER_HASH],
          ).toStrictEqual(unsubmittedBlock3);

          yield* UnsubmittedBlocksDB.deleteByBlocks([unsubmittedBlock3]);
          const afterDeleteByBlock = yield* UnsubmittedBlocksDB.retrieve;
          expect(afterDeleteByBlock.length).toEqual(0);

          yield* UnsubmittedBlocksDB.clear;
          const afterClear = yield* UnsubmittedBlocksDB.retrieve;
          expect(afterClear.length).toEqual(0);
        }),
      ),
  );

  it.effect(
    "retrieve latest unsubmitted block with tx hashes and tx cbors in a single query",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const headerHash1 = randomBytes(32);
          const headerHash2 = randomBytes(32);
          const walletUtxoA: UTxO = {
            txHash: "11".repeat(32),
            outputIndex: 0,
            address: address1,
            assets: { lovelace: 1_000_000n },
          };
          const walletUtxoB: UTxO = {
            txHash: "12".repeat(32),
            outputIndex: 1,
            address: address2,
            assets: { lovelace: 2_000_000n },
          };
          const walletUtxoC: UTxO = {
            txHash: "13".repeat(32),
            outputIndex: 2,
            address: address1,
            assets: { lovelace: 3_000_000n },
          };
          const producedUtxoA: UTxO = {
            txHash: "21".repeat(32),
            outputIndex: 0,
            address: address2,
            assets: { lovelace: 1_500_000n },
          };
          const producedUtxoB: UTxO = {
            txHash: "22".repeat(32),
            outputIndex: 1,
            address: address1,
            assets: { lovelace: 2_500_000n },
          };
          const producedUtxoC: UTxO = {
            txHash: "23".repeat(32),
            outputIndex: 2,
            address: address2,
            assets: { lovelace: 3_500_000n },
          };

          const walletUtxos1 =
            yield* UnsubmittedBlocksDB.serializeUTxOsForStorage([walletUtxoA]);
          const walletUtxos2 =
            yield* UnsubmittedBlocksDB.serializeUTxOsForStorage([
              walletUtxoB,
              walletUtxoC,
            ]);
          const producedUtxos1 =
            yield* UnsubmittedBlocksDB.serializeUTxOsForStorage([
              producedUtxoA,
            ]);
          const producedUtxos2 =
            yield* UnsubmittedBlocksDB.serializeUTxOsForStorage([
              producedUtxoB,
              producedUtxoC,
            ]);
          const l1Cbor1 = randomBytes(96);
          const l1Cbor2 = randomBytes(96);
          const txHash1 = randomBytes(32);
          const txHash2 = randomBytes(32);
          const txCbor1 = randomBytes(128);
          const txCbor2 = randomBytes(128);

          yield* UnsubmittedBlocksDB.upsert({
            [UnsubmittedBlocksDB.Columns.HEADER_HASH]: headerHash1,
            [UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS]: walletUtxos1,
            [UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS]: producedUtxos1,
            [UnsubmittedBlocksDB.Columns.L1_CBOR]: l1Cbor1,
          });
          yield* UnsubmittedBlocksDB.upsert({
            [UnsubmittedBlocksDB.Columns.HEADER_HASH]: headerHash2,
            [UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS]: walletUtxos2,
            [UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS]: producedUtxos2,
            [UnsubmittedBlocksDB.Columns.L1_CBOR]: l1Cbor2,
          });

          yield* BlocksDB.insert(headerHash2, [txHash1, txHash2]);
          yield* ImmutableDB.insertTx({
            [TxUtils.Columns.TX_ID]: txHash1,
            [TxUtils.Columns.TX]: txCbor1,
          });
          yield* ImmutableDB.insertTx({
            [TxUtils.Columns.TX_ID]: txHash2,
            [TxUtils.Columns.TX]: txCbor2,
          });

          const maybeLatest =
            yield* UnsubmittedBlocksDB.retrieveLatestWithBlockTxs;
          expect(Option.isSome(maybeLatest)).toEqual(true);
          if (Option.isNone(maybeLatest)) {
            return;
          }
          const latest = maybeLatest.value;
          expect(latest[UnsubmittedBlocksDB.Columns.HEADER_HASH]).toStrictEqual(
            headerHash2,
          );
          expect(
            latest[UnsubmittedBlocksDB.Columns.NEW_WALLET_UTXOS],
          ).toStrictEqual([walletUtxoB, walletUtxoC]);
          expect(
            latest[UnsubmittedBlocksDB.Columns.PRODUCED_UTXOS],
          ).toStrictEqual([producedUtxoB, producedUtxoC]);
          expect(latest.txHashes).toStrictEqual([txHash1, txHash2]);
          expect(latest.txCbors).toStrictEqual([txCbor1, txCbor2]);

          yield* flushAll;
          const maybeNone =
            yield* UnsubmittedBlocksDB.retrieveLatestWithBlockTxs;
          expect(Option.isNone(maybeNone)).toEqual(true);
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

          const pTxId1 = randomBytes(32);
          const pTx1 = randomBytes(64);
          const pSpent1 = randomBytes(32);
          const processedTx1: ProcessedTx = {
            txId: pTxId1,
            txCbor: pTx1,
            spent: [pSpent1],
            produced: [ledgerEntry1],
          };
          const pTxId2 = randomBytes(32);
          const pTx2 = randomBytes(64);
          const pSpent2 = randomBytes(32);
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
        expect(
          new Set(all.map((e) => removeTimestampFromLedgerEntry(e))),
        ).toStrictEqual(new Set([ledgerEntry1, ledgerEntry2]));

        // clear UTxOs
        yield* LatestLedgerDB.clearUTxOs([
          ledgerEntry1[LedgerUtils.Columns.OUTREF],
        ]);
        const afterClear = yield* LatestLedgerDB.retrieve;
        expect(
          new Set(afterClear.map((e) => removeTimestampFromLedgerEntry(e))),
        ).toStrictEqual(new Set([ledgerEntry2]));

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
    "insert, retrieve by address, retrieve all, clearUTxOs, clearAll",
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

        const pTxId1 = randomBytes(32);
        const pTx1 = randomBytes(64);
        const pSpent1 = randomBytes(32);
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
        const pTxId2 = randomBytes(32);
        const pTx2 = randomBytes(64);
        const pSpent2 = randomBytes(32);
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
        const lucid = yield* Lucid;
        yield* lucid.switchToOperatorsMainWallet;
        const thisWalletAddress = yield* Effect.tryPromise(() =>
          lucid.api.wallet().address(),
        );
        // send funds to other wallet
        const tx1 = yield* Effect.tryPromise(() =>
          lucid.api
            .newTx()
            .pay.ToAddress(address1, { lovelace: 5000000n })
            .complete(),
        );
        const signedTx1 = yield* Effect.tryPromise(() =>
          tx1.sign.withWallet().complete(),
        );
        const brokenTx1 = yield* breakDownTx(fromHex(signedTx1.toCBOR()));
        yield* MempoolDB.insertMultiple([brokenTx1]);

        const sql = yield* SqlClient.SqlClient;
        const result1 =
          yield* sql<AddressHistoryDB.Entry>`SELECT * FROM address_history`;
        expect(
          result1.map((r) => r[LedgerUtils.Columns.ADDRESS]).sort(),
        ).toStrictEqual([address1, thisWalletAddress].sort());

        // send funds to same wallet
        yield* flushAll;
        const tx2 = yield* Effect.tryPromise(() =>
          lucid.api
            .newTx()
            .pay.ToAddress(thisWalletAddress, { lovelace: 5000000n })
            .complete(),
        );
        const signedTx2 = yield* Effect.tryPromise(() =>
          tx2.sign.withWallet().complete(),
        );
        const brokenTx2 = yield* breakDownTx(fromHex(signedTx2.toCBOR()));
        yield* MempoolDB.insertMultiple([brokenTx2]);

        const result2 =
          yield* sql<AddressHistoryDB.Entry>`SELECT * FROM address_history`;
        expect(
          result2.map((r) => r[LedgerUtils.Columns.ADDRESS]),
        ).toStrictEqual([thisWalletAddress]);
      }),
    ),
  );
});

const blockHeader1 = randomBytes(32);
const blockHeader2 = randomBytes(32);

const txId1 = randomBytes(32);
const txId2 = randomBytes(32);

const tx1 = randomBytes(64);
const tx2 = randomBytes(64);
const tx3 = randomBytes(64);

const outref1 = randomBytes(36);
const outref2 = randomBytes(36);

const output1 = randomBytes(80);
const output2 = randomBytes(80);

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
