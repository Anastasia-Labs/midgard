import { describe, expect, beforeAll } from "vitest";
import { fromHex, toHex } from "@lucid-evolution/lucid";
import dotenv from "dotenv";
dotenv.config({ path: ".env" });
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import { Database } from "../src/services/database.js";
import { NodeConfig, User } from "../src/config.js";
import { initializeDb } from "../src/database/init.js";
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

  // Utils
  TxUtils,
  LedgerUtils,
} from "../src/database/index.js";
import { breakDownTx, ProcessedTx } from "../src/utils.js";

const provideLayers = <A, E, R>(eff: Effect.Effect<A, E, R>) =>
  eff.pipe(
    Effect.provide(Database.layer),
    Effect.provide(User.layer),
    Effect.provide(NodeConfig.layer),
  );

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
    ],
    { discard: true },
  );
});

const randomBytes = (n: number) =>
  Buffer.from(Array.from({ length: n }, () => Math.floor(Math.random() * 255)));

beforeAll(async () => {
  await Effect.runPromise(
    provideLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // Ensure a clean schema: drop tables (and thus indexes) if they exist
        yield* sql`
          DROP SCHEMA public CASCADE;
          CREATE SCHEMA public;`;
        yield* initializeDb();
        yield* flushAll;
      }),
    ),
  );
});

describe("Database: initialization and basic operations", () => {
  it.effect("initialize and flush", (_) =>
    provideLayers(
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
      provideLayers(
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
          const all = yield* BlocksDB.retrieve();
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
          const afterClear = yield* BlocksDB.retrieve();
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
          const afterClearAll = yield* BlocksDB.retrieve();
          expect(afterClearAll.length).toEqual(0);
        }),
      ),
  );
});

describe("MempoolDB", () => {
  it.effect(
    "insert, retrieve single, retrieve all, retrieve cbor by hash, retrieve cbors by hashes, retrieve count, clear txs, clear all",
    (_) =>
      provideLayers(
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
          const gotAll = yield* MempoolDB.retrieve();
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
          const afterClear = yield* MempoolDB.retrieve();
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
          const afterClearAll = yield* MempoolDB.retrieve();
          expect(afterClearAll.length).toEqual(0);

          // insert single
          yield* flushAll;
          yield* MempoolDB.insert(processedTx1);
          const afterInsertOne = yield* MempoolDB.retrieve();
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
      provideLayers(
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
      provideLayers(
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
          const gotAll: TxUtils.EntryWithTimeStamp[] =
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
    provideLayers(
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
      provideLayers(
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
    provideLayers(
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
    provideLayers(
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
    provideLayers(
      Effect.gen(function* () {
        yield* flushAll;
        const addr =
          "addr_test1qr48rgffpnfjx66ffp8gkzal04pkxnm2pj47y2cddqhw9vzv2up4nnc8q8lmyaqq536anzhhcvydgs8mz95tx9k2ujmq7ctfyz";
        // send funds from addr to another address
        const txToSubmit =
          "84a800d9010282825820071204405c4ea4ed17faf45231c77e0808aaee680ca2241d8192c398c87701cb00825820071204405c4ea4ed17faf45231c77e0808aaee680ca2241d8192c398c87701cb020183a300581d702ffbb8e0ca9c656db8ca88f003d4ddc219cd8865a02fa3aa91d4b68a01821a00256ba6a1581cf56b0b5c60c7bca716b05335dc886e87a378115b52de44e33d84c125a158204e6f64658a1f1f7826e0fcbda030fad4a074ad6c534e6f4556fe0875d578a7fe01028201d818590126d8799fd8799f581c0a9d03cb57a83b601e9032b5d26cf59d7c7b9a7d4a0d46a4b3522aabffd87a80d8799f582009aecf46eab5f38bbad7afacd80e5e86314e1bdf06936830602282522de46b3d5820e8126ccc1b8894f7f3184dff5bd4b44b752c01227439c8d14b86165c30585e6c582058e6726497a8f6643236b9aee219cfce991cf573eb5597dc5b6f4097370c670f58200000000000000000000000000000000000000000000000000000000000000000582000000000000000000000000000000000000000000000000000000000000000001b0000019981f121ec1b00000199858035b3581c0a9d03cb57a83b601e9032b5d26cf59d7c7b9a7d4a0d46a4b3522aab581cea71a1290cd3236b49484e8b0bbf7d43634f6a0cabe22b0d682ee2b000ffffa300581d702ffbb8e0ca9c656db8ca88f003d4ddc219cd8865a02fa3aa91d4b68a01821a00277590a1581cf56b0b5c60c7bca716b05335dc886e87a378115b52de44e33d84c125a158204e6f64650a9d03cb57a83b601e9032b5d26cf59d7c7b9a7d4a0d46a4b3522aab01028201d818590145d8799fd8799f581ce4af364a4f65e60ac4c594333daca69e3ab96b8ae20eee4865299667ffd8799f581c0a9d03cb57a83b601e9032b5d26cf59d7c7b9a7d4a0d46a4b3522aabffd8799f58201e1a7d7e3933d0fb5c6ff1de198e5fce4ce669f52cbf2f7490df88aebbd01b59582009aecf46eab5f38bbad7afacd80e5e86314e1bdf06936830602282522de46b3d5820773626cfd9fcfd0d5d8b1965cf4cc1c4120be9ac269442e0e941e79ea288e8f758200000000000000000000000000000000000000000000000000000000000000000582000000000000000000000000000000000000000000000000000000000000000001b0000019981ed8d111b0000019981f121ec581ce4af364a4f65e60ac4c594333daca69e3ab96b8ae20eee4865299667581cea71a1290cd3236b49484e8b0bbf7d43634f6a0cabe22b0d682ee2b000ffff82583900ea71a1290cd3236b49484e8b0bbf7d43634f6a0cabe22b0d682ee2b04c570359cf0701ffb27400a475d98af7c308d440fb1168b316cae4b61a42e0f9fd021a0003687309a1581cf56b0b5c60c7bca716b05335dc886e87a378115b52de44e33d84c125a158204e6f64658a1f1f7826e0fcbda030fad4a074ad6c534e6f4556fe0875d578a7fe010b58203f177f309f3cee86b218b75cdaced0c917157a384fc76ca413cdb29df8f989db0dd9010281825820071204405c4ea4ed17faf45231c77e0808aaee680ca2241d8192c398c87701cb021082583900ea71a1290cd3236b49484e8b0bbf7d43634f6a0cabe22b0d682ee2b04c570359cf0701ffb27400a475d98af7c308d440fb1168b316cae4b61a42bf8cc0111a004c4b40a20582840000d8798082190a491a00082c5f840100d8798082190a491a00082c5f07d9010282585258500101002332259800a998012481154d6964676172642044656d6f20e28093204d696e740014a3149a2a660049211856616c696461746f722072657475726e65642066616c736500136564004ae715cd01585358510101002332259800a998012481164d6964676172642044656d6f20e28093205370656e640014a3149a2a660049211856616c696461746f722072657475726e65642066616c736500136564004ae715cd01f5f6";
        const brokenTx = yield* breakDownTx(fromHex(txToSubmit));
        yield* MempoolDB.insertMultiple([brokenTx]);

        const sql = yield* SqlClient.SqlClient;
        const result = yield* sql<AddressHistoryDB.Entry>`SELECT * FROM address_history`;
        expect(result.length).toBe(2);
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

const address1 = "addr_test1vz9xsqexampleaddress1";
const address2 = "addr_test1vz9xsqexampleaddress2";

const txEntry1: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId1,
  [TxUtils.Columns.TX]: tx1,
};

const txEntry2: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId2,
  [TxUtils.Columns.TX]: tx2,
};

const removeTimestampFromTxEntry = (
  e: TxUtils.EntryWithTimeStamp,
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
  e: LedgerUtils.EntryWithTimeStamp,
): LedgerUtils.EntryNoTimeStamp => {
  return {
    [LedgerUtils.Columns.TX_ID]: e[LedgerUtils.Columns.TX_ID],
    [LedgerUtils.Columns.OUTREF]: e[LedgerUtils.Columns.OUTREF],
    [LedgerUtils.Columns.OUTPUT]: e[LedgerUtils.Columns.OUTPUT],
    [LedgerUtils.Columns.ADDRESS]: e[LedgerUtils.Columns.ADDRESS],
  };
};
