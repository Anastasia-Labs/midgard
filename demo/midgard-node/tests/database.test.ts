import { describe, expect, beforeAll } from "vitest";
import { toHex } from "@lucid-evolution/lucid";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import * as InitDB from "../src/database/init.js";
import {
  // Block
  BlocksTxsDB,

  // Address history
  AddressHistoryDB,

  // Tx
  ImmutableDB,
  MempoolDB,

  // Ledger
  LatestLedgerDB,
  MempoolLedgerDB,
  ConfirmedLedgerDB,

  // Utils
  Tx,
  Ledger,
} from "../src/database/index.js";
import { provideDatabaseLayers } from "./utils.js";

const flushAll = Effect.gen(function* () {
  yield* Effect.all(
    [
      MempoolLedgerDB.clear,
      LatestLedgerDB.clear,
      ConfirmedLedgerDB.clear,
      BlocksTxsDB.clear,
      ImmutableDB.clear,
      MempoolDB.clear,
      AddressHistoryDB.clear,
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

describe("BlocksTxsDB", () => {
  it.effect(
    "insert, retrieve all, retrieve by header, retrieve by tx, clear block, clear all",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          // insert with some txs
          yield* BlocksTxsDB.insert(blockHeader1, [tx1, tx2]);
          yield* BlocksTxsDB.insert(blockHeader2, [tx3]);

          // retrieve tx hashes by header
          const txs =
            yield* BlocksTxsDB.retrieveTxHashesByHeaderHash(blockHeader1);
          const txsHex = txs.map((row) => toHex(row));
          expect(new Set(txsHex)).toStrictEqual(
            new Set([toHex(tx1), toHex(tx2)]),
          );

          // retrieve header by tx hash
          const retrievedHeader =
            yield* BlocksTxsDB.retrieveHeaderHashByTxHash(tx1);
          expect(toHex(retrievedHeader)).toEqual(toHex(blockHeader1));

          // retrieve all
          const all = yield* BlocksTxsDB.retrieve;
          expect(
            new Set(
              all.map((a) => ({
                [BlocksTxsDB.Columns.HEADER_HASH]:
                  a[BlocksTxsDB.Columns.HEADER_HASH],
                [BlocksTxsDB.Columns.TX_ID]: a[BlocksTxsDB.Columns.TX_ID],
              })),
            ),
          ).toStrictEqual(
            new Set([
              {
                [BlocksTxsDB.Columns.HEADER_HASH]: blockHeader1,
                [BlocksTxsDB.Columns.TX_ID]: tx1,
              },
              {
                [BlocksTxsDB.Columns.HEADER_HASH]: blockHeader1,
                [BlocksTxsDB.Columns.TX_ID]: tx2,
              },
              {
                [BlocksTxsDB.Columns.HEADER_HASH]: blockHeader2,
                [BlocksTxsDB.Columns.TX_ID]: tx3,
              },
            ]),
          );

          //clear block
          yield* BlocksTxsDB.clearBlock(blockHeader1);
          const afterClear = yield* BlocksTxsDB.retrieve;
          expect(
            new Set(
              afterClear.map((a) => ({
                [BlocksTxsDB.Columns.HEADER_HASH]:
                  a[BlocksTxsDB.Columns.HEADER_HASH],
                [BlocksTxsDB.Columns.TX_ID]: a[BlocksTxsDB.Columns.TX_ID],
              })),
            ),
          ).toStrictEqual(
            new Set([
              {
                [BlocksTxsDB.Columns.HEADER_HASH]: blockHeader2,
                [BlocksTxsDB.Columns.TX_ID]: tx3,
              },
            ]),
          );

          // clear all
          yield* BlocksTxsDB.clear;
          const afterClearAll = yield* BlocksTxsDB.retrieve;
          expect(afterClearAll.length).toEqual(0);
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
          const gotAll: readonly Tx.EntryWithTimeStamp[] =
            yield* ImmutableDB.retrieve;
          expect(
            new Set(
              gotAll.map((e: Tx.EntryWithTimeStamp) =>
                removeTimestampFromTxEntry(e),
              ),
            ),
          ).toStrictEqual(
            new Set([
              {
                [Tx.Columns.TX_ID]: txId1,
                [Tx.Columns.TX]: tx1,
              },
              {
                [Tx.Columns.TX_ID]: txId2,
                [Tx.Columns.TX]: tx2,
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
              [Tx.Columns.TX_ID]: txId1,
              [Tx.Columns.TX]: tx1,
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
        yield* LatestLedgerDB.clearUTxOs([ledgerEntry1[Ledger.Columns.OUTREF]]);
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
            ledgerEntry1[Ledger.Columns.OUTREF],
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
          ledgerEntry1[Ledger.Columns.OUTREF],
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

const txEntry1: Tx.Entry = {
  [Tx.Columns.TX_ID]: txId1,
  [Tx.Columns.TX]: tx1,
};

const txEntry2: Tx.Entry = {
  [Tx.Columns.TX_ID]: txId2,
  [Tx.Columns.TX]: tx2,
};

const removeTimestampFromTxEntry = (e: Tx.Entry): Tx.EntryNoTimeStamp => {
  return {
    [Tx.Columns.TX_ID]: e[Tx.Columns.TX_ID],
    [Tx.Columns.TX]: e[Tx.Columns.TX],
  };
};

const ledgerEntry1: Ledger.Entry = {
  [Ledger.Columns.TX_ID]: txId1,
  [Ledger.Columns.OUTREF]: outref1,
  [Ledger.Columns.OUTPUT]: output1,
  [Ledger.Columns.ADDRESS]: address1,
};

const ledgerEntry2: Ledger.Entry = {
  [Ledger.Columns.TX_ID]: txId2,
  [Ledger.Columns.OUTREF]: outref2,
  [Ledger.Columns.OUTPUT]: output2,
  [Ledger.Columns.ADDRESS]: address2,
};

const removeTimestampFromLedgerEntry = (
  e: Ledger.Entry,
): Ledger.EntryNoTimeStamp => {
  return {
    [Ledger.Columns.TX_ID]: e[Ledger.Columns.TX_ID],
    [Ledger.Columns.OUTREF]: e[Ledger.Columns.OUTREF],
    [Ledger.Columns.OUTPUT]: e[Ledger.Columns.OUTPUT],
    [Ledger.Columns.ADDRESS]: e[Ledger.Columns.ADDRESS],
  };
};
