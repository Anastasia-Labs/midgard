import { describe, it, expect } from "@effect/vitest";
import { Effect, Option } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import type { ConfigError } from "effect/ConfigError";
import { Buffer } from "node:buffer";

import { testSqlLayer } from "./runner";

import {
  insert,
  insertTxs,
  retrieve,
  retrieveTxCborByHash,
  retrieveTxCborsByHashes,
  clear,
  tableName,
} from "../../src/database/immutable.js";

type Tx = {
  txHash: Uint8Array | Buffer;
  txCbor: Uint8Array | Buffer;
};

const makeHash = (value: number, length = 8): Uint8Array => {
  const arr = new Uint8Array(length);
  for (let i = 0; i < length; i++) {
    arr[i] = value + i;
  }
  return arr;
};

const makeCbor = (value: number, length = 32): Uint8Array => {
  const arr = new Uint8Array(length);
  for (let i = 0; i < length; i++) {
    arr[i] = value + i * 2;
  }
  return arr;
};

const makeTx = (idVal: number): { txHash: Uint8Array; txCbor: Uint8Array } => ({
  txHash: makeHash(idVal),
  txCbor: makeCbor(idVal * 10),
});

const compareTx = (a: Tx, b: Tx): boolean => {
  return (
    Buffer.compare(Buffer.from(a.txHash), Buffer.from(b.txHash)) === 0 &&
    Buffer.compare(Buffer.from(a.txCbor), Buffer.from(b.txCbor)) === 0
  );
};

const sortTxArray = (arr: ReadonlyArray<Tx>): ReadonlyArray<Tx> => {
  return [...arr].sort((a, b) =>
    Buffer.compare(Buffer.from(a.txHash), Buffer.from(b.txHash)),
  );
};

const sortByteArray = (
  arr: ReadonlyArray<Uint8Array | Buffer>,
): ReadonlyArray<Uint8Array | Buffer> => {
  return [...arr].sort((a, b) =>
    Buffer.compare(Buffer.from(a), Buffer.from(b)),
  );
};

const byteArraysToNumberArrays = (
  arr: ReadonlyArray<Uint8Array | Buffer>,
): number[][] => {
  return arr.map((byteArr) => Array.from(byteArr));
};

describe("Immutable Database", () => {
  it.effect("should insert a single tx and retrieve all", () =>
    Effect.provide(
      Effect.gen(function* () {
        const tx1 = makeTx(10);
        yield* clear();
        yield* insert(tx1.txHash, tx1.txCbor);
        const retrievedData = yield* retrieve();
        expect(retrievedData.length).toBe(1);
        expect(compareTx(retrievedData[0], tx1)).toBe(true);
      }),
      testSqlLayer,
    ),
  );

  it.effect("should insert multiple txs and retrieve all", () =>
    Effect.provide(
      Effect.gen(function* () {
        const tx1 = makeTx(20);
        const tx2 = makeTx(30);
        const txsToInsert = [tx1, tx2];
        yield* clear();
        yield* insertTxs(txsToInsert);
        const retrievedData = yield* retrieve();
        expect(retrievedData.length).toBe(txsToInsert.length);
        const sortedRetrieved = sortTxArray(retrievedData);
        const sortedExpected = sortTxArray(txsToInsert);
        expect(sortedRetrieved.length).toBe(sortedExpected.length);
        for (let i = 0; i < sortedRetrieved.length; i++) {
          expect(compareTx(sortedRetrieved[i], sortedExpected[i])).toBe(true);
        }
      }),
      testSqlLayer,
    ),
  );

  it.effect(
    "should return an empty array when retrieving from an empty table",
    () =>
      Effect.provide(
        Effect.gen(function* () {
          yield* clear();
          const retrievedData = yield* retrieve();
          expect(retrievedData.length).toBe(0);
          expect(retrievedData).toEqual([]);
        }),
        testSqlLayer,
      ),
  );

  it.effect("should clear the table", () =>
    Effect.provide(
      Effect.gen(function* () {
        const tx1 = makeTx(40);
        yield* clear();
        yield* insert(tx1.txHash, tx1.txCbor);
        const beforeClear = yield* retrieve();
        expect(beforeClear.length).toBe(1);
        yield* clear();
        const afterClear = yield* retrieve();
        expect(afterClear.length).toBe(0);
      }),
      testSqlLayer,
    ),
  );

  it.effect("should handle inserting an empty array of txs gracefully", () =>
    Effect.provide(
      Effect.gen(function* () {
        yield* clear();
        yield* insertTxs([]);
        const retrievedData = yield* retrieve();
        expect(retrievedData.length).toBe(0);
      }),
      testSqlLayer,
    ),
  );

  it.effect("should retrieve tx cbor by hash", () =>
    Effect.provide(
      Effect.gen(function* () {
        const tx1 = makeTx(50);
        const tx2 = makeTx(60);
        const nonExistentHash = makeHash(99);
        yield* clear();
        yield* insertTxs([tx1, tx2]);
        const result1 = yield* retrieveTxCborByHash(tx1.txHash);
        expect(Option.isSome(result1)).toBe(true);
        if (Option.isSome(result1)) {
          expect(Buffer.from(result1.value).toString("hex")).toEqual(
            Buffer.from(tx1.txCbor).toString("hex"),
          );
        }
        const result2 = yield* retrieveTxCborByHash(tx2.txHash);
        expect(Option.isSome(result2)).toBe(true);
        if (Option.isSome(result2)) {
          expect(Buffer.from(result2.value).toString("hex")).toEqual(
            Buffer.from(tx2.txCbor).toString("hex"),
          );
        }
        const resultNone = yield* retrieveTxCborByHash(nonExistentHash);
        expect(Option.isNone(resultNone)).toBe(true);
      }),
      testSqlLayer,
    ),
  );

  it.effect("should retrieve multiple tx cbors by hashes", () =>
    Effect.provide(
      Effect.gen(function* () {
        const tx1 = makeTx(70);
        const tx2 = makeTx(80);
        const tx3 = makeTx(90);
        const nonExistentHash = makeHash(99);
        const hashesToRetrieve = [tx1.txHash, tx3.txHash, nonExistentHash];
        const expectedSubset = [tx1.txCbor, tx3.txCbor];
        const allTxs = [tx1, tx2, tx3];
        const allHashes = allTxs.map((tx) => tx.txHash);
        const allExpectedCbors = allTxs.map((tx) => tx.txCbor);

        yield* clear();
        yield* insertTxs(allTxs);

        const resultSubset = yield* retrieveTxCborsByHashes(hashesToRetrieve);
        expect(resultSubset.length).toBe(2);
        expect(byteArraysToNumberArrays(sortByteArray(resultSubset))).toEqual(
          byteArraysToNumberArrays(sortByteArray(expectedSubset)),
        );

        const resultAll = yield* retrieveTxCborsByHashes(allHashes);
        expect(resultAll.length).toBe(3);
        expect(byteArraysToNumberArrays(sortByteArray(resultAll))).toEqual(
          byteArraysToNumberArrays(sortByteArray(allExpectedCbors)),
        );

        const resultEmpty = yield* retrieveTxCborsByHashes([]);
        expect(resultEmpty.length).toBe(0);
      }),
      testSqlLayer,
    ),
  );
});
