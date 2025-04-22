import { describe, it, expect } from "@effect/vitest";
import { Effect, Option } from "effect";
import { SqlClient, SqlError } from "@effect/sql";

import { testLayer } from "./runner";

import {
  insert,
  retrieve,
  retrieveTxCborByHash,
  retrieveTxCborsByHashes,
  clearTxs,
  clear,
  tableName,
} from "../../src/database/mempool.js";

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

describe("Mempool Database", () => {
  it("should insert a single tx and retrieve all", () =>
    Effect.gen(function* () {
      const tx1 = makeTx(10);

      yield* clear();
      yield* insert(tx1.txHash, tx1.txCbor);

      const retrievedData = yield* retrieve();

      yield* Effect.try(() => expect(retrievedData.length).toBe(1));
      yield* Effect.try(() =>
        expect(Array.from(retrievedData[0].txHash)).toEqual(
          Array.from(tx1.txHash),
        ),
      );
      yield* Effect.try(() =>
        expect(Array.from(retrievedData[0].txCbor)).toEqual(
          Array.from(tx1.txCbor),
        ),
      );
    }).pipe(Effect.provide(testLayer)));

  it("should retrieve multiple txs", () =>
    Effect.gen(function* () {
      const tx1 = makeTx(20);
      const tx2 = makeTx(30);
      const txsToInsert = [tx1, tx2];

      yield* clear();
      yield* insert(tx1.txHash, tx1.txCbor);
      yield* insert(tx2.txHash, tx2.txCbor);

      const retrievedData = yield* retrieve();

      yield* Effect.try(() =>
        expect(retrievedData.length).toBe(txsToInsert.length),
      );

      const mapData = (d: { txHash: Uint8Array; txCbor: Uint8Array }) => ({
        txHash: Array.from(d.txHash),
        txCbor: Array.from(d.txCbor),
      });
      const sortData = (a: { txHash: number[] }, b: { txHash: number[] }) =>
        a.txHash[0] - b.txHash[0];

      const retrievedMapped = retrievedData.map(mapData).sort(sortData);
      const expectedMapped = txsToInsert.map(mapData).sort(sortData);

      yield* Effect.try(() => expect(retrievedMapped).toEqual(expectedMapped));
    }).pipe(Effect.provide(testLayer)));

  it("should return an empty array when retrieving from an empty table", () =>
    Effect.gen(function* () {
      yield* clear();
      const retrievedData = yield* retrieve();
      yield* Effect.try(() => expect(retrievedData.length).toBe(0));
      yield* Effect.try(() => expect(retrievedData).toEqual([]));
    }).pipe(Effect.provide(testLayer)));

  it("should clear the table", () =>
    Effect.gen(function* () {
      const tx1 = makeTx(40);
      yield* clear();
      yield* insert(tx1.txHash, tx1.txCbor);

      const beforeClear = yield* retrieve();
      yield* Effect.try(() => expect(beforeClear.length).toBe(1));

      yield* clear();

      const afterClear = yield* retrieve();
      yield* Effect.try(() => expect(afterClear.length).toBe(0));
    }).pipe(Effect.provide(testLayer)));

  it("should retrieve tx cbor by hash", () =>
    Effect.gen(function* () {
      const tx1 = makeTx(50);
      const tx2 = makeTx(60);
      const nonExistentHash = makeHash(99);

      yield* clear();
      yield* insert(tx1.txHash, tx1.txCbor);
      yield* insert(tx2.txHash, tx2.txCbor);

      const result1 = yield* retrieveTxCborByHash(tx1.txHash);
      yield* Effect.try(() => expect(Option.isSome(result1)).toBe(true));
      if (Option.isSome(result1)) {
        yield* Effect.try(() =>
          expect(Array.from(result1.value)).toEqual(Array.from(tx1.txCbor)),
        );
      }

      const result2 = yield* retrieveTxCborByHash(tx2.txHash);
      yield* Effect.try(() => expect(Option.isSome(result2)).toBe(true));
      if (Option.isSome(result2)) {
        yield* Effect.try(() =>
          expect(Array.from(result2.value)).toEqual(Array.from(tx2.txCbor)),
        );
      }

      const resultNone = yield* retrieveTxCborByHash(nonExistentHash);
      yield* Effect.try(() => expect(Option.isNone(resultNone)).toBe(true));
    }).pipe(Effect.provide(testLayer)));

  it("should retrieve multiple tx cbors by hashes", () =>
    Effect.gen(function* () {
      const tx1 = makeTx(70);
      const tx2 = makeTx(80);
      const tx3 = makeTx(90);
      const nonExistentHash = makeHash(99);
      const hashesToRetrieve = [tx1.txHash, tx3.txHash, nonExistentHash];
      const allTxs = [tx1, tx2, tx3];
      const allHashes = allTxs.map((tx) => tx.txHash);

      yield* clear();
      yield* Effect.forEach(allTxs, (tx) => insert(tx.txHash, tx.txCbor), {
        discard: true,
      });

      const resultSubset = yield* retrieveTxCborsByHashes(hashesToRetrieve);
      yield* Effect.try(() => expect(resultSubset.length).toBe(2));

      const mapCbor = (c: Uint8Array) => Array.from(c);
      const sortCbor = (a: number[], b: number[]) => a[0] - b[0];

      const retrievedSubsetMapped = resultSubset.map(mapCbor).sort(sortCbor);
      const expectedSubsetMapped = [tx1.txCbor, tx3.txCbor]
        .map(mapCbor)
        .sort(sortCbor);
      yield* Effect.try(() =>
        expect(retrievedSubsetMapped).toEqual(expectedSubsetMapped),
      );

      const resultAll = yield* retrieveTxCborsByHashes(allHashes);
      yield* Effect.try(() => expect(resultAll.length).toBe(3));
      const retrievedAllMapped = resultAll.map(mapCbor).sort(sortCbor);
      const expectedAllMapped = [tx1.txCbor, tx2.txCbor, tx3.txCbor]
        .map(mapCbor)
        .sort(sortCbor);
      yield* Effect.try(() =>
        expect(retrievedAllMapped).toEqual(expectedAllMapped),
      );

      const resultEmpty = yield* retrieveTxCborsByHashes([]);
      yield* Effect.try(() => expect(resultEmpty.length).toBe(0));
    }).pipe(Effect.provide(testLayer)));

  it("should clear specific txs by hash", () =>
    Effect.gen(function* () {
      const txToClear1 = makeTx(100);
      const txToClear2 = makeTx(110);
      const txToKeep = makeTx(120);
      const allTxs = [txToClear1, txToClear2, txToKeep];
      const hashesToClear = [txToClear1.txHash, txToClear2.txHash];

      yield* clear();
      yield* Effect.forEach(allTxs, (tx) => insert(tx.txHash, tx.txCbor), {
        discard: true,
      });

      let retrieved = yield* retrieve();
      yield* Effect.try(() => expect(retrieved.length).toBe(3));

      yield* clearTxs(hashesToClear);

      retrieved = yield* retrieve();
      yield* Effect.try(() => expect(retrieved.length).toBe(1));

      const mapData = (d: { txHash: Uint8Array; txCbor: Uint8Array }) => ({
        txHash: Array.from(d.txHash),
        txCbor: Array.from(d.txCbor),
      });

      yield* Effect.try(() =>
        expect(retrieved.map(mapData)).toEqual([mapData(txToKeep)]),
      );
    }).pipe(Effect.provide(testLayer)));

  it("should handle clearing txs with an empty hash array", () =>
    Effect.gen(function* () {
      const tx1 = makeTx(130);
      yield* clear();
      yield* insert(tx1.txHash, tx1.txCbor);

      yield* clearTxs([]);

      const retrieved = yield* retrieve();
      yield* Effect.try(() => expect(retrieved.length).toBe(1));
    }).pipe(Effect.provide(testLayer)));
});
