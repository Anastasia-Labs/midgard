import { describe, it, expect } from "@effect/vitest";
import { Effect, Option } from "effect";
import { SqlClient, SqlError } from "@effect/sql";

import { testLayer } from "./runner";

import {
  insert,
  retrieve,
  clear,
  retrieveTxHashesByBlockHash,
  retrieveBlockHashByTxHash,
  clearBlock,
  tableName,
} from "../../src/database/blocks";

const makeHash = (value: number): Uint8Array =>
  new Uint8Array([value, value + 1, value + 2]);

describe("Blocks Database", () => {
  it("should insert and retrieve blocks", () =>
    Effect.gen(function* () {
      const headerHash1 = makeHash(10);
      const txHashes1 = [makeHash(11), makeHash(12)];
      const headerHash2 = makeHash(20);
      const txHashes2 = [makeHash(21)];

      yield* clear();
      yield* insert(headerHash1, txHashes1);
      yield* insert(headerHash2, txHashes2);

      const retrievedData = yield* retrieve();

      expect(retrievedData.length).toBe(3);

      const retrievedMapped = retrievedData.map(([h, t]) => [
        Array.from(h),
        Array.from(t),
      ]);
      const expectedMapped = [
        [Array.from(headerHash1), Array.from(txHashes1[0])],
        [Array.from(headerHash1), Array.from(txHashes1[1])],
        [Array.from(headerHash2), Array.from(txHashes2[0])],
      ];

      expect(retrievedMapped.sort()).toEqual(expectedMapped.sort());
    }).pipe(Effect.provide(testLayer)));

  it("should return an empty array when retrieving from an empty table", () =>
    Effect.gen(function* () {
      yield* clear();
      const retrievedData = yield* retrieve();

      expect(retrievedData.length).toBe(0);
      expect(retrievedData).toEqual([]);
    }).pipe(Effect.provide(testLayer)));

  it("should clear the table", () =>
    Effect.gen(function* () {
      const headerHash = makeHash(30);
      const txHashes = [makeHash(31)];
      yield* clear();
      yield* insert(headerHash, txHashes);

      const beforeClear = yield* retrieve();
      expect(beforeClear.length).toBe(1);

      yield* clear();

      const afterClear = yield* retrieve();
      expect(afterClear.length).toBe(0);
    }).pipe(Effect.provide(testLayer)));

  it("should handle inserting an empty array of txHashes gracefully", () =>
    Effect.gen(function* () {
      const headerHash = makeHash(40);
      yield* clear();
      yield* insert(headerHash, []);

      const retrievedData = yield* retrieve();
      expect(retrievedData.length).toBe(0);
    }).pipe(Effect.provide(testLayer)));

  it("should retrieve transaction hashes by block hash", () =>
    Effect.gen(function* () {
      const headerHash1 = makeHash(50);
      const txHashes1 = [makeHash(51), makeHash(52), makeHash(53)];
      const headerHash2 = makeHash(60);
      const txHashes2 = [makeHash(61)];

      yield* clear();
      yield* insert(headerHash1, txHashes1);
      yield* insert(headerHash2, txHashes2);

      const retrievedTxs = yield* retrieveTxHashesByBlockHash(headerHash1);

      expect(retrievedTxs.length).toBe(txHashes1.length);

      const retrievedMapped = retrievedTxs.map((t) => Array.from(t)).sort();
      const expectedMapped = txHashes1.map((t) => Array.from(t)).sort();
      expect(retrievedMapped).toEqual(expectedMapped);

      const retrievedTxsForNonExistent = yield* retrieveTxHashesByBlockHash(
        makeHash(99),
      );
      expect(retrievedTxsForNonExistent.length).toBe(0);
    }).pipe(Effect.provide(testLayer)));

  it("should retrieve block hash by transaction hash", () =>
    Effect.gen(function* () {
      const headerHash1 = makeHash(70);
      const txHash1_1 = makeHash(71);
      const txHash1_2 = makeHash(72);
      const headerHash2 = makeHash(80);
      const txHash2_1 = makeHash(81);
      const nonExistentTxHash = makeHash(99);

      yield* clear();
      yield* insert(headerHash1, [txHash1_1, txHash1_2]);
      yield* insert(headerHash2, [txHash2_1]);

      const retrievedBlock1_1 = yield* retrieveBlockHashByTxHash(txHash1_1);
      expect(Option.isSome(retrievedBlock1_1)).toBe(true);
      if (Option.isSome(retrievedBlock1_1)) {
        expect(Array.from(retrievedBlock1_1.value)).toEqual(
          Array.from(headerHash1),
        );
      }

      const retrievedBlock1_2 = yield* retrieveBlockHashByTxHash(txHash1_2);
      expect(Option.isSome(retrievedBlock1_2)).toBe(true);
      if (Option.isSome(retrievedBlock1_2)) {
        expect(Array.from(retrievedBlock1_2.value)).toEqual(
          Array.from(headerHash1),
        );
      }

      const retrievedBlock2_1 = yield* retrieveBlockHashByTxHash(txHash2_1);
      expect(Option.isSome(retrievedBlock2_1)).toBe(true);
      if (Option.isSome(retrievedBlock2_1)) {
        expect(Array.from(retrievedBlock2_1.value)).toEqual(
          Array.from(headerHash2),
        );
      }

      const retrievedNonExistent =
        yield* retrieveBlockHashByTxHash(nonExistentTxHash);
      expect(Option.isNone(retrievedNonExistent)).toBe(true);
    }).pipe(Effect.provide(testLayer)));

  it("should clear a specific block", () =>
    Effect.gen(function* () {
      const headerHashToClear = makeHash(100);
      const txHashesToClear = [makeHash(101), makeHash(102)];
      const headerHashToKeep = makeHash(110);
      const txHashesToKeep = [makeHash(111)];

      yield* clear();
      yield* insert(headerHashToClear, txHashesToClear);
      yield* insert(headerHashToKeep, txHashesToKeep);

      let allBlocks = yield* retrieve();
      expect(allBlocks.length).toBe(
        txHashesToClear.length + txHashesToKeep.length,
      );

      yield* clearBlock(headerHashToClear);

      allBlocks = yield* retrieve();
      expect(allBlocks.length).toBe(txHashesToKeep.length);

      const remainingMapped = allBlocks.map(([h, t]) => [
        Array.from(h),
        Array.from(t),
      ]);
      const expectedRemaining = [
        [Array.from(headerHashToKeep), Array.from(txHashesToKeep[0])],
      ];
      expect(remainingMapped).toEqual(expectedRemaining);

      const checkCleared =
        yield* retrieveTxHashesByBlockHash(headerHashToClear);
      expect(checkCleared.length).toBe(0);
    }).pipe(Effect.provide(testLayer)));
});
