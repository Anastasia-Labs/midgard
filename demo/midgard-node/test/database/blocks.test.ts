import { describe, it, expect } from "@effect/vitest";
import { Effect } from "effect";
import { testSqlLayer } from "./runner";
import {
  insert,
  retrieve,
  clear,
  retrieveTxHashesByBlockHash,
  clearBlock,
} from "../../src/database/blocks";

const makeHash = (value: number): Uint8Array =>
  new Uint8Array([value, value + 1, value + 2]);

describe("Blocks Database", () => {
  it.effect("should insert and retrieve blocks", () =>
    Effect.provide(
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
        const sortFn = (a: number[][], b: number[][]) =>
          JSON.stringify(a).localeCompare(JSON.stringify(b));
        expect(retrievedMapped.sort(sortFn)).toEqual(
          expectedMapped.sort(sortFn),
        );
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
        const headerHash = makeHash(30);
        const txHashes = [makeHash(31)];
        yield* clear();
        yield* insert(headerHash, txHashes);
        const beforeClear = yield* retrieve();
        expect(beforeClear.length).toBe(1);
        yield* clear();
        const afterClear = yield* retrieve();
        expect(afterClear.length).toBe(0);
      }),
      testSqlLayer,
    ),
  );

  it.effect(
    "should handle inserting an empty array of txHashes gracefully",
    () =>
      Effect.provide(
        Effect.gen(function* () {
          const headerHash = makeHash(40);
          yield* clear();
          yield* insert(headerHash, []);
          const retrievedData = yield* retrieve();
          expect(retrievedData.length).toBe(0);
        }),
        testSqlLayer,
      ),
  );

  it.effect("should retrieve transaction hashes by block hash", () =>
    Effect.provide(
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
        const retrievedAsNumbers = retrievedTxs.map((bufferOrUint8) =>
          Array.from(bufferOrUint8),
        );
        const expectedAsNumbers = txHashes1.map((uint8array) =>
          Array.from(uint8array),
        );
        const sortFn = (a: number[], b: number[]) =>
          JSON.stringify(a).localeCompare(JSON.stringify(b));
        expect(retrievedAsNumbers.sort(sortFn)).toEqual(
          expectedAsNumbers.sort(sortFn),
        );
        const retrievedTxsForNonExistent = yield* retrieveTxHashesByBlockHash(
          makeHash(99),
        );
        expect(retrievedTxsForNonExistent.length).toBe(0);
      }),
      testSqlLayer,
    ),
  );

  it.effect("should retrieve transaction hashes by block hash", () =>
    Effect.provide(
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
        expect(
          retrievedTxs.map((bufferOrUint8) => Array.from(bufferOrUint8)),
        ).toEqual(txHashes1.map((uint8array) => Array.from(uint8array)));

        const retrievedTxsForNonExistent = yield* retrieveTxHashesByBlockHash(
          makeHash(99),
        );
        expect(retrievedTxsForNonExistent.length).toBe(0);
      }),
      testSqlLayer,
    ),
  );

  it.effect("should clear a specific block", () =>
    Effect.provide(
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
        expect(allBlocks.length).toBe(1);
        expect(Array.from(allBlocks[0][0])).toEqual(
          Array.from(headerHashToKeep),
        );
        expect(Array.from(allBlocks[0][1])).toEqual(
          Array.from(txHashesToKeep[0]),
        );
        const checkCleared =
          yield* retrieveTxHashesByBlockHash(headerHashToClear);
        expect(checkCleared.length).toBe(0);
      }),
      testSqlLayer,
    ),
  );
});
