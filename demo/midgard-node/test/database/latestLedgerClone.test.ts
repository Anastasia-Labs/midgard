import { describe, it, expect } from "@effect/vitest";
import { Effect } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import type { ConfigError } from "effect/ConfigError";
import { Buffer } from "node:buffer";

import { testSqlLayer } from "./runner";

import {
  insert,
  retrieve,
  clearUTxOs,
  clear,
  tableName,
} from "../../src/database/latestLedgerClone.js";

type Utxo = {
  outputReference: Uint8Array | Buffer;
  output: Uint8Array | Buffer;
};

const makeBuffer = (value: number, length = 8): Uint8Array => {
  const arr = new Uint8Array(length);
  for (let i = 0; i < length; i++) {
    arr[i] = value + i;
  }
  return arr;
};

const makeUtxo = (
  refVal: number,
  outVal: number,
): { outputReference: Uint8Array; output: Uint8Array } => ({
  outputReference: makeBuffer(refVal),
  output: makeBuffer(outVal, 16),
});

const compareUtxo = (a: Utxo, b: Utxo): boolean => {
  return (
    Buffer.compare(
      Buffer.from(a.outputReference),
      Buffer.from(b.outputReference),
    ) === 0 &&
    Buffer.compare(Buffer.from(a.output), Buffer.from(b.output)) === 0
  );
};

const sortUtxoArray = (arr: ReadonlyArray<Utxo>): ReadonlyArray<Utxo> => {
  return [...arr].sort((a, b) =>
    Buffer.compare(
      Buffer.from(a.outputReference),
      Buffer.from(b.outputReference),
    ),
  );
};

describe("Latest Ledger Clone Database", () => {
  it.effect("should insert and retrieve UTXOs", () =>
    Effect.provide(
      Effect.gen(function* () {
        const utxo1 = makeUtxo(10, 100);
        const utxo2 = makeUtxo(20, 200);
        const dataToInsert = [utxo1, utxo2];

        yield* clear();
        yield* insert(dataToInsert);
        const retrievedData = yield* retrieve();

        expect(retrievedData.length).toBe(dataToInsert.length);

        const sortedRetrieved = sortUtxoArray(retrievedData);
        const sortedExpected = sortUtxoArray(dataToInsert);

        expect(sortedRetrieved.length).toBe(sortedExpected.length);
        for (let i = 0; i < sortedRetrieved.length; i++) {
          expect(compareUtxo(sortedRetrieved[i], sortedExpected[i])).toBe(true);
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
        const utxo1 = makeUtxo(30, 300);
        yield* clear();
        yield* insert([utxo1]);

        const beforeClear = yield* retrieve();
        expect(beforeClear.length).toBe(1);

        yield* clear();

        const afterClear = yield* retrieve();
        expect(afterClear.length).toBe(0);
      }),
      testSqlLayer,
    ),
  );

  it.effect("should handle inserting an empty array of UTXOs gracefully", () =>
    Effect.provide(
      Effect.gen(function* () {
        yield* clear();
        yield* insert([]);
        const retrievedData = yield* retrieve();
        expect(retrievedData.length).toBe(0);
      }),
      testSqlLayer,
    ),
  );

  it.effect("should clear specific UTXOs by refs", () =>
    Effect.provide(
      Effect.gen(function* () {
        const utxoToClear1 = makeUtxo(40, 400);
        const utxoToClear2 = makeUtxo(50, 500);
        const utxoToKeep = makeUtxo(60, 600);
        const allUtxos = [utxoToClear1, utxoToClear2, utxoToKeep];
        const refsToClear = [
          utxoToClear1.outputReference,
          utxoToClear2.outputReference,
        ];

        yield* clear();
        yield* insert(allUtxos);

        let retrieved = yield* retrieve();
        expect(retrieved.length).toBe(3);

        yield* clearUTxOs(refsToClear);

        retrieved = yield* retrieve();
        expect(retrieved.length).toBe(1);

        expect(compareUtxo(retrieved[0], utxoToKeep)).toBe(true);
      }),
      testSqlLayer,
    ),
  );

  it.effect("should handle clearing UTXOs with an empty refs array", () =>
    Effect.provide(
      Effect.gen(function* () {
        const utxo1 = makeUtxo(70, 700);
        yield* clear();
        yield* insert([utxo1]);

        yield* clearUTxOs([]);

        const retrieved = yield* retrieve();
        expect(retrieved.length).toBe(1);
      }),
      testSqlLayer,
    ),
  );
});
