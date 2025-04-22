import { describe, it, expect } from "@effect/vitest";
import { Effect } from "effect";
import { SqlClient, SqlError } from "@effect/sql";

import { testLayer } from "./runner";

import {
  insert,
  retrieve,
  clearUTxOs,
  clear,
  tableName,
} from "../../src/database/confirmedLedger";

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

describe("Confirmed Ledger Database", () => {
  it("should insert and retrieve UTXOs", () =>
    Effect.gen(function* () {
      const utxo1 = makeUtxo(10, 100);
      const utxo2 = makeUtxo(20, 200);
      const dataToInsert = [utxo1, utxo2];

      yield* clear();
      yield* insert(dataToInsert);
      const retrievedData = yield* retrieve();

      expect(retrievedData.length).toBe(dataToInsert.length);

      const mapData = (d: {
        outputReference: Uint8Array;
        output: Uint8Array;
      }) => ({
        outputReference: Array.from(d.outputReference),
        output: Array.from(d.output),
      });
      const sortData = (
        a: { outputReference: number[] },
        b: { outputReference: number[] },
      ) => a.outputReference[0] - b.outputReference[0];

      const retrievedMapped = retrievedData.map(mapData).sort(sortData);
      const expectedMapped = dataToInsert.map(mapData).sort(sortData);

      expect(retrievedMapped).toEqual(expectedMapped);
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
      const utxo1 = makeUtxo(30, 300);
      yield* clear();
      yield* insert([utxo1]);

      const beforeClear = yield* retrieve();
      expect(beforeClear.length).toBe(1);

      yield* clear();

      const afterClear = yield* retrieve();
      expect(afterClear.length).toBe(0);
    }).pipe(Effect.provide(testLayer)));

  it("should handle inserting an empty array of UTXOs gracefully", () =>
    Effect.gen(function* () {
      yield* clear();
      yield* insert([]);
      const retrievedData = yield* retrieve();
      expect(retrievedData.length).toBe(0);
    }).pipe(Effect.provide(testLayer)));

  it("should clear specific UTXOs by refs", () =>
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

      const mapData = (d: {
        outputReference: Uint8Array;
        output: Uint8Array;
      }) => ({
        outputReference: Array.from(d.outputReference),
        output: Array.from(d.output),
      });

      expect(retrieved.map(mapData)).toEqual([mapData(utxoToKeep)]);
    }).pipe(Effect.provide(testLayer)));

  it("should handle clearing UTXOs with an empty refs array", () =>
    Effect.gen(function* () {
      const utxo1 = makeUtxo(70, 700);
      yield* clear();
      yield* insert([utxo1]);

      yield* clearUTxOs([]);

      const retrieved = yield* retrieve();
      expect(retrieved.length).toBe(1);
    }).pipe(Effect.provide(testLayer)));
});
