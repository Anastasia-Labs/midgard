import fs from "node:fs";
import path from "node:path";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxBodyCompact,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  benchmarkOperation,
  buildBenchmarkMeta,
  type OperationStats,
  printOperationTable,
  writeBenchmarkJson,
} from "./benchmark-utils.js";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

type Report = {
  meta: {
    generatedAtIso: string;
    benchmarkVersion: string;
    hostname: string;
    cpuModel: string;
    cpuCount: number;
    platform: string;
    nodeVersion: string;
    gitCommit: string;
    txCount: number;
  };
  config: {
    quickMode: boolean;
    warmupRuns: number;
    measuredRuns: number;
  };
  operations: OperationStats[];
};

const BENCHMARK_VERSION = "3.0.0";
const QUICK_MODE = process.env.BENCH_QUICK === "1";
const WARMUP_RUNS = QUICK_MODE ? 1 : 2;
const MEASURED_RUNS = QUICK_MODE ? 5 : 15;
const TX_LIMIT = QUICK_MODE ? 300 : 1200;

const fixturePath = path.resolve(__dirname, "../txs/txs_0.json");
const outputPath = path.resolve(
  __dirname,
  "./output/midgard-tx-codec-benchmark.json",
);

const benchmarkOptions = {
  warmupRuns: WARMUP_RUNS,
  measuredRuns: MEASURED_RUNS,
} as const;

describe("midgard native tx codec benchmark", () => {
  it("measures native serialization, deserialization, conversion, and hashing", async () => {
    const fixtures = JSON.parse(
      fs.readFileSync(fixturePath, "utf8"),
    ) as readonly TxFixture[];
    const txBytes = fixtures
      .slice(0, TX_LIMIT)
      .map((tx) => Buffer.from(tx.cborHex, "hex"));

    const nativeCanonicalCbors = txBytes.map((bytes) =>
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(bytes),
    );
    const nativeDecoded = nativeCanonicalCbors.map((bytes) =>
      decodeMidgardNativeTxFullFromCanonicalCbor(bytes),
    );
    const nativeBodyCompactDecoded = nativeDecoded.map((tx) =>
      deriveMidgardNativeTxBodyCompact(tx.body),
    );
    const nativeWitnessCompactDecoded = nativeDecoded.map((tx) =>
      deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet),
    );
    const nativeCompactDecoded = nativeDecoded.map((tx) => tx.compact);
    const nativeBodyCompactBytes = nativeBodyCompactDecoded.map((body) =>
      encodeMidgardNativeTxBodyCompact(body),
    );
    const nativeWitnessCompactBytes = nativeWitnessCompactDecoded.map((wits) =>
      encodeMidgardNativeTxWitnessSetCompact(wits),
    );
    const nativeCompactBytes = nativeCompactDecoded.map((compact) =>
      encodeMidgardNativeTxCompact(compact),
    );

    const operations: OperationStats[] = [
      await benchmarkOperation(
        "serialize_native_full",
        () => {
          for (const tx of nativeDecoded) {
            encodeMidgardNativeTxCanonical(tx);
          }
        },
        txBytes.length,
        benchmarkOptions,
      ),
      await benchmarkOperation(
        "deserialize_native_full",
        () => {
          for (const bytes of nativeCanonicalCbors) {
            decodeMidgardNativeTxFullFromCanonicalCbor(bytes);
          }
        },
        txBytes.length,
        benchmarkOptions,
      ),
      await benchmarkOperation(
        "serialize_native_compact",
        () => {
          for (const tx of nativeCompactDecoded) {
            encodeMidgardNativeTxCompact(tx);
          }
        },
        txBytes.length,
        benchmarkOptions,
      ),
      await benchmarkOperation(
        "deserialize_native_compact",
        () => {
          for (const bytes of nativeCompactBytes) {
            decodeMidgardNativeTxCompact(bytes);
          }
        },
        txBytes.length,
        benchmarkOptions,
      ),
      await benchmarkOperation(
        "serialize_native_body_compact",
        () => {
          for (const body of nativeBodyCompactDecoded) {
            encodeMidgardNativeTxBodyCompact(body);
          }
        },
        txBytes.length,
        benchmarkOptions,
      ),
      await benchmarkOperation(
        "deserialize_native_body_compact",
        () => {
          for (const bytes of nativeBodyCompactBytes) {
            decodeMidgardNativeTxBodyCompact(bytes);
          }
        },
        txBytes.length,
        benchmarkOptions,
      ),
      await benchmarkOperation(
        "serialize_native_witness_compact",
        () => {
          for (const wits of nativeWitnessCompactDecoded) {
            encodeMidgardNativeTxWitnessSetCompact(wits);
          }
        },
        txBytes.length,
        benchmarkOptions,
      ),
      await benchmarkOperation(
        "deserialize_native_witness_compact",
        () => {
          for (const bytes of nativeWitnessCompactBytes) {
            decodeMidgardNativeTxWitnessSetCompact(bytes);
          }
        },
        txBytes.length,
        benchmarkOptions,
      ),
      await benchmarkOperation(
        "convert_cardano_to_midgard_native_full",
        () => {
          for (const bytes of txBytes) {
            cardanoTxBytesToMidgardNativeTxCanonicalCbor(bytes);
          }
        },
        txBytes.length,
        benchmarkOptions,
      ),
      await benchmarkOperation(
        "hash_midgard_native_tx_id",
        () => {
          for (const tx of nativeDecoded) {
            computeMidgardNativeTxId(tx);
          }
        },
        txBytes.length,
        benchmarkOptions,
      ),
    ];

    const report: Report = {
      meta: {
        ...buildBenchmarkMeta(BENCHMARK_VERSION),
        txCount: txBytes.length,
      },
      config: {
        quickMode: QUICK_MODE,
        warmupRuns: WARMUP_RUNS,
        measuredRuns: MEASURED_RUNS,
      },
      operations,
    };

    writeBenchmarkJson(outputPath, report);

    console.log(`\nCodec benchmark written to ${outputPath}`);
    printOperationTable(operations);

    expect(operations.length).toBe(10);
  });
});
