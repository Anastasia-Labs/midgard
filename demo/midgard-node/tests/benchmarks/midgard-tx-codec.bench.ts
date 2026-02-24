import { describe, expect, it } from "vitest";
import { performance } from "node:perf_hooks";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execSync } from "node:child_process";
import {
  cardanoTxBytesToMidgardNativeTxFullBytes,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxBodyCompact,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFull,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxBodyCompactFromFull,
  deriveMidgardNativeTxWitnessSetCompactFromFull,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxFull,
  encodeMidgardNativeTxWitnessSetCompact,
} from "@/midgard-tx-codec/index.js";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

type OperationStats = {
  name: string;
  runs: number;
  minMs: number;
  p50Ms: number;
  p95Ms: number;
  maxMs: number;
  meanMs: number;
  throughputPerSec: number;
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

const quantile = (values: readonly number[], q: number): number => {
  const sorted = [...values].sort((a, b) => a - b);
  const position = (sorted.length - 1) * q;
  const left = Math.floor(position);
  const right = Math.ceil(position);
  if (left === right) {
    return sorted[left];
  }
  const weight = position - left;
  return sorted[left] + weight * (sorted[right] - sorted[left]);
};

const getGitCommit = (): string => {
  try {
    return execSync("git rev-parse --short HEAD", {
      stdio: ["ignore", "pipe", "ignore"],
    })
      .toString("utf8")
      .trim();
  } catch {
    return "unknown";
  }
};

const summarize = (
  name: string,
  runTimesMs: readonly number[],
  txCount: number,
): OperationStats => {
  const minMs = Math.min(...runTimesMs);
  const maxMs = Math.max(...runTimesMs);
  const meanMs =
    runTimesMs.reduce((sum, value) => sum + value, 0) / runTimesMs.length;
  return {
    name,
    runs: runTimesMs.length,
    minMs,
    p50Ms: quantile(runTimesMs, 0.5),
    p95Ms: quantile(runTimesMs, 0.95),
    maxMs,
    meanMs,
    throughputPerSec: txCount / (meanMs / 1000),
  };
};

const benchmarkOperation = (
  name: string,
  run: () => void,
  txCount: number,
): OperationStats => {
  for (let i = 0; i < WARMUP_RUNS; i++) {
    run();
  }

  const runTimesMs: number[] = [];
  for (let i = 0; i < MEASURED_RUNS; i++) {
    const start = performance.now();
    run();
    runTimesMs.push(performance.now() - start);
  }

  return summarize(name, runTimesMs, txCount);
};

describe("midgard native tx codec benchmark", () => {
  it("measures native serialization, deserialization, conversion, and hashing", () => {
    const fixtures = JSON.parse(
      fs.readFileSync(fixturePath, "utf8"),
    ) as readonly TxFixture[];
    const txBytes = fixtures
      .slice(0, TX_LIMIT)
      .map((tx) => Buffer.from(tx.cborHex, "hex"));

    const nativeFullBytes = txBytes.map((bytes) =>
      cardanoTxBytesToMidgardNativeTxFullBytes(bytes),
    );
    const nativeFullDecoded = nativeFullBytes.map((bytes) =>
      decodeMidgardNativeTxFull(bytes),
    );
    const nativeBodyCompactDecoded = nativeFullDecoded.map((tx) =>
      deriveMidgardNativeTxBodyCompactFromFull(tx.body),
    );
    const nativeWitnessCompactDecoded = nativeFullDecoded.map((tx) =>
      deriveMidgardNativeTxWitnessSetCompactFromFull(tx.witnessSet),
    );
    const nativeCompactDecoded = nativeFullDecoded.map((tx) => tx.compact);
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
      benchmarkOperation(
        "serialize_native_full",
        () => {
          for (const tx of nativeFullDecoded) {
            encodeMidgardNativeTxFull(tx);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "deserialize_native_full",
        () => {
          for (const bytes of nativeFullBytes) {
            decodeMidgardNativeTxFull(bytes);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "serialize_native_compact",
        () => {
          for (const tx of nativeCompactDecoded) {
            encodeMidgardNativeTxCompact(tx);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "deserialize_native_compact",
        () => {
          for (const bytes of nativeCompactBytes) {
            decodeMidgardNativeTxCompact(bytes);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "serialize_native_body_compact",
        () => {
          for (const body of nativeBodyCompactDecoded) {
            encodeMidgardNativeTxBodyCompact(body);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "deserialize_native_body_compact",
        () => {
          for (const bytes of nativeBodyCompactBytes) {
            decodeMidgardNativeTxBodyCompact(bytes);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "serialize_native_witness_compact",
        () => {
          for (const wits of nativeWitnessCompactDecoded) {
            encodeMidgardNativeTxWitnessSetCompact(wits);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "deserialize_native_witness_compact",
        () => {
          for (const bytes of nativeWitnessCompactBytes) {
            decodeMidgardNativeTxWitnessSetCompact(bytes);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "convert_cardano_to_midgard_native_full",
        () => {
          for (const bytes of txBytes) {
            cardanoTxBytesToMidgardNativeTxFullBytes(bytes);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "hash_midgard_native_tx_id",
        () => {
          for (const tx of nativeFullDecoded) {
            computeMidgardNativeTxIdFromFull(tx);
          }
        },
        txBytes.length,
      ),
    ];

    const report: Report = {
      meta: {
        generatedAtIso: new Date().toISOString(),
        benchmarkVersion: BENCHMARK_VERSION,
        hostname: os.hostname(),
        cpuModel: os.cpus()[0]?.model ?? "unknown",
        cpuCount: os.cpus().length,
        platform: `${os.platform()} ${os.release()}`,
        nodeVersion: process.version,
        gitCommit: getGitCommit(),
        txCount: txBytes.length,
      },
      config: {
        quickMode: QUICK_MODE,
        warmupRuns: WARMUP_RUNS,
        measuredRuns: MEASURED_RUNS,
      },
      operations,
    };

    fs.mkdirSync(path.dirname(outputPath), { recursive: true });
    fs.writeFileSync(outputPath, JSON.stringify(report, null, 2));

    console.log(`\nCodec benchmark written to ${outputPath}`);
    console.table(
      operations.map((op) => ({
        operation: op.name,
        runs: op.runs,
        p50: `${op.p50Ms.toFixed(2)} ms`,
        p95: `${op.p95Ms.toFixed(2)} ms`,
        mean: `${op.meanMs.toFixed(2)} ms`,
        throughput: `${op.throughputPerSec.toFixed(2)} tx/s`,
      })),
    );

    expect(operations.length).toBe(10);
  });
});
