import { describe, expect, it } from "vitest";
import { performance } from "node:perf_hooks";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execSync } from "node:child_process";
import {
  cardanoTxBytesToMidgardCompactBytes,
  cardanoTxBytesToMidgardFullBytes,
  decodeMidgardTransactionCompact,
  decodeMidgardTransactionFull,
  encodeMidgardTransactionCompact,
  encodeMidgardTransactionFull,
  midgardFullBytesToCardanoTxBytes,
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

const BENCHMARK_VERSION = "1.0.0";
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

describe("midgard tx codec benchmark", () => {
  it("measures serialization, deserialization, and conversion performance", () => {
    const fixtures = JSON.parse(
      fs.readFileSync(fixturePath, "utf8"),
    ) as readonly TxFixture[];
    const txBytes = fixtures
      .slice(0, TX_LIMIT)
      .map((tx) => Buffer.from(tx.cborHex, "hex"));

    const fullBytes = txBytes.map((bytes) =>
      cardanoTxBytesToMidgardFullBytes(bytes),
    );
    const fullDecoded = fullBytes.map((bytes) =>
      decodeMidgardTransactionFull(bytes),
    );

    const compactBytes = txBytes.map((bytes) =>
      cardanoTxBytesToMidgardCompactBytes(bytes),
    );
    const compactDecoded = compactBytes.map((bytes) =>
      decodeMidgardTransactionCompact(bytes),
    );

    const operations: OperationStats[] = [
      benchmarkOperation(
        "serialize_full",
        () => {
          for (const tx of fullDecoded) {
            encodeMidgardTransactionFull(tx);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "deserialize_full",
        () => {
          for (const bytes of fullBytes) {
            decodeMidgardTransactionFull(bytes);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "serialize_compact",
        () => {
          for (const tx of compactDecoded) {
            encodeMidgardTransactionCompact(tx);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "deserialize_compact",
        () => {
          for (const bytes of compactBytes) {
            decodeMidgardTransactionCompact(bytes);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "convert_cardano_to_midgard_full",
        () => {
          for (const bytes of txBytes) {
            cardanoTxBytesToMidgardFullBytes(bytes);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "convert_cardano_to_midgard_compact",
        () => {
          for (const bytes of txBytes) {
            cardanoTxBytesToMidgardCompactBytes(bytes);
          }
        },
        txBytes.length,
      ),
      benchmarkOperation(
        "convert_midgard_full_to_cardano",
        () => {
          for (const bytes of fullBytes) {
            midgardFullBytesToCardanoTxBytes(bytes);
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

    expect(operations.length).toBe(7);
  });
});
