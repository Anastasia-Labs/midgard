import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import { performance } from "node:perf_hooks";
import { execSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {
  cardanoTxBytesToMidgardNativeTxFullBytes,
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxFull,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";
import { QueuedTx, runPhaseAValidation } from "@/validation/index.js";

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
    nativeAccepted: number;
    nativeRejected: number;
  };
  config: {
    quickMode: boolean;
    warmupRuns: number;
    measuredRuns: number;
    nativeExpectedNetworkId: string;
    concurrency: number;
  };
  operations: OperationStats[];
};

type PhaseAVerdicts = {
  accepted: number;
  rejected: number;
  verdictByArrival: readonly string[];
};

const BENCHMARK_VERSION = "1.0.0";
const QUICK_MODE = process.env.BENCH_QUICK === "1";
const WARMUP_RUNS = QUICK_MODE ? 1 : 2;
const MEASURED_RUNS = QUICK_MODE ? 5 : 15;
const TX_LIMIT = QUICK_MODE ? 300 : 1500;
const CONCURRENCY = Number.parseInt(process.env.BENCH_PHASE_A_CONCURRENCY ?? "32", 10);
const EXPECTED_NETWORK_ID = BigInt(process.env.BENCH_NETWORK_ID ?? "0");

const fixturePath = path.resolve(__dirname, "../txs/txs_0.json");
const outputPath = path.resolve(
  __dirname,
  "./output/native-phase-a-benchmark.json",
);
const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_LIST_ROOT = computeHash32(EMPTY_CBOR_LIST);

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

const runPhaseAOnce = async (
  queuedTxs: readonly QueuedTx[],
  expectedNetworkId: bigint,
): Promise<PhaseAVerdicts> => {
  const result = await Effect.runPromise(
    runPhaseAValidation(queuedTxs, {
      expectedNetworkId,
      minFeeA: 0n,
      minFeeB: 0n,
      concurrency: CONCURRENCY,
      strictnessProfile: "phase1_midgard",
    }),
  );
  const acceptedById = new Set(
    result.accepted.map((tx) => tx.txId.toString("hex")),
  );
  const rejectedCodeById = new Map(
    result.rejected.map((tx) => [tx.txId.toString("hex"), tx.code]),
  );
  const verdictByArrival = queuedTxs.map((queuedTx) => {
    const txIdHex = queuedTx.txId.toString("hex");
    if (acceptedById.has(txIdHex)) {
      return "accepted";
    }
    const rejectCode = rejectedCodeById.get(txIdHex);
    return rejectCode === undefined ? "missing" : `rejected:${rejectCode}`;
  });
  return {
    accepted: result.accepted.length,
    rejected: result.rejected.length,
    verdictByArrival,
  };
};

const verdictsMatch = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length && left.every((value, i) => value === right[i]);

const benchmarkOperation = async (
  name: string,
  run: () => Promise<void>,
  txCount: number,
): Promise<OperationStats> => {
  for (let i = 0; i < WARMUP_RUNS; i++) {
    await run();
  }

  const runTimesMs: number[] = [];
  for (let i = 0; i < MEASURED_RUNS; i++) {
    const start = performance.now();
    await run();
    runTimesMs.push(performance.now() - start);
  }

  return summarize(name, runTimesMs, txCount);
};

describe("native tx phase-A benchmark", () => {
  it("measures phase-A throughput for native payloads", async () => {
    const fixtures = JSON.parse(
      fs.readFileSync(fixturePath, "utf8"),
    ) as readonly TxFixture[];
    const txBytes = fixtures
      .slice(0, TX_LIMIT)
      .map((tx) => Buffer.from(tx.cborHex, "hex"));

    const nativeFullBytes = txBytes.map((bytes) =>
      cardanoTxBytesToMidgardNativeTxFullBytes(bytes),
    );
    const normalizedNative = nativeFullBytes.map((bytes) => {
      const converted = decodeMidgardNativeTxFull(bytes);
      const normalized = {
        version: converted.version,
        body: {
          ...converted.body,
          requiredSignersRoot: EMPTY_LIST_ROOT,
          requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        },
        witnessSet: {
          ...converted.witnessSet,
          addrTxWitsRoot: EMPTY_LIST_ROOT,
          addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
          scriptTxWitsRoot: EMPTY_LIST_ROOT,
          scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
          redeemerTxWitsRoot: EMPTY_LIST_ROOT,
          redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        },
      };
      return {
        ...normalized,
        compact: deriveMidgardNativeTxCompact(
          normalized.body,
          normalized.witnessSet,
          "TxIsValid",
        ),
      };
    });
    const nativeExpectedNetworkId = EXPECTED_NETWORK_ID;
    const nativeQueued: QueuedTx[] = normalizedNative.map((tx, i) => ({
      txId: computeMidgardNativeTxIdFromFull(tx),
      txCbor: encodeMidgardNativeTxFull(tx),
      arrivalSeq: BigInt(i),
      createdAt: new Date(0),
    }));

    const baselineNative = await runPhaseAOnce(
      nativeQueued,
      nativeExpectedNetworkId,
    );
    if (baselineNative.rejected !== 0 || baselineNative.accepted !== nativeQueued.length) {
      throw new Error(
        `Native benchmark baseline must fully validate: accepted=${baselineNative.accepted}, rejected=${baselineNative.rejected}`,
      );
    }

    const operations: OperationStats[] = [
      await benchmarkOperation(
        "phase_a_native",
        async () => {
          const result = await runPhaseAOnce(
            nativeQueued,
            nativeExpectedNetworkId,
          );
          if (
            result.accepted !== baselineNative.accepted ||
            result.rejected !== baselineNative.rejected ||
            !verdictsMatch(
              result.verdictByArrival,
              baselineNative.verdictByArrival,
            )
          ) {
            throw new Error(
              `Native phase-A verdicts changed across runs.`,
            );
          }
        },
        nativeQueued.length,
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
        nativeAccepted: baselineNative.accepted,
        nativeRejected: baselineNative.rejected,
      },
      config: {
        quickMode: QUICK_MODE,
        warmupRuns: WARMUP_RUNS,
        measuredRuns: MEASURED_RUNS,
        nativeExpectedNetworkId: nativeExpectedNetworkId.toString(),
        concurrency: CONCURRENCY,
      },
      operations,
    };

    fs.mkdirSync(path.dirname(outputPath), { recursive: true });
    fs.writeFileSync(outputPath, JSON.stringify(report, null, 2));

    console.log(`\nNative phase-A benchmark written to ${outputPath}`);
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

    expect(operations.length).toBe(1);
  });
});
