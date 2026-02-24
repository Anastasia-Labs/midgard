import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import { performance } from "node:perf_hooks";
import { execSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import * as LedgerUtils from "@/database/utils/ledger.js";
import {
  PhaseAAccepted,
  QueuedTx,
  runPhaseAValidation,
  runPhaseBValidation,
} from "@/validation/index.js";

type InitialLedgerEntryFixture = {
  txIdHex: string;
  outRefCborHex: string;
  outputCborHex: string;
  address: string;
};

type InitialLedgerStateFixture = {
  network: string;
  generatedAtIso: string;
  ledgerEntries: InitialLedgerEntryFixture[];
};

type GeneratedTxFixture = {
  index: number;
  txId: string;
  cborHex: string;
  fromAddress: string;
  toAddress: string;
};

type GeneratedTxSequenceFixture = {
  network: string;
  generatedAtIso: string;
  txCount: number;
  transactions: GeneratedTxFixture[];
};

type StatSummary = {
  min: number;
  p50: number;
  p95: number;
  max: number;
  mean: number;
};

type BenchSizeResult = {
  size: number;
  warmupRuns: number;
  measuredRuns: number;
  acceptedCountExpected: number;
  acceptedCountObserved: number;
  phaseA: StatSummary;
  phaseB: StatSummary;
  total: StatSummary;
  throughputTps: {
    p50: number;
    mean: number;
  };
};

type BenchmarkReport = {
  meta: {
    generatedAtIso: string;
    benchmarkVersion: string;
    hostname: string;
    cpuModel: string;
    cpuCount: number;
    platform: string;
    nodeVersion: string;
    gitCommit: string;
  };
  config: {
    fixtureDirectory: string;
    initialLedgerFixturePath: string;
    txSequenceFixturePath: string;
    quickMode: boolean;
    sizes: number[];
    warmupRuns: number;
    smallMeasuredRuns: number;
    largeMeasuredRuns: number;
    phaseAConcurrency: number;
    strictnessProfile: string;
    expectedNetworkId: string;
    minFeeA: string;
    minFeeB: string;
    resultPath: string;
  };
  results: BenchSizeResult[];
};

const BENCHMARK_VERSION = "2.0.0";
const BENCHMARK_SIZES = [600, 700, 800, 1000, 2000, 4000] as const;

const QUICK_MODE = process.env.BENCH_QUICK === "1";
const WARMUP_RUNS = QUICK_MODE ? 1 : 2;
const SMALL_MEASURED_RUNS = QUICK_MODE ? 5 : 20;
const LARGE_MEASURED_RUNS = QUICK_MODE ? 3 : 10;
const PHASE_A_CONCURRENCY = Number.parseInt(
  process.env.BENCH_PHASE_A_CONCURRENCY ?? "32",
  10,
);
const STRICTNESS_PROFILE =
  process.env.BENCH_STRICTNESS_PROFILE ?? "phase1_midgard";
const EXPECTED_NETWORK_ID = BigInt(process.env.BENCH_NETWORK_ID ?? "0");
const MIN_FEE_A = BigInt(process.env.BENCH_MIN_FEE_A ?? "0");
const MIN_FEE_B = BigInt(process.env.BENCH_MIN_FEE_B ?? "0");

const FIXTURE_DIRECTORY = fileURLToPath(new URL("./fixtures", import.meta.url));
const INITIAL_LEDGER_FIXTURE_PATH = fileURLToPath(
  new URL("./fixtures/initial-ledger-state.json", import.meta.url),
);
const TX_SEQUENCE_FIXTURE_PATH = fileURLToPath(
  new URL("./fixtures/tx-sequence.json", import.meta.url),
);
const OUTPUT_JSON_PATH = fileURLToPath(
  new URL("./output/validation-benchmark.json", import.meta.url),
);

const phaseAConfig = {
  expectedNetworkId: EXPECTED_NETWORK_ID,
  minFeeA: MIN_FEE_A,
  minFeeB: MIN_FEE_B,
  concurrency: PHASE_A_CONCURRENCY,
  strictnessProfile: STRICTNESS_PROFILE,
} as const;

const assertOrThrow = (condition: boolean, message: string) => {
  if (!condition) {
    throw new Error(message);
  }
};

const quantile = (values: readonly number[], q: number): number => {
  assertOrThrow(values.length > 0, "quantile requires at least one value");
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

const summarize = (values: readonly number[]): StatSummary => {
  const min = Math.min(...values);
  const max = Math.max(...values);
  const mean = values.reduce((sum, value) => sum + value, 0) / values.length;
  return {
    min,
    p50: quantile(values, 0.5),
    p95: quantile(values, 0.95),
    max,
    mean,
  };
};

const loadInitialLedgerFixture = (): InitialLedgerStateFixture => {
  assertOrThrow(
    fs.existsSync(INITIAL_LEDGER_FIXTURE_PATH),
    `Initial ledger fixture not found at ${INITIAL_LEDGER_FIXTURE_PATH}. Run: pnpm run bench:fixtures:generate`,
  );
  return JSON.parse(
    fs.readFileSync(INITIAL_LEDGER_FIXTURE_PATH, "utf8"),
  ) as InitialLedgerStateFixture;
};

const loadTxSequenceFixture = (): GeneratedTxSequenceFixture => {
  assertOrThrow(
    fs.existsSync(TX_SEQUENCE_FIXTURE_PATH),
    `Tx sequence fixture not found at ${TX_SEQUENCE_FIXTURE_PATH}. Run: pnpm run bench:fixtures:generate`,
  );
  return JSON.parse(
    fs.readFileSync(TX_SEQUENCE_FIXTURE_PATH, "utf8"),
  ) as GeneratedTxSequenceFixture;
};

const buildPreState = (
  fixture: InitialLedgerStateFixture,
): readonly LedgerUtils.Entry[] =>
  fixture.ledgerEntries.map((entry) => ({
    [LedgerUtils.Columns.TX_ID]: Buffer.from(entry.txIdHex, "hex"),
    [LedgerUtils.Columns.OUTREF]: Buffer.from(entry.outRefCborHex, "hex"),
    [LedgerUtils.Columns.OUTPUT]: Buffer.from(entry.outputCborHex, "hex"),
    [LedgerUtils.Columns.ADDRESS]: entry.address,
  }));

const buildQueuedBlock = (
  txFixture: GeneratedTxSequenceFixture,
  size: number,
): QueuedTx[] => {
  assertOrThrow(
    txFixture.transactions.length >= size,
    `Tx fixture has ${txFixture.transactions.length} txs, but benchmark size ${size} was requested.`,
  );
  return txFixture.transactions.slice(0, size).map((tx, index) => ({
    txId: Buffer.from(tx.txId, "hex"),
    txCbor: Buffer.from(tx.cborHex, "hex"),
    arrivalSeq: BigInt(index + 1),
    createdAt: new Date(txFixture.generatedAtIso),
  }));
};

const runPhaseA = (queued: readonly QueuedTx[]) =>
  Effect.runPromise(runPhaseAValidation(queued, phaseAConfig));

const runPhaseB = (
  accepted: readonly PhaseAAccepted[],
  preState: readonly LedgerUtils.Entry[],
) =>
  Effect.runPromise(
    runPhaseBValidation(accepted, preState, {
      nowMillis: 0n,
      bucketConcurrency: Math.max(1, Math.floor(PHASE_A_CONCURRENCY / 2)),
    }),
  );

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

const measureBlockSize = async (
  queued: readonly QueuedTx[],
  preState: readonly LedgerUtils.Entry[],
  size: number,
): Promise<BenchSizeResult> => {
  const baselinePhaseA = await runPhaseA(queued);
  assertOrThrow(
    baselinePhaseA.rejected.length === 0,
    `Baseline phase A rejected txs for size ${size}: ${baselinePhaseA.rejected.length}`,
  );
  assertOrThrow(
    baselinePhaseA.accepted.length === size,
    `Baseline phase A accepted ${baselinePhaseA.accepted.length}/${size}`,
  );

  const baselinePhaseB = await runPhaseB(baselinePhaseA.accepted, preState);
  assertOrThrow(
    baselinePhaseB.rejected.length === 0,
    `Baseline phase B rejected txs for size ${size}: ${baselinePhaseB.rejected.length}`,
  );
  assertOrThrow(
    baselinePhaseB.accepted.length === size,
    `Baseline phase B accepted ${baselinePhaseB.accepted.length}/${size}`,
  );

  const warmupRuns = WARMUP_RUNS;
  const measuredRuns = size <= 1000 ? SMALL_MEASURED_RUNS : LARGE_MEASURED_RUNS;

  for (let i = 0; i < warmupRuns; i++) {
    const warmupPhaseA = await runPhaseA(queued);
    const warmupPhaseB = await runPhaseB(warmupPhaseA.accepted, preState);
    assertOrThrow(
      warmupPhaseA.rejected.length === 0 && warmupPhaseB.rejected.length === 0,
      `Warmup run rejected txs for size ${size}`,
    );
  }

  const phaseATimings: number[] = [];
  const phaseBTimings: number[] = [];
  const totalTimings: number[] = [];
  const throughputTps: number[] = [];
  let acceptedCountObserved = 0;

  for (let i = 0; i < measuredRuns; i++) {
    const phaseAStart = performance.now();
    const phaseAResult = await runPhaseA(queued);
    const phaseAEnd = performance.now();

    assertOrThrow(
      phaseAResult.rejected.length === 0,
      `Phase A rejected txs for size ${size} run ${i + 1}: ${phaseAResult.rejected.length}`,
    );
    assertOrThrow(
      phaseAResult.accepted.length === size,
      `Phase A accepted ${phaseAResult.accepted.length}/${size} for run ${i + 1}`,
    );

    const phaseBStart = performance.now();
    const phaseBResult = await runPhaseB(phaseAResult.accepted, preState);
    const phaseBEnd = performance.now();

    assertOrThrow(
      phaseBResult.rejected.length === 0,
      `Phase B rejected txs for size ${size} run ${i + 1}: ${phaseBResult.rejected.length}`,
    );
    assertOrThrow(
      phaseBResult.accepted.length === size,
      `Phase B accepted ${phaseBResult.accepted.length}/${size} for run ${i + 1}`,
    );

    const phaseAMs = phaseAEnd - phaseAStart;
    const phaseBMs = phaseBEnd - phaseBStart;
    const totalMs = phaseAMs + phaseBMs;

    phaseATimings.push(phaseAMs);
    phaseBTimings.push(phaseBMs);
    totalTimings.push(totalMs);
    throughputTps.push(size / (totalMs / 1000));
    acceptedCountObserved = phaseBResult.accepted.length;
  }

  return {
    size,
    warmupRuns,
    measuredRuns,
    acceptedCountExpected: size,
    acceptedCountObserved,
    phaseA: summarize(phaseATimings),
    phaseB: summarize(phaseBTimings),
    total: summarize(totalTimings),
    throughputTps: {
      p50: quantile(throughputTps, 0.5),
      mean:
        throughputTps.reduce((sum, value) => sum + value, 0) /
        throughputTps.length,
    },
  };
};

const formatMs = (value: number) => `${value.toFixed(2)} ms`;
const formatTps = (value: number) => `${value.toFixed(2)} tx/s`;

const writeReport = (report: BenchmarkReport) => {
  fs.mkdirSync(path.dirname(OUTPUT_JSON_PATH), { recursive: true });
  fs.writeFileSync(OUTPUT_JSON_PATH, `${JSON.stringify(report, null, 2)}\n`);
};

const printSummary = (report: BenchmarkReport) => {
  console.log(
    `\nValidation benchmark written to ${report.config.resultPath}\n`,
  );
  const rows = report.results.map((result) => ({
    size: result.size,
    runs: result.measuredRuns,
    phaseA_p50: formatMs(result.phaseA.p50),
    phaseB_p50: formatMs(result.phaseB.p50),
    total_p50: formatMs(result.total.p50),
    total_p95: formatMs(result.total.p95),
    total_mean: formatMs(result.total.mean),
    throughput_mean: formatTps(result.throughputTps.mean),
  }));
  console.table(rows);
};

describe("validation benchmark", () => {
  const timeoutMs = QUICK_MODE ? 180_000 : 900_000;
  const initialLedgerFixture = loadInitialLedgerFixture();
  const txSequenceFixture = loadTxSequenceFixture();
  const preState = buildPreState(initialLedgerFixture);
  const resultsBySize = new Map<number, BenchSizeResult>();

  for (const size of BENCHMARK_SIZES) {
    it(
      `size=${size}`,
      async () => {
        const queuedBlock = buildQueuedBlock(txSequenceFixture, size);
        const measuredRuns =
          size <= 1000 ? SMALL_MEASURED_RUNS : LARGE_MEASURED_RUNS;
        console.log(
          `Running validation benchmark for size=${size} (warmup=${WARMUP_RUNS}, runs=${measuredRuns})`,
        );
        const result = await measureBlockSize(queuedBlock, preState, size);
        resultsBySize.set(size, result);
      },
      timeoutMs,
    );
  }

  it("writes summary report", () => {
    const results: BenchSizeResult[] = [];
    for (const size of BENCHMARK_SIZES) {
      const result = resultsBySize.get(size);
      assertOrThrow(
        result !== undefined,
        `Missing benchmark result for size ${size}`,
      );
      results.push(result);
    }

    const report: BenchmarkReport = {
      meta: {
        generatedAtIso: new Date().toISOString(),
        benchmarkVersion: BENCHMARK_VERSION,
        hostname: os.hostname(),
        cpuModel: os.cpus()[0]?.model ?? "unknown",
        cpuCount: os.cpus().length,
        platform: `${os.platform()} ${os.release()}`,
        nodeVersion: process.version,
        gitCommit: getGitCommit(),
      },
      config: {
        fixtureDirectory: FIXTURE_DIRECTORY,
        initialLedgerFixturePath: INITIAL_LEDGER_FIXTURE_PATH,
        txSequenceFixturePath: TX_SEQUENCE_FIXTURE_PATH,
        quickMode: QUICK_MODE,
        sizes: [...BENCHMARK_SIZES],
        warmupRuns: WARMUP_RUNS,
        smallMeasuredRuns: SMALL_MEASURED_RUNS,
        largeMeasuredRuns: LARGE_MEASURED_RUNS,
        phaseAConcurrency: PHASE_A_CONCURRENCY,
        strictnessProfile: STRICTNESS_PROFILE,
        expectedNetworkId: EXPECTED_NETWORK_ID.toString(),
        minFeeA: MIN_FEE_A.toString(),
        minFeeB: MIN_FEE_B.toString(),
        resultPath: OUTPUT_JSON_PATH,
      },
      results,
    };

    writeReport(report);
    printSummary(report);

    expect(report.results).toHaveLength(BENCHMARK_SIZES.length);
    for (const result of report.results) {
      expect(result.acceptedCountObserved).toBe(result.acceptedCountExpected);
    }
  });
});
