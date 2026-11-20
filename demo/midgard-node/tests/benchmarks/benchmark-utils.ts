import { execSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

export type BenchmarkMeta = {
  readonly generatedAtIso: string;
  readonly benchmarkVersion: string;
  readonly hostname: string;
  readonly cpuModel: string;
  readonly cpuCount: number;
  readonly platform: string;
  readonly nodeVersion: string;
  readonly gitCommit: string;
};

export type OperationStats = {
  readonly name: string;
  readonly runs: number;
  readonly minMs: number;
  readonly p50Ms: number;
  readonly p95Ms: number;
  readonly maxMs: number;
  readonly meanMs: number;
  readonly throughputPerSec: number;
};

export type StatSummary = {
  readonly min: number;
  readonly p50: number;
  readonly p95: number;
  readonly max: number;
  readonly mean: number;
};

export type BenchmarkRunOptions = {
  readonly warmupRuns: number;
  readonly measuredRuns: number;
};

export const getGitCommit = (): string => {
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

export const buildBenchmarkMeta = (
  benchmarkVersion: string,
): BenchmarkMeta => ({
  generatedAtIso: new Date().toISOString(),
  benchmarkVersion,
  hostname: os.hostname(),
  cpuModel: os.cpus()[0]?.model ?? "unknown",
  cpuCount: os.cpus().length,
  platform: `${os.platform()} ${os.release()}`,
  nodeVersion: process.version,
  gitCommit: getGitCommit(),
});

export const quantile = (values: readonly number[], q: number): number => {
  if (values.length === 0) {
    throw new Error("quantile requires at least one value");
  }
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

export const summarizeStats = (values: readonly number[]): StatSummary => {
  if (values.length === 0) {
    throw new Error("summary requires at least one value");
  }
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

export const summarizeOperation = (
  name: string,
  runTimesMs: readonly number[],
  txCount: number,
): OperationStats => {
  const summary = summarizeStats(runTimesMs);
  return {
    name,
    runs: runTimesMs.length,
    minMs: summary.min,
    p50Ms: summary.p50,
    p95Ms: summary.p95,
    maxMs: summary.max,
    meanMs: summary.mean,
    throughputPerSec: txCount / (summary.mean / 1000),
  };
};

export const benchmarkOperation = async (
  name: string,
  run: () => void | Promise<void>,
  txCount: number,
  { warmupRuns, measuredRuns }: BenchmarkRunOptions,
): Promise<OperationStats> => {
  for (let i = 0; i < warmupRuns; i++) {
    await run();
  }

  const runTimesMs: number[] = [];
  for (let i = 0; i < measuredRuns; i++) {
    const start = performance.now();
    await run();
    runTimesMs.push(performance.now() - start);
  }

  return summarizeOperation(name, runTimesMs, txCount);
};

export const writeBenchmarkJson = (
  outputPath: string,
  report: unknown,
  options: { readonly trailingNewline?: boolean } = {},
): void => {
  fs.mkdirSync(path.dirname(outputPath), { recursive: true });
  const json = JSON.stringify(report, null, 2);
  fs.writeFileSync(
    outputPath,
    options.trailingNewline === true ? `${json}\n` : json,
  );
};

export const formatMs = (value: number): string => `${value.toFixed(2)} ms`;

export const formatTps = (value: number): string => `${value.toFixed(2)} tx/s`;

export const printOperationTable = (
  operations: readonly OperationStats[],
): void => {
  console.table(
    operations.map((op) => ({
      operation: op.name,
      runs: op.runs,
      p50: formatMs(op.p50Ms),
      p95: formatMs(op.p95Ms),
      mean: formatMs(op.meanMs),
      throughput: formatTps(op.throughputPerSec),
    })),
  );
};
