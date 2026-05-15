#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const BENCHMARK_VERSION = "1.0.0";
const DEFAULT_MAX_SIZE = 1;
const DEFAULT_CARDANO_MAX_TX_EX_UNITS = {
  memory: 16_500_000,
  cpu: 10_000_000_000,
};

const scriptPath = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(scriptPath);
const projectDir = path.resolve(scriptDir, "..");
const repoRoot = path.resolve(projectDir, "../..");
const defaultOutputPath = path.resolve(
  projectDir,
  "benchmarks/output/native-tx-decoder-benchmark.json",
);

const usage =
  () => `Usage: node onchain/aiken/scripts/native-tx-decoder-benchmark-report.mjs [options]

Options:
  --max-size <n>                 Aiken benchmark max size. Default: ${DEFAULT_MAX_SIZE}
  --seed <n>                     Forwarded to aiken bench for reproducible sampling.
  -m, --match-benchmarks <text>  Forwarded to aiken bench. Can be repeated.
  --exact-match                  Forwarded to aiken bench.
  --output <path>                Report output path. Default: ${path.relative(repoRoot, defaultOutputPath)}
  --cardano-max-tx-mem <n>       Cardano max transaction memory units.
                                  Default: ${DEFAULT_CARDANO_MAX_TX_EX_UNITS.memory}
                                  Env override: CARDANO_MAX_TX_EX_UNITS_MEM
  --cardano-max-tx-cpu <n>       Cardano max transaction CPU/step units.
                                  Default: ${DEFAULT_CARDANO_MAX_TX_EX_UNITS.cpu}
                                  Env override: CARDANO_MAX_TX_EX_UNITS_CPU
  --help                         Show this help.
`;

const parsePositiveInteger = (source, value) => {
  if (!/^[1-9][0-9]*$/.test(value ?? "")) {
    throw new Error(
      `${source} must be a positive integer, got ${JSON.stringify(value)}`,
    );
  }

  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) {
    throw new Error(
      `${source} exceeds JavaScript's safe integer range: ${value}`,
    );
  }

  return parsed;
};

const takeOptionValue = (args, index, option) => {
  const value = args[index + 1];
  if (value === undefined || value.startsWith("-")) {
    throw new Error(`${option} requires a value`);
  }
  return value;
};

const parseArgs = (args) => {
  const options = {
    maxSize: DEFAULT_MAX_SIZE,
    seed: undefined,
    matchBenchmarks: [],
    exactMatch: false,
    outputPath: defaultOutputPath,
    cardanoMaxTxExUnits: {
      memory: process.env.CARDANO_MAX_TX_EX_UNITS_MEM
        ? parsePositiveInteger(
            "CARDANO_MAX_TX_EX_UNITS_MEM",
            process.env.CARDANO_MAX_TX_EX_UNITS_MEM,
          )
        : DEFAULT_CARDANO_MAX_TX_EX_UNITS.memory,
      cpu: process.env.CARDANO_MAX_TX_EX_UNITS_CPU
        ? parsePositiveInteger(
            "CARDANO_MAX_TX_EX_UNITS_CPU",
            process.env.CARDANO_MAX_TX_EX_UNITS_CPU,
          )
        : DEFAULT_CARDANO_MAX_TX_EX_UNITS.cpu,
    },
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    switch (arg) {
      case "--":
        break;
      case "--help":
      case "-h":
        process.stdout.write(usage());
        process.exit(0);
      case "--max-size": {
        const value = takeOptionValue(args, i, arg);
        options.maxSize = parsePositiveInteger(arg, value);
        i += 1;
        break;
      }
      case "--seed": {
        options.seed = takeOptionValue(args, i, arg);
        i += 1;
        break;
      }
      case "--match-benchmarks":
      case "-m": {
        const value = takeOptionValue(args, i, arg);
        options.matchBenchmarks.push(value);
        i += 1;
        break;
      }
      case "--exact-match":
        options.exactMatch = true;
        break;
      case "--output": {
        const value = takeOptionValue(args, i, arg);
        options.outputPath = path.resolve(process.cwd(), value);
        i += 1;
        break;
      }
      case "--cardano-max-tx-mem": {
        const value = takeOptionValue(args, i, arg);
        options.cardanoMaxTxExUnits.memory = parsePositiveInteger(arg, value);
        i += 1;
        break;
      }
      case "--cardano-max-tx-cpu": {
        const value = takeOptionValue(args, i, arg);
        options.cardanoMaxTxExUnits.cpu = parsePositiveInteger(arg, value);
        i += 1;
        break;
      }
      default:
        throw new Error(`Unknown option: ${arg}\n\n${usage()}`);
    }
  }

  return options;
};

const run = (command, args, cwd) => {
  const result = spawnSync(command, args, {
    cwd,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  });

  if (result.error) {
    throw result.error;
  }

  return result;
};

const getGitCommit = () => {
  const result = run("git", ["rev-parse", "--short", "HEAD"], repoRoot);
  if (result.status !== 0) {
    return "unknown";
  }
  return result.stdout.trim() || "unknown";
};

const getAikenVersion = () => {
  const result = run("aiken", ["--version"], projectDir);
  if (result.status !== 0) {
    return "unknown";
  }
  return result.stdout.trim() || "unknown";
};

const parseAikenBenchJson = (stdout) => {
  const marker = '"benchmarks"';
  const markerIndex = stdout.indexOf(marker);
  if (markerIndex < 0) {
    throw new Error(
      "aiken bench output did not contain a benchmarks JSON object",
    );
  }

  const jsonStart = stdout.lastIndexOf("{", markerIndex);
  if (jsonStart < 0) {
    throw new Error("could not locate start of aiken bench JSON output");
  }

  try {
    return JSON.parse(stdout.slice(jsonStart));
  } catch (error) {
    throw new Error(
      `failed to parse aiken bench JSON output: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
};

const roundPercent = (value) => Math.round(value * 1_000_000) / 1_000_000;

const withBudgetPercentages = (aikenReport, cardanoMaxTxExUnits) => {
  if (!Array.isArray(aikenReport.benchmarks)) {
    throw new Error("aiken bench JSON is missing benchmarks[]");
  }

  return aikenReport.benchmarks
    .map((benchmark) => {
      if (!Array.isArray(benchmark.measures)) {
        throw new Error(
          `benchmark ${benchmark.name ?? "<unknown>"} is missing measures[]`,
        );
      }

      const measures = benchmark.measures
        .map((measure) => {
          if (
            !Number.isFinite(measure.memory) ||
            !Number.isFinite(measure.cpu)
          ) {
            throw new Error(
              `benchmark ${benchmark.name ?? "<unknown>"} has a measure without numeric memory/cpu`,
            );
          }

          return {
            ...measure,
            memoryPctOfCardanoMaxTxBudget: roundPercent(
              (measure.memory / cardanoMaxTxExUnits.memory) * 100,
            ),
            cpuPctOfCardanoMaxTxBudget: roundPercent(
              (measure.cpu / cardanoMaxTxExUnits.cpu) * 100,
            ),
          };
        })
        .sort((left, right) => left.size - right.size);

      if (measures.length === 0) {
        throw new Error(
          `benchmark ${benchmark.name ?? "<unknown>"} has no measures`,
        );
      }

      const maxMemory = Math.max(...measures.map((measure) => measure.memory));
      const maxCpu = Math.max(...measures.map((measure) => measure.cpu));

      return {
        ...benchmark,
        maxMemory,
        maxCpu,
        maxMemoryPctOfCardanoMaxTxBudget: roundPercent(
          (maxMemory / cardanoMaxTxExUnits.memory) * 100,
        ),
        maxCpuPctOfCardanoMaxTxBudget: roundPercent(
          (maxCpu / cardanoMaxTxExUnits.cpu) * 100,
        ),
        exceedsCardanoMaxTxMemoryBudget: maxMemory > cardanoMaxTxExUnits.memory,
        exceedsCardanoMaxTxCpuBudget: maxCpu > cardanoMaxTxExUnits.cpu,
        measures,
      };
    })
    .sort((left, right) =>
      `${left.module}.${left.name}`.localeCompare(
        `${right.module}.${right.name}`,
      ),
    );
};

const buildAikenBenchArgs = (options) => {
  const args = ["bench", "--max-size", String(options.maxSize)];

  if (options.seed !== undefined) {
    args.push("--seed", options.seed);
  }

  for (const match of options.matchBenchmarks) {
    args.push("--match-benchmarks", match);
  }

  if (options.exactMatch) {
    args.push("--exact-match");
  }

  return args;
};

const formatPercent = (value) => `${value.toFixed(2)}%`;

const printSummary = (report) => {
  const rows = report.benchmarks.flatMap((benchmark) =>
    benchmark.measures.map((measure) => ({
      benchmark: `${benchmark.module}.${benchmark.name}`,
      size: measure.size,
      memory: measure.memory,
      cpu: measure.cpu,
      memoryPct: formatPercent(measure.memoryPctOfCardanoMaxTxBudget),
      cpuPct: formatPercent(measure.cpuPctOfCardanoMaxTxBudget),
    })),
  );

  console.table(rows);
  console.log(`Wrote ${path.relative(repoRoot, report.meta.outputPath)}`);
};

const main = () => {
  const options = parseArgs(process.argv.slice(2));
  const aikenArgs = buildAikenBenchArgs(options);
  const benchResult = run("aiken", aikenArgs, projectDir);

  if (benchResult.status !== 0) {
    process.stdout.write(benchResult.stdout);
    process.stderr.write(benchResult.stderr);
    process.exit(benchResult.status ?? 1);
  }

  const aikenReport = parseAikenBenchJson(benchResult.stdout);
  const report = {
    meta: {
      generatedAtIso: new Date().toISOString(),
      benchmarkVersion: BENCHMARK_VERSION,
      tool: "aiken bench",
      aikenVersion: getAikenVersion(),
      hostname: os.hostname(),
      platform: process.platform,
      nodeVersion: process.version,
      gitCommit: getGitCommit(),
      projectDir,
      outputPath: options.outputPath,
    },
    config: {
      maxSize: options.maxSize,
      seed: options.seed,
      matchBenchmarks: options.matchBenchmarks,
      exactMatch: options.exactMatch,
      cardanoMaxTxExUnits: options.cardanoMaxTxExUnits,
      budgetPercentBasis: "per-transaction Cardano maxTxExUnits",
    },
    seed: aikenReport.seed,
    benchmarks: withBudgetPercentages(aikenReport, options.cardanoMaxTxExUnits),
  };

  fs.mkdirSync(path.dirname(options.outputPath), { recursive: true });
  fs.writeFileSync(options.outputPath, `${JSON.stringify(report, null, 2)}\n`);
  printSummary(report);
};

try {
  main();
} catch (error) {
  console.error(error instanceof Error ? error.message : error);
  process.exit(1);
}
