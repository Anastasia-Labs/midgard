#!/usr/bin/env node
import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const REPORT_VERSION = "1.0.0";
const BENCHMARK_NAME = "da_attestation_add_signature_capacity_curve";
const MAX_INDEXED_SIGNER_COUNT = 256;
const DEFAULT_MAX_SIZE = MAX_INDEXED_SIGNER_COUNT - 1;
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
  "benchmarks/output/da-attestation-capacity.json",
);

const usage =
  () => `Usage: node onchain/aiken/scripts/da-attestation-capacity-report.mjs [options]

Options:
  --max-size <n>                 Aiken benchmark max size. Default: ${DEFAULT_MAX_SIZE}
                                  Size n measures n + 1 signatures.
  --seed <n>                     Forwarded to aiken bench for reproducible sampling.
  --output <path>                Report output path. Default: ${path.relative(repoRoot, defaultOutputPath)}
  --fail-below <n>               Exit non-zero if max fitting signatures is below n.
  --cardano-max-tx-mem <n>       Cardano max transaction memory units.
                                  Default: ${DEFAULT_CARDANO_MAX_TX_EX_UNITS.memory}
                                  Env override: CARDANO_MAX_TX_EX_UNITS_MEM
  --cardano-max-tx-cpu <n>       Cardano max transaction CPU/step units.
                                  Default: ${DEFAULT_CARDANO_MAX_TX_EX_UNITS.cpu}
                                  Env override: CARDANO_MAX_TX_EX_UNITS_CPU
  --help                         Show this help.
`;

const parseNonNegativeInteger = (source, value) => {
  if (!/^(0|[1-9][0-9]*)$/.test(value ?? "")) {
    throw new Error(
      `${source} must be a non-negative integer, got ${JSON.stringify(value)}`,
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

const parsePositiveInteger = (source, value) => {
  const parsed = parseNonNegativeInteger(source, value);
  if (parsed <= 0) {
    throw new Error(`${source} must be greater than zero`);
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
    outputPath: defaultOutputPath,
    failBelow: undefined,
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
      case "--help":
      case "-h":
        process.stdout.write(usage());
        process.exit(0);
      case "--max-size": {
        const value = takeOptionValue(args, i, arg);
        options.maxSize = parseNonNegativeInteger(arg, value);
        i += 1;
        break;
      }
      case "--seed": {
        options.seed = takeOptionValue(args, i, arg);
        i += 1;
        break;
      }
      case "--output": {
        const value = takeOptionValue(args, i, arg);
        options.outputPath = path.resolve(process.cwd(), value);
        i += 1;
        break;
      }
      case "--fail-below": {
        const value = takeOptionValue(args, i, arg);
        options.failBelow = parsePositiveInteger(arg, value);
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

  if (options.maxSize >= MAX_INDEXED_SIGNER_COUNT) {
    throw new Error(
      `--max-size must be at most ${MAX_INDEXED_SIGNER_COUNT - 1}; size n measures n + 1 signatures`,
    );
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

const buildAikenBenchArgs = (options) => {
  const args = [
    "bench",
    "--max-size",
    String(options.maxSize),
    "--match-benchmarks",
    BENCHMARK_NAME,
  ];

  if (options.seed !== undefined) {
    args.push("--seed", options.seed);
  }

  return args;
};

const readCapacityMeasures = (aikenReport, budget) => {
  const benchmark = aikenReport.benchmarks?.find(
    (entry) => entry.name === BENCHMARK_NAME,
  );
  if (!benchmark) {
    throw new Error(`aiken bench output did not include ${BENCHMARK_NAME}`);
  }
  if (!Array.isArray(benchmark.measures)) {
    throw new Error(`${BENCHMARK_NAME} is missing measures[]`);
  }

  return benchmark.measures
    .map((measure) => {
      const signatureCount = measure.size + 1;
      const fitsMemory = measure.memory <= budget.memory;
      const fitsCpu = measure.cpu <= budget.cpu;
      return {
        size: measure.size,
        signatureCount,
        memory: measure.memory,
        cpu: measure.cpu,
        fitsMemory,
        fitsCpu,
        fitsTransactionBudget: fitsMemory && fitsCpu,
      };
    })
    .sort((left, right) => left.signatureCount - right.signatureCount);
};

const computeSummary = (measures) => {
  const fitting = measures.filter((measure) => measure.fitsTransactionBudget);
  const maxFitting = fitting.at(-1) ?? null;
  const firstExceeding = measures.find(
    (measure) => !measure.fitsTransactionBudget,
  ) ?? null;
  const bindingBudget =
    firstExceeding === null
      ? null
      : firstExceeding.fitsMemory
        ? "cpu"
        : firstExceeding.fitsCpu
          ? "memory"
          : "memory-and-cpu";

  return {
    maxFitting,
    firstExceeding,
    bindingBudget,
  };
};

const printSummary = (report) => {
  const { maxFitting, firstExceeding, bindingBudget } = report.summary;
  if (maxFitting === null) {
    console.log("No measured signature count fits within the tx budget.");
  } else {
    console.log(
      `Max fitting DA signatures: ${maxFitting.signatureCount} ` +
        `(${maxFitting.memory} mem, ${maxFitting.cpu} cpu)`,
    );
  }

  if (firstExceeding !== null) {
    console.log(
      `First exceeding count: ${firstExceeding.signatureCount} ` +
        `(${firstExceeding.memory} mem, ${firstExceeding.cpu} cpu; binding: ${bindingBudget})`,
    );
  }

  console.log(`Wrote ${path.relative(repoRoot, report.meta.outputPath)}`);
};

const main = () => {
  const options = parseArgs(process.argv.slice(2));
  const benchResult = run("aiken", buildAikenBenchArgs(options), projectDir);

  if (benchResult.status !== 0) {
    process.stdout.write(benchResult.stdout);
    process.stderr.write(benchResult.stderr);
    process.exit(benchResult.status ?? 1);
  }

  const aikenReport = parseAikenBenchJson(benchResult.stdout);
  const measures = readCapacityMeasures(
    aikenReport,
    options.cardanoMaxTxExUnits,
  );
  const summary = computeSummary(measures);

  const report = {
    meta: {
      generatedAtIso: new Date().toISOString(),
      reportVersion: REPORT_VERSION,
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
      benchmarkName: BENCHMARK_NAME,
      maxSize: options.maxSize,
      signatureCountForSize: "size + 1",
      maxIndexedSignerCount: MAX_INDEXED_SIGNER_COUNT,
      seed: options.seed,
      cardanoMaxTxExUnits: options.cardanoMaxTxExUnits,
      budgetPercentBasis: "per-transaction Cardano maxTxExUnits",
    },
    seed: aikenReport.seed,
    summary,
    measures,
  };

  fs.mkdirSync(path.dirname(options.outputPath), { recursive: true });
  fs.writeFileSync(options.outputPath, `${JSON.stringify(report, null, 2)}\n`);
  printSummary(report);

  if (
    options.failBelow !== undefined &&
    (summary.maxFitting === null ||
      summary.maxFitting.signatureCount < options.failBelow)
  ) {
    throw new Error(
      `max fitting signature count is below ${options.failBelow}`,
    );
  }
};

try {
  main();
} catch (error) {
  console.error(error instanceof Error ? error.message : error);
  process.exit(1);
}
