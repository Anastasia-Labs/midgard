#!/usr/bin/env node
import fs from "node:fs";
import path from "node:path";

const DEFAULT_REGRESSION_THRESHOLD = 0.10;
const DEFAULT_TRAILING_WINDOW = 5;

export const median = (values) => {
  if (values.length === 0) {
    return null;
  }
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 0
    ? (sorted[mid - 1] + sorted[mid]) / 2
    : sorted[mid];
};

const toFiniteNumber = (value) => {
  const number = Number(value);
  return Number.isFinite(number) ? number : null;
};

const sanitizeScenario = (scenario) =>
  String(scenario ?? "unknown")
    .trim()
    .replace(/[^A-Za-z0-9_.-]+/g, "-");

const reportScenario = (report, filePath) =>
  sanitizeScenario(
    report.scenario ??
      report.config?.scenario ??
      report.config?.env?.STRESS_SCENARIO_NAME ??
      report.summary?.scenario ??
      path.basename(filePath ?? "unknown", ".json"),
  );

const reportSha = (report) =>
  report.sha ??
  report.metadata?.git?.sha ??
  report.metadata?.git?.commit ??
  process.env.GITHUB_SHA ??
  "unknown";

const reportTimestamp = (report) =>
  report.ts ?? report.generatedAtIso ?? new Date().toISOString();

const pushEntry = (entries, entry) => {
  if (entry.tps !== null && entry.tps >= 0) {
    entries.push(entry);
  }
};

export const extractTrendEntriesFromReport = (report, filePath = "") => {
  if (report.status === "blocked") {
    return [];
  }
  const scenario = reportScenario(report, filePath);
  const sha = reportSha(report);
  const ts = reportTimestamp(report);
  const base = { sha, ts, scenario };
  const entries = [];

  for (const explicit of report.trendStages ?? []) {
    pushEntry(entries, {
      ...base,
      stage: String(explicit.stage),
      tps: toFiniteNumber(explicit.tps),
      classA: explicit.classA === true,
    });
  }
  if (entries.length > 0) {
    return entries;
  }

  for (const stage of report.stages ?? []) {
    const elapsedSec = toFiniteNumber(stage.measuredElapsedSec);
    pushEntry(entries, {
      ...base,
      stage: "offered",
      tps: toFiniteNumber(stage.queuedSubmitSuccessPerSec),
      classA: true,
      sourceStage: stage.name,
    });
    pushEntry(entries, {
      ...base,
      stage: "accepted",
      tps: toFiniteNumber(stage.measuredAcceptedTps),
      classA: true,
      sourceStage: stage.name,
    });
    pushEntry(entries, {
      ...base,
      stage: "committed",
      tps:
        elapsedSec && elapsedSec > 0
          ? toFiniteNumber(stage.commitTxDelta) / elapsedSec
          : null,
      classA: false,
      sourceStage: stage.name,
    });
    pushEntry(entries, {
      ...base,
      stage: "finality",
      tps:
        elapsedSec && elapsedSec > 0
          ? toFiniteNumber(stage.mergeBlockDelta) / elapsedSec
          : null,
      classA: false,
      sourceStage: stage.name,
    });
  }

  if (entries.length > 0) {
    return entries;
  }

  const summary = report.summary ?? {};
  pushEntry(entries, {
    ...base,
    stage: "offered",
    tps: toFiniteNumber(summary.queuedSubmitSuccessPerSec),
    classA: true,
  });
  pushEntry(entries, {
    ...base,
    stage: "accepted",
    tps: toFiniteNumber(summary.avgAcceptedTps),
    classA: true,
  });
  pushEntry(entries, {
    ...base,
    stage: "committed",
    tps: toFiniteNumber(summary.committedTxPerSec),
    classA: false,
  });
  pushEntry(entries, {
    ...base,
    stage: "finality",
    tps: toFiniteNumber(summary.mergeBlocksPerSec),
    classA: false,
  });
  return entries;
};

export const parseNdjson = (text, source = "ndjson") =>
  text
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter(Boolean)
    .map((line, index) => {
      try {
        return JSON.parse(line);
      } catch (error) {
        throw new Error(`${source}:${index + 1}: ${error.message}`);
      }
    });

export const readTrendEntries = (trendDir, scenario) => {
  const filePath = path.join(trendDir, `${sanitizeScenario(scenario)}.ndjson`);
  if (!fs.existsSync(filePath)) {
    return [];
  }
  return parseNdjson(fs.readFileSync(filePath, "utf8"), filePath);
};

export const evaluateBenchmarkRegressions = ({
  trendEntries,
  currentEntries,
  classAOnly = true,
  threshold = DEFAULT_REGRESSION_THRESHOLD,
  trailingWindow = DEFAULT_TRAILING_WINDOW,
}) => {
  const results = [];
  for (const current of currentEntries) {
    if (classAOnly && current.classA !== true) {
      results.push({ current, status: "informational_class_b" });
      continue;
    }
    const history = trendEntries.filter(
      (entry) =>
        entry.scenario === current.scenario &&
        entry.stage === current.stage &&
        entry.classA === true &&
        Number.isFinite(Number(entry.tps)),
    );
    if (history.length < trailingWindow) {
      results.push({
        current,
        status: "bootstrap",
        historicalCount: history.length,
        requiredHistoricalCount: trailingWindow,
      });
      continue;
    }
    const trailing = history.slice(-trailingWindow).map((entry) => Number(entry.tps));
    const trailingMedian = median(trailing);
    const currentTps = Number(current.tps);
    const regressionPct =
      trailingMedian && trailingMedian > 0
        ? (trailingMedian - currentTps) / trailingMedian
        : 0;
    results.push({
      current,
      status: regressionPct > threshold ? "regression" : "ok",
      historicalCount: history.length,
      trailingMedian,
      regressionPct,
      threshold,
    });
  }
  return results;
};

const collectJsonFiles = (inputPath) => {
  if (!fs.existsSync(inputPath)) {
    return [];
  }
  const stat = fs.statSync(inputPath);
  if (stat.isFile()) {
    return inputPath.endsWith(".json") ? [inputPath] : [];
  }
  return fs
    .readdirSync(inputPath, { withFileTypes: true })
    .flatMap((entry) => {
      const child = path.join(inputPath, entry.name);
      if (entry.isDirectory()) {
        return collectJsonFiles(child);
      }
      return entry.isFile() && entry.name.endsWith(".json") ? [child] : [];
    })
    .sort();
};

export const loadCurrentEntries = (resultsPath) =>
  collectJsonFiles(resultsPath).flatMap((filePath) => {
    const report = JSON.parse(fs.readFileSync(filePath, "utf8"));
    return extractTrendEntriesFromReport(report, filePath);
  });

export const appendTrendEntries = ({ trendDir, entries }) => {
  fs.mkdirSync(trendDir, { recursive: true });
  for (const entry of entries) {
    const filePath = path.join(trendDir, `${sanitizeScenario(entry.scenario)}.ndjson`);
    fs.appendFileSync(filePath, `${JSON.stringify(entry)}\n`);
  }
};

const parseArgs = (argv) => {
  const args = {
    trendDir: "docs/benchmark-trends",
    results: null,
    classAOnly: false,
    append: false,
  };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === "--trend-dir") {
      args.trendDir = argv[++index];
    } else if (arg === "--results") {
      args.results = argv[++index];
    } else if (arg === "--class-a-only") {
      args.classAOnly = true;
    } else if (arg === "--append") {
      args.append = true;
    } else {
      throw new Error(`unknown argument: ${arg}`);
    }
  }
  if (!args.results) {
    throw new Error("--results is required");
  }
  return args;
};

const main = () => {
  const args = parseArgs(process.argv.slice(2));
  const currentEntries = loadCurrentEntries(args.results);
  if (args.append) {
    appendTrendEntries({ trendDir: args.trendDir, entries: currentEntries });
  }
  const trendEntries = [
    ...new Set(currentEntries.map((entry) => entry.scenario)),
  ].flatMap((scenario) => readTrendEntries(args.trendDir, scenario));
  const evaluations = evaluateBenchmarkRegressions({
    trendEntries,
    currentEntries,
    classAOnly: args.classAOnly,
  });
  const regressions = evaluations.filter(
    (evaluation) => evaluation.status === "regression",
  );
  process.stdout.write(
    `${JSON.stringify(
      {
        currentEntryCount: currentEntries.length,
        regressionCount: regressions.length,
        evaluations,
      },
      null,
      2,
    )}\n`,
  );
  if (regressions.length > 0) {
    process.exit(1);
  }
};

if (import.meta.url === `file://${process.argv[1]}`) {
  try {
    main();
  } catch (error) {
    process.stderr.write(`${error.stack ?? error.message}\n`);
    process.exit(2);
  }
}
