#!/usr/bin/env node
import fs from "node:fs";
import path from "node:path";

export const defectSignatures = {
  "DEF-001": {
    any: [
      "DatabaseInitializationError",
      "Commitment worker failed",
      "Confirmation worker failed",
    ],
  },
  "DEF-002": {
    all: ['"observedCommittedCount":0', '"finalityTimedOutCount":'],
  },
  "DEF-003": { any: ["state-queue mutation lease is busy"] },
};

export const findDefectSignatures = (logText, signatures = defectSignatures) =>
  Object.entries(signatures)
    .filter(([, matcher]) => {
      const any = Array.isArray(matcher) ? matcher : (matcher.any ?? []);
      const all = Array.isArray(matcher) ? [] : (matcher.all ?? []);
      return (
        (any.length > 0 && any.some((needle) => logText.includes(needle))) ||
        (all.length > 0 && all.every((needle) => logText.includes(needle)))
      );
    })
    .map(([defectId]) => defectId)
    .sort();

export const tagReportWithDefects = ({ reportPath, logPaths }) => {
  const report = JSON.parse(fs.readFileSync(reportPath, "utf8"));
  const logText = logPaths
    .filter((logPath) => fs.existsSync(logPath))
    .map((logPath) => fs.readFileSync(logPath, "utf8"))
    .join("\n");
  const observed = findDefectSignatures(
    `${logText}\n${JSON.stringify(report)}`,
  );
  report.defectSignaturesObserved = observed;
  fs.writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`);
  return observed;
};

const parseArgs = (argv) => {
  const args = { report: null, logs: [] };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === "--report") {
      args.report = argv[++index];
    } else if (arg === "--log") {
      args.logs.push(argv[++index]);
    } else if (arg === "--logs-dir") {
      const logsDir = argv[++index];
      if (fs.existsSync(logsDir)) {
        args.logs.push(
          ...fs
            .readdirSync(logsDir)
            .filter((name) => /\.(log|ndjson|json)$/.test(name))
            .map((name) => path.join(logsDir, name)),
        );
      }
    } else {
      throw new Error(`unknown argument: ${arg}`);
    }
  }
  if (!args.report) {
    throw new Error("--report is required");
  }
  return args;
};

const main = () => {
  const args = parseArgs(process.argv.slice(2));
  const observed = tagReportWithDefects({
    reportPath: args.report,
    logPaths: args.logs,
  });
  process.stdout.write(`${JSON.stringify({ defectSignaturesObserved: observed })}\n`);
};

if (import.meta.url === `file://${process.argv[1]}`) {
  try {
    main();
  } catch (error) {
    process.stderr.write(`${error.stack ?? error.message}\n`);
    process.exit(2);
  }
}
