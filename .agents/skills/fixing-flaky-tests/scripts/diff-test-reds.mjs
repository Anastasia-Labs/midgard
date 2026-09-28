#!/usr/bin/env node
// Compares the failures in vitest JSON reports with a program's list of
// accepted failures, so a gate run investigates only the failures that are new.
//
// Usage:
//   node diff-test-reds.mjs --accepted <list.json> --suite <suite> [--log <run.log>]... <report.json> [<report.json>...] [--json]
//
//   --accepted  the accepted-failures list (format below)
//   --suite     which list entries apply: the package directory name the
//               reports come from, e.g. midgard-watcher (not demo/midgard-watcher)
//   --log       the default reporter's output of a run, repeatable. The JSON
//               report leaves out errors raised outside any test (an unhandled
//               rejection fails the run while every test passes), so a log
//               whose summary counts errors, or that has no summary because
//               the run did not finish, is a NEW failure, unless the list
//               accepts the errors (see "(run)" below). Without --log the
//               tool cannot see either, and warns.
//   --json      print the result as JSON instead of text
//
// Produce a report and its log with (delete the old report first: vitest
// writes it only at the end, so a killed run leaves the previous one):
//   rm -f <report.json>
//   <env> pnpm --dir demo/<package> exec vitest run <files> <flags> --reporter=default --reporter=json --outputFile=<report.json> 2>&1 | tee <run.log>
// where <env> and <flags> are what the package's `test` script sets (watcher:
// env MALLOC_MMAP_THRESHOLD_=131072; node and node-tools: NODE_ENV=emulator
// and --disableConsoleIntercept, node-tools after its node --test preludes).
//
// The list:
//   { "version": 1, "program": string, "base": string, "generatedFrom": [string],
//     "entries": [ { "suite": string, "file": string, "name": string,
//                    "kind": "deterministic" | "flaky", "reason": string } ] }
// `file` is relative to the package directory ("tests/x.test.ts"); `name` is
// the test's `assertionResults[].fullName`, or "*" for every test in the file
// and for a file that failed as a whole (failed to load, or a hook threw).
// A "*" entry also accepts any new failure in its file: failures it accepts
// are marked "via *" (`wildcard: true` in JSON), and a "*" entry whose file
// also has passing tests is warned about.
// An entry with file "(run)" records the base's errors outside any test. Its
// name is the tool's own wording, "errors outside any test: vitest reported
// <M> error(s); they are not in the JSON report", and it accepts a log that
// reports N such errors when N <= M. A "(run)" entry whose name carries no
// count is malformed (exit 2). A run that printed no summary is always NEW.
// Names are compared with surrounding whitespace trimmed: vitest 3.0.7 puts a
// leading space in `fullName` when the run has no configured project name.
//
// It prints the NEW failures (no entry), the ACCEPTED failures, the
// ACCEPTED-FLAKY failures, the STALE deterministic entries (their tests ran
// and passed), and counts the flaky entries that passed and the entries with no
// result in these reports.
//
// Exit codes:
//   0  no new failure
//   1  at least one new failure
//   2  bad arguments, or a list or report that is unreadable or malformed:
//      nothing was compared

import { readFileSync } from "node:fs";
import { isAbsolute, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const USAGE =
  "usage: diff-test-reds.mjs --accepted <list.json> --suite <suite> [--log <run.log>]... <report.json> [<report.json>...] [--json]";

export const WHOLE_FILE = "*";
const FILE_FAILURE_NAME = "(the file failed as a whole)";

export class InputError extends Error {}

export const parseArgs = (argv) => {
  const options = { accepted: undefined, suite: undefined, json: false };
  const reports = [];
  const logs = [];
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--json") {
      options.json = true;
    } else if (arg === "--accepted" || arg === "--suite") {
      const value = argv[i + 1];
      if (value === undefined || value.startsWith("--")) {
        throw new InputError(`${arg} needs a value`);
      }
      options[arg.slice(2)] = value;
      i += 1;
    } else if (arg === "--log") {
      const value = argv[i + 1];
      if (value === undefined || value.startsWith("--")) {
        throw new InputError(`${arg} needs a value`);
      }
      logs.push(value);
      i += 1;
    } else if (arg.startsWith("--")) {
      throw new InputError(`unknown option ${arg}`);
    } else {
      reports.push(arg);
    }
  }
  if (options.accepted === undefined)
    throw new InputError("--accepted is required");
  if (options.suite === undefined) throw new InputError("--suite is required");
  if (reports.length === 0) throw new InputError("no vitest JSON report given");
  return { ...options, reports, logs };
};

const RUN_FILE = "(run)";
const RUN_ERRORS_RE = /vitest reported (\d+) error\(s\)/u;

/** The error count a "(run)" failure or entry names, or null. */
export const runErrorCount = (name) => {
  const match = name.match(RUN_ERRORS_RE);
  return match === null ? null : Number(match[1]);
};

/**
 * The failures a vitest log shows that its JSON report does not: errors
 * outside any test, and a run that never printed its summary.
 */
export const runLevelFailures = ({ label, text }) => {
  const plain = text.replace(/\u001b\[[0-9;]*m/gu, "");
  const failures = [];
  const errors = plain.match(/^\s*Errors\s+(\d+)\s+errors?\b/mu);
  if (errors !== null) {
    failures.push({
      file: RUN_FILE,
      name: `errors outside any test: vitest reported ${errors[1]} error(s); they are not in the JSON report`,
      reports: [label],
    });
  }
  if (!/^\s*Test Files\s/mu.test(plain)) {
    failures.push({
      file: RUN_FILE,
      name: "the run printed no vitest summary: it did not finish, or failed before running tests",
      reports: [label],
    });
  }
  return failures;
};

const isObject = (value) =>
  typeof value === "object" && value !== null && !Array.isArray(value);
const isNonEmptyString = (value) =>
  typeof value === "string" && value.trim() !== "";

const LIST_KEYS = ["version", "program", "base", "generatedFrom", "entries"];
const ENTRY_KEYS = ["suite", "file", "name", "kind", "reason"];

const exactKeys = (value, keys, where) => {
  const extra = Object.keys(value).filter((key) => !keys.includes(key));
  const missing = keys.filter((key) => !(key in value));
  if (extra.length > 0 || missing.length > 0) {
    throw new InputError(
      `${where}: ${[
        ...missing.map((key) => `missing "${key}"`),
        ...extra.map((key) => `unknown key "${key}"`),
      ].join(", ")}`,
    );
  }
};

const normalizeFile = (file) =>
  file.replaceAll("\\", "/").replace(/^\.\//u, "");
const normalizeName = (name) => name.trim();

export const validateAcceptedList = (list, where = "accepted list") => {
  if (!isObject(list)) throw new InputError(`${where}: not a JSON object`);
  exactKeys(list, LIST_KEYS, where);
  if (list.version !== 1) {
    throw new InputError(
      `${where}: version must be 1, got ${JSON.stringify(list.version)}`,
    );
  }
  for (const key of ["program", "base"]) {
    if (!isNonEmptyString(list[key])) {
      throw new InputError(`${where}: "${key}" must be a non-empty string`);
    }
  }
  if (
    !Array.isArray(list.generatedFrom) ||
    !list.generatedFrom.every((item) => typeof item === "string")
  ) {
    throw new InputError(
      `${where}: "generatedFrom" must be an array of strings`,
    );
  }
  if (!Array.isArray(list.entries)) {
    throw new InputError(`${where}: "entries" must be an array`);
  }
  const seen = new Set();
  list.entries.forEach((entry, index) => {
    const at = `${where}: entries[${String(index)}]`;
    if (!isObject(entry)) throw new InputError(`${at}: not an object`);
    exactKeys(entry, ENTRY_KEYS, at);
    for (const key of ["suite", "file", "name", "reason"]) {
      if (!isNonEmptyString(entry[key])) {
        throw new InputError(`${at}: "${key}" must be a non-empty string`);
      }
    }
    if (
      normalizeFile(entry.file) === RUN_FILE &&
      runErrorCount(entry.name) === null
    ) {
      throw new InputError(
        `${at}: a "${RUN_FILE}" entry's name must report the base's count ("... vitest reported <M> error(s) ..."), got ${JSON.stringify(entry.name)}`,
      );
    }
    if (entry.kind !== "deterministic" && entry.kind !== "flaky") {
      throw new InputError(
        `${at}: "kind" must be "deterministic" or "flaky", got ${JSON.stringify(entry.kind)}`,
      );
    }
    const key = JSON.stringify([
      entry.suite,
      normalizeFile(entry.file),
      normalizeName(entry.name),
    ]);
    if (seen.has(key)) {
      throw new InputError(
        `${at}: duplicates an earlier entry for ${entry.suite} ${entry.file} :: ${entry.name}`,
      );
    }
    seen.add(key);
  });
  return list;
};

export const validateReport = (report, where) => {
  if (!isObject(report) || !Array.isArray(report.testResults)) {
    throw new InputError(
      `${where}: not a vitest JSON report (no testResults array)`,
    );
  }
  report.testResults.forEach((file, index) => {
    const at = `${where}: testResults[${String(index)}]`;
    if (
      !isObject(file) ||
      typeof file.name !== "string" ||
      typeof file.status !== "string" ||
      !Array.isArray(file.assertionResults)
    ) {
      throw new InputError(
        `${at}: needs a string name, a string status and an assertionResults array`,
      );
    }
    file.assertionResults.forEach((assertion, inner) => {
      if (
        !isObject(assertion) ||
        typeof assertion.fullName !== "string" ||
        typeof assertion.status !== "string"
      ) {
        throw new InputError(
          `${at}.assertionResults[${String(inner)}]: needs a string fullName and a string status`,
        );
      }
    });
  });
  return report;
};

// Whether a report's file path is the entry's package-relative file. vitest
// reports absolute paths; an absolute path must end in `/<suite>/<file>`, so a
// same-named file in another package never matches.
export const fileMatches = (reportPath, entryFile, suite) => {
  const path = normalizeFile(reportPath);
  const file = normalizeFile(entryFile);
  if (isAbsolute(path) || /^[A-Za-z]:\//u.test(path)) {
    return path.endsWith(`/${suite}/${file}`);
  }
  return path === file || path.endsWith(`/${file}`);
};

// The package-relative path for display, when the path shows the package.
const displayFile = (reportPath, suite) => {
  const path = normalizeFile(reportPath);
  const marker = `/${suite}/`;
  const at = path.lastIndexOf(marker);
  return at === -1 ? path : path.slice(at + marker.length);
};

/**
 * Collects every result per (file, name). A file whose status is failed with
 * no failed test in it (it failed to load, or a hook threw) is recorded as a
 * failure named FILE_FAILURE_NAME, which only a "*" entry accepts.
 */
export const collectResults = (reports) => {
  const results = new Map();
  const record = (reportPath, name, status, reportLabel) => {
    const key = JSON.stringify([reportPath, name]);
    const result = results.get(key) ?? {
      reportPath,
      name,
      statuses: [],
      reports: new Set(),
    };
    result.statuses.push(status);
    result.reports.add(reportLabel);
    results.set(key, result);
  };
  for (const { label, report } of reports) {
    for (const file of report.testResults) {
      for (const assertion of file.assertionResults) {
        record(
          file.name,
          normalizeName(assertion.fullName),
          assertion.status,
          label,
        );
      }
      const anyTestFailed = file.assertionResults.some(
        (assertion) => assertion.status === "failed",
      );
      if (file.status === "failed" && !anyTestFailed) {
        record(file.name, FILE_FAILURE_NAME, "failed", label);
      }
    }
  }
  return [...results.values()];
};

const entryMatches = (entry, result, suite) =>
  fileMatches(result.reportPath, entry.file, suite) &&
  (entry.name === WHOLE_FILE || normalizeName(entry.name) === result.name);

export const diffReds = ({ list, suite, reports, logs = undefined }) => {
  const entries = list.entries.filter((entry) => entry.suite === suite);
  const results = collectResults(reports);
  const failed = results.filter((result) => result.statuses.includes("failed"));
  const show = (result) => ({
    file: displayFile(result.reportPath, suite),
    name: result.name,
    reports: [...result.reports],
  });

  const outcome = {
    new: [],
    accepted: [],
    acceptedFlaky: [],
    stale: [],
    flakyPassed: [],
    notRun: [],
    warnings: [],
  };
  for (const result of failed) {
    const matching = entries.filter((entry) =>
      entryMatches(entry, result, suite),
    );
    const entry =
      matching.find((candidate) => candidate.name !== WHOLE_FILE) ??
      matching[0];
    if (entry === undefined) {
      outcome.new.push(show(result));
    } else {
      (entry.kind === "flaky" ? outcome.acceptedFlaky : outcome.accepted).push({
        ...show(result),
        reason: entry.reason,
        ...(entry.name === WHOLE_FILE ? { wildcard: true } : {}),
      });
    }
  }
  // A "(run)" entry accepts errors outside any test up to the base's count.
  const runEntries = entries.filter(
    (entry) => normalizeFile(entry.file) === RUN_FILE,
  );
  const usedRunEntries = new Set();
  for (const failure of (logs ?? []).flatMap(runLevelFailures)) {
    const count = runErrorCount(failure.name);
    const entry =
      count === null
        ? undefined
        : runEntries.find(
            (candidate) => count <= runErrorCount(candidate.name),
          );
    if (entry === undefined) {
      outcome.new.push(failure);
    } else {
      usedRunEntries.add(entry);
      (entry.kind === "flaky" ? outcome.acceptedFlaky : outcome.accepted).push({
        ...failure,
        reason: entry.reason,
        acceptedUpTo: runErrorCount(entry.name),
      });
    }
  }
  for (const entry of entries) {
    if (usedRunEntries.has(entry)) continue;
    const covered = results.filter((result) =>
      entryMatches(entry, result, suite),
    );
    const summary = {
      file: entry.file,
      name: entry.name,
      kind: entry.kind,
      reason: entry.reason,
    };
    if (covered.some((result) => result.statuses.includes("failed"))) {
      const passed = covered.filter(
        (result) =>
          !result.statuses.includes("failed") &&
          result.statuses.includes("passed"),
      ).length;
      if (entry.name === WHOLE_FILE && passed > 0) {
        outcome.warnings.push(
          `the "*" entry for ${entry.file} accepts every failure in a file where ${String(passed)} test(s) passed; a new failure there is hidden, so list the failing names instead`,
        );
      }
      continue;
    }
    if (covered.some((result) => result.statuses.includes("passed"))) {
      (entry.kind === "flaky" ? outcome.flakyPassed : outcome.stale).push(
        summary,
      );
    } else {
      outcome.notRun.push(summary);
    }
  }
  const outside = new Set(
    reports
      .flatMap(({ report }) =>
        report.testResults.map((file) => normalizeFile(file.name)),
      )
      .filter((path) => isAbsolute(path) && !path.includes(`/${suite}/`)),
  );
  if (outside.size > 0) {
    outcome.warnings.push(
      `${String(outside.size)} reported file(s) are not under a '${suite}/' directory, so no entry of suite '${suite}' can accept them; is --suite right? First: ${[...outside][0]}`,
    );
  }
  if (logs === undefined) {
    outcome.warnings.push(
      "no --log given: errors outside any test (an unhandled rejection) and a run that did not finish are not in JSON reports, so exit 0 here does not cover them",
    );
  }
  return { ...outcome, exitCode: outcome.new.length > 0 ? 1 : 0 };
};

const formatText = ({ outcome, options, list }) => {
  const lines = [
    `diff-test-reds: suite ${options.suite}; ${String(options.reports.length)} report(s); accepted list ${options.accepted} (program ${list.program}, base ${list.base})`,
  ];
  const section = (title, items, withReason) => {
    if (items.length === 0) {
      lines.push(`${title}: none`);
      return;
    }
    lines.push(`${title} (${String(items.length)}):`);
    for (const item of items) {
      lines.push(
        `  ${item.file} :: ${item.name}${item.wildcard === true ? " (via *)" : ""}${item.acceptedUpTo === undefined ? "" : ` (the list accepts up to ${String(item.acceptedUpTo)})`}${withReason ? `    [${item.reason}]` : ""}`,
      );
    }
  };
  section("NEW failures", outcome.new, false);
  section("ACCEPTED failures", outcome.accepted, false);
  section("ACCEPTED-FLAKY failures", outcome.acceptedFlaky, false);
  section(
    "STALE accepted entries (their tests passed; remove them from the list)",
    outcome.stale,
    true,
  );
  lines.push(
    `${String(outcome.flakyPassed.length)} flaky entr${outcome.flakyPassed.length === 1 ? "y" : "ies"} passed; ${String(outcome.notRun.length)} entr${outcome.notRun.length === 1 ? "y" : "ies"} had no result in these reports.`,
  );
  for (const warning of outcome.warnings) lines.push(`warning: ${warning}`);
  lines.push(
    outcome.exitCode === 0
      ? "verdict: no new failures (exit 0)"
      : `verdict: ${String(outcome.new.length)} new failure(s) (exit 1)`,
  );
  return `${lines.join("\n")}\n`;
};

const readJson = (path, what) => {
  let text;
  try {
    text = readFileSync(path, "utf8");
  } catch (error) {
    throw new InputError(`cannot read ${what} ${path}: ${error.message}`);
  }
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new InputError(`${what} ${path} is not valid JSON: ${error.message}`);
  }
};

export const main = (
  argv,
  {
    stdout = (text) => process.stdout.write(text),
    stderr = (text) => process.stderr.write(text),
  } = {},
) => {
  try {
    const options = parseArgs(argv);
    const list = validateAcceptedList(
      readJson(options.accepted, "accepted list"),
      options.accepted,
    );
    const reports = options.reports.map((path) => ({
      label: path,
      report: validateReport(readJson(path, "report"), path),
    }));
    const logs =
      options.logs.length === 0
        ? undefined
        : options.logs.map((path) => {
            try {
              return { label: path, text: readFileSync(path, "utf8") };
            } catch (error) {
              throw new InputError(`cannot read log ${path}: ${error.message}`);
            }
          });
    const outcome = diffReds({ list, suite: options.suite, reports, logs });
    stdout(
      options.json
        ? `${JSON.stringify({ suite: options.suite, program: list.program, base: list.base, reports: options.reports, ...outcome }, null, 2)}\n`
        : formatText({ outcome, options, list }),
    );
    return outcome.exitCode;
  } catch (error) {
    if (error instanceof InputError) {
      stderr(`diff-test-reds: ${error.message}\n${USAGE}\n`);
      return 2;
    }
    throw error;
  }
};

if (
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  process.exitCode = main(process.argv.slice(2));
}
