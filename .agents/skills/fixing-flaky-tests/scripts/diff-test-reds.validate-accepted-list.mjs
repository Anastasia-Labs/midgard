import { isAbsolute } from "node:path";

export const USAGE =
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

export const RUN_FILE = "(run)";

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

export const ENTRY_KEYS = ["suite", "file", "name", "kind", "reason"];

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

export const normalizeFile = (file) =>
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
export const displayFile = (reportPath, suite) => {
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

export const entryMatches = (entry, result, suite) =>
  fileMatches(result.reportPath, entry.file, suite) &&
  (entry.name === WHOLE_FILE || normalizeName(entry.name) === result.name);
