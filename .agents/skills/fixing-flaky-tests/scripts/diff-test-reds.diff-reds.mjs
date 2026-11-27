import { readFileSync } from "node:fs";
import { isAbsolute } from "node:path";

import {
  collectResults,
  displayFile,
  entryMatches,
  InputError,
  normalizeFile,
  parseArgs,
  RUN_FILE,
  runErrorCount,
  runLevelFailures,
  USAGE,
  validateAcceptedList,
  validateReport,
  WHOLE_FILE,
} from "./diff-test-reds.validate-accepted-list.mjs";

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
