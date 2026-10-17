import { relative } from "node:path";

const VALUE_FLAGS = new Set(["config", "exclude", "maxWorkers"]);
const BOOLEAN_FLAGS = new Set(["disableConsoleIntercept"]);
export const CALLER_FLAGS = [...VALUE_FLAGS, ...BOOLEAN_FLAGS];

export const declaredTestCommand = (task) => {
  const command = {
    env: task.environment ?? {},
    config: undefined,
    exclude: [],
    maxWorkers: undefined,
    disableConsoleIntercept: false,
    preludes: task.before ?? [],
  };
  const flags = task.command.slice(2);
  for (let index = 0; index < flags.length; index += 1) {
    const [, name, inline] = /^--([^=]+)(?:=(.*))?$/u.exec(flags[index]) ?? [];
    if (BOOLEAN_FLAGS.has(name) && inline === undefined) command[name] = true;
    else if (VALUE_FLAGS.has(name)) {
      const value = inline ?? flags[++index];
      if (value === undefined || value.startsWith("--"))
        throw new Error(`--${name} needs a value`);
      if (name === "exclude") command.exclude.push(value);
      else command[name] = value;
    } else
      throw new Error(
        `contrib test does not understand '${flags[index]}' in midgardTasks.test.command`,
      );
  }
  return command;
};

/** The script's command with the caller's flags added. */
export const withCallerFlags = (command, flags = {}) => ({
  ...command,
  config: flags.config ?? command.config,
  exclude: [...command.exclude, ...(flags.exclude ?? [])],
  maxWorkers: flags.maxWorkers ?? command.maxWorkers,
  disableConsoleIntercept:
    command.disableConsoleIntercept || Boolean(flags.disableConsoleIntercept),
});

/** Vitest argv flags for a command, in the `--flag=value` form. */
export const vitestFlags = (command) => [
  ...(command.config === undefined ? [] : [`--config=${command.config}`]),
  ...command.exclude.map((glob) => `--exclude=${glob}`),
  ...(command.maxWorkers === undefined
    ? []
    : [`--maxWorkers=${command.maxWorkers}`]),
  ...(command.disableConsoleIntercept ? ["--disableConsoleIntercept"] : []),
];

const firstLine = (text) =>
  String(text ?? "")
    .split("\n")
    .map((line) => line.trim())
    .find(Boolean) ?? "";

/**
 * What failed in a Vitest JSON report, one entry per failed test or per file
 * that failed outside any test (an import or setup error).
 */
export const failures = (report, directory) =>
  (report.testResults ?? []).flatMap((suite) => {
    const file = relative(directory, suite.name);
    const failed = (suite.assertionResults ?? []).filter(
      (assertion) => assertion.status === "failed",
    );
    if (failed.length)
      return failed.map((assertion) => ({
        file,
        test:
          assertion.fullName ??
          [...(assertion.ancestorTitles ?? []), assertion.title].join(" > "),
        message: firstLine(assertion.failureMessages?.[0]),
      }));
    return suite.status === "failed"
      ? [{ file, message: firstLine(suite.message) }]
      : [];
  });

/** A few lines a person can read: the verdict, what failed, where to look. */
export const summaryLines = (receipt, { limit = 10 } = {}) => {
  const counts = receipt.counts;
  const lines = [
    `contrib test ${receipt.package}: ${receipt.status}` +
      (counts
        ? ` (${counts.passed} passed, ${counts.failed} failed, ${counts.skipped} skipped, in ${receipt.selectedFiles?.length ?? 0} file(s), seed ${receipt.seed})`
        : ""),
  ];
  if (receipt.reason) lines.push(`  ${receipt.reason}`);
  if (receipt.reportError) lines.push(`  report: ${receipt.reportError}`);
  for (const failure of (receipt.failures ?? []).slice(0, limit))
    lines.push(
      `  FAIL ${failure.file}${failure.test ? ` > ${failure.test}` : ""}`,
      ...(failure.message ? [`       ${failure.message}`] : []),
    );
  if ((receipt.failures?.length ?? 0) > limit)
    lines.push(`  ... ${receipt.failures.length - limit} more`);
  for (const step of receipt.steps ?? [])
    if (step.exitCode !== 0 || step.reason)
      lines.push(`  log: ${step.logPath}`);
  lines.push(`  receipt: ${receipt.path}`);
  return lines;
};
