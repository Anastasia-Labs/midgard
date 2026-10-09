import { relative } from "node:path";

// `contrib test` runs a package's suites the way its `test` script (the
// command CI runs) does, so the two cannot drift: the environment the script
// exports, the flags it passes Vitest and the plain-Node preludes it runs
// first all come from the script itself. Only the Vitest flags below are
// understood; any other flag in a script is refused rather than dropped.

const VALUE_FLAGS = new Set(["exclude", "maxWorkers"]);
const BOOLEAN_FLAGS = new Set(["disableConsoleIntercept"]);

/** The flags a caller may add: Vitest's own spellings, nothing else. */
export const CALLER_FLAGS = [...VALUE_FLAGS, ...BOOLEAN_FLAGS];

// Shell words: whitespace separates, quotes group and are removed
// (`NODE_ENV='emulator'` is the one word `NODE_ENV=emulator`).
const words = (text) =>
  [...text.matchAll(/(?:'[^']*'|"[^"]*"|[^\s'"])+/gu)].map(([word]) =>
    word.replace(/'([^']*)'|"([^"]*)"/gu, "$1$2"),
  );

const assignment = (word) => {
  const match = /^([A-Za-z_][A-Za-z0-9_]*)=(.*)$/u.exec(word);
  return match ? [match[1], match[2]] : undefined;
};

/**
 * Parse a package `test` script of the forms the workspace uses:
 * `[prelude &&]... [export K=V &&]... [env K=V...] vitest run [flags]`.
 */
export const parseTestScript = (script) => {
  const command = {
    env: {},
    exclude: [],
    maxWorkers: undefined,
    disableConsoleIntercept: false,
    preludes: [],
  };
  let vitest = 0;
  for (const segment of script.split("&&").map((part) => part.trim())) {
    let tokens = words(segment);
    if (tokens[0] === "export") {
      for (const word of tokens.slice(1)) {
        const pair = assignment(word);
        if (!pair) throw new Error(`cannot read export in: ${segment}`);
        command.env[pair[0]] = pair[1];
      }
      continue;
    }
    if (tokens[0] === "env") {
      tokens = tokens.slice(1);
      while (tokens.length && assignment(tokens[0])) {
        const [key, value] = assignment(tokens.shift());
        command.env[key] = value;
      }
    }
    if (tokens[0] !== "vitest") {
      if (vitest) throw new Error(`a step follows vitest in: ${script}`);
      command.preludes.push(segment);
      continue;
    }
    vitest += 1;
    if (tokens[1] !== "run")
      throw new Error(`the test script must use 'vitest run': ${segment}`);
    const flags = tokens.slice(2);
    for (let index = 0; index < flags.length; index += 1) {
      const [, name, inline] =
        /^--([^=]+)(?:=(.*))?$/u.exec(flags[index]) ?? [];
      if (BOOLEAN_FLAGS.has(name) && inline === undefined) command[name] = true;
      else if (VALUE_FLAGS.has(name)) {
        const value = inline ?? flags[++index];
        if (value === undefined) throw new Error(`--${name} needs a value`);
        if (name === "exclude") command.exclude.push(value);
        else command[name] = value;
      } else
        throw new Error(
          `contrib test does not understand '${flags[index]}' in the package test script '${script}'; teach scripts/contrib/vitest-command.mjs or drop it`,
        );
    }
  }
  if (vitest !== 1)
    throw new Error(`the test script must run vitest exactly once: ${script}`);
  return command;
};

/** The script's command with the caller's flags added. */
export const withCallerFlags = (command, flags = {}) => ({
  ...command,
  exclude: [...command.exclude, ...(flags.exclude ?? [])],
  maxWorkers: flags.maxWorkers ?? command.maxWorkers,
  disableConsoleIntercept:
    command.disableConsoleIntercept || Boolean(flags.disableConsoleIntercept),
});

/** Vitest argv flags for a command, in the `--flag=value` form. */
export const vitestFlags = (command) => [
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
