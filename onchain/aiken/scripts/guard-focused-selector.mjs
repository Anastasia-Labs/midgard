#!/usr/bin/env node

// Fail-closed wrapper for focused `aiken check -m <selector>` invocations.
//
// A bare `aiken check -m <selector>` whose selector matches no module and no
// test prints an empty report and exits 0. Every gate that cites such a command
// therefore reports success while executing nothing, which is how issue #519's
// finding V-1 turned Q44's two 17-test selectors into a green gate that ran
// zero tests. This wrapper turns "0 tests collected" — and any nonzero aiken
// status, unparseable report, or failing test — into a nonzero exit whose
// diagnostic names the exact selector that collected nothing.
//
// Use it wherever a focused selector is the evidence. When the exact expected
// test names are known, prefer scripts/run-focused-check.mjs, which pins the
// exact count as well.
//
// `--all` runs the whole suite (`aiken check` with no `-m`) under the same
// rules: it prints the collected count and fails on zero, on any failing
// test, and on a missing report. CI's full-suite step runs through it.

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { assertPinnedAiken, defaultAikenBinary } from "./pinned-compiler.mjs";

const MAX_SELECTOR_COUNT = 64;
const MAX_SELECTOR_LENGTH = 256;
const VALID_SELECTOR = /^[a-z0-9_][a-z0-9_/]*$/u;
const VALID_ENVIRONMENT = /^[a-z0-9_-]+$/u;

export const ALL_MODULES = "--all";
const ALL_MODULES_LABEL = "(all modules)";
const MAX_LISTED_FAILURES = 20;

export const usage =
  "usage: node scripts/guard-focused-selector.mjs <module-selector> [<module-selector> ...] | --all";

export const parseSelectors = (args) => {
  if (
    args.length === 0 ||
    args.length > MAX_SELECTOR_COUNT ||
    args.some(
      (selector) =>
        selector.length > MAX_SELECTOR_LENGTH || !VALID_SELECTOR.test(selector),
    )
  ) {
    throw new Error(usage);
  }
  if (new Set(args).size !== args.length) {
    throw new Error("module selectors must be unique");
  }
  return args;
};

// `--all` alone selects the whole suite; otherwise every argument is a module
// selector. `--all` mixed with selectors is refused rather than guessed at.
export const parseInvocation = (args) =>
  args.length === 1 && args[0] === ALL_MODULES
    ? [ALL_MODULES]
    : parseSelectors(args);

const describeSelector = (selector) =>
  selector === ALL_MODULES ? ALL_MODULES_LABEL : selector;

// The failing entries of a report, so a red full-suite run names its tests.
const failingTests = (report) =>
  (Array.isArray(report?.modules) ? report.modules : []).flatMap((module) =>
    (Array.isArray(module?.tests) ? module.tests : [])
      .filter((test) => test?.status !== "pass")
      .map((test) => ({ module: module?.name, ...test })),
  );

const listFailures = (report) => {
  const failures = failingTests(report);
  if (failures.length === 0) {
    return "";
  }
  const listed = failures
    .slice(0, MAX_LISTED_FAILURES)
    .map((failure) => `\n  ${JSON.stringify(failure)}`)
    .join("");
  const more =
    failures.length > MAX_LISTED_FAILURES
      ? `\n  ... and ${String(failures.length - MAX_LISTED_FAILURES)} more`
      : "";
  return `${listed}${more}`;
};

// `result` is the shape spawnSync returns: { stdout, status, error }.
export const evaluateSelectorReport = (selector, result) => {
  const label = describeSelector(selector);
  if (result.error !== undefined && result.error !== null) {
    return {
      selector,
      ok: false,
      diagnostic: `focused selector ${label}: Aiken could not be executed: ${result.error.message}`,
    };
  }

  let report;
  try {
    report = JSON.parse(result.stdout);
  } catch {
    return {
      selector,
      ok: false,
      diagnostic: `focused selector ${label}: Aiken did not return its structured test report (status ${String(result.status)}); with its output captured, Aiken prints no compile error, so rerun under a pseudo-terminal to read it: script -qec "aiken check" /dev/null`,
    };
  }

  const summary = report?.summary;
  if (
    typeof summary?.total !== "number" ||
    typeof summary?.passed !== "number" ||
    typeof summary?.failed !== "number"
  ) {
    return {
      selector,
      ok: false,
      diagnostic: `focused selector ${label}: Aiken report has no numeric summary`,
    };
  }

  const { total, passed, failed } = summary;
  if (total === 0) {
    return {
      selector,
      total,
      passed,
      failed,
      ok: false,
      diagnostic: `focused selector ${label} collected 0 tests; a selector that matches no module and no test cannot establish anything, so this gate fails closed`,
    };
  }
  if (failed !== 0 || passed !== total) {
    return {
      selector,
      total,
      passed,
      failed,
      ok: false,
      diagnostic: `focused selector ${label}: collected=${String(total)}, passed=${String(passed)}, failed=${String(failed)}${listFailures(report)}`,
    };
  }
  if (result.status !== null && result.status !== 0) {
    return {
      selector,
      total,
      passed,
      failed,
      ok: false,
      diagnostic: `focused selector ${label}: Aiken exited with status ${String(result.status)}`,
    };
  }

  return { selector, total, passed, failed, ok: true };
};

export const runSelector = (
  selector,
  { binary, projectDirectory, environment },
) => {
  const args =
    selector === ALL_MODULES
      ? ["check", "--plain-numbers"]
      : ["check", "-m", selector, "--plain-numbers"];
  if (environment !== undefined) {
    args.push("--env", environment);
  }
  const result = spawnSync(binary, args, {
    cwd: projectDirectory,
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
  if (result.stderr) {
    process.stderr.write(result.stderr);
  }
  return evaluateSelectorReport(selector, result);
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  let exitCode = 1;
  try {
    const selectors = parseInvocation(process.argv.slice(2));
    const environment = process.env.MIDGARD_AIKEN_ENV;
    if (environment !== undefined && !VALID_ENVIRONMENT.test(environment)) {
      throw new Error("MIDGARD_AIKEN_ENV contains an invalid environment name");
    }
    const binary = defaultAikenBinary();
    assertPinnedAiken(binary);
    const context = {
      binary,
      projectDirectory: resolve(dirname(fileURLToPath(import.meta.url)), ".."),
      environment,
    };

    const outcomes = selectors.map((selector) =>
      runSelector(selector, context),
    );
    for (const outcome of outcomes) {
      if (outcome.ok) {
        process.stdout.write(
          `${JSON.stringify({
            selector: describeSelector(outcome.selector),
            collected: outcome.total,
            passed: outcome.passed,
            failed: outcome.failed,
          })}\n`,
        );
      } else {
        console.error(outcome.diagnostic);
      }
    }
    exitCode = outcomes.every((outcome) => outcome.ok) ? 0 : 1;
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error));
  } finally {
    process.exitCode = exitCode;
  }
}
