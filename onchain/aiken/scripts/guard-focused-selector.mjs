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

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const MAX_SELECTOR_COUNT = 64;
const MAX_SELECTOR_LENGTH = 256;
const VALID_SELECTOR = /^[a-z0-9_][a-z0-9_/]*$/u;
const VALID_ENVIRONMENT = /^[a-z0-9_-]+$/u;

export const usage =
  "usage: node scripts/guard-focused-selector.mjs <module-selector> [<module-selector> ...]";

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

// `result` is the shape spawnSync returns: { stdout, status, error }.
export const evaluateSelectorReport = (selector, result) => {
  if (result.error !== undefined && result.error !== null) {
    return {
      selector,
      ok: false,
      diagnostic: `focused selector ${selector}: Aiken could not be executed: ${result.error.message}`,
    };
  }

  let report;
  try {
    report = JSON.parse(result.stdout);
  } catch {
    return {
      selector,
      ok: false,
      diagnostic: `focused selector ${selector}: Aiken did not return its structured test report (status ${String(result.status)})`,
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
      diagnostic: `focused selector ${selector}: Aiken report has no numeric summary`,
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
      diagnostic: `focused selector ${selector} collected 0 tests; a selector that matches no module and no test cannot establish anything, so this gate fails closed`,
    };
  }
  if (failed !== 0 || passed !== total) {
    return {
      selector,
      total,
      passed,
      failed,
      ok: false,
      diagnostic: `focused selector ${selector}: collected=${String(total)}, passed=${String(passed)}, failed=${String(failed)}`,
    };
  }
  if (result.status !== null && result.status !== 0) {
    return {
      selector,
      total,
      passed,
      failed,
      ok: false,
      diagnostic: `focused selector ${selector}: Aiken exited with status ${String(result.status)}`,
    };
  }

  return { selector, total, passed, failed, ok: true };
};

export const runSelector = (
  selector,
  { binary, projectDirectory, environment },
) => {
  const args = ["check", "-m", selector, "--plain-numbers"];
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
    const selectors = parseSelectors(process.argv.slice(2));
    const environment = process.env.MIDGARD_AIKEN_ENV;
    if (environment !== undefined && !VALID_ENVIRONMENT.test(environment)) {
      throw new Error("MIDGARD_AIKEN_ENV contains an invalid environment name");
    }
    const context = {
      binary: process.env.MIDGARD_AIKEN_BIN ?? "aiken",
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
            selector: outcome.selector,
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
