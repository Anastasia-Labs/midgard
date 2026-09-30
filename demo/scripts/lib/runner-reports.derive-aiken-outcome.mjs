import { spawnSync } from "node:child_process";

import { assertPinnedAiken } from "../../../onchain/aiken/scripts/pinned-compiler.mjs";
import {
  aikenBinary,
  RunnerCheckError,
} from "./runner-reports.derive-vitest-outcome.mjs";

export const aikenPublishedCommand = ({
  projectDirectory,
  selectors,
  command = "aiken",
}) =>
  `cd ${projectDirectory} && ${command} check -e ${selectors
    .map((selector) => `-m ${selector}`)
    .join(" ")}`;

export const runAikenCheck = ({ projectRoot, selectors, binary }) => {
  const compiler = binary ?? aikenBinary();
  // A report is only a measurement under the pinned fork; stock v1.1.22 can
  // pass a suite it compiles unsoundly.
  try {
    assertPinnedAiken(compiler);
  } catch (error) {
    throw new RunnerCheckError(
      "ERR_AIKEN_NOT_PINNED",
      error instanceof Error ? error.message : String(error),
    );
  }
  const run = spawnSync(
    compiler,
    [
      "check",
      "-e",
      ...selectors.flatMap((selector) => ["-m", selector]),
      "--plain-numbers",
    ],
    {
      cwd: projectRoot,
      encoding: "utf8",
      maxBuffer: 128 * 1024 * 1024,
    },
  );
  if (run.error !== undefined) {
    throw run.error;
  }
  let report = null;
  try {
    report = JSON.parse(run.stdout);
  } catch (error) {
    throw new RunnerCheckError(
      "ERR_AIKEN_NO_REPORT",
      `${compiler} produced no readable structured report (exit ${String(
        run.status,
      )}): ${error instanceof Error ? error.message : String(error)}\n${
        run.stderr ?? ""
      }`,
    );
  }
  return { report, status: run.status };
};

// Pure: takes an `aiken check` JSON report plus the (module, selector) pairs the
// artifact declares and returns the measured per-selector results, or throws.
//
// A declaration may carry `modules: [...]` instead of relying on `module` alone
// when the citation it comes from names a source-path stem that two compiled
// modules share — `lib/midgard/x-v1.ak` and its `lib/midgard/x-v1.test.ak`
// sibling both answer to the `midgard/x_v1` prefix that `aiken check -m` matches.
// The accepted set is always an explicit, bounded list; anything outside it is
// still an ERR_AIKEN_SELECTOR_MODULE_MISMATCH.
//
// `aiken check -m <selector>` exits 0 when the selector matches nothing — the
// zero-collection shape that put 17 declared tests behind a `midgard/` prefix
// that could never match (issue #519, finding V-1). It is rejected here first.
export const deriveAikenOutcome = ({ label, report, status, declared }) => {
  const modules = Array.isArray(report.modules) ? report.modules : [];
  const collected = modules.flatMap((module) =>
    (Array.isArray(module.tests) ? module.tests : []).map((test) => ({
      module: module.name ?? "<unnamed module>",
      title: test.title ?? "<unnamed test>",
      status: test.status ?? "<no status>",
    })),
  );
  if (collected.length === 0) {
    throw new RunnerCheckError(
      "ERR_AIKEN_ZERO_COLLECTION",
      `${label}: the declared selectors collected 0 tests (aiken exits 0 on a selector that matches nothing, so this must fail closed) — declared ${declared
        .map(({ module, selector }) => `${module}.{${selector}}`)
        .join(", ")}`,
    );
  }
  const byTitle = new Map();
  for (const test of collected) {
    byTitle.set(test.title, [...(byTitle.get(test.title) ?? []), test]);
  }
  const measured = [];
  const missing = [];
  const ambiguous = [];
  const misplaced = [];
  const failed = [];
  for (const { module, modules, selector } of declared) {
    const acceptedModules = modules ?? [module];
    const matches = byTitle.get(selector) ?? [];
    if (matches.length === 0) {
      missing.push(`${module}.{${selector}}`);
      continue;
    }
    if (matches.length > 1) {
      ambiguous.push(
        `${selector} (collected from ${matches
          .map((match) => match.module)
          .join(", ")})`,
      );
      continue;
    }
    const [match] = matches;
    if (!acceptedModules.includes(match.module)) {
      misplaced.push(
        `${selector} is declared in ${acceptedModules.join(" or ")} but was collected from ${match.module}`,
      );
      continue;
    }
    if (match.status !== "pass") {
      failed.push(`${module}.{${selector}} (${match.status})`);
      continue;
    }
    measured.push({ module, selector, collectedFrom: match.module });
  }
  if (missing.length > 0) {
    throw new RunnerCheckError(
      "ERR_AIKEN_SELECTOR_NOT_COLLECTED",
      `${label}: ${String(
        missing.length,
      )} declared selector(s) were never collected by the runner and may not be counted — ${missing.join(
        "; ",
      )}`,
    );
  }
  if (ambiguous.length > 0) {
    throw new RunnerCheckError(
      "ERR_AIKEN_SELECTOR_AMBIGUOUS",
      `${label}: ${String(
        ambiguous.length,
      )} declared selector(s) resolve to more than one collected test, so no single result backs the citation — ${ambiguous.join(
        "; ",
      )}`,
    );
  }
  if (misplaced.length > 0) {
    throw new RunnerCheckError(
      "ERR_AIKEN_SELECTOR_MODULE_MISMATCH",
      `${label}: ${String(
        misplaced.length,
      )} declared selector(s) passed in a different module than the artifact cites — ${misplaced.join(
        "; ",
      )}`,
    );
  }
  if (failed.length > 0) {
    throw new RunnerCheckError(
      "ERR_AIKEN_CHECK_FAILED",
      `${label}: ${String(failed.length)} of ${String(
        declared.length,
      )} declared selectors did not pass — ${failed.join("; ")}`,
    );
  }
  const summary = report.summary ?? {};
  if (
    summary.total !== collected.length ||
    summary.passed !== collected.length ||
    summary.failed !== 0
  ) {
    throw new RunnerCheckError(
      "ERR_AIKEN_REPORT_INCONSISTENT",
      `${label}: the aiken report totals contradict its own per-test results (total ${String(
        summary.total,
      )}, passed ${String(summary.passed)}, failed ${String(
        summary.failed,
      )}, collected ${String(collected.length)})`,
    );
  }
  if (status !== 0) {
    throw new RunnerCheckError(
      "ERR_AIKEN_NONZERO_EXIT",
      `${label}: every declared selector passed but aiken exited ${String(
        status,
      )}`,
    );
  }
  return { collected: collected.length, passed: measured.length, measured };
};
