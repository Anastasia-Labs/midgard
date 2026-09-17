// Runner-backed check execution for the canonical-V1 evidence verifiers.
//
// The house style these verifiers grew out of published *existence* as
// *passage*: a count of `it(` lines in a test file, or of `test <name>(`
// declarations in an Aiken module, was reported as the number of checks that
// passed. Nine throwing test bodies, an `it.skip`, or a selector that collects
// nothing all leave those counts untouched (issue #519, finding V-2).
//
// the canonical V1 domain checks
// established the remedy for the Vitest side: spawn the runner, read its
// machine-readable report, and derive every published number from that report
// alone. This module is that same mechanism, generalized so the sibling gates
// share one implementation, plus the Aiken equivalent — `aiken check` prints
// its structured JSON report whenever stdout is not a TTY, which makes the same
// derivation possible for focused on-chain selectors.
//
// Nothing here reads test *source*. Every function below takes a runner report
// and either returns measured counts or throws with the exact reason the run
// may not be published as a pass.

import { spawnSync } from "node:child_process";
import { mkdtempSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";

export class RunnerCheckError extends Error {
  constructor(code, detail) {
    super(`${code}: ${detail}`);
    this.name = "RunnerCheckError";
    this.code = code;
  }
}

// ---------------------------------------------------------------------------
// Vitest
// ---------------------------------------------------------------------------

// The runner is resolved out of the package's own dependency tree rather than
// through a package-manager shim, so the measurement cannot silently become a
// PATH lookup that resolves to a different runner (or to nothing at all).
const vitestCliFor = (packageRoot) =>
  resolve(
    dirname(
      createRequire(resolve(packageRoot, "package.json")).resolve(
        "vitest/package.json",
      ),
    ),
    "vitest.mjs",
  );

export const vitestPublishedCommand = ({ packageDirectory, testFile }) =>
  `pnpm --dir ${packageDirectory} exec vitest run ${testFile}`;

export const runVitest = ({ packageRoot, testFile, root }) => {
  const reportDirectory = mkdtempSync(join(tmpdir(), "midgard-runner-check-"));
  const reportPath = join(reportDirectory, "vitest-report.json");
  try {
    const run = spawnSync(
      process.execPath,
      [
        vitestCliFor(packageRoot),
        "run",
        testFile,
        ...(root === undefined ? [] : [`--root=${root}`]),
        "--pool=forks",
        "--no-file-parallelism",
        "--maxWorkers=1",
        "--reporter=json",
        `--outputFile=${reportPath}`,
      ],
      {
        cwd: packageRoot,
        encoding: "utf8",
        maxBuffer: 128 * 1024 * 1024,
      },
    );
    if (run.error !== undefined) {
      throw run.error;
    }
    let report = null;
    try {
      report = JSON.parse(readFileSync(reportPath, "utf8"));
    } catch (error) {
      throw new RunnerCheckError(
        "ERR_FOCUSED_CHECK_NO_REPORT",
        `${testFile} produced no readable runner report (runner exit ${String(
          run.status,
        )}): ${error instanceof Error ? error.message : String(error)}\n${
          run.stdout ?? ""
        }${run.stderr ?? ""}`,
      );
    }
    return { report, status: run.status };
  } finally {
    rmSync(reportDirectory, { recursive: true, force: true });
  }
};

// Pure: takes a Vitest JSON report and returns the measured {collected, passed}
// pair, or throws with the exact reason the run may not be published as a pass.
// `requiredTitles` additionally demands that each named test really was among
// the tests the runner collected, so a renamed or deleted citation cannot be
// absorbed by the file's remaining tests.
export const deriveVitestOutcome = ({
  label,
  report,
  status,
  requiredTitles = [],
}) => {
  const files = Array.isArray(report.testResults) ? report.testResults : [];
  if (files.length === 0) {
    throw new RunnerCheckError(
      "ERR_FOCUSED_CHECK_NO_FILES",
      `${label}: the declared selector matched no test file, so nothing could have passed`,
    );
  }
  const assertions = files.flatMap((file) =>
    Array.isArray(file.assertionResults) ? file.assertionResults : [],
  );
  if (assertions.length === 0) {
    throw new RunnerCheckError(
      "ERR_FOCUSED_CHECK_ZERO_COLLECTION",
      `${label}: the declared selector collected 0 tests from ${String(
        files.length,
      )} matched file(s) — ${files
        .map(
          (file) =>
            `${file.name ?? "<unnamed>"} (${file.status ?? "<no status>"}${
              file.message ? `: ${file.message}` : ""
            })`,
        )
        .join("; ")}`,
    );
  }
  const named = (assertion) =>
    assertion.fullName ?? assertion.title ?? "<unnamed test>";
  const failed = assertions.filter(({ status: result }) => result === "failed");
  if (failed.length > 0) {
    throw new RunnerCheckError(
      "ERR_FOCUSED_CHECK_FAILED",
      `${label}: ${String(failed.length)} of ${String(
        assertions.length,
      )} collected tests failed — ${failed
        .map(
          (assertion) =>
            `${named(assertion)}: ${
              (assertion.failureMessages ?? []).join(" | ") ||
              "failed without a diagnostic"
            }`,
        )
        .join("; ")}`,
    );
  }
  // A skipped or todo test is a declaration, not a result. Counting one as a
  // pass is exactly the defect this gate exists to prevent, so it is rejected
  // rather than quietly excluded from the denominator.
  const notExecuted = assertions.filter(
    ({ status: result }) => result !== "passed",
  );
  if (notExecuted.length > 0) {
    throw new RunnerCheckError(
      "ERR_FOCUSED_CHECK_NOT_EXECUTED",
      `${label}: ${String(
        notExecuted.length,
      )} collected tests never executed and may not be published as passing — ${notExecuted
        .map(
          (assertion) =>
            `${named(assertion)} (${assertion.status ?? "<no status>"})`,
        )
        .join("; ")}`,
    );
  }
  const collectedTitles = new Set(
    assertions.flatMap((assertion) =>
      [assertion.title, assertion.fullName].filter(
        (value) => typeof value === "string",
      ),
    ),
  );
  const missingTitles = requiredTitles.filter(
    (title) =>
      !collectedTitles.has(title) &&
      ![...collectedTitles].some((collected) => collected.includes(title)),
  );
  if (missingTitles.length > 0) {
    throw new RunnerCheckError(
      "ERR_FOCUSED_CHECK_TITLE_NOT_COLLECTED",
      `${label}: the runner never collected ${String(
        missingTitles.length,
      )} cited test title(s) — ${missingTitles.join("; ")}`,
    );
  }
  const collected = assertions.length;
  if (
    report.numTotalTests !== collected ||
    report.numPassedTests !== collected ||
    report.numFailedTests !== 0 ||
    report.success !== true
  ) {
    throw new RunnerCheckError(
      "ERR_FOCUSED_CHECK_REPORT_INCONSISTENT",
      `${label}: the runner report totals contradict its own per-test results (total ${String(
        report.numTotalTests,
      )}, passed ${String(report.numPassedTests)}, failed ${String(
        report.numFailedTests,
      )}, success ${String(report.success)}, collected ${String(collected)})`,
    );
  }
  if (status !== 0) {
    throw new RunnerCheckError(
      "ERR_FOCUSED_CHECK_NONZERO_EXIT",
      `${label}: every collected test passed but the runner exited ${String(
        status,
      )}`,
    );
  }
  return { collected, passed: collected };
};

// ---------------------------------------------------------------------------
// Aiken
// ---------------------------------------------------------------------------

export const aikenBinary = () => process.env.MIDGARD_AIKEN_BIN ?? "aiken";

// `.github/workflows/aiken-ci.yml` runs ONE compiler: the patched fork
// v1.1.23+5adf783 (Anastasia-Labs/aiken, tag midgard-5adf7837) is the authority
// for compilation, for every applied validator hash, and for executing the test
// suite. Stock aiken was retired from all roles — it is not a second authority
// to agree with, because the stock v1.1.22 build that used to hold the
// compilation role ships a live unsound-codegen defect, and upstream aiken#1389
// makes a full stock `aiken check` take ~485 minutes on this tree. A gate that
// publishes a claim about the compiler must spawn the fork by name rather than
// leaving it to whatever `aiken` happens to be first on PATH.
// RETIRED 2026-08-14 (#579, owner ruling A): `stockAikenBinary()` is gone with
// its last caller, the `dual` compiler fixture. It read MIDGARD_STOCK_AIKEN_BIN
// and fell back to `aikenBinary()`, so with stock retired from every role the
// export could only resolve to the fork (in CI, where MIDGARD_AIKEN_BIN is the
// fork) or to whatever unpinned `aiken` sat on PATH — either way supplying a
// result under a name no longer entitled to one. There is no stock resolver.
export const forkAikenBinary = () =>
  process.env.MIDGARD_FORK_AIKEN_BIN ?? "aiken-fork";

// The compiler identity is measured, never assumed: a cached or shadowed binary
// that is not the pinned build must fail the gate that cites it rather than
// silently supplying the result under another compiler's name.
export const aikenCompilerVersion = (binary) => {
  const run = spawnSync(binary, ["--version"], { encoding: "utf8" });
  if (run.error !== undefined || run.status !== 0) {
    throw new RunnerCheckError(
      "ERR_AIKEN_BINARY_UNAVAILABLE",
      `${binary} could not report its version (${
        run.error === undefined
          ? `exit ${String(run.status)}`
          : run.error.message
      }); set MIDGARD_AIKEN_BIN / MIDGARD_FORK_AIKEN_BIN to the fork compiler .github/workflows/aiken-ci.yml pins`,
    );
  }
  return (run.stdout ?? "").trim();
};

// Aiken derives a module's name from its source path under `lib/` or
// `validators/`, with hyphens folded to underscores: `lib/midgard/x-v1.test.ak`
// is the module `midgard/x_v1.test`. The task manifest cites modules in either
// spelling, so an index of both is what turns a citation into the exact module
// a selector must have been collected from.
export const aikenModuleIndex = (projectRoot) => {
  const collect = (directory, prefix, found) => {
    let entries;
    try {
      entries = readdirSync(directory, { withFileTypes: true });
    } catch {
      return found;
    }
    for (const entry of entries) {
      if (entry.isDirectory()) {
        collect(
          resolve(directory, entry.name),
          `${prefix}${entry.name}/`,
          found,
        );
      } else if (entry.isFile() && entry.name.endsWith(".ak")) {
        found.push(`${prefix}${entry.name.slice(0, -".ak".length)}`);
      }
    }
    return found;
  };
  const index = new Map();
  for (const root of ["lib", "validators"]) {
    for (const found of collect(resolve(projectRoot, root), "", [])) {
      const module = found.replaceAll("-", "_");
      for (const alias of [found, module]) {
        if (!index.has(alias)) {
          index.set(alias, module);
        }
      }
    }
  }
  return index;
};

// `aiken check -m` splits its selector on the first `.` to separate the module
// from the `{test}` list, so a module whose own name contains a `.` — every
// `*.test.ak` file — cannot be spelled out in full. The prefix up to the first
// `.` still selects it, which is what `onchain/aiken/scripts/run-focused-check.mjs`
// passes and therefore what a manifest citation means.
export const aikenSelectorPattern = ({ module, selector }) =>
  `${module.split(".")[0]}.{${selector}}`;

// `onchain/aiken/validators/fraud-proofs/no-input/step-01.ak` is the module
// `fraud_proofs/no_input/step_01` to the compiler; `lib/midgard/x-v1.test.ak`
// is `midgard/x_v1.test`. Deriving the name lets the gate insist that a cited
// selector was collected from the module the artifact says it lives in.
export const aikenModuleName = (modulePath) =>
  modulePath
    .replace(/^onchain\/aiken\//u, "")
    .replace(/^(?:validators|lib)\//u, "")
    .replace(/\.ak$/u, "")
    .replaceAll("-", "_");

export const aikenPublishedCommand = ({
  projectDirectory,
  selectors,
  command = "aiken",
}) =>
  `cd ${projectDirectory} && ${command} check -e ${selectors
    .map((selector) => `-m ${selector}`)
    .join(" ")}`;

export const runAikenCheck = ({ projectRoot, selectors, binary }) => {
  const run = spawnSync(
    binary ?? aikenBinary(),
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
      `${binary ?? aikenBinary()} produced no readable structured report (exit ${String(
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
