import { spawnSync } from "node:child_process";
import { mkdtempSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";

import { defaultAikenBinary } from "../../../onchain/aiken/scripts/pinned-compiler.mjs";

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

export const aikenBinary = defaultAikenBinary;

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
