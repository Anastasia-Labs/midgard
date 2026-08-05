#!/usr/bin/env node

// Verifies the Q49 structural handoff artifact against the final tree: every
// F21 physical structural row (coverage-matrix.md L295-L303) must name its exact
// current matrix concern text and at least one EXECUTABLE piece of evidence that
// really exists in the tree. GOAL_SPEC.md 9.1 closing rule forbids prose-only
// closure of a structurally enforced rule, so a row with no resolvable selector,
// test title or absent-constructor check fails this gate.
//
// Issue #529 (finding V-2 of #519). This gate used to resolve "executable"
// evidence by string matching over test source — `source.includes("\ntest
// <selector>(")` for Aiken and `source.includes('it("<title>"')` for Vitest —
// and published the number of matches as "31 executable checks". Nine throwing
// test bodies, an `it.skip`, or a selector that collects nothing all leave that
// number at 31, so the row could report 31/31 while nothing had run.
//
// Every published count now comes from a runner report instead. The cited Aiken
// selectors are executed in one `aiken check -e` invocation and the cited Vitest
// titles by spawning each package's own Vitest CLI; a selector that collects
// nothing, a test that fails, a test that never executed, and a citation the
// runner never collected each fail the gate closed with a distinct diagnostic.
// Source-absence evidence (`absentConstructor`) is a static structural check by
// construction: it is counted and published separately and is never added to
// the executed total.
//
// usage: node demo/scripts/verify-canonical-v1-q49-structural-handoff.mjs [--json]

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { isPassStatus } from "./lib/evidence-status.mjs";
import { runFixtureMode } from "./lib/runner-fixtures.mjs";
import {
  aikenModuleName,
  aikenPublishedCommand,
  deriveAikenOutcome,
  deriveVitestOutcome,
  runAikenCheck,
  runVitest,
  vitestPublishedCommand,
} from "./lib/runner-reports.mjs";

const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repositoryRoot = resolve(demoRoot, "..");
const aikenProjectDirectory = "onchain/aiken";
const aikenProjectRoot = resolve(repositoryRoot, aikenProjectDirectory);

// Fixture mode runs one seeded defect and nothing else, so the negative
// self-tests at the bottom can spawn this very gate and observe its real exit
// code. It must run before any artifact is read.
runFixtureMode({
  argv: process.argv.slice(2),
  packageRoot: resolve(repositoryRoot, "demo/midgard-validation"),
});

const emitJson = process.argv.slice(2).includes("--json");

const readRepositoryFile = async (relativePath) =>
  readFile(resolve(repositoryRoot, relativePath), "utf8");

const evidence = JSON.parse(
  await readRepositoryFile(
    "docs/exec-plans/evidence/canonical-v1-q49-structural-handoff-v1.json",
  ),
);

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-q49-structural-handoff.v1",
  "unexpected handoff schema",
);
assert.deepEqual(evidence.goalIds, ["Q49"]);
assert.equal(evidence.issue, 481);

const matrixLines = (await readRepositoryFile(evidence.source.path)).split(
  /\r?\n/u,
);
const firstCell = (line) => {
  const cells = line.split("|");
  assert.ok(cells.length > 2, `line ${line} is not a table row`);
  return cells[1].trim();
};

assert.ok(
  /^\|[\s-]+\|/u.test(matrixLines[evidence.source.separatorLine - 1]),
  `line ${String(evidence.source.separatorLine)} is not the structural table separator`,
);

const expectedLines = [];
for (
  let line = evidence.source.firstClaimLine;
  line <= evidence.source.lastClaimLine;
  line += 1
) {
  expectedLines.push(line);
}
assert.deepEqual(
  evidence.rows.map((row) => row.line),
  expectedLines,
  "handoff rows must cover exactly the physical structural claim lines in order",
);

const fileCache = new Map();
const loadFile = async (relativePath) => {
  if (!fileCache.has(relativePath)) {
    fileCache.set(relativePath, await readRepositoryFile(relativePath));
  }
  return fileCache.get(relativePath);
};

// ---------------------------------------------------------------------------
// Phase 1 — collect what the artifact declares. Nothing here decides whether a
// check passed; the declarations are only the plan that the runners execute.
// ---------------------------------------------------------------------------

const declaredAikenChecks = [];
const declaredVitestFiles = new Map();
const declaredStaticChecks = [];
const rowPlans = [];

for (const row of evidence.rows) {
  assert.equal(
    firstCell(matrixLines[row.line - 1]),
    row.concern,
    `matrix concern text drifted at line ${String(row.line)}`,
  );
  assert.ok(
    isPassStatus(row.disposition),
    `structural row ${String(row.line)} is not PASS`,
  );
  assert.equal(
    row.remainingTask,
    null,
    `structural row ${String(row.line)} still names a remaining task`,
  );
  assert.ok(
    Array.isArray(row.executableEvidence) && row.executableEvidence.length > 0,
    `structural row ${String(row.line)} has no executable evidence (prose-only closure is forbidden)`,
  );

  const plan = { line: row.line, aiken: [], vitest: [], static: 0 };
  for (const item of row.executableEvidence) {
    if (item.kind === "aiken") {
      assert.ok(
        Array.isArray(item.selectors) && item.selectors.length > 0,
        `row ${String(row.line)} cites ${item.module} with no selectors`,
      );
      // The module file must exist, but its existence is a precondition of the
      // run, never a substitute for it.
      await loadFile(item.module);
      const module = aikenModuleName(item.module);
      for (const selector of item.selectors) {
        assert.ok(
          /^[a-z0-9_]+$/u.test(selector),
          `selector ${selector} is not a focused-check-safe name`,
        );
        const check = { module, selector, source: item.module };
        declaredAikenChecks.push(check);
        plan.aiken.push(check);
      }
    } else if (item.kind === "vitest") {
      assert.ok(
        Array.isArray(item.titles) && item.titles.length > 0,
        `row ${String(row.line)} cites ${item.file} with no test titles`,
      );
      const packageDirectory = item.file.split("/").slice(0, 2).join("/");
      const testFile = item.file.split("/").slice(2).join("/");
      const existing = declaredVitestFiles.get(item.file) ?? {
        file: item.file,
        packageDirectory,
        testFile,
        titles: [],
      };
      existing.titles.push(...item.titles);
      declaredVitestFiles.set(item.file, existing);
      plan.vitest.push({ file: item.file, titles: item.titles });
    } else if (item.kind === "absentConstructor") {
      const source = await loadFile(item.file);
      const declarations = source.match(
        new RegExp(`pub type ${item.singleConstructorType} \\{`, "gu"),
      );
      assert.equal(
        declarations?.length,
        1,
        `${item.file} must declare exactly one ${item.singleConstructorType}`,
      );
      for (const forbidden of item.forbiddenConstructorSubstrings) {
        assert.ok(
          !source.includes(forbidden),
          `${item.file} unexpectedly mentions ${forbidden}: the structural claim no longer holds`,
        );
      }
      declaredStaticChecks.push({ line: row.line, file: item.file });
      plan.static += 1;
    } else {
      throw new Error(
        `unknown executable evidence kind ${String(item.kind)} at row ${String(row.line)}`,
      );
    }
  }

  assert.ok(
    plan.aiken.length + plan.vitest.length + plan.static > 0,
    `structural row ${String(row.line)} resolved zero checks`,
  );
  rowPlans.push(plan);
}

// ---------------------------------------------------------------------------
// Phase 2 — execute. One `aiken check` invocation covers every cited on-chain
// selector (compilation dominates its cost, so batching is both faster and
// exactly as strict), and each cited Vitest file is run by its own package's
// runner.
// ---------------------------------------------------------------------------

const aikenSelectors = [
  ...new Set(declaredAikenChecks.map(({ selector }) => selector)),
];
const aikenOutcome = deriveAikenOutcome({
  label: "Q49 structural handoff on-chain selectors",
  declared: declaredAikenChecks,
  ...runAikenCheck({
    projectRoot: aikenProjectRoot,
    selectors: aikenSelectors,
  }),
});
assert.equal(
  aikenOutcome.passed,
  declaredAikenChecks.length,
  "every cited on-chain selector must be measured as passing",
);

const vitestOutcomes = new Map();
for (const declaration of declaredVitestFiles.values()) {
  const outcome = deriveVitestOutcome({
    label: `Q49 structural handoff ${declaration.file}`,
    requiredTitles: declaration.titles,
    ...runVitest({
      packageRoot: resolve(repositoryRoot, declaration.packageDirectory),
      testFile: declaration.testFile,
    }),
  });
  vitestOutcomes.set(declaration.file, outcome);
}

// A cited Vitest title is credited as one executed check only because the
// runner collected it and reported it passing; the surrounding file must also
// have no failed or unexecuted test, which `deriveVitestOutcome` enforces.
const executedVitestChecks = [...declaredVitestFiles.values()].reduce(
  (total, declaration) => total + declaration.titles.length,
  0,
);
const executedChecks = aikenOutcome.passed + executedVitestChecks;
const staticStructuralChecks = declaredStaticChecks.length;

const checkedRows = rowPlans.map((plan) => ({
  line: plan.line,
  executed:
    plan.aiken.length +
    plan.vitest.reduce((total, item) => total + item.titles.length, 0),
  static: plan.static,
}));

assert.equal(
  evidence.summary.executedChecks,
  executedChecks,
  "published executed-check count is not the number of checks the runners passed",
);
assert.equal(
  evidence.summary.staticStructuralChecks,
  staticStructuralChecks,
  "published static structural-check count drifted from the artifact's source-absence evidence",
);
assert.deepEqual(
  evidence.summary.runners,
  [
    aikenPublishedCommand({
      projectDirectory: aikenProjectDirectory,
      selectors: aikenSelectors,
    }),
    ...[...declaredVitestFiles.values()].map((declaration) =>
      vitestPublishedCommand({
        packageDirectory: declaration.packageDirectory,
        testFile: declaration.testFile,
      }),
    ),
  ],
  "published runner commands drifted from the commands this gate executes",
);

const dispositions = evidence.rows.map((row) => row.disposition);
assert.deepEqual(
  {
    rows: evidence.summary.rows,
    pass: evidence.summary.pass,
    partial: evidence.summary.partial,
    open: evidence.summary.open,
  },
  {
    rows: evidence.rows.length,
    pass: dispositions.filter((value) => isPassStatus(value)).length,
    partial: dispositions.filter((value) => value === "PARTIAL").length,
    open: dispositions.filter((value) => value === "OPEN").length,
  },
);
assert.equal(evidence.summary.partial, 0, "no structural row may stay PARTIAL");
assert.equal(evidence.summary.open, 0, "no structural row may stay OPEN");

// The two rows this task closed must be executable, not inherited from F21.
const closedHere = evidence.rows.filter(
  (row) => row.inheritedFromF21 === false,
);
assert.deepEqual(
  closedHere.map((row) => row.closedBy),
  ["Q49-L298", "Q49-L302"],
  "Q49 must close exactly the two F21 partials L298 and L302",
);
for (const row of closedHere) {
  assert.ok(
    typeof row.claim === "string" && row.claim.length > 0,
    `row ${String(row.line)} must state the reduction it proves`,
  );
  assert.ok(
    row.executableEvidence.every((item) => item.kind === "aiken"),
    `row ${String(row.line)} must close with on-chain selectors`,
  );
}

// Unreachable proof surface must be an explicit inventory, never silence.
assert.ok(
  Array.isArray(evidence.unreachableProofSurface.removedModules) &&
    Array.isArray(evidence.unreachableProofSurface.removedSelectors),
  "unreachable proof surface must be inventoried as arrays",
);
assert.ok(
  typeof evidence.unreachableProofSurface.inventory === "string" &&
    evidence.unreachableProofSurface.inventory.length > 0,
  "unreachable proof surface needs a stated inventory",
);
assert.equal(
  evidence.parentIntegration.owner,
  "parent",
  "matrix and reconciliation edits stay parent-owned",
);
assert.ok(
  evidence.parentIntegration.pendingEdits.length > 0,
  "the parent handoff must list the edits it owns",
);

// ---------------------------------------------------------------------------
// Negative self-tests: spawn this gate against seeded fixtures and require a
// non-zero exit carrying the specific diagnostic. Without these, a future edit
// could reintroduce existence-as-passage and nothing would notice.
// ---------------------------------------------------------------------------

const runSelfTest = (flag) =>
  spawnSync(process.execPath, [fileURLToPath(import.meta.url), flag], {
    cwd: repositoryRoot,
    encoding: "utf8",
    maxBuffer: 128 * 1024 * 1024,
  });

const selfTests = [
  [
    "--vitest-fixture=failing",
    /ERR_FOCUSED_CHECK_FAILED: .*executes and fails/su,
  ],
  [
    "--vitest-fixture=zero-collection",
    /ERR_FOCUSED_CHECK_ZERO_COLLECTION: .*collected 0 tests/su,
  ],
  [
    "--vitest-fixture=skipped",
    /ERR_FOCUSED_CHECK_NOT_EXECUTED: .*never executed/su,
  ],
  [
    "--vitest-fixture=renamed-title",
    /ERR_FOCUSED_CHECK_TITLE_NOT_COLLECTED: .*executes and passes/su,
  ],
  [
    "--vitest-fixture=missing-file",
    /ERR_FOCUSED_CHECK_NO_FILES: .*matched no test file/su,
  ],
  ["--aiken-fixture=failing", /ERR_AIKEN_CHECK_FAILED: .*selftest_probe/su],
  [
    "--aiken-fixture=zero-collection",
    /ERR_AIKEN_ZERO_COLLECTION: .*collected 0 tests/su,
  ],
  [
    "--aiken-fixture=missing-selector",
    /ERR_AIKEN_SELECTOR_NOT_COLLECTED: .*selftest_probe_absent/su,
  ],
  [
    "--aiken-fixture=module-mismatch",
    /ERR_AIKEN_SELECTOR_MODULE_MISMATCH: .*selftest\/elsewhere/su,
  ],
];
for (const [flag, expectedDiagnostic] of selfTests) {
  const selfTest = runSelfTest(flag);
  assert.notEqual(
    selfTest.status,
    0,
    `Q49 handoff gate accepted the seeded defect ${flag}`,
  );
  assert.match(
    selfTest.stderr,
    expectedDiagnostic,
    `Q49 handoff gate rejected ${flag} without its specific diagnostic`,
  );
}
// Positive controls: the same harness must still accept real passing runs, so
// the rejections above cannot be a gate that rejects everything.
for (const [flag, expectedStdout] of [
  ["--vitest-fixture=passing", /vitest fixture passing: 1\/1 passed/u],
  ["--aiken-fixture=passing", /aiken fixture passing: 1\/1 passed/u],
]) {
  const control = runSelfTest(flag);
  assert.equal(
    control.status,
    0,
    `Q49 handoff gate rejected a passing fixture (${flag}): ${control.stderr}`,
  );
  assert.match(control.stdout, expectedStdout);
}

const report = {
  status: "PASS",
  structuralRows: evidence.rows.length,
  partial: evidence.summary.partial,
  open: evidence.summary.open,
  executedChecks,
  staticStructuralChecks,
  aikenSelectorsCollected: aikenOutcome.collected,
  vitestFilesExecuted: vitestOutcomes.size,
  rows: checkedRows,
  closedHere: closedHere.map((row) => ({ line: row.line, task: row.closedBy })),
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `Q49 structural handoff: PASS (${String(evidence.rows.length)} rows, ${String(executedChecks)} runner-executed checks, ${String(staticStructuralChecks)} static structural check, 0 partial, 0 open)`,
  );
}
