import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { readFile, readdir } from "node:fs/promises";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repositoryRoot = resolve(demoRoot, "..");

// ---------------------------------------------------------------------------
// Focused-check execution gate (F20, issue #518)
//
// The published `focusedChecksPassed` count used to be the number of `it(`
// lines in the cited test source, so existence was reported as passage: the
// row could publish "45/45 passed" while every one of those tests failed, was
// skipped, or was never collected at all. Everything below derives the count
// from a machine-readable runner report instead — the test runner is spawned,
// its JSON report is parsed, and a selector that collects nothing, a test that
// fails, and a test that never executed each fail the gate closed with a
// distinct diagnostic code.
//
// This is the runner-backed pattern the sibling evidence verifiers copy. To
// reuse it, change `focusedCheckPlan` and the two `assert.equal` calls that
// bind the artifact counts; the runner, the outcome derivation, and the
// negative self-tests are subject-independent.
// ---------------------------------------------------------------------------

const focusedCheckPlan = {
  name: "family-scaffold-v1.test.ts strict scanner",
  packageDirectory: "demo/midgard-fault-proofs",
  testFile: "tests/family-scaffold-v1.test.ts",
};
const focusedCheckPackageRoot = resolve(
  repositoryRoot,
  focusedCheckPlan.packageDirectory,
);
// The published command is the human-reproducible form. The gate executes that
// same Vitest CLI out of the package's own dependency tree rather than through
// a package-manager shim, so the measurement cannot silently become a PATH
// lookup that resolves to a different runner (or to nothing at all).
const focusedCheckPublishedCommand = `pnpm --dir ${focusedCheckPlan.packageDirectory} exec vitest run ${focusedCheckPlan.testFile}`;
const focusedCheckRunnerCli = resolve(
  dirname(
    createRequire(resolve(focusedCheckPackageRoot, "package.json")).resolve(
      "vitest/package.json",
    ),
  ),
  "vitest.mjs",
);

class FocusedCheckError extends Error {
  constructor(code, detail) {
    super(`${code}: ${detail}`);
    this.name = "FocusedCheckError";
    this.code = code;
  }
}

const runFocusedCheck = ({ testFile, root }) => {
  const reportDirectory = mkdtempSync(join(tmpdir(), "midgard-focused-check-"));
  const reportPath = join(reportDirectory, "vitest-report.json");
  try {
    const run = spawnSync(
      process.execPath,
      [
        focusedCheckRunnerCli,
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
        cwd: focusedCheckPackageRoot,
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
      throw new FocusedCheckError(
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
const deriveFocusedCheckOutcome = ({ label, report, status }) => {
  const files = Array.isArray(report.testResults) ? report.testResults : [];
  if (files.length === 0) {
    throw new FocusedCheckError(
      "ERR_FOCUSED_CHECK_NO_FILES",
      `${label}: the declared selector matched no test file, so nothing could have passed`,
    );
  }
  const assertions = files.flatMap((file) =>
    Array.isArray(file.assertionResults) ? file.assertionResults : [],
  );
  if (assertions.length === 0) {
    throw new FocusedCheckError(
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
    throw new FocusedCheckError(
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
    throw new FocusedCheckError(
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
  const collected = assertions.length;
  if (
    report.numTotalTests !== collected ||
    report.numPassedTests !== collected ||
    report.numFailedTests !== 0 ||
    report.success !== true
  ) {
    throw new FocusedCheckError(
      "ERR_FOCUSED_CHECK_REPORT_INCONSISTENT",
      `${label}: the runner report totals contradict its own per-test results (total ${String(
        report.numTotalTests,
      )}, passed ${String(report.numPassedTests)}, failed ${String(
        report.numFailedTests,
      )}, success ${String(report.success)}, collected ${String(collected)})`,
    );
  }
  if (status !== 0) {
    throw new FocusedCheckError(
      "ERR_FOCUSED_CHECK_NONZERO_EXIT",
      `${label}: every collected test passed but the runner exited ${String(
        status,
      )}`,
    );
  }
  return { collected, passed: collected };
};

// Seeded-defect fixtures. Each one is a real Vitest run against a mutated copy
// of the focused-check shape, executed through the identical spawn path, so the
// negative self-tests measure the gate rather than describing it.
const focusedCheckFixtures = {
  passing: {
    file: "tests/self-test.test.ts",
    source: `import { describe, expect, it } from "vitest";

describe("focused-check self-test", () => {
  it("executes and passes", () => {
    expect(1).toBe(1);
  });
});
`,
  },
  failing: {
    file: "tests/self-test.test.ts",
    source: `import { describe, expect, it } from "vitest";

describe("focused-check self-test", () => {
  it("executes and passes", () => {
    expect(1).toBe(1);
  });
  it("executes and fails", () => {
    expect(1).toBe(2);
  });
});
`,
  },
  "zero-collection": {
    file: "tests/self-test.test.ts",
    source: `import { describe } from "vitest";

describe("focused-check self-test declaring no test", () => {});
`,
  },
  skipped: {
    file: "tests/self-test.test.ts",
    source: `import { describe, it } from "vitest";

describe("focused-check self-test", () => {
  it.skip("is declared but never executed", () => {});
});
`,
  },
  "missing-file": {
    file: "tests/self-test.test.ts",
    source: null,
  },
};

const evaluateFocusedCheckFixture = (fixtureName) => {
  const fixture = focusedCheckFixtures[fixtureName];
  assert.ok(fixture, `unknown focused-check fixture ${fixtureName}`);
  const fixtureRoot = mkdtempSync(join(tmpdir(), "midgard-focused-fixture-"));
  try {
    if (fixture.source !== null) {
      const fixturePath = resolve(fixtureRoot, fixture.file);
      mkdirSync(dirname(fixturePath), { recursive: true });
      writeFileSync(fixturePath, fixture.source);
    }
    return deriveFocusedCheckOutcome({
      label: `focused-check fixture ${fixtureName}`,
      ...runFocusedCheck({ testFile: fixture.file, root: fixtureRoot }),
    });
  } finally {
    rmSync(fixtureRoot, { recursive: true, force: true });
  }
};

// Fixture mode: run one seeded fixture and nothing else, so the negative
// self-tests below can spawn this very gate and observe its real exit code.
const fixtureArgument = process.argv
  .slice(2)
  .find((argument) => argument.startsWith("--focused-check-fixture="));
if (fixtureArgument !== undefined) {
  const fixtureName = fixtureArgument.slice("--focused-check-fixture=".length);
  try {
    const outcome = evaluateFocusedCheckFixture(fixtureName);
    process.stdout.write(
      `focused-check fixture ${fixtureName}: ${String(
        outcome.passed,
      )}/${String(outcome.collected)} passed\n`,
    );
    process.exit(0);
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exit(1);
  }
}

const evidencePath = resolve(
  repositoryRoot,
  "docs/exec-plans/evidence/canonical-v1-fault-proof-reconciliation-v1.json",
);

// ---------------------------------------------------------------------------
// Coverage-matrix content reconciliation (V-5, issue #528)
//
// The coverage rows used to be checked by shape alone: every published line
// number had to start with "| " and carry enough pipe characters. Nothing ever
// read a cell, so flipping every ✅ in the matrix to ❌ was undetected, and
// nothing tied `summary.open` to the row arrays, so moving rows out of
// `openRows` published a smaller open count than the document supports.
//
// Everything below derives each row's disposition from the document itself:
// the matrix's own status legend supplies the glyph → disposition mapping, each
// row's status cell supplies its glyph, and the artifact's arrays and summary
// are reconciled against that derivation. A flipped glyph, a row moved between
// arrays, an unreconciled row, and a wrong summary each fail the gate closed
// with a distinct diagnostic naming the offending row.
// ---------------------------------------------------------------------------

class CoverageMatrixError extends Error {
  constructor(code, detail) {
    super(`${code}: ${detail}`);
    this.name = "CoverageMatrixError";
    this.code = code;
  }
}

// Pure: the legend row of the column table is the document's own definition of
// what each status glyph means, so the classification rule is read out of the
// matrix rather than hard-coded here. "Complete & verified" is the only locally
// complete state and "N/A" the only structural/N/A one; every other declared
// glyph is an open state.
const parseCoverageStatusLegend = (matrixLines) => {
  const legendLine = matrixLines.find((line) =>
    line.startsWith("| **Status**"),
  );
  if (legendLine === undefined) {
    throw new CoverageMatrixError(
      "ERR_COVERAGE_LEGEND_MISSING",
      "the matrix no longer declares a `**Status**` column legend, so no row disposition can be derived from it",
    );
  }
  const legendCell = legendLine
    .split("|")
    .slice(1, -1)
    .map((cell) => cell.trim())[1];
  const legend = new Map();
  for (const entry of (legendCell ?? "")
    .split("·")
    .map((part) => part.trim())
    .filter((part) => part.length > 0)) {
    const [glyph] = [...entry];
    const meaning = entry.slice(glyph.length).trim();
    legend.set(
      glyph,
      meaning.startsWith("Complete & verified")
        ? "locallyComplete"
        : meaning === "N/A"
          ? "structuralOrNA"
          : "open",
    );
  }
  const dispositions = [...legend.values()];
  const countOf = (disposition) =>
    dispositions.filter((value) => value === disposition).length;
  if (
    legend.size < 3 ||
    countOf("locallyComplete") !== 1 ||
    countOf("structuralOrNA") !== 1 ||
    countOf("open") === 0
  ) {
    throw new CoverageMatrixError(
      "ERR_COVERAGE_LEGEND_AMBIGUOUS",
      `the status legend must declare exactly one "Complete & verified" glyph, exactly one "N/A" glyph, and at least one open glyph — read ${JSON.stringify(
        Object.fromEntries(legend),
      )}`,
    );
  }
  return legend;
};

// The §8 removal/correction table carries six columns; every earlier table
// carries ten. `split("|")` counts the empty fragments either side of a Markdown
// row, hence the +2.
const requiredCoverageColumns = (lineNumber) => (lineNumber >= 182 ? 7 : 11);

const readMatrixRowCells = ({ matrixLines, lineNumber, kind }) => {
  const row = matrixLines[lineNumber - 1];
  if (row === undefined || !row.startsWith("| ")) {
    throw new CoverageMatrixError(
      "ERR_COVERAGE_ROW_MISSING",
      `${kind} row ${String(lineNumber)} is not a matrix row: ${JSON.stringify(
        row ?? null,
      )}`,
    );
  }
  const columns = row.split("|");
  const required =
    kind === "structural" ? 5 : requiredCoverageColumns(lineNumber);
  if (columns.length < required) {
    throw new CoverageMatrixError(
      "ERR_COVERAGE_ROW_SHAPE",
      `${kind} row ${String(lineNumber)} lost required reconciliation columns (${String(
        columns.length,
      )} < ${String(required)}): ${row.slice(0, 96)}`,
    );
  }
  return columns.slice(1, -1).map((cell) => cell.trim());
};

const coverageRowIdentity = (cells) =>
  (cells[0] ?? "").replace(/\s+/gu, " ").trim().slice(0, 64);

// Pure: matrix text plus the artifact's row inventory in, the document-derived
// disposition of every reconciled row out.
const deriveCoverageDispositions = ({ matrixLines, evidence: artifact }) => {
  const legend = parseCoverageStatusLegend(matrixLines);
  const derived = new Map();
  const identities = new Map();
  for (const lineNumber of artifact.coverageRows) {
    const cells = readMatrixRowCells({
      matrixLines,
      lineNumber,
      kind: "coverage",
    });
    identities.set(lineNumber, coverageRowIdentity(cells));
    const status = cells.at(-2) ?? "";
    if (status.length === 0) {
      throw new CoverageMatrixError(
        "ERR_COVERAGE_STATUS_EMPTY",
        `coverage row ${String(lineNumber)} (${identities.get(
          lineNumber,
        )}) publishes an empty status cell, so no disposition is claimed`,
      );
    }
    const [glyph] = [...status];
    if (!legend.has(glyph)) {
      throw new CoverageMatrixError(
        "ERR_COVERAGE_STATUS_UNKNOWN_GLYPH",
        `coverage row ${String(lineNumber)} (${identities.get(
          lineNumber,
        )}) opens its status with ${JSON.stringify(
          glyph,
        )}, which the column legend does not define`,
      );
    }
    derived.set(lineNumber, { disposition: legend.get(glyph), glyph, status });
  }
  for (const lineNumber of artifact.structuralRows) {
    const cells = readMatrixRowCells({
      matrixLines,
      lineNumber,
      kind: "structural",
    });
    identities.set(lineNumber, coverageRowIdentity(cells));
    // Structural claims live in their own table and are reconciled by the
    // structural audit below; the partition counts them as structural/N/A.
    derived.set(lineNumber, {
      disposition: "structuralOrNA",
      glyph: null,
      status: null,
    });
  }
  return { derived, identities };
};

// Pure: throws unless the artifact's three disposition arrays and its summary
// are exactly what the matrix content supports.
const reconcileCoverageMatrix = ({ matrixLines, evidence: artifact }) => {
  const { derived, identities } = deriveCoverageDispositions({
    matrixLines,
    evidence: artifact,
  });
  const published = new Map();
  for (const [bucket, rows] of [
    ["locallyComplete", artifact.locallyCompleteRows],
    ["structuralOrNA", artifact.structuralOrNARows],
    ["open", artifact.openRows],
  ]) {
    for (const lineNumber of rows) {
      if (published.has(lineNumber)) {
        throw new CoverageMatrixError(
          "ERR_COVERAGE_ROW_DOUBLE_DISPOSITION",
          `row ${String(lineNumber)} is published under both ${published.get(
            lineNumber,
          )} and ${bucket}`,
        );
      }
      published.set(lineNumber, bucket);
      if (!derived.has(lineNumber)) {
        throw new CoverageMatrixError(
          "ERR_COVERAGE_ROW_UNRECONCILED",
          `row ${String(
            lineNumber,
          )} is published under ${bucket} but is not one of the reconciled coverage or structural rows`,
        );
      }
    }
  }
  const mismatches = [...derived.keys()]
    .sort((left, right) => left - right)
    .flatMap((lineNumber) => {
      const { disposition, status } = derived.get(lineNumber);
      const publishedBucket = published.get(lineNumber);
      if (publishedBucket === disposition) return [];
      return [
        `row ${String(lineNumber)} (${identities.get(lineNumber)}) reads ${
          status === null
            ? "as a structural claim"
            : JSON.stringify(status.slice(0, 48))
        } in the matrix, which is ${disposition}, but the artifact publishes it as ${
          publishedBucket ?? "no disposition at all"
        }`,
      ];
    });
  if (mismatches.length > 0) {
    throw new CoverageMatrixError(
      "ERR_COVERAGE_DISPOSITION_MISMATCH",
      `${String(
        mismatches.length,
      )} reconciled row(s) contradict the matrix — ${mismatches.join("; ")}`,
    );
  }
  const counts = {
    locallyComplete: 0,
    structuralOrNA: 0,
    open: 0,
  };
  for (const { disposition } of derived.values()) counts[disposition] += 1;
  const expectedSummary = {
    ...counts,
    total: artifact.coverageRows.length + artifact.structuralRows.length,
    coverageRows: artifact.coverageRows.length,
    structuralRows: artifact.structuralRows.length,
  };
  const summaryMismatches = Object.entries(expectedSummary).flatMap(
    ([key, value]) =>
      artifact.summary?.[key] === value
        ? []
        : [
            `summary.${key} publishes ${JSON.stringify(
              artifact.summary?.[key] ?? null,
            )} where the reconciled matrix supports ${String(value)}`,
          ],
  );
  if (summaryMismatches.length > 0) {
    throw new CoverageMatrixError(
      "ERR_COVERAGE_SUMMARY_MISMATCH",
      summaryMismatches.join("; "),
    );
  }
  return counts;
};

// Seeded-defect fixtures. Each one mutates a deep copy of the real matrix and
// the real artifact — the exact mutations V-5 demonstrated were invisible — and
// is evaluated through the identical reconciliation path, so the negative
// self-tests measure the gate rather than describing it.
const coverageMatrixFixtureRows = {
  flippedComplete: 94,
  forgedComplete: 84,
  unknownGlyph: 86,
  truncatedRow: 105,
  movedRowCount: 5,
};

const replaceMatrixStatusGlyph = (row, replacement) => {
  const columns = row.split("|");
  const index = columns.length - 3;
  const [glyph] = [...columns[index].trim()];
  columns[index] = columns[index].replace(glyph, replacement);
  return columns.join("|");
};

const coverageMatrixFixtures = {
  // Positive control: the untouched pair must reconcile, so the rejections
  // below cannot be a gate that rejects everything.
  passing: () => {},
  // V-5's headline mutation: a published ✅ silently becomes a ❌.
  "flipped-status": ({ matrixLines }) => {
    const lineNumber = coverageMatrixFixtureRows.flippedComplete;
    matrixLines[lineNumber - 1] = replaceMatrixStatusGlyph(
      matrixLines[lineNumber - 1],
      "❌",
    );
  },
  // The same hole in the other direction: an open row is upgraded in the
  // document without the artifact ever noticing.
  "forged-complete": ({ matrixLines }) => {
    const lineNumber = coverageMatrixFixtureRows.forgedComplete;
    matrixLines[lineNumber - 1] = replaceMatrixStatusGlyph(
      matrixLines[lineNumber - 1],
      "✅",
    );
  },
  "unknown-glyph": ({ matrixLines }) => {
    const lineNumber = coverageMatrixFixtureRows.unknownGlyph;
    matrixLines[lineNumber - 1] = replaceMatrixStatusGlyph(
      matrixLines[lineNumber - 1],
      "🚀",
    );
  },
  "truncated-row": ({ matrixLines }) => {
    const lineNumber = coverageMatrixFixtureRows.truncatedRow;
    const columns = matrixLines[lineNumber - 1].split("|");
    matrixLines[lineNumber - 1] = [...columns.slice(0, 4), ""].join("|");
  },
  // V-5's second mutation: five rows move out of the open set, shrinking the
  // published open count without touching the document.
  "moved-row": ({ evidence: artifact }) => {
    const moved = artifact.openRows.slice(
      0,
      coverageMatrixFixtureRows.movedRowCount,
    );
    artifact.openRows = artifact.openRows.filter(
      (lineNumber) => !moved.includes(lineNumber),
    );
    artifact.structuralOrNARows = [
      ...artifact.structuralOrNARows,
      ...moved,
    ].sort((left, right) => left - right);
    artifact.summary.open -= moved.length;
    artifact.summary.structuralOrNA += moved.length;
  },
  // The arrays stay honest but the published headline count does not.
  "wrong-summary": ({ evidence: artifact }) => {
    artifact.summary.open -= 1;
  },
};

const evaluateCoverageMatrixFixture = (fixtureName) => {
  const mutate = coverageMatrixFixtures[fixtureName];
  assert.ok(mutate, `unknown coverage-matrix fixture ${fixtureName}`);
  const fixtureEvidence = JSON.parse(readFileSync(evidencePath, "utf8"));
  const fixtureMatrixLines = readFileSync(
    resolve(repositoryRoot, fixtureEvidence.source.path),
    "utf8",
  ).split(/\r?\n/u);
  mutate({ matrixLines: fixtureMatrixLines, evidence: fixtureEvidence });
  return reconcileCoverageMatrix({
    matrixLines: fixtureMatrixLines,
    evidence: fixtureEvidence,
  });
};

// Fixture mode: reconcile one seeded fixture and nothing else, so the negative
// self-tests below can spawn this very gate and observe its real exit code.
const coverageFixtureArgument = process.argv
  .slice(2)
  .find((argument) => argument.startsWith("--coverage-matrix-fixture="));
if (coverageFixtureArgument !== undefined) {
  const fixtureName = coverageFixtureArgument.slice(
    "--coverage-matrix-fixture=".length,
  );
  try {
    const counts = evaluateCoverageMatrixFixture(fixtureName);
    process.stdout.write(
      `coverage-matrix fixture ${fixtureName}: ${String(
        counts.open,
      )} open, ${String(counts.locallyComplete)} locally complete, ${String(
        counts.structuralOrNA,
      )} structural/N/A\n`,
    );
    process.exit(0);
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exit(1);
  }
}

const evidence = JSON.parse(await readFile(evidencePath, "utf8"));
const matrixPath = resolve(repositoryRoot, evidence.source.path);
const matrixBytes = await readFile(matrixPath);
const matrixLines = matrixBytes.toString("utf8").split(/\r?\n/u);

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-fault-proof-reconciliation.v1",
);

// Content reconciliation, first: every published row's disposition and every
// partition count must be exactly what the matrix cells say. This subsumes the
// former shape-only loops — a row that is missing, truncated, re-glyphed, moved
// between arrays, or miscounted fails here with a diagnostic naming it, before
// any literal pin below can report the same drift as an opaque diff.
const coverageCounts = reconcileCoverageMatrix({ matrixLines, evidence });

assert.deepEqual(evidence.summary, {
  total: 70,
  coverageRows: 61,
  structuralRows: 9,
  locallyComplete: 8,
  structuralOrNA: 13,
  open: 49,
  preprodComplete: 0,
  focusedChecksPassed: 45,
  focusedChecksTotal: 45,
  focusedCheckName: "family-scaffold-v1.test.ts strict scanner",
  focusedCommand:
    "pnpm --dir demo/midgard-fault-proofs exec vitest run tests/family-scaffold-v1.test.ts",
});

const allRows = [...evidence.coverageRows, ...evidence.structuralRows];
assert.equal(new Set(allRows).size, 70, "row identifiers must be unique");
assert.equal(
  evidence.locallyCompleteRows.length +
    evidence.structuralOrNARows.length +
    evidence.openRows.length,
  70,
  "every row must have exactly one disposition",
);
assert.deepEqual(
  [
    ...new Set([
      ...evidence.locallyCompleteRows,
      ...evidence.structuralOrNARows,
      ...evidence.openRows,
    ]),
  ].sort((left, right) => left - right),
  [...allRows].sort((left, right) => left - right),
  "dispositions must cover the reconciled row set",
);

assert.deepEqual(
  evidence.locallyCompleteRows,
  [93, 94, 95, 96, 137, 174, 176, 182],
);
assert.deepEqual(
  evidence.structuralRows,
  [295, 296, 297, 298, 299, 300, 301, 302, 303],
);
assert.deepEqual(evidence.initialLaunchScope, {
  sourceAnchor:
    "GOAL_SPEC.md §9.1 and §9.3; current deployed positional catalogue",
  status: "INITIAL_ONLY",
  families: [
    "double-spend",
    "no-input",
    "input-no-idx",
    "invalid-range",
    "transition-trace",
    "zero-input",
    "validation-trace-dispute",
    "da-hash-preimage",
  ],
  count: 8,
  note: "This is F20's concrete initial §9.1 list, not a final launch decision or a LOCAL_PASS/LIVE_PASS promotion. Q50/Q55 own final route and enabled-state integration.",
});
assert.deepEqual(evidence.taskResidues, {
  completedPrerequisites: ["Q00", "Q02", "Q03", "Q24", "Q25", "Q44", "Q54"],
  completedFamilyLifecycle: ["Q13"],
  openFamilyClosures: ["Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20"],
  openStructural: ["Q49-L298", "Q49-L302"],
  openLifecycleAndAcceptance: [
    "Q50",
    "Q51",
    "Q52",
    "Q53",
    "Q55",
    "Q56",
    "Q57",
    "Q58",
    "Q59",
    "Q60",
    "Q61",
    "Q62",
    "Q63",
    "QG1",
    "QG2",
    "QG3",
  ],
});
assert.deepEqual(evidence.structuralAudit.summary, {
  rows: 9,
  pass: 7,
  partial: 2,
  open: 0,
});
const structuralAuditRows = Object.entries(evidence.structuralAudit.rows);
assert.equal(structuralAuditRows.length, 9);
assert.equal(
  structuralAuditRows.filter(([, row]) => row.disposition === "PASS").length,
  7,
);
assert.equal(
  structuralAuditRows.filter(([, row]) => row.disposition === "PARTIAL").length,
  2,
);
assert.equal(
  structuralAuditRows.filter(([, row]) => row.disposition === "OPEN").length,
  0,
);
for (const [rowId, row] of structuralAuditRows) {
  if (row.disposition === "PASS") {
    assert.ok(
      ["L295", "L296", "L297", "L299", "L300", "L301", "L303"].includes(rowId),
    );
    assert.equal(row.remainingTask, null);
  } else {
    assert.match(row.remainingTask, /^Q49-L\d{3}$/u);
  }
}
const structuralContract = {
  L295: ["Duplicate TxId in a block", "PASS", null],
  L296: ["Header carry-over fields", "PASS", null],
  L297: ["Oversized deposits", "PASS", null],
  L298: ["Cross-block replay", "PARTIAL", "Q49-L298"],
  L299: ["L2 fee misdirection", "PASS", null],
  L300: ["Withdrawal payout amount/destination", "PASS", null],
  L301: ["Deposit `inclusion_time` value forgery", "PASS", null],
  L302: ["Malformed validity interval", "PARTIAL", "Q49-L302"],
  L303: ["Deposit refund double-representation", "PASS", null],
};
for (const [rowId, [concern, disposition, remainingTask]] of Object.entries(
  structuralContract,
)) {
  const lineNumber = Number(rowId.slice(1));
  assert.ok(
    matrixLines[lineNumber - 1]?.includes(concern),
    `${rowId} no longer identifies ${concern}`,
  );
  const row = evidence.structuralAudit.rows[rowId];
  assert.equal(row?.disposition, disposition, `${rowId} disposition drifted`);
  assert.equal(row?.remainingTask, remainingTask, `${rowId} task drifted`);
  assert.ok(row.executableEvidence?.length > 0, `${rowId} lacks evidence`);
}
assert.ok(
  matrixLines[293]?.startsWith("| ---"),
  "L294 must remain the table separator, never a structural claim",
);
const [
  phaseBSource,
  phaseBTestSource,
  depositTypeSource,
  depositValidatorSource,
  depositSdkSource,
  blueprintSource,
] = await Promise.all([
  readFile(
    resolve(repositoryRoot, "demo/midgard-validation/src/phase-b.ts"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "demo/midgard-validation/tests/phase-b.test.ts"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "onchain/aiken/lib/midgard/user-events/deposit.ak"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "onchain/aiken/validators/user-events/deposit.ak"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "demo/midgard-sdk/src/user-events/deposit.ts"),
    "utf8",
  ),
  readFile(resolve(repositoryRoot, "onchain/aiken/plutus.json"), "utf8"),
]);
for (const requiredFeeSymbol of [
  "valuePreservationDelta(",
  "ledgerTx.fee",
  "RejectCodes.ValueNotPreserved",
  '"valueAndMint"',
]) {
  assert.ok(
    phaseBSource.includes(requiredFeeSymbol),
    `L299 production fee invariant lost ${requiredFeeSymbol}`,
  );
}
assert.ok(
  phaseBTestSource.includes(
    "burns the exact L2 fee in production value accounting and rejects fee redirection",
  ),
  "L299 exact fee-burn control is absent",
);
for (const source of [
  depositTypeSource,
  depositValidatorSource,
  depositSdkSource,
]) {
  assert.doesNotMatch(source, /\bRefund\b/u, "L303 deposit refund path exists");
}
for (const requiredDepositGuard of [
  "singular_utxo_indexer.one_to_one(",
  "BurnEventNFT",
  "settlement.valid_counted_membership(",
  "output_datum == NoDatum",
  "output_address == reserve_addr",
]) {
  assert.ok(
    depositValidatorSource.includes(requiredDepositGuard),
    `L303 deposit absorption guard lost ${requiredDepositGuard}`,
  );
}
assert.ok(
  depositSdkSource.includes(
    "export const DepositSpendRedeemerSchema = Data.Object({",
  ),
  "L303 SDK deposit spend schema is not one exact record",
);
const blueprint = JSON.parse(blueprintSource);
const depositSpendSchema =
  blueprint.definitions?.["midgard/user_events/deposit/SpendRedeemer"];
assert.equal(depositSpendSchema?.anyOf?.length, 1);
assert.equal(depositSpendSchema.anyOf[0]?.index, 0);
assert.deepEqual(
  depositSpendSchema.anyOf[0]?.fields?.map((field) => field.title),
  [
    "input_index",
    "output_index",
    "hub_ref_input_index",
    "settlement_ref_input_index",
    "mint_redeemer_index",
    "membership_proof",
    "inclusion_proof_script_withdraw_redeemer_index",
  ],
);
assert.equal(evidence.structuralAudit.adjacentRegression.status, "PASS");
assert.equal(evidence.acceptance.F20, "PASS");
assert.equal(evidence.acceptance.F21, "PASS");
assert.equal(evidence.acceptance.QG1, "OPEN");
assert.equal(evidence.acceptance.QG2, "OPEN");
assert.equal(evidence.acceptance.QG3, "OPEN");

const [
  catalogueSource,
  faultProofContractsSource,
  submitInitSource,
  inspectContractsSource,
  commonFaultProofSource,
  transitionTraceSource,
] = await Promise.all([
  readFile(
    resolve(repositoryRoot, "demo/midgard-sdk/src/fraud-proof/catalogue.ts"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "demo/midgard-sdk/src/fraud-proof/contracts.ts"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "demo/midgard-fault-proofs/src/submit-init.ts"),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "demo/midgard-fault-proofs/src/inspect-contracts.ts",
    ),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "onchain/aiken/lib/midgard/fraud-proofs/common.ak"),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak",
    ),
    "utf8",
  ),
]);
const stringLiteralsBetween = (source, start, end) => {
  const startIndex = source.indexOf(start);
  assert.notEqual(startIndex, -1, `missing source anchor ${start}`);
  const endIndex = source.indexOf(end, startIndex);
  assert.notEqual(endIndex, -1, `missing source anchor ${end}`);
  return [...source.slice(startIndex, endIndex).matchAll(/"([^"\n]+)"/gu)].map(
    ([, value]) => value,
  );
};
const registeredCategoryNames = stringLiteralsBetween(
  catalogueSource,
  "export const FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = [",
  "] as const",
);
assert.equal(evidence.bindingInventory.compiledStandaloneFamilies, 12);
assert.deepEqual(
  evidence.bindingInventory.registeredCategoryNames,
  registeredCategoryNames,
  "evidence category inventory diverged from the SDK catalogue",
);
assert.equal(
  evidence.bindingInventory.registeredCatalogueCategories,
  registeredCategoryNames.length,
);
assert.equal(
  evidence.bindingInventory.initializationCliCategories,
  registeredCategoryNames.length,
);
assert.equal(
  evidence.bindingInventory.inspectionCategories,
  registeredCategoryNames.length,
);
assert.deepEqual(
  stringLiteralsBetween(
    submitInitSource,
    "export type SubmitInitFraudCategory =",
    ";",
  ),
  registeredCategoryNames,
  "submit-init category union diverged from the catalogue",
);
const inspectionCategoryNames = stringLiteralsBetween(
  inspectContractsSource,
  "export type InspectContractsProofCategory =",
  ";",
);
assert.equal(
  inspectionCategoryNames.length,
  8,
  "inspect-contracts category count changed",
);
assert.deepEqual(
  [...inspectionCategoryNames].sort(),
  [...registeredCategoryNames].sort(),
  "inspect-contracts category set diverged from the catalogue",
);
const nativeHelperDefinition =
  /\bpub\s+fn\s+pass_native_tx_to_next_step\s*\(/gu;
assert.equal(
  [...commonFaultProofSource.matchAll(nativeHelperDefinition)].length,
  1,
  "native V1 transition helper must retain one exact identifier definition",
);
assert.doesNotMatch(
  commonFaultProofSource,
  /verify_tx_in_state_queue_node/u,
  "legacy PlutusData binding helper returned",
);
for (const requiredTransitionTraceAnchor of [
  "header.transition_trace_root",
  "validation_claim_v1.committed_claim_is_valid",
]) {
  assert.ok(
    transitionTraceSource.includes(requiredTransitionTraceAnchor),
    `transition-trace native V1 binding lost ${requiredTransitionTraceAnchor}`,
  );
}
const [
  computationThreadTestSource,
  catalogueTestSource,
  stateQueueTestSource,
  onchainReferenceSource,
  testingStatusSource,
] = await Promise.all([
  readFile(
    resolve(repositoryRoot, "onchain/aiken/validators/computation-thread.ak"),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "onchain/aiken/validators/fraud-proof-catalogue.ak",
    ),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "onchain/aiken/validators/state-queue.ak"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "docs/fault-proofs/onchain-reference.md"),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "docs/fault-proofs/testing-status.md"),
    "utf8",
  ),
]);
const aikenTestSelectors = (source) =>
  [...source.matchAll(/^test\s+([^\s(]+)\(/gmu)].map(
    ([, selector]) => selector,
  );
const computationThreadSelectors = aikenTestSelectors(
  computationThreadTestSource,
);
const catalogueSelectors = aikenTestSelectors(catalogueTestSource);
const stateQueueSelectors = aikenTestSelectors(stateQueueTestSource);
assert.equal(computationThreadSelectors.length, 15);
assert.equal(catalogueSelectors.length, 4);
assert.equal(stateQueueSelectors.length, 6);
assert.ok(
  computationThreadSelectors.every((selector) => selector.startsWith("ct_")),
  "computation-thread direct-test inventory lost its exact selectors",
);
assert.ok(
  catalogueSelectors.every((selector) => selector.startsWith("catalogue_")),
  "catalogue direct-test inventory lost its exact selectors",
);
assert.ok(
  stateQueueSelectors.every((selector) => selector.startsWith("q49_l295_")),
  "state-queue HeaderV1 selector inventory drifted",
);
for (const anchor of [
  "validators/computation-thread.ak:280-513`                                       | 15",
  "validators/fraud-proof-catalogue.ak:50-76`                                      | 4",
  "validators/state-queue.ak:841-1065`                                             | 6",
]) {
  assert.ok(
    onchainReferenceSource.includes(anchor),
    `onchain reference lost direct-test inventory anchor ${anchor}`,
  );
}
for (const anchor of [
  "computation-thread (15)",
  "immutable catalogue (4)",
  "state-queue commit controls (6)",
]) {
  assert.ok(
    testingStatusSource.includes(anchor),
    `testing status lost direct-test inventory anchor ${anchor}`,
  );
}
for (const [path, anchor] of Object.entries(evidence.documentationAnchors)) {
  const source = await readFile(resolve(repositoryRoot, path), "utf8");
  assert.ok(source.includes(anchor), `${path} lost reconciliation anchor`);
}
const completedDocTaskIds = ["Q13", "Q24", "Q25", "Q44", "Q54"];
const currentBlockerWord =
  /\b(?:OPEN|BLOCK(?:S|ED|ING)|REMAIN(?:S|ING)?|UNIMPLEMENTED)\b/iu;
const historicalStatement =
  /\b(?:historical(?:ly)?|superseded|formerly|previously|prior)\b/iu;
const semanticDocUnits = (source) =>
  source
    .split(/\r?\n/u)
    .flatMap((line) =>
      line.startsWith("|")
        ? line
            .split("|")
            .map((cell) => cell.trim())
            .filter(Boolean)
        : line.split(/(?<=[.!?])\s+/u),
    )
    .flatMap((unit) => unit.split(/[;:]/u))
    .map((unit) => unit.trim())
    .filter(Boolean);
for (const path of Object.keys(evidence.documentationAnchors)) {
  const source = await readFile(resolve(repositoryRoot, path), "utf8");
  for (const unit of semanticDocUnits(source)) {
    if (historicalStatement.test(unit) || !currentBlockerWord.test(unit))
      continue;
    for (const taskId of completedDocTaskIds) {
      const gapWithoutAnotherTask =
        "(?:(?!\\bQ[A-Z0-9][A-Z0-9-]*\\b)[\\s\\S]){0,120}";
      const currentTaskBlocker = new RegExp(
        `(?:\\b${taskId}\\b${gapWithoutAnotherTask}\\b(?:OPEN|BLOCK(?:S|ED|ING)|REMAIN(?:S|ING)?|UNIMPLEMENTED)\\b|\\b(?:OPEN|BLOCK(?:S|ED|ING)|REMAIN(?:S|ING)?|UNIMPLEMENTED)\\b${gapWithoutAnotherTask}\\b${taskId}\\b)`,
        "iu",
      );
      if (!currentTaskBlocker.test(unit)) continue;
      const explicitlyNegatesBlocker = new RegExp(
        `(?:\\b(?:not|no|never)\\b[^.]{0,120}\\b${taskId}\\b|\\b${taskId}\\b[^.]{0,120}\\b(?:not|no|never)\\b)[^.]{0,120}\\b(?:OPEN|BLOCK(?:S|ED|ING)|REMAIN(?:S|ING)?|UNIMPLEMENTED)\\b`,
        "iu",
      );
      const explicitlyExcludesCompletedTask = new RegExp(
        `\\b${taskId}\\b\\s+is\\s+no\\s+longer\\b`,
        "iu",
      );
      if (
        explicitlyNegatesBlocker.test(unit) ||
        explicitlyExcludesCompletedTask.test(unit)
      )
        continue;
      assert.ok(
        !new RegExp(`\\b${taskId}\\b`, "u").test(unit),
        `${path} presents completed ${taskId} as a current blocker/open residue: ${unit}`,
      );
    }
  }
}

const [
  goalProgressSource,
  taskManifestSource,
  goalSpecSource,
  watcherDependencyMapSource,
  binSource,
  inputNoIdxSource,
  inputNoIdxEmulatorTest,
  inputNoIdxPreparationTest,
  deployedValidatorEntries,
] = await Promise.all([
  readFile(resolve(repositoryRoot, "GOAL_PROGRESS.md"), "utf8"),
  readFile(
    resolve(
      repositoryRoot,
      "docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json",
    ),
    "utf8",
  ),
  readFile(resolve(repositoryRoot, "GOAL_SPEC.md"), "utf8"),
  readFile(
    resolve(
      repositoryRoot,
      "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
    ),
    "utf8",
  ),
  readFile(
    resolve(repositoryRoot, "demo/midgard-fault-proofs/src/bin.ts"),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "demo/midgard-fault-proofs/src/submit-input-no-idx-step-04.ts",
    ),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "demo/midgard-fault-proofs/tests/submit-init-emulator-input-no-idx.test.ts",
    ),
    "utf8",
  ),
  readFile(
    resolve(
      repositoryRoot,
      "demo/midgard-fault-proofs/tests/prepare-input-no-idx.test.ts",
    ),
    "utf8",
  ),
  readdir(resolve(repositoryRoot, "onchain/aiken/validators/fraud-proofs"), {
    withFileTypes: true,
  }),
]);
const queueSource = goalProgressSource.slice(
  goalProgressSource.indexOf("## Task queue"),
);
const firstQueueStatuses = new Map();
const firstQueueDetails = new Map();
for (const line of queueSource.split(/\r?\n/u)) {
  const cells = line.split("|").map((cell) => cell.trim());
  const taskId = cells[1];
  const status = cells[5];
  if (
    /^[A-Z][A-Z0-9-]*$/u.test(taskId) &&
    status &&
    !firstQueueStatuses.has(taskId)
  ) {
    firstQueueStatuses.set(taskId, status);
    firstQueueDetails.set(taskId, cells.at(-2) ?? "");
  }
}
// GOAL_PROGRESS.md is the orchestrator-owned ledger and is not writable from
// here, so this pin still reads its captured wording — "five locally complete,
// 12 structural/N/A, 53 open" — which #528 re-derived to 8/13/49. The pin is
// deliberately exact so that the ledger's next sync trips this gate rather
// than passing silently in either direction.
assert.match(
  firstQueueDetails.get("F20") ?? "",
  /61 coverage rows and nine (?:physical )?structural claims: five locally complete, 12 structural\/N\/A, 53 open/u,
  "first F20 queue record retained stale reconciliation totals",
);
assert.match(
  firstQueueDetails.get("F21") ?? "",
  /PARTIAL L298 cross-block replay and L302 malformed interval.*7 PASS\s*\/\s*2 PARTIAL\s*\/\s*0 OPEN/iu,
  "first F21 queue record retained stale physical structural dispositions",
);
assert.doesNotMatch(
  firstQueueDetails.get("F21") ?? "",
  /Q49-L29[7]|8 PASS\s*\/\s*1 PARTIAL|L297 is the sole remaining/u,
  "first F21 queue record retained stale physical-row identity",
);
assert.ok(
  goalSpecSource.includes(
    "F20 also emits the initial concrete §9.1 launch-scope family list.",
  ),
  "GOAL_SPEC F20 authority anchor is absent",
);
assert.ok(
  goalSpecSource.includes(
    "Unsupported prose-only N/A claims become open tasks.",
  ),
  "GOAL_SPEC F21 authority anchor is absent",
);
const passIds = new Set(
  [...firstQueueStatuses]
    .filter(([, status]) => status.startsWith("PASS"))
    .map(([taskId]) => taskId),
);
for (const taskId of evidence.taskResidues.completedPrerequisites) {
  assert.ok(
    passIds.has(taskId),
    `${taskId} is not PASS in the first task queue`,
  );
}
assert.ok(passIds.has("Q13"), "Q13 is not PASS in the first task queue");
const residueTaskIds = [
  ...evidence.taskResidues.openFamilyClosures,
  ...evidence.taskResidues.openLifecycleAndAcceptance,
  ...evidence.criticalFindings.flatMap((finding) => finding.remainingTasks),
];
for (const taskId of residueTaskIds) {
  assert.ok(!passIds.has(taskId), `${taskId} is PASS but remains open`);
}
const criticalFindingById = new Map(
  evidence.criticalFindings.map((finding) => [finding.id, finding]),
);
assert.deepEqual(criticalFindingById.get("F20-04")?.remainingTasks, [
  "Q51",
  "Q57",
  "Q58",
  "Q59",
]);
assert.deepEqual(criticalFindingById.get("F20-06")?.remainingTasks, [
  "Q23",
  "Q26",
  "Q27",
  "Q28",
  "Q29",
  "Q30",
  "Q31",
  "Q32",
  "Q33",
  "Q34",
  "Q35",
  "Q36",
  "Q37",
  "Q38",
  "Q39",
  "Q40",
  "Q41",
  "Q42",
  "Q43",
  "Q45",
  "Q46",
  "Q47",
  "Q48",
  "Q49",
  "Q50",
  "Q55",
]);
assert.deepEqual(criticalFindingById.get("F20-06")?.sources, [
  "demo/midgard-sdk/src/fraud-proof/catalogue.ts",
  "onchain/aiken/validators/fraud-proofs/input-no-idx/step-01.ak",
]);
assert.ok(!evidence.taskResidues.openLifecycleAndAcceptance.includes("Q54"));
assert.ok(
  !evidence.criticalFindings
    .find(({ id }) => id === "F20-04")
    .remainingTasks.includes("Q54"),
);
assert.ok(
  !evidence.criticalFindings
    .find(({ id }) => id === "F20-06")
    .remainingTasks.includes("Q24"),
);
assert.ok(
  !evidence.criticalFindings
    .find(({ id }) => id === "F20-06")
    .remainingTasks.includes("Q25"),
);
const taskManifest = JSON.parse(taskManifestSource);
const manifestTasks = [];
const visitManifest = (value) => {
  if (Array.isArray(value)) value.forEach(visitManifest);
  else if (value && typeof value === "object") {
    if (typeof value.id === "string") manifestTasks.push(value);
    Object.values(value).forEach(visitManifest);
  }
};
visitManifest(taskManifest);
const manifestTaskById = new Map(manifestTasks.map((task) => [task.id, task]));
const f20Manifest = manifestTaskById.get("F20");
const f21Manifest = manifestTaskById.get("F21");
const f20ManifestText = JSON.stringify(f20Manifest);
const f21ManifestText = JSON.stringify(f21Manifest);
// The manifest prose must republish the partition the matrix actually
// supports, not the pre-#528 shape-only numbers.
assert.match(
  f20ManifestText,
  new RegExp(
    `${String(evidence.summary.locallyComplete)} locally complete, ${String(
      evidence.summary.structuralOrNA,
    )} structural/N/A, and ${String(evidence.summary.open)} open`,
    "u",
  ),
  "F20 manifest retained stale reconciliation totals",
);
assert.doesNotMatch(
  f20ManifestText,
  /6 registered|5 initialization|53 open|54 open|44 of 45/u,
  "F20 manifest retained stale completed-task mismatch wording",
);
assert.match(
  f21ManifestText,
  /7 PASS, 2 PARTIAL.*L298.*Q49-L298.*L302.*Q49-L302/su,
  "F21 manifest retained stale physical structural contract",
);
assert.doesNotMatch(
  f21ManifestText,
  /8 PASS|Q49-L29[7]|L297 is exactly the 1 PARTIAL/u,
  "F21 manifest retained stale completed-task structural wording",
);
for (const task of manifestTasks) {
  if (typeof task.blockedBecause !== "string") continue;
  assert.doesNotMatch(
    task.blockedBecause,
    /\b(?:Q00|Q02|Q03|Q13|Q24|Q25|Q44|Q54)\b\s+(?:is|are|remains?)\s+(?:OPEN|PENDING|BLOCKED|not PASS)\b/iu,
    `${task.id} retained a completed task as a blocker`,
  );
}
const watcherDependencyMap = JSON.parse(watcherDependencyMapSource);
const watcherRemainingTaskArrays = [];
const collectRemainingTasks = (value) => {
  if (Array.isArray(value)) {
    value.forEach(collectRemainingTasks);
  } else if (value && typeof value === "object") {
    if (Array.isArray(value.remainingTasks)) {
      watcherRemainingTaskArrays.push(value.remainingTasks);
    }
    Object.values(value).forEach(collectRemainingTasks);
  }
};
collectRemainingTasks(watcherDependencyMap);
assert.ok(
  watcherRemainingTaskArrays.every((tasks) => !tasks.includes("F20")),
  "watcher dependency map retained completed F20 as a remaining task",
);
for (const taskId of evidence.taskResidues.openFamilyClosures) {
  const task = manifestTasks.find(({ id }) => id === taskId);
  assert.ok(task?.blockedBecause, `${taskId} lacks a current manifest blocker`);
  assert.ok(!passIds.has(taskId), `${taskId} is queue PASS but manifest-open`);
}
assert.ok(binSource.includes("submit-input-no-idx-step-04"));
assert.ok(inputNoIdxSource.includes("submitInputNoIdxStep04"));
assert.ok(
  inputNoIdxEmulatorTest.includes(
    "input-no-idx fault-proof emulator lifecycle",
  ),
  "Q13 emulator lifecycle test is absent",
);
assert.ok(
  inputNoIdxEmulatorTest.includes(
    "cannot finalize an input-no-idx thread against a valid block",
  ),
  "Q13 valid-block negative is absent",
);
assert.ok(
  inputNoIdxPreparationTest.includes("Q13 input-no-idx canonical evidence"),
  "Q13 preparation evidence test is absent",
);
// The focused counts are measured, not counted from declarations: the runner is
// spawned and its JSON report is the only source of `focusedChecksTotal` and
// `focusedChecksPassed`.
assert.equal(
  evidence.summary.focusedCheckName,
  focusedCheckPlan.name,
  "focused check identity drifted from the executed plan",
);
assert.equal(
  evidence.summary.focusedCommand,
  focusedCheckPublishedCommand,
  "published focused command drifted from the command this gate executes",
);
assert.ok(
  taskManifestSource.includes(evidence.summary.focusedCommand),
  "focused external gate is not named by the task authority",
);
const focusedCheckOutcome = deriveFocusedCheckOutcome({
  label: focusedCheckPlan.name,
  ...runFocusedCheck({ testFile: focusedCheckPlan.testFile }),
});
assert.equal(
  evidence.summary.focusedChecksTotal,
  focusedCheckOutcome.collected,
  "published focused total is not the number of tests the runner collected",
);
assert.equal(
  evidence.summary.focusedChecksPassed,
  focusedCheckOutcome.passed,
  "published focused pass count is not the number of tests the runner passed",
);

// Negative self-tests: spawn this gate against seeded fixtures and require a
// non-zero exit carrying the specific diagnostic. Without these, a future edit
// could reintroduce existence-as-passage and nothing would notice.
const focusedCheckSelfTests = [
  ["failing", /ERR_FOCUSED_CHECK_FAILED: .*executes and fails/su],
  [
    "zero-collection",
    /ERR_FOCUSED_CHECK_ZERO_COLLECTION: .*collected 0 tests/su,
  ],
  ["skipped", /ERR_FOCUSED_CHECK_NOT_EXECUTED: .*never executed/su],
  ["missing-file", /ERR_FOCUSED_CHECK_NO_FILES: .*matched no test file/su],
];
const runFocusedCheckSelfTest = (fixtureName) =>
  spawnSync(
    process.execPath,
    [fileURLToPath(import.meta.url), `--focused-check-fixture=${fixtureName}`],
    { cwd: repositoryRoot, encoding: "utf8", maxBuffer: 128 * 1024 * 1024 },
  );
for (const [fixtureName, expectedDiagnostic] of focusedCheckSelfTests) {
  const selfTest = runFocusedCheckSelfTest(fixtureName);
  assert.notEqual(
    selfTest.status,
    0,
    `focused-check gate accepted the seeded ${fixtureName} defect`,
  );
  assert.match(
    selfTest.stderr,
    expectedDiagnostic,
    `focused-check gate rejected the seeded ${fixtureName} defect without its specific diagnostic`,
  );
}
// Positive control: the same harness must still accept a real passing run, so
// the four rejections above cannot be a gate that rejects everything.
const focusedCheckControl = runFocusedCheckSelfTest("passing");
assert.equal(
  focusedCheckControl.status,
  0,
  `focused-check gate rejected a passing fixture: ${focusedCheckControl.stderr}`,
);
assert.match(focusedCheckControl.stdout, /passing: 1\/1 passed/u);

// Negative self-tests for the coverage-matrix reconciliation: spawn this gate
// against each seeded mutation of the real matrix/artifact pair and require a
// non-zero exit carrying the specific diagnostic. The first two are the exact
// mutations V-5 showed the shape-only check could not see.
const coverageMatrixSelfTests = [
  [
    "flipped-status",
    /ERR_COVERAGE_DISPOSITION_MISMATCH: 1 reconciled row\(s\).*row 94 \(Input exists \(NO-INPUT\)\).*which is open, but the artifact publishes it as locallyComplete/su,
  ],
  [
    "forged-complete",
    /ERR_COVERAGE_DISPOSITION_MISMATCH:.*row 84 .*which is locallyComplete, but the artifact publishes it as open/su,
  ],
  [
    "moved-row",
    /ERR_COVERAGE_DISPOSITION_MISMATCH: 5 reconciled row\(s\).*publishes it as structuralOrNA/su,
  ],
  [
    "wrong-summary",
    /ERR_COVERAGE_SUMMARY_MISMATCH: summary\.open publishes 48 where the reconciled matrix supports 49/su,
  ],
  [
    "unknown-glyph",
    /ERR_COVERAGE_STATUS_UNKNOWN_GLYPH: coverage row 86 .*which the column legend does not define/su,
  ],
  [
    "truncated-row",
    /ERR_COVERAGE_ROW_SHAPE: coverage row 105 lost required reconciliation columns/su,
  ],
];
const runCoverageMatrixSelfTest = (fixtureName) =>
  spawnSync(
    process.execPath,
    [
      fileURLToPath(import.meta.url),
      `--coverage-matrix-fixture=${fixtureName}`,
    ],
    { cwd: repositoryRoot, encoding: "utf8", maxBuffer: 128 * 1024 * 1024 },
  );
for (const [fixtureName, expectedDiagnostic] of coverageMatrixSelfTests) {
  const selfTest = runCoverageMatrixSelfTest(fixtureName);
  assert.notEqual(
    selfTest.status,
    0,
    `coverage-matrix gate accepted the seeded ${fixtureName} defect`,
  );
  assert.match(
    selfTest.stderr,
    expectedDiagnostic,
    `coverage-matrix gate rejected the seeded ${fixtureName} defect without its specific diagnostic`,
  );
}
// Positive control: the unmutated pair must still reconcile.
const coverageMatrixControl = runCoverageMatrixSelfTest("passing");
assert.equal(
  coverageMatrixControl.status,
  0,
  `coverage-matrix gate rejected the unmutated matrix: ${coverageMatrixControl.stderr}`,
);
assert.match(
  coverageMatrixControl.stdout,
  new RegExp(
    `passing: ${String(coverageCounts.open)} open, ${String(
      coverageCounts.locallyComplete,
    )} locally complete, ${String(coverageCounts.structuralOrNA)} structural/N/A`,
    "u",
  ),
);
const parsedCliCategoryNames = [
  ...binSource
    .slice(
      binSource.indexOf("export const parseFraudCategory"),
      binSource.indexOf("export const parseArgs"),
    )
    .matchAll(/value === "([^"\n]+)"/gu),
].map(([, name]) => name);
assert.deepEqual(
  [...parsedCliCategoryNames].sort(),
  [...registeredCategoryNames].sort(),
  "CLI accepted fraud-category set drifted",
);
const deployedValidatorDirectories = deployedValidatorEntries
  .filter((entry) => entry.isDirectory())
  .map((entry) => entry.name)
  .sort();
const registeredValidatorDirectories = [
  ...new Set(
    [...faultProofContractsSource.matchAll(/"fraud_proofs\/([^/]+)\//gu)].map(
      ([, directory]) => directory.replaceAll("_", "-"),
    ),
  ),
].sort();
const unregisteredValidatorDirectories = deployedValidatorDirectories.filter(
  (directory) => !registeredValidatorDirectories.includes(directory),
);
assert.deepEqual(
  evidence.bindingInventory.deployedValidatorDirectories,
  deployedValidatorDirectories,
  "evidence deployed-validator inventory diverged from the validators tree",
);
assert.equal(
  registeredValidatorDirectories.length,
  registeredCategoryNames.length,
  "compiled category-title groups diverged from the catalogue category count",
);
assert.deepEqual(
  evidence.bindingInventory.unregisteredValidatorDirectories,
  unregisteredValidatorDirectories,
  "six catalogue-decision residues must derive from source titles and deployed directories",
);
assert.equal(
  unregisteredValidatorDirectories.length,
  6,
  "the deployed inventory must expose exactly six unregistered validator directories",
);
const nativeStepSources = await Promise.all(
  deployedValidatorDirectories.map(async (directory) => {
    try {
      return [
        directory,
        await readFile(
          resolve(
            repositoryRoot,
            `onchain/aiken/validators/fraud-proofs/${directory}/step-01.ak`,
          ),
          "utf8",
        ),
      ];
    } catch {
      return [directory, null];
    }
  }),
);
const nativeHelperCall = /\bpass_native_tx_to_next_step\s*\(/gu;
const nativeV1StepFamilies = nativeStepSources
  .filter(
    ([, source]) =>
      [...(source?.matchAll(nativeHelperCall) ?? [])].length === 1,
  )
  .map(([directory]) => directory);
for (const [directory, source] of nativeStepSources) {
  if (source === null) continue;
  const callCount = [...source.matchAll(nativeHelperCall)].length;
  if (callCount !== 0) {
    assert.equal(
      callCount,
      1,
      `${directory} must call pass_native_tx_to_next_step exactly once`,
    );
  }
}
const nativeV1Families = [
  ...nativeV1StepFamilies,
  ...(transitionTraceSource.includes("header.transition_trace_root") &&
  transitionTraceSource.includes("validation_claim_v1.committed_claim_is_valid")
    ? ["transition-trace"]
    : []),
].sort();
assert.deepEqual(
  evidence.bindingInventory.nativeV1Families,
  nativeV1Families,
  "native V1 family inventory must derive from step sources and transition-trace proof source",
);
assert.equal(
  evidence.bindingInventory.compiledStandaloneFamilies,
  nativeV1Families.length,
);
assert.deepEqual(evidence.bindingInventory.legacyPlutusDataFamilies, []);
for (const stalePattern of [
  /7 registered\s+categories/u,
  /5 tooled legacy/u,
  /30 ms\s+maturity/u,
  /zero code/u,
  /nothing binds retention windows/u,
  /Witness-set encoding split in the shipped signature proofs/u,
  /DA hash-preimage proofs/u,
]) {
  for (const path of Object.keys(evidence.documentationAnchors)) {
    const source = await readFile(resolve(repositoryRoot, path), "utf8");
    assert.doesNotMatch(
      source,
      stalePattern,
      `${path} retained stale documentation`,
    );
  }
}

for (const finding of evidence.criticalFindings) {
  assert.ok(
    finding.remainingTasks.length > 0,
    `${finding.id} has no closure task`,
  );
  for (const source of finding.sources) {
    await readFile(resolve(repositoryRoot, source));
  }
}

console.log(
  `canonical V1 fault-proof reconciliation verified: ${allRows.length} rows, ${evidence.openRows.length} open`,
);
