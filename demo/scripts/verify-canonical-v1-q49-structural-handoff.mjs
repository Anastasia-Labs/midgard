#!/usr/bin/env node

// Verifies the Q49 structural handoff artifact against the final tree: every
// F21 physical structural row (coverage-matrix.md L295-L303) must name its exact
// current matrix concern text and at least one EXECUTABLE piece of evidence that
// really exists in the tree. GOAL_SPEC.md 9.1 closing rule forbids prose-only
// closure of a structurally enforced rule, so a row with no resolvable selector,
// test title or absent-constructor check fails this gate.
//
// usage: node demo/scripts/verify-canonical-v1-q49-structural-handoff.mjs [--json]

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

const emitJson = process.argv.slice(2).includes("--json");
const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repositoryRoot = resolve(demoRoot, "..");

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

const checkedRows = [];
let executableChecks = 0;

for (const row of evidence.rows) {
  assert.equal(
    firstCell(matrixLines[row.line - 1]),
    row.concern,
    `matrix concern text drifted at line ${String(row.line)}`,
  );
  assert.equal(
    row.disposition,
    "PASS",
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

  let rowChecks = 0;
  for (const item of row.executableEvidence) {
    if (item.kind === "aiken") {
      const source = await loadFile(item.module);
      assert.ok(
        Array.isArray(item.selectors) && item.selectors.length > 0,
        `row ${String(row.line)} cites ${item.module} with no selectors`,
      );
      for (const selector of item.selectors) {
        assert.ok(
          /^[a-z0-9_]+$/u.test(selector),
          `selector ${selector} is not a focused-check-safe name`,
        );
        assert.ok(
          source.includes(`\ntest ${selector}(`),
          `missing Aiken selector ${selector} in ${item.module}`,
        );
        rowChecks += 1;
      }
    } else if (item.kind === "vitest") {
      const source = await loadFile(item.file);
      for (const title of item.titles) {
        assert.ok(
          source.includes(`it("${title}"`),
          `missing vitest title "${title}" in ${item.file}`,
        );
        rowChecks += 1;
      }
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
      rowChecks += 1;
    } else {
      throw new Error(
        `unknown executable evidence kind ${String(item.kind)} at row ${String(row.line)}`,
      );
    }
  }

  assert.ok(
    rowChecks > 0,
    `structural row ${String(row.line)} resolved zero executable checks`,
  );
  executableChecks += rowChecks;
  checkedRows.push({ line: row.line, checks: rowChecks });
}

const dispositions = evidence.rows.map((row) => row.disposition);
assert.deepEqual(evidence.summary, {
  rows: evidence.rows.length,
  pass: dispositions.filter((value) => value === "PASS").length,
  partial: dispositions.filter((value) => value === "PARTIAL").length,
  open: dispositions.filter((value) => value === "OPEN").length,
});
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

const report = {
  status: "PASS",
  structuralRows: evidence.rows.length,
  partial: evidence.summary.partial,
  open: evidence.summary.open,
  executableChecks,
  rows: checkedRows,
  closedHere: closedHere.map((row) => ({ line: row.line, task: row.closedBy })),
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `Q49 structural handoff: PASS (${String(evidence.rows.length)} rows, ${String(executableChecks)} executable checks, 0 partial, 0 open)`,
  );
}
