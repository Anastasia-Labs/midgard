#!/usr/bin/env node

// Control for the decorated-status classification fixed under issue #529
// (finding V-2/V-12 of #519).
//
// GOAL_PROGRESS.md records *why* a claim holds inside the status cell itself:
// `PASS (structural N/A)`, `PASS (LOCAL_PASS; Q57/QG3 owns LIVE)`. Consumers
// written as `status === "PASS"` classify every one of those rows as non-PASS,
// so the status rules keyed on them quietly stop applying to exactly the rows
// whose decoration records the strongest provenance.
//
// This control is behavioral, not a source review. It reads the authoritative
// first task queue, finds the rows a whole-cell comparison would misclassify,
// and then runs the scheduling helper that used to misclassify them and
// requires its real output to govern them. It fails closed if the ledger ever
// stops containing a decorated PASS row, because a control that silently
// becomes vacuous is the defect class this issue exists to remove.
//
// usage: node demo/scripts/verify-canonical-v1-status-role-control.mjs

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { isPassStatus, statusRole } from "./lib/evidence-status.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");

// Unit level: the exact decorated shapes this repository publishes.
for (const [status, role] of [
  ["PASS", "PASS"],
  ["PASS (structural N/A)", "PASS"],
  ["PASS (LOCAL_PASS; Q57/QG3 owns LIVE)", "PASS"],
  ["PASS (live-verified on preprod)", "PASS"],
  ["`PASS (2026-08-04)`", "PASS"],
  ["PARTIAL (C26 remainder)", "PARTIAL"],
  ["IN_PROGRESS", "IN_PROGRESS"],
  ["TODO", "TODO"],
]) {
  assert.equal(statusRole(status), role, `statusRole misread ${status}`);
  assert.equal(
    isPassStatus(status),
    role === "PASS",
    `isPassStatus misread ${status}`,
  );
}

const ledgerLines = readFileSync(
  resolve(repositoryRoot, "GOAL_PROGRESS.md"),
  "utf8",
).split("\n");
const queueStart = ledgerLines.findIndex((line) =>
  /^##\s+Task queue\s*$/u.test(line),
);
assert.notEqual(queueStart, -1, "GOAL_PROGRESS.md has no Task queue section");
const queueEnd = ledgerLines.findIndex(
  (line, index) => index > queueStart && /^##\s/u.test(line),
);
const queueRows = ledgerLines
  .slice(queueStart, queueEnd === -1 ? ledgerLines.length : queueEnd)
  .filter((line) => line.trimStart().startsWith("|"))
  .map((line) =>
    line
      .trim()
      .replace(/^\|/u, "")
      .replace(/\|$/u, "")
      .split("|")
      .map((cell) => cell.trim()),
  );
const headerRow = queueRows[0] ?? [];
const taskColumn = headerRow.findIndex((cell) => /^task$/iu.test(cell));
const statusColumn = headerRow.findIndex((cell) => /^status$/iu.test(cell));
assert.ok(
  taskColumn !== -1 && statusColumn !== -1,
  "task queue lacks Task/Status columns",
);

const ledgerStatusById = new Map();
for (const row of queueRows.slice(1)) {
  const rawTask = (row[taskColumn] ?? "").replace(/[`*]/gu, "").trim();
  const status = (row[statusColumn] ?? "").replace(/[`*]/gu, "").trim();
  if (rawTask === "" || /^-+$/u.test(rawTask) || status === "") continue;
  for (const id of rawTask.split("/")) {
    const trimmed = id.trim();
    if (trimmed !== "" && !ledgerStatusById.has(trimmed)) {
      ledgerStatusById.set(trimmed, status);
    }
  }
}

// The rows a whole-cell `=== "PASS"` comparison gets wrong today.
const misclassifiedByStringEquality = [...ledgerStatusById]
  .filter(([, status]) => status !== "PASS" && isPassStatus(status))
  .map(([id, status]) => ({ id, status }));
assert.ok(
  misclassifiedByStringEquality.length > 0,
  "no decorated PASS row remains in the task queue: this control can no longer prove anything and must be re-pointed at whatever now carries decorated statuses, not deleted",
);

const manifest = JSON.parse(
  readFileSync(
    resolve(
      repositoryRoot,
      "docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json",
    ),
    "utf8",
  ),
);
const decoratedIds = new Set(misclassifiedByStringEquality.map(({ id }) => id));
const dependentsOfDecorated = (
  Array.isArray(manifest.tasks) ? manifest.tasks : []
).filter((task) =>
  (Array.isArray(task.dependsOn) ? task.dependsOn : []).some((dependency) =>
    decoratedIds.has(dependency),
  ),
);
assert.ok(
  dependentsOfDecorated.length > 0,
  "no manifest task depends on a decorated-PASS row: this control can no longer observe the misclassification and must be re-pointed",
);

// Behavioral: the scheduling helper that consumed these statuses must now
// govern them. Before the fix it reported each decorated row as incomplete and
// every dependent above as blocked on it.
const helper = spawnSync(
  process.execPath,
  [resolve(scriptDirectory, "canonical-v1-goal-tasks-ready.mjs"), "--json"],
  { cwd: repositoryRoot, encoding: "utf8", maxBuffer: 64 * 1024 * 1024 },
);
assert.equal(
  helper.status,
  0,
  `goal:tasks:ready did not run: ${helper.stderr}`,
);
const helperReport = JSON.parse(helper.stdout);
const manifestTasks = Array.isArray(manifest.tasks) ? manifest.tasks : [];
const completeByBaseRole = manifestTasks.filter(({ id }) =>
  isPassStatus(ledgerStatusById.get(id) ?? "UNLISTED"),
).length;
const completeByStringEquality = manifestTasks.filter(
  ({ id }) => (ledgerStatusById.get(id) ?? "UNLISTED") === "PASS",
).length;
assert.ok(
  completeByBaseRole > completeByStringEquality,
  "the ledger no longer contains a decorated PASS row that a whole-cell comparison would drop, so this control proves nothing and must be re-pointed",
);
assert.equal(
  helperReport.summary.complete,
  completeByBaseRole,
  `goal:tasks:ready counts ${String(helperReport.summary.complete)} complete tasks; base-role classification of the same ledger gives ${String(completeByBaseRole)} (whole-cell equality gives ${String(completeByStringEquality)}), so the decorated rows are still ungoverned`,
);

// No decorated-PASS row may be scheduled as outstanding work, and nothing may
// still be reported as blocked on one.
const readyIds = new Set(helperReport.ready.map((row) => row.id));
for (const { id, status } of misclassifiedByStringEquality) {
  assert.ok(
    !readyIds.has(id),
    `${id} (${status}) is still scheduled as outstanding work despite a PASS base role`,
  );
}
const stillBlockedOnDecorated = helperReport.ready
  .filter((row) =>
    row.blockedBy.some((dependency) => decoratedIds.has(dependency)),
  )
  .map((row) => row.id);
assert.deepEqual(
  stillBlockedOnDecorated,
  [],
  `still reported blocked on a decorated-PASS dependency: ${stillBlockedOnDecorated.join(", ")}`,
);

console.log(
  `canonical V1 status-role control: PASS (${String(
    misclassifiedByStringEquality.length,
  )} decorated ledger rows — ${misclassifiedByStringEquality
    .map(({ id }) => id)
    .join(", ")} — are classified by base role and govern ${String(
    dependentsOfDecorated.length,
  )} dependent manifest tasks)`,
);
