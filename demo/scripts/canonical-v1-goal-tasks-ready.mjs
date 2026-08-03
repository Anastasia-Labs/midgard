#!/usr/bin/env node

// GOAL_SPEC §13.1: "F40 additionally provides a non-gating `goal:tasks:ready`
// helper that joins the F05 task manifest with the `GOAL_PROGRESS.md` task
// queue and prints dependency-ready tasks with their owned paths and focused
// commands. It is scheduling tooling, not a gate; no acceptance claim may cite
// it."
//
// Both inputs are read-only here. The task manifest is parent-owned and is
// edited concurrently, so this reads it defensively: a task row may be a
// detailed lease (writable paths, anchors, focused commands) or a bare
// `PENDING_ASSIGNMENT` stub, and unknown extra fields are ignored rather than
// rejected.

import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const manifestPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json",
);
const ledgerPath = resolve(repoRoot, "GOAL_PROGRESS.md");

const args = process.argv.slice(2);
const asJson = args.includes("--json");
const limitArgument = args.find((argument) => argument.startsWith("--limit="));
const limit =
  limitArgument === undefined
    ? null
    : Number.parseInt(limitArgument.slice("--limit=".length), 10);
const unsupported = args.filter(
  (argument) => argument !== "--json" && argument !== limitArgument,
);
if (
  unsupported.length > 0 ||
  (limit !== null && !Number.isSafeInteger(limit))
) {
  process.stderr.write(
    "usage: canonical-v1-goal-tasks-ready.mjs [--json] [--limit=<n>]\n",
  );
  process.exit(2);
}

const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const tasks = Array.isArray(manifest.tasks) ? manifest.tasks : [];
if (tasks.length === 0) {
  process.stderr.write(`${manifestPath} declares no tasks\n`);
  process.exit(1);
}

// The authoritative queue is the first `## Task queue` section: later
// same-named tables in the ledger are merged remote-session checkpoints kept
// for provenance, not the current queue.
const ledgerLines = readFileSync(ledgerPath, "utf8").split("\n");
const queueStart = ledgerLines.findIndex((line) =>
  /^##\s+Task queue\s*$/.test(line),
);
if (queueStart === -1) {
  process.stderr.write(`${ledgerPath} has no "## Task queue" section\n`);
  process.exit(1);
}
const queueEnd = ledgerLines.findIndex(
  (line, index) => index > queueStart && /^##\s/.test(line),
);
const queueRows = ledgerLines
  .slice(queueStart, queueEnd === -1 ? ledgerLines.length : queueEnd)
  .filter((line) => line.trimStart().startsWith("|"))
  .map((line) =>
    line
      .trim()
      .replace(/^\|/, "")
      .replace(/\|$/, "")
      .split("|")
      .map((cell) => cell.trim()),
  );
const headerRow = queueRows[0] ?? [];
const taskColumn = headerRow.findIndex((cell) => /^task$/i.test(cell));
const statusColumn = headerRow.findIndex((cell) => /^status$/i.test(cell));
if (taskColumn === -1 || statusColumn === -1) {
  process.stderr.write(`${ledgerPath} task queue lacks Task/Status columns\n`);
  process.exit(1);
}

const ledgerStatusById = new Map();
for (const row of queueRows.slice(1)) {
  const rawTask = row[taskColumn] ?? "";
  const status = (row[statusColumn] ?? "").replace(/[`*]/g, "").trim();
  if (rawTask === "" || /^-+$/.test(rawTask) || status === "") {
    continue;
  }
  // A single row may close several ids ("C20-6/C20-7"); both are that status.
  for (const id of rawTask.replace(/[`*]/g, "").split("/")) {
    const trimmed = id.trim();
    if (trimmed !== "" && !ledgerStatusById.has(trimmed)) {
      ledgerStatusById.set(trimmed, status);
    }
  }
}

const statusOf = (id) => ledgerStatusById.get(id) ?? "UNLISTED";
const isComplete = (id) => statusOf(id) === "PASS";

const rows = tasks.map((task) => {
  const dependsOn = Array.isArray(task.dependsOn) ? task.dependsOn : [];
  const blockedBy = dependsOn.filter((dependency) => !isComplete(dependency));
  const ledgerStatus = statusOf(task.id);
  return {
    id: task.id,
    title: task.title ?? null,
    section: task.section ?? null,
    size: task.size ?? null,
    risk: task.risk ?? null,
    ledgerStatus,
    detailStatus: task.detailStatus ?? "UNKNOWN",
    dependsOn,
    blockedBy,
    ready: ledgerStatus !== "PASS" && blockedBy.length === 0,
    ownedPaths: Array.isArray(task.writablePaths) ? task.writablePaths : [],
    focusedCommands: Array.isArray(task.focusedCommands)
      ? task.focusedCommands
      : [],
    // §5.1/F05: a PENDING_ASSIGNMENT row must be fully detailed before it is
    // assigned, so the helper reports it as ready-to-detail, not ready-to-run.
    detailingRequired: task.detailStatus === "PENDING_ASSIGNMENT",
  };
});

const ready = rows.filter(({ ready: isReady }) => isReady);
const shown = limit === null ? ready : ready.slice(0, Math.max(limit, 0));
const summary = {
  tasks: rows.length,
  complete: rows.filter(({ ledgerStatus }) => ledgerStatus === "PASS").length,
  ready: ready.length,
  readyAwaitingDetail: ready.filter(
    ({ detailingRequired }) => detailingRequired,
  ).length,
  blocked: rows.filter(
    ({ ready: isReady, ledgerStatus }) => !isReady && ledgerStatus !== "PASS",
  ).length,
  shown: shown.length,
};

if (asJson) {
  process.stdout.write(
    `${JSON.stringify(
      {
        helper: "goal:tasks:ready",
        gating: false,
        note: "scheduling tooling only; no acceptance claim may cite this output (GOAL_SPEC §13.1)",
        summary,
        ready: shown,
      },
      null,
      2,
    )}\n`,
  );
  process.exit(0);
}

const lines = [
  "goal:tasks:ready — non-gating scheduling helper (GOAL_SPEC §13.1).",
  "No acceptance claim may cite this output.",
  `queue: ${String(summary.tasks)} tasks, ${String(summary.complete)} PASS, ${String(summary.ready)} dependency-ready, ${String(summary.blocked)} blocked.`,
  "",
];
for (const row of shown) {
  lines.push(
    `${row.id}  [${row.ledgerStatus}] size=${String(row.size)} risk=${String(row.risk)} detail=${row.detailStatus}`,
  );
  if (row.title !== null) {
    lines.push(`  title: ${row.title}`);
  }
  lines.push(
    `  dependsOn: ${row.dependsOn.length === 0 ? "none" : row.dependsOn.join(", ")}`,
  );
  if (row.detailingRequired) {
    lines.push(
      "  owned paths / focused commands: NOT YET DETAILED — GOAL_SPEC §5.1/F05 requires writable paths, source anchors and focused commands before assignment",
    );
  } else {
    lines.push(
      `  owned paths: ${row.ownedPaths.length === 0 ? "none declared" : row.ownedPaths.join(", ")}`,
    );
    lines.push(
      `  focused commands: ${row.focusedCommands.length === 0 ? "none declared" : row.focusedCommands.join(" ; ")}`,
    );
  }
  lines.push("");
}
if (shown.length !== ready.length) {
  lines.push(
    `(${String(ready.length - shown.length)} further dependency-ready tasks not shown)`,
  );
}
process.stdout.write(`${lines.join("\n")}\n`);
