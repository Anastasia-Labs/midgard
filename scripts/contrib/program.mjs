import { existsSync } from "node:fs";
import { resolve } from "node:path";

import { inside, json } from "./files.mjs";
import { verifyReceipt } from "./receipts.mjs";

export const validateProgram = (root, path) => {
  const program = json(path);
  if (
    program.schema !== "midgard-work-program/v1" ||
    typeof program.id !== "string" ||
    !Array.isArray(program.tasks) ||
    !program.tasks.length ||
    !Array.isArray(program.decisions)
  )
    throw new Error(
      "program requires schema, id, nonempty tasks and decisions",
    );
  const states = [
    "planned",
    "implemented",
    "reviewed",
    "integrated",
    "published",
    "accepted",
  ];
  const tasks = new Map();
  for (const task of program.tasks) {
    if (
      typeof task.id !== "string" ||
      tasks.has(task.id) ||
      !states.includes(task.state) ||
      !Array.isArray(task.dependsOn) ||
      !Array.isArray(task.paths) ||
      !task.paths.length ||
      !Array.isArray(task.requiredReceipts) ||
      !Array.isArray(task.issues)
    )
      throw new Error(`invalid/duplicate task ${task.id}`);
    for (const file of task.paths) inside(root, file);
    for (const issue of task.issues)
      if (
        !["exact", "related"].includes(issue.relation) ||
        !/^https:\/\//u.test(issue.url)
      )
        throw new Error(`issue relation/url invalid: ${task.id}`);
    if (
      task.state !== "planned" &&
      (!task.owner ||
        !/^[a-f0-9]{40}$/u.test(task.base) ||
        !/^[a-f0-9]{40}$/u.test(task.candidate))
    )
      throw new Error(
        `task ${task.id} needs owner, exact base and candidate commits`,
      );
    if (
      states.indexOf(task.state) >= states.indexOf("reviewed") &&
      !task.review
    )
      throw new Error(`task ${task.id} lacks review evidence`);
    if (
      states.indexOf(task.state) >= states.indexOf("published") &&
      !task.published
    )
      throw new Error(`task ${task.id} lacks publication identity`);
    if (task.state === "accepted") {
      if (!task.acceptance || !task.requiredReceipts.length)
        throw new Error(
          `task ${task.id} lacks explicit acceptance/required receipts`,
        );
      for (const receipt of task.requiredReceipts) {
        const evidence = verifyReceipt(root, resolve(root, receipt));
        if (
          evidence.proofKind !== "live-acceptance" &&
          task.requiresLiveAcceptance
        )
          throw new Error(`task ${task.id} requires live acceptance evidence`);
      }
    }
    tasks.set(task.id, task);
  }
  const active = new Set();
  const visited = new Set();
  const visit = (id) => {
    if (!tasks.has(id)) throw new Error(`unknown dependency ${id}`);
    if (active.has(id)) throw new Error(`cyclic dependency ${id}`);
    if (visited.has(id)) return;
    active.add(id);
    for (const dependency of tasks.get(id).dependsOn) visit(dependency);
    active.delete(id);
    visited.add(id);
  };
  for (const id of tasks.keys()) visit(id);
  const decisions = new Set(program.decisions.map((decision) => decision.id));
  if (decisions.size !== program.decisions.length)
    throw new Error("duplicate decision identity");
  for (const decision of program.decisions) {
    if (
      !decision.id ||
      !decision.path ||
      !existsSync(inside(root, decision.path))
    )
      throw new Error(`decision needs a repository record: ${decision.id}`);
    if (decision.supersededBy && !decisions.has(decision.supersededBy))
      throw new Error(`unknown superseding decision ${decision.supersededBy}`);
  }
  return program;
};

export const renderProgram = (program) =>
  `# ${program.id}\n\n| Task | State | Owner | Dependencies | Required receipts |\n| --- | --- | --- | --- | --- |\n${program.tasks.map((task) => `| ${task.id} | ${task.state} | ${task.owner ?? "unassigned"} | ${task.dependsOn.join(", ")} | ${task.requiredReceipts.join(", ")} |`).join("\n")}\n\nDecisions: ${program.decisions.map((decision) => `${decision.id}: ${decision.path}${decision.supersededBy ? ` (superseded by ${decision.supersededBy})` : ""}`).join(", ") || "none"}\n`;
