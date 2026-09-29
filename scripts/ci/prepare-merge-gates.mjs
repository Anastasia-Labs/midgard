#!/usr/bin/env node
// Prepare a disabled policy and its prerequisite trigger changes. Never calls GitHub.
import { createHash } from "node:crypto";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const yaml = createRequire(join(root, "demo/package.json"))("yaml");
export const gates = [
  ["repo-tools-ci.yml", "repo-tools"],
  ["agent-skills-ci.yml", "skills"],
  ["aiken-ci.yml", "gate"],
  ["midgard-node-ci.yml", "gate"],
  ["midgard-watcher-ci.yml", "watcher"],
];

export function prepareMergeGates(
  branches,
  readWorkflow = (file) =>
    readFileSync(join(root, ".github/workflows", file), "utf8"),
) {
  if (!branches.length || new Set(branches).size !== branches.length) {
    throw new Error(
      "Specify at least one distinct --branch; branch targets require an explicit choice.",
    );
  }
  for (const branch of branches) {
    const check = spawnSync("git", [
      "check-ref-format",
      `refs/heads/${branch}`,
    ]);
    if (
      check.status !== 0 ||
      branch.startsWith("refs/") ||
      branch.includes("*")
    ) {
      throw new Error(`Invalid literal branch name: ${branch}`);
    }
  }
  const changes = [],
    contexts = [];
  for (const [file, id] of gates) {
    const before = readWorkflow(file),
      doc = yaml.parseDocument(before);
    if (doc.errors.length) throw new Error(`${file}: invalid YAML`);
    const data = doc.toJS(),
      job = data.jobs?.[id];
    if (!job || !Object.hasOwn(data.on ?? {}, "pull_request")) {
      throw new Error(
        `${file}: missing ${id} job or pull_request trigger; update the proposal.`,
      );
    }
    if (
      job.strategy ||
      (Object.hasOwn(job, "if") && job.if !== "${{ !cancelled() }}")
    ) {
      throw new Error(
        `${file}: selected job can be skipped or matrix-expanded; review its required-check contract.`,
      );
    }
    const context = job.name ?? id;
    if (
      typeof context !== "string" ||
      context.includes("${{") ||
      contexts.includes(context)
    ) {
      throw new Error(
        `${file}: required check names must be static and unique.`,
      );
    }
    contexts.push(context);
    if (
      data.on.pull_request !== null &&
      Object.keys(data.on.pull_request).length
    ) {
      doc.setIn(["on", "pull_request"], null);
      changes.push({
        path: `.github/workflows/${file}`,
        before,
        after: doc.toString(),
        sourceSha256: createHash("sha256").update(before).digest("hex"),
      });
    }
  }
  return {
    ruleset: {
      name: "Reviewed contributions with required CI",
      target: "branch",
      enforcement: "disabled",
      bypass_actors: [],
      conditions: {
        ref_name: {
          include: branches.map((b) => `refs/heads/${b}`),
          exclude: [],
        },
      },
      rules: [
        { type: "deletion" },
        { type: "non_fast_forward" },
        {
          type: "pull_request",
          parameters: {
            dismiss_stale_reviews_on_push: true,
            require_code_owner_review: false,
            require_last_push_approval: true,
            required_approving_review_count: 1,
            required_review_thread_resolution: true,
          },
        },
        {
          type: "required_status_checks",
          parameters: {
            strict_required_status_checks_policy: true,
            required_status_checks: contexts.map((context) => ({ context })),
          },
        },
      ],
    },
    changes,
  };
}

export function workflowPatch(changes) {
  const scratch = mkdtempSync(join(tmpdir(), "midgard-merge-gates-"));
  try {
    return changes
      .map(({ path, before, after }) => {
        const left = join(scratch, "before"),
          right = join(scratch, "after");
        writeFileSync(left, before);
        writeFileSync(right, after);
        const result = spawnSync(
          "diff",
          ["-u", "--label", `a/${path}`, "--label", `b/${path}`, left, right],
          { encoding: "utf8" },
        );
        if (result.error || ![0, 1].includes(result.status))
          throw new Error(result.error?.message ?? result.stderr);
        return result.stdout;
      })
      .join("");
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
}

export function main(args) {
  const branches = [];
  let format = "plan";
  while (args.length) {
    const arg = args.shift();
    if (arg === "--branch" && args[0]) branches.push(args.shift());
    else if (arg === "--format" && args[0]) format = args.shift();
    else
      throw new Error(
        "Usage: prepare-merge-gates.mjs --branch NAME [--branch NAME] [--format plan|ruleset|patch]",
      );
  }
  if (!["plan", "ruleset", "patch"].includes(format))
    throw new Error("Unknown format; choose plan, ruleset or patch.");
  const proposal = prepareMergeGates(branches);
  if (format === "patch") return workflowPatch(proposal.changes);
  if (format === "ruleset")
    return JSON.stringify(proposal.ruleset, null, 2) + "\n";
  return (
    JSON.stringify(
      {
        ruleset: proposal.ruleset,
        workflowChanges: proposal.changes.map(({ path, sourceSha256 }) => ({
          path,
          sourceSha256,
        })),
        status:
          "proposal-only; confirm branches, check provenance, workflow tests and current PR CI before activation",
      },
      null,
      2,
    ) + "\n"
  );
}

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  try {
    process.stdout.write(main(process.argv.slice(2)));
  } catch (error) {
    console.error(`Could not prepare merge gates: ${error.message}`);
    process.exitCode = 1;
  }
}
