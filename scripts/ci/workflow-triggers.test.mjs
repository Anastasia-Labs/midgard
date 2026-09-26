// Which CI jobs run for which change, written down by hand and checked against
// the workflows. A path filter that silently stops matching, a branch filter
// that never fires, or a new job nobody decided about all change a row below
// and fail this test, so "CI passed" keeps meaning "these jobs ran".
//
// The evaluator handles exactly what the workflows use: `on:` as a string,
// list or map; `branches`, `paths` and `paths-ignore` globs (with `!`
// negation); and job `if:`s of `!cancelled()` / `always()`. Anything else
// fails loudly instead of being guessed at — there is no expression evaluator.

import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const workflowsDir = join(repositoryRoot, ".github/workflows");

const loadYaml = () => {
  try {
    return createRequire(join(repositoryRoot, "demo/package.json"))("yaml");
  } catch {
    return undefined;
  }
};
const yaml = loadYaml();
const skip =
  yaml === undefined && process.env.GITHUB_ACTIONS !== "true"
    ? "could not check: yaml absent (run `pnpm --dir demo install`)"
    : false;

const workingBranch = "colll78/canonical-v1-watcher-l1-source-checkpoint";
const everyWorkflowFile = readdirSync(workflowsDir)
  .filter((name) => /\.ya?ml$/u.test(name))
  .map((name) => `.github/workflows/${name}`);

// Each scenario: the event, and the jobs ("workflow-file:job") that must run.
const scenarios = {
  "push to the working branch": {
    event: "push",
    branch: workingBranch,
    files: ["demo/midgard-node/src/index.ts", "onchain/aiken/lib/x.ak"],
    runs: [],
  },
  "docs-only pull request": {
    event: "pull_request",
    branch: "main",
    files: ["docs/exec-plans/agent-contribution-hardening.md"],
    runs: [
      "docs-site-ci:build",
      "repo-tools-ci:repo-tools",
      "agent-skills-ci:skills",
    ],
  },
  "onchain/** pull request": {
    event: "pull_request",
    branch: "main",
    files: ["onchain/aiken/validators/state_queue.ak"],
    runs: [
      "aiken-ci:deployment-profiles",
      "aiken-ci:guard-self-tests",
      "aiken-ci:aiken",
      "aiken-ci:gate",
      ...[
        "aiken-fork",
        "blueprint",
        "compose-render",
        "node-image",
        "golden-channels",
        "lucid-midgard",
        "midgard-core",
        "midgard-sdk",
        "midgard-validation",
        "midgard-fault-proofs",
        "midgard-watcher",
        "native-mpf",
        "lint",
        "da-committee-node",
        "midgard-node",
        "midgard-node-tools",
        "da-e2e",
        "gate",
      ].map((job) => `midgard-node-ci:${job}`),
      "repo-tools-ci:repo-tools",
      "agent-skills-ci:skills",
    ],
  },
  "demo/** pull request": {
    event: "pull_request",
    branch: "main",
    files: ["demo/midgard-node/src/index.ts"],
    runs: [
      "midgard-node-ci:*",
      "repo-tools-ci:repo-tools",
      "agent-skills-ci:skills",
    ],
  },
  "watcher pull request": {
    event: "pull_request",
    branch: "main",
    files: ["demo/midgard-watcher/src/index.ts"],
    runs: [
      "midgard-node-ci:*",
      "midgard-watcher-ci:watcher",
      "repo-tools-ci:repo-tools",
      "agent-skills-ci:skills",
    ],
  },
  ".github/workflows/** pull request": {
    event: "pull_request",
    branch: "main",
    files: everyWorkflowFile,
    runs: [
      "aiken-ci:*",
      "docs-site-ci:build",
      "latex-ci:generate_pdf",
      "midgard-node-ci:*",
      "midgard-watcher-ci:watcher",
      "repo-tools-ci:repo-tools",
      "agent-skills-ci:skills",
    ],
  },
  "push to main touching the specification": {
    event: "push",
    branch: "main",
    files: ["technical-spec/midgard.tex"],
    runs: [
      "latex-ci:generate_pdf",
      "pages-deploy:deploy",
      "repo-tools-ci:repo-tools",
      "agent-skills-ci:skills",
    ],
  },
};

// GitHub filter glob: `**` crosses `/` (and `**/` may match no directory at
// all), `*` and `?` do not.
const globToRegExp = (glob) =>
  new RegExp(
    `^${glob
      .split(/(\*\*\/|\*\*|\*|\?)/u)
      .map((part) =>
        part === "**/"
          ? "(?:.*/)?"
          : part === "**"
            ? ".*"
            : part === "*"
              ? "[^/]*"
              : part === "?"
                ? "[^/]"
                : part.replace(/[.+^${}()|[\]\\]/gu, "\\$&"),
      )
      .join("")}$`,
    "u",
  );
const matchesFilter = (patterns, value) => {
  let matched = false;
  for (const pattern of patterns) {
    if (pattern.startsWith("!")) {
      if (globToRegExp(pattern.slice(1)).test(value)) matched = false;
    } else if (globToRegExp(pattern).test(value)) matched = true;
  }
  return matched;
};

const understoodFilters = new Set(["branches", "paths", "paths-ignore"]);
const triggerFor = (on, event) => {
  if (typeof on === "string") return on === event ? {} : undefined;
  if (Array.isArray(on)) return on.includes(event) ? {} : undefined;
  if (!(event in on)) return undefined;
  const filters = on[event] ?? {};
  for (const key of Object.keys(filters)) {
    if (!understoodFilters.has(key)) {
      throw new Error(`unsupported ${event} filter '${key}'; teach this test`);
    }
  }
  return filters;
};

const workflowRuns = (on, scenario) => {
  const filters = triggerFor(on, scenario.event);
  if (filters === undefined) return false;
  if (filters.branches && !matchesFilter(filters.branches, scenario.branch)) {
    return false;
  }
  if (filters.paths && filters["paths-ignore"]) {
    throw new Error("paths and paths-ignore together are invalid");
  }
  if (filters.paths) {
    return scenario.files.some((file) => matchesFilter(filters.paths, file));
  }
  if (filters["paths-ignore"]) {
    return scenario.files.some(
      (file) => !matchesFilter(filters["paths-ignore"], file),
    );
  }
  return true;
};

const alwaysTrue =
  /^\$\{\{\s*(!\s*cancelled\(\)|always\(\))\s*\}\}$|^(!\s*cancelled\(\)|always\(\))$/u;
const jobRuns = (file, name, job) => {
  if (job.if === undefined) return true;
  if (alwaysTrue.test(String(job.if).trim())) return true;
  throw new Error(
    `${file}:${name} has 'if: ${String(job.if)}', which this test cannot evaluate; teach it`,
  );
};

const loadWorkflows = () =>
  everyWorkflowFile.map((path) => {
    const document = yaml.parse(
      readFileSync(join(repositoryRoot, path), "utf8"),
    );
    const id = path
      .replace(/^\.github\/workflows\//u, "")
      .replace(/\.ya?ml$/u, "");
    return { id, path, on: document.on, jobs: document.jobs ?? {} };
  });

const expand = (workflows, runs) =>
  runs
    .flatMap((entry) => {
      const [id, job] = entry.split(":");
      if (job !== "*") return [entry];
      const workflow = workflows.find((candidate) => candidate.id === id);
      assert.ok(workflow, `no workflow '${id}'`);
      return Object.keys(workflow.jobs).map((name) => `${id}:${name}`);
    })
    .sort();

const running = (workflows, scenario) =>
  workflows
    .filter((workflow) => workflowRuns(workflow.on, scenario))
    .flatMap((workflow) =>
      Object.entries(workflow.jobs)
        .filter(([name, job]) => jobRuns(workflow.path, name, job))
        .map(([name]) => `${workflow.id}:${name}`),
    )
    .sort();

for (const [title, scenario] of Object.entries(scenarios)) {
  test(`jobs that run: ${title}`, { skip }, () => {
    const workflows = loadWorkflows();
    assert.deepEqual(
      running(workflows, scenario),
      expand(workflows, scenario.runs),
    );
  });
}

test(
  "every job of every workflow is decided by some row of the table",
  { skip },
  () => {
    const workflows = loadWorkflows();
    const covered = new Set(
      Object.values(scenarios).flatMap((scenario) =>
        expand(workflows, scenario.runs),
      ),
    );
    const missing = workflows
      .flatMap((workflow) =>
        Object.keys(workflow.jobs).map((name) => `${workflow.id}:${name}`),
      )
      .filter((job) => !covered.has(job));
    assert.deepEqual(
      missing,
      [],
      "jobs no scenario runs; add them to the table",
    );
  },
);

test("the evaluator fails rather than guesses", { skip }, () => {
  assert.throws(
    () =>
      workflowRuns(
        { push: { tags: ["v*"] } },
        scenarios["push to the working branch"],
      ),
    /unsupported/u,
  );
  assert.throws(
    () => jobRuns("w.yml", "j", { if: "github.event_name == 'push'" }),
    /cannot evaluate/u,
  );
  assert.equal(
    matchesFilter(["demo/**", "!demo/docs/**"], "demo/docs/a.md"),
    false,
  );
  assert.equal(matchesFilter(["*.md"], "docs/a.md"), false);
  assert.equal(matchesFilter(["**/*.md"], "a.md"), true);
  assert.equal(matchesFilter(["**/*.md"], "docs/spec/a.md"), true);
});
