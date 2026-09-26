// Tests for ci-status.mjs. Every gh call goes to an in-memory fake built from
// fixture JSON; nothing here touches the network.
//
//   node --test .agents/skills/debugging-ci-failures/scripts/ci-status.test.mjs

import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { describe, test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  EXIT,
  failedJobs,
  main,
  matchesPatterns,
  parseWorkflow,
} from "./ci-status.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = resolve(HERE, "../../../..");
const REAL_JOBS = JSON.parse(
  readFileSync(
    join(HERE, "fixtures/node-ci-run-35676544721-jobs.json"),
    "utf8",
  ),
);

const SHA = "a".repeat(40);
const OLD_SHA = "b".repeat(40);

const WORKFLOWS = {
  "aiken-ci.yml": `name: Aiken CI
on:
  push:
    branches: ["main"]
    paths:
      - onchain/aiken/**
  pull_request:
    paths:
      - onchain/aiken/**
jobs: {}
`,
  "midgard-node-ci.yml": `name: Midgard Node CI
on:
  push:
    branches: ["main"]
    paths:
      - "demo/midgard-node/**"
      - "onchain/aiken/**"
  pull_request:
    paths:
      - "demo/midgard-node/**"
      - "onchain/aiken/**"
jobs: {}
`,
  "docs-site-ci.yml": `name: Docs Site CI
on:
  push:
    branches: ["main"]
    paths: ["docs-site/**", "**/*.md"]
  pull_request:
    paths: ["docs-site/**", "**/*.md"]
  workflow_dispatch:
    inputs:
      mode:
        type: choice
        options:
          - fast
          - full
jobs: {}
`,
};

const WORKFLOW_IDS = [
  { id: 1, name: "Aiken CI", path: ".github/workflows/aiken-ci.yml" },
  {
    id: 2,
    name: "Midgard Node CI",
    path: ".github/workflows/midgard-node-ci.yml",
  },
  { id: 3, name: "Docs Site CI", path: ".github/workflows/docs-site-ci.yml" },
];

const pr = (overrides = {}) => ({
  number: 7,
  state: "OPEN",
  headRefName: "feature/x",
  headRefOid: SHA,
  baseRefName: "main",
  mergeable: "MERGEABLE",
  mergeStateStatus: "CLEAN",
  changedFiles: 2,
  url: "https://github.com/o/r/pull/7",
  ...overrides,
});

const runOf = (id, workflowId, name, conclusion, status = "completed") => ({
  databaseId: id,
  workflowDatabaseId: workflowId,
  workflowName: name,
  status,
  conclusion: status === "completed" ? conclusion : "",
  event: "pull_request",
  createdAt: `2026-09-2${String(id % 10)}T00:00:00Z`,
  url: `https://github.com/o/r/actions/runs/${String(id)}`,
});

const baseWorld = (overrides = {}) => ({
  prs: { 7: pr() },
  prList: [],
  prFiles: ["onchain/aiken/lib/a.ak", "demo/midgard-node/src/b.ts"],
  branchSha: SHA,
  commitFiles: [],
  workflows: WORKFLOWS,
  workflowIds: WORKFLOW_IDS,
  runs: [
    runOf(101, 1, "Aiken CI", "success"),
    runOf(102, 2, "Midgard Node CI", "success"),
  ],
  branchRuns: [],
  jobs: {},
  ...overrides,
});

// Read-only verbs this script may use. Anything else is a bug.
const READ_ONLY = new Set([
  "pr view",
  "pr list",
  "run list",
  "run view",
  "workflow list",
  "api",
]);

const fakeGh = (world) => {
  const calls = [];
  const route = (args) => {
    const verb = `${args[0]} ${args[1]}`;
    if (verb === "pr view") return world.prs[args[2]];
    if (verb === "pr list") return world.prList;
    if (verb === "workflow list") return world.workflowIds;
    if (verb === "run list") {
      return args.includes("--commit") ? world.runs : world.branchRuns;
    }
    if (verb === "run view") return world.jobs[args[2]];
    if (args[0] === "api") {
      const path = args.find((arg) => arg.startsWith("repos/"));
      if (/\/pulls\/\d+\/files/u.test(path)) return world.prFiles.join("\n");
      if (/\/branches\//u.test(path)) return world.branchSha;
      if (/\/commits\//u.test(path)) return world.commitFiles.join("\n");
      const file = /\/contents\/\.github\/workflows\/([^?]+)\?ref=/u.exec(path);
      if (file !== null) return world.workflows[file[1]];
      if (/\/contents\/\.github\/workflows\?ref=/u.test(path)) {
        return Object.keys(world.workflows).map((name) => ({
          name,
          path: `.github/workflows/${name}`,
          type: "file",
        }));
      }
    }
    return undefined;
  };
  const run = (args) => {
    calls.push(args);
    if (world.fail !== undefined && world.fail(args)) {
      return { status: 1, stdout: "", stderr: "HTTP 502: Bad Gateway" };
    }
    const answer = route(args);
    if (answer === undefined) {
      return {
        status: 1,
        stdout: "",
        stderr: `unexpected: gh ${args.join(" ")}`,
      };
    }
    return {
      status: 0,
      stdout: typeof answer === "string" ? answer : JSON.stringify(answer),
      stderr: "",
    };
  };
  return { run, calls };
};

const invoke = (world, argv = ["7"]) => {
  const gh = fakeGh(world);
  const out = [];
  const err = [];
  const code = main([...argv, "--repo", "o/r"], {
    run: gh.run,
    out: (line) => out.push(line),
    err: (line) => err.push(line),
  });
  return { code, out: out.join("\n"), err: err.join("\n"), calls: gh.calls };
};

const jsonReport = (world, argv = ["7"]) => {
  const result = invoke(world, [...argv, "--json"]);
  return { ...result, report: JSON.parse(result.out) };
};

const rowFor = (report, name) =>
  report.workflows.find((row) => row.workflow === name);

describe("verdicts", () => {
  test("all expected workflows ran and passed: exit 0; the path-filtered one is not expected", () => {
    const { code, report } = jsonReport(baseWorld());
    assert.equal(code, EXIT.passed);
    assert.equal(rowFor(report, "Docs Site CI").state, "not-triggered");
    assert.match(
      rowFor(report, "Docs Site CI").reason,
      /no changed file matches/u,
    );
  });

  test("negative: a failed run makes it exit 1 and names the step and what it hid", () => {
    const world = baseWorld({
      runs: [
        runOf(101, 1, "Aiken CI", "success"),
        runOf(102, 2, "Midgard Node CI", "failure"),
      ],
      jobs: { 102: REAL_JOBS },
    });
    const { code, out } = invoke(world);
    assert.equal(code, EXIT.failed);
    assert.match(
      out,
      /step 14 "Check canonical V1 profile documentation" failed/u,
    );
    assert.match(out, /28 later steps were skipped and measured nothing/u);
  });

  test("negative: one expected workflow with no run is exit 2, not a pass", () => {
    const world = baseWorld({
      runs: [runOf(102, 2, "Midgard Node CI", "success")],
    });
    const { code, report } = jsonReport(world);
    assert.equal(code, EXIT.noRun);
    assert.equal(rowFor(report, "Aiken CI").state, "missing");
  });

  test("a CONFLICTING pull request with no runs on its head is exit 2 and says why", () => {
    const world = baseWorld({
      prs: { 7: pr({ mergeable: "CONFLICTING", mergeStateStatus: "DIRTY" }) },
      runs: [],
      branchRuns: [
        {
          workflowName: "Midgard Node CI",
          headSha: OLD_SHA,
          createdAt: "2026-09-22T01:39:04Z",
          status: "completed",
          conclusion: "failure",
          event: "pull_request",
        },
      ],
    });
    const { code, out } = invoke(world);
    assert.equal(code, EXIT.noRun);
    assert.match(
      out,
      /pull request is CONFLICTING: GitHub creates no pull_request runs/u,
    );
    assert.match(out, /MISSING\s+Aiken CI/u);
    assert.match(
      out,
      /latest\s+2026-09-22T01:39:04Z Midgard Node CI on bbbbbbbbbb/u,
    );
  });

  test("a diff past the path-filter limit leaves path-filtered workflows undetermined, never passed", () => {
    const world = baseWorld({
      prs: { 7: pr({ changedFiles: 4919 }) },
      runs: [runOf(102, 2, "Midgard Node CI", "success")],
    });
    const { code, report, calls } = jsonReport(world);
    assert.equal(code, EXIT.noRun);
    assert.equal(rowFor(report, "Aiken CI").state, "undetermined");
    assert.match(
      rowFor(report, "Aiken CI").reason,
      /4919 changed files cannot be evaluated/u,
    );
    assert.ok(
      !calls.some((args) => args.some((arg) => /\/pulls\/7\/files/u.test(arg))),
      "does not page through a file list GitHub itself would not read",
    );
  });

  test("runs still in progress with nothing failed or missing: exit 4", () => {
    const world = baseWorld({
      runs: [
        runOf(101, 1, "Aiken CI", "success"),
        runOf(102, 2, "Midgard Node CI", "", "in_progress"),
      ],
    });
    assert.equal(invoke(world).code, EXIT.pending);
  });

  test("a feature branch with no pull request: nothing triggers, exit 2", () => {
    const world = baseWorld({ runs: [] });
    const { code, report } = jsonReport(world, ["feature/x"]);
    assert.equal(code, EXIT.noRun);
    assert.ok(report.workflows.every((row) => row.state === "not-triggered"));
    assert.match(
      rowFor(report, "Aiken CI").reason,
      /branch feature\/x not in filter/u,
    );
  });

  test("main judges push path filters on the head commit's files", () => {
    const world = baseWorld({
      commitFiles: ["onchain/aiken/validators/x.ak"],
      runs: [runOf(101, 1, "Aiken CI", "success")],
    });
    const { code, report } = jsonReport(world, ["main"]);
    assert.equal(
      code,
      EXIT.noRun,
      "node CI matches onchain/aiken/** and did not run",
    );
    assert.equal(rowFor(report, "Midgard Node CI").state, "missing");
    assert.equal(rowFor(report, "Docs Site CI").state, "not-triggered");
  });
});

describe("could not look is not found nothing", () => {
  test("negative: a gh failure is exit 3, distinct from every CI verdict", () => {
    const world = baseWorld({
      fail: (args) => args[0] === "run" && args[1] === "list",
    });
    const { code, err, out } = invoke(world);
    assert.equal(code, EXIT.queryFailed);
    assert.match(err, /could not query GitHub: .*502/u);
    assert.equal(out, "", "prints no verdict it did not earn");
  });

  test("a response that is not JSON is exit 3", () => {
    const gh = fakeGh(baseWorld());
    const original = gh.run;
    const run = (args) =>
      args[0] === "workflow"
        ? { status: 0, stdout: "<html>", stderr: "" }
        : original(args);
    const code = main(["7", "--repo", "o/r"], {
      run,
      out: () => {},
      err: () => {},
    });
    assert.equal(code, EXIT.queryFailed);
  });

  test("a runner that throws (gh not installed) is exit 3", () => {
    const run = () => {
      throw new Error("spawnSync gh ENOENT");
    };
    const code = main(["7"], { run, out: () => {}, err: () => {} });
    assert.equal(code, EXIT.queryFailed);
  });

  test("usage errors are exit 64", () => {
    const quiet = {
      run: () => assert.fail("no gh call on a usage error"),
      out: () => {},
      err: () => {},
    };
    assert.equal(main([], quiet), EXIT.usage);
    assert.equal(main(["7", "8"], quiet), EXIT.usage);
    assert.equal(main(["7", "--repo"], quiet), EXIT.usage);
    assert.equal(main(["7", "--rerun"], quiet), EXIT.usage);
  });

  test("every gh call is a read-only verb", () => {
    const { calls } = invoke(
      baseWorld({
        runs: [
          runOf(101, 1, "Aiken CI", "success"),
          runOf(102, 2, "Midgard Node CI", "failure"),
        ],
        jobs: { 102: REAL_JOBS },
      }),
    );
    for (const args of calls) {
      const verb = args[0] === "api" ? "api" : `${args[0]} ${args[1]}`;
      assert.ok(READ_ONLY.has(verb), `unexpected verb: gh ${args.join(" ")}`);
      assert.ok(
        !args.includes("-X") && !args.includes("--method"),
        `non-GET call: gh ${args.join(" ")}`,
      );
    }
  });
});

describe("trigger parsing and matching", () => {
  test("every workflow in this repository parses without falling back to 'not understood'", () => {
    const dir = join(REPO_ROOT, ".github/workflows");
    const files = readdirSync(dir).filter((name) => /\.ya?ml$/u.test(name));
    assert.ok(files.length > 0);
    for (const name of files) {
      const parsed = parseWorkflow(readFileSync(join(dir, name), "utf8"));
      assert.equal(parsed.error, undefined, `${name}: ${String(parsed.error)}`);
    }
  });

  test("an `on:` block it cannot read is reported undetermined, not skipped", () => {
    const world = baseWorld({
      workflows: {
        ...WORKFLOWS,
        "odd.yml":
          "name: Odd\non:\n  pull_request:\n    paths: *anchor\njobs: {}\n",
      },
    });
    const { code, report } = jsonReport(world);
    assert.equal(code, EXIT.noRun);
    assert.equal(rowFor(report, "Odd").state, "undetermined");
  });

  test("the parser reads inline, flow-list and block forms", () => {
    assert.deepEqual(parseWorkflow("on: push\n").events, { push: {} });
    assert.deepEqual(parseWorkflow("on: [push, pull_request]\n").events, {
      push: {},
      pull_request: {},
    });
    assert.deepEqual(
      parseWorkflow(WORKFLOWS["docs-site-ci.yml"]).events.pull_request,
      {
        paths: ["docs-site/**", "**/*.md"],
      },
    );
    assert.ok(parseWorkflow("name: x\njobs: {}\n").error);
  });

  test("filter globs follow GitHub's rules, including negation", () => {
    assert.ok(matchesPatterns("onchain/aiken/lib/a.ak", ["onchain/aiken/**"]));
    assert.ok(!matchesPatterns("onchain/other/a.ak", ["onchain/aiken/**"]));
    assert.ok(matchesPatterns("README.md", ["**/*.md"]));
    assert.ok(matchesPatterns("docs/agents/a.md", ["**/*.md"]));
    assert.ok(!matchesPatterns("docs/a.mdx", ["**/*.md"]));
    assert.ok(!matchesPatterns("docs/x.md", ["**/*.md", "!docs/**"]));
    assert.ok(matchesPatterns("main", ["main"]));
    assert.ok(!matchesPatterns("mainline", ["main"]));
  });

  test("failedJobs counts only skipped steps after the first failure, not Post steps", () => {
    const [job] = failedJobs(REAL_JOBS);
    assert.deepEqual(job.failingSteps, [
      { number: 14, name: "Check canonical V1 profile documentation" },
    ]);
    const expected = REAL_JOBS.jobs[0].steps.filter(
      (step) =>
        step.number > 14 &&
        step.conclusion === "skipped" &&
        !step.name.startsWith("Post "),
    ).length;
    assert.equal(job.skippedAfterFailure, expected);
    assert.equal(expected, 28);
  });
});
