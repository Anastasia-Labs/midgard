import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";

import {
  changedFiles,
  collectChanges,
  EXIT,
  planPreflight,
  resolveBase,
  runPreflight,
  UsageError,
} from "./run.mjs";

// --- temporary repositories -------------------------------------------------

const GIT_ENV = {
  ...process.env,
  GIT_CONFIG_NOSYSTEM: "1",
  GIT_CONFIG_GLOBAL: "/dev/null",
  GIT_AUTHOR_NAME: "test",
  GIT_AUTHOR_EMAIL: "test@example.invalid",
  GIT_COMMITTER_NAME: "test",
  GIT_COMMITTER_EMAIL: "test@example.invalid",
};

const git = (cwd, ...args) => {
  const run = spawnSync("git", args, { cwd, env: GIT_ENV, encoding: "utf8" });
  assert.equal(run.status, 0, `git ${args.join(" ")}: ${run.stderr}`);
  return run.stdout.trim();
};

const write = (repo, files) => {
  for (const [path, text] of Object.entries(files)) {
    mkdirSync(dirname(join(repo, path)), { recursive: true });
    writeFileSync(join(repo, path), text);
  }
};

const pinWorkflows = (version) => ({
  ".github/workflows/aiken-ci.yml": `env:\n  AIKEN_FORK_VERSION: ${version}\n`,
  ".github/workflows/midgard-node-ci.yml": `env:\n  AIKEN_FORK_VERSION: ${version}\n`,
});

const withRepo = async (body) => {
  const repo = mkdtempSync(join(tmpdir(), "preflight-run-test-"));
  try {
    git(repo, "init", "--quiet", "--initial-branch=main");
    write(repo, { "a.txt": "a\n", ...pinWorkflows("aiken v1") });
    git(repo, "add", "--", ".");
    git(repo, "commit", "--quiet", "-m", "base");
    git(repo, "branch", "base");
    return await body(repo);
  } finally {
    rmSync(repo, { recursive: true, force: true });
  }
};

test("strict judges committed changes only; the default adds the working tree", () =>
  withRepo((repo) => {
    write(repo, { "committed.txt": "c\n" });
    git(repo, "add", "--", "committed.txt");
    git(repo, "commit", "--quiet", "-m", "change");
    write(repo, { "a.txt": "edited\n", "untracked.txt": "u\n" });
    const mergeBase = git(repo, "merge-base", "HEAD", "base");
    assert.deepEqual(changedFiles(repo, mergeBase, { strict: true }), [
      "committed.txt",
    ]);
    assert.deepEqual(changedFiles(repo, mergeBase, { strict: false }), [
      "a.txt",
      "committed.txt",
      "untracked.txt",
    ]);
    const strict = collectChanges(repo, { base: "base", strict: true });
    assert.equal(strict.dirty, true);
    assert.deepEqual(strict.fullReasons, []);
  }));

test("a renamed file counts at both paths", () =>
  withRepo((repo) => {
    git(repo, "mv", "a.txt", "b.txt");
    git(repo, "commit", "--quiet", "-m", "rename");
    const { changed } = collectChanges(repo, { base: "base", strict: true });
    assert.deepEqual(changed, ["a.txt", "b.txt"]);
  }));

test("a base that names no commit is a usage error, never an empty diff", () =>
  withRepo((repo) => {
    assert.throws(() => resolveBase(repo, "no-such-ref"), UsageError);
    assert.equal(resolveBase(repo, "base"), "base");
    // No upstream and no checkpoint branch in this repository.
    assert.throws(() => resolveBase(repo, undefined), UsageError);
  }));

test("a moved compiler pin forces a full run; another workflow edit does not", () =>
  withRepo((repo) => {
    write(repo, {
      ".github/workflows/aiken-ci.yml":
        "name: x\nenv:\n  AIKEN_FORK_VERSION: aiken v1\n",
    });
    git(repo, "commit", "--quiet", "-am", "unrelated workflow edit");
    assert.deepEqual(
      collectChanges(repo, { base: "base", strict: true }).fullReasons,
      [],
    );
    write(repo, pinWorkflows("aiken v2"));
    git(repo, "commit", "--quiet", "-am", "move the pin");
    const { fullReasons } = collectChanges(repo, {
      base: "base",
      strict: true,
    });
    assert.equal(fullReasons.length, 1);
    assert.match(fullReasons[0], /aiken v1 -> aiken v2/u);
  }));

// --- running ----------------------------------------------------------------

const check = (id, extra = {}) => ({
  id,
  title: id,
  triggers: ["**"],
  capabilities: [],
  warnOnly: false,
  prePush: false,
  always: false,
  display: `run ${id}`,
  plan: () => [{ argv: ["run", id], cwd: "." }],
  ...extra,
});

const probes = (statuses) => ({
  get: async (name) => ({
    name,
    status: statuses[name] ?? "available",
    detail: `${name} is ${statuses[name] ?? "available"}`,
    fix: `install ${name}`,
    ...(name === "db-prefix"
      ? { env: { MIDGARD_TEST_DATABASE_PREFIX: "ah_test_" } }
      : {}),
  }),
  invalidate: () => {},
});

const run = async (checks, { statuses = {}, outcomes = {}, exists } = {}) => {
  const seen = [];
  const plan = planPreflight({ checks }, ["x"]);
  const report = await runPreflight({
    root: "/nonexistent",
    plan,
    probes: probes(statuses),
    base: "base",
    log: () => {},
    exists: exists ?? (() => true),
    runStep: async (_root, step, env) => {
      seen.push({ id: step.argv[1], env });
      return { status: 0, output: "", ...outcomes[step.argv[1]] };
    },
  });
  return { ...report, seen };
};

const statusOf = (report) =>
  Object.fromEntries(report.results.map((r) => [r.id, r.status]));

test("everything passing exits 0 with the stable result schema", async () => {
  const report = await run([check("one"), check("two")]);
  assert.equal(report.exitCode, EXIT.passed);
  for (const result of report.results) {
    assert.deepEqual(Object.keys(result).sort(), [
      "command",
      "durationMs",
      "id",
      "reason",
      "status",
    ]);
    assert.equal(result.command, `run ${result.id}`);
  }
});

test("a missing capability skips with the probe's reason and exits 3", async () => {
  const report = await run(
    [check("needs-pg", { capabilities: ["postgres"] }), check("plain")],
    { statuses: { postgres: "missing" } },
  );
  assert.deepEqual(statusOf(report), {
    "needs-pg": "skipped",
    plain: "passed",
  });
  assert.match(report.results[0].reason, /postgres missing.*install postgres/u);
  assert.equal(report.exitCode, EXIT.skipped);
  assert.deepEqual(
    report.seen.map((s) => s.id),
    ["plain"],
  );
});

test("an unknown capability is never treated as available", async () => {
  const report = await run([check("c", { capabilities: ["blueprint"] })], {
    statuses: { blueprint: "unknown" },
  });
  assert.equal(statusOf(report).c, "skipped");
  assert.equal(report.exitCode, EXIT.skipped);
});

test("a failure exits 1 even beside skips, and carries its fix", async () => {
  const report = await run(
    [
      check("bad", { fix: "repair it" }),
      check("skip", { capabilities: ["aiken"] }),
    ],
    { statuses: { aiken: "missing" }, outcomes: { bad: { status: 2 } } },
  );
  assert.equal(statusOf(report).bad, "failed");
  assert.equal(report.results[0].fix, "repair it");
  assert.equal(report.exitCode, EXIT.failed);
});

test("a warn-only failure is warned and does not fail the run", async () => {
  const report = await run([check("soft", { warnOnly: true })], {
    outcomes: { soft: { status: 1 } },
  });
  assert.equal(statusOf(report).soft, "warned");
  assert.equal(report.exitCode, EXIT.passed);
});

test("'No test files found' fails the check whatever the exit code says", async () => {
  const report = await run([check("vitest")], {
    outcomes: { vitest: { status: 0, output: "No test files found, exiting" } },
  });
  assert.equal(statusOf(report).vitest, "failed");
  assert.match(report.results[0].reason, /nothing was tested/u);
  assert.equal(report.exitCode, EXIT.failed);
});

test("a command that cannot start is a failure", async () => {
  const report = await run([check("gone")], {
    outcomes: { gone: { status: null, error: new Error("spawn ENOENT") } },
  });
  assert.equal(statusOf(report).gone, "failed");
  assert.match(report.results[0].reason, /could not start/u);
});

test("an absent sibling script is skipped as could-not-check", async () => {
  const report = await run(
    [check("lint", { requiresFiles: ["scripts/ci/lint-workflows.mjs"] })],
    { exists: () => false },
  );
  assert.equal(statusOf(report).lint, "skipped");
  assert.match(report.results[0].reason, /could not check/u);
  assert.equal(report.exitCode, EXIT.skipped);
});

test("a capability's environment reaches the command", async () => {
  const report = await run([check("db", { capabilities: ["db-prefix"] })]);
  assert.equal(report.seen[0].env.MIDGARD_TEST_DATABASE_PREFIX, "ah_test_");
});

test("an input edited without its generated artifact earns a numbered regenerate advisory", () => {
  const channel = check("golden:x", {
    triggers: ["gen.mjs", "out.json"],
    artifacts: ["out.json"],
    fix: "pnpm run x:sync",
  });
  const stale = planPreflight({ checks: [channel] }, ["gen.mjs"]);
  assert.equal(stale.advisories.length, 1);
  assert.equal(stale.advisories[0].id, "regenerate:golden:x");
  assert.match(stale.advisories[0].steps.at(-1), /git add out\.json$/u);
  const fresh = planPreflight({ checks: [channel] }, ["gen.mjs", "out.json"]);
  assert.deepEqual(fresh.advisories, []);
});

test("merge-tree predicts a conflict with the base as a warning", async () =>
  withRepo(async (repo) => {
    git(repo, "checkout", "--quiet", "base");
    write(repo, { "a.txt": "base side\n" });
    git(repo, "commit", "--quiet", "-am", "base edit");
    git(repo, "checkout", "--quiet", "main");
    write(repo, { "a.txt": "branch side\n" });
    git(repo, "commit", "--quiet", "-am", "branch edit");
    const conflicts = {
      ...check("merge-conflicts"),
      internal: "merge-tree",
      always: true,
      warnOnly: true,
      display: "git merge-tree --write-tree HEAD <base>",
      plan: () => [],
    };
    const plan = planPreflight({ checks: [conflicts] }, []);
    const report = await runPreflight({
      root: repo,
      plan,
      probes: probes({}),
      base: "base",
      log: () => {},
    });
    assert.equal(report.results[0].status, "warned");
    assert.match(report.results[0].reason, /would conflict in: a\.txt/u);
    assert.equal(
      report.results[0].command,
      "git merge-tree --write-tree HEAD base",
    );
    assert.equal(report.exitCode, EXIT.passed);
  }));
