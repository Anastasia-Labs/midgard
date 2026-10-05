import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";

import { createProbeSet, probeNix } from "./probes.mjs";
import {
  changedFiles,
  collectChanges,
  DEFAULT_BASE,
  EXIT,
  planPreflight,
  resolveBase,
  runPreflight,
  UsageError,
} from "./run.mjs";

// --- temporary repositories -------------------------------------------------

// A hook running these tests exports GIT_DIR and GIT_INDEX_FILE. Inherited
// by the `git init` below, they re-initialise the outer repository as bare
// instead of the temporary one, which breaks every worktree that shares it.
for (const key of Object.keys(process.env))
  if (key.startsWith("GIT_")) delete process.env[key];

test("interrupted preflight stops dispatching later checks and never reports a pass", async () => {
  const controller = new AbortController();
  const checks = ["first", "later"].map((id) => ({
    id,
    title: id,
    triggers: ["x"],
    capabilities: [],
    plan: () => [{ argv: ["node", "probe.mjs"], cwd: "." }],
  }));
  const plan = planPreflight({ checks }, ["x"]);
  const dispatched = [];
  const report = await runPreflight({
    root: ".",
    base: "HEAD",
    plan,
    signal: controller.signal,
    probes: { get: async () => ({ status: "available" }), invalidate() {} },
    log() {},
    runStep: async (_root, step) => {
      dispatched.push(step.argv);
      controller.abort();
      return { status: 0, output: "", durationMs: 1 };
    },
  });
  assert.equal(dispatched.length, 1);
  assert.equal(report.exitCode, EXIT.failed);
  assert.equal(
    report.results.some((entry) => entry.id === "later"),
    false,
  );
});

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

test("the integration target wins over a feature branch tracking its own pushed head", () =>
  withRepo((repo) => {
    git(repo, "update-ref", `refs/remotes/${DEFAULT_BASE}`, "base");
    write(repo, { "feature.txt": "change\n" });
    git(repo, "add", "feature.txt");
    git(repo, "commit", "--quiet", "-m", "feature");
    git(repo, "remote", "add", "origin", "/nonexistent/preflight-fixture");
    git(repo, "update-ref", "refs/remotes/origin/main", "HEAD");
    git(repo, "config", "branch.main.remote", "origin");
    git(repo, "config", "branch.main.merge", "refs/heads/main");
    assert.equal(resolveBase(repo), DEFAULT_BASE);
    assert.deepEqual(
      collectChanges(repo, { base: resolveBase(repo), strict: true }).changed,
      ["feature.txt"],
    );
    git(repo, "update-ref", "-d", `refs/remotes/${DEFAULT_BASE}`);
    assert.throws(() => resolveBase(repo), /no base/u);
  }));

test("selection excludes equivalent already-landed changes without dropping new or dirty work", () =>
  withRepo((repo) => {
    write(repo, { "foundation.txt": "shared\n" });
    git(repo, "add", "foundation.txt");
    git(repo, "commit", "--quiet", "-m", "feature foundation");
    write(repo, { "feature.txt": "new\n" });
    git(repo, "add", "feature.txt");
    git(repo, "commit", "--quiet", "-m", "new work");
    git(repo, "checkout", "--quiet", "-b", "target", "base");
    write(repo, {
      "foundation.txt": "shared\n",
      "target-only.txt": "retain\n",
    });
    git(repo, "add", "foundation.txt", "target-only.txt");
    git(repo, "commit", "--quiet", "-m", "independently landed foundation");
    git(repo, "checkout", "--quiet", "main");
    assert.deepEqual(
      collectChanges(repo, { base: "target", strict: true }).changed,
      ["feature.txt"],
    );
    write(repo, { "a.txt": "dirty\n", "untracked.txt": "new\n" });
    assert.deepEqual(
      collectChanges(repo, { base: "target", strict: false }).changed,
      ["a.txt", "feature.txt", "untracked.txt"],
    );
  }));

test("an old explicit target ancestor is refused rather than selecting landed work", () =>
  withRepo((repo) => {
    write(repo, { "a.txt": "landed\n" });
    git(repo, "commit", "--quiet", "-am", "landed");
    git(repo, "update-ref", `refs/remotes/${DEFAULT_BASE}`, "HEAD");
    assert.throws(() => resolveBase(repo, "base"), /stale.*target/u);
    assert.equal(resolveBase(repo, DEFAULT_BASE), DEFAULT_BASE);
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

test("SDK lane credits only both completed full suites in this unchanged run", async () => {
  const suites = ["@al-ft/lucid-midgard", "@al-ft/midgard-sdk"];
  const checks = [
    check("demo-test", {
      plan: () =>
        suites.map((name) => ({
          argv: ["pnpm", "--filter", name, "test"],
          cwd: "demo",
          sdkSuite: name,
        })),
    }),
    check("tx-preparation:sdk", {
      plan: () => [
        { argv: ["pnpm", "--dir", "demo", "run", "test:tx-prep:sdk"] },
      ],
    }),
  ];
  const dispatched = [];
  const report = await runPreflight({
    root: "/nonexistent",
    base: "base",
    plan: planPreflight({ checks }, ["x"]),
    probes: probes({}),
    log() {},
    sdkContext: () => "frozen",
    runStep: async (_root, step) => {
      dispatched.push(step.argv);
      return {
        status: 0,
        output: "",
        sdkSuite: step.sdkSuite
          ? {
              name: step.sdkSuite,
              identity: "frozen",
              receipt: `receipt-${step.sdkSuite}`,
            }
          : undefined,
      };
    },
  });
  assert.equal(report.exitCode, EXIT.passed);
  assert.equal(
    dispatched.length,
    2,
    "the identical SDK lane must not run again",
  );
  assert.match(report.results.at(-1).reason, /same-run/u);
});

test("SDK dedup refuses missing, failed, skipped, stale or future evidence and other lanes", async () => {
  const names = ["@al-ft/lucid-midgard", "@al-ft/midgard-sdk"];
  for (const scenario of [
    "missing-lucid",
    "missing-sdk",
    "failure",
    "setup-refusal",
    "skip",
    "stale",
    "unknown-context",
    "future",
    "second-run",
    "node",
    "emulator",
    "changed-command",
  ]) {
    const dispatched = [];
    const full = check("demo-test", {
      plan: () =>
        names.map((name) => ({
          argv: ["pnpm", "--filter", name, "test"],
          cwd: "demo",
          sdkSuite: name,
        })),
    });
    const lane = check(
      `tx-preparation:${["node", "emulator"].includes(scenario) ? scenario : "sdk"}`,
      {
        plan: () => [
          {
            argv: [
              "pnpm",
              "--dir",
              "demo",
              "run",
              "test:tx-prep:sdk",
              ...(scenario === "changed-command" ? ["--filter"] : []),
            ],
          },
        ],
      },
    );
    const report = await runPreflight({
      root: "/nonexistent",
      base: "base",
      probes: probes({}),
      log() {},
      plan: planPreflight(
        { checks: scenario === "future" ? [lane, full] : [full, lane] },
        ["x"],
      ),
      sdkContext: () => {
        if (scenario === "unknown-context") throw new Error("missing input");
        return scenario === "stale" ? "changed" : "frozen";
      },
      runStep: async (_root, step) => {
        dispatched.push(step);
        const failed =
          step.sdkSuite === names[1] &&
          ["failure", "setup-refusal", "skip"].includes(scenario);
        const absent =
          scenario === "second-run" ||
          (scenario === "missing-lucid" && step.sdkSuite === names[0]) ||
          (scenario === "missing-sdk" && step.sdkSuite === names[1]);
        return {
          status: failed ? 1 : 0,
          output: "",
          ...(scenario === "setup-refusal" && failed
            ? { error: new Error("setup refused") }
            : {}),
          ...(!absent && step.sdkSuite
            ? {
                sdkSuite: {
                  name: step.sdkSuite,
                  identity: "frozen",
                  receipt: "completed-report",
                },
              }
            : {}),
        };
      },
    });
    assert.equal(
      dispatched.filter((step) => !step.sdkSuite).length,
      1,
      scenario,
    );
    assert.equal(
      report.results.find((result) => result.id.startsWith("tx-preparation:"))
        .evidence,
      undefined,
      scenario,
    );
    if (["failure", "setup-refusal", "skip"].includes(scenario))
      assert.equal(report.exitCode, EXIT.failed, scenario);
  }
});

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

test("preflight check durations remain nonnegative when UTC moves backwards", async () => {
  const original = Date.now;
  let observations = 0;
  Date.now = () => (observations++ === 0 ? 2000 : 1000);
  try {
    const result = await run([check("clock")]);
    assert.ok(
      result.results[0].durationMs >= 0,
      "UTC correction must not reverse elapsed check duration",
    );
  } finally {
    Date.now = original;
  }
});

test("verified equivalent CI records coverage and avoids dispatch; changed commands still run", async () => {
  let dispatched = 0;
  const checks = [check("required-checks-doc"), check("other")];
  const result = await runPreflight({
    root: "/nonexistent",
    base: "base",
    plan: planPreflight({ checks }, ["x"]),
    probes: probes({}),
    log() {},
    ciEvidence: new Map([
      [
        "required-checks-doc",
        {
          command: "run required-checks-doc",
          run: "verified-run",
          coverage: "completed declared validator",
        },
      ],
      ["other", { command: "old command", run: "old-run" }],
    ]),
    runStep: async () => {
      dispatched += 1;
      return { status: 0, output: "" };
    },
  });
  assert.equal(result.exitCode, 0);
  assert.equal(dispatched, 1);
  assert.match(result.results[0].reason, /reused verified CI/u);
  assert.equal(
    result.results[0].evidence.coverage,
    "completed declared validator",
  );
  assert.equal(result.results[1].evidence, undefined);
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

test("Vitest's empty-run line fails the check whatever the exit code says", async () => {
  for (const output of [
    "No test files found, exiting with code 1\n",
    "setup error\n\u001b[31mNo test files found, exiting with code 1\u001b[39m\n",
  ]) {
    const report = await run([check("vitest")], {
      outcomes: { vitest: { status: 0, output } },
    });
    assert.equal(statusOf(report).vitest, "failed");
    assert.match(report.results[0].reason, /nothing was tested/u);
    assert.equal(report.exitCode, EXIT.failed);
  }
  // A runner that only names the phrase (this very test's title) passes.
  const named = await run([check("tooling")], {
    outcomes: {
      tooling: { status: 0, output: "ok 1 - 'No test files found' fails\n" },
    },
  });
  assert.equal(statusOf(named).tooling, "passed");
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

test("docs dependencies cannot be satisfied by the separate demo workspace", async () => {
  const root = mkdtempSync(join(tmpdir(), "preflight-docs-deps-"));
  try {
    write(root, { "demo/node_modules/.modules.yaml": "" });
    const probes = createProbeSet({ root });
    assert.equal((await probes.get("node-modules")).status, "available");
    const missing = await probes.get("docs-site-node-modules");
    assert.equal(missing.status, "missing");
    assert.equal(missing.fix, "pnpm --dir docs-site install --frozen-lockfile");
    write(root, { "docs-site/node_modules/.modules.yaml": "" });
    probes.invalidate(["docs-site-node-modules"]);
    assert.equal(
      (await probes.get("docs-site-node-modules")).status,
      "available",
    );
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});

test("the spec probe distinguishes absent Nix from a probe that could not run", () => {
  for (const [outcome, expected] of [
    [{ status: 0, stdout: "nix (Nix) 2.24.0\n" }, "available"],
    [
      { status: null, error: { code: "ENOENT", message: "not found" } },
      "missing",
    ],
    [{ status: null, error: { code: "EPERM", message: "denied" } }, "unknown"],
    [
      { status: null, error: { code: "ETIMEDOUT", message: "timed out" } },
      "unknown",
    ],
    [{ status: 1, stderr: "broken installation" }, "unknown"],
  ]) {
    const probe = probeNix({
      root: "/tmp",
      run: (command, argv, options) => {
        assert.equal(command, "nix");
        assert.deepEqual(argv, ["--version"]);
        assert.equal(options.cwd, "/tmp");
        assert.ok(options.timeout > 0);
        return outcome;
      },
    });
    assert.equal(probe.status, expected);
    if (expected !== "available") assert.match(probe.fix, /make spec/);
  }
});
