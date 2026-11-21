import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { after, before, test } from "node:test";
import { fileURLToPath } from "node:url";

import { JSON_SCHEMA, main, parseArguments } from "../preflight.mjs";
import { buildRegistry } from "./registry.mjs";
import { DEFAULT_BASE } from "./run.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// CLI presentation tests need a fixed Git topology, independent of the live
// checkpoint advancing beyond this checkout. The fixture's target is one
// commit ahead of HEAD, so HEAD is deliberately stale and the target is valid.
let cliRoot;
const fixtureGit = (cwd, args) => {
  const env = Object.fromEntries(
    Object.entries(process.env).filter(([key]) => !key.startsWith("GIT_")),
  );
  const run = spawnSync("git", args, {
    cwd,
    env: {
      ...env,
      GIT_CONFIG_NOSYSTEM: "1",
      GIT_CONFIG_GLOBAL: "/dev/null",
      GIT_AUTHOR_NAME: "test",
      GIT_AUTHOR_EMAIL: "test@example.invalid",
      GIT_COMMITTER_NAME: "test",
      GIT_COMMITTER_EMAIL: "test@example.invalid",
    },
    encoding: "utf8",
  });
  assert.equal(run.status, 0, `git ${args.join(" ")}: ${run.stderr}`);
  return run.stdout.trim();
};
before(() => {
  cliRoot = mkdtempSync(join(tmpdir(), "preflight-cli-test-"));
  fixtureGit(root, [
    "clone",
    "--quiet",
    "--shared",
    "--no-checkout",
    "--origin",
    "fixture",
    root,
    cliRoot,
  ]);
  fixtureGit(cliRoot, [
    "-c",
    "core.hooksPath=/dev/null",
    "checkout",
    "--quiet",
    "--detach",
    "HEAD",
  ]);
  const head = fixtureGit(cliRoot, ["rev-parse", "HEAD"]);
  fixtureGit(cliRoot, [
    "-c",
    "core.hooksPath=/dev/null",
    "commit",
    "--quiet",
    "--allow-empty",
    "-m",
    "target moved",
  ]);
  fixtureGit(cliRoot, ["update-ref", `refs/remotes/${DEFAULT_BASE}`, "HEAD"]);
  fixtureGit(cliRoot, [
    "-c",
    "core.hooksPath=/dev/null",
    "checkout",
    "--quiet",
    "--detach",
    head,
  ]);
});
after(() => {
  if (cliRoot !== undefined) rmSync(cliRoot, { recursive: true, force: true });
});

const capture = () => {
  const out = { stdout: "", stderr: "" };
  return {
    out,
    stdout: (text) => {
      out.stdout += text;
    },
    stderr: (text) => {
      out.stderr += text;
    },
  };
};

const allAvailable = {
  get: async (name) => ({ name, status: "available", detail: name }),
  invalidate: () => {},
};

test("explicit full-local mode keeps path selection; --full also widens scope", () => {
  assert.equal(parseArguments(["--full-local"]).fullLocal, true);
  assert.equal(parseArguments(["--full-local"]).full, false);
  assert.equal(parseArguments(["--full"]).full, true);
  assert.equal(parseArguments([]).fullLocal, false);
});

test("usage errors exit 2", async () => {
  for (const argv of [
    ["--bogus"],
    ["--base"],
    ["--base", "--json"],
    ["--ci-run"],
    ["--ci-run", "--json"],
    ["--strict", "--base", "refs/heads/no-such-branch-for-preflight"],
    ["--run"],
    ["--run", "no-such-check"],
    ["--run", "merge-conflicts"],
    ["--run", "registry-paths", "--run", "registry-paths"],
    ["--run", "registry-paths", "--strict"],
    ["--run", "registry-paths", "--json"],
    ["--full", "--run", "registry-paths"],
  ]) {
    const io = capture();
    assert.equal(await main(argv, { root, ...io }), 2, argv.join(" "));
    assert.equal(io.out.stdout, "", argv.join(" "));
  }
});

test("a stale target ancestor is refused before any check runs", async () => {
  const io = capture();
  let ran = 0;
  const exitCode = await main(["--strict", "--base", "HEAD", "--json"], {
    root: cliRoot,
    env: {},
    ...io,
    probes: allAvailable,
    runStep: async () => {
      ran += 1;
      return { status: 0, output: "" };
    },
  });
  assert.equal(exitCode, 2);
  assert.equal(ran, 0);
  assert.equal(io.out.stdout, "");
});

test("--json prints one document with the stable schema and nothing else", async () => {
  const io = capture();
  const exitCode = await main(["--strict", "--base", DEFAULT_BASE, "--json"], {
    root: cliRoot,
    env: {},
    ...io,
    probes: allAvailable,
    runStep: async () => ({ status: 0, output: "" }),
  });
  const report = JSON.parse(io.out.stdout);
  assert.equal(report.schema, JSON_SCHEMA);
  assert.equal(report.strict, true);
  assert.deepEqual(report.changedFiles, []);
  assert.equal(report.exitCode, exitCode);
  assert.equal(exitCode, 0);
  for (const check of report.checks) {
    assert.deepEqual(Object.keys(check).sort(), [
      "command",
      "durationMs",
      "id",
      "reason",
      "status",
    ]);
    assert.ok(["passed", "failed", "skipped", "warned"].includes(check.status));
  }
  assert.match(io.out.stderr, /Selection is not the final gate/u);
});

test("--list runs nothing and --json reports the plan separately", async () => {
  const io = capture();
  let ran = 0;
  const exitCode = await main(
    ["--strict", "--base", DEFAULT_BASE, "--list", "--json", "--full"],
    {
      root: cliRoot,
      env: {},
      ...io,
      probes: allAvailable,
      runStep: async () => {
        ran += 1;
        return { status: 0, output: "" };
      },
    },
  );
  assert.equal(exitCode, 0);
  assert.equal(ran, 0);
  const report = JSON.parse(io.out.stdout);
  assert.deepEqual(report.checks, []);
  assert.equal(report.full, true);
  assert.deepEqual(report.fullReasons, ["--full"]);
  assert.equal(report.planned.length, buildRegistry(cliRoot).checks.length);
});

test("the kill switch forces a full run and a failed check exits 1 with its fix", async () => {
  const io = capture();
  const exitCode = await main(["--strict", "--base", DEFAULT_BASE], {
    root: cliRoot,
    env: { MIDGARD_PREFLIGHT_FULL: "1" },
    ...io,
    probes: allAvailable,
    runStep: async (_root, step) => ({
      status: step.argv.includes("scripts/contrib/enroll-builds.mjs") ? 1 : 0,
      output: "",
    }),
  });
  assert.equal(exitCode, 1);
  assert.match(io.out.stdout, /full run: MIDGARD_PREFLIGHT_FULL=1/u);
  assert.match(io.out.stdout, /FAILED +contributor-build-guards/u);
  assert.match(
    io.out.stdout,
    /fix: node scripts\/contrib\/enroll-builds\.mjs --write/u,
  );
  assert.match(io.out.stdout, /preflight FAILED/u);
});

test("--run executes exactly the named checks at full scope, in registry order", async () => {
  const registry = buildRegistry(root);
  const expected = ["registry-paths", "toolchain-pins"].map((id) =>
    registry.checks
      .find((check) => check.id === id)
      .plan({ matched: [], full: true, advise: () => {} })
      .map((step) => step.argv),
  );
  const io = capture();
  const ran = [];
  const exitCode = await main(
    ["--run", "toolchain-pins", "--run", "registry-paths"],
    {
      root,
      env: {},
      ...io,
      probes: allAvailable,
      runStep: async (_root, step) => {
        ran.push(step.argv);
        return { status: 0, output: "" };
      },
    },
  );
  assert.equal(exitCode, 0);
  assert.deepEqual(ran, expected.flat());
  assert.match(io.out.stdout, /PASSED +registry-paths/u);
  assert.match(io.out.stdout, /PASSED +toolchain-pins/u);
  assert.doesNotMatch(io.out.stdout, /merge-conflicts/u);
  assert.match(io.out.stdout, /preflight passed/u);
});

test("--run fails a check that is warn-only locally and fails closed on a missing capability", async () => {
  const warnOnly = buildRegistry(root).checks.find(
    (check) => check.warnOnly && check.internal === undefined,
  );
  assert.ok(warnOnly, "the registry has a warn-only check to name");
  const failed = capture();
  assert.equal(
    await main(["--run", warnOnly.id], {
      root,
      env: {},
      ...failed,
      probes: allAvailable,
      runStep: async () => ({ status: 1, output: "" }),
    }),
    1,
  );
  assert.match(failed.out.stdout, new RegExp(`FAILED +${warnOnly.id}`, "u"));
  assert.match(failed.out.stdout, /preflight FAILED/u);

  const needs = buildRegistry(root).checks.find(
    (check) => check.id === "demo-lint",
  );
  assert.ok(needs.capabilities.includes("node-modules"));
  const skipped = capture();
  let ran = 0;
  assert.equal(
    await main(["--run", "demo-lint"], {
      root,
      env: {},
      ...skipped,
      probes: {
        get: async (name) => ({
          name,
          status: "missing",
          detail: `${name} absent`,
        }),
        invalidate: () => {},
      },
      runStep: async () => {
        ran += 1;
        return { status: 0, output: "" };
      },
    }),
    3,
  );
  assert.equal(ran, 0);
  assert.match(skipped.out.stdout, /SKIPPED +demo-lint/u);
});

test("--run --list prints the named check's command and runs nothing", async () => {
  const io = capture();
  let ran = 0;
  assert.equal(
    await main(["--run", "registry-paths", "--list"], {
      root,
      env: {},
      ...io,
      probes: allAvailable,
      runStep: async () => {
        ran += 1;
        return { status: 0, output: "" };
      },
    }),
    0,
  );
  assert.equal(ran, 0);
  assert.match(
    io.out.stdout,
    /would run registry-paths\n {4}\$ node scripts\/ci\/check-registry-paths\.mjs\n/u,
  );
});
