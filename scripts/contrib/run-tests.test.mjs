import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdirSync, readFileSync, realpathSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import test from "node:test";

import { HELP, parse } from "../contrib.mjs";
import { packageNeedsPostgres } from "../preflight/derive.mjs";
import { atomicJson, workspacePackages } from "./files.mjs";
import { fixture } from "./fixture.test-support.mjs";
import { runTests, testCommand } from "./tests.mjs";
import {
  failures,
  parseTestScript,
  summaryLines,
  vitestFlags,
  withCallerFlags,
} from "./vitest-command.mjs";

const repository = resolve(import.meta.dirname, "../..");

test("every workspace test script reads as one Vitest command, or says it has none", () => {
  for (const pkg of workspacePackages(repository).filter(
    (pkg) => pkg.scripts?.test,
  )) {
    if (!/\bvitest\b/u.test(pkg.scripts.test)) {
      assert.throws(() => testCommand(pkg), /has no Vitest suites/u);
      continue;
    }
    const command = testCommand(pkg);
    assert.ok(command, pkg.name);
  }
});

test("every package whose suites read the test Postgres is known to need it", () => {
  // A hand-kept list once left the watcher out, so its runs skipped the
  // Postgres probe and preflight ran it among the database-free suites.
  const needs = Object.fromEntries(
    workspacePackages(repository).map((pkg) => [
      pkg.name,
      packageNeedsPostgres(repository, pkg),
    ]),
  );
  for (const name of [
    "midgard-node",
    "midgard-node-tools",
    "midgard-watcher",
    "da-committee-node",
    "@al-ft/midgard-l1-follower",
  ])
    assert.equal(needs[name], true, name);
  assert.equal(needs["@al-ft/midgard-core"], false);
});

test("a test script's environment, flags and preludes are read, quotes and all", () => {
  assert.deepEqual(
    parseTestScript(
      "pnpm run a && pnpm run b && export NODE_ENV='emulator' && vitest run --disableConsoleIntercept",
    ),
    {
      env: { NODE_ENV: "emulator" },
      exclude: [],
      maxWorkers: undefined,
      disableConsoleIntercept: true,
      preludes: ["pnpm run a", "pnpm run b"],
    },
  );
  const watcher = parseTestScript(
    "env MALLOC_MMAP_THRESHOLD_=131072 vitest run --exclude 'tests/fork-sim/**' --maxWorkers=2",
  );
  assert.deepEqual(watcher.env, { MALLOC_MMAP_THRESHOLD_: "131072" });
  assert.deepEqual(vitestFlags(watcher), [
    "--exclude=tests/fork-sim/**",
    "--maxWorkers=2",
  ]);
});

test("a test script flag contrib does not understand is refused, not dropped", () => {
  // Dropping it would make `contrib test` run something other than CI does.
  assert.throws(
    () => parseTestScript("vitest run --pool=threads"),
    /does not understand '--pool=threads'/u,
  );
  assert.throws(
    () => parseTestScript("vitest --run"),
    /must use 'vitest run'/u,
  );
  assert.throws(
    () => parseTestScript("vitest run && vitest run"),
    /exactly once/u,
  );
  assert.throws(
    () => parseTestScript("vitest run && node after.mjs"),
    /a step follows vitest/u,
  );
});

test("caller flags add to the script's: excludes accumulate, a worker cap replaces", () => {
  const merged = withCallerFlags(
    parseTestScript("vitest run --exclude a --maxWorkers 4"),
    { exclude: ["b"], maxWorkers: "1", disableConsoleIntercept: true },
  );
  assert.deepEqual(vitestFlags(merged), [
    "--exclude=a",
    "--exclude=b",
    "--maxWorkers=1",
    "--disableConsoleIntercept",
  ]);
});

test("only Vitest's own flag spellings pass, and only to test", () => {
  const options = parse([
    "test",
    "--exclude",
    "a",
    "--exclude",
    "b",
    "--maxWorkers",
    "2",
    "--disableConsoleIntercept",
  ]);
  assert.deepEqual(options.excludes, ["a", "b"]);
  assert.equal(options.maxWorkers, "2");
  assert.equal(options.disableConsoleIntercept, true);
  for (const spelling of ["--max-workers", "--pool", "--bail"])
    assert.throws(() => parse(["test", spelling, "1"]), /unknown option/u);
  assert.match(HELP, /--maxWorkers N\] \[--exclude GLOB\]/u);
});

test("a list option takes every word up to the next option", () => {
  const options = parse([
    "test",
    "--file",
    "tests/a.test.ts",
    "tests/b.test.ts",
    "--name",
    "x",
    "--file",
    "tests/c.test.ts",
  ]);
  assert.deepEqual(options.words, ["test"]);
  assert.deepEqual(options.files, [
    "tests/a.test.ts",
    "tests/b.test.ts",
    "tests/c.test.ts",
  ]);
  assert.equal(options.name, "x");
  assert.deepEqual(
    parse(["test", "--related", "src/a.ts", "src/b.ts"]).related,
    ["src/a.ts", "src/b.ts"],
  );
  assert.deepEqual(parse(["test", "--exclude", "a", "b"]).excludes, ["a", "b"]);
});

test("the failure summary names each failed test and each file that failed to load", () => {
  const report = {
    testResults: [
      {
        name: "/pkg/tests/a.test.ts",
        status: "failed",
        assertionResults: [
          { status: "passed", fullName: "a fine" },
          {
            status: "failed",
            fullName: "a broken",
            failureMessages: ["\nAssertionError: expected 1 to be 2\n    at x"],
          },
        ],
      },
      {
        name: "/pkg/tests/b.test.ts",
        status: "failed",
        message: "Failed to load url ./missing.js",
        assertionResults: [],
      },
    ],
  };
  const found = failures(report, "/pkg");
  assert.deepEqual(found, [
    {
      file: "tests/a.test.ts",
      test: "a broken",
      message: "AssertionError: expected 1 to be 2",
    },
    { file: "tests/b.test.ts", message: "Failed to load url ./missing.js" },
  ]);
  const lines = summaryLines({
    package: "pkg",
    status: "failed",
    counts: { passed: 1, failed: 1, skipped: 0 },
    selectedFiles: ["a", "b"],
    seed: 1,
    failures: found,
    steps: [{ exitCode: 1, logPath: "/run/test.log" }],
    path: "/run/receipt.json",
  });
  assert.deepEqual(lines, [
    "contrib test pkg: failed (1 passed, 1 failed, 0 skipped, in 2 file(s), seed 1)",
    "  FAIL tests/a.test.ts > a broken",
    "       AssertionError: expected 1 to be 2",
    "  FAIL tests/b.test.ts",
    "       Failed to load url ./missing.js",
    "  log: /run/test.log",
    "  receipt: /run/receipt.json",
  ]);
});

// A stand-in Vitest: `list` collects tests/*.test.ts under the file filters
// and --exclude globs; `run` records what it was given, whether the
// checkout's workspace lease was held, and reports one test per file
// (failing where the file says FAIL).
const fakeVitest = `
import {appendFileSync,readdirSync,readFileSync,writeFileSync} from 'node:fs';
import {resolve} from 'node:path';
import {listResources} from ${JSON.stringify(new URL("./resources.mjs", import.meta.url).href)};
const [mode, ...rest] = process.argv.slice(2);
const value = (name) => rest.filter((arg) => arg.startsWith('--' + name + '=')).map((arg) => arg.slice(name.length + 3));
const glob = (pattern) => new RegExp('^' + pattern.replace(/[.+?^$()|[\\]\\\\]/g, '\\\\$&').replace(/\\*\\*/g, '\\0').replace(/\\*/g, '[^/]*').replace(/\\0/g, '.*') + '$');
const filters = rest.filter((arg) => arg.endsWith('.test.ts'));
const collected = readdirSync('tests').map((name) => 'tests/' + name)
  .filter((file) => !filters.length || filters.some((filter) => file.includes(filter)))
  .filter((file) => !value('exclude').some((pattern) => glob(pattern).test(file)));
if (mode === 'list') {
  writeFileSync(value('json')[0], JSON.stringify(collected.map((file) => ({file: resolve(file)}))));
  process.exit(0);
}
const lease = listResources().some((entry) => entry.resource === 'workspace:' + process.env.FAKE_ROOT);
appendFileSync(process.env.FAKE_LOG, JSON.stringify({vitest: rest, NODE_ENV: process.env.NODE_ENV, lease}) + '\\n');
const results = collected.map((file) => ({name: resolve(file), status: readFileSync(file, 'utf8').includes('FAIL') ? 'failed' : 'passed',
  assertionResults: [{status: readFileSync(file, 'utf8').includes('FAIL') ? 'failed' : 'passed', fullName: file + ' case', failureMessages: ['Error: broke in ' + file]}]}));
const failed = results.filter((suite) => suite.status === 'failed').length;
writeFileSync(value('outputFile')[0], JSON.stringify({success: failed === 0, numPassedTests: results.length - failed, numFailedTests: failed, testResults: results}));
process.exit(failed ? 1 : 0);
`;

const workspace = (t, script, tests) => {
  const root = realpathSync(fixture(t));
  const runner = resolve(root, "demo/node_modules/vitest");
  mkdirSync(runner, { recursive: true });
  atomicJson(resolve(runner, "package.json"), { name: "vitest" });
  writeFileSync(resolve(runner, "vitest.mjs"), fakeVitest);
  const manifest = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(manifest, "utf8"));
  pkg.scripts.test = script;
  atomicJson(manifest, pkg);
  mkdirSync(resolve(root, "demo/example/tests"));
  for (const [name, body] of Object.entries(tests))
    writeFileSync(resolve(root, "demo/example/tests", name), body);
  const tools = resolve(root, "tools");
  mkdirSync(tools);
  const log = resolve(root, "calls.jsonl");
  writeFileSync(log, "");
  // `corepack pnpm run <script>` runs a prelude; record it.
  writeFileSync(
    resolve(tools, "corepack"),
    `#!/bin/sh\nprintf '{"prelude":"%s"}\\n' "$*" >> ${JSON.stringify(log)}\n`,
    { mode: 0o755 },
  );
  const env = {
    ...process.env,
    PATH: `${tools}:${process.env.PATH}`,
    FAKE_ROOT: root,
    FAKE_LOG: log,
  };
  const calls = () =>
    readFileSync(log, "utf8")
      .trim()
      .split("\n")
      .filter(Boolean)
      .map((line) => JSON.parse(line));
  return { root, env, calls };
};

test("a whole-package run is the package's test script: its environment, flags and preludes, outside the lease", async (t) => {
  const { root, env, calls } = workspace(
    t,
    "pnpm run test:extra && export NODE_ENV='emulator' && vitest run --exclude 'tests/slow*'",
    {
      "a.test.ts": "",
      "b.test.ts": "",
      "slow.test.ts": "",
      "skip.test.ts": "",
    },
  );
  const receipt = await runTests(root, "example", {
    sourceOnly: true,
    env,
    flags: { exclude: ["tests/skip*"], maxWorkers: "1" },
  });
  assert.equal(receipt.status, "passed", receipt.reason ?? receipt.reportError);
  assert.deepEqual(
    receipt.selectedFiles,
    ["a", "b"].map((name) =>
      resolve(root, `demo/example/tests/${name}.test.ts`),
    ),
  );
  const [prelude, run] = calls();
  assert.deepEqual(prelude, { prelude: "pnpm run test:extra" });
  assert.equal(run.NODE_ENV, "emulator");
  assert.equal(run.lease, false, "the suites run without the workspace lease");
  for (const flag of [
    "--exclude=tests/slow*",
    "--exclude=tests/skip*",
    "--maxWorkers=1",
  ])
    assert.ok(run.vitest.includes(flag), flag);
});

test("a focused run skips the preludes and defaults to the test runtime", async (t) => {
  const { root, env, calls } = workspace(
    t,
    "pnpm run test:extra && vitest run",
    { "a.test.ts": "", "b.test.ts": "" },
  );
  const receipt = await runTests(root, "example", {
    files: ["tests/a.test.ts"],
    sourceOnly: true,
    env: { ...env, NODE_ENV: "emulator" },
  });
  assert.equal(receipt.status, "passed");
  const [run, ...rest] = calls();
  assert.deepEqual(rest, []);
  assert.equal(run.NODE_ENV, "test");
});

test("a named file that Vitest would not run is refused before anything runs", async (t) => {
  const { root, env, calls } = workspace(
    t,
    "vitest run --exclude 'tests/slow*'",
    { "slow.test.ts": "", "a.test.ts": "", "data.test.ts": "" },
  );
  await assert.rejects(
    runTests(root, "example", {
      files: ["tests/slow.test.ts"],
      sourceOnly: true,
      env,
    }),
    /does not collect tests\/slow\.test\.ts/u,
  );
  await assert.rejects(
    runTests(root, "example", {
      files: ["tests/a.test.ts"],
      sourceOnly: true,
      env,
      flags: { exclude: ["tests/a*"] },
    }),
    /does not collect tests\/a\.test\.ts/u,
  );
  assert.deepEqual(calls(), []);
});

test("a failing run fails its receipt and names the failure", async (t) => {
  const { root, env } = workspace(t, "vitest run", {
    "a.test.ts": "",
    "b.test.ts": "FAIL",
  });
  const receipt = await runTests(root, "example", { sourceOnly: true, env });
  assert.equal(receipt.status, "failed");
  assert.equal(receipt.exitCode, 1);
  assert.deepEqual(receipt.failures, [
    {
      file: "tests/b.test.ts",
      test: "tests/b.test.ts case",
      message: "Error: broke in tests/b.test.ts",
    },
  ]);
  assert.match(summaryLines(receipt).join("\n"), /FAIL tests\/b\.test\.ts/u);
});

test("a related run runs what the change reaches, with the preludes, and nothing when it reaches nothing", async (t) => {
  const { root, env, calls } = workspace(
    t,
    "pnpm run test:extra && vitest run",
    { "a.test.ts": "", "b.test.ts": "" },
  );
  const stub = (answer) => async (_root, pkg, _command, changed) => {
    assert.equal(pkg.name, "example");
    assert.deepEqual(changed, ["demo/example/src/a.ts"]);
    return { why: [], relevant: changed, ...answer };
  };
  const related = [resolve(root, "demo/example/src/a.ts")];
  const reached = await runTests(root, "example", {
    related,
    sourceOnly: true,
    env,
    reach: stub({ whole: false, files: ["tests/a.test.ts"] }),
  });
  assert.equal(reached.status, "passed");
  assert.deepEqual(reached.selectedFiles, [
    resolve(root, "demo/example/tests/a.test.ts"),
  ]);
  assert.deepEqual(reached.reach.files, ["tests/a.test.ts"]);
  const [prelude, run, ...rest] = calls();
  assert.deepEqual(prelude, { prelude: "pnpm run test:extra" });
  assert.ok(
    run.vitest.some((arg) => arg.endsWith("a.test.ts")),
    run.vitest,
  );
  assert.deepEqual(rest, []);

  const nothing = await runTests(root, "example", {
    related,
    sourceOnly: true,
    env,
    reach: stub({ whole: false, files: [], relevant: [] }),
  });
  assert.equal(nothing.status, "not-reached");
  assert.equal(nothing.exitCode, 0);
  assert.equal(calls().length, 2, "a change reaching nothing runs nothing");

  const whole = await runTests(root, "example", {
    related,
    sourceOnly: true,
    env,
    reach: stub({ whole: true, files: ["tests/a.test.ts"] }),
  });
  assert.deepEqual(
    whole.selectedFiles,
    ["a", "b"].map((name) =>
      resolve(root, `demo/example/tests/${name}.test.ts`),
    ),
  );
});

test("a related change that reaches only the plain-Node preludes runs the whole script", async (t) => {
  const { root, env, calls } = workspace(
    t,
    "pnpm run test:extra && vitest run",
    { "a.test.ts": "", "b.test.ts": "" },
  );
  const receipt = await runTests(root, "example", {
    related: ["demo/example/scripts/check.mjs"],
    sourceOnly: true,
    env,
    reach: async (_root, _pkg, _command, changed) => ({
      whole: false,
      files: [],
      relevant: changed,
      why: [],
    }),
  });
  assert.equal(receipt.reach.whole, true);
  assert.match(receipt.reach.why.at(-1), /plain-Node test steps/u);
  assert.equal(receipt.selectedFiles.length, 2);
  assert.deepEqual(calls()[0], { prelude: "pnpm run test:extra" });
  // Without preludes, the same answer reaches nothing.
  const plain = workspace(t, "vitest run", { "a.test.ts": "" });
  const skipped = await runTests(plain.root, "example", {
    related: ["demo/example/scripts/check.mjs"],
    sourceOnly: true,
    env: plain.env,
    reach: async (_root, _pkg, _command, changed) => ({
      whole: false,
      files: [],
      relevant: changed,
      why: [],
    }),
  });
  assert.equal(skipped.status, "not-reached");
  assert.deepEqual(plain.calls(), []);
});

test("--related is a test option, exclusive with --file", async (t) => {
  const options = parse([
    "test",
    "--package",
    "example",
    "--related",
    "a.ts",
    "--related",
    "b.ts",
  ]);
  assert.deepEqual(options.related, ["a.ts", "b.ts"]);
  const refused = spawnSync(
    process.execPath,
    [
      resolve(repository, "scripts/contrib.mjs"),
      "build",
      "--package",
      "example",
      "--related",
      "a.ts",
    ],
    { encoding: "utf8" },
  );
  assert.notEqual(refused.status, 0);
  assert.match(refused.stderr, /--related is supported only for test/u);
  const { root, env, calls } = workspace(t, "vitest run", { "a.test.ts": "" });
  await assert.rejects(
    runTests(root, "example", {
      related: ["a.ts"],
      files: ["tests/a.test.ts"],
      sourceOnly: true,
      env,
    }),
    /exclusive/u,
  );
  assert.deepEqual(calls(), []);
});
