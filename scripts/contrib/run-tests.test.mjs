import {
  packageTasks,
  taskProcess,
} from "../../demo/scripts/package-tasks.mjs";
import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  realpathSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { resolve } from "node:path";
import test from "node:test";

import { HELP, parse } from "../contrib.mjs";
import { packageNeedsPostgres } from "../preflight/derive.mjs";
import { atomicJson, workspacePackages } from "./files.mjs";
import { fixture } from "./fixture.test-support.mjs";
import { preparationPlan, runTests, testCommand } from "./tests.mjs";
import {
  failures,
  declaredTestCommand,
  summaryLines,
  vitestFlags,
  withCallerFlags,
} from "./vitest-command.mjs";

const repository = resolve(import.meta.dirname, "../..");

test("declared compiled prerequisites cover a CLI path assembled by the test", (t) => {
  const root = realpathSync(fixture(t));
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path, "utf8"));
  pkg.midgardTasks = {
    build: {
      steps: [{ command: ["tsup", "src/index.ts"] }],
      outputs: ["dist"],
    },
    test: {
      command: ["vitest", "run"],
      prerequisites: [{ kind: "build", package: "example" }],
    },
  };
  atomicJson(path, pkg);
  mkdirSync(resolve(root, "demo/example/tests"));
  writeFileSync(
    resolve(root, "demo/example/tests/cli.test.ts"),
    'spawnSync(process.execPath, [join(import.meta.dirname, "..", "dist", "cli.js")]);\n',
  );
  assert.deepEqual(
    preparationPlan(root, "example").prerequisites.map(({ name }) => name),
    ["example"],
  );
});

test("every workspace test script reads as one Vitest command, or says it has none", () => {
  for (const pkg of workspacePackages(repository).filter(
    (pkg) => pkg.scripts?.test,
  )) {
    if (!pkg.midgardTasks?.test) {
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

test("a package whose tests run its own dist plans that dist as a prerequisite", () => {
  // devnet-stack-acceptance-cli spawns dist/devnet-stack.js; the plan once
  // built only the package's dependencies, so a focused run used a stale or
  // missing dist.
  const plan = preparationPlan(repository, "midgard-node-tools");
  assert.ok(
    plan.prerequisites.some(({ name }) => name === "midgard-node-tools"),
    plan.prerequisites.map(({ name }) => name).join(", "),
  );
});

test("task declarations reject unknown prerequisites and duplicate environment ownership", () => {
  const pkg = {
    name: "example",
    midgardTasks: {
      test: {
        command: ["vitest", "run"],
        prerequisites: [{ kind: "database" }],
      },
    },
  };
  assert.throws(
    () => packageTasks(pkg),
    /prerequisite kind database is unknown/u,
  );
  pkg.midgardTasks.test.prerequisites = [{ kind: "build", package: "missing" }];
  assert.throws(
    () => packageTasks(pkg),
    /must be a declared workspace dependency/u,
  );
  pkg.midgardTasks = {
    build: {
      steps: [
        {
          command: ["node", "build.mjs"],
          environment: { FLAG: "a" },
          appendEnvironment: { FLAG: "b" },
        },
      ],
      outputs: ["dist"],
    },
  };
  assert.throws(() => packageTasks(pkg), /both replaces and appends FLAG/u);
});

test("declared test flags are preserved and unknown flags are refused", () => {
  const command = declaredTestCommand({
    command: [
      "vitest",
      "run",
      "--exclude",
      "tests/fork-sim/**",
      "--maxWorkers=2",
    ],
    environment: { MALLOC_MMAP_THRESHOLD_: "131072" },
  });
  assert.deepEqual(command.env, { MALLOC_MMAP_THRESHOLD_: "131072" });
  assert.deepEqual(vitestFlags(command), [
    "--exclude=tests/fork-sim/**",
    "--maxWorkers=2",
  ]);
  assert.throws(
    () => declaredTestCommand({ command: ["vitest", "run", "--pool=threads"] }),
    /does not understand '--pool=threads'/u,
  );
});

test("caller flags add to the declaration: excludes accumulate, a worker cap replaces", () => {
  const merged = withCallerFlags(
    declaredTestCommand({
      command: ["vitest", "run", "--exclude", "a", "--maxWorkers", "4"],
    }),
    {
      exclude: ["b"],
      maxWorkers: "1",
      disableConsoleIntercept: true,
    },
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

// A stand-in Vitest: `list` collects tests/*.test.ts (journeys/*.test.ts
// under any --config) under the file filters and --exclude globs; `run` records what it was given, whether the
// checkout's workspace lease was held, and reports one test per file
// (failing where the file says FAIL).
const fakeVitest = `
import {appendFileSync,readdirSync,readFileSync,writeFileSync} from 'node:fs';
import {resolve,join} from 'node:path';
import {spawnSync} from 'node:child_process';
import {listResources} from ${JSON.stringify(new URL("./resources.mjs", import.meta.url).href)};
const [mode, ...rest] = process.argv.slice(2);
const value = (name) => rest.filter((arg) => arg.startsWith('--' + name + '=')).map((arg) => arg.slice(name.length + 3));
const glob = (pattern) => new RegExp('^' + pattern.replace(/[.+?^$()|[\\]\\\\]/g, '\\\\$&').replace(/\\*\\*/g, '\\0').replace(/\\*/g, '[^/]*').replace(/\\0/g, '.*') + '$');
const filters = rest.filter((arg, index) => !arg.startsWith('--') && rest[index - 1] !== '--testNamePattern');
const directory = value('config').length ? 'journeys' : 'tests';
const collected = readdirSync(directory).map((name) => directory + '/' + name)
  .filter((file) => !filters.length || filters.some((filter) => file.includes(filter)))
  .filter((file) => !value('exclude').some((pattern) => glob(pattern).test(file)));
if (mode === 'list') {
  writeFileSync(value('json')[0], JSON.stringify(collected.map((file) => ({file: resolve(file)}))));
  process.exit(0);
}
const lease = listResources().some((entry) => entry.resource === 'workspace:' + process.env.FAKE_ROOT);
appendFileSync(process.env.FAKE_LOG, JSON.stringify({vitest: rest, NODE_ENV: process.env.NODE_ENV, lease}) + '\\n');
for (const file of collected) if (readFileSync(file, 'utf8').includes('RUN_CLI')) {
  const result = spawnSync(process.execPath, [join(process.cwd(), 'dist', 'cli.js')], {encoding:'utf8'});
  if (result.status !== 0) throw new Error(result.stderr || 'compiled CLI failed');
  appendFileSync(process.env.FAKE_LOG, JSON.stringify({cli:result.stdout.trim()}) + '\\n');
}
const results = collected.map((file) => {
  const body = readFileSync(file, 'utf8');
  const status = body.includes('FAIL') ? 'failed' : body.includes('SKIP') ? 'pending' : body.includes('TODO') ? 'todo' : 'passed';
  return {name: resolve(file), status: status === 'failed' ? 'failed' : 'passed',
    assertionResults: body.includes('ZERO') ? [] : [{status, fullName: file + ' case', failureMessages: ['Error: broke in ' + file]}]};
});
const assertions = results.flatMap((suite) => suite.assertionResults);
const failed = assertions.filter((entry) => entry.status === 'failed').length;
const passed = assertions.filter((entry) => entry.status === 'passed').length;
const setup = collected.some((file) => readFileSync(file, 'utf8').includes('SETUP'));
if (collected.some((file) => readFileSync(file, 'utf8').includes('NO_REPORT'))) process.exit(0);
writeFileSync(value('outputFile')[0], JSON.stringify({success: failed === 0 && !setup, numRuntimeErrorTestSuites: setup ? 1 : 0, numPassedTests: passed, numFailedTests: failed, testResults: results}));
process.exit(failed || setup ? 1 : 0);
`;

const workspace = (t, task, tests) => {
  const root = realpathSync(fixture(t));
  const runner = resolve(root, "demo/node_modules/vitest");
  mkdirSync(runner, { recursive: true });
  atomicJson(resolve(runner, "package.json"), {
    name: "vitest",
    bin: "vitest.mjs",
  });
  writeFileSync(resolve(runner, "vitest.mjs"), fakeVitest);
  atomicJson(resolve(root, "demo/package.json"), {
    packageManager: "pnpm@9.15.4",
  });
  const manifest = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(manifest, "utf8"));
  pkg.scripts.test = "node ../../scripts/contrib.mjs test --package example";
  pkg.scripts["test:extra"] = "node extra.mjs";
  pkg.midgardTasks.test = task;
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
    `#!/bin/sh\nif [ "$*" = "pnpm --version" ]; then printf "9.15.4\\n"; exit 0; fi\nprintf '{"prelude":"%s"}\\n' "$*" >> ${JSON.stringify(log)}\n`,
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
    {
      environment: { NODE_ENV: "emulator" },
      command: ["vitest", "run", "--exclude", "tests/slow*"],
      before: [{ command: ["pnpm", "run", "test:extra"] }],
    },
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
    "--sequence.seed=1",
    "--sequence.shuffle.files",
  ])
    assert.ok(run.vitest.includes(flag), flag);
  // Tests in a file run in their declared order, as in CI: suites whose last
  // test checks what the earlier ones recorded (the fault-proof lifecycle
  // coverage gates) failed only under contrib while tests were shuffled.
  assert.ok(
    !run.vitest.some((arg) => arg.startsWith("--sequence.shuffle.tests")),
    run.vitest.join(" "),
  );
});

test("a focused run skips the preludes and defaults to the test runtime", async (t) => {
  const { root, env, calls } = workspace(
    t,
    {
      command: ["vitest", "run"],
      before: [{ command: ["pnpm", "run", "test:extra"] }],
    },
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
    { command: ["vitest", "run", "--exclude", "tests/slow*"] },
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
  const { root, env } = workspace(
    t,
    { command: ["vitest", "run"] },
    {
      "a.test.ts": "",
      "b.test.ts": "FAIL",
    },
  );
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
    {
      command: ["vitest", "run"],
      before: [{ command: ["pnpm", "run", "test:extra"] }],
    },
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
    {
      command: ["vitest", "run"],
      before: [{ command: ["pnpm", "run", "test:extra"] }],
    },
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
  const plain = workspace(
    t,
    { command: ["vitest", "run"] },
    { "a.test.ts": "" },
  );
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
  const { root, env, calls } = workspace(
    t,
    { command: ["vitest", "run"] },
    { "a.test.ts": "" },
  );
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

test("--config runs named files of another package config, and only named files", async (t) => {
  const { root, env, calls } = workspace(
    t,
    { environment: { NODE_ENV: "emulator" }, command: ["vitest", "run"] },
    { "a.test.ts": "" },
  );
  mkdirSync(resolve(root, "demo/example/journeys"));
  writeFileSync(resolve(root, "demo/example/journeys/j.test.ts"), "");
  writeFileSync(resolve(root, "demo/example/vitest.journeys.config.ts"), "");
  const config = "vitest.journeys.config.ts";
  await assert.rejects(
    runTests(root, "example", {
      files: ["journeys/j.test.ts"],
      sourceOnly: true,
      env,
    }),
    /does not collect journeys\/j\.test\.ts/u,
  );
  await assert.rejects(
    runTests(root, "example", { sourceOnly: true, env, flags: { config } }),
    /--config runs only the files --file names/u,
  );
  for (const wrong of ["../vitest.journeys.config.ts", "vitest.gone.config.ts"])
    await assert.rejects(
      runTests(root, "example", {
        files: ["journeys/j.test.ts"],
        sourceOnly: true,
        env,
        flags: { config: wrong },
      }),
      /--config takes a Vitest config file in demo\/example/u,
    );
  assert.deepEqual(calls(), []);
  const receipt = await runTests(root, "example", {
    files: ["journeys/j.test.ts"],
    sourceOnly: true,
    env,
    flags: { config },
  });
  assert.equal(receipt.status, "passed", receipt.reason ?? receipt.reportError);
  assert.deepEqual(receipt.selectedFiles, [
    resolve(root, "demo/example/journeys/j.test.ts"),
  ]);
  assert.deepEqual(receipt.flags, [`--config=${config}`]);
  const [run, ...rest] = calls();
  assert.deepEqual(rest, []);
  assert.ok(run.vitest.includes(`--config=${config}`), run.vitest.join(" "));
  assert.equal(run.NODE_ENV, "emulator");
  assert.equal(parse(["test", "--config", config]).config, config);
  assert.throws(
    () => parse(["test", "--config", config, "--config", config]),
    /duplicate --config/u,
  );
});

test("--timeout-seconds bounds the Vitest step, and only as whole seconds", async (t) => {
  const { root, env, calls } = workspace(
    t,
    { command: ["vitest", "run"] },
    { "a.test.ts": "" },
  );
  for (const timeoutMs of [0, -1000, 2 ** 31, 1.5])
    await assert.rejects(
      runTests(root, "example", { sourceOnly: true, env, timeoutMs }),
      /--timeout-seconds must be a positive whole number/u,
    );
  assert.deepEqual(calls(), []);
  // A deadline shorter than the fake runner's start-up stops the step.
  const receipt = await runTests(root, "example", {
    files: ["tests/a.test.ts"],
    sourceOnly: true,
    env,
    timeoutMs: 1,
  });
  assert.equal(receipt.status, "failed");
  assert.match(JSON.stringify(receipt.steps), /deadline exceeded 1 ms/u);
  assert.equal(
    parse(["test", "--timeout-seconds", "60"])["timeout-seconds"],
    "60",
  );
});

test("a blueprint override is refused unless it is this checkout's own ready blueprint", async (t) => {
  const { root, env, calls } = workspace(
    t,
    { command: ["vitest", "run"] },
    { "a.test.ts": "" },
  );
  const copy = resolve(root, "frozen-plutus.json");
  writeFileSync(copy, "{}\n");
  await assert.rejects(
    runTests(root, "example", {
      files: ["tests/a.test.ts"],
      sourceOnly: true,
      env: { ...env, MIDGARD_REAL_BLUEPRINT_PATH: copy },
    }),
    /refuse MIDGARD_REAL_BLUEPRINT_PATH=.*byte-identical copy of this checkout's ready blueprint/u,
  );
  assert.deepEqual(calls(), []);
});

test("computed CLI paths execute newly prepared and stale rebuilt outputs", async (t) => {
  const { root, env, calls } = workspace(
    t,
    {
      command: ["vitest", "run"],
      prerequisites: [{ kind: "build", package: "example" }],
    },
    {
      "cli.test.ts":
        "// RUN_CLI: join(import.meta.dirname, '..', 'dist', 'cli.js')",
    },
  );
  const manifest = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(manifest, "utf8"));
  pkg.midgardTasks.build.steps = [{ command: ["node", "scripts/build.mjs"] }];
  atomicJson(manifest, pkg);
  mkdirSync(resolve(root, "demo/example/scripts"));
  writeFileSync(
    resolve(root, "demo/example/scripts/build.mjs"),
    'import {mkdirSync,copyFileSync} from "node:fs"; mkdirSync("dist",{recursive:true}); copyFileSync("src/cli.js","dist/cli.js");\n',
  );
  const source = resolve(root, "demo/example/src/cli.js");
  writeFileSync(source, 'console.log("first");\n');
  const ordinary = spawnSync(
    process.execPath,
    [
      resolve(repository, "demo/scripts/package-test.mjs"),
      "tests/cli.test.ts",
      `--outputFile=${resolve(root, "ordinary.json")}`,
    ],
    { cwd: resolve(root, "demo/example"), env, encoding: "utf8" },
  );
  assert.equal(ordinary.status, 0, ordinary.stderr);
  writeFileSync(source, 'console.log("second");\n');
  assert.equal((await runTests(root, "example", { env })).status, "passed");
  assert.deepEqual(
    calls()
      .filter((entry) => entry.cli)
      .map((entry) => entry.cli),
    ["first", "second"],
  );
});

test("a prerequisite that builds several output directories passes when none of them change", async (t) => {
  // midgard-validation emits dist and vendor; the post-run check once hashed
  // only dist against a stamp of both, so every run failed with "artifacts
  // changed during test".
  const { root, env } = workspace(
    t,
    {
      command: ["vitest", "run"],
      prerequisites: [{ kind: "build", package: "example" }],
    },
    { "a.test.ts": "" },
  );
  const manifest = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(manifest, "utf8"));
  pkg.midgardTasks.build = {
    steps: [{ command: ["node", "scripts/build.mjs"] }],
    outputs: ["dist", "vendor"],
  };
  atomicJson(manifest, pkg);
  mkdirSync(resolve(root, "demo/example/scripts"));
  writeFileSync(
    resolve(root, "demo/example/scripts/build.mjs"),
    'import {mkdirSync,writeFileSync} from "node:fs"; for (const d of ["dist","vendor"]) { mkdirSync(d,{recursive:true}); writeFileSync(d + "/index.js", "export {};\\n"); }\n',
  );
  const receipt = await runTests(root, "example", { env });
  assert.equal(receipt.status, "passed", receipt.reason ?? receipt.reportError);
});

test("task argv preserves shell characters and environment append values literally", (t) => {
  const root = fixture(t);
  const script = resolve(root, "echo.mjs");
  writeFileSync(
    script,
    "console.log(JSON.stringify({argv:process.argv.slice(2),flag:process.env.FLAG}));",
  );
  const invocation = taskProcess(
    {
      command: ["node", script, "$(touch forbidden)", ">", "a b", "&&"],
      appendEnvironment: { FLAG: " suffix" },
    },
    root,
    { ...process.env, FLAG: "base" },
  );
  const result = spawnSync(invocation.argv[0], invocation.argv.slice(1), {
    cwd: invocation.cwd,
    env: invocation.env,
    encoding: "utf8",
  });
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(JSON.parse(result.stdout), {
    argv: ["$(touch forbidden)", ">", "a b", "&&"],
    flag: "base suffix",
  });
});

test("guarded evidence stays incomplete for selected skips and todos and fails missing execution", async (t) => {
  for (const [body, expectedStatus, expectedExit] of [
    ["SKIP", "incomplete", 3],
    ["FAIL", "failed", 1],
    ["SETUP", "failed", 1],
    ["NO_REPORT", "failed", 1],
    ["TODO", "incomplete", 3],
  ]) {
    const { root, env } = workspace(
      t,
      { command: ["vitest", "run"] },
      { "passed.test.ts": "", "other.test.ts": body },
    );
    const result = await runTests(root, "example", { sourceOnly: true, env });
    assert.equal(result.status, expectedStatus, body);
    assert.equal(result.exitCode, expectedExit, body);
    if (body === "SKIP") {
      assert.equal(result.exitCode, 3, "direct contrib keeps its strict exit");
      assert.equal(
        JSON.parse(readFileSync(result.path, "utf8")).status,
        "incomplete",
        "selected skips cannot accept the receipt",
      );
    }
  }
  for (const body of ["ZERO", "SKIP"]) {
    const { root, env } = workspace(
      t,
      { command: ["vitest", "run"] },
      { "empty.test.ts": body },
    );
    const result = await runTests(root, "example", { sourceOnly: true, env });
    assert.equal(result.counts.executed, 0);
    assert.equal(result.exitCode, 1, "zero executed assertions never pass");
  }
});

test("ordinary package scripts preserve Vitest selectors, reporters, CI environment and native exit status", (t) => {
  const root = realpathSync(fixture(t));
  const cwd = resolve(root, "demo/example");
  const vitest = realpathSync(resolve(repository, "demo/node_modules/vitest"));
  mkdirSync(resolve(cwd, "node_modules"));
  symlinkSync(vitest, resolve(cwd, "node_modules/vitest"));
  const manifest = resolve(cwd, "package.json");
  const pkg = JSON.parse(readFileSync(manifest, "utf8"));
  pkg.midgardTasks.test = {
    command: ["vitest", "run"],
    environment: { NODE_ENV: "emulator", DECLARED_FLAG: "literal" },
    prerequisites: [{ kind: "postgres" }],
  };
  atomicJson(manifest, pkg);
  mkdirSync(resolve(cwd, "tests"));
  writeFileSync(
    resolve(cwd, "tests/selected.test.ts"),
    `
    import {test,expect} from "vitest";
    test("selected case", () => {
      expect(process.env.POSTGRES_PORT).toBe("5432");
      expect(process.env.POSTGRES_DB).toBe("midgard");
      expect(process.env.NODE_ENV).toBe("emulator");
      expect(process.env.DECLARED_FLAG).toBe("literal");
    });
    test("other case", () => { throw new Error("name filter was lost"); });
    test.skip("intentional skip", () => {});
    test.todo("intentional todo");
  `,
  );
  writeFileSync(
    resolve(cwd, "tests/unselected.test.ts"),
    `import {test} from "vitest"; test("selected case",()=>{throw new Error("file filter was lost")});`,
  );
  const report = resolve(root, "report.json");
  const env = { ...process.env, POSTGRES_PORT: "5432", POSTGRES_DB: "midgard" };
  const args = [
    "--maxWorkers=1",
    "--reporter=json",
    `--outputFile=${report}`,
    "--pool=forks",
  ];
  for (const nameFlag of ["-t", "--testNamePattern"]) {
    const result = spawnSync(
      process.execPath,
      [
        resolve(repository, "demo/scripts/package-test.mjs"),
        "tests/selected",
        nameFlag,
        "selected case",
        ...args,
      ],
      { cwd, env, encoding: "utf8" },
    );
    assert.equal(result.status, 0, result.stderr);
    const summary = JSON.parse(readFileSync(report, "utf8"));
    assert.equal(summary.numPassedTests, 1);
    assert.equal(summary.numFailedTests, 0);
    assert.equal(summary.testResults.length, 1);
  }
  const reporterOutput = resolve(root, "configured-reporter.txt");
  writeFileSync(
    resolve(cwd, "vitest.config.mjs"),
    `
    import {writeFileSync} from "node:fs";
    export default {test:{reporters:[{onTestRunEnd(){writeFileSync(${JSON.stringify(reporterOutput)},"called")}}]}};
  `,
  );
  const configured = spawnSync(
    process.execPath,
    [
      resolve(repository, "demo/scripts/package-test.mjs"),
      "tests/selected",
      "-t",
      "selected case|intentional",
      "--maxWorkers=1",
    ],
    { cwd, env, encoding: "utf8" },
  );
  assert.equal(configured.status, 0, configured.stderr);
  assert.equal(
    readFileSync(reporterOutput, "utf8"),
    "called",
    "configured duration reporters run without injected CLI reporters",
  );
  // A whole selected file still returns Vitest's failure, without translating it
  // into a receipt status. The same frontend accepts CI's shard spelling.
  const failed = spawnSync(
    process.execPath,
    [
      resolve(repository, "demo/scripts/package-test.mjs"),
      "tests/selected",
      "--shard=1/1",
      ...args,
    ],
    { cwd, env, encoding: "utf8" },
  );
  assert.equal(failed.status, 1);
  assert.equal(JSON.parse(readFileSync(report, "utf8")).numFailedTests, 1);
});

test("ordinary preludes inherit the caller environment, run for focused selections and stop on first failure", (t) => {
  const { root, env, calls } = workspace(
    t,
    {
      command: ["vitest", "run"],
      environment: { NODE_ENV: "emulator" },
      before: [
        {
          command: ["pnpm", "run", "test:first"],
          environment: { PRELUDE_FLAG: "step" },
        },
        { command: ["pnpm", "run", "test:second"] },
      ],
    },
    { "a.test.ts": "" },
  );
  const cwd = resolve(root, "demo/example");
  const manifest = resolve(cwd, "package.json");
  const pkg = JSON.parse(readFileSync(manifest, "utf8"));
  pkg.scripts["test:first"] = "node first.mjs";
  pkg.scripts["test:second"] = "node second.mjs";
  atomicJson(manifest, pkg);
  atomicJson(resolve(root, "demo/node_modules/vitest/package.json"), {
    name: "vitest",
    bin: "vitest.mjs",
  });
  writeFileSync(
    resolve(root, "tools/corepack"),
    String.raw`#!${process.execPath}
    const {appendFileSync}=require("node:fs");
    const name=process.argv.at(-1);
    appendFileSync(process.env.FAKE_LOG, JSON.stringify({prelude:name,NODE_ENV:process.env.NODE_ENV,flag:process.env.PRELUDE_FLAG})+'\n');
    process.exit(process.env.FAIL_PRELUDE === "1" && name === "test:first" ? 7 : 0);
  `,
    { mode: 0o755 },
  );
  const args = [
    resolve(repository, "demo/scripts/package-test.mjs"),
    "tests/a",
    `--outputFile=${resolve(root, "result.json")}`,
  ];
  const inherited = { ...env, NODE_ENV: "caller" };
  const passed = spawnSync(process.execPath, args, {
    cwd,
    env: inherited,
    encoding: "utf8",
  });
  assert.equal(passed.status, 0, passed.stderr);
  assert.deepEqual(calls().slice(0, 2), [
    { prelude: "test:first", NODE_ENV: "caller", flag: "step" },
    { prelude: "test:second", NODE_ENV: "caller" },
  ]);
  assert.equal(calls().at(-1).NODE_ENV, "emulator");
  const count = calls().length;
  const failed = spawnSync(process.execPath, args, {
    cwd,
    env: { ...inherited, FAIL_PRELUDE: "1" },
    encoding: "utf8",
  });
  assert.equal(failed.status, 7, failed.stderr);
  assert.deepEqual(calls().slice(count), [
    { prelude: "test:first", NODE_ENV: "caller", flag: "step" },
  ]);
});
