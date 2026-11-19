import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import {
  cpSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  realpathSync,
  rmSync,
  statSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, relative, resolve } from "node:path";
import test from "node:test";

import { HELP, parse } from "../contrib.mjs";
import { atomicJson, packageByName } from "./files.mjs";
import { fixture } from "./fixture.test-support.mjs";
import { reachedTests, resetReachIndexes } from "./reached.mjs";
import {
  cacheStats,
  clearCache,
  filePassed,
  incrementalChanges,
  pruneCache,
  recordPasses,
  repositorySnapshot,
  runIdentity,
  runnerIdentity,
  runnerSources,
  selectedByFilters,
  uncacheableReason,
  unrecordableReason,
} from "./test-cache.mjs";
import { runTests, testCommand } from "./tests.mjs";
import { summaryLines } from "./vitest-command.mjs";

// A hook running these tests exports GIT_DIR; the fixture's own `git`
// would read the outer repository under it.
for (const key of Object.keys(process.env))
  if (key.startsWith("GIT_")) delete process.env[key];

// A stand-in Vitest whose report has Vitest 5's shape: `list` collects
// tests/*.test.ts under its file filters (a filter selects every path
// containing it, ignoring case, as Vitest's do); `run` logs the files it ran
// and reports one test per file, failing where the file says FAIL or
// FAKE_FAIL names it, skipped where it says SKIP, and a file that failed to
// load (a failed suite with no tests) where it says SETUP. UNHANDLED, or
// FAKE_UNHANDLED, is an error outside every test: the report says success,
// the exit code says failure, as Vitest's do. FAKE_TOUCH names a file the
// run writes back unchanged.
const fakeVitest = `
import {appendFileSync,readdirSync,readFileSync,writeFileSync} from 'node:fs';
import {resolve} from 'node:path';
const [mode, ...rest] = process.argv.slice(2);
const value = (name) => rest.filter((arg) => arg.startsWith('--' + name + '=')).map((arg) => arg.slice(name.length + 3));
const filters = rest.filter((arg) => !arg.startsWith('--')).map((filter) => filter.toLowerCase());
const collected = readdirSync('tests').filter((name) => name.endsWith('.test.ts')).map((name) => 'tests/' + name)
  .filter((file) => !filters.length || filters.some((filter) => file.toLowerCase().includes(filter)));
if (mode === 'list') {
  writeFileSync(value('json')[0], JSON.stringify(collected.map((file) => ({file: resolve(file)}))));
  process.exit(0);
}
appendFileSync(process.env.FAKE_LOG, JSON.stringify(collected) + '\\n');
const named = (process.env.FAKE_FAIL ?? '').split(',').filter(Boolean);
const results = collected.map((file) => {
  const body = readFileSync(file, 'utf8');
  const times = {startTime: 1, endTime: 2};
  if (body.includes('SETUP')) return {name: resolve(file), status: 'failed', message: 'failed to load', ...times, assertionResults: []};
  const status = body.includes('FAIL') || named.some((name) => file.includes(name)) ? 'failed' : body.includes('SKIP') ? 'pending' : 'passed';
  return {name: resolve(file), status: status === 'failed' ? 'failed' : 'passed', message: '', ...times,
    assertionResults: [{status, fullName: file + ' case', failureMessages: []}]};
});
const assertions = results.flatMap((suite) => suite.assertionResults);
const failed = assertions.filter((entry) => entry.status === 'failed').length;
const passed = assertions.filter((entry) => entry.status === 'passed').length;
const failedSuites = results.filter((suite) => suite.status === 'failed').length;
const unhandled = process.env.FAKE_UNHANDLED === '1' || collected.some((file) => readFileSync(file, 'utf8').includes('UNHANDLED'));
if (process.env.FAKE_TOUCH) writeFileSync(process.env.FAKE_TOUCH, readFileSync(process.env.FAKE_TOUCH));
if (collected.some((file) => readFileSync(file, 'utf8').includes('NO_REPORT'))) process.exit(1);
writeFileSync(value('outputFile')[0], JSON.stringify({numTotalTestSuites: results.length, numPassedTestSuites: results.length - failedSuites,
  numFailedTestSuites: failedSuites, numPendingTestSuites: 0, numTotalTests: assertions.length, numPassedTests: passed,
  numFailedTests: failed, success: failedSuites === 0, testResults: results}));
process.exit(failedSuites || unhandled ? 1 : 0);
`;

// The import graph as reached-probe.mjs writes it, read off the fixture's
// text: relative imports are edges, other string literals are spellings.
const fakeProbe = (calls) => async (root, pkg, _command, options) => {
  calls.push({ incremental: Boolean(options.previous) });
  const base = resolve(root, pkg.directory);
  const scripts = [];
  const visit = (directory) => {
    for (const entry of readdirSync(directory, { withFileTypes: true })) {
      const path = resolve(directory, entry.name);
      if (entry.isDirectory()) visit(path);
      else if (/\.ts$/u.test(entry.name)) scripts.push(path);
    }
  };
  for (const directory of ["src", "tests"]) visit(resolve(base, directory));
  const edges = {};
  const spelled = {};
  for (const file of scripts) {
    const text = readFileSync(file, "utf8");
    const imports = [...text.matchAll(/from\s+"(\.[^"]+)"/gu)].map(
      ([, specifier]) =>
        resolve(dirname(file), specifier).replace(/\.js$/u, ".ts"),
    );
    edges[file] = imports;
    spelled[file] = {
      paths: [...text.matchAll(/"([^"]+)"/gu)]
        .map(([, literal]) => literal)
        .filter((literal) => !literal.startsWith(".")),
      walks: false,
    };
  }
  return {
    tests: scripts.filter((file) => file.endsWith(".test.ts")),
    edges,
    failed: {},
    spelled,
  };
};

const workspace = (t, tests, sources = {}) => {
  const root = realpathSync(fixture(t));
  writeFileSync(resolve(root, ".gitignore"), "node_modules/\n/scratch/\n");
  const runner = resolve(root, "demo/node_modules/vitest");
  mkdirSync(runner, { recursive: true });
  atomicJson(resolve(runner, "package.json"), {
    name: "vitest",
    bin: "vitest.mjs",
  });
  writeFileSync(resolve(runner, "vitest.mjs"), fakeVitest);
  mkdirSync(resolve(root, "demo/node_modules/.pnpm"));
  writeFileSync(resolve(root, "demo/node_modules/.pnpm/lock.yaml"), "a: 1\n");
  atomicJson(resolve(root, "demo/package.json"), {
    packageManager: "pnpm@9.15.4",
  });
  const manifest = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(manifest, "utf8"));
  pkg.midgardTasks.test = { command: ["vitest", "run"] };
  atomicJson(manifest, pkg);
  for (const [name, body] of Object.entries({ ...sources, ...tests })) {
    const path = resolve(
      root,
      "demo/example",
      name in tests ? "tests" : "",
      name,
    );
    mkdirSync(dirname(path), { recursive: true });
    writeFileSync(path, body);
  }
  const scratch = resolve(root, "scratch");
  mkdirSync(scratch);
  const log = resolve(scratch, "calls.jsonl");
  writeFileSync(log, "");
  const cache = mkdtempSync(resolve(tmpdir(), "midgard-test-cache-"));
  t.after(() => rmSync(cache, { recursive: true, force: true }));
  const probes = [];
  const options = {
    sourceOnly: true,
    env: { ...process.env, FAKE_LOG: log },
    cached: true,
    cache: { directory: cache, probe: fakeProbe(probes) },
  };
  const runs = () =>
    readFileSync(log, "utf8")
      .trim()
      .split("\n")
      .filter(Boolean)
      .map((line) => JSON.parse(line));
  // The files the last test run ran (none if it ran no Vitest at all).
  const ran = async (extra = {}) => {
    const before = runs().length;
    resetReachIndexes();
    const receipt = await runTests(root, "example", { ...options, ...extra });
    return { receipt, ran: runs().length > before ? runs().at(-1) : [] };
  };
  const write = (path, body) =>
    writeFileSync(resolve(root, "demo/example", path), body);
  return { root, cache, probes, options, ran, write };
};

const ALL = ["tests/a.test.ts", "tests/b.test.ts", "tests/c.test.ts"];

const threeFiles = (t, extraTests = {}) =>
  workspace(
    t,
    {
      "a.test.ts": 'import { one } from "../src/one.js";\none;\n',
      "b.test.ts": 'import { two } from "../src/two.js";\ntwo;\n',
      "c.test.ts": 'const data = "fixtures/table.json";\n',
      ...extraTests,
    },
    {
      "src/one.ts": "export const one = 1;\n",
      "src/two.ts": "export const two = 2;\n",
      "tests/fixtures/table.json": "[1]\n",
      "README.md": "example\n",
    },
  );

test("a second run with nothing changed reuses every pass and runs no Vitest", async (t) => {
  const { ran } = threeFiles(t);
  const first = await ran();
  assert.equal(first.receipt.status, "passed", first.receipt.reason);
  assert.deepEqual(first.ran, ALL);
  assert.equal(first.receipt.cache.recorded, 3);
  const second = await ran();
  assert.equal(second.receipt.status, "passed", second.receipt.reason);
  assert.equal(second.receipt.exitCode, 0);
  assert.deepEqual(second.ran, []);
  assert.equal(second.receipt.cache.ran, 0);
  assert.deepEqual(
    second.receipt.cache.reused.map(({ file }) => file),
    ALL,
  );
  // Each reuse names the run whose report holds the pass.
  for (const entry of second.receipt.cache.reused)
    assert.equal(entry.from.receipt, first.receipt.path);
  assert.match(
    summaryLines(second.receipt).join("\n"),
    /cache: 0 file\(s\) ran, 3 reused from 1 earlier run/u,
  );
  // Nothing ran, so there is nothing to explain as not recorded.
  assert.equal(second.receipt.cache.notRecorded, undefined);
});

test("a changed dependency reruns exactly the files that reach it, as --related selects them", async (t) => {
  const { root, ran, write, options } = threeFiles(t);
  await ran();
  write("src/one.ts", "export const one = 11;\n");
  const second = await ran();
  assert.deepEqual(second.ran, ["tests/a.test.ts"]);
  assert.deepEqual(
    second.receipt.cache.reused.map(({ file }) => file),
    ["tests/b.test.ts", "tests/c.test.ts"],
  );
  resetReachIndexes();
  const pkg = packageByName(root, "example");
  const related = await reachedTests(
    root,
    pkg,
    testCommand(pkg),
    ["demo/example/src/one.ts"],
    {
      env: options.env,
      directory: mkdtempSync(resolve(root, "scratch/reach-")),
      probe: options.cache.probe,
    },
  );
  assert.deepEqual(related.files, second.ran);
});

test("an unrelated change reruns nothing", async (t) => {
  const { ran, write } = threeFiles(t);
  await ran();
  write("README.md", "example, explained\n");
  write("src/two.ts", "export const two = 2;\n// same\n");
  const second = await ran();
  assert.deepEqual(second.ran, ["tests/b.test.ts"]);
  write("src/extra.ts", "export const unused = 0;\n");
  const third = await ran();
  assert.deepEqual(third.ran, []);
  assert.equal(third.receipt.cache.reused.length, 3);
});

test("a changed fixture a test reads by name reruns that test", async (t) => {
  const { ran, write } = threeFiles(t);
  await ran();
  write("tests/fixtures/table.json", "[2]\n");
  const second = await ran();
  assert.deepEqual(second.ran, ["tests/c.test.ts"]);
});

test("a changed test file reruns itself only", async (t) => {
  const { ran, write } = threeFiles(t);
  await ran();
  write("tests/b.test.ts", 'import { two } from "../src/two.js";\ntwo;\n//\n');
  assert.deepEqual((await ran()).ran, ["tests/b.test.ts"]);
});

test("a changed environment, profile, lockfile or artifact reruns everything", async (t) => {
  const { root, ran, options } = threeFiles(t);
  await ran();
  assert.deepEqual(
    (await ran({ env: { ...options.env, MIDGARD_EXAMPLE: "1" } })).ran,
    ALL,
  );
  writeFileSync(resolve(root, "demo/node_modules/.pnpm/lock.yaml"), "a: 2\n");
  assert.deepEqual((await ran()).ran, ALL);
  // A compiled prerequisite, read by content.
  assert.deepEqual((await ran({ sourceOnly: false })).ran, ALL);
  writeFileSync(
    resolve(root, "demo/example/dist/index.js"),
    "export const x = 2;\n",
  );
  assert.deepEqual((await ran({ sourceOnly: false })).ran, ALL);
  assert.deepEqual((await ran({ sourceOnly: false })).ran, []);
  // Identical inputs but another seed: the files ran in another order.
  assert.deepEqual((await ran({ sourceOnly: false, seed: 2 })).ran, ALL);
});

test("the run key binds tier, environment, Node, installed packages and artifacts", (t) => {
  const root = realpathSync(fixture(t));
  const pkg = packageByName(root, "example");
  const command = {
    env: {},
    exclude: [],
    disableConsoleIntercept: false,
    preludes: [],
  };
  const base = {
    root,
    pkg,
    command,
    seed: 1,
    sourceOnly: false,
    env: { PATH: "/bin", UNRELATED_SESSION_THING: "a" },
    artifacts: { blueprint: "x" },
    runner: "r",
  };
  const key = (change) => runIdentity({ ...base, ...change }).sha256;
  assert.equal(key({}), key({}));
  assert.notEqual(key({}), key({ tier: "fast" }));
  assert.notEqual(key({ tier: "fast" }), key({ tier: "slow" }));
  assert.notEqual(key({}), key({ env: { ...base.env, PATH: "/usr/bin" } }));
  assert.notEqual(key({}), key({ env: { ...base.env, MIDGARD_X: "1" } }));
  assert.notEqual(key({}), key({ env: { ...base.env, TZ: "UTC" } }));
  assert.notEqual(key({}), key({ artifacts: { blueprint: "y" } }));
  assert.notEqual(key({}), key({ postgres: { version: "PostgreSQL 16" } }));
  assert.notEqual(
    key({}),
    key({ command: { ...command, env: { NODE_ENV: "emulator" } } }),
  );
  // Every variable is bound, spelled by a repository script or not: Node,
  // the loader and the libraries a suite loads read their own.
  for (const name of [
    "UNRELATED_SESSION_THING",
    "LD_PRELOAD",
    "http_proxy",
    "NODE_OPTIONS",
  ])
    assert.notEqual(
      key({}),
      key({ env: { ...base.env, [name]: "b" } }),
      `${name} is bound`,
    );
  // Only the documented per-session and per-invocation names are not.
  for (const name of [
    "PWD",
    "OLDPWD",
    "SHLVL",
    "_",
    "SSH_TTY",
    "SSH_CONNECTION",
    "INVOCATION_ID",
    "TERM_SESSION_ID",
    "MIDGARD_TEST_DATABASE_PREFIX",
    "MIDGARD_PUBLISHED_WORKFLOW_RECEIPT_PATH",
  ])
    assert.equal(
      key({}),
      key({ env: { ...base.env, [name]: "p" } }),
      `${name} is not bound`,
    );
  // The runner's own logic is bound.
  assert.notEqual(key({}), key({ runner: "s" }));
  // Values are bound by digest only.
  const parts = runIdentity({
    ...base,
    env: { ...base.env, MIDGARD_SECRET: "hunter2" },
  }).parts;
  assert.ok(!JSON.stringify(parts).includes("hunter2"));
});

test("the run key holds the content of every script that decides what runs or records", (t) => {
  const contrib = dirname(new URL(import.meta.url).pathname);
  const copy = mkdtempSync(resolve(tmpdir(), "midgard-runner-"));
  t.after(() => rmSync(copy, { recursive: true, force: true }));
  const sources = runnerSources();
  const names = sources.map((path) =>
    relative(resolve(contrib, "../.."), path),
  );
  for (const name of [
    "scripts/contrib/tests.mjs",
    "scripts/contrib/vitest-command.mjs",
    "scripts/contrib/test-cache.mjs",
    "scripts/contrib/test-cache-key.mjs",
    "scripts/contrib/test-cache-store.mjs",
    "scripts/contrib/receipts.mjs",
    "scripts/contrib/reached.mjs",
    "scripts/contrib/reached-probe.mjs",
    "scripts/contrib.mjs",
    "demo/scripts/package-tasks.mjs",
  ])
    assert.ok(names.includes(name), name);
  assert.ok(!names.some((name) => /\.test[.-]/u.test(name)));
  const copies = sources.map((path, index) => {
    const target = resolve(
      copy,
      String(index),
      relative(resolve(contrib, "../.."), path),
    );
    mkdirSync(dirname(target), { recursive: true });
    cpSync(path, target);
    return target;
  });
  assert.equal(runnerIdentity(copies), runnerIdentity(sources));
  const recording = copies.find((path) => path.endsWith("test-cache.mjs"));
  writeFileSync(recording, `${readFileSync(recording, "utf8")}\n// changed\n`);
  assert.notEqual(runnerIdentity(copies), runnerIdentity(sources));
});

test("a failed, skipped, unreported or partial run records nothing it cannot vouch for", async (t) => {
  const { ran, write } = threeFiles(t, {
    "d.test.ts": "FAIL\n",
    "e.test.ts": "SKIP\n",
  });
  const first = await ran();
  assert.equal(first.receipt.status, "failed");
  assert.equal(first.receipt.exitCode, 1);
  // Any failure and the run vouches for nothing: no pass beside it records.
  assert.equal(first.receipt.cache.recorded, 0);
  assert.match(first.receipt.cache.notRecorded, /Vitest exited 1/u);
  const second = await ran();
  assert.deepEqual(second.ran, [...ALL, "tests/d.test.ts", "tests/e.test.ts"]);
  assert.equal(second.receipt.exitCode, 1);
  // A file that fails forgets its earlier pass.
  write("tests/d.test.ts", "fine\n");
  write("tests/e.test.ts", "fine\n");
  assert.equal((await ran()).receipt.cache.recorded, 5);
  write("src/two.ts", "export const two = 3;\n");
  write("tests/b.test.ts", 'import { two } from "../src/two.js";\nFAIL\n');
  assert.deepEqual((await ran()).ran, ["tests/b.test.ts"]);
  write("tests/b.test.ts", 'import { two } from "../src/two.js";\ntwo;\n');
  write("src/two.ts", "export const two = 2;\n");
  assert.deepEqual(
    (await ran()).ran,
    ["tests/b.test.ts"],
    "the earlier pass of the same content was forgotten",
  );
  // A file that fails to load: nothing records.
  write("src/one.ts", "export const one = 12;\n");
  write("tests/d.test.ts", "SETUP\n");
  const setup = await ran();
  assert.deepEqual(setup.ran, ["tests/a.test.ts", "tests/d.test.ts"]);
  assert.equal(setup.receipt.cache.recorded, 0);
  // No report: nothing records.
  write("tests/d.test.ts", "NO_REPORT\n");
  const unreported = await ran();
  assert.equal(unreported.receipt.status, "failed");
  assert.equal(unreported.receipt.cache.recorded, 0);
  write("tests/d.test.ts", "fine\n");
  assert.deepEqual((await ran()).ran, ["tests/a.test.ts", "tests/d.test.ts"]);
});

test("an error outside every test fails the exit code alone, and records nothing", async (t) => {
  const { ran, options } = threeFiles(t);
  const env = { ...options.env, FAKE_UNHANDLED: "1" };
  const first = await ran({ env });
  // Vitest's report says success; its exit code does not.
  assert.equal(first.receipt.status, "failed");
  assert.equal(first.receipt.cache.recorded, 0);
  assert.match(first.receipt.cache.notRecorded, /Vitest exited 1/u);
  assert.deepEqual((await ran({ env })).ran, ALL);
});

test("a failure in a run without --cached forgets the passes it recorded, under every run key", async (t) => {
  const { ran, options } = threeFiles(t);
  await ran();
  await ran({ tier: undefined, seed: 2 });
  assert.deepEqual((await ran()).ran, []);
  assert.deepEqual((await ran({ seed: 2 })).ran, []);
  // The same content fails without --cached (a flaky or environment-bound
  // test): its passes under every key are forgotten.
  const plain = await ran({
    cached: false,
    env: { ...options.env, FAKE_FAIL: "b.test" },
  });
  assert.equal(plain.receipt.status, "failed");
  assert.deepEqual(plain.ran, ALL);
  assert.deepEqual((await ran()).ran, ["tests/b.test.ts"]);
  assert.deepEqual((await ran({ seed: 2 })).ran, ["tests/b.test.ts"]);
  // An exit code with no file to blame forgets every file the run ran.
  const unblamed = await ran({
    cached: false,
    files: ["tests/a.test.ts", "tests/c.test.ts"],
    env: { ...options.env, FAKE_UNHANDLED: "1" },
  });
  assert.equal(unblamed.receipt.status, "failed");
  assert.deepEqual((await ran()).ran, ["tests/a.test.ts", "tests/c.test.ts"]);
});

test("Vitest itself: an unhandled rejection after a pass, or a file that fails to import, records nothing", async (t) => {
  const root = realpathSync(fixture(t));
  // The checkout's own Vitest, copied where the runner requires it to live
  // (inside the checkout), its dependencies linked beside it.
  writeFileSync(resolve(root, ".gitignore"), "node_modules/\n");
  atomicJson(resolve(root, "demo/package.json"), {
    packageManager: "pnpm@9.15.4",
  });
  const installed = realpathSync(
    resolve(
      dirname(new URL(import.meta.url).pathname),
      "../../demo/node_modules/vitest",
    ),
  );
  const modules = resolve(root, "demo/node_modules");
  mkdirSync(resolve(modules, ".pnpm"), { recursive: true });
  writeFileSync(resolve(modules, ".pnpm/lock.yaml"), "a: 1\n");
  cpSync(installed, resolve(modules, "vitest"), { recursive: true });
  for (const name of readdirSync(dirname(installed)))
    if (name !== "vitest")
      symlinkSync(resolve(dirname(installed), name), resolve(modules, name));
  const manifest = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(manifest, "utf8"));
  pkg.midgardTasks.test = { command: ["vitest", "run"] };
  atomicJson(manifest, pkg);
  mkdirSync(resolve(root, "demo/example/tests"));
  const write = (name, body) =>
    writeFileSync(resolve(root, "demo/example/tests", name), body);
  write("a.test.ts", 'import { it } from "vitest";\nit("passes", () => {});\n');
  write(
    "b.test.ts",
    [
      'import { it } from "vitest";',
      'it("passes then leaks", () => {',
      '  void Promise.reject(new Error("after the test"));',
      "});",
      "",
    ].join("\n"),
  );
  const cache = mkdtempSync(resolve(tmpdir(), "midgard-test-cache-"));
  t.after(() => rmSync(cache, { recursive: true, force: true }));
  const options = {
    sourceOnly: true,
    cached: true,
    cache: { directory: cache, probe: fakeProbe([]) },
  };
  resetReachIndexes();
  const unhandled = await runTests(root, "example", options);
  assert.equal(unhandled.status, "failed", unhandled.reason);
  assert.equal(unhandled.counts.failed, 0, "every test passed");
  assert.equal(unhandled.cache.ran, 2);
  assert.equal(unhandled.cache.recorded, 0);
  write(
    "b.test.ts",
    'import { it } from "vitest";\nimport "./missing.js";\nit("never", () => {});\n',
  );
  resetReachIndexes();
  const unloadable = await runTests(root, "example", options);
  assert.equal(unloadable.status, "failed");
  assert.equal(unloadable.cache.ran, 2);
  assert.equal(unloadable.cache.recorded, 0);
  assert.equal(cacheStats(cache).passes, 0);
});

test("--cached refuses --name, which runs part of a file", async (t) => {
  const { ran } = threeFiles(t);
  await assert.rejects(ran({ testName: "case" }), /--name runs part/u);
  assert.equal(parse(["test", "--cached"]).cached, true);
  assert.match(HELP, /\[--cached\]/u);
  assert.match(HELP, /test-cache stats \| clear/u);
});

test("--file and --related narrow a cached run as they narrow any run", async (t) => {
  const { ran, write } = threeFiles(t);
  const named = await ran({ files: ["tests/a.test.ts", "tests/b.test.ts"] });
  assert.deepEqual(named.ran, ["tests/a.test.ts", "tests/b.test.ts"]);
  const again = await ran({ files: ["tests/a.test.ts", "tests/b.test.ts"] });
  assert.deepEqual(again.ran, []);
  write("src/one.ts", "export const one = 13;\n");
  const related = await ran({ related: ["demo/example/src/one.ts"] });
  assert.deepEqual(related.ran, ["tests/a.test.ts"]);
  assert.deepEqual(
    (await ran({ related: ["demo/example/src/one.ts"] })).ran,
    [],
  );
});

test("files drawing random inputs always run", async (t) => {
  const { ran } = threeFiles(t, {
    "r.test.ts": 'import { pick } from "./support/pick.js";\npick();\n',
    "support/pick.ts": "export const pick = () => Math.random();\n",
  });
  await ran();
  const second = await ran();
  assert.deepEqual(second.ran, ["tests/r.test.ts"]);
  assert.match(
    second.receipt.cache.uncacheable["tests/r.test.ts"],
    /random inputs/u,
  );
});

test("unseeded randomness in a test or the test-side code it imports marks it; seeded fuzzing does not", (t) => {
  const root = realpathSync(fixture(t));
  const put = (path, body) => {
    mkdirSync(dirname(resolve(root, path)), { recursive: true });
    writeFileSync(resolve(root, path), body);
  };
  const manifest = resolve(root, "demo/example/package.json");
  atomicJson(manifest, {
    ...JSON.parse(readFileSync(manifest, "utf8")),
    imports: { "#support/*": "./tests/support/*.ts" },
  });
  put(
    "demo/midgard-test-support/package.json",
    JSON.stringify({
      name: "@al-ft/midgard-test-support",
      type: "module",
      exports: {
        "./fuzz": "./src/fuzz.ts",
        "./keys": { types: "./src/keys.d.ts", import: "./src/keys.ts" },
      },
    }),
  );
  // Seeded by the test: the same cases every run.
  put(
    "demo/midgard-test-support/src/fuzz.ts",
    [
      "export const makeFuzzRng = (seed) => () => (seed = (seed * 48271) % 2147483647);",
      "export const randomInteger = (rng, max) => rng() % max;",
      "",
    ].join("\n"),
  );
  put(
    "demo/midgard-test-support/src/keys.ts",
    'import { generatePrivateKey } from "@lucid-evolution/lucid";\nexport const key = () => generatePrivateKey();\n',
  );
  put(
    "demo/example/tests/support/ids.ts",
    'import { randomUUID } from "node:crypto";\nexport const id = () => randomUUID();\n',
  );
  put(
    "demo/example/src/nonce.ts",
    'import { randomBytes } from "node:crypto";\nexport const nonce = () => randomBytes(8);\n',
  );
  const cases = {
    "tests/bytes.test.ts":
      'import { randomBytes } from "node:crypto";\nrandomBytes(32);\n',
    "tests/seed.test.ts": "generateSeedPhrase();\n",
    "tests/alias.test.ts": 'import { id } from "#support/ids";\nid();\n',
    "tests/keys.test.ts":
      'import { key } from "@al-ft/midgard-test-support/keys";\nkey();\n',
    "tests/fuzz.test.ts":
      'import { makeFuzzRng, randomInteger } from "@al-ft/midgard-test-support/fuzz";\nrandomInteger(makeFuzzRng(7), 10);\n',
    "tests/product.test.ts":
      'import { nonce } from "../src/nonce.js";\nnonce();\n',
  };
  for (const [file, body] of Object.entries(cases))
    put(`demo/example/${file}`, body);
  const pkg = packageByName(root, "example");
  const reason = (file) => uncacheableReason(root, pkg, file);
  assert.match(reason("tests/bytes.test.ts"), /random inputs/u);
  assert.match(reason("tests/seed.test.ts"), /random inputs/u);
  assert.match(reason("tests/alias.test.ts"), /tests\/support\/ids\.ts draws/u);
  assert.match(
    reason("tests/keys.test.ts"),
    /midgard-test-support\/src\/keys\.ts draws/u,
  );
  // The fuzz helper draws from the generator the test seeds.
  assert.equal(reason("tests/fuzz.test.ts"), undefined);
  // The code under test is what the test checks, not where it draws inputs.
  assert.equal(reason("tests/product.test.ts"), undefined);
});

test("a suite gated on the machine or a global setup always runs, and --cached refuses another config", async (t) => {
  const { root, ran } = threeFiles(t, {
    "devnet.test.ts":
      'describe.skipIf(!process.env.DEVNET)("devnet", () => {});\n',
    "docker.test.ts":
      'import { needsDocker } from "./support/gate.js";\nneedsDocker();\n',
    "support/gate.ts":
      "export const needsDocker = () => it.runIf(hasDocker)('x', () => {});\n",
    "binary.test.ts": 'const bin = inject("binary");\n',
    "dynamic.test.ts": "it('x', (context) => { context.skip(!ready); });\n",
  });
  const pkg = packageByName(root, "example");
  for (const file of [
    "tests/devnet.test.ts",
    "tests/docker.test.ts",
    "tests/binary.test.ts",
    "tests/dynamic.test.ts",
  ])
    assert.match(
      uncacheableReason(root, pkg, file),
      /decides at run time|global setup/u,
      file,
    );
  // A plain skip decides nothing at run time.
  writeFileSync(
    resolve(root, "demo/example/tests/plain.test.ts"),
    'it.skip("later", () => {});\n',
  );
  assert.equal(uncacheableReason(root, pkg, "tests/plain.test.ts"), undefined);
  await ran();
  assert.deepEqual([...(await ran()).ran].sort(), [
    "tests/binary.test.ts",
    "tests/devnet.test.ts",
    "tests/docker.test.ts",
    "tests/dynamic.test.ts",
  ]);
  writeFileSync(
    resolve(root, "demo/example/vitest.journeys.config.ts"),
    "export default {};\n",
  );
  await assert.rejects(
    ran({
      flags: { config: "vitest.journeys.config.ts" },
      files: ["tests/a.test.ts"],
    }),
    /--cached is for the package's default config/u,
  );
});

test("a package that runs files without isolation records nothing", (t) => {
  const root = realpathSync(fixture(t));
  const pkg = packageByName(root, "example");
  assert.equal(unrecordableReason(root, pkg), undefined);
  writeFileSync(
    resolve(root, "demo/example/vitest.config.ts"),
    'export default { test: { pool: "threads", isolate: false } };\n',
  );
  assert.match(unrecordableReason(root, pkg), /without isolation/u);
  rmSync(resolve(root, "demo/example/vitest.config.ts"));
  assert.match(
    unrecordableReason(root, {
      ...pkg,
      scripts: { test: "vitest run --no-isolate" },
    }),
    /without isolation/u,
  );
});

test("a graph probed while the checkout changed is neither trusted nor stored", async (t) => {
  const { cache, ran, write } = threeFiles(t);
  await ran();
  write("src/two.ts", "export const two = 22;\n");
  // The probe reads the files, then c starts importing what changed.
  const calls = [];
  const probe = async (...args) => {
    const graph = await fakeProbe(calls)(...args);
    if (calls.length === 1)
      write("tests/c.test.ts", 'import { two } from "../src/two.js";\ntwo;\n');
    return graph;
  };
  const second = await ran({ cache: { directory: cache, probe } });
  assert.equal(calls.length, 1);
  assert.deepEqual(second.ran, ALL);
  assert.equal(second.receipt.cache.graph.unstable, true);
  const graphs = resolve(cache, "graphs");
  const stored = (
    existsSync(graphs) ? readdirSync(graphs, { recursive: true }) : []
  ).filter(
    (name) =>
      String(name).endsWith(".json") && !String(name).endsWith("latest.json"),
  );
  const ids = stored.map((name) =>
    String(name)
      .split("/")
      .at(-1)
      .replace(/\.json$/u, ""),
  );
  assert.ok(
    !ids.includes(second.receipt.cache.snapshot),
    "no graph under the probed snapshot",
  );
});

test("a file written back unchanged while a run ran keeps the run from recording", async (t) => {
  const { root, ran, options } = threeFiles(t);
  const touched = await ran({
    env: {
      ...options.env,
      FAKE_TOUCH: resolve(root, "demo/example/src/two.ts"),
    },
  });
  assert.equal(touched.receipt.status, "passed", touched.receipt.reason);
  assert.equal(touched.receipt.cache.recorded, 0);
  assert.match(
    touched.receipt.cache.notRecorded,
    /written while it ran \(demo\/example\/src\/two\.ts\)/u,
  );
  assert.equal((await ran()).receipt.cache.recorded, 3);
});

test("a changed data file outside every package reruns everything", async (t) => {
  const { root, ran } = threeFiles(t);
  mkdirSync(resolve(root, "config"));
  writeFileSync(resolve(root, "config/protocol.json"), "{}\n");
  await ran();
  assert.deepEqual((await ran()).ran, []);
  writeFileSync(resolve(root, "config/protocol.json"), '{"a":1}\n');
  const second = await ran();
  assert.deepEqual(second.ran, ALL);
  assert.match(
    Object.values(second.receipt.cache.runReasons).join("\n"),
    /config\/protocol\.json is data outside every package/u,
  );
});

test("a pass is reused from another checkout of the same content", async (t) => {
  const { root, ran, options } = threeFiles(t);
  const first = await ran();
  const copy = realpathSync(
    mkdtempSync(resolve(tmpdir(), "midgard-contrib-copy-")),
  );
  t.after(() => rmSync(copy, { recursive: true, force: true }));
  cpSync(root, copy, { recursive: true });
  resetReachIndexes();
  const other = await runTests(copy, "example", options);
  assert.equal(other.cache.ran, 0);
  assert.equal(other.cache.reused[0].from.receipt, first.receipt.path);
});

test("the graph is probed once per content, and a script-only change regraphs incrementally", async (t) => {
  const { root, ran, write, probes } = threeFiles(t);
  await ran();
  write("src/one.ts", "export const one = 14;\n");
  await ran();
  await ran();
  assert.deepEqual(probes, [{ incremental: false }]);
  write("src/one.ts", "export const one = 15;\n");
  write("src/two.ts", "export const two = 5;\n");
  const regraphed = await ran();
  assert.deepEqual(regraphed.ran, ["tests/a.test.ts", "tests/b.test.ts"]);
  assert.deepEqual(probes.at(-1), { incremental: true });
  // A new file is a new file set: a full probe.
  write("src/three.ts", "export const three = 3;\n");
  write("src/one.ts", "export const one = 16;\n");
  await ran();
  assert.deepEqual(probes.at(-1), { incremental: false });
  const before = repositorySnapshot(root);
  write("tests/fixtures/table.json", "[3]\n");
  assert.equal(
    incrementalChanges(root, before, repositorySnapshot(root)),
    undefined,
  );
});

test("Vitest's filters select by substring, so a run of misses widens to what they select", () => {
  const cwd = "/p";
  const collected = ["/p/tests/a.test.ts", "/p/tests/x/tests/a.test.ts"];
  assert.deepEqual(
    selectedByFilters(cwd, collected, ["tests/A.test.ts"]),
    collected,
  );
  assert.deepEqual(selectedByFilters(cwd, collected, ["x/tests/a.test.ts"]), [
    collected[1],
  ]);
});

test("only a file whose every test ran and passed counts as passed", () => {
  const passed = { status: "passed" };
  assert.equal(
    filePassed({ status: "passed", assertionResults: [passed] }),
    true,
  );
  assert.equal(filePassed({ status: "passed", assertionResults: [] }), false);
  for (const status of ["skipped", "pending", "todo", "failed"])
    assert.equal(
      filePassed({ status: "passed", assertionResults: [passed, { status }] }),
      false,
      status,
    );
});

test("the evidence-writing publication driver never reuses a pass", (t) => {
  const root = realpathSync(fixture(t));
  const pkg = packageByName(root, "example");
  mkdirSync(resolve(root, "demo/example/tests"));
  writeFileSync(resolve(root, "demo/example/tests/p.test.ts"), "\n");
  assert.match(
    uncacheableReason(root, pkg, "tests/p.test.ts", {
      evidence: ["tests/p.test.ts"],
    }),
    /evidence/u,
  );
  writeFileSync(
    resolve(root, "demo/example/tests/q.test.ts"),
    "it('x', { retry: 2 }, () => {});\n",
  );
  assert.match(uncacheableReason(root, pkg, "tests/q.test.ts"), /retries/u);
});

test("concurrent writers, forgetters and pruners leave every stored file whole", async () => {
  const directory = mkdtempSync(resolve(tmpdir(), "midgard-test-cache-"));
  try {
    const module = new URL("./test-cache.mjs", import.meta.url).href;
    const worker = `
      const { recordPasses, forgetFiles, pruneCache } = await import(${JSON.stringify(module)});
      const directory = process.argv[1];
      const id = Number(process.argv[2]);
      for (let round = 0; round < 40; round += 1) {
        const snapshot = { id: 's' + (round % 5), files: { a: String(round) } };
        recordPasses({ directory, runKey: 'k', packageName: 'p', snapshot,
          passes: [0, 1, 2, 3].map((n) => ({ file: 'tests/' + n + '.test.ts', assertions: 1, durationMs: 1 })),
          evidence: { worker: id } });
        if (round % 7 === id % 7) forgetFiles({ directory, runKey: 'k', files: ['tests/1.test.ts'] });
        if (round % 11 === 0) pruneCache(directory, { force: true, maxBytes: 4096 });
      }
    `;
    await Promise.all(
      Array.from(
        { length: 6 },
        (_, index) =>
          new Promise((done, fail) => {
            const child = spawn(
              process.execPath,
              ["--input-type=module", "-e", worker, directory, String(index)],
              { stdio: ["ignore", "ignore", "pipe"] },
            );
            let stderr = "";
            child.stderr.on("data", (chunk) => (stderr += chunk));
            child.on("exit", (code) =>
              code === 0 ? done() : fail(new Error(stderr)),
            );
          }),
      ),
    );
    const visit = (path) => {
      for (const entry of readdirSync(path, { withFileTypes: true })) {
        const child = resolve(path, entry.name);
        if (entry.isDirectory()) visit(child);
        else if (entry.name.endsWith(".json"))
          assert.doesNotThrow(
            () => JSON.parse(readFileSync(child, "utf8")),
            relative(directory, child),
          );
      }
    };
    visit(directory);
    assert.ok(cacheStats(directory).passes > 0);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});

test("the store is owner-only, bounded by age and size, and clears with one command", (t) => {
  const directory = mkdtempSync(resolve(tmpdir(), "midgard-test-cache-"));
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  const snapshot = { id: "s", files: {} };
  recordPasses({
    directory,
    runKey: "k",
    packageName: "p",
    snapshot,
    passes: [{ file: "tests/a.test.ts", assertions: 1, durationMs: 1 }],
    evidence: {},
  });
  const stored = resolve(directory, "snapshots/s.json");
  assert.equal(statSync(stored).mode & 0o077, 0);
  assert.deepEqual(
    {
      passes: cacheStats(directory).passes,
      snapshots: cacheStats(directory).snapshots,
    },
    { passes: 1, snapshots: 1 },
  );
  // Within the interval a prune does nothing; forced, it drops what expired.
  pruneCache(directory, { force: true });
  assert.equal(cacheStats(directory).passes, 1);
  pruneCache(directory, { force: true, now: Date.now() + 8 * 24 * 3_600_000 });
  assert.equal(cacheStats(directory).passes, 0);
  recordPasses({
    directory,
    runKey: "k",
    packageName: "p",
    snapshot,
    passes: [{ file: "tests/a.test.ts", assertions: 1, durationMs: 1 }],
    evidence: {},
  });
  pruneCache(directory, { force: true, maxBytes: 0 });
  assert.equal(cacheStats(directory).bytes, 0);
  recordPasses({
    directory,
    runKey: "k",
    packageName: "p",
    snapshot,
    passes: [{ file: "tests/a.test.ts", assertions: 1, durationMs: 1 }],
    evidence: {},
  });
  assert.equal(clearCache(directory).passes, 1);
  assert.equal(cacheStats(directory).passes, 0);
});
