import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import {
  chmodSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import {
  SDK_SUITES,
  sdkContext,
  sdkReportCounts,
} from "./sdk-suite-evidence.mjs";
import { spawnStep, runPreflight } from "./run.mjs";

// A hook running these tests exports GIT_DIR and GIT_INDEX_FILE. Inherited
// by `git init`, they re-initialise the outer repository instead of the
// temporary one.
for (const key of Object.keys(process.env))
  if (key.startsWith("GIT_")) delete process.env[key];

const withFixture = async (body) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-sdk-evidence-"));
  const write = (path, value) => {
    mkdirSync(dirname(resolve(root, path)), { recursive: true });
    writeFileSync(resolve(root, path), value);
  };
  try {
    execFileSync("git", ["init", "--quiet"], { cwd: root });
    write(
      "demo/package.json",
      JSON.stringify({
        packageManager: "pnpm@9.15.4",
        scripts: {
          "test:tx-prep:sdk":
            "pnpm --filter @al-ft/lucid-midgard test && pnpm --filter @al-ft/midgard-sdk test",
        },
      }),
    );
    write("demo/pnpm-lock.yaml", "frozen dependencies");
    write(
      "demo/pnpm-workspace.yaml",
      "packages:\n - lucid-midgard\n - midgard-sdk\n",
    );
    write("onchain/aiken/plutus.json", "blueprint");
    write("onchain/aiken/plutus.json.deployment.json", "stamp");
    write(
      "demo/node_modules/.pnpm/dependency/index.js",
      "installed dependency",
    );
    for (const [name, directory] of Object.entries(SDK_SUITES)) {
      write(
        `${directory}/package.json`,
        JSON.stringify({ name, scripts: { test: "vitest run" } }),
      );
      write(`${directory}/src/index.ts`, "source");
      write(`${directory}/dist/index.js`, "compiled runtime");
      write(`${directory}/vitest.config.ts`, "config");
      write(`${directory}/tests/example.test.ts`, "full suite");
    }
    return await body({ root, write });
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
};
const report = (root, name) => ({
  success: true,
  numTotalTests: 1,
  numPassedTests: 1,
  numFailedTests: 0,
  testResults: [
    {
      name: resolve(root, SDK_SUITES[name], "tests/example.test.ts"),
      status: "passed",
      assertionResults: [{ title: "case", status: "passed" }],
    },
  ],
});

test("SDK context binds actual source, installed and compiled dependencies, config, blueprint and environment", () =>
  withFixture(({ root, write }) => {
    const env = { NODE_OPTIONS: "", PATH: process.env.PATH };
    const before = sdkContext(root, env);
    for (const path of [
      "demo/midgard-sdk/src/index.ts",
      "demo/lucid-midgard/dist/index.js",
      "demo/node_modules/.pnpm/dependency/index.js",
      "demo/midgard-sdk/vitest.config.ts",
      "onchain/aiken/plutus.json",
      "onchain/aiken/plutus.json.deployment.json",
      "demo/pnpm-lock.yaml",
    ]) {
      const original = readFileSync(resolve(root, path));
      write(path, "changed");
      assert.notEqual(sdkContext(root, env), before, path);
      write(path, original);
    }
    assert.notEqual(
      sdkContext(root, { ...env, NODE_OPTIONS: "--conditions=other" }),
      before,
    );
    write(".git/info/exclude", "demo/private-sdk-input/\n");
    write(
      "demo/private-sdk-input/package.json",
      JSON.stringify({ name: "private", directory: "demo/midgard-sdk" }),
    );
    write("demo/private-sdk-input/src/index.ts", "ignored physical source");
    write("demo/private-sdk-input/dist/index.js", "ignored physical runtime");
    const withPrivate = sdkContext(root, env);
    for (const path of [
      "demo/private-sdk-input/src/index.ts",
      "demo/private-sdk-input/dist/index.js",
    ]) {
      const original = readFileSync(resolve(root, path));
      write(path, "changed");
      assert.notEqual(sdkContext(root, env), withPrivate, path);
      write(path, original);
    }
    write(
      "demo/midgard-sdk/node_modules/.vite/vitest/results.json",
      "generated output",
    );
    // Creating the installation directory changes its missing/present identity;
    // subsequent cache bytes themselves must not count as dependency changes.
    const withCache = sdkContext(root, env);
    write(
      "demo/midgard-sdk/node_modules/.vite/vitest/results.json",
      "updated cache",
    );
    assert.equal(sdkContext(root, env), withCache);
    write(
      "demo/midgard-sdk/node_modules/.vite/deps/runtime.js",
      "runtime cache",
    );
    const executableCache = sdkContext(root, env);
    write(
      "demo/midgard-sdk/node_modules/.vite/deps/runtime.js",
      "changed runtime cache",
    );
    assert.notEqual(sdkContext(root, env), executableCache);
    assert.throws(() =>
      sdkContext(root, { ...env, MIDGARD_BLUEPRINT_STAMP: "warn" }),
    );
    assert.throws(() =>
      sdkContext(root, {
        ...env,
        MIDGARD_REAL_BLUEPRINT_PATH: "/other/blueprint",
      }),
    );
    rmSync(resolve(root, "demo/midgard-sdk/package.json"));
    assert.throws(() => sdkContext(root, env));
  }));

test("dependency links cannot hide changed runtime bytes in outputs or omitted caches", () =>
  withFixture(({ root, write }) => {
    write("demo/midgard-sdk/coverage/private-hashes/index.js", "runtime");
    mkdirSync(resolve(root, "demo/midgard-sdk/node_modules"), {
      recursive: true,
    });
    const link = resolve(root, "demo/midgard-sdk/node_modules/hashes");
    symlinkSync("../coverage/private-hashes", link);
    const before = sdkContext(root, {});
    write(
      "demo/midgard-sdk/coverage/private-hashes/index.js",
      "changed runtime",
    );
    assert.notEqual(sdkContext(root, {}), before);
    rmSync(link);
    write(
      "demo/midgard-sdk/node_modules/.vite/private-hashes/index.js",
      "cache runtime",
    );
    symlinkSync(".vite/private-hashes", link);
    assert.throws(() => sdkContext(root, {}), /bound closure/);
  }));

test("dependency route resolution preserves symlink parent semantics and refuses missing targets", () =>
  withFixture(({ root, write }) => {
    write("node_modules/external-hashes/index.js", "unbound runtime");
    write(".git/info/exclude", "node_modules/\n");
    mkdirSync(resolve(root, "demo/midgard-sdk/coverage"), { recursive: true });
    mkdirSync(resolve(root, "demo/midgard-sdk/node_modules"), {
      recursive: true,
    });
    symlinkSync("../..", resolve(root, "demo/midgard-sdk/coverage/alias"));
    const link = resolve(root, "demo/midgard-sdk/node_modules/hashes");
    symlinkSync("../coverage/alias/../node_modules/external-hashes", link);
    assert.throws(() => sdkContext(root, {}), /bound closure/);
    rmSync(link);
    symlinkSync("missing-runtime", link);
    assert.throws(() => sdkContext(root, {}));
  }));

test("SDK structured coverage rejects skipped, todo, setup-refused, empty, missing-file and inconsistent reports", () =>
  withFixture(({ root }) => {
    const name = "@al-ft/midgard-sdk";
    assert.equal(sdkReportCounts(root, name, report(root, name)).passed, 1);
    const mutations = [
      (r) => {
        r.testResults[0].assertionResults[0].status = "skipped";
        r.numPassedTests = 0;
      },
      (r) => {
        r.testResults[0].assertionResults[0].status = "todo";
        r.numPassedTests = 0;
      },
      (r) => {
        r.success = false;
      },
      (r) => {
        r.numRuntimeErrorTestSuites = 1;
      },
      (r) => {
        r.testResults = [];
        r.numPassedTests = 0;
      },
      (r) => {
        r.testResults[0].name = resolve(root, "other.test.ts");
      },
      (r) => {
        r.numTotalTests = 2;
      },
      (r) => {
        r.testResults[0].status = "failed";
      },
    ];
    for (const mutate of mutations) {
      const value = report(root, name);
      mutate(value);
      assert.throws(() => sdkReportCounts(root, name, value));
    }
  }));

test("real preflight step produces reusable evidence only from completed unchanged structured coverage", () =>
  withFixture(async ({ root, write }) => {
    write(
      "bin/corepack",
      `#!/usr/bin/env node\nimport fs from 'node:fs';\nimport path from 'node:path';\nconst args = process.argv.slice(2);\nconst name = args[args.indexOf('--filter')+1];\nconst directory = name === '@al-ft/lucid-midgard' ? 'lucid-midgard' : 'midgard-sdk';\nconst output = args.find(arg => arg.startsWith('--outputFile='))?.slice('--outputFile='.length);\nif (!output) throw new Error('standalone unexpectedly dispatched');\nconst status = process.env.SDK_FIXTURE_SKIP ? 'skipped' : 'passed';\nif (process.env.SDK_FIXTURE_DRIFT) fs.writeFileSync(path.resolve('node_modules/.pnpm/dependency/index.js'), 'changed during execution');\nfs.writeFileSync(output, JSON.stringify({success:true,numTotalTests:1,numPassedTests:status==='passed'?1:0,numFailedTests:0,testResults:[{name:path.resolve(directory,'tests/example.test.ts'),status:'passed',assertionResults:[{title:'case',status}]}]}));\n`,
    );
    chmodSync(resolve(root, "bin/corepack"), 0o755);
    const env = {
      ...process.env,
      PATH: `${resolve(root, "bin")}:${dirname(process.execPath)}:${process.env.PATH}`,
    };
    const full = {
      id: "demo-test",
      steps: Object.keys(SDK_SUITES).map((name) => ({
        argv: ["pnpm", "--filter", name, "test"],
        cwd: "demo",
        sdkSuite: name,
      })),
    };
    const lane = {
      id: "tx-preparation:sdk",
      steps: [{ argv: ["pnpm", "--dir", "demo", "run", "test:tx-prep:sdk"] }],
    };
    const plan = {
      planned: [full, lane].map(({ id, steps }) => ({
        check: { id, capabilities: [] },
        steps,
      })),
    };
    const result = await runPreflight({
      root,
      env,
      base: "unused",
      plan,
      probes: { invalidate() {} },
      log() {},
    });
    assert.equal(result.exitCode, 0);
    assert.equal(result.results[1].evidence.kind, "same-run-sdk-suites");
    const step = full.steps[0];
    const skipped = await spawnStep(
      root,
      step,
      { ...env, SDK_FIXTURE_SKIP: "1" },
      () => {},
    );
    assert.notEqual(skipped.status, 0);
    assert.equal(skipped.sdkSuite, undefined);
    const drift = await spawnStep(
      root,
      step,
      { ...env, SDK_FIXTURE_DRIFT: "1" },
      () => {},
    );
    assert.equal(drift.sdkSuite, undefined);
  }));
