import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  renameSync,
  writeFileSync,
} from "node:fs";
import { resolve } from "node:path";
import test from "node:test";

import { BUILD_TRACE, buildEnvironment } from "./build-inputs.mjs";
import { checkBuild } from "./build.mjs";
import { checkBoundaries } from "./discovery.mjs";
import {
  atomicJson,
  compiledDependencies,
  inputIdentity,
  outputIdentity,
} from "./files.mjs";
import { fixture as bareFixture } from "./fixture.test-support.mjs";
import {
  probeInstalledPackage,
  runtimeSurfaces,
} from "./installed-boundary.mjs";
import { verifyReceipt } from "./receipts.mjs";
import { withResource } from "./resources.mjs";

// The fixture edits its manifests between installs, so each install has to
// rewrite the fixture's own lockfile. pnpm freezes the lockfile by default
// when CI is set, which would refuse the second install there and nowhere else.
const install = (root) =>
  execFileSync(
    "corepack",
    [
      "pnpm",
      "install",
      "--offline",
      "--ignore-scripts",
      "--no-frozen-lockfile",
    ],
    { cwd: resolve(root, "demo"), stdio: "pipe" },
  );
const fixture = (t) => {
  const root = bareFixture(t);
  writeFileSync(
    resolve(root, "demo/package.json"),
    JSON.stringify({
      name: "boundary-fixture",
      private: true,
      packageManager: JSON.parse(
        readFileSync(new URL("../../demo/package.json", import.meta.url)),
      ).packageManager,
    }),
  );
  writeFileSync(
    resolve(root, "demo/pnpm-workspace.yaml"),
    'packages:\n  - "*"\nforceLegacyDeploy: true\n',
  );
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.version = "1.0.0";
  pkg.midgardTasks = {
    build: {
      steps: [{ command: ["tsup", "src/index.ts"] }],
      outputs: ["dist"],
    },
  };
  writeFileSync(path, JSON.stringify(pkg));
  install(root);
  return root;
};

const stamp = (root, name) =>
  atomicJson(resolve(root, `demo/${name}/dist/.contrib-build-v1.json`), {
    schema: "midgard-contrib-build/v1",
    root,
    package: name,
    reads: BUILD_TRACE,
    inputs: inputIdentity(root, name),
    environment: buildEnvironment(root, name),
    outputs: outputIdentity(root, `demo/${name}/dist`),
    dependencies: compiledDependencies(root, name),
  });
const exported = (root, code = "export const x = 1;") => {
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.exports = { ".": { import: "./dist/index.js" } };
  writeFileSync(path, JSON.stringify(pkg));
  writeFileSync(resolve(root, "demo/example/dist/index.js"), code);
  stamp(root, "example");
};

test("compiled dependency substitution invalidates a consumer with unchanged sources", (t) => {
  const root = fixture(t);
  mkdirSync(resolve(root, "demo/dependency/dist"), { recursive: true });
  writeFileSync(
    resolve(root, "demo/dependency/package.json"),
    JSON.stringify({
      name: "dependency",
      scripts: { build: "compiler", "build:contrib-raw": "tsup src/index.ts" },
      midgardTasks: {
        build: {
          steps: [{ command: ["tsup", "src/index.ts"] }],
          outputs: ["dist"],
        },
      },
    }),
  );
  writeFileSync(
    resolve(root, "demo/dependency/dist/index.js"),
    "export const y = 1;",
  );
  const pkg = JSON.parse(
    readFileSync(resolve(root, "demo/example/package.json")),
  );
  pkg.dependencies = { dependency: "workspace:*" };
  writeFileSync(
    resolve(root, "demo/example/package.json"),
    JSON.stringify(pkg),
  );
  stamp(root, "dependency");
  stamp(root, "example");
  assert.equal(checkBuild(root, "example").status, "fresh");
  writeFileSync(
    resolve(root, "demo/dependency/dist/index.js"),
    "export const y = 2;",
  );
  assert.match(
    checkBuild(root, "example").reason,
    /compiled dependency contents changed/u,
  );
});

test("boundary imports resolve against the selected root from another cwd", async (t) => {
  const root = fixture(t);
  exported(root);
  const receipt = await checkBoundaries(root, "example");
  assert.equal(receipt.status, "passed");
  assert.equal(verifyReceipt(root, receipt.path).status, "passed");
});

test("nested workspace deployment keeps dependency bins inside the consumer", async (t) => {
  const base = fixture(t);
  const root = resolve(base, "a/b/c");
  mkdirSync(root, { recursive: true });
  renameSync(resolve(base, "demo"), resolve(root, "demo"));
  const dependency = resolve(root, "demo/bin-dependency");
  mkdirSync(dependency);
  writeFileSync(
    resolve(dependency, "package.json"),
    JSON.stringify({
      name: "bin-dependency",
      version: "1.0.0",
      bin: { "fixture-cli": "bin.js" },
    }),
  );
  writeFileSync(resolve(dependency, "bin.js"), "#!/usr/bin/env node\n");
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.dependencies = { "bin-dependency": "workspace:*" };
  pkg.exports = { ".": "./dist/index.js" };
  writeFileSync(path, JSON.stringify(pkg));
  install(root);
  const result = await probeInstalledPackage({
    root,
    pkg: { ...pkg, directory: "demo/example" },
    directory: resolve(base, "deployment"),
    env: process.env,
  });
  assert.equal(result.reason, undefined);
  assert.ok(result.steps.every((step) => step.exitCode === 0 && !step.reason));
  const installation = JSON.parse(
    readFileSync(resolve(base, "deployment/installation.json")),
  );
  assert.ok(
    installation.before.files[
      "node_modules/example/node_modules/.bin/fixture-cli"
    ],
  );
  assert.equal(
    existsSync(resolve(base, "a/deployment")),
    false,
    "deploy must not write bins using workspace-relative paths at a package root",
  );
});

test("boundary refuses a broken advertised require target even when its ESM target loads", async (t) => {
  const root = fixture(t);
  exported(root);
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.exports["."].require = "./dist/index.cjs";
  writeFileSync(path, JSON.stringify(pkg));
  writeFileSync(
    resolve(root, "demo/example/dist/index.cjs"),
    'throw new Error("broken advertised CommonJS target");',
  );
  stamp(root, "example");
  const receipt = await checkBoundaries(root, "example");
  assert.equal(receipt.status, "failed");
  assert.match(
    receipt.steps.map((step) => readFileSync(step.logPath, "utf8")).join("\n"),
    /broken advertised CommonJS target/u,
  );
});

test("boundary requires every export to finish loading before accepting a zero exit", async (t) => {
  const root = fixture(t);
  exported(root, "process.exit(0);");
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.exports = {
    ".": "./dist/index.js",
    "./broken": "./dist/broken.js",
  };
  writeFileSync(path, JSON.stringify(pkg));
  writeFileSync(
    resolve(root, "demo/example/dist/broken.js"),
    'throw new Error("later export must be loaded");',
  );
  stamp(root, "example");
  const early = await checkBoundaries(root, "example");
  assert.equal(
    early.status,
    "failed",
    "a zero exit cannot prove all exports loaded",
  );
  for (const mode of ["import", "require"]) {
    const step = early.steps.find((step) =>
      step.logPath.endsWith(`/${mode}.log`),
    );
    assert.equal(step.exitCode, 0);
    assert.match(step.reason, /did not complete every requested export/u);
  }
  writeFileSync(
    resolve(root, "demo/example/dist/index.js"),
    "export const x = 1;",
  );
  writeFileSync(
    resolve(root, "demo/example/dist/broken.js"),
    "export const y = 2;",
  );
  stamp(root, "example");
  const complete = await checkBoundaries(root, "example");
  assert.equal(complete.status, "passed");
  for (const mode of ["import", "require"]) {
    const evidence = complete.evidenceFiles.find(({ path }) =>
      path.endsWith(`/${mode}-complete.json`),
    );
    const proof = JSON.parse(readFileSync(evidence.path));
    assert.equal(proof.mode, mode);
    assert.deepEqual(
      proof.loaded.map(({ specifier }) => specifier),
      ["example", "example/broken"],
    );
  }
  assert.equal(verifyReceipt(root, complete.path).status, "passed");
});

test("boundary refuses an exported dist omitted from the installed package", async (t) => {
  const root = fixture(t);
  exported(root);
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.files = ["src"];
  writeFileSync(path, JSON.stringify(pkg));
  stamp(root, "example");
  const receipt = await checkBoundaries(root, "example");
  assert.equal(receipt.status, "failed");
  assert.match(
    receipt.steps.map((step) => readFileSync(step.logPath, "utf8")).join("\n"),
    /(?:Cannot find module|ERR_MODULE_NOT_FOUND)/u,
  );
});

test("a CLI main shared with its bin is probed with help, including startup failures", async (t) => {
  const root = fixture(t);
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.main = "dist/index.js";
  pkg.bin = { example: "./dist/index.js" };
  writeFileSync(path, JSON.stringify(pkg));
  const entry = resolve(root, "demo/example/dist/index.js");
  writeFileSync(
    entry,
    'if (!process.argv.includes("--help")) process.exit(1); console.log("Usage: example");',
  );
  stamp(root, "example");
  const passed = await checkBoundaries(root, "example");
  assert.equal(passed.status, "passed");
  assert.equal(passed.steps.length, 2);
  assert.equal(passed.steps.at(-1).argv.at(-1), "--help");
  assert.equal(verifyReceipt(root, passed.path).status, "passed");
  writeFileSync(entry, 'import "./missing-runtime.js";');
  stamp(root, "example");
  const failed = await checkBoundaries(root, "example");
  assert.equal(failed.status, "failed");
  assert.notEqual(failed.steps.at(-1).exitCode, 0);
});

test("boundary ownership excludes guarded writers throughout child imports", async (t) => {
  const root = fixture(t);
  exported(
    root,
    "await new Promise(resolve => setTimeout(resolve, 200));export const x = 1;",
  );
  let peerEntered = false;
  const running = checkBoundaries(root, "example");
  const peer = withResource(`workspace:${root}`, () => {
    peerEntered = true;
  });
  const receipt = await running;
  assert.equal(peerEntered, false);
  assert.equal(receipt.status, "passed");
  await peer;
});

test("installed mutation during import fails and later source substitutions invalidate receipts", async (t) => {
  const root = fixture(t);
  exported(
    root,
    "import {writeFileSync} from 'node:fs';writeFileSync(new URL(import.meta.url), 'export const x = 2;');",
  );
  const failed = await checkBoundaries(root, "example");
  assert.equal(failed.status, "failed");
  assert.match(failed.reason, /installed package artifacts changed/u);
  exported(root);
  const passed = await checkBoundaries(root, "example");
  writeFileSync(
    resolve(root, "demo/example/dist/index.js"),
    "export const x = 3;",
  );
  assert.throws(
    () => verifyReceipt(root, passed.path),
    /receipt artifact changed/u,
  );
});

test("boundary compares its execution snapshot even when a child restamps changed dist", async (t) => {
  const root = fixture(t);
  exported(
    root,
    `import {readFileSync,writeFileSync} from 'node:fs';import {createHash} from 'node:crypto';const hash=value=>createHash('sha256').update(value).digest('hex');const path=${JSON.stringify(resolve(root, "demo/example/dist/.contrib-build-v1.json"))};const stamp=JSON.parse(readFileSync(path));const changed='export const x = 2;';writeFileSync(${JSON.stringify(resolve(root, "demo/example/dist/index.js"))},changed);stamp.outputs.files['demo/example/dist/index.js']=hash(changed);stamp.outputs.sha256=hash(JSON.stringify(Object.entries(stamp.outputs.files).sort()));writeFileSync(path,JSON.stringify(stamp));`,
  );
  const receipt = await checkBoundaries(root, "example");
  assert.equal(
    checkBuild(root, "example").status,
    "fresh",
    "the child produced a matching stamp, so freshness alone cannot refuse",
  );
  assert.equal(receipt.status, "failed");
  assert.match(receipt.reason, /compiled artifacts changed/u);
});

test("boundary isolates production closure from a dependency available only in the workspace", async (t) => {
  const root = fixture(t);
  mkdirSync(resolve(root, "demo/hidden"));
  writeFileSync(
    resolve(root, "demo/hidden/package.json"),
    JSON.stringify({
      name: "hidden",
      version: "1.0.0",
      type: "module",
      exports: "./index.js",
    }),
  );
  writeFileSync(
    resolve(root, "demo/hidden/index.js"),
    "export const hidden = true;",
  );
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.devDependencies = { hidden: "workspace:*" };
  writeFileSync(path, JSON.stringify(pkg));
  install(root);
  exported(root, 'import "hidden"; export const x = 1;');
  const failed = await checkBoundaries(root, "example");
  assert.equal(failed.status, "failed");
  assert.match(
    readFileSync(failed.steps.at(-1).logPath, "utf8"),
    /Cannot find package 'hidden'/u,
  );
  // The identical package is valid when the dependency belongs to production.
  const fixed = JSON.parse(readFileSync(path));
  fixed.dependencies = fixed.devDependencies;
  delete fixed.devDependencies;
  writeFileSync(path, JSON.stringify(fixed));
  install(root);
  stamp(root, "example");
  const passed = await checkBoundaries(root, "example");
  assert.equal(passed.status, "passed");
  assert.equal(verifyReceipt(root, passed.path).status, "passed");
  writeFileSync(
    passed.evidenceFiles.find(({ path }) => path.endsWith("/installation.json"))
      .path,
    "changed installation evidence",
  );
  assert.throws(
    () => verifyReceipt(root, passed.path),
    /evidence missing\/changed/u,
  );
  // A declared peer supplied by the workspace in development must also be
  // available to the independent installed consumer.
  fixed.peerDependencies = fixed.dependencies;
  fixed.devDependencies = fixed.dependencies;
  delete fixed.dependencies;
  writeFileSync(path, JSON.stringify(fixed));
  install(root);
  stamp(root, "example");
  assert.equal((await checkBoundaries(root, "example")).status, "passed");
});

test("boundary exercises nested, array and default exports through Node's resolver", async (t) => {
  const root = fixture(t);
  exported(root);
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.exports = {
    ".": {
      node: { import: [null, "./dist/index.js"], require: "./dist/index.cjs" },
    },
    "./fallback": { default: "./dist/index.cjs" },
    "./sync": { "module-sync": "./dist/index.cjs" },
    "./source/*": { "midgard-source": "./src/*.ts", types: "./src/*.ts" },
  };
  writeFileSync(path, JSON.stringify(pkg));
  writeFileSync(resolve(root, "demo/example/dist/index.cjs"), "exports.x = 1;");
  stamp(root, "example");
  const passed = await checkBoundaries(root, "example");
  assert.equal(passed.status, "passed");
  assert.deepEqual(passed.surfaces.import, [
    "example",
    "example/fallback",
    "example/sync",
  ]);
  assert.deepEqual(passed.surfaces.require, [
    "example",
    "example/fallback",
    "example/sync",
  ]);
  assert.equal(passed.surfaces.inactive.length, 1);
  assert.throws(
    () =>
      runtimeSurfaces({
        name: "example",
        exports: { "./*": { default: "./dist/*.js" } },
      }),
    /runtime wildcard export/u,
  );
  pkg.exports = { ".": [false, 42] };
  pkg.bin = { example: "./dist/index.cjs" };
  writeFileSync(path, JSON.stringify(pkg));
  stamp(root, "example");
  const invalid = await checkBoundaries(root, "example");
  assert.equal(invalid.status, "failed");
  assert.match(
    readFileSync(
      invalid.steps.find((step) => step.exitCode !== 0).logPath,
      "utf8",
    ),
    /ERR_INVALID_PACKAGE_TARGET/u,
  );
});

test("boundary refuses runtime imports that escape back to the source checkout", async (t) => {
  const root = fixture(t);
  const ambient = resolve(root, "demo/example/src/ambient.mjs");
  writeFileSync(ambient, "export const ambient = true;");
  exported(root, `import ${JSON.stringify(ambient)};`);
  const receipt = await checkBoundaries(root, "example");
  assert.equal(receipt.status, "failed");
  assert.match(
    readFileSync(receipt.steps.at(-1).logPath, "utf8"),
    /outside isolated consumer/u,
  );
});
