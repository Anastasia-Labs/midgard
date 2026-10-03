import assert from "node:assert/strict";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import test from "node:test";

import { checkBuild } from "./build.mjs";
import { checkBoundaries } from "./discovery.mjs";
import {
  atomicJson,
  compiledDependencies,
  inputIdentity,
  outputIdentity,
} from "./files.mjs";
import { fixture } from "./fixture.test-support.mjs";
import { verifyReceipt } from "./receipts.mjs";
import { withResource } from "./resources.mjs";

const stamp = (root, name) =>
  atomicJson(resolve(root, `demo/${name}/dist/.contrib-build-v1.json`), {
    schema: "midgard-contrib-build/v1",
    root,
    package: name,
    inputs: inputIdentity(root, name),
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
      scripts: { build: "compiler", "build:contrib-raw": "compiler" },
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

test("a CLI main shared with its bin is probed with help, including startup failures", async (t) => {
  const root = fixture(t);
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.main = "dist/index.js";
  pkg.bin = { example: "./dist/index.js" };
  pkg.exports = { "./*": { "midgard-source": "./src/*.ts" } };
  writeFileSync(path, JSON.stringify(pkg));
  const entry = resolve(root, "demo/example/dist/index.js");
  writeFileSync(
    entry,
    'if (!process.argv.includes("--help")) process.exit(1); console.log("Usage: example");',
  );
  stamp(root, "example");
  const passed = await checkBoundaries(root, "example");
  assert.equal(passed.status, "passed");
  assert.equal(passed.steps.length, 1);
  assert.equal(passed.steps[0].argv.at(-1), "--help");
  assert.equal(verifyReceipt(root, passed.path).status, "passed");
  writeFileSync(entry, 'import "./missing-runtime.js";');
  stamp(root, "example");
  const failed = await checkBoundaries(root, "example");
  assert.equal(failed.status, "failed");
  assert.notEqual(failed.steps[0].exitCode, 0);
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

test("unguarded dist mutation during import fails and later substitutions invalidate receipts", async (t) => {
  const root = fixture(t);
  exported(
    root,
    "import {writeFileSync} from 'node:fs';writeFileSync(new URL(import.meta.url), 'export const x = 2;');",
  );
  const failed = await checkBoundaries(root, "example");
  assert.equal(failed.status, "failed");
  assert.match(failed.reason, /compiled artifacts changed/u);
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
    `import {readFileSync,writeFileSync} from 'node:fs';import {createHash} from 'node:crypto';const hash=value=>createHash('sha256').update(value).digest('hex');const path=new URL('.contrib-build-v1.json',import.meta.url);const stamp=JSON.parse(readFileSync(path));const changed='export const x = 2;';writeFileSync(new URL(import.meta.url),changed);stamp.outputs.files['demo/example/dist/index.js']=hash(changed);stamp.outputs.sha256=hash(JSON.stringify(Object.entries(stamp.outputs.files).sort()));writeFileSync(path,JSON.stringify(stamp));`,
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
