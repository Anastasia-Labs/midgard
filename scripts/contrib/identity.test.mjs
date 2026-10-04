import assert from "node:assert/strict";
import {
  readFileSync,
  writeFileSync,
  utimesSync,
  mkdirSync,
  cpSync,
  renameSync,
} from "node:fs";
import { resolve } from "node:path";
import test from "node:test";

import { checkBuild } from "./build.mjs";
import {
  atomicJson,
  filesUnder,
  inputIdentity,
  outputIdentity,
} from "./files.mjs";
import { enrollBuilds } from "./enroll-builds.mjs";
import { fixture } from "./fixture.test-support.mjs";

const stamp = (root) =>
  atomicJson(resolve(root, "demo/example/dist/.contrib-build-v1.json"), {
    schema: "midgard-contrib-build/v1",
    root,
    package: "example",
    inputs: inputIdentity(root, "example"),
    outputs: outputIdentity(root, "demo/example/dist"),
  });

test("repository receipts ignore generated site and spec outputs while binding new source inputs", (t) => {
  const root = fixture(t);
  writeFileSync(
    resolve(root, ".gitignore"),
    "technical-spec/*.pdf\ndocs-site/.source/\n",
  );
  const before = inputIdentity(root, "@repository").sha256;
  mkdirSync(resolve(root, "technical-spec"));
  writeFileSync(resolve(root, "technical-spec/midgard.pdf"), "compiled spec");
  mkdirSync(resolve(root, "docs-site/.source"), { recursive: true });
  writeFileSync(
    resolve(root, "docs-site/.source/generated.ts"),
    "compiled site",
  );
  assert.equal(inputIdentity(root, "@repository").sha256, before);
  mkdirSync(resolve(root, "scripts"));
  writeFileSync(resolve(root, "scripts/new-source.mjs"), "export const x=1;");
  assert.notEqual(inputIdentity(root, "@repository").sha256, before);
});

test("dist freshness ignores timestamps and binds actual emitted bytes", (t) => {
  const root = fixture(t);
  assert.equal(checkBuild(root, "example").status, "missing");
  stamp(root);
  assert.equal(checkBuild(root, "example").status, "fresh");
  const source = resolve(root, "demo/example/src/index.ts");
  writeFileSync(source, "export const x = 2;");
  utimesSync(source, 1, 1);
  assert.match(checkBuild(root, "example").reason, /input closure changed/u);
  stamp(root);
  writeFileSync(
    resolve(root, "demo/example/dist/index.js"),
    "foreign compiled artifact",
  );
  assert.match(checkBuild(root, "example").reason, /contents changed/u);
});

test("source directories named like outputs remain bound while owned outputs are excluded", (t) => {
  const root = fixture(t);
  for (const name of [
    "build",
    "target",
    "dist",
    "coverage",
    "logs",
    ".tmp",
    ".next",
    "deploymentInfo",
  ]) {
    const source = resolve(root, `demo/example/src/${name}/action.ts`);
    mkdirSync(resolve(source, ".."), { recursive: true });
    writeFileSync(source, "export const x=1;");
    stamp(root);
    const before = inputIdentity(root, "@repository").sha256;
    writeFileSync(source, "export const x=2;");
    assert.equal(checkBuild(root, "example").status, "stale", name);
    assert.notEqual(inputIdentity(root, "@repository").sha256, before, name);
    assert.ok(filesUnder(resolve(root, "demo/example/src")).includes(source));
  }
  mkdirSync(resolve(root, "demo/example/native/target"), { recursive: true });
  writeFileSync(resolve(root, "demo/example/native/Cargo.toml"), "[package]");
  writeFileSync(resolve(root, "demo/example/native/target/generated"), "one");
  mkdirSync(resolve(root, "onchain/aiken/build"), { recursive: true });
  writeFileSync(resolve(root, "onchain/aiken/aiken.toml"), "name='test'");
  writeFileSync(resolve(root, "onchain/aiken/build/generated"), "one");
  stamp(root);
  const before = inputIdentity(root, "@repository").sha256;
  for (const path of [
    "demo/example/dist/index.js",
    "demo/example/native/target/generated",
    "onchain/aiken/build/generated",
  ]) {
    writeFileSync(resolve(root, path), "two");
  }
  assert.equal(inputIdentity(root, "@repository").sha256, before);
  // Emitted bytes are checked independently even though they are not source.
  assert.match(checkBuild(root, "example").reason, /contents changed/u);
});

test("lockfile, exports, native sources and dependency facets invalidate", (t) => {
  const root = fixture(t);
  mkdirSync(resolve(root, "demo/dependency/src"), { recursive: true });
  writeFileSync(
    resolve(root, "demo/dependency/package.json"),
    JSON.stringify({ name: "dependency" }),
  );
  writeFileSync(
    resolve(root, "demo/dependency/src/facet.ts"),
    "export const y = 1;",
  );
  const path = resolve(root, "demo/example/package.json");
  const pkg = JSON.parse(readFileSync(path));
  pkg.dependencies = { dependency: "workspace:*" };
  writeFileSync(path, JSON.stringify(pkg));
  for (const changed of [
    "demo/pnpm-lock.yaml",
    "demo/dependency/src/facet.ts",
    "demo/example/package.json",
    "demo/example/native/Cargo.lock",
    "scripts/pnpm.mjs",
    "scripts/bin/pnpm",
  ]) {
    stamp(root);
    mkdirSync(resolve(root, changed, ".."), { recursive: true });
    writeFileSync(
      resolve(root, changed),
      `${readFileSync(resolve(root, changed), { flag: "a+" })} `,
    );
    assert.equal(checkBuild(root, "example").status, "stale", changed);
  }
});

test("copying an identical dist from another checkout is refused", (t) => {
  const first = fixture(t);
  const second = fixture(t);
  stamp(first);
  cpSync(
    resolve(first, "demo/example/dist"),
    resolve(second, "demo/example/dist"),
    { recursive: true },
  );
  assert.match(checkBuild(second, "example").reason, /another.*checkout/u);
});

test("watcher native outputs have separate ownership from compiled JavaScript", (t) => {
  const root = fixture(t);
  const directory = "demo/midgard-watcher/dist";
  renameSync(
    resolve(root, "demo/example"),
    resolve(root, "demo/midgard-watcher"),
  );
  atomicJson(resolve(root, "demo/midgard-watcher/package.json"), {
    name: "midgard-watcher",
  });
  const before = outputIdentity(root, directory).sha256;
  mkdirSync(resolve(root, directory, "native"));
  writeFileSync(
    resolve(root, directory, "native/midgard-chain-sync"),
    "Go binary",
  );
  assert.equal(
    outputIdentity(root, directory).sha256,
    before,
    "adding the separately guarded Go binary must not invalidate TypeScript",
  );
  writeFileSync(
    resolve(root, directory, "native/unowned.js"),
    "another output",
  );
  assert.notEqual(
    outputIdentity(root, directory).sha256,
    before,
    "unowned siblings in the native directory remain compiled outputs",
  );
  const withSibling = outputIdentity(root, directory).sha256;
  writeFileSync(resolve(root, directory, "index.js"), "tampered JavaScript");
  assert.notEqual(outputIdentity(root, directory).sha256, withSibling);
  renameSync(
    resolve(root, "demo/midgard-watcher"),
    resolve(root, "demo/example"),
  );
  const ordinary = outputIdentity(root, "demo/example/dist").sha256;
  writeFileSync(
    resolve(root, "demo/example/dist/native/midgard-chain-sync"),
    "changed",
  );
  assert.notEqual(
    outputIdentity(root, "demo/example/dist").sha256,
    ordinary,
    "a native directory in another package remains a compiled output",
  );
});

test("build enrollment is idempotent and refuses unknown wrappers", (t) => {
  const root = fixture(t);
  assert.throws(() => enrollBuilds(root), /unguarded/u);
  assert.equal(enrollBuilds(root, { write: true }).length, 1);
  assert.deepEqual(enrollBuilds(root), []);
});
