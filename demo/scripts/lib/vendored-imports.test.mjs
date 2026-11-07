import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { treeDifferences } from "./vendored-imports.mjs";

const script = join(
  dirname(fileURLToPath(import.meta.url)),
  "vendored-imports.mjs",
);

// A package whose `imports` are `imports`, with `dependency` installed.
const packageFixture = (imports, dependency = "@x/vendored") => {
  const base = mkdtempSync(join(tmpdir(), "vendored-imports-"));
  const installed = join(base, "store", dependency);
  mkdirSync(join(installed, "dist"), { recursive: true });
  writeFileSync(
    join(installed, "package.json"),
    JSON.stringify({ name: dependency }),
  );
  writeFileSync(join(installed, "dist/index.js"), "export const root = 1;\n");
  const packageDir = join(base, "package");
  mkdirSync(join(packageDir, "node_modules", dependency, ".."), {
    recursive: true,
  });
  symlinkSync(installed, join(packageDir, "node_modules", dependency), "dir");
  writeFileSync(join(packageDir, "package.json"), JSON.stringify({ imports }));
  return { installed, packageDir };
};

const vendored = (dependency) => ({
  "midgard-source": dependency,
  import: `./vendor/${dependency.split("/").pop()}/dist/index.js`,
});

// Runs the build step in `packageDir`, as a package build does.
const build = (packageDir) =>
  spawnSync(process.execPath, [script], { cwd: packageDir, encoding: "utf8" });

test("the build writes each vendored import as installed and replaces what was there", () => {
  const { installed, packageDir } = packageFixture({
    "#vendored": vendored("@x/vendored"),
  });
  assert.equal(build(packageDir).status, 0);
  const copy = join(packageDir, "vendor/vendored");
  assert.deepEqual(treeDifferences(installed, copy), []);
  writeFileSync(join(copy, "dist/index.js"), "export const root = 2;\n");
  writeFileSync(join(packageDir, "vendor/leftover.js"), "\n");
  assert.equal(build(packageDir).status, 0);
  assert.deepEqual(treeDifferences(installed, copy), []);
  assert.equal(existsSync(join(packageDir, "vendor/leftover.js")), false);
  const { status } = spawnSync(
    process.execPath,
    ["--input-type=module", "-e", 'await import("#vendored")'],
    { cwd: packageDir },
  );
  assert.equal(status, 0, "the package resolves its import to the copy");
});

test("the vendored Merkle Patricia Forestry gets its source notice", () => {
  const dependency = "@aiken-lang/merkle-patricia-forestry";
  const { installed, packageDir } = packageFixture(
    { "#merkle-patricia-forestry": vendored(dependency) },
    dependency,
  );
  assert.equal(build(packageDir).status, 0);
  const copy = join(packageDir, "vendor/merkle-patricia-forestry");
  const notice = readFileSync(join(copy, "SOURCE.md"), "utf8");
  assert.match(
    notice,
    /https:\/\/github\.com\/Anastasia-Labs\/midgard\/tree\/main\/demo\/vendor\/mpf-source/u,
  );
  assert.match(notice, /MPL-2\.0/u);
  assert.deepEqual(treeDifferences(installed, copy), ["SOURCE.md"]);
});

test("a package with nothing to vendor, or a vendored import not installed, fails the build and keeps vendor/", () => {
  for (const imports of [
    { "#local": { "midgard-source": "./src/local.ts" } },
    { "#missing": vendored("@x/missing") },
  ]) {
    const { packageDir } = packageFixture(imports);
    mkdirSync(join(packageDir, "vendor"));
    writeFileSync(join(packageDir, "vendor/kept.js"), "\n");
    assert.notEqual(build(packageDir).status, 0);
    assert.ok(existsSync(join(packageDir, "vendor/kept.js")));
  }
});
