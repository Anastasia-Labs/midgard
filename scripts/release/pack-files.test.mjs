import assert from "node:assert/strict";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  copyPackFiles,
  removePackFiles,
  treeDifferences,
} from "./pack-files.mjs";

const repositoryLicense = readFileSync(
  resolve(dirname(fileURLToPath(import.meta.url)), "../../LICENSE"),
);

// A package whose `#vendored` import names its installed @x/vendored.
const packageFixture = (imports = { "#vendored": "@x/vendored" }) => {
  const base = mkdtempSync(join(tmpdir(), "pack-files-"));
  const installed = join(base, "store/@x/vendored");
  mkdirSync(join(installed, "dist"), { recursive: true });
  writeFileSync(join(installed, "package.json"), '{"name":"@x/vendored"}\n');
  writeFileSync(join(installed, "dist/index.js"), "export const root = 1;\n");
  const packageDir = join(base, "package");
  mkdirSync(join(packageDir, "node_modules/@x"), { recursive: true });
  symlinkSync(installed, join(packageDir, "node_modules/@x/vendored"), "dir");
  writeFileSync(join(packageDir, "package.json"), JSON.stringify({ imports }));
  return { installed, packageDir };
};

test("copies the licence and each vendored import for the pack, then removes them", () => {
  const { installed, packageDir } = packageFixture();
  copyPackFiles(packageDir);
  assert.ok(
    readFileSync(join(packageDir, "LICENSE")).equals(repositoryLicense),
  );
  assert.deepEqual(
    treeDifferences(installed, join(packageDir, "vendor/vendored")),
    [],
  );
  copyPackFiles(packageDir);
  removePackFiles(packageDir);
  assert.equal(existsSync(join(packageDir, "LICENSE")), false);
  assert.equal(existsSync(join(packageDir, "vendor")), false);
});

test("a package without vendored imports gets only the licence", () => {
  const { packageDir } = packageFixture({ "#local": "./src/local.ts" });
  copyPackFiles(packageDir);
  assert.ok(existsSync(join(packageDir, "LICENSE")));
  assert.equal(existsSync(join(packageDir, "vendor")), false);
});

test("never replaces or deletes a licence or vendored copy it did not write", () => {
  const { packageDir } = packageFixture();
  writeFileSync(join(packageDir, "LICENSE"), "another licence\n");
  assert.throws(
    () => copyPackFiles(packageDir),
    /differs from the repository/u,
  );
  removePackFiles(packageDir);
  assert.equal(
    readFileSync(join(packageDir, "LICENSE"), "utf8"),
    "another licence\n",
  );

  const vendored = packageFixture();
  mkdirSync(join(vendored.packageDir, "vendor/vendored"), { recursive: true });
  writeFileSync(join(vendored.packageDir, "vendor/vendored/local.js"), "\n");
  assert.throws(
    () => copyPackFiles(vendored.packageDir),
    /differs from the installed @x\/vendored/u,
  );
  removePackFiles(vendored.packageDir);
  assert.ok(existsSync(join(vendored.packageDir, "vendor/vendored/local.js")));
});

test("a vendored import that is not installed stops the pack", () => {
  const { packageDir } = packageFixture({ "#missing": "@x/missing" });
  assert.throws(
    () => copyPackFiles(packageDir),
    /@x\/missing is not installed/u,
  );
});
