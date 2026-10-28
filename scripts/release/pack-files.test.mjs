import assert from "node:assert/strict";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { copyPackFiles, removePackFiles } from "./pack-files.mjs";

const repositoryLicense = readFileSync(
  resolve(dirname(fileURLToPath(import.meta.url)), "../../LICENSE"),
);

const packageFixture = () => {
  const packageDir = mkdtempSync(join(tmpdir(), "pack-files-"));
  writeFileSync(join(packageDir, "package.json"), "{}\n");
  return packageDir;
};

test("copies the licence for the pack, then removes it", () => {
  const packageDir = packageFixture();
  copyPackFiles(packageDir);
  assert.ok(
    readFileSync(join(packageDir, "LICENSE")).equals(repositoryLicense),
  );
  copyPackFiles(packageDir);
  removePackFiles(packageDir);
  assert.equal(existsSync(join(packageDir, "LICENSE")), false);
});

test("leaves the vendored copies the build wrote", () => {
  const packageDir = packageFixture();
  mkdirSync(join(packageDir, "vendor/vendored"), { recursive: true });
  writeFileSync(join(packageDir, "vendor/vendored/index.js"), "\n");
  copyPackFiles(packageDir);
  removePackFiles(packageDir);
  assert.ok(existsSync(join(packageDir, "vendor/vendored/index.js")));
});

test("never replaces or deletes a licence it did not write", () => {
  const packageDir = packageFixture();
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
});
